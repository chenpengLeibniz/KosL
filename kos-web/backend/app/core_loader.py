"""
Core 层：类型与谓词仅通过「Core 加载」菜单注入，不预加载静态文件。
支持 .kos 文件上传与粘贴；解析真实 KOS 语法（type/def/Prop）。
加载后同步到 Kernel 的 Γ。
"""
import re
from pathlib import Path
from typing import Any

# 仅动态库，不预加载
_dynamic: dict[str, list[dict]] = {"types": [], "predicates": [], "constructors": []}


def load_core() -> dict[str, Any]:
    return {
        "types": list(_dynamic["types"]),
        "predicates": list(_dynamic["predicates"]),
        "constructors": list(_dynamic["constructors"]),
    }


def get_types() -> list[dict]:
    return load_core().get("types", [])


def get_predicates() -> list[dict]:
    return load_core().get("predicates", [])


def get_constructors() -> list[dict]:
    return load_core().get("constructors", [])


def get_gamma_for_kernel() -> dict[str, Any]:
    return load_core()


def _strip_comment(line: str) -> str:
    """去掉 -- 到行尾的注释，并规范化换行。"""
    line = line.replace("\r\n", "\n").replace("\r", "")
    i = line.find("--")
    if i >= 0:
        line = line[:i]
    return line.strip()


def _parse_kos(source: str) -> tuple[list[dict], list[dict], list[dict]]:
    """
    解析真实 KOS 语法，提取类型、谓词、构造函数。
    支持：
      type Name : Expr          e.g. type BatchID : U0
      def Name : Type1 := Expr  e.g. def FailEvt : Type1 := Sigma(b : BatchID). ...
      def name : Pi(...). (A -> B) := lam ...  构造函数
      Prop PXXX 在定义中出现时收集为谓词
    """
    types: list[dict] = []
    predicates: list[dict] = []
    constructors: list[dict] = []
    seen: set[str] = set()
    # 收集所有 Prop PXXX 作为谓词名
    prop_names: set[str] = set()

    lines = source.splitlines()
    i = 0
    while i < len(lines):
        raw = lines[i]
        line = _strip_comment(raw)
        i += 1
        if not line or line.startswith("module ") or line == "where":
            continue

        # type Name : Expr  （原子类型）
        m = re.match(r"type\s+(\w+)\s*:\s*(.+)", line, re.IGNORECASE)
        if m:
            name, expr = m.group(1), m.group(2).strip()
            if name not in seen:
                seen.add(name)
                types.append({
                    "id": name, "name": name, "category": "atom",
                    "definition": expr, "description": f"type {name} : {expr}",
                })
            continue

        # def Name : TypeExpr [:= Body]  （类型或构造函数，Body 可多行）
        m = re.match(r"def\s+(\w+)\s*:\s*(.+)", line, re.IGNORECASE)
        if m:
            # 先规范化换行，避免 ":=\r" 导致 split(" := ") 失败
            rest = m.group(2).strip().replace("\r\n", "\n").replace("\r", "")
            name = m.group(1)
            type_part = rest
            body_part = ""
            # 支持 " := " / " :=" / ":=" 以便正确截断类型部分
            for sep in (" := ", " :=", ":="):
                if sep in rest:
                    type_part, body_part = rest.split(sep, 1)
                    type_part = type_part.strip()
                    body_part = body_part.strip()
                    break
            # Body 在下一行开始：读到非缩进行或新 def/type 为止；若某行含 " := " 则前半为 type 延续
            while i < len(lines):
                next_line = lines[i]
                stripped = _strip_comment(next_line).replace("\r", "")
                if not stripped:
                    i += 1
                    continue
                if stripped.startswith("def ") or stripped.startswith("type "):
                    break
                for sep in (" := ", " :=", ":="):
                    if sep in stripped:
                        before, after = stripped.split(sep, 1)
                        type_part = (type_part + " " + before.strip()).strip()
                        body_part = (body_part + " " + after.strip()).strip()
                        i += 1
                        break
                else:
                    body_part += " " + stripped
                    i += 1
            full_def = f"{type_part} := {body_part}" if body_part else type_part

            if name not in seen:
                seen.add(name)
                # 构造函数：名字 mk 开头且含 Pi(；或含 Pi( 与 -> 且带 lam 体
                has_pi = bool(re.search(r"Pi\s*\(", type_part))
                is_constructor = (
                    (name.startswith("mk") and has_pi)
                    or (
                        has_pi
                        and (" -> " in type_part or "->" in type_part)
                        and ("lam " in full_def or "lam (" in full_def)
                    )
                )
                if is_constructor:
                    # 规范化后再提取 target，避免 ":=" / \r 导致正则不匹配
                    target = (
                        type_part.replace("\r", "")
                        .replace("\n", " ")
                        .strip()
                    )
                    # 去掉末尾可能残留的 " :=" / ":="（多行合并时混入）
                    target = re.sub(r"\s*:?\s*=\s*$", "", target).strip()
                    if "->" in target:
                        target = target.split("->")[-1].strip()
                    else:
                        m = re.search(r"\.\s*(\w+)\s*$", target)
                        if m:
                            target = m.group(1)
                    target = re.sub(r"\)\s*:?\s*=\s*$", "", target).replace("\r", "").replace("\n", " ").strip().rstrip(")")
                    # 若仍过长，取最后一个 ". Identifier" 的 Identifier
                    if len(target) > 40:
                        m2 = re.search(r"\.\s*(\w+)\s*$", target)
                        if m2:
                            target = m2.group(1)
                    constructors.append({
                        "id": name, "name": name, "target": target,
                        "signature": full_def[:300] + ("..." if len(full_def) > 300 else ""),
                        "description": f"constructor {name}",
                    })
                else:
                    cat = "composite" if ("Sigma" in full_def or "Pi(" in full_def) else "atom"
                    types.append({
                        "id": name, "name": name, "category": cat,
                        "definition": full_def, "description": f"def {name}",
                    })
                for prop in re.findall(r"Prop\s+(\w+)", full_def):
                    if prop not in prop_names:
                        prop_names.add(prop)
                        predicates.append({"id": prop, "name": prop, "description": f"Prop {prop}"})
            continue

        # 单独一行的 Prop PXXX（谓词声明）
        m = re.match(r"Prop\s+(\w+)\s*$", line)
        if m and m.group(1) not in prop_names:
            prop_names.add(m.group(1))
            predicates.append({"id": m.group(1), "name": m.group(1), "description": f"Prop {m.group(1)}"})

    return types, predicates, constructors


def load_from_kos(kos_source: str, use_kos_core: bool = False) -> dict[str, Any]:
    """
    从 KOS 格式文本解析并追加类型与谓词。
    use_kos_core：若 True 且配置了 KOS_CORE_PATH，先校验（可选）；默认 False 以保证解析优先。
    """
    global _dynamic
    if use_kos_core and kos_source.strip():
        try:
            from app.kos_core_bridge import kos_core_check_file
            ok, err = kos_core_check_file(kos_source)
            if not ok:
                return {"ok": False, "error": err or "kos-core 校验失败", "added": {"types": 0, "predicates": 0, "constructors": 0}}
        except Exception as e:
            pass  # 校验失败时仍继续解析
    try:
        t, p, c = _parse_kos(kos_source)
        _dynamic["types"].extend(t)
        _dynamic["predicates"].extend(p)
        _dynamic["constructors"].extend(c)
        return {
            "ok": True,
            "added": {"types": len(t), "predicates": len(p), "constructors": len(c)},
            "types": t,
            "predicates": p,
            "constructors": c,
        }
    except Exception as e:
        return {"ok": False, "error": str(e), "added": {"types": 0, "predicates": 0, "constructors": 0}}


def load_from_kos_file_content(content: str, filename: str = "") -> dict[str, Any]:
    """从 .kos 文件内容加载（上传后调用）；不依赖 kos-core 校验，本地解析。"""
    return load_from_kos(content, use_kos_core=False)


def clear_dynamic() -> None:
    global _dynamic
    _dynamic = {"types": [], "predicates": [], "constructors": []}
