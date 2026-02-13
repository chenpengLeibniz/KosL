"""
与 kos-core（Haskell）可执行文件对接：check-file、check-term、prove 等。
若未设置 KOS_CORE_PATH 则降级为本地解析（core_loader 的 _parse_kos_like）。
"""
import os
import subprocess
import tempfile
from pathlib import Path
from typing import Any


def get_kos_core_path() -> str | None:
    return os.environ.get("KOS_CORE_PATH")


def kos_core_check_file(kos_content: str) -> tuple[bool, str]:
    """
    调用 kos-core 校验 .kos 文件内容。返回 (成功?, 错误信息)。
    若未配置 KOS_CORE_PATH，返回 (True, "") 表示跳过校验。
    """
    path = get_kos_core_path()
    if not path or not Path(path).exists():
        return True, ""
    try:
        with tempfile.NamedTemporaryFile(mode="w", suffix=".kos", delete=False, encoding="utf-8") as f:
            f.write(kos_content)
            f.flush()
            name = f.name
        try:
            result = subprocess.run(
                [path, "check-file", name],
                capture_output=True,
                text=True,
                timeout=10,
                cwd=os.path.dirname(path) or None,
            )
            if result.returncode != 0:
                return False, result.stderr or result.stdout or "check-file failed"
            return True, ""
        finally:
            try:
                os.unlink(name)
            except OSError:
                pass
    except subprocess.TimeoutExpired:
        return False, "kos-core timeout"
    except Exception as e:
        return False, str(e)


def kos_core_check_term(term_expr: str, type_expr: str) -> tuple[bool, str]:
    """
    调用 kos-core check-term 校验项是否满足类型（宪法约束）。
    返回 (成功?, 错误信息)。未配置 KOS_CORE_PATH 时返回 (True, "") 表示跳过校验。
    """
    path = get_kos_core_path()
    if not path or not Path(path).exists():
        return True, ""
    try:
        result = subprocess.run(
            [path, "check-term", term_expr, type_expr],
            capture_output=True,
            text=True,
            timeout=5,
            cwd=os.path.dirname(path) or None,
        )
        if result.returncode == 0:
            return True, (result.stdout or "").strip()
        return False, (result.stderr or result.stdout or "check-term failed").strip()
    except subprocess.TimeoutExpired:
        return False, "kos-core timeout"
    except Exception as e:
        return False, str(e)


def kos_core_load_and_export(kos_content: str) -> dict[str, Any] | None:
    """
    若配置了 kos-core，可调用其加载并导出类型/谓词（若 kos-core 支持导出 JSON）。
    当前未实现则返回 None，由 core_loader 用本地解析。
    """
    return None
