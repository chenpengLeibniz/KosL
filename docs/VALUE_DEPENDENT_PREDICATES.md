# 值依赖谓词纳入类型系统

## 概述

值依赖谓词（Value-Dependent Predicates）`gt`、`ge`、`lt`、`le`、`eq` 已纳入 KOS 类型系统，作为 `Prop` 类型参与类型检查。**推荐通过 Haskell kos-core 形式化内核调用**（`kos_core_bridge`），在 kos-core 中求值并比较，成立则接受，否则拒绝。

## 类型构造子

| 谓词 | 含义 | 示例 |
|------|------|------|
| `gt(a, b)` | a > b | `gt(200, 180)` |
| `ge(a, b)` | a >= b | `ge(185, 180)` |
| `lt(a, b)` | a < b | `lt(100, 200)` |
| `le(a, b)` | a <= b | `le(100, 100)` |
| `eq(a, b)` | a == b | `eq(42, 42)` |

## C 接口

### 构造

```c
kos_term* kos_mk_gt(kos_term* left, kos_term* right);   // a > b : Prop
kos_term* kos_mk_ge(kos_term* left, kos_term* right);   // a >= b
kos_term* kos_mk_lt(kos_term* left, kos_term* right);   // a < b
kos_term* kos_mk_le(kos_term* left, kos_term* right);   // a <= b
kos_term* kos_mk_eq(kos_term* left, kos_term* right);   // a == b
```

### 类型检查

- `kos_type_wellformed(pred)`：当 `pred` 为 GT/GE/LT/LE/EQ 且 left/right 非空时返回 true。
- `kos_check(ctx, term, pred)`：当 `pred` 为值依赖谓词时，对 left/right 归约后提取数值并比较；成立则接受 term（任意证明项），否则拒绝。

### 数值来源

当前支持从 `KOS_VAL`、`KOS_TIME` 的 `val` 字符串解析为 `double`。若无法解析或比较失败，谓词检查返回 false。

## 典型用法

### Σ 类型中的值约束

```c
// Σ(x:Val). gt(x, "180") 表示“温度大于 180 的依赖类型”
kos_term* val_type = kos_mk_val("Val");
kos_term* threshold = kos_mk_val("180");
kos_term* body = kos_mk_gt(/* 需在 body 中绑定 x */, threshold);
kos_term* sigma_type = kos_mk_sigma(val_type, body);
```

### 直接作为命题检查

```c
kos_term* pred = kos_mk_gt(kos_mk_val("200"), kos_mk_val("180"));
kos_term* proof = kos_mk_prop("trivial");
bool ok = kos_check(NULL, proof, pred);  // ok == true
```

## 序列化 / 反序列化

JSON 格式与 kos-core 兼容：

```json
{"kind":"GT","left":{"kind":"VAL","val":"200"},"right":{"kind":"VAL","val":"180"}}
```

`kos_term_serialize` / `kos_term_deserialize` 支持 GT/GE/LT/LE/EQ 的完整往返。

## 演示

运行 `value_predicate_demo` 可查看（通过 kos-core 调用）：

- `kos_core_bridge_check_term`：谓词项良构
- `kos_core_bridge_check_expr`：term : type 检查（成立/不成立谓词）
- `kos_core_bridge_term_from_kos` + 序列化/反序列化往返

```bash
# 从 KosL 根目录运行；或设置 KOS_CORE_PATH 指向 kos-core.exe
./build/bin/Debug/value_predicate_demo   # Windows
./bin/value_predicate_demo               # Unix
```

## 与 kos-core 的对应

Haskell `kos-core` 中已有对应的 `Gt`、`Ge`、`Lt`、`Le`、`Eq` 项构造子及类型检查逻辑，C 实现与其语义一致。
