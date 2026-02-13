# .kos 语言语法规范

本文档描述 KosCore 的 .kos 源文件语法，覆盖所有支持的语言特性。

---

## 一、词法

### 1.1 注释

```
-- 行注释
/* 块注释 */
```

### 1.2 标识符

- **标识符**：以字母开头，后跟字母、数字、下划线或撇号
- **保留字**（不可作标识符）：`let`, `in`, `module`, `where`, `type`, `def`, `lam`, `Pi`, `Sigma`, `case`, `of`, `Prop`, `U0`, `U1`, `U`, `Type0`, `Type1`, `Type`, `val`, `time`, `id`, `inl`, `inr`, `Id`, `IdA`, `refl`, `split`, `as`, `gt`, `ge`, `lt`, `le`, `eq`

### 1.3 字符串字面量

- 双引号包围：`"hello"`, `"batch-001"`

---

## 二、模块结构

```
module ModuleName where

  type Name : Kind
  def name : Type := term
```

- **module** 模块名 **where**：模块头
- **type** 名称 **:** 类型：类型别名声明
- **def** 名称 **:** 类型 **:=** 项：项定义（支持 δ 归约）

---

## 三、类型与项语法

### 3.1 Universe（宇宙）

| 语法 | 含义 | Kind |
|------|------|------|
| `U0` | 计算轴 0 级 | U1 |
| `U1` | 计算轴 1 级 | U2 |
| `U` | U0 同义 | U1 |
| `Type0` | 逻辑轴 0 级 | Type1 |
| `Type1` | 逻辑轴 1 级 | Type2 |
| `Type` | Type1 同义 | Type2 |

### 3.2 命题与 Base Sorts

| 语法 | 含义 | 类型 |
|------|------|------|
| `Prop P` | 命题 P | Type1 |
| `val "x"` | 值字面量 | U0 |
| `time "2025-01-01"` | 时间字面量 | U0 |
| `id "BATCH1"` | 标识符字面量 | U0 |

### 3.3 Π 类型（依赖函数）

| 语法 | 含义 |
|------|------|
| `Pi(x:A). B` | Π(x:A).B，依赖函数类型 |
| `Π(x:A). B` | 同上（Unicode） |
| `A -> B` | 非依赖简写：Π(_:A).B |
| `lam (x : A) . t` | λ(x:A).t，λ 抽象 |
| `f a` | 函数应用 |

**优先级**：`->` 右结合；应用左结合。

### 3.4 Σ 类型（依赖对）

| 语法 | 含义 |
|------|------|
| `Sigma(x:A). B` | Σ(x:A).B，依赖对类型 |
| `Σ(x:A). B` | 同上（Unicode） |
| `<d, p>` | 对构造，d 为数据，p 为证明 |
| `split (p) as x y in body` | Σ 消除：将 p 分解为 x, y，在 body 中使用（p 必须用括号包围） |

### 3.5 Sum 类型（不交并）

| 语法 | 含义 |
|------|------|
| `A + B` | 不交并类型，左结合 |
| `inl(A, B, v)` | 左注入，显式类型 |
| `inr(A, B, v)` | 右注入，显式类型 |
| `inl v` | 左注入，单参数（需 check 模式，期望 Sum A B） |
| `inr v` | 右注入，单参数 |
| `case s of inl x -> t1; inr y -> t2` | Sum 消除 |

### 3.6 Id 类型（恒等/等价）

| 语法 | 含义 |
|------|------|
| `Id(A, a, b)` | Id_A(a,b)，a 与 b 在类型 A 下的等价 |
| `IdA(A, a, b)` | 同上 |
| `refl w` | refl_w : Id_A(w,w)，自反证明 |
| `refl (w)` | 同上 |

### 3.7 Let 绑定

```
let x : A := v in body
```

- **let** 变量 **:** 类型 **:=** 值 **in** 体
- ζ 归约：`let x := v in t` ≡ `t[v/x]`

### 3.8 值依赖谓词（Value-Dependent Predicates）

| 语法 | 含义 | 类型 |
|------|------|------|
| `gt(a, b)` | a > b（数值） | Prop，成立时可用 `triv` 证明 |
| `ge(a, b)` | a ≥ b | 同上 |
| `lt(a, b)` | a < b | 同上 |
| `le(a, b)` | a ≤ b | 同上 |
| `eq(a, b)` | a = b（数值或字符串/时间/标识符） | 同上 |

类型检查时对左右操作数求值并比较；仅当谓词成立时，项（如 `triv`）才通过检查。

### 3.9 变量与括号

- `x`, `foo`, `x'`：变量引用
- `( term )`：括号提升优先级

---

## 四、语法优先级（从高到低）

1. 原子：Prop, Universe, val/time/id, 变量, 括号
2. 应用：`f a b`
3. Pair：`<a, b>`
4. inl/inr, Id, refl, split, case
5. Pi/Sigma：`Pi(x:A). B`
6. Lam：`lam (x:A) . t`
7. Let：`let x : A := v in t`
8. Arrow：`A -> B`（右结合）

---

## 五、完整 BNF 概览

```
Module     ::= "module" ident "where" Declaration*
Declaration::= "type" ident ":" Term
             | "def" ident ":" Term ":=" Term

Term       ::= Arrow
Arrow      ::= Lam ("->" Arrow)?
Lam        ::= "lam" "("? ident ":" Term ")"? "." Lam | Sum
Sum        ::= Pi ("+" Sum)?
Pi         ::= ("Pi"|"Π") "("? ident ":" Term ")"? "." Pi | Sigma
Sigma      ::= ("Sigma"|"Σ") "("? ident ":" Term ")"? "." Sigma | App
App        ::= Atomic+
```

注：`Sum` 左结合，`A + B + C` 解析为 `Sum (Sum A B) C`。

Atomic     ::= "Prop" ident
             | "U0" | "U1" | "U" | "Type0" | "Type1" | "Type"
             | "val" stringLit | "time" stringLit | "id" stringLit
             | "let" ident ":" Term ":=" Term "in" Term
             | ident
             | "(" Term ")"
             | "<" Term "," Term ">"
             | "inl" "(" Term "," Term "," Term ")" | "inr" "(" Term "," Term "," Term ")"
             | "inl" Atomic | "inr" Atomic
             | ("Id"|"IdA") "(" Term "," Term "," Term ")"
             | "refl" "("? Term ")"?
             | "split" "(" Term ")" "as" ident ident "in" Term
             | "case" Term "of" "inl" ident "->" Term ";" "inr" ident "->" Term
             | "gt" "(" Term "," Term ")" | "ge" "(" Term "," Term ")" | "lt" "(" Term "," Term ")"
             | "le" "(" Term "," Term ")" | "eq" "(" Term "," Term ")"
```

---

## 六、示例片段

```kos
-- Universe
U0
Type1

-- 命题
Prop P

-- Π 类型
Pi(x:U0). x -> x
lam (x : Type1) . x

-- Σ 类型
Sigma(x:U0). Prop P
< Prop P , Prop Q >

-- Sum
inl(Prop P, Prop Q, Prop P)
case s of inl x -> x; inr y -> y

-- Id
Id(Type1, Prop P, Prop P)
refl (Prop P)

-- Let
let x : Type1 := Prop P in x
```
