# KOS Core Layer API 参考

本文档列出 KOS Core 层（L0）的所有公开 API 接口。Core 层基于直觉主义依赖类型理论（ITT），提供静态类型检查和知识构造的基础设施。

## 目录

1. [核心数据结构](#核心数据结构)
2. [类型检查接口](#类型检查接口)
3. [Universe 层级系统](#universe-层级系统)
4. [归约操作](#归约操作)
5. [类型构建器](#类型构建器)
6. [内存管理](#内存管理)
7. [存储和加载](#存储和加载)

---

## 核心数据结构

### `kos_term`
Core 层的核心数据结构，表示类型系统中的项（term）或类型（type）。

```c
typedef struct kos_term {
    term_kind kind;              // 项的种类
    universe_info universe;      // Universe层级信息
    union {
        // atomic: VAL, TIME, ID, PROP
        struct { char* val; kos_term* type; } atomic;
        
        // pair: <d, p> - 带证数据对
        struct { kos_term* data; kos_term* proof; } pair;
        
        // sigma: Σ(x:A).B - 依赖和类型
        struct { kos_term* domain; kos_term* body; } sigma;
        
        // pi: Π(x:A).B - 依赖积类型
        struct {
            kos_term* domain;
            kos_term* body;
            kos_term* body_term;  // λ抽象体（可选）
        } pi;
        
        // sum: A + B - 和类型
        struct {
            kos_term* left_type;
            kos_term* right_type;
            bool is_left;
            kos_term* value;
        } sum;
        
        // universe: U_i 或 Type_i
        struct {
            universe_axis axis;
            int level;
        } universe;
    } data;
} kos_term;
```

### `term_kind`
项的种类枚举：

- `KOS_VAL` - 值类型（Base Sort）
- `KOS_TIME` - 时间类型（Base Sort）
- `KOS_ID` - 标识符类型（Base Sort）
- `KOS_SIGMA` - Σ-Types（依赖和类型）
- `KOS_PAIR` - 普通对类型 `<d, p>`
- `KOS_PROP` - 命题类型（`Prop : Type_1`）
- `KOS_PI` - Π-Types（依赖积类型）
- `KOS_SUM` - Sum Types（联合类型 `A + B`）
- `KOS_U` - 计算轴 Universe `U_i`
- `KOS_TYPE` - 逻辑轴 Universe `Type_i`

### `universe_info`
Universe 层级信息：

```c
typedef struct {
    universe_axis axis;  // UNIVERSE_COMPUTATIONAL 或 UNIVERSE_LOGICAL
    int level;           // 层级 i
} universe_info;
```

---

## 类型检查接口

### `kos_check`
双向类型检查：验证 `term` 是否具有类型 `type`。

```c
bool kos_check(kos_term* ctx, kos_term* term, kos_term* type);
```

**参数**：
- `ctx` - 类型检查上下文（可为 `NULL`，表示空上下文）
- `term` - 要检查的项
- `type` - 期望的类型

**返回值**：
- `true` - 类型检查通过
- `false` - 类型检查失败

**说明**：
- 函数内部会先对 `term` 和 `type` 执行 β-归约，然后在归约后的形式上进行检查
- 支持 `Prop`、`Π`、`Σ`、`Sum`、`Universe` 等类型的检查
- 对于 `Σ(x:A).B` 类型，会检查 `d : A` 和 `p : B[d/x]`（依赖类型检查）

### `kos_type_check`
验证 `proof` 是否为命题 `prop` 的有效证明。

```c
bool kos_type_check(kos_term* ctx, kos_term* proof, kos_term* prop);
```

**参数**：
- `ctx` - 类型检查上下文
- `proof` - 证明项
- `prop` - 命题类型

**返回值**：
- `true` - 证明有效
- `false` - 证明无效

**说明**：
- 这是 `kos_check` 的语义化封装，专门用于验证证明项

---

## Universe 层级系统

### `kos_get_universe_info`
获取类型的 Universe 信息。

```c
universe_info kos_get_universe_info(kos_term* type);
```

**参数**：
- `type` - 类型项

**返回值**：
- `universe_info` - Universe 信息（轴和层级）

**说明**：
- 根据类型的 `kind` 推断其 Universe 信息
- `KOS_PROP` → `Type_1`（逻辑轴，层级1）
- `KOS_VAL`/`KOS_TIME`/`KOS_ID` → `U_0`（计算轴，层级0）
- `KOS_PI`/`KOS_SIGMA`/`KOS_SUM` → 从类型结构中获取

### `kos_universe_leq`
检查 Universe 层级关系：`u1 ≤ u2`。

```c
bool kos_universe_leq(universe_info u1, universe_info u2);
```

**返回值**：
- `true` - `u1` 可以提升到 `u2` 的层级
- `false` - 不可提升

**规则**：
- 相同轴：`U_i ≤ U_j` 当且仅当 `i ≤ j`
- 跨轴提升：`U_i : Type_{i+1}`（计算轴可提升到逻辑轴）
- Prop 嵌入：`Prop : U_1`（命题可嵌入到数据轴）

### `kos_universe_lift_to_logic`
将计算轴类型提升到逻辑轴。

```c
kos_term* kos_universe_lift_to_logic(kos_term* type);
```

**规则**：`U_i : Type_{i+1}`

### `kos_prop_embed_to_data`
将逻辑轴的命题嵌入到计算轴。

```c
kos_term* kos_prop_embed_to_data(kos_term* prop);
```

**规则**：`Prop ↪ U_1`

---

## 归约操作

### `kos_reduce`
对项进行归约操作（β-reduction 和 ι-reduction）。

```c
kos_term* kos_reduce(kos_term* t);
```

**参数**：
- `t` - 要归约的项

**返回值**：
- 归约后的项（可能修改原项，也可能返回新项）

**说明**：
- 实现 β-reduction：`(λx:A.t) u → t[u/x]`
- 当前使用 `KOS_PAIR(func, arg)` 表示函数应用
- 递归归约 `Π`、`Σ`、`Sum` 的子项
- ι-reduction（`split`、`case`）当前为简化实现

---

## 类型构建器

### 基础类型构造

#### `kos_mk_atomic`
创建原子值类型。

```c
kos_term* kos_mk_atomic(const char* val, kos_term* type);
```

#### `kos_mk_prop`
创建命题类型。

```c
kos_term* kos_mk_prop(const char* prop_name);
```

#### `kos_mk_val`
创建值类型。

```c
kos_term* kos_mk_val(const char* val);
```

#### `kos_mk_time`
创建时间类型。

```c
kos_term* kos_mk_time(const char* time_val);
```

#### `kos_mk_id`
创建标识符类型。

```c
kos_term* kos_mk_id(const char* id_val);
```

### Universe 类型构造

#### `kos_mk_universe_computational`
创建计算轴 Universe 类型 `U_i`。

```c
kos_term* kos_mk_universe_computational(int level);
```

#### `kos_mk_universe_logical`
创建逻辑轴 Universe 类型 `Type_i`。

```c
kos_term* kos_mk_universe_logical(int level);
```

### 依赖类型构造

#### `kos_mk_pair`
创建对类型 `<d, p>`（Σ-Type 的核心）。

```c
kos_term* kos_mk_pair(kos_term* data, kos_term* proof);
```

#### `kos_mk_sigma`
创建依赖和类型 `Σ(x:A).B`。

```c
kos_term* kos_mk_sigma(kos_term* domain, kos_term* body);
```

**说明**：
- `domain` - 域类型 `A`
- `body` - 体类型 `B`（依赖 `x:A`）

#### `kos_mk_pi`
创建依赖积类型 `Π(x:A).B`。

```c
kos_term* kos_mk_pi(kos_term* domain, kos_term* body);
```

**说明**：
- `domain` - 域类型 `A`
- `body` - 体类型 `B`（依赖 `x:A`）

#### `kos_mk_lambda`
创建 λ 抽象（Π 类型引入）。

```c
kos_term* kos_mk_lambda(kos_term* domain, kos_term* body_term);
```

**说明**：
- 创建 `λx:domain. body_term : Π(x:domain). body_type`
- 当前约定绑定变量名为 `"x"`

#### `kos_mk_app`
创建函数应用（Π 类型消除）。

```c
kos_term* kos_mk_app(kos_term* func, kos_term* arg);
```

**说明**：
- 当前实现可能使用 `KOS_PAIR(func, arg)` 表示应用

### 和类型构造

#### `kos_mk_sum`
创建和类型 `A + B`。

```c
kos_term* kos_mk_sum(kos_term* left_type, kos_term* right_type);
```

#### `kos_mk_inl`
创建左注入 `inl`。

```c
kos_term* kos_mk_inl(kos_term* left_type, kos_term* right_type, kos_term* value);
```

#### `kos_mk_inr`
创建右注入 `inr`。

```c
kos_term* kos_mk_inr(kos_term* left_type, kos_term* right_type, kos_term* value);
```

#### `kos_mk_case`
创建 case 分析（和类型消除）。

```c
kos_term* kos_mk_case(kos_term* sum_term, kos_term* left_branch, kos_term* right_branch);
```

#### `kos_mk_split`
创建 split 操作（Σ 类型消除）。

```c
kos_term* kos_mk_split(kos_term* pair_term, kos_term* body_term);
```

---

## 内存管理

### `kos_term_copy`
复制 term（深拷贝）。

```c
kos_term* kos_term_copy(kos_term* t);
```

**返回值**：
- 新分配的 `kos_term*`，所有子项都被递归复制

### `kos_term_free`
释放 term（递归释放所有子项）。

```c
void kos_term_free(kos_term* t);
```

**说明**：
- 递归释放所有子项和内部字符串
- 调用后不应再使用 `t` 及其子项

---

## 存储和加载

### `kos_serialized`
序列化数据结构：

```c
typedef struct {
    char* data;      // JSON 字符串
    size_t length;   // 数据长度
} kos_serialized;
```

### `kos_term_serialize`
序列化 term 为 JSON 格式。

```c
kos_serialized* kos_term_serialize(kos_term* t);
```

**返回值**：
- `kos_serialized*` - 序列化后的 JSON 数据

### `kos_term_deserialize`
从 JSON 格式反序列化 term。

```c
kos_term* kos_term_deserialize(const char* json_str);
```

**返回值**：
- `kos_term*` - 反序列化后的项

### `kos_serialized_free`
释放序列化数据。

```c
void kos_serialized_free(kos_serialized* s);
```

### `kos_term_save_to_file`
存储 term 到文件。

```c
int kos_term_save_to_file(kos_term* t, const char* filename);
```

**返回值**：
- `0` - 成功
- `-1` - 失败

### `kos_term_load_from_file`
从文件加载 term。

```c
kos_term* kos_term_load_from_file(const char* filename);
```

**返回值**：
- `kos_term*` - 加载的项，失败返回 `NULL`

### `kos_knowledge_save`
存储知识集 `K` 到文件。

```c
int kos_knowledge_save(kos_term* K, const char* filename);
```

### `kos_knowledge_load`
从文件加载知识集 `K`。

```c
kos_term* kos_knowledge_load(const char* filename);
```

---

## 使用示例

### 示例 1：构造 Σ 类型并检查

```c
// 构造 FailEvt : Σ(x:ID). Σ(e:ID). Σ(t:TIME). Prop
kos_term* id_type = kos_mk_id("ID");
kos_term* time_type = kos_mk_time("Time");
kos_term* prop_type = kos_mk_prop("FailEvt");

kos_term* sigma_t_prop = kos_mk_sigma(time_type, prop_type);
kos_term* sigma_e_sigma = kos_mk_sigma(id_type, sigma_t_prop);
kos_term* fail_evt_type = kos_mk_sigma(id_type, sigma_e_sigma);

// 构造实例
kos_term* batch_id = kos_mk_id("BATCH1");
kos_term* err_id = kos_mk_id("ERR01");
kos_term* time_val = kos_mk_time("2025-01-01T00:00:00Z");
kos_term* proof_val = kos_mk_prop("FailEvtProof");

kos_term* pair_t_proof = kos_mk_pair(time_val, proof_val);
kos_term* pair_e_rest = kos_mk_pair(err_id, pair_t_proof);
kos_term* fail_evt_value = kos_mk_pair(batch_id, pair_e_rest);

// 类型检查
bool ok = kos_check(NULL, fail_evt_value, fail_evt_type);

// 清理
kos_term_free(fail_evt_value);
kos_term_free(fail_evt_type);
```

### 示例 2：构造 Π 类型并检查

```c
// 构造 F = Π(x:ID). Prop
kos_term* id_type = kos_mk_id("ID");
kos_term* prop_body = kos_mk_prop("P");
kos_term* pi_type = kos_mk_pi(id_type, prop_body);

// 构造 λ 抽象
kos_term* proof_term = kos_mk_prop("ProofOfP");
kos_term* lambda_term = kos_mk_lambda(id_type, proof_term);

// 类型检查
bool ok = kos_check(NULL, lambda_term, pi_type);

// 清理
kos_term_free(lambda_term);
kos_term_free(pi_type);
```

---

## 注意事项

1. **内存管理**：
   - 所有 `kos_mk_*` 函数返回的 `kos_term*` 都需要用 `kos_term_free` 释放
   - `kos_term_copy` 返回的项也需要释放
   - 注意避免 double-free（例如，如果 `term` 是另一个 `term` 的子项）

2. **上下文约定**：
   - 当前上下文 `ctx` 可以用 `KOS_PAIR` 链表示：`<x_n, A_n>`，其中 `A_n` 可以嵌套下一层环境
   - 查找变量时使用线性扫描

3. **变量绑定**：
   - 当前实现中，λ 抽象和替换操作约定使用 `"x"` 作为绑定变量名
   - 未来可能需要支持显式的变量名绑定

4. **归约策略**：
   - `kos_reduce` 使用弱头归约（WHNF）策略
   - 函数应用使用 `KOS_PAIR(func, arg)` 表示

5. **Universe 规则**：
   - 双轴系统：计算轴 `U_i` 和逻辑轴 `Type_i`
   - `Prop : Type_1`、`U_i : Type_{i+1}`、`Prop ↪ U_1`

---

## 相关文档

- `TYPE_CHECKING_SYSTEM.md` - 类型检查系统详细说明
- `Kos.pdf` - KOS-TL 形式化规范
- `examples/core_sigma_example.c` - Σ 类型示例
- `examples/core_pi_example.c` - Π 类型示例
