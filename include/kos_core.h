// include/kos_core.h
// L0: Core Layer - 静态真理层
// 定义"什么是合法的" - 基于直觉主义受限类型理论（ITT）

#ifndef KOS_CORE_H
#define KOS_CORE_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// ========== 双轴世界系统 (Dual-Axis Universe System) ==========
// 根据Kos.tex文档定义的双轴世界结构

// Universe轴类型
typedef enum {
    UNIVERSE_COMPUTATIONAL,  // 计算轴 U_i (Computational Axis)
    UNIVERSE_LOGICAL         // 逻辑轴 Type_i (Logical Axis)
} universe_axis;

// Universe层级信息
typedef struct {
    universe_axis axis;      // 轴类型：U_i 或 Type_i
    int level;               // 层级：i (对于U_i或Type_i)
} universe_info;

// 前向声明
typedef enum term_kind {
    KOS_VAL,      // 值类型 (Base Sort)
    KOS_TIME,     // 时间类型 (Base Sort)
    KOS_ID,       // 标识符类型 (Base Sort)
    KOS_SIGMA,    // Σ-Types: 依赖和类型 (Dependent Sum Type)
    KOS_PAIR,     // 普通对类型 <d, p>
    KOS_PROP,     // 命题类型 (Prop : Type_1)
    KOS_PI,       // Π-Types: 依赖积类型 (Dependent Product Type)
    KOS_SUM,      // Sum Types: A + B (联合类型)
    KOS_U,        // 计算轴 Universe U_i
    KOS_TYPE      // 逻辑轴 Universe Type_i
} term_kind;

// 基础项结构 - 支持完整的依赖类型系统和双轴世界
typedef struct kos_term {
    term_kind kind;
    universe_info universe;  // Universe层级信息（用于类型检查）
    union {
        struct { 
            char* val; 
            struct kos_term* type; 
        } atomic; // 基础数据（用于VAL, TIME, ID, PROP）
        
        struct { 
            struct kos_term* data;   // 数据部分
            struct kos_term* proof;   // 证明部分 - Σ-Types核心
        } pair; // <d, p> - 带证数据对
        
        struct { 
            struct kos_term* domain;  // 域类型
            struct kos_term* body;    // 体类型（依赖域）
        } sigma; // Σ(x:A).B - 依赖和类型
        
        struct {
            struct kos_term* domain;  // 域类型 A
            struct kos_term* body;    // 体类型 B（依赖域）
            struct kos_term* body_term; // λ抽象体（可选，用于构造）
        } pi; // Π(x:A).B - 依赖积类型
        
        struct {
            struct kos_term* left_type;  // 左类型 A
            struct kos_term* right_type; // 右类型 B
            bool is_left;                // true = inl, false = inr
            struct kos_term* value;      // 值（A或B类型的项）
        } sum; // A + B - 和类型
        
        struct {
            universe_axis axis;  // UNIVERSE_COMPUTATIONAL (U_i) 或 UNIVERSE_LOGICAL (Type_i)
            int level;           // 层级 i
        } universe; // Universe类型（U_i 或 Type_i）
    } data;
} kos_term;

// ========== L0 核心接口 ==========
// 双向类型检查：验证 proof 是否为命题 prop 的有效证明
bool kos_check(kos_term* ctx, kos_term* term, kos_term* type);
bool kos_type_check(kos_term* ctx, kos_term* proof, kos_term* prop);

// ========== Universe层级系统接口 ==========
// 获取类型的Universe信息
universe_info kos_get_universe_info(kos_term* type);

// 检查Universe层级关系
// 返回true如果level1在level2之前（level1 < level2 或 level1可提升到level2）
bool kos_universe_leq(universe_info u1, universe_info u2);

// 应用Universe Lifting规则
// U_i : Type_{i+1} (计算轴可提升到逻辑轴)
kos_term* kos_universe_lift_to_logic(kos_term* type);

// 应用Proposition Embedding规则
// Prop ↪ U_1 (命题可嵌入到数据轴)
kos_term* kos_prop_embed_to_data(kos_term* prop);

// 归约操作
kos_term* kos_reduce(kos_term* t);

// ========== Core 层工具：类型构建、存储和加载 ==========

// --- 类型构建器 (Type Builder) ---
// 创建原子值类型
kos_term* kos_mk_atomic(const char* val, kos_term* type);

// 创建命题类型
kos_term* kos_mk_prop(const char* prop_name);

// 创建值类型
kos_term* kos_mk_val(const char* val);

// 创建时间类型
kos_term* kos_mk_time(const char* time_val);

// 创建标识符类型
kos_term* kos_mk_id(const char* id_val);

// 创建计算轴Universe类型 U_i
kos_term* kos_mk_universe_computational(int level);

// 创建逻辑轴Universe类型 Type_i
kos_term* kos_mk_universe_logical(int level);

// 创建对类型 <d, p> (Σ-Type的核心)
kos_term* kos_mk_pair(kos_term* data, kos_term* proof);

// 创建依赖类型 Σ(x:A).B
kos_term* kos_mk_sigma(kos_term* domain, kos_term* body);

// 创建依赖积类型 Π(x:A).B
kos_term* kos_mk_pi(kos_term* domain, kos_term* body);

// 创建λ抽象（Π类型引入）
kos_term* kos_mk_lambda(kos_term* domain, kos_term* body_term);

// 创建函数应用（Π类型消除）
kos_term* kos_mk_app(kos_term* func, kos_term* arg);

// 创建和类型 A + B
kos_term* kos_mk_sum(kos_term* left_type, kos_term* right_type);

// 创建inl (左注入)
kos_term* kos_mk_inl(kos_term* left_type, kos_term* right_type, kos_term* value);

// 创建inr (右注入)
kos_term* kos_mk_inr(kos_term* left_type, kos_term* right_type, kos_term* value);

// 创建case分析（和类型消除）
kos_term* kos_mk_case(kos_term* sum_term, kos_term* left_branch, kos_term* right_branch);

// 创建split操作（Σ类型消除）
kos_term* kos_mk_split(kos_term* pair_term, kos_term* body_term);

// 复制 term（深拷贝）
kos_term* kos_term_copy(kos_term* t);

// 释放 term（递归释放所有子项）
void kos_term_free(kos_term* t);

// --- 存储和加载 (Storage & Loading) ---
// 序列化格式：JSON（便于人类阅读和调试）
typedef struct {
    char* data;      // JSON 字符串
    size_t length;   // 数据长度
} kos_serialized;

// 序列化 term 为 JSON 格式
kos_serialized* kos_term_serialize(kos_term* t);

// 从 JSON 格式反序列化 term
kos_term* kos_term_deserialize(const char* json_str);

// 释放序列化数据
void kos_serialized_free(kos_serialized* s);

// 存储 term 到文件
int kos_term_save_to_file(kos_term* t, const char* filename);

// 从文件加载 term
kos_term* kos_term_load_from_file(const char* filename);

// 存储知识集 K 到文件
int kos_knowledge_save(kos_term* K, const char* filename);

// 从文件加载知识集 K
kos_term* kos_knowledge_load(const char* filename);

#endif





