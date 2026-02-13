/**
 * @file kos_knowledge_base.h
 * @brief KOS Kernel 层知识库 (Knowledge Base)
 *
 * 知识库是 Core 层验证过的依赖项集合，对应 Kos.pdf 2.2.2 中的：
 *   K = { (id_i, t_i, A_i) | Γ_Core ⊢ t_i : A_i }
 *
 * 来源：
 * 1. 系统启动时从关联的传统数据库或其他数据源抽取
 * 2. 系统运行过程中，通过外部事件处理最终物化形成的新项（事件、根因等）
 *
 * 知识库形成体系，支持基于依赖项的可视化。
 */

#ifndef KOS_KNOWLEDGE_BASE_H
#define KOS_KNOWLEDGE_BASE_H

#include "kos_core.h"
#include <stddef.h>
#include <stdbool.h>

/* ========== 知识项 (Knowledge Unit) ==========
 * 对应 Kos.pdf 中的 (id_i, t_i, A_i)，其中 Γ_Core ⊢ t_i : A_i */

typedef enum {
    KOS_KB_SOURCE_BOOTSTRAP,   /* 启动时从 DB/文件抽取 */
    KOS_KB_SOURCE_MATERIALIZED /* 运行时物化（事件、根因等） */
} kos_kb_source_t;

typedef struct kos_kb_item {
    char* id;           /* 唯一标识 id_i */
    kos_term* term;     /* 项 t_i */
    kos_term* type;     /* 类型 A_i，满足 t_i : A_i */
    int logical_ts;     /* 物化时的逻辑时钟 */
    kos_kb_source_t source;
    struct kos_kb_item* next;  /* 链表链接 */
} kos_kb_item_t;

/* ========== 依赖关系类型 ========== */

typedef enum {
    KOS_KB_DEP_TYPE_REF,      /* 类型/项引用（term 中引用其他项） */
    KOS_KB_DEP_TYPE_SUBTERM,  /* 子项依赖（term 包含子项对应其他项） */
    KOS_KB_DEP_TYPE_CAUSAL,   /* 因果依赖（如事件→根因） */
    KOS_KB_DEP_TYPE_EXPLICIT  /* 显式添加的依赖 */
} kos_kb_dep_type_t;

/* ========== 依赖边 (Dependency Edge) ==========
 * 项之间的关系通过依赖实现，支持可视化 */

typedef struct kos_kb_dep_edge {
    char* from_id;      /* 依赖源 id（from 依赖 to） */
    char* to_id;        /* 被依赖目标 id */
    kos_kb_dep_type_t dep_type;
    struct kos_kb_dep_edge* next;
} kos_kb_dep_edge_t;

/* ========== 知识库主体 ========== */

typedef struct kos_knowledge_base {
    kos_kb_item_t* items;      /* 知识项链表 */
    kos_kb_dep_edge_t* edges;  /* 依赖边链表 */
    size_t item_count;
    size_t edge_count;
} kos_knowledge_base_t;

/* ========== 知识库 API ========== */

/** 创建空知识库 */
kos_knowledge_base_t* kos_kb_create(void);

/** 释放知识库及所有项 */
void kos_kb_free(kos_knowledge_base_t* kb);

/** 添加知识项（需已通过 Core 层验证 t : A）
 * @param kb 知识库
 * @param id 唯一标识
 * @param t 项（深拷贝）
 * @param A 类型（深拷贝）
 * @param logical_ts 逻辑时钟
 * @param source 来源（启动抽取 / 运行时物化）
 * @return 0 成功，-1 失败 */
int kos_kb_add_item(kos_knowledge_base_t* kb, const char* id,
                    kos_term* t, kos_term* A, int logical_ts,
                    kos_kb_source_t source);

/** 添加依赖边（用于可视化）
 * @param kb 知识库
 * @param from_id 依赖源（from 依赖 to）
 * @param to_id 被依赖目标
 * @return 0 成功，-1 失败 */
int kos_kb_add_dependency(kos_knowledge_base_t* kb,
                          const char* from_id, const char* to_id);

/** 添加带类型的依赖边 */
int kos_kb_add_dependency_typed(kos_knowledge_base_t* kb,
                               const char* from_id, const char* to_id,
                               kos_kb_dep_type_t dep_type);

/** 从项结构推断依赖：遍历 term 和 type，将引用其他 kb 项 id 的建立依赖边
 * @return 新增的依赖边数量 */
int kos_kb_infer_dependencies(kos_knowledge_base_t* kb);

/** 从知识库查找项 */
kos_kb_item_t* kos_kb_find(kos_knowledge_base_t* kb, const char* id);

/** 将知识库导出为依赖图 JSON（用于可视化）
 * 格式：{ nodes: [{id, label, source, ts, kind}], edges: [{from, to, relation}] }
 * 调用者负责 free 返回的字符串 */
char* kos_kb_export_dependency_graph_json(kos_knowledge_base_t* kb);

/** 导出为 standalone HTML（含 D3.js 力导向图）
 * 调用者负责 free 返回的字符串 */
char* kos_kb_export_visualization_html(kos_knowledge_base_t* kb);

/** 导出反事实推理演示 HTML（两个场景的 combined 可视化页面）
 * @param kb1 场景1 知识库（单一根因）
 * @param kb2 场景2 知识库（多因可选）
 * @param cf_result1 场景1 反事实结论（可为 NULL 使用默认）
 * @param cf_result2 场景2 反事实结论（可为 NULL 使用默认）
 * 调用者负责 free 返回的字符串 */
char* kos_export_counterfactual_demo_html(kos_knowledge_base_t* kb1,
                                          kos_knowledge_base_t* kb2,
                                          const char* cf_result1,
                                          const char* cf_result2);

/** 将知识库转换为 Σ 链形式的 kos_term（与现有 K 兼容） */
kos_term* kos_kb_to_sigma_chain(kos_knowledge_base_t* kb);

/** 从 Σ 链形式的 kos_term 合并到知识库（用于状态同步） */
int kos_kb_merge_from_sigma_chain(kos_knowledge_base_t* kb,
                                  kos_term* chain, int logical_ts,
                                  kos_kb_source_t source);

#endif /* KOS_KNOWLEDGE_BASE_H */
