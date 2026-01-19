// include/kos_ontology.h
// 类型本体管理系统 - 基于直觉类型论的类型构造系统
// 所有类型定义都通过类型构造器（Π、Σ、Sum等）构造，而不是C语言结构体

#ifndef KOS_ONTOLOGY_H
#define KOS_ONTOLOGY_H

#include "kos_core.h"
#include <stdbool.h>
#include <stddef.h>

// ========== 类型本体定义（基于类型构造） ==========

// 类型定义条目：将名称映射到类型构造
// 类型定义本身是 kos_term* 类型，通过类型构造器（Π、Σ、Sum等）构造
typedef struct {
    char* name;              // 类型名称（如 "BatchID", "FailEvt"）
    kos_term* type_def;      // 类型定义（kos_term*），通过类型构造器构造
    kos_term* ctx;           // 类型定义的上下文（可选，用于依赖类型）
} TypeDefinition;

// 类型本体（完整的类型系统定义）
// 存储类型定义的集合，所有类型都是通过类型构造器构造的 kos_term*
typedef struct {
    char domain_name[64];              // 领域名称，如 "manufacturing"
    TypeDefinition* type_definitions;  // 类型定义数组
    size_t type_count;                 // 类型定义数量
    size_t capacity;                   // 数组容量
} TypeOntology;

// ========== 类型本体管理接口 ==========

// 创建空的本体
TypeOntology* kos_ontology_create(const char* domain_name);

// 释放本体（递归释放所有资源）
void kos_ontology_free(TypeOntology* ontology);

// ========== 类型定义 CRUD（基于类型构造） ==========

// 添加类型定义
// type_def 必须是通过类型构造器（kos_mk_pi, kos_mk_sigma, kos_mk_sum等）构造的 kos_term*
// 实例化时必须通过类型检查验证是否符合类型构造条件
int kos_ontology_add_type_definition(TypeOntology* ontology,
                                     const char* name,
                                     kos_term* type_def,
                                     kos_term* ctx);

// 查找类型定义（返回类型定义的 kos_term*）
kos_term* kos_ontology_find_type_definition(TypeOntology* ontology, const char* name);

// 获取类型定义的完整信息
TypeDefinition* kos_ontology_get_type_definition_info(TypeOntology* ontology, const char* name);

// 删除类型定义
int kos_ontology_remove_type_definition(TypeOntology* ontology, const char* name);

// 更新类型定义
int kos_ontology_update_type_definition(TypeOntology* ontology,
                                        const char* name,
                                        kos_term* new_type_def,
                                        kos_term* new_ctx);

// ========== 类型实例化（基于类型检查） ==========

// 根据类型定义构造类型实例
// 实例化过程：
// 1. 查找类型定义（type_def : kos_term*）
// 2. 通过类型检查器验证 data_term 是否符合类型构造条件
// 3. 如果通过类型检查，返回构造的实例；否则返回 NULL
// data_term: 要实例化的数据项（必须是 kos_term* 类型）
// ctx: 类型检查的上下文
kos_term* kos_ontology_mk_type_instance(TypeOntology* ontology,
                                        const char* type_name,
                                        kos_term* data_term,
                                        kos_term* ctx);

// 验证实例是否符合类型定义（通过类型检查）
// 使用 kos_check 或 kos_type_check 验证 instance 是否符合 type_def
bool kos_ontology_validate_instance(TypeOntology* ontology,
                                    kos_term* instance,
                                    const char* type_name,
                                    kos_term* ctx);

// ========== 持久化存储 ==========

// 保存本体到文件（JSON格式）
// 类型定义（kos_term*）会被序列化为 JSON
int kos_ontology_save_to_file(TypeOntology* ontology, const char* filename);

// 从文件加载本体
// JSON 会被反序列化为 kos_term* 类型定义
TypeOntology* kos_ontology_load_from_file(const char* filename);

// 序列化本体为JSON字符串
char* kos_ontology_serialize(TypeOntology* ontology);

// 从JSON字符串反序列化本体
TypeOntology* kos_ontology_deserialize(const char* json_str);

// ========== 类型构造辅助函数 ==========

// 构造依赖和类型（Σ类型）：用于事件类型等
// 例如：FailEvt ≡ Σ(b: BatchID). Σ(err: ErrorCode). Σ(t: Time). Prop
kos_term* kos_ontology_mk_sigma_type(kos_term* domain, kos_term* body);

// 构造依赖积类型（Π类型）：用于谓词类型等
// 例如：InRoute ≡ Π(b: BatchID). Π(m: Machine). Prop
kos_term* kos_ontology_mk_pi_type(kos_term* domain, kos_term* body);

// 构造和类型（Sum类型）：用于联合类型
// 例如：Result ≡ Success + Failure
kos_term* kos_ontology_mk_sum_type(kos_term* left_type, kos_term* right_type);

#endif
































