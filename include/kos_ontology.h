// include/kos_ontology.h
// 类型本体管理系统
// 提供类型本体的持久化存储、加载和内存管理

#ifndef KOS_ONTOLOGY_H
#define KOS_ONTOLOGY_H

#include "kos_core.h"
#include <stdbool.h>
#include <stddef.h>

// ========== 类型本体定义 ==========

// 原子类型定义
typedef struct {
    char name[64];           // 类型名称，如 "BatchID", "Machine"
    char base_type[32];      // 基础类型，如 "String", "UInt64", "Float"
    kos_term* constraints;   // 类型约束（可选）
} AtomicTypeDef;

// 谓词类型定义
typedef struct {
    char name[64];           // 谓词名称，如 "InRoute", "Overlap"
    char* param_types[16];   // 参数类型列表
    int param_count;         // 参数数量
    kos_term* body;          // 谓词体（逻辑表达式）
} PredicateTypeDef;

// 事件类型定义
typedef struct {
    char name[64];           // 事件类型名称，如 "FailEvt", "ProcStep"
    char* field_names[16];    // 字段名称列表
    char* field_types[16];    // 字段类型列表
    int field_count;         // 字段数量
    kos_term* preconditions; // 前置条件（证明要求）
} EventTypeDef;

// 类型本体（完整的类型系统定义）
typedef struct {
    char domain_name[64];              // 领域名称，如 "manufacturing"
    AtomicTypeDef* atomic_types;       // 原子类型数组
    size_t atomic_count;               // 原子类型数量
    PredicateTypeDef* predicate_types; // 谓词类型数组
    size_t predicate_count;            // 谓词类型数量
    EventTypeDef* event_types;         // 事件类型数组
    size_t event_count;                 // 事件类型数量
} TypeOntology;

// ========== 类型本体管理接口 ==========

// 创建空的本体
TypeOntology* kos_ontology_create(const char* domain_name);

// 释放本体（递归释放所有资源）
void kos_ontology_free(TypeOntology* ontology);

// ========== 原子类型 CRUD ==========

// 添加原子类型
int kos_ontology_add_atomic_type(TypeOntology* ontology, 
                                  const char* name, 
                                  const char* base_type,
                                  kos_term* constraints);

// 查找原子类型
AtomicTypeDef* kos_ontology_find_atomic_type(TypeOntology* ontology, const char* name);

// 删除原子类型
int kos_ontology_remove_atomic_type(TypeOntology* ontology, const char* name);

// 更新原子类型
int kos_ontology_update_atomic_type(TypeOntology* ontology,
                                    const char* name,
                                    const char* new_base_type,
                                    kos_term* new_constraints);

// ========== 谓词类型 CRUD ==========

// 添加谓词类型
int kos_ontology_add_predicate_type(TypeOntology* ontology,
                                    const char* name,
                                    const char** param_types,
                                    int param_count,
                                    kos_term* body);

// 查找谓词类型
PredicateTypeDef* kos_ontology_find_predicate_type(TypeOntology* ontology, const char* name);

// 删除谓词类型
int kos_ontology_remove_predicate_type(TypeOntology* ontology, const char* name);

// ========== 事件类型 CRUD ==========

// 添加事件类型
int kos_ontology_add_event_type(TypeOntology* ontology,
                                const char* name,
                                const char** field_names,
                                const char** field_types,
                                int field_count,
                                kos_term* preconditions);

// 查找事件类型
EventTypeDef* kos_ontology_find_event_type(TypeOntology* ontology, const char* name);

// 删除事件类型
int kos_ontology_remove_event_type(TypeOntology* ontology, const char* name);

// ========== 持久化存储 ==========

// 保存本体到文件（JSON格式）
int kos_ontology_save_to_file(TypeOntology* ontology, const char* filename);

// 从文件加载本体
TypeOntology* kos_ontology_load_from_file(const char* filename);

// 序列化本体为JSON字符串
char* kos_ontology_serialize(TypeOntology* ontology);

// 从JSON字符串反序列化本体
TypeOntology* kos_ontology_deserialize(const char* json_str);

// ========== 类型构建（基于本体） ==========

// 根据本体定义构建类型实例
kos_term* kos_ontology_mk_type_instance(TypeOntology* ontology,
                                        const char* type_name,
                                        void* data);

// 根据事件类型定义构建事件实例
kos_term* kos_ontology_mk_event_instance(TypeOntology* ontology,
                                         const char* event_type_name,
                                         void* event_data);

// 验证实例是否符合类型定义
bool kos_ontology_validate_instance(TypeOntology* ontology,
                                    kos_term* instance,
                                    const char* type_name);

#endif










