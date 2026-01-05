// src/domain/manufacturing/ontology_crud.c
// 制造业领域类型本体的CRUD操作接口
// 提供对类型本体的增删改查功能

#include "../../../include/kos_ontology.h"
#include "../../../include/kos_manufacturing.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// 获取制造业类型本体（单例模式）
static TypeOntology* get_ontology(void) {
    static TypeOntology* g_ontology = NULL;
    if (!g_ontology) {
        extern TypeOntology* kos_manufacturing_ontology_init(void);
        g_ontology = kos_manufacturing_ontology_init();
    }
    return g_ontology;
}

// ========== 原子类型 CRUD ==========

// 添加原子类型到制造业本体
int kos_manufacturing_add_atomic_type(const char* name, const char* base_type) {
    TypeOntology* ontology = get_ontology();
    if (!ontology) {
        return -1;
    }
    
    int result = kos_ontology_add_atomic_type(ontology, name, base_type, NULL);
    if (result == 0) {
        // 自动保存
        extern int kos_manufacturing_ontology_save(TypeOntology* ontology);
        kos_manufacturing_ontology_save(ontology);
    }
    return result;
}

// 查询原子类型
AtomicTypeDef* kos_manufacturing_get_atomic_type(const char* name) {
    TypeOntology* ontology = get_ontology();
    if (!ontology) {
        return NULL;
    }
    
    return kos_ontology_find_atomic_type(ontology, name);
}

// 列出所有原子类型
void kos_manufacturing_list_atomic_types(void) {
    TypeOntology* ontology = get_ontology();
    if (!ontology) {
        return;
    }
    
    printf("Atomic Types (%zu):\n", ontology->atomic_count);
    for (size_t i = 0; i < ontology->atomic_count; i++) {
        printf("  - %s : %s\n",
               ontology->atomic_types[i].name,
               ontology->atomic_types[i].base_type);
    }
}

// 删除原子类型
int kos_manufacturing_remove_atomic_type(const char* name) {
    TypeOntology* ontology = get_ontology();
    if (!ontology) {
        return -1;
    }
    
    int result = kos_ontology_remove_atomic_type(ontology, name);
    if (result == 0) {
        extern int kos_manufacturing_ontology_save(TypeOntology* ontology);
        kos_manufacturing_ontology_save(ontology);
    }
    return result;
}

// ========== 谓词类型 CRUD ==========

// 添加谓词类型
int kos_manufacturing_add_predicate_type(const char* name,
                                         const char** param_types,
                                         int param_count) {
    TypeOntology* ontology = get_ontology();
    if (!ontology) {
        return -1;
    }
    
    int result = kos_ontology_add_predicate_type(ontology, name, param_types, param_count, NULL);
    if (result == 0) {
        extern int kos_manufacturing_ontology_save(TypeOntology* ontology);
        kos_manufacturing_ontology_save(ontology);
    }
    return result;
}

// 查询谓词类型
PredicateTypeDef* kos_manufacturing_get_predicate_type(const char* name) {
    TypeOntology* ontology = get_ontology();
    if (!ontology) {
        return NULL;
    }
    
    return kos_ontology_find_predicate_type(ontology, name);
}

// 列出所有谓词类型
void kos_manufacturing_list_predicate_types(void) {
    TypeOntology* ontology = get_ontology();
    if (!ontology) {
        return;
    }
    
    printf("Predicate Types (%zu):\n", ontology->predicate_count);
    for (size_t i = 0; i < ontology->predicate_count; i++) {
        printf("  - %s(", ontology->predicate_types[i].name);
        for (int j = 0; j < ontology->predicate_types[i].param_count; j++) {
            printf("%s", ontology->predicate_types[i].param_types[j] ? 
                   ontology->predicate_types[i].param_types[j] : "?");
            if (j < ontology->predicate_types[i].param_count - 1) {
                printf(", ");
            }
        }
        printf(")\n");
    }
}

// ========== 事件类型 CRUD ==========

// 添加事件类型
int kos_manufacturing_add_event_type(const char* name,
                                     const char** field_names,
                                     const char** field_types,
                                     int field_count) {
    TypeOntology* ontology = get_ontology();
    if (!ontology) {
        return -1;
    }
    
    int result = kos_ontology_add_event_type(ontology, name, field_names, 
                                            field_types, field_count, NULL);
    if (result == 0) {
        extern int kos_manufacturing_ontology_save(TypeOntology* ontology);
        kos_manufacturing_ontology_save(ontology);
    }
    return result;
}

// 查询事件类型
EventTypeDef* kos_manufacturing_get_event_type(const char* name) {
    TypeOntology* ontology = get_ontology();
    if (!ontology) {
        return NULL;
    }
    
    return kos_ontology_find_event_type(ontology, name);
}

// 列出所有事件类型
void kos_manufacturing_list_event_types(void) {
    TypeOntology* ontology = get_ontology();
    if (!ontology) {
        return;
    }
    
    printf("Event Types (%zu):\n", ontology->event_count);
    for (size_t i = 0; i < ontology->event_count; i++) {
        printf("  - %s {\n", ontology->event_types[i].name);
        for (int j = 0; j < ontology->event_types[i].field_count; j++) {
            printf("      %s: %s\n",
                   ontology->event_types[i].field_names[j] ? 
                   ontology->event_types[i].field_names[j] : "?",
                   ontology->event_types[i].field_types[j] ? 
                   ontology->event_types[i].field_types[j] : "?");
        }
        printf("    }\n");
    }
}

// 删除事件类型
int kos_manufacturing_remove_event_type(const char* name) {
    TypeOntology* ontology = get_ontology();
    if (!ontology) {
        return -1;
    }
    
    int result = kos_ontology_remove_event_type(ontology, name);
    if (result == 0) {
        extern int kos_manufacturing_ontology_save(TypeOntology* ontology);
        kos_manufacturing_ontology_save(ontology);
    }
    return result;
}

// ========== 本体管理 ==========

// 重新加载本体（从文件）
int kos_manufacturing_reload_ontology(void) {
    extern TypeOntology* kos_manufacturing_ontology_init(void);
    // 注意：这里需要实现单例的重新加载逻辑
    // 简化实现
    return 0;
}

// 导出本体为JSON（用于备份或迁移）
char* kos_manufacturing_export_ontology_json(void) {
    TypeOntology* ontology = get_ontology();
    if (!ontology) {
        return NULL;
    }
    
    return kos_ontology_serialize(ontology);
}










