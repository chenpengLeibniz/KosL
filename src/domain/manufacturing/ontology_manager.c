// src/domain/manufacturing/ontology_manager.c
// 制造业领域本体管理器：负责本体的加载、缓存和访问
// 
// 职责：
// 1. 管理本体的单例实例（延迟加载、缓存）
// 2. 提供统一的访问接口
// 3. 处理大本体时的内存优化（未来可扩展为分片加载）

#include "../../../include/kos_ontology.h"
#include "../../../include/kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// ========== 本体单例管理 ==========

// 全局本体实例（单例模式，延迟加载）
static TypeOntology* g_manufacturing_ontology = NULL;
static const char* g_ontology_file = "manufacturing_ontology.json";

// 获取制造业本体实例（单例模式，延迟加载）
// 如果本体数量很大，可以考虑：
// - 分片加载：只加载常用类型，其他按需加载
// - 索引优化：使用哈希表或B树加速类型查找
// - 内存映射：对于非常大的本体，使用mmap
TypeOntology* kos_manufacturing_ontology_get(void) {
    // 如果已经加载，直接返回
    if (g_manufacturing_ontology) {
        return g_manufacturing_ontology;
    }
    
    // 延迟加载：调用初始化函数
    extern TypeOntology* kos_manufacturing_ontology_init(void);
    g_manufacturing_ontology = kos_manufacturing_ontology_init();
    
    if (g_manufacturing_ontology) {
        printf("[OntologyManager] ✓ Manufacturing ontology loaded (%zu types)\n", 
               g_manufacturing_ontology->type_count);
    } else {
        printf("[OntologyManager] ✗ Failed to load manufacturing ontology\n");
    }
    
    return g_manufacturing_ontology;
}

// 释放制造业本体实例
// 通常在程序结束时调用，或需要重新加载时调用
void kos_manufacturing_ontology_release(void) {
    if (g_manufacturing_ontology) {
        kos_ontology_free(g_manufacturing_ontology);
        g_manufacturing_ontology = NULL;
        printf("[OntologyManager] Manufacturing ontology released\n");
    }
}

// 强制重新加载本体（从文件）
// 用于运行时更新本体定义
int kos_manufacturing_ontology_reload(void) {
    // 释放旧实例
    kos_manufacturing_ontology_release();
    
    // 重新加载
    g_manufacturing_ontology = kos_manufacturing_ontology_get();
    
    return g_manufacturing_ontology ? 0 : -1;
}

// 检查本体是否已加载
bool kos_manufacturing_ontology_is_loaded(void) {
    return g_manufacturing_ontology != NULL;
}

// 获取本体中的类型数量
size_t kos_manufacturing_ontology_get_type_count(void) {
    if (!g_manufacturing_ontology) {
        return 0;
    }
    return g_manufacturing_ontology->type_count;
}
















