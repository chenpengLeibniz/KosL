// examples/ontology_version_demo.c
// KOS Dynamic Ontology Management Demo
// Phase 2: 动态本体管理示例

#include "../include/kos_ontology.h"
#include "../include/kos_ontology_version.h"
#include "../include/kos_core.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// ========== 变更通知回调示例 ==========

void change_callback(const char* type_name, update_operation_t op, void* user_data) {
    const char* op_str = "UNKNOWN";
    switch (op) {
        case UPDATE_ADD_TYPE: op_str = "ADD"; break;
        case UPDATE_REMOVE_TYPE: op_str = "REMOVE"; break;
        case UPDATE_MODIFY_TYPE: op_str = "MODIFY"; break;
    }
    
    printf("[Callback] Type '%s' %s\n", type_name, op_str);
}

// ========== 示例 1: 版本管理 ==========

void example_version_management(void) {
    printf("\n=== Example 1: Version Management ===\n");
    
    // 创建本体
    TypeOntology* ontology = kos_ontology_create("manufacturing");
    
    // 添加初始类型定义
    kos_term* batch_id_type = kos_mk_id("BatchID");
    kos_ontology_add_type_definition(ontology, "BatchID", batch_id_type, NULL);
    
    // 创建版本 v1.0.0
    kos_ontology_version_t* v1 = kos_ontology_create_version(
        ontology, "v1.0.0", "Initial version with BatchID"
    );
    
    if (v1) {
        printf("✓ Created version: %s\n", v1->version_name);
        printf("  Commit hash: %s\n", v1->commit_hash);
        printf("  Timestamp: %s\n", v1->timestamp);
    }
    
    // 添加新类型
    kos_term* machine_type = kos_mk_id("Machine");
    kos_ontology_add_type_definition(ontology, "Machine", machine_type, NULL);
    
    // 创建版本 v1.1.0
    kos_ontology_version_t* v2 = kos_ontology_create_version(
        ontology, "v1.1.0", "Added Machine type"
    );
    
    if (v2) {
        printf("✓ Created version: %s\n", v2->version_name);
    }
    
    // 列出所有版本
    size_t version_count = 0;
    kos_ontology_version_t** versions = kos_ontology_list_versions(ontology, &version_count);
    printf("\nTotal versions: %zu\n", version_count);
    for (size_t i = 0; i < version_count; i++) {
        printf("  Version %zu: %s\n", i + 1, versions[i]->version_name);
    }
    free(versions);
    
    // 比较版本
    kos_ontology_diff_t* diff = kos_ontology_diff(ontology, "v1.0.0", "v1.1.0");
    if (diff) {
        printf("\nVersion diff (v1.0.0 -> v1.1.0):\n");
        printf("  Changes: %zu\n", diff->count);
        for (size_t i = 0; i < diff->count; i++) {
            const char* change_type_str = "UNKNOWN";
            switch (diff->items[i].change_type) {
                case DIFF_ADDED: change_type_str = "ADDED"; break;
                case DIFF_REMOVED: change_type_str = "REMOVED"; break;
                case DIFF_MODIFIED: change_type_str = "MODIFIED"; break;
            }
            printf("    - %s: %s\n", diff->items[i].type_name, change_type_str);
        }
        kos_ontology_diff_free(diff);
    }
    
    // 清理
    kos_term_free(batch_id_type);
    kos_term_free(machine_type);
    kos_ontology_free(ontology);
}

// ========== 示例 2: 运行时更新 ==========

void example_runtime_update(void) {
    printf("\n=== Example 2: Runtime Update ===\n");
    
    // 创建本体
    TypeOntology* ontology = kos_ontology_create("manufacturing");
    
    // 注册变更通知回调
    kos_ontology_register_change_callback(ontology, change_callback, NULL);
    
    // 添加初始类型
    kos_term* batch_id_type = kos_mk_id("BatchID");
    kos_ontology_add_type_definition(ontology, "BatchID", batch_id_type, NULL);
    
    // 开始事务
    kos_ontology_transaction_t* transaction = kos_ontology_begin_transaction(ontology);
    if (transaction) {
        printf("✓ Transaction started\n");
        
        // 添加更新操作
        kos_term* machine_type = kos_mk_id("Machine");
        kos_ontology_transaction_add_update(
            transaction, UPDATE_ADD_TYPE, "Machine", machine_type, NULL
        );
        
        kos_term* time_type = kos_mk_time("Time");
        kos_ontology_transaction_add_update(
            transaction, UPDATE_ADD_TYPE, "Time", time_type, NULL
        );
        
        printf("  Added 2 update operations\n");
        
        // 提交事务
        int result = kos_ontology_commit_transaction(
            ontology, transaction, "v1.0.0", "Added Machine and Time types"
        );
        
        if (result == 0) {
            printf("✓ Transaction committed successfully\n");
        } else {
            printf("✗ Transaction commit failed\n");
        }
        
        kos_ontology_transaction_free(transaction);
        kos_term_free(machine_type);
        kos_term_free(time_type);
    }
    
    // 原子性更新单个类型
    kos_term* error_type = kos_mk_id("ErrorCode");
    int result = kos_ontology_update_atomic(
        ontology, "ErrorCode", error_type, NULL, "v1.1.0", "Added ErrorCode type"
    );
    
    if (result == 0) {
        printf("✓ Atomic update successful\n");
    }
    
    kos_term_free(batch_id_type);
    kos_term_free(error_type);
    kos_ontology_free(ontology);
}

// ========== 示例 3: 影响分析 ==========

void example_impact_analysis(void) {
    printf("\n=== Example 3: Impact Analysis ===\n");
    
    // 创建本体
    TypeOntology* ontology = kos_ontology_create("manufacturing");
    
    // 添加初始类型
    kos_term* batch_id_type = kos_mk_id("BatchID");
    kos_ontology_add_type_definition(ontology, "BatchID", batch_id_type, NULL);
    
    kos_term* machine_type = kos_mk_id("Machine");
    kos_ontology_add_type_definition(ontology, "Machine", machine_type, NULL);
    
    // 分析修改 BatchID 类型的影响
    kos_term* new_batch_id_type = kos_mk_id("BatchID"); // 简化：相同类型
    kos_impact_analysis_t* impact = kos_ontology_analyze_impact(
        ontology, "BatchID", new_batch_id_type, NULL
    );
    
    if (impact) {
        printf("✓ Impact analysis completed\n");
        printf("  Type: %s\n", impact->type_name);
        printf("  Breaking change: %s\n", impact->breaking_change ? "Yes" : "No");
        printf("  Affected instances: %zu\n", impact->affected_instance_count);
        printf("  Affected queries: %zu\n", impact->affected_query_count);
        printf("  Migration hint: %s\n", impact->migration_hint ? impact->migration_hint : "N/A");
        
        // 生成迁移脚本
        kos_migration_script_t* script = kos_ontology_generate_migration(impact);
        if (script) {
            printf("\n✓ Migration script generated\n");
            printf("  Script length: %zu bytes\n", script->script_length);
            printf("  Script preview:\n%s\n", script->script_content);
            
            kos_migration_script_free(script);
        }
        
        kos_impact_analysis_free(impact);
    }
    
    kos_term_free(batch_id_type);
    kos_term_free(machine_type);
    kos_term_free(new_batch_id_type);
    kos_ontology_free(ontology);
}

// ========== 示例 4: 版本回滚 ==========

void example_version_rollback(void) {
    printf("\n=== Example 4: Version Rollback ===\n");
    
    // 创建本体
    TypeOntology* ontology = kos_ontology_create("manufacturing");
    
    // 添加初始类型并创建版本
    kos_term* batch_id_type = kos_mk_id("BatchID");
    kos_ontology_add_type_definition(ontology, "BatchID", batch_id_type, NULL);
    
    kos_ontology_create_version(ontology, "v1.0.0", "Initial version");
    printf("✓ Created v1.0.0\n");
    
    // 添加新类型
    kos_term* machine_type = kos_mk_id("Machine");
    kos_ontology_add_type_definition(ontology, "Machine", machine_type, NULL);
    
    kos_ontology_create_version(ontology, "v1.1.0", "Added Machine");
    printf("✓ Created v1.1.0 (current type count: %zu)\n", ontology->type_count);
    
    // 回滚到 v1.0.0
    int result = kos_ontology_rollback(ontology, "v1.0.0");
    if (result == 0) {
        printf("✓ Rolled back to v1.0.0\n");
        printf("  Current type count: %zu\n", ontology->type_count);
    } else {
        printf("✗ Rollback failed\n");
    }
    
    kos_term_free(batch_id_type);
    kos_term_free(machine_type);
    kos_ontology_free(ontology);
}

// ========== 主程序 ==========

int main(void) {
    printf("========================================\n");
    printf("KOS Dynamic Ontology Management Demo\n");
    printf("Phase 2: Version Control & Runtime Update\n");
    printf("========================================\n");
    
    example_version_management();
    example_runtime_update();
    example_impact_analysis();
    example_version_rollback();
    
    printf("\n========================================\n");
    printf("Demo completed successfully!\n");
    printf("========================================\n");
    
    return 0;
}
