// examples/data_integration_demo.c
// Phase 3: 数据集成与融合演示
// 演示多数据源集成和数据融合规则引擎

#include "kos_data_integration.h"
#include "kos_core.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// 创建测试 CSV 文件
static int create_test_csv(const char* filename) {
    FILE* file = fopen(filename, "w");
    if (!file) {
        return -1;
    }
    
    // 写入表头和数据
    fprintf(file, "id,name,value,timestamp\n");
    fprintf(file, "1,ProductA,100.5,2023-10-01T10:00:00\n");
    fprintf(file, "2,ProductB,200.3,2023-10-01T11:00:00\n");
    fprintf(file, "3,ProductC,150.7,2023-10-01T12:00:00\n");
    
    fclose(file);
    return 0;
}

// 创建测试 JSON 文件
static int create_test_json(const char* filename) {
    FILE* file = fopen(filename, "w");
    if (!file) {
        return -1;
    }
    
    // 写入 JSON 数据（简化格式）
    fprintf(file, "{\n");
    fprintf(file, "  \"id\": 4,\n");
    fprintf(file, "  \"name\": \"ProductD\",\n");
    fprintf(file, "  \"value\": 180.2,\n");
    fprintf(file, "  \"timestamp\": \"2023-10-01T13:00:00\"\n");
    fprintf(file, "}\n");
    
    fclose(file);
    return 0;
}

int main(void) {
    printf("=== Phase 3: Data Integration & Fusion Demo ===\n\n");
    
    // 1. 初始化内置连接器
    printf("1. Initializing builtin connectors...\n");
    if (kos_init_builtin_connectors() != 0) {
        fprintf(stderr, "Failed to initialize builtin connectors\n");
        return 1;
    }
    printf("   ✓ Builtin connectors initialized\n\n");
    
    // 2. 创建测试数据文件
    printf("2. Creating test data files...\n");
    const char* csv_file = "test_data.csv";
    const char* json_file = "test_data.json";
    
    if (create_test_csv(csv_file) != 0) {
        fprintf(stderr, "Failed to create CSV file\n");
        return 1;
    }
    printf("   ✓ Created %s\n", csv_file);
    
    if (create_test_json(json_file) != 0) {
        fprintf(stderr, "Failed to create JSON file\n");
        return 1;
    }
    printf("   ✓ Created %s\n\n", json_file);
    
    // 3. 创建 CSV 数据源连接
    printf("3. Connecting to CSV data source...\n");
    kos_data_source_config_t csv_config = {
        .type = DATA_SOURCE_CSV,
        .name = "test_csv",
        .connection_string = csv_file,
        .custom_config = NULL
    };
    
    kos_data_source_handle_t* csv_handle = kos_create_data_source(&csv_config);
    if (!csv_handle || !csv_handle->connected) {
        fprintf(stderr, "Failed to connect to CSV data source\n");
        return 1;
    }
    printf("   ✓ Connected to CSV data source\n\n");
    
    // 4. 创建 JSON 数据源连接
    printf("4. Connecting to JSON data source...\n");
    kos_data_source_config_t json_config = {
        .type = DATA_SOURCE_JSON,
        .name = "test_json",
        .connection_string = json_file,
        .custom_config = NULL
    };
    
    kos_data_source_handle_t* json_handle = kos_create_data_source(&json_config);
    if (!json_handle || !json_handle->connected) {
        fprintf(stderr, "Failed to connect to JSON data source\n");
        kos_close_data_source(csv_handle);
        return 1;
    }
    printf("   ✓ Connected to JSON data source\n\n");
    
    // 5. 读取数据源模式
    printf("5. Reading schemas from data sources...\n");
    kos_term* csv_schema = kos_data_source_read_schema(csv_handle, NULL);
    if (csv_schema) {
        printf("   ✓ CSV schema read (simplified)\n");
    }
    
    kos_term* json_schema = kos_data_source_read_schema(json_handle, NULL);
    if (json_schema) {
        printf("   ✓ JSON schema read (simplified)\n");
    }
    printf("\n");
    
    // 6. 创建数据融合规则
    printf("6. Creating data fusion rule...\n");
    kos_fusion_rule_t* fusion_rule = kos_create_fusion_rule(
        "Product",
        FUSION_LATEST_FIRST  // 默认策略：最新优先
    );
    
    if (!fusion_rule) {
        fprintf(stderr, "Failed to create fusion rule\n");
        kos_close_data_source(csv_handle);
        kos_close_data_source(json_handle);
        return 1;
    }
    
    // 添加字段规则
    kos_fusion_rule_add_field(fusion_rule, "value", FUSION_WEIGHTED_AVG, 0.5);
    kos_fusion_rule_add_field(fusion_rule, "timestamp", FUSION_LATEST_FIRST, 0.0);
    
    printf("   ✓ Fusion rule created:\n");
    printf("     - Target type: %s\n", fusion_rule->target_type);
    printf("     - Default strategy: LATEST_FIRST\n");
    printf("     - Field rules: value (WEIGHTED_AVG), timestamp (LATEST_FIRST)\n\n");
    
    // 7. 从数据源读取数据
    printf("7. Reading data from sources...\n");
    kos_term* csv_data = kos_data_source_read_data(csv_handle, NULL);
    if (csv_data) {
        printf("   ✓ Data read from CSV (simplified)\n");
    }
    
    kos_term* json_data = kos_data_source_read_data(json_handle, NULL);
    if (json_data) {
        printf("   ✓ Data read from JSON (simplified)\n");
    }
    printf("\n");
    
    // 8. 融合数据
    printf("8. Fusing data from multiple sources...\n");
    if (csv_data && json_data) {
        kos_term* fused_data = kos_fuse_data(csv_data, json_data, fusion_rule);
        if (fused_data) {
            printf("   ✓ Data fused successfully (simplified)\n");
        }
    }
    printf("\n");
    
    // 9. 多数据源融合
    printf("9. Fusing from multiple sources...\n");
    kos_data_source_handle_t* sources[] = {csv_handle, json_handle};
    kos_term* multi_fused = kos_fuse_from_sources(
        sources,
        2,
        NULL,  // 无查询过滤器
        fusion_rule
    );
    if (multi_fused) {
        printf("   ✓ Multi-source fusion completed (simplified)\n");
    }
    printf("\n");
    
    // 10. 清理资源
    printf("10. Cleaning up resources...\n");
    kos_fusion_rule_free(fusion_rule);
    kos_close_data_source(csv_handle);
    kos_close_data_source(json_handle);
    kos_cleanup_builtin_connectors();
    printf("   ✓ Resources cleaned up\n\n");
    
    // 11. 清理测试文件
    printf("11. Cleaning up test files...\n");
    remove(csv_file);
    remove(json_file);
    printf("   ✓ Test files removed\n\n");
    
    printf("=== Demo completed successfully ===\n");
    
    return 0;
}
