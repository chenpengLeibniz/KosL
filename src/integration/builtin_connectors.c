// src/integration/builtin_connectors.c
// 内置连接器初始化

#include "kos_data_integration.h"

// 前向声明
extern kos_data_source_connector_t* kos_get_csv_connector(void);
extern kos_data_source_connector_t* kos_get_json_connector(void);

// 初始化内置连接器
int kos_init_builtin_connectors(void) {
    int ret = 0;
    
    // 注册 CSV 连接器
    kos_data_source_connector_t* csv = kos_get_csv_connector();
    if (csv) {
        if (kos_register_data_source_connector("csv", csv) != 0) {
            ret = -1;
        }
    }
    
    // 注册 JSON 连接器
    kos_data_source_connector_t* json = kos_get_json_connector();
    if (json) {
        if (kos_register_data_source_connector("json", json) != 0) {
            ret = -1;
        }
    }
    
    return ret;
}

// 清理内置连接器
void kos_cleanup_builtin_connectors(void) {
    // 注销连接器（简化实现）
    kos_unregister_data_source_connector("csv");
    kos_unregister_data_source_connector("json");
}
