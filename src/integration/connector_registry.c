// src/integration/connector_registry.c
// 数据源连接器注册表实现

#include "kos_data_integration.h"
#include <stdlib.h>
#include <string.h>

#define MAX_CONNECTORS 32

// 连接器注册表
static struct {
    kos_data_source_connector_t* connectors[MAX_CONNECTORS];
    size_t count;
} connector_registry = {0};

// 注册数据源连接器
int kos_register_data_source_connector(
    const char* name,
    kos_data_source_connector_t* connector
) {
    if (!name || !connector) {
        return -1;
    }
    
    if (connector_registry.count >= MAX_CONNECTORS) {
        return -2; // 注册表已满
    }
    
    // 检查是否已存在同名连接器
    for (size_t i = 0; i < connector_registry.count; i++) {
        if (connector_registry.connectors[i] &&
            strcmp(connector_registry.connectors[i]->name, name) == 0) {
            return -3; // 已存在同名连接器
        }
    }
    
    connector_registry.connectors[connector_registry.count++] = connector;
    return 0;
}

// 注销数据源连接器
int kos_unregister_data_source_connector(const char* name) {
    if (!name) {
        return -1;
    }
    
    for (size_t i = 0; i < connector_registry.count; i++) {
        if (connector_registry.connectors[i] &&
            strcmp(connector_registry.connectors[i]->name, name) == 0) {
            // 移除连接器（移动后面的元素）
            for (size_t j = i; j < connector_registry.count - 1; j++) {
                connector_registry.connectors[j] = connector_registry.connectors[j + 1];
            }
            connector_registry.count--;
            return 0;
        }
    }
    
    return -2; // 未找到连接器
}

// 获取连接器（按类型和名称）
kos_data_source_connector_t* kos_get_data_source_connector(
    data_source_type_t type,
    const char* name
) {
    for (size_t i = 0; i < connector_registry.count; i++) {
        kos_data_source_connector_t* connector = connector_registry.connectors[i];
        if (connector) {
            if (name && strcmp(connector->name, name) == 0) {
                return connector;
            }
            if (!name && connector->type == type) {
                return connector;
            }
        }
    }
    return NULL;
}

// 创建数据源连接
kos_data_source_handle_t* kos_create_data_source(
    kos_data_source_config_t* config
) {
    if (!config || !config->connection_string) {
        return NULL;
    }
    
    // 查找匹配的连接器
    kos_data_source_connector_t* connector = kos_get_data_source_connector(
        config->type, NULL
    );
    
    if (!connector) {
        return NULL;
    }
    
    // 分配句柄
    kos_data_source_handle_t* handle = (kos_data_source_handle_t*)malloc(
        sizeof(kos_data_source_handle_t)
    );
    if (!handle) {
        return NULL;
    }
    
    handle->connector = connector;
    handle->config = *config;
    handle->connected = false;
    handle->handle = NULL;
    
    // 连接
    if (connector->connect(config, &handle->handle) == 0) {
        handle->connected = true;
    }
    
    return handle;
}

// 关闭数据源连接
int kos_close_data_source(kos_data_source_handle_t* handle) {
    if (!handle) {
        return -1;
    }
    
    if (handle->connected && handle->connector && handle->connector->disconnect) {
        handle->connector->disconnect(handle->handle);
    }
    
    if (handle->connector && handle->connector->free_handle && handle->handle) {
        handle->connector->free_handle(handle->handle);
    }
    
    handle->connected = false;
    free(handle);
    return 0;
}

// 从数据源读取模式
kos_term* kos_data_source_read_schema(
    kos_data_source_handle_t* handle,
    const char* table_or_collection
) {
    if (!handle || !handle->connected || !handle->connector) {
        return NULL;
    }
    
    if (!handle->connector->read_schema) {
        return NULL;
    }
    
    return handle->connector->read_schema(handle->handle, table_or_collection);
}

// 从数据源读取数据
kos_term* kos_data_source_read_data(
    kos_data_source_handle_t* handle,
    const char* query_or_filter
) {
    if (!handle || !handle->connected || !handle->connector) {
        return NULL;
    }
    
    if (!handle->connector->read_data) {
        return NULL;
    }
    
    return handle->connector->read_data(handle->handle, query_or_filter);
}
