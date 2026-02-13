/**
 * @file signal_source_registry.c
 * @brief KOS Runtime 层信号源适配器注册表
 *
 * 支持工业协议（MQTT、OPC-UA 等）作为 elab 的信号源。
 * 补齐路线：见 docs/KOS_TL_APPLICATION_VALUE_ANALYSIS.md Phase 1.1
 */

#include "kos_signal_source.h"
#include <stdlib.h>
#include <string.h>

#define MAX_ADAPTERS 16

static struct {
    kos_signal_source_adapter_t adapters[MAX_ADAPTERS];
    size_t count;
} s_registry = {0};

int kos_signal_source_register(const kos_signal_source_adapter_t* adapter) {
    if (!adapter || !adapter->name || s_registry.count >= MAX_ADAPTERS) {
        return -1;
    }
    for (size_t i = 0; i < s_registry.count; i++) {
        if (s_registry.adapters[i].name &&
            strcmp(s_registry.adapters[i].name, adapter->name) == 0) {
            return -2;  /* 已存在同名 */
        }
    }
    s_registry.adapters[s_registry.count++] = *adapter;
    return 0;
}

int kos_signal_source_unregister(const char* name) {
    if (!name) return -1;
    for (size_t i = 0; i < s_registry.count; i++) {
        if (s_registry.adapters[i].name &&
            strcmp(s_registry.adapters[i].name, name) == 0) {
            for (size_t j = i; j < s_registry.count - 1; j++) {
                s_registry.adapters[j] = s_registry.adapters[j + 1];
            }
            s_registry.count--;
            return 0;
        }
    }
    return -2;  /* 未找到 */
}

const kos_signal_source_adapter_t* kos_signal_source_lookup(const char* name) {
    if (!name) return NULL;
    for (size_t i = 0; i < s_registry.count; i++) {
        if (s_registry.adapters[i].name &&
            strcmp(s_registry.adapters[i].name, name) == 0) {
            return &s_registry.adapters[i];
        }
    }
    return NULL;
}

const kos_signal_source_adapter_t* kos_signal_source_lookup_by_protocol(kos_signal_protocol_t protocol) {
    for (size_t i = 0; i < s_registry.count; i++) {
        if (s_registry.adapters[i].protocol == protocol) {
            return &s_registry.adapters[i];
        }
    }
    return NULL;
}
