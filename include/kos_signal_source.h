/**
 * @file kos_signal_source.h
 * @brief KOS Runtime 层信号源扩展接口
 *
 * 支持工业协议（OPC-UA、MQTT、时序库等）作为 elab 的信号源。
 * 与 kos_data_integration 连接器对接，实现从外部系统拉取信号并送入 elab。
 *
 * 补齐路线：见 docs/KOS_TL_APPLICATION_VALUE_ANALYSIS.md Phase 1.1
 */

#ifndef KOS_SIGNAL_SOURCE_H
#define KOS_SIGNAL_SOURCE_H

#include "kos_runtime.h"
#include <stddef.h>
#include <stdbool.h>

/* ========== 信号源协议类型 ========== */

typedef enum {
    KOS_SIGNAL_MQTT,       /* MQTT 消息 */
    KOS_SIGNAL_OPCUA,      /* OPC-UA 订阅 */
    KOS_SIGNAL_KAFKA,      /* Kafka 主题 */
    KOS_SIGNAL_TIMESCALE,  /* 时序数据库 */
    KOS_SIGNAL_REST,       /* REST API 轮询 */
    KOS_SIGNAL_CUSTOM      /* 自定义协议 */
} kos_signal_protocol_t;

/* ========== 信号源适配器接口 ==========
 * 各工业协议实现此接口，将原始数据转为 bitstream 供 elab 处理 */

typedef struct kos_signal_source_adapter {
    const char* name;                    /* 适配器名称，如 "mqtt" */
    kos_signal_protocol_t protocol;      /* 协议类型 */
    
    /* 初始化：根据 config 建立连接，返回 handle */
    int (*connect)(const void* config, void** handle);
    
    /* 拉取一条信号：成功返回 0，无数据返回 1，失败返回 -1 */
    /* 调用者负责 free(signal->data) */
    int (*pull)(void* handle, bitstream* signal);
    
    /* 断开连接 */
    int (*disconnect)(void* handle);
    
    /* 释放 handle 关联资源 */
    void (*free_handle)(void* handle);
} kos_signal_source_adapter_t;

/* ========== 注册表 API ========== */

/** 注册信号源适配器 */
int kos_signal_source_register(const kos_signal_source_adapter_t* adapter);

/** 注销信号源适配器 */
int kos_signal_source_unregister(const char* name);

/** 按名称查找适配器 */
const kos_signal_source_adapter_t* kos_signal_source_lookup(const char* name);

/** 按协议类型查找适配器（返回首个匹配） */
const kos_signal_source_adapter_t* kos_signal_source_lookup_by_protocol(kos_signal_protocol_t protocol);

#endif /* KOS_SIGNAL_SOURCE_H */
