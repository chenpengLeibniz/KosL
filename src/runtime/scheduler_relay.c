// src/runtime/scheduler_relay.c
// Scheduler Relay: 作为 Kernel 顺序演化的缓冲区
// 负责多线程信号的并发接收和有序排队

#include "kos_runtime.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// ========== 信号缓冲区 ==========

signal_buffer_t* kos_signal_buffer_create(size_t capacity) {
    if (capacity == 0) {
        capacity = 64;  // 默认容量
    }
    
    signal_buffer_t* buffer = (signal_buffer_t*)calloc(1, sizeof(signal_buffer_t));
    if (!buffer) {
        return NULL;
    }
    
    buffer->signals = (bitstream*)calloc(capacity, sizeof(bitstream));
    if (!buffer->signals) {
        free(buffer);
        return NULL;
    }
    
    buffer->capacity = capacity;
    buffer->count = 0;
    buffer->head = 0;
    buffer->tail = 0;
    
    return buffer;
}

void kos_signal_buffer_free(signal_buffer_t* buffer) {
    if (!buffer) {
        return;
    }
    
    // 释放所有信号的数据
    for (size_t i = 0; i < buffer->count; i++) {
        size_t idx = (buffer->head + i) % buffer->capacity;
        if (buffer->signals[idx].data) {
            free(buffer->signals[idx].data);
        }
    }
    
    if (buffer->signals) {
        free(buffer->signals);
    }
    
    free(buffer);
}

int kos_signal_buffer_add(signal_buffer_t* buffer, bitstream signal) {
    if (!buffer || !signal.data || signal.length == 0) {
        return -1;
    }
    
    // 检查缓冲区是否已满
    if (buffer->count >= buffer->capacity) {
        // 扩展容量
        size_t new_capacity = buffer->capacity * 2;
        bitstream* new_signals = (bitstream*)realloc(buffer->signals, new_capacity * sizeof(bitstream));
        if (!new_signals) {
            return -1;
        }
        
        // 重新排列信号（如果需要）
        if (buffer->head > buffer->tail) {
            // 需要重新排列
            size_t old_count = buffer->count;
            for (size_t i = 0; i < old_count; i++) {
                size_t old_idx = (buffer->head + i) % buffer->capacity;
                new_signals[i] = new_signals[old_idx];
            }
            buffer->head = 0;
            buffer->tail = old_count;
        }
        
        buffer->signals = new_signals;
        buffer->capacity = new_capacity;
    }
    
    // 深拷贝信号数据
    unsigned char* signal_data = (unsigned char*)malloc(signal.length);
    if (!signal_data) {
        return -1;
    }
    
    memcpy(signal_data, signal.data, signal.length);
    
    // 添加到缓冲区尾部
    buffer->signals[buffer->tail].data = signal_data;
    buffer->signals[buffer->tail].length = signal.length;
    
    buffer->tail = (buffer->tail + 1) % buffer->capacity;
    buffer->count++;
    
    return 0;
}

bitstream kos_signal_buffer_take(signal_buffer_t* buffer) {
    bitstream empty_signal = {NULL, 0};
    
    if (!buffer || buffer->count == 0) {
        return empty_signal;
    }
    
    // 从头部取出信号（FIFO 顺序）
    bitstream signal = buffer->signals[buffer->head];
    
    // 清空缓冲区中的引用（避免 double-free）
    buffer->signals[buffer->head].data = NULL;
    buffer->signals[buffer->head].length = 0;
    
    buffer->head = (buffer->head + 1) % buffer->capacity;
    buffer->count--;
    
    return signal;
}

bool kos_signal_buffer_is_empty(signal_buffer_t* buffer) {
    return !buffer || buffer->count == 0;
}

size_t kos_signal_buffer_size(signal_buffer_t* buffer) {
    return buffer ? buffer->count : 0;
}
