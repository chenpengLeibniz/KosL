// src/runtime/replay.c
// 轨迹重放和自愈功能；MTK: 从 Event Log 确定性重放与崩溃恢复

#include "kos_runtime.h"
#include "kos_core.h"
#include "kos_trace.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// ========== 轨迹重放 ==========

int kos_replay_elaboration_trajectory(elaboration_record_t* records, size_t count,
                                      kos_state_t* sigma, storage_backend_t* backend) {
    if (!records || count == 0 || !sigma) {
        return -1;
    }
    
    printf("[Replay] Starting trajectory replay (%zu events)...\n", count);
    
    // 按信号索引排序（确保顺序）
    // 简化实现：假设 records 已经按 signal_index 排序
    
    int success_count = 0;
    
    for (size_t i = 0; i < count; i++) {
        elaboration_record_t* record = &records[i];
        
        if (!record->event_pair) {
            continue;
        }
        
        // 重新精化原始信号（如果需要）
        // 简化实现：直接使用记录的事件对
        
        // 将事件对加入队列
        if (kos_queue_enqueue(sigma->P, record->event_pair) != 0) {
            fprintf(stderr, "[Replay] Failed to enqueue event at index %zu\n", i);
            continue;
        }
        
        // 执行演化
        bool step_result = kos_step(sigma);
        if (step_result) {
            success_count++;
            
            // 物化到存储后端（如果提供）
            if (backend) {
                kos_materialize(sigma, backend);
            }
        } else {
            fprintf(stderr, "[Replay] Step failed at index %zu\n", i);
        }
    }
    
    printf("[Replay] Replay complete: %d/%zu events succeeded\n", success_count, count);
    
    return (success_count == (int)count) ? 0 : -1;
}

// ========== 轨迹保存和加载 ==========

int kos_save_elaboration_trajectory(elaboration_record_t* records, size_t count, const char* filename) {
    if (!records || count == 0 || !filename) {
        return -1;
    }
    
    FILE* fp = fopen(filename, "wb");
    if (!fp) {
        return -1;
    }
    
    // 写入记录数量
    if (fwrite(&count, sizeof(size_t), 1, fp) != 1) {
        fclose(fp);
        return -1;
    }
    
    // 写入每个记录
    for (size_t i = 0; i < count; i++) {
        elaboration_record_t* record = &records[i];
        
        // 写入逻辑时钟和信号索引
        if (fwrite(&record->logical_clock, sizeof(int), 1, fp) != 1 ||
            fwrite(&record->signal_index, sizeof(size_t), 1, fp) != 1) {
            fclose(fp);
            return -1;
        }
        
        // 写入原始信号
        if (fwrite(&record->original_signal.length, sizeof(size_t), 1, fp) != 1) {
            fclose(fp);
            return -1;
        }
        
        if (record->original_signal.length > 0) {
            if (fwrite(record->original_signal.data, 1, record->original_signal.length, fp) != record->original_signal.length) {
                fclose(fp);
                return -1;
            }
        }
        
        // 序列化事件对（使用 Core 层的序列化功能）
        kos_serialized* serialized = kos_term_serialize(record->event_pair);
        if (!serialized) {
            fclose(fp);
            return -1;
        }
        
        if (fwrite(&serialized->length, sizeof(size_t), 1, fp) != 1 ||
            fwrite(serialized->data, 1, serialized->length, fp) != serialized->length) {
            kos_serialized_free(serialized);
            fclose(fp);
            return -1;
        }
        
        kos_serialized_free(serialized);
    }
    
    fclose(fp);
    return 0;
}

elaboration_record_t* kos_load_elaboration_trajectory(const char* filename, size_t* count) {
    if (!filename || !count) {
        return NULL;
    }
    
    FILE* fp = fopen(filename, "rb");
    if (!fp) {
        return NULL;
    }
    
    // 读取记录数量
    size_t record_count = 0;
    if (fread(&record_count, sizeof(size_t), 1, fp) != 1) {
        fclose(fp);
        return NULL;
    }
    
    if (record_count == 0) {
        fclose(fp);
        *count = 0;
        return NULL;
    }
    
    // 分配记录数组
    elaboration_record_t* records = (elaboration_record_t*)calloc(record_count, sizeof(elaboration_record_t));
    if (!records) {
        fclose(fp);
        return NULL;
    }
    
    // 读取每个记录
    for (size_t i = 0; i < record_count; i++) {
        elaboration_record_t* record = &records[i];
        
        // 读取逻辑时钟和信号索引
        if (fread(&record->logical_clock, sizeof(int), 1, fp) != 1 ||
            fread(&record->signal_index, sizeof(size_t), 1, fp) != 1) {
            // 清理已分配的资源
            for (size_t j = 0; j < i; j++) {
                kos_elab_record_free(&records[j]);
            }
            free(records);
            fclose(fp);
            return NULL;
        }
        
        // 读取原始信号
        size_t signal_length = 0;
        if (fread(&signal_length, sizeof(size_t), 1, fp) != 1) {
            for (size_t j = 0; j < i; j++) {
                kos_elab_record_free(&records[j]);
            }
            free(records);
            fclose(fp);
            return NULL;
        }
        
        if (signal_length > 0) {
            record->original_signal.data = (unsigned char*)malloc(signal_length);
            if (!record->original_signal.data) {
                for (size_t j = 0; j < i; j++) {
                    kos_elab_record_free(&records[j]);
                }
                free(records);
                fclose(fp);
                return NULL;
            }
            
            if (fread(record->original_signal.data, 1, signal_length, fp) != signal_length) {
                free(record->original_signal.data);
                for (size_t j = 0; j < i; j++) {
                    kos_elab_record_free(&records[j]);
                }
                free(records);
                fclose(fp);
                return NULL;
            }
            
            record->original_signal.length = signal_length;
        } else {
            record->original_signal.data = NULL;
            record->original_signal.length = 0;
        }
        
        // 读取事件对（反序列化）
        size_t serialized_length = 0;
        if (fread(&serialized_length, sizeof(size_t), 1, fp) != 1) {
            if (record->original_signal.data) {
                free(record->original_signal.data);
            }
            for (size_t j = 0; j < i; j++) {
                kos_elab_record_free(&records[j]);
            }
            free(records);
            fclose(fp);
            return NULL;
        }
        
        if (serialized_length > 0) {
            char* serialized_data = (char*)malloc(serialized_length + 1);
            if (!serialized_data) {
                if (record->original_signal.data) {
                    free(record->original_signal.data);
                }
                for (size_t j = 0; j < i; j++) {
                    kos_elab_record_free(&records[j]);
                }
                free(records);
                fclose(fp);
                return NULL;
            }
            
            if (fread(serialized_data, 1, serialized_length, fp) != serialized_length) {
                free(serialized_data);
                if (record->original_signal.data) {
                    free(record->original_signal.data);
                }
                for (size_t j = 0; j < i; j++) {
                    kos_elab_record_free(&records[j]);
                }
                free(records);
                fclose(fp);
                return NULL;
            }
            
            serialized_data[serialized_length] = '\0';
            
            // 反序列化事件对
            record->event_pair = kos_term_deserialize(serialized_data);
            free(serialized_data);
            
            if (!record->event_pair) {
                if (record->original_signal.data) {
                    free(record->original_signal.data);
                }
                for (size_t j = 0; j < i; j++) {
                    kos_elab_record_free(&records[j]);
                }
                free(records);
                fclose(fp);
                return NULL;
            }
        } else {
            record->event_pair = NULL;
        }
    }
    
    fclose(fp);
    *count = record_count;
    return records;
}

// ========== MTK: 从 Event Log 确定性重放 ==========

kos_state_t* kos_replay_from_trace(const kos_trace_t* trace, kos_term* initial_K) {
    if (!trace) return NULL;
    return kos_trace_replay(trace, initial_K);
}

kos_state_t* kos_replay_from_trace_file(const char* trace_filename, kos_term* optional_initial_K,
                                        uint64_t expected_hash, bool* verify_ok) {
    if (!trace_filename) {
        if (verify_ok) *verify_ok = false;
        return NULL;
    }
    kos_trace_t* trace = kos_trace_load_from_file(trace_filename);
    if (!trace) {
        if (verify_ok) *verify_ok = false;
        return NULL;
    }
    kos_state_t* sigma = kos_trace_replay(trace, optional_initial_K);
    kos_trace_free(trace);
    if (!sigma) {
        if (verify_ok) *verify_ok = false;
        return NULL;
    }
    if (expected_hash != 0) {
        uint64_t actual = kos_state_get_hash(sigma);
        if (verify_ok) *verify_ok = (actual == expected_hash);
        if (actual != expected_hash) {
            kos_state_free(sigma);
            return NULL;
        }
    } else if (verify_ok) {
        *verify_ok = true;
    }
    return sigma;
}
