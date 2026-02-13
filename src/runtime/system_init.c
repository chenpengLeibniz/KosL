// src/runtime/system_init.c
// 系统初始化函数

#include "kos_runtime.h"
#include "kos_kernel.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// 初始化系统（创建初始本体）
kos_state_t* kos_runtime_init(kos_term* initial_ontology) {
    // 使用 Kernel 层的状态创建函数
    return kos_state_create(initial_ontology);
}

void kos_runtime_free(kos_state_t* sigma) {
    if (sigma) {
        kos_state_free(sigma);
    }
}

// 保持向后兼容
kos_state_t init_system(void) {
    kos_state_t* sigma_ptr = kos_runtime_init(NULL);
    if (!sigma_ptr) {
        kos_state_t empty;
        memset(&empty, 0, sizeof(empty));
        return empty;
    }
    
    kos_state_t sigma = *sigma_ptr;
    free(sigma_ptr);  // 注意：这里只复制了结构，队列指针等需要单独管理
    return sigma;
}

// 捕获物理信号（简化实现：从标准输入读取）
bitstream kos_capture_physical_signal(signal_source_t* source) {
    bitstream s;
    s.data = NULL;
    s.length = 0;
    
    if (!source) {
        // 默认从标准输入读取
        char buffer[1024];
        if (fgets(buffer, sizeof(buffer), stdin)) {
            size_t len = strlen(buffer);
            if (len > 0 && buffer[len - 1] == '\n') {
                buffer[len - 1] = '\0';
                len--;
            }
            
            if (len > 0) {
                s.data = (unsigned char*)malloc(len);
                if (s.data) {
                    memcpy(s.data, buffer, len);
                    s.length = len;
                }
            }
        }
    } else {
        // 根据信号源类型读取
        switch (source->source_type) {
            case RUNTIME_SOURCE_STDIN: {
                FILE* fp = (FILE*)source->source_handle;
                if (!fp) fp = stdin;
                
                char buffer[1024];
                if (fgets(buffer, sizeof(buffer), fp)) {
                    size_t len = strlen(buffer);
                    if (len > 0 && buffer[len - 1] == '\n') {
                        buffer[len - 1] = '\0';
                        len--;
                    }
                    
                    if (len > 0) {
                        s.data = (unsigned char*)malloc(len);
                        if (s.data) {
                            memcpy(s.data, buffer, len);
                            s.length = len;
                        }
                    }
                }
                break;
            }
            
            case RUNTIME_SOURCE_FILE: {
                FILE* fp = (FILE*)source->source_handle;
                if (fp) {
                    // 读取文件内容（简化实现）
                    fseek(fp, 0, SEEK_END);
                    long file_size = ftell(fp);
                    fseek(fp, 0, SEEK_SET);
                    
                    if (file_size > 0 && file_size < 1024 * 1024) {  // 限制 1MB
                        s.data = (unsigned char*)malloc(file_size);
                        if (s.data) {
                            s.length = fread(s.data, 1, file_size, fp);
                        }
                    }
                }
                break;
            }
            
            default:
                // 其他源类型待实现
                break;
        }
    }
    
    return s;
}

// 保持向后兼容
bitstream capture_physical_signal(void) {
    return kos_capture_physical_signal(NULL);
}

// ========== 信号源管理 ==========

signal_source_t* kos_signal_source_create(runtime_signal_source type, void* handle, const char* name) {
    signal_source_t* source = (signal_source_t*)calloc(1, sizeof(signal_source_t));
    if (!source) {
        return NULL;
    }
    
    source->source_type = type;
    source->source_handle = handle;
    
    if (name) {
        size_t name_len = strlen(name);
        source->source_name = (const char*)malloc(name_len + 1);
        if (source->source_name) {
            strcpy((char*)source->source_name, name);
        }
    }
    
    return source;
}

void kos_signal_source_free(signal_source_t* source) {
    if (!source) {
        return;
    }
    
    if (source->source_name) {
        free((void*)source->source_name);
    }
    
    free(source);
}









































