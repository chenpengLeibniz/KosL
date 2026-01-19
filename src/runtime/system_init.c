// src/runtime/system_init.c
// 系统初始化函数

#include "kos_runtime.h"
#include "kos_kernel.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// 初始化系统（创建初始本体）
kos_state_t init_system(void) {
    kos_state_t sigma;
    sigma.K = NULL;  // 初始知识集为空
    sigma.TS = 0;    // 逻辑时钟初始化为 0
    
    // 初始化事件队列
    sigma.P = (queue_t*)calloc(1, sizeof(queue_t));
    if (sigma.P) {
        sigma.P->front = NULL;
        sigma.P->rear = NULL;
        sigma.P->size = 0;
    }
    
    return sigma;
}

// 捕获物理信号（简化实现：从标准输入读取）
bitstream capture_physical_signal(void) {
    bitstream s;
    s.data = NULL;
    s.length = 0;
    
    // 简化实现：从标准输入读取一行数据
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
    
    return s;
}









































