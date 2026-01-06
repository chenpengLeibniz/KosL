// examples/test_storage.c
// 演示 Core 层类型构建、存储和加载工具的使用

#include "../include/kos_core.h"
#include <stdio.h>
#include <stdlib.h>

int main() {
    printf("=== KOS Core Layer Tools Demo ===\n\n");
    
    // 1. 类型构建示例
    printf("1. Type Builder Demo:\n");
    
    // 创建一个原子值
    kos_term* val = kos_mk_val("hello");
    printf("   Created atomic value: %s\n", val ? val->data.atomic.val : "NULL");
    
    // 创建一个命题
    kos_term* prop = kos_mk_prop("IsVerified(alice)");
    printf("   Created proposition: %s\n", prop ? prop->data.atomic.val : "NULL");
    
    // 创建一个 Σ-Type 对 <data, proof>
    kos_term* pair = kos_mk_pair(val, prop);
    printf("   Created Σ-Type pair: <data, proof>\n");
    
    // 2. 序列化示例
    printf("\n2. Serialization Demo:\n");
    kos_serialized* serialized = kos_term_serialize(pair);
    if (serialized) {
        printf("   Serialized JSON (length=%zu):\n", serialized->length);
        printf("   %s\n", serialized->data);
        kos_serialized_free(serialized);
    }
    
    // 3. 存储到文件示例
    printf("\n3. File Storage Demo:\n");
    const char* filename = "test_knowledge.json";
    if (kos_term_save_to_file(pair, filename) == 0) {
        printf("   Successfully saved to: %s\n", filename);
    } else {
        printf("   Failed to save to file\n");
    }
    
    // 4. 复制示例
    printf("\n4. Copy Demo:\n");
    kos_term* copy = kos_term_copy(pair);
    if (copy) {
        printf("   Successfully copied term\n");
        kos_term_free(copy);
    }
    
    // 5. 清理资源
    printf("\n5. Cleanup:\n");
    kos_term_free(pair); // 这会递归释放 val 和 prop
    printf("   Resources freed\n");
    
    printf("\n=== Demo Complete ===\n");
    return 0;
}


























