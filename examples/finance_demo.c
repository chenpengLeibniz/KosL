// examples/finance_demo.c
// 金融洗钱追查演示程序
// 演示如何使用类型论系统进行金融洗钱嫌疑追查和交易链分析

#include "../include/kos_finance.h"
#include "../include/kos_ontology.h"
#include "../include/kos_core.h"
#include "../include/kos_runtime.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// 辅助函数：创建原始数据流
static bitstream create_bitstream(const char* data) {
    bitstream bs;
    bs.length = strlen(data);
    bs.data = (unsigned char*)malloc(bs.length);
    if (bs.data) {
        memcpy(bs.data, data, bs.length);
    }
    return bs;
}

// 辅助函数：释放数据流
static void free_bitstream(bitstream* bs) {
    if (bs && bs->data) {
        free(bs->data);
        bs->data = NULL;
        bs->length = 0;
    }
}

int main(int argc, char* argv[]) {
    printf("========================================\n");
    printf("金融洗钱追查演示程序\n");
    printf("========================================\n\n");
    
    // 步骤1：初始化金融本体
    printf("[Demo] Step 1: 初始化金融领域本体...\n");
    TypeOntology* ontology = kos_finance_ontology_init();
    if (!ontology) {
        fprintf(stderr, "[Demo] ERROR: 无法初始化金融本体\n");
        return 1;
    }
    printf("[Demo] ✓ 本体初始化成功，包含 %zu 个类型定义\n\n", ontology->type_count);
    
    // 步骤2：创建交易事件
    printf("[Demo] Step 2: 创建交易事件...\n");
    
    // 创建交易事件1：正常交易
    const char* tx1_data = "TX001,ACC001,ACC002,10000.00,USD,1704067200";
    bitstream tx1_stream = create_bitstream(tx1_data);
    kos_term* tx1_event = kos_elab_transaction_event(tx1_stream, NULL);
    free_bitstream(&tx1_stream);
    
    if (tx1_event) {
        printf("[Demo] ✓ 交易事件1创建成功: TX001\n");
    } else {
        printf("[Demo] ✗ 交易事件1创建失败\n");
    }
    
    // 创建交易事件2：大额交易
    const char* tx2_data = "TX002,ACC003,ACC004,500000.00,USD,1704070800";
    bitstream tx2_stream = create_bitstream(tx2_data);
    kos_term* tx2_event = kos_elab_transaction_event(tx2_stream, NULL);
    free_bitstream(&tx2_stream);
    
    if (tx2_event) {
        printf("[Demo] ✓ 交易事件2创建成功: TX002 (大额交易)\n");
    } else {
        printf("[Demo] ✗ 交易事件2创建失败\n");
    }
    
    // 创建交易事件3：跨境交易
    const char* tx3_data = "TX003,ACC005,ACC006,250000.00,EUR,1704074400";
    bitstream tx3_stream = create_bitstream(tx3_data);
    kos_term* tx3_event = kos_elab_transaction_event(tx3_stream, NULL);
    free_bitstream(&tx3_stream);
    
    if (tx3_event) {
        printf("[Demo] ✓ 交易事件3创建成功: TX003 (跨境交易)\n");
    } else {
        printf("[Demo] ✗ 交易事件3创建失败\n");
    }
    
    printf("\n");
    
    // 步骤3：创建可疑交易
    printf("[Demo] Step 3: 创建可疑交易事件...\n");
    
    const char* suspicious_data = "TX004,ACC007,50000.00,StructuredTransactions,1704078000";
    bitstream suspicious_stream = create_bitstream(suspicious_data);
    kos_term* suspicious_tx = kos_elab_suspicious_transaction(suspicious_stream, NULL);
    free_bitstream(&suspicious_stream);
    
    if (suspicious_tx) {
        printf("[Demo] ✓ 可疑交易事件创建成功: TX004\n");
    } else {
        printf("[Demo] ✗ 可疑交易事件创建失败\n");
    }
    
    printf("\n");
    
    // 步骤4：创建洗钱嫌疑
    printf("[Demo] Step 4: 创建洗钱嫌疑事件...\n");
    
    const char* ml_data = "CASE001,ACC007,MultipleSuspiciousPatterns,1704081600";
    bitstream ml_stream = create_bitstream(ml_data);
    kos_term* ml_suspicion = kos_elab_money_laundering_suspicion(ml_stream, NULL);
    free_bitstream(&ml_stream);
    
    if (ml_suspicion) {
        printf("[Demo] ✓ 洗钱嫌疑事件创建成功: CASE001\n");
    } else {
        printf("[Demo] ✗ 洗钱嫌疑事件创建失败\n");
    }
    
    printf("\n");
    
    // 步骤5：追查洗钱嫌疑
    printf("[Demo] Step 5: 追查洗钱嫌疑...\n");
    printf("[Demo] ========================================\n");
    
    unsigned long long start_time = 1704067200;  // 2024-01-01 00:00:00
    unsigned long long end_time = 1704153600;    // 2024-01-02 00:00:00
    
    kos_term* trace_result = kos_trace_money_laundering(
        ontology,
        "ACC007",  // 可疑账户
        start_time,
        end_time
    );
    
    if (trace_result) {
        printf("[Demo] ✓ 洗钱追查完成\n");
        kos_term_free(trace_result);
    } else {
        printf("[Demo] ✗ 洗钱追查失败\n");
    }
    
    printf("\n");
    
    // 步骤6：分析交易链
    printf("[Demo] Step 6: 分析交易链...\n");
    printf("[Demo] ========================================\n");
    
    kos_term* chain_result = kos_analyze_transaction_chain(
        ontology,
        "TX001",  // 起始交易
        5         // 最大深度
    );
    
    if (chain_result) {
        printf("[Demo] ✓ 交易链分析完成\n");
        kos_term_free(chain_result);
    } else {
        printf("[Demo] ✗ 交易链分析失败\n");
    }
    
    printf("\n");
    
    // 清理资源
    printf("[Demo] 清理资源...\n");
    if (tx1_event) kos_term_free(tx1_event);
    if (tx2_event) kos_term_free(tx2_event);
    if (tx3_event) kos_term_free(tx3_event);
    if (suspicious_tx) kos_term_free(suspicious_tx);
    if (ml_suspicion) kos_term_free(ml_suspicion);
    
    kos_finance_ontology_release();
    
    printf("\n[Demo] ========================================\n");
    printf("[Demo] 演示程序执行完成\n");
    printf("[Demo] ========================================\n");
    
    return 0;
}
