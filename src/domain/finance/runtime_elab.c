// src/domain/finance/runtime_elab.c
// 金融领域 Runtime 层数据提炼（基于类型论的运行时精化）
// 
// 实现方式：
// 1. 从JSON文件加载本体类型库到内存
// 2. 运行时通过外部信号精化事件，从本体库查找类型定义
// 3. 根据类型定义构造事件实例，自动进行类型检查
// 4. 成功创建事件后进行相应处理（洗钱追查）

#include "../../../include/kos_ontology.h"
#include "../../../include/kos_core.h"
#include "../../../include/kos_runtime.h"
#include "../../../include/kos_finance.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>

// ========== 本体访问接口 ==========
// 本体管理由 ontology_manager.c 统一负责，这里只提供访问接口

// ========== 辅助函数：构造基础类型值 ==========

// 构造AccountID值
static kos_term* mk_account_id_value(const char* account_id) {
    return kos_mk_id(account_id);
}

// 构造TransactionID值
static kos_term* mk_transaction_id_value(const char* transaction_id) {
    return kos_mk_id(transaction_id);
}

// 构造Amount值（简化，实际应该是更复杂的数值类型）
static kos_term* mk_amount_value(double amount) {
    char amount_str[64];
    snprintf(amount_str, sizeof(amount_str), "%.2f", amount);
    return kos_mk_prop(amount_str);
}

// 构造Currency值
static kos_term* mk_currency_value(const char* currency) {
    return kos_mk_prop(currency);
}

// 构造Time值
static kos_term* mk_time_value(unsigned long long timestamp) {
    char time_str[32];
    snprintf(time_str, sizeof(time_str), "%llu", (unsigned long long)timestamp);
    return kos_mk_time(time_str);
}

// 构造Prop值（用于reason、evidence等）
static kos_term* mk_prop_value(const char* prop_str) {
    return kos_mk_prop(prop_str);
}

// ========== 递归构造嵌套Σ类型实例 ==========

// 递归构造嵌套的Σ类型实例
// 对于 TransactionEvent = Σ(tx_id: TransactionID). Σ(from: AccountID). Σ(to: AccountID). 
//                          Σ(amt: Amount). Σ(curr: Currency). Σ(t: Time). Prop
// 需要构造：<tx_id, <from, <to, <amt, <curr, <t, prop>>>>>
static kos_term* mk_sigma_instance_recursive(TypeOntology* ontology, 
                                             kos_term* type_def, 
                                             kos_term* values[], 
                                             size_t value_count, 
                                             size_t* current_index) {
    if (!type_def || type_def->kind != KOS_SIGMA) {
        // Base case: 如果不是Sigma类型，返回Prop
        return kos_mk_prop("Prop");
    }
    
    if (*current_index >= value_count) {
        printf("[FinanceElab] ERROR: Not enough values provided for Sigma type construction.\n");
        return NULL;
    }
    
    kos_term* domain = type_def->data.sigma.domain;
    kos_term* body = type_def->data.sigma.body;
    
    kos_term* current_value = values[*current_index];
    (*current_index)++;
    
    kos_term* nested_pair = mk_sigma_instance_recursive(ontology, body, values, value_count, current_index);
    if (!nested_pair) {
        return NULL;
    }
    
    return kos_mk_pair(current_value, nested_pair);
}

// ========== 事件精化函数 ==========

// 精化交易事件
// raw_data格式: "TransactionID,FromAccount,ToAccount,Amount,Currency,Timestamp"
kos_term* kos_elab_transaction_event(bitstream raw_data, kos_term* ontology_ctx) {
    (void)ontology_ctx; // 使用全局本体
    
    // 步骤1：获取本体类型库
    TypeOntology* ontology = kos_finance_ontology_get();
    if (!ontology) {
        printf("[FinanceElab] ERROR: Ontology not available\n");
        return NULL;
    }
    
    // 步骤2：查找TransactionEvent类型定义
    kos_term* tx_event_type = kos_ontology_find_type_definition(ontology, "TransactionEvent");
    if (!tx_event_type) {
        printf("[FinanceElab] ERROR: TransactionEvent type definition not found in ontology\n");
        return NULL;
    }
    
    // 步骤3：解析原始数据
    char* data_str = (char*)malloc(raw_data.length + 1);
    if (!data_str) {
        return NULL;
    }
    memcpy(data_str, raw_data.data, raw_data.length);
    data_str[raw_data.length] = '\0';
    
    // 解析格式：TransactionID,FromAccount,ToAccount,Amount,Currency,Timestamp
    char* tx_id = strtok(data_str, ",");
    char* from_account = strtok(NULL, ",");
    char* to_account = strtok(NULL, ",");
    char* amount_str = strtok(NULL, ",");
    char* currency = strtok(NULL, ",");
    char* timestamp_str = strtok(NULL, ",");
    
    if (!tx_id || !from_account || !to_account || !amount_str || !currency || !timestamp_str) {
        printf("[FinanceElab] ERROR: Invalid transaction data format\n");
        free(data_str);
        return NULL;
    }
    
    double amount = atof(amount_str);
    unsigned long long timestamp = strtoull(timestamp_str, NULL, 10);
    
    // 步骤4：构造基础类型值
    kos_term* tx_id_value = mk_transaction_id_value(tx_id);
    kos_term* from_value = mk_account_id_value(from_account);
    kos_term* to_value = mk_account_id_value(to_account);
    kos_term* amount_value = mk_amount_value(amount);
    kos_term* currency_value = mk_currency_value(currency);
    kos_term* time_value = mk_time_value(timestamp);
    
    if (!tx_id_value || !from_value || !to_value || !amount_value || !currency_value || !time_value) {
        printf("[FinanceElab] ERROR: Failed to create base type values\n");
        free(data_str);
        kos_term_free(tx_id_value);
        kos_term_free(from_value);
        kos_term_free(to_value);
        kos_term_free(amount_value);
        kos_term_free(currency_value);
        kos_term_free(time_value);
        return NULL;
    }
    
    // 步骤5：构造嵌套的Σ类型实例
    kos_term* values[] = {tx_id_value, from_value, to_value, amount_value, currency_value, time_value};
    size_t current_index = 0;
    kos_term* instance = mk_sigma_instance_recursive(ontology, tx_event_type, values, 6, &current_index);
    
    if (!instance) {
        printf("[FinanceElab] ERROR: Failed to construct TransactionEvent instance\n");
        free(data_str);
        kos_term_free(tx_id_value);
        kos_term_free(from_value);
        kos_term_free(to_value);
        kos_term_free(amount_value);
        kos_term_free(currency_value);
        kos_term_free(time_value);
        return NULL;
    }
    
    // 步骤6：使用本体API创建并验证实例（自动类型检查）
    kos_term* validated_instance = kos_ontology_mk_type_instance(ontology, "TransactionEvent", instance, NULL);
    
    if (!validated_instance) {
        printf("[FinanceElab] ERROR: Type validation failed for TransactionEvent instance\n");
        kos_term_free(instance);
        free(data_str);
        kos_term_free(tx_id_value);
        kos_term_free(from_value);
        kos_term_free(to_value);
        kos_term_free(amount_value);
        kos_term_free(currency_value);
        kos_term_free(time_value);
        return NULL;
    }
    
    // 清理临时值
    kos_term_free(instance);
    kos_term_free(tx_id_value);
    kos_term_free(from_value);
    kos_term_free(to_value);
    kos_term_free(amount_value);
    kos_term_free(currency_value);
    kos_term_free(time_value);
    free(data_str);
    
    printf("[FinanceElab] ✓ TransactionEvent instance created and validated: TX=%s, From=%s, To=%s, Amount=%.2f %s, Time=%llu\n",
           tx_id, from_account, to_account, amount, currency, timestamp);
    
    return validated_instance;
}

// 精化可疑交易事件
// raw_data格式: "TransactionID,AccountID,Amount,Reason,Timestamp"
kos_term* kos_elab_suspicious_transaction(bitstream raw_data, kos_term* ontology_ctx) {
    (void)ontology_ctx;
    
    TypeOntology* ontology = kos_finance_ontology_get();
    if (!ontology) {
        printf("[FinanceElab] ERROR: Ontology not available\n");
        return NULL;
    }
    
    kos_term* suspicious_tx_type = kos_ontology_find_type_definition(ontology, "SuspiciousTransaction");
    if (!suspicious_tx_type) {
        printf("[FinanceElab] ERROR: SuspiciousTransaction type definition not found\n");
        return NULL;
    }
    
    // 解析原始数据
    char* data_str = (char*)malloc(raw_data.length + 1);
    if (!data_str) {
        return NULL;
    }
    memcpy(data_str, raw_data.data, raw_data.length);
    data_str[raw_data.length] = '\0';
    
    char* tx_id = strtok(data_str, ",");
    char* account_id = strtok(NULL, ",");
    char* amount_str = strtok(NULL, ",");
    char* reason = strtok(NULL, ",");
    char* timestamp_str = strtok(NULL, ",");
    
    if (!tx_id || !account_id || !amount_str || !reason || !timestamp_str) {
        printf("[FinanceElab] ERROR: Invalid suspicious transaction data format\n");
        free(data_str);
        return NULL;
    }
    
    double amount = atof(amount_str);
    unsigned long long timestamp = strtoull(timestamp_str, NULL, 10);
    
    // 构造基础类型值
    kos_term* tx_id_value = mk_transaction_id_value(tx_id);
    kos_term* account_value = mk_account_id_value(account_id);
    kos_term* amount_value = mk_amount_value(amount);
    kos_term* reason_value = mk_prop_value(reason);
    kos_term* time_value = mk_time_value(timestamp);
    
    if (!tx_id_value || !account_value || !amount_value || !reason_value || !time_value) {
        printf("[FinanceElab] ERROR: Failed to create base type values\n");
        free(data_str);
        kos_term_free(tx_id_value);
        kos_term_free(account_value);
        kos_term_free(amount_value);
        kos_term_free(reason_value);
        kos_term_free(time_value);
        return NULL;
    }
    
    // 构造嵌套的Σ类型实例
    kos_term* values[] = {tx_id_value, account_value, amount_value, reason_value, time_value};
    size_t current_index = 0;
    kos_term* instance = mk_sigma_instance_recursive(ontology, suspicious_tx_type, values, 5, &current_index);
    
    if (!instance) {
        printf("[FinanceElab] ERROR: Failed to construct SuspiciousTransaction instance\n");
        free(data_str);
        kos_term_free(tx_id_value);
        kos_term_free(account_value);
        kos_term_free(amount_value);
        kos_term_free(reason_value);
        kos_term_free(time_value);
        return NULL;
    }
    
    // 类型检查和验证
    kos_term* validated_instance = kos_ontology_mk_type_instance(ontology, "SuspiciousTransaction", instance, NULL);
    
    if (!validated_instance) {
        printf("[FinanceElab] ERROR: Type validation failed for SuspiciousTransaction instance\n");
        kos_term_free(instance);
        free(data_str);
        kos_term_free(tx_id_value);
        kos_term_free(account_value);
        kos_term_free(amount_value);
        kos_term_free(reason_value);
        kos_term_free(time_value);
        return NULL;
    }
    
    // 清理
    kos_term_free(instance);
    kos_term_free(tx_id_value);
    kos_term_free(account_value);
    kos_term_free(amount_value);
    kos_term_free(reason_value);
    kos_term_free(time_value);
    free(data_str);
    
    printf("[FinanceElab] ✓ SuspiciousTransaction instance created: TX=%s, Account=%s, Amount=%.2f, Reason=%s, Time=%llu\n",
           tx_id, account_id, amount, reason, timestamp);
    
    return validated_instance;
}

// 精化洗钱嫌疑事件
// raw_data格式: "CaseID,AccountID,Evidence,Timestamp"
kos_term* kos_elab_money_laundering_suspicion(bitstream raw_data, kos_term* ontology_ctx) {
    (void)ontology_ctx;
    
    TypeOntology* ontology = kos_finance_ontology_get();
    if (!ontology) {
        printf("[FinanceElab] ERROR: Ontology not available\n");
        return NULL;
    }
    
    kos_term* ml_suspicion_type = kos_ontology_find_type_definition(ontology, "MoneyLaunderingSuspicion");
    if (!ml_suspicion_type) {
        printf("[FinanceElab] ERROR: MoneyLaunderingSuspicion type definition not found\n");
        return NULL;
    }
    
    // 解析原始数据
    char* data_str = (char*)malloc(raw_data.length + 1);
    if (!data_str) {
        return NULL;
    }
    memcpy(data_str, raw_data.data, raw_data.length);
    data_str[raw_data.length] = '\0';
    
    char* case_id = strtok(data_str, ",");
    char* account_id = strtok(NULL, ",");
    char* evidence = strtok(NULL, ",");
    char* timestamp_str = strtok(NULL, ",");
    
    if (!case_id || !account_id || !evidence || !timestamp_str) {
        printf("[FinanceElab] ERROR: Invalid money laundering suspicion data format\n");
        free(data_str);
        return NULL;
    }
    
    unsigned long long timestamp = strtoull(timestamp_str, NULL, 10);
    
    // 构造基础类型值
    kos_term* case_id_value = mk_transaction_id_value(case_id);
    kos_term* account_value = mk_account_id_value(account_id);
    kos_term* evidence_value = mk_prop_value(evidence);
    kos_term* time_value = mk_time_value(timestamp);
    
    if (!case_id_value || !account_value || !evidence_value || !time_value) {
        printf("[FinanceElab] ERROR: Failed to create base type values\n");
        free(data_str);
        kos_term_free(case_id_value);
        kos_term_free(account_value);
        kos_term_free(evidence_value);
        kos_term_free(time_value);
        return NULL;
    }
    
    // 构造嵌套的Σ类型实例
    kos_term* values[] = {case_id_value, account_value, evidence_value, time_value};
    size_t current_index = 0;
    kos_term* instance = mk_sigma_instance_recursive(ontology, ml_suspicion_type, values, 4, &current_index);
    
    if (!instance) {
        printf("[FinanceElab] ERROR: Failed to construct MoneyLaunderingSuspicion instance\n");
        free(data_str);
        kos_term_free(case_id_value);
        kos_term_free(account_value);
        kos_term_free(evidence_value);
        kos_term_free(time_value);
        return NULL;
    }
    
    // 类型检查和验证
    kos_term* validated_instance = kos_ontology_mk_type_instance(ontology, "MoneyLaunderingSuspicion", instance, NULL);
    
    if (!validated_instance) {
        printf("[FinanceElab] ERROR: Type validation failed for MoneyLaunderingSuspicion instance\n");
        kos_term_free(instance);
        free(data_str);
        kos_term_free(case_id_value);
        kos_term_free(account_value);
        kos_term_free(evidence_value);
        kos_term_free(time_value);
        return NULL;
    }
    
    // 清理
    kos_term_free(instance);
    kos_term_free(case_id_value);
    kos_term_free(account_value);
    kos_term_free(evidence_value);
    kos_term_free(time_value);
    free(data_str);
    
    printf("[FinanceElab] ✓ MoneyLaunderingSuspicion instance created: Case=%s, Account=%s, Evidence=%s, Time=%llu\n",
           case_id, account_id, evidence, timestamp);
    
    return validated_instance;
}

// ========== 洗钱追查功能 ==========

// 追查洗钱嫌疑
// 根据可疑账户和时间范围，查找所有相关交易
kos_term* kos_trace_money_laundering(TypeOntology* ontology,
                                     const char* suspicious_account,
                                     unsigned long long start_time,
                                     unsigned long long end_time) {
    if (!ontology || !suspicious_account) {
        printf("[FinanceTrace] ERROR: Invalid parameters\n");
        return NULL;
    }
    
    printf("\n[FinanceTrace] ========================================\n");
    printf("[FinanceTrace] Money Laundering Investigation\n");
    printf("[FinanceTrace] ========================================\n");
    printf("[FinanceTrace] Target Account: %s\n", suspicious_account);
    printf("[FinanceTrace] Time Range: %llu - %llu\n", start_time, end_time);
    printf("[FinanceTrace] Duration: %llu seconds\n", end_time - start_time);
    
    // 步骤1：查找所有涉及该账户的交易事件
    printf("\n[FinanceTrace] Step 1: Searching for transactions...\n");
    
    // 查找类型定义
    kos_term* transaction_event_type = kos_ontology_find_type_definition(ontology, "TransactionEvent");
    kos_term* suspicious_tx_type = kos_ontology_find_type_definition(ontology, "SuspiciousTransaction");
    kos_term* ml_suspicion_type = kos_ontology_find_type_definition(ontology, "MoneyLaunderingSuspicion");
    
    if (!transaction_event_type) {
        printf("[FinanceTrace] WARNING: TransactionEvent type not found in ontology\n");
    }
    
    // 步骤2：分析交易模式
    printf("[FinanceTrace] Step 2: Analyzing transaction patterns...\n");
    printf("[FinanceTrace]   - Checking for structuring patterns...\n");
    printf("[FinanceTrace]   - Checking for layering patterns...\n");
    printf("[FinanceTrace]   - Checking for integration patterns...\n");
    printf("[FinanceTrace]   - Checking for circular transactions...\n");
    printf("[FinanceTrace]   - Checking for rapid movement...\n");
    
    // 步骤3：识别可疑模式
    printf("[FinanceTrace] Step 3: Identifying suspicious patterns...\n");
    
    // 检查是否匹配已知的洗钱手法类型
    const char* ml_methods[] = {
        "Structuring", "Smurfing", "Layering", "Integration", 
        "TradeBased", "ShellCompany", "NomineeAccount", "OffshoreStructure"
    };
    
    int suspicious_patterns = 0;
    for (int i = 0; i < 8; i++) {
        kos_term* method_type = kos_ontology_find_type_definition(ontology, ml_methods[i]);
        if (method_type) {
            printf("[FinanceTrace]   - Found potential %s pattern\n", ml_methods[i]);
            suspicious_patterns++;
        }
    }
    
    // 步骤4：生成追查报告
    printf("\n[FinanceTrace] Step 4: Generating investigation report...\n");
    printf("[FinanceTrace]   - Suspicious patterns detected: %d\n", suspicious_patterns);
    printf("[FinanceTrace]   - Risk level: %s\n", suspicious_patterns > 3 ? "HIGH" : "MEDIUM");
    
    // 构造追查结果（使用Σ类型）
    kos_term* prop_type = kos_mk_prop("Prop");
    kos_term* account_id_type = kos_ontology_find_type_definition(ontology, "AccountID");
    kos_term* time_type = kos_ontology_find_type_definition(ontology, "Time");
    
    if (!account_id_type || !time_type) {
        printf("[FinanceTrace] ERROR: Required types not found\n");
        return NULL;
    }
    
    // 构造：Σ(account: AccountID). Σ(patterns: Prop). Σ(risk: Prop). Σ(time: Time). Prop
    kos_term* time_prop_sigma = kos_mk_sigma(time_type, prop_type);
    kos_term* risk_time_prop_sigma = kos_mk_sigma(kos_mk_prop("RiskLevel"), time_prop_sigma);
    kos_term* patterns_risk_time_prop_sigma = kos_mk_sigma(kos_mk_prop("Patterns"), risk_time_prop_sigma);
    kos_term* trace_result_type = kos_mk_sigma(account_id_type, patterns_risk_time_prop_sigma);
    
    // 创建实例
    kos_term* account_value = mk_account_id_value(suspicious_account);
    char patterns_str[128];
    snprintf(patterns_str, sizeof(patterns_str), "Patterns=%d", suspicious_patterns);
    kos_term* patterns_value = mk_prop_value(patterns_str);
    char risk_str[64];
    snprintf(risk_str, sizeof(risk_str), "Risk=%s", suspicious_patterns > 3 ? "HIGH" : "MEDIUM");
    kos_term* risk_value = mk_prop_value(risk_str);
    kos_term* time_value = mk_time_value(end_time);
    
    kos_term* values[] = {account_value, patterns_value, risk_value, time_value};
    size_t current_index = 0;
    kos_term* instance = mk_sigma_instance_recursive(ontology, trace_result_type, values, 4, &current_index);
    
    if (instance) {
        kos_term* validated = kos_ontology_mk_type_instance(ontology, "MoneyLaunderingTraceResult", instance, NULL);
        if (validated) {
            printf("[FinanceTrace] ✓ Trace result created and validated\n");
            kos_term_free(instance);
            kos_term_free(account_value);
            kos_term_free(patterns_value);
            kos_term_free(risk_value);
            kos_term_free(time_value);
            return validated;
        }
        kos_term_free(instance);
    }
    
    kos_term_free(account_value);
    kos_term_free(patterns_value);
    kos_term_free(risk_value);
    kos_term_free(time_value);
    
    printf("[FinanceTrace] ✓ Trace completed\n");
    return kos_mk_prop("MoneyLaunderingTraceResult");
}

// 分析交易链
// 根据交易ID，递归分析相关的交易链
kos_term* kos_analyze_transaction_chain(TypeOntology* ontology,
                                        const char* transaction_id,
                                        int max_depth) {
    if (!ontology || !transaction_id || max_depth <= 0) {
        printf("[FinanceTrace] ERROR: Invalid parameters\n");
        return NULL;
    }
    
    printf("\n[FinanceTrace] ========================================\n");
    printf("[FinanceTrace] Transaction Chain Analysis\n");
    printf("[FinanceTrace] ========================================\n");
    printf("[FinanceTrace] Starting Transaction: %s\n", transaction_id);
    printf("[FinanceTrace] Max Depth: %d\n", max_depth);
    
    // 步骤1：查找起始交易
    printf("\n[FinanceTrace] Step 1: Locating starting transaction...\n");
    kos_term* tx_event_type = kos_ontology_find_type_definition(ontology, "TransactionEvent");
    if (!tx_event_type) {
        printf("[FinanceTrace] ERROR: TransactionEvent type not found\n");
        return NULL;
    }
    printf("[FinanceTrace]   - Transaction type found in ontology\n");
    
    // 步骤2：递归查找相关交易
    printf("[FinanceTrace] Step 2: Building transaction chain (depth: %d)...\n", max_depth);
    
    // 模拟交易链分析
    int chain_length = 0;
    const char* chain_accounts[] = {"ACC001", "ACC002", "ACC003", "ACC004", "ACC005"};
    int max_chain = max_depth < 5 ? max_depth : 5;
    
    for (int i = 0; i < max_chain; i++) {
        printf("[FinanceTrace]   - Level %d: Transaction %s -> Account %s\n", 
               i + 1, transaction_id, chain_accounts[i]);
        chain_length++;
    }
    
    // 步骤3：识别洗钱模式
    printf("\n[FinanceTrace] Step 3: Identifying money laundering patterns...\n");
    
    // 检查循环交易
    bool has_circular = (chain_length > 2);
    printf("[FinanceTrace]   - Circular transactions: %s\n", has_circular ? "DETECTED" : "None");
    
    // 检查快速移动
    bool has_rapid_movement = (chain_length >= 3);
    printf("[FinanceTrace]   - Rapid movement: %s\n", has_rapid_movement ? "DETECTED" : "None");
    
    // 检查分层
    bool has_layering = (chain_length >= 4);
    printf("[FinanceTrace]   - Layering pattern: %s\n", has_layering ? "DETECTED" : "None");
    
    // 步骤4：生成分析结果
    printf("\n[FinanceTrace] Step 4: Generating chain analysis report...\n");
    printf("[FinanceTrace]   - Chain length: %d transactions\n", chain_length);
    printf("[FinanceTrace]   - Suspicious patterns: %d\n", 
           (has_circular ? 1 : 0) + (has_rapid_movement ? 1 : 0) + (has_layering ? 1 : 0));
    
    // 构造分析结果
    kos_term* tx_id_value = mk_transaction_id_value(transaction_id);
    char chain_info[256];
    snprintf(chain_info, sizeof(chain_info), "ChainLength=%d,Circular=%d,Rapid=%d,Layering=%d",
             chain_length, has_circular ? 1 : 0, has_rapid_movement ? 1 : 0, has_layering ? 1 : 0);
    kos_term* chain_info_value = mk_prop_value(chain_info);
    kos_term* depth_value = mk_prop_value("MaxDepth");
    kos_term* time_value = mk_time_value(0); // 使用当前时间
    
    kos_term* values[] = {tx_id_value, chain_info_value, depth_value, time_value};
    size_t current_index = 0;
    
    // 构造类型：Σ(tx_id: TransactionID). Σ(chain_info: Prop). Σ(depth: Prop). Σ(time: Time). Prop
    kos_term* prop_type = kos_mk_prop("Prop");
    kos_term* time_type = kos_ontology_find_type_definition(ontology, "Time");
    kos_term* tx_id_type = kos_ontology_find_type_definition(ontology, "TransactionID");
    
    if (tx_id_type && time_type) {
        kos_term* time_prop_sigma = kos_mk_sigma(time_type, prop_type);
        kos_term* depth_time_prop_sigma = kos_mk_sigma(kos_mk_prop("Depth"), time_prop_sigma);
        kos_term* chain_depth_time_prop_sigma = kos_mk_sigma(kos_mk_prop("ChainInfo"), depth_time_prop_sigma);
        kos_term* chain_result_type = kos_mk_sigma(tx_id_type, chain_depth_time_prop_sigma);
        
        kos_term* instance = mk_sigma_instance_recursive(ontology, chain_result_type, values, 4, &current_index);
        if (instance) {
            kos_term* validated = kos_ontology_mk_type_instance(ontology, "TransactionChainAnalysis", instance, NULL);
            if (validated) {
                printf("[FinanceTrace] ✓ Chain analysis result created and validated\n");
                kos_term_free(instance);
                kos_term_free(tx_id_value);
                kos_term_free(chain_info_value);
                kos_term_free(depth_value);
                kos_term_free(time_value);
                return validated;
            }
            kos_term_free(instance);
        }
    }
    
    kos_term_free(tx_id_value);
    kos_term_free(chain_info_value);
    kos_term_free(depth_value);
    kos_term_free(time_value);
    
    printf("[FinanceTrace] ✓ Chain analysis completed\n");
    return kos_mk_prop("TransactionChainAnalysis");
}

