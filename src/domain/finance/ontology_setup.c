// src/domain/finance/ontology_setup.c
// 金融领域类型本体初始化（基于类型构造）
// 使用类型构造器（Π、Σ、Sum等）构造所有类型定义

#include "../../../include/kos_ontology.h"
#include "../../../include/kos_core.h"
#include "../../../include/kos_finance.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// 金融领域类型本体文件路径
#define FINANCE_ONTOLOGY_FILE "finance_ontology.json"

// 声明：添加生成的类型定义
extern void kos_finance_ontology_add_generated_types(TypeOntology* ontology);

// 初始化金融领域类型本体
// 如果文件存在则加载，否则创建默认本体（使用类型构造器）
TypeOntology* kos_finance_ontology_init(void) {
    // 尝试从文件加载
    TypeOntology* ontology = kos_ontology_load_from_file(FINANCE_ONTOLOGY_FILE);
    
    if (ontology) {
        printf("[FinanceSetup] ✓ Loaded ontology from %s (%zu types)\n", 
               FINANCE_ONTOLOGY_FILE, ontology->type_count);
        return ontology;
    }
    
    // 文件不存在，创建默认本体
    printf("[FinanceSetup] Creating default finance ontology...\n");
    ontology = kos_ontology_create("finance");
    if (!ontology) {
        printf("[FinanceSetup] ✗ Failed to create ontology\n");
        return NULL;
    }
    
    // ========== 基础类型定义 ==========
    
    // AccountID: 账户标识符
    kos_term* account_id_type = kos_mk_id("AccountID");
    kos_ontology_add_type_definition(ontology, "AccountID", account_id_type, NULL);
    
    // TransactionID: 交易标识符
    kos_term* transaction_id_type = kos_mk_id("TransactionID");
    kos_ontology_add_type_definition(ontology, "TransactionID", transaction_id_type, NULL);
    
    // Amount: 金额（简化，实际应该是更复杂的数值类型）
    kos_term* amount_type = kos_mk_prop("Amount");
    kos_ontology_add_type_definition(ontology, "Amount", amount_type, NULL);
    
    // Currency: 货币类型
    kos_term* currency_type = kos_mk_prop("Currency");
    kos_ontology_add_type_definition(ontology, "Currency", currency_type, NULL);
    
    // BankID: 银行标识符
    kos_term* bank_id_type = kos_mk_id("BankID");
    kos_ontology_add_type_definition(ontology, "BankID", bank_id_type, NULL);
    
    // CountryCode: 国家代码
    kos_term* country_code_type = kos_mk_prop("CountryCode");
    kos_ontology_add_type_definition(ontology, "CountryCode", country_code_type, NULL);
    
    // PersonID: 个人标识符
    kos_term* person_id_type = kos_mk_id("PersonID");
    kos_ontology_add_type_definition(ontology, "PersonID", person_id_type, NULL);
    
    // CompanyID: 公司标识符
    kos_term* company_id_type = kos_mk_id("CompanyID");
    kos_ontology_add_type_definition(ontology, "CompanyID", company_id_type, NULL);
    
    // Time: 时间戳
    kos_term* time_type = kos_mk_time("Time");
    kos_ontology_add_type_definition(ontology, "Time", time_type, NULL);
    
    // ========== 事件类型定义 ==========
    
    // TransactionEvent: 交易事件
    // TransactionEvent = Σ(tx_id: TransactionID). Σ(from: AccountID). Σ(to: AccountID). 
    //                    Σ(amt: Amount). Σ(curr: Currency). Σ(t: Time). Prop
    kos_term* tx_id_domain = kos_ontology_find_type_definition(ontology, "TransactionID");
    kos_term* from_domain = kos_ontology_find_type_definition(ontology, "AccountID");
    kos_term* to_domain = kos_ontology_find_type_definition(ontology, "AccountID");
    kos_term* amt_domain = kos_ontology_find_type_definition(ontology, "Amount");
    kos_term* curr_domain = kos_ontology_find_type_definition(ontology, "Currency");
    kos_term* time_domain = kos_ontology_find_type_definition(ontology, "Time");
    kos_term* prop_final = kos_mk_prop("Prop");
    
    // 嵌套的Σ类型构造
    kos_term* transaction_event_type = kos_mk_sigma(tx_id_domain,
        kos_mk_sigma(from_domain,
            kos_mk_sigma(to_domain,
                kos_mk_sigma(amt_domain,
                    kos_mk_sigma(curr_domain,
                        kos_mk_sigma(time_domain, prop_final))))));
    kos_ontology_add_type_definition(ontology, "TransactionEvent", transaction_event_type, NULL);
    
    // SuspiciousTransaction: 可疑交易
    // SuspiciousTransaction = Σ(tx_id: TransactionID). Σ(acc: AccountID). 
    //                         Σ(amt: Amount). Σ(reason: Prop). Σ(t: Time). Prop
    kos_term* suspicious_tx_type = kos_mk_sigma(tx_id_domain,
        kos_mk_sigma(from_domain,
            kos_mk_sigma(amt_domain,
                kos_mk_sigma(kos_mk_prop("Reason"),
                    kos_mk_sigma(time_domain, prop_final)))));
    kos_ontology_add_type_definition(ontology, "SuspiciousTransaction", suspicious_tx_type, NULL);
    
    // MoneyLaunderingSuspicion: 洗钱嫌疑
    // MoneyLaunderingSuspicion = Σ(case_id: TransactionID). Σ(acc: AccountID). 
    //                            Σ(evidence: Prop). Σ(t: Time). Prop
    kos_term* ml_suspicion_type = kos_mk_sigma(tx_id_domain,
        kos_mk_sigma(from_domain,
            kos_mk_sigma(kos_mk_prop("Evidence"),
                kos_mk_sigma(time_domain, prop_final))));
    kos_ontology_add_type_definition(ontology, "MoneyLaunderingSuspicion", ml_suspicion_type, NULL);
    
    // ========== 谓词类型定义 ==========
    
    // IsHighValueTransaction: 是否为大额交易
    // IsHighValueTransaction = Π(tx: TransactionEvent). Prop
    kos_term* is_high_value_type = kos_mk_pi(
        kos_ontology_find_type_definition(ontology, "TransactionEvent"),
        kos_mk_prop("Prop"));
    kos_ontology_add_type_definition(ontology, "IsHighValueTransaction", is_high_value_type, NULL);
    
    // IsCrossBorder: 是否跨境交易
    // IsCrossBorder = Π(tx: TransactionEvent). Prop
    kos_term* is_cross_border_type = kos_mk_pi(
        kos_ontology_find_type_definition(ontology, "TransactionEvent"),
        kos_mk_prop("Prop"));
    kos_ontology_add_type_definition(ontology, "IsCrossBorder", is_cross_border_type, NULL);
    
    // IsRelatedAccount: 是否关联账户
    // IsRelatedAccount = Π(acc1: AccountID). Π(acc2: AccountID). Prop
    kos_term* is_related_type = kos_mk_pi(from_domain,
        kos_mk_pi(to_domain, kos_mk_prop("Prop")));
    kos_ontology_add_type_definition(ontology, "IsRelatedAccount", is_related_type, NULL);
    
    // ========== 添加生成的类型定义 ==========
    // 这将添加数千个金融领域常见类型
    kos_finance_ontology_add_generated_types(ontology);
    
    // 保存本体到文件
    if (kos_ontology_save_to_file(ontology, FINANCE_ONTOLOGY_FILE) == 0) {
        printf("[FinanceSetup] ✓ Saved ontology to %s\n", FINANCE_ONTOLOGY_FILE);
    } else {
        printf("[FinanceSetup] ⚠ Failed to save ontology to %s\n", FINANCE_ONTOLOGY_FILE);
    }
    
    printf("[FinanceSetup] ✓ Finance ontology initialized (%zu types)\n", ontology->type_count);
    return ontology;
}

// 保存金融本体到文件
int kos_finance_ontology_save(TypeOntology* ontology) {
    if (!ontology) {
        return -1;
    }
    return kos_ontology_save_to_file(ontology, FINANCE_ONTOLOGY_FILE);
}

