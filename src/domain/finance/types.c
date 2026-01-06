// src/domain/finance/types.c
// 金融领域类型构建器函数
// 提供便捷的类型实例构建函数，基于本体类型定义

#include "../../../include/kos_finance.h"
#include "../../../include/kos_ontology.h"
#include "../../../include/kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// 创建 AccountID
kos_term* kos_mk_account_id(const char* account_id) {
    if (!account_id) {
        return NULL;
    }
    
    // 尝试从本体获取类型定义
    TypeOntology* ontology = kos_finance_ontology_get();
    if (ontology) {
        kos_term* account_id_term = kos_mk_id(account_id);
        if (account_id_term) {
            // 验证是否符合AccountID类型定义
            kos_term* validated = kos_ontology_mk_type_instance(ontology, "AccountID", account_id_term, NULL);
            if (validated) {
                kos_term_free(account_id_term);
                return validated;
            }
            kos_term_free(account_id_term);
        }
    }
    
    // 回退到直接构建
    return kos_mk_id(account_id);
}

// 创建 TransactionID
kos_term* kos_mk_transaction_id(const char* transaction_id) {
    if (!transaction_id) {
        return NULL;
    }
    
    TypeOntology* ontology = kos_finance_ontology_get();
    if (ontology) {
        kos_term* tx_id_term = kos_mk_id(transaction_id);
        if (tx_id_term) {
            kos_term* validated = kos_ontology_mk_type_instance(ontology, "TransactionID", tx_id_term, NULL);
            if (validated) {
                kos_term_free(tx_id_term);
                return validated;
            }
            kos_term_free(tx_id_term);
        }
    }
    
    return kos_mk_id(transaction_id);
}

// 创建 Amount
kos_term* kos_mk_amount(double value, const char* currency) {
    char amount_str[128];
    snprintf(amount_str, sizeof(amount_str), "%.2f %s", value, currency ? currency : "USD");
    return kos_mk_prop(amount_str);
}

// 创建 BankID
kos_term* kos_mk_bank_id(const char* bank_id) {
    if (!bank_id) {
        return NULL;
    }
    
    TypeOntology* ontology = kos_finance_ontology_get();
    if (ontology) {
        kos_term* bank_id_term = kos_mk_id(bank_id);
        if (bank_id_term) {
            kos_term* validated = kos_ontology_mk_type_instance(ontology, "BankID", bank_id_term, NULL);
            if (validated) {
                kos_term_free(bank_id_term);
                return validated;
            }
            kos_term_free(bank_id_term);
        }
    }
    
    return kos_mk_id(bank_id);
}

// 创建 PersonID
kos_term* kos_mk_person_id(const char* person_id) {
    if (!person_id) {
        return NULL;
    }
    
    TypeOntology* ontology = kos_finance_ontology_get();
    if (ontology) {
        kos_term* person_id_term = kos_mk_id(person_id);
        if (person_id_term) {
            kos_term* validated = kos_ontology_mk_type_instance(ontology, "PersonID", person_id_term, NULL);
            if (validated) {
                kos_term_free(person_id_term);
                return validated;
            }
            kos_term_free(person_id_term);
        }
    }
    
    return kos_mk_id(person_id);
}

// 创建 CompanyID
kos_term* kos_mk_company_id(const char* company_id) {
    if (!company_id) {
        return NULL;
    }
    
    TypeOntology* ontology = kos_finance_ontology_get();
    if (ontology) {
        kos_term* company_id_term = kos_mk_id(company_id);
        if (company_id_term) {
            kos_term* validated = kos_ontology_mk_type_instance(ontology, "CompanyID", company_id_term, NULL);
            if (validated) {
                kos_term_free(company_id_term);
                return validated;
            }
            kos_term_free(company_id_term);
        }
    }
    
    return kos_mk_id(company_id);
}

// 创建交易事件（简化版本，实际应该使用runtime_elab.c中的完整实现）
kos_term* kos_mk_transaction_event(const char* transaction_id, 
                                   const char* from_account,
                                   const char* to_account,
                                   double amount,
                                   const char* currency,
                                   unsigned long long timestamp) {
    // 这里只是占位符，实际应该调用runtime_elab.c中的kos_elab_transaction_event
    (void)transaction_id;
    (void)from_account;
    (void)to_account;
    (void)amount;
    (void)currency;
    (void)timestamp;
    
    return kos_mk_prop("TransactionEvent");
}

// 创建可疑交易（简化版本）
kos_term* kos_mk_suspicious_transaction(const char* transaction_id,
                                       const char* account_id,
                                       double amount,
                                       const char* reason,
                                       unsigned long long timestamp) {
    (void)transaction_id;
    (void)account_id;
    (void)amount;
    (void)reason;
    (void)timestamp;
    
    return kos_mk_prop("SuspiciousTransaction");
}

// 创建洗钱嫌疑（简化版本）
kos_term* kos_mk_money_laundering_suspicion(const char* case_id,
                                            const char* account_id,
                                            const char* evidence,
                                            unsigned long long timestamp) {
    (void)case_id;
    (void)account_id;
    (void)evidence;
    (void)timestamp;
    
    return kos_mk_prop("MoneyLaunderingSuspicion");
}

