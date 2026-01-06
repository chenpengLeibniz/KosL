// include/kos_finance.h
// 金融领域类型定义和接口
// 用于金融洗钱追查案例

#ifndef KOS_FINANCE_H
#define KOS_FINANCE_H

#include "kos_ontology.h"
#include "kos_core.h"
#include "kos_runtime.h"
#include <stdbool.h>
#include <stddef.h>

// ========== 金融领域基础类型 ==========
// 注意：这些类型定义通过类型构造器（Π、Σ、Sum等）构造，而不是C语言结构体

// 基础类型（通过类型构造器定义）：
// - AccountID: 账户标识符
// - TransactionID: 交易标识符
// - Amount: 金额
// - Currency: 货币类型
// - BankID: 银行标识符
// - CountryCode: 国家代码
// - PersonID: 个人标识符
// - CompanyID: 公司标识符
// - Time: 时间戳

// ========== 金融领域接口 ==========

// 本体管理
TypeOntology* kos_finance_ontology_init(void);
int kos_finance_ontology_save(TypeOntology* ontology);

// 本体访问（单例模式）
TypeOntology* kos_finance_ontology_get(void);
void kos_finance_ontology_release(void);
int kos_finance_ontology_reload(void);
bool kos_finance_ontology_is_loaded(void);
size_t kos_finance_ontology_get_type_count(void);

// 类型构建器（基于类型本体）
kos_term* kos_mk_account_id(const char* account_id);
kos_term* kos_mk_transaction_id(const char* transaction_id);
kos_term* kos_mk_amount(double value, const char* currency);
kos_term* kos_mk_bank_id(const char* bank_id);
kos_term* kos_mk_person_id(const char* person_id);
kos_term* kos_mk_company_id(const char* company_id);

// 事件类型构建器
kos_term* kos_mk_transaction_event(const char* transaction_id, 
                                   const char* from_account,
                                   const char* to_account,
                                   double amount,
                                   const char* currency,
                                   unsigned long long timestamp);
kos_term* kos_mk_suspicious_transaction(const char* transaction_id,
                                       const char* account_id,
                                       double amount,
                                       const char* reason,
                                       unsigned long long timestamp);
kos_term* kos_mk_money_laundering_suspicion(const char* case_id,
                                            const char* account_id,
                                            const char* evidence,
                                            unsigned long long timestamp);

// 事件精化接口（Runtime Elaboration）
kos_term* kos_elab_transaction_event(bitstream raw_data, kos_term* ontology_ctx);
kos_term* kos_elab_suspicious_transaction(bitstream raw_data, kos_term* ontology_ctx);
kos_term* kos_elab_money_laundering_suspicion(bitstream raw_data, kos_term* ontology_ctx);

// 洗钱追查接口
kos_term* kos_trace_money_laundering(TypeOntology* ontology,
                                    const char* suspicious_account,
                                    unsigned long long start_time,
                                    unsigned long long end_time);
kos_term* kos_analyze_transaction_chain(TypeOntology* ontology,
                                       const char* transaction_id,
                                       int max_depth);

#endif // KOS_FINANCE_H

