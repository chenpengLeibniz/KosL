// src/domain/finance/ontology_setup.c
// 金融领域类型本体初始化（基于类型构造）
// 优先使用 kos-core 形式化校验，非法类型不能创建

#include "../../../include/kos_ontology.h"
#include "../../../include/kos_core.h"
#include "../../../include/kos_core_bridge.h"
#include "../../../include/kos_finance.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// 金融领域类型本体文件路径
#define FINANCE_ONTOLOGY_FILE "finance_ontology.json"

// 声明：添加生成的类型定义
extern void kos_finance_ontology_add_generated_types(TypeOntology* ontology);

// 从 .kos 添加类型（kos-core 可用时）；否则回退
static int add_type(TypeOntology* ontology, const char* name, const char* kos_expr,
                    kos_term* fallback_term, char* err, size_t err_size) {
    if (kos_core_bridge_available()) {
        return kos_ontology_add_type_from_kos(ontology, name, kos_expr, NULL, err, err_size);
    }
    if (fallback_term && kos_type_wellformed(fallback_term)) {
        return kos_ontology_add_type_definition(ontology, name, fallback_term, NULL);
    }
    return -1;
}

// 初始化金融领域类型本体
// 如果文件存在则加载，否则创建默认本体（优先经 kos-core 校验）
TypeOntology* kos_finance_ontology_init(void) {
    // 尝试从文件加载
    TypeOntology* ontology = kos_ontology_load_from_file(FINANCE_ONTOLOGY_FILE);
    
    if (ontology) {
        printf("[FinanceSetup] ✓ Loaded ontology from %s (%zu types)\n", 
               FINANCE_ONTOLOGY_FILE, ontology->type_count);
        return ontology;
    }
    
    // 文件不存在，创建默认本体
    printf("[FinanceSetup] Creating default finance ontology (kos-core validated when available)...\n");
    ontology = kos_ontology_create("finance");
    if (!ontology) {
        printf("[FinanceSetup] ✗ Failed to create ontology\n");
        return NULL;
    }
    
    char err[512];
    
    // ========== 基础类型定义 ==========
    
    add_type(ontology, "AccountID", "Prop AccountID", kos_mk_prop("AccountID"), err, sizeof(err));
    add_type(ontology, "TransactionID", "Prop TransactionID", kos_mk_prop("TransactionID"), err, sizeof(err));
    add_type(ontology, "Amount", "Prop Amount", kos_mk_prop("Amount"), err, sizeof(err));
    add_type(ontology, "Currency", "Prop Currency", kos_mk_prop("Currency"), err, sizeof(err));
    add_type(ontology, "BankID", "Prop BankID", kos_mk_prop("BankID"), err, sizeof(err));
    add_type(ontology, "CountryCode", "Prop CountryCode", kos_mk_prop("CountryCode"), err, sizeof(err));
    add_type(ontology, "PersonID", "Prop PersonID", kos_mk_prop("PersonID"), err, sizeof(err));
    add_type(ontology, "CompanyID", "Prop CompanyID", kos_mk_prop("CompanyID"), err, sizeof(err));
    add_type(ontology, "Time", "Prop Time", kos_mk_prop("Time"), err, sizeof(err));
    
    // ========== 事件类型定义（优先 kos-core） ==========
    
    if (kos_core_bridge_available()) {
        add_type(ontology, "TransactionEvent",
                 "Sigma(tx: Prop TransactionID). Sigma(f: Prop AccountID). Sigma(t: Prop AccountID). Sigma(a: Prop Amount). Sigma(c: Prop Currency). Sigma(tm: Prop Time). Prop P",
                 NULL, err, sizeof(err));
        add_type(ontology, "SuspiciousTransaction",
                 "Sigma(tx: Prop TransactionID). Sigma(acc: Prop AccountID). Sigma(a: Prop Amount). Sigma(r: Prop Reason). Sigma(t: Prop Time). Prop P",
                 NULL, err, sizeof(err));
        add_type(ontology, "MoneyLaunderingSuspicion",
                 "Sigma(cid: Prop TransactionID). Sigma(acc: Prop AccountID). Sigma(e: Prop Evidence). Sigma(t: Prop Time). Prop P",
                 NULL, err, sizeof(err));
        add_type(ontology, "IsHighValueTransaction",
                 "Pi(tx: Prop TransactionEvent). Prop P",
                 NULL, err, sizeof(err));
        add_type(ontology, "IsCrossBorder",
                 "Pi(tx: Prop TransactionEvent). Prop P",
                 NULL, err, sizeof(err));
        add_type(ontology, "IsRelatedAccount",
                 "Pi(a1: Prop AccountID). Pi(a2: Prop AccountID). Prop P",
                 NULL, err, sizeof(err));
    } else {
        kos_term* tx_id = kos_ontology_find_type_definition(ontology, "TransactionID");
        kos_term* from_domain = kos_ontology_find_type_definition(ontology, "AccountID");
        kos_term* to_domain = kos_ontology_find_type_definition(ontology, "AccountID");
        kos_term* amt_domain = kos_ontology_find_type_definition(ontology, "Amount");
        kos_term* curr_domain = kos_ontology_find_type_definition(ontology, "Currency");
        kos_term* time_domain = kos_ontology_find_type_definition(ontology, "Time");
        kos_term* prop_final = kos_mk_prop("Prop");
        if (tx_id && from_domain && to_domain && amt_domain && curr_domain && time_domain && prop_final) {
            kos_term* t = kos_mk_sigma(time_domain, prop_final);
            t = t ? kos_mk_sigma(curr_domain, t) : NULL;
            t = t ? kos_mk_sigma(amt_domain, t) : NULL;
            t = t ? kos_mk_sigma(to_domain, t) : NULL;
            t = t ? kos_mk_sigma(from_domain, t) : NULL;
            kos_term* tx_evt = t ? kos_mk_sigma(tx_id, t) : NULL;
            if (tx_evt) { kos_ontology_add_type_definition(ontology, "TransactionEvent", tx_evt, NULL); kos_term_free(tx_evt); }
        }
        if (tx_id && from_domain && amt_domain && time_domain && prop_final) {
            kos_term* r = kos_mk_prop("Reason");
            kos_term* t = r ? kos_mk_sigma(time_domain, prop_final) : NULL;
            t = t ? kos_mk_sigma(r, t) : NULL;
            t = t ? kos_mk_sigma(amt_domain, t) : NULL;
            kos_term* susp = t ? kos_mk_sigma(from_domain, kos_mk_sigma(tx_id, t)) : NULL;
            if (susp) { kos_ontology_add_type_definition(ontology, "SuspiciousTransaction", susp, NULL); kos_term_free(susp); }
        }
        if (tx_id && from_domain && time_domain && prop_final) {
            kos_term* e = kos_mk_prop("Evidence");
            kos_term* t = e ? kos_mk_sigma(time_domain, prop_final) : NULL;
            t = t ? kos_mk_sigma(e, t) : NULL;
            kos_term* ml = t ? kos_mk_sigma(from_domain, kos_mk_sigma(tx_id, t)) : NULL;
            if (ml) { kos_ontology_add_type_definition(ontology, "MoneyLaunderingSuspicion", ml, NULL); kos_term_free(ml); }
        }
        kos_term* tx_evt = kos_ontology_find_type_definition(ontology, "TransactionEvent");
        if (tx_evt && prop_final) {
            kos_term* ihv = kos_mk_pi(tx_evt, prop_final);
            if (ihv) { kos_ontology_add_type_definition(ontology, "IsHighValueTransaction", ihv, NULL); kos_term_free(ihv); }
            kos_term* icb = kos_mk_pi(tx_evt, prop_final);
            if (icb) { kos_ontology_add_type_definition(ontology, "IsCrossBorder", icb, NULL); kos_term_free(icb); }
        }
        if (from_domain && to_domain && prop_final) {
            kos_term* ira = kos_mk_pi(from_domain, kos_mk_pi(to_domain, prop_final));
            if (ira) { kos_ontology_add_type_definition(ontology, "IsRelatedAccount", ira, NULL); kos_term_free(ira); }
        }
    }
    
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

