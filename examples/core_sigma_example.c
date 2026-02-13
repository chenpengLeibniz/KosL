// examples/core_sigma_example.c
// 简单演示：在 KOS Core 层构造一个 Σ 类型并调用 kos_check 进行类型检查
//
// 目标类型：
//   FailEvt : Σ(x:ID). Σ(e:ID). Σ(t:TIME). Prop
//
// 然后构造一个具体实例：
//   < "BATCH1"
//   , < "ERR01"
//   , < "2025-01-01T00:00:00Z", "FailEvtProof" > > >
//
// 并用 kos_check 验证该实例是否具有 FailEvt 类型。

#include "../include/kos_core.h"
#include <stdio.h>

int main(void) {
    // --- 1. 构造基础类型 ---
    // ID, TIME, Prop 的名字在当前实现里主要用于可读性
    kos_term* id_type   = kos_mk_id("ID");
    kos_term* err_type  = kos_mk_id("ID");
    kos_term* time_type = kos_mk_time("Time");
    kos_term* prop_type = kos_mk_prop("FailEvt");

    if (!id_type || !err_type || !time_type || !prop_type) {
        fprintf(stderr, "[core_sigma_example] ERROR: failed to create base types\n");
        return 1;
    }

    // --- 2. 构造 Σ 类型 FailEvtType : Σ(x:ID). Σ(e:ID). Σ(t:TIME). Prop ---
    kos_term* sigma_t_prop   = kos_mk_sigma(time_type, prop_type);   // Σ(t:TIME). Prop
    kos_term* sigma_e_sigma  = kos_mk_sigma(err_type, sigma_t_prop); // Σ(e:ID). Σ(t:TIME). Prop
    kos_term* fail_evt_type  = kos_mk_sigma(id_type, sigma_e_sigma); // Σ(x:ID). Σ(e:ID). Σ(t:TIME). Prop

    if (!fail_evt_type) {
        fprintf(stderr, "[core_sigma_example] ERROR: failed to build FailEvt Σ-type\n");
        return 1;
    }

    // --- 3. 构造一个具体实例 ---
    kos_term* batch_id  = kos_mk_id("BATCH1");
    kos_term* err_id    = kos_mk_id("ERR01");
    kos_term* time_val  = kos_mk_time("2025-01-01T00:00:00Z");
    kos_term* proof_val = kos_mk_prop("FailEvtProof");

    if (!batch_id || !err_id || !time_val || !proof_val) {
        fprintf(stderr, "[core_sigma_example] ERROR: failed to create instance terms\n");
        return 1;
    }

    // 嵌套构造：<t, proof>
    kos_term* pair_t_proof   = kos_mk_pair(time_val, proof_val);
    kos_term* pair_e_rest    = kos_mk_pair(err_id, pair_t_proof);
    kos_term* fail_evt_value = kos_mk_pair(batch_id, pair_e_rest);

    if (!fail_evt_value) {
        fprintf(stderr, "[core_sigma_example] ERROR: failed to build FailEvt instance\n");
        return 1;
    }

    // --- 4. 使用 kos_check 进行类型检查 ---
    kos_term* ctx = NULL; // 当前示例不使用额外上下文
    bool ok = kos_check(ctx, fail_evt_value, fail_evt_type);

    printf("[core_sigma_example] kos_check result: %s\n", ok ? "OK" : "FAIL");

    // --- 5. 资源释放 ---
    kos_term_free(fail_evt_value);
    kos_term_free(fail_evt_type);

    // 注意：fail_evt_type 中嵌套的 domain/body 等在 kos_term_free 中会被递归释放，
    // 因此基础类型（id_type/err_type/time_type/prop_type）不需要单独 free，
    // 否则会 double free。

    return ok ? 0 : 1;
}

