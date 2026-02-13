// examples/core_pi_example.c
// 演示：在 KOS Core 层构造一个简单的 Π 类型并用 kos_check 验证
//
// 目标类型（简化版）：
//   F : Π(x:ID). Prop
//
// 我们构造一个 λ 抽象：
//   f = λx:ID. Proof   （其中 Proof : Prop）
//
// 然后检查：f : F 是否通过类型检查。

#include "../include/kos_core.h"
#include <stdio.h>

int main(void) {
    // --- 1. 构造域类型和命题 ---
    kos_term* id_type    = kos_mk_id("ID");
    kos_term* prop_body  = kos_mk_prop("P");           // 作为 Π(x:ID).P 的返回类型
    kos_term* proof_term = kos_mk_prop("ProofOfP");    // 作为 λx.body 的“证明项”

    if (!id_type || !prop_body || !proof_term) {
        fprintf(stderr, "[core_pi_example] ERROR: failed to create basic terms\n");
        return 1;
    }

    // --- 2. 构造目标类型：F = Π(x:ID). P ---
    kos_term* pi_type = kos_mk_pi(id_type, prop_body);
    if (!pi_type) {
        fprintf(stderr, "[core_pi_example] ERROR: failed to build Pi type\n");
        return 1;
    }

    // --- 3. 构造 λ 抽象：f = λx:ID. ProofOfP ---
    // 当前数据结构中，lambda 使用 KOS_PI + body_term 表示，
    // 这里直接重用 id_type 作为 domain，proof_term 作为 body_term。
    kos_term* lambda_term = kos_mk_lambda(id_type, proof_term);
    if (!lambda_term) {
        fprintf(stderr, "[core_pi_example] ERROR: failed to build lambda term\n");
        return 1;
    }

    // --- 4. 使用 kos_check 进行类型检查：Γ ⊢ f : Π(x:ID).P ---
    kos_term* ctx = NULL;
    bool ok = kos_check(ctx, lambda_term, pi_type);

    printf("[core_pi_example] kos_check result: %s\n", ok ? "OK" : "FAIL");

    // --- 5. 资源释放 ---
    kos_term_free(lambda_term);
    kos_term_free(pi_type);
    // 注意：pi_type / lambda_term 递归释放时会释放内部的 id_type/prop_body/proof_term，
    // 因此这些基础项不应再次手动 free。

    return ok ? 0 : 1;
}

