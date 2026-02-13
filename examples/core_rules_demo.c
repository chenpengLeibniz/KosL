/**
 * @file core_rules_demo.c
 * @brief 展示 kos-core 类型推理规则与规约规则的应用场景
 *
 * kos-core 实现 Kos.pdf 中的 Core 层：
 * - 类型推理规则：Π 引入/消除、Σ 引入/消除、Sum 引入/消除、Id 引入、Conversion
 * - 规约规则：β、ι(Σ)、ι(Sum)、δ、ζ、η
 * - 值依赖谓词：gt, ge, lt, le, eq
 *
 * 本演示通过调用 kos-core 的 check、infer-term、check-term 等 API，
 * 构造场景尽可能展现上述规则的应用。
 */

#include "../include/kos_core_bridge.h"
#include "../include/kos_core.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <direct.h>
#define getcwd _getcwd
#else
#include <unistd.h>
#endif

static void try_set_kos_core_path(void) {
    if (kos_core_bridge_available()) return;
    char cwd[1024];
    if (!getcwd(cwd, sizeof(cwd))) return;
    static char path[2048];
#ifdef _WIN32
    snprintf(path, sizeof(path), "%s\\kos-core\\dist-newstyle\\build\\x86_64-windows\\ghc-9.8.2\\kos-core-0.1.0.0\\x\\kos-core\\build\\kos-core\\kos-core.exe", cwd);
#else
    snprintf(path, sizeof(path), "%s/kos-core/dist-newstyle/build/x86_64-windows/ghc-9.8.2/kos-core-0.1.0.0/x/kos-core/build/kos-core/kos-core", cwd);
#endif
    kos_core_bridge_set_path(path);
}

/* ========== 场景 1：完整模块检查（触发所有规则） ========== */
static void scenario_full_module_check(void) {
    printf("\n========== 场景 1：完整模块检查 ==========\n");
    printf("kos-core check <file> 触发：\n");
    printf("  - 类型推理：Pi/Sigma/Sum/Id 引入消除、Conversion\n");
    printf("  - 规约：beta(lam app)、iota(Sigma)(split pair)、iota(Sum)(case inl/inr)、delta(def)、zeta(let)\n");
    printf("  - 值谓词：gt/ge 类型级计算\n\n");

    const char* candidates[] = {
        "kos-core/examples/09_type_inference_reduction.kos",
        "../kos-core/examples/09_type_inference_reduction.kos",
        "../../kos-core/examples/09_type_inference_reduction.kos"
    };
    char err[512];
    int found = 0;
    for (size_t i = 0; i < sizeof(candidates) / sizeof(candidates[0]); i++) {
        if (kos_core_bridge_check_file(candidates[i], err, sizeof(err))) {
            printf("  OK: %s 类型检查通过\n", candidates[i]);
            found = 1;
            break;
        }
    }
    if (!found) {
        printf("  FAIL: 未找到 09_type_inference_reduction.kos，请从项目根目录运行\n");
        printf("  或: cd kos-core && cabal run kos-core -- check examples/09_type_inference_reduction.kos\n");
    }
}

/* ========== 场景 2：Π 类型推理与 β 规约 ========== */
static void scenario_pi_beta(void) {
    printf("\n========== 场景 2：Π 引入/消除、β 规约 ==========\n");
    printf("lam (x:A).x 经 Π 引入规则得到 Π(x:A).A\n");
    printf("(lam (x:A).x) a 经 App 消除 + β 规约得到 a\n");
    printf("（单项 check 无模块上下文，完整 β 在场景 1 模块中触发）\n\n");

    char err[512];
    void* term_out = NULL, *type_out = NULL;
    if (kos_core_bridge_infer_from_kos("lam (x : Prop P) . x", &term_out, &type_out, err, sizeof(err)) == 0) {
        printf("  infer lam (x:Prop P).x : OK (Pi intro)\n");
        if (term_out) kos_term_free((kos_term*)term_out);
        if (type_out) kos_term_free((kos_term*)type_out);
    } else {
        printf("  infer FAIL: %s\n", err);
    }
    if (kos_core_bridge_infer_from_kos("(lam (x : Prop P) . x) (Prop P)", &term_out, &type_out, err, sizeof(err)) == 0) {
        printf("  infer (lam x.x)(Prop P) : OK (App elim + beta)\n");
        if (term_out) kos_term_free((kos_term*)term_out);
        if (type_out) kos_term_free((kos_term*)type_out);
    } else {
        printf("  infer (lam x.x)(Prop P) FAIL: %s\n", err);
    }
}

/* ========== 场景 3：Σ 类型推理与 ι(Σ) 规约 ========== */
static void scenario_sigma_iota(void) {
    printf("\n========== 场景 3：Σ 引入/消除、ι(Σ) 规约 ==========\n");
    printf("<a,b> 经 Σ 引入规则得到 Σ(x:A).B\n");
    printf("split(<u,v>, x.y.t) 经 ι(Σ) 规约得到 t[u/x,v/y]\n");
    printf("（在 09_type_inference_reduction.kos 中 fst/snd 定义触发）\n\n");

    char err[512];
    void* term_out = NULL, *type_out = NULL;
    if (kos_core_bridge_infer_from_kos("< Prop P , Prop Q >", &term_out, &type_out, err, sizeof(err)) == 0) {
        printf("  infer <Prop P, Prop Q> : OK (Sigma intro)\n");
        if (term_out) kos_term_free((kos_term*)term_out);
        if (type_out) kos_term_free((kos_term*)type_out);
    } else {
        printf("  infer pair FAIL: %s\n", err);
    }
    if (kos_core_bridge_infer_from_kos("split (< Prop P , Prop Q >) as x y in x", &term_out, &type_out, err, sizeof(err)) == 0) {
        printf("  infer split(...) as x y in x : OK (Sigma elim + iota)\n");
        if (term_out) kos_term_free((kos_term*)term_out);
        if (type_out) kos_term_free((kos_term*)type_out);
    } else {
        printf("  infer split fst FAIL: %s\n", err);
    }
}

/* ========== 场景 4：Sum 类型推理与 ι(Sum) 规约 ========== */
static void scenario_sum_iota(void) {
    printf("\n========== 场景 4：Sum 引入/消除、ι(Sum) 规约 ==========\n");
    printf("inl(A,B,a) 经 Sum 引入得到 A+B\n");
    printf("case(inl u, x.t, y.v) 经 ι(Sum) 规约得到 t[u/x]\n");
    printf("（在 09_type_inference_reduction.kos 中 elimSum left 触发）\n\n");

    char err[512];
    void* term_out = NULL, *type_out = NULL;
    if (kos_core_bridge_infer_from_kos("inl(Prop P, Prop Q, Prop P)", &term_out, &type_out, err, sizeof(err)) == 0) {
        printf("  infer inl(Prop P, Prop Q, Prop P) : OK (Sum intro)\n");
        if (term_out) kos_term_free((kos_term*)term_out);
        if (type_out) kos_term_free((kos_term*)type_out);
    } else {
        printf("  infer inl FAIL: %s\n", err);
    }
}

/* ========== 场景 5：Id 类型与 Refl 引入 ========== */
static void scenario_id_refl(void) {
    printf("\n========== 场景 5：Id 引入、definitionallyEqual ==========\n");
    printf("refl(a) : Id_A(a,a)，definitionallyEqual 判定等价\n\n");

    char err[512];
    if (kos_core_bridge_check_expr("refl (Prop P)", "Id(Type1, Prop P, Prop P)", err, sizeof(err))) {
        printf("  check refl(Prop P) : Id(Type1,P,P): OK\n");
    } else {
        printf("  check refl FAIL: %s\n", err);
    }
}

/* ========== 场景 6：Let 与 ζ 规约 ========== */
static void scenario_let_zeta(void) {
    printf("\n========== 场景 6：Let 绑定、ζ 规约 ==========\n");
    printf("let x:=u in t 经 ζ 规约得到 t[u/x]\n\n");

    char err[512];
    if (kos_core_bridge_check_expr("let x : Prop P := Prop P in x", "Prop P", err, sizeof(err))) {
        printf("  check let x:=P in x : Prop P: OK (zeta)\n");
    } else {
        printf("  check let FAIL: %s\n", err);
    }
}

/* ========== 场景 7：Conversion 规则 ========== */
static void scenario_conversion(void) {
    printf("\n========== 场景 7：Conversion 规则 ==========\n");
    printf("Γ ⊢ t:A, Γ ⊢ A≡B ⇒ Γ ⊢ t:B\n");
    printf("infer t 得 A，若 A 与 B 定义等价则 check t:B 通过\n\n");

    char err[512];
    /* Prop P 与 Prop P 定义等价 */
    if (kos_core_bridge_check_expr("Prop P", "Prop P", err, sizeof(err))) {
        printf("  check Prop P : Prop P: OK (Conversion: A≡A)\n");
    } else {
        printf("  check FAIL: %s\n", err);
    }
}

/* ========== 场景 8：值依赖谓词 ========== */
static void scenario_value_predicates(void) {
    printf("\n========== 场景 8：值依赖谓词（类型级计算） ==========\n");
    printf("gt(a,b)、ge(a,b) 在类型检查时求值\n");
    printf("gt(200,180) 成立，gt(100,200) 不成立\n\n");

    char err[512];
    if (kos_core_bridge_check_expr("Prop trivial", "gt(val \"200\", val \"180\")", err, sizeof(err))) {
        printf("  check Prop trivial : gt(200,180): OK\n");
    } else {
        printf("  check gt(200,180) FAIL: %s\n", err);
    }
    if (!kos_core_bridge_check_expr("Prop trivial", "gt(val \"100\", val \"200\")", err, sizeof(err))) {
        printf("  check Prop trivial : gt(100,200): FAIL (expected)\n");
    } else {
        printf("  check gt(100,200): OK (unexpected)\n");
    }
}

/* ========== 主流程 ========== */

int main(void) {
    printf("============================================================\n");
    printf("KOS-Core 类型推理与规约规则应用场景演示\n");
    printf("============================================================\n");
    printf("\nkos-core 实现 Kos.pdf Core 层：\n");
    printf("  - 类型推理：Pi/Sigma/Sum/Id 引入消除、Conversion\n");
    printf("  - 规约：beta, iota(Sigma/Sum), delta, zeta, eta\n");
    printf("  - 值谓词：gt, ge, lt, le, eq\n");

    try_set_kos_core_path();
    if (!kos_core_bridge_available()) {
        char err[512];
        kos_core_bridge_check_term("Prop P", err, sizeof(err));
        printf("\nkos-core not available: %s\n", err[0] ? err : "(no details)");
        printf("Build: cd kos-core && cabal build\n");
        printf("Run from KosL project root.\n");
        return 1;
    }
    printf("\nkos-core: available\n");

    scenario_full_module_check();
    scenario_pi_beta();
    scenario_sigma_iota();
    scenario_sum_iota();
    scenario_id_refl();
    scenario_let_zeta();
    scenario_conversion();
    scenario_value_predicates();

    printf("\n============================================================\n");
    printf("演示完成。\n");
    printf("============================================================\n");
    return 0;
}
