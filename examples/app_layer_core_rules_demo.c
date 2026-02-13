/**
 * @file app_layer_core_rules_demo.c
 * @brief 应用层示例：尽可能使用 KOS-core 的类型推理与规约规则
 *
 * 应用层流程：信号 → elab（kos-core 校验）→ Kernel 演化（verify/reduce/update）
 *
 * KOS-core 规则覆盖表：
 * ┌─────────────────────────────────────────────────────────────────────────┐
 * │ 类型推理：Pi 引入(lam)/消除(App)  Sigma 引入(<,>)/消除(split)            │
 * │           Sum 引入(inl/inr)/消除(case)  Id 引入(refl)  Conversion       │
 * │ 规约：    β(lam app)  ι(Σ)(split pair)  ι(Sum)(case inl/inr)            │
 * │          δ(def 展开)  ζ(let)  η(可选)                                    │
 * │ 值谓词：  gt, ge, lt, le, eq（类型级计算）                               │
 * └─────────────────────────────────────────────────────────────────────────┘
 *
 * 本示例通过以下方式触发 kos-core 规则：
 * 0. kos_core_bridge_check_file/check_source：完整模块检查（触发 δ 及所有 def）
 * 1. kos_elab_from_kos：.kos 信号经 kos_core_bridge_term_from_kos 校验
 * 2. kos_core_bridge_infer_from_kos：显式类型推理（Pi/Sigma/Sum 引入消除）
 * 3. kos_core_bridge_check_expr：显式类型检查（Conversion、值谓词）
 * 4. kos_kernel_step：Kernel 演化（kos_reduce 规约、kos_type_check 验证）
 * 5. 知识库：物化项依赖图可视化
 */

#include "../include/kos_core.h"
#include "../include/kos_core_bridge.h"
#include "../include/kos_kernel.h"
#include "../include/kos_knowledge_base.h"
#include "../include/kos_runtime.h"
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

/* 将 .kos 信号转为 bitstream */
static bitstream to_signal(const char* s) {
    bitstream bs = { s ? (unsigned char*)s : NULL, s ? strlen(s) : 0 };
    return bs;
}

/* ========== Phase 0: 完整模块检查（触发 δ 及所有规则） ========== */
static void phase_full_module_check(void) {
    printf("\n========== Phase 0: 完整模块检查 ==========\n");
    printf("kos_core_bridge_check_file 触发：Pi/Sigma/Sum/Id 推理、β/ι(Σ)/ι(Sum)/δ/ζ 规约、值谓词\n\n");

    const char* candidates[] = {
        "kos-core/examples/09_type_inference_reduction.kos",
        "../kos-core/examples/09_type_inference_reduction.kos",
        "../../kos-core/examples/09_type_inference_reduction.kos"
    };
    char err[512];
    int found = 0;
    for (size_t i = 0; i < sizeof(candidates) / sizeof(candidates[0]); i++) {
        if (kos_core_bridge_check_file(candidates[i], err, sizeof(err))) {
            printf("  OK: %s 类型检查通过（含 def 展开 δ）\n", candidates[i]);
            found = 1;
            break;
        }
    }
    if (!found) {
        printf("  SKIP: 未找到 09_type_inference_reduction.kos\n");
    }

    /* check_source：内联模块（含 def，触发 δ） */
    const char* inline_module =
        "module InlineDelta where\n"
        "def base : Type1 := Prop P\n"
        "def viaDef : Type1 := base\n";
    if (kos_core_bridge_check_source(inline_module, err, sizeof(err))) {
        printf("  OK: check_source 内联模块（def base, viaDef）通过\n");
    } else {
        printf("  check_source 内联模块: %s\n", err[0] ? err : "FAIL");
    }
}

/* ========== Phase 1: Elab 信号提炼（kos-core 校验） ========== */
static void phase_elab_signals(kos_state_t* sigma, kos_knowledge_base_t* kb) {
    (void)sigma;
    printf("\n========== Phase 1: Elab 信号提炼 ==========\n");
    printf("信号经 kos_elab_from_kos → kos_core_bridge_term_from_kos（kos-core 类型推理+规约）\n\n");

    const char* signals[] = {
        "Prop P",                           /* Prop 引入 */
        "Prop Evt1",                        /* Prop */
        "gt(val \"200\", val \"180\")",     /* 值谓词 gt */
        "ge(val \"185\", val \"180\")",     /* 值谓词 ge */
        "lt(val \"100\", val \"200\")",     /* 值谓词 lt */
        "< Prop P , Prop Q >",              /* Sigma 引入 Pair */
        "(lam (x : Prop P) . x) (Prop P)",  /* Pi 消除 App + beta 规约 */
        "let x : Prop P := Prop P in x",     /* Let + zeta 规约 */
        "let a : Prop P := Prop P in let b : Prop P := a in b",  /* 嵌套 Let + zeta */
        "split (< Prop P , Prop Q >) as x y in x",  /* Sigma 消除 + iota(Sigma) */
        "inl(Prop P, Prop Q, Prop P)",      /* Sum 引入 InL */
        "inr(Prop P, Prop Q, Prop Q)",      /* Sum 引入 InR */
        "case inl(Prop P, Prop Q, Prop P) of inl x -> Prop P; inr y -> Prop Q",  /* Sum 消除 + iota(Sum) */
        "refl (Prop P)",                    /* Id 引入 Refl */
    };
    const char* rule_hints[] = {
        "Prop", "Prop", "gt", "ge", "lt", "Sigma-Pair", "Pi-App+beta", "Let+zeta",
        "NestedLet+zeta", "Split+iota(Sigma)", "Sum-InL", "Sum-InR", "Case+iota(Sum)", "Id-Refl"
    };
    const size_t n = sizeof(signals) / sizeof(signals[0]);

    int ts = 0;
    for (size_t i = 0; i < n; i++) {
        bitstream sig = to_signal(signals[i]);
        kos_term* pair = kos_elab_from_kos(sig);
        if (pair) {
            printf("  [%s] %s -> OK\n", rule_hints[i], signals[i]);
            /* 加入知识库 */
            kos_term* ev = pair->kind == KOS_PAIR ? pair->data.pair.data : pair;
            if (ev) {
                char id[64];
                snprintf(id, sizeof(id), "elab_%lu", (unsigned long)i);
                kos_term* type = kos_mk_prop("Event");
                kos_kb_add_item(kb, id, kos_term_copy(ev), type, ts++, KOS_KB_SOURCE_BOOTSTRAP);
                kos_term_free(type);
            }
            kos_term_free(pair);
        } else {
            printf("  [%s] %s -> FAIL\n", rule_hints[i], signals[i]);
        }
    }
    /* 添加依赖边：体现项之间的 SUBTERM 关系，丰富可视化 */
    kos_kb_add_dependency_typed(kb, "elab_5", "elab_0", KOS_KB_DEP_TYPE_SUBTERM);  /* Pair <P,Q> 含 Prop P */
    kos_kb_add_dependency_typed(kb, "elab_6", "elab_0", KOS_KB_DEP_TYPE_SUBTERM);  /* (lam x.x)(P) 依赖参数 P */
    kos_kb_add_dependency_typed(kb, "elab_7", "elab_0", KOS_KB_DEP_TYPE_SUBTERM);  /* let x:=P in x 依赖 P */
    kos_kb_add_dependency_typed(kb, "elab_9", "elab_5", KOS_KB_DEP_TYPE_SUBTERM);  /* split pair 依赖 pair */
    kos_kb_add_dependency_typed(kb, "elab_10", "elab_0", KOS_KB_DEP_TYPE_SUBTERM); /* inl(P,Q,P) 依赖 P */
    kos_kb_add_dependency_typed(kb, "elab_12", "elab_10", KOS_KB_DEP_TYPE_SUBTERM); /* case inl 依赖 scrutinee */
    kos_kb_infer_dependencies(kb);  /* 从项结构推断额外依赖 */
}

/* ========== Phase 2: 显式类型推理（kos-core infer-term） ========== */
static void phase_explicit_inference(void) {
    printf("\n========== Phase 2: 显式类型推理 ==========\n");
    printf("kos_core_bridge_infer_from_kos 触发 Pi/Sigma/Sum 引入消除规则\n\n");

    const char* exprs[] = {
        "lam (x : Prop P) . x",
        "(lam (x : Prop P) . x) (Prop P)",
        "< Prop P , Prop Q >",
        "split (< Prop P , Prop Q >) as x y in x",
        "split (< Prop P , Prop Q >) as x y in y",
        "inl(Prop P, Prop Q, Prop P)",
        "inr(Prop P, Prop Q, Prop Q)",
        "case inl(Prop P, Prop Q, Prop P) of inl x -> Prop P; inr y -> Prop Q",
        "case inr(Prop P, Prop Q, Prop Q) of inl x -> Prop P; inr y -> Prop Q",
    };
    const char* hints[] = {
        "Pi-Lam", "App+beta", "Sigma-Pair", "Split-fst+iota", "Split-snd+iota",
        "Sum-InL", "Sum-InR", "Case-inl+iota(Sum)", "Case-inr+iota(Sum)"
    };
    const size_t n = sizeof(exprs) / sizeof(exprs[0]);

    char err[512];
    for (size_t i = 0; i < n; i++) {
        void* term = NULL, *type = NULL;
        if (kos_core_bridge_infer_from_kos(exprs[i], &term, &type, err, sizeof(err)) == 0) {
            printf("  infer %s: OK\n", hints[i]);
            if (term) kos_term_free((kos_term*)term);
            if (type) kos_term_free((kos_term*)type);
        } else {
            printf("  infer %s: FAIL - %s\n", hints[i], err);
        }
    }
}

/* ========== Phase 3: 显式类型检查（kos-core check-term） ========== */
static void phase_explicit_check(void) {
    printf("\n========== Phase 3: 显式类型检查 ==========\n");
    printf("kos_core_bridge_check_expr 触发 Conversion、值谓词规则\n\n");

    struct { const char* term; const char* type; const char* hint; } cases[] = {
        { "Prop P", "Prop P", "Conversion" },
        { "refl (Prop P)", "Id(Type1, Prop P, Prop P)", "Id-Refl" },
        { "let x : Prop P := Prop P in x", "Prop P", "Let+zeta" },
        { "let a : Prop P := Prop P in let b : Prop P := a in b", "Prop P", "NestedLet+zeta" },
        { "Prop trivial", "gt(val \"200\", val \"180\")", "gt" },
        { "Prop trivial", "ge(val \"185\", val \"180\")", "ge" },
        { "Prop trivial", "lt(val \"100\", val \"200\")", "lt" },
        { "Prop trivial", "le(val \"180\", val \"200\")", "le" },
        { "Prop trivial", "eq(val \"100\", val \"100\")", "eq" },
    };
    const size_t n = sizeof(cases) / sizeof(cases[0]);

    char err[512];
    for (size_t i = 0; i < n; i++) {
        if (kos_core_bridge_check_expr(cases[i].term, cases[i].type, err, sizeof(err))) {
            printf("  check %s: OK\n", cases[i].hint);
        } else {
            printf("  check %s: FAIL - %s\n", cases[i].hint, err);
        }
    }
    /* 负例：值谓词不成立时应拒绝 */
    if (!kos_core_bridge_check_expr("Prop trivial", "gt(val \"100\", val \"200\")", err, sizeof(err))) {
        printf("  check gt(100,200) 负例: 正确拒绝\n");
    } else {
        printf("  check gt(100,200) 负例: 意外通过\n");
    }
}

/* ========== Phase 4: Kernel 演化（kos_reduce + kos_type_check） ========== */
static void phase_kernel_evolution(kos_state_t* sigma, kos_knowledge_base_t* kb) {
    (void)kb;
    printf("\n========== Phase 4: Kernel 演化 ==========\n");
    printf("kos_kernel_step: Verify(Pre) -> Reduce(beta/iota) -> Update(K)\n\n");

    /* 构造可过 verify 的 Prop 型事件（KosValidated : Prop X） */
    const char* prop_signals[] = { "Prop P", "Prop Q", "Prop Evt1", "gt(val \"200\", val \"180\")" };
    const size_t n = sizeof(prop_signals) / sizeof(prop_signals[0]);

    for (size_t i = 0; i < n; i++) {
        bitstream sig = to_signal(prop_signals[i]);
        kos_term* pair = kos_elab_from_kos(sig);
        if (!pair) continue;
        bool ok = kos_kernel_step(sigma, pair);
        printf("  kernel_step %s: %s\n", prop_signals[i], ok ? "OK" : "FAIL");
        kos_term_free(pair);
    }
}

/* ========== Phase 5: 知识库可视化 ========== */
static void phase_kb_visualization(kos_knowledge_base_t* kb) {
    printf("\n========== Phase 5: 知识库可视化 ==========\n");

    char* html = kos_kb_export_visualization_html(kb);
    if (html) {
        FILE* f = fopen("app_layer_core_rules.html", "w");
        if (f) {
            fputs(html, f);
            fclose(f);
            printf("  OK: app_layer_core_rules.html\n");
        }
        free(html);
    }
}

/* ========== 主流程 ========== */

int main(void) {
    printf("============================================================\n");
    printf("应用层：KOS-core 类型推理与规约规则示例\n");
    printf("============================================================\n");
    printf("\n流程：信号 -> elab(kos-core) -> Kernel(verify/reduce/update) -> KB\n");

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

    /* 创建状态与知识库 */
    kos_term* initial_K = kos_mk_prop("InitialKB");
    kos_state_t* sigma = kos_state_create(initial_K);
    kos_term_free(initial_K);
    if (!sigma) {
        printf("Failed to create state\n");
        return 1;
    }

    kos_knowledge_base_t* kb = kos_kb_create();
    if (!kb) {
        kos_state_free(sigma);
        return 1;
    }
    kos_state_set_kb(sigma, kb);

    phase_full_module_check();
    phase_elab_signals(sigma, kb);
    phase_explicit_inference();
    phase_explicit_check();
    phase_kernel_evolution(sigma, kb);
    phase_kb_visualization(kb);

    printf("\n============================================================\n");
    printf("知识库: %lu 项, %lu 边\n", (unsigned long)kb->item_count, (unsigned long)kb->edge_count);
    printf("============================================================\n");

    printf("\n---------- KOS-core 规则覆盖总结 ----------\n");
    printf("类型推理: Pi(lam/App) Sigma(Pair/split) Sum(inl/inr/case) Id(refl) Conversion\n");
    printf("规约:     beta(lam app) iota(Sigma)(split) iota(Sum)(case) delta(def) zeta(let)\n");
    printf("值谓词:   gt ge lt le eq (类型级计算)\n");
    printf("-------------------------------------------\n");

    kos_state_free(sigma);
    return 0;
}

