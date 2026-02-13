/**
 * @file all_scenarios_demo.c
 * @brief KOS-TL 六大核心场景完整示例 + 知识演化轨迹展示
 *
 * 场景：轴承制造质量追溯
 * 覆盖：根因追溯、反事实推理、合规性决策、审计问责、复杂系统治理、AI 治理
 *
 * 运行前准备:
 *   1. 构建 kos-core: cd kos-core && cabal build
 *   2. 确保 kos-core 在 PATH，或设置 KOS_CORE_PATH 指向 kos-core 可执行文件
 * 运行: 从项目根目录 ./build/bin/all_scenarios_demo 或 build\bin\all_scenarios_demo.exe
 */

#include "../include/kos_kernel_session.h"
#include "../include/kos_kernel_scenarios.h"
#include "../include/kos_kernel.h"
#include "../include/kos_core.h"
#include "../include/kos_core_bridge.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* 尝试多个路径定位 .kos 文件（支持项目根或 build/bin 运行） */
static const char* resolve_kos_path(const char* rel_path) {
    static const char* prefixes[] = {
        "",               /* 项目根 */
        "../",            /* build/ */
        "../../",         /* build/bin */
        "kos-core/",
        "../kos-core/",
        "../../kos-core/",
    };
    static char buf[512];
    for (size_t i = 0; i < sizeof(prefixes) / sizeof(prefixes[0]); i++) {
        snprintf(buf, sizeof(buf), "%s%s", prefixes[i], rel_path);
        FILE* f = fopen(buf, "r");
        if (f) {
            fclose(f);
            return buf;
        }
    }
    return NULL;
}

/* 创建简单事件对（用于演化演示） */
static kos_term* mk_event_pair(const char* type_name, const char* proof_name) {
    kos_term* ty = kos_mk_prop(type_name);
    kos_term* prf = kos_mk_prop(proof_name);
    if (!ty || !prf) {
        if (ty) kos_term_free(ty);
        if (prf) kos_term_free(prf);
        return NULL;
    }
    kos_term* pair = kos_mk_pair(ty, prf);
    if (!pair) {
        kos_term_free(ty);
        kos_term_free(prf);
        return NULL;
    }
    return pair;
}

int main(void) {
    printf("============================================================\n");
    printf("KOS-TL 六大核心场景示例 + 知识演化轨迹\n");
    printf("============================================================\n");

    /* 检查 kos-core 可用性（需在 PATH 或设置 KOS_CORE_PATH） */
    if (!kos_core_bridge_available()) {
        printf("\n[!] kos-core 不可用。请先构建: cd kos-core && cabal build\n");
        printf("    并确保 kos-core 在 PATH 中，或设置环境变量 KOS_CORE_PATH。\n");
        return 1;
    }
    printf("\n[OK] kos-core 可用\n");

    const char* ctx_prove = resolve_kos_path("kos-core/examples/quality_traceability_prove.kos");
    const char* ctx_no_anomaly = resolve_kos_path("kos-core/examples/quality_traceability_prove_no_anomaly.kos");

    if (!ctx_prove) {
        printf("\n[!] 未找到 quality_traceability_prove.kos，请从项目根目录运行。\n");
        return 1;
    }
    printf("[OK] 上下文: %s\n", ctx_prove);

    /* ========== 1. 创建会话与加载本体 ========== */
    printf("\n========== 场景 0: 会话与本体 ==========\n");
    kos_kernel_session_t* session = kos_kernel_session_create(ctx_prove);
    if (!session) {
        printf("[!] 创建会话失败\n");
        return 1;
    }
    if (kos_kernel_session_load_ontology(session, ctx_prove) != 0) {
        printf("[!] 加载本体失败\n");
        kos_kernel_session_free(session);
        return 1;
    }
    printf("[OK] 会话创建，本体已加载\n");

    /* ========== 2. 知识演化：合规入队 + 逐步执行 ========== */
    printf("\n========== 场景 3 & 5: 合规性决策 + 系统治理 ==========\n");
    kos_term* evt1 = mk_event_pair("ProcStep", "ProcStepProof");
    kos_term* evt2 = mk_event_pair("Anomaly", "AnomalyProof");
    kos_term* evt3 = mk_event_pair("FailEvt", "FailEvtProof");

    int enq = 0;
    if (evt1 && kos_kernel_session_enqueue_if_compliant(session, evt1) == 0) enq++;
    if (evt1) kos_term_free(evt1);
    if (evt2 && kos_kernel_session_enqueue_if_compliant(session, evt2) == 0) enq++;
    if (evt2) kos_term_free(evt2);
    if (evt3 && kos_kernel_session_enqueue_if_compliant(session, evt3) == 0) enq++;
    if (evt3) kos_term_free(evt3);

    printf("合规入队: %d 个事件\n", enq);

    size_t steps = kos_kernel_session_evolve_until_idle(session);
    printf("演化步数: %u\n", (unsigned)steps);

    bool legal = kos_kernel_state_is_legal(kos_kernel_session_get_sigma(session));
    printf("状态合法: %s\n", legal ? "是" : "否");

    /* ========== 3. 根因追溯 ========== */
    printf("\n========== 场景 1: 根因追溯 ==========\n");
    char err[256];
    kos_term* proof = NULL;
    bool found = kos_kernel_session_find_root_cause(session, NULL, "RootCauseReport", &proof, err, sizeof(err));
    printf("根因证明: %s\n", found ? "找到" : "未找到");
    if (found && proof) {
        printf("  证明项已获取（Pair failEvt (Pair anomaly causalProof) 形式）\n");
        kos_term_free(proof);
    } else if (!found) {
        printf("  %s\n", err[0] ? err : "No proof found.");
    }

    /* ========== 4. 反事实推理 ========== */
    printf("\n========== 场景 2: 反事实推理 ==========\n");
    if (ctx_no_anomaly) {
        kos_counterfactual_result_t cf;
        if (kos_kernel_counterfactual_test(ctx_prove, ctx_no_anomaly, "RootCauseReport", &cf) == 0) {
            printf("事实可证明: %s\n", cf.factual_provable ? "是" : "否");
            printf("反事实可证明: %s\n", cf.counterfactual_provable ? "是" : "否");
            printf("排除变量因果必要: %s\n", cf.excluded_necessary ? "是（anomalyEx 对根因必要）" : "否");
        }
    } else {
        printf("未找到反事实上下文文件，跳过\n");
    }

    /* ========== 5. 审计与问责 ========== */
    printf("\n========== 场景 4: 审计与问责 ==========\n");
    const char* audit_file = "audit_trajectory.json";
    if (kos_kernel_session_export_audit_trail(session, audit_file) == 0) {
        printf("[OK] 审计轨迹已导出: %s\n", audit_file);
    } else {
        printf("[!] 导出审计轨迹失败\n");
    }

    /* 轨迹重放 */
    kos_state_t* sigma_fresh = kos_state_create(kos_mk_prop("ReplayInit"));
    if (sigma_fresh) {
        kos_trace_t* trace = kos_kernel_session_get_trace(session);
        int replayed = kos_trace_replay_into(trace, sigma_fresh);
        printf("轨迹重放: %d 步成功\n", replayed);
        kos_state_free(sigma_fresh);
    }

    /* ========== 6. AI 治理 ========== */
    printf("\n========== 场景 6: AI 治理 ==========\n");
    bool ai_ok = kos_kernel_verify_ai_suggestion(NULL, "Prop P", "Prop P");
    printf("AI 建议验证 (Prop P : Prop P): %s\n", ai_ok ? "合规" : "不合规");

    /* ========== 7. 展示轨迹摘要 ========== */
    printf("\n========== 知识演化轨迹摘要 ==========\n");
    kos_trace_t* trace = kos_kernel_session_get_trace(session);
    size_t len = kos_trace_length(trace);
    printf("轨迹长度: %u 步\n", (unsigned)len);
    for (size_t i = 0; i < len && i < 5; i++) {
        const kos_trace_step_t* step = kos_trace_get_step(trace, i);
        if (step) {
            printf("  步 %u: ts_after=%d, event_pair=%s\n",
                   (unsigned)i, step->ts_after,
                   step->event_pair ? "有" : "无");
        }
    }
    if (len > 5) printf("  ...\n");

    kos_kernel_session_free(session);
    printf("\n============================================================\n");
    printf("演示完成。\n");
    printf("============================================================\n");
    return 0;
}
