/**
 * @file unqualified_product_demo.c
 * @brief 以“检测到一个不合格的产品，其批次Batch_26_01-00001，时间为2026-01-30”为例，
 *        展示 KOS-TL 系统从信号到事件、溯源、内核演化的完整运行过程。
 *
 * 运行流程：
 * 1. Runtime: 原始信号 → elab 提炼 → <event, proof>
 * 2. Traceability: 若为 FailEvt 且 K 有因果数据 → 自动溯源找根因
 * 3. Kernel: 事件入队 → STEP 演化 → 知识集 K 更新
 */

#include "../include/kos_core.h"
#include "../include/kos_kernel.h"
#include "../include/kos_runtime.h"
#include "../include/kos_manufacturing.h"
#include "../include/kos_causal_trace.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* 2026-01-30 00:00:00 UTC 的 Unix 时间戳 */
#define TIME_2026_01_30  1738195200ULL

static void print_separator(const char* title) {
    printf("\n");
    printf("========================================\n");
    printf("  %s\n", title);
    printf("========================================\n");
}

static void print_step(int n, const char* desc) {
    printf("\n[Step %d] %s\n", n, desc);
}

int main(void) {
    printf("========================================\n");
    printf("KOS-TL 不合格产品信号运行过程演示\n");
    printf("信号: 检测到一个不合格的产品，其批次Batch_26_01-00001，时间为2026-01-30\n");
    printf("========================================\n");

    /* 注册制造业溯源处理器 */
    kos_manufacturing_register_traceability_handler();

    /* 创建初始状态，预置因果数据（异常 + 工艺步骤）以便溯源能找到根因 */
    kos_state_t* sigma = kos_runtime_init(NULL);
    if (!sigma) {
        printf("ERROR: 无法创建初始状态\n");
        return 1;
    }

    /* 预置 K：工艺步骤、环境异常、失败事件（因果链） */
    BatchID batch = {.batch_id = "Batch_26_01-00001"};
    ErrorCode err = {.code = "UNQUALIFIED"};
    Machine machine = {.machine_id = "HeatTreatment_05", .line_id = "Line2"};
    Param param = {.param_name = "temperature"};
    ParamValue pv = {.param = param, .value = 185.2};  /* 温度异常 185.2°C */
    TimeRange step_dur = {.start = TIME_2026_01_30 - 3600, .end = TIME_2026_01_30};  /* 前 1 小时 */

    kos_term* proc_step = kos_mk_proc_step(batch, machine, step_dur);
    kos_term* anomaly = kos_mk_anomaly(machine, param, pv, TIME_2026_01_30 - 1800);  /* 30 分钟前异常 */
    kos_term* fail_term = kos_mk_fail_event(batch, err, TIME_2026_01_30);

    if (proc_step && anomaly && fail_term) {
        sigma->K = kos_update_knowledge(sigma->K, proc_step);
        sigma->K = kos_update_knowledge(sigma->K, anomaly);
        sigma->K = kos_update_knowledge(sigma->K, fail_term);
        kos_term_free(proc_step);
        kos_term_free(anomaly);
        kos_term_free(fail_term);
    }

    print_separator("Phase 1: 自然语言信号处理");

    /* 原始信号：自然语言描述 */
    const char* natural_signal = "检测到一个不合格的产品，其批次Batch_26_01-00001，时间为2026-01-30";
    bitstream sig1 = {(unsigned char*)natural_signal, strlen(natural_signal)};

    print_step(1, "信号到达 Runtime 层");
    printf("  原始信号: %s\n", natural_signal);

    print_step(2, "elab 算子提炼信号");
    kos_signal_process_result_t result1;
    int r1 = kos_runtime_process_signal(sig1, NULL, sigma, &result1);

    if (r1 == 0 && result1.success) {
        printf("  ✓ 提炼成功：信号构成合法事件 <event, proof>\n");
        if (result1.event_pair && result1.event_pair->kind == KOS_PAIR) {
            kos_term* ev = result1.event_pair->data.pair.data;
            kos_term* pr = result1.event_pair->data.pair.proof;
            if (ev && ev->data.atomic.val)
                printf("  - event: %s\n", ev->data.atomic.val);
            if (pr && pr->data.atomic.val)
                printf("  - proof: %s\n", pr->data.atomic.val);
        }
        if (result1.root_cause_report) {
            RootCauseReport* r = (RootCauseReport*)result1.root_cause_report;
            printf("  ✓ 溯源结果: batch=%s, error=%s, 根因=%s\n",
                   r->failure.batch.batch_id, r->failure.error.code,
                   r->anomaly.param.param_name);
        } else {
            printf("  - 溯源: 无（自然语言格式无法解析为 FailEvt）\n");
        }
        kos_signal_process_result_free(&result1);
    } else {
        printf("  ✗ 提炼失败: %s\n", result1.errmsg);
    }

    print_separator("Phase 2: 结构化 FailEvt 信号处理");

    /* 结构化信号：符合 FailEvt(batch=..., error=..., time=...) 格式 */
    char structured_signal_buf[256];
    snprintf(structured_signal_buf, sizeof(structured_signal_buf),
             "FailEvt(batch=Batch_26_01-00001, error=UNQUALIFIED, time=%llu)", (unsigned long long)TIME_2026_01_30);
    bitstream sig2 = {(unsigned char*)structured_signal_buf, strlen(structured_signal_buf)};

    print_step(1, "结构化信号到达");
    printf("  信号: %s\n", structured_signal_buf);

    print_step(2, "elab 算子提炼");
    kos_signal_process_result_t result2;
    int r2 = kos_runtime_process_signal(sig2, NULL, sigma, &result2);

    if (r2 == 0 && result2.success) {
        printf("  ✓ 提炼成功\n");
        if (result2.root_cause_report) {
            RootCauseReport* r = (RootCauseReport*)result2.root_cause_report;
            printf("  ✓ 自动溯源成功:\n");
            printf("    - 失败: batch=%s, error=%s, time=%llu\n",
                   r->failure.batch.batch_id, r->failure.error.code,
                   (unsigned long long)r->failure.time);
            printf("    - 根因: %s 异常 (设备=%s, 参数=%s, 值=%.2f)\n",
                   r->anomaly.param.param_name, r->anomaly.machine.machine_id,
                   r->anomaly.param.param_name, r->anomaly.value.value);
        } else {
            printf("  - 溯源: 无匹配因果链（K 中无相关数据）\n");
        }

        print_step(3, "事件入队并执行 Kernel STEP");
        if (kos_queue_enqueue(sigma->P, result2.event_pair) == 0) {
            printf("  ✓ 事件入队\n");
            bool step_ok = kos_step(sigma);
            printf("  - STEP 演化: %s (TS=%d)\n", step_ok ? "成功" : "失败", kos_state_get_TS(sigma));
        }
        kos_signal_process_result_free(&result2);
    } else {
        printf("  ✗ 提炼失败: %s\n", result2.errmsg);
    }

    print_separator("Phase 3: 运行过程总结");

    printf("\n完整流程:\n");
    printf("  1. 信号 → elab: 物理比特流映射为 <event, proof>\n");
    printf("  2. 若 event 为 FailEvt 且 sigma->K 非空 → 自动溯源找根因\n");
    printf("  3. kos_queue_enqueue → kos_step: 事件入队、Peek-Verify-Reduce-Confirm 演化\n");
    printf("  4. K 单调增长，TS 递增\n");
    printf("\n");

    kos_runtime_free(sigma);
    printf("=== 演示完成 ===\n");
    return 0;
}
