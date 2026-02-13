/**
 * @file kos_tl_end_to_end_demo.c
 * @brief 贯穿 KOS-TL 的端到端示例
 *
 * 流程：
 * 1) kos-core 加载 quality_traceability.kos
 * 2) 初始化 Γ / σ
 * 3) 模拟 10 个传感器信号，扩展 Γ 与 σ
 * 4) 模拟质检失效事件，入队 σ.P
 * 5) kernel 演化 + 根因溯源，构建根因实例并写入 σ
 */

#include "kos_kernel_session.h"
#include "kos_kernel_context.h"
#include "kos_kernel.h"
#include "kos_runtime.h"
#include "kos_signal_simulate.h"
#include "kos_core.h"
#include <stdio.h>
#include <string.h>
#include <ctype.h>

static void make_gamma_name(const char* sensor_name, char* out, size_t out_size) {
    if (!out || out_size == 0) return;
    const char* prefix = "Sensor_";
    size_t i = 0;
    for (; prefix[i] && i + 1 < out_size; i++) {
        out[i] = prefix[i];
    }
    if (!sensor_name) {
        out[i] = '\0';
        return;
    }
    for (size_t j = 0; sensor_name[j] && i + 1 < out_size; j++) {
        unsigned char c = (unsigned char)sensor_name[j];
        out[i++] = (isalnum(c) || c == '_') ? (char)c : '_';
    }
    out[i] = '\0';
}

static void gamma_add_prop(kos_kernel_context_t* gamma, const char* name) {
    if (!gamma || !name || !name[0]) return;
    if (kos_gamma_lookup_type(gamma, name)) return;
    kos_term* prop = kos_mk_prop(name);
    if (!prop) return;
    kos_gamma_add_type(gamma, name, prop);
    kos_term_free(prop);
}

int main(int argc, char** argv) {
    const char* ctx_path = (argc > 1 && argv[1]) ? argv[1]
        : "kos-core/examples/quality_traceability.kos";

    kos_kernel_session_t* session = kos_kernel_session_create(ctx_path);
    if (!session) {
        fprintf(stderr, "Failed to create kernel session\n");
        return 1;
    }
    if (kos_kernel_session_load_ontology(session, ctx_path) != 0) {
        fprintf(stderr, "Failed to load ontology: %s\n", ctx_path);
        kos_kernel_session_free(session);
        return 1;
    }

    kos_kernel_context_t* gamma = kos_kernel_session_get_gamma(session);
    kos_state_t* sigma = kos_kernel_session_get_sigma(session);
    if (!gamma || !sigma) {
        fprintf(stderr, "Gamma/Sigma not initialized\n");
        kos_kernel_session_free(session);
        return 1;
    }

    const char* names[] = {
        "Temp", "Pressure", "Vibration", "Voltage", "Current",
        "Humidity", "Speed", "Flow", "Torque", "Noise"
    };
    const char* units[] = {
        "C", "kPa", "mm/s", "V", "A", "%", "rpm", "L/min", "Nm", "dB"
    };
    const double values[] = {
        36.5, 101.2, 3.8, 220.0, 5.2, 45.0, 1200.0, 30.5, 18.9, 65.0
    };

    for (size_t i = 0; i < 10; i++) {
        bitstream sig = kos_sim_signal_sensor(names[i], values[i], units[i]);
        kos_signal_process_result_t res;
        if (kos_runtime_process_signal(sig, NULL, sigma, &res) == 0 && res.success) {
            char type_name[64];
            make_gamma_name(names[i], type_name, sizeof(type_name));
            gamma_add_prop(gamma, type_name);
            sigma->K = kos_update_knowledge(sigma->K, res.event_pair);
            printf("[sensor] %s -> K updated, Gamma +%s\n", names[i], type_name);
        } else {
            fprintf(stderr, "[sensor] %s failed: %s\n", names[i], res.errmsg);
        }
        kos_signal_process_result_free(&res);
        kos_sim_signal_free(&sig);
    }

    /* 质检失效事件：入队 σ.P */
    bitstream qc = kos_sim_signal_qc_failure("B2310", "HARD_ERR", 2);
    kos_signal_process_result_t qc_res;
    if (kos_runtime_process_signal(qc, NULL, sigma, &qc_res) == 0 && qc_res.success) {
        gamma_add_prop(gamma, "FailEvt");
        if (kos_kernel_session_enqueue_event(session, qc_res.event_pair) == 0) {
            printf("[qc] failure enqueued into sigma.P\n");
        } else {
            fprintf(stderr, "[qc] enqueue failed\n");
        }
    } else {
        fprintf(stderr, "[qc] failed: %s\n", qc_res.errmsg);
    }
    kos_signal_process_result_free(&qc_res);
    kos_sim_signal_free(&qc);

    /* kernel 演化：处理事件队列 */
    size_t steps = 0;
    while (sigma->P && !kos_queue_is_empty(sigma->P)) {
        if (!kos_kernel_session_step(session)) {
            fprintf(stderr, "Kernel step failed\n");
            break;
        }
        steps++;
    }
    printf("[kernel] steps executed: %zu\n", steps);

    /* 根因溯源 */
    char errbuf[512];
    kos_term* proof = NULL;
    if (kos_kernel_session_find_root_cause(session, ctx_path, "RootCauseReport",
                                           &proof, errbuf, sizeof(errbuf))) {
        sigma->K = kos_update_knowledge(sigma->K, proof);
        printf("[root-cause] RootCauseReport added to sigma.K\n");
        kos_term_free(proof);
    } else {
        fprintf(stderr, "[root-cause] not found: %s\n", errbuf);
    }

    kos_kernel_session_free(session);
    return 0;
}
