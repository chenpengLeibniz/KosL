/**
 * @file signal_simulate.c
 * @brief 外部信号模拟生成（传感器/质检失效事件）
 *
 * 生成的 bitstream 可直接作为 runtime 层输入使用：
 * - kos_elab / kos_elab_ex
 * - kos_runtime_process_signal
 * - kos_runtime_refine_signal
 */

#include "kos_signal_simulate.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>

static bitstream make_signal_from_text(const char* text) {
    bitstream s;
    s.data = NULL;
    s.length = 0;
    if (!text) return s;
    size_t len = strlen(text);
    if (len == 0) return s;
    unsigned char* data = (unsigned char*)malloc(len);
    if (!data) return s;
    memcpy(data, text, len);
    s.data = data;
    s.length = len;
    return s;
}

static long long current_epoch_ms(void) {
    time_t now = time(NULL);
    return (long long)now * 1000;
}

/* 模拟传感器信号：温度/压力/振动等 */
bitstream kos_sim_signal_sensor(const char* sensor_name, double value, const char* unit) {
    char buf[512];
    const char* name = sensor_name ? sensor_name : "sensor";
    const char* u = unit ? unit : "";
    snprintf(buf, sizeof(buf),
             "{\"type\":\"sensor\",\"name\":\"%s\",\"value\":%.6f,\"unit\":\"%s\",\"ts\":%lld}",
             name, value, u, current_epoch_ms());
    return make_signal_from_text(buf);
}

/* 模拟质检失效事件：批次、缺陷、严重度 */
bitstream kos_sim_signal_qc_failure(const char* batch_id, const char* defect, int severity) {
    char buf[512];
    const char* batch = batch_id ? batch_id : "batch";
    const char* def = defect ? defect : "defect";
    if (severity < 0) severity = 0;
    snprintf(buf, sizeof(buf),
             "{\"type\":\"qc_failure\",\"batch\":\"%s\",\"defect\":\"%s\",\"severity\":%d,\"ts\":%lld}",
             batch, def, severity, current_epoch_ms());
    return make_signal_from_text(buf);
}

/* 释放由模拟器生成的信号 */
void kos_sim_signal_free(bitstream* signal) {
    if (!signal) return;
    if (signal->data) {
        free(signal->data);
        signal->data = NULL;
    }
    signal->length = 0;
}
