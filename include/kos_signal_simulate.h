/**
 * @file kos_signal_simulate.h
 * @brief 外部信号模拟生成接口（传感器/质检失效事件）
 */

#ifndef KOS_SIGNAL_SIMULATE_H
#define KOS_SIGNAL_SIMULATE_H

#include "kos_runtime.h"

#ifdef __cplusplus
extern "C" {
#endif

/* 模拟传感器信号：温度/压力/振动等 */
bitstream kos_sim_signal_sensor(const char* sensor_name, double value, const char* unit);

/* 模拟质检失效事件：批次、缺陷、严重度 */
bitstream kos_sim_signal_qc_failure(const char* batch_id, const char* defect, int severity);

/* 释放由模拟器生成的信号 */
void kos_sim_signal_free(bitstream* signal);

#ifdef __cplusplus
}
#endif

#endif /* KOS_SIGNAL_SIMULATE_H */
