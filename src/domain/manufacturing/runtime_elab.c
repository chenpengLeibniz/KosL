// src/domain/manufacturing/runtime_elab.c
// 制造业领域 Runtime 层数据提炼（基于类型论的运行时精化）
// 
// 实现方式：
// 1. 从JSON文件加载本体类型库到内存
// 2. 运行时通过外部信号精化事件，从本体库查找类型定义
// 3. 根据类型定义构造事件实例，自动进行类型检查
// 4. 成功创建事件后进行相应处理

#include "../../../include/kos_ontology.h"
#include "../../../include/kos_core.h"
#include "../../../include/kos_runtime.h"
#include "../../../include/kos_manufacturing.h"  // 定义Time类型
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>

// ========== 本体访问接口 ==========
// 本体管理由 ontology_manager.c 统一负责，这里只提供访问接口

// ========== 辅助函数：从原始数据构造基础类型值 ==========

// 构造BatchID类型的值
static kos_term* mk_batch_id_value(const char* batch_id_str) {
    return kos_mk_id(batch_id_str);
}

// 构造ErrorCode类型的值
static kos_term* mk_error_code_value(const char* error_code_str) {
    return kos_mk_prop(error_code_str);
}

// 构造Time类型的值
static kos_term* mk_time_value(Time time) {
    char time_str[32];
    snprintf(time_str, sizeof(time_str), "%llu", (unsigned long long)time);
    return kos_mk_time(time_str);
}

// 构造Machine类型的值
static kos_term* mk_machine_value(const char* machine_id_str) {
    return kos_mk_id(machine_id_str);
}

// 构造Param类型的值
static kos_term* mk_param_value(const char* param_name_str) {
    return kos_mk_prop(param_name_str);
}

// 构造ParamValue类型的值（简化为Prop）
static kos_term* mk_param_value_value(const char* param_name_str, double value) {
    char value_str[64];
    snprintf(value_str, sizeof(value_str), "%s=%.2f", param_name_str, value);
    return kos_mk_prop(value_str);
}

// ========== 辅助函数：构造Σ类型实例 ==========

// 构造嵌套的Σ类型实例
// 对于 FailEvt = Σ(b: BatchID). Σ(err: ErrorCode). Σ(t: Time). Prop
// 需要构造：<batch, <error, <time, prop>>>
static kos_term* mk_sigma_instance_recursive(TypeOntology* ontology, 
                                             kos_term* type_def, 
                                             kos_term** values, 
                                             size_t value_count,
                                             size_t* current_index) {
    if (!type_def || type_def->kind != KOS_SIGMA) {
        return NULL;
    }
    
    if (*current_index >= value_count) {
        return NULL; // 值不足
    }
    
    // 获取body（domain在递归中用于类型检查，但这里不需要存储）
    kos_term* body = type_def->data.sigma.body;
    
    // 当前值
    kos_term* current_value = values[*current_index];
    (*current_index)++;
    
    // 如果是最后一个Σ（body是Prop），构造<value, prop>
    if (body && body->kind == KOS_PROP) {
        kos_term* prop_value = kos_mk_prop("Prop");
        return kos_mk_pair(current_value, prop_value);
    }
    
    // 如果body是另一个Σ类型，递归构造
    if (body && body->kind == KOS_SIGMA) {
        kos_term* nested_pair = mk_sigma_instance_recursive(ontology, body, values, value_count, current_index);
        if (!nested_pair) {
            return NULL;
        }
        return kos_mk_pair(current_value, nested_pair);
    }
    
    // 其他情况暂不支持
    return NULL;
}

// ========== 主要精化函数 ==========

// 从原始数据提炼失败事件（基于类型定义）
// 输入：原始数据流（格式：BatchID,ErrorCode,Time）
// 返回：精化后的FailEvt事件实例（已通过类型检查），失败返回NULL
kos_term* kos_elab_failure_event(bitstream raw_data, kos_term* ontology_ctx) {
    (void)ontology_ctx; // 暂不使用，从全局本体获取
    
    if (!raw_data.data || raw_data.length == 0) {
        return NULL;
    }
    
    // 步骤1：获取本体类型库（通过统一的管理接口）
    TypeOntology* ontology = kos_manufacturing_ontology_get();
    if (!ontology) {
        printf("[Elab] ERROR: Ontology not available\n");
        return NULL;
    }
    
    // 步骤2：查找FailEvt类型定义
    kos_term* fail_evt_type = kos_ontology_find_type_definition(ontology, "FailEvt");
    if (!fail_evt_type) {
        printf("[Elab] ERROR: FailEvt type definition not found in ontology\n");
        return NULL;
    }
    
    // 步骤3：解析原始数据
    char* data_str = (char*)malloc(raw_data.length + 1);
    if (!data_str) {
        return NULL;
    }
    memcpy(data_str, raw_data.data, raw_data.length);
    data_str[raw_data.length] = '\0';
    
    // 解析格式：BatchID,ErrorCode,Time
    char* batch_id = data_str;
    char* error_code = strchr(data_str, ',');
    if (!error_code) {
        free(data_str);
        return NULL;
    }
    *error_code++ = '\0';
    
    char* time_str = strchr(error_code, ',');
    if (!time_str) {
        free(data_str);
        return NULL;
    }
    *time_str++ = '\0';
    
    Time time_val = (Time)strtoull(time_str, NULL, 10);
    
    // 步骤4：构造基础类型值
    kos_term* batch_value = mk_batch_id_value(batch_id);
    kos_term* error_value = mk_error_code_value(error_code);
    kos_term* time_value = mk_time_value(time_val);
    
    if (!batch_value || !error_value || !time_value) {
        if (batch_value) kos_term_free(batch_value);
        if (error_value) kos_term_free(error_value);
        if (time_value) kos_term_free(time_value);
        free(data_str);
        return NULL;
    }
    
    // 步骤5：构造嵌套的Σ类型实例
    // FailEvt = Σ(b: BatchID). Σ(err: ErrorCode). Σ(t: Time). Prop
    // 构造：<batch, <error, <time, prop>>>
    kos_term* values[] = {batch_value, error_value, time_value};
    size_t current_index = 0;
    kos_term* instance = mk_sigma_instance_recursive(ontology, fail_evt_type, values, 3, &current_index);
    
    // 清理临时值
    kos_term_free(batch_value);
    kos_term_free(error_value);
    kos_term_free(time_value);
    free(data_str);
    
    if (!instance) {
        printf("[Elab] ERROR: Failed to construct FailEvt instance\n");
        return NULL;
    }
    
    // 步骤6：使用本体API创建并验证实例（自动类型检查）
    kos_term* validated_instance = kos_ontology_mk_type_instance(ontology, "FailEvt", instance, NULL);
    
    // 如果验证失败，清理instance
    if (!validated_instance) {
        printf("[Elab] ERROR: Type validation failed for FailEvt instance\n");
        kos_term_free(instance);
        return NULL;
    }
    
    // instance已经被复制到validated_instance，可以释放
    kos_term_free(instance);
    
    printf("[Elab] ✓ FailEvt instance created and validated: Batch=%s, Error=%s, Time=%llu\n",
           batch_id, error_code, (unsigned long long)time_val);
    
    // 步骤7：成功创建事件后，进行相应处理
    // 这里可以触发事件处理流程，例如添加到事件队列、触发推理等
    
    return validated_instance;
}

// 从原始数据提炼异常事件（基于类型定义）
// 输入：原始数据流（格式：MachineID,ParamName,Value,Time）
// 返回：精化后的Anomaly事件实例（已通过类型检查），失败返回NULL
kos_term* kos_elab_anomaly(bitstream raw_data, kos_term* ontology_ctx) {
    (void)ontology_ctx; // 暂不使用，从全局本体获取
    
    if (!raw_data.data || raw_data.length == 0) {
        return NULL;
    }
    
    // 步骤1：获取本体类型库（通过统一的管理接口）
    TypeOntology* ontology = kos_manufacturing_ontology_get();
    if (!ontology) {
        printf("[Elab] ERROR: Ontology not available\n");
        return NULL;
    }
    
    // 步骤2：查找Anomaly类型定义
    kos_term* anomaly_type = kos_ontology_find_type_definition(ontology, "Anomaly");
    if (!anomaly_type) {
        printf("[Elab] ERROR: Anomaly type definition not found in ontology\n");
        return NULL;
    }
    
    // 步骤3：解析原始数据
    char* data_str = (char*)malloc(raw_data.length + 1);
    if (!data_str) {
        return NULL;
    }
    memcpy(data_str, raw_data.data, raw_data.length);
    data_str[raw_data.length] = '\0';
    
    // 解析格式：MachineID,ParamName,Value,Time
    char* machine_id = data_str;
    char* param_name = strchr(data_str, ',');
    if (!param_name) {
        free(data_str);
        return NULL;
    }
    *param_name++ = '\0';
    
    char* value_str = strchr(param_name, ',');
    if (!value_str) {
        free(data_str);
        return NULL;
    }
    *value_str++ = '\0';
    
    char* time_str = strchr(value_str, ',');
    if (!time_str) {
        free(data_str);
        return NULL;
    }
    *time_str++ = '\0';
    
    double value = strtod(value_str, NULL);
    Time time_val = (Time)strtoull(time_str, NULL, 10);
    
    // 步骤4：构造基础类型值
    kos_term* machine_value = mk_machine_value(machine_id);
    kos_term* param_value = mk_param_value(param_name);
    kos_term* param_value_value = mk_param_value_value(param_name, value);
    kos_term* time_value = mk_time_value(time_val);
    
    if (!machine_value || !param_value || !param_value_value || !time_value) {
        if (machine_value) kos_term_free(machine_value);
        if (param_value) kos_term_free(param_value);
        if (param_value_value) kos_term_free(param_value_value);
        if (time_value) kos_term_free(time_value);
        free(data_str);
        return NULL;
    }
    
    // 步骤5：构造嵌套的Σ类型实例
    // Anomaly = Σ(m: Machine). Σ(p: Param). Σ(v: ParamValue). Σ(t: Time). Prop
    kos_term* values[] = {machine_value, param_value, param_value_value, time_value};
    size_t current_index = 0;
    kos_term* instance = mk_sigma_instance_recursive(ontology, anomaly_type, values, 4, &current_index);
    
    // 清理临时值
    kos_term_free(machine_value);
    kos_term_free(param_value);
    kos_term_free(param_value_value);
    kos_term_free(time_value);
    free(data_str);
    
    if (!instance) {
        printf("[Elab] ERROR: Failed to construct Anomaly instance\n");
        return NULL;
    }
    
    // 步骤6：使用本体API创建并验证实例（自动类型检查）
    kos_term* validated_instance = kos_ontology_mk_type_instance(ontology, "Anomaly", instance, NULL);
    
    if (!validated_instance) {
        printf("[Elab] ERROR: Type validation failed for Anomaly instance\n");
        kos_term_free(instance);
        return NULL;
    }
    
    kos_term_free(instance);
    
    printf("[Elab] ✓ Anomaly instance created and validated: Machine=%s, Param=%s, Value=%.2f, Time=%llu\n",
           machine_id, param_name, value, (unsigned long long)time_val);
    
    // 步骤7：成功创建事件后，进行相应处理
    
    return validated_instance;
}

// 从原始数据提炼工艺步骤（基于类型定义）
// 输入：原始数据流（格式：WOID,BatchID,StepName,StartTime,EndTime,MachineID）
// 返回：精化后的ProcStep事件实例（已通过类型检查），失败返回NULL
kos_term* kos_elab_process_step(bitstream raw_data, kos_term* ontology_ctx) {
    (void)ontology_ctx; // 暂不使用，从全局本体获取
    
    if (!raw_data.data || raw_data.length == 0) {
        return NULL;
    }
    
    // 步骤1：获取本体类型库（通过统一的管理接口）
    TypeOntology* ontology = kos_manufacturing_ontology_get();
    if (!ontology) {
        printf("[Elab] ERROR: Ontology not available\n");
        return NULL;
    }
    
    // 步骤2：查找ProcStep类型定义
    kos_term* proc_step_type = kos_ontology_find_type_definition(ontology, "ProcStep");
    if (!proc_step_type) {
        printf("[Elab] ERROR: ProcStep type definition not found in ontology\n");
        return NULL;
    }
    
    // 步骤3：解析原始数据（简化实现）
    char* data_str = (char*)malloc(raw_data.length + 1);
    if (!data_str) {
        return NULL;
    }
    memcpy(data_str, raw_data.data, raw_data.length);
    data_str[raw_data.length] = '\0';
    
    // 解析格式：WOID,BatchID,StepName,StartTime,EndTime,MachineID
    // 简化：只提取关键字段 BatchID, MachineID, TimeRange
    char* wo_id = data_str;
    char* batch_id = strchr(wo_id, ',');
    if (!batch_id) {
        free(data_str);
        return NULL;
    }
    *batch_id++ = '\0';
    
    char* step_name = strchr(batch_id, ',');
    if (!step_name) {
        free(data_str);
        return NULL;
    }
    *step_name++ = '\0';
    
    char* start_time_str = strchr(step_name, ',');
    if (!start_time_str) {
        free(data_str);
        return NULL;
    }
    *start_time_str++ = '\0';
    
    char* end_time_str = strchr(start_time_str, ',');
    if (!end_time_str) {
        free(data_str);
        return NULL;
    }
    *end_time_str++ = '\0';
    
    char* machine_id = strchr(end_time_str, ',');
    if (!machine_id) {
        free(data_str);
        return NULL;
    }
    *machine_id++ = '\0';
    
    Time start_time_val = (Time)strtoull(start_time_str, NULL, 10);
    Time end_time_val = (Time)strtoull(end_time_str, NULL, 10);
    
    // 步骤4：构造基础类型值
    kos_term* batch_value = mk_batch_id_value(batch_id);
    kos_term* machine_value = mk_machine_value(machine_id);
    
    // TimeRange：简化为Prop（实际应该构造更复杂的TimeRange类型）
    char time_range_str[128];
    snprintf(time_range_str, sizeof(time_range_str), "TimeRange[%llu,%llu]", 
             (unsigned long long)start_time_val, (unsigned long long)end_time_val);
    kos_term* time_range_value = kos_mk_prop(time_range_str);
    
    if (!batch_value || !machine_value || !time_range_value) {
        if (batch_value) kos_term_free(batch_value);
        if (machine_value) kos_term_free(machine_value);
        if (time_range_value) kos_term_free(time_range_value);
        free(data_str);
        return NULL;
    }
    
    // 步骤5：构造嵌套的Σ类型实例
    // ProcStep = Σ(b: BatchID). Σ(m: Machine). Σ(dur: TimeRange). Prop
    kos_term* values[] = {batch_value, machine_value, time_range_value};
    size_t current_index = 0;
    kos_term* instance = mk_sigma_instance_recursive(ontology, proc_step_type, values, 3, &current_index);
    
    // 清理临时值
    kos_term_free(batch_value);
    kos_term_free(machine_value);
    kos_term_free(time_range_value);
    free(data_str);
    
    if (!instance) {
        printf("[Elab] ERROR: Failed to construct ProcStep instance\n");
        return NULL;
    }
    
    // 步骤6：使用本体API创建并验证实例（自动类型检查）
    kos_term* validated_instance = kos_ontology_mk_type_instance(ontology, "ProcStep", instance, NULL);
    
    if (!validated_instance) {
        printf("[Elab] ERROR: Type validation failed for ProcStep instance\n");
        kos_term_free(instance);
        return NULL;
    }
    
    kos_term_free(instance);
    
    printf("[Elab] ✓ ProcStep instance created and validated: Batch=%s, Machine=%s, Duration=[%llu,%llu]\n",
           batch_id, machine_id, (unsigned long long)start_time_val, (unsigned long long)end_time_val);
    
    // 步骤7：成功创建事件后，进行相应处理
    
    return validated_instance;
}
