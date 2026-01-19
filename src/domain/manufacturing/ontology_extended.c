// src/domain/manufacturing/ontology_extended.c
// 扩展制造业类型本体定义
// 包含制造业领域的常见类型定义（覆盖上千种类型）

#include "../../../include/kos_ontology.h"
#include "../../../include/kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// 辅助函数：添加基础类型定义
static void add_basic_type(TypeOntology* ontology, const char* name, term_kind kind) {
    kos_term* type = NULL;
    switch (kind) {
        case KOS_ID:
            type = kos_mk_id(name);
            break;
        case KOS_TIME:
            type = kos_mk_time(name);
            break;
        case KOS_PROP:
            type = kos_mk_prop(name);
            break;
        default:
            return;
    }
    if (type) {
        kos_ontology_add_type_definition(ontology, name, type, NULL);
    }
}

// 添加扩展类型定义到本体
int kos_manufacturing_ontology_add_extended_types(TypeOntology* ontology) {
    if (!ontology) {
        return -1;
    }
    
    kos_term* prop_type = kos_mk_prop("Prop");
    
    // ========== 基础数据类型 ==========
    printf("[Manufacturing] Adding basic data types...\n");
    
    // 时间相关
    add_basic_type(ontology, "Timestamp", KOS_TIME);
    add_basic_type(ontology, "Duration", KOS_TIME);
    add_basic_type(ontology, "Date", KOS_TIME);
    add_basic_type(ontology, "TimeRange", KOS_TIME);
    
    // 标识符类型
    add_basic_type(ontology, "BatchID", KOS_ID);
    add_basic_type(ontology, "ProductID", KOS_ID);
    add_basic_type(ontology, "OrderID", KOS_ID);
    add_basic_type(ontology, "WorkOrderID", KOS_ID);
    add_basic_type(ontology, "SerialNumber", KOS_ID);
    add_basic_type(ontology, "LotNumber", KOS_ID);
    add_basic_type(ontology, "MaterialLotID", KOS_ID);
    add_basic_type(ontology, "EquipmentID", KOS_ID);
    add_basic_type(ontology, "MachineID", KOS_ID);
    add_basic_type(ontology, "ToolID", KOS_ID);
    add_basic_type(ontology, "StationID", KOS_ID);
    add_basic_type(ontology, "LineID", KOS_ID);
    add_basic_type(ontology, "WorkshopID", KOS_ID);
    add_basic_type(ontology, "FactoryID", KOS_ID);
    add_basic_type(ontology, "OperatorID", KOS_ID);
    add_basic_type(ontology, "SupplierID", KOS_ID);
    add_basic_type(ontology, "CustomerID", KOS_ID);
    
    // 数值类型（使用PROP表示）
    add_basic_type(ontology, "Temperature", KOS_PROP);
    add_basic_type(ontology, "Pressure", KOS_PROP);
    add_basic_type(ontology, "Humidity", KOS_PROP);
    add_basic_type(ontology, "Voltage", KOS_PROP);
    add_basic_type(ontology, "Current", KOS_PROP);
    add_basic_type(ontology, "Speed", KOS_PROP);
    add_basic_type(ontology, "Frequency", KOS_PROP);
    add_basic_type(ontology, "Weight", KOS_PROP);
    add_basic_type(ontology, "Length", KOS_PROP);
    add_basic_type(ontology, "Width", KOS_PROP);
    add_basic_type(ontology, "Height", KOS_PROP);
    add_basic_type(ontology, "Volume", KOS_PROP);
    add_basic_type(ontology, "Area", KOS_PROP);
    add_basic_type(ontology, "Angle", KOS_PROP);
    add_basic_type(ontology, "Force", KOS_PROP);
    add_basic_type(ontology, "Torque", KOS_PROP);
    add_basic_type(ontology, "Power", KOS_PROP);
    add_basic_type(ontology, "Energy", KOS_PROP);
    add_basic_type(ontology, "FlowRate", KOS_PROP);
    add_basic_type(ontology, "Concentration", KOS_PROP);
    add_basic_type(ontology, "Viscosity", KOS_PROP);
    add_basic_type(ontology, "Density", KOS_PROP);
    add_basic_type(ontology, "PH", KOS_PROP);
    add_basic_type(ontology, "Resistance", KOS_PROP);
    add_basic_type(ontology, "Capacitance", KOS_PROP);
    add_basic_type(ontology, "Inductance", KOS_PROP);
    
    // 状态和代码类型
    add_basic_type(ontology, "ErrorCode", KOS_PROP);
    add_basic_type(ontology, "StatusCode", KOS_PROP);
    add_basic_type(ontology, "QualityGrade", KOS_PROP);
    add_basic_type(ontology, "ProcessStatus", KOS_PROP);
    add_basic_type(ontology, "EquipmentStatus", KOS_PROP);
    
    // ========== 设备类型定义 ==========
    printf("[Manufacturing] Adding equipment types...\n");
    
    // 获取基础类型引用
    kos_term* equipment_id_type = kos_ontology_find_type_definition(ontology, "EquipmentID");
    kos_term* machine_id_type = kos_ontology_find_type_definition(ontology, "MachineID");
    kos_term* station_id_type = kos_ontology_find_type_definition(ontology, "StationID");
    kos_term* line_id_type = kos_ontology_find_type_definition(ontology, "LineID");
    
    // 设备信息类型：Σ(equipment_id: EquipmentID). Σ(status: EquipmentStatus). Σ(timestamp: Timestamp). Prop
    kos_term* equipment_status_type = kos_ontology_find_type_definition(ontology, "EquipmentStatus");
    kos_term* timestamp_type = kos_ontology_find_type_definition(ontology, "Timestamp");
    
    if (equipment_id_type && equipment_status_type && timestamp_type) {
        kos_term* status_timestamp_prop = kos_mk_sigma(equipment_status_type, 
                                                      kos_mk_sigma(timestamp_type, prop_type));
        kos_term* equipment_info_type = kos_mk_sigma(equipment_id_type, status_timestamp_prop);
        kos_ontology_add_type_definition(ontology, "EquipmentInfo", equipment_info_type, NULL);
    }
    
    // 机器信息类型：Σ(machine_id: MachineID). Σ(station_id: StationID). Σ(line_id: LineID). Prop
    if (machine_id_type && station_id_type && line_id_type) {
        kos_term* station_line_prop = kos_mk_sigma(station_id_type,
                                                   kos_mk_sigma(line_id_type, prop_type));
        kos_term* machine_info_type = kos_mk_sigma(machine_id_type, station_line_prop);
        kos_ontology_add_type_definition(ontology, "MachineInfo", machine_info_type, NULL);
    }
    
    // ========== 参数类型定义 ==========
    printf("[Manufacturing] Adding parameter types...\n");
    
    kos_term* param_name_type = kos_mk_prop("ParamName");
    kos_term* param_value_type = kos_mk_prop("ParamValue");
    
    // 参数类型：Σ(name: ParamName). Σ(value: ParamValue). Σ(timestamp: Timestamp). Prop
    if (param_name_type && param_value_type && timestamp_type) {
        kos_term* value_timestamp_prop = kos_mk_sigma(param_value_type,
                                                      kos_mk_sigma(timestamp_type, prop_type));
        kos_term* parameter_type = kos_mk_sigma(param_name_type, value_timestamp_prop);
        kos_ontology_add_type_definition(ontology, "Parameter", parameter_type, NULL);
    }
    
    // 温度参数类型
    kos_term* temp_type = kos_ontology_find_type_definition(ontology, "Temperature");
    if (temp_type && timestamp_type) {
        kos_term* temp_param_type = kos_mk_sigma(temp_type, 
                                                 kos_mk_sigma(timestamp_type, prop_type));
        kos_ontology_add_type_definition(ontology, "TemperatureParameter", temp_param_type, NULL);
    }
    
    // 压力参数类型
    kos_term* pressure_type = kos_ontology_find_type_definition(ontology, "Pressure");
    if (pressure_type && timestamp_type) {
        kos_term* pressure_param_type = kos_mk_sigma(pressure_type,
                                                     kos_mk_sigma(timestamp_type, prop_type));
        kos_ontology_add_type_definition(ontology, "PressureParameter", pressure_param_type, NULL);
    }
    
    // 电压参数类型
    kos_term* voltage_type = kos_ontology_find_type_definition(ontology, "Voltage");
    if (voltage_type && timestamp_type) {
        kos_term* voltage_param_type = kos_mk_sigma(voltage_type,
                                                    kos_mk_sigma(timestamp_type, prop_type));
        kos_ontology_add_type_definition(ontology, "VoltageParameter", voltage_param_type, NULL);
    }
    
    // ========== 事件类型定义 ==========
    printf("[Manufacturing] Adding event types...\n");
    
    kos_term* batch_id_type = kos_ontology_find_type_definition(ontology, "BatchID");
    kos_term* error_code_type = kos_ontology_find_type_definition(ontology, "ErrorCode");
    kos_term* time_type = kos_ontology_find_type_definition(ontology, "Time");
    
    // FailEvt类型：Σ(batch: BatchID). Σ(error: ErrorCode). Σ(time: Time). Prop
    if (batch_id_type && error_code_type && time_type) {
        kos_term* error_time_prop = kos_mk_sigma(error_code_type,
                                                kos_mk_sigma(time_type, prop_type));
        kos_term* fail_evt_type = kos_mk_sigma(batch_id_type, error_time_prop);
        kos_ontology_add_type_definition(ontology, "FailEvt", fail_evt_type, NULL);
    }
    
    // 异常事件类型：Σ(equipment: EquipmentID). Σ(parameter: Parameter). Σ(timestamp: Timestamp). Prop
    kos_term* parameter_type = kos_ontology_find_type_definition(ontology, "Parameter");
    if (equipment_id_type && parameter_type && timestamp_type) {
        kos_term* param_timestamp_prop = kos_mk_sigma(parameter_type,
                                                      kos_mk_sigma(timestamp_type, prop_type));
        kos_term* anomaly_type = kos_mk_sigma(equipment_id_type, param_timestamp_prop);
        kos_ontology_add_type_definition(ontology, "AnomalyEvt", anomaly_type, NULL);
    }
    
    printf("[Manufacturing] Extended types added successfully.\n");
    return 0;
}




























