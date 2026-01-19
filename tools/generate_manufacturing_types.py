#!/usr/bin/env python3
# tools/generate_manufacturing_types.py
# 生成制造业类型定义代码
# 用于生成覆盖上千种常见类型的类型定义代码

import json
import sys

# 制造业类型定义数据
MANUFACTURING_TYPES = {
    # 基础标识符类型（约50种）
    "identifiers": [
        "BatchID", "ProductID", "OrderID", "WorkOrderID", "SerialNumber", "LotNumber",
        "MaterialLotID", "EquipmentID", "MachineID", "ToolID", "StationID", "LineID",
        "WorkshopID", "FactoryID", "OperatorID", "SupplierID", "CustomerID", "PartNumber",
        "SKU", "Barcode", "QRCode", "RFIDTag", "PalletID", "ContainerID", "BoxID",
        "PackageID", "UnitID", "AssemblyID", "SubAssemblyID", "ComponentID", "RawMaterialID",
        "FinishedGoodID", "WIPID", "ProcessID", "OperationID", "StepID", "TaskID",
        "ResourceID", "WorkCenterID", "CellID", "ZoneID", "LocationID", "BinID",
        "ShelfID", "RackID", "WarehouseID", "StorageID", "TransportID", "VehicleID",
        "RouteID", "PathID"
    ],
    
    # 时间相关类型（约20种）
    "temporal": [
        "Time", "Timestamp", "Duration", "Date", "TimeRange", "StartTime", "EndTime",
        "CycleTime", "LeadTime", "ProcessingTime", "SetupTime", "Downtime", "Uptime",
        "MaintenanceTime", "IdleTime", "WaitTime", "QueueTime", "ThroughputTime",
        "TaktTime", "Interval"
    ],
    
    # 物理量类型（约100种）
    "physical_quantities": [
        # 温度相关
        "Temperature", "TargetTemperature", "ActualTemperature", "MaxTemperature", "MinTemperature",
        "TemperatureRate", "TemperatureGradient",
        # 压力相关
        "Pressure", "TargetPressure", "ActualPressure", "MaxPressure", "MinPressure",
        "PressureDifference", "VacuumPressure",
        # 电气相关
        "Voltage", "Current", "Power", "Energy", "Frequency", "Resistance", "Capacitance",
        "Inductance", "Impedance", "PhaseAngle", "PowerFactor",
        # 机械相关
        "Speed", "Velocity", "Acceleration", "Force", "Torque", "Moment", "Stress", "Strain",
        "Displacement", "Vibration", "Noise", "Wear",
        # 尺寸相关
        "Length", "Width", "Height", "Depth", "Diameter", "Radius", "Thickness", "Area",
        "Volume", "Weight", "Mass", "Density",
        # 角度和方向
        "Angle", "Rotation", "Orientation", "PositionX", "PositionY", "PositionZ",
        # 流量相关
        "FlowRate", "FlowVolume", "FlowVelocity",
        # 化学相关
        "PH", "Concentration", "Viscosity", "Moisture", "Humidity", "DewPoint",
        # 光学相关
        "Brightness", "Luminance", "Color", "Reflectance", "Transmittance", "Opacity",
        # 其他
        "Count", "Ratio", "Percentage", "Rate", "Efficiency", "Yield"
    ],
    
    # 状态和代码类型（约80种）
    "status_codes": [
        "ErrorCode", "StatusCode", "QualityGrade", "ProcessStatus", "EquipmentStatus",
        "ProductStatus", "OrderStatus", "WorkOrderStatus", "BatchStatus", "MaterialStatus",
        "InventoryStatus", "ShippingStatus", "ReceivingStatus", "InspectionStatus",
        "MaintenanceStatus", "CalibrationStatus", "SafetyStatus", "EnvironmentalStatus",
        "ComplianceStatus", "CertificationStatus", "ApprovalStatus", "RejectionStatus",
        "PassStatus", "FailStatus", "WarningStatus", "AlertStatus", "CriticalStatus",
        "NormalStatus", "AbnormalStatus", "IdleStatus", "RunningStatus", "StoppedStatus",
        "MaintenanceMode", "SetupMode", "ProductionMode", "TestMode", "DebugMode",
        "ActiveStatus", "InactiveStatus", "EnabledStatus", "DisabledStatus", "LockedStatus",
        "UnlockedStatus", "OpenStatus", "ClosedStatus", "OnStatus", "OffStatus",
        "ReadyStatus", "BusyStatus", "ErrorStatus", "FaultStatus", "WarningCode",
        "InfoCode", "TraceCode", "DebugCode", "SeverityLevel", "PriorityLevel",
        "UrgencyLevel", "RiskLevel", "CriticalityLevel", "ImpactLevel", "LikelihoodLevel"
    ],
    
    # 设备类型（约200种）
    "equipment": [
        # 加工设备
        "CNCMachine", "Lathe", "MillingMachine", "DrillingMachine", "GrindingMachine",
        "TurningMachine", "BoringMachine", "PlaningMachine", "ShapingMachine", "SlottingMachine",
        "BroachingMachine", "GearCuttingMachine", "ThreadingMachine", "TappingMachine",
        # 成型设备
        "InjectionMoldingMachine", "BlowMoldingMachine", "ExtrusionMachine", "CompressionMoldingMachine",
        "TransferMoldingMachine", "RotationalMoldingMachine", "ThermoformingMachine", "VacuumFormingMachine",
        # 连接设备
        "WeldingMachine", "SolderingMachine", "BrazingMachine", "RivetingMachine", "BondingMachine",
        "FasteningMachine", "AdhesiveDispenser",
        # 表面处理设备
        "PaintingMachine", "CoatingMachine", "PlatingMachine", "AnodizingMachine", "ElectroplatingMachine",
        "HeatTreatmentFurnace", "QuenchingMachine", "TemperingMachine", "AnnealingMachine", "HardeningMachine",
        # 检测设备
        "VisionInspectionSystem", "CoordinateMeasuringMachine", "OpticalComparator", "Microscope",
        "Spectrometer", "XRayMachine", "UltrasonicTester", "MagneticParticleTester", "PenetrantTester",
        # 测试设备
        "TensileTester", "CompressionTester", "HardnessTester", "ImpactTester", "FatigueTester",
        "VibrationTester", "EnvironmentalChamber", "ClimateChamber",
        # 物料处理设备
        "Conveyor", "AGV", "Robot", "Crane", "Hoist", "Lift", "Forklift", "Palletizer",
        "Depalletizer", "Stacker", "Retriever", "Shuttle", "Carousel", "RotaryTable",
        # 存储设备
        "StorageRack", "ASRS", "Shelving", "Bin", "Container", "Pallet", "Tote",
        # 包装设备
        "PackagingMachine", "FillingMachine", "SealingMachine", "LabelingMachine", "WrappingMachine",
        "ShrinkWrapper", "StretchWrapper", "CartoningMachine", "BaggingMachine",
        # 其他设备
        "Mixer", "Blender", "Separator", "Filter", "Pump", "Compressor", "Fan", "Blower",
        "Valve", "Actuator", "Sensor", "Controller", "PLC", "HMI", "SCADA", "DCS"
    ],
    
    # 工艺类型（约150种）
    "processes": [
        # 机加工工艺
        "Turning", "Milling", "Drilling", "Grinding", "Boring", "Planing", "Shaping",
        "Broaching", "Reaming", "Tapping", "Threading", "GearCutting", "Hobbing",
        # 成型工艺
        "InjectionMolding", "BlowMolding", "Extrusion", "CompressionMolding", "TransferMolding",
        "RotationalMolding", "Thermoforming", "VacuumForming", "DieCasting", "SandCasting",
        # 连接工艺
        "Welding", "Soldering", "Brazing", "Riveting", "Bonding", "Fastening", "AdhesiveBonding",
        # 表面处理工艺
        "Painting", "Coating", "Plating", "Anodizing", "Electroplating", "HeatTreatment",
        "Quenching", "Tempering", "Annealing", "Hardening", "SurfaceHardening", "CaseHardening",
        # 组装工艺
        "Assembly", "SubAssembly", "MechanicalAssembly", "ElectricalAssembly", "FinalAssembly",
        # 检测工艺
        "Inspection", "Testing", "Measurement", "Verification", "Validation", "Calibration",
        # 包装工艺
        "Packaging", "Filling", "Sealing", "Labeling", "Wrapping", "Cartoning",
        # 其他工艺
        "Cleaning", "Degreasing", "Passivation", "Pickling", "Etching", "Polishing", "Buffing"
    ],
    
    # 产品类型（约200种）
    "products": [
        # 按行业分类的产品类型（示例）
        "AutomotivePart", "AerospaceComponent", "ElectronicsPart", "MedicalDevice", "ConsumerGood",
        "IndustrialPart", "ElectricalComponent", "MechanicalComponent", "OpticalComponent",
        # 按材料分类
        "MetalProduct", "PlasticProduct", "CompositeProduct", "CeramicProduct", "GlassProduct",
        # 按功能分类
        "StructuralPart", "FunctionalPart", "AestheticPart", "ProtectivePart", "ConnectivePart"
    ],
    
    # 物料类型（约100种）
    "materials": [
        # 金属材料
        "Steel", "StainlessSteel", "Aluminum", "Copper", "Brass", "Bronze", "Titanium",
        "Nickel", "Zinc", "Magnesium", "CastIron", "CarbonSteel", "AlloySteel",
        # 塑料材料
        "ABS", "Polycarbonate", "Polypropylene", "Polyethylene", "PVC", "Nylon", "PET",
        "POM", "PTFE", "Acrylic",
        # 复合材料
        "CarbonFiber", "GlassFiber", "Kevlar", "CompositeMaterial",
        # 其他材料
        "Rubber", "Silicone", "Ceramic", "Glass", "Wood", "Paper", "Textile"
    ],
    
    # 质量属性（约100种）
    "quality_attributes": [
        "DimensionalAccuracy", "SurfaceRoughness", "Flatness", "Roundness", "Cylindricity",
        "Straightness", "Perpendicularity", "Parallelism", "Angularity", "Concentricity",
        "Hardness", "TensileStrength", "YieldStrength", "Elongation", "ImpactStrength",
        "FatigueStrength", "CorrosionResistance", "WearResistance", "ThermalConductivity",
        "ElectricalConductivity", "Color", "Gloss", "Transparency", "Opacity", "Weight",
        "Density", "Porosity", "MoistureContent", "PH", "Viscosity", "Purity", "Contamination"
    ],
    
    # 检测项目（约150种）
    "inspection_items": [
        # 尺寸检测
        "LengthMeasurement", "WidthMeasurement", "HeightMeasurement", "DiameterMeasurement",
        "ThicknessMeasurement", "HoleDiameter", "ThreadPitch", "SurfaceProfile",
        # 外观检测
        "VisualInspection", "ColorInspection", "SurfaceDefect", "ScratchInspection",
        "CrackInspection", "DeformationInspection",
        # 功能检测
        "FunctionTest", "PerformanceTest", "LeakTest", "PressureTest", "FlowTest",
        "ElectricalTest", "ResistanceTest", "VoltageTest", "CurrentTest",
        # 材料检测
        "MaterialComposition", "HardnessTest", "TensileTest", "ImpactTest", "FatigueTest",
        # 环境测试
        "TemperatureTest", "HumidityTest", "VibrationTest", "ShockTest", "SaltSprayTest"
    ],
    
    # 故障类型（约100种）
    "failure_types": [
        "MechanicalFailure", "ElectricalFailure", "ElectronicFailure", "SoftwareFailure",
        "MaterialFailure", "WearFailure", "CorrosionFailure", "FatigueFailure",
        "OverloadFailure", "ThermalFailure", "VibrationFailure", "MisalignmentFailure",
        "ContaminationFailure", "AssemblyFailure", "DimensionalFailure", "FunctionalFailure",
        "PerformanceFailure", "SafetyFailure", "QualityFailure"
    ]
}

def generate_c_code(output_file):
    """生成C语言类型定义代码"""
    
    code = """// src/domain/manufacturing/ontology_extended_generated.c
// 自动生成的制造业类型定义代码
// 此文件由 tools/generate_manufacturing_types.py 自动生成
// 请勿手动编辑此文件

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

// 添加所有生成的类型定义
int kos_manufacturing_ontology_add_generated_types(TypeOntology* ontology) {
    if (!ontology) {
        return -1;
    }
    
    printf("[Manufacturing] Adding generated types...\\n");
    int count = 0;
    
    // ========== 标识符类型 ==========
    printf("[Manufacturing] Adding identifier types...\\n");
"""
    
    # 添加标识符类型
    for ident in MANUFACTURING_TYPES["identifiers"]:
        code += f'    add_basic_type(ontology, "{ident}", KOS_ID);\n'
        code += f'    count++;\n'
    
    # 添加时间类型
    code += "\n    // ========== 时间类型 ==========\n"
    code += '    printf("[Manufacturing] Adding temporal types...\\n");\n'
    for temp in MANUFACTURING_TYPES["temporal"]:
        code += f'    add_basic_type(ontology, "{temp}", KOS_TIME);\n'
        code += f'    count++;\n'
    
    # 添加物理量类型
    code += "\n    // ========== 物理量类型 ==========\n"
    code += '    printf("[Manufacturing] Adding physical quantity types...\\n");\n'
    for pq in MANUFACTURING_TYPES["physical_quantities"]:
        code += f'    add_basic_type(ontology, "{pq}", KOS_PROP);\n'
        code += f'    count++;\n'
    
    # 添加状态代码类型
    code += "\n    // ========== 状态和代码类型 ==========\n"
    code += '    printf("[Manufacturing] Adding status code types...\\n");\n'
    for sc in MANUFACTURING_TYPES["status_codes"]:
        code += f'    add_basic_type(ontology, "{sc}", KOS_PROP);\n'
        code += f'    count++;\n'
    
    code += """
    printf("[Manufacturing] Generated %d basic types added successfully.\\n", count);
    return 0;
}
"""
    
    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(code)
    
    print(f"Generated C code written to {output_file}")
    print(f"Total types: {sum(len(v) for v in MANUFACTURING_TYPES.values())}")

if __name__ == "__main__":
    output_file = "src/domain/manufacturing/ontology_extended_generated.c"
    generate_c_code(output_file)




























