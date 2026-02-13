cmake_minimum_required(VERSION 3.10)
project(KosL C)

# 设置 C 标准
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# 编译选项 (根据编译器类型设置)
if(MSVC)
    # 解决 /O2 与 /RTC1 不兼容：从 Debug 配置的默认标志中移除 /RTC1
    # （CMake 默认 Debug 会添加 /RTC1，与 /O2 冲突）
    string(REPLACE "/RTC1" "" CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}" CACHE STRING "Flags for C compiler in Debug" FORCE)
    # MSVC 编译选项
    # /utf-8: 指定源文件和执行字符集为UTF-8，解决中文注释编码问题
    add_compile_options(/W4 /O2 /utf-8)
else()
    # GCC/Clang 编译选项
    add_compile_options(-Wall -Wextra -O2)
endif()

# 包含目录
include_directories(include)
include_directories(src/domain/manufacturing)

# 收集源文件
set(CORE_SOURCES
    src/core/type_builder.c
    src/core/storage.c
    src/core/universe.c
    src/core/core_kos_adapter.c
    src/core/kos_core_bridge.c
)

set(KERNEL_SOURCES
    src/kernel/state_step.c
    src/kernel/knowledge_base.c
    src/kernel/event_op_registry.c
    src/kernel/kernel_context.c
    src/kernel/trace.c
    src/kernel/kernel_session.c
    src/kernel/kernel_scenarios.c
)

set(RUNTIME_SOURCES
    src/runtime/elab.c
    src/runtime/material.c
    src/runtime/system_init.c
    src/runtime/storage_manager.c
    src/runtime/scheduler_relay.c
    src/runtime/replay.c
    src/runtime/signal_process.c
    src/runtime/signal_source_registry.c
)

set(UTILS_SOURCES
    src/utils/signal_simulate.c
)

set(QUERY_SOURCES
    src/query/query_executor.c
    src/query/query_parser.c
    src/query/time_index.c
    src/query/graph_index.c
)

set(ONTOLOGY_VERSION_SOURCES
    src/ontology/version_manager.c
    src/ontology/runtime_update.c
    src/ontology/impact_analysis.c
    src/ontology/dependency_analyzer.c
    src/ontology/change_propagation.c
)

set(INTEGRATION_SOURCES
    src/integration/connector_registry.c
    src/integration/csv_connector.c
    src/integration/json_connector.c
    src/integration/fusion_engine.c
    src/integration/fusion_engine_enhanced.c
    src/integration/builtin_connectors.c
)

set(STREAM_SOURCES
    src/stream/pipeline.c
    src/stream/aggregation.c
    src/stream/aggregation_integration.c
)

set(API_SOURCES
    src/api/rest_server.c
    src/api/rest_endpoints.c
)

set(SECURITY_SOURCES
    src/security/rbac.c
    src/security/audit.c
    src/security/security_context.c
)

set(PERFORMANCE_SOURCES
    src/performance/query_cache.c
    src/performance/index_manager.c
    src/performance/bplus_tree.c
    src/performance/inverted_index.c
    src/performance/query_optimizer.c
    src/performance/distributed_cache.c
)

set(VISUALIZATION_ENHANCED_SOURCES
    src/visualization/query_result_viz.c
    src/visualization/query_result_viz_enhanced.c
    src/visualization/query_history.c
)

set(MANUFACTURING_SOURCES
    src/domain/manufacturing/types.c
    src/domain/manufacturing/predicates.c
    src/domain/manufacturing/causal_index.c
    src/domain/manufacturing/traceability.c
    src/domain/manufacturing/runtime_elab.c
    src/domain/manufacturing/ontology_setup.c
    src/domain/manufacturing/ontology_manager.c
    src/domain/manufacturing/ontology_crud.c
    src/domain/manufacturing/ontology_extended_generated.c
)

set(FINANCE_SOURCES
    src/domain/finance/types.c
    src/domain/finance/runtime_elab.c
    src/domain/finance/ontology_setup.c
    src/domain/finance/ontology_manager.c
    src/domain/finance/ontology_extended_generated.c
)

set(ONTOLOGY_SOURCES
    src/core/ontology_manager.c
    src/ontology/ontology_registry.c
    src/ontology/reasoning_session.c
)

set(VISUALIZATION_SOURCES
    src/core/visualization.c
)

set(MAIN_SOURCE
    main.c
)

# 所有源文件
set(ALL_SOURCES
    ${CORE_SOURCES}
    ${KERNEL_SOURCES}
    ${RUNTIME_SOURCES}
    ${ONTOLOGY_SOURCES}
    ${MANUFACTURING_SOURCES}
    ${FINANCE_SOURCES}
    ${MAIN_SOURCE}
)

# 创建可执行文件
add_executable(kos_system ${ALL_SOURCES})

# 链接 pthread 库
if(UNIX)
    target_link_libraries(kos_system pthread)
elseif(WIN32)
    # Windows 上 pthread 可能需要额外的库
    # 如果使用 MinGW，可能需要链接 pthread
    find_library(PTHREAD_LIB pthread)
    if(PTHREAD_LIB)
        target_link_libraries(kos_system ${PTHREAD_LIB})
    endif()
endif()

# 设置输出目录
set_target_properties(kos_system PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# 创建金融领域演示程序
add_executable(finance_demo
    examples/finance_demo.c
    ${CORE_SOURCES}
    ${KERNEL_SOURCES}
    ${RUNTIME_SOURCES}
    ${ONTOLOGY_SOURCES}
    ${FINANCE_SOURCES}
    ${VISUALIZATION_SOURCES}
)

# 设置输出目录
set_target_properties(finance_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# 创建制造业演示程序
add_executable(manufacturing_demo
    examples/manufacturing_demo.c
    ${CORE_SOURCES}
    ${KERNEL_SOURCES}
    ${RUNTIME_SOURCES}
    ${ONTOLOGY_SOURCES}
    ${MANUFACTURING_SOURCES}
)

# 创建可视化工具
add_executable(visualize_ontology
    tools/visualize_ontology.c
    ${CORE_SOURCES}
    ${ONTOLOGY_SOURCES}
    ${VISUALIZATION_SOURCES}
)

# 创建可视化测试工具（从程序初始化本体）
add_executable(test_visualization
    tools/test_visualization.c
    ${CORE_SOURCES}
    ${ONTOLOGY_SOURCES}
    ${VISUALIZATION_SOURCES}
    ${MANUFACTURING_SOURCES}
)

# 从JSON文件生成可视化工具
add_executable(generate_visualization_from_json
    tools/generate_visualization_from_json.c
    ${CORE_SOURCES}
    ${ONTOLOGY_SOURCES}
    ${VISUALIZATION_SOURCES}
    ${MANUFACTURING_SOURCES}
)

# HTML可视化生成工具（使用制造业本体初始化）
add_executable(generate_html_visualization
    tools/generate_html_visualization.c
    ${CORE_SOURCES}
    ${ONTOLOGY_SOURCES}
    ${VISUALIZATION_SOURCES}
    ${MANUFACTURING_SOURCES}
)

# 设置可视化工具输出目录
set_target_properties(visualize_ontology PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

set_target_properties(test_visualization PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

set_target_properties(generate_visualization_from_json PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

set_target_properties(generate_html_visualization PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# 设置演示程序输出目录
set_target_properties(manufacturing_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# 链接 pthread 库（如果需要）
if(UNIX)
    target_link_libraries(manufacturing_demo pthread)
elseif(WIN32)
    find_library(PTHREAD_LIB pthread)
    if(PTHREAD_LIB)
        target_link_libraries(manufacturing_demo ${PTHREAD_LIB})
    endif()
endif()

# ========== kos-core Bridge Demo ==========
add_executable(kos_core_bridge_demo
    examples/kos_core_bridge_demo.c
    ${CORE_SOURCES}
    ${RUNTIME_SOURCES}
    ${ONTOLOGY_SOURCES}
)
set_target_properties(kos_core_bridge_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)# ========== Core Layer Examples ==========# Core 层 Σ 类型示例
add_executable(core_sigma_example
    examples/core_sigma_example.c
    ${CORE_SOURCES}
)

set_target_properties(core_sigma_example PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# Core 层 Π 类型示例
add_executable(core_pi_example
    examples/core_pi_example.c
    ${CORE_SOURCES}
)

set_target_properties(core_pi_example PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)# Core 层值依赖谓词示例（gt/ge/lt/le/eq 纳入类型系统）
add_executable(value_predicate_demo
    examples/value_predicate_demo.c
    ${CORE_SOURCES}
)

# 应用层：KOS-core 类型推理与规约规则示例
add_executable(app_layer_core_rules_demo
    examples/app_layer_core_rules_demo.c
    ${CORE_SOURCES}
    ${KERNEL_SOURCES}
    ${RUNTIME_SOURCES}
    ${ONTOLOGY_SOURCES}
)
set_target_properties(app_layer_core_rules_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# Core 层类型推理与规约规则应用场景演示
add_executable(core_rules_demo
    examples/core_rules_demo.c
    ${CORE_SOURCES}
)
set_target_properties(core_rules_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)set_target_properties(value_predicate_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# ========== Kernel Layer Examples ==========

# Kernel 层状态演化示例
add_executable(kernel_example
    examples/kernel_example.c
    ${CORE_SOURCES}
    ${KERNEL_SOURCES}
)

set_target_properties(kernel_example PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# 质量追溯应用示例（完整三层架构 + 因果追溯）
add_executable(quality_traceability_demo
    examples/quality_traceability_demo.c
    ${CORE_SOURCES}
    ${KERNEL_SOURCES}
    ${RUNTIME_SOURCES}
    ${ONTOLOGY_SOURCES}
    ${MANUFACTURING_SOURCES}
)
set_target_properties(quality_traceability_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)# 信号处理与自动溯源示例
add_executable(signal_process_demo
    examples/signal_process_demo.c
    ${CORE_SOURCES}
    ${KERNEL_SOURCES}
    ${RUNTIME_SOURCES}
    ${ONTOLOGY_SOURCES}
    ${MANUFACTURING_SOURCES}
)
set_target_properties(signal_process_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)# 不合格产品信号运行过程演示（以“检测到一个不合格的产品，其批次Batch_26_01-00001，时间为2026-01-30”为例）

# KOS-TL 端到端示例：质量追溯 + 传感器信号 + 根因溯源
add_executable(kos_tl_end_to_end_demo
    examples/kos_tl_end_to_end_demo.c
    ${CORE_SOURCES}
    ${KERNEL_SOURCES}
    ${RUNTIME_SOURCES}
    ${ONTOLOGY_SOURCES}
    ${UTILS_SOURCES}
)
set_target_properties(kos_tl_end_to_end_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)
add_executable(manufacturing_kb_root_cause_demo
    examples/manufacturing_kb_root_cause_demo.c
    ${CORE_SOURCES}
    ${KERNEL_SOURCES}
    ${RUNTIME_SOURCES}
    ${ONTOLOGY_SOURCES}
    ${MANUFACTURING_SOURCES}
)
set_target_properties(manufacturing_kb_root_cause_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# MES/QMS 对接适配器示例（Phase 1.3）
add_executable(mes_integration_demo
    examples/mes_integration_demo.c
    ${CORE_SOURCES}
    ${KERNEL_SOURCES}
    ${RUNTIME_SOURCES}
    ${ONTOLOGY_SOURCES}
    ${MANUFACTURING_SOURCES}
)
set_target_properties(mes_integration_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# 六大核心场景完整示例 + 知识演化轨迹
add_executable(all_scenarios_demo
    examples/all_scenarios_demo.c
    ${CORE_SOURCES}
    ${KERNEL_SOURCES}
)
set_target_properties(all_scenarios_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# 反事实推理实例演示
add_executable(counterfactual_demo
    examples/counterfactual_demo.c
    ${CORE_SOURCES}
    ${KERNEL_SOURCES}
    ${RUNTIME_SOURCES}
    ${ONTOLOGY_SOURCES}
    ${MANUFACTURING_SOURCES}
)
set_target_properties(counterfactual_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

add_executable(unqualified_product_demo
    examples/unqualified_product_demo.c
    ${CORE_SOURCES}
    ${KERNEL_SOURCES}
    ${RUNTIME_SOURCES}
    ${ONTOLOGY_SOURCES}
    ${MANUFACTURING_SOURCES}
)
set_target_properties(unqualified_product_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# ========== Query Engine Examples ==========

# 查询引擎示例（Phase 1）
add_executable(query_example
    examples/query_example.c
    ${CORE_SOURCES}
    ${KERNEL_SOURCES}
    ${RUNTIME_SOURCES}
    ${QUERY_SOURCES}
)

set_target_properties(query_example PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# 动态本体管理示例（Phase 2）
add_executable(ontology_version_demo
    examples/ontology_version_demo.c
    ${CORE_SOURCES}
    ${ONTOLOGY_SOURCES}
    ${ONTOLOGY_VERSION_SOURCES}
)

set_target_properties(ontology_version_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# 数据集成与融合示例（Phase 3）
add_executable(data_integration_demo
    examples/data_integration_demo.c
    ${CORE_SOURCES}
    ${INTEGRATION_SOURCES}
)

set_target_properties(data_integration_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# 实时流处理示例（Phase 4）
add_executable(stream_demo
    examples/stream_demo.c
    ${CORE_SOURCES}
    ${KERNEL_SOURCES}
    ${STREAM_SOURCES}
)

set_target_properties(stream_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# API 层与集成示例（Phase 5）
add_executable(api_demo
    examples/api_demo.c
    ${CORE_SOURCES}
    ${KERNEL_SOURCES}
    ${RUNTIME_SOURCES}
    ${ONTOLOGY_SOURCES}
    ${VISUALIZATION_SOURCES}
    ${API_SOURCES}
)

set_target_properties(api_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# Windows 需要链接 ws2_32 库
if(WIN32)
    target_link_libraries(api_demo ws2_32)
endif()

# 权限与安全示例（Phase 6）
add_executable(security_demo
    examples/security_demo.c
    ${CORE_SOURCES}
    ${SECURITY_SOURCES}
)

set_target_properties(security_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# 性能优化示例（Phase 7）
add_executable(performance_demo
    examples/performance_demo.c
    ${CORE_SOURCES}
    ${QUERY_SOURCES}
    ${PERFORMANCE_SOURCES}
)

set_target_properties(performance_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# 可视化增强示例（Phase 8）
add_executable(visualization_demo
    examples/visualization_demo.c
    ${CORE_SOURCES}
    ${QUERY_SOURCES}
    ${VISUALIZATION_ENHANCED_SOURCES}
)
set_target_properties(visualization_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)# 分布式缓存示例（Phase 7 Enhancement）
add_executable(distributed_cache_demo
    examples/distributed_cache_demo.c
    ${CORE_SOURCES}
    ${QUERY_SOURCES}
    ${PERFORMANCE_SOURCES}
)
set_target_properties(distributed_cache_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)