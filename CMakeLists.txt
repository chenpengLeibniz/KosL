cmake_minimum_required(VERSION 3.10)
project(KosL C)

# 设置 C 标准
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# 编译选项 (根据编译器类型设置)
if(MSVC)
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
    src/core/type_checker.c
    src/core/reduction.c
    src/core/substitution.c
    src/core/type_builder.c
    src/core/storage.c
    src/core/universe.c
)

set(KERNEL_SOURCES
    src/kernel/state_step.c
)

set(RUNTIME_SOURCES
    src/runtime/elab.c
    src/runtime/material.c
    src/runtime/system_init.c
)

set(MANUFACTURING_SOURCES
    src/domain/manufacturing/types.c
    src/domain/manufacturing/predicates.c
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

