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
    src/domain/manufacturing/ontology_crud.c
)

set(ONTOLOGY_SOURCES
    src/core/ontology_manager.c
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

# 创建制造业演示程序
add_executable(manufacturing_demo
    examples/manufacturing_demo.c
    ${CORE_SOURCES}
    ${KERNEL_SOURCES}
    ${RUNTIME_SOURCES}
    ${ONTOLOGY_SOURCES}
    ${MANUFACTURING_SOURCES}
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

