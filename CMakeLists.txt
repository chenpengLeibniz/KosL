cmake_minimum_required(VERSION 3.10)
project(KosL C)

# 设置 C 标准
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# 编译选项 (根据编译器类型设置)
if(MSVC)
    # MSVC 编译选项
    add_compile_options(/W4 /O2)
else()
    # GCC/Clang 编译选项
    add_compile_options(-Wall -Wextra -O2)
endif()

# 包含目录
include_directories(include)

# 收集源文件
set(CORE_SOURCES
    src/core/type_checker.c
    src/core/reduction.c
    src/core/business_logic.c
)

set(KERNEL_SOURCES
    src/kernel/state_step.c
)

set(RUNTIME_SOURCES
    src/runtime/elab.c
    src/runtime/material.c
)

set(MAIN_SOURCE
    main.c
)

# 所有源文件
set(ALL_SOURCES
    ${CORE_SOURCES}
    ${KERNEL_SOURCES}
    ${RUNTIME_SOURCES}
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

