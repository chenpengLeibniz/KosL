cmake_minimum_required(VERSION 3.10)
project(KOS_TL_Compiler C)

# 设置 C 标准
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# 编译选项
if(MSVC)
    add_compile_options(/W4 /O2 /utf-8)
else()
    add_compile_options(-Wall -Wextra -O2)
endif()

# 包含目录
include_directories(include)
include_directories(../include)  # 包含主项目的头文件（如果需要）

# KOS 核心源文件（需要链接）
set(KOS_CORE_SOURCES
    ../src/core/type_checker.c
    ../src/core/reduction.c
    ../src/core/substitution.c
    ../src/core/type_builder.c
    ../src/core/storage.c
    ../src/core/universe.c
)

# 编译器源文件
set(COMPILER_SOURCES
    src/compiler/lexer.c
    src/compiler/parser.c
    src/compiler/type_checker.c
    src/compiler/codegen.c
    src/compiler/cic_core.c
)

# 测试源文件
set(TEST_SOURCES
    tests/test_lexer.c
    tests/test_parser.c
    tests/test_type_checker.c
)

# 创建编译器库
add_library(kos_tl_compiler STATIC ${COMPILER_SOURCES})

# 创建测试程序
add_executable(test_lexer tests/test_lexer.c)
target_link_libraries(test_lexer kos_tl_compiler)

add_executable(test_parser tests/test_parser.c)
target_link_libraries(test_parser kos_tl_compiler)

add_executable(test_type_checker tests/test_type_checker.c)
target_link_libraries(test_type_checker kos_tl_compiler)
# 需要链接主项目的库（包含kos_core.h的实现）
target_link_directories(test_type_checker PRIVATE ${CMAKE_SOURCE_DIR}/../build)

# 设置输出目录
set_target_properties(test_lexer test_parser PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# 创建完整的编译器可执行文件
add_executable(kos-tl-compiler src/main.c ${KOS_CORE_SOURCES})
target_link_libraries(kos-tl-compiler kos_tl_compiler)

# 设置输出目录
set_target_properties(kos-tl-compiler PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)



