// compiler/examples/cic_example.c
// CIC内核使用示例
// 演示如何使用Calculus of Inductive Constructions内核

#include "compiler/cic_core.h"
#include "../../include/kos_core.h"
#include <stdio.h>
#include <stdlib.h>

// 示例：定义自然数类型
void example_natural_numbers(void) {
    printf("=== 示例：定义自然数类型 ===\n\n");
    
    // 创建CIC内核
    cic_core* core = cic_core_create();
    if (!core) {
        printf("错误：无法创建CIC内核\n");
        return;
    }
    
    // 创建自然数归纳类型
    kos_term* nat_sort = kos_mk_universe_computational(1);
    cic_inductive_def* nat = cic_inductive_create("Nat", nat_sort);
    
    if (!nat) {
        printf("错误：无法创建归纳类型\n");
        cic_core_free(core);
        return;
    }
    
    // 添加 zero 构造子：zero : Nat
    kos_term* zero_type = nat_sort;
    if (cic_inductive_add_constructor(nat, "zero", zero_type)) {
        printf("✓ 添加构造子: zero : Nat\n");
    }
    
    // 添加 succ 构造子：succ : Nat → Nat
    // TODO: 需要实现 kos_mk_pi 来创建依赖积类型
    // kos_term* succ_type = kos_mk_pi("n", nat_sort, nat_sort);
    // if (cic_inductive_add_constructor(nat, "succ", succ_type)) {
    //     printf("✓ 添加构造子: succ : Nat → Nat\n");
    // }
    
    // 检查归纳类型
    if (cic_check_inductive(core, nat)) {
        printf("✓ 归纳类型检查通过\n");
        
        // 添加到环境
        cic_context_add_inductive(core->ctx, nat);
        printf("✓ 归纳类型已添加到环境\n");
    } else {
        printf("✗ 归纳类型检查失败: %s\n", cic_get_error(core));
    }
    
    // 清理
    cic_inductive_free(nat);
    kos_term_free(nat_sort);
    cic_core_free(core);
    
    printf("\n");
}

// 示例：环境操作
void example_context_operations(void) {
    printf("=== 示例：环境操作 ===\n\n");
    
    cic_core* core = cic_core_create();
    if (!core) {
        return;
    }
    
    // 添加变量到环境
    kos_term* nat_type = kos_mk_universe_computational(1);
    cic_context_add_var(core->ctx, "n", nat_type);
    printf("✓ 添加变量: n : Nat\n");
    
    // 查找变量
    cic_entry* entry = cic_context_lookup(core->ctx, "n");
    if (entry) {
        printf("✓ 找到变量: %s\n", entry->name);
    }
    
    // 清理
    kos_term_free(nat_type);
    cic_core_free(core);
    
    printf("\n");
}

int main(void) {
    printf("CIC内核使用示例\n");
    printf("===============\n\n");
    
    example_natural_numbers();
    example_context_operations();
    
    printf("示例完成\n");
    return 0;
}
