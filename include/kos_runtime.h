#ifndef KOS_RUNTIME_H
#define KOS_RUNTIME_H

#include "kos_kernel.h" // 必须包含 L1
#include <stddef.h>

// ========== L2: Runtime Layer - 环境演化层 ==========
// 定义"如何运行" - 外部I/O、物理存储映射

// 原始信号流类型（字节数组）
typedef struct {
    unsigned char* data;
    size_t length;
} bitstream;

// ========== L2 核心接口 ==========
// elab 算子：将物理比特流映射为带逻辑证明的事件对象 <e, p>
// 输入：原始信号 + 本体（知识集）
// 输出：Σ-Type事件对，若无法构造证明则返回NULL（逻辑防火墙）
kos_term* kos_elab(bitstream raw_signal, kos_term* ontology);

// M 算子：将抽象逻辑结论下沉为物理动作
// 实现原子提交栅栏（Atomic Commit Fence）
// 只有逻辑演化成功后，才执行物理存储写入
void kos_materialize(kos_state_t* sigma);

// ========== 系统初始化和I/O ==========
// 初始化系统（创建初始本体）
kos_state_t init_system(void);

// 捕获物理信号（从标准输入或API）
bitstream capture_physical_signal(void);

// ========== 数据库接口（物理存储）==========
int kos_db_init(const char* db_path);
int kos_db_close(void);

#endif