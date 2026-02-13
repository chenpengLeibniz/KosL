// include/kos_core_bridge.h
// Bridge: C Kernel/Runtime -> Haskell kos-core (formal kernel)
//
// kos-core (Haskell) 是形式化类型论内核，提供 Parser 即门卫、归纳构造、类型检查。
// 本 Bridge 允许 C 层调用 kos-core 进行 .kos 源文件的校验。
//
// 集成方式：子进程调用（kos-core 作为独立可执行文件）
// 备选：Haskell FFI（需链接 libkos-core.so + GHC RTS）

#ifndef KOS_CORE_BRIDGE_H
#define KOS_CORE_BRIDGE_H

#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

// ========== kos-core 可执行路径配置 ==========
// 默认：从 PATH 查找 "kos-core"，或相对于项目根目录的 kos-core/kos-core
// 可通过 kos_core_bridge_set_path() 覆盖

// 设置 kos-core 可执行文件路径（可选，用于自定义安装位置）
void kos_core_bridge_set_path(const char* path);

// 获取当前配置的 kos-core 路径（用于调试）
const char* kos_core_bridge_get_path(void);

// ========== .kos 模块校验 ==========
// 调用 kos-core check <file>，形式化内核验证 .kos 模块

// 校验 .kos 文件
// 返回 true 当且仅当 kos-core 类型检查通过
// 失败时 errmsg 可接收错误信息（若 errmsg 非 NULL 且 errmsg_size > 0）
bool kos_core_bridge_check_file(const char* filepath, char* errmsg, size_t errmsg_size);

// 校验 .kos 源字符串（写入临时文件后调用 kos-core）
// 适用于从内存/网络获取的 .kos 源码
bool kos_core_bridge_check_source(const char* kos_source, char* errmsg, size_t errmsg_size);

// ========== 单 Term 校验 ==========
// 调用 kos-core term "<expr>"，校验单个项

// 校验单个 .kos 项表达式
// 返回 true 当且仅当项类型检查通过
bool kos_core_bridge_check_term(const char* term_expr, char* errmsg, size_t errmsg_size);

// ========== 形式化校验后的 Term 获取 ==========
// 调用 kos-core json-term "<expr>"，校验并输出 C kos_term 兼容的 JSON
// 非法类型不能创建：只有 kos-core 形式化校验通过的项才能转为 kos_term

// 从 .kos 表达式获取经形式化校验的 kos_term
// 返回 kos_term*（调用者负责 kos_term_free），失败返回 NULL
// 若 errmsg 非 NULL 且 errmsg_size>0，可接收错误信息
void* kos_core_bridge_term_from_kos(const char* term_expr, char* errmsg, size_t errmsg_size);

// ========== kos-core 类型推理 ==========
// 调用 kos-core infer-term "<expr>"，使用 Haskell 形式化类型推理

// 从 .kos 表达式推理类型，返回 (term, type) 对
// term_out、type_out 由调用者负责 kos_term_free；失败返回 -1
int kos_core_bridge_infer_from_kos(const char* term_expr,
                                   void** term_out, void** type_out,
                                   char* errmsg, size_t errmsg_size);

// 检查 term_expr : type_expr（空上下文），使用 kos-core 类型检查
// 返回 true 当且仅当 term : type
bool kos_core_bridge_check_expr(const char* term_expr, const char* type_expr,
                                char* errmsg, size_t errmsg_size);

// ========== 证明搜索 (Prove) ==========
// 调用 kos-core prove / prove-json，用于根因查找等知识操作

// 在 ctx_file 上下文中对 goal_type 做证明搜索
// 返回 true 当且仅当找到证明；失败时 errmsg 可接收错误信息
bool kos_core_bridge_prove(const char* ctx_file, const char* goal_type,
                           char* errmsg, size_t errmsg_size);

// 同上，但返回证明项为 kos_term*（调用者负责 kos_term_free）
// 失败返回 NULL，成功返回证明项（可反序列化为 RootCauseReport 等）
void* kos_core_bridge_prove_json(const char* ctx_file, const char* goal_type,
                                 char* errmsg, size_t errmsg_size);

// ========== 可用性检测 ==========
// 检查 kos-core 是否可用（可执行文件存在且可运行）

bool kos_core_bridge_available(void);

#ifdef __cplusplus
}
#endif

#endif /* KOS_CORE_BRIDGE_H */
