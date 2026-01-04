// include/kos_core.h
// L0: Core Layer - 静态真理层
// 定义"什么是合法的" - 基于直觉主义受限类型理论（ITT）

#ifndef KOS_CORE_H
#define KOS_CORE_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// 前向声明
typedef enum term_kind {
    KOS_VAL,      // 值类型
    KOS_SIGMA,    // Σ-Types: 依赖对类型
    KOS_PAIR,     // 普通对类型 <d, p>
    KOS_PROP      // 命题类型
} term_kind;

// 基础项结构 - 支持Σ-Types（数据+证明强耦合）
typedef struct kos_term {
    term_kind kind;
    union {
        struct { 
            char* val; 
            struct kos_term* type; 
        } atomic; // 基础数据
        
        struct { 
            struct kos_term* data;   // 数据部分
            struct kos_term* proof;   // 证明部分 - Σ-Types核心
        } pair; // <d, p> - 带证数据对
        
        struct { 
            struct kos_term* domain;  // 域类型
            struct kos_term* body;    // 体类型（依赖域）
        } sigma; // Σ(x:A).B - 依赖类型
    } data;
} kos_term;

// ========== MVP业务类型定义 ==========
// Account: 账户类型（字符串ID）
typedef struct {
    char account_id[64];  // 账户标识
} Account;

// Balance: 余额类型（自然数）
typedef struct {
    uint64_t amount;      // 余额金额
} Balance;

// IsVerified: 实名证明谓词类型
// IsVerified(a: Account) : Prop
typedef struct {
    Account account;      // 账户
    bool verified;        // 是否已实名
} IsVerified;

// TransferEvent: 转账事件类型（Σ-Type）
// 包含前提条件：余额充足 + 账户已实名
typedef struct {
    Account from;         // 转出账户
    Account to;           // 转入账户
    uint64_t amount;      // 转账金额
    kos_term* balance_proof;      // 余额充足证明
    kos_term* verified_proof;     // 实名证明
} TransferEvent;

// ========== L0 核心接口 ==========
// 双向类型检查：验证 proof 是否为命题 prop 的有效证明
bool kos_check(kos_term* ctx, kos_term* term, kos_term* type);
bool kos_type_check(kos_term* ctx, kos_term* proof, kos_term* prop);

// 归约操作
kos_term* kos_reduce(kos_term* t);

// ========== MVP业务逻辑接口 ==========
// 检查账户余额是否充足
bool kos_check_balance_sufficient(Account account, uint64_t required, kos_term* K);

// 检查账户是否已实名
bool kos_check_is_verified(Account account, kos_term* K);

// 构造转账事件的证明
kos_term* kos_construct_transfer_proof(TransferEvent* event, kos_term* K);

#endif





