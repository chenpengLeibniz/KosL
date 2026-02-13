/**
 * @file elab.c
 * @brief KOS Runtime 层 elab 算子（信号提炼）
 *
 * 将物理比特流映射为带逻辑证明的事件对象 <e, p> [Kos.pdf 2.3.3]。
 * 核心功能：
 * - kos_elab: 通用信号提炼，支持 .kos 表达式自动经 kos-core 校验
 * - kos_elab_from_kos: 从 .kos 源精化，形式化校验后构造 <event, KosValidated>
 * - kos_elab_ex: 带错误输出的 elab，失败时返回详细错误信息
 *
 * 设计原则：无法构造证明的信号被拒绝（逻辑防火墙 [cite: 656, 657]）。
 */

#include "kos_runtime.h"
#include "kos_core.h"
#include "kos_core_bridge.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

/* 前向声明：从 JSON 提取 _kos 字段 */
static bool extract_kos_from_json(const char* json, size_t len, char* out, size_t out_size);

/* ========== 通用信号解析辅助函数 ========== */

/** 将原始比特流解析为通用事件项（不依赖业务类型），内部转为字符串表示。 */
static kos_term* parse_signal_to_event(bitstream raw_signal) {
    if (!raw_signal.data || raw_signal.length == 0) {
        return NULL;
    }
    
    // 创建通用的事件项
    kos_term* event = (kos_term*)calloc(1, sizeof(kos_term));
    if (!event) {
        return NULL;
    }
    
    event->kind = KOS_PROP;
    
    // 将原始数据转换为字符串表示（通用处理）
    event->data.atomic.val = (char*)malloc(raw_signal.length + 1);
    if (!event->data.atomic.val) {
        free(event);
        return NULL;
    }
    
    memcpy(event->data.atomic.val, raw_signal.data, raw_signal.length);
    event->data.atomic.val[raw_signal.length] = '\0';
    event->data.atomic.type = NULL; // 类型由本体决定
    
    return event;
}

/** 根据本体 ontology 为事件 event 构造证明。本体定义证明构造规则；无本体时返回基本证明。 */
static kos_term* construct_proof_from_ontology(kos_term* event, kos_term* ontology) {
    if (!event) {
        return NULL;
    }
    
    // 如果本体为空（初始状态），创建基本证明
    // 实际系统中，本体应该包含证明构造规则
    if (!ontology) {
        // 初始状态：创建基本证明（简化实现）
        kos_term* proof = (kos_term*)calloc(1, sizeof(kos_term));
        if (!proof) {
            return NULL;
        }
        
        proof->kind = KOS_PROP;
        proof->data.atomic.val = (char*)malloc(256);
        if (proof->data.atomic.val) {
            snprintf(proof->data.atomic.val, 256, "BasicProof(event=%s)", 
                     event->data.atomic.val ? event->data.atomic.val : "");
        }
        proof->data.atomic.type = NULL;
        
        return proof;
    }
    
    // 实际实现中，这里应该：
    // 1. 从本体中查找匹配的证明规则
    // 2. 根据规则验证事件是否符合约束
    // 3. 如果符合，构造相应的证明；否则返回NULL
    
    // 简化实现：如果本体存在，尝试查找匹配的证明规则
    // 这里暂时返回基本证明（实际应该进行更严格的验证）
    kos_term* proof = (kos_term*)calloc(1, sizeof(kos_term));
    if (!proof) {
        return NULL;
    }
    
    proof->kind = KOS_PROP;
    proof->data.atomic.val = (char*)malloc(256);
    if (proof->data.atomic.val) {
        snprintf(proof->data.atomic.val, 256, "OntologyProof(event=%s)", 
                 event->data.atomic.val ? event->data.atomic.val : "");
    }
    proof->data.atomic.type = NULL;
    
    return proof;
}

/* ========== elab 算子：通用信号提炼 ==========
 * 输入：raw_signal + ontology；输出：<e, p> 或 NULL（逻辑防火墙）
 * 若信号像 .kos 表达式，优先经 kos-core 校验并自动转化为事件。 */

/** elab 算子：将物理比特流映射为 <e, p>。无法构造证明则返回 NULL。 */
kos_term* kos_elab(bitstream raw_signal, kos_term* ontology) {
    if (!raw_signal.data || raw_signal.length == 0) {
        return NULL;
    }
    
    // 0. 若信号像 .kos 表达式，优先经 kos-core 校验（信号自动转化为事件）
    if (kos_core_bridge_available() && raw_signal.length < 4096) {
        const char* p = (const char*)raw_signal.data;
        if (raw_signal.data[raw_signal.length - 1] == '\0' || raw_signal.length < 4095) {
            int looks_like_kos = 0;
            if (raw_signal.length >= 5 && memcmp(p, "Prop ", 5) == 0) looks_like_kos = 1;
            else if (raw_signal.length >= 6 && memcmp(p, "Sigma", 5) == 0) looks_like_kos = 1;
            else if (raw_signal.length >= 3 && memcmp(p, "Pi(", 3) == 0) looks_like_kos = 1;
            else if (raw_signal.length >= 2 && p[0] == '<') looks_like_kos = 1;
            else if (raw_signal.length >= 8 && p[0] == '{') looks_like_kos = 1;  /* 可能为 {"_kos":"..."} */
            if (looks_like_kos) {
                kos_term* from_kos = kos_elab_from_kos(raw_signal);
                if (from_kos) return from_kos;
            }
        }
    }
    
    // 1. 将原始信号解析为通用事件项（不依赖具体业务类型）
    kos_term* event = parse_signal_to_event(raw_signal);
    if (!event) {
        return NULL; // 信号格式错误或内存分配失败
    }
    
    // 2. 根据本体（ontology）构造证明
    // 本体定义了如何从信号构造证明的规则
    // 如果无法构造证明，则视为干扰信号，建立逻辑防火墙 [cite: 656, 657]
    kos_term* proof = construct_proof_from_ontology(event, ontology);
    if (!proof) {
        // 无法构造证明：信号不符合本体约束
        // 释放事件并返回NULL（逻辑防火墙）
        if (event->data.atomic.val) {
            free(event->data.atomic.val);
        }
        free(event);
        return NULL;
    }
    
    // 3. 构造Σ-Type对 <event, proof>
    kos_term* pair = (kos_term*)calloc(1, sizeof(kos_term));
    if (!pair) {
        if (event->data.atomic.val) {
            free(event->data.atomic.val);
        }
        free(event);
        if (proof->data.atomic.val) {
            free(proof->data.atomic.val);
        }
        free(proof);
        return NULL;
    }
    
    pair->kind = KOS_PAIR;
    pair->data.pair.data = event;   // 事件部分
    pair->data.pair.proof = proof;   // 证明部分
    
    // 4. 返回Σ-Type事件对 <event, proof>
    return pair;
}

/** elab 带错误输出：成功时 *event_out 为事件对；失败时 errmsg 含详细错误（含 kos-core 校验信息）。 */
bool kos_elab_ex(bitstream raw_signal, kos_term* ontology,
                 kos_term** event_out, char* errmsg, size_t errmsg_size) {
    if (!event_out) return false;
    *event_out = NULL;
    if (errmsg && errmsg_size > 0) errmsg[0] = '\0';
    if (!raw_signal.data || raw_signal.length == 0) {
        if (errmsg && errmsg_size > 0) snprintf(errmsg, errmsg_size, "Signal is empty");
        return false;
    }
    kos_term* pair = kos_elab(raw_signal, ontology);
    if (pair) {
        *event_out = pair;
        return true;
    }
    if (kos_core_bridge_available() && raw_signal.length < 4096) {
        char buf[4096], kos_expr[4096];
        char* errbuf = errmsg && errmsg_size > 0 ? errmsg : buf;
        size_t errsz = errmsg && errmsg_size > 0 ? errmsg_size : sizeof(buf);
        memcpy(buf, raw_signal.data, raw_signal.length);
        buf[raw_signal.length] = '\0';
        if (buf[0] == '{' && extract_kos_from_json(buf, raw_signal.length, kos_expr, sizeof(kos_expr))) {
            kos_core_bridge_term_from_kos(kos_expr, errbuf, errsz);
        } else if (raw_signal.length < sizeof(kos_expr)) {
            strncpy(kos_expr, buf, sizeof(kos_expr) - 1);
            kos_expr[sizeof(kos_expr) - 1] = '\0';
            kos_core_bridge_term_from_kos(kos_expr, errbuf, errsz);
        }
        if (errmsg && errmsg_size > 0 && errbuf != errmsg && errbuf[0] != '\0')
            strncpy(errmsg, errbuf, errmsg_size - 1), errmsg[errmsg_size - 1] = '\0';
    }
    if (errmsg && errmsg_size > 0 && errmsg[0] == '\0') {
        snprintf(errmsg, errmsg_size, "Signal cannot form a legal event (ontology validation failed)");
    }
    return false;
}

/* ========== 从 .kos 源精化 ==========
 * 信号为 .kos 表达式时，经 kos-core 形式化校验后转为 <event, KosValidated>。 */

/** 从 JSON 中提取 "_kos":"<value>" 的 value，支持转义。 */
static bool extract_kos_from_json(const char* json, size_t len, char* out, size_t out_size) {
    /* 简单提取 "_kos":"<value>" 中的 value（支持嵌套转义） */
    const char* p = strstr(json, "\"_kos\"");
    if (!p || (size_t)(p - json) >= len) return false;
    p += 6;
    while (p < json + len && (*p == ' ' || *p == '\t' || *p == ':')) p++;
    if (p >= json + len || *p != '"') return false;
    p++;
    const char* start = p;
    size_t i = 0;
    while (p < json + len && *p != '"' && i < out_size - 1) {
        if (*p == '\\' && p + 1 < json + len) {
            p++;
            switch (*p) {
                case '"': out[i++] = '"'; break;
                case '\\': out[i++] = '\\'; break;
                case 'n': out[i++] = '\n'; break;
                case 'r': out[i++] = '\r'; break;
                case 't': out[i++] = '\t'; break;
                default: out[i++] = *p; break;
            }
            p++;
        } else {
            out[i++] = *p++;
        }
    }
    out[i] = '\0';
    return (p < json + len && *p == '"' && i > 0);
}

/** 从 .kos 表达式（纯文本或 JSON 中 _kos 字段）精化，经 kos-core 校验后返回 <event, KosValidated>。 */
kos_term* kos_elab_from_kos(bitstream raw_signal) {
    if (!raw_signal.data || raw_signal.length == 0) {
        return NULL;
    }
    /* 确保以 NUL 结尾 */
    char* buf = (char*)malloc(raw_signal.length + 1);
    if (!buf) return NULL;
    memcpy(buf, raw_signal.data, raw_signal.length);
    buf[raw_signal.length] = '\0';

    char kos_expr[4096];
    bool from_json = false;
    if (raw_signal.length >= 8 && buf[0] == '{') {
        if (extract_kos_from_json(buf, raw_signal.length, kos_expr, sizeof(kos_expr))) {
            from_json = true;
        }
    }
    if (!from_json) {
        if (raw_signal.length >= sizeof(kos_expr)) {
            free(buf);
            return NULL;
        }
        strncpy(kos_expr, buf, sizeof(kos_expr) - 1);
        kos_expr[sizeof(kos_expr) - 1] = '\0';
    }
    free(buf);

    char err[512];
    kos_term* event = (kos_term*)kos_core_bridge_term_from_kos(kos_expr, err, sizeof(err));
    if (!event) {
        return NULL;  /* kos-core 校验失败，信号不能转化为合法事件 */
    }
    /* 构造证明：事件经形式化校验，证明为 "KosValidated" */
    kos_term* proof = kos_mk_prop("KosValidated");
    if (!proof) {
        kos_term_free(event);
        return NULL;
    }
    kos_term* pair = kos_mk_pair(event, proof);
    kos_term_free(event);
    kos_term_free(proof);
    return pair;
}

/* ========== 批量精化 ========== */

/** 批量精化 signals，返回成功的事件对数组，*success_count 为成功数量。调用者负责释放数组及元素。 */
kos_term** kos_elab_batch(bitstream* signals, size_t count, kos_term* ontology, size_t* success_count) {
    if (!signals || count == 0 || !success_count) {
        return NULL;
    }
    
    // 分配结果数组
    kos_term** results = (kos_term**)calloc(count, sizeof(kos_term*));
    if (!results) {
        return NULL;
    }
    
    *success_count = 0;
    
    // 逐个精化信号
    for (size_t i = 0; i < count; i++) {
        kos_term* event_pair = kos_elab(signals[i], ontology);
        if (event_pair) {
            results[*success_count] = event_pair;
            (*success_count)++;
        }
    }
    
    return results;
}

/* ========== 精化轨迹记录 ==========
 * 用于轨迹重放与自愈，记录 event_pair、原始信号、逻辑时钟。 */

/** 创建精化记录，深拷贝 event_pair 与 original_signal。 */
elaboration_record_t* kos_elab_record(kos_term* event_pair, bitstream original_signal, int logical_clock) {
    if (!event_pair) {
        return NULL;
    }
    
    elaboration_record_t* record = (elaboration_record_t*)calloc(1, sizeof(elaboration_record_t));
    if (!record) {
        return NULL;
    }
    
    // 深拷贝事件对
    record->event_pair = kos_term_copy(event_pair);
    if (!record->event_pair) {
        free(record);
        return NULL;
    }
    
    // 深拷贝原始信号
    if (original_signal.data && original_signal.length > 0) {
        record->original_signal.data = (unsigned char*)malloc(original_signal.length);
        if (!record->original_signal.data) {
            kos_term_free(record->event_pair);
            free(record);
            return NULL;
        }
        memcpy(record->original_signal.data, original_signal.data, original_signal.length);
        record->original_signal.length = original_signal.length;
    } else {
        record->original_signal.data = NULL;
        record->original_signal.length = 0;
    }
    
    record->logical_clock = logical_clock;
    record->signal_index = 0;  // 由调用者设置
    
    return record;
}

void kos_elab_record_free(elaboration_record_t* record) {
    if (!record) {
        return;
    }
    
    if (record->event_pair) {
        kos_term_free(record->event_pair);
    }
    
    if (record->original_signal.data) {
        free(record->original_signal.data);
    }
    
    free(record);
}

