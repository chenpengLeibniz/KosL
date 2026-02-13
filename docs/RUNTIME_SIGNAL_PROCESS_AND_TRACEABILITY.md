# Runtime 层信号处理与自动溯源

## 功能概述

当发生外部信号时：
- **能构成合法事件**：自动开展溯源，找到根因
- **不能构成合法事件**：返回错误提示给用户

## 核心接口

### 1. kos_runtime_process_signal

```c
int kos_runtime_process_signal(bitstream signal, kos_term* ontology, kos_state_t* sigma,
                               kos_signal_process_result_t* result);
```

- **输入**：signal（原始信号）、ontology（本体，可为 NULL）、sigma（内核状态，可为 NULL）
- **输出**：result（成功/失败、事件对、根因报告、错误信息）
- **返回**：0 成功，-1 失败

### 2. kos_signal_process_result_t

```c
typedef struct {
    bool success;                    // 是否成功构成合法事件
    kos_term* event_pair;           // 成功时的事件对
    void* root_cause_report;        // 成功且为 FailEvt 时的根因报告 (RootCauseReport*)
    char errmsg[512];               // 失败时的错误提示
} kos_signal_process_result_t;
```

### 3. kos_signal_process_result_free

释放 result 内资源（event_pair、root_cause_report）。

### 4. kos_elab_ex（带错误输出）

```c
bool kos_elab_ex(bitstream raw_signal, kos_term* ontology,
                 kos_term** event_out, char* errmsg, size_t errmsg_size);
```

成功时 event_out 非 NULL，失败时 errmsg 含错误提示。

## 溯源处理器注册

制造业领域可注册溯源处理器，使 `kos_runtime_process_signal` 在事件为 FailEvt 时自动溯源：

```c
kos_manufacturing_register_traceability_handler();
```

注册后，当信号精化为 FailEvt 且 sigma->K 非空时，自动调用 `kos_analyze_quality_traceability` 查找根因。

## 使用示例

```c
kos_manufacturing_register_traceability_handler();
kos_state_t* sigma = kos_runtime_init(NULL);

kos_signal_process_result_t result;
bitstream sig = { (unsigned char*)"Prop Evt1", 9 };
kos_runtime_process_signal(sig, NULL, sigma, &result);

if (result.success) {
    printf("Legal event created\n");
    if (result.root_cause_report) {
        RootCauseReport* r = (RootCauseReport*)result.root_cause_report;
        printf("Root cause: batch=%s, error=%s\n", r->failure.batch.batch_id, r->failure.error.code);
    }
    kos_signal_process_result_free(&result);
} else {
    printf("Error: %s\n", result.errmsg);
}
```

## 文件变更

| 文件 | 变更 |
|------|------|
| `include/kos_runtime.h` | 新增 kos_elab_ex、kos_signal_process_result_t、kos_runtime_process_signal、kos_runtime_register_traceability_handler |
| `src/runtime/elab.c` | 实现 kos_elab_ex |
| `src/runtime/signal_process.c` | 新增，实现 kos_runtime_process_signal、kos_signal_process_result_free |
| `include/kos_manufacturing.h` | 新增 kos_try_extract_fail_evt_from_event、kos_manufacturing_register_traceability_handler |
| `src/domain/manufacturing/traceability.c` | 实现 kos_try_extract_fail_evt_from_event、kos_manufacturing_register_traceability_handler |
| `examples/signal_process_demo.c` | 新增演示 |
