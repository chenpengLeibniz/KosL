// examples/kos_core_bridge_demo.c
// 演示 Kernel/Runtime 如何调用 kos-core 形式化内核
// 非法类型不能创建：仅经 kos-core 校验的项可转为 kos_term

#include "../include/kos_core_bridge.h"
#include "../include/kos_core.h"
#include "../include/kos_ontology.h"
#include "../include/kos_runtime.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void) {
    printf("=== kos-core Bridge Demo ===\n\n");

    /* 1. 检测 kos-core 是否可用 */
    if (!kos_core_bridge_available()) {
        printf("kos-core not available. Build kos-core first:\n");
        printf("  cd kos-core && cabal build\n");
        printf("  Then ensure kos-core is in PATH.\n");
        return 1;
    }
    printf("kos-core: available\n");

    /* 2. 校验单个 Term */
    char err[512];
    if (kos_core_bridge_check_term("Prop P", err, sizeof(err))) {
        printf("check term \"Prop P\": OK\n");
    } else {
        printf("check term \"Prop P\": FAIL - %s\n", err);
    }

    /* 3. 从 .kos 获取形式化校验的 kos_term（非法类型不能创建） */
    kos_term* term = (kos_term*)kos_core_bridge_term_from_kos("Prop P", err, sizeof(err));
    if (term) {
        printf("term_from_kos \"Prop P\": OK (kind=%d)\n", term->kind);
        kos_term_free(term);
    } else {
        printf("term_from_kos \"Prop P\": FAIL - %s\n", err);
    }

    /* 4. 本体从 .kos 添加类型 */
    TypeOntology* ont = kos_ontology_create("demo");
    if (ont) {
        if (kos_ontology_add_type_from_kos(ont, "MyProp", "Prop P", NULL, err, sizeof(err)) == 0) {
            printf("ontology_add_type_from_kos MyProp: OK\n");
        } else {
            printf("ontology_add_type_from_kos: %s\n", err);
        }
        kos_ontology_free(ont);
    }

    /* 5. 信号自动转化为事件（kos_elab_from_kos） */
    const char* sig_str = "Prop Evt1";
    bitstream sig = { (unsigned char*)sig_str, strlen(sig_str) };
    kos_term* event_pair = kos_elab_from_kos(sig);
    if (event_pair) {
        printf("kos_elab_from_kos: OK (signal -> event pair)\n");
        kos_term_free(event_pair);
    } else {
        printf("kos_elab_from_kos: FAIL\n");
    }

    /* 6. 校验 .kos 文件（若存在） */
    const char* kos_file = "kos-core/examples/simple.kos";
    if (kos_core_bridge_check_file(kos_file, err, sizeof(err))) {
        printf("check file %s: OK\n", kos_file);
    } else {
        printf("check file %s: FAIL - %s\n", kos_file, err);
    }

    printf("\n=== Done ===\n");
    return 0;
}
