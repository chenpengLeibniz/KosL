// examples/signal_process_demo.c
// 演示 Runtime 层信号处理：能构成合法事件则自动溯源，否则返回错误提示

#include "../include/kos_runtime.h"
#include "../include/kos_kernel.h"
#include "../include/kos_manufacturing.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void) {
    printf("=== KOS Runtime Signal Process Demo ===\n\n");

    kos_manufacturing_register_traceability_handler();

    kos_state_t* sigma = kos_runtime_init(NULL);
    if (!sigma) {
        printf("Failed to init runtime\n");
        return 1;
    }

    kos_signal_process_result_t result;

    printf("1. Invalid signal (cannot form legal event):\n");
    const char* bad_sig = "invalid_garbage_xyz";
    bitstream bad = { (unsigned char*)bad_sig, strlen(bad_sig) };
    kos_runtime_process_signal(bad, NULL, sigma, &result);
    if (!result.success) {
        printf("   Result: FAIL (expected)\n");
        printf("   Error: %s\n", result.errmsg);
    } else {
        printf("   Unexpected success\n");
    }

    printf("\n2. Valid .kos signal (Prop Evt1):\n");
    const char* good_sig = "Prop Evt1";
    bitstream good = { (unsigned char*)good_sig, strlen(good_sig) };
    kos_runtime_process_signal(good, NULL, sigma, &result);
    if (result.success) {
        printf("   Result: OK (legal event)\n");
        printf("   Event pair: created\n");
        if (result.root_cause_report) {
            RootCauseReport* r = (RootCauseReport*)result.root_cause_report;
            printf("   Root cause: batch=%s, error=%s\n", r->failure.batch.batch_id, r->failure.error.code);
        } else {
            printf("   Root cause: (none - sigma/K empty or not FailEvt)\n");
        }
        kos_signal_process_result_free(&result);
    } else {
        printf("   Result: FAIL - %s\n", result.errmsg);
    }

    printf("\n3. Invalid .kos expression:\n");
    const char* bad_kos = "Sigma(x: invalid). Prop P";
    bitstream bad2 = { (unsigned char*)bad_kos, strlen(bad_kos) };
    kos_runtime_process_signal(bad2, NULL, sigma, &result);
    if (!result.success) {
        printf("   Result: FAIL (expected)\n");
        printf("   Error: %s\n", result.errmsg);
    } else {
        printf("   Unexpected success\n");
        kos_signal_process_result_free(&result);
    }

    kos_runtime_free(sigma);
    printf("\n=== Done ===\n");
    return 0;
}
