#include "kos_runtime.h"
#include "kos_kernel.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int main() 
{
    kos_state_t sigma = init_system();
    
    printf("KOS System initialized. Enter signals (Ctrl+C to exit):\n");
    
    while (1) {
        // 1. 感知 (Sense)
        bitstream s = capture_physical_signal();
        
        if (s.length == 0) {
            continue; // 跳过空信号
        }
        
        // 2. 提炼 (Elaborate) [cite: 646]
        kos_term* ev_p = kos_elab(s, sigma.K);
        
        // 释放信号数据
        if (s.data) {
            free(s.data);
        }
        
        if (ev_p) {
            // 3. 演化 (Evolve/Step) [cite: 623]
            if (kos_kernel_step(&sigma, ev_p)) {
                // 4. 具象化 (Materialize) [cite: 649]
                kos_materialize(&sigma);
            } else {
                printf("State evolution failed - invalid event\n");
            }
        } else {
            printf("Signal rejected - could not elaborate\n");
        }
    }
    
    // 清理资源（虽然这里永远不会执行）
    if (sigma.P) {
        free(sigma.P);
    }
    
    return 0;
}
