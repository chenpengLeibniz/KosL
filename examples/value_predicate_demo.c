// examples/value_predicate_demo.c
// 演示：值依赖谓词纳入类型系统（通过 Haskell kos-core 形式化内核）
//
// 调用 kos-core 的 check-term、check-term <term> <type>、json-term 等 API，
// 值依赖谓词 gt(a,b)、ge、lt、le、eq 在 kos-core 中求值：
// - 若谓词成立（如 200 > 180），则 check-term 接受 term : type
// - 若谓词不成立（如 100 > 200），则拒绝
//
// 典型用法：Σ(x:Val). gt(x, "180") 表示“温度大于 180 的依赖类型”

#include "kos_core_bridge.h"
#include "kos_core.h"
#include <stdio.h>

#ifdef _WIN32
#include <direct.h>
#define getcwd _getcwd
#else
#include <unistd.h>
#endif

/** 尝试设置 kos-core 路径（若 kos-core 不在 PATH 中，则使用项目内路径） */
static void try_set_kos_core_path(void) {
    if (kos_core_bridge_available()) return;  /* 已在 PATH 中 */
    char cwd[1024];
    if (!getcwd(cwd, sizeof(cwd))) return;
    static char path[2048];  /* static：bridge 仅保存指针，需在调用后仍有效 */
#ifdef _WIN32
    snprintf(path, sizeof(path), "%s\\kos-core\\dist-newstyle\\build\\x86_64-windows\\ghc-9.8.2\\kos-core-0.1.0.0\\x\\kos-core\\build\\kos-core\\kos-core.exe", cwd);
#else
    snprintf(path, sizeof(path), "%s/kos-core/dist-newstyle/build/x86_64-windows/ghc-9.8.2/kos-core-0.1.0.0/x/kos-core/build/kos-core/kos-core", cwd);
#endif
    kos_core_bridge_set_path(path);
}

static void demo_term_wellformed(void) {
    printf("\n=== kos-core check-term: gt(val \"200\", val \"180\") ===\n");
    char err[512];
    if (kos_core_bridge_check_term("gt(val \"200\", val \"180\")", err, sizeof(err))) {
        printf("gt(200, 180) wellformed: yes\n");
    } else {
        printf("gt(200, 180) wellformed: no - %s\n", err);
    }
}

static void demo_check_gt(void) {
    printf("\n=== kos-core check-term: Prop trivial : gt(val \"200\", val \"180\") ===\n");
    char err[512];
    if (kos_core_bridge_check_expr("Prop trivial", "gt(val \"200\", val \"180\")", err, sizeof(err))) {
        printf("check trivial : gt(200,180): OK\n");
    } else {
        printf("check trivial : gt(200,180): FAIL - %s\n", err);
    }
}

static void demo_check_gt_fail(void) {
    printf("\n=== kos-core check-term: Prop trivial : gt(val \"100\", val \"200\") (should fail) ===\n");
    char err[512];
    if (kos_core_bridge_check_expr("Prop trivial", "gt(val \"100\", val \"200\")", err, sizeof(err))) {
        printf("check trivial : gt(100,200): OK (unexpected)\n");
    } else {
        printf("check trivial : gt(100,200): FAIL (expected) - %s\n", err);
    }
}

static void demo_term_from_kos_serialize(void) {
    printf("\n=== kos-core json-term + C serialize/deserialize round-trip ===\n");
    char err[512];
    kos_term* pred = (kos_term*)kos_core_bridge_term_from_kos("ge(val \"185\", val \"180\")", err, sizeof(err));
    if (!pred) {
        printf("term_from_kos ge(185,180): FAIL - %s\n", err);
        return;
    }
    kos_serialized* ser = kos_term_serialize(pred);
    kos_term_free(pred);
    if (!ser || !ser->data) {
        printf("serialize failed\n");
        return;
    }
    printf("serialized: %s\n", ser->data);
    kos_term* restored = kos_term_deserialize(ser->data);
    kos_serialized_free(ser);
    if (!restored) {
        printf("deserialize failed\n");
        return;
    }
    printf("restored ge(185,180) wellformed: yes\n");
    kos_term_free(restored);
}

int main(void) {
    printf("=== Value-Dependent Predicates via Haskell kos-core ===\n");

    try_set_kos_core_path();
    {
        char err[512];
        if (!kos_core_bridge_check_term("Prop P", err, sizeof(err))) {
            char cwd[1024];
            printf("\nkos-core not available: %s\n", err[0] ? err : "(no details)");
            printf("Build kos-core: cd kos-core && cabal build\n");
            printf("Run from KosL root");
            if (getcwd(cwd, sizeof(cwd))) printf(" (cwd=%s)", cwd);
            printf(".\n");
            printf("Set KOS_CORE_PATH to kos-core.exe path, or add to PATH.\n");
            return 1;
        }
    }
    printf("kos-core: available\n");

    demo_term_wellformed();
    demo_check_gt();
    demo_check_gt_fail();
    demo_term_from_kos_serialize();

    printf("\nDone.\n");
    return 0;
}
