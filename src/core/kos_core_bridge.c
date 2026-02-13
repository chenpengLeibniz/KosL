/**
 * @file kos_core_bridge.c
 * @brief C 与 Haskell kos-core 的桥接层（子进程调用）
 *
 * 通过调用 kos-core 可执行文件进行 .kos 表达式校验与转换：
 * - check: 校验 .kos 文件或表达式
 * - json-term: 将 .kos 表达式转为 C 可解析的 JSON，再反序列化为 kos_term
 *
 * 要求：kos-core 已构建，且 kos-core/kos-core.exe 在 PATH 或通过 kos_core_bridge_set_path 指定。
 */

#include "kos_core_bridge.h"
#include "kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#if defined(_WIN32) || defined(_WIN64)
#include <windows.h>
#endif

#if defined(_WIN32) || defined(_WIN64)
#include <process.h>
#include <io.h>
#include <windows.h>
#define POPEN _popen
#define PCLOSE _pclose
#define KOS_CORE_EXE "kos-core.exe"
#else
#include <unistd.h>
#include <sys/wait.h>
#define POPEN popen
#define PCLOSE pclose
#define KOS_CORE_EXE "kos-core"
#endif

/* 默认路径：NULL 表示从 PATH 查找 kos-core */
static const char* s_kos_core_path = NULL;

/** 设置 kos-core 可执行文件路径；NULL 表示使用 PATH。 */
void kos_core_bridge_set_path(const char* path) {
    s_kos_core_path = path;
}

const char* kos_core_bridge_get_path(void) {
    return s_kos_core_path ? s_kos_core_path : "(from PATH)";
}

/** 转义字符串中的双引号，供 Windows cmd 命令行使用（" -> \"） */
static void escape_for_shell(const char* in, char* out, size_t out_size) {
    size_t pos = 0;
    for (; *in && pos < out_size - 1; in++) {
        if (*in == '"') {
            if (pos < out_size - 2) {
                out[pos++] = '\\';
                out[pos++] = '"';
            }
        } else {
            out[pos++] = *in;
        }
    }
    out[pos] = '\0';
}

/** 获取 kos-core 可执行路径：s_kos_core_path > KOS_CORE_PATH 环境变量 > PATH */
static const char* get_kos_core_exe(void) {
    if (s_kos_core_path && s_kos_core_path[0]) return s_kos_core_path;
#if defined(_WIN32) || defined(_WIN64)
    {
        static char env_path[MAX_PATH];
        DWORD n = GetEnvironmentVariableA("KOS_CORE_PATH", env_path, MAX_PATH);
        if (n > 0 && n < MAX_PATH) return env_path;
    }
#else
    {
        const char* ep = getenv("KOS_CORE_PATH");
        if (ep && ep[0]) return ep;
    }
#endif
    return KOS_CORE_EXE;
}

/** 路径是否含空格（决定是否需引号，避免 Windows cmd 解析错误） */
static int path_has_space(const char* p) {
    for (; *p; p++) if (*p == ' ') return 1;
    return 0;
}

/** 运行 kos-core check <filepath>，成功返回 0，失败返回 -1 并填充 errmsg。 */
static int run_check_file(const char* filepath, char* errmsg, size_t errmsg_size) {
    char cmd[4096];
    const char* exe = get_kos_core_exe();
    if (path_has_space(exe))
        snprintf(cmd, sizeof(cmd), "\"%s\" check \"%s\" 2>&1", exe, filepath);
    else
        snprintf(cmd, sizeof(cmd), "%s check \"%s\" 2>&1", exe, filepath);

    FILE* fp = POPEN(cmd, "r");
    if (!fp) {
        if (errmsg && errmsg_size > 0) {
            snprintf(errmsg, errmsg_size, "Failed to run kos-core");
        }
        return -1;
    }

    if (errmsg && errmsg_size > 0) {
        errmsg[0] = '\0';
        size_t total = 0;
        while (total < errmsg_size - 1 && fgets(errmsg + total, (int)(errmsg_size - total), fp)) {
            size_t len = strlen(errmsg + total);
            total += len;
        }
        if (total > 0 && errmsg[total - 1] == '\n') {
            errmsg[total - 1] = '\0';
        }
    } else {
        while (fgetc(fp) != EOF) { (void)0; }
    }

    int status = PCLOSE(fp);
#if defined(_WIN32) || defined(_WIN64)
    return (status == 0) ? 0 : -1;
#else
    return (WIFEXITED(status) && WEXITSTATUS(status) == 0) ? 0 : -1;
#endif
}

/** 运行 kos-core term "<expr>" 校验表达式，成功返回 0。 */
static int run_check_term(const char* term_expr, char* errmsg, size_t errmsg_size) {
    char cmd[16384];
    char escaped[8192];
    const char* exe = get_kos_core_exe();
    escape_for_shell(term_expr, escaped, sizeof(escaped));
    if (path_has_space(exe))
        snprintf(cmd, sizeof(cmd), "\"%s\" term \"%s\" 2>&1", exe, escaped);
    else
        snprintf(cmd, sizeof(cmd), "%s term \"%s\" 2>&1", exe, escaped);

    FILE* fp = POPEN(cmd, "r");
    if (!fp) {
        if (errmsg && errmsg_size > 0) {
            snprintf(errmsg, errmsg_size, "Failed to run kos-core");
        }
        return -1;
    }

    if (errmsg && errmsg_size > 0) {
        errmsg[0] = '\0';
        size_t total = 0;
        while (total < errmsg_size - 1 && fgets(errmsg + total, (int)(errmsg_size - total), fp)) {
            size_t len = strlen(errmsg + total);
            total += len;
        }
        if (total > 0 && errmsg[total - 1] == '\n') {
            errmsg[total - 1] = '\0';
        }
    } else {
        while (fgetc(fp) != EOF) { (void)0; }
    }

    int status = PCLOSE(fp);
#if defined(_WIN32) || defined(_WIN64)
    return (status == 0) ? 0 : -1;
#else
    return (WIFEXITED(status) && WEXITSTATUS(status) == 0) ? 0 : -1;
#endif
}

bool kos_core_bridge_check_file(const char* filepath, char* errmsg, size_t errmsg_size) {
    if (!filepath) return false;
    return run_check_file(filepath, errmsg, errmsg_size) == 0;
}

bool kos_core_bridge_check_term(const char* term_expr, char* errmsg, size_t errmsg_size) {
    if (!term_expr) return false;
    return run_check_term(term_expr, errmsg, errmsg_size) == 0;
}

/** 校验 .kos 源码：写入临时文件后调用 kos_core_bridge_check_file。 */
bool kos_core_bridge_check_source(const char* kos_source, char* errmsg, size_t errmsg_size) {
    if (!kos_source) return false;

#if defined(_WIN32) || defined(_WIN64)
    char tmp_path[MAX_PATH];
    char tmp_dir[MAX_PATH];
    if (GetTempPathA(MAX_PATH, tmp_dir) == 0) {
        if (errmsg && errmsg_size > 0) snprintf(errmsg, errmsg_size, "Failed to get temp dir");
        return false;
    }
    if (GetTempFileNameA(tmp_dir, "kos", 0, tmp_path) == 0) {
        if (errmsg && errmsg_size > 0) snprintf(errmsg, errmsg_size, "Failed to create temp file");
        return false;
    }
#else
    char tmp_path[] = "/tmp/kos_bridge_XXXXXX";
    int fd = mkstemp(tmp_path);
    if (fd < 0) {
        if (errmsg && errmsg_size > 0) snprintf(errmsg, errmsg_size, "Failed to create temp file");
        return false;
    }
    close(fd);
#endif

    FILE* f = fopen(tmp_path, "w");
    if (!f) {
        if (errmsg && errmsg_size > 0) snprintf(errmsg, errmsg_size, "Failed to write temp file");
        return false;
    }
    fputs(kos_source, f);
    fclose(f);

    bool ok = kos_core_bridge_check_file(tmp_path, errmsg, errmsg_size);
    remove(tmp_path);
    return ok;
}

/** 运行 kos-core json-term "<expr>"，捕获 stdout 为 JSON。成功返回 0，失败返回 -1 并填充 errmsg。 */
static int run_json_term(const char* term_expr, char* json_out, size_t json_size,
                         char* errmsg, size_t errmsg_size) {
    char cmd[16384];
    char escaped[8192];
    const char* exe = get_kos_core_exe();
    escape_for_shell(term_expr, escaped, sizeof(escaped));
    if (path_has_space(exe))
        snprintf(cmd, sizeof(cmd), "\"%s\" json-term \"%s\" 2>&1", exe, escaped);
    else
        snprintf(cmd, sizeof(cmd), "%s json-term \"%s\" 2>&1", exe, escaped);

    FILE* fp = POPEN(cmd, "r");
    if (!fp) {
        if (errmsg && errmsg_size > 0) {
            snprintf(errmsg, errmsg_size, "Failed to run kos-core");
        }
        return -1;
    }

    if (json_out && json_size > 0) {
        json_out[0] = '\0';
        size_t total = 0;
        while (total < json_size - 1 && fgets(json_out + total, (int)(json_size - total), fp)) {
            size_t len = strlen(json_out + total);
            total += len;
            if (total > 0 && json_out[total - 1] == '\n') {
                json_out[total - 1] = '\0';
                total--;
                break;
            }
        }
    } else {
        while (fgetc(fp) != EOF) { (void)0; }
    }

    int status = PCLOSE(fp);
#if defined(_WIN32) || defined(_WIN64)
    if (status != 0) {
        if (errmsg && errmsg_size > 0 && json_out && json_out[0] != '\0') {
            strncpy(errmsg, json_out, errmsg_size - 1);
            errmsg[errmsg_size - 1] = '\0';
        }
        return -1;
    }
#else
    if (!(WIFEXITED(status) && WEXITSTATUS(status) == 0)) {
        if (errmsg && errmsg_size > 0 && json_out && json_out[0] != '\0') {
            strncpy(errmsg, json_out, errmsg_size - 1);
            errmsg[errmsg_size - 1] = '\0';
        }
        return -1;
    }
#endif
    return 0;
}

/** 从 .kos 表达式构造 kos_term*：经 kos-core 校验后反序列化 JSON。失败返回 NULL 并填充 errmsg。 */
void* kos_core_bridge_term_from_kos(const char* term_expr, char* errmsg, size_t errmsg_size) {
    if (!term_expr) {
        if (errmsg && errmsg_size > 0) snprintf(errmsg, errmsg_size, "Null term_expr");
        return NULL;
    }

    char json_buf[65536];
    if (run_json_term(term_expr, json_buf, sizeof(json_buf), errmsg, errmsg_size) != 0) {
        if (errmsg && errmsg_size > 0 && errmsg[0] == '\0' && json_buf[0] != '\0') {
            strncpy(errmsg, json_buf, errmsg_size - 1);
            errmsg[errmsg_size - 1] = '\0';
        }
        return NULL;
    }

    kos_term* term = kos_term_deserialize(json_buf);
    if (!term) {
        if (errmsg && errmsg_size > 0) {
            snprintf(errmsg, errmsg_size, "Failed to deserialize kos-core JSON");
        }
        return NULL;
    }
    return term;
}

/** 运行 kos-core infer-term "<expr>"，捕获 stdout 为 {term,type} JSON。 */
static int run_infer_term(const char* term_expr, char* json_out, size_t json_size,
                          char* errmsg, size_t errmsg_size) {
    char cmd[16384];
    char escaped[8192];
    const char* exe = get_kos_core_exe();
    escape_for_shell(term_expr, escaped, sizeof(escaped));
    if (path_has_space(exe))
        snprintf(cmd, sizeof(cmd), "\"%s\" infer-term \"%s\" 2>&1", exe, escaped);
    else
        snprintf(cmd, sizeof(cmd), "%s infer-term \"%s\" 2>&1", exe, escaped);

    FILE* fp = POPEN(cmd, "r");
    if (!fp) {
        if (errmsg && errmsg_size > 0) snprintf(errmsg, errmsg_size, "Failed to run kos-core");
        return -1;
    }
    if (json_out && json_size > 0) {
        json_out[0] = '\0';
        size_t total = 0;
        while (total < json_size - 1 && fgets(json_out + total, (int)(json_size - total), fp)) {
            size_t len = strlen(json_out + total);
            total += len;
            if (total > 0 && json_out[total - 1] == '\n') {
                json_out[total - 1] = '\0';
                total--;
                break;
            }
        }
    } else {
        while (fgetc(fp) != EOF) { (void)0; }
    }
    int status = PCLOSE(fp);
#if defined(_WIN32) || defined(_WIN64)
    if (status != 0) {
        if (errmsg && errmsg_size > 0 && json_out && json_out[0] != '\0') {
            strncpy(errmsg, json_out, errmsg_size - 1);
            errmsg[errmsg_size - 1] = '\0';
        }
        return -1;
    }
#else
    if (!(WIFEXITED(status) && WEXITSTATUS(status) == 0)) {
        if (errmsg && errmsg_size > 0 && json_out && json_out[0] != '\0') {
            strncpy(errmsg, json_out, errmsg_size - 1);
            errmsg[errmsg_size - 1] = '\0';
        }
        return -1;
    }
#endif
    return 0;
}

/** 从 JSON 对象中提取 "term" 或 "type" 字段的 JSON 字符串。 */
static const char* find_json_field(const char* json, const char* key) {
    char pattern[64];
    snprintf(pattern, sizeof(pattern), "\"%s\":", key);
    const char* p = strstr(json, pattern);
    if (!p) return NULL;
    p += strlen(pattern);
    while (*p == ' ' || *p == '\t') p++;
    return (*p == '{') ? p : NULL;
}

/** 从 .kos 表达式推理类型，返回 (term, type)。term_out、type_out 由调用者 kos_term_free。 */
int kos_core_bridge_infer_from_kos(const char* term_expr,
                                   void** term_out, void** type_out,
                                   char* errmsg, size_t errmsg_size) {
    if (!term_expr || !term_out || !type_out) {
        if (errmsg && errmsg_size > 0) snprintf(errmsg, errmsg_size, "Null argument");
        return -1;
    }
    *term_out = NULL;
    *type_out = NULL;

    char json_buf[65536];
    if (run_infer_term(term_expr, json_buf, sizeof(json_buf), errmsg, errmsg_size) != 0) {
        return -1;
    }

    const char* term_json = find_json_field(json_buf, "term");
    const char* type_json = find_json_field(json_buf, "type");
    if (!term_json || !type_json) {
        if (errmsg && errmsg_size > 0) snprintf(errmsg, errmsg_size, "Invalid infer-term JSON");
        return -1;
    }

    /* 提取 term 对象的完整 JSON（匹配大括号） */
    int depth = 0;
    const char* start = term_json;
    const char* p = start;
    while (*p) {
        if (*p == '{') { depth++; p++; }
        else if (*p == '}') { depth--; p++; if (depth == 0) break; }
        else p++;
    }
    if (depth != 0) {
        if (errmsg && errmsg_size > 0) snprintf(errmsg, errmsg_size, "Malformed term JSON");
        return -1;
    }
    size_t term_len = (size_t)(p - start);
    char* term_str = (char*)malloc(term_len + 1);
    if (!term_str) {
        if (errmsg && errmsg_size > 0) snprintf(errmsg, errmsg_size, "Out of memory");
        return -1;
    }
    memcpy(term_str, start, term_len);
    term_str[term_len] = '\0';

    start = type_json;
    p = start;
    depth = 0;
    while (*p) {
        if (*p == '{') { depth++; p++; }
        else if (*p == '}') { depth--; p++; if (depth == 0) break; }
        else p++;
    }
    if (depth != 0) {
        free(term_str);
        if (errmsg && errmsg_size > 0) snprintf(errmsg, errmsg_size, "Malformed type JSON");
        return -1;
    }
    size_t type_len = (size_t)(p - start);
    char* type_str = (char*)malloc(type_len + 1);
    if (!type_str) {
        free(term_str);
        if (errmsg && errmsg_size > 0) snprintf(errmsg, errmsg_size, "Out of memory");
        return -1;
    }
    memcpy(type_str, start, type_len);
    type_str[type_len] = '\0';

    kos_term* term = kos_term_deserialize(term_str);
    kos_term* ty = kos_term_deserialize(type_str);
    free(term_str);
    free(type_str);
    if (!term || !ty) {
        if (term) kos_term_free(term);
        if (ty) kos_term_free(ty);
        if (errmsg && errmsg_size > 0) snprintf(errmsg, errmsg_size, "Failed to deserialize infer result");
        return -1;
    }
    *term_out = term;
    *type_out = ty;
    return 0;
}

/** 运行 kos-core check-term "<term>" "<type>"，检查 term : type。 */
static int run_check_term_expr(const char* term_expr, const char* type_expr,
                               char* errmsg, size_t errmsg_size) {
    char cmd[16384];
    char escaped_term[8192];
    char escaped_type[8192];
    const char* exe = get_kos_core_exe();
    escape_for_shell(term_expr, escaped_term, sizeof(escaped_term));
    escape_for_shell(type_expr, escaped_type, sizeof(escaped_type));
    if (path_has_space(exe))
        snprintf(cmd, sizeof(cmd), "\"%s\" check-term \"%s\" \"%s\" 2>&1", exe, escaped_term, escaped_type);
    else
        snprintf(cmd, sizeof(cmd), "%s check-term \"%s\" \"%s\" 2>&1", exe, escaped_term, escaped_type);

    FILE* fp = POPEN(cmd, "r");
    if (!fp) {
        if (errmsg && errmsg_size > 0) snprintf(errmsg, errmsg_size, "Failed to run kos-core");
        return -1;
    }
    if (errmsg && errmsg_size > 0) {
        errmsg[0] = '\0';
        size_t total = 0;
        while (total < errmsg_size - 1 && fgets(errmsg + total, (int)(errmsg_size - total), fp)) {
            size_t len = strlen(errmsg + total);
            total += len;
        }
        if (total > 0 && errmsg[total - 1] == '\n') errmsg[total - 1] = '\0';
    } else {
        while (fgetc(fp) != EOF) { (void)0; }
    }
    int status = PCLOSE(fp);
#if defined(_WIN32) || defined(_WIN64)
    return (status == 0) ? 0 : -1;
#else
    return (WIFEXITED(status) && WEXITSTATUS(status) == 0) ? 0 : -1;
#endif
}

/** 检查 term_expr : type_expr，使用 kos-core 类型检查。 */
bool kos_core_bridge_check_expr(const char* term_expr, const char* type_expr,
                                char* errmsg, size_t errmsg_size) {
    if (!term_expr || !type_expr) return false;
    return run_check_term_expr(term_expr, type_expr, errmsg, errmsg_size) == 0;
}

/** 运行 kos-core prove --ctx <ctx_file> <goal>，成功返回 0。 */
static int run_prove(const char* ctx_file, const char* goal,
                     char* errmsg, size_t errmsg_size) {
    char cmd[4096];
    char escaped[2048];
    const char* exe = get_kos_core_exe();
    escape_for_shell(goal, escaped, sizeof(escaped));
    if (path_has_space(exe))
        snprintf(cmd, sizeof(cmd), "\"%s\" prove --ctx \"%s\" %s 2>&1", exe, ctx_file, escaped);
    else
        snprintf(cmd, sizeof(cmd), "%s prove --ctx \"%s\" %s 2>&1", exe, ctx_file, escaped);

    FILE* fp = POPEN(cmd, "r");
    if (!fp) {
        if (errmsg && errmsg_size > 0) snprintf(errmsg, errmsg_size, "Failed to run kos-core prove");
        return -1;
    }
    if (errmsg && errmsg_size > 0) {
        errmsg[0] = '\0';
        size_t total = 0;
        while (total < errmsg_size - 1 && fgets(errmsg + total, (int)(errmsg_size - total), fp)) {
            size_t len = strlen(errmsg + total);
            total += len;
        }
        if (total > 0 && errmsg[total - 1] == '\n') errmsg[total - 1] = '\0';
    } else {
        while (fgetc(fp) != EOF) { (void)0; }
    }
    int status = PCLOSE(fp);
#if defined(_WIN32) || defined(_WIN64)
    return (status == 0) ? 0 : -1;
#else
    return (WIFEXITED(status) && WEXITSTATUS(status) == 0) ? 0 : -1;
#endif
}

/** 运行 kos-core prove-json --ctx <ctx_file> <goal>，捕获 stdout 为 JSON。 */
static int run_prove_json(const char* ctx_file, const char* goal,
                          char* json_out, size_t json_size,
                          char* errmsg, size_t errmsg_size) {
    char cmd[4096];
    char escaped[2048];
    const char* exe = get_kos_core_exe();
    escape_for_shell(goal, escaped, sizeof(escaped));
    if (path_has_space(exe))
        snprintf(cmd, sizeof(cmd), "\"%s\" prove-json --ctx \"%s\" %s 2>&1", exe, ctx_file, escaped);
    else
        snprintf(cmd, sizeof(cmd), "%s prove-json --ctx \"%s\" %s 2>&1", exe, ctx_file, escaped);

    FILE* fp = POPEN(cmd, "r");
    if (!fp) {
        if (errmsg && errmsg_size > 0) snprintf(errmsg, errmsg_size, "Failed to run kos-core prove-json");
        return -1;
    }
    if (json_out && json_size > 0) {
        json_out[0] = '\0';
        size_t total = 0;
        while (total < json_size - 1 && fgets(json_out + total, (int)(json_size - total), fp)) {
            size_t len = strlen(json_out + total);
            total += len;
            if (total > 0 && json_out[total - 1] == '\n') {
                json_out[total - 1] = '\0';
                total--;
                break;
            }
        }
    } else {
        while (fgetc(fp) != EOF) { (void)0; }
    }
    int status = PCLOSE(fp);
#if defined(_WIN32) || defined(_WIN64)
    if (status != 0) {
        if (errmsg && errmsg_size > 0 && json_out && json_out[0] != '\0') {
            strncpy(errmsg, json_out, errmsg_size - 1);
            errmsg[errmsg_size - 1] = '\0';
        }
        return -1;
    }
#else
    if (!(WIFEXITED(status) && WEXITSTATUS(status) == 0)) {
        if (errmsg && errmsg_size > 0 && json_out && json_out[0] != '\0') {
            strncpy(errmsg, json_out, errmsg_size - 1);
            errmsg[errmsg_size - 1] = '\0';
        }
        return -1;
    }
#endif
    return 0;
}

bool kos_core_bridge_prove(const char* ctx_file, const char* goal_type,
                           char* errmsg, size_t errmsg_size) {
    if (!ctx_file || !goal_type) return false;
    return run_prove(ctx_file, goal_type, errmsg, errmsg_size) == 0;
}

void* kos_core_bridge_prove_json(const char* ctx_file, const char* goal_type,
                                 char* errmsg, size_t errmsg_size) {
    if (!ctx_file || !goal_type) {
        if (errmsg && errmsg_size > 0) snprintf(errmsg, errmsg_size, "Null ctx_file or goal_type");
        return NULL;
    }
    char json_buf[65536];
    if (run_prove_json(ctx_file, goal_type, json_buf, sizeof(json_buf), errmsg, errmsg_size) != 0) {
        return NULL;
    }
    kos_term* proof = kos_term_deserialize(json_buf);
    if (!proof && errmsg && errmsg_size > 0) {
        snprintf(errmsg, errmsg_size, "Failed to deserialize proof JSON");
    }
    return proof;
}

/** 检测 kos-core 是否可用：尝试校验 "Prop P"。 */
bool kos_core_bridge_available(void) {
    char err[256];
    return kos_core_bridge_check_term("Prop P", err, sizeof(err));
}
