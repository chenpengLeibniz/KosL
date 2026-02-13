/**
 * @file core_kos_adapter.c
 * @brief Core 层适配：类型检查/归约委托给 kos-core
 *
 * 目标：清理 C 端 Core 逻辑，所有判定严格由 kos-core 执行。
 * 要求：必须能把 kos_term 完整序列化为 .kos 表达式，否则判定失败。
 */

#include "kos_core.h"
#include "kos_core_bridge.h"
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

static bool append_fmt(char* buf, size_t cap, size_t* pos, const char* fmt, ...) {
    if (!buf || !pos || *pos >= cap) return false;
    va_list args;
    va_start(args, fmt);
    int n = vsnprintf(buf + *pos, cap - *pos, fmt, args);
    va_end(args);
    if (n < 0) return false;
    if ((size_t)n >= cap - *pos) return false;
    *pos += (size_t)n;
    return true;
}

static bool append_escaped_str(char* buf, size_t cap, size_t* pos, const char* s) {
    if (!s) return false;
    if (!append_fmt(buf, cap, pos, "\"")) return false;
    for (const char* p = s; *p; ++p) {
        if (*p == '"' || *p == '\\') {
            if (!append_fmt(buf, cap, pos, "\\%c", *p)) return false;
        } else {
            if (!append_fmt(buf, cap, pos, "%c", *p)) return false;
        }
    }
    return append_fmt(buf, cap, pos, "\"");
}

static bool is_valid_ident(const char* s) {
    if (!s || !*s) return false;
    if (!((s[0] >= 'A' && s[0] <= 'Z') || (s[0] >= 'a' && s[0] <= 'z') || s[0] == '_')) {
        return false;
    }
    for (const char* p = s + 1; *p; ++p) {
        if (!((*p >= 'A' && *p <= 'Z') || (*p >= 'a' && *p <= 'z') || (*p >= '0' && *p <= '9') || *p == '_')) {
            return false;
        }
    }
    return true;
}

static bool has_binder_named(kos_term* t, const char* name, bool skip_root) {
    if (!t || !name || !*name) return false;
    if (!skip_root) {
        switch (t->kind) {
            case KOS_SIGMA:
                if (t->data.sigma.var_name && strcmp(t->data.sigma.var_name, name) == 0) return true;
                break;
            case KOS_PI:
                if (t->data.pi.var_name && strcmp(t->data.pi.var_name, name) == 0) return true;
                break;
            case KOS_LAM:
                if (t->data.lam.var_name && strcmp(t->data.lam.var_name, name) == 0) return true;
                break;
            case KOS_LET:
                if (t->data.let_term.var_name && strcmp(t->data.let_term.var_name, name) == 0) return true;
                break;
            case KOS_SPLIT:
                if ((t->data.split.var1 && strcmp(t->data.split.var1, name) == 0) ||
                    (t->data.split.var2 && strcmp(t->data.split.var2, name) == 0)) return true;
                break;
            case KOS_CASE:
                if ((t->data.case_term.left_var && strcmp(t->data.case_term.left_var, name) == 0) ||
                    (t->data.case_term.right_var && strcmp(t->data.case_term.right_var, name) == 0)) return true;
                break;
            default:
                break;
        }
    }

    switch (t->kind) {
        case KOS_PAIR:
            return has_binder_named(t->data.pair.data, name, false) ||
                   has_binder_named(t->data.pair.proof, name, false);
        case KOS_SIGMA:
            return has_binder_named(t->data.sigma.domain, name, false) ||
                   has_binder_named(t->data.sigma.body, name, false);
        case KOS_PI:
            return has_binder_named(t->data.pi.domain, name, false) ||
                   has_binder_named(t->data.pi.body, name, false) ||
                   has_binder_named(t->data.pi.body_term, name, false);
        case KOS_SUM:
            return has_binder_named(t->data.sum.left_type, name, false) ||
                   has_binder_named(t->data.sum.right_type, name, false) ||
                   has_binder_named(t->data.sum.value, name, false);
        case KOS_APP:
            return has_binder_named(t->data.app.func, name, false) ||
                   has_binder_named(t->data.app.arg, name, false);
        case KOS_LAM:
            return has_binder_named(t->data.lam.type, name, false) ||
                   has_binder_named(t->data.lam.body, name, false);
        case KOS_LET:
            return has_binder_named(t->data.let_term.type, name, false) ||
                   has_binder_named(t->data.let_term.value, name, false) ||
                   has_binder_named(t->data.let_term.body, name, false);
        case KOS_SPLIT:
            return has_binder_named(t->data.split.pair, name, false) ||
                   has_binder_named(t->data.split.body, name, false);
        case KOS_CASE:
            return has_binder_named(t->data.case_term.sum, name, false) ||
                   has_binder_named(t->data.case_term.left_body, name, false) ||
                   has_binder_named(t->data.case_term.right_body, name, false);
        case KOS_ID_TYPE:
            return has_binder_named(t->data.id_type.type, name, false) ||
                   has_binder_named(t->data.id_type.left, name, false) ||
                   has_binder_named(t->data.id_type.right, name, false);
        case KOS_REFL:
            return has_binder_named(t->data.refl.value, name, false);
        case KOS_GT:
        case KOS_GE:
        case KOS_LT:
        case KOS_LE:
        case KOS_EQ:
            return has_binder_named(t->data.pred.left, name, false) ||
                   has_binder_named(t->data.pred.right, name, false);
        default:
            return false;
    }
}

static bool var_occurs_free(kos_term* t, const char* name) {
    if (!t || !name || !*name) return false;
    switch (t->kind) {
        case KOS_ID:
        case KOS_VAL:
            return (t->data.atomic.val && strcmp(t->data.atomic.val, name) == 0);
        case KOS_PROP:
        case KOS_TIME:
        case KOS_U:
        case KOS_TYPE:
            return false;
        case KOS_PAIR:
            return var_occurs_free(t->data.pair.data, name) ||
                   var_occurs_free(t->data.pair.proof, name);
        case KOS_SIGMA: {
            if (var_occurs_free(t->data.sigma.domain, name)) return true;
            if (t->data.sigma.var_name && strcmp(t->data.sigma.var_name, name) == 0) return false;
            return var_occurs_free(t->data.sigma.body, name);
        }
        case KOS_PI: {
            if (var_occurs_free(t->data.pi.domain, name)) return true;
            if (t->data.pi.var_name && strcmp(t->data.pi.var_name, name) == 0) return false;
            if (t->data.pi.body && var_occurs_free(t->data.pi.body, name)) return true;
            return var_occurs_free(t->data.pi.body_term, name);
        }
        case KOS_SUM:
            return var_occurs_free(t->data.sum.left_type, name) ||
                   var_occurs_free(t->data.sum.right_type, name) ||
                   var_occurs_free(t->data.sum.value, name);
        case KOS_APP:
            return var_occurs_free(t->data.app.func, name) ||
                   var_occurs_free(t->data.app.arg, name);
        case KOS_LAM:
            if (var_occurs_free(t->data.lam.type, name)) return true;
            if (t->data.lam.var_name && strcmp(t->data.lam.var_name, name) == 0) return false;
            return var_occurs_free(t->data.lam.body, name);
        case KOS_LET:
            if (var_occurs_free(t->data.let_term.type, name)) return true;
            if (var_occurs_free(t->data.let_term.value, name)) return true;
            if (t->data.let_term.var_name && strcmp(t->data.let_term.var_name, name) == 0) return false;
            return var_occurs_free(t->data.let_term.body, name);
        case KOS_SPLIT:
            if (var_occurs_free(t->data.split.pair, name)) return true;
            if ((t->data.split.var1 && strcmp(t->data.split.var1, name) == 0) ||
                (t->data.split.var2 && strcmp(t->data.split.var2, name) == 0)) {
                return false;
            }
            return var_occurs_free(t->data.split.body, name);
        case KOS_CASE:
            if (var_occurs_free(t->data.case_term.sum, name)) return true;
            if (!(t->data.case_term.left_var && strcmp(t->data.case_term.left_var, name) == 0) &&
                var_occurs_free(t->data.case_term.left_body, name)) return true;
            if (!(t->data.case_term.right_var && strcmp(t->data.case_term.right_var, name) == 0) &&
                var_occurs_free(t->data.case_term.right_body, name)) return true;
            return false;
        case KOS_ID_TYPE:
            return var_occurs_free(t->data.id_type.type, name) ||
                   var_occurs_free(t->data.id_type.left, name) ||
                   var_occurs_free(t->data.id_type.right, name);
        case KOS_REFL:
            return var_occurs_free(t->data.refl.value, name);
        case KOS_GT:
        case KOS_GE:
        case KOS_LT:
        case KOS_LE:
        case KOS_EQ:
            return var_occurs_free(t->data.pred.left, name) ||
                   var_occurs_free(t->data.pred.right, name);
        default:
            return false;
    }
}

static bool is_type_term(kos_term* t) {
    if (!t) return false;
    switch (t->kind) {
        case KOS_PROP:
        case KOS_U:
        case KOS_TYPE:
        case KOS_SIGMA:
        case KOS_PI:
        case KOS_SUM:
        case KOS_GT:
        case KOS_GE:
        case KOS_LT:
        case KOS_LE:
        case KOS_EQ:
        case KOS_ID_TYPE:
            return true;
        case KOS_LET:
            return is_type_term(t->data.let_term.type);
        default:
            return false;
    }
}

static bool term_to_kos_expr_rec(kos_term* t, char* buf, size_t cap, size_t* pos) {
    if (!t || !buf || !pos) return false;
    switch (t->kind) {
        case KOS_PROP:
            if (!t->data.atomic.val) return false;
            return append_fmt(buf, cap, pos, "Prop %s", t->data.atomic.val);
        case KOS_VAL:
            if (!t->data.atomic.val) return false;
            if (!append_fmt(buf, cap, pos, "val ")) return false;
            return append_escaped_str(buf, cap, pos, t->data.atomic.val);
        case KOS_ID:
            if (!t->data.atomic.val) return false;
            if (!append_fmt(buf, cap, pos, "id ")) return false;
            return append_escaped_str(buf, cap, pos, t->data.atomic.val);
        case KOS_TIME:
            if (!t->data.atomic.val) return false;
            if (!append_fmt(buf, cap, pos, "time ")) return false;
            return append_escaped_str(buf, cap, pos, t->data.atomic.val);
        case KOS_U:
            return append_fmt(buf, cap, pos, "U%d", t->data.universe.level);
        case KOS_TYPE:
            return append_fmt(buf, cap, pos, "Type%d", t->data.universe.level);
        case KOS_SIGMA: {
            /* Sigma 类型或 split/case 消除式（基于启发式区分） */
            kos_term* dom = t->data.sigma.domain;
            kos_term* body = t->data.sigma.body;

            if (dom && body && is_type_term(dom) && is_type_term(body)) {
                const char* vname = t->data.sigma.var_name;
                if (!is_valid_ident(vname)) return false;
                if (var_occurs_free(dom, vname)) return false;
                if (!var_occurs_free(body, vname)) return false;
                if (has_binder_named(body, vname, false)) return false;
                if (!append_fmt(buf, cap, pos, "Sigma(%s : ", vname)) return false;
                if (!term_to_kos_expr_rec(dom, buf, cap, pos)) return false;
                if (!append_fmt(buf, cap, pos, "). ")) return false;
                return term_to_kos_expr_rec(body, buf, cap, pos);
            }

            return false;
        }
        case KOS_PI:
            /* 若包含 body_term，则视为 lambda（Π 引入） */
            if (t->data.pi.body_term) {
                const char* vname = t->data.pi.var_name;
                if (!vname && t->data.pi.body_term->kind == KOS_LAM && t->data.pi.body_term->data.lam.var_name) {
                    vname = t->data.pi.body_term->data.lam.var_name;
                }
                if (!is_valid_ident(vname)) return false;
                if (t->data.pi.body_term->kind == KOS_LAM &&
                    t->data.pi.body_term->data.lam.var_name &&
                    strcmp(t->data.pi.body_term->data.lam.var_name, vname) != 0) {
                    return false; /* 绑定变量不一致 */
                }
                if (var_occurs_free(t->data.pi.domain, vname)) return false;
                if (!var_occurs_free(t->data.pi.body_term, vname)) return false;
                if (has_binder_named(t->data.pi.body_term, vname, true)) return false;
                if (!append_fmt(buf, cap, pos, "lam (%s : ", vname)) return false;
                if (!term_to_kos_expr_rec(t->data.pi.domain, buf, cap, pos)) return false;
                if (!append_fmt(buf, cap, pos, "). ")) return false;
                return term_to_kos_expr_rec(t->data.pi.body_term, buf, cap, pos);
            }
            {
                const char* vname = t->data.pi.var_name;
                if (!is_valid_ident(vname)) return false;
                if (var_occurs_free(t->data.pi.domain, vname)) return false;
                if (!var_occurs_free(t->data.pi.body, vname)) return false;
                if (has_binder_named(t->data.pi.body, vname, false)) return false;
                if (!append_fmt(buf, cap, pos, "Pi(%s : ", vname)) return false;
            }
            if (!term_to_kos_expr_rec(t->data.pi.domain, buf, cap, pos)) return false;
            if (!append_fmt(buf, cap, pos, "). ")) return false;
            return term_to_kos_expr_rec(t->data.pi.body, buf, cap, pos);
        case KOS_SUM:
            if (t->data.sum.value) {
                const char* ctor = t->data.sum.is_left ? "inl" : "inr";
                if (!append_fmt(buf, cap, pos, "%s(", ctor)) return false;
                if (!term_to_kos_expr_rec(t->data.sum.left_type, buf, cap, pos)) return false;
                if (!append_fmt(buf, cap, pos, ", ")) return false;
                if (!term_to_kos_expr_rec(t->data.sum.right_type, buf, cap, pos)) return false;
                if (!append_fmt(buf, cap, pos, ", ")) return false;
                if (!term_to_kos_expr_rec(t->data.sum.value, buf, cap, pos)) return false;
                return append_fmt(buf, cap, pos, ")");
            }
            if (!append_fmt(buf, cap, pos, "(")) return false;
            if (!term_to_kos_expr_rec(t->data.sum.left_type, buf, cap, pos)) return false;
            if (!append_fmt(buf, cap, pos, " + ")) return false;
            if (!term_to_kos_expr_rec(t->data.sum.right_type, buf, cap, pos)) return false;
            return append_fmt(buf, cap, pos, ")");
        case KOS_PAIR:
            if (!append_fmt(buf, cap, pos, "< ")) return false;
            if (!term_to_kos_expr_rec(t->data.pair.data, buf, cap, pos)) return false;
            if (!append_fmt(buf, cap, pos, " , ")) return false;
            if (!term_to_kos_expr_rec(t->data.pair.proof, buf, cap, pos)) return false;
            return append_fmt(buf, cap, pos, " >");
        case KOS_APP:
            if (!append_fmt(buf, cap, pos, "(")) return false;
            if (!term_to_kos_expr_rec(t->data.app.func, buf, cap, pos)) return false;
            if (!append_fmt(buf, cap, pos, " ")) return false;
            if (!term_to_kos_expr_rec(t->data.app.arg, buf, cap, pos)) return false;
            return append_fmt(buf, cap, pos, ")");
        case KOS_LAM:
            if (!t->data.lam.var_name || !t->data.lam.type || !t->data.lam.body) return false;
            if (!is_valid_ident(t->data.lam.var_name)) return false;
            if (var_occurs_free(t->data.lam.type, t->data.lam.var_name)) return false;
            if (!var_occurs_free(t->data.lam.body, t->data.lam.var_name)) return false;
            if (has_binder_named(t->data.lam.body, t->data.lam.var_name, false)) return false;
            if (!append_fmt(buf, cap, pos, "lam (%s : ", t->data.lam.var_name)) return false;
            if (!term_to_kos_expr_rec(t->data.lam.type, buf, cap, pos)) return false;
            if (!append_fmt(buf, cap, pos, "). ")) return false;
            return term_to_kos_expr_rec(t->data.lam.body, buf, cap, pos);
        case KOS_GT:
        case KOS_GE:
        case KOS_LT:
        case KOS_LE:
        case KOS_EQ: {
            const char* op = (t->kind == KOS_GT) ? ">" :
                             (t->kind == KOS_GE) ? ">=" :
                             (t->kind == KOS_LT) ? "<" :
                             (t->kind == KOS_LE) ? "<=" : "==";
            if (!append_fmt(buf, cap, pos, "(")) return false;
            if (!term_to_kos_expr_rec(t->data.pred.left, buf, cap, pos)) return false;
            if (!append_fmt(buf, cap, pos, " %s ", op)) return false;
            if (!term_to_kos_expr_rec(t->data.pred.right, buf, cap, pos)) return false;
            return append_fmt(buf, cap, pos, ")");
        }
        case KOS_ID_TYPE:
            if (!append_fmt(buf, cap, pos, "Id(")) return false;
            if (!term_to_kos_expr_rec(t->data.id_type.type, buf, cap, pos)) return false;
            if (!append_fmt(buf, cap, pos, ", ")) return false;
            if (!term_to_kos_expr_rec(t->data.id_type.left, buf, cap, pos)) return false;
            if (!append_fmt(buf, cap, pos, ", ")) return false;
            if (!term_to_kos_expr_rec(t->data.id_type.right, buf, cap, pos)) return false;
            return append_fmt(buf, cap, pos, ")");
        case KOS_REFL:
            if (!append_fmt(buf, cap, pos, "refl (")) return false;
            if (!term_to_kos_expr_rec(t->data.refl.value, buf, cap, pos)) return false;
            return append_fmt(buf, cap, pos, ")");
        case KOS_LET:
            if (!t->data.let_term.var_name || !is_valid_ident(t->data.let_term.var_name)) return false;
            if (var_occurs_free(t->data.let_term.type, t->data.let_term.var_name)) return false;
            if (var_occurs_free(t->data.let_term.value, t->data.let_term.var_name)) return false;
            if (!var_occurs_free(t->data.let_term.body, t->data.let_term.var_name)) return false;
            if (has_binder_named(t->data.let_term.body, t->data.let_term.var_name, false)) return false;
            if (!append_fmt(buf, cap, pos, "let %s : ", t->data.let_term.var_name)) return false;
            if (!term_to_kos_expr_rec(t->data.let_term.type, buf, cap, pos)) return false;
            if (!append_fmt(buf, cap, pos, " := ")) return false;
            if (!term_to_kos_expr_rec(t->data.let_term.value, buf, cap, pos)) return false;
            if (!append_fmt(buf, cap, pos, " in ")) return false;
            return term_to_kos_expr_rec(t->data.let_term.body, buf, cap, pos);
        case KOS_SPLIT:
            if (!t->data.split.pair || !t->data.split.body) return false;
            if (!t->data.split.var1 || !t->data.split.var2) return false;
            if (!is_valid_ident(t->data.split.var1) || !is_valid_ident(t->data.split.var2)) return false;
            if (strcmp(t->data.split.var1, t->data.split.var2) == 0) return false;
            if (t->data.split.pair->kind != KOS_PAIR) return false;
            if (var_occurs_free(t->data.split.pair, t->data.split.var1)) return false;
            if (var_occurs_free(t->data.split.pair, t->data.split.var2)) return false;
            if (!var_occurs_free(t->data.split.body, t->data.split.var1)) return false;
            if (!var_occurs_free(t->data.split.body, t->data.split.var2)) return false;
            if (has_binder_named(t->data.split.body, t->data.split.var1, false)) return false;
            if (has_binder_named(t->data.split.body, t->data.split.var2, false)) return false;
            {
                const char* v1 = t->data.split.var1;
                const char* v2 = t->data.split.var2;
                if (!append_fmt(buf, cap, pos, "split (")) return false;
                if (!term_to_kos_expr_rec(t->data.split.pair, buf, cap, pos)) return false;
                if (!append_fmt(buf, cap, pos, ") as %s %s in ", v1, v2)) return false;
                return term_to_kos_expr_rec(t->data.split.body, buf, cap, pos);
            }
        case KOS_CASE:
            if (!t->data.case_term.sum || !t->data.case_term.left_body || !t->data.case_term.right_body) return false;
            {
                const char* lv = t->data.case_term.left_var;
                const char* rv = t->data.case_term.right_var;
                if (!is_valid_ident(lv) || !is_valid_ident(rv)) return false;
                if (strcmp(lv, rv) == 0) return false;
                if (var_occurs_free(t->data.case_term.sum, lv)) return false;
                if (var_occurs_free(t->data.case_term.sum, rv)) return false;
                if (!var_occurs_free(t->data.case_term.left_body, lv)) return false;
                if (!var_occurs_free(t->data.case_term.right_body, rv)) return false;
                if (has_binder_named(t->data.case_term.left_body, lv, false)) return false;
                if (has_binder_named(t->data.case_term.right_body, rv, false)) return false;
                if (!append_fmt(buf, cap, pos, "case ")) return false;
                if (!term_to_kos_expr_rec(t->data.case_term.sum, buf, cap, pos)) return false;
                if (!append_fmt(buf, cap, pos, " of inl %s -> ", lv)) return false;
                if (!term_to_kos_expr_rec(t->data.case_term.left_body, buf, cap, pos)) return false;
                if (!append_fmt(buf, cap, pos, "; inr %s -> ", rv)) return false;
                return term_to_kos_expr_rec(t->data.case_term.right_body, buf, cap, pos);
            }
        default:
            return false;
    }
}

static bool term_to_kos_expr(kos_term* t, char* buf, size_t cap) {
    if (!t || !buf || cap == 0) return false;
    size_t pos = 0;
    buf[0] = '\0';
    if (!term_to_kos_expr_rec(t, buf, cap, &pos)) return false;
    if (pos >= cap) return false;
    buf[pos] = '\0';
    return true;
}

bool kos_check_via_kos(const char* term_expr, const char* type_expr,
                       char* errmsg, size_t errmsg_size) {
    if (!term_expr || !type_expr) {
        if (errmsg && errmsg_size > 0) {
            snprintf(errmsg, errmsg_size, "Null term/type expr");
        }
        return false;
    }
    return kos_core_bridge_check_expr(term_expr, type_expr, errmsg, errmsg_size);
}

bool kos_type_wellformed(kos_term* type) {
    char expr[2048];
    char err[256];
    if (term_to_kos_expr(type, expr, sizeof(expr))) {
        return kos_core_bridge_check_term(expr, err, sizeof(err));
    }
    return false;
}

bool kos_check(kos_term* ctx, kos_term* term, kos_term* type) {
    (void)ctx;
    return kos_type_check(NULL, term, type);
}

bool kos_type_check(kos_term* ctx, kos_term* proof, kos_term* prop) {
    (void)ctx;
    char term_expr[2048];
    char type_expr[2048];
    char err[256];

    if (term_to_kos_expr(proof, term_expr, sizeof(term_expr)) &&
        term_to_kos_expr(prop, type_expr, sizeof(type_expr))) {
        return kos_check_via_kos(term_expr, type_expr, err, sizeof(err));
    }
    return false;
}

kos_term* kos_reduce(kos_term* t) {
    /* 归约由 kos-core 完成；C 端不做归约 */
    return t;
}
