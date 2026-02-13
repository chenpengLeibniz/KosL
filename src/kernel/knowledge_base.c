/**
 * @file knowledge_base.c
 * @brief KOS Kernel 层知识库实现
 *
 * 实现 Kos.pdf 2.2.2 中的知识库 K = { (id_i, t_i, A_i) | Γ_Core ⊢ t_i : A_i }，
 * 支持启动抽取与运行时物化两种来源，以及基于依赖项的可视化。
 */

#include "kos_knowledge_base.h"
#include "kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* ========== 知识库创建与释放 ========== */

kos_knowledge_base_t* kos_kb_create(void) {
    kos_knowledge_base_t* kb = (kos_knowledge_base_t*)calloc(1, sizeof(kos_knowledge_base_t));
    if (!kb) {
        return NULL;
    }
    kb->items = NULL;
    kb->edges = NULL;
    kb->item_count = 0;
    kb->edge_count = 0;
    return kb;
}

static void free_item(kos_kb_item_t* item) {
    if (!item) return;
    if (item->id) free(item->id);
    if (item->term) kos_term_free(item->term);
    if (item->type) kos_term_free(item->type);
    free_item(item->next);
    free(item);
}

static void free_edges(kos_kb_dep_edge_t* edge) {
    if (!edge) return;
    if (edge->from_id) free(edge->from_id);
    if (edge->to_id) free(edge->to_id);
    free_edges(edge->next);
    free(edge);
}

void kos_kb_free(kos_knowledge_base_t* kb) {
    if (!kb) return;
    free_item(kb->items);
    free_edges(kb->edges);
    free(kb);
}

/* ========== 知识项添加 ========== */

int kos_kb_add_item(kos_knowledge_base_t* kb, const char* id,
                    kos_term* t, kos_term* A, int logical_ts,
                    kos_kb_source_t source) {
    if (!kb || !id || !t || !A) {
        return -1;
    }

    /* 检查 id 是否已存在 */
    if (kos_kb_find(kb, id)) {
        return -1;  /* 重复 id */
    }

    kos_kb_item_t* item = (kos_kb_item_t*)calloc(1, sizeof(kos_kb_item_t));
    if (!item) {
        return -1;
    }

    item->id = strdup(id);
    if (!item->id) {
        free(item);
        return -1;
    }

    item->term = kos_term_copy(t);
    item->type = kos_term_copy(A);
    if (!item->term || !item->type) {
        if (item->term) kos_term_free(item->term);
        if (item->type) kos_term_free(item->type);
        free(item->id);
        free(item);
        return -1;
    }

    item->logical_ts = logical_ts;
    item->source = source;
    item->next = kb->items;
    kb->items = item;
    kb->item_count++;
    return 0;
}

/* ========== 依赖边添加 ========== */

int kos_kb_add_dependency(kos_knowledge_base_t* kb,
                          const char* from_id, const char* to_id) {
    return kos_kb_add_dependency_typed(kb, from_id, to_id, KOS_KB_DEP_TYPE_EXPLICIT);
}

int kos_kb_add_dependency_typed(kos_knowledge_base_t* kb,
                                const char* from_id, const char* to_id,
                                kos_kb_dep_type_t dep_type) {
    if (!kb || !from_id || !to_id || strcmp(from_id, to_id) == 0) {
        return -1;
    }

    kos_kb_dep_edge_t* edge = (kos_kb_dep_edge_t*)calloc(1, sizeof(kos_kb_dep_edge_t));
    if (!edge) return -1;

    edge->from_id = strdup(from_id);
    edge->to_id = strdup(to_id);
    if (!edge->from_id || !edge->to_id) {
        if (edge->from_id) free(edge->from_id);
        if (edge->to_id) free(edge->to_id);
        free(edge);
        return -1;
    }
    edge->dep_type = dep_type;
    edge->next = kb->edges;
    kb->edges = edge;
    kb->edge_count++;
    return 0;
}

/* ========== 依赖推断：从项结构收集引用 ========== */

typedef struct {
    char** refs;
    size_t count;
    size_t cap;
} ref_collector_t;

static void ref_collector_add(ref_collector_t* rc, const char* s) {
    if (!rc || !s || *s == '\0') return;
    if (rc->count >= rc->cap) {
        size_t new_cap = rc->cap ? rc->cap * 2 : 8;
        char** new_refs = (char**)realloc(rc->refs, new_cap * sizeof(char*));
        if (!new_refs) return;
        rc->refs = new_refs;
        rc->cap = new_cap;
    }
    rc->refs[rc->count++] = strdup(s);
}

static void ref_collector_free(ref_collector_t* rc) {
    if (!rc) return;
    for (size_t i = 0; i < rc->count; i++) free(rc->refs[i]);
    free(rc->refs);
    rc->refs = NULL;
    rc->count = rc->cap = 0;
}

static void collect_refs_from_term(kos_term* t, ref_collector_t* rc) {
    if (!t || !rc) return;
    switch (t->kind) {
        case KOS_PROP:
        case KOS_VAL:
        case KOS_ID:
            if (t->data.atomic.val) ref_collector_add(rc, t->data.atomic.val);
            if (t->data.atomic.type) collect_refs_from_term(t->data.atomic.type, rc);
            break;
        case KOS_PAIR:
            if (t->data.pair.data) collect_refs_from_term(t->data.pair.data, rc);
            if (t->data.pair.proof) collect_refs_from_term(t->data.pair.proof, rc);
            break;
        case KOS_SIGMA:
            if (t->data.sigma.domain) collect_refs_from_term(t->data.sigma.domain, rc);
            if (t->data.sigma.body) collect_refs_from_term(t->data.sigma.body, rc);
            break;
        case KOS_PI:
            if (t->data.pi.domain) collect_refs_from_term(t->data.pi.domain, rc);
            if (t->data.pi.body) collect_refs_from_term(t->data.pi.body, rc);
            if (t->data.pi.body_term) collect_refs_from_term(t->data.pi.body_term, rc);
            break;
        case KOS_SUM:
            if (t->data.sum.left_type) collect_refs_from_term(t->data.sum.left_type, rc);
            if (t->data.sum.right_type) collect_refs_from_term(t->data.sum.right_type, rc);
            if (t->data.sum.value) collect_refs_from_term(t->data.sum.value, rc);
            break;
        case KOS_SPLIT:
            if (t->data.split.pair) collect_refs_from_term(t->data.split.pair, rc);
            if (t->data.split.body) collect_refs_from_term(t->data.split.body, rc);
            break;
        case KOS_CASE:
            if (t->data.case_term.sum) collect_refs_from_term(t->data.case_term.sum, rc);
            if (t->data.case_term.left_body) collect_refs_from_term(t->data.case_term.left_body, rc);
            if (t->data.case_term.right_body) collect_refs_from_term(t->data.case_term.right_body, rc);
            break;
        case KOS_ID_TYPE:
            if (t->data.id_type.type) collect_refs_from_term(t->data.id_type.type, rc);
            if (t->data.id_type.left) collect_refs_from_term(t->data.id_type.left, rc);
            if (t->data.id_type.right) collect_refs_from_term(t->data.id_type.right, rc);
            break;
        case KOS_REFL:
            if (t->data.refl.value) collect_refs_from_term(t->data.refl.value, rc);
            break;
        case KOS_LET:
            if (t->data.let_term.type) collect_refs_from_term(t->data.let_term.type, rc);
            if (t->data.let_term.value) collect_refs_from_term(t->data.let_term.value, rc);
            if (t->data.let_term.body) collect_refs_from_term(t->data.let_term.body, rc);
            break;
        case KOS_GT:
        case KOS_GE:
        case KOS_LT:
        case KOS_LE:
        case KOS_EQ:
            if (t->data.pred.left) collect_refs_from_term(t->data.pred.left, rc);
            if (t->data.pred.right) collect_refs_from_term(t->data.pred.right, rc);
            break;
        default:
            break;
    }
}

static bool edge_exists(kos_knowledge_base_t* kb, const char* from_id, const char* to_id) {
    for (kos_kb_dep_edge_t* e = kb->edges; e; e = e->next) {
        if (e->from_id && e->to_id &&
            strcmp(e->from_id, from_id) == 0 && strcmp(e->to_id, to_id) == 0)
            return true;
    }
    return false;
}

int kos_kb_infer_dependencies(kos_knowledge_base_t* kb) {
    if (!kb) return 0;
    int added = 0;
    for (kos_kb_item_t* p = kb->items; p; p = p->next) {
        if (!p->id) continue;
        ref_collector_t rc = {0};
        collect_refs_from_term(p->term, &rc);
        collect_refs_from_term(p->type, &rc);
        for (size_t i = 0; i < rc.count; i++) {
            const char* ref = rc.refs[i];
            if (!ref || strcmp(ref, p->id) == 0) continue;
            if (kos_kb_find(kb, ref) && !edge_exists(kb, p->id, ref)) {
                if (kos_kb_add_dependency_typed(kb, p->id, ref, KOS_KB_DEP_TYPE_REF) == 0)
                    added++;
            }
        }
        ref_collector_free(&rc);
    }
    return added;
}

/* ========== 查找 ========== */

kos_kb_item_t* kos_kb_find(kos_knowledge_base_t* kb, const char* id) {
    if (!kb || !id) return NULL;
    for (kos_kb_item_t* p = kb->items; p; p = p->next) {
        if (p->id && strcmp(p->id, id) == 0) {
            return p;
        }
    }
    return NULL;
}

/* ========== 项标签（用于可视化） ========== */

static const char* term_kind_str(term_kind k) {
    switch (k) {
        case KOS_VAL: return "Val";
        case KOS_TIME: return "Time";
        case KOS_ID: return "Id";
        case KOS_ID_TYPE: return "IdType";
        case KOS_SIGMA: return "Sigma";
        case KOS_PAIR: return "Pair";
        case KOS_PROP: return "Prop";
        case KOS_PI: return "Pi";
        case KOS_SUM: return "Sum";
        case KOS_U: return "U";
        case KOS_TYPE: return "Type";
        case KOS_GT: return "Gt";
        case KOS_GE: return "Ge";
        case KOS_LT: return "Lt";
        case KOS_LE: return "Le";
        case KOS_EQ: return "Eq";
        case KOS_SPLIT: return "Split";
        case KOS_CASE: return "Case";
        case KOS_REFL: return "Refl";
        case KOS_LET: return "Let";
        default: return "?";
    }
}

static void term_to_label(kos_term* t, char* buf, size_t cap) {
    if (!t || !buf || cap == 0) { buf[0] = '\0'; return; }
    if (t->kind == KOS_PROP || t->kind == KOS_VAL || t->kind == KOS_ID) {
        if (t->data.atomic.val) {
            size_t len = strlen(t->data.atomic.val);
            if (len >= cap) len = cap - 1;
            memcpy(buf, t->data.atomic.val, len);
            buf[len] = '\0';
            return;
        }
    }
    snprintf(buf, cap, "%s", term_kind_str(t->kind));
}

static const char* dep_type_str(kos_kb_dep_type_t t) {
    switch (t) {
        case KOS_KB_DEP_TYPE_REF: return "ref";
        case KOS_KB_DEP_TYPE_SUBTERM: return "subterm";
        case KOS_KB_DEP_TYPE_CAUSAL: return "causal";
        case KOS_KB_DEP_TYPE_EXPLICIT: return "explicit";
        default: return "dep";
    }
}

/* ========== 依赖图 JSON 导出 ========== */

static size_t json_escape_len(const char* s) {
    size_t n = 2;  /* quotes */
    if (!s) return n;
    for (; *s; s++) {
        if (*s == '"' || *s == '\\' || *s == '\n' || *s == '\r' || *s == '\t')
            n += 2;
        else
            n += 1;
    }
    return n;
}

static void json_escape_append(const char* s, char* out, size_t* pos, size_t cap) {
    if (!out || !pos || *pos >= cap) return;
    out[(*pos)++] = '"';
    if (!s) { out[(*pos)++] = '"'; return; }
    for (; *s && *pos < cap - 2; s++) {
        switch (*s) {
            case '"':  out[(*pos)++] = '\\'; out[(*pos)++] = '"';  break;
            case '\\': out[(*pos)++] = '\\'; out[(*pos)++] = '\\'; break;
            case '\n': out[(*pos)++] = '\\'; out[(*pos)++] = 'n';  break;
            case '\r': out[(*pos)++] = '\\'; out[(*pos)++] = 'r';  break;
            case '\t': out[(*pos)++] = '\\'; out[(*pos)++] = 't';  break;
            default:   out[(*pos)++] = *s; break;
        }
    }
    out[(*pos)++] = '"';
}

char* kos_kb_export_dependency_graph_json(kos_knowledge_base_t* kb) {
    if (!kb) return NULL;

    size_t cap = 16384;
    char* buf = (char*)malloc(cap);
    if (!buf) return NULL;
    size_t pos = 0;
    char label_buf[128];

    pos += (size_t)snprintf(buf + pos, cap - pos, "{\"nodes\":[");
    bool first_node = true;
    for (kos_kb_item_t* p = kb->items; p; p = p->next) {
        if (pos + 384 >= cap) break;
        if (!first_node) buf[pos++] = ',';
        first_node = false;
        term_to_label(p->term, label_buf, sizeof(label_buf));
        pos += (size_t)snprintf(buf + pos, cap - pos, "{\"id\":");
        json_escape_append(p->id ? p->id : "", buf, &pos, cap);
        pos += (size_t)snprintf(buf + pos, cap - pos, ",\"label\":");
        json_escape_append(label_buf, buf, &pos, cap);
        pos += (size_t)snprintf(buf + pos, cap - pos, ",\"source\":\"%s\",\"ts\":%d,\"kind\":\"%s\"}",
            p->source == KOS_KB_SOURCE_BOOTSTRAP ? "bootstrap" : "materialized",
            p->logical_ts, term_kind_str(p->term->kind));
    }
    pos += (size_t)snprintf(buf + pos, cap - pos, "],\"edges\":[");
    bool first_edge = true;
    for (kos_kb_dep_edge_t* e = kb->edges; e; e = e->next) {
        if (pos + 384 >= cap) break;
        if (!first_edge) buf[pos++] = ',';
        first_edge = false;
        pos += (size_t)snprintf(buf + pos, cap - pos, "{\"from\":");
        json_escape_append(e->from_id ? e->from_id : "", buf, &pos, cap);
        pos += (size_t)snprintf(buf + pos, cap - pos, ",\"to\":");
        json_escape_append(e->to_id ? e->to_id : "", buf, &pos, cap);
        pos += (size_t)snprintf(buf + pos, cap - pos, ",\"relation\":\"%s\"}",
            dep_type_str(e->dep_type));
    }
    pos += (size_t)snprintf(buf + pos, cap - pos, "]}");
    buf[pos] = '\0';
    return buf;
}

/* ========== HTML 可视化导出 ========== */

char* kos_kb_export_visualization_html(kos_knowledge_base_t* kb) {
    if (!kb) return NULL;
    char* json = kos_kb_export_dependency_graph_json(kb);
    if (!json) return NULL;

    /* 嵌入 JS 时需转义 </script> 以防提前闭合标签 */
    size_t json_len = strlen(json);
    size_t cap = 8192 + json_len * 2;
    char* escaped = (char*)malloc(cap);
    if (!escaped) { free(json); return NULL; }
    size_t ep = 0;
    const char* p = json;
    while (*p && ep < cap - 4) {
        if (*p == '<' && strncmp(p, "</script>", 9) == 0) {
            memcpy(escaped + ep, "<\\/script>", 10);
            ep += 10;
            p += 9;
        } else {
            escaped[ep++] = *p++;
        }
    }
    escaped[ep] = '\0';
    free(json);

    static const char* html_tpl =
        "<!DOCTYPE html><html><head><meta charset=\"utf-8\">"
        "<script src=\"https://d3js.org/d3.v7.min.js\"></script>"
        "<style>"
        "body{font-family:sans-serif;margin:0;background:#f5f5f5;}"
        "svg{background:#fff;border-radius:8px;box-shadow:0 2px 8px rgba(0,0,0,.1);}"
        ".node{cursor:pointer;stroke:#fff;stroke-width:2px;}"
        ".node.bootstrap{fill:#4caf50;}"
        ".node.materialized{fill:#2196f3;}"
        ".link{stroke:#999;stroke-opacity:.6;}"
        ".link.ref{stroke:#9e9e9e;}"
        ".link.causal{stroke:#f44336;stroke-width:2px;}"
        ".link.explicit{stroke:#ff9800;}"
        "text{font-size:11px;pointer-events:none;}"
        "</style></head><body>"
        "<h2 style=\"margin:16px\">KOS 知识库依赖图</h2>"
        "<div id=\"graph\"></div>"
        "<script>"
        "const data=%s;"
        "const w=800,h=500;"
        "const svg=d3.select('#graph').append('svg').attr('width',w).attr('height',h);"
        "const g=svg.append('g');"
        "const zoom=d3.zoom().scaleExtent([.2,4]).on('zoom',ev=>g.attr('transform',ev.transform));"
        "svg.call(zoom);"
        "const nodes=data.nodes.map(d=>({...d}));"
        "const links=data.edges.map(d=>({source:d.from,target:d.to,relation:d.relation||'dep'}));"
        "const simulation=d3.forceSimulation(nodes)"
        ".force('link',d3.forceLink(links).id(d=>d.id).distance(80))"
        ".force('charge',d3.forceManyBody().strength(-120))"
        ".force('center',d3.forceCenter(w/2,h/2));"
        "const link=g.append('g').selectAll('line').data(links).join('line')"
        ".attr('class',d=>'link '+d.relation);"
        "const node=g.append('g').selectAll('circle').data(nodes).join('circle')"
        ".attr('class',d=>'node '+d.source)"
        ".attr('r',8)"
        ".call(d3.drag().on('start',(ev,d)=>{if(!ev.active)simulation.alphaTarget(.3).restart();d.fx=d.x;d.fy=d.y;})"
        ".on('drag',(ev,d)=>{d.fx=ev.x;d.fy=ev.y;})"
        ".on('end',(ev,d)=>{if(!ev.active)simulation.alphaTarget(0);d.fx=null;d.fy=null;}));"
        "node.append('title').text(d=>d.id+' | '+d.label+' | ts:'+d.ts);"
        "const label=g.append('g').selectAll('text').data(nodes).join('text')"
        ".text(d=>d.label||d.id).attr('x',10).attr('y',4);"
        "simulation.on('tick',()=>{"
        "link.attr('x1',d=>d.source.x).attr('y1',d=>d.source.y)"
        ".attr('x2',d=>d.target.x).attr('y2',d=>d.target.y);"
        "node.attr('cx',d=>d.x).attr('cy',d=>d.y);"
        "label.attr('x',d=>d.x+10).attr('y',d=>d.y+4);"
        "});"
        "</script></body></html>";

    size_t html_cap = strlen(html_tpl) + ep + 256;
    char* html = (char*)malloc(html_cap);
    if (!html) { free(escaped); return NULL; }
    snprintf(html, html_cap, html_tpl, escaped);
    free(escaped);
    return html;
}

/* ========== 反事实推理演示 HTML 导出 ==========
 * 生成包含两个场景的 combined 可视化页面 */

static void escape_json_for_script(const char* json, char* out, size_t cap) {
    const char* p = json;
    size_t ep = 0;
    while (*p && ep < cap - 4) {
        if (*p == '<' && strncmp(p, "</script>", 9) == 0) {
            memcpy(out + ep, "<\\/script>", 10);
            ep += 10;
            p += 9;
        } else {
            out[ep++] = *p++;
        }
    }
    out[ep] = '\0';
}

char* kos_export_counterfactual_demo_html(kos_knowledge_base_t* kb1,
                                          kos_knowledge_base_t* kb2,
                                          const char* cf_result1,
                                          const char* cf_result2) {
    if (!kb1 || !kb2) return NULL;
    char* json1 = kos_kb_export_dependency_graph_json(kb1);
    char* json2 = kos_kb_export_dependency_graph_json(kb2);
    if (!json1 || !json2) {
        free(json1);
        free(json2);
        return NULL;
    }
    size_t len1 = strlen(json1), len2 = strlen(json2);
    char* esc1 = (char*)malloc(len1 * 2 + 256);
    char* esc2 = (char*)malloc(len2 * 2 + 256);
    if (!esc1 || !esc2) {
        free(json1);
        free(json2);
        free(esc1);
        free(esc2);
        return NULL;
    }
    escape_json_for_script(json1, esc1, len1 * 2 + 256);
    escape_json_for_script(json2, esc2, len2 * 2 + 256);
    free(json1);
    free(json2);

    static const char* tpl =
        "<!DOCTYPE html><html><head><meta charset=\"utf-8\">"
        "<script src=\"https://d3js.org/d3.v7.min.js\"></script>"
        "<style>"
        "body{font-family:'Segoe UI',sans-serif;margin:0;background:linear-gradient(135deg,#1a1a2e 0%,#16213e 100%);color:#eee;}"
        ".container{max-width:1200px;margin:0 auto;padding:24px;}"
        "h1{text-align:center;margin-bottom:8px;font-weight:300;}"
        ".intro{text-align:center;color:#aaa;margin-bottom:32px;font-size:14px;}"
        ".scenario{background:rgba(255,255,255,.05);border-radius:12px;padding:24px;margin-bottom:24px;border:1px solid rgba(255,255,255,.1);}"
        ".scenario h2{color:#4fc3f7;font-size:18px;margin:0 0 12px 0;}"
        ".scenario .desc{color:#b0bec5;font-size:13px;margin-bottom:16px;}"
        ".scenario .graph-wrap{background:#0d1117;border-radius:8px;padding:16px;margin-bottom:16px;}"
        ".scenario .cf-result{padding:12px 16px;border-radius:8px;font-size:14px;}"
        ".scenario .cf-necessary{background:rgba(76,175,80,.2);border-left:4px solid #4caf50;}"
        ".scenario .cf-alternative{background:rgba(255,152,0,.2);border-left:4px solid #ff9800;}"
        "svg{background:#161b22;border-radius:6px;display:block;}"
        ".node{cursor:pointer;stroke:#fff;stroke-width:2px;}"
        ".node.bootstrap{fill:#4caf50;}"
        ".node.materialized{fill:#2196f3;}"
        ".link{stroke:#666;stroke-opacity:.7;}"
        ".link.causal{stroke:#f44336;stroke-width:2px;}"
        "text{font-size:11px;fill:#e0e0e0;pointer-events:none;}"
        "</style></head><body>"
        "<div class=\"container\">"
        "<h1>KOS-TL 反事实推理可视化</h1>"
        "<p class=\"intro\">反事实推理：\"若 X 未发生，Y 是否仍会发生？\" 用于验证根因的因果必要性</p>"
        "<div class=\"scenario\">"
        "<h2>场景 1：单一根因</h2>"
        "<p class=\"desc\">知识：工艺步骤、温度异常、失败事件。温度异常是唯一可解释失败的原因。</p>"
        "<div class=\"graph-wrap\"><div id=\"graph1\"></div></div>"
        "<div class=\"cf-result cf-necessary\"><strong>[反事实]</strong> 若温度异常未发生，能否解释失败？ → %s</div>"
        "</div>"
        "<div class=\"scenario\">"
        "<h2>场景 2：多因可选</h2>"
        "<p class=\"desc\">知识：工艺步骤、温度异常、压力异常、失败事件。两者都可解释失败。</p>"
        "<div class=\"graph-wrap\"><div id=\"graph2\"></div></div>"
        "<div class=\"cf-result cf-alternative\"><strong>[反事实]</strong> 若温度异常未发生，能否解释失败？ → %s</div>"
        "</div>"
        "</div>"
        "<script>"
        "function renderGraph(sel,data,resultClass){"
        "const w=700,h=350;"
        "const svg=d3.select(sel).append('svg').attr('width',w).attr('height',h);"
        "const g=svg.append('g');"
        "const zoom=d3.zoom().scaleExtent([.2,4]).on('zoom',ev=>g.attr('transform',ev.transform));"
        "svg.call(zoom);"
        "const nodes=data.nodes.map(d=>({...d}));"
        "const links=data.edges.map(d=>({source:d.from,target:d.to,relation:d.relation||'dep'}));"
        "const sim=d3.forceSimulation(nodes)"
        ".force('link',d3.forceLink(links).id(d=>d.id).distance(100))"
        ".force('charge',d3.forceManyBody().strength(-150))"
        ".force('center',d3.forceCenter(w/2,h/2));"
        "g.append('g').selectAll('line').data(links).join('line').attr('class',d=>'link '+d.relation);"
        "const node=g.append('g').selectAll('circle').data(nodes).join('circle')"
        ".attr('class',d=>'node '+d.source).attr('r',10)"
        ".call(d3.drag().on('start',(e,d)=>{if(!e.active)sim.alphaTarget(.3).restart();d.fx=d.x;d.fy=d.y;})"
        ".on('drag',(e,d)=>{d.fx=e.x;d.fy=e.y;}).on('end',(e,d)=>{if(!e.active)sim.alphaTarget(0);d.fx=null;d.fy=null;}));"
        "node.append('title').text(d=>d.label||d.id);"
        "const lbl=g.append('g').selectAll('text').data(nodes).join('text').text(d=>(d.label||d.id).substring(0,40)).attr('x',12).attr('y',4);"
        "sim.on('tick',()=>{"
        "g.selectAll('line').attr('x1',d=>d.source.x).attr('y1',d=>d.source.y).attr('x2',d=>d.target.x).attr('y2',d=>d.target.y);"
        "node.attr('cx',d=>d.x).attr('cy',d=>d.y);"
        "lbl.attr('x',d=>d.x+12).attr('y',d=>d.y+4);"
        "});"
        "}"
        "const d1=%s;"
        "const d2=%s;"
        "renderGraph('#graph1',d1);"
        "renderGraph('#graph2',d2);"
        "</script></body></html>";

    size_t r1_len = cf_result1 ? strlen(cf_result1) : 0;
    size_t r2_len = cf_result2 ? strlen(cf_result2) : 0;
    size_t html_cap = strlen(tpl) + strlen(esc1) + strlen(esc2) + r1_len + r2_len + 512;
    char* html = (char*)malloc(html_cap);
    if (!html) {
        free(esc1);
        free(esc2);
        return NULL;
    }
    snprintf(html, html_cap, tpl,
             cf_result1 ? cf_result1 : "否，无法找到因果链 → 温度异常是因果必要的",
             cf_result2 ? cf_result2 : "是，压力异常仍可解释 → 温度异常非严格必要",
             esc1, esc2);
    free(esc1);
    free(esc2);
    return html;
}

/* ========== Σ 链转换 ========== */

kos_term* kos_kb_to_sigma_chain(kos_knowledge_base_t* kb) {
    if (!kb || !kb->items) return NULL;

    kos_term* chain = NULL;
    for (kos_kb_item_t* p = kb->items; p; p = p->next) {
        /* 每个知识项表示为 <term, type> 的 pair，再链成 Σ */
        kos_term* pair = kos_mk_pair(kos_term_copy(p->term), kos_term_copy(p->type));
        if (!pair) continue;
        if (!chain) {
            chain = pair;
        } else {
            kos_term* new_chain = kos_mk_pair(pair, chain);
            kos_term_free(pair);
            if (!new_chain) break;
            kos_term_free(chain);
            chain = new_chain;
        }
    }
    return chain;
}

/* ========== 从 Σ 链合并 ==========
 * 链结构: (fact1, (fact2, (fact3, ...)))，每个 fact 为 (t, A) 的 pair */

int kos_kb_merge_from_sigma_chain(kos_knowledge_base_t* kb,
                                  kos_term* chain, int logical_ts,
                                  kos_kb_source_t source) {
    if (!kb || !chain) return -1;
    int idx = (int)kb->item_count;
    kos_term* current = chain;
    while (current && current->kind == KOS_PAIR) {
        kos_term* fact = current->data.pair.data;
        kos_term* rest = current->data.pair.proof;
        if (fact && fact->kind == KOS_PAIR && fact->data.pair.data && fact->data.pair.proof) {
            char idbuf[64];
            snprintf(idbuf, sizeof(idbuf), "kb_%d", idx++);
            kos_kb_add_item(kb, idbuf, fact->data.pair.data, fact->data.pair.proof,
                            logical_ts, source);
        }
        current = rest;
    }
    return 0;
}
