#include "kos_api.h"
#include "kos_core.h"
#include "kos_kernel.h"
#include "kos_query.h"
#include "kos_ontology_version.h"
#include "kos_ontology_registry.h"
#include "kos_visualization.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>

/* 从 query_string 解析 key= 的值，返回新分配字符串，调用方 free；未找到返回 NULL */
static char* query_param(const char* query_string, const char* key) {
    if (!query_string || !key) return NULL;
    size_t key_len = strlen(key);
    const char* p = query_string;
    for (;;) {
        p = strstr(p, key);
        if (!p) return NULL;
        if ((p == query_string || p[-1] == '&') && p[key_len] == '=') {
            p += key_len + 1;
            const char* end = strchr(p, '&');
            size_t len = end ? (size_t)(end - p) : strlen(p);
            char* val = (char*)malloc(len + 1);
            if (!val) return NULL;
            memcpy(val, p, len);
            val[len] = '\0';
            return val;
        }
        p++;
    }
}

// ========== 辅助函数：创建 JSON 响应 ==========

static char* create_json_response(const char* format, ...) {
    char buffer[4096];
    va_list args;
    va_start(args, format);
    vsnprintf(buffer, sizeof(buffer), format, args);
    va_end(args);
    return strdup(buffer);
}

static char* create_error_response(const char* error_message) {
    return create_json_response("{\"error\":\"%s\"}", error_message);
}

static char* create_success_response(const char* data) {
    return create_json_response("{\"success\":true,\"data\":%s}", data);
}

// ========== 本体类型管理端点 ==========

kos_http_response_t* kos_api_get_types(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
) {
    kos_http_response_t* response = kos_http_response_create(200, "application/json");
    
    // 简化实现：返回空数组
    // 实际应该从 kernel_state 或本体管理器获取类型列表
    char* json = create_json_response("{\"types\":[]}");
    kos_http_response_set_json_body(response, json);
    free(json);
    
    return response;
}

kos_http_response_t* kos_api_create_type(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
) {
    if (!request->body) {
        kos_http_response_t* response = kos_http_response_create(400, "application/json");
        char* json = create_error_response("Request body required");
        kos_http_response_set_json_body(response, json);
        free(json);
        return response;
    }
    
    // 简化实现：解析 JSON 并创建类型
    // 实际应该使用 kos_term_deserialize 解析，然后使用本体管理器创建类型
    
    kos_http_response_t* response = kos_http_response_create(201, "application/json");
    char* json = create_json_response("{\"message\":\"Type created\",\"id\":\"new_type\"}");
    kos_http_response_set_json_body(response, json);
    free(json);
    
    return response;
}

kos_http_response_t* kos_api_update_type(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
) {
    // 从路径中提取类型名
    // 简化实现：假设路径格式为 /api/v1/ontology/types/{name}
    const char* path = request->path;
    const char* name_start = strrchr(path, '/');
    if (!name_start) {
        kos_http_response_t* response = kos_http_response_create(400, "application/json");
        char* json = create_error_response("Invalid path");
        kos_http_response_set_json_body(response, json);
        free(json);
        return response;
    }
    name_start++; // 跳过 '/'
    
    if (!request->body) {
        kos_http_response_t* response = kos_http_response_create(400, "application/json");
        char* json = create_error_response("Request body required");
        kos_http_response_set_json_body(response, json);
        free(json);
        return response;
    }
    
    // 简化实现：更新类型
    kos_http_response_t* response = kos_http_response_create(200, "application/json");
    char* json = create_json_response("{\"message\":\"Type updated\",\"name\":\"%s\"}", name_start);
    kos_http_response_set_json_body(response, json);
    free(json);
    
    return response;
}

kos_http_response_t* kos_api_delete_type(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
) {
    // 从路径中提取类型名
    const char* path = request->path;
    const char* name_start = strrchr(path, '/');
    if (!name_start) {
        kos_http_response_t* response = kos_http_response_create(400, "application/json");
        char* json = create_error_response("Invalid path");
        kos_http_response_set_json_body(response, json);
        free(json);
        return response;
    }
    name_start++;
    
    // 简化实现：删除类型
    kos_http_response_t* response = kos_http_response_create(204, "application/json");
    // 204 No Content 通常不返回响应体
    
    return response;
}

// ========== 实例管理端点 ==========

kos_http_response_t* kos_api_get_instances(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
) {
    // 解析查询参数
    const char* type_param = NULL;
    const char* query_param = NULL;
    
    if (request->query_string) {
        // 简化解析：查找 type= 和 query=
        char* query_copy = strdup(request->query_string);
        char* token = strtok(query_copy, "&");
        while (token) {
            if (strncmp(token, "type=", 5) == 0) {
                type_param = token + 5;
            } else if (strncmp(token, "query=", 6) == 0) {
                query_param = token + 6;
            }
            token = strtok(NULL, "&");
        }
        free(query_copy);
    }
    
    // 简化实现：返回空数组
    kos_http_response_t* response = kos_http_response_create(200, "application/json");
    char* json = create_json_response("{\"instances\":[]}");
    kos_http_response_set_json_body(response, json);
    free(json);
    
    return response;
}

kos_http_response_t* kos_api_create_instance(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
) {
    if (!request->body) {
        kos_http_response_t* response = kos_http_response_create(400, "application/json");
        char* json = create_error_response("Request body required");
        kos_http_response_set_json_body(response, json);
        free(json);
        return response;
    }
    
    // 简化实现：创建实例
    kos_http_response_t* response = kos_http_response_create(201, "application/json");
    char* json = create_json_response("{\"message\":\"Instance created\",\"id\":\"new_instance\"}");
    kos_http_response_set_json_body(response, json);
    free(json);
    
    return response;
}

kos_http_response_t* kos_api_update_instance(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
) {
    // 从路径中提取实例ID
    const char* path = request->path;
    const char* id_start = strrchr(path, '/');
    if (!id_start) {
        kos_http_response_t* response = kos_http_response_create(400, "application/json");
        char* json = create_error_response("Invalid path");
        kos_http_response_set_json_body(response, json);
        free(json);
        return response;
    }
    id_start++;
    
    if (!request->body) {
        kos_http_response_t* response = kos_http_response_create(400, "application/json");
        char* json = create_error_response("Request body required");
        kos_http_response_set_json_body(response, json);
        free(json);
        return response;
    }
    
    // 简化实现：更新实例
    kos_http_response_t* response = kos_http_response_create(200, "application/json");
    char* json = create_json_response("{\"message\":\"Instance updated\",\"id\":\"%s\"}", id_start);
    kos_http_response_set_json_body(response, json);
    free(json);
    
    return response;
}

kos_http_response_t* kos_api_delete_instance(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
) {
    // 从路径中提取实例ID
    const char* path = request->path;
    const char* id_start = strrchr(path, '/');
    if (!id_start) {
        kos_http_response_t* response = kos_http_response_create(400, "application/json");
        char* json = create_error_response("Invalid path");
        kos_http_response_set_json_body(response, json);
        free(json);
        return response;
    }
    id_start++;
    
    // 简化实现：删除实例
    kos_http_response_t* response = kos_http_response_create(204, "application/json");
    
    return response;
}

// ========== 查询执行端点 ==========

kos_http_response_t* kos_api_execute_query(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
) {
    if (!request->body) {
        kos_http_response_t* response = kos_http_response_create(400, "application/json");
        char* json = create_error_response("Request body required");
        kos_http_response_set_json_body(response, json);
        free(json);
        return response;
    }
    
    // 解析查询请求（JSON格式）
    // 简化实现：假设 body 包含 {"query": "SELECT ...", "type": "..."}
    
    // 执行查询
    // 实际应该使用 kos_query_parse 和 kos_query_execute
    
    kos_http_response_t* response = kos_http_response_create(200, "application/json");
    char* json = create_json_response(
        "{\"query_id\":\"query_123\",\"status\":\"executing\",\"message\":\"Query submitted\"}"
    );
    kos_http_response_set_json_body(response, json);
    free(json);
    
    return response;
}

kos_http_response_t* kos_api_get_query_result(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
) {
    // 从路径中提取查询ID
    // 路径格式：/api/v1/query/{id}/result
    const char* path = request->path;
    const char* id_start = strstr(path, "/query/");
    if (!id_start) {
        kos_http_response_t* response = kos_http_response_create(400, "application/json");
        char* json = create_error_response("Invalid path");
        kos_http_response_set_json_body(response, json);
        free(json);
        return response;
    }
    id_start += 7; // 跳过 "/query/"
    
    // 提取查询ID（到下一个 '/' 或字符串结尾）
    char query_id[256];
    const char* id_end = strchr(id_start, '/');
    if (id_end) {
        size_t len = id_end - id_start;
        strncpy(query_id, id_start, len);
        query_id[len] = '\0';
    } else {
        strncpy(query_id, id_start, sizeof(query_id) - 1);
        query_id[sizeof(query_id) - 1] = '\0';
    }
    
    // 简化实现：返回查询结果
    kos_http_response_t* response = kos_http_response_create(200, "application/json");
    char* json = create_json_response(
        "{\"query_id\":\"%s\",\"status\":\"completed\",\"results\":[]}",
        query_id
    );
    kos_http_response_set_json_body(response, json);
    free(json);
    
    return response;
}

// ========== 多本体与依存关系（在线可视化） ==========

kos_http_response_t* kos_api_list_ontologies(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
) {
    kos_ontology_registry_t* reg = (kos_ontology_registry_t*)user_data;
    kos_http_response_t* response = kos_http_response_create(200, "application/json");
    if (!reg) {
        kos_http_response_set_json_body(response, "{\"ids\":[]}");
        return response;
    }
    char** ids = NULL;
    size_t n = kos_ontology_registry_list(reg, &ids);
    if (n == 0) {
        kos_http_response_set_json_body(response, "{\"ids\":[]}");
        return response;
    }
    size_t buf_size = 256;
    char* buf = (char*)malloc(buf_size);
    if (!buf) {
        for (size_t i = 0; i < n; i++) free(ids[i]);
        free(ids);
        kos_http_response_set_json_body(response, "{\"ids\":[]}");
        return response;
    }
    int pos = snprintf(buf, buf_size, "{\"ids\":[");
    for (size_t i = 0; i < n; i++) {
        size_t need = pos + strlen(ids[i]) + 8;
        if (need >= buf_size) {
            buf_size = need + 256;
            char* t = (char*)realloc(buf, buf_size);
            if (!t) break;
            buf = t;
        }
        pos += snprintf(buf + pos, buf_size - pos, "%s\"%s\"", i ? "," : "", ids[i]);
        free(ids[i]);
    }
    free(ids);
    pos += snprintf(buf + pos, buf_size - pos, "]}");
    kos_http_response_set_body(response, buf, (size_t)pos + 1);
    free(buf);
    return response;
}

kos_http_response_t* kos_api_get_ontology_dependencies(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
) {
    kos_ontology_registry_t* reg = (kos_ontology_registry_t*)user_data;
    kos_http_response_t* response = kos_http_response_create(200, "application/json");
    if (!reg) {
        kos_http_response_set_json_body(response, "{\"error\":\"No registry\"}");
        return response;
    }
    char* id = query_param(request->query_string ? request->query_string : "", "id");
    if (!id) {
        kos_http_response_set_json_body(response, "{\"error\":\"Missing id\"}");
        return response;
    }
    TypeOntology* ontology = kos_ontology_registry_get(reg, id);
    free(id);
    if (!ontology) {
        kos_http_response_set_json_body(response, "{\"error\":\"Ontology not found\"}");
        return response;
    }
    TypeDependencyGraph* graph = kos_visualization_extract_dependencies(ontology);
    if (!graph) {
        kos_http_response_set_json_body(response, "{\"nodes\":[]}");
        return response;
    }
    char* json = kos_visualization_generate_json(graph);
    kos_visualization_free_graph(graph);
    if (!json) {
        kos_http_response_set_json_body(response, "{\"nodes\":[]}");
        return response;
    }
    kos_http_response_set_body(response, json, strlen(json) + 1);
    free(json);
    return response;
}

kos_http_response_t* kos_api_get_ontology_dependencies_html(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
) {
    kos_ontology_registry_t* reg = (kos_ontology_registry_t*)user_data;
    kos_http_response_t* response = kos_http_response_create(200, "text/html; charset=utf-8");
    if (!reg) {
        kos_http_response_set_body(response, "<html><body><p>No registry</p></body></html>", 52);
        return response;
    }
    char* id = query_param(request->query_string ? request->query_string : "", "id");
    if (!id) {
        kos_http_response_set_body(response, "<html><body><p>Missing id</p></body></html>", 48);
        return response;
    }
    TypeOntology* ontology = kos_ontology_registry_get(reg, id);
    free(id);
    if (!ontology) {
        kos_http_response_set_body(response, "<html><body><p>Ontology not found</p></body></html>", 52);
        return response;
    }
    TypeDependencyGraph* graph = kos_visualization_extract_dependencies(ontology);
    if (!graph) {
        kos_http_response_set_body(response, "<html><body><p>No dependencies</p></body></html>", 54);
        return response;
    }
    char title[128];
    snprintf(title, sizeof(title), "Ontology Dependencies: %s", ontology->domain_name);
    char* html = kos_visualization_generate_html(graph, title);
    kos_visualization_free_graph(graph);
    if (!html) {
        kos_http_response_set_body(response, "<html><body><p>Failed to generate HTML</p></body></html>", 60);
        return response;
    }
    kos_http_response_set_body(response, html, strlen(html) + 1);
    free(html);
    return response;
}
