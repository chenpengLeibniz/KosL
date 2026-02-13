// Phase 5: API Layer & Integration
// API 层与集成框架 - 对标 Palantir API 能力
#ifndef KOS_API_H
#define KOS_API_H

#include "kos_core.h"
#include "kos_kernel.h"
#include "kos_query.h"
#include "kos_ontology_version.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// ========== HTTP 方法 ==========
typedef enum {
    KOS_HTTP_GET,
    KOS_HTTP_POST,
    KOS_HTTP_PUT,
    KOS_HTTP_DELETE,
    KOS_HTTP_PATCH,
    KOS_HTTP_OPTIONS
} kos_http_method_t;

// ========== HTTP 请求 ==========
typedef struct {
    kos_http_method_t method;
    char* path;                    // 请求路径
    char* query_string;             // 查询字符串
    char* body;                     // 请求体
    size_t body_length;             // 请求体长度
    char* content_type;             // Content-Type
    char* authorization;            // Authorization header
    // 简化的头部映射（实际应该使用哈希表）
    char** headers;                 // 其他头部
    size_t header_count;
} kos_http_request_t;

// ========== HTTP 响应 ==========
typedef struct {
    int status_code;                // HTTP 状态码 (200, 404, 500等)
    char* content_type;              // Content-Type
    char* body;                      // 响应体
    size_t body_length;              // 响应体长度
    char** headers;                  // 额外的响应头
    size_t header_count;
} kos_http_response_t;

// ========== API 服务器 ==========
typedef struct kos_api_server kos_api_server_t;

// ========== 路由处理器 ==========
typedef kos_http_response_t* (*kos_route_handler_t)(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
);

// ========== 路由定义 ==========
typedef struct {
    kos_http_method_t method;
    const char* path_pattern;       // 路径模式（支持简单通配符）
    kos_route_handler_t handler;
    void* user_data;
} kos_route_t;

// ========== REST API 核心接口 ==========

// --- 服务器生命周期 ---
kos_api_server_t* kos_api_server_create(uint16_t port);
void kos_api_server_free(kos_api_server_t* server);

// --- 路由注册 ---
int kos_api_register_route(
    kos_api_server_t* server,
    const kos_route_t* route
);

// --- 服务器运行 ---
int kos_api_server_start(kos_api_server_t* server);
int kos_api_server_stop(kos_api_server_t* server);
bool kos_api_server_is_running(kos_api_server_t* server);

// --- 请求/响应处理 ---
kos_http_request_t* kos_http_request_create(void);
void kos_http_request_free(kos_http_request_t* request);

kos_http_response_t* kos_http_response_create(int status_code, const char* content_type);
void kos_http_response_free(kos_http_response_t* response);

int kos_http_response_set_body(
    kos_http_response_t* response,
    const char* body,
    size_t length
);

int kos_http_response_set_json_body(
    kos_http_response_t* response,
    const char* json_string
);

// ========== REST API 端点处理器 ==========

// --- 本体类型管理 ---
kos_http_response_t* kos_api_get_types(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
);

kos_http_response_t* kos_api_create_type(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
);

kos_http_response_t* kos_api_update_type(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
);

kos_http_response_t* kos_api_delete_type(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
);

// --- 实例管理 ---
kos_http_response_t* kos_api_get_instances(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
);

kos_http_response_t* kos_api_create_instance(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
);

kos_http_response_t* kos_api_update_instance(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
);

kos_http_response_t* kos_api_delete_instance(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
);

// --- 多本体与依存关系（在线可视化） ---
// GET /api/v1/ontologies：列举本体 id 列表；user_data = kos_ontology_registry_t*
kos_http_response_t* kos_api_list_ontologies(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
);
// GET /api/v1/ontologies/dependencies?id=xxx：返回指定本体的依存关系 JSON；user_data = kos_ontology_registry_t*
kos_http_response_t* kos_api_get_ontology_dependencies(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
);
// GET /api/v1/ontologies/dependencies/html?id=xxx：返回指定本体的依存关系 HTML 页面；user_data = kos_ontology_registry_t*
kos_http_response_t* kos_api_get_ontology_dependencies_html(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
);

// --- 查询执行 ---
kos_http_response_t* kos_api_execute_query(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
);

kos_http_response_t* kos_api_get_query_result(
    kos_api_server_t* server,
    const kos_http_request_t* request,
    void* user_data
);

// ========== GraphQL API ==========

// GraphQL 请求
typedef struct {
    char* query;                    // GraphQL 查询字符串
    char* variables;               // JSON 格式的变量
    char* operation_name;           // 操作名称
} kos_graphql_request_t;

// GraphQL 响应
typedef struct {
    char* data;                     // JSON 格式的响应数据
    char* errors;                   // JSON 格式的错误（如果有）
} kos_graphql_response_t;

// GraphQL Schema
typedef struct kos_graphql_schema kos_graphql_schema_t;

// GraphQL API
kos_graphql_schema_t* kos_graphql_schema_create_from_ontology(kos_term* ontology);
void kos_graphql_schema_free(kos_graphql_schema_t* schema);

kos_graphql_response_t* kos_graphql_execute(
    kos_graphql_schema_t* schema,
    const kos_graphql_request_t* request,
    kos_term* knowledge_set
);

void kos_graphql_response_free(kos_graphql_response_t* response);

// ========== WebSocket API ==========

// WebSocket 连接
typedef struct kos_websocket_connection kos_websocket_connection_t;

// WebSocket 消息类型
typedef enum {
    KOS_WS_MESSAGE_TEXT,
    KOS_WS_MESSAGE_BINARY,
    KOS_WS_MESSAGE_PING,
    KOS_WS_MESSAGE_PONG,
    KOS_WS_MESSAGE_CLOSE
} kos_ws_message_type_t;

// WebSocket 消息
typedef struct {
    kos_ws_message_type_t type;
    char* data;
    size_t length;
} kos_ws_message_t;

// WebSocket 事件回调
typedef void (*kos_ws_on_connect_t)(kos_websocket_connection_t* conn, void* user_data);
typedef void (*kos_ws_on_message_t)(kos_websocket_connection_t* conn, const kos_ws_message_t* msg, void* user_data);
typedef void (*kos_ws_on_disconnect_t)(kos_websocket_connection_t* conn, void* user_data);

// WebSocket 服务器
typedef struct kos_websocket_server kos_websocket_server_t;

kos_websocket_server_t* kos_websocket_server_create(uint16_t port);
void kos_websocket_server_free(kos_websocket_server_t* server);

int kos_websocket_server_set_callbacks(
    kos_websocket_server_t* server,
    kos_ws_on_connect_t on_connect,
    kos_ws_on_message_t on_message,
    kos_ws_on_disconnect_t on_disconnect,
    void* user_data
);

int kos_websocket_server_start(kos_websocket_server_t* server);
int kos_websocket_server_stop(kos_websocket_server_t* server);

// WebSocket 连接操作
int kos_websocket_send(
    kos_websocket_connection_t* conn,
    const kos_ws_message_t* message
);

int kos_websocket_broadcast(
    kos_websocket_server_t* server,
    const kos_ws_message_t* message
);

// ========== 辅助函数 ==========

// JSON 辅助函数
char* kos_json_encode_term(kos_term* term);
kos_term* kos_json_decode_term(const char* json_string);

// 路径解析
int kos_parse_path_params(
    const char* path_pattern,
    const char* actual_path,
    char*** param_names,
    char*** param_values,
    size_t* param_count
);

// 查询字符串解析
int kos_parse_query_string(
    const char* query_string,
    char*** keys,
    char*** values,
    size_t* count
);

#endif // KOS_API_H
