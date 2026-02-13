#include "kos_api.h"
#include "kos_core.h"
#include "kos_kernel.h"
#include "kos_query.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>

#ifdef _WIN32
typedef int ssize_t;
#endif

#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
#pragma comment(lib, "ws2_32.lib")
#define close closesocket
#define sleep(x) Sleep(x * 1000)
#else
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <pthread.h>
#endif

// ========== 内部数据结构 ==========

struct kos_api_server {
    uint16_t port;
    int socket_fd;
    bool is_running;
    
    // 路由表
    kos_route_t* routes;
    size_t route_count;
    size_t route_capacity;
    
    // 关联的知识集和状态
    kos_state_t* kernel_state;
};

// ========== HTTP 请求解析 ==========

static kos_http_method_t parse_http_method(const char* method_str) {
    if (strcmp(method_str, "GET") == 0) return KOS_HTTP_GET;
    if (strcmp(method_str, "POST") == 0) return KOS_HTTP_POST;
    if (strcmp(method_str, "PUT") == 0) return KOS_HTTP_PUT;
    if (strcmp(method_str, "DELETE") == 0) return KOS_HTTP_DELETE;
    if (strcmp(method_str, "PATCH") == 0) return KOS_HTTP_PATCH;
    if (strcmp(method_str, "OPTIONS") == 0) return KOS_HTTP_OPTIONS;
    return KOS_HTTP_GET;
}

static const char* http_method_to_string(kos_http_method_t method) {
    switch (method) {
        case KOS_HTTP_GET: return "GET";
        case KOS_HTTP_POST: return "POST";
        case KOS_HTTP_PUT: return "PUT";
        case KOS_HTTP_DELETE: return "DELETE";
        case KOS_HTTP_PATCH: return "PATCH";
        case KOS_HTTP_OPTIONS: return "OPTIONS";
        default: return "GET";
    }
}

static int parse_http_request(const char* raw_request, kos_http_request_t* request) {
    if (!raw_request || !request) {
        return -1;
    }
    
    // 简化解析：解析第一行（方法、路径、HTTP版本）
    char method[16], path[1024], version[16];
    if (sscanf(raw_request, "%15s %1023s %15s", method, path, version) != 3) {
        return -1;
    }
    
    request->method = parse_http_method(method);
    
    // 分离路径和查询字符串
    char* query_start = strchr(path, '?');
    if (query_start) {
        *query_start = '\0';
        request->query_string = strdup(query_start + 1);
    } else {
        request->query_string = NULL;
    }
    request->path = strdup(path);
    
    // 解析头部和请求体（简化实现）
    const char* body_start = strstr(raw_request, "\r\n\r\n");
    if (body_start) {
        body_start += 4;
        size_t body_len = strlen(body_start);
        if (body_len > 0) {
            request->body = (char*)malloc(body_len + 1);
            strcpy(request->body, body_start);
            request->body_length = body_len;
        }
    }
    
    // 解析 Content-Type
    const char* content_type_start = strstr(raw_request, "Content-Type:");
    if (content_type_start) {
        content_type_start += 13;
        while (*content_type_start == ' ') content_type_start++;
        const char* content_type_end = strstr(content_type_start, "\r\n");
        if (content_type_end) {
            size_t len = content_type_end - content_type_start;
            request->content_type = (char*)malloc(len + 1);
            strncpy(request->content_type, content_type_start, len);
            request->content_type[len] = '\0';
        }
    }
    
    // 解析 Authorization
    const char* auth_start = strstr(raw_request, "Authorization:");
    if (auth_start) {
        auth_start += 13;
        while (*auth_start == ' ') auth_start++;
        const char* auth_end = strstr(auth_start, "\r\n");
        if (auth_end) {
            size_t len = auth_end - auth_start;
            request->authorization = (char*)malloc(len + 1);
            strncpy(request->authorization, auth_start, len);
            request->authorization[len] = '\0';
        }
    }
    
    request->headers = NULL;
    request->header_count = 0;
    
    return 0;
}

// ========== HTTP 响应构建 ==========

static int build_http_response(const kos_http_response_t* response, char* buffer, size_t buffer_size) {
    if (!response || !buffer) {
        return -1;
    }
    
    int offset = 0;
    
    // 状态行
    const char* status_text = "OK";
    if (response->status_code == 404) status_text = "Not Found";
    else if (response->status_code == 400) status_text = "Bad Request";
    else if (response->status_code == 500) status_text = "Internal Server Error";
    else if (response->status_code == 201) status_text = "Created";
    else if (response->status_code == 204) status_text = "No Content";
    
    offset += snprintf(buffer + offset, buffer_size - offset,
        "HTTP/1.1 %d %s\r\n", response->status_code, status_text);
    
    // Content-Type
    if (response->content_type) {
        offset += snprintf(buffer + offset, buffer_size - offset,
            "Content-Type: %s\r\n", response->content_type);
    } else {
        offset += snprintf(buffer + offset, buffer_size - offset,
            "Content-Type: application/json\r\n");
    }
    
    // Content-Length
    if (response->body) {
        offset += snprintf(buffer + offset, buffer_size - offset,
            "Content-Length: %zu\r\n", response->body_length);
    }
    
    // CORS 头部（简化实现）
    offset += snprintf(buffer + offset, buffer_size - offset,
        "Access-Control-Allow-Origin: *\r\n");
    offset += snprintf(buffer + offset, buffer_size - offset,
        "Access-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS\r\n");
    offset += snprintf(buffer + offset, buffer_size - offset,
        "Access-Control-Allow-Headers: Content-Type, Authorization\r\n");
    
    // 额外的头部
    for (size_t i = 0; i < response->header_count && response->headers; i++) {
        offset += snprintf(buffer + offset, buffer_size - offset,
            "%s\r\n", response->headers[i]);
    }
    
    // 空行
    offset += snprintf(buffer + offset, buffer_size - offset, "\r\n");
    
    // 响应体
    if (response->body) {
        memcpy(buffer + offset, response->body, response->body_length);
        offset += response->body_length;
    }
    
    return offset;
}

// ========== 路由匹配 ==========

static bool match_route(const kos_route_t* route, const kos_http_request_t* request) {
    if (!route || !request) {
        return false;
    }
    
    // 方法匹配
    if (route->method != request->method) {
        return false;
    }
    
    // 路径匹配（简化实现：精确匹配）
    // 实际应该支持路径参数和通配符
    return strcmp(route->path_pattern, request->path) == 0;
}

// ========== 服务器实现 ==========

kos_api_server_t* kos_api_server_create(uint16_t port) {
    kos_api_server_t* server = (kos_api_server_t*)calloc(1, sizeof(kos_api_server_t));
    if (!server) {
        return NULL;
    }
    
    server->port = port;
    server->socket_fd = -1;
    server->is_running = false;
    server->route_capacity = 16;
    server->routes = (kos_route_t*)calloc(server->route_capacity, sizeof(kos_route_t));
    server->kernel_state = NULL;
    
    if (!server->routes) {
        free(server);
        return NULL;
    }
    
    return server;
}

void kos_api_server_free(kos_api_server_t* server) {
    if (!server) {
        return;
    }
    
    if (server->is_running) {
        kos_api_server_stop(server);
    }
    
    if (server->routes) {
        free(server->routes);
    }
    
    if (server->kernel_state) {
        kos_state_free(server->kernel_state);
    }
    
    free(server);
}

int kos_api_register_route(
    kos_api_server_t* server,
    const kos_route_t* route
) {
    if (!server || !route) {
        return -1;
    }
    
    // 扩展路由数组（如果需要）
    if (server->route_count >= server->route_capacity) {
        size_t new_capacity = server->route_capacity * 2;
        kos_route_t* new_routes = (kos_route_t*)realloc(
            server->routes,
            new_capacity * sizeof(kos_route_t)
        );
        if (!new_routes) {
            return -1;
        }
        server->routes = new_routes;
        server->route_capacity = new_capacity;
    }
    
    // 添加路由
    server->routes[server->route_count] = *route;
    server->route_count++;
    
    return 0;
}

// 处理单个客户端连接
static void handle_client_connection(kos_api_server_t* server, int client_fd) {
    char buffer[8192];
    ssize_t bytes_received = recv(client_fd, buffer, sizeof(buffer) - 1, 0);
    
    if (bytes_received <= 0) {
        close(client_fd);
        return;
    }
    
    buffer[bytes_received] = '\0';
    
    // 解析请求
    kos_http_request_t* request = kos_http_request_create();
    if (parse_http_request(buffer, request) != 0) {
        kos_http_response_t* error_response = kos_http_response_create(400, "application/json");
        kos_http_response_set_json_body(error_response, "{\"error\":\"Invalid request\"}");
        
        char response_buffer[4096];
        int response_len = build_http_response(error_response, response_buffer, sizeof(response_buffer));
        send(client_fd, response_buffer, response_len, 0);
        
        kos_http_request_free(request);
        kos_http_response_free(error_response);
        close(client_fd);
        return;
    }
    
    // 查找匹配的路由
    kos_http_response_t* response = NULL;
    for (size_t i = 0; i < server->route_count; i++) {
        if (match_route(&server->routes[i], request)) {
            response = server->routes[i].handler(server, request, server->routes[i].user_data);
            break;
        }
    }
    
    // 如果没有匹配的路由，返回 404
    if (!response) {
        response = kos_http_response_create(404, "application/json");
        kos_http_response_set_json_body(response, "{\"error\":\"Not found\"}");
    }
    
    // 构建并发送响应
    char response_buffer[8192];
    int response_len = build_http_response(response, response_buffer, sizeof(response_buffer));
    send(client_fd, response_buffer, response_len, 0);
    
    kos_http_request_free(request);
    if (response) {
        kos_http_response_free(response);
    }
    
    close(client_fd);
}

int kos_api_server_start(kos_api_server_t* server) {
    if (!server || server->is_running) {
        return -1;
    }
    
#ifdef _WIN32
    WSADATA wsa_data;
    if (WSAStartup(MAKEWORD(2, 2), &wsa_data) != 0) {
        return -1;
    }
#endif
    
    // 创建 socket
    server->socket_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (server->socket_fd < 0) {
        return -1;
    }
    
    // 设置 socket 选项
    int opt = 1;
    setsockopt(server->socket_fd, SOL_SOCKET, SO_REUSEADDR, (char*)&opt, sizeof(opt));
    
    // 绑定地址
    struct sockaddr_in address;
    memset(&address, 0, sizeof(address));
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = INADDR_ANY;
    address.sin_port = htons(server->port);
    
    if (bind(server->socket_fd, (struct sockaddr*)&address, sizeof(address)) < 0) {
        close(server->socket_fd);
        return -1;
    }
    
    // 监听
    if (listen(server->socket_fd, 10) < 0) {
        close(server->socket_fd);
        return -1;
    }
    
    server->is_running = true;
    
    // 接受连接循环（简化实现：单线程）
    // 实际应该使用多线程或事件循环
    printf("KOS API Server started on port %d\n", server->port);
    
    while (server->is_running) {
        struct sockaddr_in client_addr;
        socklen_t client_len = sizeof(client_addr);
        int client_fd = accept(server->socket_fd, (struct sockaddr*)&client_addr, &client_len);
        
        if (client_fd < 0) {
            continue;
        }
        
        // 处理客户端连接（简化：同步处理）
        // 实际应该使用线程池或异步IO
        handle_client_connection(server, client_fd);
    }
    
    return 0;
}

int kos_api_server_stop(kos_api_server_t* server) {
    if (!server || !server->is_running) {
        return -1;
    }
    
    server->is_running = false;
    
    if (server->socket_fd >= 0) {
        close(server->socket_fd);
        server->socket_fd = -1;
    }
    
#ifdef _WIN32
    WSACleanup();
#endif
    
    return 0;
}

bool kos_api_server_is_running(kos_api_server_t* server) {
    return server ? server->is_running : false;
}

// ========== HTTP 请求/响应创建 ==========

kos_http_request_t* kos_http_request_create(void) {
    kos_http_request_t* request = (kos_http_request_t*)calloc(1, sizeof(kos_http_request_t));
    return request;
}

void kos_http_request_free(kos_http_request_t* request) {
    if (!request) {
        return;
    }
    
    if (request->path) free(request->path);
    if (request->query_string) free(request->query_string);
    if (request->body) free(request->body);
    if (request->content_type) free(request->content_type);
    if (request->authorization) free(request->authorization);
    
    if (request->headers) {
        for (size_t i = 0; i < request->header_count; i++) {
            if (request->headers[i]) free(request->headers[i]);
        }
        free(request->headers);
    }
    
    free(request);
}

kos_http_response_t* kos_http_response_create(int status_code, const char* content_type) {
    kos_http_response_t* response = (kos_http_response_t*)calloc(1, sizeof(kos_http_response_t));
    if (!response) {
        return NULL;
    }
    
    response->status_code = status_code;
    if (content_type) {
        response->content_type = strdup(content_type);
    } else {
        response->content_type = strdup("application/json");
    }
    
    return response;
}

void kos_http_response_free(kos_http_response_t* response) {
    if (!response) {
        return;
    }
    
    if (response->content_type) free(response->content_type);
    if (response->body) free(response->body);
    
    if (response->headers) {
        for (size_t i = 0; i < response->header_count; i++) {
            if (response->headers[i]) free(response->headers[i]);
        }
        free(response->headers);
    }
    
    free(response);
}

int kos_http_response_set_body(
    kos_http_response_t* response,
    const char* body,
    size_t length
) {
    if (!response || !body) {
        return -1;
    }
    
    if (response->body) {
        free(response->body);
    }
    
    response->body = (char*)malloc(length + 1);
    if (!response->body) {
        return -1;
    }
    
    memcpy(response->body, body, length);
    response->body[length] = '\0';
    response->body_length = length;
    
    return 0;
}

int kos_http_response_set_json_body(
    kos_http_response_t* response,
    const char* json_string
) {
    if (!response || !json_string) {
        return -1;
    }
    
    response->content_type = strdup("application/json");
    return kos_http_response_set_body(response, json_string, strlen(json_string));
}
