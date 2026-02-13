// Phase 5: API Layer & Integration Demo
// API 层与集成演示程序
#include "kos_api.h"
#include "kos_core.h"
#include "kos_kernel.h"
#include "kos_ontology_registry.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    printf("=== KOS-TL Phase 5: API Layer & Integration Demo ===\n\n");
    
    kos_ontology_registry_t* registry = NULL;
    
    // 1. 创建 API 服务器
    uint16_t port = 8080;
    kos_api_server_t* server = kos_api_server_create(port);
    if (!server) {
        fprintf(stderr, "Failed to create API server\n");
        return 1;
    }
    
    printf("Created API server on port %d\n", port);
    
    // 2. 注册 REST API 路由
    
    // 本体类型管理
    kos_route_t get_types_route = {
        .method = KOS_HTTP_GET,
        .path_pattern = "/api/v1/ontology/types",
        .handler = kos_api_get_types,
        .user_data = NULL
    };
    kos_api_register_route(server, &get_types_route);
    
    kos_route_t create_type_route = {
        .method = KOS_HTTP_POST,
        .path_pattern = "/api/v1/ontology/types",
        .handler = kos_api_create_type,
        .user_data = NULL
    };
    kos_api_register_route(server, &create_type_route);
    
    kos_route_t update_type_route = {
        .method = KOS_HTTP_PUT,
        .path_pattern = "/api/v1/ontology/types",
        .handler = kos_api_update_type,
        .user_data = NULL
    };
    kos_api_register_route(server, &update_type_route);
    
    kos_route_t delete_type_route = {
        .method = KOS_HTTP_DELETE,
        .path_pattern = "/api/v1/ontology/types",
        .handler = kos_api_delete_type,
        .user_data = NULL
    };
    kos_api_register_route(server, &delete_type_route);
    
    // 实例管理
    kos_route_t get_instances_route = {
        .method = KOS_HTTP_GET,
        .path_pattern = "/api/v1/instances",
        .handler = kos_api_get_instances,
        .user_data = NULL
    };
    kos_api_register_route(server, &get_instances_route);
    
    kos_route_t create_instance_route = {
        .method = KOS_HTTP_POST,
        .path_pattern = "/api/v1/instances",
        .handler = kos_api_create_instance,
        .user_data = NULL
    };
    kos_api_register_route(server, &create_instance_route);
    
    kos_route_t update_instance_route = {
        .method = KOS_HTTP_PUT,
        .path_pattern = "/api/v1/instances",
        .handler = kos_api_update_instance,
        .user_data = NULL
    };
    kos_api_register_route(server, &update_instance_route);
    
    kos_route_t delete_instance_route = {
        .method = KOS_HTTP_DELETE,
        .path_pattern = "/api/v1/instances",
        .handler = kos_api_delete_instance,
        .user_data = NULL
    };
    kos_api_register_route(server, &delete_instance_route);
    
    // 查询执行
    kos_route_t execute_query_route = {
        .method = KOS_HTTP_POST,
        .path_pattern = "/api/v1/query",
        .handler = kos_api_execute_query,
        .user_data = NULL
    };
    kos_api_register_route(server, &execute_query_route);
    
    kos_route_t get_query_result_route = {
        .method = KOS_HTTP_GET,
        .path_pattern = "/api/v1/query",
        .handler = kos_api_get_query_result,
        .user_data = NULL
    };
    kos_api_register_route(server, &get_query_result_route);
    
    // 多本体与依存关系（在线可视化）：需要注册表，user_data 传 registry
    registry = kos_ontology_registry_create("./ontology_data");
    if (registry) {
        (void)kos_ontology_registry_load_all(registry);  /* 可选：从目录加载已有本体 */
        kos_route_t list_ontologies_route = {
            .method = KOS_HTTP_GET,
            .path_pattern = "/api/v1/ontologies",
            .handler = kos_api_list_ontologies,
            .user_data = (void*)registry
        };
        kos_route_t get_deps_route = {
            .method = KOS_HTTP_GET,
            .path_pattern = "/api/v1/ontologies/dependencies",
            .handler = kos_api_get_ontology_dependencies,
            .user_data = (void*)registry
        };
        kos_route_t get_deps_html_route = {
            .method = KOS_HTTP_GET,
            .path_pattern = "/api/v1/ontologies/dependencies/html",
            .handler = kos_api_get_ontology_dependencies_html,
            .user_data = (void*)registry
        };
        kos_api_register_route(server, &list_ontologies_route);
        kos_api_register_route(server, &get_deps_route);
        kos_api_register_route(server, &get_deps_html_route);
        /* 注意：server 不拥有 registry，演示结束前需 kos_ontology_registry_free(registry) */
    }
    
    // 注意：route_count 是内部字段，这里简化处理
    printf("Registered routes successfully\n");
    printf("\nRegistered routes:\n");
    printf("  GET    /api/v1/ontology/types\n");
    printf("  POST   /api/v1/ontology/types\n");
    printf("  PUT    /api/v1/ontology/types\n");
    printf("  DELETE /api/v1/ontology/types\n");
    printf("  GET    /api/v1/instances\n");
    printf("  POST   /api/v1/instances\n");
    printf("  PUT    /api/v1/instances\n");
    printf("  DELETE /api/v1/instances\n");
    printf("  POST   /api/v1/query\n");
    printf("  GET    /api/v1/query\n");
    printf("  GET    /api/v1/ontologies (list ontology ids)\n");
    printf("  GET    /api/v1/ontologies/dependencies?id=xxx (dependency JSON)\n");
    printf("  GET    /api/v1/ontologies/dependencies/html?id=xxx (dependency HTML)\n");
    
    printf("\n=== API Server Configuration ===\n");
    printf("Server will start and listen on port %d\n", port);
    printf("Note: This is a simplified demo. In production, use a proper HTTP library.\n");
    printf("\nTo test the API, use curl or similar tools:\n");
    printf("  curl http://localhost:%d/api/v1/ontology/types\n", port);
    printf("  curl -X POST http://localhost:%d/api/v1/ontology/types -H 'Content-Type: application/json' -d '{\"name\":\"TestType\"}'\n", port);
    
    // 注意：实际启动服务器会阻塞，这里只是演示配置
    // 在实际应用中，应该在单独的线程中启动服务器
    if (registry) {
        kos_ontology_registry_free(registry);
        registry = NULL;
    }
    if (server) {
        kos_api_server_free(server);
    }
    printf("\n=== Demo Completed ===\n");
    printf("Note: Server start/stop functionality is implemented but not executed in this demo.\n");
    printf("To start the server, call: kos_api_server_start(server)\n");
    
    return 0;
}
