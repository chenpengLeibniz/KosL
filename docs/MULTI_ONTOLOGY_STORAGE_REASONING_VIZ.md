# 大量动态本体的存储、实时推理与在线可视化

本文档说明在实际应用中，当构建大量动态本体时：**本体如何存储**、**如何进行实时推理**、**如何在线可视化展现依存关系**。

---

## 1. 本体如何存储

### 1.1 多本体注册表（Ontology Registry）

- **头文件**：`include/kos_ontology_registry.h`
- **实现**：`src/ontology/ontology_registry.c`

**能力**：

- **内存侧**：按 `id` 管理多个 `TypeOntology*`，支持注册、按 id 获取、注销、列举所有 id。
- **持久化**：创建注册表时可指定 `storage_dir`；每个本体以 `id.json` 存于该目录。
  - `kos_ontology_registry_save_all(reg)`：将当前内存中所有本体写入目录。
  - `kos_ontology_registry_load_all(reg)`：从目录扫描 `*.json`，按文件名（去掉 `.json`）作为 id 加载并注册。
  - `kos_ontology_registry_load_one(reg, id)`：仅加载 `id.json` 并注册（若已存在则替换）。

**使用示例**：

```c
kos_ontology_registry_t* reg = kos_ontology_registry_create("./ontology_data");
kos_ontology_registry_load_all(reg);   /* 启动时从目录加载 */

TypeOntology* o = kos_ontology_create("manufacturing");
/* ... 添加类型定义 ... */
kos_ontology_registry_register(reg, "manufacturing", o);
kos_ontology_registry_save_all(reg);  /* 定期或退出时持久化 */

char** ids = NULL;
size_t n = kos_ontology_registry_list(reg, &ids);
/* ... 使用 ids，最后 free(ids[i]) 和 free(ids) ... */
kos_ontology_registry_free(reg);
```

**冲突与扩展**：同一 `id` 再次 `register` 会覆盖旧本体（旧本体由注册表 free）。海量本体时，目录下文件数量与 id 数量一致；可按子目录或命名约定做分区。

---

## 2. 如何进行实时推理

### 2.1 推理会话（Reasoning Session）

- **头文件**：`include/kos_reasoning.h`
- **实现**：`src/ontology/reasoning_session.c`

**能力**：

- 会话绑定一个 **本体注册表**，按 **ontology_id** 维护各自的 kernel 状态（`kos_state_t*`）。
- **喂入事件**：`kos_reasoning_feed_event(session, ontology_id, raw_signal)`  
  对对应 id 的 state 做 `elab(raw_signal, state->K)`，将得到的事件对 enqueue 到该 state 的队列。
- **单步演化**：`kos_reasoning_tick(session, ontology_id)`  
  对该 id 的 state 执行一次 `kos_step(sigma)`（处理队列头一个事件）。
- **跑到空闲**：`kos_reasoning_run_until_idle(session, ontology_id, max_steps)`  
  对该 id 的 state 反复 `kos_step` 直到队列空或达到 `max_steps`。

**使用示例**：

```c
kos_ontology_registry_t* reg = kos_ontology_registry_create("./ontology_data");
kos_reasoning_session_t* session = kos_reasoning_session_create(reg);

/* 可选：为某 id 预先创建 state 并设置初始 K */
kos_reasoning_get_or_create_state(session, "manufacturing", initial_K);

/* 实时喂入原始信号 */
bitstream signal = { .data = (unsigned char*)"...", .length = n };
kos_reasoning_feed_event(session, "manufacturing", signal);

/* 单步推理 */
kos_reasoning_tick(session, "manufacturing");

/* 或一次跑完队列（带步数上限） */
kos_reasoning_run_until_idle(session, "manufacturing", 1000);

kos_reasoning_session_free(session);
```

**与“大量动态本体”的关系**：每个 ontology_id 对应一个独立的 kernel 状态和事件队列；多个本体可并行通过 `feed_event` + `tick`/`run_until_idle` 做实时推理，互不干扰。

---

## 3. 如何在线可视化展现依存关系

### 3.1 REST 端点（在线）

- **API 声明**：`include/kos_api.h`
- **实现**：`src/api/rest_endpoints.c`（依赖 `kos_ontology_registry.h`、`kos_visualization.h`）

**端点**（需将 `kos_ontology_registry_t*` 作为对应路由的 `user_data` 传入）：

| 方法 | 路径 | 说明 |
|------|------|------|
| GET | `/api/v1/ontologies` | 列举所有本体 id，返回 JSON：`{"ids":["id1","id2",...]}` |
| GET | `/api/v1/ontologies/dependencies?id=xxx` | 返回指定本体的**类型依存关系 JSON**（nodes + dependencies），供前端绘图或二次处理 |
| GET | `/api/v1/ontologies/dependencies/html?id=xxx` | 返回指定本体的**类型依存关系 HTML 页面**（Mermaid 图），可直接在浏览器中打开 |

**后端逻辑**：

1. 从 `user_data` 取得 `kos_ontology_registry_t*`。
2. 列表：`kos_ontology_registry_list(reg, &ids)`，序列化为 JSON。
3. 依存 JSON/HTML：`kos_ontology_registry_get(reg, id)` → `kos_visualization_extract_dependencies(ontology)` → `kos_visualization_generate_json(graph)` 或 `kos_visualization_generate_html(graph, title)`，返回给客户端。

**前端/在线使用**：

- **JSON**：前端可请求 `/api/v1/ontologies/dependencies?id=manufacturing`，用 D3.js、Cytoscape.js 等将 `nodes` 与 `dependencies` 渲染成图。
- **HTML**：直接打开或 iframe 嵌入 `/api/v1/ontologies/dependencies/html?id=manufacturing` 即可看到 Mermaid 依存图。

**与 api_demo 的集成**：`examples/api_demo.c` 中已创建注册表（`./ontology_data`）、`load_all`，并注册上述三条路由，将 `registry` 作为 `user_data` 传入；启动 API 服务后即可在线访问列表与依存关系。

---

## 4. 小结

| 需求 | 实现 |
|------|------|
| **大量动态本体的存储** | 使用 `kos_ontology_registry_t` + 目录持久化（每个本体 `id.json`），`save_all`/`load_all`/`load_one`。 |
| **实时推理** | 使用 `kos_reasoning_session_t` 按 ontology_id 维护 kernel 状态，`feed_event` 喂入信号，`tick` 或 `run_until_idle` 推进演化。 |
| **在线可视化依存关系** | 通过 REST：`GET /api/v1/ontologies` 列举 id，`GET /api/v1/ontologies/dependencies?id=xxx` 取 JSON，`GET /api/v1/ontologies/dependencies/html?id=xxx` 取 HTML 页面。 |

以上均基于现有 Core/Kernel/Runtime 与类型依存可视化（`TypeDependencyGraph`、`kos_visualization_extract_dependencies` 等），无需改动类型论或推理语义，仅增加注册表、推理会话与 REST 封装。
