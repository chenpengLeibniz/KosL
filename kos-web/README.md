# KOS-TL 应用展示系统（kos-web）

基于 **最小可信 KOS 内核（MTK）** 与三层架构（Core / Kernel / Runtime）的 Web 展示系统：实现 Core 类型与谓词加载、Runtime 信号模拟、Kernel 事件驱动的小步演化与 **Event Log（Trace）**，以及知识图谱与轨迹的交互查看。

## 与整体设计的关系

本仓库遵循 [New_Design_Plan.md](../New_Design_Plan.md) 中的 **Deterministic Event-Sourced Knowledge Kernel** 设计：

- **状态 σ** 由 Event 驱动演化；所有状态变化经 **类型检查** 后在 Kernel 内提交。
- **Trace** 即 **Event Log**：不可变追加的事件序列；支持确定性重放（同一 Trace + 同一初始 σ ⇒ 同一最终 σ）。
- **State Hash** 由 Trace 折叠计算，可验证一致性；崩溃后可从 Event Log 重放恢复。

kos-web 在浏览器端演示上述能力：Core 层类型/谓词、Runtime 信号生成、Kernel 的「摄入并演化」对应 **elab → Verify(Pre) → Reduce → Update(K,TS) → 追加 Event Log**，轨迹页对应 Trace 的可视化。详见 [MTK 与 Kernel/Runtime 对齐设计](../docs/MTK_KERNEL_RUNTIME_ALIGNMENT.md)。

## 技术栈

### 后端
- **框架**: FastAPI (Python)
- **无数据库**：演示为单进程内存状态（可扩展为 PostgreSQL/Redis）

### 前端
- **框架**: React 18
- **构建工具**: Vite
- **路由**: React Router
- **样式**: 原生 CSS
- **知识图谱**: vis-network (CDN)

### 基础设施
- **容器化**: Docker & Docker Compose（可选）

## 功能与三层/MTK 对应

| 层级 / MTK 概念 | 展示功能 | 实现要点 |
|----------------|----------|----------|
| **L0: Core** | 类型、谓词仅通过 **Core 加载** 注入；无预加载 | Core 页仅展示当前已加载内容；**Core 加载** 支持 KOS 文本与 **导入 .kos 文件**（`POST /api/core/load`、`POST /api/core/load_file`）；加载后自动同步到 Kernel 的 Γ。**宪法约束**：仅 Γ 中定义的类型才可参与对象创建，详见 [docs/CORE_CONSTITUTION.md](docs/CORE_CONSTITUTION.md) |
| **L2: Runtime** | 信号模拟、**生效**、**自动演化**、日志保存与重放 | 单条/批量/随机生成（含传感器、质检失效）；**生效** 将可精化为 Γ 中事件类型的信号送入队列 P；**自动演化** 按指定序列长度生成信号、入队并自动小步直至队列空；日志可保存与重放，支持「重放并演化」；`/api/runtime/apply`、`/api/runtime/auto_evolve`、`/api/runtime/log/save`、`/api/runtime/log/replay` |
| **L1: Kernel** | Γ、σ、事件队列 P、小步、自动场景、物化 | 展示 Γ（从 Core 同步）、σ（K）、队列 P；**执行一步** 从 P 取一条精化并小步演化，仅当类型在 Γ 中且通过可选 kos-core check-term 时才写入 K；按事件类型自动根因/审计；结果物化到 Runtime；`/api/kernel/step`、`/api/kernel/state`、`/api/kernel/trace` |
| **可视化** | 知识图谱、知识演进轨迹 | `/api/graph?step=`、轨迹步与每步场景结果 |

## 快速开始

### 1. 后端（Python 3.10+）

```bash
cd kos-web/backend
python -m venv .venv
# Windows: .venv\Scripts\activate
# Linux/macOS: source .venv/bin/activate
pip install -r requirements.txt
uvicorn app.main:app --reload --host 0.0.0.0 --port 8000
```

API 文档：<http://127.0.0.1:8000/docs>

### 2. 前端

```bash
cd kos-web/frontend
npm install
npm run dev
```

前端：<http://127.0.0.1:5173>，Vite 已配置代理将 `/api` 转发到后端 8000 端口。

**若始终使用 80 端口（如 Docker 或本机 Nginx）：**

1. **Docker**：前端镜像内的 `nginx.conf` 已配置 SPA 回退（`try_files $uri $uri/ /index.html` 与 `error_page 404 = /index.html`），访问 <http://localhost/kernel> 会返回 `index.html`，由 React Router 渲染 Kernel 页。修改 `nginx.conf` 后需**重新构建并重启**：
   ```bash
   cd kos-web && docker compose build frontend --no-cache && docker compose up -d
   ```
2. **本机 Nginx（非 Docker）**：若把 `frontend/dist` 放到 80 端口的站点根目录，须在对应 `server` 中配置：
   ```nginx
   location / { try_files $uri $uri/ /index.html; }
   error_page 404 = /index.html;
   ```
   并将 `location /api/` 代理到后端（如 `proxy_pass http://127.0.0.1:8000;`）。
3. **后端**：Kernel 页会请求 `/api/kernel/state`。Docker 下由同一 Nginx 将 `/api` 代理到 `backend:8000`；非 Docker 时须保证 80 上的 Nginx 将 `/api` 反代到实际后端地址，否则页会超时并显示「重试」。

### 3. 使用流程

1. **Core（必先）**：**Core 加载** 中输入 KOS 类型/谓词或导入 .kos 文件，点击「加载到 Core」。Γ 为空时 Runtime 无法入队、Kernel 无法创建对象；加载后类型同步到 Kernel 的 Γ。
2. **Runtime**：生成信号（随机/生产步骤/异常/失效事件/批量），或使用 **自动演化**（指定序列长度，一键生成并演化）。点击「生效」将能精化为 Γ 中事件类型的信号入队；可 **保存为日志**、**重放** 或 **重放并演化**（入队后自动执行小步）。
3. **Kernel**：点击「执行一步」或「执行至队列空」——仅当事件类型在 Γ 中（且可选经 kos-core check-term 通过）时才写入 K 并追加 Trace；按事件类型自动根因/审计。
4. **知识图谱**：切换状态步滑块，查看当前步的 Γ 与 σ。
5. **轨迹**：查看 Event Log（Trace）步骤列表与每步 ⟨e,p⟩。
6. **场景**：六大核心场景（根因、反事实、合规、审计、演化直至空闲、AI 治理）；演化或重放后可到 **场景 → 审计与问责 / 合规性决策** 做合规审计。

### 启用 kos-core（场景页完整功能）

场景页默认使用模拟结果。若要调用真实 kos-core 进行证明与类型校验，设置环境变量 `KOS_CORE_PATH` 指向 kos-core 可执行文件：

```bash
# 先构建：cd kos-core && stack build
# 可执行文件通常在 .stack-work/dist/.../build/kos-core/kos-core(.exe)

# Linux/macOS
export KOS_CORE_PATH=/path/to/KosL/kos-core/.stack-work/dist/.../build/kos-core/kos-core

# Windows PowerShell
$env:KOS_CORE_PATH = "D:\post\KOS\KosL\kos-core\.stack-work\dist\...\build\kos-core\kos-core.exe"

# Windows CMD
set KOS_CORE_PATH=D:\post\KOS\KosL\kos-core\.stack-work\dist\...\build\kos-core\kos-core.exe
```

## 目录结构

```
kos-web/
├── README.md
├── backend/
│   ├── requirements.txt
│   └── app/
│       ├── main.py           # FastAPI 入口与路由
│       ├── core_loader.py    # Core 类型/谓词加载
│       ├── sensor_simulator.py  # Runtime 信号模拟
│       ├── kernel_engine.py  # Kernel σ、Trace(Event Log)、ingest
│       ├── graph_data.py     # 知识图谱节点/边与步过滤
│       └── data/
│           └── types_and_predicates.json
└── frontend/
    ├── package.json
    ├── vite.config.js
    ├── index.html
    └── src/
        ├── main.jsx
        ├── App.jsx
        ├── api.js
        ├── index.css
        └── pages/
            ├── Dashboard.jsx
            ├── Core.jsx
            ├── Runtime.jsx
            ├── Kernel.jsx
            ├── KnowledgeGraph.jsx
            └── Trace.jsx
```

## Docker 构建与运行

在 **kos-web** 目录下执行：

**日常启动**（镜像已构建过，直接启动容器）：
```bash
docker compose up -d
```

**首次部署或修改代码/镜像后**（需要重新构建镜像再启动）：
```bash
docker compose up -d --build
```

其他常用命令：
```bash
docker compose build      # 仅构建镜像，不启动
docker compose logs -f    # 查看日志
docker compose down       # 停止并移除容器
```

启动后访问：
- **前端（推荐）**：<http://localhost> — 由 nginx 提供静态资源并将 `/api` 代理到后端
- **后端 API 文档**：<http://localhost:8000/docs> — 若需直接调 API 可访问 8000 端口

前端容器内的 nginx 会将 `http://frontend/api/...` 代理到 `http://backend:8000/api/...`，因此通过 80 端口访问时前后端同源，无需 CORS。


## 下一步改进（2026-02-12）
对kos-web进行改进和升级：
（1）“core”菜单并不预先加载类型和谓词，所有的内容通过“Core加载”菜单来实现，在菜单功能中，增加导入kos文件的功能。当从core加载了内容之后，“core”菜单实时更新和显示当前的类型和谓词，同时也自动同步到kernel层的$\Gamma$和$sigma$中。这些功能都通过kos-core层调用相关API来实现；
（2）“runtime”菜单中，调用src\utils\signal_simulate.c的功能进行模拟，可以尝试单个信号的模拟生成，也可以实现批量信号的生成，且提供随机信号生成的功能。当生成相应的信号后，可以点击“生效”，那么相应的信号就会送入系统中，一般的传感器信号会作为类型实例加入到kernel层的$\sigma$中，在这个过程中，如果出现需要构建新的类型，可以弹出相关提示，是否需要增加新的类型，并在core与kernel层进行同步。此外，runtime菜单中增加日志保存和重放功能。
（3）“kernel”菜单中，保存$\Gamma$、$\sigma$和事件队列，其中runtime中产生的事件都排队到事件队列中，然后根据小步操作语义，执行相关的知识演化，依据事件的类型，自动进行场景的自动推演（例如，如果是失效事件，就会自动进行根因溯源，如果是审计事件，并会自动进行审计），当能够有效构建根因后或者无法构建后，都会更新$\sigma$，同时在runtime层相应的实例或者类型进行物化。kernel层能够对整个状态的演化进行trace，同时能够展现知识演进的轨迹。
整个web系统是动态的，即与src和kos-core中的功能联动的。

## 文档

- **宪法约束（Core 层）**：[docs/CORE_CONSTITUTION.md](docs/CORE_CONSTITUTION.md)（中）、[docs/CORE_CONSTITUTION_EN.md](docs/CORE_CONSTITUTION_EN.md)（英）——对象创建必须通过 Γ 与可选 kos-core check-term
- **场景与 Runtime 信号**：[docs/SCENARIOS_RUNTIME_SIGNALS.md](docs/SCENARIOS_RUNTIME_SIGNALS.md)（中）、[docs/SCENARIOS_RUNTIME_SIGNALS_EN.md](docs/SCENARIOS_RUNTIME_SIGNALS_EN.md)（英）——六大场景与信号对应、自动演化与回放

## 参考

- **整体设计**：[New_Design_Plan.md](../New_Design_Plan.md)（三年路线图与 MTK 设计）
- **Kernel/Runtime 对齐**：[docs/MTK_KERNEL_RUNTIME_ALIGNMENT.md](../docs/MTK_KERNEL_RUNTIME_ALIGNMENT.md)
- **主项目说明**：[README.md](../README.md)（三层架构与关键算子）
- monograph：`monograph/KOS-TL-Book.tex` 第 8 章「KOS-TL 的应用示例」
- 知识图谱样式与数据：`monograph/kos-tl-knowledge-graph.html`
