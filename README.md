# KOS-TL (Knowledge Operation System - Type Logic)

基于《KOS-TL (Knowledge Operation System Type Logic)》设计原则开发的"知行合一"逻辑框架系统。

## 系统架构

系统严格划分为三个逻辑层，确保逻辑严密性与工程效率的平衡：

| 层级 | 名称 | 核心职责 | 技术实现重点 |
| --- | --- | --- | --- |
| **L0: Core** | **静态真理层** | 定义"什么是合法的"。建立基于直觉主义受限类型理论（ITT）的类型约束 | 依赖类型检查器（Bidirectional Type Checking）、证明项构造 |
| **L1: Kernel** | **动态迁移层** | 定义"如何改变"。通过小步操作语义处理事件驱动的状态演化 | 确定性状态机、事件队列管理、因果回溯 |
| **L2: Runtime** | **环境演化层** | 定义"如何运行"。处理外部 I/O、物理存储映射及信号提炼 | 信号提炼算子（elab）、物理存储管理器（M） |

## 核心功能

- **知识对象原子化 (Σ-Types)**：所有进入系统的知识必须以 `<d, p>` 的形式存在，即将数据 `d` 与其符合业务逻辑的证明 `p` 强耦合，消除"无根数据"
- **确定性演化 (Small-step Semantics)**：系统状态迁移必须由经过验证的事件（Event）驱动，且严禁非确定性选择
- **计算反射与内生审计 (Reflexivity)**：系统在执行每一次逻辑演化时，必须自动合成等价性证明（Identity Proof），实现运行时的实时形式化审计
- **异步提炼与顺序提交**：物理信号可并行提炼，但内核必须顺序提交，以保证因果链的唯一性

## 关键算子

- **elab (提炼算子)**：将原始物理比特流映射为带有逻辑证明的事件对象 `<e, p>`
- **STEP (迁移算子)**：驱动系统从旧状态 `σ` 到新状态 `σ'` 的单调演化
- **M (具象化算子)**：将抽象的逻辑结论下沉为物理动作（如数据库 ACID 事务或硬件电压控制）

## 构建说明

### 使用 CMake 构建

```bash
# 创建构建目录
mkdir build
cd build

# 配置（使用 MinGW）
cmake -G "MinGW Makefiles" ..

# 或者使用 Visual Studio
cmake ..

# 构建
cmake --build .

# 运行
./bin/kos_system.exe
```

## 项目结构

```
KosL/
├── include/          # 头文件
│   ├── kos_core.h    # L0 Core 层接口
│   ├── kos_kernel.h  # L1 Kernel 层接口
│   └── kos_runtime.h # L2 Runtime 层接口
├── src/
│   ├── core/         # L0 Core 实现
│   │   ├── type_checker.c    # 类型检查
│   │   ├── reduction.c       # 归约操作
│   │   ├── business_logic.c  # 业务逻辑
│   │   ├── type_builder.c    # 类型构建器
│   │   └── storage.c         # 存储和加载
│   ├── kernel/       # L1 Kernel 实现
│   └── runtime/      # L2 Runtime 实现
├── examples/         # 示例代码
│   └── test_storage.c # 存储工具演示
├── main.c            # 主程序入口
├── CMakeLists.txt    # CMake 构建配置
└── README.md         # 项目说明
```

## Core 层工具

系统提供了完整的 Core 层基础类型构建、存储和加载工具：

### 类型构建器 (Type Builder)

- `kos_mk_atomic()` - 创建原子值类型
- `kos_mk_prop()` - 创建命题类型
- `kos_mk_val()` - 创建值类型
- `kos_mk_pair()` - 创建 Σ-Type 对 `<d, p>`
- `kos_mk_sigma()` - 创建依赖类型 `Σ(x:A).B`
- `kos_term_copy()` - 深拷贝 term
- `kos_term_free()` - 递归释放 term

### 存储和加载 (Storage & Loading)

- `kos_term_serialize()` - 序列化 term 为 JSON 格式
- `kos_term_deserialize()` - 从 JSON 反序列化 term
- `kos_term_save_to_file()` - 存储 term 到文件
- `kos_term_load_from_file()` - 从文件加载 term
- `kos_knowledge_save()` - 存储知识集 K 到文件
- `kos_knowledge_load()` - 从文件加载知识集 K

### 使用示例

```c
// 创建类型
kos_term* val = kos_mk_val("hello");
kos_term* prop = kos_mk_prop("IsVerified(alice)");
kos_term* pair = kos_mk_pair(val, prop);

// 序列化
kos_serialized* json = kos_term_serialize(pair);

// 存储到文件
kos_term_save_to_file(pair, "knowledge.json");

// 从文件加载
kos_term* loaded = kos_term_load_from_file("knowledge.json");

// 清理
kos_term_free(pair);
kos_serialized_free(json);
```

## 设计原则

该架构确保了"任何物理存储中的比特翻转，在逻辑层都有完整的本体证明链支持"。

## 许可证

[待添加]

