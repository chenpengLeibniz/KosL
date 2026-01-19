# 制造业领域模块架构说明

## 文件职责划分

### 1. `ontology_setup.c`
**职责**：本体初始化
- 从JSON文件加载本体，或创建默认本体
- 定义基础类型（BatchID, Machine, Time等）
- 定义事件类型（FailEvt, Anomaly, ProcStep等）
- 定义谓词类型（InRoute, Overlap等）
- 添加扩展类型定义（通过生成的代码）
- 保存本体到文件

**接口**：
- `kos_manufacturing_ontology_init()`: 初始化本体（从文件加载或创建默认）

### 2. `ontology_manager.c` (新增)
**职责**：本体单例管理和访问
- 管理本体的单例实例（延迟加载、缓存）
- 提供统一的访问接口
- 处理大本体时的内存优化（未来可扩展）

**接口**：
- `kos_manufacturing_ontology_get()`: 获取本体实例（单例，延迟加载）
- `kos_manufacturing_ontology_release()`: 释放本体实例
- `kos_manufacturing_ontology_reload()`: 强制重新加载本体
- `kos_manufacturing_ontology_is_loaded()`: 检查本体是否已加载
- `kos_manufacturing_ontology_get_type_count()`: 获取类型数量

### 3. `runtime_elab.c`
**职责**：运行时事件精化
- 从原始数据流（bitstream）精化为类型化事件
- 通过本体管理器获取类型定义
- 构造符合类型定义的事件实例
- 自动进行类型检查和验证
- 成功创建事件后进行相应处理

**接口**：
- `kos_elab_failure_event()`: 精化失败事件
- `kos_elab_anomaly()`: 精化异常事件
- `kos_elab_process_step()`: 精化工艺步骤

**工作流程**：
1. 通过 `kos_manufacturing_ontology_get()` 获取本体
2. 从本体中查找类型定义（如 "FailEvt"）
3. 解析原始数据
4. 构造基础类型值
5. 构造嵌套的Σ类型实例
6. 使用 `kos_ontology_mk_type_instance()` 进行类型检查和验证
7. 成功创建事件后进行处理

### 4. `ontology_crud.c`
**职责**：类型定义的CRUD操作（待迁移）
- 当前为占位符实现
- 需要迁移到新的基于类型构造的API

### 5. `ontology_extended_generated.c`
**职责**：自动生成的扩展类型定义
- 包含大量制造业常见类型（217+种）
- 由 `tools/generate_manufacturing_types.py` 自动生成

### 6. `types.c`
**职责**：类型构建器函数
- 提供便捷的类型实例构建函数
- 基于本体类型定义

### 7. `predicates.c`
**职责**：谓词验证
- 实现各种谓词的验证逻辑

### 8. `traceability.c`
**职责**：追溯分析
- 实现质量追溯分析功能

## 架构设计原则

1. **职责分离**：
   - 本体初始化：`ontology_setup.c`
   - 本体管理：`ontology_manager.c`
   - 运行时精化：`runtime_elab.c`

2. **单例模式**：
   - 本体实例由 `ontology_manager.c` 统一管理
   - 延迟加载，首次使用时才加载
   - 所有模块通过统一接口访问

3. **类型论基础**：
   - 所有类型定义都通过类型构造器（Π、Σ、Sum等）构造
   - 事件实例化时必须通过类型检查验证

4. **可扩展性**：
   - 支持大本体（未来可扩展为分片加载）
   - 支持运行时更新本体定义

## 使用示例

```c
// 1. 获取本体（自动加载）
TypeOntology* ontology = kos_manufacturing_ontology_get();

// 2. 运行时精化事件
bitstream raw_data = { ... };
kos_term* event = kos_elab_failure_event(raw_data, NULL);

// 3. 程序结束时释放
kos_manufacturing_ontology_release();
```
















