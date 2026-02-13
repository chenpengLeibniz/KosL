# kos-core 证明能力与 KOS-TL 知识操作需求对照

本文档对当前 kos-core 的自动化证明（Proof 模块）与 KOS-TL 对「知识操作」的形式化需求做整体 review，给出**已满足**与**缺口**，并给出改进建议。

---

## 一、KOS-TL 对知识操作的核心要求（来自 monograph）

### 1. 知识 = 构造性项

- 知识载体为 **Σ(d:D).P(d)**：数据 d 与证明 P(d) 强耦合，无证明则不能进入知识库。
- 知识准入：只有能**构造出**类型正确、约束满足且证据可携带的项，才可被 elab 提升为知识事件。

### 2. 证明搜索机 (PSM) 配置与规则

- **配置**：⟨Γ; **σ**; G; **Δ**⟩  
  - Γ：类型上下文与见证  
  - **σ**：**知识库**（异常、工序、事件及其关系）  
  - G：当前目标  
  - **Δ**：**子目标栈**

- **关键规则**：
  - **Σ-展开**：目标 G = Σx:T.G' 时，从 **Candidates(T, σ)** 取候选 v，推进到 G'[v/x]。
  - **因果证明引入**：G = CausalProof(a,f) 时，展开为 Σe:ProcStep.(TimeOK∧SpaceOK∧BatchOK)，即需要从 σ 中选 e 并证三个谓词。
  - **合取分解**：G = G₁∧G₂ 时，压栈为 G₁、G₂。
  - **原子谓词**：TimeOK(a,e,f) 等由 **checkTime(a,e,f)** 等可计算判定，成立则归约为 ⊤，否则 ⊥。
  - **成功/失败**：⊤ 弹出子目标；⊥ 则 Fail。

### 3. Trace = Proof 与状态演化

- 决策即可构造项：合法行动 a 当且仅当存在合法轨迹 T 且 a 在 T 中被实例化。
- 状态演化：σ →^⟨e,p⟩ σ'，每步需前置验证、后置证明合成、因果链可追溯。
- 知识生成即演化：没有凭空新增事实，每条新知识都是一次受控状态迁移。

### 4. 核心层在小步演化中的角色

- 提供 **mkCausalProof** 等构造子的类型与规则。
- **checkTime / checkSpace / checkBatch**：对 TimeOK、SpaceOK、BatchOK 做可计算验证，返回证明项或失败。
- **tryWithProc(a,f,e)**：对单个工序 e 尝试构造 CausalProof(a,f)，内部链式调用上述 check。
- **deriveCausal(a,f)**：firstSome (map (λe. tryWithProc(a,f,e), **getProcSteps(f,σ)**))，即**依赖 σ 的候选枚举**。
- **analyze(f)**：firstSome (map (λa. …, **getAnomalies(f,σ)**))，同样**依赖 σ**。

---

## 二、当前 kos-core 证明能力概览

### 已具备（与 Core 层一致的部分）

| 能力 | 实现 | 说明 |
|------|------|------|
| 目标驱动的证明搜索 | `prove ctx goal` | 给定 Γ( ctx ) 与目标类型 G，尝试返回证明项 t（Maybe Term）。 |
| 值依赖谓词的可计算验证 | `tryValuePred` | Gt/Ge/Lt/Le/Eq(a,b) 求值比较，成立则返回 Triv，对应「原子谓词判定」的数值部分。 |
| Id 类型（定义等价） | `tryRefl` | 当 a ≡ b 时返回 Refl a。 |
| Σ 引入 | `tryIntroSigma` | 目标 Σ(x:A).B 时，先证 A 得 d，再证 B[d/x] 得 p，返回 Pair d p。 |
| Π 引入 | `tryIntroPi` | 目标 Π(x:A).B 时，在 Γ,x:A 下证 B 得 body，返回 Lam。 |
| 构造函数应用 | `tryConstructor` | 在 Γ 中查找类型形如 Π…→ goal 的定义，用 **proveArgs** 对参数做**依赖顺序**证明（证完 Aᵢ 代入后续类型），构造 App(..App(d,p₁)..,pₙ)。 |
| 假设（见证） | `tryAssumption` | 若目标与某上下文变量类型定义等价，返回 Var name，便于 tryWithCandidates 中候选 __e 被使用。 |
| tryWithCandidates | 使用候选 | 对每个候选 c 将 c 以 __e 加入 Γ 再 prove goal，对应 deriveCausal 的「对每个 e 尝试 tryWithProc」。 |
| 类型良构与定义等价 | TypeCheck + Reduction | 支持 normalize、expandTypeSynonym、definitionallyEqual，满足 Core 层「守门人」角色。 |

以上足以支撑：**在纯类型论层面、仅给定 Γ 与目标 G 时，对部分目标自动构造证明项**，与 monograph 中「核心层：证明构造与小步演化」的**类型与规则定义**对齐。

---

## 三、与 KOS-TL 知识操作需求的缺口

### 1. 无知识库 σ，无法实现「基于 σ 的候选与枚举」

- PSM 的 **Σ-展开** 依赖 **Candidates(T, σ)**：从知识库 σ 中取类型 T 的候选（如 ProcStep、Anomaly）。
- **getProcSteps(f,σ)**、**getAnomalies(f,σ)** 等均依赖 σ；当前 kos-core 只有 **Context（Γ）**，没有「事实集合」σ。
- **影响**：无法实现 monograph 中的 deriveCausal/analyze：无法枚举「与 f 相关的工序」或「与 f 相关的异常」，只能对**已存在于 Γ 中的项**做构造函数应用。
- **结论**：kos-core 是**纯 Core 层**；「从 σ 取候选」属于 Kernel/运行时，需在上层或通过传入候选列表对接。

### 2. 无子目标栈 Δ，无显式合取分解

- PSM 将 G = G₁∧G₂ 分解为子目标栈并依次完成；当前 prove 是**单目标递归**，没有显式 Δ。
- 书中 TimeOK∧SpaceOK∧BatchOK 若用 Σ 或乘积编码，当前可通过 tryIntroSigma + 多次 prove 模拟，但**合取 (∧) 本身不是一等类型**，没有专门的「合取分解」规则。
- **影响**：对「因果证明引入」的展开形式（Σe. (TimeOK∧SpaceOK∧BatchOK)）需在类型设计上用 Σ 表达，并由 tryConstructor + tryValuePred 配合完成；当前策略顺序可覆盖部分情况，但未显式对应「分解 → 压栈 → 完成」的 PSM 步骤。

### 3. ~~依赖参数未在证明中顺序代入~~（已修复）

- **已实现**：**piChainToGoal** 现返回 **[(x₁,A₁),…,(xₙ,Aₙ)]**（保留绑定名），**proveArgs** 在证完 Aᵢ 得到 dᵢ 后，将 dᵢ 代入后续类型再证（substitute x d ty），从而支持 **mkCausalProof(a,f,e,p_t,p_s,p_b)** 等依赖型构造子。

### 4. TimeOK/SpaceOK/BatchOK 与领域类型的对应

- 书中 TimeOK(a,e,f) ≜ (a.t ∈ e.dur)∧(e.dur.end < f.t)，SpaceOK(a,e) ≜ (a.m = e.m)，BatchOK(e,f) ≜ (e.b = f.b)，均为**结构化谓词**（依赖 a,e,f 的字段）。
- kos-core 仅有**通用** Gt/Ge/Lt/Le/Eq；没有「从项 e 投影 e.dur、e.m、e.b」及「区间包含、同资源」等**领域逻辑**。
- **影响**：若在 .kos 中把 TimeOK 定义为某种 Gt/Le 的组合或 Σ 类型，tryValuePred 可覆盖**纯数值比较**部分；但「a.t ∈ e.dur」这类区间/结构约束需在类型设计或额外策略中体现，当前需在**类型定义层面**把 TimeOK/SpaceOK/BatchOK 编码为可被 tryValuePred 或 tryConstructor 处理的形式。

### 5. ~~tryWithCandidates 未使用候选列表~~（已修复）

- **已实现**：对每个候选 c，用 **infer ctx c** 得到类型 ty，**addDef "__e" ty c ctx** 将候选加入上下文，再对 **goal** 调用 **prove**；返回第一个成功的证明。这样 **tryAssumption** 在证明子目标（如 ProcStep）时可选用上下文中的 `__e`，实现「对每个 e 尝试 tryWithProc(goal,e)」的语义。

### 6. 无状态 σ、无演化、无 Trace=Proof 运行时绑定

- 知识操作中的「知识生成即演化」、Trace = Proof、elab、materialize、step 等均依赖**状态 σ** 与**事件队列**；kos-core 不包含 σ、无 step、无 materialize。
- **结论**：这是**分层设计**下的预期：kos-core 负责**类型与证明构造**，Kernel/Runtime 负责 σ、事件与轨迹。要「满足 KOS-TL 对知识操作的需求」，需在**系统层面**将 kos-core 与 Kernel（含 σ 与候选生成）组合使用。

### 7. 无显式 ⊤/⊥ 与 Fail 配置

- PSM 有 ⊤（目标完成）、⊥（失败）与 Fail 配置；kos-core 仅用 **Maybe**（Nothing = 失败，Just t = 成功），无显式「当前目标为 ⊤/⊥」的配置。
- **影响**：对「仅做证明搜索」而言足够；若将来要完全形式化 PSM 的配置与转移，可引入 StepConfig 的扩展（如带 Δ 与成功/失败状态）。

---

## 四、总体结论与建议

### 结论

- **能满足的部分**：KOS-TL 在 **Core 层**对「证明构造与小步演化」的需求：类型与规则形式、目标驱动的证明搜索、值依赖谓词验证、Σ/Π 引入、构造函数应用（小步构造）。在这些前提下，kos-core 可以为**给定 Γ 与目标 G** 自动尝试构造证明项，并配合类型检查保证正确性。
- **尚不满足的部分**：KOS-TL 对**知识操作**的完整需求还包含：(1) **知识库 σ** 与基于 σ 的候选生成（getProcSteps/getAnomalies、Candidates(T,σ)）；(2) **子目标栈 Δ** 与显式合取分解；(3) **TimeOK/SpaceOK/BatchOK** 等领域谓词与结构化约束的完整对应；(4) **状态演化与 Trace=Proof** 的运行时绑定。其中依赖型构造函数的顺序代入、tryWithCandidates 对候选的实质使用已在 Proof 中实现；其余需在 Kernel/Runtime 或扩展 Proof 后共同实现。

### 建议（按优先级）

1. ~~**修复依赖参数**~~：已实现 proveArgs，tryConstructor 使用 piChainToGoal 的 [(x,A)] 与 proveArgs 做顺序代入。
2. ~~**明确 tryWithCandidates 语义**~~：已实现「对每个 candidate 以 __e 加入 Γ 再 prove」。
3. **与 Kernel 的接口**：在 C/上层维护 σ，实现 getProcSteps/getAnomalies（或等价物），将「候选列表」与「目标类型」传入 kos-core 的 tryWithCandidates/prove；kos-core 继续只依赖 Γ + goal（及可选 candidates），不引入 σ。
4. **领域谓词**：在 .kos 或类型库中把 TimeOK/SpaceOK/BatchOK 定义为可被当前策略处理的类型（如基于 Gt/Le/Eq 的 Σ 或应用），或为 Proof 增加「从项投影字段再比较」的专用策略（需 AST 支持投影）。
5. **可选**：若需更贴近 PSM 的形式化，可增加带 Δ 的配置类型与「合取分解」「⊤/⊥」的显式规则，与现有 prove 组合使用。

---

*文档版本：与当前 kos-core Proof 模块及 monograph 第 8 章、PSM 一节对照。*
