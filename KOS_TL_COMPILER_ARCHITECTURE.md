# KOS-TL 编译器架构设计

## 1. 编译器整体架构

```
┌─────────────────────────────────────────────────────────┐
│                    KOS-TL 源代码                         │
│              (example.kos)                              │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│              词法分析器 (Lexer)                          │
│  - 识别关键字、标识符、操作符                            │
│  - 输出 Token 流                                        │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│              语法分析器 (Parser)                         │
│  - 构建抽象语法树 (AST)                                 │
│  - 处理优先级和结合性                                   │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│              类型检查器 (Type Checker)                   │
│  - 双向类型检查                                         │
│  - Universe 层级检查                                    │
│  - 依赖类型解析                                         │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│              证明验证器 (Proof Verifier)                 │
│  - 验证证明项                                           │
│  - 检查类型约束                                         │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│              代码生成器 (Code Generator)                 │
│  - AST → C 代码                                         │
│  - 调用 KOS-TL C API                                    │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│                    C 代码                                │
│              (example.c)                                │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│              C 编译器 (GCC/Clang/MSVC)                   │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│                 可执行文件                               │
│              (example.exe)                             │
└─────────────────────────────────────────────────────────┘
```

## 2. 模块设计

### 2.1 词法分析器 (src/compiler/lexer.c)

```c
// Token 类型定义
typedef enum {
    TOKEN_EOF,
    TOKEN_IDENTIFIER,
    TOKEN_KEYWORD,
    TOKEN_NUMBER,
    TOKEN_STRING,
    TOKEN_OPERATOR,
    TOKEN_PUNCTUATION,
    // 关键字
    TOKEN_MODULE, TOKEN_WHERE, TOKEN_IMPORT,
    TOKEN_TYPE, TOKEN_DEF, TOKEN_LET, TOKEN_IN,
    TOKEN_LAMBDA, TOKEN_ARROW, TOKEN_SIGMA, TOKEN_PI,
    TOKEN_MATCH, TOKEN_WITH, TOKEN_IF, TOKEN_THEN, TOKEN_ELSE,
    // 操作符
    TOKEN_EQ, TOKEN_NE, TOKEN_LT, TOKEN_GT,
    TOKEN_PLUS, TOKEN_MINUS, TOKEN_MUL, TOKEN_DIV,
    // 标点
    TOKEN_LPAREN, TOKEN_RPAREN,
    TOKEN_LBRACE, TOKEN_RBRACE,
    TOKEN_LBRACKET, TOKEN_RBRACKET,
    TOKEN_COMMA, TOKEN_SEMICOLON, TOKEN_COLON,
    TOKEN_DOT, TOKEN_UNDERSCORE
} token_kind;

typedef struct {
    token_kind kind;
    char* value;
    int line;
    int column;
} Token;

// 词法分析器接口
typedef struct {
    const char* source;
    size_t position;
    int line;
    int column;
} Lexer;

Token* lex_token(Lexer* lexer);
Token* lex_all(const char* source);
void token_free(Token* token);
```

### 2.2 语法分析器 (src/compiler/parser.c)

```c
// AST 节点类型
typedef enum {
    AST_MODULE,
    AST_IMPORT,
    AST_TYPE_DECL,
    AST_DEF,
    AST_LAMBDA,
    AST_APPLY,
    AST_VAR,
    AST_LITERAL,
    AST_PAIR,
    AST_SIGMA,
    AST_PI,
    AST_MATCH,
    AST_LET,
    AST_IF
} ast_kind;

typedef struct AST {
    ast_kind kind;
    union {
        struct {
            char* name;
            struct AST** declarations;
            size_t decl_count;
        } module;
        
        struct {
            char* name;
            struct AST* type;
        } type_decl;
        
        struct {
            char* name;
            struct AST* type;
            struct AST* body;
        } def;
        
        struct {
            char* param;
            struct AST* param_type;
            struct AST* body;
        } lambda;
        
        struct {
            struct AST* func;
            struct AST* arg;
        } apply;
        
        // ... 其他节点类型
    } data;
    int line;
    int column;
} AST;

// 语法分析器接口
AST* parse_module(Token* tokens, size_t token_count);
AST* parse_expression(Token* tokens, size_t* position);
void ast_free(AST* ast);
```

### 2.3 类型检查器 (src/compiler/type_checker.c)

```c
// 类型环境
typedef struct {
    char** names;
    kos_term** types;
    size_t count;
} TypeContext;

// 类型检查结果
typedef struct {
    bool success;
    kos_term* inferred_type;
    char* error_message;
} TypeCheckResult;

// 类型检查接口
TypeCheckResult type_check(AST* ast, TypeContext* ctx);
TypeCheckResult type_check_expression(AST* expr, TypeContext* ctx);
TypeCheckResult type_check_definition(AST* def, TypeContext* ctx);
kos_term* infer_type(AST* expr, TypeContext* ctx);
```

### 2.4 代码生成器 (src/compiler/codegen.c)

```c
// 代码生成器状态
typedef struct {
    FILE* output;
    int indent_level;
    char* current_function;
} CodeGenState;

// 代码生成接口
void codegen_module(AST* module, CodeGenState* state);
void codegen_definition(AST* def, CodeGenState* state);
void codegen_expression(AST* expr, CodeGenState* state);
void codegen_type(kos_term* type, CodeGenState* state);
```

## 3. 目录结构

```
kosl-compiler/
├── src/
│   ├── compiler/
│   │   ├── lexer.c          # 词法分析器
│   │   ├── lexer.h
│   │   ├── parser.c          # 语法分析器
│   │   ├── parser.h
│   │   ├── type_checker.c    # 类型检查器
│   │   ├── type_checker.h
│   │   ├── codegen.c         # 代码生成器
│   │   ├── codegen.h
│   │   ├── ast.c             # AST 操作
│   │   ├── ast.h
│   │   └── error.c           # 错误处理
│   │   └── error.h
│   └── main.c                # 编译器主程序
├── include/
│   └── compiler.h            # 编译器公共接口
├── examples/                 # 示例 KOS-TL 代码
│   ├── hello.kos
│   ├── finance.kos
│   └── manufacturing.kos
├── tests/                    # 测试用例
│   ├── test_lexer.c
│   ├── test_parser.c
│   └── test_type_checker.c
├── CMakeLists.txt
└── README.md
```

## 4. 实现步骤

### 阶段 1：词法分析器 (1-2 周)

1. 定义 Token 类型
2. 实现字符流读取
3. 实现关键字识别
4. 实现标识符和字面量识别
5. 实现操作符识别
6. 添加错误处理

### 阶段 2：语法分析器 (2-3 周)

1. 定义 AST 节点类型
2. 实现表达式解析
3. 实现声明解析
4. 实现模块解析
5. 处理优先级和结合性
6. 错误恢复机制

### 阶段 3：类型检查器 (3-4 周)

1. 实现类型环境管理
2. 实现双向类型检查
3. 实现 Universe 层级检查
4. 实现依赖类型解析
5. 类型推断算法
6. 错误报告

### 阶段 4：代码生成器 (2-3 周)

1. AST 到 C 代码的映射
2. 类型到 C 类型的映射
3. 表达式代码生成
4. 函数代码生成
5. 调用 KOS-TL C API
6. 代码优化

### 阶段 5：集成测试 (1-2 周)

1. 端到端测试
2. 性能测试
3. 错误处理测试
4. 文档完善

## 5. 技术选型

### 5.1 解析器生成器

选项：
- **手写递归下降解析器**：完全控制，易于调试
- **ANTLR**：功能强大，但依赖 Java
- **Lemon**：轻量级，C 语言
- **yacc/bison**：经典工具

**推荐**：手写递归下降解析器（初期），后续可考虑 ANTLR

### 5.2 数据结构

- **AST**：使用树结构，支持递归释放
- **类型环境**：使用哈希表或数组
- **符号表**：使用哈希表

### 5.3 错误处理

- 使用错误码和错误消息
- 支持多错误收集
- 提供详细的错误位置信息

## 6. 示例：编译流程

### 输入 (example.kos)

```kos
module Example where

type Person : U₁

def hello : Person → String
  = λ(p : Person) → "Hello"
```

### 词法分析输出

```
TOKEN_MODULE "module"
TOKEN_IDENTIFIER "Example"
TOKEN_WHERE "where"
TOKEN_TYPE "type"
TOKEN_IDENTIFIER "Person"
TOKEN_COLON ":"
TOKEN_IDENTIFIER "U₁"
...
```

### 语法分析输出 (AST)

```
Module {
  name: "Example"
  declarations: [
    TypeDecl {
      name: "Person"
      type: Var("U₁")
    },
    Def {
      name: "hello"
      type: Pi("p", Var("Person"), Var("String"))
      body: Lambda("p", Var("Person"), Literal("Hello"))
    }
  ]
}
```

### 类型检查输出

```
✓ Type "Person" : U₁
✓ Function "hello" : Person → String
```

### 代码生成输出 (example.c)

```c
#include "kos_core.h"
#include "kos_runtime.h"

// Type Person
kos_term* Person_type = kos_mk_id("Person");

// Function hello
kos_term* hello(kos_term* p) {
    kos_term* str = kos_mk_prop("Hello");
    return str;
}
```

## 7. 下一步行动

1. 创建编译器项目结构
2. 实现词法分析器
3. 实现语法分析器
4. 实现类型检查器
5. 实现代码生成器
6. 集成测试
















