// include/kos_ontology_registry.h
// 多本体注册表：大量动态本体的存储、按需加载与持久化
// 支持目录持久化（每个本体 id.json）、运行时注册/注销、在线列举

#ifndef KOS_ONTOLOGY_REGISTRY_H
#define KOS_ONTOLOGY_REGISTRY_H

#include "kos_ontology.h"
#include <stdbool.h>
#include <stddef.h>

// ========== 本体注册表 ==========

typedef struct kos_ontology_registry kos_ontology_registry_t;

// 创建注册表；storage_dir 为持久化目录（NULL 表示仅内存，不持久化）
kos_ontology_registry_t* kos_ontology_registry_create(const char* storage_dir);

void kos_ontology_registry_free(kos_ontology_registry_t* reg);

// 注册本体：id 唯一；若 id 已存在则覆盖（旧本体由调用方负责释放）
// ontology 由注册表接管（unregister/clear 时会 free）
int kos_ontology_registry_register(kos_ontology_registry_t* reg,
                                   const char* id,
                                   TypeOntology* ontology);

// 按 id 获取本体（返回指针，不转移所有权）
TypeOntology* kos_ontology_registry_get(kos_ontology_registry_t* reg, const char* id);

// 注销并返回本体（调用方负责 kos_ontology_free）；未找到返回 NULL
TypeOntology* kos_ontology_registry_unregister(kos_ontology_registry_t* reg, const char* id);

// 列举所有 id；out_ids 为输出数组（调用方 free 每个元素及数组），返回数量
size_t kos_ontology_registry_list(kos_ontology_registry_t* reg, char*** out_ids);

// 将当前内存中所有本体持久化到 storage_dir（每个 id 存为 id.json）
int kos_ontology_registry_save_all(kos_ontology_registry_t* reg);

// 从 storage_dir 加载所有 *.json 作为本体，文件名（去掉 .json）为 id
int kos_ontology_registry_load_all(kos_ontology_registry_t* reg);

// 仅从 storage_dir 加载指定 id（id.json），若已存在则先 unregister 再加载
TypeOntology* kos_ontology_registry_load_one(kos_ontology_registry_t* reg, const char* id);

#endif /* KOS_ONTOLOGY_REGISTRY_H */
