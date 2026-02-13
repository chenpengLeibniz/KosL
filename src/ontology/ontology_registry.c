// src/ontology/ontology_registry.c
// 多本体注册表：存储、列举、目录持久化

#include "../../include/kos_ontology_registry.h"
#include "../../include/kos_ontology.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define REG_INIT_CAP 32

typedef struct {
    char* id;
    TypeOntology* ontology;
} registry_entry_t;

struct kos_ontology_registry {
    char* storage_dir;           /* NULL = 仅内存 */
    registry_entry_t* entries;
    size_t count;
    size_t capacity;
};

static int ensure_capacity(kos_ontology_registry_t* reg) {
    if (reg->count < reg->capacity) return 0;
    size_t new_cap = reg->capacity ? reg->capacity * 2 : REG_INIT_CAP;
    registry_entry_t* new_entries = (registry_entry_t*)realloc(
        reg->entries, new_cap * sizeof(registry_entry_t));
    if (!new_entries) return -1;
    reg->entries = new_entries;
    reg->capacity = new_cap;
    return 0;
}

static void free_entry(registry_entry_t* e) {
    if (!e) return;
    free(e->id);
    if (e->ontology) kos_ontology_free(e->ontology);
    e->id = NULL;
    e->ontology = NULL;
}

kos_ontology_registry_t* kos_ontology_registry_create(const char* storage_dir) {
    kos_ontology_registry_t* reg = (kos_ontology_registry_t*)calloc(1, sizeof(kos_ontology_registry_t));
    if (!reg) return NULL;
    if (storage_dir) {
        reg->storage_dir = strdup(storage_dir);
        if (!reg->storage_dir) { free(reg); return NULL; }
    }
    return reg;
}

void kos_ontology_registry_free(kos_ontology_registry_t* reg) {
    if (!reg) return;
    for (size_t i = 0; i < reg->count; i++)
        free_entry(&reg->entries[i]);
    free(reg->entries);
    free(reg->storage_dir);
    free(reg);
}

int kos_ontology_registry_register(kos_ontology_registry_t* reg,
                                   const char* id,
                                   TypeOntology* ontology) {
    if (!reg || !id || !ontology) return -1;
    /* 若已存在则先移除再插入（替换） */
    for (size_t i = 0; i < reg->count; i++) {
        if (strcmp(reg->entries[i].id, id) == 0) {
            free_entry(&reg->entries[i]);
            reg->entries[i].id = strdup(id);
            reg->entries[i].ontology = ontology;
            if (!reg->entries[i].id) return -1;
            return 0;
        }
    }
    if (ensure_capacity(reg) != 0) return -1;
    reg->entries[reg->count].id = strdup(id);
    reg->entries[reg->count].ontology = ontology;
    if (!reg->entries[reg->count].id) return -1;
    reg->count++;
    return 0;
}

TypeOntology* kos_ontology_registry_get(kos_ontology_registry_t* reg, const char* id) {
    if (!reg || !id) return NULL;
    for (size_t i = 0; i < reg->count; i++) {
        if (strcmp(reg->entries[i].id, id) == 0)
            return reg->entries[i].ontology;
    }
    return NULL;
}

TypeOntology* kos_ontology_registry_unregister(kos_ontology_registry_t* reg, const char* id) {
    if (!reg || !id) return NULL;
    for (size_t i = 0; i < reg->count; i++) {
        if (strcmp(reg->entries[i].id, id) == 0) {
            TypeOntology* o = reg->entries[i].ontology;
            free(reg->entries[i].id);
            reg->entries[i].id = NULL;
            reg->entries[i].ontology = NULL;
            /* 压缩：将最后一个移到此处 */
            reg->count--;
            if (i < reg->count) {
                reg->entries[i] = reg->entries[reg->count];
                reg->entries[reg->count].id = NULL;
                reg->entries[reg->count].ontology = NULL;
            }
            return o;
        }
    }
    return NULL;
}

size_t kos_ontology_registry_list(kos_ontology_registry_t* reg, char*** out_ids) {
    if (!reg || !out_ids) return 0;
    *out_ids = NULL;
    if (reg->count == 0) return 0;
    char** arr = (char**)malloc(reg->count * sizeof(char*));
    if (!arr) return 0;
    for (size_t i = 0; i < reg->count; i++) {
        arr[i] = strdup(reg->entries[i].id);
        if (!arr[i]) {
            while (i > 0) free(arr[--i]);
            free(arr);
            return 0;
        }
    }
    *out_ids = arr;
    return reg->count;
}

static int build_path(char* buf, size_t buf_size, const char* dir, const char* id) {
    int n = snprintf(buf, buf_size, "%s/%s.json", dir, id);
    return (n > 0 && (size_t)n < buf_size) ? 0 : -1;
}

int kos_ontology_registry_save_all(kos_ontology_registry_t* reg) {
    if (!reg || !reg->storage_dir) return -1;
    char path[1024];
    for (size_t i = 0; i < reg->count; i++) {
        if (build_path(path, sizeof(path), reg->storage_dir, reg->entries[i].id) != 0)
            continue;
        if (kos_ontology_save_to_file(reg->entries[i].ontology, path) != 0)
            return -1;
    }
    return 0;
}

#ifdef _WIN32
#include <windows.h>
static int scan_json_ids(const char* dir, char*** out_ids, size_t* out_count) {
    char pattern[1024];
    snprintf(pattern, sizeof(pattern), "%s\\*.json", dir);
    WIN32_FIND_DATAA fd;
    HANDLE h = FindFirstFileA(pattern, &fd);
    if (h == INVALID_HANDLE_VALUE) { *out_ids = NULL; *out_count = 0; return 0; }
    size_t cap = 16, n = 0;
    char** ids = (char**)malloc(cap * sizeof(char*));
    if (!ids) { FindClose(h); return -1; }
    do {
        if (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) continue;
        const char* name = fd.cFileName;
        size_t len = strlen(name);
        if (len <= 5 || strcmp(name + len - 5, ".json") != 0) continue;
        char* id = (char*)malloc(len - 4);
        if (!id) break;
        memcpy(id, name, len - 5);
        id[len - 5] = '\0';
        if (n >= cap) {
            char** t = (char**)realloc(ids, (cap *= 2) * sizeof(char*));
            if (!t) { free(id); break; }
            ids = t;
        }
        ids[n++] = id;
    } while (FindNextFileA(h, &fd));
    FindClose(h);
    *out_ids = ids;
    *out_count = n;
    return 0;
}
#else
#include <dirent.h>
static int scan_json_ids(const char* dir, char*** out_ids, size_t* out_count) {
    DIR* d = opendir(dir);
    if (!d) { *out_ids = NULL; *out_count = 0; return -1; }
    size_t cap = 16, n = 0;
    char** ids = (char**)malloc(cap * sizeof(char*));
    if (!ids) { closedir(d); return -1; }
    struct dirent* ent;
    while ((ent = readdir(d)) != NULL) {
        const char* name = ent->d_name;
        size_t len = strlen(name);
        if (len <= 5 || strcmp(name + len - 5, ".json") != 0) continue;
        char* id = (char*)malloc(len - 4);
        if (!id) continue;
        memcpy(id, name, len - 5);
        id[len - 5] = '\0';
        if (n >= cap) {
            char** t = (char**)realloc(ids, (cap *= 2) * sizeof(char*));
            if (!t) { free(id); break; }
            ids = t;
        }
        ids[n++] = id;
    }
    closedir(d);
    *out_ids = ids;
    *out_count = n;
    return 0;
}
#endif

int kos_ontology_registry_load_all(kos_ontology_registry_t* reg) {
    if (!reg || !reg->storage_dir) return -1;
    char** ids = NULL;
    size_t n = 0;
    if (scan_json_ids(reg->storage_dir, &ids, &n) != 0) return -1;
    for (size_t i = 0; i < n; i++) {
        TypeOntology* o = kos_ontology_registry_load_one(reg, ids[i]);
        free(ids[i]);
        if (!o) { /* 加载失败时继续 */ }
    }
    free(ids);
    return 0;
}

TypeOntology* kos_ontology_registry_load_one(kos_ontology_registry_t* reg, const char* id) {
    if (!reg || !id || !reg->storage_dir) return NULL;
    char path[1024];
    if (build_path(path, sizeof(path), reg->storage_dir, id) != 0) return NULL;
    TypeOntology* o = kos_ontology_load_from_file(path);
    if (!o) return NULL;
    TypeOntology* old = kos_ontology_registry_unregister(reg, id);
    if (old) kos_ontology_free(old);
    if (kos_ontology_registry_register(reg, id, o) != 0) {
        kos_ontology_free(o);
        return NULL;
    }
    return o;
}
