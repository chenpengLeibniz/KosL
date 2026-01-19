#!/usr/bin/env python3
# tools/verify_ontology_json.py
# 验证本体JSON文件的完整性

import json
import sys
import os

def verify_ontology_json(json_file):
    """验证本体JSON文件"""
    if not os.path.exists(json_file):
        print(f"Error: File {json_file} does not exist")
        return False
    
    try:
        with open(json_file, 'r', encoding='utf-8') as f:
            data = json.load(f)
    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON format: {e}")
        return False
    except Exception as e:
        print(f"Error reading file: {e}")
        return False
    
    # 检查基本字段
    domain_name = data.get('domain_name', '')
    type_count = data.get('type_count', 0)
    type_definitions = data.get('type_definitions', [])
    
    print(f"Domain: {domain_name}")
    print(f"type_count field: {type_count}")
    print(f"Actual type definitions in array: {len(type_definitions)}")
    
    if type_count != len(type_definitions):
        print(f"WARNING: type_count ({type_count}) does not match actual array length ({len(type_definitions)})")
    
    # 列出所有类型名称
    type_names = [t.get('name', 'N/A') for t in type_definitions]
    print(f"\nTotal unique type names: {len(set(type_names))}")
    
    # 显示前20个和最后20个类型
    print("\nFirst 20 type names:")
    for i, name in enumerate(type_names[:20]):
        print(f"  {i+1}. {name}")
    
    print(f"\nLast 20 type names:")
    for i, name in enumerate(type_names[-20:]):
        print(f"  {len(type_names)-19+i}. {name}")
    
    # 检查重复类型名称
    from collections import Counter
    name_counts = Counter(type_names)
    duplicates = {name: count for name, count in name_counts.items() if count > 1}
    if duplicates:
        print(f"\nWARNING: Found {len(duplicates)} duplicate type names:")
        for name, count in list(duplicates.items())[:10]:
            print(f"  {name}: {count} times")
    
    return True

if __name__ == "__main__":
    json_file = sys.argv[1] if len(sys.argv) > 1 else "build/bin/Release/manufacturing_ontology.json"
    verify_ontology_json(json_file)



























