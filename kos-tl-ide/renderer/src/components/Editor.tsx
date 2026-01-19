import React, { useEffect, useRef } from 'react';
import * as monaco from 'monaco-editor';
import { registerKOSTLLanguage } from '../monaco/kos-tl-language';

interface EditorProps {
    value: string;
    onChange: (value: string) => void;
    errors?: any[];
    filePath?: string;
    onHover?: (position: monaco.Position) => void;
}

export default function Editor({ value, onChange, errors = [], onHover, filePath }: EditorProps) {
    const editorRef = useRef<HTMLDivElement>(null);
    const monacoEditorRef = useRef<monaco.editor.IStandaloneCodeEditor | null>(null);
    const modelRef = useRef<monaco.editor.ITextModel | null>(null);
    const isUpdatingRef = useRef(false);

    // 初始化编辑器（只运行一次）
    useEffect(() => {
        if (!editorRef.current) return;

        // 注册KOS-TL语言
        registerKOSTLLanguage();

        // 创建Monaco编辑器（先不设置模型）
        const editor = monaco.editor.create(editorRef.current, {
            theme: 'vs-dark',
            automaticLayout: true,
            minimap: { enabled: true },
            fontSize: 14,
            lineNumbers: 'on',
            roundedSelection: false,
            scrollBeyondLastLine: false,
            readOnly: false,
            cursorStyle: 'line',
            wordWrap: 'on',
            suggestOnTriggerCharacters: true,
            quickSuggestions: true,
            parameterHints: { enabled: true },
            hover: { enabled: true }
        });

        monacoEditorRef.current = editor;

        // 监听内容变化
        editor.onDidChangeModelContent(() => {
            if (!isUpdatingRef.current && editor.getModel()) {
                const newValue = editor.getValue();
                onChange(newValue);
            }
        });

        // 监听鼠标悬停
        if (onHover) {
            editor.onMouseMove((e) => {
                if (e.target.position) {
                    onHover(e.target.position);
                }
            });
        }

        return () => {
            if (monacoEditorRef.current) {
                monacoEditorRef.current.dispose();
                monacoEditorRef.current = null;
            }
        };
    }, []);

    // 当文件路径或内容改变时，更新模型
    useEffect(() => {
        if (!monacoEditorRef.current) {
            console.log('[Editor] Editor not initialized yet');
            return;
        }

        const editor = monacoEditorRef.current;
        console.log('[Editor] Updating model - filePath:', filePath, 'value length:', value.length);
        
        isUpdatingRef.current = true;

        try {
            let model: monaco.editor.ITextModel | null = null;

            if (filePath) {
                // 使用文件路径创建URI
                const uri = monaco.Uri.file(filePath);
                console.log('[Editor] File URI:', uri.toString());
                
                // 检查是否已存在该URI的模型
                model = monaco.editor.getModel(uri);
                
                if (!model) {
                    // 创建新模型
                    console.log('[Editor] Creating new model for file:', filePath, 'with content length:', value.length);
                    model = monaco.editor.createModel(value, 'kos-tl', uri);
                    console.log('[Editor] Model created, value length:', model.getValue().length);
                } else {
                    // 更新现有模型的内容（如果内容不同）
                    const currentModelValue = model.getValue();
                    if (currentModelValue !== value) {
                        console.log('[Editor] Updating model content - old length:', currentModelValue.length, 'new length:', value.length);
                        // 使用 pushEditOperations 来保持撤销历史，但这里简单使用 setValue
                        model.pushEditOperations(
                            [],
                            [{
                                range: model.getFullModelRange(),
                                text: value
                            }],
                            () => null
                        );
                        console.log('[Editor] Model content updated');
                    } else {
                        console.log('[Editor] Model content unchanged');
                    }
                }
            } else {
                // 没有文件路径，使用临时URI
                const uri = monaco.Uri.parse('untitled://untitled.kos');
                model = monaco.editor.getModel(uri);
                
                if (!model) {
                    console.log('[Editor] Creating new untitled model');
                    model = monaco.editor.createModel(value, 'kos-tl', uri);
                } else {
                    if (model.getValue() !== value) {
                        console.log('[Editor] Updating untitled model content');
                        model.pushEditOperations(
                            [],
                            [{
                                range: model.getFullModelRange(),
                                text: value
                            }],
                            () => null
                        );
                    }
                }
            }

            // 如果模型改变了，切换模型
            if (model && model !== modelRef.current) {
                console.log('[Editor] Switching model - old:', modelRef.current?.uri.toString(), 'new:', model.uri.toString());
                
                // 保存旧模型的光标位置（如果有）
                const oldModel = modelRef.current;
                let position: monaco.Position | null = null;
                let scrollTop = 0;
                let scrollLeft = 0;
                
                if (oldModel && editor.getModel() === oldModel) {
                    position = editor.getPosition();
                    scrollTop = editor.getScrollTop();
                    scrollLeft = editor.getScrollLeft();
                }

                // 切换模型
                editor.setModel(model);
                modelRef.current = model;

                // 恢复光标位置（如果可能）
                if (position) {
                    editor.setPosition(position);
                    editor.setScrollTop(scrollTop);
                    editor.setScrollLeft(scrollLeft);
                } else {
                    // 移动到文件开头
                    editor.setPosition({ lineNumber: 1, column: 1 });
                    editor.setScrollTop(0);
                    editor.setScrollLeft(0);
                }

                console.log('[Editor] Switched to model:', filePath || 'untitled', 'model value length:', model.getValue().length);
            } else if (model && model === modelRef.current) {
                // 模型相同，但内容可能已更新
                const currentValue = model.getValue();
                if (currentValue !== value) {
                    console.log('[Editor] Same model, updating content');
                    // 保存光标位置
                    const position = editor.getPosition();
                    const scrollTop = editor.getScrollTop();
                    const scrollLeft = editor.getScrollLeft();
                    
                    // 更新内容
                    model.pushEditOperations(
                        [],
                        [{
                            range: model.getFullModelRange(),
                            text: value
                        }],
                        () => null
                    );
                    
                    // 恢复光标位置
                    if (position) {
                        editor.setPosition(position);
                        editor.setScrollTop(scrollTop);
                        editor.setScrollLeft(scrollLeft);
                    }
                } else {
                    console.log('[Editor] Model and content unchanged');
                }
            } else {
                console.log('[Editor] No model to set');
            }
        } catch (error) {
            console.error('[Editor] Error updating model:', error);
        } finally {
            isUpdatingRef.current = false;
        }
    }, [filePath, value]);

    // 更新错误标记
    useEffect(() => {
        const model = modelRef.current || monacoEditorRef.current?.getModel();
        if (!model) return;

        const markers: monaco.editor.IMarkerData[] = errors.map(error => ({
            severity: error.severity === 'error' 
                ? monaco.MarkerSeverity.Error 
                : monaco.MarkerSeverity.Warning,
            startLineNumber: (error.line || 0) + 1,
            startColumn: (error.column || 0) + 1,
            endLineNumber: (error.line || 0) + 1,
            endColumn: (error.column || 0) + (error.length || 1) + 1,
            message: error.message || String(error)
        }));

        monaco.editor.setModelMarkers(model, 'kos-tl', markers);
    }, [errors]);

    return <div ref={editorRef} style={{ width: '100%', height: '100%' }} />;
}
