// 键盘快捷键工具

export interface KeyboardShortcut {
    key: string;
    ctrl?: boolean;
    shift?: boolean;
    alt?: boolean;
    meta?: boolean; // Cmd on macOS
}

export function matchesShortcut(e: KeyboardEvent, shortcut: KeyboardShortcut): boolean {
    if (shortcut.ctrl && !e.ctrlKey) return false;
    if (shortcut.shift && !e.shiftKey) return false;
    if (shortcut.alt && !e.altKey) return false;
    if (shortcut.meta && !e.metaKey) return false;
    
    // 如果指定了ctrl或meta，另一个不应该被按下
    if (shortcut.ctrl && e.metaKey) return false;
    if (shortcut.meta && e.ctrlKey) return false;
    
    return e.key.toLowerCase() === shortcut.key.toLowerCase();
}

export const Shortcuts = {
    SAVE: { key: 's', ctrl: true, meta: true },
    OPEN: { key: 'o', ctrl: true, meta: true },
    COMMAND_PALETTE: { key: 'p', ctrl: true, meta: true },
    COMPILE: { key: 'F5' },
    NEW_FILE: { key: 'n', ctrl: true, meta: true }
};





