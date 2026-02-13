# Monograph 文档目录

本目录用于 LaTeX 编写 KOS-TL 相关文档。

## 文件说明

| 文件 | 说明 |
|------|------|
| `Coc.tex` | 类型检查器/构造演算（CoC）中文图书，参考样式 |
| `Kos.tex` | KOS-TL 论文（article 格式） |
| `KOS-TLv1.tex` | 知行逻辑章节内容（作为 `\input` 引入） |
| `KOS-TL-Book.tex` | 知行逻辑图书主文件（book 格式，参考 Coc.tex 样式） |

## 编译 KOS-TL 图书

使用 pdflatex 编译（需安装 ctex、tikz、booktabs 等包）：

```bash
cd monograph
pdflatex KOS-TL-Book.tex
pdflatex KOS-TL-Book.tex   # 第二次以生成正确目录
```

若 `KOS-TL-Book.pdf` 被占用（如 PDF 查看器打开），可使用不同输出名：

```bash
pdflatex -jobname=KOS-TL-Book-Out KOS-TL-Book.tex
pdflatex -jobname=KOS-TL-Book-Out KOS-TL-Book.tex
```

输出为 `KOS-TL-Book-Out.pdf`。第二次运行用于解析交叉引用与参考文献。

## 参考文献

主文件 `KOS-TL-Book.tex` 的 \texttt{thebibliography} 环境包含：
- Chenpeng2026：KOS-TL 技术报告
- Davidson1967：事件本体论
- Girard1972：Tait-Girard 强正规化方法
- Girard1989：Proofs and Types
- MartinLof1984：直觉主义类型论

## 样式说明

`KOS-TL-Book.tex` 参考 `Coc.tex` 的中文图书样式：
- `\documentclass[10pt]{book}`
- ctex UTF8 中文支持
- Springer 风格版心（155×235mm）
- 定理环境：Definition、Theorem、Lemma、Example 等
