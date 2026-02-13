@echo off
REM 仅用 pdflatex 编译，不运行 BibTeX（本稿使用手写参考文献，避免 BibTeX Buffer size exceeded）
cd /d "%~dp0"

if exist KOS-TL-Book.aux del KOS-TL-Book.aux
if exist KOS-TL-Book.out del KOS-TL-Book.out

echo Running pdflatex (1/2)...
pdflatex -interaction=nonstopmode KOS-TL-Book.tex
if errorlevel 1 exit /b 1

echo Running pdflatex (2/2)...
pdflatex -interaction=nonstopmode KOS-TL-Book.tex

echo Done. Open KOS-TL-Book.pdf
pause
