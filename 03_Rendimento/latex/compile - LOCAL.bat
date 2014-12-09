echo off
prompt $G
cls

del *.bbl
del *.aux
latex document -quiet 
bibtex document -quiet
latex document -quiet 
latex document -quiet 

REM dvips document
REM ps2pdf document.ps
pdflatex document -quiet 

REM dvipdfm document.dvi

del temp\*.* /q
move *.aux /temp
move *.bbl /temp
move *.blg /temp
move *.dvi /temp
move *.log /temp

echo We're done.
document.pdf
quit
