@echo off
REM USO:
REM rodar_teste.bat <HEADLESS> <URL> <TIMES>
REM Exemplo:
REM rodar_teste.bat true "http://localhost/559" 3

set HEADLESS=%1
set URL=%2
set TIMES=%3

python "C:\github\fs523673job\projects\cmdPython\teste_login.py" --headless %HEADLESS% --url "%URL%" --times %TIMES%

