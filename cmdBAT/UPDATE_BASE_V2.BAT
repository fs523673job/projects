@echo off

if not defined ScriptsBase (
    echo ERRO: a variável de ambiente ScriptsBase não está definida.
    exit /b 1
)

python "%ScriptsBase%\cmdPython\UpdateDatabase.py"