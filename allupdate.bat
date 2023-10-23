@echo off
setlocal

:: Configuração do repositório
set REPO_PATH=C:\github\fs523673job\projects

:: Atualizando o repositório
cd /d %REPO_PATH%
git pull

:: Executando o script update.bat em cmdSQL
cd /d %REPO_PATH%\cmdSQL
call update.bat

:: Executando o script update.bat em cmdGIT
cd /d %REPO_PATH%\cmdGIT
call update.bat

:: Executando o script update.bat em cmdGIT
cd /d %REPO_PATH%\cmdScripter
call update.bat


:: Adicionando todas as mudanças no repositório
cd /d %REPO_PATH%
git add --all

:: Criando uma mensagem de commit com a data e hora atuais
set DATE_TIME=%date:~6,4%-%date:~3,2%-%date:~0,2% %time:~0,2%:%time:~3,2%:%time:~6,2%
set COMMIT_MSG=Commit realizado em %DATE_TIME%

:: Realizando o commit
git commit -m "%COMMIT_MSG%"

:: Empurrando as mudanças para o servidor
git push

endlocal
