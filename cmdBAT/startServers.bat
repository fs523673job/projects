@echo off
chcp 1252 >nul
setlocal enabledelayedexpansion

:: Define o diretório onde o script está localizado
cd /d %~dp0

:: Variáveis para controle
set count=0
set choice=

:: Lista todos os diretórios no diretório atual
for /d %%d in (*) do (
    set /a count+=1
    echo !count!: %%d
    set dir!count!=%%d
)

:: Se não houver diretórios, sair
if %count%==0 (
    echo Nenhum diretório encontrado.
    goto fim
)

:: Pergunta ao usuário para escolher um diretório
set /p choice=Digite o número correspondente ao diretório que deseja acessar: 

:: Verifica se a escolha é válida
if %choice% gtr %count% (
    echo Escolha inválida.
    goto fim
)

:: Define o diretório escolhido
set chosenDir=!dir%choice%!
echo Você escolheu o diretório: %chosenDir%

:: Verifica se o arquivo ApServer.exe existe no diretório escolhido
if not exist "%~dp0%chosenDir%\ApServer.exe" (
    echo O arquivo ApServer.exe não foi encontrado no diretório escolhido. Tente novamente.
    goto fim
)

:: Muda para o diretório escolhido
cd /d "%~dp0%chosenDir%"

:menu
echo ==============================
echo Escolha a instância que deseja rodar:
echo ==============================
echo 0. Sair
echo 1. Oracle
echo 2. SQL Server
echo 3. Localhost

set /p instanceChoice=Digite o número correspondente à instância que deseja rodar: 

if "%instanceChoice%"=="0" goto end
if "%instanceChoice%"=="1" goto oracle
if "%instanceChoice%"=="2" goto sqlserver
if "%instanceChoice%"=="3" goto localhost

echo Escolha inválida. Tente novamente.
goto menu

:oracle
echo Iniciando ApServer oracle /start /enablelog /integration_timeout 130
start "" "ApServer.exe" oracle /start /enablelog /integration_timeout 130
goto end

:sqlserver
echo Iniciando ApServer sqlserver /start /enablelog /integration_timeout 130
start "" "ApServer.exe" sqlserver /start /enablelog /integration_timeout 130
goto end

:localhost
echo Iniciando ApServer localhost /start /enablelog /integration_timeout 130
start "" "ApServer.exe" localhost /start /enablelog /integration_timeout 130
goto end

:end
endlocal
exit

:fim
endlocal
exit
