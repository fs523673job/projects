@echo off
chcp 1252 >nul
setlocal enabledelayedexpansion

:: Define o diret�rio onde o script est� localizado
cd /d %~dp0

:: Vari�veis para controle
set count=0
set choice=

:: Lista todos os diret�rios no diret�rio atual
for /d %%d in (*) do (
    set /a count+=1
    echo !count!: %%d
    set dir!count!=%%d
)

:: Se n�o houver diret�rios, sair
if %count%==0 (
    echo Nenhum diret�rio encontrado.
    goto fim
)

:: Pergunta ao usu�rio para escolher um diret�rio
set /p choice=Digite o n�mero correspondente ao diret�rio que deseja acessar: 

:: Verifica se a escolha � v�lida
if %choice% gtr %count% (
    echo Escolha inv�lida.
    goto fim
)

:: Define o diret�rio escolhido
set chosenDir=!dir%choice%!
echo Voc� escolheu o diret�rio: %chosenDir%

:: Verifica se o arquivo ApServer.exe existe no diret�rio escolhido
if not exist "%~dp0%chosenDir%\ApServer.exe" (
    echo O arquivo ApServer.exe n�o foi encontrado no diret�rio escolhido. Tente novamente.
    goto fim
)

:: Muda para o diret�rio escolhido
cd /d "%~dp0%chosenDir%"

:menu
echo ==============================
echo Escolha a inst�ncia que deseja rodar:
echo ==============================
echo 0. Sair
echo 1. Oracle
echo 2. SQL Server
echo 3. Localhost

set /p instanceChoice=Digite o n�mero correspondente � inst�ncia que deseja rodar: 

if "%instanceChoice%"=="0" goto end
if "%instanceChoice%"=="1" goto oracle
if "%instanceChoice%"=="2" goto sqlserver
if "%instanceChoice%"=="3" goto localhost

echo Escolha inv�lida. Tente novamente.
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
