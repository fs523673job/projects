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

:: Verifica se o arquivo ApTools.exe ou ApManager.exe existe no diretório escolhido
if not exist "%~dp0%chosenDir%\ApTools.exe" if not exist "%~dp0%chosenDir%\ApManager.exe" (
    echo Nenhum arquivo ApTools.exe ou ApManager.exe foi encontrado no diretório escolhido. Tente novamente.
    goto fim
)

:: Muda para o diretório escolhido
cd /d "%~dp0%chosenDir%"

:menu
echo =======================
echo        Menu
echo =======================
echo.
echo 0: Sair
echo 1: Aptools
echo 2: ApManager
echo.

choice /c 012 /n /m "Escolha uma opção: "

if errorlevel 3 goto apmanager
if errorlevel 2 goto aptools
if errorlevel 1 goto sair

goto menu

:aptools
echo Iniciando Aptools...
start "" "ApTools.exe"
goto fim

:apmanager
echo Iniciando ApManager...
start "" "ApManager.exe"
goto fim

:sair
echo Saindo...
goto fim

:fim
endlocal
exit
