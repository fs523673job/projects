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

:: Verifica se o arquivo ApTools.exe ou ApManager.exe existe no diret�rio escolhido
if not exist "%~dp0%chosenDir%\ApTools.exe" if not exist "%~dp0%chosenDir%\ApManager.exe" (
    echo Nenhum arquivo ApTools.exe ou ApManager.exe foi encontrado no diret�rio escolhido. Tente novamente.
    goto fim
)

:: Muda para o diret�rio escolhido
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

choice /c 012 /n /m "Escolha uma op��o: "

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
