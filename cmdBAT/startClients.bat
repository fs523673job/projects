@echo off
chcp 1252 >nul
setlocal

cd /d %~dp0

:version
echo Escolha a vers�o do Delphi:

echo 0. Sair
echo 1. Alexandria
echo 2. Tokyo

set /p versionChoice=Digite o n�mero correspondente a sua escolha: 

if "%versionChoice%"=="0" goto fim
if "%versionChoice%"=="1" set "delphiVersion=alexandria" & goto buildtype
if "%versionChoice%"=="2" set "delphiVersion=tokyo" & goto buildtype

echo Escolha inv�lida. Tente novamente.
goto version

:buildtype
echo Escolha o tipo de build:

echo 0. Sair
echo 1. Release
echo 2. Debug

set /p buildChoice=Digite o n�mero correspondente a sua escolha: 

if "%buildChoice%"=="0" goto fim
if "%buildChoice%"=="1" set "buildType=Release" & goto menu
if "%buildChoice%"=="2" set "buildType=Debug" & goto menu

echo Escolha inv�lida. Tente novamente.
goto buildtype

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

:: Monta o diret�rio base para Win32
if "%delphiVersion%"=="alexandria" (
    set "baseDir=C:\apdata_x64\aplicacoes\aptools\bin"
) else if "%delphiVersion%"=="tokyo" (
    set "baseDir=C:\apdata_xwt\aplicacoes\aptools\bin"
)

:: Monta o caminho completo com base no tipo de build
set "fullDir=%baseDir%\Win32\%buildType%"

start "" "%fullDir%\ApTools.exe"
goto fim

:apmanager
echo Iniciando ApManager...

:: Monta o diret�rio base para Win32
if "%delphiVersion%"=="alexandria" (
    set "baseDir=C:\apdata_x64\aplicacoes\apmanager\bin"
) else if "%delphiVersion%"=="tokyo" (
    set "baseDir=C:\apdata_xwt\aplicacoes\apmanager\bin"
)

:: Monta o caminho completo com base no tipo de build
set "fullDir=%baseDir%\Win32\%buildType%"

start "" "%fullDir%\ApManager.exe"
goto fim

:sair
echo Saindo...
goto fim

:fim
endlocal
exit
