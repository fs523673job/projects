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

if "%versionChoice%"=="0" goto end
if "%versionChoice%"=="1" set "delphiVersion=alexandria" & goto architecture
if "%versionChoice%"=="2" set "delphiVersion=tokyo" & goto architecture

echo Escolha inv�lida. Tente novamente.
goto version

:architecture
echo Escolha a arquitetura:

echo 0. Sair
echo 1. Win32
echo 2. Win64

set /p archChoice=Digite o n�mero correspondente a sua escolha: 

if "%archChoice%"=="0" goto end
if "%archChoice%"=="1" set "architecture=Win32" & goto buildtype
if "%archChoice%"=="2" set "architecture=Win64" & goto buildtype

echo Escolha inv�lida. Tente novamente.
goto architecture

:buildtype
echo Escolha o tipo de build:

echo 0. Sair
echo 1. Release
echo 2. Debug

set /p buildChoice=Digite o n�mero correspondente a sua escolha: 

if "%buildChoice%"=="0" goto end
if "%buildChoice%"=="1" set "buildType=Release" & goto menu
if "%buildChoice%"=="2" set "buildType=Debug" & goto menu

echo Escolha inv�lida. Tente novamente.
goto buildtype

:menu
echo Escolha a inst�ncia que deseja rodar:

echo 0. Sair
echo 1. Oracle
echo 2. SQL Server
echo 3. Localhost

set /p choice=Digite o n�mero correspondente a sua escolha: 

if "%choice%"=="0" goto end
if "%choice%"=="1" set "instance=oracle" & goto execute
if "%choice%"=="2" set "instance=sqlserver" & goto execute
if "%choice%"=="3" set "instance=localhost" & goto execute

echo Escolha inv�lida. Tente novamente.
goto menu

:execute
:: Monta o diret�rio base
if "%delphiVersion%"=="alexandria" (
    set "baseDir=C:\apdata_x64\aplicacoes\apserver\bin"
) else if "%delphiVersion%"=="tokyo" (
    set "baseDir=C:\apdata_xwt\aplicacoes\apserver\bin"
)

:: Monta o caminho completo com base na arquitetura e no tipo de build
set "fullDir=%baseDir%\%architecture%\%buildType%"

echo.
echo Executando ApServer na inst�ncia %instance% com %architecture% em modo %buildType%
echo Diretorio: %fullDir%
echo ApServer %instance% /start /enablelog /integration_timeout 130

start "" "%fullDir%\ApServer.exe" %instance% /start /enablelog /integration_timeout 130

goto end

:end
endlocal
exit
