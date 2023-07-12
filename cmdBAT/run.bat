@echo off
setlocal

:: Define a versão
set "VRS=%1"

:: Define o aplicativo para ser executado
set "APP=%~2"

:: Define a arquitetura 
set "ARQ=%~3"

:: Define o build 
set "BLD=%~4"

:: Define o Params Executável
set "PRN=%~5"

:: Define o caminho para o executável
set "APP_PATH=C:\%VRS%\Aplicacoes\%APP%\bin\%ARQ%\%BLD%\%APP%.exe"

if not exist "%APP_PATH%" (
    echo O executavel %APP_PATH% nao existe.
    echo Possiveis causas: Nao foi compilado
    echo Deseja compilar o aplicativo novamente? [S/N]
    choice /C SN /N
    if errorlevel 2 exit /b
    call cmdd
    exit /b
)

:: Executa o aplicativo com os parâmetros
echo "%APP_PATH%" %PRN%
start "" "%APP_PATH%" %PRN%

endlocal
