@echo off
setlocal

:: Verifica se um argumento foi passado
if "%~1"=="" (
    echo Uso: %~0 aplicativo
    exit /b
)

:: Define o aplicativo para ser executado
set "APP=%~1"

:: Define a arquitetura 
set "ARQ=%~2"

set "APP_PATH=C:\Apdata_X64\Aplicacoes\%APP%\bin\Win32\%ARQ%\%APP%.exe"

:: Mapeia os nomes dos aplicativos para seus caminhos
if /I "%APP%"=="ApTools" (
    set "APP_PATH=C:\Apdata_X64\Aplicacoes\ApTools\bin\Win32\ARQ\ApTools.exe"
) else if /I "%APP%"=="ApServer" (
    set "APP_PATH=C:\Caminho\Para\ApServer.exe"
) else (
    echo Aplicativo desconhecido: %APP%
    exit /b
)

:: Verifica se um segundo argumento foi passado e ajusta o caminho do aplicativo
if not "%~2"=="" (
    if /I "%~2"=="Tokyo" (
        set "APP_PATH=C:\Apdata_TX64\Aplicacoes\%APP%\bin\Win32\Debug\%APP%.exe"
    ) else (
        echo Cidade desconhecida: %~2
        exit /b
    )
)

:: Executa o aplicativo
start "" "%APP_PATH%"

endlocal
