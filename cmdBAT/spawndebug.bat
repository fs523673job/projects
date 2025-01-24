@echo off
setlocal enabledelayedexpansion

::Exemplo de uso: spawdebug.bat ApData_x64 Win32 Debug 7002

set "VRS_DEFAULT=Apdata_X64"
set "ARQ_DEFAULT=Win64"
set "APP_DEFAULT=ApServer"
set "BLD_DEFAULT=Debug"
set "PORT_DEFAULT=7001"

set "VRS=%~1"
if "%VRS%"=="" set "VRS=%VRS_DEFAULT%"

echo VR

set "ARQ=%~2"
if "%ARQ%"=="" set "ARQ=%ARQ_DEFAULT%"

set "BLD=%~3"
if "%BLD%"=="" set "BLD=%BLD_DEFAULT%"

set "PORT=%~4"
if "%PORT%"=="" set "PORT=%PORT_DEFAULT%"

set "PRN_DEFAULT=localhost /start /spawndebug %PORT%"

call "C:\github\fs523673job\projects\cmdBAT\run.bat" %VRS% %APP_DEFAULT% %APP_DEFAULT% %ARQ% %BLD% "%PRN_DEFAULT%"

if errorlevel 1 (
    echo Erro ao executar run.bat.
    goto fim
)
