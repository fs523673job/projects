@echo off
setlocal

::Executar o ApIntegrationServer

:: Defina aqui os valores padrão para cada argumento
set "VRS_DEFAULT=Apdata_X64"
set "APP_DEFAULT=ApIntegrationServer"
set "ARQ_DEFAULT=Win64"
set "BLD_DEFAULT=Debug"
set "PRN_DEFAULT=localhost /start /enablelog /enableConsole /LogWSFile LogRequest\Request.txt" 

::if not "%~1"=="" set "BLD_DEFAULT=%~1"

if not "%~2"=="" (
    set "VRS_DEFAULT=%~1"
    set "BLD_DEFAULT=%~2"
) else (
    if not "%~1"=="" (
        if /I "%~1"=="Debug" (
            set "BLD_DEFAULT=Debug"
        ) else if /I "%~1"=="Release" (
            set "BLD_DEFAULT=Release"
        ) else (
            set "VRS_DEFAULT=%~1"
        )
    )
)

set "PROJECT_ROOT=%~dp0.."

:: Chama o primeiro script com os valores padrão
call "%PROJECT_ROOT%\cmdBAT\run.bat" %VRS_DEFAULT% %APP_DEFAULT% %APP_DEFAULT% %ARQ_DEFAULT% %BLD_DEFAULT% "%PRN_DEFAULT%"

endlocal
