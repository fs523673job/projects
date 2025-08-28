@echo off
setlocal

:: Defina aqui os valores padrão para cada argumento
set "VRS_DEFAULT=Apdata_X64"
set "APP_DEFAULT=ApAccessServer"
set "NAME_DEFAULT=ApAccessServer"
set "DELPHIVERSION=22.0"

:: Check if a version argument was passed in
if not "%~2"=="" (
    set "VRS_DEFAULT=%~1"
    set "DELPHIVERSION=%~2"
) else (
    if not "%~1"=="" (
        if /I "%~1"=="DelphiTokio" (
            set "DELPHIVERSION=19.0"
        ) else if /I "%~1"=="DelphiAlexandria" (
            set "DELPHIVERSION=22.0"
        ) else (
            set "VRS_DEFAULT=%~1"
        )
    )
)

set "PROJECT_ROOT=%~dp0.."

:: Chama o primeiro script com os valores padrão
call "%PROJECT_ROOT%\cmdBAT\prun.bat" %VRS_DEFAULT% %APP_DEFAULT% %NAME_DEFAULT% %DELPHIVERSION%

endlocal