@echo off
setlocal

::Abrir o delphi com o projeto do ApDeveloper

:: Defina aqui os valores padrão para cada argumento
set "VRS_DEFAULT=Apdata_X64"
set "APP_DEFAULT=ApDeveloper"
set "NAME_DEFAULT=ApDeveloper"

:: Check if a version argument was passed in
if "%~1"=="" (
    set "DELPHIVERSION=22.0"
) else (
    if /I "%~1"=="DelphiTokio" (
        set "DELPHIVERSION=19.0"
    ) else if /I "%~1"=="DelphiAlexandria" (
        set "DELPHIVERSION=22.0"
    )
)

set "PROJECT_ROOT=%~dp0.."

:: Chama o primeiro script com os valores padrão
call "%PROJECT_ROOT%\cmdBAT\prun.bat" %VRS_DEFAULT% %APP_DEFAULT% %NAME_DEFAULT% %DELPHIVERSION%

endlocal