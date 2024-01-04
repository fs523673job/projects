@echo off
setlocal

::Abrir o delphi com o projeto do ApTools

:: Defina aqui os valores padrão para cada argumento
set "VRS_DEFAULT=Apdata_X64"
set "APP_DEFAULT=ApADIntegratorWS"
set "NAME_DEFAULT=ApADIntegratorWS"

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

:: Chama o primeiro script com os valores padrão
call "C:\github\fs523673job\projects\cmdBAT\prun.bat" %VRS_DEFAULT% %APP_DEFAULT% %NAME_DEFAULT% %DELPHIVERSION%

endlocal