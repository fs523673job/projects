@echo off
setlocal

:: Define a versão
set "VRS=%1"

:: Define o aplicativo para ser executado
set "APP=%~2"

:: Define o dpr 
set "NAMEDRP=%~3"

::Define bds version
set "DELPHIVERSION=%4~"

::Define path version delphi
set "DELPHIVERSION=C:\Program Files (x86)\Embarcadero\Studio\%DELPHIVERSION%\bin"

:: Define o caminho para o projeto
set "APP_PATH=C:\%VRS%\Aplicacoes\%APP%\Source\%NAMEDRP%.dproj"

if not exist "%APP_PATH%" (
    echo O projeto %APP_PATH% nao existe.
    exit /b
)

:: Abre o projeto no IDE Delphi
echo %DELPHIVERSION%\bds.exe -ns -pDelphi "%APP_PATH%"
start "" %DELPHIVERSION%\bds.exe -ns -pDelphi "%APP_PATH%"

endlocal