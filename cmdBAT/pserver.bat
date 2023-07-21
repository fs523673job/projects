@echo off
setlocal

:: Defina aqui os valores padrão para cada argumento
set "VRS_DEFAULT=Apdata_X64"
set "APP_DEFAULT=ApServer"
set "NAME_DEFAULT=ApServer"
set "DELPHIVERSION=22.0"

:: Chama o primeiro script com os valores padrão
call "C:\github\fs523673job\projects\cmdBAT\prun.bat" %VRS_DEFAULT% %APP_DEFAULT% %NAME_DEFAULT% %DELPHIVERSION%

endlocal