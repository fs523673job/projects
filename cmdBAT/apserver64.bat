@echo off
setlocal

:: Defina aqui os valores padrão para cada argumento
set "VRS_DEFAULT=Apdata_X64"
set "APP_DEFAULT=ApServer"
set "ARQ_DEFAULT=Win64"
set "BLD_DEFAULT=Debug"
set "PRN_DEFAULT=localhost /start /enablelog /integration_timeout 130"

if not "%~1"=="" set "BLD_DEFAULT=%~1"

:: Chama o primeiro script com os valores padrão
call "C:\github\fs523673job\projects\cmdBAT\run.bat" %VRS_DEFAULT% %APP_DEFAULT% %APP_DEFAULT% %ARQ_DEFAULT% %BLD_DEFAULT% "%PRN_DEFAULT%"

endlocal