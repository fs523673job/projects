@echo off
setlocal

:: Defina aqui os valores padrão para cada argumento
set "VRS_DEFAULT=Apdata_X64"
set "APP_DEFAULT=ApServer"
set "ARQ_DEFAULT=Win32"
set "BLD_DEFAULT=Debug"
set "PRN_DEFAULT=localhost /start /enablelog /UseHTTPS /integration_timeout 130"

:: Chama o primeiro script com os valores padrão
call "C:\github\fs523673job\projects\cmdBAT\run.bat" %VRS_DEFAULT% %APP_DEFAULT% %APP_DEFAULT% %ARQ_DEFAULT% %BLD_DEFAULT% "%PRN_DEFAULT%"

endlocal