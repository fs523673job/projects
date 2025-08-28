@echo off
setlocal

:: Defina aqui os valores padrão para cada argumento
set "VRS_DEFAULT=Apdata_X64"
set "APP_DEFAULT=ApAccessServer"
set "ARQ_DEFAULT=Win32"
set "BLD_DEFAULT=Debug"
set "PRN_DEFAULT=LOCALHOST /start /LOGERRORS"

set "PROJECT_ROOT=%~dp0.."

:: Chama o primeiro script com os valores padrão
call "%PROJECT_ROOT%\cmdBAT\run.bat" %VRS_DEFAULT% %APP_DEFAULT% %APP_DEFAULT% %ARQ_DEFAULT% %BLD_DEFAULT% "%PRN_DEFAULT%"

endlocal
