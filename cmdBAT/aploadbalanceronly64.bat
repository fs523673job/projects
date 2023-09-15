@echo off
setlocal

:: Defina aqui os valores padrão para cada argumento
set "VRS_DEFAULT=Apdata_X64"
set "APP_DEFAULT=ApLoadBalancerServer"
set "DIR_DEFAULT=ApLoadBalancer"
set "ARQ_DEFAULT=Win64"
set "BLD_DEFAULT=Debug"
set "PRN_DEFAULT=localhost /start"

:: Chama o primeiro script com os valores padrão
call "C:\github\fs523673job\projects\cmdBAT\run.bat" %VRS_DEFAULT% %APP_DEFAULT% %DIR_DEFAULT% %ARQ_DEFAULT% %BLD_DEFAULT% "%PRN_DEFAULT%"

endlocal
