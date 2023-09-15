@echo off
setlocal enabledelayedexpansion

:: O primeiro argumento (opcional) será o número de vezes que o script será executado. 
:: Se não for fornecido, assumiremos o valor 3 como padrão.
if not "%~1"=="" (
    set "LOOPS=%~1"
) else (
    set "LOOPS=3"
)

:: O segundo argumento (opcional) será VRS_DEFAULT. Se não for fornecido, usaremos 'Apdata_X64' como padrão
if not "%~2"=="" (
    set "VRS_DEFAULT=%~2"
) else (
    set "VRS_DEFAULT=Apdata_X64"
)

set "APP_DEFAULT=ApLoadBalancerServer"
set "ARQ_DEFAULT=Win32"
set "BLD_DEFAULT=Debug"
set "DIR_DEFAULT=ApLoadBalancer"
set "PRN_DEFAULT=localhost /start"

:: Start LoadBalancer
call "C:\github\fs523673job\projects\cmdBAT\run.bat" %VRS_DEFAULT% %APP_DEFAULT% %DIR_DEFAULT% %ARQ_DEFAULT% %BLD_DEFAULT% "%PRN_DEFAULT%"

set "APP_DEFAULT=ApServer"

:: Valor inicial para o spawndebug
set "SPAWNDEBUG_START=7001"

for /L %%i in (1,1,%LOOPS%) do (
    set /a "SPAWNDEBUG_VAL=SPAWNDEBUG_START+%%i-1"
    set "PRN_DEFAULT=localhost /start /spawndebug !SPAWNDEBUG_VAL!"
    
    echo Calling with: !PRN_DEFAULT!
    call "C:\github\fs523673job\projects\cmdBAT\run.bat" %VRS_DEFAULT% %APP_DEFAULT% %APP_DEFAULT% %ARQ_DEFAULT% %BLD_DEFAULT% "!PRN_DEFAULT!"
)

endlocal


