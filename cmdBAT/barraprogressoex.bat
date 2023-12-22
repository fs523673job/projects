@echo off
setlocal EnableDelayedExpansion

set /a "total=100"
set "progressbarLabel=Progresso"
set "progressbarWidth=50"

:loop
set /a "completed=CurrentValue * progressbarWidth / total"
set /a "remaining=progressbarWidth - completed"

set "progressBar=["
for /l %%i in (1,1,%completed%) do set "progressBar=!progressBar!-"
for /l %%i in (%completed%,1,%progressbarWidth%) do set "progressBar=!progressBar! "

set /a "progressPercentage=CurrentValue * 100 / total"

<nul set /p ".=!progressBar!] !progressPercentage!%% !progressbarLabel!" <nul
set /a "CurrentValue+=1"

timeout /t 1 >nul
cls
if %CurrentValue% leq %total% goto loop

echo.
