@echo off
setlocal enabledelayedexpansion

REM Loop de 1 a 1000
for /l %%i in (1,1,1000) do (
	REM Executando solicitacoes
	echo Executando iteracao %%i
	"C:\Apdata_X64\Aplicacoes\ApScripter\Bin\Win32\Debug\ApScripter.exe" "C:\github\fs523673job\projects\cmdScripter\Integration_Simple.txt"
)

endlocal

pause