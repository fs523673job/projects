@echo off
setlocal

:: Defina aqui os valores padrão para cada argumento
set "vrsDelphi=Alexandria"
set "typeBuild=Debug"
set "dirBase=ApData_X64"
set "dirApp=ApServer"
set "arquitetura=Win32"
set "appName=ApServer"
set "addEureka=1"

set "PRN_DEFAULT=localhost /start"

echo "C:\github\fs523673job\projects\cmdBAT\build.bat" %vrsDelphi% %typeBuild% %dirBase% %dirApp% %arquitetura% %appName% %addEureka%
:: Chama o primeiro script com os valores padrão
call "C:\github\fs523673job\projects\cmdBAT\build.bat" %vrsDelphi% %typeBuild% %dirBase% %dirApp% %arquitetura% %appName% %addEureka%  

endlocal