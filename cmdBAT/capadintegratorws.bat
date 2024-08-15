@echo off
setlocal

set "vrsDelphi=alexandria" 
set "typeBuild=release"
set "dirBase=Apdata_X64"
set "dirApp=ApADIntegratorWS"
set "arquitetura=Win32"
set "appName=ApADIntegratorWS"
set "aplicacoes=Aplicacoes"
set "bin=Bin"
set "source=Source"
set "lib=lib" 
set "addEureka=0"

call "C:\github\fs523673job\projects\cmdBAT\compile.bat" %vrsDelphi% %typeBuild% %dirBase% %dirApp% %arquitetura% %appName% %aplicacoes% %bin% %source% %lib% %addEureka%

endlocal