@echo off
setlocal

set "vrsDelphi=alexandria" 
set "typeBuild=release"
set "dirBase=Apdata_X64"
set "dirApp=ApIdControl"
set "arquitetura=Win32"
set "appName=ApIdControl"
set "aplicacoes=Utils"
set "bin=bin"
set "source=\"
set "lib=lib" 
set "addEureka=0"

call "C:\github\fs523673job\projects\cmdBAT\compile.bat" %vrsDelphi% %typeBuild% %dirBase% %dirApp% %arquitetura% %appName% %aplicacoes% %bin% %source% %lib% %addEureka%

endlocal