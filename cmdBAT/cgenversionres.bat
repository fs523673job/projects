@echo off
setlocal

set "vrsDelphi=alexandria" 
set "typeBuild=release"
set "dirBase=Apdata_X64"
set "dirApp=GenVersionRes"
set "arquitetura=Win32"
set "appName=GenVersionRes"
set "aplicacoes=Utils\Compilacao"
set "bin=\"
set "source=\"
set "lib=lib" 
set "addEureka=0"

call "C:\github\fs523673job\projects\cmdBAT\compile.bat" %vrsDelphi% %typeBuild% %dirBase% %dirApp% %arquitetura% %appName% %aplicacoes% %bin% %source% %lib% %addEureka%

endlocal