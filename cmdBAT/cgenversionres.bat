@echo off
setlocal

set "vrsDelphi=alexandria" 
set "typeBuild=release"
set "dirBase=Apdata_X64"
set "dirApp=GenVersionRes"
set "arquitetura=Win32"
set "appName=GenVersionRes"
set "dirNameAux=Utils\Compilacao"
set "addEureka=0"

call "C:\github\fs523673job\projects\cmdBAT\compile.bat" %vrsDelphi% %typeBuild% %dirBase% %dirApp% %arquitetura% %appName% %dirNameAux% %addEureka%

endlocal