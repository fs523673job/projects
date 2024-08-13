@echo off
setlocal

set "vrsDelphi=alexandria" 
set "typeBuild=release"
set "dirBase=Apdata_X64"
set "dirApp=ApIdControl"
set "arquitetura=Bin\Win32"
set "appName=ApIdControl"
set "dirNameAux=Utils"
set "addEureka=0"

call "C:\github\fs523673job\projects\cmdBAT\compile.bat" %vrsDelphi% %typeBuild% %dirBase% %dirApp% %arquitetura% %appName% %dirNameAux% %addEureka%

endlocal