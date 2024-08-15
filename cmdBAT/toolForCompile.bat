@echo off
setlocal

echo Compilando ApIdControl.exe
echo . 

call "C:\github\fs523673job\projects\cmdBAT\capidcontrol.bat"

echo Copiando Arquivo ApIdControl.exe
echo .

copy "C:\ApData_x64\Utils\ApIdControl\bin\Win32\Release\ApIdControl.exe" "C:\ApData_x64\"

echo Compilando GenVersionRes
echo .

call "C:\github\fs523673job\projects\cmdBAT\cgenversionres.bat"

endlocal