@echo off
setlocal

set "vrsDelphi=%1" 
set "typeBuild=%2"
set "dirBase=%3"
set "dirApp=%4"
set "arquitetura=%5"
set "appName=%6"
set "app_source=C:\%dirBase%\Aplicacoes\%dirApp%\%dirApp%\%appName%"
set "MSBuildPath=C:\Program Files\Microsoft Visual Studio\2022\Professional\MSBuild\Current\Bin"
set "localPath=C:\Program Files (x86)\Embarcadero\Studio\22.0\bin;C:\Program Files (x86)\Embarcadero\Studio\22.0\bin64;C:\Program Files\CMake\;%MSBuildPath%;%PATH%"
set PATH=%FrameworkDir%;%FrameworkSDKDir%;%localPath%

echo %date% %time% ==== STEP 01 - Starting Building ApWebDispatcher

msbuild /m /v:m %app_source%.csproj /t:build /p:Platform=AnyCPU;Configuration=%typeBuild%