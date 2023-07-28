@echo off

setlocal

:: Define a versão do delphi
set "vrsDelphi=%1" 
set "typeBuild=%2"
set "dirBase=%3"
set "dirApp=%4"
set "arquitetura=%5"
set "appName=%6"
set "addEureka=%7" 

::Definindo variáveis de ambiente
@set BDS=C:\Program Files (x86)\Embarcadero\Studio\22.0
@set BDSINCLUDE=C:\Program Files (x86)\Embarcadero\Studio\22.0\include
@set BDSCOMMONDIR=C:\Users\Public\Documents\Embarcadero\Studio\22.0
@set FrameworkDir=C:\Windows\Microsoft.NET\Framework\v4.0.30319
@set FrameworkVersion=v4.5
@set FrameworkSDKDir=
@set PATH=%FrameworkDir%;%FrameworkSDKDir%;C:\Program Files (x86)\Embarcadero\Studio\22.0\bin;C:\Program Files (x86)\Embarcadero\Studio\22.0\bin64;C:\Program Files (x86)\Embarcadero\Studio\22.0\cmake;%PATH%
@set LANGDIR=EN
@set PLATFORM=
@set PlatformSDK=

set status=0

:: Caminhos
set "app_path=C:\%dirBase%\Aplicacoes\%dirApp%\bin\%arquitetura%\%typeBuild%\%appName%.exe"
set "warning_path=C:\%dirBase%\Aplicacoes\%dirApp%\bin\%arquitetura%\%typeBuild%\Warnings.log"
set "bpl_path=C:\%dirBase%\ApBPL"

:: Deleta pasta de dcus
echo %date% %time% ==== Step 01 - Cleaning dcu from C:\%dirBase%\Aplicacoes\ApServer\lib\%dirApp%\lib\%arquitetura%  

if exist C:\%dirBase%\Aplicacoes\ApServer\lib\%dirApp%\lib\%arquitetura% (
	rm -rf C:\%dirBase%\Aplicacoes\ApServer\lib\%dirApp%\lib\%arquitetura%
	if errorlevel 1 goto FAILDELETE
)

:: Delete o app antigo

echo %date% %time% ==== Step 02 - Cleaning App from %app_path%

if exist %app_path% (
	rm -f C:\%dirBase%\Aplicacoes\ApServer\lib\%dirApp%\lib\%arquitetura%
	if errorlevel 1 goto FAILDELETE
)

echo %date% %time% ==== Step 03 - Begin Build

msbuild /m /v:m %appName%.dproj -fl1 -flp1:logfile=%warning_path%;warningsonly /t:build /p:Platform=%arquitetura%;Config=%CONFIG%;DCC_Warnings=true;DCC_Hints=true;DCC_Message_Directive=false;DCC_Inlining=off;DCC_RangeChecking=true;DCC_IntegerOverflowCheck=true;DCC_IOChecking=true;DCC_DebugInfoInExe=false;DCC_DynamicBase=false

if errorlevel 1 goto FAILBUILD

echo %date% %time% ==== Step 04 - Generate DBG File

%bpl_path%\map2dbg.exe %app_path%

echo %date% %time% ==== Step 04 - Generate PDB File

%bpl_path%\cv2pdb.exe C:\%dirBase%\Aplicacoes\%dirApp%\bin\%arquitetura%\%typeBuild%\%appName%.dbg C:\%dirBase%\Aplicacoes\%dirApp%\bin\%arquitetura%\%typeBuild%\%appName%.pdb 


echo %date% %time% ==== Step 06 - Verify Warnings

::pesquisa a palavara warning, o comando grep retorna zero quando encontrar
grep -ich warning %warning_path%
if %errorlevel% == 0 (
	echo ==== Error Hints/Warnings has been found 
	C:\%dirBase%\BlameApServerHints.exe %warning_path%
	goto FAILWARNING
)

echo %date% %time% ==== Step 06 - Verify Add Eureka

if %addEureka% == 1 (
)


:FAILDELETE
set status=1
  
:FAILBUILD
set status=2

:FAILWARNING
set status=3

endlocal
