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

:: Caminhos
set "app_exe=C:\%dirBase%\Aplicacoes\%dirApp%\bin\%arquitetura%\%typeBuild%\%appName%.exe
set "app_base=C:\%dirBase%\Aplicacoes\%dirApp%\bin\%arquitetura%\%typeBuild%\%appName%
set "app_source=C:\%dirBase%\Aplicacoes\%dirApp%\Source\%appName%.dproj
set "app_lib=C:\%dirBase%\Aplicacoes\%dirApp%\lib\%arquitetura%"
set "warning_path=C:\%dirBase%\Aplicacoes\%dirApp%\bin\%arquitetura%\%typeBuild%\Warnings.log"
set "bpl_path=C:\%dirBase%\ApBPL"

::Definindo variáveis de ambiente
@set BDS=C:\Program Files (x86)\Embarcadero\Studio\22.0
@set BDSINCLUDE=C:\Program Files (x86)\Embarcadero\Studio\22.0\include
@set BDSCOMMONDIR=C:\Users\Public\Documents\Embarcadero\Studio\22.0
@set FrameworkDir=C:\Windows\Microsoft.NET\Framework\v4.0.30319
@set FrameworkVersion=v4.5
@set FrameworkSDKDir=
@set PATH=%FrameworkDir%;%FrameworkSDKDir%;C:\Program Files (x86)\Embarcadero\Studio\22.0\bin;C:\Program Files (x86)\Embarcadero\Studio\22.0\bin64;C:\Program Files\CMake\;%PATH%
@set LANGDIR=EN
@set PLATFORM=
@set PlatformSDK=

set status=0


:: Deleta pasta de dcus
echo %date% %time% ==== Step 01 - Cleaning dcu from %app_lib%

if exist %app_lib% (
	rm -rf %app_lib%
	if errorlevel 1 goto FAILDELETE
)

:: Delete o app antigo

echo %date% %time% ==== Step 02 - Cleaning App from %app_exe%

if exist %app_exe% (
	rm -f %app_exe%
	if errorlevel 1 goto FAILDELETE
)

echo %date% %time% ==== Step 03 - Begin Build %app_source%

msbuild /m /v:m %app_source% -fl1 -flp1:logfile=%warning_path%;warningsonly /t:build /p:Platform=%arquitetura%;Config=%typeBuild%;DCC_Warnings=true;DCC_Hints=true;DCC_Message_Directive=false;DCC_Inlining=off;DCC_RangeChecking=true;DCC_IntegerOverflowCheck=true;DCC_IOChecking=true;DCC_DebugInfoInExe=false;DCC_DynamicBase=false
if errorlevel 1 goto FAILBUILD

echo %date% %time% ==== Step 03 - End Build %app_source%

echo %date% %time% ==== Step 04 - Generate DBG File %bpl_path%\map2dbg.exe

%bpl_path%\map2dbg.exe %app_exe%

echo.
echo %date% %time% ==== Step 04 - Generate PDB File %bpl_path%\cv2pdb.exe

%bpl_path%\cv2pdb.exe %app_base%.dbg %app_base%.pdb
 
echo.
echo %date% %time% ==== Step 06 - Verify Warnings

::pesquisa a palavara warning, o comando grep retorna zero quando encontrar
grep -ich warning %warning_path%
if %errorlevel% == 0 (
	echo ==== Error Hints/Warnings has been found 
	C:\%dirBase%\BlameApServerHints.exe %warning_path%
	goto FAILWARNING
)

echo %date% %time% ==== Step 07 - Verify Add Eureka

:FAILDELETE
set status=1
  
:FAILBUILD
set status=2

:FAILWARNING
set status=3

if status == 1 (
	echo Error ao apagar arquivos
) else (
	if status == 2 (
		echo Error na compilacao
	)
	else (
		if status == 3 (
			echo Error verifique os hints e warnings
		)
	)
)

endlocal
