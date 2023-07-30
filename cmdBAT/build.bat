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
set "app_source=C:\%dirBase%\Aplicacoes\%dirApp%\Source\%appName%
set "app_lib=C:\%dirBase%\Aplicacoes\%dirApp%\lib\%arquitetura%"
set "warning_path=C:\%dirBase%\Aplicacoes\%dirApp%\bin\%arquitetura%\%typeBuild%\Warnings.log"
set "bpl_path=C:\%dirBase%\ApBPL"
set "ecc32exe=C:\%dirBase%\ApComps\EurekaLog7\Bin\ecc32.exe"

::Definindo variáveis de ambiente Alexandria
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

echo.

:: Delete o app antigo

echo %date% %time% ==== Step 02 - Cleaning App from %app_exe%

if exist %app_exe% (
	rm -f %app_exe%
	if errorlevel 1 goto FAILDELETE
)

echo.

echo %date% %time% ==== Step 03 - Begin Build %app_source%.dproj
echo.

msbuild /m /v:m %app_source%.dproj -fl1 -flp1:logfile=%warning_path%;warningsonly /t:build /p:Platform=%arquitetura%;Config=%typeBuild%;DCC_Warnings=true;DCC_Hints=true;DCC_Message_Directive=false;DCC_Inlining=off;DCC_RangeChecking=true;DCC_IntegerOverflowCheck=true;DCC_IOChecking=true;DCC_DebugInfoInExe=false;DCC_DynamicBase=false
if errorlevel 1 goto FAILBUILD

echo %date% %time% ==== Step 03 - End Build %app_source%.dproj
echo.

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


echo %date% %time% ==== Step 07 - Verify Add Eureka %ecc32exe%

if %addEureka% == 1 (
	if exist %app_exe% "%ecc32exe%" --el_config=%app_source%.eof --el_alter_exe=%app_source%.dproj;%app_exe% --el_nostats --el_injectjcl
	if errorlevel 1 goto FAILEDEUREKA
)

goto ENDSUCESS

:FAILDELETE
set status=1
echo.
  
:FAILBUILD
set status=2
echo.

:FAILWARNING
set status=3
echo.

:FAILEDEUREKA
set status=4
echo.

if %status% == 1 (
	echo Error ao apagar arquivos
	goto ENDBUILD
)

if %status% == 2 (
	echo Error na compilacao
	goto ENDBUILD
)

if %status% == 3 (
	echo Error verifique os hints e warnings
	goto ENDBUILD
)

if %status% == 4 (
	echo Error ao adicionar o eureka
	goto ENDBUILD
)

:ENDBUILD

echo.
color 04
echo Fim do script de compilacao com erros

:ENDSUCESS

echo.
color 0A
echo Fim do script de compilacao

endlocal
