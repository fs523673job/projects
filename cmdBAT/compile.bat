@echo off

setlocal

:: Pega os parâmetros passados
set "vrsDelphi=%1" 
set "typeBuild=%2"
set "dirBase=%3"
set "dirApp=%4"
set "arquitetura=%5"
set "appName=%6"
set "aplicacoes=%7"
set "bin=%8"
set "source=%9"
shift
set "lib=%~9" 
shift
set "addEureka=%~9" 
set "delphiAlexandria=alexandria"
set "dephiTokyo=tokyo"
set "studioVer=22.0"
set "localPath=C:\Program Files (x86)\Embarcadero\Studio\22.0\bin;C:\Program Files (x86)\Embarcadero\Studio\22.0\bin64;C:\Program Files\CMake\;%PATH%"

if /I "%vrsDelphi%" == "%delphiAlexandria%" (
    set "studioVer=22.0"
) else if /I "%vrsDelphi%" == "%dephiTokyo%" (
    set "studioVer=19.0"
)

:: Caminhos
set "app_exe=C:\%dirBase%\%aplicacoes%\%dirApp%\%bin%\%arquitetura%\%typeBuild%\%appName%.exe
set "app_dll=C:\%dirBase%\%aplicacoes%\%dirApp%\%bin%\%arquitetura%\%typeBuild%\%appName%.dll
set "app_base=C:\%dirBase%\%aplicacoes%\%dirApp%\%bin%\%arquitetura%\%typeBuild%\%appName%
set "app_source=C:\%dirBase%\%aplicacoes%\%dirApp%\%source%\%appName%
set "app_lib=C:\%dirBase%\%aplicacoes%\%dirApp%\%lib%\%arquitetura%\%typeBuild%"
set "warning_path=C:\%dirBase%\%aplicacoes%\%dirApp%\%bin%\%arquitetura%\%typeBuild%\Warnings.log"
set "bpl_path=C:\%dirBase%\ApBPL"
set "ecc32exe=C:\%dirBase%\ApComps\EurekaLog7\Bin\ecc32.exe"

::Definindo variáveis de ambiente Alexandria
@set BDS=C:\Program Files (x86)\Embarcadero\Studio\%studioVer%
@set BDSINCLUDE=C:\Program Files (x86)\Embarcadero\Studio\%studioVer%\include
@set BDSCOMMONDIR=C:\Users\Public\Documents\Embarcadero\Studio\%studioVer%
@set FrameworkDir=C:\Windows\Microsoft.NET\Framework\v4.0.30319
@set FrameworkVersion=v4.5
@set FrameworkSDKDir=
@set PATH=%FrameworkDir%;%FrameworkSDKDir%;%localPath%
@set LANGDIR=EN
@set PLATFORM=
@set PlatformSDK=

set status=0

::if /I %dephiTokyo% == %vrsDelphi% (
::	set studioVer="19.0"
::	set localPath="%ProgramFiles%\nodejs;%ProgramFiles(x86)%\Microsoft Visual Studio\2019\Professional\MSBuild\Current\Bin;%ProgramFiles(x86)%\Microsoft Visual Studio\2019\Professional\MSBuild\Current\Bin\amd64;%FrameworkDir%;%ProgramFiles(x86)%\Embarcadero\Studio\19.0\bin;%ProgramFiles(x86)%\Embarcadero\Studio\19.0\bin64;%SystemRoot%\system32;%SystemRoot%;%ProgramW6432%\Git\cmd;%ProgramW6432%\mingw64\bin;%ProgramW6432%\Git\usr\bin;%ProgramData%\Oracle\Java\javapath;%X64_CERTIFICATE_PATH%\bin;%X64_CERTIFICATE_PATH%\bin"
::)

:: Deleta pasta de dcus
echo %date% %time% ==== Step 01 - Cleaning dcu from %app_lib%

if exist %app_lib% (
	del /q  %app_lib%\*.dcu
	if errorlevel 1 goto FAILDELETE
)

echo.

:: Delete o app antigo
echo %date% %time% ==== Step 02 - Cleaning App from %app_exe%

if exist %app_exe% (
	del /q %app_exe%
	if errorlevel 1 goto FAILDELETE
)

if exist %app_dll% (
	del /q %app_dll%
	if errorlevel 1 goto FAILDELETE
)

echo.
echo %date% %time% ==== Step 03 - Begin Build %app_source%.dproj
echo.

:: Executa o msbuild da microsoft 
::msbuild /p:GenerateFullPaths=true /clp:NoSummary;NoItemAndPropertyList;ShowCommandLine;Verbosity=diagnostic /flp:Verbosity=diagnostic;logfile=c:\tmp\msbuild.log;append=true /m /v:m %app_source%.dproj -fl1 -flp1:logfile=%warning_path%;warningsonly /t:build /p:Platform=%arquitetura%;Config=%typeBuild%;DCC_Warnings=true;DCC_Hints=true;DCC_Message_Directive=false;DCC_Inlining=off;DCC_RangeChecking=true;DCC_IntegerOverflowCheck=true;DCC_IOChecking=true;DCC_DebugInfoInExe=false;DCC_DynamicBase=false
msbuild /m /v:m %app_source%.dproj -fl1 -flp1:logfile=%warning_path%;warningsonly /t:build /p:Platform=%arquitetura%;Config=%typeBuild%;DCC_Warnings=true;DCC_Hints=true;DCC_Message_Directive=false;DCC_Inlining=off;DCC_RangeChecking=true;DCC_IntegerOverflowCheck=true;DCC_IOChecking=true;DCC_DebugInfoInExe=false;DCC_DynamicBase=false
if %errorlevel% == 1 goto FAILBUILD 

echo %date% %time% ==== Step 03 - End Build %app_source%.dproj
echo.

:: Map2dbd Delphi
echo %date% %time% ==== Step 04 - Generate DBG File %bpl_path%\map2dbg.exe

if exist %app_exe% (
	%bpl_path%\map2dbg.exe %app_exe%
)

if exist %app_dll% (
	%bpl_path%\map2dbg.exe %app_dll%
)	

echo.
echo %date% %time% ==== Step 05 - Generate PDB File %bpl_path%\cv2pdb.exe

::Cv2Pdb Delphi
%bpl_path%\cv2pdb.exe %app_base%.dbg %app_base%.pdb
 
echo.
echo %date% %time% ==== Step 06 - Verify Warnings

if exist %warning_path% (
	rm -f %warning_path%
	if errorlevel 1 goto FAILDELETE
)
 
::pesquisa a palavara warning, o comando grep retorna zero quando encontrar
grep -ich warning %warning_path%
if %errorlevel% == 0 (
	echo ==== Error Hints/Warnings has been found 
	C:\%dirBase%\BlameApServerHints.exe %warning_path%
	goto FAILWARNING
)

echo %date% %time% ==== Step 07 - Verify Add Eureka %ecc32exe%

::Adicionando o eureka
if %addEureka% == 1 (
	if exist %app_exe% "%ecc32exe%" --el_config=%app_source%.eof --el_alter_exe=%app_source%.dproj;%app_exe% --el_nostats --el_injectjcl
	if errorlevel 1 goto FAILEDEUREKA
)

goto ENDSUCESS

:FAILDELETE
set status=1
echo.
goto ENDERROR
  
:FAILBUILD
set status=2
echo.
goto ENDERROR

:FAILWARNING
set status=3
echo.
goto ENDERROR

:FAILEDEUREKA
set status=4
echo.
goto ENDERROR

:ENDERROR

if %status% == 1 (
	echo Error ao apagar arquivos
)

if %status% == 2 (
	echo Error na compilacao
)

if %status% == 3 (
	echo Error verifique os hints e warnings
)

if %status% == 4 (
	echo Error ao adicionar o eureka
)

echo.
echo Script finalizado com erros

:ENDSUCESS

echo.
echo Fim do script de compilacao


endlocal
