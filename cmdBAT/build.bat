@echo off

setlocal

:: Pega os parâmetros passados
set "vrsDelphi=%1" 
set "typeBuild=%2"
set "dirBase=%3"
set "dirApp=%4"
set "arquitetura=%5"
set "appName=%6"
set "addEureka=%7" 
set "delphiAlexandria=alexandria"
set "dephiTokyo=tokyo"
::set "studioVer=22.0"
set "localPath=C:\Program Files (x86)\Embarcadero\Studio\22.0\bin;C:\Program Files (x86)\Embarcadero\Studio\22.0\bin64;C:\Program Files\CMake\;%PATH%"

if /I "%vrsDelphi%" == "%delphiAlexandria%" (
    set "studioVer=22.0"
) else if /I "%vrsDelphi%" == "%dephiTokyo%" (
    set "studioVer=19.0"
)

if /I "%arquitetura%" == "Win32" (
    set "dccversion=bin\dcc32.exe"
) else if /I "%arquitetura%" == "Win64" (
    set "dccversion=bin\dcc64.exe"
)

if /I "%typeBuild%"=="DEBUG"   set "typeBuild=Debug"
if /I "%typeBuild%"=="RELEASE" set "typeBuild=Release"
if /I "%typeBuild%"=="MEMLEAK" set "typeBuild=Memleak"

:: Caminhos
set "app_exe=C:\%dirBase%\Aplicacoes\%dirApp%\Bin\%arquitetura%\%typeBuild%\%appName%.exe"
set "app_dll=C:\%dirBase%\Aplicacoes\%dirApp%\Bin\%arquitetura%\%typeBuild%\%appName%.dll"
set "app_base=C:\%dirBase%\Aplicacoes\%dirApp%\Bin\%arquitetura%\%typeBuild%\%appName%"
set "app_source=C:\%dirBase%\Aplicacoes\%dirApp%\Source\%appName%"
set "app_lib=C:\%dirBase%\Aplicacoes\%dirApp%\lib\%arquitetura%\%typeBuild%"
set "app_bin=C:\%dirBase%\Aplicacoes\%dirApp%\Bin\%arquitetura%\%typeBuild%"
set "warning_path=C:\%dirBase%\Aplicacoes\%dirApp%\Bin\%arquitetura%\%typeBuild%\Warnings.log"
set "bpl_path=C:\%dirBase%\ApBPL"
set "ecc32exe=C:\%dirBase%\ApComps\EurekaLog7\Bin\ecc32.exe"
set "fastMM=C:\%dirBase%\ApComps\FastMM\"

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
@set dccbin=%BDS%\%dccversion%

set status=0

::Verifica o caminho Warning.logs existe
for %%F in ("%warning_path%") do set "warning_dir=%%~dpF"

if "%warning_dir:~-1%"=="\" (
    set "warning_dir=%warning_dir:~0,-1%"
)

if not exist "%warning_dir%" (
    mkdir %warning_dir%
)

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
msbuild /m /v:m %app_source%.dproj -fl1 -flp1:logfile=%warning_path%;warningsonly /t:build /p:Platform=%arquitetura%;Config=%typeBuild%;DCC_Warnings=true;DCC_Hints=true;DCC_Message_Directive=false;DCC_Inlining=off;DCC_RangeChecking=true;DCC_IntegerOverflowCheck=true;DCC_IOChecking=true;DCC_DebugInfoInExe=false;DCC_DynamicBase=false;DCC_UseResponseFile=true 
::if %errorlevel% == 1 goto FAILBUILD 
if %errorlevel% == 1 goto BUILDDCC

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

:BUILDDCC

echo.
echo === MSBUILD falhou. Tentando fallback com DCC...

set "rspfile=%TEMP%\fallback_dcc_%appName%.rsp"
set "sources=C:\%dirBase%\Aplicacoes\%dirApp%\Source\"

if exist "%rspfile%" del /f /q "%rspfile%"

(
    echo -B
    echo -Q
    echo -$D+ -$L+ -$Y+
    echo -U"%app_bin%;%fastMM%"
    echo -I"%app_bin%"
    echo -E"%app_bin%"
	echo -I"%sources%"
    echo -D"%typeBuild%"
    echo "%app_source%.dpr"
) > "%rspfile%"
	
if not exist "%rspfile%" (
	echo ERRO: Falha ao criar o arquivo de resposta: %rspfile%
	goto FAILBUILD
)
	
echo.
echo ==== Executando fallback 
echo. 
echo "%dccbin%" @"%rspfile%"
    
if not exist "%dccbin%" (
    echo ERRO: Não encontrou compilador DCC: "%dccbin%"
    goto FAILBUILD
)

"%dccbin%" @"%rspfile%"

if %errorlevel% NEQ 0 (
    echo ==== Falha na compilacao com DCC
    goto FAILBUILD
) else (
    echo ==== Sucesso na compilacao com DCC
)

echo %date% %time% ==== Step Out - End Build %app_source%.dproj
echo.

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
