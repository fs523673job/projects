@echo off
setlocal

rem Define o diretório de origem dos arquivos
set origem=C:\Program Files (x86)\Embarcadero\Studio\22.0\bin

rem Define o nome do arquivo compactado
set arquivo_compactado=C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\OldLSP.rar

rem Define o nome do arquivo 7z a ser descompactado
set arquivo_7z="%origem%\NewLSP.7z"

rem Caminho para o executável do WinRAR
set winrar=C:\Program Files\WinRAR\WinRAR.exe

rem Caminho para o executável do 7-Zip
set setezip="C:\Program Files\7-Zip\7z.exe"

rem Compacta os arquivos usando o WinRAR
"%winrar%" a -r "%arquivo_compactado%" "%origem%\DelphiLSP.de" "%origem%\DelphiLSP.exe" "%origem%\DelphiLSP.fr" "%origem%\DelphiLSP.ja" "%origem%\IDELSP290.bpl" "%origem%\IDELSP290.de" "%origem%\IDELSP290.fr" "%origem%\IDELSP290.ja" "%origem%\IDELSP290.jdbg"

echo Arquivos compactados com sucesso em "%arquivo_compactado%"

rem Descompacta o arquivo 7z para a mesma pasta
%setezip% x %arquivo_7z% -o"%origem%"

echo Arquivo 7z descompactado em "%origem%"

endlocal
