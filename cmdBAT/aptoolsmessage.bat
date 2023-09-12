@echo off
setlocal

:: Define o caminho para o executável
set "APP_PATH=C:\ApToolsMensagem\ApTools_Mensagem.exe"

:: Executa o aplicativo com os parâmetros
echo "%APP_PATH%"
start "" "%APP_PATH%"

endlocal
