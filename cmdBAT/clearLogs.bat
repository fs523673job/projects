@echo off
chcp 1252 >nul
setlocal enabledelayedexpansion

:: Exibe o diret�rio atual de execu��o
echo Diret�rio atual: %cd%
echo.

:: Vari�veis para controle
set /a count=0
set "choice="

:: Lista todos os diret�rios no diret�rio atual
echo Listagem de diret�rios:
echo.
for /d %%d in (*) do (
    set /a count+=1
    echo !count!: %%d
    set "dir!count!=%%d"
)

:: Verificar se houve diret�rios listados
echo.
echo Total de diret�rios encontrados: !count!
if !count! EQU 0 (
    echo Nenhum diret�rio encontrado.
    goto fim
)

:: Pergunta ao usu�rio para escolher um diret�rio
:ChooseDir
set /p choice=Digite o n�mero correspondente ao diret�rio que deseja acessar: 

:: Remove espa�os do in�cio e fim do input
set "choice=%choice: =%"

:: Verifica se a escolha � um n�mero e dentro do intervalo
echo findstr %choice% /r "^[0-9][0-9]*$" >nul
if errorlevel 1 (
    echo Entrada inv�lida. Por favor, digite um n�mero.
    goto ChooseDir
)

:: Verifica se a escolha � maior ou igual a 1
if %choice% LSS 1 (
    echo Escolha inv�lida. Por favor, selecione um n�mero entre 1 e !count!.
    goto ChooseDir
)

:: Verifica se a escolha � maior que count
if %choice% GTR !count! (
    echo Escolha inv�lida. Por favor, selecione um n�mero entre 1 e !count!.
    goto ChooseDir
)

:: Define o diret�rio escolhido com a expans�o correta usando CALL
call set "chosenDir=%%dir%choice%%%"
echo.
echo Voc� escolheu acessar o diret�rio: !chosenDir!
echo.

:: Pergunta se deseja incluir subdiret�rios
:AskSubDir
set /p includeSub=Deseja incluir subdiret�rios? (S/N): 

if /i "!includeSub!"=="S" (
    set "searchOption=/s"
    echo Voc� escolheu incluir subdiret�rios.
    goto ProcessFiles
) else if /i "!includeSub!"=="N" (
    set "searchOption="
    echo Voc� escolheu n�o incluir subdiret�rios.
    goto ProcessFiles
) else (
    echo Resposta inv�lida. Por favor, digite S ou N.
    goto AskSubDir
)

:ProcessFiles
:: Navega para o diret�rio escolhido
cd /d "!chosenDir!"
echo Diret�rio atual ap�s mudar: %cd%
echo.

:: Define os tipos de arquivos a serem deletados
set "fileTypes=.log .zip"

:: Loop para cada tipo de arquivo
for %%T in (%fileTypes%) do (
    set "ext=%%~T"
    set "ext=!ext:.=!"  :: Remove o ponto para usar em r�tulos
    call :ProcessFileType !ext! %%T
)
goto fim

:ProcessFileType
:: %1 = extens�o sem ponto (e.g., log, zip)
:: %2 = extens�o com ponto (e.g., .log, .zip)
set "fileTypeWithDot=%~2"

echo Procurando arquivos %fileTypeWithDot%...
if defined searchOption (
    dir /s /b /a-d *%fileTypeWithDot%
) else (
    dir /b /a-d *%fileTypeWithDot%
)
echo.

:: Conta os arquivos %fileTypeWithDot%
if defined searchOption (
    for /f %%a in ('dir /s /b /a-d *%fileTypeWithDot% 2^>nul ^| find /c /v ""') do set "totalFiles=%%a"
) else (
    for /f %%a in ('dir /b /a-d *%fileTypeWithDot% 2^>nul ^| find /c /v ""') do set "totalFiles=%%a"
)

echo Total de arquivos %fileTypeWithDot% a serem exclu�dos: !totalFiles!
echo.

if "!totalFiles!"=="0" (
    echo Nenhum arquivo %fileTypeWithDot% encontrado no diret�rio selecionado.
    echo.
    goto :eof
)

:: Pergunta ao usu�rio para confirmar a exclus�o dos arquivos %fileTypeWithDot%
set "confirm="
set /a maxAttempts=3
set /a attempts=0

:ConfirmDelete_%1%
set /a attempts+=1
if !attempts! GTR !maxAttempts! (
    echo M�ximo de tentativas excedido. Opera��o cancelada.
    echo.
    goto :eof
)

set /p confirm=Tem certeza que deseja excluir todos os arquivos %fileTypeWithDot%? (S/N): 

if /i "!confirm!"=="S" (
    echo Excluindo arquivos %fileTypeWithDot%...
    del /f /q *%fileTypeWithDot% %searchOption%
    if errorlevel 1 (
        echo Erro ao excluir arquivos %fileTypeWithDot%.
    ) else (
        echo Arquivos %fileTypeWithDot% exclu�dos com sucesso.
    )
    echo.
    goto :eof
) else if /i "!confirm!"=="N" (
    echo Opera��o para arquivos %fileTypeWithDot% cancelada pelo usu�rio.
    echo.
    goto :eof
) else (
    echo Resposta inv�lida. Por favor, digite S ou N.
    if !attempts! LSS !maxAttempts! (
        goto ConfirmDelete_%1%
    ) else (
        echo M�ximo de tentativas excedido. Opera��o cancelada.
        echo.
        goto :eof
    )
)

:fim
endlocal
::exit
