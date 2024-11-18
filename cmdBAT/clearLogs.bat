@echo off
chcp 1252 >nul
setlocal enabledelayedexpansion

:: Exibe o diretório atual de execução
echo Diretório atual: %cd%
echo.

:: Variáveis para controle
set /a count=0
set "choice="

:: Lista todos os diretórios no diretório atual
echo Listagem de diretórios:
echo.
for /d %%d in (*) do (
    set /a count+=1
    echo !count!: %%d
    set "dir!count!=%%d"
)

:: Verificar se houve diretórios listados
echo.
echo Total de diretórios encontrados: !count!
if !count! EQU 0 (
    echo Nenhum diretório encontrado.
    goto fim
)

:: Pergunta ao usuário para escolher um diretório
:ChooseDir
set /p choice=Digite o número correspondente ao diretório que deseja acessar: 

:: Remove espaços do início e fim do input
set "choice=%choice: =%"

:: Verifica se a escolha é um número e dentro do intervalo
echo findstr %choice% /r "^[0-9][0-9]*$" >nul
if errorlevel 1 (
    echo Entrada inválida. Por favor, digite um número.
    goto ChooseDir
)

:: Verifica se a escolha é maior ou igual a 1
if %choice% LSS 1 (
    echo Escolha inválida. Por favor, selecione um número entre 1 e !count!.
    goto ChooseDir
)

:: Verifica se a escolha é maior que count
if %choice% GTR !count! (
    echo Escolha inválida. Por favor, selecione um número entre 1 e !count!.
    goto ChooseDir
)

:: Define o diretório escolhido com a expansão correta usando CALL
call set "chosenDir=%%dir%choice%%%"
echo.
echo Você escolheu acessar o diretório: !chosenDir!
echo.

:: Pergunta se deseja incluir subdiretórios
:AskSubDir
set /p includeSub=Deseja incluir subdiretórios? (S/N): 

if /i "!includeSub!"=="S" (
    set "searchOption=/s"
    echo Você escolheu incluir subdiretórios.
    goto ProcessFiles
) else if /i "!includeSub!"=="N" (
    set "searchOption="
    echo Você escolheu não incluir subdiretórios.
    goto ProcessFiles
) else (
    echo Resposta inválida. Por favor, digite S ou N.
    goto AskSubDir
)

:ProcessFiles
:: Navega para o diretório escolhido
cd /d "!chosenDir!"
echo Diretório atual após mudar: %cd%
echo.

:: Define os tipos de arquivos a serem deletados
set "fileTypes=.log .zip"

:: Loop para cada tipo de arquivo
for %%T in (%fileTypes%) do (
    set "ext=%%~T"
    set "ext=!ext:.=!"  :: Remove o ponto para usar em rótulos
    call :ProcessFileType !ext! %%T
)
goto fim

:ProcessFileType
:: %1 = extensão sem ponto (e.g., log, zip)
:: %2 = extensão com ponto (e.g., .log, .zip)
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

echo Total de arquivos %fileTypeWithDot% a serem excluídos: !totalFiles!
echo.

if "!totalFiles!"=="0" (
    echo Nenhum arquivo %fileTypeWithDot% encontrado no diretório selecionado.
    echo.
    goto :eof
)

:: Pergunta ao usuário para confirmar a exclusão dos arquivos %fileTypeWithDot%
set "confirm="
set /a maxAttempts=3
set /a attempts=0

:ConfirmDelete_%1%
set /a attempts+=1
if !attempts! GTR !maxAttempts! (
    echo Máximo de tentativas excedido. Operação cancelada.
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
        echo Arquivos %fileTypeWithDot% excluídos com sucesso.
    )
    echo.
    goto :eof
) else if /i "!confirm!"=="N" (
    echo Operação para arquivos %fileTypeWithDot% cancelada pelo usuário.
    echo.
    goto :eof
) else (
    echo Resposta inválida. Por favor, digite S ou N.
    if !attempts! LSS !maxAttempts! (
        goto ConfirmDelete_%1%
    ) else (
        echo Máximo de tentativas excedido. Operação cancelada.
        echo.
        goto :eof
    )
)

:fim
endlocal
::exit
