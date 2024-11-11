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
echo Total de diretórios encontrados: !count!
if !count! EQU 0 (
    echo Nenhum diretório encontrado.
    goto fim
)

:: Debug: listar as variáveis dir1, dir2, etc.
echo Verificando as variáveis dirN:
for /l %%i in (1,1,!count!) do (
    echo dir%%i!=!dir%%i!
)
echo.

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

:: Define o diretório escolhido com a expansão correta
set "chosenDir=!dir%choice%!"
echo Você escolheu acessar o diretório: !chosenDir!
echo.

:: Verifica se 'isValid' é 2
set /a isValid=0
if %choice% GEQ 1 set /a isValid+=1
if %choice% LEQ !count! set /a isValid+=1

echo isValid: !isValid!
echo.

if !isValid! EQU 2 (
    rem Escolha válida, continuar
) else (
    echo Escolha inválida. Por favor, selecione um número entre 1 e !count!.
    goto ChooseDir
)

:: Pergunta se deseja incluir subdiretórios
:AskSubDir
set /p includeSub=Deseja incluir subdiretórios? (S/N): 

if /i "!includeSub!"=="S" (
    set "searchOption=/s"
    echo Você escolheu incluir subdiretórios.
    goto CountFiles
) else if /i "!includeSub!"=="N" (
    set "searchOption="
    echo Você escolheu não incluir subdiretórios.
    goto CountFiles
) else (
    echo Resposta inválida. Por favor, digite S ou N.
    goto AskSubDir
)

:CountFiles
:: Navega para o diretório escolhido
cd /d "!chosenDir!"
echo Diretório atual após mudar: %cd%
echo.

:: Lista os arquivos .log encontrados (para depuração)
echo Procurando arquivos .log...
if defined searchOption (
    dir /s /b /a-d *.log
) else (
    dir /b /a-d *.log
)
echo.

:: Conta os arquivos .log
if defined searchOption (
    for /f %%a in ('dir /s /b /a-d *.log 2^>nul ^| find /c /v ""') do set "totalFiles=%%a"
) else (
    for /f %%a in ('dir /b /a-d *.log 2^>nul ^| find /c /v ""') do set "totalFiles=%%a"
)

echo Total de arquivos .log a serem excluídos: %totalFiles%
echo.

if "%totalFiles%"=="0" (
    echo Nenhum arquivo .log encontrado no diretório selecionado.
    goto fim
)

:: Pergunta ao usuário para confirmar a exclusão
:ConfirmDelete
set /p confirm=Tem certeza que deseja excluir todos os arquivos .log? (S/N): 

if /i "!confirm!"=="S" (
    echo Excluindo arquivos .log...
    del /f /q *.log %searchOption%
    if errorlevel 1 (
        echo Erro ao excluir arquivos .log.
    ) else (
        echo Arquivos .log excluídos com sucesso.
    )
) else if /i "!confirm!"=="N" (
    echo Operação cancelada pelo usuário.
) else (
    echo Resposta inválida. Por favor, digite S ou N.
    goto ConfirmDelete
)

:fim
endlocal
::exit
