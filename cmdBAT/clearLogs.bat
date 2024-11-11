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
echo Total de diret�rios encontrados: !count!
if !count! EQU 0 (
    echo Nenhum diret�rio encontrado.
    goto fim
)

:: Debug: listar as vari�veis dir1, dir2, etc.
echo Verificando as vari�veis dirN:
for /l %%i in (1,1,!count!) do (
    echo dir%%i!=!dir%%i!
)
echo.

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

:: Define o diret�rio escolhido com a expans�o correta
set "chosenDir=!dir%choice%!"
echo Voc� escolheu acessar o diret�rio: !chosenDir!
echo.

:: Verifica se 'isValid' � 2
set /a isValid=0
if %choice% GEQ 1 set /a isValid+=1
if %choice% LEQ !count! set /a isValid+=1

echo isValid: !isValid!
echo.

if !isValid! EQU 2 (
    rem Escolha v�lida, continuar
) else (
    echo Escolha inv�lida. Por favor, selecione um n�mero entre 1 e !count!.
    goto ChooseDir
)

:: Pergunta se deseja incluir subdiret�rios
:AskSubDir
set /p includeSub=Deseja incluir subdiret�rios? (S/N): 

if /i "!includeSub!"=="S" (
    set "searchOption=/s"
    echo Voc� escolheu incluir subdiret�rios.
    goto CountFiles
) else if /i "!includeSub!"=="N" (
    set "searchOption="
    echo Voc� escolheu n�o incluir subdiret�rios.
    goto CountFiles
) else (
    echo Resposta inv�lida. Por favor, digite S ou N.
    goto AskSubDir
)

:CountFiles
:: Navega para o diret�rio escolhido
cd /d "!chosenDir!"
echo Diret�rio atual ap�s mudar: %cd%
echo.

:: Lista os arquivos .log encontrados (para depura��o)
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

echo Total de arquivos .log a serem exclu�dos: %totalFiles%
echo.

if "%totalFiles%"=="0" (
    echo Nenhum arquivo .log encontrado no diret�rio selecionado.
    goto fim
)

:: Pergunta ao usu�rio para confirmar a exclus�o
:ConfirmDelete
set /p confirm=Tem certeza que deseja excluir todos os arquivos .log? (S/N): 

if /i "!confirm!"=="S" (
    echo Excluindo arquivos .log...
    del /f /q *.log %searchOption%
    if errorlevel 1 (
        echo Erro ao excluir arquivos .log.
    ) else (
        echo Arquivos .log exclu�dos com sucesso.
    )
) else if /i "!confirm!"=="N" (
    echo Opera��o cancelada pelo usu�rio.
) else (
    echo Resposta inv�lida. Por favor, digite S ou N.
    goto ConfirmDelete
)

:fim
endlocal
::exit
