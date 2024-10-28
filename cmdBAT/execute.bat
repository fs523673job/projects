@echo off
chcp 1252 >nul
setlocal enabledelayedexpansion

:: Define o diretório onde o script está localizado
cd /d %~dp0

:: Caminho base da rede
set "networkPath=\\SRVDEVEL1\builds\V559_BUILDS"

:: Lista de opções de pesquisa
set "pesquisaList=Oficial-Feature Oficial-controladas"

:: Variáveis para controle
set "count=0"
set "choice="

:: Lista todos os diretórios no diretório atual
for /d %%d in (*) do (
    set /a count+=1
    echo !count!: %%d
    set "dir!count!=%%d"
)

:: Se não houver diretórios, sair
if %count%==0 (
    echo Nenhum diretorio encontrado.
    goto fim
)

:: Pergunta ao usuário para escolher um diretório
set /p choice=Digite o numero correspondente ao diretorio que deseja acessar: 

:: Verifica se a escolha é válida
if %choice% gtr %count% (
    echo Escolha invalida.
    goto fim
)

:: Define o diretório escolhido
set "chosenDir=!dir%choice%!"
echo Voce escolheu acessar o diretorio: %chosenDir%

:: Lista de arquivos a serem copiados
set "filesToCopy=Server\Application\Win64\ApServer.exe Server\ApWebDispatcher\.net\bin\ApClientNet.dll Server\ApWebDispatcher\.net\bin\ApWebDispatcher.dll Client\AutoUpdater\ApTools.exe APADINTEGRATORWS\Win32\ApADIntegratorWS.dll"

:: Percorre a lista de arquivos e verifica/copia cada um
for %%f in (%filesToCopy%) do (
    set "fileFound=0"
    set "sourcePath="
    
    :: Verifica nas opções de pesquisa
    for %%p in (%pesquisaList%) do (
        if "!fileFound!"=="0" (
            set "finalNetworkPath=%networkPath%\%%p\%chosenDir%-Release\%%f"
            if exist "!finalNetworkPath!" (
                echo %%f encontrado em !finalNetworkPath!
                set "sourcePath=!finalNetworkPath!"
                set "fileFound=1"
            )
        )
    )

    if "!fileFound!"=="0" (
        :: Se o arquivo não foi encontrado nas pesquisas, tenta o caminho alternativo
        set "alternativeFinalNetworkPath=%networkPath%\Oficial-%chosenDir%-Release\%%f"
        if exist "!alternativeFinalNetworkPath!" (
            echo %%f encontrado no caminho alternativo: !alternativeFinalNetworkPath!
            set "sourcePath=!alternativeFinalNetworkPath!"
            set "fileFound=1"
        ) else (
            echo O arquivo %%f nao foi encontrado em nenhum dos caminhos da rede.
        )
    )

    if "!fileFound!"=="1" (
        :: Chama a sub-rotina para perguntar se deseja copiar
        call :AskCopy "%%f" "!sourcePath!"
    )
)

goto fim

:AskCopy
:: Parâmetros: %1 = filename, %2 = sourcePath
set "fileName=%~1"
set "sourcePath=%~2"
set "validAnswer=0"

:AskCopyLoop
set /p copyChoice=Deseja copiar o arquivo %fileName%? (S/N): 
if /i "%copyChoice%"=="S" (
    echo Copiando arquivo...
    copy "%sourcePath%" "%~dp0%chosenDir%\"
    if errorlevel 1 (
        echo Erro ao copiar o arquivo %fileName%.
    ) else (
        echo Arquivo %fileName% copiado com sucesso.
    )
    set "validAnswer=1"
) else if /i "%copyChoice%"=="N" (
    echo Arquivo %fileName% nao foi copiado.
    set "validAnswer=1"
) else (
    echo Resposta invalida. Por favor, digite S ou N.
)

if "!validAnswer!"=="0" (
    goto :AskCopyLoop
)

goto :eof

:fim
endlocal
exit
