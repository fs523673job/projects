@echo off
chcp 1252 >nul
setlocal enabledelayedexpansion

:: Define o diretório onde o script está localizado
cd /d %~dp0

:: Caminho base da rede
set networkPath=\\SRVDEVEL1\builds\V559_BUILDS

:: Lista de opções de pesquisa
set pesquisaList=Oficial-Feature Oficial-controladas

:: Variáveis para controle
set count=0
set choice=

:: Lista todos os diretórios no diretório atual
for /d %%d in (*) do (
    set /a count+=1
    echo !count!: %%d
    set dir!count!=%%d
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
    echo Escolha inválida.
    goto fim
)

:: Define o diretório escolhido
set chosenDir=!dir%choice%!
echo Voce escolheu acessar o diretorio: %chosenDir%

:: Lista de arquivos a serem copiados
set filesToCopy=Server\Application\Win64\ApServer.exe Server\ApWebDispatcher\.net\bin\ApClientNet.dll Server\ApWebDispatcher\.net\bin\ApWebDispatcher.dll Client\AutoUpdater\ApTools.exe

:: Percorre a lista de arquivos e verifica/copia cada um
for %%f in (%filesToCopy%) do (
    set finalNetworkPath=%networkPath%\%pesquisaList%\%chosenDir%-Release\%%f
    echo Verificando o caminho: !finalNetworkPath!
    
    if exist "!finalNetworkPath!" (
        echo %%f encontrado. Copiando arquivo...
        copy "!finalNetworkPath!" "%~dp0!chosenDir!\"
        echo Arquivo %%f copiado com sucesso.
    ) else (
	    set alternativeFinalNetworkPath=%networkPath%\Oficial-%chosenDir%-Release\%%f
		if exist "!alternativeFinalNetworkPath!" (
		    echo %%f encontrado no caminho alternativo. Copiando arquivo...
			copy "!alternativeFinalNetworkPath!" "%~dp0!chosenDir!\"
			echo Arquivo %%f copiado com sucesso.
		) else (
            echo O arquivo %%f nao foi encontrado em nenhum dos caminhos da rede.
		)
    )
)

:fim
endlocal
exit
