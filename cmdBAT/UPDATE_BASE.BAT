@ECHO OFF

Cls

SET /p restaurar=Restaurar Base Dados e adicionar dados default (*no* apenas restauracao) [yes, no]:
SET /p baseBeta=Restaruar Base Beta [yes, no] 

if /I "%baseBeta%" == "yes" (
	SET database=Oficial_31202.BAK 
) else (
	SET database=Oficial_55900.BAK
)

echo.
echo Database a ser restaurado %database%
echo.

echo.
echo excluindo arquivo anterior
echo.

if exist "C:\Bases_Local\%database%" (
	del "C:\Bases_Local\%database%"
	echo.
	echo arquivo deletado %database%
	echo.
)

if exist "C:\Bases_Local\logData.txt" (
	del "C:\Bases_Local\logData.txt"
	echo.
	echo arquivo deletado logData.txt
	echo.
)

echo.
echo copiando arquivo em \\192.168.10.209\BackupsOficiais\%database%
echo.

COPY \\192.168.10.209\BackupsOficiais\%database% "C:\Bases_Local"
echo.
echo copia finalizada
echo.

echo.
echo iniciando restauracao da base de dados %database%
echo.

sqlcmd -S APDDBTY09T2 -U "sa" -P "18|=S1=aHbU{T1Tn" -i "C:\github\fs523673job\projects\cmdSQL\SET_OFFLINE.SQL"
sqlcmd -S APDDBTY09T2 -U "sa" -P "18|=S1=aHbU{T1Tn" -i "C:\github\fs523673job\projects\cmdSQL\PREPARE_BASE.sql" -v fileName="%database%"

echo.
echo restauracao da base de dados finalizada
echo.

if /I "%restaurar%" == "yes" (GOTO YES) else (GOTO NO)

:YES
echo.
echo iniciando restauracao de dados default
echo.

sqlcmd -S APDDBTY09T2 -U "sa" -P "18|=S1=aHbU{T1Tn" -i "C:\Users\flsantos\OneDrive - Apdata do Brasil Software Ltda\Chamados\sqls\StoredProcedure_Integration.sql" -o "C:\Bases_Local\logData.txt"

GOTO END

:NO
echo.
echo Dados default nao restaurados
echo.
GOTO END

:END
SET /p viewFileContent=Verificar arquivo de log [yes, no]:

if /I "%viewFileContent%" == "yes" (
	if exist "C:\Bases_Local\logData.txt" (
		type "C:\Bases_Local\logData.txt"
	) else (
		echo.
		echo Arquivo de log nao gerado
		echo.
	)
) 
echo.
echo Restauracao finalizada.  
echo.

pause
