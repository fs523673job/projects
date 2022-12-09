use INTEGRATION_BETA
GO

/*
drop procedure sp_deleteCascate 
GO
drop procedure sp_StandardData_FixedValues 
GO
drop procedure sp_delete 
GO
drop procedure sp_lastIdTable 
GO
drop procedure sp_takeKeyForInsertion 
GO
drop function fn_getPKFieldName 
GO
drop function fn_getTableCount 
GO
drop function fn_getTableMaxKey 
GO
drop function fn_getNuiPkLimiteApdata 
GO
drop function fn_lastIdTable 
GO
drop procedure sp_infoApDataPkLimit 
GO
drop procedure sp_deleteOptionByApDataRange 
GO
drop procedure sp_clearAllDataIntegration 
GO
drop procedure sp_Generate_Inserts_From_Selects 
GO
drop procedure sp_Simple_Generate_Inserts_From_Selects 
GO
drop procedure sp_ConvertBinaryToText 
GO
drop procedure sp_Select_Into 
GO
*/



/************************************************************ 
	  1 -	Store Procedure Delete Tables Related
*************************************************************/

--create procedure sp_deleteCascate
create or alter procedure sp_deleteCascate
  (
		@TableName varchar(50), 
		@Criteria nvarchar(50),
		@OnlyStrCmd int = 1
  )
as
begin
	set nocount on

	declare @RefTable varchar(80)
	declare @RefField varchar(80)
	declare @auxRefTable varchar(80)
	declare @sqlcommand nvarchar(4000)
	declare @paramdefinition nvarchar(500)

	declare localCursor CURSOR LOCAL FOR
		SELECT --FK.name AS Ds_Nome_FK,
			   objeto_ori.name AS Ds_Objeto,
			   coluna_ori.name AS Ds_Coluna
		       --objeto_dest.name AS Ds_Objeto_Referencia,
		       --coluna_dest.name AS Ds_Coluna_Referencia
	     FROM sys.foreign_keys AS FK WITH(NOLOCK)
	     JOIN sys.foreign_key_columns AS FK_Coluna WITH(NOLOCK) ON FK.object_id = FK_Coluna.constraint_object_id
	     JOIN .sys.objects AS objeto_ori WITH(NOLOCK) ON FK.parent_object_id = objeto_ori.object_id
	     JOIN .sys.objects AS objeto_dest WITH(NOLOCK) ON FK.referenced_object_id = objeto_dest.object_id
	     JOIN sys.schemas AS schema_ori WITH(NOLOCK) ON objeto_ori.schema_id = schema_ori.schema_id
	     JOIN sys.schemas AS schema_dest WITH(NOLOCK) ON FK.schema_id = schema_dest.schema_id
	     JOIN sys.columns AS coluna_ori WITH(NOLOCK) ON FK_Coluna.parent_object_id = coluna_ori.object_id AND FK_Coluna.parent_column_id = coluna_ori.column_id
	     JOIN sys.columns AS coluna_dest WITH(NOLOCK) ON FK_Coluna.referenced_object_id = coluna_dest.object_id AND FK_Coluna.referenced_column_id = coluna_dest.column_id
	     WHERE objeto_dest.name = @TableName

	open localCursor 

	fetch next from localCursor
	into @RefTable, @RefField

	set @auxRefTable = ''

	while @@FETCH_STATUS = 0
	begin
		set @sqlcommand = 'DELETE FROM [' + @RefTable + '] WHERE ' + @RefField + ' ' + @Criteria

		if (( @auxRefTable <> @RefTable ) and (@RefTable <> @TableName))
			exec sp_deleteCascate @RefTable, @Criteria, @OnlyStrCmd

		if (@OnlyStrCmd is null) or (@OnlyStrCmd = 0)
		begin
			exec sp_ExecuteSQL @sqlcommand
			print 'After Execute: ' + @sqlcommand + ' [rows affected  =  ' + cast(@@ROWCOUNT as char(03)) + ']'
		end
		else
			print @sqlcommand

		set @auxRefTable = @RefTable

		fetch next from localCursor
		into @RefTable, @RefField
	end

	close localCursor;
	deallocate localCursor;

	set @sqlcommand = 
	  N'select @Value = C.COLUMN_NAME ' + 
	   '  FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS T ' +
	   '  JOIN INFORMATION_SCHEMA.CONSTRAINT_COLUMN_USAGE C ON (C.CONSTRAINT_NAME = T.CONSTRAINT_NAME)  ' +
	   '  WHERE C.TABLE_NAME = ''' + @TableName + ''' and T.CONSTRAINT_TYPE = ''PRIMARY KEY'' ' 
	set @paramdefinition = N'@Value varchar(80) OUTPUT' 

	exec sp_executesql @sqlcommand, @paramdefinition, @Value = @RefField OUTPUT

	set @sqlcommand = 'DELETE FROM [' + @TableName + '] WHERE ' + @RefField + ' ' + @Criteria

	if (@OnlyStrCmd is null) or (@OnlyStrCmd = 0)
	begin
		exec sp_ExecuteSQL @sqlcommand
		print 'After Execute: ' + @sqlcommand + ' [rows affected  =  ' + cast(@@ROWCOUNT as char(03)) + ']'
	end
	else
		print @sqlcommand
end
GO

/************************************************************ 
	  1.1 -	Store Procedure Delete Tables Related
*************************************************************/

--create procedure sp_delete
create or alter procedure sp_delete
  (
		@TableName varchar(50), 
		@Criteria nvarchar(50),
		@OnlyStrCmd int = 1
  )
as
begin
	set nocount on

	declare @RefField varchar(80)
	declare @sqlcommand nvarchar(4000)
	declare @paramdefinition nvarchar(500)

	set @sqlcommand = 
	  N'select @Value = C.COLUMN_NAME ' + 
	   '  FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS T ' +
	   '  JOIN INFORMATION_SCHEMA.CONSTRAINT_COLUMN_USAGE C ON (C.CONSTRAINT_NAME = T.CONSTRAINT_NAME)  ' +
	   '  WHERE C.TABLE_NAME = ''' + @TableName + ''' and T.CONSTRAINT_TYPE = ''PRIMARY KEY'' ' 
	set @paramdefinition = N'@Value varchar(80) OUTPUT' 

	exec sp_executesql @sqlcommand, @paramdefinition, @Value = @RefField OUTPUT

	set @sqlcommand = 'DELETE FROM [' + @TableName + '] WHERE ' + @RefField + ' ' + @Criteria

	if (@OnlyStrCmd is null) or (@OnlyStrCmd = 0)
	begin
		exec sp_ExecuteSQL @sqlcommand
		print 'After Execute: ' + @sqlcommand + ' [rows affected  =  ' + cast(@@ROWCOUNT as char(03)) + ']'
	end
	else
		print @sqlcommand
end
GO

/************************************************************ 
     2 - Store Procedure For Return Last Id From Table
*************************************************************/

--create procedure sp_lastIdTable
create or alter procedure sp_lastIdTable
(
	@TableName[sysname],
	@LastIdTable int = null OUTPUT
)
as
	declare @Limit int
	declare @FieldPK varchar(80)
	declare @LastKeyPK int
	declare @sqlcommand nvarchar(4000)
	declare @paramdefinition nvarchar(500)
begin
    set @sqlcommand = N'select @Value = ATB_NuiPkLimiteApdata from Tabelas where ATB_DssTabela = ''' + @TableName + ''''
	set @paramdefinition = N'@Value int OUTPUT'

	exec sp_executesql @sqlcommand, @paramdefinition, @Value = @Limit OUTPUT   
	
	set @sqlcommand = 
	  N'select @Value = C.COLUMN_NAME ' + 
	   '  FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS T ' +
	   '  JOIN INFORMATION_SCHEMA.CONSTRAINT_COLUMN_USAGE C ON (C.CONSTRAINT_NAME = T.CONSTRAINT_NAME)  ' +
	   '  WHERE C.TABLE_NAME = ''' + @TableName + ''' and T.CONSTRAINT_TYPE = ''PRIMARY KEY'' ' 
	set @paramdefinition = N'@Value varchar(80) OUTPUT' 

	exec sp_executesql @sqlcommand, @paramdefinition, @Value = @FieldPK OUTPUT

	set @sqlcommand = N'select @Value = Max(' + @FieldPK + ') from ' + @TableName
	set @paramdefinition = N'@Value int OUTPUT'

	exec sp_executesql @sqlcommand, @paramdefinition, @Value = @LastKeyPK OUTPUT
	
	if (@Limit > 0)
	begin
	  if (@LastKeyPK < @Limit)
	     set @LastKeyPK = @Limit
	end
	  
	select @LastIdTable = @LastKeyPK
end
GO

/************************************************************ 
    2.1 - Store Procedure For Return Last Id From Table
*************************************************************/

--create procedure sp_takeKeyForInsertion
create or alter procedure sp_takeKeyForInsertion
(
	@TableName[sysname],
	@NewKey int = null OUTPUT
)
as
begin
  exec sp_lastIdTable @TableName, @NewKey OUTPUT
  select @NewKey = (@NewKey + 1)
end
GO

/************************************************************ 
     2.2 - Function For Return Last Id From Table
*************************************************************/
--create function fn_getPKFieldName(@TableName[sysname])
create or alter function fn_getPKFieldName(@TableName[sysname])
returns varchar(1000)
as
begin
    return (select C.COLUMN_NAME as PK
   		      FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS T
		      JOIN INFORMATION_SCHEMA.CONSTRAINT_COLUMN_USAGE C ON C.CONSTRAINT_NAME = T.CONSTRAINT_NAME  
	          WHERE C.TABLE_NAME = @TableName
	            and T.CONSTRAINT_TYPE = 'PRIMARY KEY'
           )
end
GO

--create function fn_getTableCount(@TableName[sysname])
create or alter function fn_getTableCount(@TableName[sysname])
returns int
as
begin
	return (select sum(row_count) tableCount
              from sys.dm_db_partition_stats p
              join sys.tables t ON p.object_id = t.object_id
              where index_id <= 1 --Just clustered index or heap
                and t.name = parsename( @TableName, 1)
                and (t.schema_id = schema_id(parsename( @TableName, 2)) or parsename( @TableName, 2) is null)
			);
end
GO

--create function fn_getTableMaxKey(@TableName[sysname])
create or alter function fn_getTableMaxKey(@TableName[sysname])
returns int
as
begin
    declare @dsql varchar(1000)
	
	select @dsql = 'select max('+ z.COLUMN_NAME +')' + ' from '+ z.TABLE_NAME  
	   from (
              SELECT u.TABLE_NAME,u.COLUMN_NAME
              FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE u
              left join INFORMATION_SCHEMA.TABLES t on u.TABLE_NAME = t.TABLE_NAME
              WHERE OBJECTPROPERTY(OBJECT_ID(CONSTRAINT_SCHEMA + '.' + QUOTENAME(CONSTRAINT_NAME)), 'IsPrimaryKey') = 1
              AND u.TABLE_NAME = t.TABLE_NAME 
              and u.TABLE_NAME = @TableName
			) as z

	return (select @dsql)
end
GO

--create function fn_getNuiPkLimiteApdata(@TableName[sysname])
create or alter function fn_getNuiPkLimiteApdata(@TableName[sysname])
returns int
as
begin
    return (select ATB_NuiPkLimiteApdata from Tabelas where ATB_DssTabela = @TableName)
end
GO

--create function fn_lastIdTable(@TableName[sysname])
create or alter function fn_lastIdTable(@TableName[sysname])
returns int
as
begin
	declare @pklimit int
	declare @pkcount int
	declare @result int

	select @pkcount = [dbo].[fn_getTableMaxKey](@TableName)
	select @pklimit = [dbo].[fn_getNuiPkLimiteApdata](@TableName)

	set @result = @pkcount

	if (@pklimit > @pkcount)
		set @result = @pklimit

	return @result;
end
GO

/************************************************************ 
	3 - Store Procedure For Return Info On Limits Of Table
*************************************************************/

--create procedure sp_infoApDataPkLimit
create or alter procedure sp_infoApDataPkLimit
  (
	@TableName[sysname],
	@OnlyApdataLimit int = null,
	@ApdataLimit int = null OUTPUT
  )
as
	declare @NuiPkLimiteApdata int
	declare @NamePKTable varchar(80)
	declare @MaxKeyPK int
	declare @sqlcommand nvarchar(200)
	declare @paramdefinition nvarchar(500)
begin
	declare queryPKLimite cursor local for
	  select ATB_NuiPkLimiteApdata from Tabelas where ATB_DssTabela = @TableName

	open queryPKLimite
	
	fetch next from queryPKLimite
	into @NuiPkLimiteApdata

	close queryPKLimite
	deallocate queryPKLimite

	declare queryPKName cursor local for
		select C.COLUMN_NAME as PK
			FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS T  
			JOIN INFORMATION_SCHEMA.CONSTRAINT_COLUMN_USAGE C ON C.CONSTRAINT_NAME=T.CONSTRAINT_NAME  
			WHERE C.TABLE_NAME = @TableName  
			  and T.CONSTRAINT_TYPE = 'PRIMARY KEY' 

    open queryPKName

	fetch next from queryPKName
	into @NamePKTable

	close queryPKName
	deallocate queryPKName

	set @sqlcommand = N'select @MaxKey = Max(' + @NamePKTable + ') from ' + @TableName
	set @paramdefinition = N'@MaxKey int OUTPUT'

	exec sp_executesql @sqlcommand, @paramdefinition, @MaxKey = @MaxKeyPK OUTPUT

	if (@OnlyApdataLimit is null) or (@OnlyApdataLimit = 0)
		select @NuiPkLimiteApdata as ApData_Limit, @MaxKeyPK as ApData_LastPkId, @NamePKTable as ApData_ColumPkName 
	else
		select @ApdataLimit = @NuiPkLimiteApdata
end
GO

/************************************************************************
   4 - Store Procedure Delete Cascate Table For ApData Limit Range
*************************************************************************/

--create procedure sp_deleteOptionByApDataRange
create or alter procedure sp_deleteOptionByApDataRange
	(
		@TableName[sysname],
		@ConsiderApdataRange int,
		@SimpleDelete int = null
	)
as
begin
	declare @Apdata_Limit int
	declare @Apdata_Limit_Str varchar(30)
	
	begin transaction deleteData
		if (@ConsiderApdataRange = 1)
		begin
			exec sp_infoApDataPkLimit @TableName, @ConsiderApdataRange, @Apdata_Limit OUTPUT
		    
			select @Apdata_Limit_Str = ' > ' +  Convert(varchar(30), @Apdata_Limit)
		    
			if (@SimpleDelete is null) or (@SimpleDelete = 0)
				exec sp_deleteCascate @TableName, @Apdata_Limit_Str, 0
			else
				exec sp_delete @TableName, @Apdata_Limit_Str, 0
		end
		else
		begin
			if (@SimpleDelete is null) or (@SimpleDelete = 0)
				exec sp_deleteCascate @TableName, ' > 0', 0
			else
				exec sp_delete @TableName, ' > 0', 0
	    end;

	commit;
end
GO

/**********************************************************************
  5 - Store Procedure User For Clear Data From Tables of Integration
***********************************************************************/

--create procedure sp_clearAllDataIntegration
create or alter procedure sp_clearAllDataIntegration
	@ConsiderApdataRange int
as
begin
	exec sp_deleteOptionByApDataRange 'ServidoresIntegracoesBDs', @ConsiderApdataRange
	exec sp_deleteOptionByApDataRange 'ServidoresIntegracoes', @ConsiderApdataRange
	exec sp_deleteOptionByApDataRange 'TransacoesIntegracoesSobs', @ConsiderApdataRange
	exec sp_deleteOptionByApDataRange 'TransacoesIntegracoes', @ConsiderApdataRange
	exec sp_deleteOptionByApDataRange 'DLLsIntegracoesMetodos', @ConsiderApdataRange
	exec sp_deleteOptionByApDataRange 'DLLsIntegracoes', @ConsiderApdataRange
	exec sp_deleteOptionByApDataRange 'ModelosIntegracoesCmdsRets', @ConsiderApdataRange
	exec sp_deleteOptionByApDataRange 'ComandosItensHTTP', @ConsiderApdataRange
	exec sp_deleteOptionByApDataRange 'ModsIntsMonitsBasesEventos', @ConsiderApdataRange
	exec sp_deleteOptionByApDataRange 'ModelosIntegracoesSobs', @ConsiderApdataRange
	exec sp_deleteOptionByApDataRange 'ModelosIntegracoesQueries', @ConsiderApdataRange
	exec sp_deleteOptionByApDataRange 'ModelosIntegracoesCmdsPars', @ConsiderApdataRange
	exec sp_deleteOptionByApDataRange 'ModelosIntegracoesCmdsCpos', @ConsiderApdataRange
	exec sp_deleteOptionByApDataRange 'ModelosIntegracoesCmds', @ConsiderApdataRange
	exec sp_deleteOptionByApDataRange 'ModelosIntegracoes', @ConsiderApdataRange
	exec sp_deleteOptionByApDataRange 'LayoutsSaidas', @ConsiderApdataRange
	exec sp_deleteOptionByApDataRange 'ComandosSQLsSobs', 1, 1
	exec sp_deleteOptionByApDataRange 'ComandosSQLs', 1, 1
	exec sp_deleteOptionByApDataRange 'ComandosSQLsGrupos', 1, 1
end
GO

/**********************************************************************
  6 - Store Procedure Standard Data
***********************************************************************/

--create procedure sp_StandardData_FixedValues
create or alter procedure sp_StandardData_FixedValues
	@ConsiderApDataRange int
as
begin
	begin transaction deleteData
		declare @SQL_CdiComandoSQL int
		declare @ADN_CdiComandoSQLGrupo int
		declare @MaxKeyFromTable int

		exec sp_clearAllDataIntegration @ConsiderApDataRange

		/*OBJETO - 550*/
		insert into ServidoresIntegracoes(BBN_CdiServidorIntegracao, BBN_D1sServidorIntegracao, BBN_CosEnderecoIP, BBN_NuiPorta) values (1, '(TESTES) - INTEGRATION - SERVIDORES', 'localhost', 7080);

		/*OBJETO - 551*/
		insert into DLLsIntegracoes(BBV_CdiDLLIntegracao, BBV_D1sDLLIntegracao, BBV_CosCaminhoArquivoDLL) values (50001, '(TESTES) - DLL INTEGRACAO INTERFACE', 'C:\Apdata_x64\Aplicacoes\ApIntegrationInterface\bin\Win32\Debug\ApIntegrationInterface.dll');
		insert into DLLsIntegracoesMetodos(BBW_CdiDLLIntegracaoMetodo, BBW_CdiDLLIntegracao, BBW_CosDLLIntegracaoMetodo, BBW_D1sDLLIntegracaoMetodo) values (50001, 50001, 'TreatTransactionEvent', 'TreatTransactionEvent');

		/*OBJETO - 559*/
		insert into TransacoesIntegracoes(BBX_CdiTransacaoIntegracao, BBX_CdiDLLIntegracaoMetodo, BBX_CdiEventoTransacao, BBX_CdiTransacao) values (50001, 50001, 2, 30063) ;
		insert into TransacoesIntegracoes(BBX_CdiTransacaoIntegracao, BBX_CdiDLLIntegracaoMetodo, BBX_CdiEventoTransacao, BBX_CdiTransacao) values (50002, 50001, 2, 43192) ;
		insert into TransacoesIntegracoes(BBX_CdiTransacaoIntegracao, BBX_CdiDLLIntegracaoMetodo, BBX_CdiEventoTransacao, BBX_CdiTransacao) values (50003, 50001, 2, 21233) ;
		insert into TransacoesIntegracoes(BBX_CdiTransacaoIntegracao, BBX_CdiDLLIntegracaoMetodo, BBX_CdiEventoTransacao, BBX_CdiTransacao) values (50004, 50001, 2, 30842) ;
		insert into TransacoesIntegracoes(BBX_CdiTransacaoIntegracao, BBX_CdiDLLIntegracaoMetodo, BBX_CdiEventoTransacao, BBX_CdiTransacao) values (50005, 50001, 2, 15953) ;

		/*OBJETO - 962*/
		insert into LayoutsSaidas(BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida) values (1001, '(TESTES) LAYOUT REST PARAMETROS', 0xEFBBBF7B226B6579223A20222376616C7565506172616D6574657223227D)
		insert into LayoutsSaidas(BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida) values (1002, '(TESTES) LAYOUT REST PARAMETROS URL', 0xEFBBBF68747470733A2F2F7669616365702E636F6D2E62722F77732F23636570232F6A736F6E2F)
		insert into LayoutsSaidas(BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida) values (1003, '(TESTES) LAYOUT REST INTEGRACAO FOTOS', 0xEFBBBF7B0D0A20226964223A22234147495F436469417373756E746F476572616C4974656D23222C0D0A2022666F746F223A22234147495F4172624172717569766F52656C61746F72696F23220D0A7D)
		insert into LayoutsSaidas(BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida) values (1004, '(TESTES) MOCK POSTMAN GET', 0xEFBBBF68747470733A2F2F32306537373664392D666164662D343763312D393163392D3032663538323931623963312E6D6F636B2E7073746D6E2E696F2F6170692F75736572646174612F236B6579696423);
		insert into LayoutsSaidas(BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida) values (1005, '(TESTES) MOCK POSTMAN PUT', 0xEFBBBF68747470733A2F2F32306537373664392D666164662D343763312D393163392D3032663538323931623963312E6D6F636B2E7073746D6E2E696F2F6170692F75736572646174612F236B6579696423);
		insert into LayoutsSaidas(BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida) values (1006, '(TESTES) JSON - PREENCHIMENTO PARAMETRO SQL', 0xEFBBBF207B22646174614F6E65223A236669656C6431232C20226461746154776F223A236669656C6432237D);
		insert into LayoutsSaidas(BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida) values (1007, '(TESTES) JSON - LAYOUT SAIDA ALTERACAO', 0xEFBBBF7B226E616D65223A22236E6F6D6523222C224F5554524F4E414D45223A2223656D61696C23222C226964223A2223696423227D);

		/*OBJETO - 3552 */
		exec sp_takeKeyForInsertion 'ComandosSQLsGrupos', @ADN_CdiComandoSQLGrupo OUTPUT
		exec sp_takeKeyForInsertion 'ComandosSQLs', @SQL_CdiComandoSQL OUTPUT
			
		insert into ComandosSQLsGrupos(ADN_CdiComandoSQLGrupo, ADN_D1sComandoSQLGrupo) values (@ADN_CdiComandoSQLGrupo + 1, '(TESTES) COMANDOS SQL TESTES')
		insert into ComandosSQLs(SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL) values(@SQL_CdiComandoSQL + 1, @ADN_CdiComandoSQLGrupo + 1, '(TESTES) - RETORNA USUARIO ATRAVES DO CONTRATADO', 0xEFBBBF53656C656374206D6178285553525F4364695573756172696F290D0A66726F6D205573756172696F730D0A696E6E6572206A6F696E205573756172696F73436F6E7472617461646F73204F4E20285553525F4364695573756172696F203D205553435F4364695573756172696F290D0A7768657265205553435F436469436F6E7472617461646F5F5573756172696F203D203A6964636F6E7472617461646F0D0A);
		insert into ComandosSQLs(SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL) values(@SQL_CdiComandoSQL + 2, @ADN_CdiComandoSQLGrupo + 1, '(TESTES) - CONVERSOR EMAIL', 0xEFBBBF4445434C4152452040656D61696C2061732056415243484152283830290D0A5345542040656D61696C203D202853656C656374206D6178285553525F436F73456D61696C2966726F6D205573756172696F730D0A09090909696E6E6572206A6F696E205573756172696F73436F6E7472617461646F73204F4E20285553525F4364695573756172696F203D205553435F4364695573756172696F290D0A090909097768657265205553435F436469436F6E7472617461646F5F5573756172696F203D203A6964636F6E7472617461646F290D0A73656C6563742043415345205748454E2040656D61696C206973206E6F74206E756C6C207468656E2027402720656C73652040656D61696C20656E6420617320656D61696C);
		insert into ComandosSQLs(SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL) values(@SQL_CdiComandoSQL + 3, @ADN_CdiComandoSQLGrupo + 1, '(TESTES) - CONTRATADOS LOTES', 0xEFBBBF0D0A0D0A0D0A53454C4543540D0A0D0A312061732069642C0D0A27303831313133313027206173206365700D0A756E696F6E20616C6C0D0A73656C6563740D0A322C0D0A273037313131333130270D0A756E696F6E20616C6C0D0A73656C656374200D0A332C0D0A273039313131333130270D0A);
		insert into ComandosSQLs(SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL) values(@SQL_CdiComandoSQL + 4, @ADN_CdiComandoSQLGrupo + 1, '(TESTES) - BUFFER', 0xEFBBBF73656C656374204147495F436469417373756E746F476572616C4974656D2C204147495F4172624172717569766F52656C61746F72696F0D0A66726F6D20417373756E746F734765726169734974656E730D0A7768657265204147495F436469417373756E746F476572616C4974656D203D203132);
		insert into ComandosSQLs(SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL) values(@SQL_CdiComandoSQL + 5, @ADN_CdiComandoSQLGrupo + 1, '(TESTES) - COMANDO PREENCHIMENTO JSON', 0xEFBBBF77697468206475616C2864756D6D7929206173202873656C65637420277827290D0A53454C45435420273931343036393227206173206B65796D61737465722C20273027206173206669656C64312C2027323032323131303127206173206669656C6432202046524F4D206475616C202057484552452031203D203A6B65796964);
		insert into ComandosSQLs(SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL) values(@SQL_CdiComandoSQL + 6, @ADN_CdiComandoSQLGrupo + 1, '(TESTES) - COMANDO SAIDA JSON', 0xEFBBBF73656C65637420636F6E5F6473736E6F6D65206E6F6D652C20434F4E5F436F73454D61696C20656D61696C2C20636F6E5F436469636F6E7472617461646F204944200D0A0966726F6D20636F6E7472617461646F730D0A776865726520636F6E5F636469636F6E7472617461646F203D203A636F6E5F636469636F6E7472617461646F0D0A);
		insert into ComandosSQLs(SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL) values(@SQL_CdiComandoSQL + 7, @ADN_CdiComandoSQLGrupo + 1, '(TESTES) - CONSULTA RANDOM NUMERO KEYID', 0xEFBBBF53454C45435420464C4F4F522852414E4428292A283130302D312B31292B3129206173206B657969643B);

		/*OBJETO - 554*/
		/*SOAP*/ insert into ModelosIntegracoes(BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao)              values (10001, '(TESTES) SOAP CORREIOS CONSULTA CEP');
		/*REST*/ insert into ModelosIntegracoes(BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao)              values (10002, '(TESTES) REST SEM PARAMETROS');
		/*REST*/ insert into ModelosIntegracoes(BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao)              values (10003, '(TESTES) REST COM PARAMETROS');
		/*REST*/ insert into ModelosIntegracoes(BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao)              values (10004, '(TESTES) REST COM PARAMETROS URL');
		/*REST*/ insert into ModelosIntegracoes(BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao)              values (10005, '(TESTES) REST ALTERACAO OBJ 2330');
		/*REST*/ insert into ModelosIntegracoes(BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao)              values (10006, '(TESTES) REST MARCACAO PONTO');
		/*REST*/ insert into ModelosIntegracoes(BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao)              values (10007, '(TESTES) REST ENTRADA BASEX64');
		/*SOAP*/ insert into ModelosIntegracoes(BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao)              values (10008, '(TESTES) SOAP CORREIOS LOTE');
		/*SOAP*/ insert into ModelosIntegracoes(BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao)              values (10009, '(TESTES) INTEGRACAO FOTO');
		/*REST MOCK POSTMAN*/ insert into ModelosIntegracoes(BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao) values (10010, '(TESTES) MOCK POSTMAN GET');
		/*REST MOCK POSTMAN*/ insert into ModelosIntegracoes(BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao) values (10011, '(TESTES) MOCK POSTMAN PUT');
		/*REST MOCK POSTMAN*/ insert into ModelosIntegracoes(BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao) values (10012, '(TESTES) MOCK POSTMAN POST');
		/*REST*/ insert into ModelosIntegracoes(BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao)              values (10013, '(TESTES) CONSULTAS REMOTAS');
		/*REST MOCK POSTMAN*/ insert into ModelosIntegracoes(BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao) values (10014, '(TESTES) MOCK POSTMAN GET ARRAY');

		/*OBJETO - 555*/
		/*SOAP*/ insert into ModelosIntegracoesCmds(BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto)     values (10001, 10001, '(TESTES) CORREIOS CONSULTA CEP - CMD', 2, 30063, 'AtendeClienteService;AtendeCliente;consultaCEP#consultaCEPResponse;return;cidade###6')  
		/*SOAP*/ insert into ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_CdiTipoCampo, BBP_DssConteudo_String, BBP_OplConteudoFixo) values (10001, 10001, 'consultaCEP;cep', 9, '03510030', 1)
		
		/*REST*/ insert into ModelosIntegracoesCmds(BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP) values (10002, 10002, '(TESTES) REST SEM PARAMETROS', 2, 30063, 'http://echo.jsontest.com/key/value/one/two', 1)  
		/*REST*/ insert into ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_CdiTipoCampo, BBP_DssConteudo_String)                                    values (10002, 10002, '', 9, '')
		
		/*REST*/ insert into ModelosIntegracoesCmds(BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP)       values (10003, 10003, '(TESTES) REST COM PARAMETROS', 2, 30063, 'http://validate.jsontest.com/?json=', 1)  
		/*REST*/ insert into ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_CdiTipoCampo, BBP_DssConteudo_String, BBP_OplConteudoFixo, BBP_CdiLayOutSaida) values (10003, 10003, 'valueParameter', 9, 'value', 1, 1001)
		
		/*REST*/ insert into ModelosIntegracoesCmds(BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP)       values (10004, 10004, '(TESTES) REST COM PARAMETROS URL', 2, 30063, '', 1)  
		/*REST*/ insert into ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_CdiTipoCampo, BBP_DssConteudo_String, BBP_OplConteudoFixo, BBP_CdiLayOutSaida) values (10004, 10004, 'cep', 9, '01001000', 1, 1002)

		/*REST*/insert into ModelosIntegracoesCmds(BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP) values (10005, 10005, '(TESTES) REST ALTERACAO SENHA 2330', 1, 21233, '', 1)  
		/*REST*/insert into ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiComandoSQL)                    values (10005, 10005, 'idcontratado', 'USR_CdiUsuario', 3, @SQL_CdiComandoSQL + 1)
		/*REST*/insert into ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiComandoSQL)                    values (10006, 10005, '', 'USR_CosEMail', 9, @SQL_CdiComandoSQL + 2)
		/*REST*/insert into ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo)                                       values (10007, 10005, 'USR_OplPrimeiroAcesso', 'USR_OplPrimeiroAcesso', 12)
		/*REST*/insert into ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo)                                       values (10008, 10005, 'USR_CosSenha', 'USR_CosSenha', 9)
		
		/*REST*/insert into ModelosIntegracoesCmds(BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP) values (10006, 10006, '(TESTES) REST MARCACAO PONTO', 2, 43192, '', 1)  
		/*REST*/insert into dbo.ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo)                                   values (10009, 10006, 'BatidaData', 'CBD_DtdBatidaData', 10)
		/*REST*/insert into dbo.ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo)                                   values (10010, 10006, 'BatidaHoraMinuto', 'CBD_HrdBatidaHoraMinuto', 11)
        /*REST*/insert into dbo.ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo)                                   values (10011, 10006, 'DispositivoAcesso', 'CBD_NuiDispositivoAcesso', 9)
		/*REST*/insert into dbo.ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo)                                   values (10012, 10006, 'Cdicontratado', 'CBD_CosCrachaBase', 9)
        /*REST*/insert into dbo.ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo)                                   values (10013, 10006, 'latitude', 'CBD_QtnLatitude', 9)
		/*REST*/insert into dbo.ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo)                                   values (10014, 10006, 'longitude', 'CBD_QtnLongitude', 9)

		/*REST*/ insert into ModelosIntegracoesCmds(BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_NuiTipoEdicao) values (10007, 10007, '(TESTES) REST ENTRADA BASEX64',  1, 30842, 407)  
		/*REST*/ insert into ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo)                     values (10015, 10007, 'D1sCargo', 'CAR_D1sCargo', 9)
		/*REST*/ insert into ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo)                     values (10016, 10007, 'D1sCargoRes', 'CAR_D1sCargoRes', 9)
		/*REST*/ insert into ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo)                     values (10017, 10007, 'anexo', 'CampoVirtual_100505', 17)
		/*REST*/ insert into ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo)                     values (10018, 10007, 'CBO', 'CAR_CdiCodBrasileiroOcupacao', 9)

		/*REST*/ insert into ModelosIntegracoesCmds(BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_NuiTipoEdicao, BBS_CdiComandoSQL, BBS_DssNomeObjeto, BBS_OplEnviarTudo, BBS_DssCamposLote) values (10008, 10008, '(TESTES) SOAP CORREIOS LOTE',  1, 30063, 0, @SQL_CdiComandoSQL + 3, 'AtendeClienteService;AtendeCliente;consultaCEP#consultaCEPResponse#consultaCEPResponse##0', 1, 'cep')  
		/*REST*/ insert into ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Origem, BBP_DssCampo_Destino, BBP_CdiTipoCampo, BBP_OplConteudoFixo)                                                                            values (10019, 10008, 'cep', 'consultaCEP;cep', 9, 0)

		/*REST*/ insert into ModelosIntegracoesCmds(BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_NuiTipoEdicao, BBS_DssNomeObjeto, BBS_CdiComandoSQL, BBS_CdiEventoTransacao, BBS_CdiVerboHTTP) values (10009, 10009, '(TESTES) INTEGRACAO FOTO',  4, 30063, 0, 'https://httpbin.org/post', @SQL_CdiComandoSQL + 4, 2, 3)  
		/*REST*/ insert into ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida)                                                                                 values (10020, 10009, 'AGI_CdiAssuntoGeralItem', 'AGI_CdiAssuntoGeralItem', 3, 1003)
		/*REST*/ insert into ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida)                                                                                 values (10021, 10009, 'AGI_ArbArquivoRelatorio', 'AGI_ArbArquivoRelatorio', 18, 1003)

		/*API POSTMAN ->  https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/ */ 

		/*REST*/ insert into ModelosIntegracoesCmds(BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP, BBS_CdiComandoSQL) values (10010, 10010, '(TESTES) MOCK POSTMAN GET', 1, 30063, 'https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/userdata/#keyid#', 1, @SQL_CdiComandoSQL + 7)  
		/*REST*/ insert into ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida)                                      values (10022, 10010, 'keyid', 'keyid', 9, 1004)
          
		/*REST*/ insert into ModelosIntegracoesCmds(BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP, BBS_CdiComandoSQL, BBS_CdiEventoTransacao) values (10011, 10011, '(TESTES) MOCK POSTMAN PUT', 1, 30063, 'https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/userdata/#keyid#', 2, @SQL_CdiComandoSQL + 5, 2)  
		/*REST*/ insert into ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida)                                                              values (10023, 10011, 'keyid', 'keyid', 9, 1005)
		/*REST*/ insert into ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida)                                                              values (10024, 10011, 'field1', 'field1', 9, 1006)
		/*REST*/ insert into ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida)                                                              values (10025, 10011, 'field2', 'field2', 9, 1006)

		/*REST*/ insert into ModelosIntegracoesCmds(BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_NuiTipoEdicao, BBS_DssNomeObjeto, BBS_CdiComandoSQL, BBS_CdiEventoTransacao, BBS_CdiVerboHTTP) values (10012, 10012, '(TESTES) SAIDA REST 1106',  1, 15953, 0, 'https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v1/add', @SQL_CdiComandoSQL + 6, 2, 3)
		/*REST*/ insert into ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida)                                                                                 values (10026, 10012, 'nome',  'nome',  9, 1007)
		/*REST*/ insert into ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida)                                                                                 values (10027, 10012, 'email', 'email', 9, 1007)
		/*REST*/ insert into ModelosIntegracoesCmdsCpos(BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida)                                                                                 values (10028, 10012, 'id',    'id',    9, 1007)

		/*REST*/ insert into ModelosIntegracoesCmds(BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr) values (10013, 10013, '(TESTES) CONSULTAS REMOTAS OAUTH', 5)

		/*REST*/ insert into ModelosIntegracoesCmds(BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP) values (10014, 10014, '(TESTES) MOCK POSTMAN GET ARRAY', 1, 30063, 'https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io//api/testdataresult/arrayjson', 1)  

		/*OBJETO - 550 - ABA BASES DE DADOS*/
		insert into ServidoresIntegracoesBDs(BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor)                                values (01, 1, '(TESTES) SOAP CORREIOS',           10001, 1, 6, 1,  'https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl'); 
		insert into ServidoresIntegracoesBDs(BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor)                                values (02, 1, '(TESTES) REST SEM PARAMETROS',     10002, 1, 10, 1, 'http://echo.jsontest.com/key/value/one/two'); 
		insert into ServidoresIntegracoesBDs(BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor)                                values (03, 1, '(TESTES) REST COM PARAMETROS',     10003, 1, 10, 1, 'http://validate.jsontest.com/'); 
		insert into ServidoresIntegracoesBDs(BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor)                                values (04, 1, '(TESTES) REST COM PARAMETROS URL', 10004, 1, 10, 1, 'https://viacep.com.br/'); 
		insert into ServidoresIntegracoesBDs(BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor)                                values (05, 1, '(TESTES) REST ALTERACAO OBJ 2330', 10005, 2, 10, 1, ''); 
		insert into ServidoresIntegracoesBDs(BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor)                                values (06, 1, '(TESTES) REST MARCACAO PONTO',     10006, 2, 10, 0, '');
		insert into ServidoresIntegracoesBDs(BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor)                                values (07, 1, '(TESTES) REST ENTRADA BASEX64',    10007, 2, 10, 0, '');
		insert into ServidoresIntegracoesBDs(BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor, BBO_CdiTipoAutentConsWebServi) values (08, 1, '(TESTES) REST CORREIOS LOTE',      10008, 1, 6, 0,  'https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl', 1);
		insert into ServidoresIntegracoesBDs(BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor)                                values (09, 1, '(TESTES) MOCK POSTMAN GET',        10010, 1, 10, 1, 'https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/');
		insert into ServidoresIntegracoesBDs(BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor)                                values (10, 1, '(TESTES) MOCK POSTMAN PUT',        10011, 1, 10, 1, 'https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/'); 
		insert into ServidoresIntegracoesBDs(BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor)                                values (11, 1, '(TESTES) MOCK POSTMAN POST AT',    10012, 1, 10, 1, 'https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/'); 
		insert into ServidoresIntegracoesBDs(BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor, BBO_CdiTipoAutentConsWebServi) values (12, 1, '(TESTES) REST CONSULTAS REMOTAS',  10013, 2, 10, 1, '', 5); 
		insert into ServidoresIntegracoesBDs(BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor)                                values (13, 1, '(TESTES) MOCK POSTMAN GET ARRAY',  10014, 1, 10, 1, 'https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/');

		/*OUTROS AJUSTES PARA TESTES*/
		update FormulariosWFSobreps set BRH_CdiOpcao_Desativado = 0 /* CdiOpcao_Desativado (bind) */ where BRH_CdiFormularioWF = 407 /* CdiFormularioWF (bind) */

		begin try
			insert into dbo.FormulariosWFCampos(FWC_CdiFormularioWFCampo, FWC_CdiFormularioWF, FWC_CdiCampo, FWC_CdiClasseProcCpoPar, FWC_NuiSequencial, FWC_NuiOrdem, FWC_OplReferencia, FWC_OplLigacao, FWC_OplCampoBase, FWC_CdiCampoAgrupamento, FWC_OplDataBase, FWC_CdiOpcao_InfObrigatoria, FWC_OplDataBaseBloqueio, FWC_CdiObjetoLookup, FWC_DsbSqlLookupField, FWC_CdiOpcao_Protocolo, FWC_DssContDefault_String, FWC_DtdContDefault_DataHora, FWC_NuiContDefault_Inteiro, FWC_OplContDefault_Logico, FWC_VlnContDefault_Numerico, FWC_VrnContDefault_Numerico, FWC_CdiOpcao_Default, FWC_CdiDominio, FWC_D1sLiteral, FWC_D2sLiteral, FWC_D3sLiteral, FWC_D4sLiteral, FWC_D5sLiteral, FWC_D6sLiteral, FWC_D7sLiteral, FWC_D8sLiteral, FWC_OplInformacaoObrigatoria, FWC_OplProtocolo, FWC_OplLigacaoFilho, FWC_CdiOpcao_LookupTodasEtapas, FWC_DsbContDefault_Blob, FWC_OplDesabilitaCpoLkpParam, FWC_CdiCampoFlexivel, FWC_OplDesativado, FWC_OplCampoCondicao, FWC_DsbSqlCampoVirtual, FWC_CdiAcaoCampo, FWC_CdiOpcao_GdMultiTransacao, FWC_NuiOrigemRegistro, FWC_OplMultiplaSelecao, FWC_OplExibirAjuda, FWC_D1bAjudaCampo, FWC_D2bAjudaCampo, FWC_D3bAjudaCampo, FWC_D4bAjudaCampo, FWC_D5bAjudaCampo, FWC_D6bAjudaCampo, FWC_D7bAjudaCampo, FWC_D8bAjudaCampo)
			values(100505, 407, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, null, 0, null, null, 0, 0, 0, 0, 0, 214, 'ANEXO', null, null, null, null, null, null, null, 0, 0, 0, 0, null, 0, 0, 0, 0, null, 0, 0, 4, 0, 0, null, null, null, null, null, null, null, null);
		end try
		begin catch
			print ('OK -> Ajuste em FormulariosWFCampos para testar anexo já executado')
		end catch

		begin try
			insert into dbo.UsuariosAutenticacoes(JVQ_CdiUsuarioAutenticacao, JVQ_CdiUsuario, JVQ_CdsClientId, JVQ_CdsSecretKey, JVQ_CdiPerfil, JVQ_NuiMinutosValidadeToken, JVQ_D1sDescricao, JVQ_D2sDescricao, JVQ_D3sDescricao, JVQ_D4sDescricao, JVQ_D5sDescricao, JVQ_D6sDescricao, JVQ_D7sDescricao, JVQ_D8sDescricao)
			values(1, 1, '1658444F-EF87-47E7-B62C-F4F70BACE420', '@@/WJ8YRQ7Di9Sq/ci8cU2qJRdFdDvz9RefzHbOTyHNfoZpTtpog9cY/qjfqtQFtwxwo3w9bBxRbZmAyW/WkkcpUcaUbu+33yM', 1, 10, 'Apdata OAuth2', 'j2Bu6Bc6xuNZMM35x8ED4qN7cGJT5eH4', null, null, null, null, null, null);
		end try
		begin catch
			print ('OK -> Autenticao OAuth ja inserida')
		end catch

		exec sp_takeKeyForInsertion 'Consultas', @MaxKeyFromTable OUTPUT 

    	/*Desativa a tag de segurança para consultas*/
		update Usuarios Set USR_OplForcarUsoTAGApDesig = 0 where USR_CdiUsuario = 1

		/*QUERY_EXECUTE*/insert into dbo.Consultas(ACS_CdiConsulta, ACS_CdiConsultaGrupo, ACS_DtdAbertura, ACS_DsbConteudo, ACS_DssConsulta, ACS_OplExigirSenhaAdicl, ACS_CdiPais, ACS_OplFolhasDesativadas, ACS_OplPublico, ACS_NuiIcone, ACS_NuiIcone_Selecionado, ACS_NuiIcone_WorkArea, ACS_OplVisContratadoConectado, ACS_D1sNomeReferencia, ACS_D2sNomeReferencia, ACS_D3sNomeReferencia, ACS_D4sNomeReferencia, ACS_D5sNomeReferencia, ACS_D6sNomeReferencia, ACS_D7sNomeReferencia, ACS_D8sNomeReferencia, ACS_D1bAjuda, ACS_D2bAjuda, ACS_D3bAjuda, ACS_D4bAjuda, ACS_D5bAjuda, ACS_D6bAjuda, ACS_D7bAjuda, ACS_D8bAjuda, ACS_OplAltoConsumoRecurso, ACS_D1sConsultaExplicacao, ACS_D2sConsultaExplicacao, ACS_D3sConsultaExplicacao, ACS_D4sConsultaExplicacao, ACS_D5sConsultaExplicacao, ACS_D6sConsultaExplicacao, ACS_D7sConsultaExplicacao, ACS_D8sConsultaExplicacao, ACS_OplDesativado)
		/*QUERY_EXECUTE*/values(@MaxKeyFromTable, 75, null, 0x545046300654646153514C00035461670372631147756964436F6C6C6174696F6E54797065070D67634D5353514C5365727665720C446174616261736554797065070D64744D5353514C5365727665721044617461506970656C696E654E616D65060B426C6F624172717569766F0D4564697453514C41735465787409094C696E6B436F6C6F720707636C426C61636B084C696E6B5479706507126C74506172616D65746572697A656453514C164D617853514C4669656C64416C6961734C656E67746802000F53514C546578742E537472696E677301063073656C656374204C57435F4364694C616E63616D656E746F57462C204C57435F447362436F6E746575646F5F426C6F62061B202046726F6D204C616E63616D656E746F73574643616D706F7320067020207768657265204C57435F4364694C616E63616D656E746F574620696E202873656C656374204C57465F4364694C616E63616D656E746F57462046726F6D204C616E63616D656E746F735746207768657265204C57465F436469466F726D756C6172696F5746203D203A496446572906262020416E64204C57435F447362436F6E746575646F5F426C6F62206973206E6F74206E756C6C1200000000000753514C547970650706737153514C32074C69746572616C060B426C6F624172717569766F06506172616D730E01084461746154797065070C667457696465537472696E67044E616D6506044964465709506172616D5479706507097074556E6B6E6F776E00000D4E6F5472616E736C6174696F6E0800085464614669656C640864614669656C643105416C69617314170000004C616EC3A7616D656E746F20646520576F726B666C6F7708446174615479706507096474496E74656765720C446973706C6179576964746802080A4669656C64416C69617314170000004C616EC3A7616D656E746F20646520576F726B666C6F770B4669656C644C656E677468020A094669656C644E616D6506134C57435F4364694C616E63616D656E746F57460C53514C4669656C644E616D6506134C57435F4364694C616E63616D656E746F5746095461626C654E616D6506134C616E63616D656E746F73574643616D706F730000085464614669656C640864614669656C643205416C6961731410000000436F6E7465C3BA646F202D20426C6F6208446174615479706507066474424C4F420C446973706C6179576964746803AD0D0A4669656C64416C6961731410000000436F6E7465C3BA646F202D20426C6F620B4669656C644C656E677468038813094669656C644E616D6506144C57435F447362436F6E746575646F5F426C6F62084C696E6B61626C65080C53514C4669656C644E616D6506144C57435F447362436F6E746575646F5F426C6F62095461626C654E616D6506134C616E63616D656E746F73574643616D706F73000000, 'BlobArquivo', 0, 1, 0, 0, 0, 0, 0, 0, 'BlobArquivo', 'Español=', 'BlobArquivo', 'BlobArquivo', 'BlobArquivo', 'BlobArquivo', 'BlobArquivo', 'BlobArquivo', null, null, null, null, null, null, null, null, 0, '', '', '', '', '', '', '', '', 0);
	
	commit;
end
GO

/**********************************************************************
  7 - Generate Insert From Select Table
***********************************************************************/

/*
Example to use:

declare @insert varchar(max),
        @part   varchar(max),
        @start  int,
        @end    int;

set @start = 1;

exec sp_Generate_Inserts_From_Selects @schema = 'dbo',
                                      @table = 'ModelosIntegracoesCmds',
                                      @where  = '',
                                      @insert = @insert output;

-- Print one line to avoid the maximum 8000 characters problem
while len(@insert) > 0
begin
  set @end = charindex(char(10), @insert);

  if @end = 0
  begin
    set @end = len(@insert) + 1;
  end;

  print substring(@insert, @start, @end - 1);
  set @insert = substring(@insert, @end + 1, len(@insert) - @end + 1);
end;


*/

create or alter procedure sp_Generate_Inserts_From_Selects(@schema    varchar(200) = 'dbo',
                                                           @table     varchar(200),
                                                           @where     varchar(max) = null,
														   @fields    varchar(max) = null,
                                                           @top       int = null,
                                                           @insert    varchar(max) output)
as
begin
  declare @insert_fields varchar(max),
          @select        varchar(max),
          @error         varchar(500),
          @query         varchar(max);

  declare @values table(description varchar(max));

  set nocount on;

  -- Get columns
  select @insert_fields = isnull(@insert_fields + ', ', '') + c.name,
         @select = case type_name(c.system_type_id)
                      when 'varchar' then isnull(@select + ' + '', '' + ', '') + ' isnull('''''''' + cast(' + c.name + ' as varchar(max)) + '''''''', ''null'')'
                      when 'nvarchar' then isnull(@select + ' + '', '' + ', '') + ' isnull('''''''' + cast(' + c.name + ' as nvarchar(max)) + '''''''', ''null'')'
					  when 'datetime' then isnull(@select + ' + '', '' + ', '') + ' isnull('''''''' + convert(varchar(max), ' + c.name + ', 121) + '''''''', ''null'')'
                      when 'image' then isnull(@select + ' + '', '' + ', '') +  ' isnull(convert(varchar(max), cast(' + c.name + ' as varbinary(max)), 1), ''null'')' 
					  else isnull(@select + ' + '', '' + ', '') + ' isnull(cast(' + c.name + ' as varchar(max)), ''null'')'
                    end
    from sys.columns c with(nolock)
         inner join sys.tables t with(nolock) on t.object_id = c.object_id
         inner join sys.schemas s with(nolock) on s.schema_id = t.schema_id
   where s.name = @schema
     and t.name = @table
	 and c.name in (select * from string_split(isnull(@fields, c.name), ','));

  -- If there's no columns...
  if @insert_fields is null or @select is null
  begin
    set @error = 'There''s no ' + @schema + '.' + @table + ' inside the target database.';
    raiserror(@error, 16, 1);
    return;
  end;

  set @insert_fields = 'insert into ' + @schema + '.' + @table + '(' + @insert_fields + ')';

  if isnull(@where, '') <> '' and charindex('where', ltrim(rtrim(@where))) < 1
  begin
    set @where = 'where ' + @where;
  end
  else
  begin
    set @where = '';
  end;

  set @query = 'select ' + isnull('top(' + cast(@top as varchar) + ')', '') + @select + ' from ' + @schema + '.' + @table + ' with (nolock) ' + @where;

  insert into @values(description)

  exec(@query);

  set @insert = isnull(@insert + char(10), '') + '--' + upper(@schema + '.' + @table);

  select @insert = @insert + char(10) + @insert_fields + char(10) + 'values(' + v.description + ');' + char(10) + 'go' + char(10)
    from @values v
   where isnull(v.description, '') <> '';
end
GO

/**********************************************************************
  8 - Generate Insert From Select Table
***********************************************************************/

create or alter procedure sp_Simple_Generate_Inserts_From_Selects(@table  varchar(200),
                                                                  @where  varchar(max) = null,
																  @fields varchar(max) = null
																 )
as
begin
	declare @insert varchar(max),
	        @part   varchar(max),
		    @start  int,
	        @end    int;

	set @start = 1;

	exec sp_Generate_Inserts_From_Selects @schema = 'dbo',
		                                  @table  = @table,
			                              @where  = @where,
										  @fields = @fields,
				                          @insert = @insert output;

-- Print one line to avoid the maximum 8000 characters problem
	while len(@insert) > 0
	begin
		set @end = charindex(char(10), @insert);

		if @end = 0
		begin
			set @end = len(@insert) + 1;
		end;

		print substring(@insert, @start, @end - 1);
		set @insert = substring(@insert, @end + 1, len(@insert) - @end + 1);
	end;
end
GO

/**********************************************************************
    9 - Convert Binary To Text
***********************************************************************/

create or alter procedure sp_ConvertBinaryToText(@TableName[sysname],
                                                 @FieldName varchar(200),
												 @Criteria varchar(200)
                                               )
as
begin
	declare @sqlcommand nvarchar(max)
	declare @paramdefinition nvarchar(max)
	declare @contentText nvarchar(max)
	
	set @sqlcommand = N'select @Value = convert(varchar(max), convert(varbinary(max), ' + @FieldName + ')) from ' + @TableName + ' where ' + @Criteria  
	set @paramdefinition = N'@Value nvarchar(max) OUTPUT'

	exec sp_executesql @sqlcommand, @paramdefinition, @Value = @contentText OUTPUT   

	select @contentText
end
GO

/**********************************************************************
    10 - SELECT INTO 
***********************************************************************/

create or alter procedure sp_Select_Into(@schema    varchar(200) = 'dbo',
                                         @table     varchar(200),
                                         @where     varchar(max) = null,
										 @fields    varchar(max) = null
                                        )
as
begin
  declare @insert_fields varchar(max),
          @select        varchar(max),
          @error         varchar(500),
          @query         varchar(max);

  set nocount on;

  -- Get columns
  select @insert_fields = isnull(@insert_fields + ', ', '') + c.name
    from sys.columns c with(nolock)
         inner join sys.tables t with(nolock) on t.object_id = c.object_id
         inner join sys.schemas s with(nolock) on s.schema_id = t.schema_id
   where s.name = @schema
     and t.name = @table
	 and c.name in (select * from string_split(isnull(@fields, c.name), ','));

  -- If there's no columns...
  if @insert_fields is null
  begin
    set @error = 'There''s no ' + @schema + '.' + @table + ' inside the target database.';
    raiserror(@error, 16, 1);
    return;
  end;

  set @insert_fields = 'insert into ' + @table + ' (' + @insert_fields + ') Select ' + @insert_fields + ' From ' + @table;

  if isnull(@where, '') <> '' and charindex('where', ltrim(rtrim(@where))) < 1
  begin
    set @where = 'where ' + @where;
  end
  else
  begin
    set @where = '';
  end;

  set @insert_fields = @insert_fields + ' ' + @where;

  print @insert_fields;
end
GO


/**********************************************************************
    11 - STRING SPLIT (COMPATIBILIDADE DE FUNCAO)
***********************************************************************/

/*
create function dbo.string_split (  
    @list varchar(1000), @delim varchar(1) = ','
)
returns table
as
return (
    select 
        x.f.value( '.', 'varchar(50)' ) AS [value]
    from ( 
        select cast ( '<v><i>' + replace ( @list, @delim, '</i><i>' ) + '</i></v>' AS xml ) AS x 
    ) AS d
    cross apply x.nodes( '//v/i' ) x( f )
)
GO
*/

exec sp_StandardData_FixedValues 0 
GO
