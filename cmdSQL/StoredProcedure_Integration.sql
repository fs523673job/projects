use INTEGRATION_BETA
GO

IF DB_NAME() <> 'INTEGRATION_BETA'
BEGIN
  RAISERROR('Este script só pode ser executado no banco de dados INTEGRATION_BETA.', 16, 1);
  SET NOEXEC ON;
END;
GO

drop procedure if exists sp_deleteCascate 
GO
drop procedure if exists sp_deleteCascateRegistry
GO
drop procedure if exists sp_Create_Aux_Table
GO
drop procedure if exists sp_StandardData_FixedValues 
GO
drop procedure if exists sp_delete 
GO
drop procedure if exists sp_lastIdTable 
GO
drop procedure if exists sp_takeKeyForInsertion 
GO
drop function if exists fn_getTableCount 
GO
drop function if exists fn_getTableMaxKey 
GO
drop function if exists fn_getNuiPkLimiteApdata 
GO
drop function if exists fn_lastIdTable 
GO
drop procedure if exists sp_infoApDataPkLimit 
GO
drop procedure if exists sp_deleteOptionByApDataRange 
GO
drop procedure if exists sp_clearAllDataIntegration 
GO
drop procedure if exists sp_Generate_Inserts_From_Selects 
GO
drop procedure if exists sp_Simple_Generate_Inserts_From_Selects 
GO
drop procedure if exists sp_ConvertBinaryToText 
GO
drop procedure if exists sp_Select_Into 
GO
drop procedure if exists sp_Execute_Insert
GO
drop procedure if exists sp_Execute_Insert_Key
GO
drop procedure if exists sp_Execute_Insert_Key_ForeignKey
GO
drop procedure if exists sp_Execute_Update
GO
drop procedure if exists sp_Execute_Insert_ThreeKey
GO
drop procedure if exists sp_Execute_Delete
GO
drop procedure if exists sp_getKeyValue
GO
drop procedure if exists sp_getNewCriteria
GO
drop function if exists fn_getPKFieldName 
GO
drop procedure if exists sp_Execute_Or_Insert
GO
drop procedure if exists sp_DuplicarRegistroComAlteracoes
GO
drop procedure if exists sp_GetLastIdFromTable
GO
drop procedure if exists sp_CriarUsuario
GO

/*** FUNÇÕES UTILITÁRIAS **********************************************/
/*** PROCEDURES UTILITÁRIAS *******************************************/

/**********************************************************************
    1 - OVERLOAD SP_EXECUTESQL (Insert)
***********************************************************************/

create or alter procedure sp_Execute_Insert(@schema         varchar(200) = 'dbo',
                                            @ordNum         int = 0,
											@table          varchar(200),
											@fields         varchar(max) = null,
											@values         varchar(max) = null,
											@showCmd        int = 0,
											@customMsgError varchar(max) = null
                                           )
as
begin
  declare @insert_fields nvarchar(max);

  set nocount on

  set @insert_fields = 'insert into ' + @schema + '.' + @table + '(' + @fields + ') values ' + '(' + @values + ')';
  
  begin try
	exec sp_ExecuteSQL @insert_fields
	if (@showCmd = 1)
		print 'After Execute Insert: OrdNum [' + cast(@ordNum as char(03)) + '] - Table  [' + @table + '] - [rows affected  =  ' + cast(@@ROWCOUNT as char(03)) + ']' + '-> Command: ' + @insert_fields
    else 
		print 'After Execute Insert: OrdNum [' + cast(@ordNum as char(03)) + '] - Table  [' + @table + '] -> Command: ' + @insert_fields
  end try
  begin catch
    if (@customMsgError is null) 
	begin
		print '--################### ERROR BEGINS ##################'
		print 'After Execute Insert: OrdNum [' + cast(@ordNum as char(03)) + '] - Table [' + @table + '] - OrdNum [' + cast(@ordNum as char(03)) + '] - Error Message: [' + ERROR_MESSAGE() + ']' + '-> Command:  ' + @insert_fields
		print '--################### ERROR ENDS ####################'
	end
	else
	  print 'After Execute Insert: OrdNum [' + cast(@ordNum as char(03)) + '] - Table [' + @table + '] - OrdNum [' + cast(@ordNum as char(03)) + ']' + 'Custom Message: [' + @customMsgError + ']'
  end catch
end
GO

/**********************************************************************
    1.1 - OVERLOAD SP_EXECUTESQL (Insert One Key)
***********************************************************************/

create or alter procedure sp_Execute_Insert_Key(@schema   varchar(200) = 'dbo',
                                                @ordNum   int = 0,
										    	@table    varchar(200),
										   	    @fields   varchar(max) = null,
												@keyTable int = 0,
												@incKey   int = 0,
											    @values   varchar(max) = null,
											    @showCmd  int = 0
                                               )
as
begin
  declare @insert_fields nvarchar(max);

  set nocount on

  set @insert_fields = 'insert into ' + @schema + '.' + @table + '(' + @fields + ') values ' + '(' + trim(cast(@keyTable + @incKey as char(10))) + ',' + @values + ')';
  
  begin try
	exec sp_ExecuteSQL @insert_fields
	if (@showCmd = 1)
		print 'After Execute Insert: OrdNum [' + cast(@ordNum as char(03)) + '] - Table  [' + @table + '] - [rows affected  =  ' + cast(@@ROWCOUNT as char(03)) + ']' + '-> Command: ' + @insert_fields
    else 
		print 'After Execute Insert: OrdNum [' + cast(@ordNum as char(03)) + '] - Table  [' + @table + '] -> Command: ' + @insert_fields
  end try
  begin catch
    print '--################### ERROR BEGINS ##################'
	print 'After Execute Insert: OrdNum [' + cast(@ordNum as char(03)) + '] - Table [' + @table + '] - OrdNum [' + cast(@ordNum as char(03)) + '] - Error Message: [' + ERROR_MESSAGE() + ']' + '-> Command:  ' + @insert_fields
	print '--################### ERROR ENDS ####################'
  end catch
end
GO

/**********************************************************************
    1.2 - OVERLOAD SP_EXECUTESQL (Insert Two Key)
***********************************************************************/

create or alter procedure sp_Execute_Insert_Key_ForeignKey(@schema   varchar(200) = 'dbo',
                                                           @ordNum   int = 0,
										    	           @table    varchar(200),
										   	               @fields   varchar(max) = null,
												           @keyTable int = 0,
												           @incKey   int = 0,
											               @forKey   int = 0,
														   @incFor   int = 0,
														   @values   varchar(max) = null,
											               @showCmd  int = 0
                                                          )
as
begin
  declare @insert_fields nvarchar(max);

  set nocount on

  if @keyTable is null or @forKey is null
  begin
	print '--################### ERROR BEGINS ##################'
    print 'After Execute Insert: OrdNum [' + cast(@ordNum as char(03)) + '] - Table [' + @table + '] - OrdNum [' + cast(@ordNum as char(03)) + '] - Error Message: [@keyTable ' + isnull(cast(@keyTable as varchar(10)), 'NULL') + '], @forKey: [' + isnull(cast(@forKey as varchar(10)), 'NULL') + ']';
	print '--################### ERROR ENDS ####################'
    return;
  end

  set @insert_fields = 'insert into ' + @schema + '.' + @table + '(' + @fields + ') values ' + '(' + trim(cast(@keyTable + @incKey as char(10))) + ',' + trim(cast(@forKey + @incFor as char(10))) + ',' + @values + ')';
  
  begin try
	exec sp_ExecuteSQL @insert_fields
	if (@showCmd = 1)
		print 'After Execute Insert: OrdNum [' + cast(@ordNum as char(03)) + '] - Table  [' + @table + '] - [rows affected  =  ' + cast(@@ROWCOUNT as char(03)) + ']' + '-> Command: ' + @insert_fields
    else 
		print 'After Execute Insert: OrdNum [' + cast(@ordNum as char(03)) + '] - Table  [' + @table + '] -> Command: ' + @insert_fields
  end try
  begin catch
    print '--################### ERROR BEGINS ##################'
	print 'After Execute Insert: OrdNum [' + cast(@ordNum as char(03)) + '] - Table [' + @table + '] - OrdNum [' + cast(@ordNum as char(03)) + '] - Error Message: [' + ERROR_MESSAGE() + ']' + '-> Command:  ' + @insert_fields
	print '--################### ERROR ENDS ####################'
  end catch
end
GO

/**********************************************************************
    1.3 - OVERLOAD SP_EXECUTESQL (Update)
***********************************************************************/

create or alter procedure sp_Execute_Update(@schema   varchar(200) = 'dbo',
                                            @ordNum   int = 0,
											@table    varchar(200),
											@fields   varchar(max) = null,
											@where    varchar(max) = null,
											@showCmd  int = 0
                                           )
as
begin
  declare @update_fields nvarchar(max);

  set nocount on

  set @update_fields = 'update ' + @schema + '.' + @table + ' set ' + @fields + ' where ' + @where;
  
  begin try
	exec sp_ExecuteSQL @update_fields
	if (@showCmd = 1)
		print 'After Execute Update: OrdNum [' + cast(@ordNum as char(03)) + '] - Table  [' + @table + '] - [rows affected  =  ' + cast(@@ROWCOUNT as char(03)) + ']' + '-> Command: ' + @update_fields
    else 
		print 'After Execute Update: OrdNum [' + cast(@ordNum as char(03)) + '] - Table  [' + @table + '] -> Command: ' + @update_fields
  end try
  begin catch
    print '--################### ERROR BEGINS ##################'
	print 'After Execute Update: OrdNum [' + cast(@ordNum as char(03)) + '] - Table [' + @table + '] - OrdNum [' + cast(@ordNum as char(03)) + '] - Error Message: [' + ERROR_MESSAGE() + ']' + '-> Command:  ' + @update_fields
	print '--################### ERROR ENDS ####################'
  end catch
end
GO

/**********************************************************************
    1.4 - OVERLOAD SP_EXECUTESQL (Insert Three Key)
***********************************************************************/

create or alter procedure sp_Execute_Insert_ThreeKey(@schema       varchar(200) = 'dbo',
                                                     @ordNum       int = 0,
										    	     @table        varchar(200),
										   	         @fields       varchar(max) = null,
												     @keyOne       int = 0,
												     @incKeyOne    int = 0,
											         @KeyTwo       int = 0,
													 @incKeyTwoFor int = 0,
													 @KeyThree     int = 0,
													 @incKeyThree  int = 0,
													 @values       varchar(max) = null,
											         @showCmd      int = 0
                                                    )
as
begin
  declare @insert_fields nvarchar(max);

  set nocount on

  if @keyOne is null or @KeyTwo is null or @KeyThree is null 
  begin
	print '--################### ERROR BEGINS ##################'
    print 'After Execute Insert: OrdNum [' + cast(@ordNum as char(03)) + '] - Table [' + @table + '] - OrdNum [' + cast(@ordNum as char(03)) + '] - Error Message: @keyOne: [' + isnull(cast(@keyOne as varchar(10)), 'NULL') + '], @KeyTwo: [' + isnull(cast(@KeyTwo as varchar(10)), 'NULL') + '], @KeyThree: [' + isnull(cast(@KeyThree as varchar(10)), 'NULL') + ']';
	print '--################### ERROR ENDS ####################'
    return;
  end


  set @insert_fields = 'insert into ' + @schema + '.' + @table + '(' + @fields + ') values ' + '(' + trim(cast(@keyOne + @incKeyOne as char(10))) + ',' + trim(cast(@KeyTwo + @incKeyTwoFor as char(10))) + ',' + trim(cast(@KeyThree + @incKeyThree as char(10))) + ',' + @values + ')';
  
  begin try
	exec sp_ExecuteSQL @insert_fields
	if (@showCmd = 1)
		print 'After Execute Insert: OrdNum [' + cast(@ordNum as char(03)) + '] - Table  [' + @table + '] - [rows affected  =  ' + cast(@@ROWCOUNT as char(03)) + ']' + '-> Command: ' + @insert_fields
    else 
		print 'After Execute Insert: OrdNum [' + cast(@ordNum as char(03)) + '] - Table  [' + @table + '] -> Command: ' + @insert_fields
  end try
  begin catch
    print '--################### ERROR BEGINS ##################'
	print 'After Execute Insert: OrdNum [' + cast(@ordNum as char(03)) + '] - Table [' + @table + '] - OrdNum [' + cast(@ordNum as char(03)) + '] - Error Message: [' + ERROR_MESSAGE() + ']' + '-> Command:  ' + @insert_fields
	print '--################### ERROR ENDS ####################'
  end catch
end
GO

/**********************************************************************
    1.5 - OVERLOAD SP_EXECUTESQL (Delete)
***********************************************************************/

create or alter procedure sp_Execute_Delete(@schema   varchar(200) = 'dbo',
                                            @ordNum   int = 0,
											@table    varchar(200),
											@where    varchar(max) = null,
											@showCmd  int = 0
                                           )
as
begin
  declare @delete_fields nvarchar(max);

  set nocount on

  set @delete_fields = 'delete from ' + @schema + '.' + @table + ' where ' + @where;
  
  begin try
	exec sp_ExecuteSQL @delete_fields
	if (@showCmd = 1)
		print 'After Execute Update: OrdNum [' + cast(@ordNum as char(03)) + '] - Table  [' + @table + '] - [rows affected  =  ' + cast(@@ROWCOUNT as char(03)) + ']' + '-> Command: ' + @delete_fields
    else 
		print 'After Execute Update: OrdNum [' + cast(@ordNum as char(03)) + '] - Table  [' + @table + '] -> Command: ' + @delete_fields
  end try
  begin catch
    print '--################### ERROR BEGINS ##################'
	print 'After Execute Update: OrdNum [' + cast(@ordNum as char(03)) + '] - Table [' + @table + '] - OrdNum [' + cast(@ordNum as char(03)) + '] - Error Message: [' + ERROR_MESSAGE() + ']' + '-> Command:  ' + @delete_fields
	print '--################### ERROR ENDS ####################'
  end catch
end
GO

/************************************************************ 
	  2 -	Store Procedure Delete Tables Related
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
		    begin try
				exec sp_ExecuteSQL @sqlcommand
				print 'After Execute Delete Table: ['+ @RefTable + '] -> Command: [' + @sqlcommand + '] -> [rows affected  =  ' + cast(@@ROWCOUNT as char(03)) + ']'
			end try
			begin catch
			    print '--################### ERROR BEGINS ##################'
				print 'After Execute Delete Table: ['+ @RefTable + '] -> Error: [' + ERROR_MESSAGE() + '] -> Command: [' + @sqlcommand + '] -> [rows affected  =  ' + cast(@@ROWCOUNT as char(03)) + ']'
				print '--################### ERROR ENDS ####################'
			end catch
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
	    begin try
			exec sp_ExecuteSQL @sqlcommand
			print 'After Execute Delete Table: ['+ @TableName + '] -> Command: [' + @sqlcommand + '] -> [rows affected  =  ' + cast(@@ROWCOUNT as char(03)) + ']'
		end try
		begin catch
		    print '--################### ERROR BEGINS ##################'
			print 'After Execute Delete Table: ['+ @RefTable + '] -> Error: [' + ERROR_MESSAGE() + '] -> Command: [' + @sqlcommand + ']'
			print '--################### ERROR ENDS ####################'
		end catch
	end
	else
		print @sqlcommand
end
GO

/************************************************************ 
  2.1 - Store Procedure Delete Only Registry Tables Related
*************************************************************/

--create procedure sp_deleteCascateRegistry
create or alter procedure sp_deleteCascateRegistry
  (
		@TableName varchar(50), 
		@Criteria nvarchar(50),
		@OnlyStrCmd int = 1,
		@DeleteOrder int = 0
  )
as
begin
    set nocount on

	declare @RefTable varchar(80)
	declare @RefField varchar(80)
	declare @auxRefTable varchar(80)
	declare @sqlcommand nvarchar(4000)
	declare @paramdefinition nvarchar(500)
	declare @FieldPrimaryKey varchar(80)
	declare @FieldKeyValue int
	declare @NewCriteria nvarchar(50)

	set @NewCriteria = ''
	set @FieldKeyValue = 0
	set @DeleteOrder = @DeleteOrder + 1

	begin try
		select @FieldPrimaryKey = [dbo].[fn_getPKFieldName](@TableName)
	end try
	begin catch
	  print '[' + @TableName + ']' + ERROR_MESSAGE()
	end catch

	set @sqlcommand = 
	  N'select top 1 @Value = ' + @FieldPrimaryKey + 
	   '  FROM ' + @TableName +
       '  WHERE ' + @Criteria

	set @paramdefinition = N'@Value int OUTPUT' 

	begin try
		exec sp_executesql @sqlcommand, @paramdefinition, @Value = @FieldKeyValue OUTPUT
	end try
	begin catch
	  print '[' + @sqlcommand + '] ' + ERROR_MESSAGE()
	end catch
	
	if @FieldKeyValue = 0
		set @NewCriteria = @FieldPrimaryKey + ' = -1 '
	else
		set @NewCriteria = @FieldPrimaryKey + ' = ' + trim(cast(@FieldKeyValue as char(15)))

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
		if @FieldKeyValue = 0
			set @NewCriteria = @RefField + ' = -1 '
		else
			set @NewCriteria = @RefField + ' = ' + trim(cast(@FieldKeyValue as char(15)))

		set @sqlcommand = 'DELETE FROM [' + @RefTable + '] WHERE ' + @NewCriteria

		if (( @auxRefTable <> @RefTable ) and (@RefTable <> @TableName))
			exec sp_deleteCascateRegistry @RefTable, @NewCriteria, @OnlyStrCmd, @DeleteOrder
		
	    begin try
			if (@OnlyStrCmd is null) or (@OnlyStrCmd = 0)
				exec sp_ExecuteSQL @sqlcommand
			print '[' + cast(@DeleteOrder as char(03)) + '] After Execute Delete Table: ['+ @RefTable + '] -> Command: [' + @sqlcommand + '] -> [rows affected  =  ' + cast(@@ROWCOUNT as char(03)) + ']'
		end try
		begin catch
		    print '--################### ERROR BEGINS ##################'
			print '[' + cast(@DeleteOrder as char(03)) + '] After Execute Delete Table: ['+ @RefTable + '] -> Error: [' + ERROR_MESSAGE() + '] -> Command: [' + @sqlcommand + '] -> [rows affected  =  ' + cast(@@ROWCOUNT as char(03)) + ']'
			print '--################### ERROR ENDS ####################'
		end catch

		set @auxRefTable = @RefTable

		fetch next from localCursor
		into @RefTable, @RefField
	end

	close localCursor;
	deallocate localCursor;

	set @DeleteOrder = @DeleteOrder + 1

	set @sqlcommand = 'DELETE FROM [' + @TableName + '] WHERE ' + @Criteria
	
    begin try
		if (@OnlyStrCmd is null) or (@OnlyStrCmd = 0)
			exec sp_ExecuteSQL @sqlcommand
		print  '[' + cast(@DeleteOrder as char(03)) + '] After Execute Delete Table: ['+ @TableName + '] -> Command: [' + @sqlcommand + '] -> [rows affected  =  ' + cast(@@ROWCOUNT as char(03)) + ']'
	end try
	begin catch
	    print '--################### ERROR BEGINS ##################'
		print '[' + cast(@DeleteOrder as char(03)) + '] After Execute Delete Table: ['+ @RefTable + '] -> Error: [' + ERROR_MESSAGE() + '] -> Command: [' + @sqlcommand + ']'
		print '--################### ERROR ENDS ####################'
	end catch
end
GO

/************************************************************ 
	  2.2 -	Store Procedure Delete Tables Related
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
	    begin try
			exec sp_ExecuteSQL @sqlcommand
			print 'After Execute Delete Table: ['+ @TableName + '] -> Command: [' + @sqlcommand + '] -> [rows affected  =  ' + cast(@@ROWCOUNT as char(03)) + ']'
		end try
		begin catch
		    print '--################### ERROR BEGINS ##################'
			print 'After Execute Delete Table: ['+ @TableName + '] -> Error: [' + ERROR_MESSAGE() + '] -> Command: [' + @sqlcommand + '] -> [rows affected  =  ' + cast(@@ROWCOUNT as char(03)) + ']'
			print '--################### ERROR ENDS ####################'
		end catch
	end
	else
		print @sqlcommand
end
GO

/************************************************************ 
     3 - Store Procedure For Return Last Id From Table
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

	set @sqlcommand = N'select @Value = IsNull(Max(' + @FieldPK + '), 0) from ' + @TableName
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
    3.1 - Store Procedure For Return Last Id From Table
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
     3.2 - Function For Return Last Id From Table
*************************************************************/

create or alter procedure sp_getNewCriteria
  (
	@TableName[sysname],
	@Criteria nvarchar(1000),
	@NewCriteria nvarchar(1000) = null OUTPUT
  )
as
begin
	declare @sqlcommand nvarchar(4000)
	declare @paramdefinition nvarchar(500)
	declare @FieldPrimaryKey varchar(80)
	declare @ValueAux int

	select @FieldPrimaryKey = [dbo].[fn_getPKFieldName](@TableName)

	set @sqlcommand = 
	  N'select @Value = ' + @FieldPrimaryKey + 
	   '  FROM ' + @TableName +
       '  WHERE ' + @Criteria
	    
	set @paramdefinition = N'@Value int OUTPUT' 

	exec sp_executesql @sqlcommand, @paramdefinition, @Value = @ValueAux OUTPUT

	select @NewCriteria = @FieldPrimaryKey + ' = ' + trim(cast(@ValueAux as char(7)))
end
GO

create or alter procedure sp_getKeyValue
  (
	@TableName[sysname],
	@Criteria nvarchar(1000),
	@KeyValue int = null OUTPUT
  )
as
begin
	declare @sqlcommand nvarchar(4000)
	declare @paramdefinition nvarchar(500)
	declare @FieldPrimaryKey varchar(80)
	declare @ValueAux int

	select @FieldPrimaryKey = [dbo].[fn_getPKFieldName](@TableName)

	set @sqlcommand = 
	  N'select @Value = ' + @FieldPrimaryKey + 
	   '  FROM ' + @TableName +
       '  WHERE ' + @Criteria
	    
	set @paramdefinition = N'@Value int OUTPUT' 

	exec sp_executesql @sqlcommand, @paramdefinition, @Value = @ValueAux OUTPUT

	select @KeyValue = @ValueAux
end
GO

--create function fn_getPKFieldName(@TableName[sysname])
create or alter function fn_getPKFieldName(@TableName[sysname])
returns varchar(1000)
as
begin
    return (select top 1 C.COLUMN_NAME as PK
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
    declare @result int
	
	select @result = Max(z.COLUMN_NAME)   
	   from (
              SELECT u.TABLE_NAME,u.COLUMN_NAME
              FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE u
              left join INFORMATION_SCHEMA.TABLES t on u.TABLE_NAME = t.TABLE_NAME
              WHERE OBJECTPROPERTY(OBJECT_ID(CONSTRAINT_SCHEMA + '.' + QUOTENAME(CONSTRAINT_NAME)), 'IsPrimaryKey') = 1
              AND u.TABLE_NAME = t.TABLE_NAME 
              and u.TABLE_NAME = @TableName
			) as z

	return @result
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
	4 - Store Procedure For Return Info On Limits Of Table
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
   5 - Store Procedure Delete Cascate Table For ApData Limit Range
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
  6 - Store Procedure User For Clear Data From Tables of Integration
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
	exec sp_deleteOptionByApDataRange 'EstruturasADxSitsAtivs', 1, 1
	exec sp_deleteOptionByApDataRange 'EstruturasADGrupos', 1, 1
	exec sp_deleteOptionByApDataRange 'EstruturasADProps', 1, 1
	exec sp_deleteOptionByApDataRange 'EstruturasAD', 1, 1
	exec sp_deleteOptionByApDataRange 'DefSisIntegracaoAD', 1, 1
	exec sp_deleteOptionByApDataRange 'ComandosSQLsSobs', 1, 1
	exec sp_deleteOptionByApDataRange 'ComandosSQLs', 1, 1
	exec sp_deleteOptionByApDataRange 'ComandosSQLsGrupos', 1, 1
	exec sp_deleteOptionByApDataRange 'LogsIntegracoes', 1, 1
	exec sp_deleteOptionByApDataRange 'LogsIntegracoesCampos', 1, 1
	exec sp_deleteOptionByApDataRange 'LogsIntegracoesServidores', 1, 1
	
	/*other deletes*/
	exec sp_Execute_Delete 'dbo', 01, 'FormulariosWFCampos', 'FWC_CdiFormularioWFCampo = 100505' 
	exec sp_Execute_Delete 'dbo', 02, 'UsuariosAutenticacoes', 'JVQ_CdiUsuarioAutenticacao = 1'
	exec sp_Execute_Delete 'dbo', 03, 'ControlesSeqsInternos', 'DJN_CdiTabela > 0'
	exec sp_Execute_Delete 'dbo', 04, 'UsuariosDesabilitacoes', 'USD_CdiUsuario = 1'
	exec sp_deleteCascate 'EstruturasAD', '= 1001', 0
	exec sp_deleteCascate 'Defaults', '> 12', 0
	exec sp_deleteCascate 'ListasGenericas', '> 50010', 0
end
GO

/**********************************************************************
    07 - SELECT INTO 
***********************************************************************/

create or alter procedure sp_Create_Aux_Table(@newRecs int = 15)
as
BEGIN
    SET NOCOUNT ON;
    
    IF OBJECT_ID('dbo.IntegrationMonitoracao', 'U') IS NOT NULL
        DROP TABLE dbo.IntegrationMonitoracao;

    CREATE TABLE dbo.IntegrationMonitoracao(
        id_integrationmonitoracao INT,
        id_cargo INT,
        id_pais INT,
        id_areaatuacao INT,
        status_ INT,
        datatransacao DATETIME,
        evento INT,
        error VARCHAR(256),
        desc_cargo VARCHAR(256)
    );

    ;WITH Numbers AS (
        SELECT TOP (@newRecs) ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS n
        FROM sys.all_columns  
    )
    INSERT INTO dbo.IntegrationMonitoracao (
        id_integrationmonitoracao,
        id_cargo,
        id_pais,
        id_areaatuacao,
        status_,
        datatransacao,
        evento,
        error,
        desc_cargo
    )
    SELECT
        n,                                               -- id_integrationmonitoracao
        n + 100,                                         -- id_cargo
        n + 200,                                         -- id_pais
        n + 300,                                         -- id_areaatuacao
        n % 2,                                           -- status_, alterna entre 0 e 1
        DATEADD(MINUTE, n, GETDATE()),                   -- datatransacao, incrementa minutos
        n % 3,                                           -- evento, valores entre 0 e 2
        'Mock error message ' + CAST(n AS VARCHAR(10)),  -- error
        'Mock description ' + CAST(n AS VARCHAR(10))     -- desc_cargo
    FROM Numbers;
END;
GO

/**********************************************************************
    7.1 - OVERLOAD SP_EXECUTESQL (Insert our Update)
***********************************************************************/

create or alter procedure sp_Execute_Or_Insert(
    @schema   varchar(200) = 'dbo',
    @ordNum   int = 0,
    @table    varchar(200),
    @keyField varchar(200),
    @keyValue varchar(200),
    @keyInc   varchar(200),
    @fields   varchar(max) = null,
    @values   varchar(max) = null,
    @showCmd  int = 1
)
as
begin
    declare @exists int;
    declare @sql nvarchar(max);
    declare @params nvarchar(max);
    declare @keyIncrement int;
    declare @updateClause nvarchar(max) = '';
    declare @i int = 1;
    declare @field nvarchar(200);
    declare @value nvarchar(max);
    declare @fieldList table (Field nvarchar(200), RowNum int identity(1,1));
    declare @valueList table (Value nvarchar(max), RowNum int identity(1,1));

    set nocount on;

    -- Verifica se o registro existe
    set @sql = 'select @exists = count(1) from ' + @schema + '.' + @table + ' where ' + @keyField + ' = ' + @keyValue;
    set @params = N'@exists int output, @keyValue varchar(200)';

    exec sp_executesql @sql, @params, @exists = @exists output, @keyValue = @keyValue;

    if @exists > 0
    begin
        -- Insere os campos e valores em tabelas temporárias com numeração de linha
        insert into @fieldList(Field) select value from string_split(@fields, ',');
        insert into @valueList(Value) select value from string_split(@values, ',');

        -- Constrói a cláusula SET
        while @i <= (select count(*) from @fieldList)
        begin
            select @field = Field from @fieldList where RowNum = @i;
            select @value = Value from @valueList where RowNum = @i;

            if @updateClause <> ''
                set @updateClause = @updateClause + ', ';

            set @updateClause = @updateClause + @field + ' = ' + @value;

            set @i = @i + 1;
        end

        -- Monta a string SQL para o UPDATE
        set @sql = 'update ' + @schema + '.' + @table + ' set ' + @updateClause + ' where ' + @keyField + ' = ' + @keyValue;

        begin try
            exec sp_executesql @sql;
            if (@showCmd = 1)
                print 'After Execute Update: OrdNum [' + cast(@ordNum as char(03)) + '] - Table  [' + @table + '] - [rows affected = ' + cast(@@ROWCOUNT as char(03)) + '] -> Command: ' + @sql;
            else 
                print 'After Execute Update: OrdNum [' + cast(@ordNum as char(03)) + '] - Table  [' + @table + '] -> Command: ' + @sql;
        end try
        begin catch
            print '--################### ERROR BEGINS ##################';
            print 'After Execute Update: OrdNum [' + cast(@ordNum as char(03)) + '] - Table [' + @table + '] - OrdNum [' + cast(@ordNum as char(03)) + '] - Error Message: [' + ERROR_MESSAGE() + ']' + ' -> Command: ' + @sql;
            print '--################### ERROR ENDS ####################';
        end catch
    end
    else
    begin
        -- Tratamento para a inserção
        if (@keyInc <> '')
        begin
            exec sp_takeKeyForInsertion @table, @keyIncrement OUTPUT;
            set @fields = @keyInc + ',' + @fields;
            set @values = cast(@keyIncrement as varchar(10)) + ',' + @values;
        end

        set @sql = 'insert into ' + @schema + '.' + @table + ' (' + @fields + ') values (' + @values + ')';
        
        begin try
            exec sp_executesql @sql;
            if (@showCmd = 1)
                print 'After Execute Insert: OrdNum [' + cast(@ordNum as char(03)) + '] - Table  [' + @table + '] - [rows affected = ' + cast(@@ROWCOUNT as char(03)) + '] -> Command: ' + @sql;
            else 
                print 'After Execute Insert: OrdNum [' + cast(@ordNum as char(03)) + '] - Table  [' + @table + '] -> Command: ' + @sql;
        end try
        begin catch
            print '--################### ERROR BEGINS ##################';
            print 'After Execute Insert: OrdNum [' + cast(@ordNum as char(03)) + '] - Table [' + @table + '] - OrdNum [' + cast(@ordNum as char(03)) + '] - Error Message: [' + ERROR_MESSAGE() + ']' + ' -> Command: ' + @sql;
            print '--################### ERROR ENDS ####################';
        end catch
    end
end
GO

/**********************************************************************
    7.2 - DUPLICAR REGISTRO
***********************************************************************/

create or alter procedure sp_DuplicarRegistroComAlteracoes(@TableName NVARCHAR(128),
                                                           @PrimaryKeyColumn NVARCHAR(128),
                                                           @PrimaryKeyValue SQL_VARIANT, 
                                                           @CamposAlterar NVARCHAR(MAX) = NULL,  -- Ex: 'Coluna1, Coluna2'
                                                           @NovosValores NVARCHAR(MAX) = NULL,     -- Ex: 'Valor1, Valor2'
														   @NovoValorChavePrimaria INT OUTPUT    -- Parâmetro de saída para a chave primária
                                                          ) 
AS
BEGIN
    SET NOCOUNT ON;

    DECLARE @sql NVARCHAR(MAX);
    DECLARE @columns NVARCHAR(MAX);
    DECLARE @selectColumns NVARCHAR(MAX);
    DECLARE @NextPrimaryKeyValue INT;

    -- Calcular o próximo valor disponível para a chave primária
    SET @sql = N'SELECT @NextPrimaryKeyValue = ISNULL(MAX(' + QUOTENAME(@PrimaryKeyColumn) + '), 0) + 1 FROM ' + QUOTENAME(@TableName);
    EXEC sp_executesql @sql, N'@NextPrimaryKeyValue INT OUTPUT', @NextPrimaryKeyValue = @NextPrimaryKeyValue OUTPUT;

    -- Obter a lista de colunas, incluindo a chave primária
    SELECT @columns = STRING_AGG(CAST(QUOTENAME(name) AS NVARCHAR(MAX)), ', ')
    FROM sys.columns
    WHERE object_id = OBJECT_ID(@TableName)
      AND is_identity = 0;  -- Exclui colunas de identidade (se houver)

    IF @columns IS NULL
    BEGIN
        RAISERROR('Tabela ou colunas não encontradas.', 16, 1);
        RETURN;
    END

    -- Inicializar @selectColumns com as colunas originais
    SET @selectColumns = @columns;

    -- Se campos para alterar forem fornecidos
    IF @CamposAlterar IS NOT NULL AND @NovosValores IS NOT NULL
    BEGIN
        -- Dividir os campos e valores em tabelas temporárias com números de linha
        DECLARE @Campos TABLE (RN INT, Nome NVARCHAR(128));
        DECLARE @Valores TABLE (RN INT, Valor NVARCHAR(MAX));

        INSERT INTO @Campos (RN, Nome)
        SELECT ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS RN, LTRIM(RTRIM(value))
        FROM STRING_SPLIT(@CamposAlterar, ',');

        INSERT INTO @Valores (RN, Valor)
        SELECT ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS RN, LTRIM(RTRIM(value))
        FROM STRING_SPLIT(@NovosValores, ',');

        -- Verificar se o número de campos e valores correspondem
        IF (SELECT COUNT(*) FROM @Campos) <> (SELECT COUNT(*) FROM @Valores)
        BEGIN
            RAISERROR('O número de campos e valores não corresponde.', 16, 1);
            RETURN;
        END

        -- Reconstituir @selectColumns com os novos valores
        SELECT @selectColumns = STRING_AGG(
            CAST(
                CASE
                    WHEN c.name = @PrimaryKeyColumn THEN CAST(@NextPrimaryKeyValue AS NVARCHAR(MAX)) + ' AS ' + QUOTENAME(c.name)
                    WHEN cn.Nome IS NOT NULL THEN vn.Valor + ' AS ' + QUOTENAME(c.name)
                    ELSE QUOTENAME(c.name)
                END AS NVARCHAR(MAX)
            ), ', '
        )
        FROM sys.columns c
        LEFT JOIN @Campos cn ON c.name = cn.Nome
        LEFT JOIN @Valores vn ON cn.RN = vn.RN
        WHERE c.object_id = OBJECT_ID(@TableName)
          AND c.is_identity = 0;
    END
    ELSE
    BEGIN
        -- Quando não há campos para alterar, ainda precisamos definir o novo valor para a chave primária
        SELECT @selectColumns = STRING_AGG(
            CAST(
                CASE
                    WHEN c.name = @PrimaryKeyColumn THEN CAST(@NextPrimaryKeyValue AS NVARCHAR(MAX)) + ' AS ' + QUOTENAME(c.name)
                    ELSE QUOTENAME(c.name)
                END AS NVARCHAR(MAX)
            ), ', '
        )
        FROM sys.columns c
        WHERE c.object_id = OBJECT_ID(@TableName)
          AND c.is_identity = 0;
    END

    -- Criar uma tabela temporária para capturar o valor da chave primária inserida
    CREATE TABLE #InsertedKeys (ChavePrimaria INT);

    -- Construir o SQL dinâmico com a cláusula OUTPUT
    SET @sql = N'
    INSERT INTO ' + QUOTENAME(@TableName) + ' (' + @columns + ')
    OUTPUT inserted.' + QUOTENAME(@PrimaryKeyColumn) + ' INTO #InsertedKeys(ChavePrimaria)
    SELECT ' + @selectColumns + '
    FROM ' + QUOTENAME(@TableName) + '
    WHERE ' + QUOTENAME(@PrimaryKeyColumn) + ' = @PrimaryKeyValue;
    ';

    -- Executar o SQL dinâmico
    EXEC sp_executesql @sql, N'@PrimaryKeyValue SQL_VARIANT', @PrimaryKeyValue = @PrimaryKeyValue;

    -- Recuperar o valor da chave primária inserida
    SELECT TOP 1 @NovoValorChavePrimaria = ChavePrimaria FROM #InsertedKeys;

    -- Limpar a tabela temporária
    DROP TABLE #InsertedKeys;
END;
GO

/**********************************************************************
    7.3 - PEGAR A ÚLTIMA CHAVE DE UMA TABELA
***********************************************************************/

create or alter procedure sp_GetLastIdFromTable(@TableName NVARCHAR(128),
                                                @IdColumn NVARCHAR(128),
                                                @Increment INT = 0,
                                                @LastId INT OUTPUT
                                               )
AS
BEGIN
    SET NOCOUNT ON;

    DECLARE @SQL NVARCHAR(MAX);
    DECLARE @ParmDefinition NVARCHAR(500);

    -- Construir o SQL dinâmico
    SET @SQL = N'SELECT @LastIdOut = MAX(' + QUOTENAME(@IdColumn) + ') FROM ' + QUOTENAME(@TableName) + ';';
    SET @ParmDefinition = N'@LastIdOut INT OUTPUT';

    -- Executar o SQL dinâmico
    EXEC sp_executesql @SQL, @ParmDefinition, @LastIdOut = @LastId OUTPUT;

    -- Tratar casos onde MAX retorna NULL (tabela vazia)
    IF @LastId IS NULL
        SET @LastId = 0;

    -- Incrementar o ID se necessário
    SET @LastId = @LastId + @Increment;
END;
GO

create or alter procedure sp_GetLastIdFromTableEx(@TableName SYSNAME,
                                                  @LastId INT OUTPUT
                                                 )
AS
BEGIN
    SET NOCOUNT ON;

    DECLARE @IdColumn SYSNAME;
    DECLARE @SQL NVARCHAR(MAX);
    DECLARE @ParmDefinition NVARCHAR(500);

    -- Obter o nome da coluna de chave primária
    SELECT TOP 1 @IdColumn = KU.COLUMN_NAME
    FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS AS TC
    INNER JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS KU
        ON TC.CONSTRAINT_TYPE = 'PRIMARY KEY' AND
           TC.CONSTRAINT_NAME = KU.CONSTRAINT_NAME
    WHERE KU.TABLE_NAME = @TableName;

    IF @IdColumn IS NULL
    BEGIN
        RAISERROR('Chave primária não encontrada para a tabela %s.', 16, 1, @TableName);
        RETURN;
    END

    -- Construir o SQL dinâmico
    SET @SQL = N'SELECT @LastIdOut = MAX(' + QUOTENAME(@IdColumn) + ') FROM ' + QUOTENAME(@TableName) + ';';
    SET @ParmDefinition = N'@LastIdOut INT OUTPUT';

    -- Executar o SQL dinâmico
    EXEC sp_executesql @SQL, @ParmDefinition, @LastIdOut = @LastId OUTPUT;

    -- Tratar casos onde MAX retorna NULL (tabela vazia)
    IF @LastId IS NULL
        SET @LastId = 0;
END;
GO

/**********************************************************************
  08 - Store Procedure Standard Data
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
		declare @AuxKey int
		declare @valuesFields nvarchar(max);

		exec sp_clearAllDataIntegration @ConsiderApDataRange

		set nocount on
		
		/*#### OBJETO - 550 Tabela ServidoresIntegracoes*/
		exec sp_Execute_Insert 'dbo', 01, 'ServidoresIntegracoes', 'BBN_CdiServidorIntegracao, BBN_D1sServidorIntegracao, BBN_CosEnderecoIP, BBN_NuiPorta', '1, ''(TESTES) - INTEGRATION - SERVIDORES'', ''localhost'', 7080', 1  

		/*#### OBJETO - 551 Tabela DLLsIntegracoes, DLLsIntegracoesMetodos*/
		exec sp_Execute_Insert 'dbo', 01, 'DLLsIntegracoes', 'BBV_CdiDLLIntegracao, BBV_D1sDLLIntegracao, BBV_CosCaminhoArquivoDLL', '50001, ''(TESTES) - DLL INTEGRACAO INTERFACE'', ''C:\Apdata_x64\Aplicacoes\ApIntegrationInterface\bin\Win64\Debug\ApIntegrationInterface.dll''', 1
		--exec sp_Execute_Update 'dbo', 01, 'DLLsIntegracoes', 'BBV_CosCaminhoArquivoDLL = ''C:\Apdata_x64\Aplicacoes\ApIntegrationInterface\bin\Win32\Debug\ApIntegrationInterface.dll''', 'BBV_CdiDLLIntegracao = 50001'
		exec sp_Execute_Insert 'dbo', 01, 'DLLsIntegracoesMetodos', 'BBW_CdiDLLIntegracaoMetodo, BBW_CdiDLLIntegracao, BBW_CosDLLIntegracaoMetodo, BBW_D1sDLLIntegracaoMetodo', '50001, 50001, ''TreatTransactionEvent'', ''TreatTransactionEvent''', 1

		/*#### OBJETO - 559 - Tabela - TransacoesIntegracoes*/
		exec sp_Execute_Insert 'dbo', 01, 'TransacoesIntegracoes', 'BBX_CdiTransacaoIntegracao, BBX_CdiDLLIntegracaoMetodo, BBX_CdiEventoTransacao, BBX_CdiTransacao', '50001, 50001, 2, 30063', 1
		exec sp_Execute_Insert 'dbo', 02, 'TransacoesIntegracoes', 'BBX_CdiTransacaoIntegracao, BBX_CdiDLLIntegracaoMetodo, BBX_CdiEventoTransacao, BBX_CdiTransacao', '50002, 50001, 2, 43192', 1
		exec sp_Execute_Insert 'dbo', 03, 'TransacoesIntegracoes', 'BBX_CdiTransacaoIntegracao, BBX_CdiDLLIntegracaoMetodo, BBX_CdiEventoTransacao, BBX_CdiTransacao', '50003, 50001, 2, 21233', 1
		exec sp_Execute_Insert 'dbo', 04, 'TransacoesIntegracoes', 'BBX_CdiTransacaoIntegracao, BBX_CdiDLLIntegracaoMetodo, BBX_CdiEventoTransacao, BBX_CdiTransacao', '50004, 50001, 2, 30842', 1
		exec sp_Execute_Insert 'dbo', 05, 'TransacoesIntegracoes', 'BBX_CdiTransacaoIntegracao, BBX_CdiDLLIntegracaoMetodo, BBX_CdiEventoTransacao, BBX_CdiTransacao', '50005, 50001, 2, 15953', 1
		exec sp_Execute_Insert 'dbo', 06, 'TransacoesIntegracoes', 'BBX_CdiTransacaoIntegracao, BBX_CdiDLLIntegracaoMetodo, BBX_CdiEventoTransacao, BBX_CdiTransacao', '50006, 50001, 2, 29993', 1
		exec sp_Execute_Insert 'dbo', 07, 'TransacoesIntegracoes', 'BBX_CdiTransacaoIntegracao, BBX_CdiDLLIntegracaoMetodo, BBX_CdiEventoTransacao, BBX_CdiTransacao', '50007, 50001, 2, 39802', 1
		exec sp_Execute_Insert 'dbo', 08, 'TransacoesIntegracoes', 'BBX_CdiTransacaoIntegracao, BBX_CdiDLLIntegracaoMetodo, BBX_CdiEventoTransacao, BBX_CdiTransacao', '50008, 50001, 2, 39803', 1
		exec sp_Execute_Insert 'dbo', 09, 'TransacoesIntegracoes', 'BBX_CdiTransacaoIntegracao, BBX_CdiDLLIntegracaoMetodo, BBX_CdiEventoTransacao, BBX_CdiTransacao', '50009, 50001, 2, 30122', 1
		exec sp_Execute_Insert 'dbo', 10, 'TransacoesIntegracoes', 'BBX_CdiTransacaoIntegracao, BBX_CdiDLLIntegracaoMetodo, BBX_CdiEventoTransacao, BBX_CdiTransacao', '50010, 50001, 2, 29642', 1

		/*#### OBJETO - 962 - Tabela - LayoutsSaidas*/
		exec sp_Execute_Insert 'dbo', 01, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida', '1001, ''LAYOUT REST PARAMETROS'', 0xEFBBBF7B226B6579223A20222376616C7565506172616D6574657223227D', 1   
		exec sp_Execute_Insert 'dbo', 02, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida', '1002, ''LAYOUT REST PARAMETROS URL'', 0xEFBBBF68747470733A2F2F7669616365702E636F6D2E62722F77732F23636570232F6A736F6E2F', 1
		exec sp_Execute_Insert 'dbo', 03, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida', '1003, ''LAYOUT REST INTEGRACAO FOTOS'', 0xEFBBBF7B0D0A20226964223A22234147495F436469417373756E746F476572616C4974656D23222C0D0A2022666F746F223A22234147495F4172624172717569766F52656C61746F72696F23220D0A7D', 1
		exec sp_Execute_Insert 'dbo', 04, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida', '1004, ''MOCK POSTMAN GET'', 0xEFBBBF68747470733A2F2F32306537373664392D666164662D343763312D393163392D3032663538323931623963312E6D6F636B2E7073746D6E2E696F2F6170692F76312F6765742F236B6579696423', 1
		exec sp_Execute_Insert 'dbo', 05, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida', '1005, ''MOCK POSTMAN PUT'', 0xEFBBBF68747470733A2F2F32306537373664392D666164662D343763312D393163392D3032663538323931623963312E6D6F636B2E7073746D6E2E696F2F6170692F76312F7075742F236B6579696423', 1
		exec sp_Execute_Insert 'dbo', 06, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida', '1006, ''JSON - PREENCHIMENTO PARAMETRO SQL'', 0xEFBBBF207B22646174614F6E65223A236669656C6431232C20226461746154776F223A236669656C6432237D', 1
		exec sp_Execute_Insert 'dbo', 07, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida', '1007, ''JSON - LAYOUT SAIDA ALTERACAO'', 0xEFBBBF7B226E616D65223A22236E6F6D6523222C224F5554524F4E414D45223A2223656D61696C23222C226964223A2223696423227D', 1
		exec sp_Execute_Insert 'dbo', 08, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida', '1008, ''API THIRDPART GET'', 0xEFBBBF68747470733A2F2F3632643662656661353165366538663036663132313466392E6D6F636B6170692E696F2F6170692F76312F706F73742F236B6579696423', 1
		exec sp_Execute_Insert 'dbo', 09, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida', '1009, ''OAUTH SERASA MODELO'', 0xEFBBBF7B22417574686F72697A6174696F6E223A224265617265722023616363657373546F6B656E23222C22436F6E74656E742D54797065223A226170706C69636174696F6E2F6A736F6E227D', 1
		exec sp_Execute_Insert 'dbo', 10, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida', '1010, ''OAUTH SERASA CAMPOS'', 0xEFBBBF7B0D0A202022736F7572636553797374656D223A202223736F7572636553797374656D23222C0D0A20202275736572223A202223757365727323222C0D0A202022726567697374726174696F6E526571756573744E756D626572223A202223726567697374726174696F6E526571756573744E756D62657223222C0D0A202022696E766F696365223A207B0D0A2020202022696E766F6963654964223A202223696E766F696365496423222C0D0A202020202264617465223A202223646174655F696E766F69636523222C0D0A2020202022616D6F756E74223A202223616D6F756E7423222C0D0A20202020226465736372697074696F6E223A2022236465736372697074696F6E5F696E766F69636523222C0D0A20202020227061796D656E744372656174696F6E44617465223A2022237061796D656E744372656174696F6E4461746523222C0D0A20202020227061796D656E7444617465223A2022237061796D656E744461746523220D0A20207D2C0D0A20202262696C6C696E6746726F6D223A207B0D0A2020202022646F63756D656E7454797065223A202223646F63756D656E74547970655F62696C6C696E6746726F6D23222C0D0A2020202022646F63756D656E744E756D626572223A202223646F63756D656E744E756D6265725F62696C6C696E6746726F6D23220D0A20207D2C0D0A20202262696C6C696E67546F223A207B0D0A2020202022646F63756D656E744E756D626572223A202223646F63756D656E744E756D6265725F62696C6C696E67546F23222C0D0A20202020226465736372697074696F6E223A202223646F63756D656E74547970655F62696C6C696E67546F23220D0A20207D2C0D0A202022696E766F6963654C696E6573223A205B0D0A202020207B0D0A202020202020226974656D4465736372697074696F6E223A2022236974656D4465736372697074696F6E23222C0D0A202020202020226974656D416D6F756E74223A2022236974656D416D6F756E7423222C0D0A202020202020226163636F756E74696E674B6579223A2022236163636F756E74696E674B657923222C0D0A202020202020226669786564417373657454797065223A202223666978656441737365745479706523222C0D0A2020202020202266697865644173736574426F6F6B54797065223A202366697865644173736574426F6F6B54797065232C0D0A20202020202022666978656441737365744163636F756E74223A2023666978656441737365744163636F756E74232C0D0A202020202020226669786564417373657443617465676F7279223A20236669786564417373657443617465676F7279230D0A202020207D0D0A20205D0D0A7D', 1
		exec sp_Execute_Insert 'dbo', 11, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida', '1011, ''api/v1/patch/{{identifier}}'', 0xEFBBBF68747470733A2F2F32306537373664392D666164662D343763312D393163392D3032663538323931623963312E6D6F636B2E7073746D6E2E696F2F6170692F76312F70617463682F236B6579696423', 1
		exec sp_Execute_Insert 'dbo', 12, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida', '1012, ''PRE-REQUEST LAYOUT'', 0xEFBBBF207B22746F6B656E3031223A202223746F6B656E23222C202022746F6B656E3032223A202223746F6B656E3223227D', 1
		exec sp_Execute_Insert 'dbo', 13, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida', '1013, ''SAIDA SQL'', 0x7B0D0A202020202243616D706F31223A20222343616D706F314C5423222C0D0A092243616D706F32223A20222343616D706F324C5423222C0D0A092243616D706F33223A20222343616D706F334C5423222C0D0A092243616D706F34223A20222343616D706F344C5423222C0D0A092243616D706F35223A20222343616D706F354C5423222C0D0A092243616D706F36223A20222343616D706F364C542322090D0A7D', 1
		exec sp_Execute_Insert 'dbo', 14, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1bLayOutSaida, BRD_D1sLayOutSaida', '1014, 0xEFBBBF68747470733A2F2F32306537373664392D666164662D343763312D393163392D3032663538323931623963312E6D6F636B2E7073746D6E2E696F2F6170692F76312F64656C6574652F236B6579696423, ''MOCK POSTMAN DELETE''', 1
		exec sp_Execute_Insert 'dbo', 15, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1bLayOutSaida, BRD_D1sLayOutSaida', '1015, 0xEFBBBF7B226B6579223A2022236B6579696423227D, ''DELETE BODY''', 1

		/*#### OBJETO - 3552 ComandosSQLsGrupos, ComandosSQLs,  */
		exec sp_takeKeyForInsertion 'ComandosSQLsGrupos', @ADN_CdiComandoSQLGrupo OUTPUT
		exec sp_takeKeyForInsertion 'ComandosSQLs', @SQL_CdiComandoSQL OUTPUT

		exec sp_Execute_Insert_Key 'dbo', 01, 'ComandosSQLsGrupos', 'ADN_CdiComandoSQLGrupo, ADN_D1sComandoSQLGrupo', @ADN_CdiComandoSQLGrupo, 01, '''(TESTES) COMANDOS SQL TESTES APINTEGRATION''', 1  
		exec sp_Execute_Insert_Key 'dbo', 02, 'ComandosSQLsGrupos', 'ADN_CdiComandoSQLGrupo, ADN_D1sComandoSQLGrupo', @ADN_CdiComandoSQLGrupo, 02, '''(TESTES) COMANDOS SQL TESTES APADINTEGRATORWS''', 1  
		
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 01, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 01, @ADN_CdiComandoSQLGrupo, 1, '''RETORNA USUARIO ATRAVES DO CONTRATADO'', 0xEFBBBF53656C656374206D6178285553525F4364695573756172696F290D0A66726F6D205573756172696F730D0A696E6E6572206A6F696E205573756172696F73436F6E7472617461646F73204F4E20285553525F4364695573756172696F203D205553435F4364695573756172696F290D0A7768657265205553435F436469436F6E7472617461646F5F5573756172696F203D203A6964636F6E7472617461646F0D0A', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 02, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 02, @ADN_CdiComandoSQLGrupo, 1, '''CONVERSOR EMAIL'', 0xEFBBBF4445434C4152452040656D61696C2061732056415243484152283830290D0A5345542040656D61696C203D202853656C656374206D6178285553525F436F73456D61696C2966726F6D205573756172696F730D0A09090909696E6E6572206A6F696E205573756172696F73436F6E7472617461646F73204F4E20285553525F4364695573756172696F203D205553435F4364695573756172696F290D0A090909097768657265205553435F436469436F6E7472617461646F5F5573756172696F203D203A6964636F6E7472617461646F290D0A73656C6563742043415345205748454E2040656D61696C206973206E6F74206E756C6C207468656E2027402720656C73652040656D61696C20656E6420617320656D61696C', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 03, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 03, @ADN_CdiComandoSQLGrupo, 1, '''CONTRATADOS LOTES'', 0xEFBBBF0D0A0D0A0D0A53454C4543540D0A0D0A312061732069642C0D0A27303831313133313027206173206365700D0A756E696F6E20616C6C0D0A73656C6563740D0A322C0D0A273037313131333130270D0A756E696F6E20616C6C0D0A73656C656374200D0A332C0D0A273039313131333130270D0A', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 04, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 04, @ADN_CdiComandoSQLGrupo, 1, '''BUFFER'', 0xEFBBBF73656C656374204147495F436469417373756E746F476572616C4974656D2C204147495F4172624172717569766F52656C61746F72696F0D0A66726F6D20417373756E746F734765726169734974656E730D0A7768657265204147495F436469417373756E746F476572616C4974656D203D203132', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 05, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 05, @ADN_CdiComandoSQLGrupo, 1, '''COMANDO PREENCHIMENTO JSON'', 0xEFBBBF77697468206475616C2864756D6D7929206173202873656C656374202778272953454C45435420273931343036393227206173206B65796D61737465722C202731313030313127206173206669656C64312C2027323032323131303127206173206669656C6432202046524F4D206475616C202057484552452031203C3D203A6B65796964', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 06, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 06, @ADN_CdiComandoSQLGrupo, 1, '''COMANDO SAIDA JSON'', 0xEFBBBF73656C65637420636F6E5F6473736E6F6D65206E6F6D652C20434F4E5F436F73454D61696C20656D61696C2C20636F6E5F436469636F6E7472617461646F204944200D0A0966726F6D20636F6E7472617461646F730D0A776865726520636F6E5F636469636F6E7472617461646F203D203A636F6E5F636469636F6E7472617461646F0D0A', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 07, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 07, @ADN_CdiComandoSQLGrupo, 1, '''RETORNA RANDOM NUMERO KEYID 1-100'', 0xEFBBBF53454C45435420464C4F4F522852414E4428292A283130302D312B31292B3129206173206B657969643B', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 08, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 08, @ADN_CdiComandoSQLGrupo, 1, '''RETORNA RANDOM NUMERO KEYID 1-45'', 0xEFBBBF53454C45435420464C4F4F522852414E4428292A2834352D312B31292B3129206173206B657969643B', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 09, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 09, @ADN_CdiComandoSQLGrupo, 1, '''RETORNA RANDOM NUMERO KEYID CORINGA'', 0xEFBBBF53454C45435420464C4F4F522852414E4428292A283A6E756D4B65792D312B31292B3129206173206B657969643B', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 10, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 10, @ADN_CdiComandoSQLGrupo, 1, '''NAME MANAGER TWO FIELDS'', 0xEFBBBF53656C656374205553525F4364735573756172696F2C20434F4E5F4473734E6F6D65436F6D706C65746F2046726F6D20436F6E7472617461646F7320496E6E6572204A6F696E205573756172696F73436F6E7472617461646F73206F6E2028434F4E5F436469436F6E7472617461646F203D205553435F436469436F6E7472617461646F5F5573756172696F2920496E6E6572204A6F696E205573756172696F73206F6E20285553435F4364695573756172696F203D205553525F4364695573756172696F2920576865726520434F4E5F436469436F6E7472617461646F203D203A436469436F6E7472617461646F', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 11, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 11, @ADN_CdiComandoSQLGrupo, 2, '''NAME MANAGER'', 0xEFBBBF53656C6563742020434F4E5F4473734E6F6D65436F6D706C65746F2046726F6D20436F6E7472617461646F73200D0A496E6E6572204A6F696E205573756172696F73436F6E7472617461646F73206F6E2028434F4E5F436469436F6E7472617461646F203D20205553435F436469436F6E7472617461646F5F5573756172696F29200D0A496E6E6572204A6F696E205573756172696F73206F6E20285553435F4364695573756172696F203D20205553525F4364695573756172696F29200D0A576865726520434F4E5F436469436F6E7472617461646F203D203A436469436F6E7472617461646F', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 12, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 12, @ADN_CdiComandoSQLGrupo, 1, '''OAUTH SERASA FINANCEIRO'', 0xEFBBBF4465636C61726520404C6F675472616E736163616F20617320696E74656765722C204056696E63756C6F20617320696E74656765720D0A0D0A73657420404C6F675472616E736163616F203D203A4C6F675472616E736163616F0D0A0D0A0D0A73656C6563740D0A0D0A546162656C612E456D705F436469456D70726573612C0D0A546162656C612E4D56465F6364696C6F677472616E736163616F2C0D0A546162656C612E646F63756D656E744E756D6265725F62696C6C696E6746726F6D2C0D0A546162656C612E646F63756D656E74547970655F62696C6C696E6746726F6D2C0D0A546162656C612E646F63756D656E744E756D6265725F62696C6C696E67546F2C0D0A546162656C612E646F63756D656E74547970655F62696C6C696E67546F2C0D0A5265706C61636528636F6E7665727428766172636861722C466F726D61742853756D28546162656C612E616D6F756E74292C27302E30302729292C272E272C272C2729617320616D6F756E742C0D0A546162656C612E646174655F696E766F6963652C0D0A546162656C612E6465736372697074696F6E5F696E766F6963652C0D0A636F6E7665727428766172636861722C546162656C612E696E766F696365496429202B272D272B2020636F6E7665727428766172636861722C726F775F6E756D6265722829206F76657220286F7264657220627920546162656C612E696E766F6963654964292920617320696E766F69636549642C0D0A546162656C612E7061796D656E744372656174696F6E446174652C0D0A546162656C612E7061796D656E74446174652C0D0A546162656C612E6163636F756E74696E674B65792C0D0A546162656C612E666978656441737365744163636F756E742C0D0A546162656C612E66697865644173736574426F6F6B547970652C0D0A546162656C612E6669786564417373657443617465676F72792C0D0A546162656C612E66697865644173736574547970652C0D0A5265706C61636528636F6E7665727428766172636861722C466F726D61742853756D28546162656C612E6974656D416D6F756E74292C27302E30302729292C272E272C272C2729206173206974656D416D6F756E742C0D0A546162656C612E6974656D4465736372697074696F6E2C0D0A546162656C612E726567697374726174696F6E526571756573744E756D6265722C0D0A546162656C612E736F7572636553797374656D2C0D0A546162656C612E75736572732C0D0A546162656C612E436F6E5F43646976696E63756C6F0D0A0D0A46726F6D20280D0A73656C6563740D0A456D7072657361732E456D705F436469456D70726573612C0D0A4D56465F6364696C6F677472616E736163616F2C0D0A2D2D2D2730272B5265706C61636528456D7072657361732E454D505F4E7573434E504A5072656669786F2C272E272C272729202B205265706C616365284C6F636169732E4C4F435F4E7573434E504A53756669786F2C272D272C2727292061732020646F63756D656E744E756D6265725F62696C6C696E6746726F6D2C0D0A63617365207768656E20456D7072657361732E456D705F436469456D70726573612020202020203D2031207468656E2027303632313733363230303030313830270D0A20202020207768656E20456D7072657361732E456D705F436469456D70726573612020202020203D2034207468656E20273033343834393132343030303136382720656E642061732020646F63756D656E744E756D6265725F62696C6C696E6746726F6D2C0D0A27302720202020202020202020202020202020202020202020202020202020202020202061732020646F63756D656E74547970655F62696C6C696E6746726F6D2C0D0A2D2D2D2D4D56465F436F73566172696176656C32202020202020202020202020202020202020202061732020646F63756D656E744E756D6265725F62696C6C696E67546F2C0D0A2D2D2730272B5265706C61636528456D7072657361732E454D505F4E7573434E504A5072656669786F2C272E272C272729202B205265706C616365284C6F636169732E4C4F435F4E7573434E504A53756669786F2C272D272C2727292061732020646F63756D656E744E756D6265725F62696C6C696E67546F2C0D0A63617365207768656E20456D7072657361732E456D705F436469456D70726573612020202020203D2031207468656E2027303632313733363230303030313830270D0A20202020207768656E20456D7072657361732E456D705F436469456D70726573612020202020203D2034207468656E20273033343834393132343030303136382720656E642061732020646F63756D656E744E756D6265725F62696C6C696E67546F2C0D0A4D56465F44737350726F636573736F45787465726E6F202020202020202020202020202061732020646F63756D656E74547970655F62696C6C696E67546F2C0D0A0D0A53756D284D56465F566C6E56616C6F72292061732020616D6F756E742C0D0A636F6E766572742876617263686172283130292C6765746461746528292C31323629202061732020646174655F696E766F6963652C0D0A27506167746F204D656E73616C27202020202020202020202020202020202020202020206173206465736372697074696F6E5F696E766F6963652C0D0A4D56465F6364696C6F677472616E736163616F20617320696E766F69636549642C0D0A636F6E766572742876617263686172283130292C6765746461746528292C3132362920206173207061796D656E744372656174696F6E446174652C0D0A636F6E766572742876617263686172283130292C4D56465F447464506167616D656E746F2C3132362920206173207061796D656E74446174652C0D0A454D465F443173436F6E746575646F466C65786976656C5F30312C0D0A454D465F443173436F6E746575646F466C65786976656C5F3031202B20272E42522E313030272B2020272E3030303030302E272B204D56465F436F73566172696176656C31202B272E30303030303030302E3030303027206173206163636F756E74696E674B65792C0D0A276E756C6C27202020202020202020202020202020202020202020202020202020202020617320666978656441737365744163636F756E742C0D0A276E756C6C2720202020202020202020202020202020202020202020202020202020202061732066697865644173736574426F6F6B547970652C0D0A276E756C6C272020202020202020202020202020202020202020202020202020202020206173206669786564417373657443617465676F72792C0D0A274E2720202020202020202020202020202020202020202020202020202020202020202061732066697865644173736574547970652C0D0A73756D284D56465F566C6E56616C6F7229202020202020202020202020202020202020206173206974656D416D6F756E742C0D0A27506167746F204D656E73616C27202020202020202020202020202020202020202020206173206974656D4465736372697074696F6E2C0D0A4D56465F6364696C6F677472616E736163616F2020202020202020202020202020202020617320726567697374726174696F6E526571756573744E756D6265722C0D0A274150444154412720202020202020202020202020202020202020202020202020202020617320736F7572636553797374656D2C0D0A27323833383127202020202020202020202020202020202020202020202020202020202061732075736572732C0D0A63617365207768656E20436F6E7472617461646F732E436F6E5F43646976696E63756C6F203C3E203337207468656E203120656C736520436F6E7472617461646F732E436F6E5F43646976696E63756C6F20656E6420617320436F6E5F43646976696E63756C6F0D0A0D0A46726F6D204D6F76696D656E746F7366696E616E636569726F7320202020202020202020204D6F76696D656E746F7366696E616E636569726F730D0A696E6E6572206A6F696E20436F6E7472617461646F73202020202020202020202020202020436F6E7472617461646F732020202020202020202020202020206F6E2028436F6E7472617461646F732E436F6E5F436469436F6E7472617461646F203D204D6F76696D656E746F7366696E616E636569726F732E4D56465F436469436F6E7472617461646F290D0A696E6E6572206A6F696E2043656E74726F73437573746F732020202020202020202020202043656E74726F73437573746F73202020202020202020202020206F6E2028436F6E7472617461646F732E436F6E5F43646943656E74726F437573746F203D2043656E74726F73437573746F732E4343555F43646943656E74726F437573746F290D0A696E6E6572206A6F696E20466F6C6861732020202020202020202020202020202020202020466F6C68617320202020202020202020202020202020202020206F6E2028436F6E7472617461646F732E436F6E5F436469466F6C6861203D20466F6C6861732E466F6C5F436469466F6C6861290D0A696E6E6572206A6F696E204C6F6361697320202020202020202020202020202020202020204C6F6361697320202020202020202020202020202020202020206F6E2028466F6C6861732E466F6C5F4364694C6F63616C203D204C6F636169732E4C6F635F4364696C6F63616C290D0A696E6E6572206A6F696E20456D707265736173202020202020202020202020202020202020456D7072657361732020202020202020202020202020202020206F6E20284C6F636169732E4C6F635F436469456D7072657361203D20456D7072657361732E456D705F436469456D7072657361290D0A696E6E6572206A6F696E20456D707265736173466C65786976656973202020202020202020456D707265736173466C657869766569732020202020202020206F6E2028456D707265736173466C657869766569732E454D465F436469456D7072657361203D20456D7072657361732E456D705F436469456D7072657361290D0A0D0A776865726520204D56465F6364696C6F677472616E736163616F203D20404C6F675472616E736163616F0D0A0D0A47726F757020627920456D7072657361732E456D705F436469456D70726573612C0D0A2020202020202020204D56465F4364694C6F675472616E736163616F2C0D0A2020202020202020202D2D456D7072657361732E454D505F4E7573434E504A5072656669786F2C0D0A20202020202020202D2D204C6F636169732E4C4F435F4E7573434E504A53756669786F2C0D0A202020202020202020454D465F443173436F6E746575646F466C65786976656C5F30312C0D0A2020202020202020204D56465F436F73566172696176656C312C0D0A2020202020202020204D56465F436F73566172696176656C322C0D0A20202020202020202D2D2D2043656E74726F73437573746F732E4343555F436F736573747275747572612C0D0A2020202020202020204D56465F44737350726F636573736F45787465726E6F2C0D0A2020202020202020204D56465F436F7350726F636573736F45787465726E6F2C0D0A2020202020202020204D56465F447464506167616D656E746F2C0D0A202020202020202020436F6E7472617461646F732E436F6E5F43646976696E63756C6F0D0A0D0A2920546162656C610D0A0D0A0D0A47726F75702062790D0A546162656C612E456D705F436469456D70726573612C0D0A546162656C612E4D56465F6364696C6F677472616E736163616F2C0D0A546162656C612E646F63756D656E744E756D6265725F62696C6C696E6746726F6D2C0D0A546162656C612E646F63756D656E74547970655F62696C6C696E6746726F6D2C0D0A546162656C612E646F63756D656E744E756D6265725F62696C6C696E67546F2C0D0A546162656C612E646F63756D656E74547970655F62696C6C696E67546F2C0D0A546162656C612E646174655F696E766F6963652C0D0A546162656C612E6465736372697074696F6E5F696E766F6963652C0D0A546162656C612E696E766F69636549642C0D0A546162656C612E7061796D656E744372656174696F6E446174652C0D0A546162656C612E7061796D656E74446174652C0D0A546162656C612E6163636F756E74696E674B65792C0D0A546162656C612E666978656441737365744163636F756E742C0D0A546162656C612E66697865644173736574426F6F6B547970652C0D0A546162656C612E6669786564417373657443617465676F72792C0D0A546162656C612E66697865644173736574547970652C0D0A546162656C612E6974656D4465736372697074696F6E2C0D0A546162656C612E726567697374726174696F6E526571756573744E756D6265722C0D0A546162656C612E736F7572636553797374656D2C0D0A546162656C612E75736572732C0D0A546162656C612E436F6E5F43646976696E63756C6F0D0A0D0A6F7264657220627920546162656C612E456D705F436469456D70726573612C0D0A2020202020202020204D56465F4364694C6F675472616E736163616F0D0A', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 13, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 13, @ADN_CdiComandoSQLGrupo, 1, '''OAUTH CREDENCIAIS SERASA'', 0xEFBBBF73656C6563740D0A273632653832356331326361383235326264393065643262322720617320757365726E616D652C0D0A27316232646530396462323532386163323163353238653236272061732050617373776F7264', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 14, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 14, @ADN_CdiComandoSQLGrupo, 1, '''RETORNA DATA HORA'', 0xEFBBBF77697468206475616C2864756D6D7929206173202873656C65637420277827292073656C656374206765746461746528292066726F6D206475616C', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 15, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 15, @ADN_CdiComandoSQLGrupo, 1, '''RETORNA O NUMERO INFORMADO'', 0xEFBBBF77697468206475616C2864756D6D7929206173202873656C65637420277827292073656C656374203A4E554D45524F494E464F524D41444F202066726F6D206475616C', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 16, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 16, @ADN_CdiComandoSQLGrupo, 1, '''COMANDO LOTE CHAVES'', 0xEFBBBF53454C45435420464C4F4F522852414E4428292A283130302D312B31292B3129206173206B657969640D0A0D0A756E696F6E20616C6C0D0A0D0A53454C45435420464C4F4F522852414E4428292A283130302D312B31292B3129206173206B657969640D0A0D0A756E696F6E20616C6C0D0A0D0A53454C45435420464C4F4F522852414E4428292A283130302D312B31292B3129206173206B65796964', 1
        exec sp_Execute_Insert_Key_ForeignKey 'dbo', 17, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 17, @ADN_CdiComandoSQLGrupo, 1, '''RETORNA RANDOM NUMERO KEYID 1-4'', 0xEFBBBF53454C45435420464C4F4F522852414E4428292A28342D312B31292B3129206173206B657969643B', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 18, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 18, @ADN_CdiComandoSQLGrupo, 1, '''RETORNA NOVO TRANSANCTION ID'', 0xEFBBBF77697468206475616C2864756D6D7929206173202873656C65637420277827292073656C656374203330383432207768657265203A43424F203D203130323130', 1
        exec sp_Execute_Insert_Key_ForeignKey 'dbo', 19, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 19, @ADN_CdiComandoSQLGrupo, 1, '''RETORNA NOVO TIPO EDICAO'', 0xEFBBBF77697468206475616C2864756D6D7929206173202873656C65637420277827292073656C65637420343037207768657265203A43424F203D203130323130', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 20, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL, SQL_D1bComentarios', @SQL_CdiComandoSQL, 20, @ADN_CdiComandoSQLGrupo, 1, '''(TESTES) - SOC SAIDA'', 0xEFBBBF53656C65637420434F455F4E75734D6174726963756C6145736F6369616C204153206D6174726963756C6152680D0A46524F4D20436F6E7472617461646F730D0A494E4E4552204A4F494E20466F6C686173204F4E2028434F4E5F436469466F6C6861203D20464F4C5F436469466F6C6861290D0A494E4E4552204A4F494E204C6F63616973204F4E2028466F6C5F4364694C6F63616C203D204C6F635F4364694C6F63616C290D0A494E4E4552204A4F494E20456D707265736173204F4E20284C6F635F436469456D7072657361203D20456D705F436469456D7072657361290D0A494E4E4552204A4F494E205469706F73456E64657265636F73204F4E20284C4F435F4364695469706F456E64657265636F203D2054454E5F4364695469706F456E64657265636F290D0A494E4E4552204A4F494E20436F6E7472617461646F73457874726173204F4E2028434F4E5F436469436F6E7472617461646F203D20434F455F436469436F6E7472617461646F290D0A494E4E4552204A4F494E20436F72657350656C65204F4E202843504C5F436469436F7250656C65203D20434F4E5F436469436F7250656C6520290D0A494E4E4552204A4F494E204772617573496E73747275636F6573204F4E2028434F4E5F43646947726175496E7374727563616F203D2047494E5F43646947726175496E7374727563616F290D0A494E4E4552204A4F494E2045737461646F73207265736964204F4E2028434F4E5F43646945737461646F5F5265736964203D2072657369642E4553545F43646945737461646F290D0A494E4E4552204A4F494E2045737461646F732061732075665247204F4E2028434F4E5F43646945737461646F5F4F7267616F5267203D20756652472E4553545F43646945737461646F290D0A494E4E4552204A4F494E2045737461646F732061732063617250726F66204F4E2028434F4E5F43646945737461646F5F4F7267616F5267203D2063617250726F662E4553545F43646945737461646F290D0A494E4E4552204A4F494E2045737461646F7320617320756E6964616465204F4E20284C4F435F43646945737461646F203D20756E69646164652E4553545F43646945737461646F290D0A494E4E4552204A4F494E2045737461646F734369766973204F4E20284554435F43646945737461646F436976696C203D20434F4E5F43646945737461646F436976696C20290D0A494E4E4552204A4F494E205365786F73204F4E20287365785F6364697365786F203D20434F4E5F4364695365786F290D0A494E4E4552204A4F494E2056696E63756C6F73204F4E202856494E5F63646976696E63756C6F203D20434F4E5F63646976696E63756C6F290D0A494E4E4552204A4F494E20436172676F73204F4E20284341525F636469636172676F203D20434F4E5F636469636172676F20290D0A494E4E4552204A4F494E20436F6442726173696C6569726F4F63757061636F6573204F4E20284341525F436469436F6442726173696C6569726F4F6375706163616F203D2043424F5F436469436F6442726173696C6569726F4F6375706163616F20290D0A494E4E4552204A4F494E2043656E74726F73437573746F73204F4E20284343555F43646943656E74726F437573746F203D20434F4E5F43646943656E74726F437573746F290D0A574845524520434F4E5F436469436F6E7472617461646F203D203A636469636F6E7472617461646F, 0xEFBBBF53656C65637420434F455F4E75734D6174726963756C6145736F6369616C204153206D6174726963756C6152680D0A2C2027747275652720415320617475616C697A617246756E63696F6E6172696F0D0A2C202766616C736527204153206175746F72697A61646F4D656E736167656D536D730D0A2C202028757070657228434F4E5F44737342616972726F2929434F4C4C4154452053514C5F4C4154494E315F47454E4552414C5F4350313235315F43495F41532061732066756E63696F6E6172696F42616972726F0D0A2C207265706C61636528434F4E5F436F7343617274616F4E6163696F6E616C53617564652C2720272C2727292041532063617274656972614E6163696F6E616C53617564650D0A2C20274D656E73616C69737461272041532066756E63696F6E6172696F43617465676F7269610D0A2C207265706C616365287265706C61636528434F4E5F4E75734365702C272E272C2727292C272D272C2727292041532046756E63696F6E6172696F4365700D0A2C20274350462720415320636861766550726F6375726146756E63696F6E6172696F0D0A2C2028757070657228434F4E5F4473734D756E69636970696F2929434F4C4C4154452053514C5F4C4154494E315F47454E4552414C5F4350313235315F43495F41532041532066756E63696F6E6172696F4369646164650D0A2C204E554C4C20415320636970610D0A2C204E554C4C20415320636E706A456D707265736146756E63696F6E6172696F0D0A2C20434F4E5F436469436F6E7472617461646F2041532066756E63696F6E6172696F436F6469676F0D0A0D0A2C2063617365207768656E20454D505F436469656D7072657361203D2031207468656E2027383036303438270D0A202020202020207768656E20454D505F436469656D7072657361203D2032207468656E2027363532383036270D0A202020202020202020202020656E6420617320636F6469676F456D70726573610D0A2C204E554C4C20415320636F6469676F4D756E69636970696F0D0A2C2028757070657228434F4E5F447373456E64657265636F436F6D706C746F2929434F4C4C4154452053514C5F4C4154494E315F47454E4552414C5F4350313235315F43495F415320415320636F6D706C656D656E746F456E64657265636F0D0A2C204E554C4C20415320636F6E7461746F456D657267656E6369610D0A2C202875707065722843504C5F443173436F7250656C652929434F4C4C4154452053514C5F4C4154494E315F47454E4552414C5F4350313235315F43495F415320415320636F720D0A2C207265706C616365287265706C61636528434F4E5F4E75734349434E756D65726F2C272E272C2727292C272D272C272729204153206370660D0A2C20636F6E766572742876617263686172283130292C434F4E5F44746441646D697373616F2C31303329204153206461746141646D697373616F0D0A2C20434F4E5F447464456D697373616F43617250726F662041532064617461456D697373616F437470730D0A2C20636F6E766572742876617263686172283130292C434F4E5F4474644E617363696D656E746F446174612C3130332920415320646174614E617363696D656E746F0D0A2C20636F6E766572742876617263686172283130292C6765746461746528292C313033292041532064617461556C74696D614D6F76696D656E746163616F0D0A2C204E554C4C204153206465736162696C69746172526973636F0D0A2C204E554C4C2041532064657363726963616F4174697669646164650D0A2C20434F4E5F436F73454D61696C20415320656D61696C0D0A2C2028757070657228434F4E5F447373456E64657265636F426173652929434F4C4C4154452053514C5F4C4154494E315F47454E4552414C5F4350313235315F43495F41532041532066756E63696F6E6172696F456E64657265636F0D0A2C204E554C4C2041532066756E63696F6E6172696F456E64657265636F456D657267656E6369610D0A2C202875707065722847494E5F44317347726175496E7374727563616F2929434F4C4C4154452053514C5F4C4154494E315F47454E4552414C5F4350313235315F43495F41532061732066756E63696F6E6172696F4573636F6C617269646164650D0A2C2072657369642E4553545F4364735369676C6145737461646F2041532066756E63696F6E6172696F45737461646F0D0A2C20287570706572284554435F44317345737461646F436976696C2929434F4C4C4154452053514C5F4C4154494E315F47454E4552414C5F4350313235315F43495F41532041532065737461646F436976696C0D0A2C204E554C4C2041532066756E63616F0D0A2C204E554C4C2041532066756E63616F42726967616461496E63656E64696F0D0A2C202745584942452720415320686973746F7269636F5050500D0A2C20434F4E5F436469436F6E7472617461646F2041532066756E63696F6E6172696F4D6174726963756C610D0A2C202766616C736527204153206E616F506F737375694370660D0A2C20277472756527204153206E616F506F73737569437470730D0A2C202766616C736527204153206E616F506F737375694D6174726963756C610D0A2C20277472756527204153206E616F506F737375695069730D0A2C2028757070657228434F4E5F4473734E617363696D656E746F4C6F63616C2929434F4C4C4154452053514C5F4C4154494E315F47454E4552414C5F4350313235315F43495F415320204153206E61747572616C69646164650D0A2C204E554C4C204153206E6F6D65436F6F70657261746976610D0A2C2028757070657228434F4E5F4473734E6F6D65436F6D706C65746F2929434F4C4C4154452053514C5F4C4154494E315F47454E4552414C5F4350313235315F43495F4153204153206E6F6D6546756E63696F6E6172696F0D0A2C2028757070657228434F4E5F4473734E6F6D654D61652929434F4C4C4154452053514C5F4C4154494E315F47454E4552414C5F4350313235315F43495F4153204153206E6F6D654D61650D0A2C20434F4E5F4E75734E756D65726F43617250726F66204153206E72437470730D0A2C2028757070657228434F4E5F447373456E64657265636F4E756D65726F2929434F4C4C4154452053514C5F4C4154494E315F47454E4552414C5F4350313235315F43495F4153202041532066756E63696F6E6172696F4E756D65726F456E64657265636F0D0A2C207265706C616365287265706C61636528434F4E5F4E75734E756D65726F5069732C272E272C2727292C272D272C272729204153207069730D0A2C20434F4E5F436F7354656C65666F6E6552616D616C2041532072616D616C0D0A2C204E554C4C2041532072616D616C54656C65666F6E65456D657267656E6369610D0A2C204E554C4C2041532072617A616F536F6369616C456D707265736146756E63696F6E6172696F0D0A2C204E554C4C20415320726567696D65526576657A616D656E746F0D0A2C20274E4F524D414C2720415320726567696D6554726162616C686F0D0A2C204E554C4C2041532072656D756E65726163616F4D656E73616C0D0A2C204E554C4C2041532072657175697369746F7346756E63616F0D0A2C20434F4E5F436F734E756D65726F52672041532072670D0A2C20636F6E766572742876617263686172283130292C434F4E5F447464456D697373616F52672C3130332920415320726744617461456D697373616F0D0A2C20434F4E5F436F734F7267616F52672041532072674F7267616F456D6973736F720D0A2C20756652472E4553545F4364735369676C6145737461646F20415320726755660D0A2C20434F4E5F4E7573536572696543617250726F66204153207365726965437470730D0A2C20287570706572287365785F6431737365786F2929434F4C4C4154452053514C5F4C4154494E315F47454E4552414C5F4350313235315F43495F4153204153207365786F0D0A2C2027415449564F2720617320736974756163616F0D0A2C207265706C616365287265706C61636528434F455F4E757354656C65666F6E6543656C756C61722C272E272C2727292C272D272C2727292041532074656C65666F6E6543656C756C61720D0A2C204E554C4C2041532074656C65666F6E65436F6D65726369616C0D0A2C204E554C4C2041532074656C65666F6E65456D657267656E6369610D0A2C207265706C616365287265706C61636528434F4E5F4E757354656C65666F6E654E756D65726F5F52657369642C272E272C2727292C272D272C2727292041532074656C65666F6E655265736964656E6369616C0D0A2C204E554C4C2041532074656C65666F6E65536D730D0A2C2027434F4449474F5F534F4327204153207469706F4275736361456D70726573610D0A2C2063617365207768656E2076696E5F6364697469706F636F6E7472617461646F203D2031207468656E2027434C54270D0A092020207768656E2076696E5F6364697469706F636F6E7472617461646F203D2033207468656E20274553544147494152494F270D0A092020207768656E2076696E5F6364697469706F636F6E7472617461646F203D2034207468656E2027415052454E44495A270D0A092020207768656E2076696E5F6364697469706F636F6E7472617461646F203D2031207468656E20275072617A6F2044455445524D494E41444F2720656E64204153207469706F436F6E747261746163616F0D0A2C2063617270726F662E4553545F4364735369676C6145737461646F204153207566437470730D0A2C20273130352720415320636F6469676F506169734E617363696D656E746F0D0A2C2027313530383038372720415320636F6469676F5573756172696F0D0A2C20273263333166353638343261663030392720415320636861766541636573736F0D0A2C20273331323938302720415320636F6469676F456D70726573615072696E636970616C0D0A2C20273131383930382720415320636F6469676F526573706F6E736176656C0D0A2C20277472756527204153206372696172436172676F0D0A2C2027747275652720415320617475616C697A6172436172676F0D0A2C202766616C73652720415320617475616C697A6144657363726963616F52657175697369746F73436172676F50656C6F43626F0D0A2C2043424F5F436F73436F6442726173696C6569726F4F6375706163616F2041532063626F0D0A2C204E554C4C20415320636172676F436F6469676F0D0A2C20434F4E5F436469436172676F20415320636172676F436F6469676F52680D0A2C204E554C4C20415320636172676F456475636163616F0D0A2C204E554C4C20415320636172676F457870657269656E6369610D0A2C204E554C4C20415320636172676F46756E63616F0D0A2C204E554C4C20415320636172676F476669700D0A2C204E554C4C20415320636172676F486162696C6964616465730D0A2C204E554C4C20415320636172676F4C6F63616C54726162616C686F0D0A2C204E554C4C20415320636172676F4D6174657269616C5574696C697A61646F0D0A2C204E554C4C20415320636172676F4D6F62696C696172696F5574696C697A61646F0D0A2C20287570706572284341525F443173436172676F29202B2027202D2027202B20636F6E766572742876617263686172283330292C4341525F436469436172676F2929434F4C4C4154452053514C5F4C4154494E315F47454E4552414C5F4350313235315F43495F415320415320636172676F4E6F6D650D0A2C204E554C4C20415320636172676F4E6F6D654C6567616C0D0A2C204E554C4C204153206F7269656E746163616F41736F0D0A2C204E554C4C20415320636172676F52657175697369746F7346756E63616F0D0A2C2027415449564F27204153207374617475730D0A2C2027434F4449474F5F52482720415320636172676F5469706F42757363610D0A2C204E554C4C20415320747265696E616D656E746F0D0A2C204E554C4C204153207365746F72436F6469676F0D0A2C20434F4E5F43646943656E74726F437573746F204153207365746F72436F6469676F52680D0A2C20287570706572286363755F64317363656E74726F637573746F29202B2027202D2027202B20636F6E766572742876617263686172283330292C4343555F43646943656E74726F437573746F292920434F4C4C4154452053514C5F4C4154494E315F47454E4552414C5F4350313235315F43495F4153204153207365746F724E6F6D650D0A2C204E554C4C204153206F62736572766163616F41736F0D0A2C2027415449564F27204153207365746F725374617475730D0A2C2027434F4449474F5F524827204153207365746F725469706F42757363610D0A2C2027747275652720415320617475616C697A61725365746F720D0A2C202774727565272041532063726961725365746F720D0A2C20287570706572284C4F435F44737342616972726F2929434F4C4C4154452053514C5F4C4154494E315F47454E4552414C5F4350313235315F43495F415320415320756E696461646542616972726F0D0A2C204E554C4C20415320756E696461646542616972726F436F6272616E63610D0A2C207265706C616365284C4F435F4E75734365702C272D272C27272920415320756E69646164654365700D0A2C204E554C4C20415320756E6964616465436570436F6272616E63610D0A2C204E554C4C20415320756E6964616465436964616465436F6272616E63610D0A2C20287570706572284C4F435F4473734D756E69636970696F2929434F4C4C4154452053514C5F4C4154494E315F47454E4552414C5F4350313235315F43495F415320415320756E69646164654369646164650D0A2C2027434E504A2720415320636E706A5F6365690D0A2C204E554C4C20415320636F6469676F0D0A2C204E554C4C20415320636F6469676F4172717569766F0D0A2C204E554C4C20415320636F6469676F436E61650D0A2C20454D505F4E7573434E504A5072656669786F202B20272F27202B204C4F435F4E7573434E504A53756669786F20415320636F6469676F436E706A4365690D0A2C204E554C4C20415320756E6964616465436F6469676F4D756E69636970696F0D0A2C204E554C4C20415320756E6964616465436F6469676F4D756E69636970696F436F6272616E63610D0A2C204C4F435F4364694C6F63616C20415320756E6964616465436F6469676F52680D0A2C2027532F432720415320636F6D706C656D656E746F0D0A2C204E554C4C20415320636F6D706C656D656E746F436F6272616E63610D0A2C204E554C4C2041532064657363726963616F436E61650D0A2C202875707065722854454E5F4431735469706F456E64657265636F20202B20272027202B204C4F435F4473734D756E69636970696F2929434F4C4C4154452053514C5F4C4154494E315F47454E4552414C5F4350313235315F43495F415320415320756E6964616465456E64657265636F0D0A2C204E554C4C20415320756E6964616465456E64657265636F436F6272616E63610D0A2C20756E69646164652E4553545F4364735369676C6145737461646F20415320756E696461646545737461646F0D0A2C204E554C4C20415320756E696461646545737461646F436F6272616E63610D0A2C204E554C4C20415320756E696461646547726175526973636F0D0A2C204C4F435F436F73496E736372457374616475616C20415320756E6964616465496E7363726963616F457374616475616C0D0A2C204E554C4C20415320756E6964616465496E7363726963616F4D756E69636970616C0D0A2C20287570706572284C4F435F4431734C6F63616C292B2027202D2027202B20636F6E766572742876617263686172283330292C4C4F435F4364694C6F63616C2929434F4C4C4154452053514C5F4C4154494E315F47454E4552414C5F4350313235315F43495F415320415320756E69646164654E6F6D650D0A2C20287570706572284C4F435F447373456E64657265636F4E756D65726F2929434F4C4C4154452053514C5F4C4154494E315F47454E4552414C5F4350313235315F43495F415320415320756E69646164654E756D65726F0D0A2C204E554C4C20415320756E69646164654E756D65726F436F6272616E63610D0A2C204E554C4C204153206F62736572766163616F41534F0D0A2C204E554C4C204153206F62736572766163616F436F6E747261746F0D0A2C204E554C4C204153206F62736572766163616F5050500D0A2C204E554C4C2041532070657263656E7475616C43616C63756C6F427269676164610D0A2C2028757070657228454D505F447373456D70726573612929434F4C4C4154452053514C5F4C4154494E315F47454E4552414C5F4350313235315F43495F41532041532072617A616F536F6369616C0D0A2C2027415449564F2720415320756E69646164655374617475730D0A2C204E554C4C20415320756E696461646554656C65666F6E654361740D0A2C2027434F4449474F5F52482720415320756E69646164655469706F42757363610D0A2C204E554C4C204153207469706F436E61650D0A2C204E554C4C20415320756E6964616465436F6E74726174616E74650D0A2C2027747275652720415320617475616C697A6172556E69646164650D0A2C20277472756527204153206372696172556E69646164650D0A2C204E554C4C204153206461746146696E616C4573746162696C69646164650D0A2C202766616C73652720415320637269617246756E63696F6E6172696F0D0A2C2056494E5F43646943617465675472616265536F6320415320636F6469676F43617465676F72696145536F6369616C0D0A2C2027435046272041532066756E63696F6E6172696F5469706F42757363610D0A2C2063617365207768656E20434F4E5F4364695469706F446566696369656E636961203D2031207468656E202774727565270D0A2020202020202020202020207768656E20434F4E5F4364695469706F446566696369656E636961203C3E2030207468656E202766616C7365270D0A202020202020202020202020656E6420617320646566696369656E74650D0A0D0A2C2063617365207768656E20434F4E5F4364695469706F446566696369656E636961203D2031207468656E202746C3AD73696361270D0A2020202020202020202020207768656E20434F4E5F4364695469706F446566696369656E636961203D2032207468656E20274175646974697661270D0A2020202020202020202020207768656E20434F4E5F4364695469706F446566696369656E636961203D2033207468656E202756697375616C270D0A2020202020202020202020207768656E20434F4E5F4364695469706F446566696369656E636961203D2034207468656E20274D656E74616C270D0A2020202020202020202020207768656E20434F4E5F4364695469706F446566696369656E636961203D2035207468656E20274DC3BA6C7469706C61270D0A2020202020202020202020207768656E20434F4E5F4364695469706F446566696369656E636961203D2036207468656E2027526162696C697461646F270D0A202020202020202020202020656E6420415320646566696369656E6369610D0A0D0A2C2063617365207768656E20434F4E5F4364695469706F446566696369656E636961203D36207468656E202774727565270D0A2020202020202020202020207768656E20434F4E5F4364695469706F446566696369656E636961203C3E2036207468656E202766616C7365270D0A202020202020202020202020656E642041532072656162696C697461646F0D0A46524F4D20436F6E7472617461646F730D0A494E4E4552204A4F494E20466F6C686173204F4E2028434F4E5F436469466F6C6861203D20464F4C5F436469466F6C6861290D0A494E4E4552204A4F494E204C6F63616973204F4E2028466F6C5F4364694C6F63616C203D204C6F635F4364694C6F63616C290D0A494E4E4552204A4F494E20456D707265736173204F4E20284C6F635F436469456D7072657361203D20456D705F436469456D7072657361290D0A494E4E4552204A4F494E205469706F73456E64657265636F73204F4E20284C4F435F4364695469706F456E64657265636F203D2054454E5F4364695469706F456E64657265636F290D0A494E4E4552204A4F494E20436F6E7472617461646F73457874726173204F4E2028434F4E5F436469436F6E7472617461646F203D20434F455F436469436F6E7472617461646F290D0A494E4E4552204A4F494E20436F72657350656C65204F4E202843504C5F436469436F7250656C65203D20434F4E5F436469436F7250656C6520290D0A494E4E4552204A4F494E204772617573496E73747275636F6573204F4E2028434F4E5F43646947726175496E7374727563616F203D2047494E5F43646947726175496E7374727563616F290D0A494E4E4552204A4F494E2045737461646F73207265736964204F4E2028434F4E5F43646945737461646F5F5265736964203D2072657369642E4553545F43646945737461646F290D0A494E4E4552204A4F494E2045737461646F732061732075665247204F4E2028434F4E5F43646945737461646F5F4F7267616F5267203D20756652472E4553545F43646945737461646F290D0A494E4E4552204A4F494E2045737461646F732061732063617250726F66204F4E2028434F4E5F43646945737461646F5F4F7267616F5267203D2063617250726F662E4553545F43646945737461646F290D0A494E4E4552204A4F494E2045737461646F7320617320756E6964616465204F4E20284C4F435F43646945737461646F203D20756E69646164652E4553545F43646945737461646F290D0A494E4E4552204A4F494E2045737461646F734369766973204F4E20284554435F43646945737461646F436976696C203D20434F4E5F43646945737461646F436976696C20290D0A494E4E4552204A4F494E205365786F73204F4E20287365785F6364697365786F203D20434F4E5F4364695365786F290D0A494E4E4552204A4F494E2056696E63756C6F73204F4E202856494E5F63646976696E63756C6F203D20434F4E5F63646976696E63756C6F290D0A494E4E4552204A4F494E20436172676F73204F4E20284341525F636469636172676F203D20434F4E5F636469636172676F20290D0A494E4E4552204A4F494E20436F6442726173696C6569726F4F63757061636F6573204F4E20284341525F436469436F6442726173696C6569726F4F6375706163616F203D2043424F5F436469436F6442726173696C6569726F4F6375706163616F20290D0A494E4E4552204A4F494E2043656E74726F73437573746F73204F4E20284343555F43646943656E74726F437573746F203D20434F4E5F43646943656E74726F437573746F290D0A574845524520434F4E5F436469436F6E7472617461646F203D203A636469636F6E7472617461646F', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 21, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 21, @ADN_CdiComandoSQLGrupo, 1, '''INTEGRACAO POR RESULT SQL'', 0x4445434C415245204052616E644D657320494E54454745520D0A20534554204052616E644D6573203D20282841425328434845434B53554D284E4557494428292929202520313229202B2031290D0A0D0A2053454C4543540D0A204D45535F4364694D65732041532043616D706F3153514C2C0D0A204D45535F4431734D65732041532043616D706F3253514C2C0D0A204745544441544528292041532043616D706F3353514C2C0D0A20464F524D4154284765744461746528292C202764642D4D4D2D7979797927292041532043616D706F3453514C2C0D0A20434F4E5645525428646174652C20476574446174652829292041532043616D706F3553514C2C0D0A204052616E644D65732041532043616D706F3653514C0D0A0D0A2046524F4D204D657365730D0A205748455245204D45535F4364694D6573203C3E204052616E644D65730D0A204F52444552204259203120415343', 1

		/*#### OBJETO - 554 ModelosIntegracoes MI*/
		/*SOAP*/ /*MI: 10001*/ exec sp_Execute_Insert 'dbo', 01, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10001, ''(OUT) SOAP CONSULTA CEP''', 1
		/*REST*/ /*MI: 10002*/ exec sp_Execute_Insert 'dbo', 02, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10002, ''(OUT) REST SEM PARAMETROS''', 1
		/*REST*/ /*MI: 10003*/ exec sp_Execute_Insert 'dbo', 03, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10003, ''(OUT) REST COM PARAMETROS''', 1
		/*REST*/ /*MI: 10004*/ exec sp_Execute_Insert 'dbo', 04, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10004, ''(OUT) REST COM PARAMETROS URL''', 1
		/*REST*/ /*MI: 10005*/ exec sp_Execute_Insert 'dbo', 05, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10005, ''(IN) REST ALTERACAO OBJ 2330''', 1
		/*REST*/ /*MI: 10006*/ exec sp_Execute_Insert 'dbo', 06, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10006, ''(IN) REST MARCACAO PONTO''', 1
		/*REST*/ /*MI: 10007*/ exec sp_Execute_Insert 'dbo', 07, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10007, ''(IN) REST ENTRADA BASEX64''', 1
		/*SOAP*/ /*MI: 10008*/ exec sp_Execute_Insert 'dbo', 08, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10008, ''(OUT) SOAP CORREIOS LOTE''', 1
		/*REST*/ /*MI: 10009*/ exec sp_Execute_Insert 'dbo', 09, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10009, ''(IN) QUERY EXECUTE INTEGRACAO FOTO''', 1
		/*SOAP*/ /*MI: 10010*/ exec sp_Execute_Insert 'dbo', 10, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10010, ''(OUT) POSTMAN api/v1/get/{{identifier}}''', 1
		/*REST*/ /*MI: 10011*/ exec sp_Execute_Insert 'dbo', 11, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10011, ''(OUT) POSTMAN api/v1/put/{{identifier}}''', 1
		/*REST*/ /*MI: 10012*/ exec sp_Execute_Insert 'dbo', 12, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10012, ''(OUT) POSTMAN api/v1/post/add''', 1
		/*REST*/ /*MI: 10013*/ exec sp_Execute_Insert 'dbo', 13, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10013, ''(IN) REST CONSULTAS REMOTAS''', 1
		/*REST*/ /*MI: 10014*/ exec sp_Execute_Insert 'dbo', 14, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10014, ''(OUT) POSTMAN api/v1/get/arrayjson''', 1
		/*REST*/ /*MI: 10015*/ exec sp_Execute_Insert 'dbo', 15, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10015, ''(OUT) POSTMAN api/v1/post/keyid''', 1
		/*REST*/ /*MI: 10016*/ exec sp_Execute_Insert 'dbo', 16, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10016, ''(OUT) REPROCESS api/v1/get/keyid''', 1
		/*REST*/ /*MI: 10017*/ exec sp_Execute_Insert 'dbo', 17, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10017, ''(OUT) POSTMAN /api/v2/get/arrayjson''', 1
		/*REST*/ /*MI: 10018*/ exec sp_Execute_Insert 'dbo', 18, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10018, ''(IN) REST COMBATIDAS REAIS''', 1
		/*SOAP*/ /*MI: 10019*/ exec sp_Execute_Insert 'dbo', 19, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10019, ''(OUT) SOAP TESTE WSDL TECBAN''', 1
		/*REST*/ /*MI: 10020*/ exec sp_Execute_Insert 'dbo', 20, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10020, ''(OUT) REST OAUTH SERASA''', 1
		/*REST*/ /*MI: 10021*/ exec sp_Execute_Insert 'dbo', 21, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10021, ''(OUT) REST ADD DATE/HOUR IN OBJ 3129''', 1
		/*REST*/ /*MI: 10022*/ exec sp_Execute_Insert 'dbo', 22, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10022, ''(OUT) REST MODELO SUCESSO''', 1
		/*REST*/ /*MI: 10023*/ exec sp_Execute_Insert 'dbo', 23, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10023, ''(OUT) POSTMAN /api/v1/get/{{identifier}}''', 1
		/*REST*/ /*MI: 10024*/ exec sp_Execute_Insert 'dbo', 24, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10024, ''(OUT) POSTMAN /api/v3/get/arrayjson''', 1
		/*REST*/ /*MI: 10025*/ exec sp_Execute_Insert 'dbo', 25, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10025, ''(IN) REST ALTER DA TRANSACAO E O TIPO EDICAO''', 1
		/*SOAP*/ /*MI: 10026*/ exec sp_Execute_Insert 'dbo', 26, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10026, ''(OUT) SOAP ADMISSAO - INCLUSAO DEPENDENTES''', 1
		/*SOAP*/ /*MI: 10027*/ exec sp_Execute_Insert 'dbo', 27, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10027, ''(OUT) SOAP ADMISSAO''', 1
		/*SOAP*/ /*MI: 10028*/ exec sp_Execute_Insert 'dbo', 28, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10028, ''(OUT) SOAP TRANSF HIERARQUIA''', 1
		/*MONI*/ /*MI: 10029*/ exec sp_Execute_Insert 'dbo', 29, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao, BBR_QtiSegundosVerNovoEvento, BBR_DsbComandoSQLNovoEvento, BBR_DsbComandoSQLProcessando, BBR_DsbComandoSQLSucesso, BBR_DsbComandoSQLFalha', '10029, ''MONITORAMENTO BASE'', 1000, 0xEFBBBF73656C656374202A2066726F6D20496E746567726174696F6E4D6F6E69746F726163616F207768657265207374617475735F203D2031, 0xEFBBBF75706461746520496E746567726174696F6E4D6F6E69746F726163616F20736574207374617475735F203D20322077686572652069645F696E746567726174696F6E6D6F6E69746F726163616F203D203A69645F696E746567726174696F6E6D6F6E69746F726163616F, 0xEFBBBF75706461746520496E746567726174696F6E4D6F6E69746F726163616F20736574207374617475735F203D20332077686572652069645F696E746567726174696F6E6D6F6E69746F726163616F203D203A69645F696E746567726174696F6E6D6F6E69746F726163616F, 0xEFBBBF75706461746520496E746567726174696F6E4D6F6E69746F726163616F20736574207374617475735F203D20342077686572652069645F696E746567726174696F6E6D6F6E69746F726163616F203D203A69645F696E746567726174696F6E6D6F6E69746F726163616F', 1
		/*SOAP*/ /*MI: 10030*/ exec sp_Execute_Insert 'dbo', 30, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10030, ''(OUT) SOAP SOC''', 1
		/*REST*/ /*MI: 10031*/ exec sp_Execute_Insert 'dbo', 31, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10031, ''(OUT) POSTMAN api/v4/get/arrayjson''', 1
		/*REST*/ /*MI: 10032*/ exec sp_Execute_Insert 'dbo', 32, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10032, ''(OUT) POSTMAN api/v1/patch/{{identifier}}''', 1
		/*REST*/ /*MI: 10033*/ exec sp_Execute_Insert 'dbo', 32, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10033, ''(OUT) POSTMAN api/v1/get/tokens/two''', 1
		/*REST*/ /*MI: 10034*/ exec sp_Execute_Insert 'dbo', 33, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10034, ''(OUT) POSTMAN PRE REQUEST''', 1
		/*REST*/ /*MI: 10035*/ exec sp_Execute_Insert 'dbo', 34, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10035, ''(OUT) POSTMAN SQL''', 1
		/*REST*/ /*MI: 10036*/ exec sp_Execute_Insert 'dbo', 36, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10036, ''(OUT) POSTMAN api/v5/get/arrayjson''', 1
		/*REST*/ /*MI: 10037*/ exec sp_Execute_Insert 'dbo', 36, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10037, ''(IN/OUT) REPROCESS MODEL''', 1
		/*REST*/ /*MI: 10038*/ exec sp_Execute_Insert 'dbo', 37, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10038, ''(IN) PROVIDER GRID''', 1
		/*REST*/ /*MI: 10039*/ exec sp_Execute_Insert 'dbo', 38, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10039, ''(IN) WEBHOOK TESTS''', 1
		/*REST*/ /*MI: 10040*/ exec sp_Execute_Insert 'dbo', 39, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10040, ''(OUT) POSTMAN A api/v1/delete''', 1
		/*REST*/ /*MI: 10041*/ exec sp_Execute_Insert 'dbo', 39, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10041, ''(OUT) POSTMAN B api/v1/delete''', 1

		/*#### OBJETO - 554 - ModeloIntegracoes/EventoMonitoracaoBase */
		/*MONI*/ exec sp_Execute_Insert 'dbo', 01, 'ModsIntsMonitsBasesEventos', 'EBK_CdiModIntMonitBaseEvento, EBK_CdiModeloIntegracao, EBK_CdiTransacao, EBK_CdiIdioma, EBK_D1sModIntMonitBaseEvento, EBK_DssCondicaoExecucao, EBK_DsbComandoSQLEvento', '10002, 10029, 19232, 1, ''(TESTES) MONIT INSERIR CARGO'', ''Evento = 1'', 0xEFBBBF73656C656374202A2066726F6D20496E746567726174696F6E4D6F6E69746F726163616F2077686572652069645F696E746567726174696F6E6D6F6E69746F726163616F203D203A69645F696E746567726174696F6E6D6F6E69746F726163616F', 1

		/*#### OBJETO - 555 */
		/*##### ModelosIntegracoesCmds MIC*/ 
		/*SOAP*/ /*MI: 10001, MIC: 10001*/ exec sp_Execute_Insert 'dbo', 01, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto', '10001, 10001, ''(TESTES) SOAP CORREIOS CONSULTA CEP - CMD'', 2, 30063, ''AtendeClienteService;AtendeCliente;consultaCEP#consultaCEPResponse;return;cep###0''', 1
		/*REST*/ /*MI: 10002, MIC: 10002*/ exec sp_Execute_Insert 'dbo', 02, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP', '10002, 10002, ''(TESTES) REST SEM PARAMETROS'', 2, 30063, ''http://echo.jsontest.com/key/value/one/two'', 1', 1
		/*REST*/ /*MI: 10003, MIC: 10003*/ exec sp_Execute_Insert 'dbo', 03, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP', '10003, 10003, ''(TESTES) REST COM PARAMETROS'', 2, 30063, ''http://validate.jsontest.com/?json='', 1', 1  
		/*REST*/ /*MI: 10004, MIC: 10004*/ exec sp_Execute_Insert 'dbo', 04, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP', '10004, 10004, ''(TESTES) REST COM PARAMETROS URL'', 2, 30063, '''', 1', 1  
		/*REST*/ /*MI: 10005, MIC: 10005*/ exec sp_Execute_Insert 'dbo', 05, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP', '10005, 10005, ''(TESTES) REST ALTERACAO SENHA 2330'', 1, 21233, '''', 1', 1  
		/*REST*/ /*MI: 10006, MIC: 10006*/ exec sp_Execute_Insert 'dbo', 06, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP', '10006, 10006, ''(TESTES) REST MARCACAO PONTO'', 2, 43192, '''', 1', 1  
		/*REST*/ /*MI: 10007, MIC: 10007*/ exec sp_Execute_Insert 'dbo', 07, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_NuiTipoEdicao', '10007, 10007, ''(TESTES) REST ENTRADA BASEX64'',  1, 30842, 407', 1  
		/*REST*/ /*MI: 10008, MIC: 10008*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 08, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_NuiTipoEdicao, BBS_DssNomeObjeto, BBS_OplEnviarTudo, BBS_DssCamposLote', 10008, 0, @SQL_CdiComandoSQL, 3, '10008, ''(TESTES) SOAP CORREIOS LOTE'',  1, 30063, 0, ''AtendeClienteService;AtendeCliente;consultaCEP#consultaCEPResponse;return;cep###0'', 1, ''cep''', 1  
		/*REST*/ /*MI: 10009, MIC: 10009*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 09, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_NuiTipoEdicao, BBS_DssNomeObjeto, BBS_CdiEventoTransacao, BBS_CdiVerboHTTP', 10009, 0, @SQL_CdiComandoSQL, 4, '10009, ''(TESTES) QUERY EXECUTE INTEGRACAO FOTO'',  1, 30063, 0, ''https://httpbin.org/post'', 2, 3', 1  
		/*REST*/ /*MI: 10010, MIC: 10010*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 10, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP', 10010, 0, @SQL_CdiComandoSQL, 7, '10010, ''(TESTES) REST MOCK POSTMAN GET'', 1, 30063, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v1/get/keyid'', 1', 1  
		/*REST*/ /*MI: 10011, MIC: 10011*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 11, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP, BBS_CdiEventoTransacao', 10011, 0, @SQL_CdiComandoSQL, 5,  '10011, ''(TESTES) REST MOCK POSTMAN PUT'', 1, 30063, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v1/put/keyid'', 2, 2', 1  
		/*REST*/ /*MI: 10012, MIC: 10012*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 12, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_NuiTipoEdicao, BBS_DssNomeObjeto, BBS_CdiEventoTransacao, BBS_CdiVerboHTTP', 10012, 0, @SQL_CdiComandoSQL, 6,  '10012, ''(TESTES) SAIDA REST 1106'',  1, 15953, 0, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v1/post/add'', 2, 3', 1
		/*REST*/ /*MI: 10013, MIC: 10013*/ exec sp_Execute_Insert 'dbo', 13, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr', '10013, 10013, ''(TESTES) REST CONSULTAS REMOTAS OAUTH'', 1', 1
		/*REST*/ /*MI: 10014, MIC: 10014*/ exec sp_Execute_Insert 'dbo', 14, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP', '10014, 10014, ''(TESTES) REST MOCK POSTMAN V1'', 1, 30063, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v1/get/arrayjson'', 1', 1  
		/*REST*/ /*MI: 10015, MIC: 10015*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 15, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP', 10015, 0, @SQL_CdiComandoSQL, 17, '10015, ''(TESTES) REST API THIRDPART GET'', 1, 30063, ''https://62d6befa51e6e8f06f1214f9.mockapi.io/api/v1/post/keyid'', 1', 1   
		/*REST*/ /*MI: 10016, MIC: 10016*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 16, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP, BBS_OplGravarResponse', 10016, 0, @SQL_CdiComandoSQL, 7, '10016, ''(TESTES) MOCK REST REPROCESS'', 1, 30063, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v1/get/keyid'', 1, 1', 1  
		/*REST*/ /*MI: 10017, MIC: 10017*/ exec sp_Execute_Insert 'dbo', 17, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP, BBS_OplGravarResponse', '10017, 10017, ''(TESTES) REST MOCK POSTMAN GET COMPLEXY ARRAY V2'', 1, 30063, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v2/get/arrayjson'', 1, 1', 1  
		/*REST*/ /*MI: 10018, MIC: 10018*/ exec sp_Execute_Insert 'dbo', 18, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_CdiTipoComandoIntegr, BBS_DssNomeObjeto, BBS_CdiTransacao, BBS_NuiOrdem, BBS_OplOneForManyInclusao, BBS_OplValidaVazio, BBS_CdiComandoSQL, BBS_OplComandoNativo, BBS_CdiEventoTransacao, BBS_CdiFormularioWF, BBS_D1sModeloIntegracaoCmd, BBS_D2sModeloIntegracaoCmd, BBS_D3sModeloIntegracaoCmd, BBS_D4sModeloIntegracaoCmd, BBS_D5sModeloIntegracaoCmd, BBS_D6sModeloIntegracaoCmd, BBS_D7sModeloIntegracaoCmd, BBS_D8sModeloIntegracaoCmd, BBS_DsbComandoSQLValidacao, BBS_D1bDescritivo, BBS_D2bDescritivo, BBS_D3bDescritivo, BBS_D4bDescritivo, BBS_D5bDescritivo, BBS_D6bDescritivo, BBS_D7bDescritivo, BBS_D8bDescritivo, BBS_CdiModeloIntegracaoCmd_Ftr, BBS_NuiTipoEdicao, BBS_CdiTransacaoWF, BBS_CdiModeloIntegracao_Exc, BBS_CdiModeloIntegracao_Suc, BBS_CdiTpTratamentoIntegracao, BBS_OplExecutarSQL_Apdata, BBS_OplEnviarTudo, BBS_OplDesativado, BBS_CdiVerboHTTP, BBS_CdiModeloIntegracaoCmd_Pre, BBS_CdiLayOutSaida_Autentica, BBS_CdiLayOutSaida', /*values*/ '10018, 10018, 1, null, 29993, 0, 0, 0, 0, 0, 2, 0, ''(TESTES) INVALIDACAO MARCACAO - Obj 3111'', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0', 1  
		/*REST*/ /*MI: 10018, MIC: 10019*/ exec sp_Execute_Insert 'dbo', 19, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_CdiTipoComandoIntegr, BBS_DssNomeObjeto, BBS_CdiTransacao, BBS_NuiOrdem, BBS_OplOneForManyInclusao, BBS_OplValidaVazio, BBS_CdiComandoSQL, BBS_OplComandoNativo, BBS_CdiEventoTransacao, BBS_CdiFormularioWF, BBS_D1sModeloIntegracaoCmd, BBS_D2sModeloIntegracaoCmd, BBS_D3sModeloIntegracaoCmd, BBS_D4sModeloIntegracaoCmd, BBS_D5sModeloIntegracaoCmd, BBS_D6sModeloIntegracaoCmd, BBS_D7sModeloIntegracaoCmd, BBS_D8sModeloIntegracaoCmd, BBS_DsbComandoSQLValidacao, BBS_D1bDescritivo, BBS_D2bDescritivo, BBS_D3bDescritivo, BBS_D4bDescritivo, BBS_D5bDescritivo, BBS_D6bDescritivo, BBS_D7bDescritivo, BBS_D8bDescritivo, BBS_CdiModeloIntegracaoCmd_Ftr, BBS_NuiTipoEdicao, BBS_CdiTransacaoWF, BBS_CdiModeloIntegracao_Exc, BBS_CdiModeloIntegracao_Suc, BBS_CdiTpTratamentoIntegracao, BBS_OplExecutarSQL_Apdata, BBS_OplEnviarTudo, BBS_OplDesativado, BBS_CdiVerboHTTP, BBS_CdiModeloIntegracaoCmd_Pre, BBS_CdiLayOutSaida_Autentica, BBS_CdiLayOutSaida', /*values*/ '10019, 10018, 1, null, 39802, 0, 0, 0, 0, 0, 2, 0, ''(TESTES) INCLUSAO MARCACAO - Obj. 4176'', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0', 1  
		/*REST*/ /*MI: 10018, MIC: 10020*/ exec sp_Execute_Insert 'dbo', 20, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_CdiTipoComandoIntegr, BBS_DssNomeObjeto, BBS_CdiTransacao, BBS_NuiOrdem, BBS_OplOneForManyInclusao, BBS_OplValidaVazio, BBS_CdiComandoSQL, BBS_OplComandoNativo, BBS_CdiEventoTransacao, BBS_CdiFormularioWF, BBS_D1sModeloIntegracaoCmd, BBS_D2sModeloIntegracaoCmd, BBS_D3sModeloIntegracaoCmd, BBS_D4sModeloIntegracaoCmd, BBS_D5sModeloIntegracaoCmd, BBS_D6sModeloIntegracaoCmd, BBS_D7sModeloIntegracaoCmd, BBS_D8sModeloIntegracaoCmd, BBS_DsbComandoSQLValidacao, BBS_D1bDescritivo, BBS_D2bDescritivo, BBS_D3bDescritivo, BBS_D4bDescritivo, BBS_D5bDescritivo, BBS_D6bDescritivo, BBS_D7bDescritivo, BBS_D8bDescritivo, BBS_CdiModeloIntegracaoCmd_Ftr, BBS_NuiTipoEdicao, BBS_CdiTransacaoWF, BBS_CdiModeloIntegracao_Exc, BBS_CdiModeloIntegracao_Suc, BBS_CdiTpTratamentoIntegracao, BBS_OplExecutarSQL_Apdata, BBS_OplEnviarTudo, BBS_OplDesativado, BBS_CdiVerboHTTP, BBS_CdiModeloIntegracaoCmd_Pre, BBS_CdiLayOutSaida_Autentica, BBS_CdiLayOutSaida', /*values*/ '10020, 10018, 1, null, 39803, 0, 0, 0, 0, 0, 2, 0, ''(TESTES) ALTERACAO MARCACAO - Obj. 4176'', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0', 1  
		/*REST*/ /*MI: 10018, MIC: 10021*/ exec sp_Execute_Insert 'dbo', 21, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_CdiTipoComandoIntegr, BBS_DssNomeObjeto, BBS_CdiTransacao, BBS_NuiOrdem, BBS_OplOneForManyInclusao, BBS_OplValidaVazio, BBS_CdiComandoSQL, BBS_OplComandoNativo, BBS_CdiEventoTransacao, BBS_CdiFormularioWF, BBS_D1sModeloIntegracaoCmd, BBS_D2sModeloIntegracaoCmd, BBS_D3sModeloIntegracaoCmd, BBS_D4sModeloIntegracaoCmd, BBS_D5sModeloIntegracaoCmd, BBS_D6sModeloIntegracaoCmd, BBS_D7sModeloIntegracaoCmd, BBS_D8sModeloIntegracaoCmd, BBS_DsbComandoSQLValidacao, BBS_D1bDescritivo, BBS_D2bDescritivo, BBS_D3bDescritivo, BBS_D4bDescritivo, BBS_D5bDescritivo, BBS_D6bDescritivo, BBS_D7bDescritivo, BBS_D8bDescritivo, BBS_CdiModeloIntegracaoCmd_Ftr, BBS_NuiTipoEdicao, BBS_CdiTransacaoWF, BBS_CdiModeloIntegracao_Exc, BBS_CdiModeloIntegracao_Suc, BBS_CdiTpTratamentoIntegracao, BBS_OplExecutarSQL_Apdata, BBS_OplEnviarTudo, BBS_OplDesativado, BBS_CdiVerboHTTP, BBS_CdiModeloIntegracaoCmd_Pre, BBS_CdiLayOutSaida_Autentica, BBS_CdiLayOutSaida', /*values*/ '10021, 10018, 1, null, 30132, 0, 0, 0, 0, 0, 1, 0, ''(TESTES) INCLUSAO DE CRACHA PROVISOARIO'', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0', 1  
		/*REST*/ /*MI: 10018, MIC: 10022*/ exec sp_Execute_Insert 'dbo', 22, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_CdiTipoComandoIntegr, BBS_DssNomeObjeto, BBS_CdiTransacao, BBS_NuiOrdem, BBS_OplOneForManyInclusao, BBS_OplValidaVazio, BBS_CdiComandoSQL, BBS_OplComandoNativo, BBS_CdiEventoTransacao, BBS_CdiFormularioWF, BBS_D1sModeloIntegracaoCmd, BBS_D2sModeloIntegracaoCmd, BBS_D3sModeloIntegracaoCmd, BBS_D4sModeloIntegracaoCmd, BBS_D5sModeloIntegracaoCmd, BBS_D6sModeloIntegracaoCmd, BBS_D7sModeloIntegracaoCmd, BBS_D8sModeloIntegracaoCmd, BBS_DsbComandoSQLValidacao, BBS_D1bDescritivo, BBS_D2bDescritivo, BBS_D3bDescritivo, BBS_D4bDescritivo, BBS_D5bDescritivo, BBS_D6bDescritivo, BBS_D7bDescritivo, BBS_D8bDescritivo, BBS_CdiModeloIntegracaoCmd_Ftr, BBS_NuiTipoEdicao, BBS_CdiTransacaoWF, BBS_CdiModeloIntegracao_Exc, BBS_CdiModeloIntegracao_Suc, BBS_CdiTpTratamentoIntegracao, BBS_OplExecutarSQL_Apdata, BBS_OplEnviarTudo, BBS_OplDesativado, BBS_CdiVerboHTTP, BBS_CdiModeloIntegracaoCmd_Pre, BBS_CdiLayOutSaida_Autentica, BBS_CdiLayOutSaida', /*values*/ '10022, 10018, 1, null, 30133, 0, 0, 0, 0, 0, 1, 0, ''(TESTES) BAIXA CRACHA PROVISOARIO'', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0', 1  
		/*REST*/ /*MI: 10019, MIC: 10023*/ exec sp_Execute_Insert 'dbo', 23, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_CdiTipoComandoIntegr, BBS_DssNomeObjeto, BBS_CdiTransacao, BBS_NuiOrdem, BBS_OplOneForManyInclusao, BBS_OplValidaVazio, BBS_CdiComandoSQL, BBS_OplComandoNativo, BBS_CdiEventoTransacao, BBS_CdiFormularioWF, BBS_D1sModeloIntegracaoCmd, BBS_D2sModeloIntegracaoCmd, BBS_D3sModeloIntegracaoCmd, BBS_D4sModeloIntegracaoCmd, BBS_D5sModeloIntegracaoCmd, BBS_D6sModeloIntegracaoCmd, BBS_D7sModeloIntegracaoCmd, BBS_D8sModeloIntegracaoCmd, BBS_DsbComandoSQLValidacao, BBS_D1bDescritivo, BBS_D2bDescritivo, BBS_D3bDescritivo, BBS_D4bDescritivo, BBS_D5bDescritivo, BBS_D6bDescritivo, BBS_D7bDescritivo, BBS_D8bDescritivo, BBS_CdiModeloIntegracaoCmd_Ftr, BBS_NuiTipoEdicao, BBS_CdiTransacaoWF, BBS_CdiModeloIntegracao_Exc, BBS_CdiModeloIntegracao_Suc, BBS_CdiTpTratamentoIntegracao, BBS_OplExecutarSQL_Apdata, BBS_OplEnviarTudo, BBS_OplDesativado, BBS_CdiVerboHTTP, BBS_CdiModeloIntegracaoCmd_Pre, BBS_CdiLayOutSaida_Autentica, BBS_CdiLayOutSaida', /*values*/ '10023, 10019, 1, ''SFAPIService12;SFAPI12;login####0'', 30063, 0, 0, 0, 0, 0, 2, 0, ''(TESTES) SOAP WSDL TECBAN'', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0', 1  
		/*REST*/ /*MI: 10020, MIC: 10024*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 23, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_CdiTipoComandoIntegr, BBS_DssNomeObjeto, BBS_CdiTransacao, BBS_NuiOrdem, BBS_OplOneForManyInclusao, BBS_OplValidaVazio, BBS_CdiModeloIntegracaoCmd_Ftr, BBS_OplComandoNativo, BBS_CdiEventoTransacao, BBS_CdiTransacaoWF, BBS_NuiTipoEdicao, BBS_CdiModeloIntegracao_Exc, BBS_CdiModeloIntegracao_Suc, BBS_CdiTpTratamentoIntegracao, BBS_OplExecutarSQL_Apdata, BBS_OplDesativado, BBS_OplEnviarTudo, BBS_CdiVerboHTTP, BBS_DssCamposLote, BBS_OplLoteBase, BBS_CdiModeloIntegracaoCmd_Pre, BBS_CdiLayOutSaida_Autentica, BBS_D1sModeloIntegracaoCmd, BBS_D2sModeloIntegracaoCmd, BBS_D3sModeloIntegracaoCmd, BBS_D4sModeloIntegracaoCmd, BBS_D5sModeloIntegracaoCmd, BBS_D6sModeloIntegracaoCmd, BBS_D7sModeloIntegracaoCmd, BBS_D8sModeloIntegracaoCmd, BBS_DsbComandoSQLValidacao, BBS_D1bDescritivo, BBS_D2bDescritivo, BBS_D3bDescritivo, BBS_D4bDescritivo, BBS_D5bDescritivo, BBS_D6bDescritivo, BBS_D7bDescritivo, BBS_D8bDescritivo, BBS_OplGravarResponse, BBS_DssCamposResponse, BBS_CdiTransacao_Retorno, BBS_NuiTipoEdicao_Retorno, BBS_CdiModeloIntegracao_Ret, BBS_CdiFormularioWF', /*values*/ 10024, 0, @SQL_CdiComandoSQL, 12, '10020, 1, ''https://api.serasaexperian.com.br/security/iam/v1/client-identities/login'', 76294, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 3, null, 0, 0, 0, ''(TESTE) TOKEN BEARER SERASA'', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, 0, null, 0, 0, 0, 0', 1
		/*REST*/ /*MI: 10020, MIC: 10025*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 24, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_CdiTipoComandoIntegr, BBS_DssNomeObjeto, BBS_CdiTransacao, BBS_NuiOrdem, BBS_OplOneForManyInclusao, BBS_OplValidaVazio, BBS_CdiModeloIntegracaoCmd_Ftr, BBS_OplComandoNativo, BBS_CdiEventoTransacao, BBS_CdiTransacaoWF, BBS_NuiTipoEdicao, BBS_CdiModeloIntegracao_Exc, BBS_CdiModeloIntegracao_Suc, BBS_CdiTpTratamentoIntegracao, BBS_OplExecutarSQL_Apdata, BBS_OplDesativado, BBS_OplEnviarTudo, BBS_CdiVerboHTTP, BBS_DssCamposLote, BBS_OplLoteBase, BBS_CdiModeloIntegracaoCmd_Pre, BBS_CdiLayOutSaida_Autentica, BBS_D1sModeloIntegracaoCmd, BBS_D2sModeloIntegracaoCmd, BBS_D3sModeloIntegracaoCmd, BBS_D4sModeloIntegracaoCmd, BBS_D5sModeloIntegracaoCmd, BBS_D6sModeloIntegracaoCmd, BBS_D7sModeloIntegracaoCmd, BBS_D8sModeloIntegracaoCmd, BBS_DsbComandoSQLValidacao, BBS_D1bDescritivo, BBS_D2bDescritivo, BBS_D3bDescritivo, BBS_D4bDescritivo, BBS_D5bDescritivo, BBS_D6bDescritivo, BBS_D7bDescritivo, BBS_D8bDescritivo, BBS_OplGravarResponse, BBS_DssCamposResponse, BBS_CdiTransacao_Retorno, BBS_NuiTipoEdicao_Retorno, BBS_CdiModeloIntegracao_Ret, BBS_CdiFormularioWF', /*values*/ 10025, 0, @SQL_CdiComandoSQL, 12, '10020, 1, ''https://api.serasaexperian.com.br/finance/accountspayable/v2/invoices'', 30063, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 3, null, 0, 10024, 1009, ''(TESTE) SERASA - APROVACAO INTERFACE FINANCEIRA'', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, 0, null, 0, 0, 0, 0', 1
		/*REST*/ /*MI: 10021, MIC: 10026*/ exec sp_Execute_Insert 'dbo', 25, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_CdiTipoComandoIntegr, BBS_DssNomeObjeto, BBS_CdiTransacao, BBS_NuiOrdem, BBS_OplOneForManyInclusao, BBS_OplValidaVazio, BBS_CdiComandoSQL, BBS_OplComandoNativo, BBS_CdiEventoTransacao, BBS_CdiFormularioWF, BBS_D1sModeloIntegracaoCmd, BBS_D2sModeloIntegracaoCmd, BBS_D3sModeloIntegracaoCmd, BBS_D4sModeloIntegracaoCmd, BBS_D5sModeloIntegracaoCmd, BBS_D6sModeloIntegracaoCmd, BBS_D7sModeloIntegracaoCmd, BBS_D8sModeloIntegracaoCmd, BBS_DsbComandoSQLValidacao, BBS_D1bDescritivo, BBS_D2bDescritivo, BBS_D3bDescritivo, BBS_D4bDescritivo, BBS_D5bDescritivo, BBS_D6bDescritivo, BBS_D7bDescritivo, BBS_D8bDescritivo, BBS_CdiModeloIntegracaoCmd_Ftr, BBS_NuiTipoEdicao, BBS_CdiTransacaoWF, BBS_CdiModeloIntegracao_Exc, BBS_CdiModeloIntegracao_Suc, BBS_CdiTpTratamentoIntegracao, BBS_OplExecutarSQL_Apdata, BBS_OplEnviarTudo, BBS_OplDesativado, BBS_CdiVerboHTTP, BBS_DssCamposLote, BBS_OplLoteBase, BBS_CdiLayOutSaida, BBS_CdiModeloIntegracaoCmd_Pre, BBS_CdiLayOutSaida_Autentica, BBS_OplGravarResponse, BBS_DssCamposResponse, BBS_CdiTransacao_Retorno, BBS_NuiTipoEdicao_Retorno, BBS_CdiModeloIntegracao_Ret', /*values*/ '10026, 10021, 1, null, 30122, 0, 0, 0, 0, 0, 0, 0, ''(TESTES) REST ADD DATE/HOUR IN OBJ 3129'', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, null, 0, 0, 0, 0, 0, null, 0, 0, 0', 1  
		/*REST*/ /*MI: 10022, MIC: 10027*/ exec sp_Execute_Insert 'dbo', 26, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_CdiTipoComandoIntegr, BBS_DssNomeObjeto, BBS_CdiTransacao, BBS_NuiOrdem, BBS_OplOneForManyInclusao, BBS_OplValidaVazio, BBS_CdiComandoSQL, BBS_OplComandoNativo, BBS_CdiEventoTransacao, BBS_CdiFormularioWF, BBS_D1sModeloIntegracaoCmd, BBS_D2sModeloIntegracaoCmd, BBS_D3sModeloIntegracaoCmd, BBS_D4sModeloIntegracaoCmd, BBS_D5sModeloIntegracaoCmd, BBS_D6sModeloIntegracaoCmd, BBS_D7sModeloIntegracaoCmd, BBS_D8sModeloIntegracaoCmd, BBS_DsbComandoSQLValidacao, BBS_D1bDescritivo, BBS_D2bDescritivo, BBS_D3bDescritivo, BBS_D4bDescritivo, BBS_D5bDescritivo, BBS_D6bDescritivo, BBS_D7bDescritivo, BBS_D8bDescritivo, BBS_CdiModeloIntegracaoCmd_Ftr, BBS_NuiTipoEdicao, BBS_CdiTransacaoWF, BBS_CdiModeloIntegracao_Exc, BBS_CdiModeloIntegracao_Suc, BBS_CdiTpTratamentoIntegracao, BBS_OplExecutarSQL_Apdata, BBS_OplEnviarTudo, BBS_OplDesativado, BBS_CdiVerboHTTP, BBS_DssCamposLote, BBS_OplLoteBase, BBS_CdiLayOutSaida, BBS_CdiModeloIntegracaoCmd_Pre, BBS_CdiLayOutSaida_Autentica, BBS_OplGravarResponse, BBS_DssCamposResponse, BBS_CdiTransacao_Retorno, BBS_NuiTipoEdicao_Retorno, BBS_CdiModeloIntegracao_Ret', /*values*/ '10027, 10022, 1, null, 30063, 0, 0, 0, 0, 0, 0, 0, ''(TESTES) REST MODELO SUCESSO'', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, null, 0, 0, 0, 0, 0, null, 0, 0, 0', 1  
		/*REST*/ /*MI: 10023, MIC: 10028*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 27, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP, BBS_DssCamposLote, BBS_OplEnviarTudo', 10028, 0, @SQL_CdiComandoSQL, 16, '10023, ''(TESTES) REST POSTMAN LOTE'', 1, 30063, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v1/get/keyid'', 1, ''keyid'', 1', 1
		/*REST*/ /*MI: 10024, MIC: 10029*/ exec sp_Execute_Insert 'dbo', 28, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP', '10029, 10024, ''(TESTES) REST MOCK POSTMAN GET COMPLEXY ARRAY'', 1, 30063, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v3/get/arrayjson'', 1', 1  
		/*REST*/ /*MI: 10025, MIC: 10030*/ exec sp_Execute_Insert_ThreeKey 'dbo', 29, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL_Transacao, BBS_CdiComandoSQL_TipoEdicao, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_NuiTipoEdicao',/*values*/ 10030, 0,  @SQL_CdiComandoSQL, 18, @SQL_CdiComandoSQL, 19, '10025, ''(TESTES) 1 REST ALTER DA TRANSACAO E O TIPO EDICAO'',  1, 30841, 257', 1 
		/*REST*/ /*MI: 10025, MIC: 10032*/ exec sp_Execute_Insert 'dbo', 31, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_NuiTipoEdicao',/*values*/ '10032, 10025, ''(TESTES) 2 REST ALTER DA TRANSACAO E O TIPO EDICAO'',  1, 30842, 407', 1 
		/*SOAP*/ /*MI: 10026, MIC: 10031*/ exec sp_Execute_Insert 'dbo', 30, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao',/*values*/ '10031, 10026, ''(TESTES) ADMISSAO - INCLUSAO DEPENDENTES'', 1, 16012', 1
		/*SOAP*/ /*MI: 10027, MIC: 10033*/ exec sp_Execute_Insert 'dbo', 32, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao',/*values*/ '10033, 10027, ''(TESTES) ADMISSAO - INCLUSAO'', 1, 15952', 1
		/*SOAP*/ /*MI: 10028, MIC: 10034*/ exec sp_Execute_Insert 'dbo', 33, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao',/*values*/ '10034, 10028, ''(TESTES) SOAP TRANSF HIERARQUIA'', 1, 29642', 1
		/*MONI*/ /*MI: 10029, MIC: 10035*/ exec sp_Execute_Insert 'dbo', 34, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao',/*values*/ '10035, 10029, ''(TESTES) MONITORAMENTO BASE INS CARGO'', 1, 19232', 1
		/*SOAP*/ /*MI: 10030, MIC: 10036*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 35, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_NuiTipoEdicao, BBS_DssNomeObjeto, BBS_CdiEventoTransacao, BBS_CdiVerboHTTP', 10036, 0, @SQL_CdiComandoSQL, 20,  '10030, ''(TESTES) SOAP SOC ENVELOPE'',  1, 30063, 0, ''FuncionarioModelo2WsService;FuncionarioModelo2Ws;importacaoFuncionario#importacaoFuncionarioResponse;FuncionarioRetorno;encontrouErro#false#importacaoFuncionarioResponse;FuncionarioRetorno;descricaoErro#0'', 1, 0', 1
		/*REST*/ /*MI: 10031, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 36, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP, BBS_OplGravarResponse', '10037, 10031, ''(TESTES) REST MOCK POSTMAN GET COMPLEXY ARRAY V4'', 1, 30063, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v4/get/arrayjson'', 1, 1', 1
		/*REST*/ /*MI: 10032, MIC: 10038*/ exec sp_Execute_Insert 'dbo', 37, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP, BBS_OplGravarResponse', '10038, 10032, ''(TESTES) REST MOCK POSTMAN VERB PATCH'', 1, 30063, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v1/patch/'', 1, 1', 1
		/*REST*/ /*MI: 10033, MIC: 10039*/ exec sp_Execute_Insert 'dbo', 38, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP, BBS_OplGravarResponse', '10039, 10033, ''(TESTES) REST MOCK POSTMAN TOKENS'', 1, 30063, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v1/get/tokens/two'', 1, 1', 1
		/*REST*/ /*MI: 10034, MIC: 10040*/ exec sp_Execute_Insert 'dbo', 39, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP, BBS_OplGravarResponse,BBS_CdiEventoTransacao', '10040, 10034, ''(TESTES) REST STEP ONE'', 1, 30063, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v1/get/tokens/two'', 1, 1, 2', 1
		/*REST*/ /*MI: 10034, MIC: 10041*/ exec sp_Execute_Insert 'dbo', 40, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP, BBS_OplGravarResponse,BBS_CdiModeloIntegracaoCmd_Pre,BBS_CdiLayOutSaida_Autentica', '10041, 10034, ''(TESTES) REST STEP TWO'', 2, 30063, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v1/get/tokens/two'', 1, 1, 10040, 1012', 1
		/*REST*/ /*MI: 10035, MIC: 10042*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 41, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP, BBS_OplEnviarTudo, BBS_CdiEventoTransacao', 10042, 0, @SQL_CdiComandoSQL, 21, '10035, ''(TESTES) SAIDA SQL'', 1, 30063, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/post/replayContent'', 3, 1, 2', 1
   	    /*REST*/ /*MI: 10036, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 42, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP, BBS_OplGravarResponse, BBS_CdiTransacao_Retorno, BBS_CdiModeloIntegracao_Ret', '10043, 10036, ''(TESTES) REST MOCK POSTMAN GET COMPLEXY ARRAY V5'', 1, 30063, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v5/get/arrayjson'', 1, 1, 39772, 10037', 1
		/*REST*/ /*MI: 10037, MIC: 10044*/ exec sp_Execute_Insert 'dbo', 43, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_CdiVerboHTTP, BBS_OplGravarResponse', '10044, 10037, ''(TESTES) MODELO REPROCESSAMENTO'', 1, 30063, 1, 1', 1
		/*REST*/ /*MI: 10038, MIC: 10045*/ exec sp_Execute_Insert 'dbo', 44, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_OplOneForManyInclusao, BBS_D1sModeloIntegracaoCmd', '10045, 10038, 1, 57852, 1, ''(IN) PROVIDER GRID''', 1
		/*REST*/ /*MI: 10039, MIC: 10046*/ exec sp_Execute_Insert 'dbo', 45, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP, BBS_OplGravarResponse', '10046, 10039, ''(IN) WEBHOOKS EX01'', 1, 30063, ''http://localhost'', 1, 1', 1
		/*REST*/ /*MI: 10039, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 46, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP, BBS_OplGravarResponse', '10047, 10039, ''(IN) WEBHOOKS EX02'', 1, 30063, ''http://localhost'', 1, 1', 1
		/*REST*/ /*MI: 10039, MIC: 10048*/ exec sp_Execute_Insert 'dbo', 47, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP, BBS_OplGravarResponse', '10048, 10039, ''(IN) WEBHOOKS EX03'', 1, 30063, ''http://localhost'', 1, 1', 1
		/*REST*/ /*MI: 10039, MIC: 10049*/ exec sp_Execute_Insert 'dbo', 48, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP, BBS_OplGravarResponse', '10049, 10039, ''(IN) WEBHOOKS EX04'', 1, 30063, ''http://localhost'', 1, 1', 1
		/*REST*/ /*MI: 10040, MIC: 10050*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 49, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP, BBS_CdiEventoTransacao', 10050, 0, @SQL_CdiComandoSQL, 5,  '10040, ''(TESTES) REST MOCK POSTMAN DELETE A'', 1, 30063, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v1/delete'', 5, 2', 1  
		/*REST*/ /*MI: 10041, MIC: 10051*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 50, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP, BBS_CdiEventoTransacao', 10051, 0, @SQL_CdiComandoSQL, 5,  '10041, ''(TESTES) REST MOCK POSTMAN DELETE B'', 1, 30063, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v1/delete/keyid'', 5, 2', 1  

		/*##### ModelosIntegracoesCmdsCpos MICC*/
		/*SOAP*/ /*MIC: 10001, MICC: 10001*/ exec sp_Execute_Insert 'dbo', 001, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_CdiTipoCampo, BBP_DssConteudo_String, BBP_OplConteudoFixo', '10001, 10001, ''consultaCEP;cep'', 9, ''03510030'', 1', 1        
		/*REST*/ /*MIC: 10002, MICC: 10002*/ exec sp_Execute_Insert 'dbo', 002, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_CdiTipoCampo, BBP_DssConteudo_String', '10002, 10002, '''', 9, ''''', 1        
		/*REST*/ /*MIC: 10003, MICC: 10003*/ exec sp_Execute_Insert 'dbo', 003, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_CdiTipoCampo, BBP_DssConteudo_String, BBP_OplConteudoFixo, BBP_CdiLayOutSaida', '10003, 10003, ''valueParameter'', 9, ''value'', 1, 1001', 1
		/*REST*/ /*MIC: 10004, MICC: 10004*/ exec sp_Execute_Insert 'dbo', 004, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_CdiTipoCampo, BBP_DssConteudo_String, BBP_OplConteudoFixo, BBP_CdiLayOutSaida', '10004, 10004, ''cep'', 9, ''01001000'', 1, 1002', 1
        /*REST*/ /*MIC: 10005, MICC: 10005*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 005, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiComandoSQL, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', 10005, 0,  @SQL_CdiComandoSQL, 1, '10005, ''idcontratado'', ''USR_CdiUsuario'', 3', 1
		/*REST*/ /*MIC: 10005, MICC: 10006*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 006, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiComandoSQL, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', 10006, 0,  @SQL_CdiComandoSQL, 2, '10005, '''', ''USR_CosEMail'', 9', 1
		/*REST*/ /*MIC: 10005, MICC: 10007*/ exec sp_Execute_Insert 'dbo', 007, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10007, 10005, ''USR_OplPrimeiroAcesso'', ''USR_OplPrimeiroAcesso'', 12', 1
		/*REST*/ /*MIC: 10005, MICC: 10008*/ exec sp_Execute_Insert 'dbo', 008, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10008, 10005, ''USR_CosSenha'', ''USR_CosSenha'', 9', 1
		/*REST*/ /*MIC: 10006, MICC: 10009*/ exec sp_Execute_Insert 'dbo', 009, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10009, 10006, ''BatidaData'', ''CBD_DtdBatidaData'', 10', 1
		/*REST*/ /*MIC: 10006, MICC: 10010*/ exec sp_Execute_Insert 'dbo', 010, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10010, 10006, ''BatidaHoraMinuto'', ''CBD_HrdBatidaHoraMinuto'', 11', 1
        /*REST*/ /*MIC: 10006, MICC: 10011*/ exec sp_Execute_Insert 'dbo', 011, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10011, 10006, ''DispositivoAcesso'', ''CBD_NuiDispositivoAcesso'', 9', 1
		/*REST*/ /*MIC: 10006, MICC: 10012*/ exec sp_Execute_Insert 'dbo', 012, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10012, 10006, ''Cdicontratado'', ''CBD_CosCrachaBase'', 9', 1
        /*REST*/ /*MIC: 10006, MICC: 10013*/ exec sp_Execute_Insert 'dbo', 013, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10013, 10006, ''latitude'', ''CBD_QtnLatitude'', 9', 1
		/*REST*/ /*MIC: 10006, MICC: 10014*/ exec sp_Execute_Insert 'dbo', 014, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10014, 10006, ''longitude'', ''CBD_QtnLongitude'', 9', 1
		/*REST*/ /*MIC: 10007, MICC: 10015*/ exec sp_Execute_Insert 'dbo', 015, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10015, 10007, ''D1sCargo'', ''CAR_D1sCargo'', 9', 1
		/*REST*/ /*MIC: 10007, MICC: 10016*/ exec sp_Execute_Insert 'dbo', 016, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10016, 10007, ''D1sCargoRes'', ''CAR_D1sCargoRes'', 9', 1
		/*REST*/ /*MIC: 10007, MICC: 10017*/ exec sp_Execute_Insert 'dbo', 017, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10017, 10007, ''anexo'', ''CampoVirtual_100505'', 17', 1
		/*REST*/ /*MIC: 10007, MICC: 10018*/ exec sp_Execute_Insert 'dbo', 018, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10018, 10007, ''CBO'', ''CAR_CdiCodBrasileiroOcupacao'', 9', 1 
		/*REST*/ /*MIC: 10008, MICC: 10019*/ exec sp_Execute_Insert 'dbo', 019, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Origem, BBP_DssCampo_Destino, BBP_CdiTipoCampo, BBP_OplConteudoFixo', '10019, 10008, ''cep'', ''consultaCEP;cep'', 9, 0', 1
		/*REST*/ /*MIC: 10009, MICC: 10020*/ exec sp_Execute_Insert 'dbo', 020, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10020, 10009, ''AGI_CdiAssuntoGeralItem'', ''AGI_CdiAssuntoGeralItem'', 3, 1003', 1
		/*REST*/ /*MIC: 10009, MICC: 10021*/ exec sp_Execute_Insert 'dbo', 021, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10021, 10009, ''AGI_ArbArquivoRelatorio'', ''AGI_ArbArquivoRelatorio'', 18, 1003', 1
		/*REST*/ /*MIC: 10010, MICC: 10022*/ exec sp_Execute_Insert 'dbo', 022, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10022, 10010, ''keyid'', ''keyid'', 9, 1004', 1
		/*REST*/ /*MIC: 10011, MICC: 10023*/ exec sp_Execute_Insert 'dbo', 023, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10023, 10011, ''keyid'', ''keyid'', 9, 1005', 1
		/*REST*/ /*MIC: 10011, MICC: 10024*/ exec sp_Execute_Insert 'dbo', 024, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10024, 10011, ''field1'', ''field1'', 9, 1006', 1
		/*REST*/ /*MIC: 10011, MICC: 10025*/ exec sp_Execute_Insert 'dbo', 025, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10025, 10011, ''field2'', ''field2'', 9, 1006', 1
		/*REST*/ /*MIC: 10012, MICC: 10026*/ exec sp_Execute_Insert 'dbo', 026, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10026, 10012, ''nome'',  ''nome'',  9, 1007', 1
		/*REST*/ /*MIC: 10012, MICC: 10027*/ exec sp_Execute_Insert 'dbo', 027, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10027, 10012, ''email'', ''email'', 9, 1007', 1
		/*REST*/ /*MIC: 10012, MICC: 10028*/ exec sp_Execute_Insert 'dbo', 028, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10028, 10012, ''id'', ''id'', 9, 1007', 1
		/*REST*/ /*MIC: 10015, MICC: 10029*/ exec sp_Execute_Insert 'dbo', 029, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10029, 10015, ''keyid'', ''keyid'', 9, 1008', 1
		/*REST*/ /*MIC: 10016, MICC: 10030*/ exec sp_Execute_Insert 'dbo', 030, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10030, 10016, ''keyid'', ''keyid'', 9, 1004', 1
		/*REST*/ /*MIC: 10018, MICC: 10031*/ exec sp_Execute_Insert 'dbo', 031, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_VrnConteudo_Numerico, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_CdiPais_Registro, BBP_OplConsiderarNulo, BBP_OplExecutarSql_Cliente, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_MsgErroDePara, BBP_D1bMensagemErroDePara, BBP_D2bMensagemErroDePara, BBP_D3bMensagemErroDePara', /*values*/ '10031, 10018, ''CBE_CdiConBatidaReal'', ''CBE_CdiConBatidaReal'', 3, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, null, null, 0, 0, 0, null, null, null', 1
		/*REST*/ /*MIC: 10018, MICC: 10032*/ exec sp_Execute_Insert 'dbo', 032, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_VrnConteudo_Numerico, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_CdiPais_Registro, BBP_OplConsiderarNulo, BBP_OplExecutarSql_Cliente, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_MsgErroDePara, BBP_D1bMensagemErroDePara, BBP_D2bMensagemErroDePara, BBP_D3bMensagemErroDePara', /*values*/ '10032, 10018, ''CBE_CdiOcorrenciaMarcacao'', ''CBE_CdiOcorrenciaMarcacao'', 3, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, null, null, 0, 0, 0, null, null, null', 1
		/*SOAP*/ /*MIC: 10023, MICC: 10033*/ exec sp_Execute_Insert 'dbo', 033, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_VrnConteudo_Numerico, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_CdiPais_Registro, BBP_OplConsiderarNulo, BBP_OplExecutarSql_Cliente, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_MsgErroDePara, BBP_D1bMensagemErroDePara, BBP_D2bMensagemErroDePara, BBP_D3bMensagemErroDePara', /*values*/ '10033, 10023, ''login;credential;companyId'', null, 9, 1, 0, null, ''tecnologiaT1'', 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, null, null, 0, 0, 0, null, null, null', 1
		/*SOAP*/ /*MIC: 10023, MICC: 10034*/ exec sp_Execute_Insert 'dbo', 034, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_VrnConteudo_Numerico, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_CdiPais_Registro, BBP_OplConsiderarNulo, BBP_OplExecutarSql_Cliente, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_MsgErroDePara, BBP_D1bMensagemErroDePara, BBP_D2bMensagemErroDePara, BBP_D3bMensagemErroDePara', /*values*/ '10034, 10023, ''login;credential;username'', null, 9, 1, 0, null, ''sfapi'', 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, null, null, 0, 0, 0, null, null, null', 1
		/*SOAP*/ /*MIC: 10023, MICC: 10035*/ exec sp_Execute_Insert 'dbo', 035, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_VrnConteudo_Numerico, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_CdiPais_Registro, BBP_OplConsiderarNulo, BBP_OplExecutarSql_Cliente, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_MsgErroDePara, BBP_D1bMensagemErroDePara, BBP_D2bMensagemErroDePara, BBP_D3bMensagemErroDePara', /*values*/ '10035, 10023, ''login;credential;password'', null, 9, 1, 0, null, ''BanTec2020@#$'', 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, null, null, 0, 0, 0, null, null, null', 1
		/*REST*/ /*MIC: 10024, MICC: 10036*/ exec sp_Execute_Insert 'dbo', 036, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10036, 10024, ''username'', 0, ''username'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 0, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ /*MIC: 10024, MICC: 10038*/ exec sp_Execute_Insert 'dbo', 038, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10038, 10024, ''documentNumber_billingFrom'', 0, ''documentNumber_billingFrom'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ /*MIC: 10024, MICC: 10039*/ exec sp_Execute_Insert 'dbo', 039, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10039, 10024, ''documentType_billingFrom'', 0, ''documentType_billingFrom'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ /*MIC: 10024, MICC: 10040*/ exec sp_Execute_Insert 'dbo', 040, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10040, 10024, ''documentNumber_billingTo'', 0, ''documentNumber_billingTo'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ /*MIC: 10024, MICC: 10041*/ exec sp_Execute_Insert 'dbo', 041, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10041, 10024, ''documentType_billingTo'', 0, ''documentType_billingTo'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ /*MIC: 10024, MICC: 10042*/ exec sp_Execute_Insert 'dbo', 042, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10042, 10024, ''amount'', 0, ''amount'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ /*MIC: 10024, MICC: 10043*/ exec sp_Execute_Insert 'dbo', 043, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10043, 10024, ''date_invoice'', 0, ''date_invoice'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ /*MIC: 10024, MICC: 10044*/ exec sp_Execute_Insert 'dbo', 044, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10044, 10024, ''description_invoice'', 0, ''description_invoice'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ /*MIC: 10024, MICC: 10045*/ exec sp_Execute_Insert 'dbo', 045, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10045, 10024, ''invoiceId'', 0, ''invoiceId'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ /*MIC: 10024, MICC: 10046*/ exec sp_Execute_Insert 'dbo', 046, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10046, 10024, ''paymentCreationDate'', 0, ''paymentCreationDate'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ /*MIC: 10024, MICC: 10047*/ exec sp_Execute_Insert 'dbo', 047, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10047, 10024, ''paymentDate'', 0, ''paymentDate'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ /*MIC: 10024, MICC: 10048*/ exec sp_Execute_Insert 'dbo', 048, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10048, 10024, ''accountingKey'', 0, ''accountingKey'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ /*MIC: 10024, MICC: 10049*/ exec sp_Execute_Insert 'dbo', 049, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10049, 10024, ''fixedAssetAccount'', 0, ''fixedAssetAccount'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ /*MIC: 10024, MICC: 10050*/ exec sp_Execute_Insert 'dbo', 050, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10050, 10024, ''fixedAssetBookType'', 0, ''fixedAssetBookType'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ /*MIC: 10024, MICC: 10051*/ exec sp_Execute_Insert 'dbo', 051, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10051, 10024, ''fixedAssetCategory'', 0, ''fixedAssetCategory'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ /*MIC: 10024, MICC: 10052*/ exec sp_Execute_Insert 'dbo', 052, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10052, 10024, ''fixedAssetType'', 0, ''fixedAssetType'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ /*MIC: 10024, MICC: 10053*/ exec sp_Execute_Insert 'dbo', 053, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10053, 10024, ''itemAmount'', 0, ''itemAmount'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ /*MIC: 10024, MICC: 10054*/ exec sp_Execute_Insert 'dbo', 054, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10054, 10024, ''itemDescription'', 0, ''itemDescription'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ /*MIC: 10024, MICC: 10055*/ exec sp_Execute_Insert 'dbo', 055, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10055, 10024, ''registrationRequestNumber'', 0, ''registrationRequestNumber'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
	    /*REST*/ /*MIC: 10024, MICC: 10056*/ exec sp_Execute_Insert 'dbo', 056, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10056, 10024, ''sourceSystem'', 0, ''sourceSystem'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ /*MIC: 10024, MICC: 10057*/ exec sp_Execute_Insert 'dbo', 057, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10057, 10024, ''users'', 0, ''users'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ /*MIC: 10026, MICC: 10058*/ exec sp_Execute_Insert 'dbo', 058, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', /*values*/ '10058, 10026, ''NUMEROINFORMADO'', ''CEX_CosCrachaBase'', 9', 1
		/*REST*/ /*MIC: 10026, MICC: 10059*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 059, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiComandoSQL, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', /*values*/ 10059, 0, @SQL_CdiComandoSQL, 14, '10026, null, ''CEX_DtdValidadeInicio'', 9', 1
        /*REST*/ /*MIC: 10026, MICC: 10060*/ exec sp_Execute_Insert 'dbo', 060, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiComandoSQL', /*values*/ '10060, 10026, ''DATA_FIM_VALIDADE'', ''CEX_DtdValidadeFim'', 16, 0', 1
		/*REST*/ /*MIC: 10026, MICC: 10061*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 061, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiComandoSQL, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', /*values*/ 10061, 0, @SQL_CdiComandoSQL, 15, '10026, null, ''CEX_D1sCrachaExtra'', 9', 1
		/*REST*/ /*MIC: 10026, MICC: 10062*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 062, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiComandoSQL, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', /*values*/ 10062, 0, @SQL_CdiComandoSQL, 15, '10026, null, ''CEX_CdiCrachaExtra'', 3', 1
		/*REST*/ /*MIC: 10028, MICC: 10063*/ exec sp_Execute_Insert 'dbo', 062, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10063, 10028, ''keyid'', ''keyid'', 9, 1004', 1
		/*REST*/ /*MIC: 10030, MICC: 10064*/ exec sp_Execute_Insert 'dbo', 063, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10064, 10030, ''D1sCargo'', ''CAR_D1sCargo'', 9', 1
		/*REST*/ /*MIC: 10030, MICC: 10065*/ exec sp_Execute_Insert 'dbo', 064, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10065, 10030, ''D1sCargoRes'', ''CAR_D1sCargoRes'', 9', 1
		/*REST*/ /*MIC: 10030, MICC: 10066*/ exec sp_Execute_Insert 'dbo', 065, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10066, 10030, ''CBO'', ''CAR_CdiCodBrasileiroOcupacao'', 9', 1 
		/*SOAP*/ /*MIC: 10031, MICC: 10067*/ exec sp_Execute_Insert 'dbo', 066, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10067, 10031, ''DEP_CdiContratado'', ''DEP_CdiContratado'', 8', 1
		/*SOAP*/ /*MIC: 10031, MICC: 10068*/ exec sp_Execute_Insert 'dbo', 067, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10068, 10031, ''DEP_CdiLigacaoPessoa'', ''DEP_CdiLigacaoPessoa'', 8', 1
		/*SOAP*/ /*MIC: 10031, MICC: 10069*/ exec sp_Execute_Insert 'dbo', 068, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10069, 10031, ''DEP_CdiSexo'', ''DEP_CdiSexo'', 8', 1
		/*SOAP*/ /*MIC: 10031, MICC: 10070*/ exec sp_Execute_Insert 'dbo', 069, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10070, 10031, ''DEP_CdiEstadoCivil'', ''DEP_CdiEstadoCivil'', 8', 1
		/*SOAP*/ /*MIC: 10031, MICC: 10071*/ exec sp_Execute_Insert 'dbo', 070, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10071, 10031, ''DEP_CdiEstado_OrgaoRg'', ''DEP_CdiEstado_OrgaoRg'', 8', 1
		/*SOAP*/ /*MIC: 10031, MICC: 10072*/ exec sp_Execute_Insert 'dbo', 071, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10072, 10031, ''DEP_CdiSituacaoDependente'', ''DEP_CdiSituacaoDependente'', 8', 1
		/*SOAP*/ /*MIC: 10031, MICC: 10073*/ exec sp_Execute_Insert 'dbo', 072, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10073, 10031, ''DEP_DssNomeCompleto'', ''DEP_DssNomeCompleto'', 9', 1
		/*SOAP*/ /*MIC: 10031, MICC: 10074*/ exec sp_Execute_Insert 'dbo', 073, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10074, 10031, ''DEP_DtdApresCertNascimento'', ''DEP_DtdApresCertNascimento'', 10', 1
        /*SOAP*/ /*MIC: 10031, MICC: 10075*/ exec sp_Execute_Insert 'dbo', 074, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10075, 10031, ''DEP_OplImpRendaDependente'', ''DEP_OplImpRendaDependente'', 8', 1
		/*SOAP*/ /*MIC: 10031, MICC: 10076*/ exec sp_Execute_Insert 'dbo', 075, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10076, 10031, ''DEP_DssNome'', ''DEP_DssNome'', 9', 1
		/*SOAP*/ /*MIC: 10031, MICC: 10077*/ exec sp_Execute_Insert 'dbo', 076, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10077, 10031, ''DEP_CosNumeroRg'', ''DEP_CosNumeroRg'', 9', 1
		/*SOAP*/ /*MIC: 10031, MICC: 10078*/ exec sp_Execute_Insert 'dbo', 077, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10078, 10031, ''DEP_CosOrgaoRg'', ''DEP_CosOrgaoRg'', 9', 1
		/*SOAP*/ /*MIC: 10031, MICC: 10079*/ exec sp_Execute_Insert 'dbo', 078, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10079, 10031, ''DEP_DtdEmissaoRg'', ''DEP_DtdEmissaoRg'', 10', 1
		/*SOAP*/ /*MIC: 10031, MICC: 10080*/ exec sp_Execute_Insert 'dbo', 079, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10080, 10031, ''DEP_NusCICNumero'', ''DEP_NusCICNumero'', 9', 1
		/*SOAP*/ /*MIC: 10031, MICC: 10081*/ exec sp_Execute_Insert 'dbo', 080, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10081, 10031, ''DEP_DssNascimentoLocal'', ''DEP_DssNascimentoLocal'', 9', 1
		/*SOAP*/ /*MIC: 10031, MICC: 10082*/ exec sp_Execute_Insert 'dbo', 081, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10082, 10031, ''DEP_DtdNascimentoData'', ''DEP_DtdNascimentoData'', 10', 1
        /*SOAP*/ /*MIC: 10031, MICC: 10083*/ exec sp_Execute_Insert 'dbo', 082, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10083, 10031, ''DEP_DssNomePai'', ''DEP_DssNomePai'', 9', 1
		/*SOAP*/ /*MIC: 10031, MICC: 10084*/ exec sp_Execute_Insert 'dbo', 083, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10084, 10031, ''DEP_DssNomeMae'', ''DEP_DssNomeMae'', 9', 1
		/*SOAP*/ /*MIC: 10031, MICC: 10085*/ exec sp_Execute_Insert 'dbo', 084, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10085, 10031, ''DEP_CosCartaoNacionalSaude'', ''DEP_CosCartaoNacionalSaude'', 9', 1
		/*SOAP*/ /*MIC: 10031, MICC: 10086*/ exec sp_Execute_Insert 'dbo', 085, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10086, 10031, ''DPF_CdiCodigoFlexivelDep01'', ''DPF_CdiCodigoFlexivelDep01'', 10', 1
		/*REST*/ /*MIC: 10032, MICC: 10087*/ exec sp_Execute_Insert 'dbo', 086, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10087, 10032, ''D1sCargo'', ''CAR_D1sCargo'', 9', 1
		/*REST*/ /*MIC: 10032, MICC: 10088*/ exec sp_Execute_Insert 'dbo', 087, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10088, 10032, ''D1sCargoRes'', ''CAR_D1sCargoRes'', 9', 1
		/*REST*/ /*MIC: 10032, MICC: 10089*/ exec sp_Execute_Insert 'dbo', 088, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10089, 10032, ''CBO'', ''CAR_CdiCodBrasileiroOcupacao'', 9', 1
		/*SOAP*/ /*MIC: 10033, MICC: 10091*/ exec sp_Execute_Insert 'dbo', 089, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10091, 10033, ''vagaid'', ''VAG_CdiVaga'', 9', 1 
		/*SOAP*/ /*MIC: 10033, MICC: 10092*/ exec sp_Execute_Insert 'dbo', 090, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10092, 10033, ''CON_CdiContratado_SupervEstag'',''CON_CdiContratado_SupervEstag'',  9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10093*/ exec sp_Execute_Insert 'dbo', 091, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10093, 10033,''CON_CdiSexo'',''CON_CdiSexo'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10094*/ exec sp_Execute_Insert 'dbo', 092, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10094, 10033,''CON_CdiEstadoCivil'',''CON_CdiEstadoCivil'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10095*/ exec sp_Execute_Insert 'dbo', 093, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10095, 10033,''CON_CdiTipoEndereco'',''CON_CdiTipoEndereco'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10096*/ exec sp_Execute_Insert 'dbo', 094, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10096, 10033,''CON_CdiEstado_Resid'',''CON_CdiEstado_Resid'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10097*/ exec sp_Execute_Insert 'dbo', 095, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10097, 10033,''CON_CdiEstado_TitEleitor'',''CON_CdiEstado_TitEleitor'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10098*/ exec sp_Execute_Insert 'dbo', 096, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10098, 10033,''CON_CdiEstado_OrgaoRg'',''CON_CdiEstado_OrgaoRg'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10099*/ exec sp_Execute_Insert 'dbo', 097, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10099, 10033,''CON_CdiDocumentoClasse'',''CON_CdiDocumentoClasse'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10100*/ exec sp_Execute_Insert 'dbo', 098, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10100, 10033,''CON_CdiCorPele'',''CON_CdiCorPele'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10101*/ exec sp_Execute_Insert 'dbo', 099, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10101, 10033,''CON_CdiNacionalidade'',''CON_CdiNacionalidade'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10102*/ exec sp_Execute_Insert 'dbo', 100, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10102, 10033,''CON_CdiEstado_Naturalidade'',''CON_CdiEstado_Naturalidade'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10103*/ exec sp_Execute_Insert 'dbo', 101, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10103, 10033,''CON_CdiGrauInstrucao'',''CON_CdiGrauInstrucao'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10104*/ exec sp_Execute_Insert 'dbo', 102, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10104, 10033,''CON_CdiAgenciaBanco_Pis'',''CON_CdiAgenciaBanco_Pis'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10105*/ exec sp_Execute_Insert 'dbo', 103, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10105, 10033,''CON_CdiPrazoContratado'',''CON_CdiPrazoContratado'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10106*/ exec sp_Execute_Insert 'dbo', 104, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10106, 10033,''CON_CdiTextoAdmissaoGrupo'',''CON_CdiTextoAdmissaoGrupo'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10107*/ exec sp_Execute_Insert 'dbo', 105, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10107, 10033,''CON_CdiEstado_CarProf'',''CON_CdiEstado_CarProf'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10108*/ exec sp_Execute_Insert 'dbo', 107, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10108, 10033,''CON_CdiTipoEmprego'',''CON_CdiTipoEmprego'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10109*/ exec sp_Execute_Insert 'dbo', 108, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10109, 10033,''CON_CdiTipoDeficiencia'',''CON_CdiTipoDeficiencia'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10110*/ exec sp_Execute_Insert 'dbo', 109, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10110, 10033,''CON_CdiAgenciaBanco_Credito'',''CON_CdiAgenciaBanco_Credito'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10111*/ exec sp_Execute_Insert 'dbo', 110, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10111, 10033,''CON_CdiTipoContaPagamento'',''CON_CdiTipoContaPagamento'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10112*/ exec sp_Execute_Insert 'dbo', 111, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10112, 10033,''CON_CdiMunicipio_Nascimento'',''CON_CdiMunicipio_Nascimento'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10113*/ exec sp_Execute_Insert 'dbo', 112, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10113, 10033,''CON_CdiPais_Nacionalidade'',''CON_CdiPais_Nacionalidade'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10114*/ exec sp_Execute_Insert 'dbo', 113, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10114, 10033,''CON_CdiPais_Nascimento'',''CON_CdiPais_Nascimento'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10115*/ exec sp_Execute_Insert 'dbo', 114, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10115, 10033,''COE_CdiNivelEstagio'',''COE_CdiNivelEstagio'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10116*/ exec sp_Execute_Insert 'dbo', 115, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10116, 10033,''COE_NusInscricaoEmpregOrigem'',''COE_NusInscricaoEmpregOrigem'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10117*/ exec sp_Execute_Insert 'dbo', 116, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10117, 10033,''COE_CdiEstado_DocumentoClasse'',''COE_CdiEstado_DocumentoClasse'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10118*/ exec sp_Execute_Insert 'dbo', 117, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10118, 10033,''COE_CosTelefoneCelularDDD'',''COE_CosTelefoneCelularDDD'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10119*/ exec sp_Execute_Insert 'dbo', 118, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10119, 10033,''COE_NusTelefoneCelular'',''COE_NusTelefoneCelular'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10120*/ exec sp_Execute_Insert 'dbo', 119, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10120, 10033,''COE_CosIdentidadeEstrangeiro'',''COE_CosIdentidadeEstrangeiro'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10121*/ exec sp_Execute_Insert 'dbo', 120, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10121, 10033,''COE_CosOrgaoIdentEstrangeiro'',''COE_CosOrgaoIdentEstrangeiro'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10122*/ exec sp_Execute_Insert 'dbo', 121, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10122, 10033,''COE_DtdExpedIdentEstrangeiro'',''COE_DtdExpedIdentEstrangeiro'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10123*/ exec sp_Execute_Insert 'dbo', 122, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10123, 10033,''COE_CosOrgaoDocumentoClasse'',''COE_CosOrgaoDocumentoClasse'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10124*/ exec sp_Execute_Insert 'dbo', 123, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10124, 10033,''COE_DtdExpedDocumentoClasse'',''COE_DtdExpedDocumentoClasse'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10125*/ exec sp_Execute_Insert 'dbo', 124, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10125, 10033,''COE_DtdExpedicaoCNH'',''COE_DtdExpedicaoCNH'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10126*/ exec sp_Execute_Insert 'dbo', 125, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10126, 10033,''COE_CosOrgaoExpedidorCNH'',''COE_CosOrgaoExpedidorCNH'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10127*/ exec sp_Execute_Insert 'dbo', 126, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10127, 10033,''COE_CdiPais_Residencia'',''COE_CdiPais_Residencia'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10128*/ exec sp_Execute_Insert 'dbo', 127, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10128, 10033,''COE_CdiStatusEnvioCAGED'',''COE_CdiStatusEnvioCAGED'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10129*/ exec sp_Execute_Insert 'dbo', 128, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10129, 10033,''COE_CdiEstado_CNH'',''COE_CdiEstado_CNH'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10130*/ exec sp_Execute_Insert 'dbo', 129, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10130, 10033,''COE_DtdPrimeiraHabilitacao'',''COE_DtdPrimeiraHabilitacao'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10131*/ exec sp_Execute_Insert 'dbo', 130, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10131, 10033,''COE_CdiOpcao_DeficienciaFisica'',''COE_CdiOpcao_DeficienciaFisica'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10132*/ exec sp_Execute_Insert 'dbo', 131, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10132, 10033,''COE_CdiOpcao_DeficienciaVisual'',''COE_CdiOpcao_DeficienciaVisual'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10133*/ exec sp_Execute_Insert 'dbo', 132, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10133, 10033,''COE_CdiOpcao_DeficiencAuditiva'',''COE_CdiOpcao_DeficiencAuditiva'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10134*/ exec sp_Execute_Insert 'dbo', 133, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10134, 10033,''COE_CdiOpcao_DeficiencMental'',''COE_CdiOpcao_DeficiencMental'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10135*/ exec sp_Execute_Insert 'dbo', 134, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10135, 10033,''COE_CdiOpcao_DeficIntelectual'',''COE_CdiOpcao_DeficIntelectual'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10136*/ exec sp_Execute_Insert 'dbo', 135, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10136, 10033,''COE_CdiOpcao_EReabilitado'',''COE_CdiOpcao_EReabilitado'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10137*/ exec sp_Execute_Insert 'dbo', 136, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10137, 10033,''COF_CdiCodigoFlexivelCon22'',''COF_CdiCodigoFlexivelCon22'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10138*/ exec sp_Execute_Insert 'dbo', 137, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10138, 10033,''COF_DssConteudoFlexivel_04'',''COF_DssConteudoFlexivel_04'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10139*/ exec sp_Execute_Insert 'dbo', 138, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10139, 10033,''COF_DssConteudoFlexivel_09'',''COF_DssConteudoFlexivel_09'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10140*/ exec sp_Execute_Insert 'dbo', 139, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10140, 10033,''COF_DssConteudoFlexivel_10'',''COF_DssConteudoFlexivel_10'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10141*/ exec sp_Execute_Insert 'dbo', 140, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10141, 10033,''COF_DtdConteudoData_22'',''COF_DtdConteudoData_22'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10142*/ exec sp_Execute_Insert 'dbo', 141, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10142, 10033,''COF_DtdConteudoData_23'',''COF_DtdConteudoData_23'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10143*/ exec sp_Execute_Insert 'dbo', 142, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10143, 10033,''COF_DtdConteudoData_24'',''COF_DtdConteudoData_24'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10144*/ exec sp_Execute_Insert 'dbo', 143, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10144, 10033,''COF_DtdConteudoData_25'',''COF_DtdConteudoData_25'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10145*/ exec sp_Execute_Insert 'dbo', 144, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10145, 10033,''COF_CdiCodigoFlexivelCon03'',''COF_CdiCodigoFlexivelCon03'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10146*/ exec sp_Execute_Insert 'dbo', 145, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10146, 10033,''COF_DtdConteudoData_01'',''COF_DtdConteudoData_01'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10147*/ exec sp_Execute_Insert 'dbo', 146, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10147, 10033,''CON_DssNome'',''CON_DssNome'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10148*/ exec sp_Execute_Insert 'dbo', 147, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10148, 10033,''CON_DssNomePai'',''CON_DssNomePai'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10149*/ exec sp_Execute_Insert 'dbo', 148, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10149, 10033,''CON_DssNomeMae'',''CON_DssNomeMae'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10150*/ exec sp_Execute_Insert 'dbo', 149, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10150, 10033,''CON_DssApelido'',''CON_DssApelido'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10151*/ exec sp_Execute_Insert 'dbo', 150, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10151, 10033,''CON_DssEnderecoBase'',''CON_DssEnderecoBase'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10152*/ exec sp_Execute_Insert 'dbo', 151, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10152, 10033,''CON_DssEnderecoNumero'',''CON_DssEnderecoNumero'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10153*/ exec sp_Execute_Insert 'dbo', 152, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10153, 10033,''CON_DssEnderecoComplto'',''CON_DssEnderecoComplto'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10154*/ exec sp_Execute_Insert 'dbo', 153, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10154, 10033,''CON_DssBairro'',''CON_DssBairro'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10155*/ exec sp_Execute_Insert 'dbo', 154, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10155, 10033,''CON_DssMunicipio'',''CON_DssMunicipio'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10156*/ exec sp_Execute_Insert 'dbo', 155, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10156, 10033,''CON_NusCep'',''CON_NusCep'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10157*/ exec sp_Execute_Insert 'dbo', 156, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10157, 10033,''CON_CosTelefoneDDD_Resid'',''CON_CosTelefoneDDD_Resid'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10158*/ exec sp_Execute_Insert 'dbo', 157, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10158, 10033,''CON_NusTelefoneNumero_Resid'',''CON_NusTelefoneNumero_Resid'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10159*/ exec sp_Execute_Insert 'dbo', 158, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10159, 10033,''CON_NusNumeroTitEleitor'',''CON_NusNumeroTitEleitor'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10160*/ exec sp_Execute_Insert 'dbo', 159, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10160, 10033,''CON_NusZonaTitEleitor'',''CON_NusZonaTitEleitor'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10161*/ exec sp_Execute_Insert 'dbo', 160, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10161, 10033,''CON_NusSecaoTitEleitor'',''CON_NusSecaoTitEleitor'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10162*/ exec sp_Execute_Insert 'dbo', 161, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10162, 10033,''CON_CosNumeroRg'',''CON_CosNumeroRg'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10163*/ exec sp_Execute_Insert 'dbo', 162, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10163, 10033,''CON_CosOrgaoRg'',''CON_CosOrgaoRg'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10164*/ exec sp_Execute_Insert 'dbo', 163, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10164, 10033,''CON_DtdEmissaoRg'',''CON_DtdEmissaoRg'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10165*/ exec sp_Execute_Insert 'dbo', 164, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10165, 10033,''CON_DtdValidadeRg'',''CON_DtdValidadeRg'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10166*/ exec sp_Execute_Insert 'dbo', 165, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10166, 10033,''CON_CosReservista'',''CON_CosReservista'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10167*/ exec sp_Execute_Insert 'dbo', 166, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10167, 10033,''CON_CosCnhTipo1'',''CON_CosCnhTipo1'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10168*/ exec sp_Execute_Insert 'dbo', 167, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10168, 10033,''CON_NusCnhNumero'',''CON_NusCnhNumero'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10169*/ exec sp_Execute_Insert 'dbo', 168, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10169, 10033,''CON_DtdCnhVencimento'',''CON_DtdCnhVencimento'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10170*/ exec sp_Execute_Insert 'dbo', 169, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10170, 10033,''CON_CosDocClasseNumero'',''CON_CosDocClasseNumero'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10171*/ exec sp_Execute_Insert 'dbo', 170, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10171, 10033,''CON_NusCICNumero'',''CON_NusCICNumero'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10172*/ exec sp_Execute_Insert 'dbo', 171, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10172, 10033,''CON_DtdChegadaPais'',''CON_DtdChegadaPais'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10173*/ exec sp_Execute_Insert 'dbo', 172, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10173, 10033,''CON_CosTipoVisto'',''CON_CosTipoVisto'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10174*/ exec sp_Execute_Insert 'dbo', 173, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10174, 10033,''CON_DssNascimentoLocal'',''CON_DssNascimentoLocal'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10175*/ exec sp_Execute_Insert 'dbo', 174, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10175, 10033,''CON_DtdNascimentoData'',''CON_DtdNascimentoData'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10176*/ exec sp_Execute_Insert 'dbo', 175, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10176, 10033,''CON_NusNumeroPis'',''CON_NusNumeroPis'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10177*/ exec sp_Execute_Insert 'dbo', 176, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10177, 10033,''CON_DssNomeCompleto'',''CON_DssNomeCompleto'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10178*/ exec sp_Execute_Insert 'dbo', 177, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10178, 10033,''CON_CosEMailPessoal'',''CON_CosEMailPessoal'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10179*/ exec sp_Execute_Insert 'dbo', 178, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10179, 10033,''CON_DtdAdmissao'',''CON_DtdAdmissao'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10180*/ exec sp_Execute_Insert 'dbo', 179, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10180, 10033,''CON_NusNumeroCarProf'',''CON_NusNumeroCarProf'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10181*/ exec sp_Execute_Insert 'dbo', 180, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10181, 10033,''CON_NusSerieCarProf'',''CON_NusSerieCarProf'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10182*/ exec sp_Execute_Insert 'dbo', 181, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10182, 10033,''CON_DtdEmissaoCarProf'',''CON_DtdEmissaoCarProf'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10183*/ exec sp_Execute_Insert 'dbo', 182, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10183, 10033,''CON_DtdValidadeCarProf'',''CON_DtdValidadeCarProf'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10184*/ exec sp_Execute_Insert 'dbo', 183, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10184, 10033,''CON_PcnInsalubridade'',''CON_PcnInsalubridade'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10185*/ exec sp_Execute_Insert 'dbo', 184, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10185, 10033,''CON_PcnPericulosidade'',''CON_PcnPericulosidade'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10186*/ exec sp_Execute_Insert 'dbo', 185, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10186, 10033,''CON_CosPagamentoConta'',''CON_CosPagamentoConta'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10187*/ exec sp_Execute_Insert 'dbo', 186, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10187, 10033,''CON_CosPagamentoContaDigito'',''CON_CosPagamentoContaDigito'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10188*/ exec sp_Execute_Insert 'dbo', 187, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10188, 10033,''COF_CdiCodigoFlexivelCon04'',''COF_CdiCodigoFlexivelCon04'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10189*/ exec sp_Execute_Insert 'dbo', 188, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10189, 10033,''CON_CosCartaoNacionalSaude'',''CON_CosCartaoNacionalSaude'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10190*/ exec sp_Execute_Insert 'dbo', 189, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10190, 10033,''COE_CdiJuridicaLocal_InstEns'',''COE_CdiJuridicaLocal_InstEns'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10191*/ exec sp_Execute_Insert 'dbo', 190, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10191, 10033,''COE_CdiJuridicaLocal_AgIntegr'',''COE_CdiJuridicaLocal_AgIntegr'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10192*/ exec sp_Execute_Insert 'dbo', 191, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10192, 10033,''COE_OplCondicaoEspDeficiente'',''COE_OplCondicaoEspDeficiente'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10193*/ exec sp_Execute_Insert 'dbo', 192, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10193, 10033,''COE_CdiOpcao_PreencheCotaDefic'',''COE_CdiOpcao_PreencheCotaDefic'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10194*/ exec sp_Execute_Insert 'dbo', 193, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10194, 10033,''CON_DtdAdmissaoGrupo'',''CON_DtdAdmissaoGrupo'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10195*/ exec sp_Execute_Insert 'dbo', 194, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10195, 10033,''CON_DtdOpcaoFGTS'',''CON_DtdOpcaoFGTS'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10196*/ exec sp_Execute_Insert 'dbo', 195, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10196, 10033,'''',''CON_CdiFolha'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10197*/ exec sp_Execute_Insert 'dbo', 196, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10197, 10033,'''',''CON_CdiMarcadorPonto'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10198*/ exec sp_Execute_Insert 'dbo', 197, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10198, 10033,'''',''CON_CdiCentroCusto'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10199*/ exec sp_Execute_Insert 'dbo', 198, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10199, 10033,'''',''CON_CdiHorario'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10200*/ exec sp_Execute_Insert 'dbo', 199, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10200, 10033,'''',''CON_CdiSindicato'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10201*/ exec sp_Execute_Insert 'dbo', 200, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10201, 10033,'''',''CON_CdiCategoria'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10202*/ exec sp_Execute_Insert 'dbo', 201, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10202, 10033,'''',''CON_CdiVinculo'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10203*/ exec sp_Execute_Insert 'dbo', 202, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10203, 10033,'''',''CON_CdiQuantidadeHoraMes'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10204*/ exec sp_Execute_Insert 'dbo', 203, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10204, 10033,'''',''CON_CdiCargo'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10205*/ exec sp_Execute_Insert 'dbo', 204, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10205, 10033,'''',''CON_CdiTabelaSalarial'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10206*/ exec sp_Execute_Insert 'dbo', 205, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10206, 10033,'''',''CON_CdiTabelaSalarialColuna'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10207*/ exec sp_Execute_Insert 'dbo', 206, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10207, 10033,'''',''CON_VlnSalario'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10208*/ exec sp_Execute_Insert 'dbo', 207, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10208, 10033,'''',''CON_CdiAreaAtuacao'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10209*/ exec sp_Execute_Insert 'dbo', 208, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10209, 10033,'''',''COF_DssConteudoFlexivel_05'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10210*/ exec sp_Execute_Insert 'dbo', 209, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10210, 10033,'''',''COF_DssConteudoFlexivel_17'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10211*/ exec sp_Execute_Insert 'dbo', 210, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10211, 10033,'''',''COF_CdiCodigoFlexivelCon02'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10212*/ exec sp_Execute_Insert 'dbo', 211, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10212, 10033,'''',''CON_NuiContratado_Superior'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10213*/ exec sp_Execute_Insert 'dbo', 212, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10213, 10033,'''',''CON_NuiNivelHierarquia'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10214*/ exec sp_Execute_Insert 'dbo', 213, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10214, 10033,''CON_CdiMunicipio_Resid'',''CON_CdiMunicipio_Resid'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10215*/ exec sp_Execute_Insert 'dbo', 214, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10215, 10033,'''',''COE_CdiModoAdmissaoeSocial'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10216*/ exec sp_Execute_Insert 'dbo', 215, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10216, 10033,'''',''COE_CdiTipoAdmissaoeSocial'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10217*/ exec sp_Execute_Insert 'dbo', 216, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10217, 10033,''CON_CdiHorario'',''CON_CdiHorario'', 9', 1
        /*SOAP*/ /*MIC: 10033, MICC: 10218*/ exec sp_Execute_Insert 'dbo', 217, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10218, 10033,''divisao'',''COF_CdiCodigoFlexivelCon37'', 9', 1
        /*SOAP*/ /*MIC: 10034, MICC: 10219*/ exec sp_Execute_Insert 'dbo', 218, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10219, 10034,''colaboradorid'',''TRS_CdiContratado_Transf'', 3', 1
		/*SOAP*/ /*MIC: 10034, MICC: 10220*/ exec sp_Execute_Insert 'dbo', 219, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10220, 10034,''datavigencia'',''TRS_DtdTranferencia'', 10', 1
		/*SOAP*/ /*MIC: 10034, MICC: 10221*/ exec sp_Execute_Insert 'dbo', 220, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10221, 10034,''motivo'',''TRS_CdiMotivoAltGrade'', 3', 1
		/*SOAP*/ /*MIC: 10034, MICC: 10222*/ exec sp_Execute_Insert 'dbo', 221, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10222, 10034,''superiroid'',''TRS_NuiContratado_Superior'', 3', 1
		/*SOAP*/ /*MIC: 10034, MICC: 10223*/ exec sp_Execute_Insert 'dbo', 222, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10223, 10034,''areaatuacaoid'',''TRS_CdiAreaAtuacao'', 3', 1
		/*SOAP*/ /*MIC: 10034, MICC: 10224*/ exec sp_Execute_Insert 'dbo', 223, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10224, 10034,''localid'',''TRS_CdiFolha_Destino'', 3', 1
		/*SOAP*/ /*MIC: 10034, MICC: 10225*/ exec sp_Execute_Insert 'dbo', 224, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10225, 10034,''centroCustoId'',''TRS_CdiCentroCusto'', 3', 1
		/*SOAP*/ /*MIC: 10034, MICC: 10226*/ exec sp_Execute_Insert 'dbo', 225, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10226, 10034,''tranferirEquipe'',''TRS_CdiOpcao_TranfEquipe'', 3', 1
		/*SOAP*/ /*MIC: 10034, MICC: 10227*/ exec sp_Execute_Insert 'dbo', 226, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10227, 10034,''gestorSubstituto'',''TRS_NuiContratado_Substituto'', 3', 1
		/*SOAP*/ /*MIC: 10034, MICC: 10228*/ exec sp_Execute_Insert 'dbo', 227, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10228, 10034,''vagaid'',''TRS_CdiVaga_Destino'', 3', 1
		/*SOAP*/ /*MIC: 10034, MICC: 10229*/ exec sp_Execute_Insert 'dbo', 228, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10229, 10034,''motivotransf'',''TRS_CdiMotivoTransferencia'', 3', 1
		/*SOAP*/ /*MIC: 10034, MICC: 10230*/ exec sp_Execute_Insert 'dbo', 229, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10230, 10034,''cargoid'',''TRS_CdiCargo'', 3', 1
		/*SOAP*/ /*MIC: 10034, MICC: 10231*/ exec sp_Execute_Insert 'dbo', 230, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10231, 10034,''nivelhierarquia'',''TRS_NuiNivelHierarquia'', 3', 1
		/*SOAP*/ /*MIC: 10034, MICC: 10232*/ exec sp_Execute_Insert 'dbo', 231, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10232, 10034,''TRS_VlnSalario'',''TRS_VlnSalario'', 8', 1
		/*SOAP*/ /*MIC: 10034, MICC: 10233*/ exec sp_Execute_Insert 'dbo', 232, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10233, 10034,''marcadorPontoId'',''TRS_CdiMarcadorPonto'', 8', 1
		/*MONI*/ /*MIC: 10035, MICC: 10234*/ exec sp_Execute_Insert 'dbo', 233, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10234, 10035,''id_cargo'',''CAR_CdiCargo'', 3', 1
		/*MONI*/ /*MIC: 10035, MICC: 10235*/ exec sp_Execute_Insert 'dbo', 234, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10235, 10035,''desc_cargo'',''CAR_Ds1Cargo'', 9', 1
		/*MONI*/ /*MIC: 10035, MICC: 10236*/ exec sp_Execute_Insert 'dbo', 235, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10236, 10035,''id_pais'',''CAR_CdiPais'', 3', 1
		/*MONI*/ /*MIC: 10035, MICC: 10237*/ exec sp_Execute_Insert 'dbo', 236, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10237, 10035,''id_areaatuacao'',''CAR_CdiAreaAtuacao_Inicial'', 3', 1
		/*SOAP*/ /*MIC: 10036, MICC: 10238*/ exec sp_Execute_Insert 'dbo', 237, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10238, 10036,''matriculaRh'',''importacaoFuncionario;Funcionario;funcionarioWsVo;matriculaRh'', 9', 1
		/*REST*/ /*MIC: 10040, MICC: 10239*/ exec sp_Execute_Insert 'dbo', 238, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_CdiTipoCampo, BBP_OplConteudoFixo', '10239, 10040, ''mockField'', 9, 1', 1
		/*REST*/ /*MIC: 10040, MICC: 10240*/ exec sp_Execute_Insert 'dbo', 239, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_CdiTipoCampo, BBP_OplConteudoFixo', '10240, 10041, ''mockField'', 9, 1', 1 
		/*REST*/ /*MIC: 10042, MICC: 10241*/ exec sp_Execute_Insert 'dbo', 240, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica', '10241, 10042, ''Campo1LT'', ''Campo1SQL'', 9, 1013, 1', 1 
		/*REST*/ /*MIC: 10042, MICC: 10242*/ exec sp_Execute_Insert 'dbo', 241, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica', '10242, 10042, ''Campo2LT'', ''Campo2SQL'', 9, 1013, 1', 1 
		/*REST*/ /*MIC: 10042, MICC: 10243*/ exec sp_Execute_Insert 'dbo', 242, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica', '10243, 10042, ''Campo3LT'', ''Campo3SQL'', 9, 1013, 1', 1
		/*REST*/ /*MIC: 10042, MICC: 10244*/ exec sp_Execute_Insert 'dbo', 243, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica', '10244, 10042, ''Campo4LT'', ''Campo4SQL'', 9, 1013, 1', 1
		/*REST*/ /*MIC: 10042, MICC: 10245*/ exec sp_Execute_Insert 'dbo', 244, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica', '10245, 10042, ''Campo5LT'', ''Campo5SQL'', 9, 1013, 1', 1
		/*REST*/ /*MIC: 10042, MICC: 10246*/ exec sp_Execute_Insert 'dbo', 245, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica', '10246, 10042, ''Campo6LT'', ''Campo6SQL'', 9, 1013, 1', 1 
		/*REST*/ /*MIC: 10045, MICC: 10247*/ exec sp_Execute_Insert 'dbo', 246, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiModeloIntegracaoCmdCpo', '10045, ''BED_CdiJuridicaLocal'', ''BED_CdiJuridicaLocal'', 9, 10247', 1
		/*REST*/ /*MIC: 10045, MICC: 10248*/ exec sp_Execute_Insert 'dbo', 247, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiModeloIntegracaoCmdCpo', '10045, ''BED_CdiContratoPJ'', ''BED_CdiContratoPJ'', 9, 10248', 1
		/*REST*/ /*MIC: 10050, MICC: 10249*/ exec sp_Execute_Insert 'dbo', 248, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10249, 10050, ''keyid'', ''keyid'', 9, 1015', 1
		/*REST*/ /*MIC: 10051, MICC: 10250*/ exec sp_Execute_Insert 'dbo', 249, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10250, 10051, ''keyid'', ''keyid'', 9, 1014', 1

		/*##### ModelosIntegracoesCmdsRets*/
		/*REST*/ /*MICR: 10003, MIC: 10016*/ exec sp_Execute_Insert 'dbo', 002, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10003, 10016, ''name'', ''data;name''', 1
		/*REST*/ /*MICR: 10004, MIC: 10016*/ exec sp_Execute_Insert 'dbo', 003, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10004, 10016, ''age'', ''data;age''', 1
		/*REST*/ /*MICR: 10005, MIC: 10016*/ exec sp_Execute_Insert 'dbo', 004, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10005, 10016, ''verb'', ''data;verb''', 1
		/*REST*/ /*MICR: 10006, MIC: 10017*/ exec sp_Execute_Insert 'dbo', 005, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10006, 10017, ''id'', ''root/dataArray/element/data/element;id''', 1
		/*REST*/ /*MICR: 10007, MIC: 10017*/ exec sp_Execute_Insert 'dbo', 006, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10007, 10017, ''nomeProfissionalExterno'', ''root/dataArray/element/data/element;nomeProfissionalExterno''', 1
        /*REST*/ /*MICR: 10008, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 007, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10008, 10037, ''IDAPOLLUS'', ''Data/element;id''', 1
        /*REST*/ /*MICR: 10009, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 008, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10009, 10037, ''status'', ''Data/element;status''', 1
        /*REST*/ /*MICR: 10010, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 009, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10010, 10037, ''inicioAfastamento'', ''Data/element;inicioAfastamento''', 1
        /*REST*/ /*MICR: 10011, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 010, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10011, 10037, ''previsao'', ''Data/element;previsao''', 1
        /*REST*/ /*MICR: 10012, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 011, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10012, 10037, ''totalDiasAfastado'', ''Data/element;totalDiasAfastado''', 1
        /*REST*/ /*MICR: 10013, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 012, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10013, 10037, ''dataRetorno'', ''Data/element;dataRetorno''', 1
        /*REST*/ /*MICR: 10014, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 013, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10014, 10037, ''motivoAfastamento'', ''Data/element;motivoAfastamento''', 1
        /*REST*/ /*MICR: 10015, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 014, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10015, 10037, ''tipoAcidenteTransito'', ''Data/element;tipoAcidenteTransito''', 1
        /*REST*/ /*MICR: 10016, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 015, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10016, 10037, ''horaSaida'', ''Data/element;horaSaida''', 1
        /*REST*/ /*MICR: 10017, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 016, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10017, 10037, ''horaRetorno'', ''Data/element;horaRetorno''', 1
        /*REST*/ /*MICR: 10018, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 017, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10018, 10037, ''afastamentoINSS'', ''Data/element;afastamentoINSS''', 1
        /*REST*/ /*MICR: 10019, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 018, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10019, 10037, ''maior15Dias'', ''Data/element;maior15Dias''', 1
        /*REST*/ /*MICR: 10020, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 019, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10020, 10037, ''afastamentoDefinitivo'', ''Data/element;afastamentoDefinitivo''', 1
        /*REST*/ /*MICR: 10021, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 020, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10021, 10037, ''cidCodigo'', ''Data/element;cidCodigo''', 1
        /*REST*/ /*MICR: 10022, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 021, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10022, 10037, ''CRM'', ''Data/element;inscricaoProfissionalExterno''', 1
        /*REST*/ /*MICR: 10023, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 022, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10023, 10037, ''cidade'', ''Data/element;cidade''', 1
        /*REST*/ /*MICR: 10024, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 023, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10024, 10037, ''unidadeAtendimento'', ''Data/element;descricao''', 1
        /*REST*/ /*MICR: 10025, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 024, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10025, 10037, ''profissionalAtestado'', ''Data/element;profissionalAtestado;nome''', 1
        /*REST*/ /*MICR: 10026, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 025, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10026, 10037, ''responsavel'', ''Data/element;pessoa;nome''', 1
        /*REST*/ /*MICR: 10027, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 026, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10027, 10037, ''responsavelRegistro'', ''Data/element;responsavelRegistro;nome''', 1
        /*REST*/ /*MICR: 10028, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 027, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10028, 10037, ''responsavelRegistroID'', ''Data/element;responsavelRegistro;matriculaEmpresa''', 1
        /*REST*/ /*MICR: 10029, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 028, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10029, 10037, ''matriculaempresa'', ''Data/element;pessoa;matriculaEmpresa''', 1
        /*REST*/ /*MICR: 10030, MIC: 10037*/ exec sp_Execute_Insert 'dbo', 029, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10030, 10037, ''PaginaGerada'', ''ResponseArray/element;page''', 1
        /*REST*/ /*MICR: 10031, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 030, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10031, 10043, ''IDAPOLLUS'', ''Data/element;id''', 1
        /*REST*/ /*MICR: 10032, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 031, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10032, 10043, ''status'', ''Data/element;status''', 1
        /*REST*/ /*MICR: 10033, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 032, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10033, 10043, ''inicioAfastamento'', ''Data/element;inicioAfastamento''', 1
        /*REST*/ /*MICR: 10034, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 033, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10034, 10043, ''previsao'', ''Data/element;previsao''', 1
        /*REST*/ /*MICR: 10035, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 034, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10035, 10043, ''totalDiasAfastado'', ''Data/element;totalDiasAfastado''', 1
        /*REST*/ /*MICR: 10036, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 035, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10036, 10043, ''dataRetorno'', ''Data/element;dataRetorno''', 1
        /*REST*/ /*MICR: 10037, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 036, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10037, 10043, ''motivoAfastamento'', ''Data/element;motivoAfastamento''', 1
        /*REST*/ /*MICR: 10038, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 037, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10038, 10043, ''tipoAcidenteTransito'', ''Data/element;tipoAcidenteTransito''', 1
        /*REST*/ /*MICR: 10039, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 038, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10039, 10043, ''horaSaida'', ''Data/element;horaSaida''', 1
        /*REST*/ /*MICR: 10040, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 039, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10040, 10043, ''horaRetorno'', ''Data/element;horaRetorno''', 1
        /*REST*/ /*MICR: 10041, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 040, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10041, 10043, ''afastamentoINSS'', ''Data/element;afastamentoINSS''', 1
        /*REST*/ /*MICR: 10042, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 041, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10042, 10043, ''maior15Dias'', ''Data/element;maior15Dias''', 1
        /*REST*/ /*MICR: 10043, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 042, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10043, 10043, ''afastamentoDefinitivo'', ''Data/element;afastamentoDefinitivo''', 1
        /*REST*/ /*MICR: 10044, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 043, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10044, 10043, ''cidCodigo'', ''Data/element;cidCodigo''', 1
        /*REST*/ /*MICR: 10045, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 044, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10045, 10043, ''CRM'', ''Data/element;inscricaoProfissionalExterno''', 1
        /*REST*/ /*MICR: 10046, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 045, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10046, 10043, ''cidade'', ''Data/element;cidade''', 1
        /*REST*/ /*MICR: 10047, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 046, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10047, 10043, ''unidadeAtendimento'', ''Data/element;descricao''', 1
        /*REST*/ /*MICR: 10048, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 047, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10048, 10043, ''profissionalAtestado'', ''Data/element;profissionalAtestado;nome''', 1
        /*REST*/ /*MICR: 10049, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 048, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10049, 10043, ''responsavel'', ''Data/element;pessoa;nome''', 1
        /*REST*/ /*MICR: 10050, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 049, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10050, 10043, ''responsavelRegistro'', ''Data/element;responsavelRegistro;nome''', 1
        /*REST*/ /*MICR: 10051, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 050, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10051, 10043, ''responsavelRegistroID'', ''Data/element;responsavelRegistro;matriculaEmpresa''', 1
        /*REST*/ /*MICR: 10052, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 051, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10052, 10043, ''matriculaempresa'', ''Data/element;pessoa;matriculaEmpresa''', 1
        /*REST*/ /*MICR: 10053, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 052, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10053, 10043, ''PaginaGerada'', ''ResponseArray/element;page''', 1
		/*REST*/ /*MICR: 10054, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 053, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10054, 10043, ''code'', ''root;code''', 1
		/*REST*/ /*MICR: 10055, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 054, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10055, 10043, ''message'', ''root;message''', 1
		/*REST*/ /*MICR: 10056, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 055, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10056, 10043, ''page'', ''root;page''', 1
		/*REST*/ /*MICR: 10057, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 056, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10057, 10043, ''totalPages'', ''root;totalPages''', 1
		/*REST*/ /*MICR: 10058, MIC: 10043*/ exec sp_Execute_Insert 'dbo', 057, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10058, 10043, ''totalItens'', ''root;totalItens''', 1
		/*REST*/ /*MICR: 10059, MIC: 10046*/ exec sp_Execute_Insert 'dbo', 058, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10059, 10046, ''transactionId'', ''root;transactionId''', 1
		/*REST*/ /*MICR: 10060, MIC: 10046*/ exec sp_Execute_Insert 'dbo', 059, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10060, 10046, ''timestamp'', ''root;timestamp''', 1
		/*REST*/ /*MICR: 10061, MIC: 10046*/ exec sp_Execute_Insert 'dbo', 060, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10061, 10046, ''stringField'', ''payload;stringField''', 1
		/*REST*/ /*MICR: 10062, MIC: 10046*/ exec sp_Execute_Insert 'dbo', 061, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10062, 10046, ''integerField'', ''payload;integerField''', 1
		/*REST*/ /*MICR: 10063, MIC: 10046*/ exec sp_Execute_Insert 'dbo', 062, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10063, 10046, ''floatField'', ''payload;floatField''', 1
		/*REST*/ /*MICR: 10064, MIC: 10046*/ exec sp_Execute_Insert 'dbo', 063, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10064, 10046, ''booleanTrue'', ''payload;booleanTrue''', 1
		/*REST*/ /*MICR: 10065, MIC: 10046*/ exec sp_Execute_Insert 'dbo', 064, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10065, 10046, ''booleanFalse'', ''payload;booleanFalse''', 1
		/*REST*/ /*MICR: 10066, MIC: 10046*/ exec sp_Execute_Insert 'dbo', 065, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10066, 10046, ''nullField'', ''payload;nullField''', 1
		/*REST*/ /*MICR: 10067, MIC: 10046*/ exec sp_Execute_Insert 'dbo', 066, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10067, 10046, ''subObjectInArray'', ''payload;arrayField;element;subObjectInArray''', 1
		/*REST*/ /*MICR: 10068, MIC: 10046*/ exec sp_Execute_Insert 'dbo', 067, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10068, 10046, ''nestedString'', ''payload;objectField;nestedString''', 1
		/*REST*/ /*MICR: 10069, MIC: 10046*/ exec sp_Execute_Insert 'dbo', 068, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10069, 10046, ''nestedInteger'', ''payload;objectField;nestedInteger''', 1
		/*REST*/ /*MICR: 10070, MIC: 10046*/ exec sp_Execute_Insert 'dbo', 069, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10070, 10046, ''dateField'', ''payload;dateField''', 1
		/*REST*/ /*MICR: 10071, MIC: 10046*/ exec sp_Execute_Insert 'dbo', 070, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10071, 10046, ''uuidField'', ''payload;uuidField''', 1
		/*REST*/ /*MICR: 10072, MIC: 10046*/ exec sp_Execute_Insert 'dbo', 071, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10072, 10046, ''nestedArrayElement'', ''root/payload/objectField/nestedArray;element''', 1
		/*REST*/ /*MICR: 10073, MIC: 10046*/ exec sp_Execute_Insert 'dbo', 072, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10073, 10046, ''arrayFieldElement'', ''root/payload/arrayField;element''', 1
		/*REST*/ /*MICR: 10074, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 073, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10074, 10047, ''companyName'', ''root;companyName''', 1
		/*REST*/ /*MICR: 10075, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 074, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10075, 10047, ''rootId'', ''root;id''', 1
		/*REST*/ /*MICR: 10076, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 075, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10076, 10047, ''event'', ''root;event''', 1
		/*REST*/ /*MICR: 10077, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 075, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10077, 10047, ''date'', ''root;date''', 1
		/*REST*/ /*MICR: 10078, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 076, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10078, 10047, ''jobId'', ''root;data;job;id''', 1
		/*REST*/ /*MICR: 10079, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 077, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10079, 10047, ''name'', ''root;data;job;name''', 1
		/*REST*/ /*MICR: 10080, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 078, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10080, 10047, ''departmentCode'', ''root;data;job;departmentCode''', 1
		/*REST*/ /*MICR: 10081, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 079, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10081, 10047, ''roleCode'', ''root;data;job;roleCode''', 1
		/*REST*/ /*MICR: 10082, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 080, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10082, 10047, ''branchCode'', ''root;data;job;branchCode''', 1
		/*REST*/ /*MICR: 10083, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 081, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10083, 10047, ''applicationId'', ''root;data;application;id''', 1
		/*REST*/ /*MICR: 10084, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 082, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10084, 10047, ''vacancyCode'', ''root;data;application;vacancyCode''', 1
		/*REST*/ /*MICR: 10085, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 083, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10085, 10047, ''score'', ''root;data;application;score''', 1
		/*REST*/ /*MICR: 10086, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 084, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10086, 10047, ''preHiringInformation'', ''root;data;application;preHiringInformation''', 1
		/*REST*/ /*MICR: 10087, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 085, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10087, 10047, ''element'', ''root;data;application;tags;element''', 1
		/*REST*/ /*MICR: 10088, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 086, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10088, 10047, ''hiringDate'', ''root;data;application;hiringDate''', 1
		/*REST*/ /*MICR: 10089, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 087, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10089, 10047, ''currentStepId'', ''root;data;application;currentStep;id''', 1
		/*REST*/ /*MICR: 10090, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 088, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10090, 10047, ''currentStepName'', ''root;data;application;currentStep;name''', 1
		/*REST*/ /*MICR: 10091, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 089, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10091, 10047, ''candidateId'', ''root;data;candidate;id''', 1
		/*REST*/ /*MICR: 10092, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 090, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10092, 10047, ''candidateName'', ''root;data;candidate;name''', 1
		/*REST*/ /*MICR: 10093, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 091, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10093, 10047, ''lastName'', ''root;data;candidate;lastName''', 1
		/*REST*/ /*MICR: 10094, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 092, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10094, 10047, ''email'', ''root;data;candidate;email''', 1
		/*REST*/ /*MICR: 10095, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 093, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10095, 10047, ''identificationDocument'', ''root;data;candidate;identificationDocument''', 1
		/*REST*/ /*MICR: 10096, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 094, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10096, 10047, ''countryOfOrigin'', ''root;data;candidate;countryOfOrigin''', 1
		/*REST*/ /*MICR: 10097, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 095, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10097, 10047, ''birthdate'', ''root;data;candidate;birthdate''', 1
		/*REST*/ /*MICR: 10098, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 096, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10098, 10047, ''addressZipCode'', ''root;data;candidate;addressZipCode''', 1
		/*REST*/ /*MICR: 10099, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 097, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10099, 10047, ''addressStreet'', ''root;data;candidate;addressStreet''', 1
		/*REST*/ /*MICR: 10100, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 098, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10100, 10047, ''addressNumber'', ''root;data;candidate;addressNumber''', 1
		/*REST*/ /*MICR: 10101, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 099, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10101, 10047, ''addressCity'', ''root;data;candidate;addressCity''', 1
		/*REST*/ /*MICR: 10102, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 100, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10102, 10047, ''addressState'', ''root;data;candidate;addressState''', 1
		/*REST*/ /*MICR: 10103, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 101, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10103, 10047, ''addressStateShortName'', ''root;data;candidate;addressStateShortName''', 1
		/*REST*/ /*MICR: 10104, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 102, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10104, 10047, ''addressCountry'', ''root;data;candidate;addressCountry''', 1
		/*REST*/ /*MICR: 10105, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 103, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10105, 10047, ''addressCountryShortName'', ''root;data;candidate;addressCountryShortName''', 1
		/*REST*/ /*MICR: 10106, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 104, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10106, 10047, ''mobileNumber'', ''root;data;candidate;mobileNumber''', 1
		/*REST*/ /*MICR: 10107, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 105, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10107, 10047, ''phoneNumber'', ''root;data;candidate;phoneNumber''', 1
		/*REST*/ /*MICR: 10108, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 106, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10108, 10047, ''schooling'', ''root;data;candidate;schooling''', 1
		/*REST*/ /*MICR: 10109, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 107, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10109, 10047, ''schoolingStatus'', ''root;data;candidate;schoolingStatus''', 1
		/*REST*/ /*MICR: 10110, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 108, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10110, 10047, ''disabilities'', ''root;data;candidate;disabilities''', 1
		/*REST*/ /*MICR: 10111, MIC: 10047*/ exec sp_Execute_Insert 'dbo', 109, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10111, 10047, ''gend'', ''root;data;candidate;gend''', 1
		/*REST*/ /*MICR: 10112, MIC: 10048*/ exec sp_Execute_Insert 'dbo', 110, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10112, 10048, ''id'', ''root/employees/element;id''', 1
		/*REST*/ /*MICR: 10113, MIC: 10048*/ exec sp_Execute_Insert 'dbo', 111, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10113, 10048, ''changedFields'', ''root/employees/element;changedFields;element''', 1
		/*REST*/ /*MICR: 10114, MIC: 10048*/ exec sp_Execute_Insert 'dbo', 112, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10114, 10048, ''employee'', ''root/employees/element;fields;Employee#;value''', 1
		/*REST*/ /*MICR: 10115, MIC: 10048*/ exec sp_Execute_Insert 'dbo', 113, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10115, 10048, ''firstName'', ''root/employees/element;fields;FirstName;value''', 1
		/*REST*/ /*MICR: 10116, MIC: 10048*/ exec sp_Execute_Insert 'dbo', 114, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10116, 10048, ''lastName'', ''root/employees/element;fields;LastName;value''', 1
		/*REST*/ /*MICR: 10117, MIC: 10048*/ exec sp_Execute_Insert 'dbo', 115, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10117, 10048, ''jobTitle'', ''root/employees/element;fields;JobTitle;value''', 1
		/*REST*/ /*MICR: 10118, MIC: 10049*/ exec sp_Execute_Insert 'dbo', 116, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10118, 10049, ''companyId'', ''root;companyId''', 1
		/*REST*/ /*MICR: 10118, MIC: 10049*/ exec sp_Execute_Insert 'dbo', 117, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10119, 10049, ''preEmployeeId'', ''root;preEmployeeId''', 1
		/*REST*/ /*MICR: 10118, MIC: 10049*/ exec sp_Execute_Insert 'dbo', 118, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10120, 10049, ''fullName'', ''root;fullName''', 1
		/*REST*/ /*MICR: 10118, MIC: 10049*/ exec sp_Execute_Insert 'dbo', 119, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10121, 10049, ''cpf'', ''root;cpf''', 1
		/*REST*/ /*MICR: 10118, MIC: 10049*/ exec sp_Execute_Insert 'dbo', 120, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10122, 10049, ''contractUrl'', ''root;contract;url''', 1
		/*REST*/ /*MICR: 10118, MIC: 10049*/ exec sp_Execute_Insert 'dbo', 121, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10123, 10049, ''contractsUrl'', ''root;contracts;element;url''', 1
		/*REST*/ /*MICR: 10118, MIC: 10049*/ exec sp_Execute_Insert 'dbo', 122, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10124, 10049, ''typeSignature'', ''root/contract/signatures/element;type''', 1
		/*REST*/ /*MICR: 10118, MIC: 10049*/ exec sp_Execute_Insert 'dbo', 123, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10125, 10049, ''nameSignature'', ''root/contract/signatures/element;name''', 1
		/*REST*/ /*MICR: 10118, MIC: 10049*/ exec sp_Execute_Insert 'dbo', 124, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10126, 10049, ''signedAtSignature'', ''root/contract/signatures/element;signedAt''', 1
		/*REST*/ /*MICR: 10118, MIC: 10049*/ exec sp_Execute_Insert 'dbo', 125, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10127, 10049, ''typeSignature'', ''root/contracts/signatures/element;type''', 1
		/*REST*/ /*MICR: 10118, MIC: 10049*/ exec sp_Execute_Insert 'dbo', 126, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10128, 10049, ''nameSignature'', ''root/contracts/signatures/element;name''', 1
		/*REST*/ /*MICR: 10118, MIC: 10049*/ exec sp_Execute_Insert 'dbo', 127, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10129, 10049, ''emailSignature'', ''root/contracts/signatures/element;email''', 1
		/*REST*/ /*MICR: 10118, MIC: 10049*/ exec sp_Execute_Insert 'dbo', 128, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10130, 10049, ''nameForms'', ''root/forms/element;name''', 1
		/*REST*/ /*MICR: 10118, MIC: 10049*/ exec sp_Execute_Insert 'dbo', 129, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10131, 10049, ''stepForms'', ''root/forms/element;step''', 1
		/*REST*/ /*MICR: 10118, MIC: 10049*/ exec sp_Execute_Insert 'dbo', 130, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10132, 10049, ''pdfURLForms'', ''root/forms/element;pdfURL''', 1
		/*REST*/ /*MICR: 10118, MIC: 10049*/ exec sp_Execute_Insert 'dbo', 131, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10133, 10049, ''nameImages'', ''root/forms/element/images/element;name''', 1
		/*REST*/ /*MICR: 10118, MIC: 10049*/ exec sp_Execute_Insert 'dbo', 132, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10134, 10049, ''urlImages'', ''root/forms/element/images/element;url''', 1
		/*REST*/ /*MICR: 10118, MIC: 10049*/ exec sp_Execute_Insert 'dbo', 133, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10135, 10049, ''idData'', ''root/forms/element/data/element;id''', 1
		/*REST*/ /*MICR: 10118, MIC: 10049*/ exec sp_Execute_Insert 'dbo', 134, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10136, 10049, ''fieldData'', ''root/forms/element/data/element;field''', 1
		/*REST*/ /*MICR: 10118, MIC: 10049*/ exec sp_Execute_Insert 'dbo', 135, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10137, 10049, ''valueData'', ''root/forms/element/data/element;value''', 1
		
		/*#### OBJETO - 550 - ABA BASES DE DADOS - ServidoresIntegracoesBDs*/
		exec sp_Execute_Insert 'dbo', 01, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '01, 1, ''(TESTES) SOAP CORREIOS'', 10001, 1, 6, 1, ''https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl''', 1
		exec sp_Execute_Insert 'dbo', 02, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '02, 1, ''(TESTES) REST SEM PARAMETROS'', 10002, 1, 10, 1, ''http://echo.jsontest.com/key/value/one/two''', 1
		exec sp_Execute_Insert 'dbo', 03, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '03, 1, ''(TESTES) REST COM PARAMETROS'', 10003, 1, 10, 1, ''http://validate.jsontest.com/''', 1
		exec sp_Execute_Insert 'dbo', 04, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '04, 1, ''(TESTES) REST COM PARAMETROS URL'', 10004, 1, 10, 1, ''https://viacep.com.br/''', 1
		exec sp_Execute_Insert 'dbo', 05, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '05, 1, ''(TESTES) REST ALTERACAO OBJ 2330'', 10005, 2, 10, 1, ''''', 1
		exec sp_Execute_Insert 'dbo', 06, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '06, 1, ''(TESTES) REST MARCACAO PONTO'', 10006, 2, 10, 0, ''''', 1
		exec sp_Execute_Insert 'dbo', 07, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '07, 1, ''(TESTES) REST ENTRADA BASEX64'', 10007, 2, 10, 0, ''''', 1
		exec sp_Execute_Insert 'dbo', 08, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor, BBO_CdiTipoAutentConsWebServi', '08, 1, ''(TESTES) SERVICO SOAP CORREIOS LOTE'', 10008, 1, 6, 0,  ''https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl'', 1', 1
		exec sp_Execute_Insert 'dbo', 09, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '09, 1, ''(TESTES) MOCK POSTMAN GET'', 10010, 1, 10, 1, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/''', 1
		exec sp_Execute_Insert 'dbo', 10, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '10, 1, ''(TESTES) MOCK POSTMAN PUT'', 10011, 1, 10, 1, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/''', 1 
		exec sp_Execute_Insert 'dbo', 11, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '11, 1, ''(TESTES) MOCK POSTMAN POST AT'', 10012, 1, 10, 1, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/''', 1 
		exec sp_Execute_Insert 'dbo', 12, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor, BBO_CdiTipoAutentConsWebServi', '12, 1, ''(TESTES) SERVICO REST CONSULTAS REMOTAS'',  10013, 2, 10, 1, '''', 5', 1 
		exec sp_Execute_Insert 'dbo', 13, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '13, 1, ''(TESTES) MOCK POSTMAN GET ARRAY'',  10014, 1, 10, 1, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/''', 1
		exec sp_Execute_Insert 'dbo', 14, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '14, 1, ''(TESTES) API THIRDPART GET'', 10015, 1, 10, 1, ''https://62d6befa51e6e8f06f1214f9.mockapi.io/api''', 1
		exec sp_Execute_Insert 'dbo', 15, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '15, 1, ''(TESTES) MOCK REST REPROCESS'', 10016, 1, 10, 1, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/''', 1
		exec sp_Execute_Insert 'dbo', 16, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '16, 1, ''(TESTES)  REST MOCK POSTMAN V2'',  10017, 1, 10, 1, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/''', 1
		exec sp_Execute_Insert 'dbo', 17, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '17, 1, ''(TESTES) COMBATIDAS REAIS'',  10018, 1, 10, 1, ''''', 1
		exec sp_Execute_Insert 'dbo', 18, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '18, 1, ''(TESTES) SOAP TESTE WSDL TECBAN'',  10019, 1, 6, 1, ''C:\Users\flsantos\OneDrive - Apdata do Brasil Software Ltda\Chamados\Wsdl\SFSF_PO.wsdl''', 1
		exec sp_Execute_Insert 'dbo', 19, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '19, 1, ''(TESTES) LOTES REST'',  10023, 1, 10, 1, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/''', 1
		exec sp_Execute_Insert 'dbo', 20, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '20, 1, ''(TESTES) MOCK POSTMAN GET COMPLEXY ARRAY'',  10024, 1, 10, 1, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/''', 1
		exec sp_Execute_Insert 'dbo', 21, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '21, 1, ''(TESTES) REST ALTER DA TRANSACAO E O TIPO EDICAO'', 10025, 2, 10, 0, ''''', 1
		exec sp_Execute_Insert 'dbo', 22, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '22, 1, ''(TESTES) SOAP ADMISSAO - INCLUSAO DEPENDENTES'', 10026, 2, 10, 0, ''''', 1
		exec sp_Execute_Insert 'dbo', 23, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '23, 1, ''(TESTES) REST ADD DATE/HOUR IN OBJ 3129'', 10021, 2, 10, 0, ''''', 1
		exec sp_Execute_Insert 'dbo', 24, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '24, 1, ''(TESTES) SOAP ADMISSAO - EX_EMS'', 10027, 1, 6, 1, ''''', 1
		exec sp_Execute_Insert 'dbo', 25, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor, BBO_CdsUsuario_Banco, BBO_CosSenha_Banco', '25, 1, ''(TESTES) MONITORAMENTO DE BASE'', 10029, 4, 1, 1, ''APDNSON0220:INTEGRATION_BETA'', ''apdata'', ''0E6B70606C7B68''', 1
		exec sp_Execute_Insert 'dbo', 26, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '26, 1, ''(TESTES) SOAP SOC'', 10030, 2, 10, 0, ''''', 1
		exec sp_Execute_Insert 'dbo', 27, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '27, 1, ''(TESTES) REST MOCK POSTMAN V4'', 10031, 1, 10, 1, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/''', 1
		exec sp_Execute_Insert 'dbo', 28, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '28, 1, ''(TESTES) REST MOCK POSTMAN TOKENS'', 10033, 1, 10, 1, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/''', 1
		exec sp_Execute_Insert 'dbo', 29, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '29, 1, ''(TESTES) REST MOCK POSTMAN PRE REQUEST'', 10034, 1, 10, 1, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/''', 1
		exec sp_Execute_Insert 'dbo', 30, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '30, 1, ''(TESTES) REST MOCK POSTMAN V5'', 10036, 1, 10, 1, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/''', 1
		exec sp_Execute_Insert 'dbo', 31, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '31, 1, ''(TESTES) REST REPROCESSAMENTO RETORNO 7313'', 10037, 1, 10, 1, ''https://localhost:7081/soap/IApWebServices''', 1
		exec sp_Execute_Insert 'dbo', 32, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_CdsUsuario_Banco, BBO_DssNomeServidor, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiTipoAutentConsWebServi, BBO_CosSenha_Banco', '32, 1, ''test'', ''tes'', 7, 1, 10038, 2, ''(TESTES) PROVIDER GRID IMP.XML'', 1, ''063D31''', 1
		exec sp_Execute_Insert 'dbo', 33, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiTipoAutentConsWebServi, BBO_CosSenha_Banco', '33, 1, 10, 1, 10038, 2, ''(TESTES) PROVIDER GRID REST'', 1, ''02''', 1
		exec sp_Execute_Insert 'dbo', 34, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '34, 1, ''(TESTES) MOCK POSTMAN DELETE A'', 10040, 1, 10, 1, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/''', 1 
		exec sp_Execute_Insert 'dbo', 35, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '35, 1, ''(TESTES) MOCK POSTMAN DELETE B'', 10041, 1, 10, 1, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/''', 1 

		/*OUTROS AJUSTES PARA TESTES - INICIO*/
			exec sp_Execute_Update 'dbo', 01, 'FormulariosWFSobreps', 'BRH_CdiOpcao_Desativado = 0', 'BRH_CdiFormularioWF = 407'
			exec sp_Execute_Update 'dbo', 02, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracao_Suc = 10022', 'BBS_CdiModeloIntegracaoCmd = 10005' 
			exec sp_Execute_Update 'dbo', 03, 'Usuarios', 'USR_CdsUsuario = ''System''', 'USR_CdiUsuario = 1' 

			exec sp_Execute_Insert 'dbo', 01, 'FormulariosWFCampos', 'FWC_CdiFormularioWFCampo, FWC_CdiFormularioWF, FWC_CdiCampo, FWC_CdiClasseProcCpoPar, FWC_NuiSequencial, FWC_NuiOrdem, FWC_OplReferencia, FWC_OplLigacao, FWC_OplCampoBase, FWC_CdiCampoAgrupamento, FWC_OplDataBase, FWC_CdiOpcao_InfObrigatoria, FWC_OplDataBaseBloqueio, FWC_CdiObjetoLookup, FWC_DsbSqlLookupField, FWC_CdiOpcao_Protocolo, FWC_DssContDefault_String, FWC_DtdContDefault_DataHora, FWC_NuiContDefault_Inteiro, FWC_OplContDefault_Logico, FWC_VlnContDefault_Numerico, FWC_VrnContDefault_Numerico, FWC_CdiOpcao_Default, FWC_CdiDominio, FWC_D1sLiteral, FWC_D2sLiteral, FWC_D3sLiteral, FWC_D4sLiteral, FWC_D5sLiteral, FWC_D6sLiteral, FWC_D7sLiteral, FWC_D8sLiteral, FWC_OplInformacaoObrigatoria, FWC_OplProtocolo, FWC_OplLigacaoFilho, FWC_CdiOpcao_LookupTodasEtapas, FWC_DsbContDefault_Blob, FWC_OplDesabilitaCpoLkpParam, FWC_CdiCampoFlexivel, FWC_OplDesativado, FWC_OplCampoCondicao, FWC_DsbSqlCampoVirtual, FWC_CdiAcaoCampo, FWC_CdiOpcao_GdMultiTransacao, FWC_NuiOrigemRegistro, FWC_OplMultiplaSelecao, FWC_OplExibirAjuda, FWC_D1bAjudaCampo, FWC_D2bAjudaCampo, FWC_D3bAjudaCampo, FWC_D4bAjudaCampo, FWC_D5bAjudaCampo, FWC_D6bAjudaCampo, FWC_D7bAjudaCampo, FWC_D8bAjudaCampo', '100505, 407, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, null, 0, null, null, 0, 0, 0, 0, 0, 214, ''ANEXO'', null, null, null, null, null, null, null, 0, 0, 0, 0, null, 0, 0, 0, 0, null, 0, 0, 4, 0, 0, null, null, null, null, null, null, null, null', 1
			exec sp_Execute_Insert 'dbo', 02, 'UsuariosAutenticacoes', 'JVQ_CdiUsuarioAutenticacao, JVQ_CdiUsuario, JVQ_CdsClientId, JVQ_CdsSecretKey, JVQ_CdiPerfil, JVQ_NuiMinutosValidadeToken, JVQ_D1sDescricao, JVQ_D2sDescricao, JVQ_D3sDescricao, JVQ_D4sDescricao, JVQ_D5sDescricao, JVQ_D6sDescricao, JVQ_D7sDescricao, JVQ_D8sDescricao', '1, 1, ''1658444F-EF87-47E7-B62C-F4F70BACE420'', ''@@/WJ8YRQ7Di9Sq/ci8cU2qJRdFdDvz9RefzHbOTyHNfoZpTtpog9cY/qjfqtQFtwxwo3w9bBxRbZmAyW/WkkcpUcaUbu+33yM'', 1, 10, ''Apdata OAuth2'', ''j2Bu6Bc6xuNZMM35x8ED4qN7cGJT5eH4'', null, null, null, null, null, null', 1 

			exec sp_Execute_Delete 'dbo', 01, 'UsuariosDesabilitacoes', 'USD_CdiUsuario = 1' 

			exec sp_takeKeyForInsertion 'Consultas', @MaxKeyFromTable OUTPUT 

    		/*Desativa a tag de segurança para consultas*/
			exec sp_Execute_Update 'dbo', 01, 'Usuarios', 'USR_OplForcarUsoTAGApDesig = 0', 'USR_CdiUsuario = 1', 1

			/*Teste para utilização do Manager AD*/
			exec sp_Execute_Update 'dbo', 01, 'Contratados', 'CON_NuiContratado_Superior = 1', 'CON_CdiContratado = 2', 1


			/*Query Execute*/
			exec sp_Execute_Insert_Key 'dbo', 01, 'Consultas', 'ACS_CdiConsulta, ACS_CdiConsultaGrupo, ACS_DtdAbertura, ACS_DsbConteudo, ACS_DssConsulta, ACS_OplExigirSenhaAdicl, ACS_CdiPais, ACS_OplFolhasDesativadas, ACS_OplPublico, ACS_NuiIcone, ACS_NuiIcone_Selecionado, ACS_NuiIcone_WorkArea, ACS_OplVisContratadoConectado, ACS_D1sNomeReferencia, ACS_D2sNomeReferencia, ACS_D3sNomeReferencia, ACS_D4sNomeReferencia, ACS_D5sNomeReferencia, ACS_D6sNomeReferencia, ACS_D7sNomeReferencia, ACS_D8sNomeReferencia, ACS_D1bAjuda, ACS_D2bAjuda, ACS_D3bAjuda, ACS_D4bAjuda, ACS_D5bAjuda, ACS_D6bAjuda, ACS_D7bAjuda, ACS_D8bAjuda, ACS_OplAltoConsumoRecurso, ACS_D1sConsultaExplicacao, ACS_D2sConsultaExplicacao, ACS_D3sConsultaExplicacao, ACS_D4sConsultaExplicacao, ACS_D5sConsultaExplicacao, ACS_D6sConsultaExplicacao, ACS_D7sConsultaExplicacao, ACS_D8sConsultaExplicacao, ACS_OplDesativado', @MaxKeyFromTable, 0, '75, null, 0x545046300654646153514C00035461670372631147756964436F6C6C6174696F6E54797065070D67634D5353514C5365727665720C446174616261736554797065070D64744D5353514C5365727665721044617461506970656C696E654E616D65060B426C6F624172717569766F0D4564697453514C41735465787409094C696E6B436F6C6F720707636C426C61636B084C696E6B5479706507126C74506172616D65746572697A656453514C164D617853514C4669656C64416C6961734C656E67746802000F53514C546578742E537472696E677301063073656C656374204C57435F4364694C616E63616D656E746F57462C204C57435F447362436F6E746575646F5F426C6F62061B202046726F6D204C616E63616D656E746F73574643616D706F7320067020207768657265204C57435F4364694C616E63616D656E746F574620696E202873656C656374204C57465F4364694C616E63616D656E746F57462046726F6D204C616E63616D656E746F735746207768657265204C57465F436469466F726D756C6172696F5746203D203A496446572906262020416E64204C57435F447362436F6E746575646F5F426C6F62206973206E6F74206E756C6C1200000000000753514C547970650706737153514C32074C69746572616C060B426C6F624172717569766F06506172616D730E01084461746154797065070C667457696465537472696E67044E616D6506044964465709506172616D5479706507097074556E6B6E6F776E00000D4E6F5472616E736C6174696F6E0800085464614669656C640864614669656C643105416C69617314170000004C616EC3A7616D656E746F20646520576F726B666C6F7708446174615479706507096474496E74656765720C446973706C6179576964746802080A4669656C64416C69617314170000004C616EC3A7616D656E746F20646520576F726B666C6F770B4669656C644C656E677468020A094669656C644E616D6506134C57435F4364694C616E63616D656E746F57460C53514C4669656C644E616D6506134C57435F4364694C616E63616D656E746F5746095461626C654E616D6506134C616E63616D656E746F73574643616D706F730000085464614669656C640864614669656C643205416C6961731410000000436F6E7465C3BA646F202D20426C6F6208446174615479706507066474424C4F420C446973706C6179576964746803AD0D0A4669656C64416C6961731410000000436F6E7465C3BA646F202D20426C6F620B4669656C644C656E677468038813094669656C644E616D6506144C57435F447362436F6E746575646F5F426C6F62084C696E6B61626C65080C53514C4669656C644E616D6506144C57435F447362436F6E746575646F5F426C6F62095461626C654E616D6506134C616E63616D656E746F73574643616D706F73000000, ''BlobArquivo'', 0, 1, 0, 0, 0, 0, 0, 0, ''BlobArquivo'', ''Español='', ''BlobArquivo'', ''BlobArquivo'', ''BlobArquivo'', ''BlobArquivo'', ''BlobArquivo'', ''BlobArquivo'', null, null, null, null, null, null, null, null, 0, '''', '''', '''', '''', '''', '''', '''', '''', 0', 1
		/*OUTROS AJUSTES PARA TESTES - FIM*/	

		/*1440 - Listas Genéricas de Conteúdo*/
			declare @ListasGenericasKey int
			declare @ListasGenericasItensKey int

			exec sp_takeKeyForInsertion 'ListasGenericas', @ListasGenericasKey OUTPUT 
			exec sp_takeKeyForInsertion 'ListasGenericasItens', @ListasGenericasItensKey OUTPUT 
			
			exec sp_Execute_Insert_Key 'dbo', 01, 'ListasGenericas', 'CJT_CdiListaGenerica, CJT_D1sListaGenerica', @ListasGenericasKey, 1, '''OU=ListaGenerica_01''', 1 
			/*Contratado 1 - CJU_NuiConteudo_Inteiro (Id do Contratado)*/
			exec sp_Execute_Insert_Key_ForeignKey 'dbo', 02, 'ListasGenericasItens', 'CJU_CdiListaGenericaItem, CJU_CdiListaGenerica, CJU_NuiConteudo_Inteiro', @ListasGenericasItensKey, 1, @ListasGenericasKey, 1, '1', 1 

			exec sp_Execute_Insert_Key 'dbo', 03, 'ListasGenericas', 'CJT_CdiListaGenerica, CJT_D1sListaGenerica', @ListasGenericasKey, 2, '''OU=ListaGenerica_02''', 1 
			/*Contratado 2 - CJU_NuiConteudo_Inteiro (Id do Contratado)*/
			exec sp_Execute_Insert_Key_ForeignKey 'dbo', 04, 'ListasGenericasItens', 'CJU_CdiListaGenericaItem, CJU_CdiListaGenerica, CJU_NuiConteudo_Inteiro', @ListasGenericasItensKey, 2, @ListasGenericasKey, 2, '2', 1 

		/*1048 - CONTEUDO PRE-DEFINIDO - DEFAULTS - INICIO*/
			declare @DefaultsKey int
			declare @DefaultsChavesKey int

			exec sp_takeKeyForInsertion 'Defaults', @DefaultsKey OUTPUT
			exec sp_takeKeyForInsertion 'DefaultsChaves', @DefaultsChavesKey OUTPUT

			exec sp_Execute_Insert_Key 'dbo', 01, 'Defaults', 'DEF_CdiDefault, DEF_D1sDefault, DEF_CdiTipoDefault', @DefaultsKey, 01, '''(TESTES) AD DEFAULT 1'',5', 1  
            exec sp_Execute_Insert_ThreeKey 'dbo', 02, 'DefaultsChaves', 'BDI_CdiDefaultChave, BDI_CdiDefault, BDI_CdiListaGenerica, BDI_CdiCampo_Chave, BDI_CdiOperacaoLogica', @DefaultsChavesKey, 1,  @DefaultsKey, 1, @ListasGenericasKey, 1, '10430, 9', 1
			
			exec sp_Execute_Insert_Key 'dbo', 03, 'Defaults', 'DEF_CdiDefault, DEF_D1sDefault, DEF_CdiTipoDefault', @DefaultsKey, 02, '''(TESTES) AD DEFAULT 2'',5', 1  
			exec sp_Execute_Insert_ThreeKey 'dbo', 02, 'DefaultsChaves', 'BDI_CdiDefaultChave, BDI_CdiDefault, BDI_CdiListaGenerica, BDI_CdiCampo_Chave, BDI_CdiOperacaoLogica', @DefaultsChavesKey, 2,  @DefaultsKey, 2, @ListasGenericasKey, 2, '10430, 9', 1

		/*1048 - CONTEUDO PRE-DEFINIDO - DEFAULTS - FIM*/

		/*2330 - CRIAÇÃO/ATUALIZAÇÃO DE NOVOS/ATUAIS USUÁRIOS PARA TESTES - BEGIN*/
		    declare @ultimaChaveUsuario int
			declare @ultimaChaveTabela int
			declare @ultimaChaveNovoGrupo int
			declare @ultimaChavePerfil int
			declare @WhereNovoUsuario nvarchar(max)
			declare @valoresCampos nvarchar(max)
			declare @valoresCamposSet nvarchar(max)

			/*Adicionar Perfis ao usuário 1672 - flsantos*/
			if not EXISTS(Select 1 From UsuariosxPerfis Where USP_CdiUsuario = 1672 and USP_CdiPerfil in (217, 218, 220, 222, 382))
			begin
			  exec sp_GetLastIdFromTable 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil', 0, @ultimaChavePerfil OUTPUT
			  exec sp_Execute_Insert_Key 'dbo', 01, 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil, USP_CdiUsuario, USP_CdiPerfil', @ultimaChavePerfil, 01, '1672, 217', 1  
			  exec sp_Execute_Insert_Key 'dbo', 02, 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil, USP_CdiUsuario, USP_CdiPerfil', @ultimaChavePerfil, 02, '1672, 218', 1  
			  exec sp_Execute_Insert_Key 'dbo', 03, 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil, USP_CdiUsuario, USP_CdiPerfil', @ultimaChavePerfil, 03, '1672, 220', 1  
			  exec sp_Execute_Insert_Key 'dbo', 04, 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil, USP_CdiUsuario, USP_CdiPerfil', @ultimaChavePerfil, 04, '1672, 222', 1
			  exec sp_Execute_Insert_Key 'dbo', 05, 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil, USP_CdiUsuario, USP_CdiPerfil', @ultimaChavePerfil, 05, '1672, 382', 1
			end

		    if not EXISTS(Select 1 From GruposUsuarios where GUS_D1sGrupoUsuario = 'Apdata - Programação - Special Login')
			begin
			  /*Grupo para login especial da apdata*/
			  exec sp_DuplicarRegistroComAlteracoes 'GruposUsuarios', 'GUS_CdiGrupoUsuario', 1019, 'GUS_D1sGrupoUsuario, GUS_CdiOpcao_AtivaIntegracao, GUS_CdiTipoAutenticacao, GUS_CdiOpcao_IntegraViaWS', '''Apdata - Programação - Special Login'', 1, 2, 1', @ultimaChaveNovoGrupo OUTPUT
			end
			else
			begin
			  Select @ultimaChaveNovoGrupo = GUS_CdiGrupoUsuario
			  From GruposUsuarios
			  Where GUS_D1sGrupoUsuario = 'Apdata - Programação - Special Login';
			end

		    if not EXISTS(Select 1 From Usuarios Where USR_CdsUsuario = 'flsantos@apdata.com.br')
			begin
			  /*Cria o usuario flsantos@apdata.com.br*/
			  exec sp_DuplicarRegistroComAlteracoes 'Usuarios', 'USR_CdiUsuario', 1672, 'USR_CdsUsuario, USR_CosEMail, USR_DssNomeCompletoPessoa', '''flsantos@apdata.com.br'', ''flsantos@apdata.com.br'', ''Flsantos Apdata Com Br''', @ultimaChaveUsuario OUTPUT

			  /*Adiciona o grupo ao novo usuario*/
			  set @valoresCamposSet = 'USR_CdiGrupoUsuario = ' + CAST(@ultimaChaveNovoGrupo AS NVARCHAR(20))
			  set @WhereNovoUsuario = 'USR_CdiUsuario = ' + CAST(@ultimaChaveUsuario AS NVARCHAR(20))
			  exec sp_Execute_Update 'dbo', 06, 'Usuarios', @valoresCamposSet, @WhereNovoUsuario

			  /*Adicionnar Usuarios Contratados*/
			  set @valoresCampos = CAST(@ultimaChaveUsuario AS NVARCHAR(20)) + ',0'
			  exec sp_Execute_Insert 'dbo', 07, 'UsuariosContratados', 'USC_CdiUsuario, USC_CdiContratado_Usuario', @valoresCampos, 1 

			  /*Adicionar Perfis*/
			  exec sp_GetLastIdFromTable 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil', 0, @ultimaChavePerfil OUTPUT
			  exec sp_Execute_Insert_Key_ForeignKey 'dbo', 08, 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil, USP_CdiUsuario, USP_CdiPerfil', @ultimaChavePerfil, 01, @ultimaChaveUsuario, 0, '217', 1  
			  exec sp_Execute_Insert_Key_ForeignKey 'dbo', 09, 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil, USP_CdiUsuario, USP_CdiPerfil', @ultimaChavePerfil, 02, @ultimaChaveUsuario, 0, '218', 1  
			  exec sp_Execute_Insert_Key_ForeignKey 'dbo', 10, 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil, USP_CdiUsuario, USP_CdiPerfil', @ultimaChavePerfil, 03, @ultimaChaveUsuario, 0, '220', 1  
			  exec sp_Execute_Insert_Key_ForeignKey 'dbo', 11, 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil, USP_CdiUsuario, USP_CdiPerfil', @ultimaChavePerfil, 04, @ultimaChaveUsuario, 0, '222', 1
			  exec sp_Execute_Insert_Key_ForeignKey 'dbo', 12, 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil, USP_CdiUsuario, USP_CdiPerfil', @ultimaChavePerfil, 05, @ultimaChaveUsuario, 0, '382', 1
			  
			  /*Cria MFA Teste Objeto: 7172*/
			  exec sp_GetLastIdFromTable 'ModFatoresAutenticacoes', 'JRZ_CdiModFatorAutenticacao', 1, @ultimaChaveTabela OUTPUT
			  
			  set @valoresCampos = CAST(@ultimaChaveTabela AS NVARCHAR(20)) + ',''Modelo Teste MFA'''
			  exec sp_Execute_Insert 'dbo', 13, 'ModFatoresAutenticacoes', 'JRZ_CdiModFatorAutenticacao, JRZ_D1sModFatorAutenticacao', @valoresCampos, 1

			  /*Atualiza tipo autenticação para teste Objeto: 2330*/
			  set @WhereNovoUsuario = 'USR_CdiUsuario = ' + CAST(@ultimaChaveUsuario AS NVARCHAR(20))
			  exec sp_Execute_Update 'dbo', 14, 'Usuarios', 'USR_CdiOpcao_AutenticacaoNativ = 2', @WhereNovoUsuario

			  /*Atualiza fator de autenticação*/
			  set @valoresCamposSet = 'USR_CdiModFatorAutenticacao = ' + CAST(@ultimaChaveTabela AS NVARCHAR(20))  
			  exec sp_Execute_Update 'dbo', 15, 'Usuarios', @valoresCamposSet, @WhereNovoUsuario

			  /*Cria tipo de fator de autenticação por e-mail*/
			  set @valoresCampos = CAST(@ultimaChaveTabela AS NVARCHAR(20)) + ',1,1'
			  			  
			  exec sp_GetLastIdFromTable 'ModFatoresAutenticacoesIts', 'JSB_CdiModFatorAutenticacaoIt', 1, @ultimaChaveTabela OUTPUT

			  set @valoresCampos = CAST(@ultimaChaveTabela AS NVARCHAR(20)) + ',' + @valoresCampos

			  exec sp_Execute_Insert 'dbo', 16, 'ModFatoresAutenticacoesIts', 'JSB_CdiModFatorAutenticacaoIt, JSB_CdiModFatorAutenticacao, JSB_CdiFatorAutenticacao, JSB_NuiOrdem', @valoresCampos, 1
			  
			  Print 'Criado o Grupo [' + CAST(@ultimaChaveNovoGrupo AS NVARCHAR(20)) + '] Adicionado flsantos@apdata.com.br'
			  Print 'Criação e adição do usuário flsantos@apdata.com.br para testes de MFA autenticação especial apdata.'
			end
			else
			  Print 'Usuário flsantos@apdata.com.br já existe. Operção não realizada.';

		    if not EXISTS(Select 1 From GruposUsuarios where GUS_D1sGrupoUsuario = 'Apdata - Programação - Login Integrator')
			begin
			  /*Grupo para login especial da apdata*/
			  exec sp_DuplicarRegistroComAlteracoes 'GruposUsuarios', 'GUS_CdiGrupoUsuario', 1019, 'GUS_D1sGrupoUsuario, GUS_CdiOpcao_AtivaIntegracao, GUS_CdiTipoAutenticacao, GUS_CdiOpcao_IntegraViaWS', '''Apdata - Programação - Login Integrator'', 0, 2, 0', @ultimaChaveNovoGrupo OUTPUT
			end
			else
			begin
			  Select @ultimaChaveNovoGrupo = GUS_CdiGrupoUsuario
			  From GruposUsuarios
			  Where GUS_D1sGrupoUsuario = 'Apdata - Programação - Login Integrator';
			end

		    if not EXISTS(Select 1 From Usuarios Where USR_CdsUsuario = 'flsantos@apdatatst.com.br')
			begin
			  exec sp_DuplicarRegistroComAlteracoes 'Usuarios', 'USR_CdiUsuario', 1672, 'USR_CdsUsuario, USR_CosEMail, USR_DssNomeCompletoPessoa', '''flsantos@apdatatst.com.br'', ''flsantos@apdatatst.com.br'', ''Flsantos ApdataTst Com Br''', @ultimaChaveUsuario OUTPUT

			  set @valoresCampos = CAST(@ultimaChaveUsuario AS NVARCHAR(20)) + ',0'
			  exec sp_Execute_Insert 'dbo', 17, 'UsuariosContratados', 'USC_CdiUsuario, USC_CdiContratado_Usuario', @valoresCampos , 1
			  
			  set @valoresCamposSet = 'USR_CdiGrupoUsuario = ' + CAST(@ultimaChaveNovoGrupo AS NVARCHAR(20))
			  set @WhereNovoUsuario = 'USR_CdiUsuario = ' + CAST(@ultimaChaveUsuario AS NVARCHAR(20))
			  exec sp_Execute_Update 'dbo', 18, 'Usuarios', @valoresCamposSet, @WhereNovoUsuario
			  
			  exec sp_GetLastIdFromTable 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil', 0, @ultimaChavePerfil OUTPUT
			  exec sp_Execute_Insert_Key_ForeignKey 'dbo', 19, 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil, USP_CdiUsuario, USP_CdiPerfil', @ultimaChavePerfil, 01, @ultimaChaveUsuario, 0, '217'  
			  exec sp_Execute_Insert_Key_ForeignKey 'dbo', 20, 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil, USP_CdiUsuario, USP_CdiPerfil', @ultimaChavePerfil, 02, @ultimaChaveUsuario, 0, '218'  
			  exec sp_Execute_Insert_Key_ForeignKey 'dbo', 21, 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil, USP_CdiUsuario, USP_CdiPerfil', @ultimaChavePerfil, 03, @ultimaChaveUsuario, 0, '220'  
			  exec sp_Execute_Insert_Key_ForeignKey 'dbo', 22, 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil, USP_CdiUsuario, USP_CdiPerfil', @ultimaChavePerfil, 04, @ultimaChaveUsuario, 0, '222'
			  exec sp_Execute_Insert_Key_ForeignKey 'dbo', 23, 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil, USP_CdiUsuario, USP_CdiPerfil', @ultimaChavePerfil, 05, @ultimaChaveUsuario, 0, '382'

			  Print 'Criado o Grupo [' + CAST(@ultimaChaveNovoGrupo AS NVARCHAR(20)) + '] Adicionado flsantos@apdatatst.com.br'
			  Print 'Criação e adição do usuário flsantos@apdatatst.com.br para testes de no servidor http://172.26.100.149:7080/ADIDebug/ApADIntegratorWS.dll/soap/IApADIntegrationIntf.'
			end
			else
			  Print 'Usuário flsantos@apdatatst.com.br já existe. Operção não realizada.';
		/*2330 - CRIAÇÃO DE NOVOS USUÁRIOS PARA TESTES - END*/
		
	    /*-> ADINTEGRATOR - ACTIVE DIRECTORY -INICIO */
			--exec sp_takeKeyForInsertion 'DefSisIntegracaoAD', @AuxKey OUTPUT
			--exec sp_Execute_Insert_Key 'dbo', 01, 'DefSisIntegracaoAD', 'DZW_CdiSistema, DZW_DtdOficializacaoSistema, DZW_OplAtivaIntegracao, DZW_OplCriacaoUsuarioAut, DZW_DssCaminhoLDAP, DZW_OplIntegraViaWS, DZW_DssWSCriaUsuario, DZW_DssWSAtualizaDados, DZW_DssWSTrocaSenha, DZW_DssWSResetaSenha, DZW_DssWSAtivaDesativaUsuario, DZW_CdsWSUsuario, DZW_CosWSSenha, DZW_OplAtivaLogIntegracao, DZW_OplNaoSincronizarGrupo, DZW_OplNaoSincronizarEstrutura, DZW_DssWSValidaLogin, DZW_DssWSTrataSSO', @AuxKey/*72*/, 0,  'null, 1, 0, ''LDAP://DC=apdatatst,DC=com,DC=br'', 1, ''http://172.26.100.149:7080/ADIDebug/ApADIntegratorWS.dll/soap/IApADIntegrationIntf'', ''http://172.26.100.149:7080/ADIDebug/ApADIntegratorWS.dll/soap/IApADIntegrationIntf'', null, null, ''http://172.26.100.149:7080/ADIDebug/ApADIntegratorWS.dll/soap/IApADIntegrationIntf'', ''flsantos'', ''Fls12345@'', 1, 0, 0, null, null', 1

		/*Objeto 3090*/
			exec sp_Execute_Update 'dbo', '01', 'DefSisIntegracaoAD', 'DZW_DtdOficializacaoSistema = null, DZW_OplAtivaIntegracao = 1, DZW_OplCriacaoUsuarioAut =  0, DZW_DssCaminhoLDAP = ''DC=apdatatst,DC=com,DC=br'', DZW_OplIntegraViaWS = 1, DZW_DssWSCriaUsuario = ''http://172.26.100.149:7080/ADIDebug/ApADIntegratorWS.dll/soap/IApADIntegrationIntf'', DZW_DssWSAtualizaDados = ''http://172.26.100.149:7080/ADIDebug/ApADIntegratorWS.dll/soap/IApADIntegrationIntf'', DZW_DssWSTrocaSenha = ''http://172.26.100.149:7080/ADIDebug/ApADIntegratorWS.dll/soap/IApADIntegrationIntf'', DZW_DssWSResetaSenha = ''http://172.26.100.149:7080/ADIDebug/ApADIntegratorWS.dll/soap/IApADIntegrationIntf'', DZW_DssWSAtivaDesativaUsuario = ''http://172.26.100.149:7080/ADIDebug/ApADIntegratorWS.dll/soap/IApADIntegrationIntf'', DZW_CdsWSUsuario = ''flsantos'', DZW_CosWSSenha = ''Fls12345@'', DZW_OplAtivaLogIntegracao = 1, DZW_OplNaoSincronizarGrupo = 0, DZW_OplNaoSincronizarEstrutura = 0, DZW_DssWSValidaLogin = ''http://172.26.100.149:7080/ADIDebug/ApADIntegratorWS.dll/soap/IApADIntegrationIntf'', DZW_DssWSTrataSSO = ''http://172.26.100.149:7080/ADIDebug/ApADIntegratorWS.dll/soap/IApADIntegrationIntf''', 'DZW_CdiSistema = 72', 1
			--Para ativar a integração da ApData marcar a configuração da seguinte forma [GUS_CdiOpcao_AtivaIntegracao = 1, GUS_CdiTipoAutenticacao = 2, GUS_CdiOpcao_IntegraViaWS = 1]
			--exec sp_Execute_Update 'dbo', '02', 'GruposUsuarios', 'GUS_CdiOpcao_AtivaIntegracao = 1, GUS_CdiTipoAutenticacao = 2, GUS_CdiOpcao_IntegraViaWS = 1', 'GUS_CdiGrupoUsuario = 1019', 1
			
			--Para ativar a validação da apdata [usuários com nome e-mail]
			exec sp_Execute_Or_Insert 'dbo', 01, 'IdentificacoesApServer', 'EON_DssNomeMaquina', '''APDNSON0220''', 'EON_CdiIdentificacaoApServer', 'EON_DssNomeMaquina, EON_DssNomeInstancia, EON_DssLinkADIntegratorWS', '''APDNSON0220'',''localhost'',''https://apad.apdata.com.br/aPAD/ApADIntegratorWS.dll/soap/IApADIntegrationIntf''', 1

			declare @EstruturasAD int
			declare @EstruturasADProps int
			declare @EstruturasADGroup int
			declare @EstruturasADxSitsAtivs int

		/*Objeto - 3091 - Criação do grupo básico para realização de testes*/
			exec sp_takeKeyForInsertion 'EstruturasAD', @EstruturasAD OUTPUT
			exec sp_takeKeyForInsertion 'EstruturasADProps', @EstruturasADProps OUTPUT
			exec sp_takeKeyForInsertion 'EstruturasADGrupos', @EstruturasADGroup OUTPUT
			exec sp_takeKeyForInsertion 'EstruturasADxSitsAtivs', @EstruturasADxSitsAtivs OUTPUT

			/*3091 - Configuração - Básica*/
				exec sp_Execute_Insert_Key 'dbo', 01, 'EstruturasAD', 'DZY_CdiEstruturaAD, DZY_D1sDescricaoEstruturaAD, DZY_D2sDescricaoEstruturaAD, DZY_D3sDescricaoEstruturaAD, DZY_D4sDescricaoEstruturaAD, DZY_D5sDescricaoEstruturaAD, DZY_D6sDescricaoEstruturaAD, DZY_D7sDescricaoEstruturaAD, DZY_D8sDescricaoEstruturaAD, DZY_CdiDefault, DZY_OplSemFiltro, DZY_NuiOrdem, DZY_DssCaminhoLDAP, DZY_OplIgnorarEstrutsSup, DZY_OplNaoIntegrar', @EstruturasAD/*1002*/, 0, '''(TESTE) CONFIGURACAO BASICA'', null, null, null, null, null, null, null, 1, 0, 2, ''OU=ProdutoTestes'', 0, 0', 1 

				exec sp_Execute_Insert_Key_ForeignKey 'dbo', 02, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor, EBC_CdiComandoSQL', @EstruturasADProps, 0, @EstruturasAD, 0, '105848, 38, 0, 0', 1   
				exec sp_Execute_Insert_Key_ForeignKey 'dbo', 03, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor, EBC_CdiComandoSQL', @EstruturasADProps, 1, @EstruturasAD, 0, '91403, 7, 0, 0', 1
				exec sp_Execute_Insert_Key_ForeignKey 'dbo', 04, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor, EBC_CdiComandoSQL', @EstruturasADProps, 2, @EstruturasAD, 0, '96978, 39, 0, 0', 1
				exec sp_Execute_Insert_Key_ForeignKey 'dbo', 05, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor, EBC_CdiComandoSQL', @EstruturasADProps, 3, @EstruturasAD, 0, '9420, 35, 1, 0', 1
			
				exec sp_Execute_Insert_ThreeKey 'dbo', 06, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiComandoSQL, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor', @EstruturasADProps, 4, @EstruturasAD, 0, @SQL_CdiComandoSQL, 11, '89918, 6, 0', 1
				exec sp_Execute_Insert_ThreeKey 'dbo', 07, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiComandoSQL, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor', @EstruturasADProps, 5, @EstruturasAD, 0, @SQL_CdiComandoSQL, 10, '0, 3, 0', 1
				exec sp_Execute_Insert_ThreeKey 'dbo', 08, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiComandoSQL, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor', @EstruturasADProps, 6, @EstruturasAD, 0, @SQL_CdiComandoSQL, 11, '0, 0, 0', 1

				exec sp_Execute_Insert_Key_ForeignKey 'dbo', 09, 'EstruturasADGrupos', 'DZZ_CdiEstruturaAdGrupo, DZZ_CdiEstruturaAD, DZZ_D1sDssGrupoUsuario ', @EstruturasADGroup, 1, @EstruturasAD, 0, '''Provisorio''', 1  

				exec sp_Execute_Insert_Key_ForeignKey 'dbo', 10, 'EstruturasADxSitsAtivs', 'EBA_CdiEstruturaADSitAtiv, EBA_CdiEstruturaAD, EBA_CdiSituacao ', @EstruturasADxSitsAtivs, 0, @EstruturasAD, 0, '1', 1   

			/*3091 - Configuração - Para Valores Default 1*/
				exec sp_Execute_Insert_Key 'dbo', 11, 'EstruturasAD', 'DZY_CdiEstruturaAD, DZY_D1sDescricaoEstruturaAD, DZY_D2sDescricaoEstruturaAD, DZY_D3sDescricaoEstruturaAD, DZY_D4sDescricaoEstruturaAD, DZY_D5sDescricaoEstruturaAD, DZY_D6sDescricaoEstruturaAD, DZY_D7sDescricaoEstruturaAD, DZY_D8sDescricaoEstruturaAD, DZY_CdiDefault, DZY_OplSemFiltro, DZY_NuiOrdem, DZY_DssCaminhoLDAP, DZY_OplIgnorarEstrutsSup, DZY_OplNaoIntegrar', @EstruturasAD, 1, '''(TESTE) CONFIGURACAO VALOR PRE-DEFINIDO UM'', null, null, null, null, null, null, null, 14, 0, 1, ''OU=Contratados'', 0, 0', 1 

				exec sp_Execute_Insert_Key_ForeignKey 'dbo', 12, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor, EBC_CdiComandoSQL', @EstruturasADProps, 7,  @EstruturasAD, 1, '105848, 38, 0, 0', 1   
				exec sp_Execute_Insert_Key_ForeignKey 'dbo', 13, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor, EBC_CdiComandoSQL', @EstruturasADProps, 8,  @EstruturasAD, 1, '91403, 7, 0, 0', 1
				exec sp_Execute_Insert_Key_ForeignKey 'dbo', 14, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor, EBC_CdiComandoSQL', @EstruturasADProps, 9,  @EstruturasAD, 1, '96978, 39, 0, 0', 1
				exec sp_Execute_Insert_Key_ForeignKey 'dbo', 15, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor, EBC_CdiComandoSQL', @EstruturasADProps, 10, @EstruturasAD, 1, '9420, 35, 1, 0', 1
			
				exec sp_Execute_Insert_ThreeKey 'dbo', 16, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiComandoSQL, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor', @EstruturasADProps, 11, @EstruturasAD, 1, @SQL_CdiComandoSQL, 11, '89918, 6, 0', 1
				exec sp_Execute_Insert_ThreeKey 'dbo', 17, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiComandoSQL, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor', @EstruturasADProps, 12, @EstruturasAD, 1, @SQL_CdiComandoSQL, 10, '0, 3, 0', 1
				exec sp_Execute_Insert_ThreeKey 'dbo', 18, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiComandoSQL, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor', @EstruturasADProps, 13, @EstruturasAD, 1, @SQL_CdiComandoSQL, 11, '0, 0, 0', 1

				exec sp_Execute_Insert_Key_ForeignKey 'dbo', 19, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor, EBC_CdiComandoSQL', @EstruturasADProps, 14, @EstruturasAD, 1, '10025, 47, 0, 0', 1

				exec sp_Execute_Insert_Key_ForeignKey 'dbo', 20, 'EstruturasADGrupos', 'DZZ_CdiEstruturaAdGrupo, DZZ_CdiEstruturaAD, DZZ_D1sDssGrupoUsuario ', @EstruturasADGroup, 3, @EstruturasAD, 1, '''Programadores''', 1  

				exec sp_Execute_Insert_Key_ForeignKey 'dbo', 21, 'EstruturasADxSitsAtivs', 'EBA_CdiEstruturaADSitAtiv, EBA_CdiEstruturaAD, EBA_CdiSituacao ', @EstruturasADxSitsAtivs, 1, @EstruturasAD, 1, '1', 1   

			/*3091 - Configuração - Para Valores Default 2*/
				exec sp_Execute_Insert_Key 'dbo', 21, 'EstruturasAD', 'DZY_CdiEstruturaAD, DZY_D1sDescricaoEstruturaAD, DZY_D2sDescricaoEstruturaAD, DZY_D3sDescricaoEstruturaAD, DZY_D4sDescricaoEstruturaAD, DZY_D5sDescricaoEstruturaAD, DZY_D6sDescricaoEstruturaAD, DZY_D7sDescricaoEstruturaAD, DZY_D8sDescricaoEstruturaAD, DZY_CdiDefault, DZY_OplSemFiltro, DZY_NuiOrdem, DZY_DssCaminhoLDAP, DZY_OplIgnorarEstrutsSup, DZY_OplNaoIntegrar', @EstruturasAD, 2, '''(TESTE) CONFIGURACAO VALOR PRE-DEFINIDO DOIS'', null, null, null, null, null, null, null, 15, 0, 1, ''OU=Gerentes'', 0, 0', 1 

				exec sp_Execute_Insert_Key_ForeignKey 'dbo', 22, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor, EBC_CdiComandoSQL', @EstruturasADProps, 15, @EstruturasAD, 2, '105848, 38, 0, 0', 1   
				exec sp_Execute_Insert_Key_ForeignKey 'dbo', 23, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor, EBC_CdiComandoSQL', @EstruturasADProps, 16, @EstruturasAD, 2, '91403, 7, 0, 0', 1
				exec sp_Execute_Insert_Key_ForeignKey 'dbo', 24, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor, EBC_CdiComandoSQL', @EstruturasADProps, 17, @EstruturasAD, 2, '96978, 39, 0, 0', 1
				exec sp_Execute_Insert_Key_ForeignKey 'dbo', 25, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor, EBC_CdiComandoSQL', @EstruturasADProps, 18, @EstruturasAD, 2, '9420, 35, 1, 0', 1
			
				exec sp_Execute_Insert_ThreeKey 'dbo', 26, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiComandoSQL, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor', @EstruturasADProps, 19, @EstruturasAD, 2, @SQL_CdiComandoSQL, 11, '89918, 6, 0', 1
				exec sp_Execute_Insert_ThreeKey 'dbo', 27, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiComandoSQL, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor', @EstruturasADProps, 20, @EstruturasAD, 2, @SQL_CdiComandoSQL, 10, '0, 3, 0', 1
				exec sp_Execute_Insert_ThreeKey 'dbo', 28, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiComandoSQL, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor', @EstruturasADProps, 21, @EstruturasAD, 2, @SQL_CdiComandoSQL, 11, '0, 0, 0', 1

				exec sp_Execute_Insert_Key_ForeignKey 'dbo', 29, 'EstruturasADGrupos', 'DZZ_CdiEstruturaAdGrupo, DZZ_CdiEstruturaAD, DZZ_D1sDssGrupoUsuario ', @EstruturasADGroup, 5, @EstruturasAD, 2, '''Gerentes''', 1  

				exec sp_Execute_Insert_Key_ForeignKey 'dbo', 30, 'EstruturasADxSitsAtivs', 'EBA_CdiEstruturaADSitAtiv, EBA_CdiEstruturaAD, EBA_CdiSituacao ', @EstruturasADxSitsAtivs, 2, @EstruturasAD, 2, '1', 1   
		/*ADINTEGRATOR - ACTIVE DIRECTORY - FIM*/

		exec sp_Create_Aux_Table;
	commit;
end
GO

/**********************************************************************
  9 - Generate Insert From Select Table
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
  10 - Generate Insert From Select Table
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
    11 - Convert Binary To Text
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
    12 - SELECT INTO 
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
    16 - CRIA USUARIO
***********************************************************************/


create or alter procedure sp_CriarUsuario
(
    @NomeUsuario        NVARCHAR(100),  -- 'jmenotti'
    @BaseUsuarioID      INT = 1672,     -- usuário que será duplicado
    @NovoGrupoID        INT = NULL,     -- se precisar definir explicitamente o grupo do usuário
    @Email              NVARCHAR(200) = NULL,  -- se quiser parametrizar email
    @NomeCompleto       NVARCHAR(200) = NULL   -- se quiser parametrizar nome completo
)
AS
BEGIN
    SET NOCOUNT ON;

    DECLARE @ultimaChaveUsuario      INT,
            @ultimaChaveTabela       INT,
            @ultimaChaveNovoGrupo    INT,
            @ultimaChavePerfil       INT,
            @WhereNovoUsuario        NVARCHAR(MAX),
            @valoresCampos           NVARCHAR(MAX),
            @valoresCamposSet        NVARCHAR(MAX);

    -------------------------------------------------------------------------
    -- Se @NovoGrupoID não for passado, você pode setar algum valor default
    -- ou buscá-lo de alguma tabela. Exemplo (opcional):
    -- SET @NovoGrupoID = ISNULL(@NovoGrupoID, 9999); 
    -------------------------------------------------------------------------

    IF NOT EXISTS (SELECT 1 FROM Usuarios WHERE USR_CdsUsuario = @NomeUsuario)
    BEGIN
        ---------------------------------------------------------------------
        -- 1) Duplicar registro base (ex: id 1672) e gerar novo usuário
        ---------------------------------------------------------------------
        -- Exemplo de construção dinâmica dos campos a serem alterados:
        DECLARE @CamposAlterar  NVARCHAR(MAX) = 'USR_CdsUsuario, USR_CosEMail, USR_DssNomeCompletoPessoa';
        DECLARE @ValoresAlterar NVARCHAR(MAX);

        SET @ValoresAlterar =  '''' + @NomeUsuario + ''', '
                            + COALESCE('''' + @Email + '''', '''jmenotti''') + ', ' 
                            + COALESCE('''' + @NomeCompleto + '''', '''Criação de usuários''');

        EXEC sp_DuplicarRegistroComAlteracoes
              @TableName              = 'Usuarios',
              @PrimaryKeyColumn       = 'USR_CdiUsuario',
              @PrimaryKeyValue        = @BaseUsuarioID,            -- 1672
              @CamposAlterar          = @CamposAlterar, 
              @NovosValores           = @ValoresAlterar, 
              @NovoValorChavePrimaria = @ultimaChaveUsuario OUTPUT;

        ---------------------------------------------------------------------
        -- 2) Inserir registro na tabela UsuariosContratados
        ---------------------------------------------------------------------
        SET @valoresCampos = CAST(@ultimaChaveUsuario AS NVARCHAR(20)) + ',0';
        EXEC sp_Execute_Insert 
              @schema         = 'dbo',
              @ordNum         = 17,
              @table          = 'UsuariosContratados',
              @fields         = 'USC_CdiUsuario, USC_CdiContratado_Usuario',
              @values         = @valoresCampos,
			  @showCmd        = 1,
              @customMsgError = '';


        ---------------------------------------------------------------------
        -- 3) Atualizar o grupo do novo usuário (USR_CdiGrupoUsuario)
        --    Obs.: aqui estamos usando @ultimaChaveNovoGrupo. 
        --    Se for o valor de @NovoGrupoID, use-o. Se não, defina algum valor.
        ---------------------------------------------------------------------
        SET @ultimaChaveNovoGrupo = ISNULL(@NovoGrupoID, 9999);  -- Exemplo de fallback
        SET @valoresCamposSet = 'USR_CdiGrupoUsuario = ' + CAST(@ultimaChaveNovoGrupo AS NVARCHAR(20));
        SET @WhereNovoUsuario = 'USR_CdiUsuario = ' + CAST(@ultimaChaveUsuario AS NVARCHAR(20));

        EXEC sp_Execute_Update
              @Esquema    = 'dbo',
              @IdAcao     = 18,
              @NomeTabela = 'Usuarios',
              @Campos     = @valoresCamposSet,
              @Where      = @WhereNovoUsuario;

        ---------------------------------------------------------------------
        -- 4) Inserir perfis (UsuariosXPerfis) para o novo usuário
        ---------------------------------------------------------------------
        EXEC sp_GetLastIdFromTable 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil', 0, @ultimaChavePerfil OUTPUT;

        -- Exemplo: inserindo vários perfis (217,218,220,222,382).
        -- Note que no seu script, há 5 inserts, cada um com um ID_Acao diferente (19, 20, 21, 22, 23).
        EXEC sp_Execute_Insert_Key_ForeignKey 'dbo', 19, 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil, USP_CdiUsuario, USP_CdiPerfil',
                                              @ultimaChavePerfil, 01, @ultimaChaveUsuario, 0, '217';
        EXEC sp_Execute_Insert_Key_ForeignKey 'dbo', 20, 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil, USP_CdiUsuario, USP_CdiPerfil',
                                              @ultimaChavePerfil, 02, @ultimaChaveUsuario, 0, '218';
        EXEC sp_Execute_Insert_Key_ForeignKey 'dbo', 21, 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil, USP_CdiUsuario, USP_CdiPerfil',
                                              @ultimaChavePerfil, 03, @ultimaChaveUsuario, 0, '220';
        EXEC sp_Execute_Insert_Key_ForeignKey 'dbo', 22, 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil, USP_CdiUsuario, USP_CdiPerfil',
                                              @ultimaChavePerfil, 04, @ultimaChaveUsuario, 0, '222';
        EXEC sp_Execute_Insert_Key_ForeignKey 'dbo', 23, 'UsuariosXPerfis', 'USP_CdiUsuarioxPerfil, USP_CdiUsuario, USP_CdiPerfil',
                                              @ultimaChavePerfil, 05, @ultimaChaveUsuario, 0, '382';

        ---------------------------------------------------------------------
        -- 5) Mensagens para depuração/log
        ---------------------------------------------------------------------
        PRINT 'Criado o Grupo [' + CAST(@ultimaChaveNovoGrupo AS NVARCHAR(20)) + '] e adicionado o usuário ' + @NomeUsuario;
        PRINT 'Criação e adição do usuário ' + @NomeUsuario + ' para testes no servidor http://172.26.100.149:7080/ADIDebug/ApADIntegratorWS.dll/soap/IApADIntegrationIntf.';
    END
    ELSE
    BEGIN
        PRINT 'O usuário ' + @NomeUsuario + ' já existe na tabela [Usuarios]. Nenhuma ação foi executada.';
    END;
END;
GO

/**********************************************************************
    17 - EXPORTA NO FORMATO SP_EXECUTE_INSERT
***********************************************************************/

CREATE OR ALTER PROCEDURE sp_Generate_Exec_Insert
(
    @table VARCHAR(200),
    @where VARCHAR(MAX) = NULL,
    @fields VARCHAR(MAX) = NULL,
    @ordNum INT = 0,
    @showCmd INT = 0
)
AS
BEGIN
    SET NOCOUNT ON;
    
    DECLARE @insert VARCHAR(MAX);
    DECLARE @schema VARCHAR(200) = 'dbo';
    DECLARE @tempTable TABLE (LineText VARCHAR(MAX));
    
    -- Usar tabela temporária para capturar a saída
    DECLARE @outputTable TABLE (LineText VARCHAR(MAX));
    
    -- Gerar os INSERTs básicos diretamente na tabela temporária
	declare @lineResult varchar(max);
    
	exec sp_Generate_Inserts_From_Selects @schema = 'dbo',
		                                  @table  = @table,
			                              @where  = @where,
										  @fields = @fields,
				                          @insert = @lineResult output;

   print @lineResult

   INSERT INTO @outputTable select @lineResult

    -- Processar cada linha para formatar como sp_Execute_Insert
   DECLARE @processedResults TABLE (FormattedLine VARCHAR(MAX));
    
   DECLARE @currentLine VARCHAR(MAX);
   DECLARE @tableName VARCHAR(200);
   DECLARE @fieldsList VARCHAR(MAX);
   DECLARE @valuesList VARCHAR(MAX);
    
    -- Extrair informações de cada INSERT
   DECLARE line_cursor CURSOR FOR 
   SELECT LineText FROM @outputTable 
   WHERE LineText LIKE '%insert into dbo.%' OR LineText LIKE 'insert into %values%';
    
    OPEN line_cursor;
    FETCH NEXT FROM line_cursor INTO @currentLine;
    
    WHILE @@FETCH_STATUS = 0
    BEGIN
        -- Extrair nome da tabela
        SET @tableName = SUBSTRING(@currentLine, 
                                  CHARINDEX('.', @currentLine) + 1,
                                  CHARINDEX('(', @currentLine) - CHARINDEX('.', @currentLine) - 1);
        SET @tableName = REPLACE(@tableName, CHAR(13), '');
        SET @tableName = REPLACE(@tableName, CHAR(10), '');
        SET @tableName = LTRIM(RTRIM(@tableName));
        
        -- Extrair lista de campos
        SET @fieldsList = SUBSTRING(@currentLine,
                                   CHARINDEX('(', @currentLine) + 1,
                                   CHARINDEX(')', @currentLine) - CHARINDEX('(', @currentLine) - 1);
        SET @fieldsList = REPLACE(@fieldsList, CHAR(13), '');
        SET @fieldsList = REPLACE(@fieldsList, CHAR(10), '');
        SET @fieldsList = LTRIM(RTRIM(@fieldsList));
        
        -- Extrair lista de valores
        SET @valuesList = SUBSTRING(@currentLine,
                                   CHARINDEX('values(', @currentLine) + 7,
                                   CHARINDEX(');', @currentLine) - CHARINDEX('values(', @currentLine) - 7);
        SET @valuesList = REPLACE(@valuesList, CHAR(13), '');
        SET @valuesList = REPLACE(@valuesList, CHAR(10), '');
        SET @valuesList = LTRIM(RTRIM(@valuesList));
        
        -- Processar campos e valores para remover nulos/zerados
        DECLARE @filteredFields VARCHAR(MAX) = '';
        DECLARE @filteredValues VARCHAR(MAX) = '';
        
        DECLARE @field VARCHAR(200);
        DECLARE @value VARCHAR(MAX);
        DECLARE @pos INT;
        DECLARE @nextPos INT;
        DECLARE @commaPos INT;
        
        -- Processar cada par campo/valor
        WHILE LEN(@fieldsList) > 0 AND LEN(@valuesList) > 0
        BEGIN
            -- Extrair próximo campo
            SET @commaPos = CHARINDEX(',', @fieldsList);
            IF @commaPos = 0
                SET @commaPos = LEN(@fieldsList) + 1;
                
            SET @field = SUBSTRING(@fieldsList, 1, @commaPos - 1);
            SET @field = LTRIM(RTRIM(@field));
            
            -- Extrair próximo valor correspondente
            SET @pos = 1;
            SET @nextPos = 1;
            DECLARE @inQuotes BIT = 0;
            DECLARE @char CHAR(1);
            
            WHILE @nextPos <= LEN(@valuesList)
            BEGIN
                SET @char = SUBSTRING(@valuesList, @nextPos, 1);
                
                IF @char = '''' 
                    SET @inQuotes = ~@inQuotes;
                ELSE IF @char = ',' AND @inQuotes = 0
                    BREAK;
                    
                SET @nextPos = @nextPos + 1;
            END
            
            IF @nextPos > LEN(@valuesList)
                SET @nextPos = LEN(@valuesList) + 1;
                
            SET @value = SUBSTRING(@valuesList, 1, @nextPos - 1);
            SET @value = LTRIM(RTRIM(@value));
            
            -- Verificar se o valor não é nulo ou zero
            IF @value NOT IN ('null', '0', '''''')
            BEGIN
                IF LEN(@filteredFields) > 0
                BEGIN
                    SET @filteredFields = @filteredFields + ', ';
                    SET @filteredValues = @filteredValues + ', ';
                END
                
                SET @filteredFields = @filteredFields + @field;
                SET @filteredValues = @filteredValues + @value;
            END
            
            -- Remover campo e valor processados
            IF @commaPos <= LEN(@fieldsList)
                SET @fieldsList = SUBSTRING(@fieldsList, @commaPos + 1, LEN(@fieldsList) - @commaPos);
            ELSE
                SET @fieldsList = '';
                
            IF @nextPos <= LEN(@valuesList)
                SET @valuesList = SUBSTRING(@valuesList, @nextPos + 1, LEN(@valuesList) - @nextPos);
            ELSE
                SET @valuesList = '';
        END
        
		print @tableName

        DECLARE @origTable VARCHAR(MAX) = @tableName;

		SET @pos = CHARINDEX('dbo.', LOWER(@origTable));

        IF @pos > 0
          SET @origTable = SUBSTRING(@origTable, @pos + 4, LEN(@origTable));

        -- Gerar o comando formatado para sp_Execute_Insert
        DECLARE @formattedCmd VARCHAR(MAX);
        SET @formattedCmd = '/*REST*/ /*????: 00000, ????: 00000*/ exec sp_Execute_Insert ''' + @schema + ''', ' + 
                           CAST(@ordNum AS VARCHAR) + ', ''' + @origTable + ''', ''' + 
                           @filteredFields + ''', ''' + @filteredValues + ''', ' + 
                           CAST(@showCmd AS VARCHAR);
        
        INSERT INTO @processedResults VALUES (@formattedCmd);
        
        FETCH NEXT FROM line_cursor INTO @currentLine;
    END
    
    CLOSE line_cursor;
    DEALLOCATE line_cursor;
    
    -- Exibir resultados
    SELECT FormattedLine FROM @processedResults;
END
GO

/**********************************************************************
    13 - STRING SPLIT (COMPATIBILIDADE DE FUNCAO)
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

/**********************************************************************
    14 - CREATE FOLDER
***********************************************************************/
/*
CREATE PROCEDURE [dbo].[CreateFolder] (@newfolder varchar(1000)) AS  
BEGIN  
DECLARE @OLEfolder   INT  
DECLARE @OLEsource   VARCHAR(255)  
DECLARE @OLEdescription  VARCHAR(255) 
DECLARE @init   INT  
DECLARE @OLEfilesytemobject INT  
 
-- it will fail if OLE automation not enabled
EXEC @init=sp_OACreate 'Scripting.FileSystemObject', @OLEfilesytemobject OUT  
IF @init <> 0  
BEGIN  
	EXEC sp_OAGetErrorInfo @OLEfilesytemobject  
	RETURN  
END  
-- check if folder exists  
EXEC @init=sp_OAMethod @OLEfilesytemobject, 'FolderExists', @OLEfolder OUT, @newfolder  
-- if folder doesnt exist, create it  
IF @OLEfolder=0  
	BEGIN  
	EXEC @init=sp_OAMethod @OLEfilesytemobject, 'CreateFolder', @OLEfolder OUT, @newfolder  
END  
-- in case of error, raise it   
IF @init <> 0  
	BEGIN  
		EXEC sp_OAGetErrorInfo @OLEfilesytemobject, @OLEsource OUT, @OLEdescription OUT  
		SELECT @OLEdescription='Could not create folder: ' + @OLEdescription  
		RAISERROR (@OLEdescription, 16, 1)   
	END  
EXECUTE @init = sp_OADestroy @OLEfilesytemobject  
END
GO
*/

/**********************************************************************
    15 - EXPORT FILE
***********************************************************************/
/*
create or alter procedure sp_Export_Binary_To_File(@schema     varchar(200) = 'dbo',
                                                   @table      nvarchar(200),
                                                   @where      varchar(max) = null,
										           @fields     varchar(max) = null,
												   @outPutPath varchar(200)
                                                  )
as
begin
	declare @i bigint,
            @init int,
            @data        varbinary(max), 
            @fPath       varchar(max),  
            @folderPath  varchar(max) 
 
    --Get Data into temp Table variable so that we can iterate over it 
    declare @Doctable TABLE (id int identity(1,1), [Doc_Num]  varchar(100) , [FileName]  varchar(100), [Doc_Content] varBinary(max) )

	INSERT INTO @Doctable ([Doc_Num] , [FileName],[Doc_Content])
    Select @fields FROM @table Where @where
 
	--SELECT * FROM @table

	SELECT @i = COUNT(1) FROM @Doctable
 
	WHILE @i >= 1
	BEGIN 

		SELECT 
		@data = [Doc_Content],
		@fPath = @outPutPath + '\'+ [Doc_Num] + '\' +[FileName],
		@folderPath = @outPutPath + '\'+ [Doc_Num]
		FROM @Doctable WHERE id = @i
 
	--Create folder first
	EXEC  [dbo].[CreateFolder]  @folderPath
  
	EXEC sp_OACreate 'ADODB.Stream', @init OUTPUT; -- An instace created
	EXEC sp_OASetProperty @init, 'Type', 1;  
	EXEC sp_OAMethod @init, 'Open'; -- Calling a method
	EXEC sp_OAMethod @init, 'Write', NULL, @data; -- Calling a method
	EXEC sp_OAMethod @init, 'SaveToFile', NULL, @fPath, 2; -- Calling a method
	EXEC sp_OAMethod @init, 'Close'; -- Calling a method
	EXEC sp_OADestroy @init; -- Closed the resources
 
	print 'Document Generated at - '+  @fPath   

	--Reset the variables for next use
	SELECT @data = NULL  
	, @init = NULL
	, @fPath = NULL  
	, @folderPath = NULL
	SET @i -= 1
	END
end
GO
*/

IF DB_NAME() = 'INTEGRATION_BETA'
begin
	exec sp_StandardData_FixedValues 0
end;
GO	

