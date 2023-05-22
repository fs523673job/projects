use INTEGRATION_BETA
GO

drop procedure sp_deleteCascate 
GO
drop procedure sp_deleteCascateRegistry
GO
drop procedure sp_StandardData_FixedValues 
GO
drop procedure sp_delete 
GO
drop procedure sp_lastIdTable 
GO
drop procedure sp_takeKeyForInsertion 
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
drop procedure sp_Execute_Insert
GO
drop procedure sp_Execute_Insert_Key
GO
drop procedure sp_Execute_Insert_Key_ForeignKey
GO
drop procedure sp_Execute_Update
GO
drop procedure sp_Execute_Insert_ThreeKey
GO
drop procedure sp_Execute_Delete
GO
drop procedure sp_getKeyValue
GO
drop procedure sp_getNewCriteria
GO
drop function fn_getPKFieldName 
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

	select @FieldPrimaryKey = [dbo].[fn_getPKFieldName](@TableName)

	set @sqlcommand = 
	  N'select @Value = ' + @FieldPrimaryKey + 
	   '  FROM ' + @TableName +
       '  WHERE ' + @Criteria
	    
	set @paramdefinition = N'@Value int OUTPUT' 

	exec sp_executesql @sqlcommand, @paramdefinition, @Value = @FieldKeyValue OUTPUT
	
	set @NewCriteria = @FieldPrimaryKey + ' = ' + cast(@FieldKeyValue as char(6))

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
	    set @NewCriteria = @RefField + ' = ' + trim(cast(@FieldKeyValue as char(6)))

		set @sqlcommand = 'DELETE FROM [' + @RefTable + '] WHERE ' + @NewCriteria

		if (( @auxRefTable <> @RefTable ) and (@RefTable <> @TableName))
		begin
			set @DeleteOrder = @DeleteOrder + 1
			exec sp_deleteCascateRegistry @RefTable, @NewCriteria, @OnlyStrCmd, @DeleteOrder
		end
		
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

	set @NewCriteria = @FieldPrimaryKey + ' = ' + trim(cast(@FieldKeyValue as char(6)))

	set @sqlcommand = 'DELETE FROM [' + @TableName + '] WHERE ' + @NewCriteria
	
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
	exec sp_deleteOptionByApDataRange 'EstruturasADProps', 1, 1
	exec sp_deleteOptionByApDataRange 'EstruturasAD', 1, 1
	exec sp_deleteOptionByApDataRange 'DefSisIntegracaoAD', 1, 1
	exec sp_deleteOptionByApDataRange 'ComandosSQLsSobs', 1, 1
	exec sp_deleteOptionByApDataRange 'ComandosSQLs', 1, 1
	exec sp_deleteOptionByApDataRange 'ComandosSQLsGrupos', 1, 1
	exec sp_deleteOptionByApDataRange 'LogsIntegracoes', 1, 1
	exec sp_deleteOptionByApDataRange 'LogsIntegracoesCampos', 1, 1
	/*other deletes*/
	exec sp_Execute_Delete 'dbo', 01, 'FormulariosWFCampos', 'FWC_CdiFormularioWFCampo = 100505' 
	exec sp_Execute_Delete 'dbo', 02, 'UsuariosAutenticacoes', 'JVQ_CdiUsuarioAutenticacao = 1'
	exec sp_Execute_Delete 'dbo', 03, 'ControlesSeqsInternos', 'DJN_CdiTabela > 0'
end
GO

/**********************************************************************
  7 - Store Procedure Standard Data
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
		exec sp_Execute_Insert 'dbo', 01, 'DLLsIntegracoes', 'BBV_CdiDLLIntegracao, BBV_D1sDLLIntegracao, BBV_CosCaminhoArquivoDLL', '50001, ''(TESTES) - DLL INTEGRACAO INTERFACE'', ''C:\Apdata_x64\Aplicacoes\ApIntegrationInterface\bin\Win32\Debug\ApIntegrationInterface.dll''', 1
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

		/*#### OBJETO - 962 - Tabela - LayoutsSaidas*/
		exec sp_Execute_Insert 'dbo', 01, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida', '1001, ''(TESTES) LAYOUT REST PARAMETROS'', 0xEFBBBF7B226B6579223A20222376616C7565506172616D6574657223227D', 1   
		exec sp_Execute_Insert 'dbo', 02, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida', '1002, ''(TESTES) LAYOUT REST PARAMETROS URL'', 0xEFBBBF68747470733A2F2F7669616365702E636F6D2E62722F77732F23636570232F6A736F6E2F', 1
		exec sp_Execute_Insert 'dbo', 03, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida', '1003, ''(TESTES) LAYOUT REST INTEGRACAO FOTOS'', 0xEFBBBF7B0D0A20226964223A22234147495F436469417373756E746F476572616C4974656D23222C0D0A2022666F746F223A22234147495F4172624172717569766F52656C61746F72696F23220D0A7D', 1
		exec sp_Execute_Insert 'dbo', 04, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida', '1004, ''(TESTES) MOCK POSTMAN GET'', 0xEFBBBF68747470733A2F2F32306537373664392D666164662D343763312D393163392D3032663538323931623963312E6D6F636B2E7073746D6E2E696F2F6170692F76312F6765742F236B6579696423', 1
		exec sp_Execute_Insert 'dbo', 05, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida', '1005, ''(TESTES) MOCK POSTMAN PUT'', 0xEFBBBF68747470733A2F2F32306537373664392D666164662D343763312D393163392D3032663538323931623963312E6D6F636B2E7073746D6E2E696F2F6170692F76312F7075742F236B6579696423', 1
		exec sp_Execute_Insert 'dbo', 06, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida', '1006, ''(TESTES) JSON - PREENCHIMENTO PARAMETRO SQL'', 0xEFBBBF207B22646174614F6E65223A236669656C6431232C20226461746154776F223A236669656C6432237D', 1
		exec sp_Execute_Insert 'dbo', 07, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida', '1007, ''(TESTES) JSON - LAYOUT SAIDA ALTERACAO'', 0xEFBBBF7B226E616D65223A22236E6F6D6523222C224F5554524F4E414D45223A2223656D61696C23222C226964223A2223696423227D', 1
		exec sp_Execute_Insert 'dbo', 08, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida', '1008, ''(TESTES) API THIRDPART GET'', 0xEFBBBF68747470733A2F2F3632643662656661353165366538663036663132313466392E6D6F636B6170692E696F2F6170692F76312F706F73742F236B6579696423', 1
		exec sp_Execute_Insert 'dbo', 09, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida', '1009, ''(TESTES) OAUTH SERASA MODELO'', 0xEFBBBF7B22417574686F72697A6174696F6E223A224265617265722023616363657373546F6B656E23222C22436F6E74656E742D54797065223A226170706C69636174696F6E2F6A736F6E227D', 1
		exec sp_Execute_Insert 'dbo', 10, 'LayoutsSaidas', 'BRD_CdiLayOutSaida, BRD_D1sLayOutSaida, BRD_D1bLayOutSaida', '1010, ''(TESTES) OAUTH SERASA CAMPOS'', 0xEFBBBF7B0D0A202022736F7572636553797374656D223A202223736F7572636553797374656D23222C0D0A20202275736572223A202223757365727323222C0D0A202022726567697374726174696F6E526571756573744E756D626572223A202223726567697374726174696F6E526571756573744E756D62657223222C0D0A202022696E766F696365223A207B0D0A2020202022696E766F6963654964223A202223696E766F696365496423222C0D0A202020202264617465223A202223646174655F696E766F69636523222C0D0A2020202022616D6F756E74223A202223616D6F756E7423222C0D0A20202020226465736372697074696F6E223A2022236465736372697074696F6E5F696E766F69636523222C0D0A20202020227061796D656E744372656174696F6E44617465223A2022237061796D656E744372656174696F6E4461746523222C0D0A20202020227061796D656E7444617465223A2022237061796D656E744461746523220D0A20207D2C0D0A20202262696C6C696E6746726F6D223A207B0D0A2020202022646F63756D656E7454797065223A202223646F63756D656E74547970655F62696C6C696E6746726F6D23222C0D0A2020202022646F63756D656E744E756D626572223A202223646F63756D656E744E756D6265725F62696C6C696E6746726F6D23220D0A20207D2C0D0A20202262696C6C696E67546F223A207B0D0A2020202022646F63756D656E744E756D626572223A202223646F63756D656E744E756D6265725F62696C6C696E67546F23222C0D0A20202020226465736372697074696F6E223A202223646F63756D656E74547970655F62696C6C696E67546F23220D0A20207D2C0D0A202022696E766F6963654C696E6573223A205B0D0A202020207B0D0A202020202020226974656D4465736372697074696F6E223A2022236974656D4465736372697074696F6E23222C0D0A202020202020226974656D416D6F756E74223A2022236974656D416D6F756E7423222C0D0A202020202020226163636F756E74696E674B6579223A2022236163636F756E74696E674B657923222C0D0A202020202020226669786564417373657454797065223A202223666978656441737365745479706523222C0D0A2020202020202266697865644173736574426F6F6B54797065223A202366697865644173736574426F6F6B54797065232C0D0A20202020202022666978656441737365744163636F756E74223A2023666978656441737365744163636F756E74232C0D0A202020202020226669786564417373657443617465676F7279223A20236669786564417373657443617465676F7279230D0A202020207D0D0A20205D0D0A7D', 1

		/*#### OBJETO - 3552 ComandosSQLsGrupos, ComandosSQLs,  */
		exec sp_takeKeyForInsertion 'ComandosSQLsGrupos', @ADN_CdiComandoSQLGrupo OUTPUT
		exec sp_takeKeyForInsertion 'ComandosSQLs', @SQL_CdiComandoSQL OUTPUT

		exec sp_Execute_Insert_Key 'dbo', 01, 'ComandosSQLsGrupos', 'ADN_CdiComandoSQLGrupo, ADN_D1sComandoSQLGrupo', @ADN_CdiComandoSQLGrupo, 01, '''(TESTES) COMANDOS SQL TESTES APINTEGRATION''', 1  
		exec sp_Execute_Insert_Key 'dbo', 02, 'ComandosSQLsGrupos', 'ADN_CdiComandoSQLGrupo, ADN_D1sComandoSQLGrupo', @ADN_CdiComandoSQLGrupo, 02, '''(TESTES) COMANDOS SQL TESTES APADINTEGRATORWS''', 1  
		
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 01, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 01, @ADN_CdiComandoSQLGrupo, 1, '''(TESTES) - RETORNA USUARIO ATRAVES DO CONTRATADO'', 0xEFBBBF53656C656374206D6178285553525F4364695573756172696F290D0A66726F6D205573756172696F730D0A696E6E6572206A6F696E205573756172696F73436F6E7472617461646F73204F4E20285553525F4364695573756172696F203D205553435F4364695573756172696F290D0A7768657265205553435F436469436F6E7472617461646F5F5573756172696F203D203A6964636F6E7472617461646F0D0A', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 02, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 02, @ADN_CdiComandoSQLGrupo, 1, '''(TESTES) - CONVERSOR EMAIL'', 0xEFBBBF4445434C4152452040656D61696C2061732056415243484152283830290D0A5345542040656D61696C203D202853656C656374206D6178285553525F436F73456D61696C2966726F6D205573756172696F730D0A09090909696E6E6572206A6F696E205573756172696F73436F6E7472617461646F73204F4E20285553525F4364695573756172696F203D205553435F4364695573756172696F290D0A090909097768657265205553435F436469436F6E7472617461646F5F5573756172696F203D203A6964636F6E7472617461646F290D0A73656C6563742043415345205748454E2040656D61696C206973206E6F74206E756C6C207468656E2027402720656C73652040656D61696C20656E6420617320656D61696C', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 03, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 03, @ADN_CdiComandoSQLGrupo, 1, '''(TESTES) - CONTRATADOS LOTES'', 0xEFBBBF0D0A0D0A0D0A53454C4543540D0A0D0A312061732069642C0D0A27303831313133313027206173206365700D0A756E696F6E20616C6C0D0A73656C6563740D0A322C0D0A273037313131333130270D0A756E696F6E20616C6C0D0A73656C656374200D0A332C0D0A273039313131333130270D0A', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 04, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 04, @ADN_CdiComandoSQLGrupo, 1, '''(TESTES) - BUFFER'', 0xEFBBBF73656C656374204147495F436469417373756E746F476572616C4974656D2C204147495F4172624172717569766F52656C61746F72696F0D0A66726F6D20417373756E746F734765726169734974656E730D0A7768657265204147495F436469417373756E746F476572616C4974656D203D203132', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 05, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 05, @ADN_CdiComandoSQLGrupo, 1, '''(TESTES) - COMANDO PREENCHIMENTO JSON'', 0xEFBBBF77697468206475616C2864756D6D7929206173202873656C656374202778272953454C45435420273931343036393227206173206B65796D61737465722C202731313030313127206173206669656C64312C2027323032323131303127206173206669656C6432202046524F4D206475616C202057484552452031203C3D203A6B65796964', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 06, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 06, @ADN_CdiComandoSQLGrupo, 1, '''(TESTES) - COMANDO SAIDA JSON'', 0xEFBBBF73656C65637420636F6E5F6473736E6F6D65206E6F6D652C20434F4E5F436F73454D61696C20656D61696C2C20636F6E5F436469636F6E7472617461646F204944200D0A0966726F6D20636F6E7472617461646F730D0A776865726520636F6E5F636469636F6E7472617461646F203D203A636F6E5F636469636F6E7472617461646F0D0A', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 07, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 07, @ADN_CdiComandoSQLGrupo, 1, '''(TESTES) - RETORNA RANDOM NUMERO KEYID 1-100'', 0xEFBBBF53454C45435420464C4F4F522852414E4428292A283130302D312B31292B3129206173206B657969643B', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 08, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 08, @ADN_CdiComandoSQLGrupo, 1, '''(TESTES) - RETORNA RANDOM NUMERO KEYID 1-45'', 0xEFBBBF53454C45435420464C4F4F522852414E4428292A2834352D312B31292B3129206173206B657969643B', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 09, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 09, @ADN_CdiComandoSQLGrupo, 1, '''(TESTES) - RETORNA RANDOM NUMERO KEYID CORINGA'', 0xEFBBBF53454C45435420464C4F4F522852414E4428292A283A6E756D4B65792D312B31292B3129206173206B657969643B', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 10, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 10, @ADN_CdiComandoSQLGrupo, 1, '''(TESTES) - NAME MANAGER TWO FIELDS'', 0xEFBBBF53656C656374205553525F4364735573756172696F2C20434F4E5F4473734E6F6D65436F6D706C65746F2046726F6D20436F6E7472617461646F7320496E6E6572204A6F696E205573756172696F73436F6E7472617461646F73206F6E2028434F4E5F436469436F6E7472617461646F203D205553435F436469436F6E7472617461646F5F5573756172696F2920496E6E6572204A6F696E205573756172696F73206F6E20285553435F4364695573756172696F203D205553525F4364695573756172696F2920576865726520434F4E5F436469436F6E7472617461646F203D203A436469436F6E7472617461646F', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 11, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 11, @ADN_CdiComandoSQLGrupo, 2, '''(TESTES) - NAME MANAGER'', 0xEFBBBF53656C6563742020434F4E5F4473734E6F6D65436F6D706C65746F2046726F6D20436F6E7472617461646F73200D0A496E6E6572204A6F696E205573756172696F73436F6E7472617461646F73206F6E2028434F4E5F436469436F6E7472617461646F203D20205553435F436469436F6E7472617461646F5F5573756172696F29200D0A496E6E6572204A6F696E205573756172696F73206F6E20285553435F4364695573756172696F203D20205553525F4364695573756172696F29200D0A576865726520434F4E5F436469436F6E7472617461646F203D203A436469436F6E7472617461646F', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 12, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 12, @ADN_CdiComandoSQLGrupo, 1, '''(TESTES) - OAUTH SERASA FINANCEIRO'', 0xEFBBBF4465636C61726520404C6F675472616E736163616F20617320696E74656765722C204056696E63756C6F20617320696E74656765720D0A0D0A73657420404C6F675472616E736163616F203D203A4C6F675472616E736163616F0D0A0D0A0D0A73656C6563740D0A0D0A546162656C612E456D705F436469456D70726573612C0D0A546162656C612E4D56465F6364696C6F677472616E736163616F2C0D0A546162656C612E646F63756D656E744E756D6265725F62696C6C696E6746726F6D2C0D0A546162656C612E646F63756D656E74547970655F62696C6C696E6746726F6D2C0D0A546162656C612E646F63756D656E744E756D6265725F62696C6C696E67546F2C0D0A546162656C612E646F63756D656E74547970655F62696C6C696E67546F2C0D0A5265706C61636528636F6E7665727428766172636861722C466F726D61742853756D28546162656C612E616D6F756E74292C27302E30302729292C272E272C272C2729617320616D6F756E742C0D0A546162656C612E646174655F696E766F6963652C0D0A546162656C612E6465736372697074696F6E5F696E766F6963652C0D0A636F6E7665727428766172636861722C546162656C612E696E766F696365496429202B272D272B2020636F6E7665727428766172636861722C726F775F6E756D6265722829206F76657220286F7264657220627920546162656C612E696E766F6963654964292920617320696E766F69636549642C0D0A546162656C612E7061796D656E744372656174696F6E446174652C0D0A546162656C612E7061796D656E74446174652C0D0A546162656C612E6163636F756E74696E674B65792C0D0A546162656C612E666978656441737365744163636F756E742C0D0A546162656C612E66697865644173736574426F6F6B547970652C0D0A546162656C612E6669786564417373657443617465676F72792C0D0A546162656C612E66697865644173736574547970652C0D0A5265706C61636528636F6E7665727428766172636861722C466F726D61742853756D28546162656C612E6974656D416D6F756E74292C27302E30302729292C272E272C272C2729206173206974656D416D6F756E742C0D0A546162656C612E6974656D4465736372697074696F6E2C0D0A546162656C612E726567697374726174696F6E526571756573744E756D6265722C0D0A546162656C612E736F7572636553797374656D2C0D0A546162656C612E75736572732C0D0A546162656C612E436F6E5F43646976696E63756C6F0D0A0D0A46726F6D20280D0A73656C6563740D0A456D7072657361732E456D705F436469456D70726573612C0D0A4D56465F6364696C6F677472616E736163616F2C0D0A2D2D2D2730272B5265706C61636528456D7072657361732E454D505F4E7573434E504A5072656669786F2C272E272C272729202B205265706C616365284C6F636169732E4C4F435F4E7573434E504A53756669786F2C272D272C2727292061732020646F63756D656E744E756D6265725F62696C6C696E6746726F6D2C0D0A63617365207768656E20456D7072657361732E456D705F436469456D70726573612020202020203D2031207468656E2027303632313733363230303030313830270D0A20202020207768656E20456D7072657361732E456D705F436469456D70726573612020202020203D2034207468656E20273033343834393132343030303136382720656E642061732020646F63756D656E744E756D6265725F62696C6C696E6746726F6D2C0D0A27302720202020202020202020202020202020202020202020202020202020202020202061732020646F63756D656E74547970655F62696C6C696E6746726F6D2C0D0A2D2D2D2D4D56465F436F73566172696176656C32202020202020202020202020202020202020202061732020646F63756D656E744E756D6265725F62696C6C696E67546F2C0D0A2D2D2730272B5265706C61636528456D7072657361732E454D505F4E7573434E504A5072656669786F2C272E272C272729202B205265706C616365284C6F636169732E4C4F435F4E7573434E504A53756669786F2C272D272C2727292061732020646F63756D656E744E756D6265725F62696C6C696E67546F2C0D0A63617365207768656E20456D7072657361732E456D705F436469456D70726573612020202020203D2031207468656E2027303632313733363230303030313830270D0A20202020207768656E20456D7072657361732E456D705F436469456D70726573612020202020203D2034207468656E20273033343834393132343030303136382720656E642061732020646F63756D656E744E756D6265725F62696C6C696E67546F2C0D0A4D56465F44737350726F636573736F45787465726E6F202020202020202020202020202061732020646F63756D656E74547970655F62696C6C696E67546F2C0D0A0D0A53756D284D56465F566C6E56616C6F72292061732020616D6F756E742C0D0A636F6E766572742876617263686172283130292C6765746461746528292C31323629202061732020646174655F696E766F6963652C0D0A27506167746F204D656E73616C27202020202020202020202020202020202020202020206173206465736372697074696F6E5F696E766F6963652C0D0A4D56465F6364696C6F677472616E736163616F20617320696E766F69636549642C0D0A636F6E766572742876617263686172283130292C6765746461746528292C3132362920206173207061796D656E744372656174696F6E446174652C0D0A636F6E766572742876617263686172283130292C4D56465F447464506167616D656E746F2C3132362920206173207061796D656E74446174652C0D0A454D465F443173436F6E746575646F466C65786976656C5F30312C0D0A454D465F443173436F6E746575646F466C65786976656C5F3031202B20272E42522E313030272B2020272E3030303030302E272B204D56465F436F73566172696176656C31202B272E30303030303030302E3030303027206173206163636F756E74696E674B65792C0D0A276E756C6C27202020202020202020202020202020202020202020202020202020202020617320666978656441737365744163636F756E742C0D0A276E756C6C2720202020202020202020202020202020202020202020202020202020202061732066697865644173736574426F6F6B547970652C0D0A276E756C6C272020202020202020202020202020202020202020202020202020202020206173206669786564417373657443617465676F72792C0D0A274E2720202020202020202020202020202020202020202020202020202020202020202061732066697865644173736574547970652C0D0A73756D284D56465F566C6E56616C6F7229202020202020202020202020202020202020206173206974656D416D6F756E742C0D0A27506167746F204D656E73616C27202020202020202020202020202020202020202020206173206974656D4465736372697074696F6E2C0D0A4D56465F6364696C6F677472616E736163616F2020202020202020202020202020202020617320726567697374726174696F6E526571756573744E756D6265722C0D0A274150444154412720202020202020202020202020202020202020202020202020202020617320736F7572636553797374656D2C0D0A27323833383127202020202020202020202020202020202020202020202020202020202061732075736572732C0D0A63617365207768656E20436F6E7472617461646F732E436F6E5F43646976696E63756C6F203C3E203337207468656E203120656C736520436F6E7472617461646F732E436F6E5F43646976696E63756C6F20656E6420617320436F6E5F43646976696E63756C6F0D0A0D0A46726F6D204D6F76696D656E746F7366696E616E636569726F7320202020202020202020204D6F76696D656E746F7366696E616E636569726F730D0A696E6E6572206A6F696E20436F6E7472617461646F73202020202020202020202020202020436F6E7472617461646F732020202020202020202020202020206F6E2028436F6E7472617461646F732E436F6E5F436469436F6E7472617461646F203D204D6F76696D656E746F7366696E616E636569726F732E4D56465F436469436F6E7472617461646F290D0A696E6E6572206A6F696E2043656E74726F73437573746F732020202020202020202020202043656E74726F73437573746F73202020202020202020202020206F6E2028436F6E7472617461646F732E436F6E5F43646943656E74726F437573746F203D2043656E74726F73437573746F732E4343555F43646943656E74726F437573746F290D0A696E6E6572206A6F696E20466F6C6861732020202020202020202020202020202020202020466F6C68617320202020202020202020202020202020202020206F6E2028436F6E7472617461646F732E436F6E5F436469466F6C6861203D20466F6C6861732E466F6C5F436469466F6C6861290D0A696E6E6572206A6F696E204C6F6361697320202020202020202020202020202020202020204C6F6361697320202020202020202020202020202020202020206F6E2028466F6C6861732E466F6C5F4364694C6F63616C203D204C6F636169732E4C6F635F4364696C6F63616C290D0A696E6E6572206A6F696E20456D707265736173202020202020202020202020202020202020456D7072657361732020202020202020202020202020202020206F6E20284C6F636169732E4C6F635F436469456D7072657361203D20456D7072657361732E456D705F436469456D7072657361290D0A696E6E6572206A6F696E20456D707265736173466C65786976656973202020202020202020456D707265736173466C657869766569732020202020202020206F6E2028456D707265736173466C657869766569732E454D465F436469456D7072657361203D20456D7072657361732E456D705F436469456D7072657361290D0A0D0A776865726520204D56465F6364696C6F677472616E736163616F203D20404C6F675472616E736163616F0D0A0D0A47726F757020627920456D7072657361732E456D705F436469456D70726573612C0D0A2020202020202020204D56465F4364694C6F675472616E736163616F2C0D0A2020202020202020202D2D456D7072657361732E454D505F4E7573434E504A5072656669786F2C0D0A20202020202020202D2D204C6F636169732E4C4F435F4E7573434E504A53756669786F2C0D0A202020202020202020454D465F443173436F6E746575646F466C65786976656C5F30312C0D0A2020202020202020204D56465F436F73566172696176656C312C0D0A2020202020202020204D56465F436F73566172696176656C322C0D0A20202020202020202D2D2D2043656E74726F73437573746F732E4343555F436F736573747275747572612C0D0A2020202020202020204D56465F44737350726F636573736F45787465726E6F2C0D0A2020202020202020204D56465F436F7350726F636573736F45787465726E6F2C0D0A2020202020202020204D56465F447464506167616D656E746F2C0D0A202020202020202020436F6E7472617461646F732E436F6E5F43646976696E63756C6F0D0A0D0A2920546162656C610D0A0D0A0D0A47726F75702062790D0A546162656C612E456D705F436469456D70726573612C0D0A546162656C612E4D56465F6364696C6F677472616E736163616F2C0D0A546162656C612E646F63756D656E744E756D6265725F62696C6C696E6746726F6D2C0D0A546162656C612E646F63756D656E74547970655F62696C6C696E6746726F6D2C0D0A546162656C612E646F63756D656E744E756D6265725F62696C6C696E67546F2C0D0A546162656C612E646F63756D656E74547970655F62696C6C696E67546F2C0D0A546162656C612E646174655F696E766F6963652C0D0A546162656C612E6465736372697074696F6E5F696E766F6963652C0D0A546162656C612E696E766F69636549642C0D0A546162656C612E7061796D656E744372656174696F6E446174652C0D0A546162656C612E7061796D656E74446174652C0D0A546162656C612E6163636F756E74696E674B65792C0D0A546162656C612E666978656441737365744163636F756E742C0D0A546162656C612E66697865644173736574426F6F6B547970652C0D0A546162656C612E6669786564417373657443617465676F72792C0D0A546162656C612E66697865644173736574547970652C0D0A546162656C612E6974656D4465736372697074696F6E2C0D0A546162656C612E726567697374726174696F6E526571756573744E756D6265722C0D0A546162656C612E736F7572636553797374656D2C0D0A546162656C612E75736572732C0D0A546162656C612E436F6E5F43646976696E63756C6F0D0A0D0A6F7264657220627920546162656C612E456D705F436469456D70726573612C0D0A2020202020202020204D56465F4364694C6F675472616E736163616F0D0A', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 13, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 13, @ADN_CdiComandoSQLGrupo, 1, '''(TESTES) - OAUTH CREDENCIAIS SERASA'', 0xEFBBBF73656C6563740D0A273632653832356331326361383235326264393065643262322720617320757365726E616D652C0D0A27316232646530396462323532386163323163353238653236272061732050617373776F7264', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 14, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 14, @ADN_CdiComandoSQLGrupo, 1, '''(TESTES) - RETORNA DATA HORA'', 0xEFBBBF77697468206475616C2864756D6D7929206173202873656C65637420277827292073656C656374206765746461746528292066726F6D206475616C', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 15, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 15, @ADN_CdiComandoSQLGrupo, 1, '''(TESTES) - RETORNA O NUMERO INFORMADO'', 0xEFBBBF77697468206475616C2864756D6D7929206173202873656C65637420277827292073656C656374203A4E554D45524F494E464F524D41444F202066726F6D206475616C', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 16, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 16, @ADN_CdiComandoSQLGrupo, 1, '''(TESTES) - COMANDO LOTE CHAVES'', 0xEFBBBF53454C45435420464C4F4F522852414E4428292A283130302D312B31292B3129206173206B657969640D0A0D0A756E696F6E20616C6C0D0A0D0A53454C45435420464C4F4F522852414E4428292A283130302D312B31292B3129206173206B657969640D0A0D0A756E696F6E20616C6C0D0A0D0A53454C45435420464C4F4F522852414E4428292A283130302D312B31292B3129206173206B65796964', 1
        exec sp_Execute_Insert_Key_ForeignKey 'dbo', 17, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 17, @ADN_CdiComandoSQLGrupo, 1, '''(TESTES) - RETORNA RANDOM NUMERO KEYID 1-4'', 0xEFBBBF53454C45435420464C4F4F522852414E4428292A28342D312B31292B3129206173206B657969643B', 1
		exec sp_Execute_Insert_Key_ForeignKey 'dbo', 18, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 18, @ADN_CdiComandoSQLGrupo, 1, '''(TESTES) - RETORNA NOVO TRANSANCTION ID'', 0xEFBBBF77697468206475616C2864756D6D7929206173202873656C65637420277827292073656C656374203330383432207768657265203A43424F203D203130323130', 1
        exec sp_Execute_Insert_Key_ForeignKey 'dbo', 19, 'ComandosSQLs', 'SQL_CdiComandoSQL, SQL_CdiComandoSQLGrupo, SQL_D1sComandoSQL, SQL_DsbComandoSQL', @SQL_CdiComandoSQL, 19, @ADN_CdiComandoSQLGrupo, 1, '''(TESTES) - RETORNA NOVO TIPO EDICAO'', 0xEFBBBF77697468206475616C2864756D6D7929206173202873656C65637420277827292073656C65637420343037207768657265203A43424F203D203130323130', 1

		/*#### OBJETO - 554 ModelosIntegracoes*/
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 01, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10001, ''(TESTES) SOAP CONSULTA CEP''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 02, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10002, ''(TESTES) REST SEM PARAMETROS''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 03, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10003, ''(TESTES) REST COM PARAMETROS''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 04, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10004, ''(TESTES) REST COM PARAMETROS URL''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 05, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10005, ''(TESTES) REST ALTERACAO OBJ 2330''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 06, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10006, ''(TESTES) REST MARCACAO PONTO''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 07, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10007, ''(TESTES) REST ENTRADA BASEX64''', 1
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 08, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10008, ''(TESTES) SOAP CORREIOS LOTE''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 09, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10009, ''(TESTES) QUERY EXECUTE INTEGRACAO FOTO''', 1
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 10, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10010, ''(TESTES) REST MOCK POSTMAN GET''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 11, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10011, ''(TESTES) REST MOCK POSTMAN PUT''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 12, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10012, ''(TESTES) REST MOCK POSTMAN POST''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 13, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10013, ''(TESTES) REST CONSULTAS REMOTAS''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 14, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10014, ''(TESTES) REST MOCK POSTMAN GET ARRAY''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 15, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10015, ''(TESTES) REST API THIRDPART GET''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 16, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10016, ''(TESTES) REST MOCK REST REPROCESS''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 17, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10017, ''(TESTES) REST MOCK POSTMAN GET COMPLEXY ARRAY''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 18, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10018, ''(TESTES) REST REST COMBATIDAS REAIS''', 1
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 19, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10019, ''(TESTES) SOAP TESTE WSDL TECBAN''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 20, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10020, ''(TESTES) REST OAUTH SERASA''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 21, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10021, ''(TESTES) REST ADD DATE/HOUR IN OBJ 3129''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 22, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10022, ''(TESTES) REST MODELO SUCESSO''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 23, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10023, ''(TESTES) REST POSTMAN LOTE''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 24, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10024, ''(TESTES) REST MOCK POSTMAN GET COMPLEXY ARRAY 2''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 25, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10025, ''(TESTES) REST ALTER DA TRANSACAO E O TIPO EDICAO''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 26, 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao, BBR_D1sModeloIntegracao', '10026, ''(TESTES) SOAP ADMISSAO - INCLUSAO DEPENDENTES''', 1

		/*#### OBJETO - 555 */
		/*##### ModelosIntegracoesCmds*/
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 01, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto', '10001, 10001, ''(TESTES) SOAP CORREIOS CONSULTA CEP - CMD'', 2, 30063, ''AtendeClienteService;AtendeCliente;consultaCEP''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 02, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP', '10002, 10002, ''(TESTES) REST SEM PARAMETROS'', 2, 30063, ''http://echo.jsontest.com/key/value/one/two'', 1', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 03, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP', '10003, 10003, ''(TESTES) REST COM PARAMETROS'', 2, 30063, ''http://validate.jsontest.com/?json='', 1', 1  
		/*REST*/ exec sp_Execute_Insert 'dbo', 04, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP', '10004, 10004, ''(TESTES) REST COM PARAMETROS URL'', 2, 30063, '''', 1', 1  
		/*REST*/ exec sp_Execute_Insert 'dbo', 05, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP', '10005, 10005, ''(TESTES) REST ALTERACAO SENHA 2330'', 1, 21233, '''', 1', 1  
		/*REST*/ exec sp_Execute_Insert 'dbo', 06, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP', '10006, 10006, ''(TESTES) REST MARCACAO PONTO'', 2, 43192, '''', 1', 1  
		/*REST*/ exec sp_Execute_Insert 'dbo', 07, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_NuiTipoEdicao', '10007, 10007, ''(TESTES) REST ENTRADA BASEX64'',  1, 30842, 407', 1  
		/*REST*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 08, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_NuiTipoEdicao, BBS_DssNomeObjeto, BBS_OplEnviarTudo, BBS_DssCamposLote', 10008, 0, @SQL_CdiComandoSQL, 3, '10008, ''(TESTES) SOAP CORREIOS LOTE'',  1, 30063, 0, ''AtendeClienteService;AtendeCliente;consultaCEP'', 1, ''cep''', 1  
		/*REST*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 09, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_NuiTipoEdicao, BBS_DssNomeObjeto, BBS_CdiEventoTransacao, BBS_CdiVerboHTTP', 10009, 0, @SQL_CdiComandoSQL, 4, '10009, ''(TESTES) QUERY EXECUTE INTEGRACAO FOTO'',  4, 30063, 0, ''https://httpbin.org/post'', 2, 3', 1  
		/*REST*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 10, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP', 10010, 0, @SQL_CdiComandoSQL, 7, '10010, ''(TESTES) REST MOCK POSTMAN GET'', 1, 30063, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v1/get/keyid'', 1', 1  
		/*REST*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 11, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP, BBS_CdiEventoTransacao', 10011, 0, @SQL_CdiComandoSQL, 5,  '10011, ''(TESTES) REST MOCK POSTMAN PUT'', 1, 30063, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v1/put/keyid'', 2, 2', 1  
		/*REST*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 12, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_NuiTipoEdicao, BBS_DssNomeObjeto, BBS_CdiEventoTransacao, BBS_CdiVerboHTTP', 10012, 0, @SQL_CdiComandoSQL, 6,  '10012, ''(TESTES) SAIDA REST 1106'',  1, 15953, 0, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v1/post/add'', 2, 3', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 13, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr', '10013, 10013, ''(TESTES) REST CONSULTAS REMOTAS OAUTH'', 5', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 14, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP', '10014, 10014, ''(TESTES) REST MOCK POSTMAN GET ARRAY'', 1, 30063, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v1/get/arrayjson'', 1', 1  
		/*REST*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 15, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP', 10015, 0, @SQL_CdiComandoSQL, 17, '10015, ''(TESTES) REST API THIRDPART GET'', 1, 30063, ''https://62d6befa51e6e8f06f1214f9.mockapi.io/api/v1/post/keyid'', 1', 1   
		/*REST*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 16, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP, BBS_OplGravarResponse', 10016, 0, @SQL_CdiComandoSQL, 7, '10016, ''(TESTES) MOCK REST REPROCESS'', 1, 30063, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v1/get/keyid'', 1, 1', 1  
		/*REST*/ exec sp_Execute_Insert 'dbo', 17, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP', '10017, 10017, ''(TESTES) REST MOCK POSTMAN GET COMPLEXY ARRAY'', 1, 30063, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v2/get/arrayjson'', 1', 1  
		/*REST*/ exec sp_Execute_Insert 'dbo', 18, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_CdiTipoComandoIntegr, BBS_DssNomeObjeto, BBS_CdiTransacao, BBS_NuiOrdem, BBS_OplOneForManyInclusao, BBS_OplValidaVazio, BBS_CdiComandoSQL, BBS_OplComandoNativo, BBS_CdiEventoTransacao, BBS_CdiFormularioWF, BBS_D1sModeloIntegracaoCmd, BBS_D2sModeloIntegracaoCmd, BBS_D3sModeloIntegracaoCmd, BBS_D4sModeloIntegracaoCmd, BBS_D5sModeloIntegracaoCmd, BBS_D6sModeloIntegracaoCmd, BBS_D7sModeloIntegracaoCmd, BBS_D8sModeloIntegracaoCmd, BBS_DsbComandoSQLValidacao, BBS_D1bDescritivo, BBS_D2bDescritivo, BBS_D3bDescritivo, BBS_D4bDescritivo, BBS_D5bDescritivo, BBS_D6bDescritivo, BBS_D7bDescritivo, BBS_D8bDescritivo, BBS_CdiModeloIntegracaoCmd_Ftr, BBS_NuiTipoEdicao, BBS_CdiTransacaoWF, BBS_CdiModeloIntegracao_Exc, BBS_CdiModeloIntegracao_Suc, BBS_CdiTpTratamentoIntegracao, BBS_OplExecutarSQL_Apdata, BBS_OplEnviarTudo, BBS_OplDesativado, BBS_CdiVerboHTTP, BBS_CdiModeloIntegracaoCmd_Pre, BBS_CdiLayOutSaida_Autentica, BBS_CdiLayOutSaida', /*values*/ '10018, 10018, 1, null, 29993, 0, 0, 0, 0, 0, 2, 0, ''(TESTES) INVALIDACAO MARCACAO - Obj 3111'', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0', 1  
		/*REST*/ exec sp_Execute_Insert 'dbo', 19, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_CdiTipoComandoIntegr, BBS_DssNomeObjeto, BBS_CdiTransacao, BBS_NuiOrdem, BBS_OplOneForManyInclusao, BBS_OplValidaVazio, BBS_CdiComandoSQL, BBS_OplComandoNativo, BBS_CdiEventoTransacao, BBS_CdiFormularioWF, BBS_D1sModeloIntegracaoCmd, BBS_D2sModeloIntegracaoCmd, BBS_D3sModeloIntegracaoCmd, BBS_D4sModeloIntegracaoCmd, BBS_D5sModeloIntegracaoCmd, BBS_D6sModeloIntegracaoCmd, BBS_D7sModeloIntegracaoCmd, BBS_D8sModeloIntegracaoCmd, BBS_DsbComandoSQLValidacao, BBS_D1bDescritivo, BBS_D2bDescritivo, BBS_D3bDescritivo, BBS_D4bDescritivo, BBS_D5bDescritivo, BBS_D6bDescritivo, BBS_D7bDescritivo, BBS_D8bDescritivo, BBS_CdiModeloIntegracaoCmd_Ftr, BBS_NuiTipoEdicao, BBS_CdiTransacaoWF, BBS_CdiModeloIntegracao_Exc, BBS_CdiModeloIntegracao_Suc, BBS_CdiTpTratamentoIntegracao, BBS_OplExecutarSQL_Apdata, BBS_OplEnviarTudo, BBS_OplDesativado, BBS_CdiVerboHTTP, BBS_CdiModeloIntegracaoCmd_Pre, BBS_CdiLayOutSaida_Autentica, BBS_CdiLayOutSaida', /*values*/ '10019, 10018, 1, null, 39802, 0, 0, 0, 0, 0, 2, 0, ''(TESTES) INCLUSAO MARCACAO - Obj. 4176'', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0', 1  
		/*REST*/ exec sp_Execute_Insert 'dbo', 20, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_CdiTipoComandoIntegr, BBS_DssNomeObjeto, BBS_CdiTransacao, BBS_NuiOrdem, BBS_OplOneForManyInclusao, BBS_OplValidaVazio, BBS_CdiComandoSQL, BBS_OplComandoNativo, BBS_CdiEventoTransacao, BBS_CdiFormularioWF, BBS_D1sModeloIntegracaoCmd, BBS_D2sModeloIntegracaoCmd, BBS_D3sModeloIntegracaoCmd, BBS_D4sModeloIntegracaoCmd, BBS_D5sModeloIntegracaoCmd, BBS_D6sModeloIntegracaoCmd, BBS_D7sModeloIntegracaoCmd, BBS_D8sModeloIntegracaoCmd, BBS_DsbComandoSQLValidacao, BBS_D1bDescritivo, BBS_D2bDescritivo, BBS_D3bDescritivo, BBS_D4bDescritivo, BBS_D5bDescritivo, BBS_D6bDescritivo, BBS_D7bDescritivo, BBS_D8bDescritivo, BBS_CdiModeloIntegracaoCmd_Ftr, BBS_NuiTipoEdicao, BBS_CdiTransacaoWF, BBS_CdiModeloIntegracao_Exc, BBS_CdiModeloIntegracao_Suc, BBS_CdiTpTratamentoIntegracao, BBS_OplExecutarSQL_Apdata, BBS_OplEnviarTudo, BBS_OplDesativado, BBS_CdiVerboHTTP, BBS_CdiModeloIntegracaoCmd_Pre, BBS_CdiLayOutSaida_Autentica, BBS_CdiLayOutSaida', /*values*/ '10020, 10018, 1, null, 39803, 0, 0, 0, 0, 0, 2, 0, ''(TESTES) ALTERACAO MARCACAO - Obj. 4176'', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0', 1  
		/*REST*/ exec sp_Execute_Insert 'dbo', 21, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_CdiTipoComandoIntegr, BBS_DssNomeObjeto, BBS_CdiTransacao, BBS_NuiOrdem, BBS_OplOneForManyInclusao, BBS_OplValidaVazio, BBS_CdiComandoSQL, BBS_OplComandoNativo, BBS_CdiEventoTransacao, BBS_CdiFormularioWF, BBS_D1sModeloIntegracaoCmd, BBS_D2sModeloIntegracaoCmd, BBS_D3sModeloIntegracaoCmd, BBS_D4sModeloIntegracaoCmd, BBS_D5sModeloIntegracaoCmd, BBS_D6sModeloIntegracaoCmd, BBS_D7sModeloIntegracaoCmd, BBS_D8sModeloIntegracaoCmd, BBS_DsbComandoSQLValidacao, BBS_D1bDescritivo, BBS_D2bDescritivo, BBS_D3bDescritivo, BBS_D4bDescritivo, BBS_D5bDescritivo, BBS_D6bDescritivo, BBS_D7bDescritivo, BBS_D8bDescritivo, BBS_CdiModeloIntegracaoCmd_Ftr, BBS_NuiTipoEdicao, BBS_CdiTransacaoWF, BBS_CdiModeloIntegracao_Exc, BBS_CdiModeloIntegracao_Suc, BBS_CdiTpTratamentoIntegracao, BBS_OplExecutarSQL_Apdata, BBS_OplEnviarTudo, BBS_OplDesativado, BBS_CdiVerboHTTP, BBS_CdiModeloIntegracaoCmd_Pre, BBS_CdiLayOutSaida_Autentica, BBS_CdiLayOutSaida', /*values*/ '10021, 10018, 1, null, 30132, 0, 0, 0, 0, 0, 1, 0, ''(TESTES) INCLUSAO DE CRACHA PROVISOARIO'', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0', 1  
		/*REST*/ exec sp_Execute_Insert 'dbo', 22, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_CdiTipoComandoIntegr, BBS_DssNomeObjeto, BBS_CdiTransacao, BBS_NuiOrdem, BBS_OplOneForManyInclusao, BBS_OplValidaVazio, BBS_CdiComandoSQL, BBS_OplComandoNativo, BBS_CdiEventoTransacao, BBS_CdiFormularioWF, BBS_D1sModeloIntegracaoCmd, BBS_D2sModeloIntegracaoCmd, BBS_D3sModeloIntegracaoCmd, BBS_D4sModeloIntegracaoCmd, BBS_D5sModeloIntegracaoCmd, BBS_D6sModeloIntegracaoCmd, BBS_D7sModeloIntegracaoCmd, BBS_D8sModeloIntegracaoCmd, BBS_DsbComandoSQLValidacao, BBS_D1bDescritivo, BBS_D2bDescritivo, BBS_D3bDescritivo, BBS_D4bDescritivo, BBS_D5bDescritivo, BBS_D6bDescritivo, BBS_D7bDescritivo, BBS_D8bDescritivo, BBS_CdiModeloIntegracaoCmd_Ftr, BBS_NuiTipoEdicao, BBS_CdiTransacaoWF, BBS_CdiModeloIntegracao_Exc, BBS_CdiModeloIntegracao_Suc, BBS_CdiTpTratamentoIntegracao, BBS_OplExecutarSQL_Apdata, BBS_OplEnviarTudo, BBS_OplDesativado, BBS_CdiVerboHTTP, BBS_CdiModeloIntegracaoCmd_Pre, BBS_CdiLayOutSaida_Autentica, BBS_CdiLayOutSaida', /*values*/ '10022, 10018, 1, null, 30133, 0, 0, 0, 0, 0, 1, 0, ''(TESTES) BAIXA CRACHA PROVISOARIO'', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0', 1  
		/*REST*/ exec sp_Execute_Insert 'dbo', 23, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_CdiTipoComandoIntegr, BBS_DssNomeObjeto, BBS_CdiTransacao, BBS_NuiOrdem, BBS_OplOneForManyInclusao, BBS_OplValidaVazio, BBS_CdiComandoSQL, BBS_OplComandoNativo, BBS_CdiEventoTransacao, BBS_CdiFormularioWF, BBS_D1sModeloIntegracaoCmd, BBS_D2sModeloIntegracaoCmd, BBS_D3sModeloIntegracaoCmd, BBS_D4sModeloIntegracaoCmd, BBS_D5sModeloIntegracaoCmd, BBS_D6sModeloIntegracaoCmd, BBS_D7sModeloIntegracaoCmd, BBS_D8sModeloIntegracaoCmd, BBS_DsbComandoSQLValidacao, BBS_D1bDescritivo, BBS_D2bDescritivo, BBS_D3bDescritivo, BBS_D4bDescritivo, BBS_D5bDescritivo, BBS_D6bDescritivo, BBS_D7bDescritivo, BBS_D8bDescritivo, BBS_CdiModeloIntegracaoCmd_Ftr, BBS_NuiTipoEdicao, BBS_CdiTransacaoWF, BBS_CdiModeloIntegracao_Exc, BBS_CdiModeloIntegracao_Suc, BBS_CdiTpTratamentoIntegracao, BBS_OplExecutarSQL_Apdata, BBS_OplEnviarTudo, BBS_OplDesativado, BBS_CdiVerboHTTP, BBS_CdiModeloIntegracaoCmd_Pre, BBS_CdiLayOutSaida_Autentica, BBS_CdiLayOutSaida', /*values*/ '10023, 10019, 1, ''SFAPIService12;SFAPI12;login####0'', 30063, 0, 0, 0, 0, 0, 2, 0, ''(TESTES) SOAP WSDL TECBAN'', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0', 1  
		/*REST*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 23, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_CdiTipoComandoIntegr, BBS_DssNomeObjeto, BBS_CdiTransacao, BBS_NuiOrdem, BBS_OplOneForManyInclusao, BBS_OplValidaVazio, BBS_CdiModeloIntegracaoCmd_Ftr, BBS_OplComandoNativo, BBS_CdiEventoTransacao, BBS_CdiTransacaoWF, BBS_NuiTipoEdicao, BBS_CdiModeloIntegracao_Exc, BBS_CdiModeloIntegracao_Suc, BBS_CdiTpTratamentoIntegracao, BBS_OplExecutarSQL_Apdata, BBS_OplDesativado, BBS_OplEnviarTudo, BBS_CdiVerboHTTP, BBS_DssCamposLote, BBS_OplLoteBase, BBS_CdiModeloIntegracaoCmd_Pre, BBS_CdiLayOutSaida_Autentica, BBS_D1sModeloIntegracaoCmd, BBS_D2sModeloIntegracaoCmd, BBS_D3sModeloIntegracaoCmd, BBS_D4sModeloIntegracaoCmd, BBS_D5sModeloIntegracaoCmd, BBS_D6sModeloIntegracaoCmd, BBS_D7sModeloIntegracaoCmd, BBS_D8sModeloIntegracaoCmd, BBS_DsbComandoSQLValidacao, BBS_D1bDescritivo, BBS_D2bDescritivo, BBS_D3bDescritivo, BBS_D4bDescritivo, BBS_D5bDescritivo, BBS_D6bDescritivo, BBS_D7bDescritivo, BBS_D8bDescritivo, BBS_OplGravarResponse, BBS_DssCamposResponse, BBS_CdiTransacao_Retorno, BBS_NuiTipoEdicao_Retorno, BBS_CdiModeloIntegracao_Ret, BBS_CdiFormularioWF', /*values*/ 10024, 0, @SQL_CdiComandoSQL, 12, '10020, 1, ''https://api.serasaexperian.com.br/security/iam/v1/client-identities/login'', 76294, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 3, null, 0, 0, 0, ''(TESTE) TOKEN BEARER SERASA'', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, 0, null, 0, 0, 0, 0', 1
		/*REST*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 24, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_CdiTipoComandoIntegr, BBS_DssNomeObjeto, BBS_CdiTransacao, BBS_NuiOrdem, BBS_OplOneForManyInclusao, BBS_OplValidaVazio, BBS_CdiModeloIntegracaoCmd_Ftr, BBS_OplComandoNativo, BBS_CdiEventoTransacao, BBS_CdiTransacaoWF, BBS_NuiTipoEdicao, BBS_CdiModeloIntegracao_Exc, BBS_CdiModeloIntegracao_Suc, BBS_CdiTpTratamentoIntegracao, BBS_OplExecutarSQL_Apdata, BBS_OplDesativado, BBS_OplEnviarTudo, BBS_CdiVerboHTTP, BBS_DssCamposLote, BBS_OplLoteBase, BBS_CdiModeloIntegracaoCmd_Pre, BBS_CdiLayOutSaida_Autentica, BBS_D1sModeloIntegracaoCmd, BBS_D2sModeloIntegracaoCmd, BBS_D3sModeloIntegracaoCmd, BBS_D4sModeloIntegracaoCmd, BBS_D5sModeloIntegracaoCmd, BBS_D6sModeloIntegracaoCmd, BBS_D7sModeloIntegracaoCmd, BBS_D8sModeloIntegracaoCmd, BBS_DsbComandoSQLValidacao, BBS_D1bDescritivo, BBS_D2bDescritivo, BBS_D3bDescritivo, BBS_D4bDescritivo, BBS_D5bDescritivo, BBS_D6bDescritivo, BBS_D7bDescritivo, BBS_D8bDescritivo, BBS_OplGravarResponse, BBS_DssCamposResponse, BBS_CdiTransacao_Retorno, BBS_NuiTipoEdicao_Retorno, BBS_CdiModeloIntegracao_Ret, BBS_CdiFormularioWF', /*values*/ 10025, 0, @SQL_CdiComandoSQL, 12, '10020, 1, ''https://api.serasaexperian.com.br/finance/accountspayable/v2/invoices'', 30063, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 3, null, 0, 10024, 1009, ''(TESTE) SERASA - APROVACAO INTERFACE FINANCEIRA'', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, 0, null, 0, 0, 0, 0', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 25, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_CdiTipoComandoIntegr, BBS_DssNomeObjeto, BBS_CdiTransacao, BBS_NuiOrdem, BBS_OplOneForManyInclusao, BBS_OplValidaVazio, BBS_CdiComandoSQL, BBS_OplComandoNativo, BBS_CdiEventoTransacao, BBS_CdiFormularioWF, BBS_D1sModeloIntegracaoCmd, BBS_D2sModeloIntegracaoCmd, BBS_D3sModeloIntegracaoCmd, BBS_D4sModeloIntegracaoCmd, BBS_D5sModeloIntegracaoCmd, BBS_D6sModeloIntegracaoCmd, BBS_D7sModeloIntegracaoCmd, BBS_D8sModeloIntegracaoCmd, BBS_DsbComandoSQLValidacao, BBS_D1bDescritivo, BBS_D2bDescritivo, BBS_D3bDescritivo, BBS_D4bDescritivo, BBS_D5bDescritivo, BBS_D6bDescritivo, BBS_D7bDescritivo, BBS_D8bDescritivo, BBS_CdiModeloIntegracaoCmd_Ftr, BBS_NuiTipoEdicao, BBS_CdiTransacaoWF, BBS_CdiModeloIntegracao_Exc, BBS_CdiModeloIntegracao_Suc, BBS_CdiTpTratamentoIntegracao, BBS_OplExecutarSQL_Apdata, BBS_OplEnviarTudo, BBS_OplDesativado, BBS_CdiVerboHTTP, BBS_DssCamposLote, BBS_OplLoteBase, BBS_CdiLayOutSaida, BBS_CdiModeloIntegracaoCmd_Pre, BBS_CdiLayOutSaida_Autentica, BBS_OplGravarResponse, BBS_DssCamposResponse, BBS_CdiTransacao_Retorno, BBS_NuiTipoEdicao_Retorno, BBS_CdiModeloIntegracao_Ret', /*values*/ '10026, 10021, 1, null, 30122, 0, 0, 0, 0, 0, 0, 0, ''(TESTES) REST ADD DATE/HOUR IN OBJ 3129'', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, null, 0, 0, 0, 0, 0, null, 0, 0, 0', 1  
		/*REST*/ exec sp_Execute_Insert 'dbo', 26, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_CdiTipoComandoIntegr, BBS_DssNomeObjeto, BBS_CdiTransacao, BBS_NuiOrdem, BBS_OplOneForManyInclusao, BBS_OplValidaVazio, BBS_CdiComandoSQL, BBS_OplComandoNativo, BBS_CdiEventoTransacao, BBS_CdiFormularioWF, BBS_D1sModeloIntegracaoCmd, BBS_D2sModeloIntegracaoCmd, BBS_D3sModeloIntegracaoCmd, BBS_D4sModeloIntegracaoCmd, BBS_D5sModeloIntegracaoCmd, BBS_D6sModeloIntegracaoCmd, BBS_D7sModeloIntegracaoCmd, BBS_D8sModeloIntegracaoCmd, BBS_DsbComandoSQLValidacao, BBS_D1bDescritivo, BBS_D2bDescritivo, BBS_D3bDescritivo, BBS_D4bDescritivo, BBS_D5bDescritivo, BBS_D6bDescritivo, BBS_D7bDescritivo, BBS_D8bDescritivo, BBS_CdiModeloIntegracaoCmd_Ftr, BBS_NuiTipoEdicao, BBS_CdiTransacaoWF, BBS_CdiModeloIntegracao_Exc, BBS_CdiModeloIntegracao_Suc, BBS_CdiTpTratamentoIntegracao, BBS_OplExecutarSQL_Apdata, BBS_OplEnviarTudo, BBS_OplDesativado, BBS_CdiVerboHTTP, BBS_DssCamposLote, BBS_OplLoteBase, BBS_CdiLayOutSaida, BBS_CdiModeloIntegracaoCmd_Pre, BBS_CdiLayOutSaida_Autentica, BBS_OplGravarResponse, BBS_DssCamposResponse, BBS_CdiTransacao_Retorno, BBS_NuiTipoEdicao_Retorno, BBS_CdiModeloIntegracao_Ret', /*values*/ '10027, 10022, 1, null, 30063, 0, 0, 0, 0, 0, 0, 0, ''(TESTES) REST MODELO SUCESSO'', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, null, 0, 0, 0, 0, 0, null, 0, 0, 0', 1  
		/*REST*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 27, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP, BBS_DssCamposLote, BBS_OplEnviarTudo', 10028, 0, @SQL_CdiComandoSQL, 16, '10023, ''(TESTES) REST POSTMAN LOTE'', 1, 30063, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v1/get/keyid'', 1, ''keyid'', 1', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 28, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_DssNomeObjeto, BBS_CdiVerboHTTP', '10029, 10024, ''(TESTES) REST MOCK POSTMAN GET COMPLEXY ARRAY'', 1, 30063, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/v3/get/arrayjson'', 1', 1  
		/*REST*/ exec sp_Execute_Insert_ThreeKey 'dbo', 29, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiComandoSQL_Transacao, BBS_CdiComandoSQL_TipoEdicao, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao, BBS_NuiTipoEdicao',/*values*/ 10030, 0,  @SQL_CdiComandoSQL, 18, @SQL_CdiComandoSQL, 19, '10025, ''(TESTES) REST ALTER DA TRANSACAO E O TIPO EDICAO'',  1, 30842, 407', 1 
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 30, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd, BBS_CdiModeloIntegracao, BBS_D1sModeloIntegracaoCmd, BBS_CdiTipoComandoIntegr, BBS_CdiTransacao', '10031, 10026, ''(TESTES) ADMISSAO - INCLUSAO DEPENDENTES'', 1, 16012', 1
		
		/*##### ModelosIntegracoesCmdsCpos*/
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 01, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_CdiTipoCampo, BBP_DssConteudo_String, BBP_OplConteudoFixo', '10001, 10001, ''consultaCEP;cep'', 9, ''03510030'', 1', 1        
		/*REST*/ exec sp_Execute_Insert 'dbo', 02, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_CdiTipoCampo, BBP_DssConteudo_String', '10002, 10002, '''', 9, ''''', 1        
		/*REST*/ exec sp_Execute_Insert 'dbo', 03, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_CdiTipoCampo, BBP_DssConteudo_String, BBP_OplConteudoFixo, BBP_CdiLayOutSaida', '10003, 10003, ''valueParameter'', 9, ''value'', 1, 1001', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 04, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_CdiTipoCampo, BBP_DssConteudo_String, BBP_OplConteudoFixo, BBP_CdiLayOutSaida', '10004, 10004, ''cep'', 9, ''01001000'', 1, 1002', 1
        /*REST*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 05, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiComandoSQL, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', 10005, 0,  @SQL_CdiComandoSQL, 1, '10005, ''idcontratado'', ''USR_CdiUsuario'', 3', 1
		/*REST*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 06, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiComandoSQL, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', 10006, 0,  @SQL_CdiComandoSQL, 2, '10005, '''', ''USR_CosEMail'', 9', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 07, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10007, 10005, ''USR_OplPrimeiroAcesso'', ''USR_OplPrimeiroAcesso'', 12', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 08, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10008, 10005, ''USR_CosSenha'', ''USR_CosSenha'', 9', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 09, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10009, 10006, ''BatidaData'', ''CBD_DtdBatidaData'', 10', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 10, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10010, 10006, ''BatidaHoraMinuto'', ''CBD_HrdBatidaHoraMinuto'', 11', 1
        /*REST*/ exec sp_Execute_Insert 'dbo', 11, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10011, 10006, ''DispositivoAcesso'', ''CBD_NuiDispositivoAcesso'', 9', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 12, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10012, 10006, ''Cdicontratado'', ''CBD_CosCrachaBase'', 9', 1
        /*REST*/ exec sp_Execute_Insert 'dbo', 13, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10013, 10006, ''latitude'', ''CBD_QtnLatitude'', 9', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 14, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10014, 10006, ''longitude'', ''CBD_QtnLongitude'', 9', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 15, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10015, 10007, ''D1sCargo'', ''CAR_D1sCargo'', 9', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 16, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10016, 10007, ''D1sCargoRes'', ''CAR_D1sCargoRes'', 9', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 17, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10017, 10007, ''anexo'', ''CampoVirtual_100505'', 17', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 18, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10018, 10007, ''CBO'', ''CAR_CdiCodBrasileiroOcupacao'', 9', 1 
		/*REST*/ exec sp_Execute_Insert 'dbo', 19, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Origem, BBP_DssCampo_Destino, BBP_CdiTipoCampo, BBP_OplConteudoFixo', '10019, 10008, ''cep'', ''consultaCEP;cep'', 9, 0', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 20, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10020, 10009, ''AGI_CdiAssuntoGeralItem'', ''AGI_CdiAssuntoGeralItem'', 3, 1003', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 21, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10021, 10009, ''AGI_ArbArquivoRelatorio'', ''AGI_ArbArquivoRelatorio'', 18, 1003', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 22, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10022, 10010, ''keyid'', ''keyid'', 9, 1004', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 23, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10023, 10011, ''keyid'', ''keyid'', 9, 1005', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 24, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10024, 10011, ''field1'', ''field1'', 9, 1006', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 25, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10025, 10011, ''field2'', ''field2'', 9, 1006', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 26, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10026, 10012, ''nome'',  ''nome'',  9, 1007', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 27, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10027, 10012, ''email'', ''email'', 9, 1007', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 28, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10028, 10012, ''id'', ''id'', 9, 1007', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 29, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10029, 10015, ''keyid'', ''keyid'', 9, 1008', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 30, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10030, 10016, ''keyid'', ''keyid'', 9, 1004', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 31, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_VrnConteudo_Numerico, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_CdiPais_Registro, BBP_OplConsiderarNulo, BBP_OplExecutarSql_Cliente, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_MsgErroDePara, BBP_D1bMensagemErroDePara, BBP_D2bMensagemErroDePara, BBP_D3bMensagemErroDePara', /*values*/ '10031, 10018, ''CBE_CdiConBatidaReal'', ''CBE_CdiConBatidaReal'', 3, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, null, null, 0, 0, 0, null, null, null', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 32, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_VrnConteudo_Numerico, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_CdiPais_Registro, BBP_OplConsiderarNulo, BBP_OplExecutarSql_Cliente, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_MsgErroDePara, BBP_D1bMensagemErroDePara, BBP_D2bMensagemErroDePara, BBP_D3bMensagemErroDePara', /*values*/ '10032, 10018, ''CBE_CdiOcorrenciaMarcacao'', ''CBE_CdiOcorrenciaMarcacao'', 3, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, null, null, 0, 0, 0, null, null, null', 1
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 33, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_VrnConteudo_Numerico, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_CdiPais_Registro, BBP_OplConsiderarNulo, BBP_OplExecutarSql_Cliente, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_MsgErroDePara, BBP_D1bMensagemErroDePara, BBP_D2bMensagemErroDePara, BBP_D3bMensagemErroDePara', /*values*/ '10033, 10023, ''login;credential;companyId'', null, 9, 1, 0, null, ''tecnologiaT1'', 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, null, null, 0, 0, 0, null, null, null', 1
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 34, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_VrnConteudo_Numerico, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_CdiPais_Registro, BBP_OplConsiderarNulo, BBP_OplExecutarSql_Cliente, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_MsgErroDePara, BBP_D1bMensagemErroDePara, BBP_D2bMensagemErroDePara, BBP_D3bMensagemErroDePara', /*values*/ '10034, 10023, ''login;credential;username'', null, 9, 1, 0, null, ''sfapi'', 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, null, null, 0, 0, 0, null, null, null', 1
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 35, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_VrnConteudo_Numerico, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_CdiPais_Registro, BBP_OplConsiderarNulo, BBP_OplExecutarSql_Cliente, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_MsgErroDePara, BBP_D1bMensagemErroDePara, BBP_D2bMensagemErroDePara, BBP_D3bMensagemErroDePara', /*values*/ '10035, 10023, ''login;credential;password'', null, 9, 1, 0, null, ''BanTec2020@#$'', 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, null, null, 0, 0, 0, null, null, null', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 36, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10036, 10024, ''username'', 0, ''username'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 0, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 38, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10038, 10024, ''documentNumber_billingFrom'', 0, ''documentNumber_billingFrom'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 39, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10039, 10024, ''documentType_billingFrom'', 0, ''documentType_billingFrom'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 40, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10040, 10024, ''documentNumber_billingTo'', 0, ''documentNumber_billingTo'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 41, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10041, 10024, ''documentType_billingTo'', 0, ''documentType_billingTo'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 42, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10042, 10024, ''amount'', 0, ''amount'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 43, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10043, 10024, ''date_invoice'', 0, ''date_invoice'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 44, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10044, 10024, ''description_invoice'', 0, ''description_invoice'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 45, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10045, 10024, ''invoiceId'', 0, ''invoiceId'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 46, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10046, 10024, ''paymentCreationDate'', 0, ''paymentCreationDate'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 47, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10047, 10024, ''paymentDate'', 0, ''paymentDate'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 48, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10048, 10024, ''accountingKey'', 0, ''accountingKey'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 49, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10049, 10024, ''fixedAssetAccount'', 0, ''fixedAssetAccount'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 50, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10050, 10024, ''fixedAssetBookType'', 0, ''fixedAssetBookType'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 51, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10051, 10024, ''fixedAssetCategory'', 0, ''fixedAssetCategory'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 52, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10052, 10024, ''fixedAssetType'', 0, ''fixedAssetType'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 53, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10053, 10024, ''itemAmount'', 0, ''itemAmount'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 54, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10054, 10024, ''itemDescription'', 0, ''itemDescription'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 55, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10055, 10024, ''registrationRequestNumber'', 0, ''registrationRequestNumber'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
	    /*REST*/ exec sp_Execute_Insert 'dbo', 56, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10056, 10024, ''sourceSystem'', 0, ''sourceSystem'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 57, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_VrnConteudo_Numerico, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_OplConteudoFixo, BBP_NuiConteudo_Inteiro, BBP_DtdConteudo_DataHora, BBP_DssConteudo_String, BBP_OplConteudo_Logico, BBP_VlnConteudo_Numerico, BBP_OplConsiderarNulo, BBP_DsbConteudo_Blob, BBP_OplFiltro, BBP_CdiModeloDeParaGeral, BBP_CdiCampo_TabelaRelacionada, BBP_CdiComandoSQL, BBP_OplExecutarSql_Cliente, BBP_CdiCalculoLPC, BBP_CdiLayOutSaida, BBP_CdiOperacaoLogica, BBP_OplComandoNativo, BBP_CosAgrupamento, BBP_OplParametroWSRetorno, BBP_OplEncriptografarCpoBlob, BBP_OplDesconsiderarCpoVazio, BBP_DssArquivoRetorno, BBP_CosExtensaoArquivoRetorno, BBP_CdiAcaoArquivoIntegracao, BBP_CdiTipoArquivoIntegracao, BBP_CdiOpcao_OcultarVazio, BBP_NuiOrdem, BBP_DssConteudo_Senha, BBP_CdiPais_Registro', /*values*/ '10057, 10024, ''users'', 0, ''users'', 9, 0, 0, null, null, 0, 0, 0, null, 0, 0, 0, 0, 0, 0, 1010, 0, 0, null, 0, 0, 0, null, null, 0, 0, 0, 0, null, 0', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 58, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', /*values*/ '10058, 10026, ''NUMEROINFORMADO'', ''CEX_CosCrachaBase'', 9', 1
		/*REST*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 59, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiComandoSQL, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', /*values*/ 10059, 0, @SQL_CdiComandoSQL, 14, '10026, null, ''CEX_DtdValidadeInicio'', 9', 1
        /*REST*/ exec sp_Execute_Insert 'dbo', 60, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiComandoSQL', /*values*/ '10060, 10026, ''DATA_FIM_VALIDADE'', ''CEX_DtdValidadeFim'', 16, 0', 1
		/*REST*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 61, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiComandoSQL, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', /*values*/ 10061, 0, @SQL_CdiComandoSQL, 15, '10026, null, ''CEX_D1sCrachaExtra'', 9', 1
		/*REST*/ exec sp_Execute_Insert_Key_ForeignKey 'dbo', 62, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiComandoSQL, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', /*values*/ 10062, 0, @SQL_CdiComandoSQL, 15, '10026, null, ''CEX_CdiCrachaExtra'', 3', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 62, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo, BBP_CdiLayOutSaida', '10063, 10028, ''keyid'', ''keyid'', 9, 1004', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 63, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10064, 10030, ''D1sCargo'', ''CAR_D1sCargo'', 9', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 64, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10065, 10030, ''D1sCargoRes'', ''CAR_D1sCargoRes'', 9', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 65, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10066, 10030, ''CBO'', ''CAR_CdiCodBrasileiroOcupacao'', 9', 1 
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 66, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10067, 10031, ''DEP_CdiContratado'', ''DEP_CdiContratado'', 8', 1
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 67, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10068, 10031, ''DEP_CdiLigacaoPessoa'', ''DEP_CdiLigacaoPessoa'', 8', 1
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 68, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10069, 10031, ''DEP_CdiSexo'', ''DEP_CdiSexo'', 8', 1
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 69, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10070, 10031, ''DEP_CdiEstadoCivil'', ''DEP_CdiEstadoCivil'', 8', 1
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 70, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10071, 10031, ''DEP_CdiEstado_OrgaoRg'', ''DEP_CdiEstado_OrgaoRg'', 8', 1
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 71, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10072, 10031, ''DEP_CdiSituacaoDependente'', ''DEP_CdiSituacaoDependente'', 8', 1
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 72, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10073, 10031, ''DEP_DssNomeCompleto'', ''DEP_DssNomeCompleto'', 9', 1
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 73, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10074, 10031, ''DEP_DtdApresCertNascimento'', ''DEP_DtdApresCertNascimento'', 10', 1
        /*SOAP*/ exec sp_Execute_Insert 'dbo', 74, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10075, 10031, ''DEP_OplImpRendaDependente'', ''DEP_OplImpRendaDependente'', 8', 1
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 75, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10076, 10031, ''DEP_DssNome'', ''DEP_DssNome'', 9', 1
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 76, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10077, 10031, ''DEP_CosNumeroRg'', ''DEP_CosNumeroRg'', 9', 1
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 77, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10078, 10031, ''DEP_CosOrgaoRg'', ''DEP_CosOrgaoRg'', 9', 1
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 78, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10079, 10031, ''DEP_DtdEmissaoRg'', ''DEP_DtdEmissaoRg'', 10', 1
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 79, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10080, 10031, ''DEP_NusCICNumero'', ''DEP_NusCICNumero'', 9', 1
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 80, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10081, 10031, ''DEP_DssNascimentoLocal'', ''DEP_DssNascimentoLocal'', 9', 1
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 81, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10082, 10031, ''DEP_DtdNascimentoData'', ''DEP_DtdNascimentoData'', 10', 1
        /*SOAP*/ exec sp_Execute_Insert 'dbo', 81, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10083, 10031, ''DEP_DssNomePai'', ''DEP_DssNomePai'', 9', 1
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 81, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10084, 10031, ''DEP_DssNomeMae'', ''DEP_DssNomeMae'', 9', 1
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 81, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10085, 10031, ''DEP_CosCartaoNacionalSaude'', ''DEP_CosCartaoNacionalSaude'', 9', 1
		/*SOAP*/ exec sp_Execute_Insert 'dbo', 81, 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmdCpo, BBP_CdiModeloIntegracaoCmd, BBP_DssCampo_Destino, BBP_DssCampo_Origem, BBP_CdiTipoCampo', '10086, 10031, ''DPF_CdiCodigoFlexivelDep01'', ''DPF_CdiCodigoFlexivelDep01'', 10', 1

		/*##### ModelosIntegracoesCmdsRets*/
		/*REST*/ exec sp_Execute_Insert 'dbo', 01, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10002, 10016, ''userid'', ''data;userid''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 02, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10003, 10016, ''name'', ''data;name''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 03, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10004, 10016, ''age'', ''data;age''', 1
		/*REST*/ exec sp_Execute_Insert 'dbo', 04, 'ModelosIntegracoesCmdsRets', 'JWR_CdiModeloIntegracaoCmdRets, JWR_CdiModeloIntegracaoCmd, JWR_DssCampoDestino, JWR_DssCampoOrigem', '10005, 10016, ''verb'', ''data;verb''', 1

		/*#### OBJETO - 550 - ABA BASES DE DADOS - ServidoresIntegracoesBDs*/
		exec sp_Execute_Insert 'dbo', 01, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '01, 1, ''(TESTES) SERVICO SOAP CORREIOS'', 10001, 1, 6, 1, ''https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl''', 1
		exec sp_Execute_Insert 'dbo', 02, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '02, 1, ''(TESTES) SERVICO REST SEM PARAMETROS'', 10002, 1, 10, 1, ''http://echo.jsontest.com/key/value/one/two''', 1
		exec sp_Execute_Insert 'dbo', 03, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '03, 1, ''(TESTES) SERVICO REST COM PARAMETROS'', 10003, 1, 10, 1, ''http://validate.jsontest.com/''', 1
		exec sp_Execute_Insert 'dbo', 04, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '04, 1, ''(TESTES) SERVICO REST COM PARAMETROS URL'', 10004, 1, 10, 1, ''https://viacep.com.br/''', 1
		exec sp_Execute_Insert 'dbo', 05, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '05, 1, ''(TESTES) SERVICO REST ALTERACAO OBJ 2330'', 10005, 2, 10, 1, ''''', 1
		exec sp_Execute_Insert 'dbo', 06, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '06, 1, ''(TESTES) SERVICO REST MARCACAO PONTO'', 10006, 2, 10, 0, ''''', 1
		exec sp_Execute_Insert 'dbo', 07, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '07, 1, ''(TESTES) SERVICO REST ENTRADA BASEX64'', 10007, 2, 10, 0, ''''', 1
		exec sp_Execute_Insert 'dbo', 08, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor, BBO_CdiTipoAutentConsWebServi', '08, 1, ''(TESTES) SERVICO SOAP CORREIOS LOTE'', 10008, 1, 6, 0,  ''https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl'', 1', 1
		exec sp_Execute_Insert 'dbo', 09, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '09, 1, ''(TESTES) SERVICO MOCK POSTMAN GET'', 10010, 1, 10, 1, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/''', 1
		exec sp_Execute_Insert 'dbo', 10, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '10, 1, ''(TESTES) SERVICO MOCK POSTMAN PUT'', 10011, 1, 10, 1, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/''', 1 
		exec sp_Execute_Insert 'dbo', 11, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '11, 1, ''(TESTES) SERVICO MOCK POSTMAN POST AT'', 10012, 1, 10, 1, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/''', 1 
		exec sp_Execute_Insert 'dbo', 12, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor, BBO_CdiTipoAutentConsWebServi', '12, 1, ''(TESTES) SERVICO REST CONSULTAS REMOTAS'',  10013, 2, 10, 1, '''', 5', 1 
		exec sp_Execute_Insert 'dbo', 13, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '13, 1, ''(TESTES) SERVICO MOCK POSTMAN GET ARRAY'',  10014, 1, 10, 1, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/''', 1
		exec sp_Execute_Insert 'dbo', 14, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '14, 1, ''(TESTES) SERVICO API THIRDPART GET'', 10015, 1, 10, 1, ''https://62d6befa51e6e8f06f1214f9.mockapi.io/api''', 1
		exec sp_Execute_Insert 'dbo', 15, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '15, 1, ''(TESTES) SERVICO MOCK REST REPROCESS'', 10016, 1, 10, 1, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/''', 1
		exec sp_Execute_Insert 'dbo', 16, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '16, 1, ''(TESTES) SERVICO MOCK POSTMAN GET COMPLEXY ARRAY'',  10017, 1, 10, 1, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/''', 1
		exec sp_Execute_Insert 'dbo', 17, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '17, 1, ''(TESTES) SERVICO COMBATIDAS REAIS'',  10018, 1, 10, 1, ''''', 1
		exec sp_Execute_Insert 'dbo', 18, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '18, 1, ''(TESTES) SERVICO SOAP TESTE WSDL TECBAN'',  10019, 1, 6, 1, ''C:\Users\flsantos\OneDrive - Apdata do Brasil Software Ltda\Chamados\Wsdl\SFSF_PO.wsdl''', 1
		exec sp_Execute_Insert 'dbo', 19, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '19, 1, ''(TESTES) SERVICO LOTES REST'',  10023, 1, 10, 1, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/''', 1
		exec sp_Execute_Insert 'dbo', 20, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '20, 1, ''(TESTES) SERVICO MOCK POSTMAN GET COMPLEXY ARRAY'',  10024, 1, 10, 1, ''https://20e776d9-fadf-47c1-91c9-02f58291b9c1.mock.pstmn.io/api/''', 1
		exec sp_Execute_Insert 'dbo', 21, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '21, 1, ''(TESTES) REST ALTER DA TRANSACAO E O TIPO EDICAO'', 10025, 2, 10, 0, ''''', 1
		exec sp_Execute_Insert 'dbo', 22, 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD, BBO_CdiServidorIntegracao, BBO_D1sServidorIntegracaoBD, BBO_CdiModeloIntegracao, BBO_CdiTipoIntegracao, BBO_CdiBaseDado, BBO_CdiTipoConexaoBaseDado, BBO_DssNomeServidor', '22, 1, ''(TESTES) SOAP ADMISSAO - INCLUSAO DEPENDENTES'', 10026, 2, 10, 0, ''''', 1

		/*OUTROS AJUSTES PARA TESTES*/
		exec sp_Execute_Update 'dbo', 01, 'FormulariosWFSobreps', 'BRH_CdiOpcao_Desativado = 0', 'BRH_CdiFormularioWF = 407'
		exec sp_Execute_Update 'dbo', 02, 'FormulariosWFSobreps', 'BRH_CdiOpcao_Desativado = 0', 'BRH_CdiFormularioWF = 407'
		exec sp_Execute_Update 'dbo', 03, 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracao_Suc = 10022', 'BBS_CdiModeloIntegracaoCmd = 10005' 

		exec sp_Execute_Insert 'dbo', 01, 'FormulariosWFCampos', 'FWC_CdiFormularioWFCampo, FWC_CdiFormularioWF, FWC_CdiCampo, FWC_CdiClasseProcCpoPar, FWC_NuiSequencial, FWC_NuiOrdem, FWC_OplReferencia, FWC_OplLigacao, FWC_OplCampoBase, FWC_CdiCampoAgrupamento, FWC_OplDataBase, FWC_CdiOpcao_InfObrigatoria, FWC_OplDataBaseBloqueio, FWC_CdiObjetoLookup, FWC_DsbSqlLookupField, FWC_CdiOpcao_Protocolo, FWC_DssContDefault_String, FWC_DtdContDefault_DataHora, FWC_NuiContDefault_Inteiro, FWC_OplContDefault_Logico, FWC_VlnContDefault_Numerico, FWC_VrnContDefault_Numerico, FWC_CdiOpcao_Default, FWC_CdiDominio, FWC_D1sLiteral, FWC_D2sLiteral, FWC_D3sLiteral, FWC_D4sLiteral, FWC_D5sLiteral, FWC_D6sLiteral, FWC_D7sLiteral, FWC_D8sLiteral, FWC_OplInformacaoObrigatoria, FWC_OplProtocolo, FWC_OplLigacaoFilho, FWC_CdiOpcao_LookupTodasEtapas, FWC_DsbContDefault_Blob, FWC_OplDesabilitaCpoLkpParam, FWC_CdiCampoFlexivel, FWC_OplDesativado, FWC_OplCampoCondicao, FWC_DsbSqlCampoVirtual, FWC_CdiAcaoCampo, FWC_CdiOpcao_GdMultiTransacao, FWC_NuiOrigemRegistro, FWC_OplMultiplaSelecao, FWC_OplExibirAjuda, FWC_D1bAjudaCampo, FWC_D2bAjudaCampo, FWC_D3bAjudaCampo, FWC_D4bAjudaCampo, FWC_D5bAjudaCampo, FWC_D6bAjudaCampo, FWC_D7bAjudaCampo, FWC_D8bAjudaCampo', '100505, 407, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, null, 0, null, null, 0, 0, 0, 0, 0, 214, ''ANEXO'', null, null, null, null, null, null, null, 0, 0, 0, 0, null, 0, 0, 0, 0, null, 0, 0, 4, 0, 0, null, null, null, null, null, null, null, null', 1
		exec sp_Execute_Insert 'dbo', 01, 'UsuariosAutenticacoes', 'JVQ_CdiUsuarioAutenticacao, JVQ_CdiUsuario, JVQ_CdsClientId, JVQ_CdsSecretKey, JVQ_CdiPerfil, JVQ_NuiMinutosValidadeToken, JVQ_D1sDescricao, JVQ_D2sDescricao, JVQ_D3sDescricao, JVQ_D4sDescricao, JVQ_D5sDescricao, JVQ_D6sDescricao, JVQ_D7sDescricao, JVQ_D8sDescricao', '1, 1, ''1658444F-EF87-47E7-B62C-F4F70BACE420'', ''@@/WJ8YRQ7Di9Sq/ci8cU2qJRdFdDvz9RefzHbOTyHNfoZpTtpog9cY/qjfqtQFtwxwo3w9bBxRbZmAyW/WkkcpUcaUbu+33yM'', 1, 10, ''Apdata OAuth2'', ''j2Bu6Bc6xuNZMM35x8ED4qN7cGJT5eH4'', null, null, null, null, null, null', 1 

		exec sp_takeKeyForInsertion 'Consultas', @MaxKeyFromTable OUTPUT 

    	/*Desativa a tag de segurança para consultas*/
		exec sp_Execute_Update 'dbo', '01', 'Usuarios', 'USR_OplForcarUsoTAGApDesig = 0', 'USR_CdiUsuario = 1', 1

		/*Query Execute*/
		exec sp_Execute_Insert_Key 'dbo', 01, 'Consultas', 'ACS_CdiConsulta, ACS_CdiConsultaGrupo, ACS_DtdAbertura, ACS_DsbConteudo, ACS_DssConsulta, ACS_OplExigirSenhaAdicl, ACS_CdiPais, ACS_OplFolhasDesativadas, ACS_OplPublico, ACS_NuiIcone, ACS_NuiIcone_Selecionado, ACS_NuiIcone_WorkArea, ACS_OplVisContratadoConectado, ACS_D1sNomeReferencia, ACS_D2sNomeReferencia, ACS_D3sNomeReferencia, ACS_D4sNomeReferencia, ACS_D5sNomeReferencia, ACS_D6sNomeReferencia, ACS_D7sNomeReferencia, ACS_D8sNomeReferencia, ACS_D1bAjuda, ACS_D2bAjuda, ACS_D3bAjuda, ACS_D4bAjuda, ACS_D5bAjuda, ACS_D6bAjuda, ACS_D7bAjuda, ACS_D8bAjuda, ACS_OplAltoConsumoRecurso, ACS_D1sConsultaExplicacao, ACS_D2sConsultaExplicacao, ACS_D3sConsultaExplicacao, ACS_D4sConsultaExplicacao, ACS_D5sConsultaExplicacao, ACS_D6sConsultaExplicacao, ACS_D7sConsultaExplicacao, ACS_D8sConsultaExplicacao, ACS_OplDesativado', @MaxKeyFromTable, 0, '75, null, 0x545046300654646153514C00035461670372631147756964436F6C6C6174696F6E54797065070D67634D5353514C5365727665720C446174616261736554797065070D64744D5353514C5365727665721044617461506970656C696E654E616D65060B426C6F624172717569766F0D4564697453514C41735465787409094C696E6B436F6C6F720707636C426C61636B084C696E6B5479706507126C74506172616D65746572697A656453514C164D617853514C4669656C64416C6961734C656E67746802000F53514C546578742E537472696E677301063073656C656374204C57435F4364694C616E63616D656E746F57462C204C57435F447362436F6E746575646F5F426C6F62061B202046726F6D204C616E63616D656E746F73574643616D706F7320067020207768657265204C57435F4364694C616E63616D656E746F574620696E202873656C656374204C57465F4364694C616E63616D656E746F57462046726F6D204C616E63616D656E746F735746207768657265204C57465F436469466F726D756C6172696F5746203D203A496446572906262020416E64204C57435F447362436F6E746575646F5F426C6F62206973206E6F74206E756C6C1200000000000753514C547970650706737153514C32074C69746572616C060B426C6F624172717569766F06506172616D730E01084461746154797065070C667457696465537472696E67044E616D6506044964465709506172616D5479706507097074556E6B6E6F776E00000D4E6F5472616E736C6174696F6E0800085464614669656C640864614669656C643105416C69617314170000004C616EC3A7616D656E746F20646520576F726B666C6F7708446174615479706507096474496E74656765720C446973706C6179576964746802080A4669656C64416C69617314170000004C616EC3A7616D656E746F20646520576F726B666C6F770B4669656C644C656E677468020A094669656C644E616D6506134C57435F4364694C616E63616D656E746F57460C53514C4669656C644E616D6506134C57435F4364694C616E63616D656E746F5746095461626C654E616D6506134C616E63616D656E746F73574643616D706F730000085464614669656C640864614669656C643205416C6961731410000000436F6E7465C3BA646F202D20426C6F6208446174615479706507066474424C4F420C446973706C6179576964746803AD0D0A4669656C64416C6961731410000000436F6E7465C3BA646F202D20426C6F620B4669656C644C656E677468038813094669656C644E616D6506144C57435F447362436F6E746575646F5F426C6F62084C696E6B61626C65080C53514C4669656C644E616D6506144C57435F447362436F6E746575646F5F426C6F62095461626C654E616D6506134C616E63616D656E746F73574643616D706F73000000, ''BlobArquivo'', 0, 1, 0, 0, 0, 0, 0, 0, ''BlobArquivo'', ''Español='', ''BlobArquivo'', ''BlobArquivo'', ''BlobArquivo'', ''BlobArquivo'', ''BlobArquivo'', ''BlobArquivo'', null, null, null, null, null, null, null, null, 0, '''', '''', '''', '''', '''', '''', '''', '''', 0', 1
	
	    /*ADINTEGRATOR - ACTIVE DIRECTORY*/
			exec sp_takeKeyForInsertion 'DefSisIntegracaoAD', @AuxKey OUTPUT

			exec sp_Execute_Insert_Key 'dbo', 01, 'DefSisIntegracaoAD', 'DZW_CdiSistema, DZW_DtdOficializacaoSistema, DZW_OplAtivaIntegracao, DZW_OplCriacaoUsuarioAut, DZW_DssCaminhoLDAP, DZW_OplIntegraViaWS, DZW_DssWSCriaUsuario, DZW_DssWSAtualizaDados, DZW_DssWSTrocaSenha, DZW_DssWSResetaSenha, DZW_DssWSAtivaDesativaUsuario, DZW_CdsWSUsuario, DZW_CosWSSenha, DZW_OplAtivaLogIntegracao, DZW_OplNaoSincronizarGrupo, DZW_OplNaoSincronizarEstrutura, DZW_DssWSValidaLogin, DZW_DssWSTrataSSO', @AuxKey/*72*/, 0,  'null, 1, 0, ''LDAP://DC=apdatatst,DC=com,DC=br'', 1, ''http://172.27.10.50/ADIProduto/ApADIntegratorWS.dll/soap/IApADIntegrationIntf'', ''http://172.27.10.50/ADIProduto/ApADIntegratorWS.dll/soap/IApADIntegrationIntf'', null, null, ''http://172.27.10.50/ADIProduto/ApADIntegratorWS.dll/soap/IApADIntegrationIntf'', ''caraujo'', ''CAgelado!@24'', 1, 0, 0, null, null', 1
			
			declare @EstruturasADKey int
			exec sp_takeKeyForInsertion 'EstruturasAD', @EstruturasADKey OUTPUT
			exec sp_Execute_Insert_Key 'dbo', 01, 'EstruturasAD', 'DZY_CdiEstruturaAD, DZY_D1sDescricaoEstruturaAD, DZY_D2sDescricaoEstruturaAD, DZY_D3sDescricaoEstruturaAD, DZY_D4sDescricaoEstruturaAD, DZY_D5sDescricaoEstruturaAD, DZY_D6sDescricaoEstruturaAD, DZY_D7sDescricaoEstruturaAD, DZY_D8sDescricaoEstruturaAD, DZY_CdiDefault, DZY_OplSemFiltro, DZY_NuiOrdem, DZY_DssCaminhoLDAP, DZY_OplIgnorarEstrutsSup, DZY_OplNaoIntegrar', @EstruturasADKey/*1002*/, 0, '''(TESTE) BASIC FIELDS FOR AD CONFIGURATION'', null, null, null, null, null, null, null, 1, 0, 0, ''LDAP://OU=Transitorio'', 0, 0', 1 

			declare @EstruturasADPropsKey int
			exec sp_takeKeyForInsertion 'EstruturasADProps', @EstruturasADPropsKey OUTPUT

			exec sp_Execute_Insert_Key_ForeignKey 'dbo', 01, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor, EBC_CdiComandoSQL', @EstruturasADPropsKey, 0, @EstruturasADKey, 0, '105848, 38, 0, 0', 1   
	        exec sp_Execute_Insert_Key_ForeignKey 'dbo', 02, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor, EBC_CdiComandoSQL', @EstruturasADPropsKey, 1, @EstruturasADKey, 0, '91403, 7, 0, 0', 1
			exec sp_Execute_Insert_Key_ForeignKey 'dbo', 03, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor, EBC_CdiComandoSQL', @EstruturasADPropsKey, 2, @EstruturasADKey, 0, '96978, 39, 0, 0', 1
			exec sp_Execute_Insert_Key_ForeignKey 'dbo', 04, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor, EBC_CdiComandoSQL', @EstruturasADPropsKey, 3, @EstruturasADKey, 0, '9420, 35, 1, 0', 1
			
			exec sp_Execute_Insert_ThreeKey 'dbo', 05, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiComandoSQL, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor', @EstruturasADPropsKey, 4, @EstruturasADKey, 0, @SQL_CdiComandoSQL, 11, '89918, 6, 0', 1
			exec sp_Execute_Insert_ThreeKey 'dbo', 06, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiComandoSQL, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor', @EstruturasADPropsKey, 5, @EstruturasADKey, 0, @SQL_CdiComandoSQL, 10, '0, 3, 0', 1
			exec sp_Execute_Insert_ThreeKey 'dbo', 07, 'EstruturasADProps', 'EBC_CdiEstruturaADProp, EBC_CdiEstruturaAD, EBC_CdiComandoSQL, EBC_CdiCampo, EBC_CdiPropriedadeAD, EBC_OplConsDescLookupValor', @EstruturasADPropsKey, 6, @EstruturasADKey, 0, @SQL_CdiComandoSQL, 11, '0, 0, 0', 1
		/*ADINTEGRATOR - ACTIVE DIRECTORY*/
	commit;
end
GO

/**********************************************************************
  8 - Generate Insert From Select Table
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
  9 - Generate Insert From Select Table
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
    10 - Convert Binary To Text
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
    11 - SELECT INTO 
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
    12 - STRING SPLIT (COMPATIBILIDADE DE FUNCAO)
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
    13 - CREATE FOLDER
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
    14 - EXPORT FILE
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

exec sp_StandardData_FixedValues 0 
GO