DECLARE @FileName NVARCHAR(1000)
SET @FileName = '$(fileName)'
DECLARE @ComandoRestore NVARCHAR(MAX)
SET @ComandoRestore = N'RESTORE DATABASE [INTEGRATION_BETA] FROM DISK = N''C:\BasesOficiais\' + @FileName + N''' WITH FILE = 1, NOUNLOAD, REPLACE, STATS = 10'
EXEC sp_executesql @ComandoRestore
