SELECT 'DROP SEQUENCE ' + sch.name + '.' + seq.name
  FROM sys.sequences AS seq
 INNER JOIN sys.schemas AS sch ON seq.schema_id = sch.schema_id