SET NOCOUNT ON;

DECLARE @Arquivo VARBINARY(MAX);

SELECT @Arquivo = KBR_DsbFoto
FROM ConFotosOriginais
WHERE KBR_CdiConFotoOriginal = 1;

SELECT @Arquivo;

--bcp "SELECT KBR_DsbFoto FROM C437_HOM_GSS.dbo.ConFotosOriginais WHERE KBR_CdiConFotoOriginal=54" queryout "C:\tmp\foto_54.jpg" -S 172.26.100.149,1518 -U apdata -P apdata -n

/*
SELECT 
  'bcp "SELECT KBR_DsbFoto FROM C437_HOM_GSS.dbo.ConFotosOriginais WHERE KBR_CdiConFotoOriginal=' 
  + CAST(KBR_CdiConFotoOriginal AS VARCHAR) 
  + '" queryout "C:\tmp\foto_' 
  + CAST(KBR_CdiConFotoOriginal AS VARCHAR) 
  + '.jpg" -S 172.26.100.149 -U apdata -P apdata -n'
FROM ConFotosOriginais
where KBR_CdiConFotoOriginal = 54
*/

