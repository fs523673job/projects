/* Inserir arquivos em campos Blobs */
select * from RepositoriosArqsIntegrs

Insert Into RepositoriosArqsIntegrs (IYS_CdiRepositorioArqIntegr, IYS_DssNomeArquivo, IYS_ArbArquivoAnexo, IYS_CdiStatusProcArquivo, IYS_CdiTipoArquivoIntegracao)
values (1, 'ExArqRetorno.Xml',  (SELECT * FROM OPENROWSET(BULK N'D:\Chamados\Arquivos\421168\Arquivos_XML\ExArqRetorno.Xml', SINGLE_BLOB) AS IYS_ArbArquivoAnexo), 1, 1)