use INTEGRATION_BETA
go

select * from ServidoresIntegracoesBDs --where BBO_CdiServidorIntegracaoBD = 5
select * from ServidoresIntegracoes
select * from TransacoesIntegracoesSobs
select * from TransacoesIntegracoes
select * from DLLsIntegracoesMetodos
select * from DLLsIntegracoes
select * from ModelosIntegracoesCmdsRets
select * from ComandosItensHTTP
select * from ModsIntsMonitsBasesEventos
select * from ModelosIntegracoesSobs
select * from ModelosIntegracoesCmdsRets
select * from ModelosIntegracoesQueries
select * from ModelosIntegracoesCmdsPars
select * from ModelosIntegracoesCmdsCpos --where BBP_CdiModeloIntegracaoCmd = 10005
select * from ModelosIntegracoesCmds
select * from ModelosIntegracoes 
select * from LayoutsSaidas
select * from ComandosSQLs
select * from ComandosSQLsSobs
select * from ComandosSQLsGrupos
select * from TiposComandosIntegrs
select * from LogsTransacoes where LTR_CdiLogTransacao = 10
select * from LogsIntegracoes
select * from LogsIntegracoesServidores
select * from EstruturasAD
select * from EstruturasADProps

select count(BBO_CdiServidorIntegracaoBD) as ServidoresIntegracoesBDs  from ServidoresIntegracoesBDs
select count(BBN_CdiServidorIntegracao) as ServidoresIntegracoes from ServidoresIntegracoes
select count(HIS_CdiTransacaoIntegracao) as TransacoesIntegracoesSobs from TransacoesIntegracoesSobs
select count(BBX_CdiTransacaoIntegracao) as TransacoesIntegracoes from TransacoesIntegracoes
select count(BBW_CdiDLLIntegracaoMetodo) as DLLsIntegracoesMetodos from DLLsIntegracoesMetodos
select count(BBV_CdiDLLIntegracao) as DLLsIntegracoes from DLLsIntegracoes
select count(JWR_CdiModeloIntegracaoCmdRets) as ModelosIntegracoesCmdsRets from ModelosIntegracoesCmdsRets
select count(JIF_CdiComandoItemHTTP) as ComandosItensHTTP from ComandosItensHTTP
select count(EBK_CdiModIntMonitBaseEvento) as ModsIntsMonitsBasesEventos from ModsIntsMonitsBasesEventos
select count(IKG_CdiModeloIntegracao) as ModelosIntegracoesSobs from ModelosIntegracoesSobs
select count(EMB_CdiModeloIntegracaoQuerie) as ModelosIntegracoesQueries from ModelosIntegracoesQueries
select count(ETE_CdiModeloIntegracaoCmdPar) as ModelosIntegracoesCmdsPars from ModelosIntegracoesCmdsPars
select count(BBP_CdiModeloIntegracaoCmdCpo) as ModelosIntegracoesCmdsCpos from ModelosIntegracoesCmdsCpos
select count(BBS_CdiModeloIntegracaoCmd) as ModelosIntegracoesCmds from ModelosIntegracoesCmds
select count(BBR_CdiModeloIntegracao) as ModelosIntegracoes from ModelosIntegracoes
select count(BRD_CdiLayOutSaida) as LayoutsSaidas from LayoutsSaidas

select [dbo].[fn_getPKFieldName]('ServidoresIntegracoes')
select [dbo].[fn_getTableCount]('ServidoresIntegracoes')
select [dbo].[fn_getNuiPkLimiteApdata]('ServidoresIntegracoes')
select [dbo].[fn_lastIdTable]('ServidoresIntegracoes')
select [dbo].[fn_getTableMaxKey]('ServidoresIntegracoes')

exec sp_lastIdTable 'ServidoresIntegracoesBDs'
exec sp_lastIdTable 'ServidoresIntegracoes'
exec sp_lastIdTable 'TransacoesIntegracoesSobs'
exec sp_lastIdTable 'TransacoesIntegracoes'
exec sp_lastIdTable 'DLLsIntegracoesMetodos'
exec sp_lastIdTable 'DLLsIntegracoes'
exec sp_lastIdTable 'ModelosIntegracoesCmdsRets'
exec sp_lastIdTable 'ComandosItensHTTP'
exec sp_lastIdTable 'ModsIntsMonitsBasesEventos'
exec sp_lastIdTable 'ModelosIntegracoesSobs'
exec sp_lastIdTable 'ModelosIntegracoesQueries'
exec sp_lastIdTable 'ModelosIntegracoesCmdsPars'
exec sp_lastIdTable 'ModelosIntegracoesCmdsCpos'
exec sp_lastIdTable 'ModelosIntegracoesCmds'
exec sp_lastIdTable 'ModelosIntegracoes' 
exec sp_lastIdTable 'ComandosSQLs'
exec sp_lastIdTable 'ComandosSQLsGrupos'
exec sp_lastIdTable 'EstruturasAD'
exec sp_lastIdTable 'DefSisIntegracaoAD'
exec sp_lastIdTable 'Defaults'

exec sp_takeKeyForInsertion 'DefSisIntegracaoAD'
exec sp_takeKeyForInsertion 'ServidoresIntegracoesBDs'
exec sp_takeKeyForInsertion 'Defaults'

exec sp_infoApDataPkLimit 'ServidoresIntegracoes'
exec sp_infoApDataPkLimit 'TransacoesIntegracoesSobs'
exec sp_infoApDataPkLimit 'TransacoesIntegracoes'
exec sp_infoApDataPkLimit 'DLLsIntegracoesMetodos'
exec sp_infoApDataPkLimit 'DLLsIntegracoes'
exec sp_infoApDataPkLimit 'ModelosIntegracoesCmdsRets'
exec sp_infoApDataPkLimit 'ComandosItensHTTP'
exec sp_infoApDataPkLimit 'ModsIntsMonitsBasesEventos'
exec sp_infoApDataPkLimit 'ModelosIntegracoesSobs'
exec sp_infoApDataPkLimit 'ModelosIntegracoesQueries'
exec sp_infoApDataPkLimit 'ModelosIntegracoesCmdsPars'
exec sp_infoApDataPkLimit 'ModelosIntegracoesCmdsCpos'
exec sp_infoApDataPkLimit 'ModelosIntegracoesCmds'
exec sp_infoApDataPkLimit 'ModelosIntegracoes'
exec sp_infoApDataPkLimit 'ComandosSQLs'
exec sp_infoApDataPkLimit 'ComandosSQLsGrupos'
exec sp_infoApDataPkLimit 'EstruturasAD'
exec sp_infoApDataPkLimit 'EstruturasADProps'
exec sp_infoApDataPkLimit 'DefSisIntegracaoAD'
exec sp_infoApDataPkLimit 'Defaults'

exec sp_deleteCascate 'ConDependentes', '= 1037', 1                          /*0 - Deletar, 1 - Mostra os Comandos - Excluir todos os registros relacionados em todas tabelas*/
exec sp_deleteCascateRegistry 'ConDependentes', 'DEP_CdiConDependente = 1037', 0 /*0 - Deletar, 1 - Mostra os Comandos - Excluir todos os registros relacionados em todas tabelas*/
exec sp_deleteCascateRegistry 'FormulariosWFCampos', 'FWC_CdiFormularioWFCampo = 100505', 0 
exec sp_deleteCascateRegistry 'CrachasExtras', 'CEX_CdiCrachaExtra = 9000010', 0
exec sp_deleteCascate 'CrachasExtras', '= 9000010', 0
exec sp_deleteCascate 'EstruturasAD', '= 1002', 1

exec sp_Simple_Generate_Inserts_From_Selects 'ServidoresIntegracoesBDs', 'BBO_CdiServidorIntegracaoBD = 5', 'BBO_CdiServidorIntegracaoBD,BBO_CdiServidorIntegracao,BBO_D1sServidorIntegracaoBD,BBO_CdiModeloIntegracao,BBO_CdiTipoIntegracao,BBO_CdiBaseDado,BBO_CdiTipoConexaoBaseDado,BBO_DssNomeServidor'
exec sp_Simple_Generate_Inserts_From_Selects 'ServidoresIntegracoes'
exec sp_Simple_Generate_Inserts_From_Selects 'TransacoesIntegracoesSobs'
exec sp_Simple_Generate_Inserts_From_Selects 'TransacoesIntegracoes'
exec sp_Simple_Generate_Inserts_From_Selects 'DLLsIntegracoesMetodos'
exec sp_Simple_Generate_Inserts_From_Selects 'DLLsIntegracoes'
exec sp_Simple_Generate_Inserts_From_Selects 'ModelosIntegracoesCmdsRets'
exec sp_Simple_Generate_Inserts_From_Selects 'ComandosItensHTTP'
exec sp_Simple_Generate_Inserts_From_Selects 'ModsIntsMonitsBasesEventos'
exec sp_Simple_Generate_Inserts_From_Selects 'ModelosIntegracoesSobs'
exec sp_Simple_Generate_Inserts_From_Selects 'ModelosIntegracoesQueries'
exec sp_Simple_Generate_Inserts_From_Selects 'ModelosIntegracoesCmdsPars'
exec sp_Simple_Generate_Inserts_From_Selects 'ModelosIntegracoesCmdsCpos', 'BBP_CdiModeloIntegracaoCmd = 10031', 'BBP_CdiModeloIntegracaoCmdCpo,BBP_CdiModeloIntegracaoCmd,BBP_DssCampo_Destino,BBP_DssCampo_Origem,BBP_CdiTipoCampo'
exec sp_Simple_Generate_Inserts_From_Selects 'ModelosIntegracoesCmds', 'BBS_CdiModeloIntegracaoCmd = 10026'
exec sp_Simple_Generate_Inserts_From_Selects 'ModelosIntegracoes', 'BBR_CdiModeloIntegracao = 10021'
exec sp_Simple_Generate_Inserts_From_Selects 'ComandosSQLs', 'SQL_CdiComandoSQL = 20069', 'SQL_CdiComandoSQL,SQL_CdiComandoSQLGrupo,SQL_D1sComandoSQL,SQL_DsbComandoSQL,SQL_D1bComentarios'
exec sp_Simple_Generate_Inserts_From_Selects 'ComandosSQLsGrupos'
exec sp_Simple_Generate_Inserts_From_Selects 'Consultas', 'ACS_CdiConsulta = 10018'
exec sp_Simple_Generate_Inserts_From_Selects 'LayoutsSaidas', 'BRD_CdiLayOutSaida in (1004, 1005)', 'BRD_CdiLayOutSaida,BRD_D1sLayOutSaida,BRD_D1bLayOutSaida'
exec sp_Simple_Generate_Inserts_From_Selects 'UsuariosAutenticacoes', 'JVQ_CdiUsuarioAutenticacao = 1'
exec sp_Simple_Generate_Inserts_From_Selects 'TiposComandosIntegrs'
exec sp_Simple_Generate_Inserts_From_Selects 'ModelosIntegracoesCmdsRets'
exec sp_Simple_Generate_Inserts_From_Selects 'DefSisIntegracaoAD'
exec sp_Simple_Generate_Inserts_From_Selects 'EstruturasAD'
exec sp_Simple_Generate_Inserts_From_Selects 'EstruturasADProps' 
exec sp_Simple_Generate_Inserts_From_Selects 'ComandosSQLs', 'SQL_CdiComandoSQL = 20069', 'SQL_CdiComandoSQL,SQL_CdiComandoSQLGrupo,SQL_D1sComandoSQL,SQL_DsbComandoSQL,SQL_D1bComentarios'

/*CAMPOS GERAIS UTILIZADOS PARA CLONAR*/

exec sp_New_Execute_Sql 'dbo', 01, 'ServidoresIntegracoes', 'BBN_CdiServidorIntegracao, BBN_D1sServidorIntegracao, BBN_CosEnderecoIP, BBN_NuiPorta', '1, ''(TESTES) - INTEGRATION - SERVIDORES'', ''localhost'', 7080', 1  

exec sp_Simple_Generate_Inserts_From_Selects 'ComandosSQLs', 'SQL_CdiComandoSQL = 10006', 'SQL_CdiComandoSQL,SQL_CdiComandoSQLGrupo,SQL_D1sComandoSQL,SQL_DsbComandoSQL'

exec sp_clearAllDataIntegration 0

exec sp_StandardData_FixedValues 0

with dual(dummy) as (select 'x') select :NUMEROINFORMADO  from dual

with dual(dummy) as (select 'x')SELECT '9140692' as keymaster, '110011' as field1, '20221101' as field2  FROM dual  WHERE 1 <= 1

select * from tabelas where atb_cditabela in (select djn_cditabela from ControlesSeqsInternos)

select * from LogsIntegracoes
select * from LogsIntegracoesServidores
select * from LogsIntegracoesCampos

exec sp_deleteOptionByApDataRange 'LogsIntegracoes', 1, 1
exec sp_deleteOptionByApDataRange 'LogsIntegracoesCampos', 1, 1
exec sp_deleteOptionByApDataRange 'LogsIntegracoesServidores', 1, 1

SELECT FLOOR(RAND()*(100-1+1)+1) as keyid;

select top 100 DEP_CdiConDependente, DEP_DtdNascimentoData, DEP_DtdApresCertNascimento, DEP_DtdEmissaoRg, DPF_DtdConteudoData_01
  from ConDependentes 
  left join ConDependentesFlexiveis on (DEP_CdiConDependente = DPF_CdiConDependente)
  order by 1 desc

exec sp_deleteCascateRegistry 'ConDependentes', 'DEP_CdiConDependente = 1037', 0

select 
  BCN_CdiLogIntegracaoServidor,
  BCN_CdiLogIntegracao,
  BCN_CdiServidorIntegracao,
  case when (BCN_DsbErro is null) then 0 else 1 end as TemErro
  from LogsIntegracoesServidores 
  where BCN_CdiLogIntegracaoServidor = (select max(BCN_CdiLogIntegracaoServidor) from LogsIntegracoesServidores)

select SQL_CdiComandoSQL,SQL_CdiComandoSQLGrupo,SQL_D1sComandoSQL,SQL_DsbComandoSQL,SQL_D1bComentarios from ComandosSQLs where SQL_CdiComandoSQL = 20069

select * from ListasGenericasItens