declare @dsscampo as Varchar(50) = ''
declare @dsstabela as Varchar (50) = 'Tabelas'
select OBJ_CdiObjeto, OBJ_D1sObjeto from campos, objetos
where OBJ_CdiObjeto = ACP_CdiObjeto_Primitivo
and (acp_dsscampo = @dsscampo or ACP_CdiCampo = (select top 1 ACI_CdiCampo from CamposIndices where ACI_CdiIndice = (select AIN_CdiIndice from Indices where AIN_CdiTabela = (select ATB_CdiTabela from tabelas where ATB_DssTabela = @dsstabela) and AIN_OplPrimario = 1)))