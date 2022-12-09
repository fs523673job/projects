select LTW_NuiPK, ECV_CdiInstanciaApServer ,EON_DssNomeInstancia, EON_DssNomeMaquina, ECV_NusBuildApServer, ECV_DtdInicioInstancia, ECV_DtdFimInstancia, LTW_CdiTransacaoWF, 
LTW_DtdDataHoraTransacaoInicio, TWF_CdiEtapaWorkflow_De, TWF_CdiEtapaWorkflow_Para, LTW_NuiIdentificacaoApServer
from LogsTransacoesWF
    inner Join IdentificacoesApServer on (LTW_NuiIdentificacaoApServer = EON_CdiIdentificacaoApServer)
    inner join TransacoesWF           on (LTW_CdiTransacaoWF = TWF_CdiTransacaoWF)
    inner join InstanciasApServer     on (EON_DssNomeMaquina = ECV_DssNomeMaquina and EON_DssNomeInstancia = ECV_DssNomeInstancia)    
Where LTW_CdiUsuario = 8
and LTW_NuiPK = 160016
and (LTW_DtdDataHoraTransacaoInicio between ECV_DtdInicioInstancia and ECV_DtdFimInstancia or ECV_DtdFimInstancia is null)
