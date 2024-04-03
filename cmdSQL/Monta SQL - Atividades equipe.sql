-- Monta comando para listar atividades da equipe

-- Filtros --------------------------------------------------------------------
DECLARE @DataChamadoInicial DATETIME
DECLARE @ListaChamados VARCHAR(1000)
DECLARE @NomeCliente VARCHAR(100)
DECLARE @NomeAtendente VARCHAR(100)
DECLARE @TodasEquipes INTEGER
DECLARE @Severidade INTEGER
DECLARE @EquipeOrigem INTEGER
DECLARE @Descricao VARCHAR(100)
DECLARE @SomenteComData INTEGER

DECLARE @EquipeOrigem_Salva INTEGER
DECLARE @Severidade_Salva INTEGER
-------------------------------------------------------------------------------

DECLARE @CRLF VARCHAR(2)

-- Componentes do comando
DECLARE @ATIVIDADE_COMANDO_SQL_ON INTEGER
DECLARE @ATIVIDADE_SEVERIDADE_1_ON INTEGER
DECLARE @ATIVIDADE_DATA_ENTREGA_ON INTEGER
DECLARE @ATIVIDADE_DATA_ANALISE_ON INTEGER
DECLARE @ATIVIDADE_PERSONALIZACAO_ON INTEGER
DECLARE @ATIVIDADE_COMANDO_SQL VARCHAR(30)
DECLARE @ATIVIDADE_SEVERIDADE_1 VARCHAR(30)
DECLARE @ATIVIDADE_DATA_ENTREGA VARCHAR(30)
DECLARE @ATIVIDADE_DATA_ANALISE VARCHAR(30)
DECLARE @ATIVIDADE_PERSONALIZACAO VARCHAR(30)
DECLARE @COMANDO_SQL_ComandosSQL VARCHAR(8000)
DECLARE @COMANDO_SQL_Severidade1 VARCHAR(8000)
DECLARE @COMANDO_SQL_DataEntrega VARCHAR(8000)
DECLARE @COMANDO_SQL_DataAnalise VARCHAR(8000)
DECLARE @COMANDO_SQL_Personalizacao VARCHAR(8000)
DECLARE @WHERE_ChamadosAbertos VARCHAR(1000)
DECLARE @WHERE_Equipe VARCHAR(1000)
DECLARE @WHERE_Equipe_ComandosSQL VARCHAR(1000)
DECLARE @WHERE_Equipe_Analise VARCHAR(1000)
DECLARE @WHERE_ListaChamados VARCHAR(1000)
DECLARE @WHERE_NomeCliente VARCHAR(1000)
DECLARE @WHERE_NomeAtendente VARCHAR(1000)
DECLARE @WHERE_Severidade VARCHAR(1000)
DECLARE @WHERE_EquipeOrigem VARCHAR(1000)
DECLARE @WHERE_Descricao VARCHAR(1000)
DECLARE @WHERE_SomenteComData VARCHAR(1000)
DECLARE @WHERE_DataInicial VARCHAR(1000)
DECLARE @WHERE_FaltaDataAnalise VARCHAR(1000)
DECLARE @WHERE_FaltaDataEntrega VARCHAR(1000)
DECLARE @JOIN_Pessoas VARCHAR(1000)
DECLARE @JOIN_DadosClientes VARCHAR(1000)
DECLARE @JOIN_DadosChamadas VARCHAR(1000)
DECLARE @JOIN_DadosOcorrencias VARCHAR(1000)
DECLARE @JOIN_DadosEquipes VARCHAR(1000)

DECLARE @WHERE_Severidade_Salva VARCHAR(1000)
DECLARE @WHERE_EquipeOrigem_Salva VARCHAR(1000)

SET @CRLF = CHAR(13)

SET @ATIVIDADE_COMANDO_SQL_ON    = 1
SET @ATIVIDADE_SEVERIDADE_1_ON   = 1
SET @ATIVIDADE_DATA_ENTREGA_ON   = 1
SET @ATIVIDADE_DATA_ANALISE_ON   = 1
SET @ATIVIDADE_PERSONALIZACAO_ON = 1

SET @ATIVIDADE_COMANDO_SQL    = '01 - Comando SQL'
SET @ATIVIDADE_SEVERIDADE_1   = '02 - Severidade 1'
SET @ATIVIDADE_DATA_ENTREGA   = '03 - Dar data entrega'
SET @ATIVIDADE_DATA_ANALISE   = '04 - Dar data análise'
SET @ATIVIDADE_PERSONALIZACAO = '05 - Personalização'

SET @COMANDO_SQL_ComandosSQL = ''
SET @COMANDO_SQL_Severidade1 = ''
SET @COMANDO_SQL_DataEntrega = ''
SET @COMANDO_SQL_DataAnalise = ''
SET @COMANDO_SQL_Personalizacao = ''


SET @DataChamadoInicial = '01-01-2022 01:00:00.000'
SET @ListaChamados = NULL

SET @NomeCliente = NULL
SET @NomeAtendente = NULL
SET @Severidade = NULL
SET @TodasEquipes = NULL
SET @EquipeOrigem = NULL
SET @Descricao = NULL
SET @SomenteComData = NULL

-- Monta componentes do comando - Deve ser feito logo antes de montar o comando
SET @WHERE_ChamadosAbertos = '  AND CHA_CdiStatusChamada <> 31' + @CRLF
SET @WHERE_Equipe = CASE WHEN COALESCE(@TodasEquipes, 0) = 0 THEN '  AND CHA_CdiEquipeAtendimento_Atual IN (87, 103, 130) ' + @CRLF ELSE '' END
SET @WHERE_Equipe_ComandosSQL = '  AND CHA_CdiEquipeAtendimento_Atual IN (152) ' + @CRLF
SET @WHERE_Equipe_Analise = '  AND CHA_CdiEquipeAtendimento_Atual IN (87) ' + @CRLF
--SET @WHERE_ListaChamados = CASE WHEN COALESCE(@ListaChamados, '') <> '' THEN '  AND (CHA_CdiChamada IN (' + @ListaChamados + '))' + @CRLF ELSE '' END
SET @WHERE_ListaChamados = '  AND (COALESCE(@ListaChamados, '''') = '''' OR CHA_CdiChamada IN (@ListaChamados))' + @CRLF
--SET @WHERE_NomeCliente = CASE WHEN COALESCE(@NomeCliente, '') <> '' THEN     '  AND (CHA_D1sReferencia like ''%' + @NomeCliente + '%'' OR ' + @CRLF +
--                                                                         '       PBB_D1sReferencia like ''%' + @NomeCliente + '%'' OR ' + @CRLF +
--                                                                         '       GCF_D1sJuridicaGrupo like ''%' + @NomeCliente + '%'' OR ' + @CRLF +
--                                                                         '       PBG_D1sReferencia like ''%' + @NomeCliente + '%'' OR ' + @CRLF +
--                                                                         '       PBB_D1sReferencia like ''%' + @NomeCliente + '%'')' + @CRLF 
--                                                                    ELSE '' 
--                         END
SET @WHERE_NomeCliente = '  AND (COALESCE(@NomeCliente, '''') = '''' OR ' + @CRLF +
                         '       (CHA_D1sReferencia like ''%'' + @NomeCliente + ''%'' OR ' + @CRLF +
                         '        PBB_D1sReferencia like ''%'' + @NomeCliente + ''%'' OR ' + @CRLF +
                         '        GCF_D1sJuridicaGrupo like ''%'' + @NomeCliente + ''%'' OR ' + @CRLF +
                         '        PBG_D1sReferencia like ''%'' + @NomeCliente + ''%'' OR ' + @CRLF +
                         '        PBB_D1sReferencia like ''%'' + @NomeCliente + ''%''))' + @CRLF 
SET @WHERE_NomeAtendente = CASE WHEN COALESCE(@NomeAtendente, '') <> '' THEN '  AND COALESCE(CON_DssNome, '''') LIKE ''%' + @NomeAtendente + '%'' ' + @CRLF ELSE '' END
--SET @WHERE_Severidade = CASE WHEN COALESCE(@Severidade, 0) > 0 THEN '  AND (ESY_D1sSeveridadeSLA IS NULL OR ESY_D1sSeveridadeSLA LIKE ''Severidade ' + LTRIM(STR(@Severidade)) + ''') ' + @CRLF ELSE '' END
SET @WHERE_Severidade = CASE WHEN COALESCE(@Severidade, 0) > 0 THEN '  AND (CHA_CdiSeveridadeSLA = ' + LTRIM(STR(@Severidade+1)) + ') ' + @CRLF ELSE '' END
SET @WHERE_EquipeOrigem = CASE WHEN COALESCE(@EquipeOrigem, 0) > 0 THEN '  AND (' + LTRIM(STR(@EquipeOrigem)) + ' = ' + @CRLF +
                                                                        '       COALESCE( ' + @CRLF +
                                                                        '                (SELECT TOP 1 AHX_CdiEquipeAtendimento ' + @CRLF +
                                                                        '                 FROM ChamadasOcorrencias ' + @CRLF +
                                                                        '                 WHERE AHX_CdiChamada = CHA_CdiChamada ' + @CRLF +
                                                                        '                   AND AHX_CdiEquipeAtendimento <> 0 ' + @CRLF +
                                                                        '                   AND AHX_CdiEquipeAtendimento <> AHX_CdiEquipeAtendimento_Atual ' + @CRLF +
                                                                        '                   AND AHX_CdiEquipeAtendimento_Atual IN (87) ' + @CRLF +
                                                                        '                 ORDER BY AHX_CdiChamadaOcorrencia DESC) ' + @CRLF +
                                                                        '                , 0))' + @CRLF
                                                                   ELSE ''
                          END
SET @WHERE_Descricao = CASE WHEN COALESCE(@Descricao, '') <> '' THEN '  AND (CHA_D1sReferencia LIKE ''' + @Descricao + ''')' + @CRLF
                            ELSE ''
                       END
SET @WHERE_SomenteComData = CASE WHEN COALESCE(@SomenteComData, 0) = 1 THEN ' AND (CHA_DtdPrevistaAnalise IS NOT NULL OR' + @CRLF +
                                                                            '      CHA_DtdPrevistaProgramacao IS NOT NULL OR' + @CRLF +
                                                                            '      CHA_DtdPrevistaTeste IS NOT NULL OR' + @CRLF +
                                                                            '      PDI_DtdEntrega_Real IS NOT NULL OR' + @CRLF +
                                                                            '      PDI_DtdPrevisaoTecnica IS NOT NULL)' + @CRLF
                                                                       ELSE ''
                            END
--SET @WHERE_DataInicial = CASE WHEN @DataChamadoInicial IS NOT NULL THEN '  AND (CHA_DtdChamada >= ''' + FORMAT(@DataChamadoInicial, 'yyyy-MM-dd hh:mm:ss') + ''') ' + @CRLF
--                                                                   ELSE ''
--                        END
SET @WHERE_DataInicial = '  AND (@DataChamadoInicial IS NULL OR CHA_DtdChamada >= FORMAT(@DataChamadoInicial, ''yyyy-MM-dd hh:mm:ss'')) ' + @CRLF
SET @WHERE_FaltaDataAnalise ='  AND (CHA_DtdPrevistaAnalise IS NULL AND ' + @CRLF +
                             '       (CHA_CdiEquipeAtendimento_Atual = 87 AND NOT COALESCE(SCH_D1sStatusChamada, '''') LIKE ''%Teste%'')) ' + @CRLF

SET @WHERE_FaltaDataEntrega ='  AND (CHA_DtdPrevistaAnalise IS NOT NULL AND ' + @CRLF +
                             '       (((CHA_CdiEquipeAtendimento_Atual = 103) OR ' + @CRLF +
                             '         (CHA_CdiEquipeAtendimento_Atual = 87 AND COALESCE(SCH_D1sStatusChamada, '''') LIKE ''%Teste%'')) AND ' + @CRLF +
                             '        (CHA_DtdPrevistaProgramacao IS NULL OR ' + @CRLF +
                             '         CHA_DtdPrevistaTeste IS NULL OR ' + @CRLF +
                             '         CHA_DtdPrevisaoTecnica IS NULL OR ' + @CRLF +
                             '         PDI_DtdEntrega_Real IS NULL))) ' + @CRLF

SET @JOIN_Pessoas = 'INNER JOIN ' + @CRLF + 
                    '    ( ' + @CRLF + 
                    '    SELECT ' + @CRLF + 
                    '        HFE_CdiContratoItemxPessoa AS CON_CdiContratado, ' + @CRLF + 
                    '        HFE_CosEMailContratoItem AS CON_CosEmail, ' + @CRLF + 
                    '        PDD_DssNome AS CON_DssNome ' + @CRLF + 
                    '    FROM ' + @CRLF + 
                    '        ContratosItensxPessoas ' + @CRLF + 
                    '        INNER JOIN PessoasContratos ON (HFE_CdiPessoaContrato = PDD_CdiPessoaContrato) ' + @CRLF + 
                    '    ) a ON (CHA_CdiContratoItemxPessoa_AtA = a.CON_CdiContratado) ' + @CRLF
SET @JOIN_DadosClientes = 'LEFT JOIN ContratosItensxPessoas ON HFE_CdiContratoItemxPessoa = CHA_CdiContratoItemxPessoa ' + @CRLF + 
                          'LEFT JOIN ContratosItens ON PBB_CdiContratoItem = HFE_CdiContratoItem ' + @CRLF + 
                          'LEFT JOIN Contratos ON PBB_CdiContrato = PBG_CdiContrato ' + @CRLF + 
                          'LEFT JOIN (SELECT AHX_Cdichamada, PDI_DtdEntrega_Real, PDI_DtdPrevisaoTecnica FROM ChamadasOcorrencias ' + @CRLF + 
                          '           INNER JOIN Pendencias ON PDI_CdiChamadaOcorrencia_Abert = AHX_CdiChamadaOcorrencia) Pendencias ON AHX_Cdichamada = CHA_CdiChamada ' + @CRLF + 
                          'LEFT JOIN JuridicasGrupos on PBG_CdiJuridicaGrupo = GCF_CdiJuridicaGrupo ' + @CRLF
SET @JOIN_DadosChamadas = 'LEFT JOIN TiposChamadas ON TCH_CdiTipoChamada = CHA_CdiTipoChamada ' + @CRLF +
                          'LEFT JOIN StatusChamadas ON CHA_CdiStatusChamada = SCH_CdiStatusChamada ' + @CRLF + 
                          'LEFT JOIN SeveridadesSLA ON ESY_CdiSeveridadeSLA = CHA_CdiSeveridadeSLA ' + @CRLF
SET @JOIN_DadosOcorrencias = 'INNER JOIN ( ' + @CRLF + 
                             '    SELECT ' + @CRLF + 
                             '    AHX_CdiChamada AS CdiChamada, ' + @CRLF + 
                             '    MAX(AHX_CdiChamadaOcorrencia) as MaxChamadaOcorrencia ' + @CRLF + 
                             '    FROM ' + @CRLF + 
                             '    ChamadasOcorrencias ' + @CRLF + 
                             '    GROUP BY ' + @CRLF + 
                             '    AHX_CdiChamada ' + @CRLF + 
                             ') MaxOcorrencias ON (MaxOcorrencias.CdiChamada = CHA_CdiChamada) ' + @CRLF
SET @JOIN_DadosEquipes = 'LEFT JOIN EquipesAtendimentos ON (CHA_CdiEquipeAtendimento_Atual = AJG_CdiEquipeAtendimento) ' + @CRLF

-- Chamados para aprovacao de comandos SQL (equipe 152) -------------------------------------------
IF @ATIVIDADE_COMANDO_SQL_ON = 1
BEGIN
    SET @COMANDO_SQL_ComandosSQL = 
        'SELECT ''' + @ATIVIDADE_COMANDO_SQL + ''' AS Atividade, ' + @CRLF +
        '       CHA_CdiChamada AS Chamada, ' + @CRLF + 
        '       COALESCE(TCH_D1sTipoChamada, '''') AS Tipo, ' + @CRLF +
        '       ESY_D1sSeveridadeSLA AS Severidade, ' + @CRLF + 
        '       CHA_CdiEquipeAtendimento_Atual AS "Id Equipe", ' + @CRLF + 
        '       CASE' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 87 THEN  ''Infra/Análise''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 103 THEN ''Infra/Progr.''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 130 THEN ''Infra/Back''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 85 THEN  ''Folha/Análise''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 101 THEN ''Folha/Progr.''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 128 THEN ''Folha/Back''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 86 THEN  ''RH/Análise''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 102 THEN ''RH/Progr.''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 129 THEN ''RH/Back''' + @CRLF + 
        '          ELSE AJG_D1sEquipeAtendimento' + @CRLF + 
        '       END AS Equipe,' + @CRLF + 
        '       COALESCE(PBG_D1sReferencia, '''') AS Grupo, ' + @CRLF +
        '       COALESCE(PBB_D1sReferencia, '''') AS Cliente, ' + @CRLF +
        '       CHA_D1sReferencia AS Assunto, ' + @CRLF +
        '       REPLACE(REPLACE(REPLACE(COALESCE(SCH_D1sStatusChamada, ''''), ''Apdata - '', ''''), ''Pendência em '', ''''), ''Pendência '', '''') AS Status, ' + @CRLF +
        '       COALESCE(CON_DssNome, '''') AS Atendente, ' + @CRLF +
        '       CASE WHEN PDI_DtdEntrega_Real IS NOT NULL THEN FORMAT(PDI_DtdEntrega_Real, ''dd/MM/yyyy'') ELSE '''' END AS ''Entrega Real'', ' + @CRLF +
        '       ''''/*CASE WHEN (CHA_DtdPrevisaoTecnica IS NOT NULL OR PDI_DtdPrevisaoTecnica IS NOT NULL) THEN ' +
                            'FORMAT(CASE WHEN COALESCE(CHA_DtdPrevisaoTecnica, DATEFROMPARTS(2050, 12, 31)) < COALESCE(PDI_DtdPrevisaoTecnica, DATEFROMPARTS(2050, 12, 31)) ' +
                                        'THEN CHA_DtdPrevisaoTecnica ' +
                                        'ELSE PDI_DtdPrevisaoTecnica ' +
                                    'END, ''dd/MM/yyyy'') ' +
                            'ELSE '''' END*/ AS ''Entrega Técnica'', ' + @CRLF +
        '       '''' /*CASE WHEN CHA_DtdPrevistaAnalise IS NOT NULL THEN FORMAT(CHA_DtdPrevistaAnalise, ''dd/MM/yyyy'') ELSE '''' END*/ AS ''Entrega Análise'', ' + @CRLF + 
        '       '''' /*CASE WHEN CHA_DtdPrevistaProgramacao IS NOT NULL THEN FORMAT(CHA_DtdPrevistaProgramacao, ''dd/MM/yyyy'') ELSE '''' END*/ AS ''Entrega Programação'', ' + @CRLF + 
        '       '''' /*CASE WHEN CHA_DtdPrevistaTeste IS NOT NULL THEN FORMAT(CHA_DtdPrevistaTeste, ''dd/MM/yyyy'') ELSE '''' END*/ AS ''Entrega Teste'' ' + @CRLF + 
        'FROM Chamadas ' + @CRLF + 
        @JOIN_DadosClientes +
        @JOIN_DadosChamadas +
        @JOIN_Pessoas + 
        @JOIN_DadosOcorrencias +
        @JOIN_DadosEquipes +
        'WHERE (1=1) ' + @CRLF +
        @WHERE_ChamadosAbertos +
        @WHERE_Equipe_ComandosSQL +
    --    @WHERE_ListaChamados +
        @WHERE_NomeCliente + 
    --    @WHERE_NomeAtendente + 
        @WHERE_Severidade + 
    --    @WHERE_EquipeOrigem + 
        @WHERE_Descricao +
    --    @WHERE_SomenteComData +
        @WHERE_DataInicial
END

-- Chamados com severidade 1 ----------------------------------------------------------------------
IF @ATIVIDADE_SEVERIDADE_1_ON = 1
BEGIN
    SET @Severidade_Salva = @Severidade
    SET @WHERE_Severidade_Salva = @WHERE_Severidade
    SET @Severidade = 1
    SET @WHERE_Severidade = CASE WHEN COALESCE(@Severidade, 0) > 0 THEN '  AND (CHA_CdiSeveridadeSLA = ' + LTRIM(STR(@Severidade+1)) + ') ' + @CRLF ELSE '' END
    SET @COMANDO_SQL_Severidade1 = 
        'SELECT ''' + @ATIVIDADE_SEVERIDADE_1 + ''' AS Atividade, ' + @CRLF +
        '       CHA_CdiChamada AS Chamada, ' + @CRLF + 
        '       COALESCE(TCH_D1sTipoChamada, '''') AS Tipo, ' + @CRLF +
        '       ESY_D1sSeveridadeSLA AS Severidade, ' + @CRLF + 
        '       CHA_CdiEquipeAtendimento_Atual AS "Id Equipe", ' + @CRLF + 
        '       CASE' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 87 THEN  ''Infra/Análise''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 103 THEN ''Infra/Progr.''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 130 THEN ''Infra/Back''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 85 THEN  ''Folha/Análise''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 101 THEN ''Folha/Progr.''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 128 THEN ''Folha/Back''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 86 THEN  ''RH/Análise''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 102 THEN ''RH/Progr.''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 129 THEN ''RH/Back''' + @CRLF + 
        '          ELSE AJG_D1sEquipeAtendimento' + @CRLF + 
        '       END AS Equipe,' + @CRLF + 
        '       COALESCE(PBG_D1sReferencia, '''') AS Grupo, ' + @CRLF +
        '       COALESCE(PBB_D1sReferencia, '''') AS Cliente, ' + @CRLF +
        '       CHA_D1sReferencia AS Assunto, ' + @CRLF +
        '       REPLACE(REPLACE(REPLACE(COALESCE(SCH_D1sStatusChamada, ''''), ''Apdata - '', ''''), ''Pendência em '', ''''), ''Pendência '', '''') AS Status, ' + @CRLF +
        '       COALESCE(CON_DssNome, '''') AS Atendente, ' + @CRLF +
        '       CASE WHEN PDI_DtdEntrega_Real IS NOT NULL THEN FORMAT(PDI_DtdEntrega_Real, ''dd/MM/yyyy'') ELSE '''' END AS ''Entrega Real'', ' + @CRLF +
        '       CASE WHEN (CHA_DtdPrevisaoTecnica IS NOT NULL OR PDI_DtdPrevisaoTecnica IS NOT NULL) THEN ' +
                            'FORMAT(CASE WHEN COALESCE(CHA_DtdPrevisaoTecnica, DATEFROMPARTS(2050, 12, 31)) < COALESCE(PDI_DtdPrevisaoTecnica, DATEFROMPARTS(2050, 12, 31)) ' +
                                        'THEN CHA_DtdPrevisaoTecnica ' +
                                        'ELSE PDI_DtdPrevisaoTecnica ' +
                                    'END, ''dd/MM/yyyy'') ' +
                            'ELSE '''' END AS ''Entrega Técnica'', ' + @CRLF +
        '       CASE WHEN CHA_DtdPrevistaAnalise IS NOT NULL THEN FORMAT(CHA_DtdPrevistaAnalise, ''dd/MM/yyyy'') ELSE '''' END AS ''Entrega Análise'', ' + @CRLF + 
        '       CASE WHEN CHA_DtdPrevistaProgramacao IS NOT NULL THEN FORMAT(CHA_DtdPrevistaProgramacao, ''dd/MM/yyyy'') ELSE '''' END AS ''Entrega Programação'', ' + @CRLF + 
        '       CASE WHEN CHA_DtdPrevistaTeste IS NOT NULL THEN FORMAT(CHA_DtdPrevistaTeste, ''dd/MM/yyyy'') ELSE '''' END AS ''Entrega Teste'' ' + @CRLF + 
        'FROM Chamadas ' + @CRLF + 
        @JOIN_DadosClientes +
        @JOIN_DadosChamadas +
        @JOIN_Pessoas + 
        @JOIN_DadosOcorrencias +
        @JOIN_DadosEquipes +
        'WHERE (1=1) ' + @CRLF +
        @WHERE_ChamadosAbertos +
        @WHERE_Equipe +
        @WHERE_ListaChamados +
        @WHERE_NomeCliente + 
    --    @WHERE_NomeAtendente + 
        @WHERE_Severidade + 
    --    @WHERE_EquipeOrigem + 
        @WHERE_Descricao +
    --    @WHERE_SomenteComData +
        @WHERE_DataInicial + 
        @WHERE_FaltaDataEntrega
    SET @Severidade = @Severidade_Salva
    SET @WHERE_Severidade = @WHERE_Severidade_Salva
END

-- Chamados que já passaram da análise e para os quais falta datas de entrega ---------------------
IF @ATIVIDADE_DATA_ENTREGA_ON = 1
BEGIN
    SET @COMANDO_SQL_DataEntrega = 
        'SELECT ''' + @ATIVIDADE_DATA_ENTREGA + ''' AS Atividade, ' + @CRLF +
        '       CHA_CdiChamada AS Chamada, ' + @CRLF + 
        '       COALESCE(TCH_D1sTipoChamada, '''') AS Tipo, ' + @CRLF +
        '       ESY_D1sSeveridadeSLA AS Severidade, ' + @CRLF + 
        '       CHA_CdiEquipeAtendimento_Atual AS "Id Equipe", ' + @CRLF + 
        '       CASE' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 87 THEN  ''Infra/Análise''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 103 THEN ''Infra/Progr.''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 130 THEN ''Infra/Back''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 85 THEN  ''Folha/Análise''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 101 THEN ''Folha/Progr.''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 128 THEN ''Folha/Back''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 86 THEN  ''RH/Análise''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 102 THEN ''RH/Progr.''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 129 THEN ''RH/Back''' + @CRLF + 
        '          ELSE AJG_D1sEquipeAtendimento' + @CRLF + 
        '       END AS Equipe,' + @CRLF + 
        '       COALESCE(PBG_D1sReferencia, '''') AS Grupo, ' + @CRLF +
        '       COALESCE(PBB_D1sReferencia, '''') AS Cliente, ' + @CRLF +
        '       CHA_D1sReferencia AS Assunto, ' + @CRLF +
        '       REPLACE(REPLACE(REPLACE(COALESCE(SCH_D1sStatusChamada, ''''), ''Apdata - '', ''''), ''Pendência em '', ''''), ''Pendência '', '''') AS Status, ' + @CRLF +
        '       COALESCE(CON_DssNome, '''') AS Atendente, ' + @CRLF +
        '       CASE WHEN PDI_DtdEntrega_Real IS NOT NULL THEN FORMAT(PDI_DtdEntrega_Real, ''dd/MM/yyyy'') ELSE '''' END AS ''Entrega Real'', ' + @CRLF +
        '       CASE WHEN (CHA_DtdPrevisaoTecnica IS NOT NULL OR PDI_DtdPrevisaoTecnica IS NOT NULL) THEN ' +
                            'FORMAT(CASE WHEN COALESCE(CHA_DtdPrevisaoTecnica, DATEFROMPARTS(2050, 12, 31)) < COALESCE(PDI_DtdPrevisaoTecnica, DATEFROMPARTS(2050, 12, 31)) ' +
                                        'THEN CHA_DtdPrevisaoTecnica ' +
                                        'ELSE PDI_DtdPrevisaoTecnica ' +
                                    'END, ''dd/MM/yyyy'') ' +
                            'ELSE '''' END AS ''Entrega Técnica'', ' + @CRLF +
        '       CASE WHEN CHA_DtdPrevistaAnalise IS NOT NULL THEN FORMAT(CHA_DtdPrevistaAnalise, ''dd/MM/yyyy'') ELSE '''' END AS ''Entrega Análise'', ' + @CRLF + 
        '       CASE WHEN CHA_DtdPrevistaProgramacao IS NOT NULL THEN FORMAT(CHA_DtdPrevistaProgramacao, ''dd/MM/yyyy'') ELSE '''' END AS ''Entrega Programação'', ' + @CRLF + 
        '       CASE WHEN CHA_DtdPrevistaTeste IS NOT NULL THEN FORMAT(CHA_DtdPrevistaTeste, ''dd/MM/yyyy'') ELSE '''' END AS ''Entrega Teste'' ' + @CRLF + 
        'FROM Chamadas ' + @CRLF + 
        @JOIN_DadosClientes +
        @JOIN_DadosChamadas +
        @JOIN_Pessoas + 
        @JOIN_DadosOcorrencias +
        @JOIN_DadosEquipes +
        'WHERE (1=1) ' + @CRLF +
        @WHERE_ChamadosAbertos +
        @WHERE_Equipe +
        @WHERE_ListaChamados +
        @WHERE_NomeCliente + 
    --    @WHERE_NomeAtendente + 
        @WHERE_Severidade + 
    --    @WHERE_EquipeOrigem + 
        @WHERE_Descricao +
    --    @WHERE_SomenteComData +
        @WHERE_DataInicial + 
        @WHERE_FaltaDataEntrega
END

-- Chamados que entraram e para os quais ainda não foi dada data de análise -----------------------
IF @ATIVIDADE_DATA_ANALISE_ON = 1
BEGIN
    SET @COMANDO_SQL_DataAnalise = 
        'SELECT ''' + @ATIVIDADE_DATA_ANALISE + ''' AS Atividade, ' + @CRLF +
        '       CHA_CdiChamada AS Chamada, ' + @CRLF + 
        '       COALESCE(TCH_D1sTipoChamada, '''') AS Tipo, ' + @CRLF +
        '       ESY_D1sSeveridadeSLA AS Severidade, ' + @CRLF + 
        '       CHA_CdiEquipeAtendimento_Atual AS "Id Equipe", ' + @CRLF + 
        '       CASE' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 87 THEN  ''Infra/Análise''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 103 THEN ''Infra/Progr.''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 130 THEN ''Infra/Back''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 85 THEN  ''Folha/Análise''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 101 THEN ''Folha/Progr.''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 128 THEN ''Folha/Back''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 86 THEN  ''RH/Análise''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 102 THEN ''RH/Progr.''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 129 THEN ''RH/Back''' + @CRLF + 
        '          ELSE AJG_D1sEquipeAtendimento' + @CRLF + 
        '       END AS Equipe,' + @CRLF + 
        '       COALESCE(PBG_D1sReferencia, '''') AS Grupo, ' + @CRLF +
        '       COALESCE(PBB_D1sReferencia, '''') AS Cliente, ' + @CRLF +
        '       CHA_D1sReferencia AS Assunto, ' + @CRLF +
        '       REPLACE(REPLACE(REPLACE(COALESCE(SCH_D1sStatusChamada, ''''), ''Apdata - '', ''''), ''Pendência em '', ''''), ''Pendência '', '''') AS Status, ' + @CRLF +
        '       COALESCE(CON_DssNome, '''') AS Atendente, ' + @CRLF +
        '       CASE WHEN PDI_DtdEntrega_Real IS NOT NULL THEN FORMAT(PDI_DtdEntrega_Real, ''dd/MM/yyyy'') ELSE '''' END AS ''Entrega Real'', ' + @CRLF +
        '       CASE WHEN (CHA_DtdPrevisaoTecnica IS NOT NULL OR PDI_DtdPrevisaoTecnica IS NOT NULL) THEN ' +
                            'FORMAT(CASE WHEN COALESCE(CHA_DtdPrevisaoTecnica, DATEFROMPARTS(2050, 12, 31)) < COALESCE(PDI_DtdPrevisaoTecnica, DATEFROMPARTS(2050, 12, 31)) ' +
                                        'THEN CHA_DtdPrevisaoTecnica ' +
                                        'ELSE PDI_DtdPrevisaoTecnica ' +
                                    'END, ''dd/MM/yyyy'') ' +
                            'ELSE '''' END AS ''Entrega Técnica'', ' + @CRLF +
        '       CASE WHEN CHA_DtdPrevistaAnalise IS NOT NULL THEN FORMAT(CHA_DtdPrevistaAnalise, ''dd/MM/yyyy'') ELSE '''' END AS ''Entrega Análise'', ' + @CRLF + 
        '       CASE WHEN CHA_DtdPrevistaProgramacao IS NOT NULL THEN FORMAT(CHA_DtdPrevistaProgramacao, ''dd/MM/yyyy'') ELSE '''' END AS ''Entrega Programação'', ' + @CRLF + 
        '       CASE WHEN CHA_DtdPrevistaTeste IS NOT NULL THEN FORMAT(CHA_DtdPrevistaTeste, ''dd/MM/yyyy'') ELSE '''' END AS ''Entrega Teste'' ' + @CRLF + 
        'FROM Chamadas ' + @CRLF + 
        @JOIN_DadosClientes +
        @JOIN_DadosChamadas +
        @JOIN_Pessoas + 
        @JOIN_DadosOcorrencias +
        @JOIN_DadosEquipes +
        'WHERE (1=1) ' + @CRLF +
        @WHERE_ChamadosAbertos +
        @WHERE_Equipe +
        @WHERE_ListaChamados +
        @WHERE_NomeCliente + 
    --    @WHERE_NomeAtendente + 
        @WHERE_Severidade + 
    --    @WHERE_EquipeOrigem + 
        @WHERE_Descricao +
    --    @WHERE_SomenteComData +
        @WHERE_DataInicial + 
        @WHERE_FaltaDataAnalise
END

-- Chamados vindos da equipe de Design (teste personalização) -------------------------------------

IF @ATIVIDADE_PERSONALIZACAO_ON = 1 AND @EquipeOrigem IS NULL
BEGIN
    SET @EquipeOrigem_Salva = @EquipeOrigem
    SET @WHERE_EquipeOrigem_Salva = @WHERE_EquipeOrigem
    SET @EquipeOrigem = 104
    SET @WHERE_EquipeOrigem = CASE WHEN COALESCE(@EquipeOrigem, 0) > 0 THEN '  AND (' + LTRIM(STR(@EquipeOrigem)) + ' = ' + @CRLF +
                                                                            '       COALESCE( ' + @CRLF +
                                                                            '                (SELECT TOP 1 AHX_CdiEquipeAtendimento ' + @CRLF +
                                                                            '                 FROM ChamadasOcorrencias ' + @CRLF +
                                                                            '                 WHERE AHX_CdiChamada = CHA_CdiChamada ' + @CRLF +
                                                                            '                   AND AHX_CdiEquipeAtendimento <> 0 ' + @CRLF +
                                                                            '                   AND AHX_CdiEquipeAtendimento <> AHX_CdiEquipeAtendimento_Atual ' + @CRLF +
                                                                            '                   AND AHX_CdiEquipeAtendimento_Atual IN (87) ' + @CRLF +
                                                                            '                 ORDER BY AHX_CdiChamadaOcorrencia DESC) ' + @CRLF +
                                                                            '                , 0))' + @CRLF
                                                                       ELSE ''
                              END
    SET @COMANDO_SQL_Personalizacao = 
        'SELECT ''' + @ATIVIDADE_PERSONALIZACAO + ''' AS Atividade, ' + @CRLF +
        '       CHA_CdiChamada AS Chamada, ' + @CRLF + 
        '       COALESCE(TCH_D1sTipoChamada, '''') AS Tipo, ' + @CRLF +
        '       ESY_D1sSeveridadeSLA AS Severidade, ' + @CRLF + 
        '       CHA_CdiEquipeAtendimento_Atual AS "Id Equipe", ' + @CRLF + 
        '       CASE' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 87 THEN  ''Infra/Análise''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 103 THEN ''Infra/Progr.''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 130 THEN ''Infra/Back''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 85 THEN  ''Folha/Análise''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 101 THEN ''Folha/Progr.''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 128 THEN ''Folha/Back''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 86 THEN  ''RH/Análise''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 102 THEN ''RH/Progr.''' + @CRLF + 
        '          WHEN CHA_CdiEquipeAtendimento_Atual = 129 THEN ''RH/Back''' + @CRLF + 
        '          ELSE AJG_D1sEquipeAtendimento' + @CRLF + 
        '       END AS Equipe,' + @CRLF + 
        '       COALESCE(PBG_D1sReferencia, '''') AS Grupo, ' + @CRLF +
        '       COALESCE(PBB_D1sReferencia, '''') AS Cliente, ' + @CRLF +
        '       CHA_D1sReferencia AS Assunto, ' + @CRLF +
        '       REPLACE(REPLACE(REPLACE(COALESCE(SCH_D1sStatusChamada, ''''), ''Apdata - '', ''''), ''Pendência em '', ''''), ''Pendência '', '''') AS Status, ' + @CRLF +
        '       COALESCE(CON_DssNome, '''') AS Atendente, ' + @CRLF +
        '       CASE WHEN PDI_DtdEntrega_Real IS NOT NULL THEN FORMAT(PDI_DtdEntrega_Real, ''dd/MM/yyyy'') ELSE '''' END AS ''Entrega Real'', ' + @CRLF +
        '       CASE WHEN (CHA_DtdPrevisaoTecnica IS NOT NULL OR PDI_DtdPrevisaoTecnica IS NOT NULL) THEN ' +
                          'FORMAT(CASE WHEN COALESCE(CHA_DtdPrevisaoTecnica, DATEFROMPARTS(2050, 12, 31)) < COALESCE(PDI_DtdPrevisaoTecnica, DATEFROMPARTS(2050, 12, 31)) ' +
                                      'THEN CHA_DtdPrevisaoTecnica ' +
                                      'ELSE PDI_DtdPrevisaoTecnica ' +
                                 'END, ''dd/MM/yyyy'') ' +
                          'ELSE '''' END AS ''Entrega Técnica'', ' + @CRLF +
        '       CASE WHEN CHA_DtdPrevistaAnalise IS NOT NULL THEN FORMAT(CHA_DtdPrevistaAnalise, ''dd/MM/yyyy'') ELSE '''' END AS ''Entrega Análise'', ' + @CRLF + 
        '       CASE WHEN CHA_DtdPrevistaProgramacao IS NOT NULL THEN FORMAT(CHA_DtdPrevistaProgramacao, ''dd/MM/yyyy'') ELSE '''' END AS ''Entrega Programação'', ' + @CRLF + 
        '       CASE WHEN CHA_DtdPrevistaTeste IS NOT NULL THEN FORMAT(CHA_DtdPrevistaTeste, ''dd/MM/yyyy'') ELSE '''' END AS ''Entrega Teste'' ' + @CRLF + 
        'FROM Chamadas ' + @CRLF + 
        @JOIN_DadosClientes +
        @JOIN_DadosChamadas +
        @JOIN_Pessoas + 
        @JOIN_DadosOcorrencias +
        @JOIN_DadosEquipes +
        'WHERE (1=1) ' + @CRLF +
        @WHERE_ChamadosAbertos +
        @WHERE_Equipe_Analise +
--        @WHERE_Equipe +
--        @WHERE_ListaChamados +
        @WHERE_NomeCliente + 
        @WHERE_NomeAtendente + 
        @WHERE_Severidade + 
        @WHERE_EquipeOrigem + 
        @WHERE_Descricao +
        @WHERE_SomenteComData +
        @WHERE_DataInicial +
        '  AND (COALESCE(SCH_D1sStatusChamada, '''') LIKE ''%teste%'')'
    SET @EquipeOrigem = @EquipeOrigem_Salva
    SET @WHERE_EquipeOrigem = @WHERE_EquipeOrigem_Salva
END

PRINT '--============================================================================='
PRINT '-- ATIVIDADES DA EQUIPE INFRA'
PRINT ''
PRINT '-- Filtros --------------------------------------------------------------------'
PRINT 'DECLARE @DataChamadoInicial DATETIME'
PRINT 'DECLARE @ListaChamados VARCHAR(1000)'
PRINT 'DECLARE @NomeCliente VARCHAR(100)'
PRINT 'DECLARE @NomeAtendente VARCHAR(100)'
PRINT 'DECLARE @TodasEquipes INTEGER'
PRINT 'DECLARE @Severidade INTEGER'
PRINT 'DECLARE @EquipeOrigem INTEGER'
PRINT 'DECLARE @Descricao VARCHAR(100)'
PRINT 'DECLARE @SomenteComData INTEGER'
PRINT ''
PRINT 'SET @DataChamadoInicial = ''' + FORMAT(@DataChamadoInicial, 'dd-MM-yyyy hh:mm:ss.000') + ''''
PRINT 'SET @ListaChamados = ' + CASE WHEN @ListaChamados IS NOT NULL THEN '''' + @ListaChamados + '''' ELSE 'NULL' END
PRINT 'SET @NomeCliente = ' + CASE WHEN @NomeCliente IS NOT NULL THEN '''' + @NomeCliente + '''' ELSE 'NULL' END
PRINT 'SET @NomeAtendente = ' + CASE WHEN @NomeAtendente IS NOT NULL THEN '''' + @NomeAtendente + '''' ELSE 'NULL' END
PRINT 'SET @Severidade = ' + CASE WHEN @Severidade IS NOT NULL THEN LTRIM(STR(@Severidade)) ELSE 'NULL' END
PRINT 'SET @TodasEquipes = ' + CASE WHEN @TodasEquipes IS NOT NULL THEN LTRIM(STR(@TodasEquipes)) ELSE 'NULL' END
PRINT 'SET @EquipeOrigem = ' + CASE WHEN @EquipeOrigem IS NOT NULL THEN LTRIM(STR(@EquipeOrigem)) ELSE 'NULL' END
PRINT 'SET @Descricao = ' + CASE WHEN @Descricao IS NOT NULL THEN '''' + @Descricao + '''' ELSE 'NULL' END
PRINT 'SET @SomenteComData = ' + CASE WHEN @SomenteComData IS NOT NULL THEN LTRIM(STR(@SomenteComData)) ELSE 'NULL' END
PRINT ''
PRINT '-------------------------------------------------------------------------------'
PRINT ''


PRINT @COMANDO_SQL_ComandosSQL
IF (@COMANDO_SQL_ComandosSQL <> '') AND (@COMANDO_SQL_Severidade1 + @COMANDO_SQL_DataEntrega + @COMANDO_SQL_DataAnalise + @COMANDO_SQL_Personalizacao <> '')
    PRINT 'UNION ' + @CRLF
PRINT @COMANDO_SQL_Severidade1
IF (@COMANDO_SQL_ComandosSQL + @COMANDO_SQL_Severidade1 <> '') AND (@COMANDO_SQL_DataEntrega + @COMANDO_SQL_DataAnalise + @COMANDO_SQL_Personalizacao <> '')
    PRINT 'UNION ' + @CRLF
PRINT @COMANDO_SQL_DataEntrega
IF (@COMANDO_SQL_ComandosSQL + @COMANDO_SQL_Severidade1 + @COMANDO_SQL_DataEntrega <> '') AND (@COMANDO_SQL_DataAnalise + @COMANDO_SQL_Personalizacao <> '')
    PRINT 'UNION ' + @CRLF
PRINT @COMANDO_SQL_DataAnalise
IF (@COMANDO_SQL_ComandosSQL + @COMANDO_SQL_Severidade1 + @COMANDO_SQL_DataEntrega + @COMANDO_SQL_DataAnalise <> '') AND (@COMANDO_SQL_Personalizacao <> '')
    PRINT 'UNION ' + @CRLF
IF COALESCE(@COMANDO_SQL_Personalizacao, '') <> ''
    PRINT @COMANDO_SQL_Personalizacao

PRINT 'ORDER BY 1, CHA_CdiChamada DESC'



