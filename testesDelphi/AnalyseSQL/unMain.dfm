object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 442
  ClientWidth = 777
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  DesignSize = (
    777
    442)
  TextHeight = 15
  object SpeedButton1: TSpeedButton
    Left = 0
    Top = 392
    Width = 217
    Height = 42
    Anchors = [akLeft, akBottom]
    Caption = 'Execute'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    OnClick = SpeedButton1Click
  end
  object SynEdit1: TSynEdit
    Left = 0
    Top = 0
    Width = 777
    Height = 361
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
    CodeFolding.GutterShapeSize = 11
    CodeFolding.CollapsedLineColor = clGrayText
    CodeFolding.FolderBarLinesColor = clGrayText
    CodeFolding.IndentGuidesColor = clGray
    CodeFolding.IndentGuides = True
    CodeFolding.ShowCollapsedLine = False
    CodeFolding.ShowHintMark = True
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -13
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Lines.Strings = (
      '/*NO_AUDIT*/'
      'Select distinct'
      '/*       BasesFGTSAux.JFK_CdiMes_Base,*/'
      '/*       BasesFGTSAux.JFK_DtiAno_Base,*/'
      '       BasesFGTSAux.Id, '
      '       BasesFGTSAux.DssBase,'
      '       BasesFGTSAux.JFA_CdiTipoBaseFGTS, '
      '       BasesFGTSAux.JFA_D1sTipoBaseFGTS'
      ''
      '       '
      ''
      'From ('
      ''
      ''
      'Select RetseSFGTrs.JFK_CdiMes_Base,'
      '       RetseSFGTrs.JFK_DtiAno_Base,'
      ''
      'Case when TiposBasesFGTS.JFA_D1sTipoBaseFGTS like '#39'%13%'#39'  Then 2'
      '     else 1 '
      '     end as id,'
      ''
      
        'Case when TiposBasesFGTS.JFA_D1sTipoBaseFGTS like '#39'%13%'#39'  Then '#39 +
        '13'#186' Sal'#225'rio'#39
      '     else '#39'Mensal'#39' '
      '     end as DssBase,'
      ''
      '       TiposBasesFGTS.JFA_CdiTipoBaseFGTS, '
      '       TiposBasesFGTS.JFA_D1sTipoBaseFGTS, '
      '       SUM(RetseSFGTrsEsLsTrsBs.JFO_VlnBaseCalculo) as SOMA'
      ''
      ''
      'FROM '
      '    RetseSFGTrs'
      '     '
      '    '
      
        '    INNER JOIN ProcessosLPC                 ON (JFK_CdiProcessoL' +
        'PC        = PRO_CdiProcessoLPC) '
      
        '    LEFT JOIN (                                                 ' +
        '     '
      
        '      SELECT SGN_CdiRemeSoc                                     ' +
        '     '
      
        '        , SGN_CdiStatusEnvioeSocial                             ' +
        '     '
      
        '        , MAX(FIZ_CdiContratado) AS FIZ_CdiContratado           ' +
        '     '
      
        '        , COUNT(DISTINCT FIZ_CdiContratado) AS TOTAL            ' +
        '     '
      
        '      FROM RemseSoc                                             ' +
        '     '
      
        '        INNER JOIN RemseSocRecs on (SGN_CdiRemeSoc = FIZ_CdiReme' +
        'Soc) '
      
        '      GROUP BY SGN_CdiRemeSoc, SGN_CdiStatusEnvioeSocial        ' +
        '     '
      
        '    ) VINCS                                    ON (VINCS.SGN_Cdi' +
        'RemeSoc      = JFK_NuiIdentificadorOrigem) '
      
        '                                              AND (JFK_CdiTipoAr' +
        'quivoeSocial = 11)                         '
      
        '    LEFT JOIN ConsComVincTeS                   ON (SKK_CdiConCom' +
        'VincTeS      = JFK_NuiIdentificadorOrigem) '
      
        '                                              AND (JFK_CdiTipoAr' +
        'quivoeSocial = 33)                         '
      
        '    LEFT JOIN ConsSemVincTeS                   ON (SKI_CdiConSem' +
        'VincTeS      = JFK_NuiIdentificadorOrigem) '
      
        '                                              AND (JFK_CdiTipoAr' +
        'quivoeSocial = 36)                         '
      
        '    LEFT JOIN RemsRPPSeS                       ON (FOH_CdiRemRPP' +
        'SeS          = JFK_NuiIdentificadorOrigem) '
      
        '                                              AND (JFK_CdiTipoAr' +
        'quivoeSocial = 43)                         '
      
        '    LEFT JOIN StatusEnvioseSocial StatusSGN    ON (StatusSGN.SCN' +
        '_CdiStatusEnvioeSocial = SGN_CdiStatusEnvioeSocial and StatusSGN' +
        '.SCN_CdiStatusEnvioeSocial > 0)'
      
        '    LEFT JOIN StatusEnvioseSocial StatusSKK    ON (StatusSKK.SCN' +
        '_CdiStatusEnvioeSocial = SKK_CdiStatusEnvioeSocial and StatusSKK' +
        '.SCN_CdiStatusEnvioeSocial > 0)'
      
        '    LEFT JOIN StatusEnvioseSocial StatusSKI    ON (StatusSKI.SCN' +
        '_CdiStatusEnvioeSocial = SKI_CdiStatusEnvioeSocial and StatusSKI' +
        '.SCN_CdiStatusEnvioeSocial > 0)'
      
        '    LEFT JOIN StatusEnvioseSocial StatusFOH    ON (StatusFOH.SCN' +
        '_CdiStatusEnvioeSocial = FOH_CdiStatusEnvioeSocial and StatusFOH' +
        '.SCN_CdiStatusEnvioeSocial > 0)'
      ''
      
        '    LEFT  JOIN Contratados  Contratados        ON ( Contratados.' +
        'CON_CdiContratado = VINCS.FIZ_CdiContratado'
      
        '                                               or   Contratados.' +
        'CON_CdiContratado = SKK_CdiContratado'
      
        '                                               or   Contratados.' +
        'CON_CdiContratado = SKI_CdiContratado)'
      ''
      
        '    INNER JOIN StatusEnvEsocPreceds             ON (HYY_CdiStatu' +
        'sEnvEsocPreced =      Coalesce(NullIf(StatusSGN.SCN_CdiStatusEnv' +
        'ioeSocial, 0), Coalesce(NullIf(StatusSKK.SCN_CdiStatusEnvioeSoci' +
        'al, 0), Coalesce(NullIf(StatusSKI.SCN_CdiStatusEnvioeSocial, 0),' +
        ' Coalesce(NullIf(StatusFOH.SCN_CdiStatusEnvioeSocial, 0), 0)))))' +
        ' '
      
        '    LEFT  JOIN  LocaisMatrizesView               ON (JFK_NusInsc' +
        'ricao_Tratado  = ESR_NusInscricao_Tratado ) '
      
        '    LEFT  JOIN Folhas  Folhas                ON (Folhas.FOL_CdiF' +
        'olha = Contratados.CON_CdiFolha)'
      
        '    left  Join DefFolFerias DefFolFerias ON ( DefFolFerias.DFF_C' +
        'diFolha = Folhas.FOL_CdiFolha )'
      
        '    LEFT  JOIN Locais  Locais                ON (Locais.LOC_CdiL' +
        'ocal = Folhas.FOL_CdiLocal)'
      '   '
      
        '    LEFT JOIN  Empresas                         ON (ESR_CdiEmpre' +
        'sa             = EMP_CdiEmpresa) '
      '    '
      
        '   Inner Join Situacoes Situacoes ON ( Situacoes.SIT_CdiSituacao' +
        ' = Contratados.CON_CdiSituacao )'
      
        '   Inner Join Cargos Cargos ON (Cargos.CAR_CdiCargo = Contratado' +
        's.CON_CdiCargo)'
      
        '   Inner Join Vinculos Vinculos ON ( Vinculos.VIN_CdiVinculo = C' +
        'ontratados.CON_CdiVinculo )'
      
        '   Inner join TiposArquivoseSocial TiposArquivoseSocial on (Tipo' +
        'sArquivoseSocial.SAY_CdiTipoArquivoeSocial = RetseSFGTrs.JFK_Cdi' +
        'TipoArquivoeSocial)'
      ''
      
        '   Inner join RetseSFGTrsEs RetseSFGTrsEs on (RetseSFGTrs.JFK_Cd' +
        'iReteSFGTr = RetseSFGTrsEs.YAG_CdiReteSFGTr)'
      
        '   Inner join RetseSFGTrsEsLs RetseSFGTrsEsLs on (RetseSFGTrsEsL' +
        's.JFM_CdiReteSFGTrE = RetseSFGTrsEs.YAG_CdiReteSFGTrE)'
      
        '   Inner join RetseSFGTrsEsLsTrs RetseSFGTrsEsLsTrs on (RetseSFG' +
        'TrsEsLsTrs.JFN_CdiReteSFGTrEL = RetseSFGTrsEsLs.JFM_CdiReteSFGTr' +
        'EL )'
      
        '   Inner join RetseSFGTrsEsLsTrsBs RetseSFGTrsEsLsTrsBs on (Rets' +
        'eSFGTrsEsLsTrsBs.JFO_CdiReteSFGTrELTr = RetseSFGTrsEsLsTrs.JFN_C' +
        'diReteSFGTrELTr)'
      ''
      
        '   Inner join TiposBasesFGTS TiposBasesFGTS on (RetseSFGTrsEsLsT' +
        'rsBs.JFO_CdiTipoBaseFGTS = TiposBasesFGTS.JFA_CdiTipoBaseFGTS)'
      '     '
      'WHERE '
      
        '    Coalesce(NullIf(StatusSGN.SCN_CdiStatusEnvioeSocial, 0), Coa' +
        'lesce(NullIf(StatusSKK.SCN_CdiStatusEnvioeSocial, 0), Coalesce(N' +
        'ullIf(StatusSKI.SCN_CdiStatusEnvioeSocial, 0), Coalesce(NullIf(S' +
        'tatusFOH.SCN_CdiStatusEnvioeSocial, 0), 0)))) > 0'
      ''
      '    AND (JFK_DtiAno_Base = :ano) '
      
        '    and ((JFK_CdiMes_Base = :mes) or (JFK_CdiMes_Base = 0 and :G' +
        'erar13S = 1)) '
      ''
      'AND EMPRESAS.EMP_CdiEmpresa in (:Empresa)'
      'AND CONTRATADOS.CON_CdiContratado in (:Contratado)'
      ''
      '/*AutoEmployeeFilter=Contratados*/'
      ''
      'GROUP BY '
      '       RetseSFGTrs.JFK_CdiMes_Base,'
      '       RetseSFGTrs.JFK_DtiAno_Base,'
      '       TiposBasesFGTS.JFA_CdiTipoBaseFGTS, '
      '       TiposBasesFGTS.JFA_D1sTipoBaseFGTS'
      ''
      ') BasesFGTSAux'
      ''
      'GROUP BY '
      '       BasesFGTSAux.JFK_CdiMes_Base,'
      '       BasesFGTSAux.JFK_DtiAno_Base,'
      '       BasesFGTSAux.Id, '
      '       BasesFGTSAux.DssBase,'
      '       BasesFGTSAux.JFA_CdiTipoBaseFGTS, '
      '       BasesFGTSAux.JFA_D1sTipoBaseFGTS'
      ''
      ''
      '   Order By '
      '-- 2,1,'
      '1,3')
    FontSmoothing = fsmNone
  end
end
