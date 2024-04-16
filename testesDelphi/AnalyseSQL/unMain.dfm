object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 437
  ClientWidth = 930
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object SynEdit1: TSynEdit
    Left = 0
    Top = 0
    Width = 422
    Height = 395
    Align = alClient
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
    Highlighter = SynSQLSyn1
    Lines.Strings = (
      
        'select CAV_NusCNPJ, CAV_CdiContratado, case when CAV_CdiRetencao' +
        'ImpostoRenda = 3562  or CAV_CdiRetencaoImpostoRenda = 1889 then ' +
        '561 else CAV_CdiRetencaoImpostoRenda end as CAV_CdiRetencaoImpos' +
        'toRenda, CAV_CosOficialDIRF, '
      '       SUM(CAV_VlnValor) as CAV_VlnValor'
      'from ConAnuaisDIRFValores'
      'where ('
      '       0 = 1 and '
      
        '       Exists(select SUM(x.CAV_VlnValor) from ConAnuaisDIRFValor' +
        'es x '
      
        '              where x.CAV_CdiContratado = ConAnuaisDIRFValores.C' +
        'AV_CdiContratado and '
      '                    x.CAV_DtiAno_Base = 2016 and '
      
        '                    x.CAV_CdiTipoValorAnual in (1, 2, 22, 23, 26' +
        ', 27) '
      '              group by x.CAV_CdiContratado '
      '              having SUM(x.CAV_VlnValor) > 0) or '
      '       0 <> 1'
      #9'  ) '
      #9'  and ('
      #9'        ('
      
        #9#9'      0 = 2016 and CAV_DtiAno_Base = (select AVR_DtiAnoBase fr' +
        'om DefSisFolhaPagamento where AVR_CdiSistema = 2)'
      #9#9'    ) '
      #9'        or CAV_DtiAno_Base = 2016'
      #9'     )'
      ''
      '      and CAV_CdiRetencaoImpostoRenda = 473'
      '      and CAV_CosOficialDIRF in ('#39'RTRT'#39', '#39'RTIRF'#39')'
      '/*autoemployeefilter=ConAnuaisDIRFValores*/'#9'  '
      #9'  '
      
        'group by CAV_NusCNPJ, CAV_CdiContratado, case when CAV_CdiRetenc' +
        'aoImpostoRenda = 3562  or CAV_CdiRetencaoImpostoRenda = 1889 the' +
        'n 561 else CAV_CdiRetencaoImpostoRenda end, CAV_CosOficialDIRF'
      ''
      '')
    FontSmoothing = fsmNone
  end
  object SynEdit2: TSynEdit
    Left = 422
    Top = 0
    Width = 508
    Height = 395
    Align = alRight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 1
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
      '')
    FontSmoothing = fsmNone
  end
  object Panel1: TPanel
    Left = 0
    Top = 395
    Width = 930
    Height = 42
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 386
    ExplicitWidth = 924
    DesignSize = (
      930
      42)
    object SpeedButton1: TSpeedButton
      Left = 0
      Top = 1
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
    object SpeedButton2: TSpeedButton
      Left = 233
      Top = 1
      Width = 217
      Height = 41
      Anchors = [akLeft, akBottom]
      Caption = 'Clear'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton2Click
    end
  end
  object SynSQLSyn1: TSynSQLSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 320
    Top = 184
  end
end
