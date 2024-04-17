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
      'SELECT'
      'LTR_CDILOGTRANSACAO,'
      'LTR_CDILOG,'
      'LTR_CDIUSUARIO,'
      'CWQ_CDSUSUARIO,'
      'CWQ_CDICONTRATADO,'
      'CWQ_DSSNOMECOMPLETO,'
      'DATA_INICIO,'
      'TRANSACAO,'
      'TTR_D1STIPOTRANSACAO,'
      'OBJ_CDIOBJETO,'
      'OBJ_D1SOBJETO,'
      'CALCULO,'
      'ACL_D1SCALCULOLPC'
      'FROM'
      '('
      '  SELECT '
      '  LTR_CDILOGTRANSACAO,'
      '  LTR_CDILOG,'
      '  LTR_CDIUSUARIO,'
      '  LTR_DTDDATAHORATRANSACAOINICIO AS DATA_INICIO,'
      '  --LTR_DSSREFERENCIA_BASE,'
      
        '  SUBSTR(LTR_DSSREFERENCIA_BASE, (INSTR(LTR_DSSREFERENCIA_BASE, ' +
        #39'('#39')) + 1, 5) AS TRANSACAO,'
      
        '  SUBSTR(LTR_DSSREFERENCIA_BASE, (INSTR(LTR_DSSREFERENCIA_BASE, ' +
        #39'('#39')) + 13, 5) AS CALCULO'
      '  FROM '
      '  LOGSTRANSACOES, '
      '  LOGS'
      '  WHERE'
      '  LTR_CDILOG = LOS_CDILOG'
      '  AND LTR_CDITRANSACAO IN (33633)'
      '  AND LTR_DTDDATAHORATRANSACAOINICIO > 0'
      ')'
      ','
      'TRANSACOES,'
      'TIPOSTRANSACOES,'
      'PASTAS,'
      'REGIOES,'
      'OBJETOS,'
      'CALCULOSLPC,'
      'USUARIOSNOMESVIEW'
      'WHERE TRANSACAO = TRN_CDITRANSACAO'
      'AND TRN_CDITIPOTRANSACAO = TTR_CDITIPOTRANSACAO'
      'AND TRN_CDIPASTA = ATA_CDIPASTA'
      'AND ATA_CDIREGIAO = APD_CDIREGIAO'
      'AND APD_CDIOBJETO = OBJ_CDIOBJETO'
      'AND CALCULO = ACL_CDICALCULOLPC'
      'AND LTR_CDIUSUARIO = CWQ_CDIUSUARIO'
      '/*AutoEmployeeFilter=Contratados*/'
      'ORDER BY 4 DESC'
      ''
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
