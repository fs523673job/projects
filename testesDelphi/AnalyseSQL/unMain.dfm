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
      'Select (Field1 + Field2) As Field,'
      'CASE'
      
        '/* INDICADOR 42101025 DEVE SER SEMPRE PLANEJADO, ENQUANTO DEMAIS' +
        ' DEVEM SER BASEADOS NO M'#202'S CORRENTE DA @DTDBASE*/'
      
        'WHEN @TIPO=1 AND DATEDIFF = 0 OR INDICADOR NOT LIKE '#39'42101025'#39' T' +
        'HEN '#39'REAL'#39
      
        'WHEN @TIPO=1 AND DATEDIFF > 0 OR INDICADOR LIKE '#39'42101025'#39' THEN ' +
        #39'PLANEJADO'#39
      'WHEN @TIPO=0 THEN '#39'REAL'#39
      
        'WHEN (Select (Field1 + Field2) From Tabela Where (select Field1,' +
        ' Field2 from Contratados where CON_CdiContratado = 1 or CON_CdiC' +
        'ontratado = 2 and CON_CdiContratado > 99 /*autoemployeefilter=XP' +
        'TO*/))'
      'ELSE '#39'PLANEJADO'#39
      'END AS VERSAO,'
      ' '
      '  From Meses '
      '  where MES_CdiMes = 1 '
      '  and (select 1 '
      '        from Contratados '
      
        #9#9' where CON_CdiContratado = 1 or CON_CdiContratado = 2 and CON_' +
        'CdiContratado > 99'
      '  '#9'   ) '
      
        '  and (Field1 < Field 3 or (Field4 > Field5) and Field2 < (Field' +
        '1 or Field2) and MES_CdiMes < 10)'
      '/*autoemployeefilter=XPTO*/'
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
