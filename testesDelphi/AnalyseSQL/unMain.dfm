object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 445
  ClientWidth = 932
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
    Width = 424
    Height = 403
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
    Lines.Strings = (
      'Select CON_CdiCOntratado, CON_CdiSItuacao'
      'From contratados'
      'Where CON_CdiSItuacao = 1 or CON_CdiSItuacao = 9'
      '/*AutoEmployeeFilter=Contratados*/'
      'Order by 1')
    FontSmoothing = fsmNone
    ExplicitWidth = 441
    ExplicitHeight = 361
  end
  object SynEdit2: TSynEdit
    Left = 424
    Top = 0
    Width = 508
    Height = 403
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
    Top = 403
    Width = 932
    Height = 42
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 409
    DesignSize = (
      932
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
end
