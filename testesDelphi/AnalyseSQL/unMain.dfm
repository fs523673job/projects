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
      'Select CON_CdiCOntratado, CON_CdiSItuacao'
      'From contratados'
      'Where CON_CdiSItuacao = 1 or CON_CdiSItuacao = 9'
      '/*AutoEmployeeFilter=Contratados*/'
      'Order by 1')
    FontSmoothing = fsmNone
    ExplicitWidth = 773
  end
end
