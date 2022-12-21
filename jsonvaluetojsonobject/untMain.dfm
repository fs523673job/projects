object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'JSONValue to JSONObject'
  ClientHeight = 427
  ClientWidth = 478
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblexemplosjson: TLabel
    Left = 8
    Top = 11
    Width = 70
    Height = 13
    Caption = 'Exemplos Json'
  end
  object cmbexemplosjson: TComboBox
    Left = 81
    Top = 8
    Width = 376
    Height = 22
    Style = csOwnerDrawFixed
    ItemIndex = 0
    TabOrder = 0
    Text = '01 - Json Simple'
    OnChange = cmbexemplosjsonChange
    Items.Strings = (
      '01 - Json Simple'
      '02 - Json Array'
      '03 - Json String')
  end
  object smexemposjson: TSynMemo
    Left = 8
    Top = 36
    Width = 449
    Height = 197
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 1
    CodeFolding.CollapsedLineColor = clGrayText
    CodeFolding.FolderBarLinesColor = clGrayText
    CodeFolding.ShowCollapsedLine = True
    CodeFolding.IndentGuidesColor = clGray
    CodeFolding.IndentGuides = True
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Highlighter = SynJSONSyn
    Lines.Strings = (
      '')
    FontSmoothing = fsmNone
  end
  object btnExecute: TButton
    Left = 8
    Top = 239
    Width = 105
    Height = 25
    Caption = 'Executar'
    TabOrder = 2
    OnClick = btnExecuteClick
  end
  object smjsonresult: TSynMemo
    Left = 8
    Top = 270
    Width = 449
    Height = 150
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 3
    CodeFolding.CollapsedLineColor = clGrayText
    CodeFolding.FolderBarLinesColor = clGrayText
    CodeFolding.ShowCollapsedLine = True
    CodeFolding.IndentGuidesColor = clGray
    CodeFolding.IndentGuides = True
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    FontSmoothing = fsmNone
  end
  object SynJSONSyn: TSynJSONSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 280
    Top = 56
  end
end
