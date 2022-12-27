object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Json To Xml'
  ClientHeight = 591
  ClientWidth = 876
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    876
    591)
  PixelsPerInch = 96
  TextHeight = 13
  object seXML: TSynEdit
    Left = 464
    Top = 8
    Width = 404
    Height = 487
    Anchors = [akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
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
    Highlighter = SynXMLSyn1
    Lines.Strings = (
      'seXML')
    FontSmoothing = fsmNone
    ExplicitLeft = 423
    ExplicitHeight = 462
  end
  object btnConvert: TButton
    Left = 8
    Top = 503
    Width = 290
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Convert JSON To XML'
    TabOrder = 1
    OnClick = btnConvertClick
    ExplicitTop = 476
  end
  object btConvert2: TButton
    Left = 8
    Top = 534
    Width = 290
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Convert JSON To XML [2]'
    TabOrder = 2
    OnClick = btConvert2Click
    ExplicitTop = 507
  end
  object seJSON: TSynEdit
    Left = 8
    Top = 8
    Width = 444
    Height = 489
    Anchors = [akLeft, akTop, akRight, akBottom]
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
    Highlighter = SynJSONSyn1
    Lines.Strings = (
      'seJSON')
    FontSmoothing = fsmNone
    ExplicitWidth = 407
    ExplicitHeight = 462
  end
  object SynXMLSyn1: TSynXMLSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    WantBracesParsed = False
    Left = 424
    Top = 168
  end
  object SynJSONSyn1: TSynJSONSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 168
    Top = 168
  end
end
