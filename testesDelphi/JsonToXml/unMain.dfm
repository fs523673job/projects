object frmMain: TfrmMain
  Left = 862
  Top = 0
  Caption = 'Main'
  ClientHeight = 743
  ClientWidth = 1553
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesigned
  TextHeight = 15
  object seJson: TSynEdit
    Left = 8
    Top = 8
    Width = 625
    Height = 553
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Consolas'
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    TabOrder = 0
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -13
    Gutter.Font.Name = 'Consolas'
    Gutter.Font.Style = []
    Gutter.Bands = <
      item
        Kind = gbkMarks
        Width = 13
      end
      item
        Kind = gbkLineNumbers
      end
      item
        Kind = gbkFold
      end
      item
        Kind = gbkTrackChanges
      end
      item
        Kind = gbkMargin
        Width = 3
      end>
    Highlighter = SynJSONSyn1
    Lines.Strings = (
      'seJson')
    SelectedColor.Alpha = 0.400000005960464500
  end
  object synXML: TSynEdit
    Left = 648
    Top = 8
    Width = 625
    Height = 553
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Consolas'
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    TabOrder = 1
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -13
    Gutter.Font.Name = 'Consolas'
    Gutter.Font.Style = []
    Gutter.Bands = <
      item
        Kind = gbkMarks
        Width = 13
      end
      item
        Kind = gbkLineNumbers
      end
      item
        Kind = gbkFold
      end
      item
        Kind = gbkTrackChanges
      end
      item
        Kind = gbkMargin
        Width = 3
      end>
    Highlighter = SynXMLSyn1
    Lines.Strings = (
      'seXML')
    SelectedColor.Alpha = 0.400000005960464500
  end
  object btConvertJsonToXml: TButton
    Left = 8
    Top = 567
    Width = 273
    Height = 25
    Caption = 'Convert Json To Xml'
    TabOrder = 2
  end
  object SynJSONSyn1: TSynJSONSyn
    Left = 72
    Top = 160
  end
  object SynXMLSyn1: TSynXMLSyn
    WantBracesParsed = False
    Left = 760
    Top = 192
  end
end
