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
  DesignSize = (
    876
    591)
  TextHeight = 13
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
    Font.Quality = fqClearTypeNatural
    TabOrder = 0
    CodeFolding.ShowCollapsedLine = True
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.Font.Quality = fqClearTypeNatural
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
      'seJSON')
    SelectedColor.Alpha = 0.400000005960464500
    ExplicitWidth = 440
    ExplicitHeight = 488
  end
  object btnConvertJasonXML: TButton
    Left = 8
    Top = 517
    Width = 290
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Convert JSON To XML'
    TabOrder = 1
    OnClick = btnConvertJasonXMLClick
    ExplicitLeft = 4
    ExplicitTop = 516
  end
  object seXML: TMemo
    Left = 458
    Top = 8
    Width = 410
    Height = 489
    Lines.Strings = (
      'seXML')
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object SynJSONSyn1: TSynJSONSyn
    Left = 168
    Top = 168
  end
end
