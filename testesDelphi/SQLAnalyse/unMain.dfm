object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Main'
  ClientHeight = 442
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object seSQLText: TSynEdit
    Left = 8
    Top = 8
    Width = 615
    Height = 399
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    TabOrder = 0
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
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
    Highlighter = SynSQLSyn1
    Lines.Strings = (
      'seSQLText')
    SelectedColor.Alpha = 0.400000005960464500
  end
  object btnExecutar: TButton
    Left = 8
    Top = 413
    Width = 201
    Height = 25
    Caption = 'Executar'
    TabOrder = 1
    OnClick = btnExecutarClick
  end
  object SynSQLSyn1: TSynSQLSyn
    Left = 392
    Top = 80
  end
end
