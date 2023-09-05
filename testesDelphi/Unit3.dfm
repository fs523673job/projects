object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Teste TAG Seguran'#231'a'
  ClientHeight = 267
  ClientWidth = 1091
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
    Width = 1091
    Height = 226
    Align = alTop
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
      'select field1, field2'
      '  From tabela tabela'
      '  Where field1 = 1 '
      '         or field3 = 2 '
      '         or field4 in '
      '             (select field3, field4 '
      '               from tabela2 tabela2 '
      
        '               where field1 = 1 or field3 = 2 and condicao = 3 /' +
        '*AutoEmployeeFilter=ContratadosExtras,Contratados*/'
      '              )'
      '  /*AutoEmployeeFilter=Tabela*/')
    SelectedColor.Alpha = 0.400000005960464500
  end
  object Button1: TButton
    Left = 0
    Top = 232
    Width = 121
    Height = 25
    Caption = 'Executa Verifica'#231#227'o'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 127
    Top = 232
    Width = 121
    Height = 25
    Caption = 'Limpa Conte'#250'do'
    TabOrder = 2
    OnClick = Button2Click
  end
  object SynSQLSyn1: TSynSQLSyn
    Left = 1008
    Top = 16
  end
end
