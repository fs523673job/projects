object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Verificar N'#243's Filhos'
  ClientHeight = 400
  ClientWidth = 701
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    701
    400)
  TextHeight = 15
  object Label1: TLabel
    Left = 4
    Top = 234
    Width = 42
    Height = 15
    Caption = 'Arquivo'
  end
  object seXML: TSynEdit
    Left = 0
    Top = 0
    Width = 701
    Height = 225
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
    Highlighter = SynXMLSyn1
    Lines.Strings = (
      'seXML')
    SelectedColor.Alpha = 0.400000005960464500
  end
  object edtFileName: TEdit
    Left = 56
    Top = 231
    Width = 637
    Height = 25
    Anchors = [akRight]
    TabOrder = 1
    Text = 
      'C:\github\fs523673job\projects\testesDelphi\Verificar_Nos_Filhos' +
      '\XMLRetorno.xml'
    ExplicitTop = 154
  end
  object btCarregarArquivo: TButton
    Left = 0
    Top = 262
    Width = 137
    Height = 25
    Caption = 'Carregar Arquivo'
    TabOrder = 2
    OnClick = btCarregarArquivoClick
  end
  object btTest: TButton
    Left = 143
    Top = 262
    Width = 137
    Height = 25
    Caption = 'Testar'
    TabOrder = 3
    OnClick = btTestClick
  end
  object SynXMLSyn1: TSynXMLSyn
    WantBracesParsed = False
    Left = 160
    Top = 16
  end
  object OpenDialog1: TOpenDialog
    Left = 408
    Top = 72
  end
end
