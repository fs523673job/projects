object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Criptografia Sim'#233'trica - SecureBlackbox'
  ClientHeight = 442
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object spFileName: TSpeedButton
    Left = 597
    Top = 24
    Width = 23
    Height = 22
    Caption = '...'
    OnClick = spFileNameClick
  end
  object Label1: TLabel
    Left = 8
    Top = 99
    Width = 113
    Height = 15
    Caption = 'Conte'#250'do do arquivo'
  end
  object leFilePath: TLabeledEdit
    Left = 8
    Top = 24
    Width = 582
    Height = 23
    EditLabel.Width = 109
    EditLabel.Height = 15
    EditLabel.Caption = 'Caminho do arquivo'
    TabOrder = 0
    Text = 'Informe o caminho do arquivo'
  end
  object btnEncrypt: TButton
    Left = 8
    Top = 409
    Width = 161
    Height = 25
    Caption = 'Encriptografar'
    TabOrder = 1
    OnClick = btnEncryptClick
  end
  object ckAlterExtension: TCheckBox
    Left = 8
    Top = 51
    Width = 225
    Height = 17
    Caption = 'Salvar Criptografia Para Novo Arquivo '
    TabOrder = 2
  end
  object seContentFile: TSynEdit
    Left = 8
    Top = 120
    Width = 612
    Height = 283
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    TabOrder = 3
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
    Lines.Strings = (
      '')
    SelectedColor.Alpha = 0.400000005960464500
  end
  object Button1: TButton
    Left = 175
    Top = 409
    Width = 161
    Height = 25
    Caption = 'Descriptografar'
    TabOrder = 4
    OnClick = Button1Click
  end
  object OpenDialog: TOpenDialog
    Left = 424
    Top = 88
  end
end
