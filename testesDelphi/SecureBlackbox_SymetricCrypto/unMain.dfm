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
  object btnExecute: TButton
    Left = 8
    Top = 112
    Width = 161
    Height = 25
    Caption = 'Executar'
    TabOrder = 1
    OnClick = btnExecuteClick
  end
  object OpenDialog: TOpenDialog
    Left = 424
    Top = 88
  end
end
