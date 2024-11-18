object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Criptografia Sim'#233'trica - SecureBlackbox'
  ClientHeight = 533
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
  object ckAlterExtension: TCheckBox
    Left = 8
    Top = 51
    Width = 225
    Height = 17
    Caption = 'Salvar Criptografia Para Novo Arquivo '
    TabOrder = 1
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
    TabOrder = 2
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
  object ckLoadFile: TCheckBox
    Left = 234
    Top = 51
    Width = 113
    Height = 17
    Caption = 'Carregar arquivo'
    TabOrder = 3
  end
  object ckbAddHeaderSecuritySymetric: TCheckBox
    Left = 8
    Top = 74
    Width = 257
    Height = 17
    Caption = 'Salvar Header de Criptografia nos arquivos'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object pgControl: TPageControl
    Left = 8
    Top = 416
    Width = 612
    Height = 105
    ActivePage = tbClasseReSymetricCript
    TabOrder = 5
    object tabApenasMetodos: TTabSheet
      Caption = 'Apenas Metodos'
      object btnEncrypt: TButton
        Left = 3
        Top = 10
        Width = 161
        Height = 25
        Caption = 'Encriptografar'
        TabOrder = 0
        OnClick = btnEncryptClick
      end
      object btnDescriptografar: TButton
        Left = 170
        Top = 10
        Width = 161
        Height = 25
        Caption = 'Descriptografar'
        TabOrder = 1
        OnClick = btnDescriptografarClick
      end
      object btStress: TButton
        Left = 341
        Top = 10
        Width = 203
        Height = 25
        Caption = 'Stress Encryp/Decript'
        TabOrder = 2
        OnClick = btStressClick
      end
      object edtCount: TEdit
        Left = 552
        Top = 10
        Width = 51
        Height = 23
        Alignment = taRightJustify
        NumbersOnly = True
        TabOrder = 3
        Text = '100'
      end
    end
    object tbClasseSymetricCript: TTabSheet
      Caption = 'Classe SymetricCript'
      ImageIndex = 1
      object btnEncryptSC: TButton
        Left = 3
        Top = 10
        Width = 161
        Height = 25
        Caption = 'Encriptografar'
        TabOrder = 0
        OnClick = btnEncryptSCClick
      end
      object btDescriptografarSC: TButton
        Left = 170
        Top = 10
        Width = 161
        Height = 25
        Caption = 'Descriptografar'
        TabOrder = 1
        OnClick = btDescriptografarSCClick
      end
      object btStressSC: TButton
        Left = 341
        Top = 10
        Width = 203
        Height = 25
        Caption = 'Stress Encryp/Decript'
        TabOrder = 2
        OnClick = btStressClick
      end
      object Edit1: TEdit
        Left = 552
        Top = 10
        Width = 51
        Height = 23
        Alignment = taRightJustify
        NumbersOnly = True
        TabOrder = 3
        Text = '100'
      end
    end
    object tbClasseReSymetricCript: TTabSheet
      Caption = 'Classe ReSymetricCript'
      ImageIndex = 2
      object btEncrypt3: TButton
        Left = 3
        Top = 10
        Width = 161
        Height = 25
        Caption = 'Encriptografar'
        TabOrder = 0
        OnClick = btEncrypt3Click
      end
      object btDecript3: TButton
        Left = 170
        Top = 10
        Width = 161
        Height = 25
        Caption = 'Descriptografar'
        TabOrder = 1
      end
      object btStress3: TButton
        Left = 341
        Top = 10
        Width = 203
        Height = 25
        Caption = 'Stress Encryp/Decript'
        TabOrder = 2
      end
      object Edit2: TEdit
        Left = 552
        Top = 10
        Width = 51
        Height = 23
        Alignment = taRightJustify
        NumbersOnly = True
        TabOrder = 3
        Text = '100'
      end
      object cmbTypeCriptografic: TComboBox
        Left = 3
        Top = 43
        Width = 161
        Height = 22
        Style = csOwnerDrawFixed
        ItemIndex = 0
        TabOrder = 4
        Text = 'Legado'
        Items.Strings = (
          'Legado'
          'Keks/Deks')
      end
    end
  end
  object OpenDialog: TOpenDialog
    Left = 424
    Top = 88
  end
end
