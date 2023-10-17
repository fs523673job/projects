object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Assinatura de PDF'
  ClientHeight = 408
  ClientWidth = 453
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 136
    Width = 438
    Height = 85
    Caption = 'Certificado em Arquivo'
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 57
      Width = 34
      Height = 13
      Caption = 'Senha:'
    end
    object edtCertFile: TEdit
      Left = 16
      Top = 25
      Width = 302
      Height = 21
      TabOrder = 0
    end
    object btnLoadCert: TBitBtn
      Left = 322
      Top = 23
      Width = 109
      Height = 25
      Caption = 'Carregar Certificado'
      TabOrder = 1
      OnClick = btnLoadCertClick
    end
    object edtPassCert: TEdit
      Left = 57
      Top = 54
      Width = 259
      Height = 21
      TabOrder = 2
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 226
    Width = 438
    Height = 62
    Caption = 'Certificado do Windows'
    TabOrder = 1
    object cmbCertificate: TComboBox
      Left = 16
      Top = 26
      Width = 415
      Height = 21
      TabOrder = 0
      Text = 'cmbCertificate'
    end
  end
  object btnSignDocument: TBitBtn
    Left = 8
    Top = 371
    Width = 153
    Height = 25
    Caption = 'Assinar PDF'
    TabOrder = 2
    OnClick = btnSignDocumentClick
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 300
    Width = 438
    Height = 66
    Caption = 'Adicionar uma image JPEG'
    TabOrder = 3
    object edtJPEGPath: TEdit
      Left = 16
      Top = 24
      Width = 302
      Height = 21
      TabOrder = 0
      Text = 'D:\Chamados\Arquivos\489275\Signer\Assinatura.jpg'
    end
    object btnLoadJpegPath: TBitBtn
      Left = 322
      Top = 22
      Width = 109
      Height = 25
      Caption = 'Carregar JPEG'
      TabOrder = 1
      OnClick = btnLoadJpegPathClick
    end
  end
  object GroupBox4: TGroupBox
    Left = 8
    Top = 8
    Width = 438
    Height = 120
    Caption = 'PDF Para Ser Assinado'
    TabOrder = 4
    object Label2: TLabel
      Left = 16
      Top = 48
      Width = 166
      Height = 13
      Caption = 'Campos dispon'#237'vel para assinatura'
    end
    object edtPDF: TEdit
      Left = 16
      Top = 20
      Width = 300
      Height = 21
      TabOrder = 0
    end
    object btnLoadPDF: TBitBtn
      Left = 322
      Top = 18
      Width = 109
      Height = 25
      Caption = 'Carregar PDF'
      TabOrder = 1
      OnClick = btnLoadPDFClick
    end
    object cmbCamposAssinaturas: TComboBox
      Left = 16
      Top = 67
      Width = 415
      Height = 21
      TabOrder = 2
    end
    object ckbTodos: TCheckBox
      Left = 16
      Top = 94
      Width = 193
      Height = 17
      Caption = 'Teste: Assinar Todos os Campos'
      TabOrder = 3
    end
    object btnRemoveSignature: TButton
      Left = 322
      Top = 92
      Width = 109
      Height = 25
      Caption = 'Remove Signature'
      TabOrder = 4
      OnClick = btnRemoveSignatureClick
    end
  end
  object OpenDialog: TOpenDialog
    Left = 256
  end
end
