object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Teste Conceito PDF (Visualizar e N'#250'mero de P'#225'ginas)'
  ClientHeight = 442
  ClientWidth = 645
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 69
    Height = 15
    Caption = 'Arquivo PDF:'
  end
  object edtPDFFile: TEdit
    Left = 83
    Top = 5
    Width = 467
    Height = 23
    TabOrder = 0
    Text = 
      'C:\github\fs523673job\projects\testesDelphi\Teste_Conceito_PDF\W' +
      'in64\Debug\app\NewPDFTest.PDF'
  end
  object btnLoadPDF: TBitBtn
    Left = 556
    Top = 4
    Width = 86
    Height = 25
    Caption = 'Carregar PDF'
    TabOrder = 1
    OnClick = btnLoadPDFClick
  end
  object PRPage1: TPRPage
    Left = 24
    Top = 35
    Width = 596
    Height = 842
    MarginTop = 32
    MarginLeft = 32
    MarginRight = 32
    MarginBottom = 32
  end
  object PReport1: TPReport
    FileName = 'default.pdf'
    CreationDate = 45554.485532187500000000
    UseOutlines = False
    ViewerPreference = []
    Left = 144
    Top = 104
  end
end
