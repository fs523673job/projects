object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Teste'
  ClientHeight = 454
  ClientWidth = 988
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    988
    454)
  PixelsPerInch = 96
  TextHeight = 13
  object btnHierarquiaTags: TBitBtn
    Left = 8
    Top = 8
    Width = 161
    Height = 25
    Caption = 'Hierarquia das Tags'
    TabOrder = 0
    OnClick = btnHierarquiaTagsClick
  end
  object Memo: TMemo
    Left = 175
    Top = 32
    Width = 805
    Height = 193
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object cmbFiles: TComboBox
    Left = 175
    Top = 8
    Width = 687
    Height = 21
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 2
    Text = 'D:\Chamados\Arquivos\396089\ConverterXmlToXml\TesteXML.XML'
    Items.Strings = (
      'D:\Chamados\Arquivos\396089\ConverterXmlToXml\TesteXML.XML'
      'D:\Chamados\Arquivos\396089\ConverterXmlToXml\TesteXML_1.XML'
      'D:\Chamados\Arquivos\396089\ConverterXmlToXml\TesteXML_2.XML'
      'D:\Chamados\Arquivos\396089\ConverterXmlToXml\TesteXML_3.XML'
      'D:\Chamados\Arquivos\396089\ConverterXmlToXml\TesteXML_4.XML'
      'D:\Chamados\Arquivos\396089\ConverterXmlToXml\TesteXML_5.XML'
      'D:\Chamados\Arquivos\396089\ConverterXmlToXml\TesteXML_6.XML')
  end
  object btnConvertXmlToCsv: TBitBtn
    Left = 8
    Top = 35
    Width = 161
    Height = 25
    Caption = 'Convert XML Para CSV'
    TabOrder = 3
    OnClick = btnConvertXmlToCsvClick
  end
  object btnConvertXmlToCsvTags: TBitBtn
    Left = 8
    Top = 62
    Width = 161
    Height = 25
    Caption = 'Convert XML Para CSV'
    TabOrder = 4
    OnClick = btnConvertXmlToCsvTagsClick
  end
  object btnClear: TBitBtn
    Left = 8
    Top = 177
    Width = 161
    Height = 25
    Caption = 'Limpar Memo'
    TabOrder = 5
    OnClick = btnClearClick
  end
  object MemoHeader: TMemo
    Left = 175
    Top = 231
    Width = 805
    Height = 215
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 6
  end
  object btnColunaCSV: TBitBtn
    Left = 8
    Top = 88
    Width = 161
    Height = 25
    Caption = 'Coluna CSV'
    TabOrder = 7
    OnClick = btnColunaCSVClick
  end
  object btnTiposCSV: TBitBtn
    Left = 8
    Top = 117
    Width = 161
    Height = 25
    Caption = 'Tipos CSV'
    TabOrder = 8
    OnClick = btnTiposCSVClick
  end
  object btnAliasCSV: TBitBtn
    Left = 8
    Top = 146
    Width = 161
    Height = 25
    Caption = 'Alias CSV'
    TabOrder = 9
    OnClick = btnAliasCSVClick
  end
  object btnCarregarXML: TBitBtn
    Left = 868
    Top = 8
    Width = 110
    Height = 21
    Caption = 'Carregar XML'
    TabOrder = 10
    OnClick = btnCarregarXMLClick
  end
  object OpenDialog: TOpenDialog
    Left = 776
    Top = 40
  end
end
