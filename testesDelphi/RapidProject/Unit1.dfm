object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 348
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Button1: TButton
    Left = 81
    Top = 304
    Width = 75
    Height = 25
    Caption = 'Exec'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 628
    Height = 298
    Align = alTop
    Lines.Strings = (
      'Select Campo1, Campo2'
      '  From Tabela'
      '  Where  Condicao = Conteudo1'
      '           or Condicao = Conteudo2 '
      
        '        and (select field1, field2 from tabela2 where field3 = f' +
        'ield4)'
      ' /*AutoEmployeeFilter=Contratatados*/'
      'Order by 1')
    TabOrder = 1
    ExplicitWidth = 624
  end
  object Button4: TButton
    Left = 0
    Top = 304
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 2
  end
  object ComboBox1: TComboBox
    Left = 162
    Top = 304
    Width = 183
    Height = 23
    Style = csOwnerDrawFixed
    TabOrder = 3
    OnChange = ComboBox1Change
  end
end
