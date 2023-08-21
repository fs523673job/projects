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
  TextHeight = 15
  object Button1: TButton
    Left = 8
    Top = 312
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
      ' /*tag seguranca*/')
    TabOrder = 1
    ExplicitLeft = 8
    ExplicitTop = 8
    ExplicitWidth = 593
  end
end
