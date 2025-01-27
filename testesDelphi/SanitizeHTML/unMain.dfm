object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 480
  ClientWidth = 976
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object seInput: TSynEdit
    Left = 8
    Top = 8
    Width = 473
    Height = 345
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
    Highlighter = SynHTMLSyn1
    Lines.Strings = (
      '<div>'
      #9'<u>'
      #9#9'<b>'
      
        #9#9#9'<span style="color:purple;font-family:'#39'MS Sans Serif'#39';;">Desc' +
        'ritivo</span>'
      #9#9'</b>'
      #9'</u>'
      '</div>'
      '<div>'
      
        #9'<span style="color:navy;font-family:'#39'MS Sans Serif'#39';;">Este car' +
        'go foi criado com objetivo de atender '#224' demanda de atividades bu' +
        'rocr'#225'ticas das '#225'reas administrativas da empresa, para que sejam ' +
        'as Chefias liberadas para atividades mais gerenciais</span>'
      '</div>'
      '<div>'
      
        #9'<span style="color:navy;font-family:'#39'MS Sans Serif'#39';;">do que o' +
        'peracionais.</span>'
      '</div>'
      '<div>'
      #9'<u>'
      #9#9'<b>'
      
        #9#9#9'<span style="color:purple;font-family:'#39'MS Sans Serif'#39';;">Ativ' +
        'idades</span>'
      #9#9'</b>'
      #9'</u>'
      '</div>'
      '<div>'
      
        #9'<span style="color:teal;font-family:'#39'MS Sans Serif'#39';;">.Arquiva' +
        'r os documentos gerais, na frequ'#234'ncia, local e forma, conforme n' +
        'orma estabelecida.</span>'
      '</div>'
      '<div>'
      
        #9'<span style="color:teal;font-family:'#39'MS Sans Serif'#39';;">.Efetuar' +
        ' as cobran'#231'as de atividades das '#225'reas correlatas</span>'
      '</div>'
      '<div>'
      
        #9'<span style="color:teal;font-family:'#39'MS Sans Serif'#39';;">.Posicio' +
        'nar seu superior diariamente sobre as atividades pendentes</span' +
        '>'
      '</div>'
      '<div>'
      
        #9'<span style="color:teal;font-family:'#39'MS Sans Serif'#39';;">.Outras ' +
        'atividades, testes</span>'
      '</div>'
      '<div>'
      #9'<u>'
      #9#9'<b>'
      
        #9#9#9'<span style="color:olive;font-family:'#39'MS Sans Serif'#39';;">Respo' +
        'nsabilidades</span>'
      #9#9'</b>'
      #9'</u>'
      '</div>'
      '<div>'
      #9'<u>'
      #9#9'<b>'
      #9#9#9'<span style="color:olive;font-family:'#39'Symbol'#39';;">'#183' </span>'
      #9#9'</b>'
      #9'</u>'
      
        #9'<span style="color:maroon;font-family:'#39'Comic Sans MS'#39';;">Respon' +
        's'#225'vel pelo cumprimento de seus prazos, conforme estabelece a nor' +
        'ma em vigor</span>'
      '</div>'
      '<div>'
      #9'<span style="color:maroon;font-family:'#39'Symbol'#39';;">'#183' </span>'
      
        #9'<span style="color:maroon;font-family:'#39'Comic Sans MS'#39';;">Respon' +
        's'#225'vel pela cobran'#231'a das atividades cr'#237'ticas relativas ao seu dep' +
        'artamento, com rela'#231#227'o a terceiros</span>'
      '</div>'
      '<div>'
      #9'<span style="color:maroon;font-family:'#39'Symbol'#39';;">'#183' </span>'
      
        #9'<span style="color:maroon;font-family:'#39'Comic Sans MS'#39';;">Outras' +
        ' responsabilidades</span>'
      '</div>'
      '<div>'
      '    <img src=x onerror=alert("XSS Teste" )>'
      
        '    <img src="http://teste.teste.jpg" onerror=alert("XSS Teste" ' +
        ')>'
      '    <img src=http://teste.teste.jpg onerror=alert("XSS Teste" )>'
      
        '    <img src="https://teste.teste.jpg" onerror=alert("XSS Teste"' +
        ' )>'
      
        '    <img src=https://teste.teste.jpg onerror=alert("XSS Teste" )' +
        '>'
      '</div>'
      '<div>'
      
        #9'<span style="font-family:'#39#39'Arial'#39#39';font-size:11.00pt;">&amp;lt;' +
        'img src=x onerror=alert(&#39;teste&#39;)&amp;gt;</span>'
      
        #9'<span style="font-family:'#39#39'Arial'#39#39';font-size:11.00pt;">&amp;lt;' +
        'img src="http://teste.teste.jpg" onerror=alert(&#39;teste&#39;)&' +
        'amp;gt;</span>'
      
        #9'<span style="font-family:'#39#39'Arial'#39#39';font-size:11.00pt;">&amp;lt;' +
        'img src=http://teste.teste.jpg onerror=alert(&#39;teste&#39;)&am' +
        'p;gt;</span>'
      
        #9'<span style="font-family:'#39#39'Arial'#39#39';font-size:11.00pt;">&amp;lt;' +
        'img src="https://teste.teste.jpg" onerror=alert(&#39;teste&#39;)' +
        '&amp;gt;</span>'
      
        #9'<span style="font-family:'#39#39'Arial'#39#39';font-size:11.00pt;">&amp;lt;' +
        'img src=https://teste.teste.jpg onerror=alert(&#39;teste&#39;)&a' +
        'mp;gt;</span>'
      '</div>'#9
      ''
      #239#187#191'<img src=x onerror=alert(document.location)'
      #239#187#191'<img src=x onerror="alert(document.location)" '
      #239#187#191'<img src=x onerror="alert(document.location)"'
      #239#187#191'&lt;img src=x onerror=alert(document.location)'
      #239#187#191'<'#239#188#339#239#185#164'<img src=x onerror=alert(document.location>>'
      #239#187#191'<<img src=x onerror="alert(document.location)'
      #239#187#191#239#185#164'img src=x onerror="alert(document.location)"'
      #239#187#191'aaa'
      #239#187#191'<'#239#188#339#239#185#164'<img src=x onerror=alert(document.location)>>'
      #239#187#191'<'#28'd<img src=x onerror=alert(document.location)>>'
      #239#187#191'<<img src=x onerror=alert(document.location)>>'
      #239#187#191'<img src=x onerror=alert(document.location)>>'
      #239#187#191'<<img src=x onerror=alert(document.location)>>'
      #239#187#191'<<img src=x onerror=alert(document.location)>')
    SelectedColor.Alpha = 0.400000005960464500
  end
  object seOutput: TSynEdit
    Left = 489
    Top = 8
    Width = 473
    Height = 345
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    TabOrder = 1
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
    Highlighter = SynHTMLSyn1
    Lines.Strings = (
      'seOutput')
    SelectedColor.Alpha = 0.400000005960464500
  end
  object btnSanitizeHTML: TButton
    Left = 10
    Top = 392
    Width = 473
    Height = 25
    Caption = 'Sanitizar HTML'
    TabOrder = 2
    OnClick = btnSanitizeHTMLClick
  end
  object btClearAll: TButton
    Left = 8
    Top = 359
    Width = 473
    Height = 25
    Caption = 'Limpar Campos'
    TabOrder = 3
    OnClick = btClearAllClick
  end
  object cbAnalisysXSS: TCheckBox
    Left = 487
    Top = 359
    Width = 138
    Height = 17
    Caption = 'Analisa Tags Permitidas'
    TabOrder = 4
  end
  object cbAnalysisXSSTags: TCheckBox
    Left = 487
    Top = 382
    Width = 170
    Height = 17
    Caption = 'Analisa Conte'#250'do das Tags'
    TabOrder = 5
  end
  object btnProvavelHTML: TButton
    Left = 10
    Top = 428
    Width = 473
    Height = 25
    Caption = 'Prov'#225'vel HTML'
    TabOrder = 6
    OnClick = btnProvavelHTMLClick
  end
  object cmbExamples: TComboBox
    Left = 489
    Top = 428
    Width = 473
    Height = 22
    Style = csOwnerDrawFixed
    ItemIndex = 0
    TabOrder = 7
    Text = '01 - Exemplo Geral'
    OnChange = cmbExamplesChange
    Items.Strings = (
      '01 - Exemplo Geral'
      '02 - Conteudo 01'
      '03 - Conteudo 02'
      '04 - Conteudo 03'
      '05 - Conteudo 04')
  end
  object cbAnalysisXSSAllContent: TCheckBox
    Left = 488
    Top = 404
    Width = 153
    Height = 17
    Caption = 'Analisar Todo Conte'#250'do'
    Checked = True
    State = cbChecked
    TabOrder = 8
  end
  object chAnalysisXSSTexto: TCheckBox
    Left = 663
    Top = 359
    Width = 186
    Height = 17
    Caption = 'Analisar Conte'#250'do de um Texto'
    TabOrder = 9
  end
  object SynHTMLSyn1: TSynHTMLSyn
    Left = 440
    Top = 120
  end
end
