object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 437
  ClientWidth = 930
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object SynEdit1: TSynEdit
    Left = 0
    Top = 0
    Width = 422
    Height = 395
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
    CodeFolding.GutterShapeSize = 11
    CodeFolding.CollapsedLineColor = clGrayText
    CodeFolding.FolderBarLinesColor = clGrayText
    CodeFolding.IndentGuidesColor = clGray
    CodeFolding.IndentGuides = True
    CodeFolding.ShowCollapsedLine = False
    CodeFolding.ShowHintMark = True
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -13
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Highlighter = SynSQLSyn1
    Lines.Strings = (
      '/* no_audit */'
      'select (tab.CAV_NusCNPJ) as CAV_NusCNPJ,'
      
        '       (tab.CAV_CdiRetencaoImpostoRenda) as CAV_CdiRetencaoImpos' +
        'toRenda,'
      '       (tab.CAV_NusCampoQuebraMM) as CAV_NusCampoQuebraMM,'
      '       (tab.CAV_CosOficialDIRF) as CAV_CosOficialDIRF,'
      '       (tab.Tipo) as Tipo,'
      '       (tab.DZK_NusProcesso) as DZK_NusProcesso,'
      '       sum(coalesce(tab.CAV_VlnValor01,0)) as CAV_VlnValor01,'
      '       sum(coalesce(tab.CAV_VlnValor02,0)) as CAV_VlnValor02,'
      '       sum(coalesce(tab.CAV_VlnValor03,0)) as CAV_VlnValor03,'
      '       sum(coalesce(tab.CAV_VlnValor04,0)) as CAV_VlnValor04,'
      '       sum(coalesce(tab.CAV_VlnValor05,0)) as CAV_VlnValor05,'
      '       sum(coalesce(tab.CAV_VlnValor06,0)) as CAV_VlnValor06,'
      '       sum(coalesce(tab.CAV_VlnValor07,0)) as CAV_VlnValor07,'
      '       sum(coalesce(tab.CAV_VlnValor08,0)) as CAV_VlnValor08,'
      '       sum(coalesce(tab.CAV_VlnValor09,0)) as CAV_VlnValor09,'
      '       sum(coalesce(tab.CAV_VlnValor10,0)) as CAV_VlnValor10,'
      '       sum(coalesce(tab.CAV_VlnValor11,0)) as CAV_VlnValor11,'
      '       sum(coalesce(tab.CAV_VlnValor12,0)) as CAV_VlnValor12,'
      '       sum(coalesce(tab.CAV_VlnValor13,0)) as CAV_VlnValor13'
      'from ('
      'SELECT y.CAV_NusCNPJ,'
      
        '       (Case When Y.CAV_CdiProcTrabDIRF = 0 then y.CAV_CdiRetenc' +
        'aoImpostoRenda else 1889 end) as CAV_CdiRetencaoImpostoRenda,'
      '       y.CAV_NusCampoQuebraMM,'
      '       y.CAV_CosOficialDIRF,'
      
        '       (Case When Y.CAV_CdiProcTrabDIRF = 0  then '#39'01'#39'  Else '#39'02' +
        #39' End) as Tipo,'
      '       DZK_NusProcesso,'
      ''
      '       (select SUM(x.CAV_VlnValor)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CosOficialDIRF = y.CAV_CosOficialDIRF'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 1'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor01,'
      ''
      '       (select SUM(x.CAV_VlnValor)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CosOficialDIRF = y.CAV_CosOficialDIRF'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 2'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor02,'
      ''
      '       (select SUM(x.CAV_VlnValor)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CosOficialDIRF = y.CAV_CosOficialDIRF'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 3'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor03,'
      ''
      '       (select SUM(x.CAV_VlnValor)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CosOficialDIRF = y.CAV_CosOficialDIRF'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 4'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor04,'
      ''
      '       (select SUM(x.CAV_VlnValor)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CosOficialDIRF = y.CAV_CosOficialDIRF'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 5'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor05,'
      ''
      '       (select SUM(x.CAV_VlnValor)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CosOficialDIRF = y.CAV_CosOficialDIRF'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 6'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor06,'
      ''
      '       (select SUM(x.CAV_VlnValor)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CosOficialDIRF = y.CAV_CosOficialDIRF'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 7'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor07,'
      ''
      '       (select SUM(x.CAV_VlnValor)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CosOficialDIRF = y.CAV_CosOficialDIRF'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 8'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor08,'
      ''
      '       (select SUM(x.CAV_VlnValor)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CosOficialDIRF = y.CAV_CosOficialDIRF'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 9'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor09,'
      ''
      '       (select SUM(x.CAV_VlnValor)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CosOficialDIRF = y.CAV_CosOficialDIRF'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 10'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor10,'
      ''
      '       (select SUM(x.CAV_VlnValor)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CosOficialDIRF = y.CAV_CosOficialDIRF'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 11'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor11,'
      ''
      '       (select SUM(x.CAV_VlnValor)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CosOficialDIRF = y.CAV_CosOficialDIRF'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 12'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor12,'
      ''
      '       (select SUM(x.CAV_VlnValor)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CosOficialDIRF = y.CAV_CosOficialDIRF'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and w.TLA_D1sTipoValorAnual like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor13'
      ''
      '  FROM ConAnuaisDIRFValores y'
      
        ' INNER JOIN Contratados ON (y.CAV_CdiContratado = CON_CdiContrat' +
        'ado)'
      ' INNER JOIN Vinculos ON (CON_CdiVinculo = VIN_CdiVinculo)'
      ' INNER JOIN TiposValoresAnuais ON (CAV_CdiTipoValorAnual ='
      '                                  TLA_CdiTipoValorAnual)'
      
        ' INNER JOIN ProcsTrabsDIRF on (DZK_CdiProcTrabDIRF = y.CAV_CdiPr' +
        'ocTrabDIRF)'
      ''
      ' WHERE ((0 = 2016 AND y.CAV_DtiAno_Base ='
      '       (select AVR_DtiAnoBase'
      '                         from DefSisFolhaPagamento'
      '                        where AVR_CdiSistema = 2)) OR'
      '       y.CAV_DtiAno_Base = 2016)'
      '   AND VIN_CdiTipoContratado NOT IN (7, 6)'
      '   AND y.CAV_CosOficialDIRF IN'
      
        '       ('#39'CJAA'#39', '#39'CJAC'#39', '#39'DTPSE'#39', '#39'ESDJ'#39', '#39'ESDP'#39', '#39'ESIR'#39', '#39'ESPA'#39',' +
        ' '#39'ESPO'#39','
      
        '        '#39'ESPP'#39', '#39'ESRT'#39', '#39'RIAP'#39', '#39'RIDAC'#39', '#39'RIIRP'#39', '#39'RIL96'#39', '#39'RIMO' +
        'G'#39', '#39'RIO'#39','
      
        '        '#39'RIP65'#39', '#39'RIPTS'#39', '#39'RTDP'#39', '#39'RTIRF'#39', '#39'RTPA'#39', '#39'RTPO'#39', '#39'RTPP' +
        #39', '#39'RTRT'#39','
      '        '#39'TPSE'#39','#39'DAJUD'#39','#39'QTMESES'#39','#39'RIVC'#39','#39'RIBMR'#39')'
      
        '   and y.CAV_CosOficialDIRF not in ('#39'RIL96'#39', '#39'RIPTS'#39', '#39'RIO'#39', '#39'TP' +
        'SE'#39', '#39'DTPSE'#39')'
      ''
      '       AND Y.CAV_CdiRetencaoImpostoRenda <> 473'
      ''
      '/*autoemployeefilter=y*/'
      ''
      ' GROUP BY y.CAV_DtiAno_Base,'
      '          y.CAV_CosOficialDIRF,'
      '          y.CAV_NusCNPJ,'
      '          y.CAV_CdiRetencaoImpostoRenda,'
      '          y.CAV_NusCampoQuebraMM,'
      '          DZK_NusProcesso,'
      '          y.CAV_CdiProcTrabDIRF'
      ''
      ''
      'union'
      ''
      ''
      'SELECT y.CAV_NusCNPJ,'
      
        '       (Case When Y.CAV_CdiProcTrabDIRF = 0  then y.CAV_CdiReten' +
        'caoImpostoRenda else 1889 end) as CAV_CdiRetencaoImpostoRenda,'
      '       y.CAV_NusCampoQuebraMM,'
      '       '#39'zQTMESES'#39' as CAV_CosOficialDIRF,'
      
        '       (Case When Y.CAV_CdiProcTrabDIRF = 0  then '#39'01'#39'  Else '#39'02' +
        #39' End) as Tipo,'
      '       DZK_NusProcesso,'
      ''
      '       (select SUM(x.CAV_QtnMesesRRA)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 1'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and x.CAV_CdiProcTrabDIRF > 0'
      '           and x.CAV_CdiTipoValorAnual = 63'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor01,'
      ''
      '       (select SUM(x.CAV_QtnMesesRRA)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 2'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and x.CAV_CdiProcTrabDIRF > 0'
      '           and x.CAV_CdiTipoValorAnual = 63'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor02,'
      ''
      '       (select SUM(x.CAV_QtnMesesRRA)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 3'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and x.CAV_CdiProcTrabDIRF > 0'
      '           and x.CAV_CdiTipoValorAnual = 63'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor03,'
      ''
      '       (select SUM(x.CAV_QtnMesesRRA)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 4'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and x.CAV_CdiProcTrabDIRF > 0'
      '           and x.CAV_CdiTipoValorAnual = 63'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor04,'
      ''
      '       (select SUM(x.CAV_QtnMesesRRA)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 5'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and x.CAV_CdiProcTrabDIRF > 0'
      '           and x.CAV_CdiTipoValorAnual = 63'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor05,'
      ''
      '       (select SUM(x.CAV_QtnMesesRRA)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 6'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and x.CAV_CdiProcTrabDIRF > 0'
      '           and x.CAV_CdiTipoValorAnual = 63'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor06,'
      ''
      '       (select SUM(x.CAV_QtnMesesRRA)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 7'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and x.CAV_CdiProcTrabDIRF > 0'
      '           and x.CAV_CdiTipoValorAnual = 63'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor07,'
      ''
      '       (select SUM(x.CAV_QtnMesesRRA)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 8'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and x.CAV_CdiProcTrabDIRF > 0'
      '           and x.CAV_CdiTipoValorAnual = 63'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor08,'
      ''
      '       (select SUM(x.CAV_QtnMesesRRA)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 9'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and x.CAV_CdiProcTrabDIRF > 0'
      '           and x.CAV_CdiTipoValorAnual = 63'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor09,'
      ''
      '       (select SUM(x.CAV_QtnMesesRRA)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 10'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and x.CAV_CdiProcTrabDIRF > 0'
      '           and x.CAV_CdiTipoValorAnual = 63'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor10,'
      ''
      '       (select SUM(x.CAV_QtnMesesRRA)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 11'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and x.CAV_CdiProcTrabDIRF > 0'
      '           and x.CAV_CdiTipoValorAnual = 63'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor11,'
      ''
      '       (select SUM(x.CAV_QtnMesesRRA)'
      '          from ConAnuaisDIRFValores x, TiposValoresAnuais w'
      '         where x.CAV_NusCNPJ = y.CAV_NusCNPJ'
      '           and x.CAV_DtiAno_Base = y.CAV_DtiAno_Base'
      
        '           and x.CAV_CdiRetencaoImpostoRenda = y.CAV_CdiRetencao' +
        'ImpostoRenda'
      '           and x.CAV_NusCampoQuebraMM = y.CAV_NusCampoQuebraMM'
      '           and x.CAV_CdiProcTrabDIRF = y.CAV_CdiProcTrabDIRF'
      '           and x.CAV_CdiMes = 12'
      '           and x.CAV_CdiTipoValorAnual = w.TLA_CdiTipoValorAnual'
      '           and x.CAV_CdiProcTrabDIRF > 0'
      '           and x.CAV_CdiTipoValorAnual = 63'
      '           and w.TLA_D1sTipoValorAnual not like '#39'%13%'#39
      '           /*autoemployeefilter=x*/'
      '       ) AS CAV_VlnValor12,'
      ''
      '       0 AS CAV_VlnValor13'
      '  FROM ConAnuaisDIRFValores y'
      
        ' INNER JOIN Contratados ON (y.CAV_CdiContratado = CON_CdiContrat' +
        'ado)'
      ' INNER JOIN Vinculos ON (CON_CdiVinculo = VIN_CdiVinculo)'
      ' INNER JOIN TiposValoresAnuais ON (CAV_CdiTipoValorAnual ='
      '                                  TLA_CdiTipoValorAnual)'
      
        ' INNER JOIN ProcsTrabsDIRF on (DZK_CdiProcTrabDIRF = y.CAV_CdiPr' +
        'ocTrabDIRF)'
      ''
      ' WHERE ((0 = 2016 AND y.CAV_DtiAno_Base ='
      '       (select AVR_DtiAnoBase'
      '                         from DefSisFolhaPagamento'
      '                        where AVR_CdiSistema = 2)) OR'
      '       y.CAV_DtiAno_Base = 2016)'
      '   AND VIN_CdiTipoContratado NOT IN (7, 6)'
      '   AND y.CAV_CosOficialDIRF IN'
      
        '       ('#39'CJAA'#39', '#39'CJAC'#39', '#39'DTPSE'#39', '#39'ESDJ'#39', '#39'ESDP'#39', '#39'ESIR'#39', '#39'ESPA'#39',' +
        ' '#39'ESPO'#39','
      
        '        '#39'ESPP'#39', '#39'ESRT'#39', '#39'RIAP'#39', '#39'RIDAC'#39', '#39'RIIRP'#39', '#39'RIL96'#39', '#39'RIMO' +
        'G'#39', '#39'RIO'#39','
      
        '        '#39'RIP65'#39', '#39'RIPTS'#39', '#39'RTDP'#39', '#39'RTIRF'#39', '#39'RTPA'#39', '#39'RTPO'#39', '#39'RTPP' +
        #39', '#39'RTRT'#39','
      '        '#39'TPSE'#39','#39'DAJUD'#39','#39'QTMESES'#39','#39'RIVC'#39','#39'RIBMR'#39')'
      
        '   and y.CAV_CosOficialDIRF not in ('#39'RIL96'#39', '#39'RIPTS'#39', '#39'RIO'#39', '#39'TP' +
        'SE'#39', '#39'DTPSE'#39')'
      '   and Y.CAV_CdiProcTrabDIRF > 0'
      '   and y.CAV_CdiTipoValorAnual = 63'
      ''
      '       AND Y.CAV_CdiRetencaoImpostoRenda <> 473'
      ''
      '/*autoemployeefilter=y*/'
      '       '
      ' GROUP BY y.CAV_DtiAno_Base,'
      '          y.CAV_NusCNPJ,'
      '          y.CAV_CdiRetencaoImpostoRenda,'
      '          y.CAV_NusCampoQuebraMM,'
      '          DZK_NusProcesso,'
      '          y.CAV_CdiProcTrabDIRF'
      ') tab'
      'where  tab.CAV_VlnValor01 > 0 '
      '    or tab.CAV_VlnValor02 > 0 '
      '    or tab.CAV_VlnValor03 > 0 '
      '    or tab.CAV_VlnValor04 > 0 '
      '    or tab.CAV_VlnValor05 > 0 '
      '    or tab.CAV_VlnValor06 > 0 '
      '    or tab.CAV_VlnValor07 > 0 '
      '    or tab.CAV_VlnValor08 > 0 '
      '    or tab.CAV_VlnValor09 > 0 '
      '    or tab.CAV_VlnValor10 > 0 '
      '    or tab.CAV_VlnValor11 > 0 '
      '    or tab.CAV_VlnValor12 > 0 '
      '    or tab.CAV_VlnValor13 > 0 '
      ''
      'group by '
      '       tab.CAV_NusCNPJ,'
      '       tab.CAV_CdiRetencaoImpostoRenda,'
      '       tab.CAV_NusCampoQuebraMM,'
      '       tab.CAV_CosOficialDIRF,'
      '       tab.Tipo,'
      '       tab.DZK_NusProcesso'
      ''
      'ORDER BY 1, 2, 5, 6, 3, 4 '
      '')
    FontSmoothing = fsmNone
    ExplicitWidth = 416
    ExplicitHeight = 386
  end
  object SynEdit2: TSynEdit
    Left = 422
    Top = 0
    Width = 508
    Height = 395
    Align = alRight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 1
    CodeFolding.GutterShapeSize = 11
    CodeFolding.CollapsedLineColor = clGrayText
    CodeFolding.FolderBarLinesColor = clGrayText
    CodeFolding.IndentGuidesColor = clGray
    CodeFolding.IndentGuides = True
    CodeFolding.ShowCollapsedLine = False
    CodeFolding.ShowHintMark = True
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -13
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Lines.Strings = (
      '')
    FontSmoothing = fsmNone
    ExplicitLeft = 416
    ExplicitHeight = 386
  end
  object Panel1: TPanel
    Left = 0
    Top = 395
    Width = 930
    Height = 42
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 386
    ExplicitWidth = 924
    DesignSize = (
      930
      42)
    object SpeedButton1: TSpeedButton
      Left = 0
      Top = 1
      Width = 217
      Height = 42
      Anchors = [akLeft, akBottom]
      Caption = 'Execute'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 233
      Top = 1
      Width = 217
      Height = 41
      Anchors = [akLeft, akBottom]
      Caption = 'Clear'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton2Click
    end
  end
  object SynSQLSyn1: TSynSQLSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 320
    Top = 184
  end
end
