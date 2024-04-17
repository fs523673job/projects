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
      'select CdiContratado,'
      '       CdiProcessoLPC,'
      '       CdiVerba,'
      '       sum(Valor_LPC1)  as Valor_LPC1,'
      '       sum(Valor_LPC13) as Valor_LPC13,       '
      '       sum(Quant_LPC1)  as Quant_LPC1,'
      '       sum(Quant_LPC13) as Quant_LPC13,'
      '       sum(Valor_LPC1) - sum(Valor_LPC13) as Valor_Dif,'
      '       sum(Quant_LPC1) - sum(Quant_LPC13) as Quant_Dif,'
      '       VEV_CdiNaturezaVerba,'
      '       VEV_D1sVerba'
      '   from VerbasView'
      '   inner join ('
      '               /* LPC Utiliza'#231#227'o 1 */'
      
        '               select EGP_CdiContratado              as CdiContr' +
        'atado,'
      
        '                      EGP_CdiProcessoLPC             as CdiProce' +
        'ssoLPC,'
      
        '                      EGP_CdiVerba                   as CdiVerba' +
        ','
      
        '                      sum(coalesce(EGP_VlnVerba, 0)) as Valor_LP' +
        'C1,'
      
        '                      sum(coalesce(EGP_QtnVerba, 0)) as Quant_LP' +
        'C1,'
      
        '                      0                              as Valor_LP' +
        'C13,'
      
        '                      0                              as Quant_LP' +
        'C13'
      '                  from ContratadosRCView'
      
        '                  inner join Contratados on (EGP_CdiContratado =' +
        ' CON_CdiContratado)'
      '                  where      ((1or_Contatado_Focado > 0'
      
        '                         and   EGP_CdiContratado = (select USC_C' +
        'diContratado_Focado from UsuariosContratados where USC_CdiUsuari' +
        'o = 0))'
      '                         or   (1or_Contatado_Focado = 0'
      
        '                         and   CON_CdiFolha in (select UFC_CdiFo' +
        'lha from UsuariosFolhas where UFC_CdiUsuario = 0)))'
      
        '                         and ((0       > 0                      ' +
        '   '
      '                         and   EGP_CdiProcessoLPC = 0)'
      '                         or   0        = 0)'
      
        '                 group by EGP_CdiContratado, EGP_CdiProcessoLPC,' +
        ' EGP_CdiVerba'
      '               union'
      '               /* LPC Utiliza'#231#227'o 13 */'
      
        '               select EGP_CdiContratado              as CdiContr' +
        'atado,'
      
        '                      EGP_CdiProcessoLPC             as CdiProce' +
        'ssoLPC,'
      
        '                      EGP_CdiVerba                   as CdiVerba' +
        ','
      
        '                      0                              as Valor_LP' +
        'C1,'
      
        '                      0                              as Quant_LP' +
        'C1,'
      
        '                      sum(coalesce(EGP_VlnVerba, 0)) as Valor_LP' +
        'C13,'
      
        '                      sum(coalesce(EGP_QtnVerba, 0)) as Quant_LP' +
        'C13'
      '                  from ContratadosRCT'
      
        '                  inner join Contratados on (EGP_CdiContratado =' +
        ' CON_CdiContratado)'
      '                  where      ((1or_Contatado_Focado > 0'
      
        '                         and   EGP_CdiContratado = (select USC_C' +
        'diContratado_Focado from UsuariosContratados where USC_CdiUsuari' +
        'o = 0))'
      '                         or   (1or_Contatado_Focado = 0'
      
        '                         and   CON_CdiFolha in (select UFC_CdiFo' +
        'lha from UsuariosFolhas where UFC_CdiUsuario = 0)))'
      
        '                         and ((0       > 0                      ' +
        '   '
      '                         and   EGP_CdiProcessoLPC = 0)'
      '                         or   0        = 0)'
      
        '                 group by EGP_CdiContratado, EGP_CdiProcessoLPC,' +
        ' EGP_CdiVerba'
      
        '                 having    Primeiro(Valor_LPC1) - sum(Valor_LPC1' +
        '3) not between -0.001 and 0.001'
      
        '                        or Primeiro(Quant_LPC1) - sum(Quant_LPC1' +
        '4) not between -0.001 and 0.001   '
      '                 '
      '              ) X on (X.CdiVerba = VEV_CdiVerba)'
      
        '   inner join Contratados on (CdiContratado = CON_CdiContratado)' +
        '   '
      
        '   where CON_CdiSituacao not in (0, 2, 99) /*'#180'Esta cl'#225'usula '#233' in' +
        #243'cua, mas sem ela a consulta apresenta erro */'
      '/*AUTOEMPLOYEEFILTER=Contratados*/'
      
        '   group by CdiContratado, CdiProcessoLPC, CdiVerba, VEV_CdiNatu' +
        'rezaVerba, VEV_D1sVerba             '
      
        '   having    Segundo(Valor_LPC1) - sum(Valor_LPC13) not between ' +
        '-0.001 and 0.001'
      
        '          or Segundo(Quant_LPC1) - sum(Quant_LPC13) not between ' +
        '-0.001 and 0.001   '
      ''
      ''
      ''
      '')
    FontSmoothing = fsmNone
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
