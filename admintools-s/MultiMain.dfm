object frmMultiUser: TfrmMultiUser
  Left = 444
  Top = 186
  Caption = 'Domain users'
  ClientHeight = 495
  ClientWidth = 696
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label6: TLabel
    Left = 10
    Top = 10
    Width = 39
    Height = 13
    Caption = 'Domain:'
  end
  object btnExit: TSpeedButton
    Left = 660
    Top = 430
    Width = 31
    Height = 31
    Hint = 'Quit program'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00330000000000
      03333377777777777F333301BBBBBBBB033333773F3333337F3333011BBBBBBB
      0333337F73F333337F33330111BBBBBB0333337F373F33337F333301110BBBBB
      0333337F337F33337F333301110BBBBB0333337F337F33337F333301110BBBBB
      0333337F337F33337F333301110BBBBB0333337F337F33337F333301110BBBBB
      0333337F337F33337F333301110BBBBB0333337F337FF3337F33330111B0BBBB
      0333337F337733337F333301110BBBBB0333337F337F33337F333301110BBBBB
      0333337F3F7F33337F333301E10BBBBB0333337F7F7F33337F333301EE0BBBBB
      0333337F777FFFFF7F3333000000000003333377777777777333}
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    OnClick = btnExitClick
  end
  object btnInfo: TSpeedButton
    Left = 660
    Top = 25
    Width = 31
    Height = 31
    Hint = 'Info'
    Glyph.Data = {
      06020000424D0602000000000000760000002800000028000000140000000100
      0400000000009001000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333333333333333333333333333333333333333333333
      3333333333330033333333333333333377F33333333333333330FF0333333333
      33333337F37F333333333333330FFFF0333333333333337F3337F33333333333
      330FF078033333333333337F37F87F33333333333337FF003333333333333337
      F377F333333333333330FF733333333333333337F37F33333333333333337FF0
      33333333333333337F37F3333333333333300FF733333333333333377F37F333
      33333333330870FF033333333333337F87F37F33333333333330FFFF03333333
      33333337F3337F333333333333330FF033333333333333337F37F33333333333
      333330033333333333333333377F333333333333333333003333333333333333
      3377F33333333333333330FF033333333333333337F37F3333333333333330FF
      033333333333333337F37F33333333333333330033333333333333333377F333
      3333333333333333333333333333333333333333333333333333333333333333
      33333333333333333333}
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    OnClick = btnInfoClick
  end
  object btnLayout: TSpeedButton
    Left = 625
    Top = 25
    Width = 31
    Height = 31
    Hint = 'Label layout'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      0400000000000001000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
      DDDDDDFFFFFFFFFFFFFDD0000000000000DDD7777777777777FDD07777777777
      70DDD7FDDDD7FDDDD7FDD07FFFF7FFFF70DDD7FDDDD7FDDDD7FDD07F87F7F87F
      70DDD7FDDDD7FDDDD7FDD07FFFF7FFFF70DDD7FFFFF7FFFFF7FDD07777777777
      70DDD7777777777777FDD07FFFF7FFFF70DDD7FDDDD7FDDDD7FDD07F78F7F78F
      70DDD7FDDDD7FDDDD7FDD07FFFF7FFFF70DDD7FFFFF7FFFFF7FDD07777777777
      70DDD7777777777777FDD07FFFF7FFFF70DDD7FDDDD7FDDDD7FDD07F87F7F87F
      70DDD7FDDDD7FDDDD7FDD07FFFF7FFFF70DDD7FDDDD7FDDDD7FDD07777777777
      70DDD7FFFFF7FFFFF7FDD0000000000000DDD7777777777777DD}
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    OnClick = btnLayoutClick
  end
  object btnFont: TSpeedButton
    Left = 590
    Top = 25
    Width = 31
    Height = 31
    Hint = 'Printer font'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333333333333333333333FFF33FFFFF33333300033000
      00333337773377777333333330333300033333337FF33777F333333330733300
      0333333377FFF777F33333333700000073333333777777773333333333033000
      3333333337FF777F333333333307300033333333377F777F3333333333703007
      33333333377F7773333333333330000333333333337777F33333333333300003
      33333333337777F3333333333337007333333333337777333333333333330033
      3333333333377333333333333333033333333333333733333333333333333333
      3333333333333333333333333333333333333333333333333333}
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    OnClick = btnFontClick
  end
  object btnConnect: TBitBtn
    Left = 259
    Top = 28
    Width = 101
    Height = 26
    Hint = 'Connect to domain'
    Caption = '&Connect'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFF0FFFFFFFF
      FFFFFFF00FFFFFFFFFFFFFFF0B0FFFFFFFFFFFFF0BB0FFFFFFFFFFFFF0BB0FFF
      FFFFFFFFF0BBB0FFFFFFFFF0000BBB0FFFFFFFFF0BBBB0FFFFFFFFFFF0BBBB0F
      FFFFFFFFFF0BBBB0FFFFFF00000BBBBB0FFFFFF0BBBBBB0FFFFFFFF0BBBBBBB0
      FFFFFFFF0BBBBBBB0FFFFFFF0BBBBBBBB0FFFFFFF0000000000F}
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    OnClick = btnConnectClick
  end
  object btnMakeUser: TBitBtn
    Left = 235
    Top = 430
    Width = 126
    Height = 31
    Hint = 'Create new domain users'
    Caption = '&Create Accounts'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
      777700000077777777770FFFF077777777770FFFF07777777777000000777777
      77770FFFF077777777770FFFF07777777777000000774700000077777774470E
      FEF077777744440FEFE0777777744700000000000077477777770FFFF0777777
      77770FFFF0777777777700000077777777777777777777777777}
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnClick = btnMakeUserClick
  end
  object gbxUser: TGroupBox
    Left = 10
    Top = 60
    Width = 681
    Height = 361
    Caption = 'New Domain User'
    TabOrder = 0
    object Label1: TLabel
      Left = 10
      Top = 170
      Width = 25
      Height = 13
      Caption = 'User:'
    end
    object Label5: TLabel
      Left = 10
      Top = 20
      Width = 79
      Height = 13
      Caption = 'Name of Course:'
    end
    object Label9: TLabel
      Left = 75
      Top = 190
      Width = 18
      Height = 13
      Caption = '01..'
    end
    object Label2: TLabel
      Left = 230
      Top = 170
      Width = 31
      Height = 13
      Alignment = taRightJustify
      Caption = 'Count:'
    end
    object Label3: TLabel
      Left = 10
      Top = 60
      Width = 62
      Height = 13
      Caption = 'Organization:'
    end
    object edtDNSName: TEdit
      Left = 100
      Top = 185
      Width = 116
      Height = 21
      TabStop = False
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 9
    end
    object edtBaseName: TEdit
      Left = 10
      Top = 185
      Width = 61
      Height = 21
      TabOrder = 4
    end
    object gbxOU: TGroupBox
      Left = 295
      Top = 130
      Width = 376
      Height = 76
      Caption = 'Add'
      TabOrder = 6
      object rbtGlobal: TRadioButton
        Left = 15
        Top = 20
        Width = 156
        Height = 17
        Caption = 'as Global User'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = rbtGlobalClick
      end
      object rbtOU: TRadioButton
        Left = 190
        Top = 20
        Width = 171
        Height = 17
        Caption = 'in Organizational Unit'
        TabOrder = 1
        OnClick = rbtOUClick
      end
      object cbxOU: TComboBox
        Left = 15
        Top = 45
        Width = 346
        Height = 21
        Style = csDropDownList
        DropDownCount = 16
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        Sorted = True
        TabOrder = 2
      end
    end
    object gbxAg: TGroupBox
      Left = 295
      Top = 210
      Width = 376
      Height = 141
      Caption = 'Member of'
      TabOrder = 7
      object lbxAg: TListBox
        Left = 10
        Top = 20
        Width = 356
        Height = 111
        Hint = 'Domain groups'
        ItemHeight = 13
        MultiSelect = True
        ParentShowHint = False
        ShowHint = True
        Sorted = True
        TabOrder = 0
      end
    end
    object lvwUser: TListView
      Left = 10
      Top = 215
      Width = 268
      Height = 136
      Columns = <
        item
          Caption = 'Nr.'
          Width = 30
        end
        item
          Caption = 'Name'
          Width = 120
        end
        item
          Caption = 'Password'
          Width = 80
        end>
      ReadOnly = True
      TabOrder = 10
      ViewStyle = vsReport
    end
    object edtDescription: TComboBox
      Left = 10
      Top = 35
      Width = 268
      Height = 21
      TabOrder = 0
      OnDropDown = edtDescriptionDropDown
      OnSelect = edtDescriptionSelect
    end
    object speCount: TEdit
      Left = 225
      Top = 185
      Width = 36
      Height = 21
      TabOrder = 5
      Text = '1'
    end
    object udCount: TUpDown
      Left = 261
      Top = 185
      Width = 15
      Height = 21
      Associate = speCount
      Min = 1
      Max = 25
      Position = 1
      TabOrder = 11
      OnClick = udCountClick
    end
    object hcbInstitut: THistoryCombo
      Left = 10
      Top = 75
      Width = 268
      Height = 21
      TabOrder = 1
      MaxItems = 15
    end
    object Panel1: TPanel
      Left = 10
      Top = 100
      Width = 276
      Height = 36
      Alignment = taLeftJustify
      BevelOuter = bvNone
      TabOrder = 2
      object rbSummer: TRadioButton
        Left = 0
        Top = 10
        Width = 106
        Height = 17
        Caption = 'Summer Term'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rbWinter: TRadioButton
        Left = 110
        Top = 10
        Width = 91
        Height = 17
        Caption = 'Winter Term'
        TabOrder = 1
      end
      object edtYear: TEdit
        Left = 215
        Top = 7
        Width = 36
        Height = 21
        TabOrder = 2
        Text = '2005'
        OnChange = edtYearChange
      end
      object udYear: TUpDown
        Left = 251
        Top = 7
        Width = 16
        Height = 21
        Associate = edtYear
        Min = 2000
        Max = 2100
        Position = 2005
        TabOrder = 3
        Thousands = False
      end
    end
    object gbxProfile: TGroupBox
      Left = 295
      Top = 15
      Width = 376
      Height = 111
      Caption = 'Server Based Profiles'
      TabOrder = 8
      object Label4: TLabel
        Left = 10
        Top = 20
        Width = 57
        Height = 13
        Caption = 'Profile Path:'
      end
      object Label7: TLabel
        Left = 10
        Top = 65
        Width = 76
        Height = 13
        Caption = 'Home Directory:'
      end
      object Label8: TLabel
        Left = 320
        Top = 65
        Width = 28
        Height = 13
        Alignment = taRightJustify
        Caption = 'Drive:'
      end
      object hcbHomeDir: THistoryCombo
        Left = 10
        Top = 80
        Width = 301
        Height = 21
        TabOrder = 0
        OnExit = hcbHomeDirExit
        MaxItems = 15
      end
      object hcbProfile: THistoryCombo
        Left = 10
        Top = 35
        Width = 356
        Height = 21
        TabOrder = 1
        OnCloseUp = hcbProfileExit
        OnExit = hcbProfileExit
        MaxItems = 15
      end
      object cbHomeDrive: TComboBox
        Left = 320
        Top = 80
        Width = 46
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 2
        OnCloseUp = cbHomeDriveCloseUp
        Items.Strings = (
          ''
          'A:'
          'B:'
          'C:'
          'D:'
          'E:'
          'F:'
          'G:'
          'H:'
          'I:'
          'J:'
          'K:'
          'L:'
          'M:'
          'N:'
          'O:'
          'P:'
          'Q:'
          'R:'
          'S:'
          'T:'
          'U:'
          'V:'
          'W:'
          'X:'
          'Y:'
          'Z:')
      end
    end
    object Panel2: TPanel
      Left = 10
      Top = 135
      Width = 276
      Height = 36
      BevelOuter = bvNone
      TabOrder = 3
      object rbNoExpire: TRadioButton
        Left = 0
        Top = 10
        Width = 106
        Height = 17
        Caption = 'no Expiration'
        TabOrder = 0
        OnClick = rbNoExpireClick
      end
      object rbExpire: TRadioButton
        Left = 110
        Top = 10
        Width = 71
        Height = 17
        Caption = 'valid until'
        Checked = True
        TabOrder = 1
        TabStop = True
        OnClick = rbExpireClick
      end
      object dtpExpire: TDateTimePicker
        Left = 187
        Top = 7
        Width = 81
        Height = 21
        Date = 38513.362531851850000000
        Time = 38513.362531851850000000
        TabOrder = 2
      end
    end
  end
  object btnNew: TBitBtn
    Left = 10
    Top = 430
    Width = 71
    Height = 31
    Hint = 'Clear all fields'
    Caption = '&New'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0033333333B333
      333B33FF33337F3333F73BB3777BB7777BB3377FFFF77FFFF77333B000000000
      0B3333777777777777333330FFFFFFFF07333337F33333337F333330FFFFFFFF
      07333337F33333337F333330FFFFFFFF07333337F33333337F333330FFFFFFFF
      07333FF7F33333337FFFBBB0FFFFFFFF0BB37777F3333333777F3BB0FFFFFFFF
      0BBB3777F3333FFF77773330FFFF000003333337F333777773333330FFFF0FF0
      33333337F3337F37F3333330FFFF0F0B33333337F3337F77FF333330FFFF003B
      B3333337FFFF77377FF333B000000333BB33337777777F3377FF3BB3333BB333
      3BB33773333773333773B333333B3333333B7333333733333337}
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = btnNewClick
  end
  object hcbDomain: THistoryCombo
    Left = 10
    Top = 30
    Width = 236
    Height = 21
    TabOrder = 4
    AutoUpdate = False
    MaxItems = 15
  end
  object btnGenPwd: TBitBtn
    Left = 85
    Top = 430
    Width = 136
    Height = 31
    Hint = 'Create new passwords'
    Caption = 'New &Passwords'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
      5000555555555555577755555555555550B0555555555555F7F7555555555550
      00B05555555555577757555555555550B3B05555555555F7F557555555555000
      3B0555555555577755755555555500B3B0555555555577555755555555550B3B
      055555FFFF5F7F5575555700050003B05555577775777557555570BBB00B3B05
      555577555775557555550BBBBBB3B05555557F555555575555550BBBBBBB0555
      55557F55FF557F5555550BB003BB075555557F577F5575F5555577B003BBB055
      555575F7755557F5555550BB33BBB0555555575F555557F555555507BBBB0755
      55555575FFFF7755555555570000755555555557777775555555}
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = btnGenPwdClick
  end
  object stbStatus: TStatusBar
    Left = 0
    Top = 469
    Width = 696
    Height = 26
    Panels = <
      item
        Width = 500
      end
      item
        Width = 50
      end>
  end
  object btnPrint: TBitBtn
    Left = 370
    Top = 430
    Width = 126
    Height = 31
    Hint = 'Print account infos'
    Caption = 'Print &Labels'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00300000000000
      00033FFFFFFFFFFFFFFF0888888888888880777777777777777F088888888888
      8880777777777777777F0000000000000000FFFFFFFFFFFFFFFF0F8F8F8F8F8F
      8F80777777777777777F08F8F8F8F8F8F9F0777777777777777F0F8F8F8F8F8F
      8F807777777777777F7F0000000000000000777777777777777F3330FFFFFFFF
      03333337F3FFFF3F7F333330F0000F0F03333337F77773737F333330FFFFFFFF
      03333337F3FF3FFF7F333330F00F000003333337F773777773333330FFFF0FF0
      33333337F3FF7F3733333330F08F0F0333333337F7737F7333333330FFFF0033
      33333337FFFF7733333333300000033333333337777773333333}
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    OnClick = btnPrintClick
  end
  object btnDelCourse: TBitBtn
    Left = 540
    Top = 430
    Width = 111
    Height = 31
    Caption = '&Delete course'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      0400000000000001000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333FF3333333333333300033333333333337773333333333333000333333333
      333377733333FFFFFF333333333339999993F3333333F7777773003333333999
      9993773333333777777300333333333333337733333333333333333333333333
      3333F333333333F3333300333333333333337733333333F73333003333333339
      33337733333333F773333333333333399333F333FFFFFFF77F33993399999999
      9993773377777777777399339999999999937733777777777773333333333339
      9333FF33333333F7733300033333333933337773333333373333000333333333
      3333777333333333333333333333333333333333333333333333}
    NumGlyphs = 2
    TabOrder = 8
    OnClick = btnDelCourseClick
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 595
    Top = 70
  end
  object PrintDialog: TPrintDialog
    Left = 630
    Top = 70
  end
end
