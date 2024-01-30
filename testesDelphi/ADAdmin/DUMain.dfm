object frmDomUser: TfrmDomUser
  Left = 423
  Top = 185
  Caption = 'New Domain User'
  ClientHeight = 499
  ClientWidth = 764
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
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
  object btnInfo: TSpeedButton
    Left = 715
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
  object btnPref: TSpeedButton
    Left = 680
    Top = 25
    Width = 31
    Height = 31
    Hint = 'Preferences'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      0400000000000001000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
      DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDFFFDDDDDDDDDDDD444DDDDDDDD
      DDDDD777DFDDDDDDDDDDD4FF4DDDDDDDDDDDD7FD7DFDDDDDDDDDD4FFF4DDDDDD
      DDDDD7DFD7DFDDDDDDDDDD4FFF4DDDDDDDDDDD7DFD7DFDDDDDDDDDD4FFF4DDDD
      DDDDDDD7DFD7DFDDDDDDDDDD4FFF4DDDDDDDDDDD7DFD7DFFFFDDDDDDD4FFF444
      CDDDDDDDD7DFD7777DFDDDDDDD4FFFFF4CDDDDDDDD7DFDDF77FDDDDDDDD4FF44
      F4DDDDDDDDD7FD77D7FDDDDDDDD4F4DD44DDDDDDDDD7F7FD77FDDDDDDDD4F4DD
      C4DDDDDDDDD7F7DF77DDDDDDDDDC4F4CDDDDDDDDDDD77F77FDDDDDDDDDDDC444
      DDDDDDDDDDDD7777DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD}
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    OnClick = btnPrefClick
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
    TabOrder = 0
    OnClick = btnConnectClick
  end
  object gbxUser: TGroupBox
    Left = 5
    Top = 60
    Width = 756
    Height = 436
    Caption = 'New Domain User'
    TabOrder = 1
    object Label1: TLabel
      Left = 10
      Top = 30
      Width = 74
      Height = 13
      Caption = 'Account Name:'
    end
    object Label2: TLabel
      Left = 10
      Top = 65
      Width = 53
      Height = 13
      Caption = 'First Name:'
    end
    object Label3: TLabel
      Left = 10
      Top = 95
      Width = 54
      Height = 13
      Caption = 'Last Name:'
    end
    object Label4: TLabel
      Left = 10
      Top = 125
      Width = 50
      Height = 13
      Caption = 'Full Name:'
    end
    object Label5: TLabel
      Left = 10
      Top = 175
      Width = 56
      Height = 13
      Caption = 'Description:'
    end
    object Label7: TLabel
      Left = 15
      Top = 240
      Width = 48
      Height = 13
      Caption = 'Room Nr.:'
    end
    object Label8: TLabel
      Left = 15
      Top = 280
      Width = 34
      Height = 13
      Caption = 'Phone:'
    end
    object btnExit: TSpeedButton
      Left = 710
      Top = 395
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
    object edtDNSName: TEdit
      Left = 190
      Top = 25
      Width = 156
      Height = 21
      ReadOnly = True
      TabOrder = 0
    end
    object edtKontoName: TEdit
      Left = 100
      Top = 25
      Width = 81
      Height = 21
      TabOrder = 1
    end
    object edtFirstName: TEdit
      Left = 100
      Top = 60
      Width = 246
      Height = 21
      TabOrder = 2
      OnChange = edtNameChange
    end
    object edtLastName: TEdit
      Left = 100
      Top = 90
      Width = 246
      Height = 21
      TabOrder = 3
      OnChange = edtNameChange
    end
    object edtFullName: TEdit
      Left = 10
      Top = 145
      Width = 336
      Height = 21
      TabOrder = 4
    end
    object edtDescription: TEdit
      Left = 10
      Top = 195
      Width = 336
      Height = 21
      TabOrder = 5
    end
    object edtRoom: TEdit
      Left = 75
      Top = 235
      Width = 76
      Height = 21
      TabOrder = 6
    end
    object edtPhone: TEdit
      Left = 75
      Top = 275
      Width = 76
      Height = 21
      TabOrder = 7
    end
    object chbDisabled: TCheckBox
      Left = 220
      Top = 240
      Width = 106
      Height = 17
      Caption = 'Account disabled'
      Checked = True
      State = cbChecked
      TabOrder = 8
      OnClick = chbDisabledClick
    end
    object gbxOU: TGroupBox
      Left = 365
      Top = 20
      Width = 376
      Height = 96
      Caption = 'Add'
      TabOrder = 9
      object rbtGlobal: TRadioButton
        Left = 15
        Top = 25
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
        Top = 25
        Width = 171
        Height = 17
        Caption = 'in Organizational Unit'
        TabOrder = 1
        OnClick = rbtOUClick
      end
      object cbxOU: TComboBox
        Left = 15
        Top = 55
        Width = 346
        Height = 21
        Style = csDropDownList
        DropDownCount = 16
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        Sorted = True
        TabOrder = 2
        OnChange = cbxOUChange
      end
    end
    object gbxAg: TGroupBox
      Left = 365
      Top = 125
      Width = 376
      Height = 261
      Caption = 'Member of'
      TabOrder = 10
      object lbxAg: TListBox
        Left = 10
        Top = 20
        Width = 356
        Height = 231
        Hint = 'Domain groups'
        ItemHeight = 13
        MultiSelect = True
        ParentShowHint = False
        ShowHint = True
        Sorted = True
        TabOrder = 0
      end
    end
    object btnPwd: TBitBtn
      Left = 215
      Top = 270
      Width = 131
      Height = 31
      Caption = 'Password'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      NumGlyphs = 2
      ParentFont = False
      Spacing = 10
      TabOrder = 11
      OnClick = btnPwdClick
    end
    object gbxProfile: TGroupBox
      Left = 10
      Top = 315
      Width = 346
      Height = 111
      Caption = 'Server Based Profiles'
      TabOrder = 12
      object Label9: TLabel
        Left = 10
        Top = 20
        Width = 57
        Height = 13
        Caption = 'Profile Path:'
      end
      object Label10: TLabel
        Left = 10
        Top = 65
        Width = 76
        Height = 13
        Caption = 'Home Directory:'
      end
      object Label11: TLabel
        Left = 290
        Top = 65
        Width = 28
        Height = 13
        Alignment = taRightJustify
        Caption = 'Drive:'
      end
      object cbHomeDrive: TComboBox
        Left = 290
        Top = 80
        Width = 46
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 0
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
    object btnMakeUser: TBitBtn
      Left = 565
      Top = 395
      Width = 141
      Height = 31
      Hint = 'Create new domain user account'
      Caption = 'Create &Account'
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
      TabOrder = 13
      OnClick = btnMakeUserClick
    end
    object btnNew: TBitBtn
      Left = 365
      Top = 395
      Width = 141
      Height = 31
      Hint = 'Clear all fields'
      Caption = '&Clear Fields'
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
      TabOrder = 14
      OnClick = btnNewClick
    end
  end
end
