object AccountCopyDlg: TAccountCopyDlg
  Left = 192
  Top = 114
  BorderStyle = bsDialog
  BorderWidth = 8
  ClientHeight = 98
  ClientWidth = 289
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    289
    98)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 6
    Width = 57
    Height = 13
    Caption = 'New name: '
  end
  object Label2: TLabel
    Left = 0
    Top = 40
    Width = 42
    Height = 13
    Caption = 'Storage:'
  end
  object NameEd: TEdit
    Left = 64
    Top = 4
    Width = 222
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object StorageCbx: TComboBox
    Left = 64
    Top = 36
    Width = 223
    Height = 22
    Style = csOwnerDrawFixed
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnDrawItem = StorageCbxDrawItem
  end
  object OkBtn: TButton
    Left = 131
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 212
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object Images: TImageList
    Left = 16
    Top = 53
    Bitmap = {
      494C010103004000480010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B781
      8300B7818300B7818300B7818300B7818300B7818300B7818300B7818300B781
      8300B7818300B781830000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000F9F99000F1F1
      5B0080800000F9F99000F1F15B0080800000F9F99000F1F15B00808000000000
      000000000000000000000000000000000000000000000000000000000000C7A7
      9C00FEEED400F7E3C500F5DBB400F3D3A200F1CF9A00F0CF9700F0CF9800F0CF
      9800F5D49A00B7818300000000000000000000000000078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00078DBE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000FFFFB500F9F9
      900080800000FFFFB500F9F9900080800000FFFFB500F9F99000808000000000
      000000000000000000000000000000000000000000000000000000000000C7A7
      9E00FDEFD900F6E3CB00F4DBBA00F1D4A900F1D0A200EECC9900EECC9700EECC
      9700F3D19900B78183000000000000000000078DBE0063CBF800078DBE00A3E1
      FB0066CDF90065CDF80065CDF90065CDF90065CDF80065CDF90065CDF80066CD
      F8003AADD800ACE7F500078DBE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000808000008080
      000080800000808000008080000080800000FFFFFF00FFFF0000808000000000
      000080800000000000000000000000000000000000000000000000000000C7A9
      A100FEF3E300F8E7D300F5DFC300F2D7B200F1D4AB00F0D0A300EECC9A00EECC
      9700F3D19900B78183000000000000000000078DBE006AD1F900078DBE00A8E5
      FC006FD4FA006FD4F9006ED4FA006FD4F9006FD4FA006FD4FA006FD4FA006ED4
      F9003EB1D900B1EAF500078DBE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000F9F99000F1F1
      5B0080800000F9F99000F1F15B00808000000000000000000000000000000000
      000080800000000000000000000000000000000000000000000000000000CEB2
      AA00FFFCF400FAEFE400F7E7D300F5DFC200F4DBBB00F1D7B200F1D3AA00F0D0
      A100F3D29B00B78183000000000000000000078DBE0072D6FA00078DBE00AEEA
      FC0079DCFB0079DCFB0079DCFB0079DCFB0079DCFB007ADCFB0079DCFA0079DC
      FA0044B5D900B6EEF600078DBE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000FFFFB500F9F9
      900080800000FFFFB500F9F99000808000000000000080800000808000000000
      000080800000000000000000000000000000000000000000000000000000D3B7
      AF00FFFFFD00FBF4EC00F9EBDA00F5E3C900F5DFC200F4DBBA00F2D7B100F0D4
      A900F5D5A300B78183000000000000000000078DBE0079DDFB00078DBE00B5EE
      FD0083E4FB0084E4FB0083E4FC0083E4FC0084E4FC0083E4FC0083E4FB0084E5
      FC0048B9DA00BBF2F600078DBE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000808000008080
      00000000000080800000FFFFB5008080000000000000FFFF0000808000000000
      000000000000000000000000000000000000000000000000000000000000D7BB
      B200FFFFFF00FEF9F500FAEFE200F8E7D200F5E3CA00F5DEC200F4DBBA00F2D8
      B200F6D9AC00B78183000000000000000000078DBE0082E3FC00078DBE00BAF3
      FD008DEBFC008DEBFC008DEBFC008DEBFD008DEBFD008DEBFC008DEBFD008DEB
      FC004CBBDA00BEF4F700078DBE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000F9F99000F1F1
      5B008080000000000000000000000000000000000000FFFFB500808000000000
      000080800000000000000000000000000000000000000000000000000000DEC1
      B500FFFFFF00FFFFFF00FEF9F400FAEFE200F9EBD900F8E6D100F6E2C800F7E1
      C200F0DAB700B78183000000000000000000078DBE008AEAFC00078DBE00FFFF
      FF00C9F7FE00C8F7FE00C9F7FE00C9F7FE00C9F7FE00C8F7FE00C9F7FE00C8F7
      FE009BD5E700DEF9FB00078DBE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000FFFFB500F9F9
      9000808000000000000080800000808000008080000000000000000000000000
      000080800000000000000000000000000000000000000000000000000000E2C5
      B500FFFFFF00FFFFFF00FFFEFD00FBF3EB00FAEEE200FAEDDC00FCEFD900E6D9
      C400C6BCA900B78183000000000000000000078DBE0093F0FE00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000080808000FFFF
      0000FFFFB50000000000FFFF0000FFFFB5008080000000000000808000000000
      000080808000808000000000000000000000000000000000000000000000E5C7
      B700FFFFFF00FFFFFF00FFFFFF00FDF8F300FDF6EC00F1E1D500C6A19400B594
      8900B08F8100B78183000000000000000000078DBE009BF5FE009AF6FE009AF6
      FE009BF5FD009BF6FE009AF6FE009BF5FE009AF6FD009BF5FE009AF6FE009AF6
      FE000989BA000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      80008080000080800000FFFFB500FFFFB5008080000000000000808000008080
      800080800000FFFF00008080000000000000000000000000000000000000E9CB
      B800FFFFFF00FFFFFF00FFFFFF00FFFEFC00FFFEF900E3CFC900BF8C7600E8B2
      7000ECA54A00C58768000000000000000000078DBE00FEFEFE00A0FBFF00A0FB
      FE00A0FBFE00A1FAFE00A1FBFE00A0FAFE00A1FBFE00A1FBFF00A0FBFF00A1FB
      FF000989BA000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080008080800080808000808080008080800080800000000000000000
      000080808000FFFFB5000000000000000000000000000000000000000000ECCD
      BA00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E4D4D200C89A7F00FAC5
      7700CD93770000000000000000000000000000000000078DBE00FEFEFE00A5FE
      FF00A5FEFF00A5FEFF00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000008080800080800000FFFF0000808000000000
      000000000000808080000000000000000000000000000000000000000000EACA
      B600FCF7F400FCF7F300FBF6F300F9F5F300F9F5F300E1D0CE00C7977C00CF9B
      8600000000000000000000000000000000000000000000000000078DBE00078D
      BE00078DBE00078DBE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000080808000FFFFB500000000000000
      000000000000000000000000000000000000000000000000000000000000E9C6
      B100EBCCB800EBCCB800EBCBB800EACBB800EACCB900DABBB000B8857A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFF0000801FE003FFFF0000
      800FE003800300008007E003000100008003E003000100008003E00300010000
      8003E003000100008003E003000100008003E003000100008003E00300010000
      C001E00300070000E000E00300070000F011E007800F0000FE0BE00FC3FF0000
      FF1FE01FFFFF0000FFBFFFFFFFFF000000000000000000000000000000000000
      000000000000}
  end
end
