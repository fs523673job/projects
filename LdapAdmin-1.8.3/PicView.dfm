object ViewPicFrm: TViewPicFrm
  Left = 192
  Top = 107
  ClientHeight = 461
  ClientWidth = 529
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poDefault
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 29
    Width = 529
    Height = 413
    Align = alClient
    AutoSize = True
    ExplicitTop = 23
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 529
    Height = 29
    Images = ImageList1
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object btnSave: TToolButton
      Left = 0
      Top = 0
      Action = ActSave
    end
    object ToolButton2: TToolButton
      Left = 23
      Top = 0
      Width = 8
      ImageIndex = 4
      Style = tbsSeparator
    end
    object btnToFile: TToolButton
      Left = 31
      Top = 0
      Action = ActSaveToFile
    end
    object btnFromFile: TToolButton
      Left = 54
      Top = 0
      Action = ActLoadFromFile
    end
    object ToolButton5: TToolButton
      Left = 77
      Top = 0
      Width = 8
      ImageIndex = 2
      Style = tbsSeparator
    end
    object btnCopy: TToolButton
      Left = 85
      Top = 0
      Action = ActCopy
    end
    object btnPaste: TToolButton
      Left = 108
      Top = 0
      Action = ActPaste
    end
    object btnFitToPicture: TToolButton
      Left = 131
      Top = 0
      Action = ActFitToPicture
      AllowAllUp = True
      Grouped = True
    end
    object ToolButton3: TToolButton
      Left = 154
      Top = 0
      Width = 8
      ImageIndex = 4
      Style = tbsSeparator
    end
    object btnResize: TToolButton
      Left = 162
      Top = 0
      Action = ActResize
      AllowAllUp = True
      Grouped = True
      Style = tbsCheck
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 442
    Width = 529
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object ImageList1: TImageList
    Left = 48
    Top = 192
    Bitmap = {
      494C010107000900180010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003881310022852E00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00078DBE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BD5A2200BB5A
      2400C7B0A400C7B0A400C8B5AC005198540031A946003CE2610021852F00BAB1
      A000BF653200B759260000000000000000000000000000000000350023006500
      4300040404000404040004040400040404000404040004040400650043006500
      430065004300650043000000000000000000078DBE0025A1D10072C7E70085D7
      FA0066CDF90065CDF90065CDF90065CDF90065CDF80065CDF90065CDF80066CE
      F90039ADD800078DBE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A0573300C4612700C461
      2700C7B0A400D3CDCB0057A1630037A94C004FED77004FED77004BE471002487
      3200B8713A00C46127009D5734000000000000000000530138006E014A009B48
      8000B9ABAB00B1659800B1659800B5A7A700B5A7A700B9ABAB00B1659800C377
      AA00C074A700D68ABD006E014A0000000000078DBE004CBCE70039A8D100A0E2
      FB006FD4FA006FD4F9006ED4FA006FD4F9006FD4FA006FD4FA006FD4FA006ED4
      F9003EB1D90084D7EB00078DBE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A1583400CB672C00CB67
      2C00C7B0A40058A161003EAC540063F18B0063F18B0056DD7A0063F18B005DE8
      830025863000BC6A2C009E573400000000000000000077025000D488BB00AD61
      9400BAB1B100AD619400AD619400B1A8A800B1A8A800BAB1B100AD619400C377
      AA00B86C9F00D88CBF007702500000000000078DBE0072D6FA00078DBE00AEEA
      FC0079DCFB0079DCFB0079DCFB0079DCFB0079DCFB007ADCFB0079DCFA0079DC
      FA0044B5D900AEF1F900078DBE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A1593400D26E3000D26E
      3000C7B0A40052A15D0047B15F0077F4A100389B4A003B8C3B0047B05B0077F4
      A1006FEA970029863100935B340000000000000000007B025300D286B900A95D
      9000C0BBBB0095497C0095497C00B7B2B200B7B2B200C0BBBB00A95D9000C377
      AA00B0649700DA8EC1007B02530000000000078DBE0079DDFB001899C7009ADF
      F30092E7FB0084E4FB0083E4FC0083E4FC0084E4FC0083E4FC0083E4FB0084E5
      FC0048B9DA00B3F4F900078DBE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A2593500D6723300D672
      3300C7B0A400F3E7E0005CA8690030913F0074B37E00EFE2D800489A500051B6
      6A008AF8B60080EEAA0029863200506E2F00000000007E025500D589BC00A559
      8C00C8C7C700C3C2C200C3C2C200C3C2C200C3C2C200C8C7C700A5598C00C377
      AA00A85C8F00DD91C4007E02550000000000078DBE0082E3FC0043B7DC0065C3
      E000ACF0FD008DEBFC008DEBFC008DEBFD008DEBFD008DEBFC008DEBFD000C85
      18004CBBDA00B6F7F9006DCAE000078DBE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A35A3500DB763600DB76
      3600DB763600DB763600DB763600C5783500DB763600DB763600D77636004489
      380057B56E009AFAC7005BBD7800258934000000000080025700D98DC000A256
      8900A1558800A1558800A1558800A1558800A1558800A1558800A2568900A256
      8900A2568900E195C8008002570000000000078DBE008AEAFC0077DCF300229C
      C600FDFFFF00C8F7FE00C9F7FE00C9F7FE00C9F7FE00C8F7FE000C8518003CBC
      5D000C851800DEF9FB00D6F6F900078DBE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A35A3500DF7A3900F2EC
      E800FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F8FB
      F80043964C005CB77300A4FCD1005DBC79000000000083025900DC90C300D387
      BA00D387BA00D387BA00D387BA00D387BA00D387BA00D387BA00D387BA00D387
      BA00D387BA00DC90C3008302590000000000078DBE0093F0FE0093F0FD001697
      C500078DBE00078DBE00078DBE00078DBE00078DBE000C85180052D97F0062ED
      970041C465000C851800078DBE00078DBE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A45B3600E4803E00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00E0D8CD00418B390052AB6500258934000000000085025A00E094C700F0DD
      DE00F4F4E400F4F4E400F4F4E400F4F4E400F4F4E400F4F4E400F4F4E400F4F4
      E400F0DDDE00E094C70085025A0000000000078DBE009BF5FE009AF6FE009AF6
      FE009BF5FD009BF6FE009AF6FE009BF5FE000C85180046CE6C0059E4880058E1
      880061EB940040C165000C851800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A45C3700E9874600FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00E5D9D100E98746003C843900278330000000000087025C00E397CA00F6F6
      E900ECECDF00ECECDF00ECECDF00ECECDF00ECECDF00ECECDF00ECECDF00BC8F
      5D00AD713300CB80850087025C0000000000078DBE00FEFEFE00A0FBFF00A0FB
      FE00A0FBFE00A1FAFE00A1FBFE000C8518000C8518000C8518000C85180056E1
      840047CD6E000C8518000C8518000C8518000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A55D3800EE915000F3F3
      F300F3F3F300F3F3F300F3F3F300F3F3F300F3F3F300F3F3F300F3F3F300F3F3
      F300DED3CC00EE915000A059370000000000000000008A035E00E69ACD00F8F8
      EF00F1F1E700F1F1E700F1F1E700F1F1E700F1F1E700F1F1E700F1F1E700B579
      3900FFC12C00B4683400982B3B009F373B0000000000078DBE00FEFEFE00A5FE
      FF00A5FEFF00A5FEFF00078CB60043B7DC0043B7DC0043B7DC000C8518004EDD
      790036BA54000C85180000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A55E3A00F49C5D00E5E5
      E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5
      E500DAD4CF00F49C5D00A05A370000000000000000008C035F00EA9ED100FBFB
      F500F6F6F000F6F6F000F6F6F000F6F6F000D0A36F00C2864300C2864300B97D
      3900DDA13700FFCE5100AF5D2100B76C19000000000000000000078DBE00078D
      BE00078DBE00078DBE00000000000000000000000000000000000C85180040D0
      65000C8518000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A65F3B00F8A66800D3D3
      D300D3D3D300D3D3D300D3D3D300D3D3D300D3D3D300D3D3D300D3D3D300D3D3
      D300CCC7C300F8A66800A05B380000000000000000008D036000ECA0D300FDFD
      FB00FBFBF800FBFBF800FBFBF800FBFBF800E7B15200FFE08800FFD36200FFCF
      5600FFCF5600FFCF5600FFD97400C37923000000000000000000000000000000
      000000000000000000000000000000000000000000000C8518002AB743002DBA
      49000C8518000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A45E3B00F9AC6F00D0CE
      CD00D3D3D300D3D3D300D3D3D300D3D3D300D3D3D300D3D3D300D3D3D300D3D3
      D300CCC7C400F9AC6F009E59370000000000000000008F036100F2A6D900FFFF
      FF00FFFFFE00FFFFFE00FFFFFE00FFFFFE00E4B78100DB9F5800DB9F5800BB7F
      3B00DDAD5B00FFE29000C9773500D0852D000000000000000000000000000000
      000000000000000000000000000000000000000000000C85180021B538000C85
      1800000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009B5635009B56
      35009B5635009B5635009B5635009B5635009B5635009B5635009B5635009B56
      35009B5635009B5635000000000000000000000000009003620090036200AAAA
      8000AAAA8000AAAA8000AAAA8000AAAA8000AAAA8000AAAA8000AAAA8000DA96
      3E00FFE59900D6833D00BC554900D2872E000000000000000000000000000000
      00000000000000000000000000000C8518000C8518000C8518000C8518000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000090036200AAAA
      8000AAAA8000AAAA8000AAAA8000AAAA8000AAAA8000AAAA8000AAAA8000E79C
      3E00E89D3F00E79C3E00B5474E00D2872E000000000000000000000000000000
      0000000000000C8518000C8518000C8518000C85180000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A4787400A4787400A4787400A4787400A4787400A4787400A478
      7400A4787400A47874008C5D5C0000000000000000000000000000669A000066
      9A0000669A00A4787400A4787400A4787400A4787400A4787400A4787400A478
      7400A4787400A47874008C5D5C00000000000000000011771100117711001177
      1100117711001177110011771100117711001177110011771100117711001177
      1100117711001177110011771100000000000000000011771100117711001177
      1100117711001177110011771100117711001177110011771100117711001177
      1100117711001177110011771100000000000000000000000000000000000000
      000000000000A87C7500FEE5CB00FFE2C400FFDFBE00FFDCB800FFD9B100FED6
      AC00FFD4A600FFD1A2008C5D5C00000000000000000000669A003CBEE30036BA
      E10030B6DF00B7818300FCEDDD00FAF2E400F8F1E000F7EEDD00F7EEDB00F7ED
      DB00F7ECDA00F8EDD9008C5D5C0000000000000000001B811B0055BB550055BB
      5500389E380055BB550055BB550055BB550055BB550055BB550055BB5500389E
      380055BB550055BB55001B811B0000000000000000001B811B0055BB55001B81
      1B001B811B0055BB55001B811B0055BB550055BB55001B811B0055BB55001B81
      1B001B811B0055BB55001B811B00000000000000000000000000000000000000
      000000000000AD807800FFEAD400E5A65700E5A65700E5A65700E5A65700E5A6
      5700E5A65700FFD4A8008C5D5C00000000000000000000669A0045C4E6003FC0
      E40038BCE200B7818300F6E4DA00E4A85C00E4A75900E3A75900E3A75900E3A6
      5800E3A75900F0E3D0008C5D5C0000000000000000002B912B0088EE880064CA
      64002B912B002B912B00000000000000000000000000000000002B912B002B91
      2B0064CA640088EE88002B912B0000000000000000002B912B002B912B0064CA
      64002B912B0048AE48002B912B0064CA640064CA64002B912B0048AE48002B91
      2B0064CA64002B912B002B912B00000000000000000000000000000000000000
      000000000000B4867A00FEEEDD00FFEBD600FFE8CF00FFE4C900FEE1C200FEDD
      BB00FFDBB500FFD8AF008C5D5C00000000000000000000669A004DC9E90047C4
      E70041C0E500B9848400F8E8DF00F9E4CE00F9DBBD00F9DBBD00F9DBBD00F8D9
      B800F5DDC200F0E4D2008C5D5C0000000000000000003BA13B005AC05A003BA1
      3B0079DF79003BA13B003BA13B0079DF790079DF79003BA13B003BA13B0079DF
      79003BA13B005AC05A003BA13B0000000000000000003BA13B003BA13B003BA1
      3B0079DF790079DF79003BA13B0079DF790079DF79003BA13B0079DF790079DF
      79003BA13B003BA13B003BA13B0000000000A4787400A4787400A4787400A478
      7400A4787400BA8D7D00FEF2E500E5A65700E5A65700E5A65700E5A65700E5A6
      5700E5A65700FEDCB7008C5D5C00000000000000000000669A0056CDED0050C9
      EA004AC5E800BC878500F9EBE400E4A85C00E4A75800E4A75800E4A75800E4A6
      5700E3A75900F2E6D6008C5D5C0000000000000000004BB14B0088EE88004BB1
      4B004BB14B0088EE88004BB14B0088EE880088EE88004BB14B0088EE88004BB1
      4B004BB14B0088EE88004BB14B0000000000000000004BB14B0088EE88006AD0
      6A0088EE880088EE88004BB14B0088EE880088EE88004BB14B0088EE880088EE
      88006AD06A0088EE88004BB14B0000000000A87C7500FEE5CB00FFE2C400FFDF
      BE00FFDCB800C2958100FEF6EC00FEF3E600FEEFE100FFEDDA00FEE9D400FEE6
      CC00FFE2C600FEDFBF008C5D5C00000000000000000000669A005ED2F00058CF
      ED0052CBEB00C08B8500FAEFE900FAEDDE00FAE5D000F9E5CF00F9E3CD00F8E1
      CA00F5E4D000F3E8DB008C5D5C00000000000000000055BB550088EE880088EE
      880055BB550055BB550055BB550088EE880088EE880055BB550055BB550055BB
      550088EE880088EE880055BB5500000000000000000055BB550055BB550055BB
      550055BB550055BB550055BB5500000000000000000055BB550055BB550055BB
      550055BB550055BB550055BB550000000000AD807800FFEAD400E5A65700E5A6
      5700E5A65700CA9B8300FFF9F300E5A65700E5A65700E5A65700E5A65700E5A6
      5700E5A65700FEE3C8008C5D5C00000000000000000000669A0066D7F30060D4
      F1005AD0EE00C4908600FCF4F000E5A95C00E5A65700E5A65700E4A65700E4A6
      5600E4A75A00F7EEE3008C5D5C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B4867A00FEEEDD00FFEBD600FFE8
      CF00FFE4C900D1A28600FEFBF900FEF9F400FEF7EF00FEF5EA00FEF1E400FEEE
      DE00FEEBD700FEE8D0008C5D5C00000000000000000000669A006DDBF60067D8
      F30062D4F200C8948800FEF7F400FEF7F000FBF0E500FBEFE300FAEDE000FAEE
      E100F9F0E500E8E3DD008C5D5C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BA8D7D00FEF2E500E5A65700E5A6
      5700E5A65700D8A98A00FEFEFD00FEFCFA00FEFAF600FEF8F100FEF5EC00EBDF
      DB00D3C2C000BAA9AA008C5D5C00000000000000000000669A0074DFF8006FDC
      F6006ADAF400CC998900FEF8F500FFFFFF00FEFFFE00FCFAF900FBFBF900B481
      7600B4817600B4817600B4817600000000000000000011771100000000000000
      0000117711001177110011771100000000000000000011771100117711001177
      1100000000000000000011771100000000000000000011771100117711001177
      1100117711001177110011771100000000000000000011771100117711001177
      110011771100117711001177110000000000C2958100FEF6EC00FEF3E600FEEF
      E100FFEDDA00DFB08D00FEFEFE00FEFEFE00FEFCFB00FEFBF700FEF8F200B481
      7600B4817600B4817600B17F7400000000000000000000669A007AE3FA0076E1
      F80070DDF600D09C8900FFFAF800FFFFFF00FFFFFF00FFFFFF00FFFFFF00B481
      7600E0A87000F7935B00FF00FE0000000000000000001B811B0055BB55001B81
      1B001B811B0055BB55001B811B0055BB550055BB55001B811B0055BB55001B81
      1B001B811B0055BB55001B811B0000000000000000001B811B0055BB5500389E
      380055BB550055BB55001B811B0055BB550055BB55001B811B0055BB550055BB
      5500389E380055BB55001B811B0000000000CA9B8300FFF9F300E5A65700E5A6
      5700E5A65700E4B58E00FEFEFE00FEFEFE00FEFEFE00FEFDFC00FEFBF800B481
      7600EBB56F00E49B420000000000000000000000000000669A007FE6FC007BE4
      FA0077E1F900D29F8A00DCA88700DCA88700DCA88700DCA88700DCA88700B481
      7600C7AF890000669A000000000000000000000000002B912B0048AE48002B91
      2B0064CA64002B912B002B912B0064CA640064CA64002B912B002B912B0064CA
      64002B912B0048AE48002B912B0000000000000000002B912B002B912B002B91
      2B0064CA640088EE88002B912B0000000000000000002B912B0088EE880064CA
      64002B912B002B912B002B912B0000000000D1A28600FEFBF900FEF9F400FEF7
      EF00FEF5EA00E8B89000DCA88700DCA88700DCA88700DCA88700DCA88700B481
      7600F0B25E000000000000000000000000000000000000669A0083E8FE0080E6
      FC007DE5FC007DE5FC0078E2FA0072DFF8006BDAF50064D5F2005DD0EF0056CD
      ED0052CAEB0000669A000000000000000000000000003BA13B0079DF790079DF
      79003BA13B003BA13B0079DF790079DF790079DF790079DF79003BA13B003BA1
      3B0079DF790079DF79003BA13B0000000000000000003BA13B003BA13B0079DF
      79003BA13B005AC05A003BA13B0079DF790079DF79003BA13B005AC05A003BA1
      3B0079DF79003BA13B003BA13B0000000000D8A98A00FEFEFD00FEFCFA00FEFA
      F600FEF8F100FEF5EC00EBDFDB00D3C2C000BAA9AA008C5D5C00000000000000
      0000000000000000000000000000000000000000000000669A0084E9FE0084E9
      FE007373730073737300737373007373730073737300737373007373730060D4
      F0005ACFEE0000669A000000000000000000000000004BB14B0088EE880088EE
      88006AD06A0088EE880088EE880088EE880088EE880088EE880088EE88006AD0
      6A0088EE880088EE88004BB14B0000000000000000004BB14B0088EE88004BB1
      4B004BB14B0088EE88004BB14B0088EE880088EE88004BB14B0088EE88004BB1
      4B004BB14B0088EE88004BB14B0000000000DFB08D00FEFEFE00FEFEFE00FEFC
      FB00FEFBF700FEF8F200B4817600B4817600B4817600B17F7400000000000000
      0000000000000000000000000000000000000000000000669A0084E9FE0084E9
      FE0073737300CFC1BC00CFC1BB00CFC1BB00CFC1BB00CEBEB7007373730068D8
      F40062D4F10000669A0000000000000000000000000055BB550055BB550055BB
      550055BB550055BB550055BB550055BB550055BB550055BB550055BB550055BB
      550055BB550055BB550055BB5500000000000000000055BB550055BB550055BB
      550055BB550055BB550055BB550055BB550055BB550055BB550055BB550055BB
      550055BB550055BB550055BB550000000000E4B58E00FEFEFE00FEFEFE00FEFE
      FE00FEFDFC00FEFBF800B4817600EBB56F00E49B420000000000000000000000
      000000000000000000000000000000000000000000000000000000669A000066
      9A0073737300E2D8D300FAF9F800F9F8F700F9F8F700D0C5BF00737373000066
      9A0000669A000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000E8B89000DCA88700DCA88700DCA8
      8700DCA88700DCA88700B4817600F0B25E000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007373730073737300737373007373730073737300000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF00FF3FFFFF80030000C003C00300030000
      8001800100010000800180010001000080018001000100008000800100000000
      8000800100000000800080010000000080008001000100008000800100000000
      800180008003000080018000C3C7000080018000FF87000080018000FF8F0000
      C0038000FE1F0000FFFFC000F87F0000FFFFFFFFF801C00180018001F8018001
      80018001F801800183C18001F801800180018001000180018001800100018001
      8001818100018001FFFFFFFF00018001FFFFFFFF00018001B18D818100018001
      8001800100038003800181810007800380018001003F800380018001003F8003
      80018001007FC007FFFFFFFF00FFF83F00000000000000000000000000000000
      000000000000}
  end
  object ActionList1: TActionList
    Images = ImageList1
    OnUpdate = ActionList1Update
    Left = 48
    Top = 240
    object ActFitToPicture: TAction
      Hint = 'Fit window to image size'
      ImageIndex = 0
      OnExecute = ActFitToPictureExecute
    end
    object ActResize: TAction
      Hint = 'Resize window to image size'
      ImageIndex = 1
      OnExecute = ActResizeExecute
    end
    object ActCopy: TAction
      Hint = 'Copy to clipboard'
      ImageIndex = 2
      OnExecute = ActCopyExecute
    end
    object ActPaste: TAction
      Hint = 'Paste from clipboard'
      ImageIndex = 3
      OnExecute = ActPasteExecute
    end
    object ActSave: TAction
      Hint = 'Save changes and close'
      ImageIndex = 4
      OnExecute = ActSaveExecute
    end
    object ActSaveToFile: TAction
      Hint = 'Save to file'
      ImageIndex = 5
      OnExecute = ActSaveToFileExecute
    end
    object ActLoadFromFile: TAction
      Hint = 'Load from file'
      ImageIndex = 6
      OnExecute = ActLoadFromFileExecute
    end
  end
  object SaveDialog: TSavePictureDialog
    Left = 48
    Top = 296
  end
  object OpenDialog: TOpenPictureDialog
    Left = 48
    Top = 352
  end
end