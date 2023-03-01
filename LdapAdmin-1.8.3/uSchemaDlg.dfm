object SchemaDlg: TSchemaDlg
  Left = 325
  Top = 294
  Caption = 'Schema Viewer'
  ClientHeight = 508
  ClientWidth = 741
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 208
    Top = 53
    Height = 436
    ExplicitHeight = 447
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 489
    Width = 741
    Height = 19
    Panels = <
      item
        Width = 50
      end
      item
        Width = 130
      end
      item
        Width = 50
      end>
  end
  object Tabs: TTabControl
    Left = 0
    Top = 30
    Width = 741
    Height = 23
    Align = alTop
    DockSite = True
    TabOrder = 0
    Tabs.Strings = (
      'Tabs')
    TabIndex = 0
    OnChange = TabsChange
    OnMouseDown = TabsMouseDown
    OnMouseMove = TabsMouseMove
    OnMouseUp = TabsMouseUp
  end
  object Tree: TTreeView
    Left = 0
    Top = 53
    Width = 208
    Height = 436
    Align = alLeft
    Ctl3D = True
    HideSelection = False
    Images = ImageList1
    Indent = 19
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 2
    OnChange = TreeChange
  end
  object View: TTreeView
    Left = 211
    Top = 53
    Width = 530
    Height = 436
    Align = alClient
    Ctl3D = True
    Indent = 19
    ParentCtl3D = False
    PopupMenu = PopupMenu
    ReadOnly = True
    RightClickSelect = True
    TabOrder = 3
    OnAdvancedCustomDrawItem = ViewAdvancedCustomDrawItem
    OnContextPopup = ViewContextPopup
    OnMouseDown = ViewMouseDown
    OnMouseMove = ViewMouseMove
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 741
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 4
    object UndoBtn: TSpeedButton
      Left = 3
      Top = 2
      Width = 27
      Height = 26
      Hint = 'Back'
      Enabled = False
      Flat = True
      Glyph.Data = {
        36060000424D3606000000000000360000002800000020000000100000000100
        18000000000000060000C30E0000C30E00000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FF83A184247B24066F09066F090066001A5E1F666666FF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB5B5B58C8C8C7B7B7B7B
        7B7B7B7B7B7B7B7B999999FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FF247B2416991615B73509B21C09B21C09B21C09B21C009900066F0983A1
        84FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF9393938C8C8C99999999999993
        93939393939393938585857B7B7BB5B5B5FF00FFFF00FFFF00FFFF00FFFF00FF
        187B1922BD5222BD5215B73515B7352FD24E15B73509B21C09B21C09B21C0066
        0083A184FF00FFFF00FFFF00FFFF00FF8C8C8C999999999999999999999999AD
        ADAD9999999393939393939393937B7B7BB5B5B5FF00FFFF00FFFF00FF36993E
        22BD5222BD5222BD5222BD5276D683FFFFFFA4E4BE09B21C09B21C09B21C09B2
        1C066F09FF00FFFF00FFFF00FF999999A5A5A5A5A5A5A5A5A5A5A5A5BDBDBDFF
        FFFFDEDEDE9999999999999999999393937B7B7BFF00FFFF00FFFF00FF1F9740
        33CC6633CC6622BD5277D798FFFFFFFFFFFF98E0AD15B73515B73509B21C09B2
        1C169916408F50FF00FFFF00FF939393A5A5A5A5A5A5A5A5A5C5C5C5FFFFFFFF
        FFFFD6D6D69999999999999999999999998C8C8C999999FF00FF59B95F22BD52
        33CC6633CC6677D798FFFFFFFFFFFFA4E4BE22BD5222BD5215B73515B73515B7
        3509B21C206F20FF00FFADADADA5A5A5A5A5A5A5A5A5C5C5C5FFFFFFFFFFFFDE
        DEDEA5A5A5999999999999999999999999999999858585FF00FF36993E33CC66
        33CC6677D798FFFFFFFFFFFFF7F7F798E0AD98E0AD98E0AD98E0AD85E49815B7
        3515B73506780BFF00FFA5A5A5A5A5A5A5A5A5C5C5C5FFFFFFFFFFFFF7F7F7CC
        CCCCCCCCCCCCCCCCCCCCCCCCCCCC999999999999858585FF00FF33B73633CC66
        50D77FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF22BD
        5215B735169916FF00FFA5A5A5A5A5A5B5B5B5FFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF9999999999998C8C8CFF00FF33B73633CC66
        33CC6677D798FFFFFFFFFFFFFFFFFFA4E4BE98E0AD98E0AD98E0AD98E0AD22BD
        5222BD5206780BFF00FFA5A5A5A5A5A5A5A5A5C5C5C5FFFFFFFFFFFFFFFFFFD6
        D6D6D6D6D6D6D6D6D6D6D6D6D6D6A5A5A5A5A5A5858585FF00FF65C06B3FCF6F
        3FCF6F33CC6677D798FFFFFFFFFFFFD8F4E33FCF6F33CC6633CC6633CC6622BD
        5222BD52208F30FF00FFB5B5B5ADADADADADADA5A5A5C5C5C5FFFFFFFFFFFFEF
        EFEFADADADA5A5A5A5A5A5A5A5A5A5A5A5A5A5A5939393FF00FFFF00FF33B736
        77D79850D77F33CC6677D798FFFFFFFFFFFFA4E4BE33CC6633CC6633CC6633CC
        661F974080B480FF00FFFF00FFA5A5A5C5C5C5B5B5B5ADADADC5C5C5FFFFFFFF
        FFFFD6D6D6A5A5A5A5A5A5A5A5A5A5A5A5999999B5B5B5FF00FFFF00FF5CC65C
        76D68385E4985FD8803FCF6F85E498F7F7F798E0AD33CC6633CC6633CC6633CC
        66229322FF00FFFF00FFFF00FFB5B5B5C5C5C5CCCCCCBDBDBDADADADCCCCCCF7
        F7F7CCCCCCA5A5A5A5A5A5A5A5A5A5A5A5939393FF00FFFF00FFFF00FFFF00FF
        33B73676D68398E0AD77D7985FD8803FCF6F33CC6633CC6633CC6622BD521699
        16FF00FFFF00FFFF00FFFF00FFFF00FFA5A5A5C5C5C5D6D6D6C5C5C5BDBDBDAD
        ADADA5A5A5A5A5A5A5A5A5A5A5A5939393FF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FF5CC65C49C45785E49885E49877D79850D77F33CC6615B7353EB63EFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB5B5B5B5B5B5CCCCCCCCCCCCBD
        BDBDB5B5B5A5A5A5999999A5A5A5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FF65C06B33B73633B73633B73659B95FFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFBDBDBDA5A5A5A5
        A5A5A5A5A5B5B5B5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = UndoBtnClick
    end
    object RedoBtn: TSpeedButton
      Left = 32
      Top = 2
      Width = 27
      Height = 26
      Hint = 'Forward'
      Enabled = False
      Flat = True
      Glyph.Data = {
        36060000424D3606000000000000360000002800000020000000100000000100
        18000000000000060000C30E0000C30E00000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FF83A184247B24066F09066F090066001A5E1F666666FF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB5B5B58C8C8C7B7B7B7B
        7B7B7B7B7B7B7B7B999999FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FF247B2416991615B73509B21C09B21C09B21C09B21C009900066F0983A1
        84FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF9393938C8C8C99999999999993
        93939393939393938585857B7B7BB5B5B5FF00FFFF00FFFF00FFFF00FFFF00FF
        187B1922BD5222BD5215B73565C06B76D68309B21C09B21C09B21C09B21C0066
        0083A184FF00FFFF00FFFF00FFFF00FF8C8C8C999999999999999999BDBDBDC5
        C5C59999999393939393939393937B7B7BB5B5B5FF00FFFF00FFFF00FF36993E
        22BD5222BD5222BD5222BD52D8F4E3FFFFFFA4E4BE09B21C09B21C09B21C09B2
        1C066F09FF00FFFF00FFFF00FF999999A5A5A5A5A5A5A5A5A5A5A5A5EFEFEFFF
        FFFFD6D6D69999999999999999999393937B7B7BFF00FFFF00FFFF00FF1F9740
        33CC6633CC6622BD5222BD5277D798FFFFFFFFFFFFA4E4BE15B73509B21C09B2
        1C169916408F50FF00FFFF00FF939393A5A5A5A5A5A5A5A5A5A5A5A5C5C5C5FF
        FFFFFFFFFFD6D6D69999999999999999998C8C8C999999FF00FF59B95F22BD52
        33CC6633CC6622BD5222BD5222BD5277D798FFFFFFFFFFFFA4E4BE15B73515B7
        3509B21C206F20FF00FFADADADA5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5C5
        C5C5FFFFFFFFFFFFD6D6D6999999999999999999858585FF00FF36993E33CC66
        5FD880FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA4E4BE15B7
        3515B73506780BFF00FFA5A5A5A5A5A5BDBDBDFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFD6D6D6999999999999858585FF00FF33B73633CC66
        5FD880FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD8F4E322BD
        5215B735169916FF00FFA5A5A5A5A5A5BDBDBDFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFEFEFEF9999999999998C8C8CFF00FF33B73633CC66
        33CC6633CC6633CC6633CC6633CC6698E0ADFFFFFFFFFFFFD8F4E333CC6622BD
        5222BD5206780BFF00FFA5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5CC
        CCCCFFFFFFFFFFFFEFEFEFA5A5A5A5A5A5A5A5A5858585FF00FF65C06B3FCF6F
        3FCF6F33CC6633CC6633CC6650D77FF7F7F7FFFFFFD8F4E333CC6633CC6622BD
        5222BD52208F30FF00FFB5B5B5ADADADADADADA5A5A5A5A5A5A5A5A5B5B5B5F7
        F7F7FFFFFFEFEFEFA5A5A5A5A5A5A5A5A5A5A5A5939393FF00FFFF00FF33B736
        77D79850D77F33CC6633CC66BFD9BFFFFFFFD8F4E333CC6633CC6633CC6633CC
        661F974080B480FF00FFFF00FFA5A5A5C5C5C5B5B5B5ADADADA5A5A5E6E6E6FF
        FFFFEFEFEFA5A5A5A5A5A5A5A5A5A5A5A5999999B5B5B5FF00FFFF00FF5CC65C
        76D68385E4985FD88033CC6633CC6698E0AD33CC6633CC6633CC6633CC6633CC
        66229322FF00FFFF00FFFF00FFB5B5B5C5C5C5CCCCCCBDBDBDA5A5A5A5A5A5D6
        D6D6A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5939393FF00FFFF00FFFF00FFFF00FF
        33B73676D68398E0AD77D7985FD8803FCF6F33CC6633CC6633CC6622BD521699
        16FF00FFFF00FFFF00FFFF00FFFF00FFA5A5A5C5C5C5D6D6D6C5C5C5BDBDBDAD
        ADADA5A5A5A5A5A5A5A5A5A5A5A5939393FF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FF5CC65C49C45785E49885E49877D79850D77F33CC6615B7353EB63EFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB5B5B5B5B5B5CCCCCCCCCCCCBD
        BDBDB5B5B5A5A5A5999999A5A5A5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FF65C06B33B73633B73633B73659B95FFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFBDBDBDA5A5A5A5
        A5A5A5A5A5B5B5B5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = RedoBtnClick
    end
    object Label1: TLabel
      Left = 55
      Top = 7
      Width = 55
      Height = 13
      Alignment = taRightJustify
      Caption = '     Search: '
    end
    object btnSave: TSpeedButton
      Left = 552
      Top = 2
      Width = 27
      Height = 26
      Hint = 'Export Schema'
      Flat = True
      Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        20000000000000040000120B0000120B00000000000000000000FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BA6833C5C38458FFD38B68FFE18F
        70FFDC8D6CFFDA8B6DFFD78A6EFFCD8B6CFFAB6D44FFA65F2EFFFFFFFF00FFFF
        FF00FFFFFF00FFFFFF009999990D95959531C68355FFEFCEBAFFDDFFFFFF87EE
        C7FFA2F4D7FFA2F6D7FF8CEEC7FFE0FFFFFFDDA285FFAB6A3EFFFFFFFF00A3A3
        A321A1A1A19D9F9F9FF0A1A1A1FFABABABFFC37F51FFEFB69AFFEAF3E8FF51BF
        84FF6FC998FF71C999FF54BF84FFE4F4E9FFDD9C7BFFAA693AFFA7A7A74AA5A5
        A5F4CECECEFFEDEDEDFFF4F4F4FFF5F5F5FFC48154FFEAB697FFF3F3EAFFEDF1
        E6FFEFF1E6FFEFF0E6FFEDF1E5FFF3F5EDFFD59C79FFB07044FFA9A9A9EADEDE
        DEFFF3F3F3FFDBDBDBFFD2D2D2FFDBDBDBFFC98B61FFE6B592FFE2A781FFE1A7
        81FFDEA37DFFDCA17BFFDB9F79FFD99E77FFD49A73FFBB7E57FFACACACEAF0F0
        F0FFDEDEDEFFD4D4D4FFD2D2D2FFDBDBDBFFCA8D65FFEAB899FFDDA57EFFDDA6
        80FFDBA37CFFD9A07AFFD9A079FFD89F78FFD89E78FFBF845DFFAEAEAEEAF2F2
        F2FFE2E2E2FFD8D8D8FFD5D5D5FFDCDCDCFFC8885DFFEFBFA1FFFDFCFAFFFEFC
        FBFFFEFDFDFFFEFDFCFFFDFBFAFFFDFCFBFFDDA885FFC17F53FFB0B0B0EAF3F3
        F3FFE7E7E7FFDDDDDDFFD9D9D9FFE0E0E0FFC7865BFFEFC09EFFFFFFFFFFCC93
        6EFFFFFFFFFFFFFFFFFFFFFBF7FFFFF8F1FFE4AF8CFFC78A61FFB2B2B2EAF4F4
        F4FFEAEAEAFFE1E1E1FFDDDDDDFFE3E3E3FFCC8D65FFF3CDB0FFFFFFFFFFE3C7
        B3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEABFA1FFC98960FFB4B4B4EAF5F5
        F5FFEEEEEEFFE6E6E6FFE2E2E2FFE6E6E6FFD4976EFFD49E7BFFD09871FFD6A4
        82FFCD8E68FFCD9069FFD09A75FFD19973FFC88B62FFAD5A2036B5B5B5EAF6F6
        F6FFEBEBEBFFDEDEDEFFD6D6D6FFD5D5D5FFD1D1D1FFC3C3C3FFBCBCBCFFC0C0
        C0FFE5E5E5FFA3A3A3EAFFFFFF00FFFFFF00FFFFFF00FFFFFF00B7B7B7EAF7F7
        F7FFE7E7E7FFEFEFEFFFF6F6F6FFFBFBFBFFFAFAFAFFF0F0F0FFDEDEDEFFC3C3
        C3FFE6E6E6FFA5A5A5EAFFFFFF00FFFFFF00FFFFFF00FFFFFF00B9B9B9EAF8F8
        F8FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFB
        FBFFEAEAEAFFA7A7A7EAFFFFFF00FFFFFF00FFFFFF00FFFFFF00BABABABFE1E1
        E1FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFB
        FBFFCFCFCFFFA9A9A9A1FFFFFF00FFFFFF00FFFFFF00FFFFFF00BCBCBC2ABBBB
        BBD1D0D0D0FFE8E8E8FFF3F3F3FFFDFDFDFFFCFCFCFFEDEDEDFFE0E0E0FFC2C2
        C2FFADADADC3ACACAC1BFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BCBC
        BC07BBBBBB55BABABAAEB8B8B8D6B7B7B7FBB6B6B6F9B4B4B4CDB3B3B3A9B1B1
        B146AFAFAF03FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00}
      ParentShowHint = False
      ShowHint = True
      OnClick = btnSaveClick
    end
    object Bevel2: TBevel
      Left = 550
      Top = 2
      Width = 2
      Height = 25
      Shape = bsLeftLine
    end
    object btnClose: TSpeedButton
      Left = 584
      Top = 2
      Width = 27
      Height = 26
      Hint = 'Close'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFF00FFFF00FFFF00FF8596DC1031B51737B81B3AB81B3AB7193ABB18
        3AB91438BD0E36C00B33C00830BE032DC00127B68090D1FF00FFFFFFFF1239D4
        2045D92B4EDA3052DA2F52DB2D52DB2A52DC2550DF1D4CE01648E00E43E0063B
        E00233D70127B5FFFFFFFFFFFF1A42DE2D51E1385AE34565E57F94EDE2E8FBFF
        FFFFFFFFFFDCE4FB7292F1144CE90B44E8053AE0032CBEFFFFFFFFFFFF2349DF
        395BE34464E4A2B2F2FFFFFFBBC8F6738FEE708FEFBACAF8FFFFFF98B1F60F49
        E9093FE1062FC1FFFFFFFFFFFF2D52E14362E48B9EEEFFFFFF8398EE476AE641
        67E73864E82D5EE97394F1FFFFFF6C8DF01044E10C34C1FFFFFFFFFFFF3659E2
        4C69E5EBEFFCBBC6F54F6EE64A6BE6FFFFFFFFFFFF2E5DE82557E8B6C7F8DBE3
        FB1949E01339C2FFFFFFFFFFFF4060E45470E7FFFFFF8195ED516EE64969E5FF
        FFFFFFFFFF2D59E62453E66687EEFFFFFF204DDF193DC0FFFFFFFFFFFF4665E5
        5B76E8FFFFFF8195ED516DE64968E5FFFFFFFFFFFF2D56E42551E46583ECFFFF
        FF264FDE1E40BFFFFFFFFFFFFF506DE6647EE8EFF1FDB7C2F5526DE64966E4FF
        FFFFFFFFFF2D53E2274FE2B0BFF5E0E6FB2B51DC2242BFFFFFFFFFFFFF5470E7
        6D85EA97A9F1FFFFFF8193ED4D68E54362E43B5CE33155E26D86EBFFFFFF738A
        EC2E52DC2443BEFFFFFFFFFFFF5F7AE87B91EC7189EBA6B5F2FFFFFFB1BDF471
        86EA6C83E9B0BDF4FFFFFF95A8F03154E13053DB2443BDFFFFFFFFFFFF6C85EA
        8DA1EF8197ED7088EB97A8F0EEF0FCFFFFFFFFFFFFE9EDFC899DEE4263E43B5D
        E33154DC1F3FBCFFFFFFFFFFFF788EEC9DAEF18CA0EF7A90EC7189EB6B83E966
        7FE9637DE95E79E85774E74F6DE64263E43053DB1A3ABAFFFFFFFF00FFB2BFF4
        778DEC6881EA5C77E85571E7506DE64B6AE64C6AE54766E54061E33C5EE33255
        E22448D88A9BDEFF00FFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FF}
      ParentShowHint = False
      ShowHint = True
      OnClick = btnCloseClick
    end
    object Bevel3: TBevel
      Left = 582
      Top = 2
      Width = 2
      Height = 25
      Shape = bsLeftLine
    end
    object SearchEdit: TEdit
      Left = 112
      Top = 4
      Width = 321
      Height = 21
      TabOrder = 0
      OnKeyDown = SearchEditKeyDown
    end
    object WholeWordsCbx: TCheckBox
      Left = 442
      Top = 6
      Width = 105
      Height = 17
      Caption = 'Whole words only'
      TabOrder = 1
      OnKeyDown = SearchEditKeyDown
    end
  end
  object ImageList1: TImageList
    Left = 40
    Top = 424
    Bitmap = {
      494C0101070009000C0010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B7818300B781
      8300B7818300B7818300B7818300B7818300B7818300B7818300B7818300B781
      8300B7818300B7818300B7818300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000039660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000D50000000000000000000000
      0000000000000000000000000000000000000000000000000000C7A79C00FEEE
      D400F7E3C500F6DFBC00F5DBB400F3D7AB00F3D3A200F1CF9A00F0CF9700F0CF
      9800F0CF9800F5D49A00B7818300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000032590000427600003C6B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000BA000000F6000000DF00000000000000
      0000000000000000000000000000000000000000000000000000C7A79E00FDEF
      D900F6E3CB00F5DFC200F4DBBA00F2D7B200F1D4A900F1D0A200EECC9900EECC
      9700EECC9700F3D19900B7818300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000003865003F92D60000467E000043780000335B000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000D1007373FF000404FF000000F9000000BD000000
      0000000000000000000000000000000000000000000000000000C7A9A100FEF3
      E300E5A65700E5A65700E5A65700E5A65700E5A65700E5A65700E5A65700E5A6
      5700E5A65700F3D19900B7818300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000003560003E91D5002A7DC100085B9F00004A8500005090000035
      5E00000000000000000000000000000000000000000000000000000000000000
      0000000000000000C7007272FF005555FF002525FF000A0AFF001414FF000000
      C400000000000000000000000000000000000000000000000000C9ACA500FFF7
      EB00F9EBDA00F7E7D200F6E3CA00F5DFC200F4DBB900F2D7B200F1D4AA00F0D0
      A100EFCD9900F3D19800B7818300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000003763004396DA003386CA002477BB001467AB000E61A500003E70000048
      8200003661000000000000000000000000000000000000000000000000000000
      00000000CE007979FF006262FF004D4DFF003636FF002D2DFF000000E9000707
      FF000000CB000000000000000000000000000000000000000000CEB2AA00FFFC
      F400E5A65700E5A65700E5A65700E5A657000C851800E5A65700E5A65700E5A6
      5700E5A65700F3D29B00B7818300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000034
      5D003E91D5003B8ED2002477BB001D70B4001B6EB200176AAE001265A9000044
      7B0000457D000039660000000000000000000000000000000000000000000000
      C1007272FF006E6EFF004D4DFF004343FF004040FF003A3AFF003333FF000101
      FE000202FF000000D50000000000000000000000000000000000D3B7AF00FFFF
      FD00FBF4EC00FAEFE300F9EBDA000C8518003CBC5D000C851800F4DBBA00F2D7
      B100F0D4A900F5D5A300B7818300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000325900ADDB
      FF003E91D5003B8ED2003B8ED200287BBF002376BA001669AD001E71B5001A6D
      B1000048820000467E00003560000000000000000000000000000000BA00D3D3
      FF007272FF006E6EFF006E6EFF005353FF004B4BFF003939FF004444FF003F3F
      FF000707FF000404FF000000C700000000000000000000000000D7BBB200FFFF
      FF00E5A65700E5A657000C85180052D97F0062ED970041C465000C851800E5A6
      5700E5A65700F6D9AC00B7818300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000032
      5900ADDBFF003E91D5003B8ED200398CD0003588CC002F82C6003083C7001669
      AD00004D8B000037630000000000000000000000000000000000000000000000
      BA00D3D3FF007272FF006E6EFF006B6BFF006565FF005D5DFF005E5EFF003939
      FF000F0FFF000000CE0000000000000000000000000000000000DABEB300FFFF
      FF00FFFEFD000C85180046CE6C0059E4880058E1880061EB940040C165000C85
      1800F4DBB900F8DDB400B7818300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000003D6E00ADDBFF003E91D5003B8ED2003B8ED2003B8ED2003A8DD100176A
      AE00003B6A000000000000000000000000000000000000000000000000000000
      00000000E500D3D3FF007272FF006E6EFF006E6EFF006E6EFF006C6CFF003A3A
      FF000000DB000000000000000000000000000000000000000000DEC1B500FFFF
      FF000C8518000C8518000C8518000C85180056E1840047CD6E000C8518000C85
      18000C851800F0DAB700B7818300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000325900ADDBFF003E91D5003B8ED2003B8ED2004598DC000035
      6000000000000000000000000000000000000000000000000000000000000000
      0000000000000000BA00D3D3FF007272FF006E6EFF006E6EFF007C7CFF000000
      C700000000000000000000000000000000000000000000000000E2C5B500FFFF
      FF00FFFFFF00FFFFFF00FFFEFD000C8518004EDD790036BA54000C851800FCEF
      D900E6D9C400C6BCA900B7818300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000325900ADDBFF003E91D5003B8ED200003763000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000BA00D3D3FF007272FF006E6EFF000000CE000000
      0000000000000000000000000000000000000000000000000000E5C7B700FFFF
      FF00E5A65700E5A65700E5A657000C85180040D065000C851800E5A65700C6A1
      9400B5948900B08F8100B7818300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000325900ADDBFF0000335B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000BA00D3D3FF000000BD00000000000000
      0000000000000000000000000000000000000000000000000000E9CBB800FFFF
      FF00FFFFFF00FFFFFF000C8518002AB743002DBA49000C851800E3CFC900BF8C
      7600E8B27000ECA54A00C5876800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000032590000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000BA0000000000000000000000
      0000000000000000000000000000000000000000000000000000ECCDBA00FFFF
      FF00FFFFFF00FFFFFF000C85180021B538000C851800FFFFFF00E4D4D200C89A
      7F00FAC57700CD93770000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EACAB600FCF7
      F4000C8518000C8518000C8518000C851800F9F5F300F9F5F300E1D0CE00C797
      7C00CF9B86000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000C8518000C85
      18000C8518000C851800EBCBB800EACBB800EACBB800EACCB900DABBB000B885
      7A00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00078DBE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000078DBE0063CBF800078DBE00A3E1
      FB0066CDF90065CDF80065CDF90065CDF90065CDF80065CDF90065CDF80066CD
      F8003AADD800ACE7F500078DBE00000000000000000000000000000000000000
      0000000000000000000000000000000000001B4F310000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009A009C0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF2D220000000000000000000000
      000000000000000000000000000000000000078DBE006AD1F900078DBE00A8E5
      FC006FD4FA006FD4F9006ED4FA006FD4F9006FD4FA006FD4FA006FD4FA006ED4
      F9003EB1D900B1EAF500078DBE00000000000000000000000000000000000000
      0000000000000000000000000000005F00000777090000700200000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000087008800B200B400A100A300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF241900FF382E00FF302600000000000000
      000000000000000000000000000000000000078DBE0072D6FA00078DBE00AEEA
      FC0079DCFB0079DCFB0079DCFB0079DCFB0079DCFB007ADCFB0079DCFA0079DC
      FA0044B5D900B6EEF600078DBE00000000000000000000000000000000000000
      000000000000000000000D60110061C664000C7C0E0008780A00104E20000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000098009900FD59FF00B904BB00B500B70089008B000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF2C2100FF938E00FF3D3400FF392F00FF251A000000
      000000000000000000000000000000000000078DBE0079DDFB00078DBE00B5EE
      FD0083E4FB0084E4FB0083E4FC0083E4FC0084E4FC0083E4FC0083E4FB0084E5
      FC0048B9DA00BBF2F600078DBE00000000000000000000000000000000000000
      00000000000000610A0040CA640021BE450000A21600008C00000B8C13000261
      0600000000000000000000000000000000000000000000000000000000000000
      00000000000090009200FD58FF00F23DF400D01BD200BD08BF00C40FC6008E00
      8F00000000000000000000000000000000000000000000000000000000000000
      000000000000FF291E00FF928D00FF7C7600FF574E00FF423800FF4A4000FF27
      1D0000000000000000000000000000000000078DBE0082E3FC00078DBE00BAF3
      FD008DEBFC008DEBFC008DEBFC008DEBFD008DEBFD008DEBFC008DEBFD008DEB
      FC004CBBDA00BEF4F700078DBE00000000000000000000000000000000000000
      0000125423005FC770003BBD5B0024B3430004AD250000A91D00007700001877
      1C0022531B000000000000000000000000000000000000000000000000000000
      000095009700FD60FF00FB46FD00EC37EE00DC27DE00D621D800A800AA00BB06
      BD00930094000000000000000000000000000000000000000000000000000000
      0000FF2B2000FF989300FF868000FF766F00FF645C00FF5D5500FF342900FF40
      3600FF2A1F00000000000000000000000000078DBE008AEAFC00078DBE00FFFF
      FF00C9F7FE00C8F7FE00C9F7FE00C9F7FE00C9F7FE00C8F7FE00C9F7FE00C8F7
      FE009BD5E700DEF9FB00078DBE00000000000000000000000000000000002150
      18005CC6610031D0520016BD36000FB62F000EAF37000EAB32001DA527000181
      040001820300006C000000000000000000000000000000000000000000008B00
      8D00FD58FF00FD53FF00EC37EE00E530E700E32EE500DF2AE100DA25DC00B702
      B900B803BA009A009C000000000000000000000000000000000000000000FF26
      1B00FF928D00FF8F8900FF766F00FF6E6700FF6C6400FF676000FF625A00FF3B
      3100FF3C3300FF2D22000000000000000000078DBE0093F0FE00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE000000000000000000000000001F452700B0FF
      B5005CC6610031D0520031D0520019C03C0015BA370005B1240029B133002CAC
      2F00048506000A820500104C2D0000000000000000000000000087008800FECB
      FF00FD58FF00FD53FF00FD53FF00F03BF200EB36ED00DE29E000E631E800E22D
      E400BB06BD00B904BB0090009200000000000000000000000000FF241900FFDD
      DC00FF928D00FF8F8900FF8F8900FF7A7300FF756E00FF665F00FF6F6800FF6B
      6300FF403600FF3D3400FF291E0000000000078DBE009BF5FE009AF6FE009AF6
      FE009BF5FD009BF6FE009AF6FE009BF5FE009AF6FD009BF5FE009AF6FE009AF6
      FE000989BA000000000000000000000000000000000000000000000000001F45
      2700B0FFB5005CC6610031D052002FCE500028CA4F0025C446002BC6450022A9
      2B001D8611001965000000000000000000000000000000000000000000008700
      8800FECBFF00FD58FF00FD53FF00FD4FFF00FD48FF00F742F900F843FA00DE29
      E000C10CC300950097000000000000000000000000000000000000000000FF24
      1900FFDDDC00FF928D00FF8F8900FF8D8700FF888200FF827B00FF837D00FF66
      5F00FF463D00FF2B20000000000000000000078DBE00FEFEFE00A0FBFF00A0FB
      FE00A0FBFE00A1FAFE00A1FBFE00A0FAFE00A1FBFE00A1FBFF00A0FBFF00A1FB
      FF000989BA000000000000000000000000000000000000000000000000000000
      0000004C5200B0FFB5005CC6610031D0520031D0520031D0520035D04F0023AA
      2C00066F00000000000000000000000000000000000000000000000000000000
      0000A600A800FECBFF00FD58FF00FD53FF00FD53FF00FD53FF00FD51FF00DF2A
      E1009F00A1000000000000000000000000000000000000000000000000000000
      0000FF322800FFDDDC00FF928D00FF8F8900FF8F8900FF8F8900FF8E8800FF67
      6000FF2F250000000000000000000000000000000000078DBE00FEFEFE00A5FE
      FF00A5FEFF00A5FEFF00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00000000000000000000000000000000000000000000000000000000000000
      0000000000001F452700B0FFB5005CC6610031D0520031D052005EC973001054
      1D00000000000000000000000000000000000000000000000000000000000000
      00000000000087008800FECBFF00FD58FF00FD53FF00FD53FF00FD64FF009000
      9200000000000000000000000000000000000000000000000000000000000000
      000000000000FF241900FFDDDC00FF928D00FF8F8900FF8F8900FF9A9500FF29
      1E00000000000000000000000000000000000000000000000000078DBE00078D
      BE00078DBE00078DBE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001F452700B0FFB5005CC661003FC6620000630D000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000087008800FECBFF00FD58FF00FD53FF00950097000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF241900FFDDDC00FF928D00FF8F8900FF2B20000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000001F452700B0FFB50012521600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000087008800FECBFF0089008B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF241900FFDDDC00FF251A00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000056130000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008700880000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF24190000000000000000000000
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
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFC0010000FF7FFF7FC0010000
      FE3FFE3FC0010000FC1FFC1FC0010000F80FF80FC0010000F007F007C0010000
      E003E003C0010000C001C001C0010000E003E003C0010000F007F007C0010000
      F80FF80FC0010000FC1FFC1FC0010000FE3FFE3FC0010000FF7FFF7FC0030000
      FFFFFFFFC0070000FFFFFFFFC00F00008003FFFFFFFFFFFF0001FF7FFF7FFF7F
      0001FE3FFE3FFE3F0001FC1FFC1FFC1F0001F80FF80FF80F0001F007F007F007
      0001E003E003E0030001C001C001C0010007E003E003E0030007F007F007F007
      800FF80FF80FF80FC3FFFC1FFC1FFC1FFFFFFE3FFE3FFE3FFFFFFF7FFF7FFF7F
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 272
    Top = 424
    object pmCopy: TMenuItem
      Caption = '&Copy'
      ShortCut = 16451
      OnClick = pmCopyClick
    end
    object pmUndo: TMenuItem
      Caption = 'Undo'
      ShortCut = 8
      Visible = False
      OnClick = pmUndoClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object pmOpen: TMenuItem
      Caption = 'Open'
      ShortCut = 13
      OnClick = pmOpenClick
    end
    object pmOpenNewTab: TMenuItem
      Caption = 'Open in new Tab'
      ShortCut = 16397
      OnClick = pmOpenNewTabClick
    end
  end
end