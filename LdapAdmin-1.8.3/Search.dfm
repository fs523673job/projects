object SearchFrm: TSearchFrm
  Left = 482
  Top = 181
  ActiveControl = edName
  Caption = 'Search'
  ClientHeight = 499
  ClientWidth = 714
  Color = clBtnFace
  Constraints.MinHeight = 409
  Constraints.MinWidth = 473
  ParentFont = True
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 480
    Width = 714
    Height = 19
    Panels = <
      item
        Width = 50
      end
      item
        Width = 50
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 29
    Width = 714
    Height = 172
    Align = alTop
    BorderWidth = 3
    TabOrder = 1
    object Panel2: TPanel
      Left = 4
      Top = 4
      Width = 706
      Height = 164
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object Panel40: TPanel
        Left = 0
        Top = 0
        Width = 706
        Height = 164
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object Panel41: TPanel
          Left = 0
          Top = 0
          Width = 706
          Height = 49
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            706
            49)
          object Label5: TLabel
            Left = 8
            Top = 12
            Width = 26
            Height = 13
            Caption = 'Path:'
          end
          object Bevel1: TBevel
            Left = 8
            Top = 28
            Width = 702
            Height = 10
            Anchors = [akLeft, akTop, akRight]
            Shape = bsBottomLine
          end
          object cbBasePath: TComboBox
            Left = 48
            Top = 8
            Width = 561
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
          end
          object PathBtn: TButton
            Left = 615
            Top = 7
            Width = 85
            Height = 23
            Anchors = [akTop, akRight]
            Caption = '&Browse...'
            TabOrder = 1
            OnClick = PathBtnClick
          end
        end
        object Panel4: TPanel
          Left = 0
          Top = 49
          Width = 706
          Height = 115
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
          object PageControl: TPageControl
            Left = 0
            Top = 0
            Width = 609
            Height = 115
            ActivePage = TabSheet1
            Align = alClient
            TabOrder = 0
            object TabSheet1: TTabSheet
              Caption = '&Search'
              DesignSize = (
                601
                87)
              object Label6: TLabel
                Left = 24
                Top = 0
                Width = 31
                Height = 13
                Caption = '&Name:'
              end
              object Label7: TLabel
                Left = 24
                Top = 40
                Width = 32
                Height = 13
                Caption = '&E-Mail:'
              end
              object sbCustom1: TSpeedButton
                Left = 576
                Top = 16
                Width = 21
                Height = 21
                AllowAllUp = True
                Anchors = [akTop, akRight]
                GroupIndex = 1
                Glyph.Data = {
                  36030000424D3603000000000000360000002800000010000000100000000100
                  1800000000000003000000000000000000000000000000000000000000000000
                  0000000000000000000000000000000000000000000000000000000000000000
                  0000000000000000000000000000000000000000000000000000000000000000
                  0000000000000000000000000000000000000000000000000000000000000000
                  0000000000000000000000006640496640496640496640496640496640496640
                  49664049664049000000000000000000000000000000000000000000664049FF
                  FFFFE0E0E0E3E3E3E7E7E7EAEAEAEDEDEDF0F0F0664049000000000000000000
                  000000000000000000000000664049FFFFFFDEDEDEE1E1E18989898B8B8B8D8D
                  8DEEEEEE664049000000000000000000000000000000000000000000664049FF
                  FFFFDBDBDBDFDFDFE2E2E2E5E5E5E8E8E8ECECEC664049000000000000000000
                  6640496640496640496640496640496640496640496640496640498888888B8B
                  8BE9E9E9664049000000000000000000664049FFFFFFABABABE3E3E3E7E7E7EA
                  EAEAEDEDEDEEEFF065434CDADEDFDEE2E3E3E6E665404A000000000000000000
                  664049FFFFFF0000009D9D9D7C7C7C7D7D7D7D8081E2EBED6A5D6A7E7F8A7E7F
                  8A7F7883664D577FBFFF000000000000000000000000000000000000BCBCBCE4
                  E5E5DDE6E8C7E3EB789AAB7FADBD7FADBD789AAB6D6F7C79D2E8000000000000
                  664049FFFFFF000000A8A8A886868687898A87999EACDDEBA4D3E2D1EFF8D1F0
                  F8AAE3F284D4EB7DD3EB000000000000664049FFFFFFA4A4A4DADADADDDDDDDC
                  DFE0C8DFE5A7DDECCCE8F0F6FAFBF6FAFBCFECF490D9EE7BD2EC000000000000
                  6640498C6F7781616981616981616980646D7F7F8A8CB8CBCCE7EFF6FAFBF6FA
                  FBCFECF491DAEE7BD2EC00000000000066404966404966404966404966404966
                  434D695D6A7898ABA4D1E2D1EFF7D1EFF7AAE2F284D4EB7DD3EB000000000000
                  0000000000000000000000000000007FFFFF79D2E87CD0E984D4ED88D2E788D2
                  E784D4ED7CD0E979D2E800000000000000000000000000000000000000000000
                  00003FBFFF6EC7DD7ACDE679D1E879D1E87ACDE66EC7DD7FBFFF}
                OnClick = sbCustom1Click
              end
              object sbCustom2: TSpeedButton
                Left = 576
                Top = 56
                Width = 21
                Height = 21
                AllowAllUp = True
                Anchors = [akTop, akRight]
                GroupIndex = 2
                Glyph.Data = {
                  36030000424D3603000000000000360000002800000010000000100000000100
                  1800000000000003000000000000000000000000000000000000000000000000
                  0000000000000000000000000000000000000000000000000000000000000000
                  0000000000000000000000000000000000000000000000000000000000000000
                  0000000000000000000000000000000000000000000000000000000000000000
                  0000000000000000000000006640496640496640496640496640496640496640
                  49664049664049000000000000000000000000000000000000000000664049FF
                  FFFFE0E0E0E3E3E3E7E7E7EAEAEAEDEDEDF0F0F0664049000000000000000000
                  000000000000000000000000664049FFFFFFDEDEDEE1E1E18989898B8B8B8D8D
                  8DEEEEEE664049000000000000000000000000000000000000000000664049FF
                  FFFFDBDBDBDFDFDFE2E2E2E5E5E5E8E8E8ECECEC664049000000000000000000
                  6640496640496640496640496640496640496640496640496640498888888B8B
                  8BE9E9E9664049000000000000000000664049FFFFFFABABABE3E3E3E7E7E7EA
                  EAEAEDEDEDEEEFF065434CDADEDFDEE2E3E3E6E665404A000000000000000000
                  664049FFFFFF0000009D9D9D7C7C7C7D7D7D7D8081E2EBED6A5D6A7E7F8A7E7F
                  8A7F7883664D577FBFFF000000000000000000000000000000000000BCBCBCE4
                  E5E5DDE6E8C7E3EB789AAB7FADBD7FADBD789AAB6D6F7C79D2E8000000000000
                  664049FFFFFF000000A8A8A886868687898A87999EACDDEBA4D3E2D1EFF8D1F0
                  F8AAE3F284D4EB7DD3EB000000000000664049FFFFFFA4A4A4DADADADDDDDDDC
                  DFE0C8DFE5A7DDECCCE8F0F6FAFBF6FAFBCFECF490D9EE7BD2EC000000000000
                  6640498C6F7781616981616981616980646D7F7F8A8CB8CBCCE7EFF6FAFBF6FA
                  FBCFECF491DAEE7BD2EC00000000000066404966404966404966404966404966
                  434D695D6A7898ABA4D1E2D1EFF7D1EFF7AAE2F284D4EB7DD3EB000000000000
                  0000000000000000000000000000007FFFFF79D2E87CD0E984D4ED88D2E788D2
                  E784D4ED7CD0E979D2E800000000000000000000000000000000000000000000
                  00003FBFFF6EC7DD7ACDE679D1E879D1E87ACDE66EC7DD7FBFFF}
                OnClick = sbCustom2Click
              end
              object edCustom1: TEdit
                Left = 24
                Top = 16
                Width = 548
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                Color = clInfoBk
                Enabled = False
                TabOrder = 2
              end
              object edName: TEdit
                Left = 24
                Top = 16
                Width = 548
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                TabOrder = 0
              end
              object edCustom2: TEdit
                Left = 24
                Top = 56
                Width = 548
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                Color = clInfoBk
                Enabled = False
                TabOrder = 3
              end
              object edEmail: TEdit
                Left = 24
                Top = 56
                Width = 548
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                TabOrder = 1
              end
            end
            object TabSheet2: TTabSheet
              Caption = '&Custom'
              ImageIndex = 1
              DesignSize = (
                601
                87)
              object Label1: TLabel
                Left = 8
                Top = 8
                Width = 28
                Height = 13
                Caption = 'Filter:'
              end
              object Memo1: TMemo
                Left = 40
                Top = 8
                Width = 559
                Height = 49
                Anchors = [akLeft, akTop, akRight]
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Courier New'
                Font.Style = []
                ParentFont = False
                TabOrder = 0
              end
              object cbFilters: TComboBox
                Left = 40
                Top = 61
                Width = 423
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                TabOrder = 1
                OnChange = cbFiltersChange
                OnDropDown = cbFiltersDropDown
              end
              object SaveFilterBtn: TButton
                Left = 465
                Top = 60
                Width = 65
                Height = 23
                Anchors = [akRight]
                Caption = 'Sa&ve'
                Enabled = False
                TabOrder = 2
                OnClick = SaveFilterBtnClick
              end
              object DeleteFilterBtn: TButton
                Left = 534
                Top = 60
                Width = 65
                Height = 23
                Anchors = [akRight]
                Caption = '&Delete'
                Enabled = False
                TabOrder = 3
                OnClick = DeleteFilterBtnClick
              end
            end
            object TabSheet3: TTabSheet
              Caption = '&Options'
              ImageIndex = 2
              DesignSize = (
                601
                87)
              object Label4: TLabel
                Left = 15
                Top = 12
                Width = 52
                Height = 13
                Alignment = taRightJustify
                Caption = 'Attributes:'
              end
              object Label2: TLabel
                Left = 4
                Top = 52
                Width = 62
                Height = 13
                Alignment = taRightJustify
                Caption = 'Search level:'
              end
              object Label3: TLabel
                Left = 254
                Top = 52
                Width = 99
                Height = 13
                Alignment = taRightJustify
                Caption = 'Dereference aliases:'
              end
              object cbAttributes: TComboBox
                Left = 72
                Top = 9
                Width = 441
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                TabOrder = 0
              end
              object edAttrBtn: TButton
                Left = 519
                Top = 8
                Width = 80
                Height = 23
                Anchors = [akTop, akRight]
                Caption = 'Edit...'
                TabOrder = 1
                OnClick = edAttrBtnClick
              end
              object cbSearchLevel: TComboBox
                Left = 72
                Top = 48
                Width = 153
                Height = 21
                Style = csDropDownList
                TabOrder = 2
                Items.Strings = (
                  'This entry only'
                  'Next level'
                  'Entire subtree')
              end
              object cbDerefAliases: TComboBox
                Left = 359
                Top = 48
                Width = 153
                Height = 21
                Style = csDropDownList
                Anchors = [akLeft, akTop, akRight]
                TabOrder = 3
                Items.Strings = (
                  'Never'
                  'When searching'
                  'When finding'
                  'Always')
              end
            end
            object TabSheet4: TTabSheet
              Caption = '&Regular Expressions'
              ImageIndex = 3
              DesignSize = (
                601
                87)
              object Label8: TLabel
                Left = 9
                Top = 10
                Width = 50
                Height = 13
                Alignment = taRightJustify
                Caption = 'Evaluator:'
              end
              object cbRegExp: TComboBox
                Left = 64
                Top = 60
                Width = 399
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                TabOrder = 4
                OnChange = cbRegExpChange
                OnDropDown = cbRegExpDropDown
              end
              object btnSaveRegEx: TButton
                Left = 464
                Top = 59
                Width = 65
                Height = 23
                Anchors = [akRight]
                Caption = 'Sa&ve'
                Enabled = False
                TabOrder = 5
                OnClick = btnSaveRegExClick
              end
              object btnDeleteRegEx: TButton
                Left = 533
                Top = 59
                Width = 65
                Height = 23
                Anchors = [akRight]
                Caption = '&Delete'
                Enabled = False
                TabOrder = 6
                OnClick = btnDeleteRegExClick
              end
              object edRegExp: TEdit
                Left = 64
                Top = 7
                Width = 534
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                TabOrder = 0
              end
              object cbxReGreedy: TCheckBox
                Left = 64
                Top = 35
                Width = 135
                Height = 17
                Caption = 'Greedy mode'
                Checked = True
                State = cbChecked
                TabOrder = 1
              end
              object cbxReCase: TCheckBox
                Left = 205
                Top = 35
                Width = 141
                Height = 17
                Caption = 'Case sensitive'
                Checked = True
                State = cbChecked
                TabOrder = 2
              end
              object cbxReMultiline: TCheckBox
                Left = 352
                Top = 35
                Width = 182
                Height = 17
                Caption = 'Multiline matching'
                TabOrder = 3
              end
            end
          end
          object Panel3: TPanel
            Left = 609
            Top = 0
            Width = 97
            Height = 115
            Align = alRight
            BevelOuter = bvNone
            TabOrder = 1
            object StartBtn: TBitBtn
              Left = 8
              Top = 20
              Width = 83
              Height = 25
              Action = ActStart
              Caption = 'Sta&rt'
              Glyph.Data = {
                36040000424D3604000000000000360000002800000010000000100000000100
                2000000000000004000000000000000000000000000000000000FF00FF004A66
                7C00BE959600FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF006B9CC3001E89
                E8004B7AA300C8969300FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF004BB4FE0051B5
                FF002089E9004B7AA200C6959200FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0051B7
                FE0051B3FF001D87E6004E7AA000CA979200FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF0051B7FE004EB2FF001F89E6004E7BA200B9949700FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF0052B8FE004BB1FF002787D9005F6A7600FF00FF00B0857F00C09F
                9400C09F9600BC988E00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF0055BDFF00B5D6ED00BF9D9200BB9B8C00E7DAC200FFFF
                E300FFFFE500FDFADA00D8C3B300B58D8500FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00CEA79500FDEEBE00FFFFD800FFFF
                DA00FFFFDB00FFFFE600FFFFFB00EADDDC00AE837F00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00C1A09100FBDCA800FEF7D000FFFF
                DB00FFFFE300FFFFF800FFFFFD00FFFFFD00C6A99C00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00C1A09100FEE3AC00F1C49100FCF2CA00FFFF
                DD00FFFFE400FFFFF700FFFFF700FFFFE900EEE5CB00B9948C00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00C2A19100FFE6AE00EEB58100F7DCAE00FEFD
                D800FFFFDF00FFFFE300FFFFE400FFFFE000F3ECD200BB968E00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00BC978C00FBE7B700F4C79100F2C99400F8E5
                B900FEFCD800FFFFDD00FFFFDC00FFFFE000E2D2BA00B68E8600FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00D9C3A900FFFEE500F7DCB800F2C9
                9400F5D4A500FAE8BD00FDF4C900FDFBD600B6908900FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00B58D8500E8DEDD00FFFEF200F9D8
                A300F4C48C00F9D49F00FDEAB800D0B49F00B8908600FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00AD827F00C9AA9E00EFE0
                B700EFDFB200E7CEAC00B8908600B8908600FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00BA96
                8A00BB988C00B7918800FF00FF00FF00FF00FF00FF00FF00FF00}
              ParentShowHint = False
              ShowHint = True
              TabOrder = 0
            end
            object ClearAllBtn: TButton
              Left = 8
              Top = 52
              Width = 83
              Height = 25
              Action = ActClearAll
              ParentShowHint = False
              ShowHint = True
              TabOrder = 1
            end
          end
        end
      end
    end
  end
  object ResultPanel: TPanel
    Left = 0
    Top = 201
    Width = 714
    Height = 279
    Align = alClient
    BorderWidth = 3
    TabOrder = 2
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 714
    Height = 29
    ButtonHeight = 28
    ButtonWidth = 28
    DisabledImages = MainFrm.ImageList
    Images = MainFrm.ImageList
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    object btnSearch: TToolButton
      Left = 0
      Top = 0
      Hint = 'Search'
      Down = True
      Grouped = True
      ImageIndex = 20
      Style = tbsCheck
      OnClick = btnSearchModifyClick
    end
    object btnModify: TToolButton
      Left = 28
      Top = 0
      Hint = 'Modify'
      Grouped = True
      ImageIndex = 38
      Style = tbsCheck
      OnClick = btnSearchModifyClick
    end
    object ToolButton7: TToolButton
      Left = 56
      Top = 0
      Width = 8
      ImageIndex = 6
      Style = tbsSeparator
    end
    object ToolButton1: TToolButton
      Left = 64
      Top = 0
      Action = ActLoad
    end
    object ToolButton6: TToolButton
      Left = 92
      Top = 0
      Action = ActSave
    end
    object ToolButton8: TToolButton
      Left = 120
      Top = 0
      Width = 8
      ImageIndex = 32
      Style = tbsSeparator
    end
    object ToolButton3: TToolButton
      Left = 128
      Top = 0
      Action = ActEdit
    end
    object ToolButton4: TToolButton
      Left = 156
      Top = 0
      Action = ActProperties
    end
    object ToolButton5: TToolButton
      Left = 184
      Top = 0
      Action = ActGoto
    end
    object ToolButton2: TToolButton
      Left = 212
      Top = 0
      Width = 8
      ImageIndex = 19
      Style = tbsSeparator
    end
    object ToolButton9: TToolButton
      Left = 220
      Top = 0
      Action = ActClose
    end
  end
  object PopupMenu1: TPopupMenu
    Images = MainFrm.ImageList
    Left = 24
    Top = 408
    object pbGoto: TMenuItem
      Action = ActGoto
    end
    object Editentry1: TMenuItem
      Action = ActEdit
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Move1: TMenuItem
      Action = ActMove
    end
    object Copy1: TMenuItem
      Action = ActCopy
    end
    object Delete1: TMenuItem
      Action = ActDelete
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Save1: TMenuItem
      Action = ActSave
    end
    object Saveselected1: TMenuItem
      Action = ActSaveSelected
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object pbProperties: TMenuItem
      Action = ActProperties
    end
  end
  object ActionList: TActionList
    Images = MainFrm.ImageList
    OnUpdate = ActionListUpdate
    Left = 88
    Top = 408
    object ActStart: TAction
      Caption = '&Start'
      Hint = 'Start searching'
      ImageIndex = 20
      OnExecute = ActStartExecute
    end
    object ActGoto: TAction
      Caption = '&Go to...'
      Hint = 'Find entry in LDAP tree'
      ImageIndex = 30
      OnExecute = ActGotoExecute
    end
    object ActProperties: TAction
      Caption = '&Properties...'
      Hint = 'Edit object properties'
      ImageIndex = 16
      OnExecute = ActPropertiesExecute
    end
    object ActSave: TAction
      Caption = '&Save'
      Hint = 'Save results to file'
      ImageIndex = 31
      OnExecute = ActSaveExecute
    end
    object ActEdit: TAction
      Caption = '&Edit entry...'
      Hint = 'Edit with raw editor'
      ImageIndex = 14
      OnExecute = ActEditExecute
    end
    object ActClose: TAction
      Caption = '&Close'
      Hint = 'Close search window'
      ImageIndex = 18
      OnExecute = ActCloseExecute
    end
    object ActClearAll: TAction
      Caption = 'Clear a&ll'
      Hint = 'Clear all search results'
      OnExecute = ActClearAllExecute
    end
    object ActLoad: TAction
      Caption = '&Load'
      ImageIndex = 48
      OnExecute = ActLoadExecute
    end
    object ActCopy: TAction
      Caption = 'Co&py...'
      OnExecute = ActCopyExecute
    end
    object ActMove: TAction
      Caption = '&Move...'
      OnExecute = ActMoveExecute
    end
    object ActDelete: TAction
      Caption = '&Delete'
      ImageIndex = 12
      OnExecute = ActDeleteExecute
    end
    object ActSaveSelected: TAction
      Caption = 'Save selected...'
      OnExecute = ActSaveExecute
    end
  end
  object OpenDialog: TOpenDialog
    Left = 144
    Top = 409
  end
end
