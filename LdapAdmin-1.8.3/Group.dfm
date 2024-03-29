object GroupDlg: TGroupDlg
  Left = 396
  Top = 179
  BorderStyle = bsDialog
  Caption = 'Create Group'
  ClientHeight = 472
  ClientWidth = 411
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poMainFormCenter
  OnClose = FormClose
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 62
    Height = 13
    Caption = '&Group name:'
  end
  object Label2: TLabel
    Left = 16
    Top = 64
    Width = 57
    Height = 13
    Caption = '&Description:'
  end
  object edName: TEdit
    Left = 16
    Top = 32
    Width = 377
    Height = 21
    TabOrder = 0
    OnChange = edNameChange
  end
  object edDescription: TEdit
    Left = 16
    Top = 80
    Width = 377
    Height = 21
    TabOrder = 1
    OnChange = edDescriptionChange
  end
  object OkBtn: TButton
    Left = 248
    Top = 440
    Width = 75
    Height = 25
    Caption = '&OK'
    Enabled = False
    ModalResult = 1
    TabOrder = 2
  end
  object CancelBtn: TButton
    Left = 328
    Top = 440
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 112
    Width = 393
    Height = 321
    ActivePage = TabSheet1
    TabOrder = 4
    OnChange = PageControl1Change
    object TabSheet1: TTabSheet
      Caption = '&Members'
      object UserList: TListView
        Left = 8
        Top = 8
        Width = 369
        Height = 249
        Columns = <
          item
            Caption = 'Name'
            Width = 120
          end
          item
            Caption = 'Path'
            Width = 220
          end>
        HideSelection = False
        MultiSelect = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnColumnClick = ListViewColumnClick
        OnCompare = ListViewCompare
        OnDeletion = UserListDeletion
      end
      object AddUserBtn: TButton
        Left = 8
        Top = 264
        Width = 75
        Height = 25
        Caption = '&Add'
        TabOrder = 1
        OnClick = AddUserBtnClick
      end
      object RemoveUserBtn: TButton
        Left = 88
        Top = 264
        Width = 75
        Height = 25
        Caption = '&Remove'
        Enabled = False
        TabOrder = 2
        OnClick = RemoveUserBtnClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = '&Samba'
      ImageIndex = 2
      object Label3: TLabel
        Left = 24
        Top = 112
        Width = 73
        Height = 13
        Caption = 'Samba domain:'
      end
      object Label4: TLabel
        Left = 272
        Top = 112
        Width = 36
        Height = 13
        Caption = 'NT-Rid:'
      end
      object Bevel1: TBevel
        Left = 24
        Top = 40
        Width = 337
        Height = 9
        Shape = bsBottomLine
      end
      object Label5: TLabel
        Left = 24
        Top = 64
        Width = 67
        Height = 13
        Caption = 'Display name:'
      end
      object cbSambaDomain: TComboBox
        Left = 24
        Top = 128
        Width = 233
        Height = 21
        Style = csDropDownList
        TabOrder = 2
        OnChange = cbSambaDomainChange
      end
      object RadioGroup1: TRadioGroup
        Left = 24
        Top = 160
        Width = 337
        Height = 113
        Caption = 'Group type:'
        ItemIndex = 0
        Items.Strings = (
          'Domain group'
          'Local group'
          'Built-in group:')
        TabOrder = 4
        OnClick = RadioGroup1Click
      end
      object cbBuiltin: TComboBox
        Left = 160
        Top = 240
        Width = 185
        Height = 21
        Style = csDropDownList
        Color = clBtnFace
        Enabled = False
        TabOrder = 5
        OnChange = cbBuiltinChange
        Items.Strings = (
          'Domain Admins'
          'Domain Users'
          'Domain Guests')
      end
      object edRid: TEdit
        Left = 272
        Top = 128
        Width = 89
        Height = 21
        TabOrder = 3
        OnChange = edRidChange
      end
      object cbSambaGroup: TCheckBox
        Left = 24
        Top = 24
        Width = 153
        Height = 17
        Caption = 'Samba domain mapping'
        TabOrder = 0
        OnClick = cbSambaGroupClick
      end
      object edDisplayName: TEdit
        Left = 24
        Top = 80
        Width = 337
        Height = 21
        TabOrder = 1
        OnChange = edDisplayNameChange
      end
    end
  end
end
