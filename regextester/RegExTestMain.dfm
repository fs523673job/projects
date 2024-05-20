object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'RegEx Tester'
  ClientHeight = 459
  ClientWidth = 757
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 8
    Width = 92
    Height = 13
    Caption = 'Regular Expression'
  end
  object Label2: TLabel
    Left = 24
    Top = 144
    Width = 59
    Height = 13
    Caption = 'Sample Text'
  end
  object Label3: TLabel
    Left = 400
    Top = 143
    Width = 71
    Height = 13
    Caption = 'Search Results'
  end
  object Label4: TLabel
    Left = 400
    Top = 9
    Width = 37
    Height = 13
    Caption = 'Options'
  end
  object txtRegEx: TMemo
    Left = 24
    Top = 24
    Width = 353
    Height = 114
    TabOrder = 0
  end
  object txtSample: TMemo
    Left = 24
    Top = 160
    Width = 353
    Height = 281
    TabOrder = 1
  end
  object tvResults: TTreeView
    Left = 398
    Top = 160
    Width = 339
    Height = 281
    Indent = 19
    TabOrder = 2
  end
  object cmdTest: TButton
    Left = 400
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 3
    OnClick = cmdTestClick
  end
  object chkIgnoreCase: TCheckBox
    Left = 400
    Top = 28
    Width = 97
    Height = 17
    Caption = 'Ignore Case'
    TabOrder = 4
  end
  object chkMultiLine: TCheckBox
    Left = 512
    Top = 28
    Width = 97
    Height = 17
    Caption = 'MultiLine'
    TabOrder = 5
  end
  object chkSingleLine: TCheckBox
    Left = 615
    Top = 28
    Width = 97
    Height = 17
    Caption = 'SingleLine'
    TabOrder = 6
  end
  object chkExplicitCapture: TCheckBox
    Left = 400
    Top = 60
    Width = 97
    Height = 17
    Caption = 'Explicit Capture'
    TabOrder = 7
  end
  object chkCompiled: TCheckBox
    Left = 512
    Top = 60
    Width = 97
    Height = 17
    Caption = 'Compiled'
    TabOrder = 8
  end
  object chkIgnorePatternSpace: TCheckBox
    Left = 615
    Top = 60
    Width = 122
    Height = 17
    Caption = 'IgnorePatternSpace'
    TabOrder = 9
  end
end
