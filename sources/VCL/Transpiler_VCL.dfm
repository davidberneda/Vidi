object FormTranspiler: TFormTranspiler
  Left = 0
  Top = 0
  Caption = 'Transpiler'
  ClientHeight = 498
  ClientWidth = 557
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 451
    Width = 557
    Height = 6
    Cursor = crVSplit
    Align = alBottom
    Visible = False
    ExplicitLeft = -8
    ExplicitTop = 342
    ExplicitWidth = 487
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 557
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 487
    object Label1: TLabel
      Left = 16
      Top = 14
      Width = 51
      Height = 13
      Caption = 'Language:'
    end
    object CBLang: TComboBox
      Left = 73
      Top = 11
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Vidi'
      OnChange = CBLangChange
      Items.Strings = (
        'Vidi'
        'Delphi'
        'FreePascal'
        'C#')
    end
    object BRefresh: TButton
      Left = 240
      Top = 9
      Width = 57
      Height = 25
      Caption = 'Refresh'
      TabOrder = 1
      OnClick = BRefreshClick
    end
    object BAsNew: TButton
      Left = 303
      Top = 9
      Width = 58
      Height = 25
      Caption = 'As New'
      Enabled = False
      TabOrder = 2
      OnClick = BAsNewClick
    end
    object BCompile: TButton
      Left = 376
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Compile'
      Enabled = False
      TabOrder = 3
      OnClick = BCompileClick
    end
    object BRun: TButton
      Left = 465
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Run'
      Enabled = False
      TabOrder = 4
      OnClick = BRunClick
    end
  end
  object PanelCompile: TPanel
    Left = 0
    Top = 457
    Width = 557
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 487
    object Label2: TLabel
      Left = 11
      Top = 13
      Width = 26
      Height = 13
      Caption = 'Path:'
    end
    object LabelPath: TLabel
      Left = 43
      Top = 14
      Width = 3
      Height = 13
      Cursor = crHandPoint
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      OnClick = LabelPathClick
    end
  end
end
