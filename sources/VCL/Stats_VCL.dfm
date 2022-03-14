object FormStats: TFormStats
  Left = 0
  Top = 0
  Caption = 'Code Statistics'
  ClientHeight = 477
  ClientWidth = 457
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 147
    Width = 457
    Height = 6
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 261
  end
  object MemoStats: TMemo
    Left = 0
    Top = 41
    Width = 457
    Height = 106
    Align = alTop
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    ExplicitTop = 0
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 153
    Width = 457
    Height = 324
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 112
    ExplicitHeight = 365
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 457
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 24
    ExplicitTop = -11
    object CBAllModules: TCheckBox
      Left = 16
      Top = 13
      Width = 97
      Height = 17
      Caption = 'All Modules'
      TabOrder = 0
      OnClick = CBAllModulesClick
    end
  end
end
