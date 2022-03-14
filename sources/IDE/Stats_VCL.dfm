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
    Top = 106
    Width = 457
    Height = 6
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 261
  end
  object MemoStats: TMemo
    Left = 0
    Top = 0
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
    ExplicitHeight = 89
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 112
    Width = 457
    Height = 365
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
  end
end
