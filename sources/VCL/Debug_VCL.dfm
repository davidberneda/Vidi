object FormDebug: TFormDebug
  Left = 0
  Top = 0
  Caption = 'FormDebug'
  ClientHeight = 572
  ClientWidth = 311
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
    Top = 183
    Width = 311
    Height = 6
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 153
  end
  object Splitter2: TSplitter
    Left = 0
    Top = 342
    Width = 311
    Height = 6
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 312
  end
  object PanelBreak: TPanel
    Left = 0
    Top = 0
    Width = 311
    Height = 183
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
  end
  object PanelTrace: TPanel
    Left = 0
    Top = 189
    Width = 311
    Height = 153
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 159
  end
  object PanelWatch: TPanel
    Left = 0
    Top = 348
    Width = 311
    Height = 224
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 318
    ExplicitHeight = 254
  end
end
