object FormComplete: TFormComplete
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 365
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  PixelsPerInch = 96
  TextHeight = 13
  object LBItems: TListBox
    Left = 0
    Top = 0
    Width = 554
    Height = 338
    Align = alClient
    BorderStyle = bsNone
    Ctl3D = False
    ItemHeight = 13
    ParentCtl3D = False
    TabOrder = 0
    OnDblClick = LBItemsDblClick
    OnKeyUp = LBItemsKeyUp
  end
  object Panel1: TPanel
    Left = 0
    Top = 338
    Width = 554
    Height = 27
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 299
    ExplicitWidth = 538
    object CBAlpha: TCheckBox
      Left = 8
      Top = 6
      Width = 97
      Height = 17
      Caption = '&Alphabetical'
      TabOrder = 0
      OnClick = CBAlphaClick
    end
  end
end
