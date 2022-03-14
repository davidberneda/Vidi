object FormTypes: TFormTypes
  Left = 0
  Top = 0
  Caption = 'Types'
  ClientHeight = 515
  ClientWidth = 339
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
    Top = 153
    Width = 339
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 249
    ExplicitWidth = 266
  end
  object Splitter2: TSplitter
    Left = 0
    Top = 319
    Width = 339
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 280
  end
  object PanelTypes: TPanel
    Left = 0
    Top = 0
    Width = 339
    Height = 153
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
  end
  object PanelOptions: TPanel
    Left = 0
    Top = 291
    Width = 339
    Height = 28
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object CBAncestor: TCheckBox
      Left = 8
      Top = 6
      Width = 177
      Height = 17
      Caption = 'Show Ancestor Items'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 0
      OnClick = CBAncestorClick
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 322
    Width = 339
    Height = 193
    ActivePage = TabReferences
    Align = alBottom
    TabOrder = 2
    object TabReferences: TTabSheet
      Caption = 'References'
    end
  end
  object PanelFields: TPanel
    Left = 0
    Top = 156
    Width = 339
    Height = 135
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
  end
  object PopupMenu1: TPopupMenu
    Left = 112
    Top = 192
    object Jumpto1: TMenuItem
      Caption = '&Jump to'
      OnClick = Jumpto1Click
    end
  end
end
