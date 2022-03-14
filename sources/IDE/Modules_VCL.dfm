object FormModules: TFormModules
  Left = 0
  Top = 0
  Caption = 'Modules'
  ClientHeight = 436
  ClientWidth = 296
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter4: TSplitter
    Left = 0
    Top = 281
    Width = 296
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 193
    ExplicitWidth = 333
  end
  object LBModules: TListBox
    Left = 0
    Top = 37
    Width = 296
    Height = 244
    Style = lbOwnerDrawFixed
    Align = alClient
    ItemHeight = 13
    PopupMenu = PopupModules
    Sorted = True
    TabOrder = 0
    OnClick = LBModulesClick
    OnDblClick = LBModulesDblClick
    OnDrawItem = LBModulesDrawItem
  end
  object Panel1: TPanel
    Left = 0
    Top = 284
    Width = 296
    Height = 44
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object LModulePath: TLabel
      Left = 8
      Top = 16
      Width = 3
      Height = 13
      Cursor = crHandPoint
      Hint = 'Go to folder'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = LModulePathClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 296
    Height = 37
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object BSearch: TSpeedButton
      Left = 161
      Top = 7
      Width = 23
      Height = 22
      Enabled = False
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000FF00FF4A667C
        BE9596FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FF6B9CC31E89E84B7AA3C89693FF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF4BB4FE51B5FF
        2089E94B7AA2C69592FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FF51B7FE51B3FF1D87E64E7AA0CA9792FF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        51B7FE4EB2FF1F89E64E7BA2B99497FF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF52B8FE4BB1FF2787D95F6A76FF
        00FFB0857FC09F94C09F96BC988EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FF55BDFFB5D6EDBF9D92BB9B8CE7DAC2FFFFE3FFFFE5FDFADAD8C3
        B3B58D85FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEA795FD
        EEBEFFFFD8FFFFDAFFFFDBFFFFE6FFFFFBEADDDCAE837FFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFC1A091FBDCA8FEF7D0FFFFDBFFFFE3FFFFF8FFFF
        FDFFFFFDC6A99CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC1A091FEE3ACF1
        C491FCF2CAFFFFDDFFFFE4FFFFF7FFFFF7FFFFE9EEE5CBB9948CFF00FFFF00FF
        FF00FFFF00FFFF00FFC2A191FFE6AEEEB581F7DCAEFEFDD8FFFFDFFFFFE3FFFF
        E4FFFFE0F3ECD2BB968EFF00FFFF00FFFF00FFFF00FFFF00FFBC978CFBE7B7F4
        C791F2C994F8E5B9FEFCD8FFFFDDFFFFDCFFFFE0E2D2BAB68E86FF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFD9C3A9FFFEE5F7DCB8F2C994F5D4A5FAE8BDFDF4
        C9FDFBD6B69089FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB58D85E8
        DEDDFFFEF2F9D8A3F4C48CF9D49FFDEAB8D0B49FB89086FF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFAD827FC9AA9EEFE0B7EFDFB2E7CEACB890
        86B89086FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFBA968ABB988CB79188FF00FFFF00FFFF00FFFF00FF}
      OnClick = BSearchClick
    end
    object LModuleCount: TLabel
      Left = 198
      Top = 12
      Width = 42
      Height = 13
      Caption = 'Count: 0'
    end
    object CBSearch: TComboBox
      Left = 8
      Top = 8
      Width = 149
      Height = 21
      TabOrder = 0
      OnChange = CBSearchChange
      OnCloseUp = CBSearchCloseUp
      OnKeyUp = CBSearchKeyUp
    end
  end
  object PanelUsages: TPanel
    Left = 0
    Top = 328
    Width = 296
    Height = 108
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
  end
  object PopupModules: TPopupMenu
    OnPopup = PopupModulesPopup
    Left = 140
    Top = 88
    object Open1: TMenuItem
      Caption = '&Open'
      OnClick = Open1Click
    end
    object CloseModule: TMenuItem
      Caption = '&Close'
      OnClick = CloseModuleClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Folderlocation1: TMenuItem
      Caption = '&Folder location...'
      OnClick = Folderlocation1Click
    end
  end
end
