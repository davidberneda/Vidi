object FormExplorer: TFormExplorer
  Left = 0
  Top = 0
  Caption = 'File Explorer'
  ClientHeight = 589
  ClientWidth = 411
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 411
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = Panel1Resize
    object CBRoot: TComboBox
      Left = 8
      Top = 11
      Width = 313
      Height = 21
      TabOrder = 0
      OnCloseUp = CBRootCloseUp
      OnKeyUp = CBRootKeyUp
    end
    object Panel2: TPanel
      Left = 374
      Top = 0
      Width = 37
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object SpeedButton1: TSpeedButton
        Left = 6
        Top = 10
        Width = 23
        Height = 22
        Hint = 'Select folder...'
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
        ParentShowHint = False
        ShowHint = True
        OnClick = SpeedButton1Click
      end
    end
  end
  object Tree1: TTreeView
    Left = 0
    Top = 41
    Width = 411
    Height = 548
    Align = alClient
    Images = ImageList1
    Indent = 19
    TabOrder = 1
    OnChange = Tree1Change
    OnDblClick = Tree1DblClick
  end
  object PopupFolder: TPopupMenu
    OnPopup = PopupFolderPopup
    Left = 200
    Top = 304
    object New1: TMenuItem
      Caption = '&New'
      object Vidimodule1: TMenuItem
        Caption = '&Vidi module...'
        OnClick = Vidimodule1Click
      end
      object Folder1: TMenuItem
        Caption = '&Folder...'
        OnClick = Folder1Click
      end
    end
    object ShowinExplorer1: TMenuItem
      Caption = '&Locate...'
      OnClick = ShowinExplorer1Click
    end
  end
  object PopupModule: TPopupMenu
    OnPopup = PopupModulePopup
    Left = 200
    Top = 376
    object Open1: TMenuItem
      Caption = '&Open'
      OnClick = Open1Click
    end
    object Locate1: TMenuItem
      Caption = '&Locate...'
      OnClick = Locate1Click
    end
  end
  object ImageList1: TImageList
    Left = 104
    Top = 152
    Bitmap = {
      494C010102000800040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000091E7FF0091E7
      FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7
      FF0091E7FF0091E7FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000091E7FF0091E7
      FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7
      FF0091E7FF0091E7FF0000000000000000000000000000000000000000008093
      F9008093F9008093F9008093F9008093F9008093F9008093F9008093F9008093
      F900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000091E7FF0091E7
      FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7
      FF0091E7FF0091E7FF0000000000000000000000000000000000000000008093
      F9008093F9008093F9008093F9008093F9008093F9008093F9008093F9008093
      F900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000091E7FF0091E7
      FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7
      FF0091E7FF0091E7FF0000000000000000000000000000000000000000008093
      F9008093F900EBCBFA00ADDCFF008093F9008093F900EBCBFA00ADDCFF008093
      F900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000091E7FF0091E7
      FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7
      FF0091E7FF0091E7FF0000000000000000000000000000000000000000008093
      F9009793F900C2DCFC00D6DCFB008093FB009793F900C2DCFC00D6DCFB008093
      FB00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000091E7FF0091E7
      FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7
      FF0091E7FF0091E7FF0000000000000000000000000000000000000000008093
      F900D6B9F90080B9FD00D6B9F90080B9FD00D6B9F90080B9FD00D6B9F90080B9
      FD00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000091E7FF0091E7
      FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7
      FF0091E7FF0091E7FF0000000000000000000000000000000000000000008093
      F900D6DCFB008093FB009793F900C2DCFC00D8DEFC008093FB009793F900C2DC
      FC00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000091E7FF0091E7
      FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7
      FF0091E7FF0091E7FF0000000000000000000000000000000000000000008093
      F9008093F9008093F9008093F9008093F9008093F9008093F9008093F9008093
      F900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000091E7FF0091E7
      FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7FF0091E7
      FF0091E7FF0091E7FF0000000000000000000000000000000000000000008093
      F9008093F9008093F9008093F9008093F9008093F9008093F9008195F9008397
      F900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF00000000FFFFFFFF00000000
      FFFFFFFF00000000C003FFFF00000000C003E00F00000000C003E00F00000000
      C003E00F00000000C003E00F00000000C003E00F00000000C003E00F00000000
      C003E00F00000000C003E00F00000000FFFFFFFF00000000FFFFFFFF00000000
      FFFFFFFF00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
end