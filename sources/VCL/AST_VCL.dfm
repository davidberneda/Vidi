object FormAST: TFormAST
  Left = 24
  Top = 253
  Caption = 'FormAST'
  ClientHeight = 436
  ClientWidth = 541
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 393
    Width = 541
    Height = 43
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object LClass: TLabel
      Left = 8
      Top = 8
      Width = 3
      Height = 13
      Color = clBtnFace
      ParentColor = False
    end
    object LOwner: TLabel
      Left = 8
      Top = 22
      Width = 3
      Height = 13
      Cursor = crHandPoint
      Color = clBtnFace
      ParentColor = False
      OnClick = LOwnerClick
    end
    object LPosition: TLabel
      Left = 165
      Top = 8
      Width = 16
      Height = 13
      Caption = '0,0'
      Color = clBtnFace
      ParentColor = False
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 541
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 14
      Width = 37
      Height = 13
      Caption = '&Search:'
      Color = clBtnFace
      FocusControl = CBSearch
      ParentColor = False
    end
    object SBSearch: TSpeedButton
      Left = 202
      Top = 11
      Width = 23
      Height = 22
      Hint = 'Search'
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
      ParentShowHint = False
      ShowHint = True
      OnClick = SBSearchClick
    end
    object CBSearch: TComboBox
      Left = 51
      Top = 11
      Width = 145
      Height = 21
      TabOrder = 0
      OnChange = CBSearchChange
      OnKeyUp = CBSearchKeyUp
    end
    object TestButton: TButton
      Left = 231
      Top = 8
      Width = 60
      Height = 25
      Caption = 'Test'
      TabOrder = 1
      OnClick = TestButtonClick
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 41
    Width = 541
    Height = 352
    ActivePage = TabTree
    Align = alClient
    TabOrder = 2
    OnChange = PageControl1Change
    object TabTree: TTabSheet
      Caption = 'Tree'
    end
    object TabMap: TTabSheet
      Caption = 'Map'
      ImageIndex = 1
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 533
        Height = 35
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object LCount: TLabel
          Left = 161
          Top = 11
          Width = 42
          Height = 13
          Caption = 'Count: 0'
          Color = clBtnFace
          ParentColor = False
        end
        object CBFilter: TComboBox
          Left = 4
          Top = 8
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 0
          Text = 'All'
          OnChange = CBFilterChange
          Items.Strings = (
            'All'
            'Comments'
            'Identifiers'
            'Keywords'
            'Literals'
            'Symbols')
        end
        object Button1: TButton
          Left = 288
          Top = 8
          Width = 75
          Height = 25
          Caption = 'Check'
          TabOrder = 1
          OnClick = Button1Click
        end
      end
    end
    object TabReferences: TTabSheet
      Caption = 'References'
      ImageIndex = 2
      object Splitter1: TSplitter
        Left = 185
        Top = 0
        Width = 6
        Height = 324
        ExplicitHeight = 326
      end
      object PanelUsages: TPanel
        Left = 0
        Top = 0
        Width = 185
        Height = 324
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
      end
    end
  end
end
