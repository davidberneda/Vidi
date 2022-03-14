object FormRecent: TFormRecent
  Left = 0
  Top = 0
  ActiveControl = EFilter
  Caption = 'Recent Files'
  ClientHeight = 494
  ClientWidth = 603
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelBottom: TPanel
    Left = 0
    Top = 453
    Width = 603
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Panel2: TPanel
      Left = 360
      Top = 0
      Width = 243
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object BOK: TButton
        Left = 72
        Top = 8
        Width = 75
        Height = 25
        Caption = 'OK'
        Enabled = False
        ModalResult = 1
        TabOrder = 0
      end
      object BCancel: TButton
        Left = 160
        Top = 8
        Width = 75
        Height = 25
        Cancel = True
        Caption = '&Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 603
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object LFilter: TLabel
      Left = 8
      Top = 14
      Width = 28
      Height = 13
      Caption = 'Filter:'
    end
    object EFilter: TEdit
      Left = 56
      Top = 11
      Width = 217
      Height = 21
      TabOrder = 0
      OnChange = EFilterChange
      OnKeyUp = EFilterKeyUp
    end
    object CBViewFolders: TCheckBox
      Left = 294
      Top = 13
      Width = 97
      Height = 17
      Caption = 'View Folders'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CBViewFoldersClick
    end
  end
  object PopupMenuGrid: TPopupMenu
    OnPopup = PopupMenuGridPopup
    Left = 200
    Top = 112
    object LocateFolder1: TMenuItem
      Caption = 'Locate Folder...'
      OnClick = LocateFolder1Click
    end
    object Remove1: TMenuItem
      Caption = '&Remove from list'
      OnClick = Remove1Click
    end
  end
end
