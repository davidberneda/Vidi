object FormModuleUsage: TFormModuleUsage
  Left = 0
  Top = 0
  ClientHeight = 364
  ClientWidth = 354
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
    Left = 154
    Top = 25
    Width = 6
    Height = 339
    OnMoved = Splitter1Moved
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 354
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object LabelUses: TLabel
      Left = 8
      Top = 8
      Width = 27
      Height = 13
      Caption = 'Uses'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object LabelUsedBy: TLabel
      Left = 160
      Top = 8
      Width = 45
      Height = 13
      Caption = 'Used by'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object LBUses: TListBox
    Left = 0
    Top = 25
    Width = 154
    Height = 339
    Align = alLeft
    ItemHeight = 13
    TabOrder = 1
    OnDblClick = LBUsesDblClick
  end
  object LBUsedBy: TListBox
    Left = 160
    Top = 25
    Width = 194
    Height = 339
    Align = alClient
    ItemHeight = 13
    TabOrder = 2
    OnDblClick = LBUsedByDblClick
  end
end
