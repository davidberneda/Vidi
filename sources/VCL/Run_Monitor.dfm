object FormRunMonitor: TFormRunMonitor
  Left = 0
  Top = 0
  Caption = 'Run Monitor'
  ClientHeight = 572
  ClientWidth = 691
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
    Top = 266
    Width = 691
    Height = 6
    Cursor = crVSplit
    Align = alTop
    ExplicitLeft = 8
    ExplicitTop = 305
    ExplicitWidth = 355
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 691
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 35
      Height = 13
      Caption = 'Status:'
    end
    object Label2: TLabel
      Left = 8
      Top = 32
      Width = 51
      Height = 13
      Caption = 'Instances:'
    end
    object LCount: TLabel
      Left = 65
      Top = 32
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Label3: TLabel
      Left = 8
      Top = 51
      Width = 41
      Height = 13
      Caption = 'Current:'
    end
    object LabelCurrent: TLabel
      Left = 65
      Top = 51
      Width = 3
      Height = 13
    end
    object Label4: TLabel
      Left = 163
      Top = 51
      Width = 24
      Height = 13
      Caption = 'Last:'
    end
    object LabelLast: TLabel
      Left = 195
      Top = 51
      Width = 3
      Height = 13
    end
    object CBStatus: TComboBox
      Left = 49
      Top = 5
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 0
      Text = 'Stopped'
      Items.Strings = (
        'Running'
        'Stopped'
        'Paused')
    end
    object Button1: TButton
      Left = 224
      Top = 3
      Width = 75
      Height = 25
      Caption = 'Refresh'
      TabOrder = 1
      OnClick = Button1Click
    end
    object CBExtraRefresh: TCheckBox
      Left = 312
      Top = 7
      Width = 97
      Height = 17
      Caption = 'Extra Refresh'
      TabOrder = 2
      OnClick = CBExtraRefreshClick
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 73
    Width = 691
    Height = 193
    ActivePage = TabMemory
    Align = alTop
    TabOrder = 1
    object TabItems: TTabSheet
      Caption = 'Instances'
      ImageIndex = 1
      object Splitter2: TSplitter
        Left = 0
        Top = 0
        Width = 6
        Height = 165
      end
    end
    object TabShared: TTabSheet
      Caption = 'Shared'
      ImageIndex = 1
      object Splitter3: TSplitter
        Left = 0
        Top = 0
        Width = 6
        Height = 165
        ExplicitLeft = 8
      end
    end
    object TabMemory: TTabSheet
      Caption = 'Memory'
      ImageIndex = 2
      object Memory: TMemo
        Left = 0
        Top = 34
        Width = 683
        Height = 131
        Align = alClient
        TabOrder = 0
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 683
        Height = 34
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object LMsec: TLabel
          Left = 422
          Top = 8
          Width = 51
          Height = 13
          Caption = '1000 msec'
        end
        object CBAutoRefresh: TCheckBox
          Left = 4
          Top = 8
          Width = 97
          Height = 17
          Caption = 'Auto refresh'
          TabOrder = 0
          OnClick = CBAutoRefreshClick
        end
        object TBRefresh: TTrackBar
          Left = 135
          Top = 8
          Width = 270
          Height = 32
          Max = 20
          Position = 1
          TabOrder = 1
          ThumbLength = 12
          OnChange = TBRefreshChange
        end
      end
    end
  end
  object LBrunStack: TListBox
    Left = 0
    Top = 272
    Width = 691
    Height = 300
    Align = alClient
    ItemHeight = 13
    TabOrder = 2
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 48
    Top = 128
  end
end
