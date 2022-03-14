object FormProfiler: TFormProfiler
  Left = 0
  Top = 0
  Caption = 'Profiler'
  ClientHeight = 429
  ClientWidth = 541
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
    Top = 242
    Width = 541
    Height = 6
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 245
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 541
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object CBEnabled: TCheckBox
      Left = 16
      Top = 13
      Width = 92
      Height = 17
      Caption = 'Enabled'
      TabOrder = 0
      OnClick = CBEnabledClick
    end
    object CBCoverage: TCheckBox
      Left = 112
      Top = 13
      Width = 97
      Height = 17
      Caption = 'Coverage'
      Enabled = False
      TabOrder = 1
      OnClick = CBCoverageClick
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 248
    Width = 541
    Height = 181
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
  end
  object PageTop: TPageControl
    Left = 0
    Top = 41
    Width = 541
    Height = 201
    ActivePage = TabResults
    Align = alClient
    TabOrder = 2
    object TabResults: TTabSheet
      Caption = 'Results'
      object Panel2: TPanel
        Left = 0
        Top = 147
        Width = 533
        Height = 26
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitTop = 146
        object LTotals: TLabel
          Left = 4
          Top = 8
          Width = 6
          Height = 13
          Caption = '0'
        end
      end
    end
    object TabCoverage: TTabSheet
      Caption = 'Coverage'
      ImageIndex = 1
    end
  end
end
