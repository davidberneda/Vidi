object FormBreaks: TFormBreaks
  Left = 0
  Top = 0
  Caption = 'FormBreaks'
  ClientHeight = 157
  ClientWidth = 559
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 368
    Top = 0
    Width = 6
    Height = 157
    Align = alRight
    Visible = False
    ExplicitLeft = 143
    ExplicitTop = -8
  end
  object LBBreaks: TListBox
    Left = 0
    Top = 0
    Width = 368
    Height = 157
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
    OnClick = LBBreaksClick
    OnDblClick = LBBreaksDblClick
    ExplicitWidth = 106
  end
  object PanelBreak: TPanel
    Left = 374
    Top = 0
    Width = 185
    Height = 157
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    ExplicitLeft = 112
    object Label1: TLabel
      Left = 13
      Top = 54
      Width = 49
      Height = 13
      Caption = '&Condition:'
      FocusControl = ECondition
    end
    object CBEnabled: TCheckBox
      Left = 13
      Top = 8
      Width = 131
      Height = 17
      Caption = '&Enabled'
      TabOrder = 0
      OnClick = CBEnabledClick
    end
    object CBStop: TCheckBox
      Left = 13
      Top = 31
      Width = 147
      Height = 17
      Caption = '&Stop execution'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CBStopClick
    end
    object CBOutput: TCheckBox
      Left = 13
      Top = 99
      Width = 131
      Height = 17
      Caption = '&Output expression:'
      TabOrder = 2
      OnClick = CBOutputClick
    end
    object EExpression: TEdit
      Left = 13
      Top = 122
      Width = 140
      Height = 21
      Enabled = False
      TabOrder = 3
      OnChange = EExpressionChange
    end
    object ECondition: TEdit
      Left = 13
      Top = 73
      Width = 140
      Height = 21
      TabOrder = 4
      OnChange = EConditionChange
    end
  end
end
