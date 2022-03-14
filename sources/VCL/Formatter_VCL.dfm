object FormFormatter: TFormFormatter
  Left = 0
  Top = 0
  Caption = 'Code Formatter'
  ClientHeight = 512
  ClientWidth = 695
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
    Width = 169
    Height = 512
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object RGBlock: TRadioGroup
      Left = 8
      Top = 8
      Width = 153
      Height = 49
      Caption = 'Code Block'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        '{ }'
        'begin end')
      TabOrder = 0
      OnClick = RGBlockClick
    end
    object RGAssign: TRadioGroup
      Left = 8
      Top = 63
      Width = 153
      Height = 49
      Caption = 'Assignment / Equal'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        ':=  ='
        '= ==')
      TabOrder = 1
      OnClick = RGAssignClick
    end
    object RGIfThen: TRadioGroup
      Left = 8
      Top = 118
      Width = 153
      Height = 49
      Caption = 'If Then'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        '(empty)'
        'then')
      TabOrder = 2
      OnClick = RGIfThenClick
    end
    object RGNotEqual: TRadioGroup
      Left = 8
      Top = 173
      Width = 153
      Height = 49
      Caption = 'Not Equal'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        '<>'
        '!=')
      TabOrder = 3
      OnClick = RGNotEqualClick
    end
  end
end
