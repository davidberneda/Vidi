object FormTrace: TFormTrace
  Left = 0
  Top = 0
  Caption = 'FormTrace'
  ClientHeight = 334
  ClientWidth = 413
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LBTrace: TListBox
    Left = 0
    Top = 0
    Width = 413
    Height = 334
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = LBTraceDblClick
  end
end
