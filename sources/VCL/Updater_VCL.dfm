object Updater: TUpdater
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Updater'
  ClientHeight = 142
  ClientWidth = 396
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LStatus: TLabel
    Left = 24
    Top = 21
    Width = 61
    Height = 13
    Caption = 'Downloading'
  end
  object LPercent: TLabel
    Left = 320
    Top = 21
    Width = 49
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0%'
  end
  object ProgressBar1: TProgressBar
    Left = 24
    Top = 40
    Width = 345
    Height = 17
    TabOrder = 0
  end
  object BCancel: TButton
    Left = 160
    Top = 80
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 1
    OnClick = BCancelClick
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 10
    OnTimer = Timer1Timer
    Left = 24
    Top = 72
  end
end
