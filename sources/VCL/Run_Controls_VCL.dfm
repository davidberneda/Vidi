object FormRunControls: TFormRunControls
  Left = 0
  Top = 0
  Caption = 'Run Controls'
  ClientHeight = 158
  ClientWidth = 220
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Tool_Run: TImage
    Left = 11
    Top = 8
    Width = 32
    Height = 32
    Cursor = crHandPoint
    Hint = 'Run'
    PopupMenu = PopupRun
    Stretch = True
  end
  object Tool_Pause: TImage
    Left = 49
    Top = 8
    Width = 32
    Height = 32
    Cursor = crHandPoint
    Hint = 'Pause'
    Enabled = False
    Stretch = True
  end
  object Tool_Stop: TImage
    Left = 87
    Top = 8
    Width = 32
    Height = 32
    Cursor = crHandPoint
    Hint = 'Stop'
    Enabled = False
    Stretch = True
  end
  object Tool_Stepin: TImage
    Left = 135
    Top = 8
    Width = 32
    Height = 32
    Cursor = crHandPoint
    Hint = 'Step in '
    Stretch = True
  end
  object Tool_Stepover: TImage
    Left = 173
    Top = 8
    Width = 32
    Height = 32
    Cursor = crHandPoint
    Hint = 'Step over'
    Stretch = True
  end
  object PopupRun: TPopupMenu
    Left = 56
    Top = 64
    object Runwithoutdebug1: TMenuItem
      Caption = 'Run without debug'
    end
  end
end
