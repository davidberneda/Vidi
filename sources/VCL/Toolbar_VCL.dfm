object FormToolbar: TFormToolbar
  Left = 95
  Top = 160
  Caption = 'Toolbar'
  ClientHeight = 55
  ClientWidth = 992
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
  object Toolbar: TPanel
    Left = 0
    Top = 0
    Width = 992
    Height = 55
    Align = alClient
    BevelOuter = bvNone
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object PanelButtons: TPanel
      Left = 0
      Top = 0
      Width = 465
      Height = 55
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object Tool_New: TImage
        Left = 9
        Top = 3
        Width = 32
        Height = 32
        Cursor = crHandPoint
        Hint = 'New'
        Stretch = True
      end
      object Tool_Open: TImage
        Left = 47
        Top = 3
        Width = 32
        Height = 32
        Cursor = crHandPoint
        Hint = 'Open'
        Stretch = True
      end
      object Tool_Save: TImage
        Left = 85
        Top = 3
        Width = 32
        Height = 32
        Cursor = crHandPoint
        Hint = 'Save'
        Stretch = True
      end
      object Tool_SaveAll: TImage
        Left = 123
        Top = 3
        Width = 32
        Height = 32
        Cursor = crHandPoint
        Hint = 'Save All'
        Stretch = True
      end
      object Tool_Search: TImage
        Left = 171
        Top = 3
        Width = 32
        Height = 32
        Cursor = crHandPoint
        Hint = 'Search'
        Stretch = True
      end
      object Tool_Compile: TImage
        Left = 433
        Top = 3
        Width = 32
        Height = 32
        Cursor = crHandPoint
        Hint = 'Compile'
        Stretch = True
      end
      object Tool_Prompt: TImage
        Left = 230
        Top = 3
        Width = 32
        Height = 32
        Cursor = crHandPoint
        Hint = 'Console'
        Stretch = True
      end
      object Tool_Explorer: TImage
        Left = 268
        Top = 3
        Width = 32
        Height = 32
        Cursor = crHandPoint
        Hint = 'Explorer'
        ParentShowHint = False
        ShowHint = True
        Stretch = True
      end
      object Tool_Log: TImage
        Left = 306
        Top = 3
        Width = 32
        Height = 32
        Cursor = crHandPoint
        Hint = 'Error Log'
        Stretch = True
      end
      object Tool_Modules: TImage
        Left = 344
        Top = 3
        Width = 32
        Height = 32
        Cursor = crHandPoint
        Hint = 'Modules'
        Stretch = True
      end
      object Tool_Types: TImage
        Left = 382
        Top = 3
        Width = 32
        Height = 32
        Cursor = crHandPoint
        Hint = 'Types'
        Stretch = True
      end
    end
    object PanelRun: TPanel
      Left = 465
      Top = 0
      Width = 319
      Height = 55
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 471
    end
  end
end
