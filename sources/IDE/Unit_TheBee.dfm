object FormVidi: TFormVidi
  Left = 192
  Top = 125
  Caption = 'Vidi'
  ClientHeight = 587
  ClientWidth = 1021
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  Position = poScreenCenter
  WindowState = wsMaximized
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 470
    Width = 1021
    Height = 8
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 338
  end
  object Splitter3: TSplitter
    Left = 361
    Top = 0
    Width = 8
    Height = 470
    ExplicitHeight = 338
  end
  object PageLeft: TPageControl
    Left = 0
    Top = 0
    Width = 361
    Height = 470
    ActivePage = TabModules
    Align = alLeft
    TabOrder = 0
    OnChange = PageLeftChange
    object TabTypes: TTabSheet
      Caption = 'Types'
    end
    object TabAST: TTabSheet
      Caption = 'AST'
      TabVisible = False
    end
    object TabModules: TTabSheet
      Caption = 'Modules'
    end
    object TabDebugger: TTabSheet
      Caption = 'Debug'
      TabVisible = False
    end
    object TabTranspiler: TTabSheet
      Caption = 'Transpiler'
      ImageIndex = 4
      TabVisible = False
    end
    object TabFormatter: TTabSheet
      Caption = 'Formatter'
      ImageIndex = 5
      TabVisible = False
    end
    object TabFileExplorer: TTabSheet
      Caption = 'Explorer'
      ImageIndex = 6
      TabVisible = False
    end
    object TabRunMonitor: TTabSheet
      Caption = 'Run Monitor'
      ImageIndex = 7
      TabVisible = False
    end
    object TabProfiler: TTabSheet
      Caption = 'Profiler'
      ImageIndex = 8
      TabVisible = False
    end
    object TabStats: TTabSheet
      Caption = 'Statistics'
      ImageIndex = 9
      TabVisible = False
    end
    object TabRecent: TTabSheet
      Caption = 'Recent'
      ImageIndex = 10
      TabVisible = False
    end
  end
  object PanelCode: TPanel
    Left = 369
    Top = 0
    Width = 652
    Height = 470
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object PageCode: TPageControl
      Left = 0
      Top = 0
      Width = 652
      Height = 446
      Align = alClient
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnChange = PageCodeChange
    end
    object PanelStatus: TPanel
      Left = 0
      Top = 446
      Width = 652
      Height = 24
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object CodeStatus1: TPanel
        Left = 0
        Top = 0
        Width = 50
        Height = 24
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        OnMouseUp = CodeStatusMouseUp
      end
      object CodeStatus2: TPanel
        Left = 50
        Top = 0
        Width = 87
        Height = 24
        Align = alLeft
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
        OnMouseUp = CodeStatusMouseUp
      end
      object PanelJumps: TPanel
        Left = 137
        Top = 0
        Width = 86
        Height = 24
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 1
        object JumpLeft: TSpeedButton
          Left = 9
          Top = 1
          Width = 23
          Height = 22
          Hint = 'Go to previous'
          Caption = '<'
          Enabled = False
          Flat = True
          OnClick = JumpLeftClick
        end
        object JumpList: TSpeedButton
          Left = 35
          Top = 1
          Width = 23
          Height = 22
          Hint = 'List of jumps'
          Caption = '...'
          Enabled = False
          Flat = True
          OnClick = JumpListClick
        end
        object JumpRight: TSpeedButton
          Left = 61
          Top = 1
          Width = 23
          Height = 22
          Hint = 'Go to next'
          Caption = '>'
          Enabled = False
          Flat = True
          OnClick = JumpRightClick
        end
      end
      object PanelAddress: TPanel
        Left = 223
        Top = 0
        Width = 360
        Height = 24
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 2
      end
      object PanelFont: TPanel
        Left = 583
        Top = 0
        Width = 69
        Height = 24
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 4
        object BFontUp: TButton
          Left = 8
          Top = 0
          Width = 24
          Height = 25
          Caption = '+'
          TabOrder = 0
          OnClick = BFontUpClick
        end
        object BFontDown: TButton
          Left = 38
          Top = 0
          Width = 24
          Height = 25
          Caption = '-'
          TabOrder = 1
          OnClick = BFontDownClick
        end
      end
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 478
    Width = 1021
    Height = 109
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Splitter4: TSplitter
      Left = 315
      Top = 0
      Width = 6
      Height = 109
      Align = alRight
      ExplicitLeft = 0
    end
    object PageBottom: TPageControl
      Left = 0
      Top = 0
      Width = 315
      Height = 109
      ActivePage = TabErrors
      Align = alClient
      TabOrder = 0
      OnChange = PageBottomChange
      object TabErrors: TTabSheet
        Caption = 'Errors'
      end
      object TabPrompt: TTabSheet
        Caption = 'Prompt'
      end
      object TabConsole: TTabSheet
        Caption = 'Console'
      end
    end
    object PageRight: TPageControl
      Left = 321
      Top = 0
      Width = 700
      Height = 109
      ActivePage = TabSearch
      Align = alRight
      TabOrder = 1
      OnChange = PageRightChange
      object TabSearch: TTabSheet
        Caption = 'Search'
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 100
    Top = 50
    object File1: TMenuItem
      Caption = '&File'
      object New1: TMenuItem
        Caption = '&New...'
        ShortCut = 16462
        OnClick = New1Click
      end
      object Open1: TMenuItem
        Caption = '&Open...'
        ShortCut = 114
        OnClick = Open1Click
      end
      object Reopen1: TMenuItem
        Caption = '&Reopen...'
        ShortCut = 16498
        Visible = False
        OnClick = Reopen1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Save1: TMenuItem
        Caption = '&Save'
        ShortCut = 113
        OnClick = Save1Click
      end
      object Saveas1: TMenuItem
        Caption = 'Sa&ve as...'
        OnClick = Saveas1Click
      end
      object SaveAll1: TMenuItem
        Caption = 'Save &All'
        ShortCut = 8305
        OnClick = SaveAll1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Caption = '&Close'
        Enabled = False
        ShortCut = 32882
        OnClick = Close1Click
      end
      object CloseAll1: TMenuItem
        Caption = 'Close A&ll'
        Enabled = False
        OnClick = CloseAll1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = '&Exit'
        ShortCut = 32883
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      OnClick = Edit1Click
      object Cut1: TMenuItem
        Caption = 'C&ut'
        ShortCut = 16472
        OnClick = Cut1Click
      end
      object Copy1: TMenuItem
        Caption = '&Copy'
        ShortCut = 16451
        OnClick = Copy1Click
      end
      object Paste1: TMenuItem
        Caption = '&Paste'
        ShortCut = 16470
        OnClick = Paste1Click
      end
      object Delete1: TMenuItem
        Caption = '&Delete'
        ShortCut = 16430
        OnClick = Delete1Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object SelectAll1: TMenuItem
        Caption = 'Select &All'
        ShortCut = 16449
        OnClick = SelectAll1Click
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Undo1: TMenuItem
        Caption = '&Undo'
        ShortCut = 32776
        OnClick = Undo1Click
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object Search1: TMenuItem
        Caption = '&Search...'
        ShortCut = 16454
        OnClick = Search1Click
      end
    end
    object Options1: TMenuItem
      Caption = '&Options'
      OnClick = Options1Click
      object Font1: TMenuItem
        Caption = '&Font'
        object Code1: TMenuItem
          Caption = '&Editor...'
          OnClick = Code1Click
        end
        object Run2: TMenuItem
          Caption = '&Console...'
          OnClick = Run2Click
        end
      end
      object Keyboard1: TMenuItem
        Caption = '&Keyboard'
        object PascalClassic1: TMenuItem
          Caption = '&Pascal Classic'
          Checked = True
          RadioItem = True
          OnClick = PascalClassic1Click
        end
        object VisualStudioCode1: TMenuItem
          Caption = '&Visual Studio Code'
          RadioItem = True
          OnClick = VisualStudioCode1Click
        end
      end
      object Language1: TMenuItem
        Caption = '&Language'
        object English1: TMenuItem
          Caption = '&English'
          Checked = True
          RadioItem = True
          OnClick = English1Click
        end
        object Catalan1: TMenuItem
          Caption = '&Catal'#224
          RadioItem = True
          OnClick = Catalan1Click
        end
        object Spanish1: TMenuItem
          Caption = 'E&spa'#241'ol'
          RadioItem = True
          OnClick = Spanish1Click
        end
      end
      object hemes1: TMenuItem
        Caption = '&Themes'
        object Day1: TMenuItem
          Caption = '&Day'
          Checked = True
          RadioItem = True
          OnClick = Day1Click
        end
        object Night1: TMenuItem
          Caption = '&Night'
          RadioItem = True
          OnClick = Night1Click
        end
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object Internal1: TMenuItem
        Caption = '&Internal'
        object AST1: TMenuItem
          Caption = '&AST'
          OnClick = AST1Click
        end
        object Casesensitive1: TMenuItem
          Caption = '&Case sensitive'
          OnClick = Casesensitive1Click
        end
        object Formatter1: TMenuItem
          Caption = '&Formatter'
          OnClick = Formatter1Click
        end
        object Highlightcode1: TMenuItem
          Caption = '&Highlight code'
          Checked = True
          OnClick = Highlightcode1Click
        end
        object LogErrors1: TMenuItem
          Caption = '&Log Errors'
          ShortCut = 49228
          OnClick = LogErrors1Click
        end
        object RunMonitor1: TMenuItem
          Caption = '&Run Monitor'
          OnClick = RunMonitor1Click
        end
        object ranspiler1: TMenuItem
          Caption = '&Transpiler'
          ShortCut = 16507
          OnClick = ranspiler1Click
        end
      end
    end
    object View1: TMenuItem
      Caption = '&View'
      object Debugger1: TMenuItem
        Caption = '&Debugger'
        OnClick = Debugger1Click
      end
      object FileExplorer1: TMenuItem
        Caption = '&File Explorer'
        OnClick = FileExplorer1Click
      end
      object Log1: TMenuItem
        Caption = '&Log'
        Checked = True
        OnClick = Log1Click
      end
      object Modules1: TMenuItem
        Caption = '&Modules'
        Checked = True
        OnClick = Modules1Click
      end
      object Profiler1: TMenuItem
        Caption = '&Profiler'
        OnClick = Profiler1Click
      end
      object Recent1: TMenuItem
        Caption = '&Recent '
        OnClick = Recent1Click
      end
      object Statistics1: TMenuItem
        Caption = '&Statistics'
        OnClick = Statistics1Click
      end
      object ypes1: TMenuItem
        Caption = '&Types'
        Checked = True
        OnClick = ypes1Click
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object oolbar1: TMenuItem
        Caption = 'T&oolbar'
        Checked = True
        OnClick = oolbar1Click
      end
    end
    object Run1: TMenuItem
      Caption = 'E&xecute'
      OnClick = Run1Click
      object Start1: TMenuItem
        Caption = '&Start'
        ShortCut = 120
        OnClick = Start1Click
      end
      object Stop1: TMenuItem
        Caption = 'St&op'
        Enabled = False
        ShortCut = 16497
        OnClick = Stop1Click
      end
      object Pause1: TMenuItem
        Caption = 'P&ause'
        Enabled = False
        OnClick = Pause1Click
      end
      object Stepin1: TMenuItem
        Caption = 'St&ep in'
        ShortCut = 118
        OnClick = Stepin1Click
      end
      object Stepover1: TMenuItem
        Caption = 'Step o&ver'
        ShortCut = 119
        OnClick = Stepover1Click
      end
      object N15: TMenuItem
        Caption = '-'
      end
      object Breakhere1: TMenuItem
        Caption = '&Break here'
        ShortCut = 117
        OnClick = Breakhere1Click
      end
      object ogglebreak1: TMenuItem
        Caption = '&Toggle break'
        Enabled = False
        ShortCut = 16503
        OnClick = ogglebreak1Click
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object Prompt1: TMenuItem
        Caption = '&Prompt'
        ShortCut = 16499
        OnClick = Prompt1Click
      end
      object Compileonly1: TMenuItem
        Caption = '&Compile'
        ShortCut = 116
        OnClick = Compileonly1Click
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object LanguageReference1: TMenuItem
        Caption = '&Language Reference...'
        OnClick = LanguageReference1Click
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Caption = '&About...'
        OnClick = About1Click
      end
    end
    object UPDATE1: TMenuItem
      Caption = 'UPDATE !'
      Visible = False
      OnClick = UPDATE1Click
    end
  end
  object PopupEditor: TPopupMenu
    Left = 30
    Top = 90
    object Close2: TMenuItem
      Caption = '&Close'
      OnClick = Close2Click
    end
  end
  object PopupEdit: TPopupMenu
    OnPopup = PopupEditPopup
    Left = 30
    Top = 170
    object Cut2: TMenuItem
      Caption = 'C&ut'
      ShortCut = 16472
      OnClick = Cut1Click
    end
    object Copy2: TMenuItem
      Caption = '&Copy'
      ShortCut = 16451
      OnClick = Copy1Click
    end
    object Paste2: TMenuItem
      Caption = '&Paste'
      ShortCut = 16470
      OnClick = Paste1Click
    end
    object Delete2: TMenuItem
      Caption = '&Delete'
      ShortCut = 16430
      OnClick = Delete1Click
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object SelectAll2: TMenuItem
      Caption = 'Select &All'
      ShortCut = 16449
      OnClick = SelectAll1Click
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object Undo2: TMenuItem
      Caption = '&Undo'
      OnClick = Undo1Click
    end
    object N14: TMenuItem
      Caption = '-'
    end
    object Runandstophere1: TMenuItem
      Caption = '&Run and stop here'
      ShortCut = 115
      OnClick = Runandstophere1Click
    end
    object N18: TMenuItem
      Caption = '-'
    end
    object extformat1: TMenuItem
      Caption = 'Text format'
      object ANSI1: TMenuItem
        Caption = 'ANSI'
        Checked = True
        RadioItem = True
        OnClick = ANSI1Click
      end
      object Unicode1: TMenuItem
        Caption = 'Unicode'
        RadioItem = True
        OnClick = Unicode1Click
      end
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 45
    Top = 250
  end
  object PopupJumps: TPopupMenu
    Left = 128
    Top = 120
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 500
    OnTimer = Timer1Timer
    Left = 128
    Top = 200
  end
  object LogMenu: TPopupMenu
    Left = 32
    Top = 432
    object Copy3: TMenuItem
      Caption = '&Copy'
      OnClick = Copy3Click
    end
    object N13: TMenuItem
      Caption = '-'
    end
    object Clear1: TMenuItem
      Caption = 'Cl&ear'
      OnClick = Clear1Click
    end
  end
  object TimerUpdate: TTimer
    Interval = 50000
    OnTimer = TimerUpdateTimer
    Left = 56
    Top = 320
  end
end
