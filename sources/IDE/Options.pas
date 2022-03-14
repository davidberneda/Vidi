unit Options;

interface

uses
  Forms, Menus;

type
  TKeyboardShortcuts=(PascalClassic, VisualCode);

  TOptions=record
  public
    Form : TForm;

    procedure Save;
    procedure Load;

    procedure SetKeyboard(const AKeyboard:TKeyboardShortcuts);

    class function HomePath:String; static;
  end;

implementation

uses
  {$IFDEF FPC}
  LCLProc,  // TextToShortCut
  {$ELSE}
  Windows,
  {$ENDIF}
  Registry, Controls, Unit_TheBee, SysUtils, Bee.Parser, IDE_Lang_English;

const
  Vidi_RegKey='Software\Vidi';

  Key_AST='AST';
  Key_Debugger='Debugger';
  Key_Explorer='Explorer';
  Key_ExplorerPath='Explorer_Path';
  Key_Keyboard='Keyboard';
  Key_Language='Language';
  Key_LastOpened='LastOpened';
  Key_LeftActive='Left_Active';
  Key_Modules='Modules';
  Key_RunMonitor='Run_Monitor';
  Key_Profiler='Profiler';
  Key_Recent='Recent';
  Key_SourcePath='Source';
  Key_Stats='Statistics';
  Key_Theme='Theme';
  Key_Toolbar='Toolbar';
  Key_Transpiler='Transpiler';
  Key_Types='Types';

function ControlName(const AControl:TControl):String;
begin
  if AControl=nil then
     result:=''
  else
     result:=AControl.Name;
end;

class function TOptions.HomePath:String;
var R : TRegistry;
begin
  result:='';

  R:=TRegistry.Create(KEY_READ);
  try
    if R.OpenKey(Vidi_RegKey+'\Main',False) then
       if R.ValueExists(Key_SourcePath) then
          result:=R.ReadString(Key_SourcePath)+'\..';
  finally
    R.Free;
  end;
end;

procedure TOptions.Save;
var R : TRegistry;
    Vidi : TFormVidi;
begin
  Vidi:=TFormVidi(Form);

  R:=TRegistry.Create;
  try
    if R.OpenKey(Vidi_RegKey+'\Main',True) then
    begin
      R.WriteBool('Maximized',Form.WindowState=wsMaximized);

      if Form.Width>0 then
         R.WriteInteger('TreeWidth',Round(1000*Vidi.PageLeft.Width/Vidi.Width));

      if Form.Height>0 then
         R.WriteInteger('LogHeight',Round(1000*Vidi.PanelBottom.Height/Vidi.Height));

      R.WriteBool(Key_AST,Vidi.AST1.Checked);

      R.WriteBool(Key_Explorer,Vidi.FileExplorer1.Checked);

      if Vidi.FormExplorer<>nil then
         R.WriteString(Key_ExplorerPath,Vidi.FormExplorer.Root);

      R.WriteString(Key_LeftActive,ControlName(Vidi.PageLeft.ActivePage));

      R.WriteBool(Key_Debugger,Vidi.Debugger1.Checked);
      R.WriteBool(Key_Modules,Vidi.Modules1.Checked);
      R.WriteString(Key_SourcePath,Vidi.Bee.ParserPath);
      R.WriteBool(Key_Toolbar,Vidi.oolbar1.Checked);
      R.WriteBool(Key_Profiler,Vidi.Profiler1.Checked);
      R.WriteBool(Key_Recent,Vidi.Recent1.Checked);
      R.WriteBool(Key_RunMonitor,Vidi.RunMonitor1.Checked);
      R.WriteBool(Key_Stats,Vidi.Statistics1.Checked);
      R.WriteBool(Key_Transpiler,Vidi.ranspiler1.Checked);
      R.WriteBool(Key_Types,Vidi.ypes1.Checked);

      R.WriteInteger(Key_Language,Ord(Vidi_Language));

      if Vidi.Day1.Checked then
         R.WriteInteger(Key_Theme,Ord(TVidi_Theme.Day))
      else
         R.WriteInteger(Key_Theme,Ord(TVidi_Theme.Night));

      R.WriteString(Key_LastOpened,Vidi.LastOpened);

      if Vidi.PascalClassic1.Checked then
         R.WriteInteger(Key_Keyboard,0)
      else
         R.WriteInteger(Key_Keyboard,1);

    end;
  finally
    R.Free;
  end;
end;

// https://code.visualstudio.com/shortcuts/keyboard-shortcuts-windows.pdf

procedure TOptions.SetKeyboard(const AKeyboard: TKeyboardShortcuts);
var Vidi : TFormVidi;
begin
  Vidi:=TFormVidi(Form);

  if AKeyboard=TKeyboardShortcuts.PascalClassic then
  begin
    Vidi.New1.ShortCut:=TextToShortCut('Ctrl+N');
    Vidi.Open1.ShortCut:=TextToShortCut('F3');
    Vidi.ReOpen1.ShortCut:=TextToShortCut('Ctrl+F3');
    Vidi.Save1.ShortCut:=TextToShortCut('F2');
    Vidi.Saveas1.ShortCut:=TextToShortCut('Ctrl+Shift+S'); // <-- nothing?
    Vidi.SaveAll1.ShortCut:=TextToShortCut('Shift+F2');
    Vidi.Close1.ShortCut:=TextToShortCut('Alt+F3');
    Vidi.CloseAll1.ShortCut:=TextToShortCut('Ctrl+Shift+F4');
    Vidi.Exit1.ShortCut:=TextToShortCut('Alt+F4');

    Vidi.Start1.ShortCut:=TextToShortCut('F9');
    Vidi.Stop1.ShortCut:=TextToShortCut('Ctrl+F2');
    // Vidi.Pause1.ShortCut:=TextToShortCut('');
    Vidi.StepIn1.ShortCut:=TextToShortCut('F7');
    Vidi.StepOver1.ShortCut:=TextToShortCut('F8');
    Vidi.BreakHere1.ShortCut:=TextToShortCut('F6');
    Vidi.ogglebreak1.ShortCut:=TextToShortCut('Ctrl+F8');
  end
  else
  if AKeyboard=TKeyboardShortcuts.VisualCode then
  begin
    Vidi.New1.ShortCut:=TextToShortCut('Ctrl+N');
    Vidi.Open1.ShortCut:=TextToShortCut('Ctrl+O');
    Vidi.ReOpen1.ShortCut:=TextToShortCut('Ctrl+P');
    Vidi.Save1.ShortCut:=TextToShortCut('Ctrl+S');
    Vidi.Saveas1.ShortCut:=TextToShortCut('Ctrl+Shift+S');
//    Vidi.SaveAll1.ShortCut:=TextToShortCut('Ctrl+K  S');
    Vidi.Close1.ShortCut:=TextToShortCut('Ctrl+F4');
//    Vidi.CloseAll1.ShortCut:=TextToShortCut('Ctrl+K Ctrl+W');
    Vidi.Exit1.ShortCut:=TextToShortCut('Alt+F4');

    Vidi.Start1.ShortCut:=TextToShortCut('F5');
    Vidi.Stop1.ShortCut:=TextToShortCut('Shift+F5');
    // Vidi.Pause1.ShortCut:=TextToShortCut('');
    Vidi.StepIn1.ShortCut:=TextToShortCut('F11');
    Vidi.StepOver1.ShortCut:=TextToShortCut('F10');
//    Vidi.BreakHere1.ShortCut:=TextToShortCut('');
    Vidi.ogglebreak1.ShortCut:=TextToShortCut('F9');

  end;
end;

procedure TOptions.Load;
var R : TRegistry;

  procedure ReadMenuItem(const AKey:String; const AMenu:TMenuItem);
  begin
    if R.ValueExists(AKey) then
       AMenu.Checked:=R.ReadBool(AKey);
  end;

var Vidi : TFormVidi;
begin
  Vidi:=TFormVidi(Form);

  R:=TRegistry.Create(KEY_READ);
  try
    if R.OpenKey(Vidi_RegKey+'\Main',False) then
    begin
      if R.ReadBool('Maximized') then
         Form.WindowState:=wsMaximized
      else
         Form.WindowState:=wsNormal;

      if R.ValueExists('TreeWidth') then
         Vidi.TreeWidth:=R.ReadInteger('TreeWidth')
      else
         Vidi.TreeWidth:=0;

      if R.ValueExists('LogHeight') then
         Vidi.LogHeight:=R.ReadInteger('LogHeight')
      else
         Vidi.LogHeight:=0;

      ReadMenuItem(Key_AST,Vidi.AST1);
      ReadMenuItem(Key_Explorer,Vidi.FileExplorer1);

      if R.ValueExists(Key_ExplorerPath) then
         Vidi.InitialExplorerRoot:=R.ReadString(Key_ExplorerPath);

      if R.ValueExists(Key_LeftActive) then
         Vidi.InitialLeftActive:=R.ReadString(Key_LeftActive);

      ReadMenuItem(Key_Modules,Vidi.Modules1);
      ReadMenuItem(Key_Debugger,Vidi.Debugger1);

      if R.ValueExists(Key_SourcePath) then
         Vidi.Bee.ParserPath:=R.ReadString(Key_SourcePath);

      ReadMenuItem(Key_RunMonitor,Vidi.RunMonitor1);
      ReadMenuItem(Key_Toolbar,Vidi.oolbar1);
      ReadMenuItem(Key_Transpiler,Vidi.ranspiler1);
      ReadMenuItem(Key_Profiler,Vidi.Profiler1);
      ReadMenuItem(Key_Recent,Vidi.Recent1);
      ReadMenuItem(Key_Stats,Vidi.Statistics1);
      ReadMenuItem(Key_Types,Vidi.ypes1);

      if R.ValueExists(Key_Language) then
      begin
        Vidi_Language:=TVidi_Languages(R.ReadInteger(Key_Language));

        case Vidi_Language of
          TVidi_Languages.English: Vidi.English1.Checked:=True;
          TVidi_Languages.Catalan: Vidi.Catalan1.Checked:=True;
          TVidi_Languages.Spanish: Vidi.Spanish1.Checked:=True;
        end;
      end;

      if R.ValueExists(Key_Theme) then
         Vidi.ChangeTheme(TVidi_Theme(R.ReadInteger(Key_Theme)));

      if R.ValueExists(Key_LastOpened) then
         Vidi.LastOpenedFile:=R.ReadString(Key_LastOpened);

      if R.ValueExists(Key_Keyboard) then
      case R.ReadInteger(Key_Keyboard) of
        0: Vidi.PascalClassic1Click(Vidi);
        1: Vidi.VisualStudioCode1Click(Vidi);
      end;

      R.CloseKey;
    end;
  finally
    R.Free;
  end;
end;

end.
