unit Unit_TheBee;

{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}

{$IFDEF INTERNAL}
{.$DEFINE INTERNAL_TIMING}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$IFNDEF FPC}
  Messages,
  {$ENDIF}
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Menus, Buttons,

  Sys, AST, Parser, Exceptions, Map, Bee.Parser, Editor,

  TheBee_VCL, Grid_VCL,
  Prompt_VCL, Modules_VCL,
  Options, Completion_VCL, Evaluator,
  Types_VCL, Run_VCL, Debug_VCL, Search_VCL, AST_VCL, Run_Breaks,
  Transpiler_VCL, Formatter_VCL, Explorer_VCL,
  Run_Monitor, Toolbar_VCL, Run_Controls_VCL, Profiler_VCL,
  Stats_VCL, RecentFiles_VCL;

type
  TVidi_Theme=(Day,Night);

  { TFormVidi }

  TFormVidi = class(TForm)
    Help1: TMenuItem;
    PageCode: TPageControl;
    Splitter1: TSplitter;
    Splitter3: TSplitter;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Edit1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAll1: TMenuItem;
    N1: TMenuItem;
    Close1: TMenuItem;
    CloseAll1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    N3: TMenuItem;
    Reopen1: TMenuItem;
    PopupEditor: TPopupMenu;
    Close2: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    N4: TMenuItem;
    SelectAll1: TMenuItem;
    PopupEdit: TPopupMenu;
    Cut2: TMenuItem;
    Copy2: TMenuItem;
    Paste2: TMenuItem;
    Delete2: TMenuItem;
    N5: TMenuItem;
    SelectAll2: TMenuItem;
    Options1: TMenuItem;
    hemes1: TMenuItem;
    N6: TMenuItem;
    Undo1: TMenuItem;
    N7: TMenuItem;
    Undo2: TMenuItem;
    View1: TMenuItem;
    ypes1: TMenuItem;
    Log1: TMenuItem;
    PageLeft: TPageControl;
    TabAST: TTabSheet;
    Font1: TMenuItem;
    FontDialog1: TFontDialog;
    TabModules: TTabSheet;
    TabTypes: TTabSheet;
    LogErrors1: TMenuItem;
    N9: TMenuItem;
    Search1: TMenuItem;
    Run1: TMenuItem;
    Start1: TMenuItem;
    N10: TMenuItem;
    Prompt1: TMenuItem;
    Saveas1: TMenuItem;
    AST1: TMenuItem;
    Stop1: TMenuItem;
    Pause1: TMenuItem;
    Stepin1: TMenuItem;
    Modules1: TMenuItem;
    TabDebugger: TTabSheet;
    Language1: TMenuItem;
    English1: TMenuItem;
    Catalan1: TMenuItem;
    Spanish1: TMenuItem;
    Code1: TMenuItem;
    Run2: TMenuItem;
    About1: TMenuItem;
    LanguageReference1: TMenuItem;
    N12: TMenuItem;
    Compileonly1: TMenuItem;
    N8: TMenuItem;
    Internal1: TMenuItem;
    Casesensitive1: TMenuItem;
    PanelCode: TPanel;
    PanelStatus: TPanel;
    CodeStatus1: TPanel;
    CodeStatus2: TPanel;
    PopupJumps: TPopupMenu;
    ranspiler1: TMenuItem;
    TabTranspiler: TTabSheet;
    Formatter1: TMenuItem;
    TabFormatter: TTabSheet;
    Timer1: TTimer;
    FileExplorer1: TMenuItem;
    TabFileExplorer: TTabSheet;
    N11: TMenuItem;
    oolbar1: TMenuItem;
    LogMenu: TPopupMenu;
    Copy3: TMenuItem;
    N13: TMenuItem;
    Clear1: TMenuItem;
    Highlightcode1: TMenuItem;
    PanelJumps: TPanel;
    JumpLeft: TSpeedButton;
    JumpList: TSpeedButton;
    JumpRight: TSpeedButton;
    PanelAddress: TPanel;
    Debugger1: TMenuItem;
    RunMonitor1: TMenuItem;
    TabRunMonitor: TTabSheet;
    Stepover1: TMenuItem;
    Profiler1: TMenuItem;
    TabProfiler: TTabSheet;
    Day1: TMenuItem;
    Night1: TMenuItem;
    N14: TMenuItem;
    Runandstophere1: TMenuItem;
    Breakhere1: TMenuItem;
    N15: TMenuItem;
    ogglebreak1: TMenuItem;
    Keyboard1: TMenuItem;
    PascalClassic1: TMenuItem;
    VisualStudioCode1: TMenuItem;
    Statistics1: TMenuItem;
    TabStats: TTabSheet;
    Recent1: TMenuItem;
    TabRecent: TTabSheet;
    PanelFont: TPanel;
    BFontUp: TButton;
    BFontDown: TButton;
    PanelBottom: TPanel;
    PageBottom: TPageControl;
    TabErrors: TTabSheet;
    TabPrompt: TTabSheet;
    TabConsole: TTabSheet;
    PageRight: TPageControl;
    TabSearch: TTabSheet;
    Splitter4: TSplitter;
    TimerUpdate: TTimer;
    UPDATE1: TMenuItem;
    N18: TMenuItem;
    extformat1: TMenuItem;
    ANSI1: TMenuItem;
    Unicode1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LogClick(Sender: TObject);
    procedure PageCodeChange(Sender: TObject);
    procedure TreeASTChange(Sender: TObject; const APosition:TNodePosition);
    procedure Close1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure CloseAll1Click(Sender: TObject);
    procedure SaveAll1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Close2Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure PopupEditPopup(Sender: TObject);
    procedure Options1Click(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ypes1Click(Sender: TObject);
    procedure Log1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LogErrors1Click(Sender: TObject);
    procedure PageBottomChange(Sender: TObject);
    procedure Search1Click(Sender: TObject);
    procedure Prompt1Click(Sender: TObject);
    procedure Start1Click(Sender: TObject);
    procedure Saveas1Click(Sender: TObject);
    procedure PageLeftChange(Sender: TObject);
    procedure BSaveClick(Sender: TObject);
    procedure AST1Click(Sender: TObject);
    procedure LogDblClick(Sender: TObject);
    procedure Stop1Click(Sender: TObject);
    procedure Pause1Click(Sender: TObject);
    procedure Stepin1Click(Sender: TObject);
    procedure Modules1Click(Sender: TObject);
    procedure English1Click(Sender: TObject);
    procedure Catalan1Click(Sender: TObject);
    procedure Spanish1Click(Sender: TObject);
    procedure Code1Click(Sender: TObject);
    procedure Run2Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure LanguageReference1Click(Sender: TObject);
    procedure Compileonly1Click(Sender: TObject);
    procedure Casesensitive1Click(Sender: TObject);
    procedure JumpLeftClick(Sender: TObject);
    procedure JumpRightClick(Sender: TObject);
    procedure JumpListClick(Sender: TObject);
    procedure ranspiler1Click(Sender: TObject);
    procedure Formatter1Click(Sender: TObject);
    procedure CodeStatusMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LogKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
    procedure FileExplorer1Click(Sender: TObject);
    procedure oolbar1Click(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure Copy3Click(Sender: TObject);
    procedure Highlightcode1Click(Sender: TObject);
    procedure Run1Click(Sender: TObject);
    procedure Reopen1Click(Sender: TObject);
    procedure Debugger1Click(Sender: TObject);
    procedure RunMonitor1Click(Sender: TObject);
    procedure Stepover1Click(Sender: TObject);
    procedure Profiler1Click(Sender: TObject);
    procedure Day1Click(Sender: TObject);
    procedure Night1Click(Sender: TObject);
    procedure Runandstophere1Click(Sender: TObject);
    procedure Breakhere1Click(Sender: TObject);
    procedure ogglebreak1Click(Sender: TObject);
    procedure PascalClassic1Click(Sender: TObject);
    procedure VisualStudioCode1Click(Sender: TObject);
    procedure Statistics1Click(Sender: TObject);
    procedure Recent1Click(Sender: TObject);
    procedure BFontUpClick(Sender: TObject);
    procedure BFontDownClick(Sender: TObject);
    procedure PageRightChange(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
    procedure UPDATE1Click(Sender: TObject);
    procedure ANSI1Click(Sender: TObject);
    procedure Unicode1Click(Sender: TObject);
  private
    { Private declarations }

    AutoParsing : Boolean; // Flag to avoid changing code tab and focus while parsing with errors

    FormAST : TFormAST;
    FormComplete : TFormComplete;
    FormDebug : TFormDebug;
    FormFormatter : TFormFormatter;
    FormProfiler : TFormProfiler;
    FormRecent : TFormRecent;
    FormRun : TFormRun;
    FormRunMonitor : TFormRunMonitor;
    FormSearch : TFormSearch;
    FormStats : TFormStats;
    FormToolBar: TFormToolbar;
    FormTypes : TFormTypes;

    Options: TOptions;

    Prompt : TPrompt;

    Runner : TRunner;

    ClosingAll,
    CanChangeEditorPosition : Boolean;

    function ActiveEditor:TRichEdit;

    function Add(const APath,AModule:String):TCodeEditor;
    function AddCode(const APath,AName,AText:String):TCodeEditor;
    procedure AddJump(const AModule:String; const ALine,AColumn,ATopLine:Integer);
    procedure AddToFormModules(const ABee:TBee; const ANode:TNode);

    procedure AddASTForm;
    procedure AddDebugForm;
    procedure AddFormatterForm;
    procedure AddFormExplorer;
    procedure AddModulesForm;
    procedure AddProfilerForm;
    procedure AddRecentForm;
    procedure AddRunForm;
    procedure AddRunMonitorForm;
    procedure AddStatsForm;
    procedure AddTypesForm;

    {$IFDEF FPC}
    procedure AppException(Sender : TObject; E : Exception);
    {$ENDIF}

    procedure BreakEditorChange(Sender:TObject; const ABreak:TBreakHere);

    procedure ChangeEditorFont(Delta:Integer);
    procedure ChangePage(const AIndex:Integer);
    procedure ChangeStart(Sender: TObject);

    function CheckAllCompiled(const AModule:TNode):Boolean;

    procedure CheckFileClose;
    procedure CheckForUpdates;
    procedure CheckPrompt;

    procedure ClearLog;
    procedure ClearModified(const AEditor:TCodeEditor);
    function CloseEditor(const AIndex:Integer):Boolean;

    procedure CloseModule(Sender: TObject; const ANode:TNode);
    procedure CodeCompletion(const Editor:TCodeEditor; const Partial:Boolean);

    procedure CreateRunner;
    procedure CreateSearchForm;
    procedure CreateToolbar;

    function CursorAtStop(const AEditor:TCodeEditor):Boolean;

    procedure DoChangePage(const AIndex:Integer);
    procedure DoRemoveCurrent(const ANode:TNode);
    procedure DoRestart(Sender: TObject; const AFile:String);

    procedure EditorChangeLineCol(Sender: TObject);

    procedure EnableJumpButtons;

    procedure FinishRunning;
    procedure ForceStopRunning;

    function GetSaveFileName(const AOld:String):String;

    procedure GotoBreak(Sender:TObject; const ABreak:TBreakHere);
    procedure GotoDeclaration(Sender: TObject; const ANode:TNode);
    procedure GotoNode(Sender: TObject; const ANode:TNode);
    procedure GotoSearch(Sender:TObject);
    procedure GotoTrace(Sender:TObject; const ACall:TDataCall);
    procedure GotoUsage(Sender:TObject);

    function OpenModule(const AModule:String):Boolean; overload;
    procedure OpenModule(Sender:TObject; const AModule:TParsedModule); overload;

    procedure DoSaveAs(const AFile:String);

    function EditorOfPage(const AIndex:Integer):TCodeEditor;

    procedure JumpTo(const AModule:String; const ALine,AColumn{,ATopLine}:Integer);
    procedure JumpToError(const AError:TNodeError);
    procedure JumpToNode(ANode:TNode; const AtDeclaration:Boolean);

    procedure NodeClick(Sender:TObject; const ANode: TNode);
    procedure NodeJump(Sender:TObject; const ANode: TNode);

    procedure EditorChange(Sender: TObject);
    procedure EditorKeyPress(Sender: TObject; var Key: Char);

    procedure Error(const Sender:TParser; const AError:TNodeError);

    function FindModule(const APath,AName:String):Integer; overload;
    function FindModule(const ANode:TNode):Integer; overload;

    procedure FinishParsing;

    function GotoNodeAt(const AEditor:TCodeEditor;
                        const ANode: TNode; out APosition:TNodePosition):Boolean;

    procedure GutterBreakAdded(Sender:TObject; const AIndex:Integer);
    procedure GutterBreakRemoved(Sender:TObject; const AIndex:Integer);
    procedure GutterBreakToggled(Sender:TObject; const AIndex:Integer);

    procedure DoAddCode(const APath,AName,AText:String);
    procedure DoStep(IsStepOver:Boolean);

    procedure ModuleParsed(const Sender:TParser; const ANode:TNode);
    procedure NewFromTranspiled(Sender: TObject; const AText:String);
    procedure ParseCurrent;

    procedure PauseClick(Sender: TObject);

    procedure RefreshBreakStops(const AEditor:TCodeEditor);
    procedure RefreshFormatter;
    procedure RefreshRunUI(const ALast:TNode);
    procedure RefreshTranspiler(const DoTranspile:Boolean);

    procedure ReplaceErrors(Sender: TObject; {const AModule:TParsedModule;} const AParser:TBee);
    procedure RunnerBreak(Sender:TObject; const AIndex:Integer);

    procedure RunWithoutDebug(Sender: TObject);

    function SamePathOf(const AIndex:Integer; const APath:String):Boolean;

    procedure SelectTreeNodeOf(const ANode:TNode);
    procedure SelectedRecent(Sender: TObject);
    function SetCurrent(const APath,AModule:String):Boolean;

    procedure SetEnabledRun(const ARun:TFormRunControls);

    procedure SetRunControls(const AStatus:TRunning);
    procedure SetRunMenuItems(const AStatus:TRunning);

    procedure ShowFileExplorer(Sender:TObject);
    procedure ShowLog(Sender:TObject);
    procedure ShowModules(Sender:TObject);
    procedure ShowTypes(Sender:TObject);

    procedure DoStepIn(Sender: TObject);
    procedure DoStepOver(Sender: TObject);

    procedure SetRunEvents(const ARun:TFormRunControls);

    function StartContext:TNode;
    procedure StartRunning(const WithDebug:Boolean);

    procedure StopAndCleanRunner_UI;
    procedure StopClick(Sender: TObject);

    function TreeASTGetText(Sender:TObject; const APosition:TNodePosition):String;

    procedure TryAddRecent(const AFileName:String);
    function TryAST(const AEditor:TCodeEditor):TVCLTreeAST;

    procedure TryBreakAt(const ANode:TNode);
    function TryContext(const AEditor:TCodeEditor):TNode;
    function CheckModified(const AEditor:TCodeEditor):Boolean;
    procedure TryFillModules;
    procedure TryOpenFile(Sender: TObject; const AFileName:String);
    procedure TryParse(const AEditor:TCodeEditor);
    procedure TryRefreshSearch;
    procedure TryRemoveCurrent(const AModule:TNode);
    function TryTypes(const AEditor:TCodeEditor):TVCLTreeAST;
    procedure TryWrite(const AIndex:Integer); overload;
    procedure TryWrite(const AEditor:TCodeEditor); overload;

    {$IFDEF MSWINDOWS}
    procedure WMDROPFILES(var Message: TWMDROPFILES); message WM_DROPFILES;
    {$ENDIF}
  protected

    ErrorLog : TVidiGrid;

    // Internal Debug
    ShouldAutoParse,
    ShouldHighLight,
    ShouldNotLoadParam0: Boolean;

    FormModules : TFormModules;
    FormTranspiler : TFormTranspiler;

    procedure AddNew(const ACode:String);
    procedure AddTranspilerForm;
    procedure CheckConsole;
    function CheckRunForm:TFormRun;
    procedure CheckSaveAll;
    function CurrentEditor:TCodeEditor;
    function DoParseCurrent(const APath,AModule,AText:String):TNamedType;

    procedure EndStep(const ALast:TNode);

    function GetRunner:TRunner;

    function IsRecreating(const Sender:TObject):Boolean;

    procedure ModuleRemoved(const AModule:TNode);

    {$IFNDEF FPC}
    class procedure SetBinaryVidiFiles(const AItem:TFileTypeItem); static;
    {$ENDIF}

    procedure TryStartProfiler;
    procedure TryStopProfiler;
  public
    { Public declarations }

    Bee : TBee;

    LastOpenedFile,
    Changed_Text : String;

    LogHeight,
    TreeWidth : Integer;

    // Internal, for registry options loading only
    InitialLeftActive,
    InitialExplorerRoot:String;

    // options registry needs this:
    FormExplorer : TFormExplorer;

    procedure ChangeTheme(const ATheme:TVidi_Theme);

    function LastOpened:String;

    procedure TryOpenFileName(const AFileName:String);
  end;

var
  FormVidi: TFormVidi;

implementation

{$R *.dfm}

uses
  Emit,

  {$IFDEF FPC}
  LCLType,
  {$ELSE}
  IOUtils, FileCtrl,
  {$ENDIF}

  UITypes, Clipbrd, Themes,
  Module, ModuleArray, Highlight,
  IO, Search, Position, Utils_VCL, Gutter, Checker.AST,

  IDE_Lang_English, IDE_Lang_Catalan, IDE_Lang_Spanish, Constants,

  {$IFDEF MSWINDOWS}
  Windows_Register_Extension, ShellApi,
  {$ENDIF}

  Code_Jumps,

  {$IFDEF INTERNAL_TIMING}
  {$IFDEF FPC}
  FPC_StopWatch,
  {$ELSE}
  Diagnostics,
  {$ENDIF}
  {$ENDIF}

  {$IFDEF INTERNAL}
  Internal,
  {$ENDIF}

  //Files_FromDirectory,
  About_VCL, Updater_VCL, Compile.Pascal, Trace_VCL,
  Find.AST, Runner, Profiler, Syntax, Stops, Instance_Type, RunningNode,
  Vidi_Lang_English, Vidi_Lang_Catalan, Vidi_Lang_Spanish,

  Plugin, Sys_Plugin, LCL_Plugin;

{$IFNDEF FPC}
procedure SetVidiFiles(const AItem:TFileTypeItem);
begin
  AItem.DisplayName:=Vidi_Lang.Vidi_Files;
  AItem.FileMask:='*'+TVidiConstants.Extension;
end;

class procedure TFormVidi.SetBinaryVidiFiles(const AItem:TFileTypeItem);
begin
  AItem.DisplayName:='Binary Vidi files';
  AItem.FileMask:='*'+TVidiConstants.BinaryExtension;
end;
{$ENDIF}

procedure TFormVidi.ChangeTheme(const ATheme:TVidi_Theme);

  procedure RefreshHighlightColors;
  var t : Integer;
  begin
    case ATheme of
      TVidi_Theme.Day: THighlightColors.ChangeColors(True);
      TVidi_Theme.Night: THighlightColors.ChangeColors(False);
    end;

    for t:=0 to PageCode.PageCount-1 do
        EditorOfPage(t).RefreshHighlight;
  end;

{$IFNDEF FPC}
var s: String;
{$ENDIF}
begin
  {$IFNDEF FPC}
  case ATheme of
    TVidi_Theme.Day: S:='Windows';
    TVidi_Theme.Night: S:='Windows10 Dark';
  end;

  TStyleManager.SetStyle(S);
  {$ENDIF}

  case ATheme of
    TVidi_Theme.Day: Day1.Checked:=True;
    TVidi_Theme.Night: Night1.Checked:=True;
  end;

  RefreshHighlightColors;

  if FormToolBar<>nil then
  begin
    case ATheme of
      TVidi_Theme.Day: FormToolBar.LoadIcons('',True);
      TVidi_Theme.Night: FormToolBar.LoadIcons('white_',True);
    end;

    FormToolBar.RunControls.ResetIcons(Day1.Checked);
  end;
end;

procedure TFormVidi.Cut1Click(Sender: TObject);
begin
  ActiveEditor.CutToClipboard
end;

procedure TFormVidi.BSaveClick(Sender: TObject);
begin
  Save1Click(Self);
end;

procedure TFormVidi.CheckFileClose;
begin
  Close1.Enabled:=PageCode.ActivePageIndex<>-1;
  CloseAll1.Enabled:=PageCode.PageCount>0;
end;

function TFormVidi.CloseEditor(const AIndex:Integer):Boolean;

  procedure TryRemove(const AST:TVCLTreeAST; const AParent:TWinControl);
  begin
    if AST<>nil then
       if AST.Tree<>nil then
          if AST.Tree.Parent=AParent then
              RemoveControls(AParent);
  end;

var e : TCodeEditor;
begin
  e:=CurrentEditor;

  if e<>nil then
  begin
    if not CheckModified(e) then
       Exit(False);

    if FormAST<>nil then
    begin
      TryRemove(e.Tree_AST,FormAST.TabTree);

      FormAST.Clear;
    end;

    if FormTypes<>nil then
       FormTypes.RemoveTree;
  end;

  PageCode.Pages[AIndex].Free;

  if PageCode.ActivePageIndex=-1 then
     if PageCode.PageCount>0 then
        PageCode.ActivePageIndex:=0;

  PageCodeChange(Self);

  CheckFileClose;

  result:=True;
end;

procedure TFormVidi.Close1Click(Sender: TObject);
begin
  CloseEditor(PageCode.ActivePageIndex);
end;

procedure TFormVidi.Close2Click(Sender: TObject);
begin
  Close1Click(Self);
end;

procedure TFormVidi.Clear1Click(Sender: TObject);
begin
  ClearLog;
end;

procedure TFormVidi.ClearLog;
begin
  ErrorLog.Clear;
end;

procedure TFormVidi.CloseAll1Click(Sender: TObject);
begin
  ClosingAll:=True;
  try
    while PageCode.PageCount>0 do
          if not CloseEditor(0) then
             break;

    ClearLog;
  finally
    ClosingAll:=False;
  end;
end;

procedure TFormVidi.Copy1Click(Sender: TObject);
begin
  ActiveEditor.CopyToClipboard
end;

procedure TFormVidi.Copy3Click(Sender: TObject);
begin
  Clipboard.AsText:=ErrorLog.AsText;
end;

procedure TFormVidi.Error(const Sender:TParser; const AError:TNodeError);
begin
  ErrorLog.AppendRow(AError.AsStrings,AError);
end;

procedure TFormVidi.Exit1Click(Sender: TObject);
begin
  Close;
end;

function TFormVidi.FindModule(const ANode:TNode):Integer;
var t : Integer;
begin
  for t:=0 to PageCode.PageCount-1 do
      if EditorOfPage(t).Context=ANode then
         Exit(t);

  result:=-1;
end;

function AnyTabVisible(const APage:TPageControl):Boolean;
var t : Integer;
begin
  for t:=0 to APage.PageCount-1 do
      if APage.Pages[t].TabVisible then
         Exit(True);

  result:=False;
end;

procedure DoShowHideTab(const ATab:TTabSheet; DoShow:Boolean);

  function FirstVisibleTab(const APage:TPageControl; AStart:Integer):TTabSheet;
  var t : Integer;
  begin
    if AStart=-1 then
       AStart:=0;

    if AStart>=APage.PageCount then
       Exit(nil);

    for t:=AStart to APage.PageCount-1 do
        if APage.Pages[t].TabVisible then
           Exit(APage.Pages[t]);

    for t:=AStart downto 0 do
        if APage.Pages[t].TabVisible then
           Exit(APage.Pages[t]);

    result:=nil;
  end;

var tmpTabIndex: Integer;
begin
  tmpTabIndex:=ATab.TabIndex;
  ATab.TabVisible:=DoShow;

  if ATab.TabVisible then
     ATab.PageControl.Visible:=True
  else
  begin
    if ATab.PageControl.ActivePage=nil then
       ATab.PageControl.ActivePage:=FirstVisibleTab(ATab.PageControl,tmpTabIndex);

    ATab.PageControl.Visible:=AnyTabVisible(ATab.PageControl);
  end;
end;

procedure ShowHideTab(const ATab:TTabSheet; DoShow:Boolean);
begin
  if ATab.TabVisible<>DoShow then
     DoShowHideTab(ATab,DoShow);
end;

procedure ToggleTab(const AItem:TMenuItem; const ATab:TTabSheet);
begin
  AItem.Checked:=not AItem.Checked;
  ShowHideTab(ATab,AItem.Checked);

  if ATab.TabVisible then
     ATab.PageControl.ActivePage:=ATab;

  ATab.PageControl.OnChange(ATab);
end;

procedure TFormVidi.FileExplorer1Click(Sender: TObject);
begin
  ToggleTab(FileExplorer1,TabFileExplorer);
end;

function TFormVidi.SamePathOf(const AIndex:Integer; const APath:String):Boolean;
var tmp : TCodeEditor;
begin
  tmp:=EditorOfPage(AIndex);
  result:=SamePath(tmp.ModulePath,APath);
end;

function TFormVidi.FindModule(const APath,AName:String):Integer;
var t : Integer;
begin
  for t:=0 to PageCode.PageCount-1 do
      if PageCode.Pages[t].Caption=AName then
         if (APath='') or SamePathOf(t,APath) then
            Exit(t);

  result:=-1;
end;

procedure TFormVidi.CloseModule(Sender: TObject; const ANode:TNode);
var tmp : Integer;
begin
  tmp:=FindModule(ANode);

  if tmp<>-1 then
     CloseEditor(tmp);
end;

procedure TFormVidi.Day1Click(Sender: TObject);
begin
  ChangeTheme(TVidi_Theme.Day);
end;

procedure TFormVidi.Debugger1Click(Sender: TObject);
begin
  ToggleTab(Debugger1,TabDebugger);
end;

procedure TFormVidi.Delete1Click(Sender: TObject);
begin
  ActiveEditor.SelText:='';
end;

procedure TFormVidi.DoAddCode(const APath,AName,AText:String);
var tmp : TCodeEditor;
begin
  tmp:=AddCode(APath,AName,AText);
  ChangePage(PageCode.PageCount-1);

  tmp.Focus;
end;

procedure TFormVidi.AddNew(const ACode:String);

  function NewModuleName:String;
  var tmp : Integer;
  begin
    tmp:=0;

    repeat
      Inc(tmp);
      result:=Vidi_Lang.Module+tmp.ToString;

    until FindModule('',result)=-1;
  end;

begin
  DoAddCode('',NewModuleName,ACode);

  CheckFileClose;

  CurrentEditor.Edit.Modified:=True;

  CheckSaveAll;
end;

procedure TFormVidi.New1Click(Sender: TObject);
begin
  AddNew('');
end;

procedure TFormVidi.FinishParsing;

  procedure TryModules;
  begin
    if (FormModules<>nil) and (PageLeft.ActivePage=TabModules) then
       FormModules.Items.Invalidate;
  end;

var tmp : TCodeEditor;
begin
  tmp:=CurrentEditor;

  TryContext(tmp);

  if Highlightcode1.Checked then
     if ShouldHighLight and tmp.NeedsHighlight then
        tmp.TryHighLight;

  RefreshBreakStops(tmp);

  tmp.Gutter.Invalidate;

  TryAST(tmp);
  TryTypes(tmp);

  Prompt.CLI.Context:=tmp.Context;

  TryModules;

  SetRunControls(Runner.Status);

  RefreshTranspiler(PageLeft.ActivePage=TabTranspiler);
end;

// Returns whether AFile LastModified DateTime and file size and
// Hash of file content, are equal than the ones when AModule was compiled.
function FileHasNotChanged(const AFile:String; const AModule:TFileStamp):Boolean;
begin
  result:=FileExists(AFile);

  if result then
     result:=(FileSize(AFile)=AModule.FileSize) and
             (FileLastModified(AFile)<=AModule.LastModified);

  // TODO: Hash/CRC file contents
end;

procedure TFormVidi.TryAddRecent(const AFileName:String);
begin
  TFormRecent.AddFile(AFileName);
  Reopen1.Visible:=TFormRecent.RecentFiles.Count>0;
end;

procedure TFormVidi.TryOpenFileName(const AFileName:String);
var tmpName,
    tmpPath,
    tmpFile : String;
    tmpEditor : TCodeEditor;
    tmpModule : TType;
    tmpStamp : TFileStamp;
begin
  tmpFile:=AFileName;

  tmpFile:=TryAddExtension(tmpFile,TVidiConstants.Extension);

  tmpName:=ExtractFileName(tmpFile);
  tmpName:=ChangeFileExt(tmpName,'');

  tmpPath:=ExtractFilePath(AFileName);

  if not SetCurrent(tmpPath,tmpName) then
  begin
    tmpEditor:=AddCode(tmpPath,tmpName,ReadFile(tmpFile));

    if ShouldAutoParse then
    begin
      tmpModule:=TBee.FindModuleStamp(tmpName,tmpStamp);

      if tmpModule=nil then
         TryParse(tmpEditor)
      else
      // Can we reuse an in-memory compiled module?
      if not FileHasNotChanged(AFileName,tmpStamp) then
         TryParse(tmpEditor);
    end;

    CloseAll1.Enabled:=PageCode.PageCount>0;

    TryAddRecent(tmpFile);

    DoChangePage(PageCode.PageCount-1);

    ClearModified(CurrentEditor);

    if FormRecent<>nil then
       FormRecent.RefreshList;

    TryRefreshSearch;
  end;
end;

procedure TFormVidi.Reopen1Click(Sender: TObject);
var tmp : String;
begin
  tmp:=TFormRecent.Select(Self);

  if tmp<>'' then
     TryOpenFileName(tmp);
end;

procedure TFormVidi.Run1Click(Sender: TObject);
begin
  SetRunMenuItems(Runner.Status);
end;

procedure TFormVidi.Run2Click(Sender: TObject);
begin
  CheckRunForm;

  FontDialog1.Font:=FormRun.Console.Font;

  if FontDialog1.Execute then
     FormRun.Console.Font:=FontDialog1.Font;
end;

procedure TFormVidi.Runandstophere1Click(Sender: TObject);
begin
  // Add temporary break stop at current editor line, and run.
  // When stopped, remove break.
end;

procedure TFormVidi.RunMonitor1Click(Sender: TObject);
begin
  ToggleTab(RunMonitor1,TabRunMonitor);
end;

procedure TFormVidi.Open1Click(Sender: TObject);
var OpenDialog : {$IFDEF FPC}TOpenDialog{$ELSE}TFileOpenDialog{$ENDIF};
    t : Integer;
begin
  OpenDialog:={$IFDEF FPC}TOpenDialog{$ELSE}TFileOpenDialog{$ENDIF}.Create(Self);
  try
    OpenDialog.{$IFDEF FPC}DefaultExt{$ELSE}DefaultExtension{$ENDIF}:=TVidiConstants.NoDot_Extension;

    OpenDialog.Options:=OpenDialog.Options+
        [{$IFDEF FPC}ofFileMustExist,ofAllowMultiselect{$ELSE}fdoFileMustExist,fdoAllowMultiSelect{$ENDIF}];

    {$IFNDEF FPC}
    SetVidiFiles(OpenDialog.FileTypes.Add);
    {$ENDIF}

    if OpenDialog.Execute then
       for t:=0 to OpenDialog.Files.Count-1 do
           TryOpenFileName(OpenDialog.Files[t]);
  finally
    OpenDialog.Free;
  end;
end;

procedure TFormVidi.Options1Click(Sender: TObject);
begin
  // Editor Font
  Code1.Enabled:=PageCode.ActivePage<>nil;
end;

procedure SetNodeAddress(const Edit:TCodeEditor; const AParent:TWinControl);

  function AddLabel(const X:Integer; const S:String):TLabel;
  begin
    result:=TLabel.Create(AParent.Owner);
    result.Caption:=S;
    result.Parent:=AParent;

    result.Left:=X;
    result.Top:=4;
  end;

var tmpNode : TNode;
    tmpRoutine : TRoutine;
    tmpClass : TClassType;
    X : Integer;
begin
  while AParent.ControlCount>0 do
     AParent.Controls[0].Free;

  tmpNode:=Edit.NodeAtCursor(0);

  if tmpNode<>nil then
  begin
    tmpRoutine:=TChecker.GetRoutineOf(tmpNode);

    X:=2;

    if tmpRoutine<>nil then
    begin
      AddLabel(X,tmpRoutine.Name);
      Inc(X,8*Length(tmpRoutine.Name));
    end;

    tmpClass:=TChecker.GetClassTypeOf(tmpNode);

    if tmpClass<>nil then
       AddLabel(X,tmpClass.Name);
  end;
end;

procedure TFormVidi.EditorChangeLineCol(Sender: TObject);
var Edit : TCodeEditor;
begin
  Edit:=(Sender as TCodeEditor);

  CodeStatus1.Caption:=IntToStr(Edit.Edit.CaretLine)+','+IntToStr(Edit.Edit.Column)
+' '+IntToStr(Edit.Edit.SelStart)+' '+IntToStr(Edit.Edit.CurrentPos)
                  {$IFDEF INTERNAL}+' '+IntToStr(Edit.Edit.SelStart){$ENDIF};

  CodeStatus1.Width:=TextWidth(CodeStatus1,CodeStatus1.Caption)+8;

  SetNodeAddress(Edit,PanelAddress);
end;

function TFormVidi.AddCode(const APath,AName,AText:String):TCodeEditor;
var tmp : TTabSheet;
    Editor : TCodeEditor;
begin
  tmp:=TTabSheet.Create(Self);
  tmp.PageControl:=PageCode;
  tmp.Caption:=AName;

  Editor:=TCodeEditor.Create(Self);

  Editor.Parent:=tmp;
  Editor.Align:=TAlign.alClient;

  Editor.OnChangeLineCol:=EditorChangeLineCol;

  Editor.ModuleName:=AName;
  Editor.ModulePath:=APath;

  Editor.Init;

  Editor.FromText(AText);

  Editor.OnNodeClick:=NodeClick;
  Editor.OnNodeJump:=NodeJump;

  Editor.Edit.OnChange:=EditorChange;
  Editor.Edit.OnKeyPress:=EditorKeyPress;

  Editor.Edit.PopupMenu:=PopupEdit;

  Editor.NeedsHighlight:=True;

  Editor.Gutter.OnBreakAdded:=GutterBreakAdded;
  Editor.Gutter.OnBreakToggled:=GutterBreakToggled;
  Editor.Gutter.OnBreakRemoved:=GutterBreakRemoved;

  Editor.ChangeLineCol;

  result:=Editor;
end;

function TFormVidi.LastOpened:String;
var tmp : TCodeEditor;
begin
  tmp:=CurrentEditor;

  if tmp=nil then
     result:=''
  else
     result:=tmp.FileName;
end;

procedure TFormVidi.GutterBreakAdded(Sender:TObject; const AIndex:Integer);
var tmp : TNode;
    Gutter : TEditorGutter;
begin
  Gutter:=(Sender as TEditorGutter);

  tmp:=CurrentEditor.Positions.NodeAt(Gutter.Breaks[AIndex].Line);

  if tmp=nil then
     Gutter.Breaks[AIndex].Status:=TBreakStatus.Wrong
  else
  begin
    Runner.Breaks.Add(tmp,Gutter.Breaks[AIndex].Line);

    if FormDebug<>nil then
       FormDebug.Breaks.Fill;
  end;
end;

procedure TFormVidi.ChangeEditorFont(Delta:Integer);
var e : TCodeEditor;
begin
  e:=CurrentEditor;

  if e<>nil then
  begin
    e.Edit.Font.Size:=e.Edit.Font.Size+Delta;
    e.Edit.ChangeFont(e.Edit.Font);
    e.RecalcGutterWidth;
    e.RefreshHighlight;
  end;
end;

procedure TFormVidi.BFontDownClick(Sender: TObject);
begin
  ChangeEditorFont(-1);
end;

procedure TFormVidi.BFontUpClick(Sender: TObject);
begin
  ChangeEditorFont(1);
end;

// Break properties have been changed in Breaks editor dialog
procedure TFormVidi.BreakEditorChange(Sender:TObject; const ABreak:TBreakHere);
var tmp : TCodeEditor;
    tmpIndex : Integer;
begin
  tmp:=CurrentEditor;

  if tmp<>nil then
  begin
    tmpIndex:=tmp.Gutter.FindBreakAt(ABreak.Line);

    if ABreak.Enabled then
       tmp.Gutter.Breaks[tmpIndex].Status:=TBreakStatus.Enabled
    else
       tmp.Gutter.Breaks[tmpIndex].Status:=TBreakStatus.Disabled;

    tmp.Gutter.Invalidate;
  end;
end;

procedure TFormVidi.Breakhere1Click(Sender: TObject);
begin
  Runner.OneTimeBreak:=CurrentEditor.Positions.NodeAt(CurrentEditor.Edit.CaretLine);

  Start1Click(Self);
end;

procedure TFormVidi.GutterBreakToggled(Sender:TObject; const AIndex:Integer);
begin
  Runner.Breaks.Items[AIndex].Enabled:=not Runner.Breaks.Items[AIndex].Enabled;

  BreakEditorChange(Sender,Runner.Breaks.Items[AIndex]);

  if FormDebug<>nil then
     FormDebug.Breaks.Fill;
end;

procedure TFormVidi.GutterBreakRemoved(Sender:TObject; const AIndex:Integer);
var tmp : TNode;
    Gutter : TEditorGutter;
begin
  Gutter:=(Sender as TEditorGutter);

  tmp:=CurrentEditor.Positions.NodeAt(Gutter.Breaks[AIndex].Line);

  if tmp<>nil then
  begin
    Runner.Breaks.Remove(tmp);

    if FormDebug<>nil then
       FormDebug.Breaks.Fill;
  end;
end;

procedure TFormVidi.Highlightcode1Click(Sender: TObject);
begin
  Highlightcode1.Checked:=not Highlightcode1.Checked;
  ShouldHighLight:=Highlightcode1.Checked;
end;

procedure TFormVidi.JumpLeftClick(Sender: TObject);
var tmp : TCodeJump;
begin
  tmp:=CodeJumps.Previous;

  if tmp.Module<>'' then
     JumpTo(tmp.Module,tmp.Line,tmp.Column{,tmp.TopLine});

  EnableJumpButtons;
end;

procedure TFormVidi.JumpListClick(Sender: TObject);
var J : TCodeJump;
    tmp : TMenuItem;
begin
  PopupJumps.Items.Clear;

  for J in CodeJumps.Items do
  begin
    tmp:=TMenuItem.Create(Self);
    tmp.Caption:=J.Module+' '+IntToStr(J.Line)+','+IntToStr(J.Column);

    PopupJumps.Items.Add(tmp);
  end;

  PopupJumps.Popup(JumpList.Left,JumpList.Top);
end;

procedure TFormVidi.JumpRightClick(Sender: TObject);
var tmp : TCodeJump;
begin
  tmp:=CodeJumps.Next;

  if tmp.Module<>'' then
     JumpTo(tmp.Module,tmp.Line,tmp.Column{,tmp.TopLine});

  EnableJumpButtons;
end;

procedure TFormVidi.AddToFormModules(const ABee:TBee; const ANode:TNode);
begin
  if FormModules=nil then
     AddModulesForm;

  FormModules.AddModule(ABee,ANode);
end;

procedure TFormVidi.ModuleParsed(const Sender:TParser; const ANode:TNode);
var b : TBee;
    t : Integer;

    {$IFDEF INTERNAL}
    //tmpErrors: TNodeErrors;
    {$ENDIF}
begin
  b:=Sender as TBee;
  t:=FindModule(b.ModulePath,b.ModuleName);

  if t<>-1 then
     EditorOfPage(t).SetParsed(b,ANode);

  AddToFormModules(b,ANode);

  {$IFDEF INTERNAL}
  {
  tmpErrors:=FormModules.GetErrors(b.ModuleName);

  if tmpErrors.Count=0 then
     b.Positions.CheckMissing(b.Text);
     }
  {$ENDIF}
end;

procedure TFormVidi.Modules1Click(Sender: TObject);
begin
  ToggleTab(Modules1,TabModules);
end;

procedure TFormVidi.Formatter1Click(Sender: TObject);
begin
  ToggleTab(Formatter1,TabFormatter);
end;

procedure TFormVidi.ForceStopRunning;
begin
  if Runner.Status<>TRunning.Stopped then
     Stop1Click(Self);
end;

procedure TFormVidi.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Options.Save;

  FormRun.Free;
  FormComplete.Free;
  FormModules.Free;
  FormTypes.Free;
  FormToolBar.Free;

  ClearLog;
end;

function TFormVidi.CheckModified(const AEditor:TCodeEditor):Boolean;
begin
  if AEditor.Edit.Modified then
   case YesNoCancel(Format(Vidi_Lang.DoYouWantToSave,[AEditor.ModuleName])) of
     mrYes : TryWrite(AEditor);

     mrNo  : ClearModified(AEditor);

     mrCancel : Exit(False);
   end;

  result:=True;
end;

procedure TFormVidi.FormCloseQuery(Sender: TObject; var CanClose: Boolean);

  procedure CheckAllEditors;
  var t : Integer;
      tmp : TCodeEditor;
  begin
    for t:=0 to PageCode.PageCount-1 do
    begin
      tmp:=EditorOfPage(t);

      if not CheckModified(tmp) then
      begin
        CanClose:=False;
        Exit;
      end;
    end;
  end;

begin
  CanClose:=False;

  if Runner.Status<>TRunning.Stopped then
     if not YesNo(Vidi_Lang.StopRunAndClose) then
        Exit;

  ForceStopRunning;

  CheckAllEditors;

  CheckSaveAll;
  CanClose:=not SaveAll1.Enabled;
end;

procedure TFormVidi.DoRestart(Sender: TObject; const AFile:String);
begin
  if CloseQuery then
  begin
    ExecuteFile(AFile,GetCurrentDir);
    Close;
  end;
end;

procedure TFormVidi.About1Click(Sender: TObject);
begin
  TFormAbout.Show(Self,DoRestart);
end;

function TFormVidi.Add(const APath,AModule:String):TCodeEditor;
begin
  result:=AddCode(APath,AModule,ReadFile(APath,AModule+TVidiConstants.Extension));
end;

{$IFDEF FPC}
procedure TFormVidi.AppException(Sender : TObject; E : Exception);
begin
  ShowMessage(E.Message);
end;
{$ENDIF}

procedure TFormVidi.FormCreate(Sender: TObject);

  procedure LoadOptions;
  begin
    Options.Form:=Self;
    Options.Load;

    TCompiler.Sources:=Bee.ParserPath+PathDelimiter+'..';
  end;

  procedure CreateLogGrid;
  begin
    ErrorLog:=TFormSearch.CreateEditorGrid(Self,TabErrors);
    ErrorLog.OnClick:=LogClick;
    ErrorLog.OnDblClick:=LogDblClick;
    ErrorLog.PopupMenu:=LogMenu;
  end;

  procedure TryChangeLanguage;
  begin
    case Vidi_Language of
      TVidi_Languages.English: ;
      TVidi_Languages.Catalan:
          begin
            Vidi_Lang_Catalan.Change;
            IDE_Lang_Catalan.Change(Self);
          end;

      TVidi_Languages.Spanish:
          begin
            Vidi_Lang_Spanish.Change;
            IDE_Lang_Spanish.Change(Self);
          end;
    end;
  end;

{$IFDEF INTERNAL}
var R:TRunStack;
{$ENDIF}
begin
  {$IFDEF FPC}
  Application.OnException:=AppException;
  {$ENDIF}

  Caption:=Caption+' '+TVidiConstants.Vidi_Version;

  {$IFDEF MSWINDOWS}
  RegisterFileExtension(TVidiConstants.Extension);
  {$ENDIF}

  Changed_Text:=Vidi_Lang.Changed;

  TVidiEmit.RaiseErrors:=False;

  ShouldAutoParse:=True;
  ShouldHighLight:=Highlightcode1.Checked;

  CreateLogGrid;

  {$IFNDEF FPC}
  PageCode.HotTrack:=True;
  PageLeft.HotTrack:=True;
  PageBottom.HotTrack:=True;
  {$ENDIF}

  CanChangeEditorPosition:=True;

  PageLeft.ActivePageIndex:=0;

  FormatSettings.DecimalSeparator:='.';

  Bee:=TBee.Create;
  Bee.ParserPath:='..'+PathDelimiter+'..'+PathDelimiter+'Source';
  Bee.OnParsed:=ModuleParsed;

  TPlugin.Checker:=Bee.Checker;
  TPlugin.Finder:=Bee.Finder;

  LogErrors1Click(Self);

  LoadOptions;

  {$IFDEF MSWINDOWS}
  // Tell Windows to accept dragged files from Explorer
  DragAcceptFiles(Handle,True);
  {$ENDIF}

  Reopen1.Visible:=TFormRecent.RecentFiles.Count>0;

  CreateRunner;

  {$IFDEF FPC}
  PageCode.Options:=PageCode.Options+[nboShowCloseButtons];
  {$ENDIF}
  Modules.OnRemoved:=ModuleRemoved;

  if oolbar1.Checked then
     CreateToolbar;

  TabTypes.TabVisible:=ypes1.Checked;
  TabModules.TabVisible:=Modules1.Checked;
  TabTranspiler.TabVisible:=ranspiler1.Checked;

  PageLeft.Visible:=AnyTabVisible(PageLeft);

  TryChangeLanguage;

  if PageLeft.Visible then
     PageLeftChange(Self);

  {$IFDEF INTERNAL}
  // Debug
  TFormTrace.StackAsText(R);
  {$ENDIF}
end;

procedure TFormVidi.Recent1Click(Sender: TObject);
begin
  ToggleTab(Recent1,TabRecent);
end;

procedure TFormVidi.RefreshBreakStops(const AEditor:TCodeEditor);
begin
  AEditor.RefreshBreakStops(CodeStopsOf(AEditor.Positions,AEditor.Edit.Text));
end;

function TFormVidi.CursorAtStop(const AEditor:TCodeEditor):Boolean;

  function Find(const AItems:TStopsArray; const AValue:Integer):Boolean;
  var t : Integer;
  begin
    for t:=Low(AItems) to High(AItems) do
        if AItems[t]=AValue then
           Exit(True);

    result:=False;
  end;

begin
  result:=Find(AEditor.Gutter.Stops,AEditor.Edit.CaretLine);
end;

procedure TFormVidi.ShowFileExplorer(Sender:TObject);
begin
  if not FileExplorer1.Checked then
     FileExplorer1Click(Self);

  DoShowHideTab(TabFileExplorer,True);
end;

procedure TFormVidi.ShowLog(Sender:TObject);
begin
  if not Log1.Checked then
     Log1Click(Self);

  DoShowHideTab(TabErrors,True);
end;

procedure TFormVidi.ShowModules(Sender:TObject);
begin
  if not Modules1.Checked then
     Modules1Click(Self);

  DoShowHideTab(TabModules,True);
end;

procedure TFormVidi.ShowTypes(Sender:TObject);
begin
  if not ypes1.Checked then
     ypes1Click(Self);

  DoShowHideTab(TabTypes,True);
end;

procedure TFormVidi.RunWithoutDebug(Sender: TObject);
begin
  StartRunning(False);
end;

procedure TFormVidi.SetRunEvents(const ARun:TFormRunControls);
begin
  ARun.Tool_Run.OnClick:=Start1Click;
  ARun.Tool_Pause.OnClick:=Pause1Click;
  ARun.Tool_StepIn.OnClick:=StepIn1Click;
  ARun.Tool_StepOver.OnClick:=StepOver1Click;
  ARun.Tool_Stop.OnClick:=Stop1Click;

  ARun.Runwithoutdebug1.OnClick:=RunWithoutDebug;
end;

procedure TFormVidi.CreateToolbar;
begin
  FormToolBar:=TFormToolbar.Create(Self);

  FormToolBar.Tool_New.OnClick:=New1Click;
  FormToolBar.Tool_Open.OnClick:=Open1Click;
  FormToolBar.Tool_Save.OnClick:=Save1Click;
  FormToolBar.Tool_SaveAll.OnClick:=SaveAll1Click;
  FormToolBar.Tool_Search.OnClick:=Search1Click;
  FormToolBar.Tool_Compile.OnClick:=Compileonly1Click;
  FormToolBar.Tool_Prompt.OnClick:=Prompt1Click;

  FormToolBar.Tool_Explorer.OnClick:=ShowFileExplorer;
  FormToolBar.Tool_Log.OnClick:=ShowLog;
  FormToolBar.Tool_Modules.OnClick:=ShowModules;
  FormToolBar.Tool_Types.OnClick:=ShowTypes;

  SetRunEvents(FormToolBar.RunControls);

  FormToolBar.Toolbar.Parent:=Self;
  FormToolBar.Toolbar.Align:=TAlign.alTop;
  FormToolBar.Toolbar.Height:=42;
end;

procedure TFormVidi.CreateRunner;
begin
  TRunner.OnClear:=CheckRunForm.MemoRunClear;
  TSysPlugin.OnGet:=CheckRunForm.MemoRunGet;
  TRunner.OnPut:=CheckRunForm.MemoRunPut;

  Runner:=TRunner.Create;
  Runner.Finder:=Bee.Finder;

  Runner.OnBreak:=RunnerBreak;
end;

procedure TFormVidi.FormDestroy(Sender: TObject);
begin
  Bee.Free;
  Runner.Free;

  {$IFDEF INTERNAL}
  if TRunningNode.RunStack<>nil then
     InternalError('Leaking '+IntToStr(Length(TRunningNode.RunStack))+' RunStack',nil);
 {$ENDIF}
end;

procedure TFormVidi.FormResize(Sender: TObject);
begin
  if TreeWidth>0 then
  begin
    PageLeft.Width:=Round(TreeWidth*Width/1000);
    TreeWidth:=0;
  end;

  if LogHeight>0 then
  begin
    PanelBottom.Height:=Round(LogHeight*Height/1000);
    LogHeight:=0;
  end;

  if CurrentEditor<>nil then
     CurrentEditor.Realign;
end;

procedure TFormVidi.AddModulesForm;
begin
  FormModules:=TFormModules.Create(Self);

  FormModules.OnCloseModule:=CloseModule;
  FormModules.OnOpenModule:=OpenModule;
  FormModules.OnReplaceErrors:=ReplaceErrors;

  AddForm(FormModules,TabModules,TAlign.alClient);
  FormModules.Show;
end;

function TFormVidi.TreeASTGetText(Sender:TObject; const APosition:TNodePosition):String;
var tmp : TCodeEditor;
begin
  tmp:=CurrentEditor;

  if tmp=nil then
     result:='?'
  else
     result:=Copy(tmp.Edit.Text,APosition.Position.Position,APosition.Length);
end;

procedure TFormVidi.AddASTForm;
begin
  FormAST:=TFormAST.Create(Self);
  AddForm(FormAST,TabAST,TAlign.alClient);

  FormAST.OnTreeChange:=TreeASTChange;
  FormAST.OnGetText:=TreeASTGetText;

  FormAST.Used.OnDblClick:=GotoUsage;
  FormAST.References.OnClick:=GotoUsage;

  FormAST.Show;
end;

procedure TFormVidi.EnableJumpButtons;
begin
  JumpLeft.Enabled:=CodeJumps.Count>1;
  JumpRight.Enabled:=CodeJumps.Current<CodeJumps.Count-1;
  JumpList.Enabled:=CodeJumps.Count>0;
end;

procedure TFormVidi.AddJump(const AModule:String; const ALine,AColumn,ATopLine:Integer);
begin
  CodeJumps.Add(AModule,ALine,AColumn,ATopLine);
  EnableJumpButtons;
end;

procedure TFormVidi.JumpTo(const AModule:String; const ALine,AColumn{,ATopLine}:Integer);
var tmpEdit : TCodeEditor;
    tmp : Boolean;
begin
  tmp:=OpenModule(AModule);

  if not tmp then
  begin
    TryOpenFileName(AModule);
    tmp:=OpenModule(AModule);
  end;

  if tmp then
  begin
    tmpEdit:=CurrentEditor;

    tmpEdit.Edit.SelectText(ALine,AColumn,1);
    tmpEdit.Edit.SetFocus;
  end;
end;

procedure TFormVidi.JumpToNode(ANode:TNode; const AtDeclaration:Boolean);

  procedure AddCurrentJump;
  var tmp : TCodeEditor;
  begin
    tmp:=CurrentEditor;

    if tmp<>nil then
       AddJump(tmp.ModuleName,tmp.Edit.CaretLine,tmp.Edit.Column,tmp.Edit.TopLine);
  end;

var tmpPos : TNodePosition;
    tmpEdit : TCodeEditor;
    tmpS : String;
begin
  AddCurrentJump;

  if AtDeclaration then
     ANode:=TFinder.NodeOfDeclaration(ANode);

  if ANode is TWith then
     OpenModule(TWith(ANode).Module.Name)
  else
  begin
    tmpS:=TFinder.ModuleNameOf(ANode);

    if OpenModule(tmpS) then
    begin
      tmpEdit:=CurrentEditor;

      if GotoNodeAt(tmpEdit,ANode,tmpPos) then
      begin
        tmpEdit.Focus;

        AddJump(tmpS,tmpPos.Position.Line,tmpPos.Position.Column,tmpEdit.Edit.TopLine);
      end;
    end;
  end;
end;

procedure TFormVidi.GotoBreak(Sender:TObject; const ABreak:TBreakHere);
begin
  JumpToNode(ABreak.Node,False);
end;

procedure TFormVidi.GotoTrace(Sender:TObject; const ACall:TDataCall);
begin
  JumpToNode(ACall,False);
end;

procedure TFormVidi.GotoUsage(Sender:TObject);
var Grid : TVidiGrid;
    tmp : TObject;
begin
  Grid:=Sender as TVidiGrid;

  if Grid.Row>0 then
  begin
    tmp:=TObject(Grid.RowObjects[Grid.Row]);

    if tmp is TNode then
       JumpToNode(TNode(tmp),False);
  end;
end;

procedure TFormVidi.GotoSearch(Sender:TObject);
var tmp : TObject;
    Grid : TVidiGrid;
begin
  // Jump !

  Grid:=Sender as TVidiGrid;

  if Grid.Row>0 then
  begin
    tmp:=TObject(Grid.RowObjects[Grid.Row]);

    if tmp is TFoundItem then
       JumpToNode(TFoundItem(tmp).Node,False)
    else
    if tmp is TFileFoundItem then
       JumpTo(TFileFoundItem(tmp).Path+PathDelimiter+TFileFoundItem(tmp).Module,
              TFileFoundItem(tmp).Line,
              TFileFoundItem(tmp).Column
              {,0}
              );
  end;
end;

procedure TFormVidi.SelectedRecent(Sender: TObject);
var tmp : String;
begin
  tmp:=FormRecent.SelectedFile;

  if tmp<>'' then
     TryOpenFileName(tmp);
end;

procedure TFormVidi.AddRecentForm;
begin
  FormRecent:=TFormRecent.Create(Self);
  FormRecent.PanelBottom.Visible:=False;

  AddForm(FormRecent,TabRecent,TAlign.alClient);
  FormRecent.Show;

  FormRecent.OnSelected:=SelectedRecent;
end;

procedure TFormVidi.AddDebugForm;
begin
  FormDebug:=TFormDebug.Create(Self);

  AddForm(FormDebug,TabDebugger,TAlign.alClient);
  FormDebug.Show;

  FormDebug.Breaks.OnGoToBreak:=GotoBreak;
  FormDebug.Breaks.OnBreakChange:=BreakEditorChange;

  FormDebug.Trace.OnTraceJump:=GotoTrace;
end;

procedure TFormVidi.RefreshFormatter;
var tmp : TCodeEditor;
begin
  tmp:=CurrentEditor;

  if tmp<>nil then
  begin
    FormFormatter.Text:=tmp.Edit.Text;
    FormFormatter.Map:=tmp.Positions;

    FormFormatter.Reformat;
  end;
end;

procedure TFormVidi.RefreshTranspiler(const DoTranspile:Boolean);
var tmp : TCodeEditor;
    tmpModule : TNode;
begin
  tmp:=CurrentEditor;

  if tmp<>nil then
  begin
    if tmp.Errors.Count=0 then
    begin
      tmpModule:=tmp.Context;

      if tmpModule is TNamedType then
         if FormTranspiler<>nil then
            if DoTranspile then
               FormTranspiler.Transpile(tmpModule as TNamedType)
            else
               FormTranspiler.Module:=tmpModule as TNamedType;
    end;
  end;
end;

procedure TFormVidi.NewFromTranspiled(Sender: TObject; const AText:String);
begin
  AddNew(AText);
end;

procedure TFormVidi.Night1Click(Sender: TObject);
begin
  ChangeTheme(TVidi_Theme.Night);
end;

procedure TFormVidi.AddTranspilerForm;
begin
  FormTranspiler:=TFormTranspiler.Create(Self);

  AddForm(FormTranspiler,TabTranspiler,TAlign.alClient);
  FormTranspiler.Show;

  FormTranspiler.Edit.PopupMenu:=PopupEdit;

  FormTranspiler.OnNewCode:=NewFromTranspiled;

  RefreshTranspiler(True);
end;

procedure TFormVidi.AddRunMonitorForm;
begin
  FormRunMonitor:=TFormRunMonitor.Create(Self);

  AddForm(FormRunMonitor,TabRunMonitor,TAlign.alClient);

  FormRunMonitor.Runner:=Runner;

  FormRunMonitor.Show;
end;

procedure TFormVidi.AddStatsForm;
begin
  FormStats:=TFormStats.Create(Self);
  AddForm(FormStats,TabStats,TAlign.alClient);
  FormStats.Show;
end;

procedure TFormVidi.AddFormatterForm;
begin
  FormFormatter:=TFormFormatter.Create(Self);

  AddForm(FormFormatter,TabFormatter,TAlign.alClient);
  FormFormatter.Show;

  RefreshFormatter;
end;

procedure TFormVidi.TryOpenFile(Sender: TObject; const AFileName:String);
begin
  TryOpenFileName(AFileName);
end;

procedure TFormVidi.AddFormExplorer;
begin
  FormExplorer:=TFormExplorer.Create(Self);

  AddForm(FormExplorer,TabFileExplorer,TAlign.alClient);
  FormExplorer.Show;

  FormExplorer.OnOpenFile:=TryOpenFile;
end;

procedure TFormVidi.GotoNode(Sender: TObject; const ANode:TNode);
begin
  JumpToNode(ANode,False);
end;

procedure TFormVidi.GotoDeclaration(Sender: TObject; const ANode:TNode);
begin
  JumpToNode(ANode,True);
end;

procedure TFormVidi.AddTypesForm;
begin
  FormTypes:=TFormTypes.Create(Self);

  AddForm(FormTypes,TabTypes,TAlign.alClient);

  FormTypes.OnGotoNode:=GotoNode;
  FormTypes.OnGotoDeclaration:=GotoDeclaration;

  FormTypes.Show;
end;

procedure TFormVidi.ANSI1Click(Sender: TObject);
var tmp : TStrings;
begin
  tmp:=CurrentEditor.Edit.Lines;
  tmp.Text:=Utf8ToAnsi(tmp.Text);
end;

procedure TFormVidi.Unicode1Click(Sender: TObject);
var tmp : TStrings;
begin
  tmp:=CurrentEditor.Edit.Lines;
  tmp.Text:=AnsiToUtf8(tmp.Text);
end;

procedure TFormVidi.AST1Click(Sender: TObject);
begin
  ToggleTab(AST1,TabAST);
end;

procedure TFormVidi.FormShow(Sender: TObject);

  function ParserPathCorrect(const ADir:String):Boolean;
  begin
    result:=SysUtils.DirectoryExists(ADir) and
            DoFileExists(ADir,TModules._SystemModule+TVidiConstants.Extension);
  end;

  function RemoveLastFolder(const S:String):String;
  begin
    result:=S;

    while Copy(result,Length(result),1)<>PathDelimiter do
    begin
      result:=Copy(result,1,Length(result)-1);

      if result='' then
         break;
    end;

    while Copy(result,Length(result),1)=PathDelimiter do
    begin
      result:=Copy(result,1,Length(result)-1);

      if result='' then
         break;
    end;
  end;

  procedure AutoFindParserPath;
  var tmp : String;
  begin
    tmp:=GetCurrentDir;

    while tmp<>'' do
    begin
      if ParserPathCorrect(tmp+PathDelimiter+'source') then
      begin
        Bee.ParserPath:=tmp+PathDelimiter+'source';
        break;
      end
      else
        tmp:=RemoveLastFolder(tmp);
    end;
  end;

  procedure CheckSourcePath;
  var tmp : String;
  begin
    if not ParserPathCorrect(Bee.ParserPath) then
       AutoFindParserPath;

    if not ParserPathCorrect(Bee.ParserPath) then // again
    begin
      tmp:=Bee.ParserPath;

      if tmp='' then
         tmp:=GetCurrentDir;

      if SelectDirectory(Vidi_Lang.SelectVidiSystemFolder,'',tmp) then
         Bee.ParserPath:=tmp;
    end;
  end;

  procedure TrySetActiveLeft;
  var tmp : TControl;
  begin
     tmp:=PageLeft.FindChildControl(InitialLeftActive);

     if tmp is TTabSheet then
        if PageLeft.ActivePage<>tmp then
        begin
          PageLeft.ActivePage:=TTabSheet(tmp);
          PageLeftChange(Self)
        end;
  end;

var tmp : Integer;
begin
  if IsRecreating(Sender) then
     Exit;

  CheckPrompt;

  CreateSearchForm;

  if AST1.Checked then
     ShowHideTab(TabAST,True);

  if FileExplorer1.Checked then
     ShowHideTab(TabFileExplorer,True);

  if Profiler1.Checked then
     ShowHideTab(TabProfiler,True);

  if RunMonitor1.Checked then
     ShowHideTab(TabRunMonitor,True);

  if Debugger1.Checked then
     ShowHideTab(TabDebugger,True);

  if Recent1.Checked then
     ShowHideTab(TabRecent,True);

  if Statistics1.Checked then
     ShowHideTab(TabStats,True);

  if InitialLeftActive<>'' then
     TrySetActiveLeft;

  CheckSourcePath;

  try
    if not ShouldNotLoadParam0 then
    begin
      ShouldNotLoadParam0:=True; // Flag for re-entrance here when changing Theme

      if ParamCount>0 then
         TryOpenFileName(ParamStr(1))
      else
      if LastOpenedFile<>'' then
         TryOpenFileName(LastOpenedFile);
    end;

    tmp:=PageCode.ActivePageIndex;

    if tmp<>-1 then
       EditorOfPage(tmp).Focus;

  finally
    SetRunControls(Runner.Status);
  end;
end;

function TFormVidi.EditorOfPage(const AIndex:Integer):TCodeEditor;
begin
  result:=PageCode.Pages[AIndex].Controls[0] as TCodeEditor;
end;

procedure TFormVidi.English1Click(Sender: TObject);
begin
  English1.Checked:=True;

  Vidi_Language:=TVidi_Languages.English;
  Vidi_Lang_English.Change;
  IDE_Lang_English.Change(Self);
end;

function TFormVidi.CurrentEditor:TCodeEditor;
begin
  if PageCode.ActivePage=nil then
     result:=nil
  else
     result:=EditorOfPage(PageCode.ActivePage.TabIndex);
end;

procedure TFormVidi.Casesensitive1Click(Sender: TObject);
begin
  CaseSensitive1.Checked:=not CaseSensitive1.Checked;

  TChecker.CaseSensitive:=CaseSensitive1.Checked;

  Compileonly1Click(Self);
end;

procedure TFormVidi.Catalan1Click(Sender: TObject);
begin
  Catalan1.Checked:=True;

  Vidi_Language:=TVidi_Languages.Catalan;
  Vidi_Lang_Catalan.Change;
  IDE_Lang_Catalan.Change(Self);
end;

procedure TFormVidi.DoChangePage(const AIndex:Integer);
begin
  PageCode.ActivePageIndex:=AIndex;
  PageCodeChange(Self);

  EditorOfPage(AIndex).Focus;
end;

procedure TFormVidi.ChangePage(const AIndex:Integer);
begin
  if PageCode.ActivePageIndex<>AIndex then
     DoChangePage(AIndex);
end;

function TFormVidi.SetCurrent(const APath,AModule:String):Boolean;
var tmp : Integer;
begin
  tmp:=FindModule(APath,AModule);

  result:=tmp<>-1;

  if result then
  begin
    if APath<>'' then
       if not SamePathOf(tmp,APath) then
          Exit(False);

    ChangePage(tmp);
  end;
end;

function TFormVidi.TryContext(const AEditor:TCodeEditor):TNode;
begin
  result:=nil;

  if AEditor<>nil then
  begin
    if not AEditor.ASTValid then
       TryParse(AEditor);

    if AEditor.Errors<>nil then
    begin
      PageBottom.ActivePage:=TabErrors;

      if ErrorLog.RowCount>1 then
         ErrorLog.Row:=1;

      if not AutoParsing then
         LogClick(Self);
    end
    else
      result:=AEditor.Context;
  end;
end;

function TFormVidi.StartContext:TNode;

  function StartModule:TNode;
  begin
    result:=nil;

    if FormRun<>nil then
       if FormRun.CBModule.Text<>'' then
          result:=TBee.FindModule(FormRun.CBModule.Text);

    if result=nil then
       if CurrentEditor<>nil then
          result:=CurrentEditor.Context;
  end;

begin
  result:=StartModule;

  if result=nil then
     result:=TryContext(CurrentEditor);

  if result<>nil then
     if not CheckAllCompiled(result) then
        result:=nil;
end;

procedure TFormVidi.Statistics1Click(Sender: TObject);
begin
  ToggleTab(Statistics1,TabStats);
end;

function TFormVidi.CheckAllCompiled(const AModule:TNode):Boolean;

  function ValidModule(const AModule:TNode):Boolean;
  var tmp : Integer;
  begin
    tmp:=FindModule(AModule);

    result:=(tmp=-1) or (TryContext(EditorOfPage(tmp))<>nil);
  end;

var M : TModuleArray;
    N : TType;
begin
  result:=False;

  if ValidModule(AModule) then
  begin
    M:=TModules.UsesOf(AModule,True);

    for N in M do
        if not ValidModule(N) then
           Exit(False);

    result:=True;
  end;
end;

function CurrentModule(const ANode:TNode):TNode;
begin
  result:=ANode;

  while result<>nil do
     if result.Owner=nil then
        break
     else
        result:=result.Owner;
end;

procedure TFormVidi.TryStartProfiler;
begin
  if Profile.Enabled then
     Profile.Clear;
end;

procedure TFormVidi.TryStopProfiler;
begin
  if Profile.Enabled then
     if Profiler1.Checked then
        if FormProfiler<>nil then
           FormProfiler.RefreshResults;
end;

function CurrentModuleOf(const ALast:TNode):TNode;
begin
  if ALast is TRunningBlock then
     result:=CurrentModule(TRunningBlock(ALast).TheType)
  else
     result:=CurrentModule(ALast);
end;

procedure TFormVidi.StartRunning(const WithDebug:Boolean);
var tmp : TNode;
begin
  tmp:=StartContext;

  if tmp<>nil then
  begin
    CheckConsole;

    //SetRunControls(TRunning.Running);

    TryStartProfiler;
    try
      if WithDebug then
         Runner.Start(tmp)
      else
      begin
        {tmpError:=}TCodeRunner.RunCode(False,tmp as TType);

        {
        if tmpError.Code<>TRunExceptionCode.None then
           CheckRunForm.MemoRunPut(tmpError.FullText);
        }
      end;
    finally
      FinishRunning;
    end;
  end;
end;

procedure TFormVidi.FinishRunning;
var tmpLast : TNode;
begin
  tmpLast:=Runner.CurrentNode;

  // NO !!! Runner.Stop;
  EndStep(tmpLast);

  TryStopProfiler;

  // if not Debugger1.Checked then
  //    ShowHideTab(TabDebugger,Runner.Status<>TRunning.Stopped);
end;

procedure TFormVidi.Start1Click(Sender: TObject);
begin
  if Runner.Status=TRunning.Paused then
  begin
    Runner.Status:=TRunning.Running;
    SetRunControls(Runner.Status);

    DoRemoveCurrent(CurrentModuleOf(Runner.CurrentNode));

    Runner.Loop;

    FinishRunning;
  end
  else
    StartRunning(True);
end;

procedure TFormVidi.SetRunMenuItems(const AStatus:TRunning);
var tmpAtStop : Boolean;
begin
  if (AStatus=TRunning.Stopped) and (StartContext=nil) then
  begin
    Start1.Enabled:=False;
    StepIn1.Enabled:=False;
    StepOver1.Enabled:=False;
    Pause1.Enabled:=False;
    Stop1.Enabled:=False;
    Breakhere1.Enabled:=False;
    ogglebreak1.Enabled:=False;
  end
  else
  begin
    Start1.Enabled:=AStatus<>TRunning.Running;
    StepIn1.Enabled:=AStatus<>TRunning.Running;
    StepOver1.Enabled:=AStatus<>TRunning.Running;

    Pause1.Enabled:=AStatus=TRunning.Running;
    Stop1.Enabled:=AStatus<>TRunning.Stopped;

    tmpAtStop:=(CurrentEditor<>nil) and CursorAtStop(CurrentEditor);

    Breakhere1.Enabled:=(AStatus<>TRunning.Running) and tmpAtStop;

    ogglebreak1.Enabled:=tmpAtStop;
  end;
end;

procedure TFormVidi.SetEnabledRun(const ARun:TFormRunControls);
begin
  ARun.Tool_Run.Enabled:=Start1.Enabled;
  ARun.Tool_StepIn.Enabled:=StepIn1.Enabled;
  ARun.Tool_StepOver.Enabled:=StepOver1.Enabled;

  ARun.Tool_Pause.Enabled:=Pause1.Enabled;
  ARun.Tool_Stop.Enabled:=Stop1.Enabled;

  ARun.ResetIcons(Day1.Checked);
end;

procedure TFormVidi.SetRunControls(const AStatus:TRunning);
begin
  SetRunMenuItems(AStatus);

  SetEnabledRun(FormToolBar.RunControls);
end;

procedure TFormVidi.Spanish1Click(Sender: TObject);
begin
  Spanish1.Checked:=True;

  Vidi_Language:=TVidi_Languages.Spanish;
  Vidi_Lang_Spanish.Change;
  IDE_Lang_Spanish.Change(Self);
end;

procedure TFormVidi.Stepin1Click(Sender: TObject);
begin
  DoStepIn(Self);
end;

procedure TFormVidi.Stepover1Click(Sender: TObject);
begin
  DoStepOver(Self);
end;

procedure TFormVidi.RefreshRunUI(const ALast:TNode);

  function FirstItemOf(const AType:TType):TNode;
  begin
    if AType.Items.Count>0 then
       result:=AType.Items[0]
    else
       result:=AType;
  end;

  // Find node to mark as current while debugging
  function FindRunningBlock(const ALast:TNode):TNode;
  var tmpCurrent : TNode;
  begin
    result:=ALast;

    if result is TRunningNode then
    begin
      while not (result is TRunningBlock) do
        if TRunningNode(result).Caller=nil then
           break
        else
           result:=TRunningNode(result).Caller;
    end;

    if result is TRunningBlock then
    begin
      tmpCurrent:=TRunningBlock(result).Current;

      if tmpCurrent=nil then
         result:=TRunningBlock(result).TheType
      else
         result:=tmpCurrent;
    end;
  end;

var tmp,
    tmpLast : TNode;
begin
  if Runner.RunException.Code<>TRunExceptionCode.None then
     TRunner.OnPut(Runner.RunException.FullText);

  SetRunControls(Runner.Status);

  if FormDebug<>nil then
     if PageLeft.ActivePage=TabDebugger then
        FormDebug.Fill(Runner);

  if ALast=nil then
     CurrentEditor.BreakAt(-1) // Internal Error !!
  else
  begin
    tmpLast:=FindRunningBlock(ALast);

    // Show or remove break line at gutter
    tmp:=CurrentModule(tmpLast);

    if tmp is TNamedType then
    begin
      if Runner.Status=TRunning.Stopped then
         TryRemoveCurrent(tmp)
      else
      if OpenModule(TNamedType(tmp).Name) then
      begin
        DoRemoveCurrent(CurrentModuleOf(Runner.LastBreak));

        TryBreakAt(tmpLast);

        Runner.LastBreak:=tmpLast;
      end;
    end;
  end;
end;

procedure TFormVidi.EndStep(const ALast:TNode);
begin
  RefreshRunUI(ALast);

  if Runner.Status=TRunning.Stopped then
     StopClick(Self);
end;

procedure TFormVidi.CheckConsole;
begin
  PageBottom.ActivePage:=TabConsole;
  PageBottomChange(Self);

  if CheckRunForm.CBClearBeforeRun.Checked then
     CheckRunForm.Console.Clear;
end;

procedure TFormVidi.DoStep(IsStepOver:Boolean);
begin
  //ShowHideTab(TabDebugger,True);

  if Runner.Status=TRunning.Stopped then
  begin
    CheckConsole;
    Runner.Step(StartContext);
  end
  else
    Runner.Step(IsStepOver);

  EndStep(Runner.Current);
end;

procedure TFormVidi.DoStepIn(Sender: TObject);
begin
  DoStep(False);
end;

procedure TFormVidi.DoStepOver(Sender: TObject);
begin
  DoStep(True);
end;

procedure TFormVidi.TryRemoveCurrent(const AModule:TNode);
var tmpPage : Integer;
    tmpEd : TCodeEditor;
begin
  tmpPage:=FindModule(AModule);

  if tmpPage<>-1 then
  begin
    tmpEd:=EditorOfPage(tmpPage);
    tmpEd.Gutter.SetCurrent(-1);
    tmpEd.Edit.SelLength:=0;
  end;
end;

procedure TFormVidi.Stop1Click(Sender: TObject);
begin
  {$IFDEF INTERNAL}
  Runner.CheckInstanceLeaks:=False;
  try
    StopClick(Self);

    // Destroy leaking instances *after* stop
    Instances.FreeAll;
  finally
    Runner.CheckInstanceLeaks:=True;
  end;

  {$ELSE}
  StopClick(Self);
  {$ENDIF}
end;

procedure TFormVidi.StopAndCleanRunner_UI;
begin
  if Runner.Status<>TRunning.Stopped then
     Runner.Stop;

  SetRunControls(Runner.Status);

  if TabDebugger.Visible then
     FormDebug.Fill(Runner);

//  if not Debugger1.Checked then
//     ShowHideTab(TabDebugger,False);
end;

procedure TFormVidi.DoRemoveCurrent(const ANode:TNode);
begin
  if ANode is TNamedType then
     TryRemoveCurrent(ANode);
end;

procedure TFormVidi.StopClick(Sender: TObject);
var tmp : TNode;
begin
  tmp:=CurrentModuleOf(Runner.CurrentNode);

  StopAndCleanRunner_UI;

  DoRemoveCurrent(tmp);
end;

procedure TFormVidi.ReplaceErrors(Sender: TObject; {const AModule:TParsedModule;} const AParser:TBee);
var E : TNodeError;
begin
  ClearLog;

  for E in AParser.Errors do
      ErrorLog.AppendRow(E.AsStrings,E);
end;

procedure TFormVidi.OpenModule(Sender:TObject; const AModule:TParsedModule);
var tmp : Integer;
    Editor : TCodeEditor;
begin
  tmp:=FindModule(AModule.Path,AModule.ModuleName);  // fix duplicate modulename editor tabs

  if tmp=-1 then
  begin
    Editor:=Add(AModule.Path,AModule.ModuleName);

    Editor.Positions:=AModule.Positions;

    Editor.AddErrors(AModule.Errors);

    Editor.ASTValid:=True;

    Editor.Context:=AModule.Module;

    tmp:=PageCode.PageCount-1;
  end;

  ChangePage(tmp);
end;

procedure TFormVidi.LanguageReference1Click(Sender: TObject);
begin
  TFormAbout.GotoLanguageReference;
end;

procedure TFormVidi.Log1Click(Sender: TObject);
begin
  ToggleTab(Log1,TabErrors);
end;

function TFormVidi.OpenModule(const AModule:String):Boolean;
begin
  result:=SetCurrent(ExtractFilePath(AModule),ExtractFileName(AModule)) or
          FormModules.TryOpen(AModule);
end;

procedure TFormVidi.JumpToError(const AError:TNodeError);
var Editor : TCodeEditor;
    N : TNodePosition;
begin
  if OpenModule(AError.Module) then
  begin
    Editor:=CurrentEditor;

    N.Position:=AError.Position;
    N.Length:=AError.Length;

    Editor.SelectPosition(N);

//      Log.SetFocus;

    AddJump(AError.Module,AError.Position.Line,AError.Position.Column,Editor.Edit.TopLine);
  end;
end;

procedure TFormVidi.LogClick(Sender: TObject);
var tmp : TObject;
begin
  if ErrorLog.Row>0 then
  begin
    tmp:=ErrorLog.RowObjects[ErrorLog.Row];

    if tmp<>nil then
       JumpToError(TNodeError(tmp));
  end;
end;

procedure TFormVidi.LogDblClick(Sender: TObject);
var tmp : TCodeEditor;
begin
  tmp:=CurrentEditor;

  if tmp<>nil then
     tmp.Focus;
end;

procedure TFormVidi.LogErrors1Click(Sender: TObject);
begin
  LogErrors1.Checked:=not LogErrors1.Checked;

  if LogErrors1.Checked then
     Bee.OnError:=Error
  else
     Bee.OnError:=nil;
end;

procedure TFormVidi.LogKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then
     if ErrorLog.Row>0 then
        CurrentEditor.Focus;
end;

function TFormVidi.ActiveEditor:TRichEdit;

  function ActiveTranspiler:Boolean;
  begin
    result:=(PageLeft.ActivePage=TabTranspiler) and
            (FormTranspiler<>nil) and
            FormTranspiler.Edit.Focused;
  end;

  function ActiveRunner:Boolean;
  begin
    result:=(PageBottom.ActivePage=TabConsole) and
            (FormRun<>nil) and
            FormRun.Console.Focused;
  end;

begin
  if ActiveTranspiler then
     result:=FormTranspiler.Edit
  else
  if ActiveRunner then
     result:=FormRun.Console
  else
  if PageCode.ActivePage=nil then
     result:=nil
  else
     result:=CurrentEditor.Edit;
end;

procedure TFormVidi.Edit1Click(Sender: TObject);
var tmp : TRichEdit;
begin
  tmp:=ActiveEditor;

  Cut1.Enabled:=(tmp<>nil) and (tmp.SelLength>0);
  Copy1.Enabled:=Cut1.Enabled;
  Delete1.Enabled:=Copy1.Enabled;

  Paste1.Enabled:=(tmp<>nil) and Clipboard.HasFormat(CF_TEXT);

  SelectAll1.Enabled:=(tmp<>nil) and (tmp.Text<>'');

  Undo1.Enabled:=tmp<>nil;
end;

function TFormVidi.IsRecreating(const Sender:TObject):Boolean;
begin
  {$IFDEF FPC}
  result:=False;
  {$ELSE}

  // This happens when switching Themes:
  result:=(Sender is TControl) and
          (csRecreating in TControl(Sender).ControlState);
  {$ENDIF}
end;

procedure TFormVidi.EditorChange(Sender: TObject);
var tmp : TCodeEditor;
begin
  if IsRecreating(Sender) then
     Exit;

  tmp:=CurrentEditor;

  if (tmp=nil) or (tmp.Edit<>Sender) then
     Exit;

  tmp.Edit.Modified:=True;

  if not Save1.Enabled then
     CheckSaveAll;

  if SameText(tmp.ModuleName,'sys') then
     TRunner.ClearHooks;

  tmp.ASTValid:=False;

  tmp.ChangeLineCol;
  CodeStatus2.Caption:=Changed_Text;

// TODO: this will work when auto-compile is effective at code editor key changes:
//  if TabTranspiler.TabVisible then
//     RefreshTranspiler;

  if not AutoParsing then
  begin
    Timer1.Enabled:=False;
    Timer1.Enabled:=True;
  end;
end;

procedure TFormVidi.NodeClick(Sender:TObject; const ANode: TNode);
begin
  SelectTreeNodeOf(ANode);
end;

procedure TFormVidi.NodeJump(Sender:TObject; const ANode: TNode);
begin
  JumpToNode(ANode,True);
end;

procedure TFormVidi.ogglebreak1Click(Sender: TObject);
var tmp : TCodeEditor;
begin
  tmp:=CurrentEditor;
  tmp.Gutter.ToggleBreak(tmp.Edit.CaretLine);
  tmp.Gutter.Invalidate;
end;

procedure TFormVidi.oolbar1Click(Sender: TObject);
begin
  oolbar1.Checked:=not oolbar1.Checked;

  if oolbar1.Checked then
  begin
    if FormToolBar=nil then
       CreateToolbar;

    FormToolBar.Toolbar.Visible:=True;
  end
  else
  if FormToolBar<>nil then
     FormToolBar.Toolbar.Visible:=False;
end;

procedure TFormVidi.ClearModified(const AEditor:TCodeEditor);
begin
  AEditor.Edit.Modified:=False;
  CodeStatus2.Caption:='';
end;

procedure TFormVidi.TryWrite(const AIndex:Integer);
begin
  TryWrite(EditorOfPage(AIndex));
end;

procedure TFormVidi.TryWrite(const AEditor:TCodeEditor);
begin
  if AEditor.Edit.Modified then
  begin
    if DoFileExists(AEditor.FileName) then
    begin
      WriteFile(AEditor.FileName,AEditor.Edit.Text,AEditor.Edit.Lines.Encoding);
      ClearModified(AEditor);
    end
    else
      DoSaveAs(AEditor.FileName);
  end;
end;

{$IFDEF MSWINDOWS}
// Open *.vidi / *.vidic files mouse-dropped from Windows Explorer
procedure TFormVidi.WMDROPFILES(var Message: TWMDROPFILES);
var FileName : Array[0..255] of Char;
begin
  if DragQueryFile(Message.Drop,$FFFFFFFF,nil,0)>0 then
  begin
    DragQueryFile(Message.Drop,0,@FileName,SizeOf(FileName));

    TryOpenFileName(FileName)
  end;
end;
{$ENDIF}

procedure TFormVidi.Undo1Click(Sender: TObject);
begin
  ActiveEditor.Undo;
end;

procedure TFormVidi.UPDATE1Click(Sender: TObject);
var tmpFile,
    tmpLatest : String;
begin
  if TUpdater.CheckUpdate(tmpLatest) then
     if TUpdater.WantsToUpdate(Self,tmpLatest,tmpFile) then
        DoRestart(Self,tmpFile);
end;

procedure TFormVidi.VisualStudioCode1Click(Sender: TObject);
begin
  VisualStudioCode1.Checked:=True;
  Options.SetKeyboard(TKeyboardShortcuts.VisualCode);
end;

procedure TFormVidi.ypes1Click(Sender: TObject);
begin
  ToggleTab(ypes1,TabTypes);
end;

procedure TFormVidi.Save1Click(Sender: TObject);
begin
  TryWrite(PageCode.ActivePageIndex);
  CheckSaveAll;
end;

procedure TFormVidi.SaveAll1Click(Sender: TObject);
var t : Integer;
begin
  for t:=0 to PageCode.PageCount-1 do
      TryWrite(t);

  CheckSaveAll;
end;

function TFormVidi.GetRunner: TRunner;
begin
  result:=Runner;
end;

function TFormVidi.GetSaveFileName(const AOld:String):String;
var SaveDialog : {$IFDEF FPC}TSaveDialog{$ELSE}TFileSaveDialog{$ENDIF};
begin
  result:='';

  SaveDialog:= {$IFDEF FPC}TSaveDialog{$ELSE}TFileSaveDialog{$ENDIF}.Create(Self);
  try
    SaveDialog.{$IFDEF FPC}DefaultExt{$ELSE}DefaultExtension{$ENDIF}:=TVidiConstants.NoDot_Extension;

    {$IFNDEF FPC}
    SetVidiFiles(SaveDialog.FileTypes.Add);
    SetBinaryVidiFiles(SaveDialog.FileTypes.Add);
    {$ENDIF}

    SaveDialog.FileName:=AOld;

    if SaveDialog.Execute then
       result:=SaveDialog.FileName;
  finally
    SaveDialog.Free;
  end;
end;

procedure TFormVidi.DoSaveAs(const AFile:String);

  function SureToOverwrite(const AName:String):Boolean;
  begin
    result:=YesNo(Format(Vidi_Lang.SureOverwrite,[AName]));
  end;

var tmpName : String;
    tmp : TCodeEditor;
begin
  tmpName:=GetSaveFileName(AFile);

  if tmpName<>'' then
     if (not DoFileExists(tmpName)) or SureToOverwrite(tmpName) then
     begin
       tmp:=CurrentEditor;

       WriteFile(tmpName,tmp.Edit.Text);

       ClearModified(tmp);

       Modules.Remove(tmp.ModuleName);

       tmp.ModuleName:=RemoveExtension(ExtractFileName(tmpName));
       tmp.ModulePath:=ExtractFilePath(tmpName);

       PageCode.ActivePage.Caption:=tmp.ModuleName;

       TryAddRecent(tmpName);

       if FormRun<>nil then
          FormRun.FillModules;
     end;
end;

procedure TFormVidi.Saveas1Click(Sender: TObject);
begin
  DoSaveAs('');
end;

procedure TFormVidi.Search1Click(Sender: TObject);
var tmp : TCodeEditor;
begin
  TabSearch.PageControl.ActivePage:=TabSearch;
  PageRightChange(Self);

  tmp:=CurrentEditor;

  if tmp<>nil then
  begin
    FormSearch.CBSearch.Text:=tmp.Edit.WordUnderCursor;
    FormSearch.CBSearchChange(Self);
  end;

  FormSearch.CBSearch.SetFocus;
end;

procedure TFormVidi.SelectAll1Click(Sender: TObject);
begin
  ActiveEditor.SelectAll;
end;

procedure TFormVidi.SelectTreeNodeOf(const ANode:TNode);

  function NodeOf(const AItem:TTreeNode):TTreeNode;
  var t : Integer;
  begin
    for t:=0 to AItem.Count-1 do
        if AItem.{$IFDEF FPC}Items{$ELSE}Item{$ENDIF}[t].Data=ANode then
           Exit(AItem.{$IFDEF FPC}Items{$ELSE}Item{$ENDIF}[t]);

    result:=nil;
  end;

var tmp : TTreeNode;
    AST : TVCLTreeAST;
    Item : TTreeNode;
begin
  AST:=TryAST(CurrentEditor);

  if AST<>nil then
  for Item in AST.Tree.Items do
  begin
    tmp:=NodeOf(Item);

    if tmp<>nil then
    begin
      CanChangeEditorPosition:=False;
      AST.Tree.Selected:=tmp;
      CanChangeEditorPosition:=True;

      Exit;
    end;
  end;
end;

procedure TFormVidi.Code1Click(Sender: TObject);
var e : TCodeEditor;
begin
  e:=CurrentEditor;

  if e<>nil then
  begin
    FontDialog1.Font:=e.Edit.Font;

    if FontDialog1.Execute then
    begin
      e.Edit.ChangeFont(FontDialog1.Font);
      e.RecalcGutterWidth;
      e.RefreshHighlight;
    end;
  end;
end;

procedure TFormVidi.CodeCompletion(const Editor:TCodeEditor; const Partial:Boolean);
begin
  if FormComplete=nil then
  begin
    FormComplete:=TFormComplete.Create(nil);
    FormComplete.LBItems.Font:=Editor.Edit.Font;
  end;

  FormComplete.ShowList(Editor,Partial);
end;

procedure TFormVidi.CodeStatusMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var tmp : TCodeEditor;
    tmpLine : String;
    tmpValue : Integer;
begin
  if X<CodeStatus1.Width then
  begin
    tmp:=CurrentEditor;

    if tmp<>nil then
    begin
      tmpValue:=tmp.Edit.CaretLine;

      if tmpValue<1 then
         tmpValue:=1;

      tmpLine:=IntToStr(tmpValue);

      if InputQuery(Vidi_Lang.JumpToLine,Vidi_Lang.Line,tmpLine) then
         if TryStrToInt(tmpLine,tmpValue) then
            tmp.Edit.SelectText(tmpValue,1,1);
    end;
  end;
end;

procedure TFormVidi.Compileonly1Click(Sender: TObject);
{$IFDEF INTERNAL_TIMING}
var t1 : TStopWatch;
{$ENDIF}
begin
  {$IFDEF INTERNAL_TIMING}
  Caption:='';
  t1:=TStopWatch.StartNew;
  {$ENDIF}

  if CurrentEditor<>nil then
  begin
    Runner.Current:=nil;  // <-- force destroyed, just in case

    ForceStopRunning;
    //if Runner.Status<>TRunning.Stopped then
    //   Runner.Stop;

//    CurrentEditor.HighLightDone:=AutoParsing;

    ParseCurrent;
    FinishParsing;

    StopAndCleanRunner_UI;
  end;

  {$IFDEF INTERNAL_TIMING}
  Caption:=Caption+' Total: '+t1.ElapsedMilliseconds.ToString;
  {$ENDIF}
end;

procedure TFormVidi.EditorKeyPress(Sender: TObject; var Key: Char);

  function ControlPressed:Boolean;
  {$IFDEF FPC}
  begin
    result:={$IFDEF MSWINDOWS}GetKeyState(VK_CONTROL) < 0{$ELSE}False{$ENDIF}; // TODO: Linux etc
  end;
  {$ELSE}
  var KeyState : TKeyboardState;
  begin
    GetKeyboardState(KeyState);
    result:=ssCtrl in KeyboardStateToShiftState(KeyState);
  end;
  {$ENDIF}

var Editor : TCodeEditor;
begin
  Editor:=(Sender as TRichEdit).Owner as TCodeEditor;

  if Key=' ' then
  begin
    if ControlPressed then
    begin
      Key:=#0; // skip changing text
      CodeCompletion(Editor,True);
    end;
  end
  else
  if Key='.' then
     CodeCompletion(Editor,False);
end;

procedure TFormVidi.CheckSaveAll;
var tmp : Boolean;
    t : Integer;
begin
  tmp:=False;

  for t:=0 to PageCode.PageCount-1 do
      if EditorOfPage(t).Edit.Modified then
      begin
        tmp:=True;
        break;
      end;

  if SaveAll1.Enabled<>tmp then
  begin
    SaveAll1.Enabled:=tmp;

    if FormToolBar<>nil then
       if tmp then
          LoadPng(FormToolBar.Tool_SaveAll,'saveall')
       else
          LoadPng(FormToolBar.Tool_SaveAll,'saveall_disabled');
  end;

  tmp:=SaveAll1.Enabled and CurrentEditor.Edit.Modified;

  if Save1.Enabled<>tmp then
  begin
    Save1.Enabled:=tmp;

    if FormToolBar<>nil then
       if tmp then
          LoadPng(FormToolBar.Tool_Save,'save')
       else
          LoadPng(FormToolBar.Tool_Save,'save_disabled');
  end;

  if FormToolBar<>nil then
  begin
    FormToolBar.Tool_Save.Enabled:=Save1.Enabled;
    FormToolBar.Tool_SaveAll.Enabled:=SaveAll1.Enabled;
  end;
end;

function TFormVidi.TryTypes(const AEditor:TCodeEditor):TVCLTreeAST;
begin
  result:=AEditor.Tree_Classes;

  if (FormTypes<>nil) and (PageLeft.ActivePage=TabTypes) then
  begin
    FormTypes.RemoveTree;
    FormTypes.Usages:=AEditor.Usages;

    TFormAST.TryFillTree(result,FormTypes,FormTypes.PanelTypes,AEditor.ModuleName,False);

    FormTypes.Positions:=AEditor.Positions;

    FormTypes.AddTree(result.Tree);

  end;

  {
  if (FormAST<>nil) and (PageLeft.ActivePage=TabAST) then
  begin
    FormAST.Reset(AEditor.Positions,AEditor.Usages);
    FormAST.FullText:=AEditor.Edit.Text;
  end;
  }
end;

function TFormVidi.TryAST(const AEditor:TCodeEditor):TVCLTreeAST;
begin
  if PageLeft.Visible and TabAST.TabVisible and (PageLeft.ActivePage=TabAST) then
     result:=FormAST.GetAST(AEditor)
  else
     result:=nil;
end;

procedure TFormVidi.TryFillModules;
begin
  if FormModules<>nil then
     if PageLeft.ActivePage=TabModules then
        FormModules.Fill;
end;

procedure TFormVidi.PageLeftChange(Sender: TObject);

  procedure TryRefreshTypes;
  var tmp : TCodeEditor;
  begin
    if FormTypes=nil then
       AddTypesForm;

    tmp:=CurrentEditor;

    if tmp<>nil then
       TryTypes(tmp);
  end;

begin
  if PageLeft.ActivePage=TabModules then
  begin
    if FormModules=nil then
       AddModulesForm;

    TryFillModules;
  end
  else
  if PageLeft.ActivePage=TabAST then
  begin
    if FormAST=nil then
       AddASTForm;

    if PageCode.ActivePage<>nil then
       TryAST(CurrentEditor);
  end
  else
  if PageLeft.ActivePage=TabDebugger then
  begin
    if FormDebug=nil then
       AddDebugForm;

    // pending: if Runner.DebuggerNeedsRefresh then
    FormDebug.Fill(Runner);
  end
  else
  if PageLeft.ActivePage=TabRunMonitor then
  begin
    if FormRunMonitor=nil then
       AddRunMonitorForm;
  end
  else
  if PageLeft.ActivePage=TabStats then
  begin
    if FormStats=nil then
       AddStatsForm;

    FormStats.RefreshCode(CurrentEditor);
  end
  else
  if PageLeft.ActivePage=TabTranspiler then
  begin
    if FormTranspiler=nil then
       AddTranspilerForm;
  end
  else
  if PageLeft.ActivePage=TabFormatter then
  begin
    if FormFormatter=nil then
       AddFormatterForm;
  end
  else
  if PageLeft.ActivePage=TabProfiler then
  begin
    if FormProfiler=nil then
       AddProfilerForm;
  end
  else
  if PageLeft.ActivePage=TabFileExplorer then
  begin
    if FormExplorer=nil then
    begin
      AddFormExplorer;

      if InitialExplorerRoot='' then
         InitialExplorerRoot:=GetCurrentDir;

      FormExplorer.Init(InitialExplorerRoot);
    end;
  end
  else
  if PageLeft.ActivePage=TabRecent then
  begin
    if FormRecent=nil then
       AddRecentForm;
  end
  else
  if PageLeft.ActivePage=TabTypes then
     TryRefreshTypes;
end;

procedure TFormVidi.CreateSearchForm;
begin
  FormSearch:=TFormSearch.Create(Self);
  AddForm(FormSearch,TabSearch);

  FormSearch.Grid.OnDblClick:=GotoSearch;

  FormSearch.SearchPath:=Bee.ParserPath;

  TryRefreshSearch;

  FormSearch.Show;
end;

procedure TFormVidi.PageRightChange(Sender: TObject);
begin
  if PageRight.ActivePage=TabSearch then
  begin
    if FormSearch=nil then
       CreateSearchForm;

    if FormSearch.Grid.RowCount=1 then
       FormSearch.CBSearch.SetFocus
  end;
end;

procedure TFormVidi.CheckPrompt;
begin
  if Prompt=nil then
  begin
    Prompt:=TPrompt.Create(Self);
    AddForm(Prompt,TabPrompt);
    Prompt.Show;
  end;
end;

procedure TFormVidi.PageBottomChange(Sender: TObject);
begin
  if PageBottom.ActivePage=TabPrompt then
  begin
    CheckPrompt;
    Prompt.SetFocus;
  end
  else
  if PageBottom.ActivePage=TabConsole then
  begin
    if FormRun=nil then
    begin
      AddRunForm;
      SetRunControls(Runner.Status);
    end;

    FormRun.FillModules;
  end;
end;

procedure TFormVidi.AddProfilerForm;
begin
  FormProfiler:=TFormProfiler.Create(Self);
  AddForm(FormProfiler,TabProfiler);

  FormProfiler.Show;
  FormProfiler.OnGotoDeclaration:=GotoDeclaration;
end;

procedure TFormVidi.AddRunForm;
begin
  FormRun:=TFormRun.Create(Self);
  AddForm(FormRun,TabConsole);

  FormRun.Show;

  FormRun.Console.PopupMenu:=PopupEdit;

  FormRun.OnChangeStart:=ChangeStart;
end;

procedure TFormVidi.ChangeStart(Sender: TObject);
begin
  SetRunControls(Runner.Status);
end;

function TFormVidi.CheckRunForm:TFormRun;
begin
  if FormRun=nil then
     AddRunForm;

  result:=FormRun;
end;

procedure TFormVidi.PageCodeChange(Sender: TObject);

  procedure DoAutoParse;
  begin
    AutoParsing:=True;
    try
      FinishParsing;
    finally
      AutoParsing:=False;
    end;
  end;

  procedure TryFillErrors;

    procedure AddErrors(const AErrors:TNodeErrors);
    var E : TNodeError;
    begin
      ErrorLog.BeginUpdate;
      try
        for E in AErrors do
            ErrorLog.AppendRow(E.AsStrings,E);
      finally
        ErrorLog.EndUpdate;
      end;
    end;

  var tmp : TCodeEditor;
  begin
    if ErrorLog.RowCount<=1 then
    begin
      tmp:=CurrentEditor;

      if tmp<>nil then
         if tmp.Errors<>nil then
            AddErrors(tmp.Errors);
    end;
  end;

begin
  CheckSaveAll;
  CheckFileClose;

  if PageCode.ActivePage=nil then
  begin
     //PageCode.Hint:=''
  end
  else
  if not ClosingAll then
  begin
    if ShouldAutoParse then
       DoAutoParse;

    TryFillErrors;

    //PageCode.Hint:=tmp.FileName;

    if PageLeft.ActivePage=TabTranspiler then
       RefreshTranspiler(True)
    else
    if PageLeft.ActivePage=TabFormatter then
       RefreshFormatter
    else
    if PageLeft.ActivePage=TabStats then
       FormStats.RefreshCode(CurrentEditor);

    TryRefreshSearch;
  end;
end;

procedure TFormVidi.TryRefreshSearch;
begin
  if FormSearch<>nil then
     if CurrentEditor<>nil then
        FormSearch.CurrentFile:=CurrentEditor.FileName;
end;

procedure TFormVidi.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:=False;

  AutoParsing:=True;
  try
    Compileonly1Click(Self);
  finally
    AutoParsing:=False;
  end;
end;

procedure TFormVidi.TimerUpdateTimer(Sender: TObject);
begin
  TimerUpdate.Enabled:=False;
  CheckForUpdates;
end;

procedure TFormVidi.CheckForUpdates;
var S : String;
begin
  S:=TUpdater.LatestRelease;
  UPDATE1.Visible:=(S<>'') and (S<>TVidiConstants.Vidi_Version);
end;

procedure TFormVidi.TreeASTChange(Sender: TObject; const APosition : TNodePosition);
begin
  if CanChangeEditorPosition then
     CurrentEditor.DoSelectPosition(APosition);
end;

procedure TFormVidi.ModuleRemoved(const AModule:TNode);

  procedure TryRefreshBreaks;
  var tmp : Boolean;
  begin
    tmp:=Runner.Breaks.RemoveIn(AModule);

    if tmp and (FormDebug<>nil) then
       FormDebug.Fill(Runner);
  end;

var tmp : Integer;
begin
  Runner.Last:=nil; // <-- necessary ???

  tmp:=FindModule(AModule);

  if tmp<>-1 then
     EditorOfPage(tmp).ClearContext;

  TryRefreshBreaks;

  if FormModules<>nil then
     FormModules.Removed(AModule);

  TryFillModules;

  ClearLog;
end;

function TFormVidi.DoParseCurrent(const APath,AModule,AText:String):TNamedType;
{$IFDEF INTERNAL_TIMING}
var t1 : TStopWatch;
{$ENDIF}
var tmp : TCodeEditor;
begin
  ClearLog;

  tmp:=CurrentEditor;

  if tmp<>nil then
     if tmp.ModuleName=AModule then
        if tmp.ModulePath=APath then
           tmp.ClearContext;

  Modules.Remove(AModule);

  {$IFDEF INTERNAL_TIMING}
  t1:=TStopWatch.StartNew;
  {$ENDIF}

  result:=Bee.ParseModule(APath,AModule,AText);

  {$IFDEF INTERNAL_TIMING}
  Caption:=Caption+' Parse: '+t1.ElapsedMilliseconds.ToString;
  {$ENDIF}

  TryFillModules;
end;

procedure TFormVidi.TryParse(const AEditor:TCodeEditor);
begin
  AEditor.Context:=DoParseCurrent(AEditor.ModulePath,AEditor.ModuleName,AEditor.Edit.GetFullText);

  if ErrorLog.Row>1 then
     if TabErrors.TabVisible then
        PageBottom.ActivePage:=TabErrors;
end;

procedure TFormVidi.ParseCurrent;
begin
  TryParse(CurrentEditor);
end;

procedure TFormVidi.PascalClassic1Click(Sender: TObject);
begin
  PascalClassic1.Checked:=True;
  Options.SetKeyboard(TKeyboardShortcuts.PascalClassic);
end;

procedure TFormVidi.Paste1Click(Sender: TObject);
begin
  ActiveEditor.PasteFromClipboard
end;

procedure TFormVidi.Pause1Click(Sender: TObject);
begin
  PauseClick(Self);
end;

function TFormVidi.GotoNodeAt(const AEditor:TCodeEditor;
         const ANode: TNode; out APosition:TNodePosition):Boolean;

  function FindEndOfRoutine(const P:TNodePositions):Boolean;
  var t : Integer;
  begin
    for t:=0 to P.Count-1 do
        if P.Items[t].Node=ANode then
        begin
          if Copy(AEditor.Edit.Text,P.Items[t].Position.Position,1)=TSyntax._EndBlock then
           begin
             APosition:=P.Items[t];
             Exit(True);
           end;
        end;

    result:=False;
  end;

begin
  if ANode is TRoutine then
     result:=FindEndOfRoutine(AEditor.Positions)
  else
     result:=False;

  if not result then
     result:=AEditor.Positions.Find(ANode,APosition);

  if not result then
     if ANode is TReturn then
        result:=AEditor.Positions.Find(TReturn(ANode).Value,APosition);

  if not result then
     if ANode is TCallData then
        result:=AEditor.Positions.Find(TCallData(ANode).Value,APosition);

  if result then
     AEditor.DoSelectPosition(APosition);
end;

procedure TFormVidi.TryBreakAt(const ANode:TNode);
var tmp : TNodePosition;
    tmpEd : TCodeEditor;
begin
  tmpEd:=CurrentEditor;

  if GotoNodeAt(tmpEd,ANode,tmp) then
     tmpEd.BreakAt(tmp.Position.Line)
  else
     tmpEd.BreakAt(-1); // Internal Error !!!
end;

procedure TFormVidi.RunnerBreak(Sender:TObject; const AIndex:Integer);
var tmp : String;
    tmpNode : TNode;
begin
  SetRunControls(Runner.Status);

  if AIndex=-1 then
     tmpNode:=Runner.CurrentNode
  else
     tmpNode:=Runner.Breaks.Items[AIndex].Node;

  tmp:=TFinder.ModuleNameOf(tmpNode);

  if OpenModule(tmp) then
     if AIndex=-1 then
        TryBreakAt(tmpNode)
     else
        CurrentEditor.Gutter.SetCurrent(Runner.Breaks.Items[AIndex].Line);

  ShowHideTab(TabDebugger,True);
end;

procedure TFormVidi.PauseClick(Sender: TObject);
begin
  Runner.Pause;
  RunnerBreak(Self,-1);
end;

procedure TFormVidi.PopupEditPopup(Sender: TObject);
var tmp : TRichEdit;
begin
  tmp:=ActiveEditor;

  Cut2.Enabled:=tmp.SelLength>0;
  Copy2.Enabled:=Cut2.Enabled;
  Delete2.Enabled:=Copy2.Enabled;

  Paste2.Enabled:=Clipboard.HasFormat(CF_TEXT);

  SelectAll2.Enabled:=tmp.Text<>'';

  if tmp.Lines.Encoding=TEncoding.Unicode then
     Unicode1.Checked:=True
  else
     Ansi1.Checked:=True
end;

procedure TFormVidi.Profiler1Click(Sender: TObject);
begin
  ToggleTab(Profiler1,TabProfiler);
end;

procedure TFormVidi.Prompt1Click(Sender: TObject);
begin
  PageBottom.ActivePage:=TabPrompt;
  PageBottomChange(Self);
end;

procedure TFormVidi.ranspiler1Click(Sender: TObject);
begin
  ToggleTab(ranspiler1,TabTranspiler);
end;

end.
