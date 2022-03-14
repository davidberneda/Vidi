program Vidi;

{$weaklinkrtti on}

{$DEFINE FASTMM}

uses
  {$IFDEF FASTMM}
  FastMM5,
  FastMM_Setup,
  {$ENDIF }
  Forms,
  Unit_TheBee in 'Unit_TheBee.pas' {FormVidi},
  TheBee_VCL in '..\VCL\TheBee_VCL.pas',
  Editor in 'Editor.pas',
  Parser in '..\Core\Parser.pas',
  Emit in '..\Core\Emit.pas',
  Bee.Parser in '..\Core\Bee.Parser.pas',
  Sys in '..\Core\Sys.pas',
  Module in '..\Core\Module.pas',
  Runner in '..\Runner\Runner.pas',
  AST in '..\Core\AST.pas',
  Checker.AST in '..\Core\Checker.AST.pas',
  Find.AST in '..\Core\Find.AST.pas',
  PascalSyntax in 'PascalSyntax.pas',
  Emit.Pascal in '..\Core\Emit.Pascal.pas',
  Emit.CSharp in '..\Core\Emit.CSharp.pas',
  Graphics,
  Winapi.Messages,
  Winapi.RichEdit,
  Vcl.ComCtrls,
  Vcl.Themes,
  Vcl.Styles,
  Map in '..\Core\Map.pas',
  Position in '..\Core\Position.pas',
  Text in '..\Core\Text.pas',
  Exceptions in '..\Core\Exceptions.pas',
  Tree.AST in '..\Core\Tree.AST.pas',
  IO in '..\Core\IO.pas',
  Tests_VCL in '..\VCL\Tests_VCL.pas' {FormTests},
  Prompt_VCL in '..\VCL\Prompt_VCL.pas' {Prompt},
  Vidi_CLI in '..\CLI\Vidi_CLI.pas',
  Search in 'Search.pas',
  Expression.Parser in '..\Core\Expression.Parser.pas',
  Modules_VCL in '..\VCL\Modules_VCL.pas' {FormModules},
  Options in 'Options.pas',
  Completion_VCL in '..\VCL\Completion_VCL.pas' {FormComplete},
  Types_VCL in '..\VCL\Types_VCL.pas' {FormTypes},
  Evaluator in '..\Core\Evaluator.pas',
  Gutter in 'Gutter.pas',
  Syntax in '..\Core\Syntax.pas',
  Run_VCL in '..\VCL\Run_VCL.pas' {FormRun},
  Debug_VCL in '..\VCL\Debug_VCL.pas' {FormDebug},
  Utils_VCL in '..\VCL\Utils_VCL.pas',
  Watch_VCL in '..\VCL\Watch_VCL.pas' {FormWatch},
  Breaks_VCL in '..\VCL\Breaks_VCL.pas' {FormBreaks},
  Trace_VCL in '..\VCL\Trace_VCL.pas' {FormTrace},
  Search_VCL in '..\VCL\Search_VCL.pas' {FormSearch},
  IDE_Lang_Spanish in '..\Languages\IDE_Lang_Spanish.pas',
  IDE_Lang_Catalan in '..\Languages\IDE_Lang_Catalan.pas',
  IDE_Lang_English in '..\Languages\IDE_Lang_English.pas',
  AST_VCL in '..\VCL\AST_VCL.pas' {FormAST},
  Windows_Register_Extension in 'Windows_Register_Extension.pas',
  Editor_VCL in '..\VCL\Editor_VCL.pas',
  Cloner in '..\Core\Cloner.pas',
  Run_Breaks in '..\Core\Run_Breaks.pas',
  About_VCL in '..\VCL\About_VCL.pas' {FormAbout},
  Code_Jumps in 'Code_Jumps.pas',
  Highlight in '..\Core\Highlight.pas',
  Grid_VCL in '..\VCL\Grid_VCL.pas',
  Sort in '..\Core\Sort.pas',
  Usage in '..\Core\Usage.pas',
  Checker.Usage in '..\Core\Checker.Usage.pas',
  Compile in '..\Core\Compile.pas',
  Compile.Pascal in '..\Core\Compile.Pascal.pas' {,
  Transpiler_VCL in '..\VCL\Transpiler_VCL.pas' {FormTranspiler},
  Transpiler_VCL in '..\VCL\Transpiler_VCL.pas' {FormTranspiler},
  Streamer in '..\Core\Streamer.pas',
  RichEmit in '..\Core\RichEmit.pas',
  Editor_Pairs in 'Editor_Pairs.pas',
  Checker.Shared in '..\Core\Checker.Shared.pas',
  Formatter_VCL in '..\VCL\Formatter_VCL.pas' {FormFormatter},
  Evaluator.CompileTime in '..\Core\Evaluator.CompileTime.pas',
  ModuleArray in '..\Core\ModuleArray.pas',
  StringArray in '..\Core\StringArray.pas',
  Magic in '..\Core\Magic.pas',
  Utils.AST in '..\Core\Utils.AST.pas',
  Constants in '..\Core\Constants.pas',
  Compare.AST in '..\Core\Compare.AST.pas',
  Creator.AST in '..\Core\Creator.AST.pas',
  AST.Parser in '..\Core\AST.Parser.pas',
  Explorer_VCL in '..\VCL\Explorer_VCL.pas' {FormExplorer},
  Toolbar_VCL in '..\VCL\Toolbar_VCL.pas' {FormToolbar},
  Completion in 'Completion.pas',
  RecentFiles_VCL in '..\VCL\RecentFiles_VCL.pas' {FormRecent},
  Module_Usage_VCL in '..\VCL\Module_Usage_VCL.pas' {FormModuleUsage},
  Error_Gutter in 'Error_Gutter.pas',
  Run_Monitor in '..\VCL\Run_Monitor.pas' {FormRunMonitor},
  Run_Controls_VCL in '..\VCL\Run_Controls_VCL.pas' {FormRunControls},
  Profiler in '..\Runner\Profiler.pas',
  Profiler_VCL in '..\VCL\Profiler_VCL.pas' {FormProfiler},
  Stats_VCL in '..\VCL\Stats_VCL.pas' {FormStats},
  RunningNode in '..\Runner\RunningNode.pas',
  Instance_Type in '..\Core\Instance_Type.pas',
  Vidi_Lang_English in '..\Languages\Vidi_Lang_English.pas',
  Vidi_Lang_Catalan in '..\Languages\Vidi_Lang_Catalan.pas',
  Vidi_Lang_Spanish in '..\Languages\Vidi_Lang_Spanish.pas',
  Updater_VCL in '..\VCL\Updater_VCL.pas' {Updater},
  LCL_Plugin in '..\Runner\LCL_Plugin.pas',
  Plugin in '..\Runner\Plugin.pas',
  Sys_Plugin in '..\Runner\Sys_Plugin.pas',
  Evaluator_Utils in '..\Core\Evaluator_Utils.pas';

{$R *.res}

{$IF CompilerVersion<35} // 35 = RAD 11.0
{$SETPEOPTFLAGS $160}  // DEP
{$SetPEFlags $20}  // ASLR
{$ENDIF}

type
  TRichEditStyleHookFix = class(TScrollingStyleHook)
  strict private
    procedure EMSetBkgndColor(var Message: TMessage); message EM_SETBKGNDCOLOR;
  end;

{ TRichEditStyleHookFix }

procedure TRichEditStyleHookFix.EMSetBkgndColor(var Message: TMessage);
begin
  Message.LParam := ColorToRGB(StyleServices.GetStyleColor(scEdit));
  Handled := False;
end;

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}

  {$IFDEF FASTMM}
  SetupFastMM;
  {$ENDIF}

  Application.Initialize;
  Application.Title := 'Vidi';
//  TStyleManager.TrySetStyle('Windows10');
//  TStyleManager.Engine.RegisterStyleHook(TRichEdit, TRichEditStyleHookFix);
  Application.CreateForm(TFormVidi, FormVidi);
  Application.Run;
end.
