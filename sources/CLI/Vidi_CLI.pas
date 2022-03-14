unit Vidi_CLI;

interface

uses
  Sys, AST, Module, Emit, Evaluator, Constants;

const
  CLI_Msg_Lines      = '%d lines in %d msec.';
  CLI_Msg_PressEnter = 'Press ENTER to exit ';
  CLI_Msg_Welcome    = 'Type .q to quit, .h for help';
  CLI_Msg_Help       = 'Type .q to quit';

  CLI_Error_NoFile   = 'Error: No file specified';

  CLI_Welcome        = 'Vidi '+TVidiConstants.Vidi_Version+' www.vidi.dev';

type
  TVidi_CLI=class
  private
    Emit : TVidiEmit;
  public
    Context : TNode;

    Destructor Destroy; override;

    function AsString(const ANode:TNode):String;
    function NodeTypeOf(const ANode:TNode):String;
    function ParseAndEvaluate(const S:String):TNode;
  end;

procedure Vidi_Main(const AFile:String); overload;
procedure Vidi_Main; overload;

implementation

uses
  Bee.Parser,
  SysUtils,

  {$IFDEF FPC}
  FPC_StopWatch,
  {$ELSE}
  Diagnostics,
  {$ENDIF}

  Parser,
  Exceptions,
  Position,
  IO,
  // LCL_Plugin,  <-- pending to switch from console app to gui app
  Runner;

function TVidi_CLI.ParseAndEvaluate(const S:String):TNode;
begin
  if Context=nil then
     Context:=Modules.SystemModule;

  result:=TBee.ParseExpression(Context,S);
end;

const CRLF=#13#10;

function TVidi_CLI.AsString(const ANode:TNode):String;
begin
  if Emit=nil then
     Emit:=TVidiEmit.Create;

  result:=Emit.AsString(ANode);

  (*
  if (ANode is TArrayExpression) and (TArrayExpression(ANode).Data=nil) then
     // Nothing. This means is an anonymous array [a] instead of Foo[a]
  else
  *)
  if ANode is TData then
     result:=result+CRLF+TEvaluate.AsText(TData(ANode));
end;

destructor TVidi_CLI.Destroy;
begin
  Emit.Free;
  inherited;
end;

function TVidi_CLI.NodeTypeOf(const ANode:TNode):String;
begin
  result:=ANode.ClassName;
end;

procedure Welcome;
begin
  TRunner.OnPut(CLI_Welcome);
  TRunner.OnPut(CRLF);
end;

procedure Usage;
begin
  Welcome;

  Writeln;
  Writeln('Usage: Vidi.exe MyProgram'+TVidiConstants.Extension);
  Writeln('Options:');
  Writeln(' -p -prompt   Interactive mode');
  Writeln(' -c -compile  Compile only, do not run');
  Writeln(' -s -search   Paths to source code files');
  Writeln(' -q -quiet    Not verbose');
end;

function IsParam(P:String; const AShort,ALong:String):Boolean; overload;
begin
  P:=UpperCase(Trim(P));
  result:=(P='-'+AShort) or (P='-'+ALong);
end;

function IsParam(const AShort,ALong:String):Boolean; overload;
var t : Integer;
begin
  for t:=1 to ParamCount do
      if IsParam(ParamStr(t),AShort,ALong) then
         Exit(True);

  result:=False;
end;

function IsCLI:Boolean;
begin
  result:=IsParam('P','Prompt');
end;

function IsCompile:Boolean;
begin
  result:=IsParam('C','Compile');
end;

function RemoveVidiExtension(const AFile:String):String;
begin
  if SameText(ExtractFileExt(AFile),TVidiConstants.Extension) then
     result:=ChangeFileExt(AFile,'')
  else
     result:=AFile;
end;

// TODO: Parameter "-s c:\mypath" or similar
procedure FindSystem(var APath:String);

  function SysExists:Boolean;
  begin
    result:=FileExists(APath+PathDelimiter+'sys'+TVidiConstants.Extension);
  end;

var tmp,
    tmpSource : String;
    t: Integer;
begin
  tmp:=ExtractFilePath(ParamStr(0));

  tmpSource:=PathDelimiter+'..'+PathDelimiter+'source';

  for t:=0 to 10 do
  begin
    APath:=tmp+tmpSource;

    if SysExists then
       break
    else
       tmpSource:=PathDelimiter+'..'+tmpSource;
  end;

  if not SysExists then
     Raise_Exception('Error: System file sys'+TVidiConstants.Extension+' not found at: '+APath);
end;

function Compile(const AFile:String):TType;
begin
  Modules.Clear;

  FindSystem(TCodeRunner.Bee.ParserPath);

  result:=TCodeRunner.Bee.ParseModule(ExtractFilePath(AFile),RemoveVidiExtension(ExtractFileName(AFile)));
end;

var
  Verbose:Boolean=True;

procedure WaitUserReturn;
begin
  if Verbose and IsConsole then
  begin
    WriteLn;
    Write(CLI_Msg_PressEnter);
    Readln;
  end;
end;

procedure DoCompile(const AFile:String);

  procedure CompileAndRun;
  var tmp : TType;
      t1 : TStopWatch;
  begin
    t1:=TStopWatch.StartNew;

    tmp:=Compile(AFile);

    if Verbose then
       TRunner.OnPut(Format(CLI_Msg_Lines,[TModules.ParsedLines,t1.ElapsedMilliseconds]));

    if not IsCompile then
       if tmp<>nil then
          TCodeRunner.RunCode(tmp);
  end;

  procedure BeeError(const E:EBeeException);
  begin
    if IsConsole then
    begin
      Writeln(ExtractFileName(AFile));
      Writeln(E.Message);
      Writeln;
      Writeln(Copy(TCodeRunner.Bee.Text,E.Error.Position.Position-10,20));
    end
    else
      TCodeRunner.Bee.OnError(TCodeRunner.Bee,E.Error);
  end;

begin
  try
    CompileAndRun;
  except
    on E: EBeeException do
       BeeError(E);
  end;

  WaitUserReturn;
end;

procedure Loop;
var CLI: TVidi_CLI;

  procedure Process(const S:String);
  var tmp : TNode;
  begin
    try
      tmp:=CLI.ParseAndEvaluate(S);
      try
        if tmp=nil then
           Writeln('nil')
        else
           Writeln(CLI.NodeTypeOf(tmp)+': '+CLI.AsString(tmp));
      finally
        tmp.Free;
      end;

    except
      on E:Exception do
         Writeln(E.ClassName+': '+E.Message);
    end;
  end;

var S,tmp : String;
begin
  CLI:=TVidi_CLI.Create;
  try
    repeat
      Write('>');
      ReadLn(S);

      tmp:=UpperCase(Trim(S));

      if tmp='.Q' then
         Exit
      else
      if tmp='.H' then
         Writeln(CLI_Msg_Help)
      else
         Process(S);

    until False;
  finally
    CLI.Free;
  end;
end;

procedure DoCLI;
begin
  Writeln(CLI_Msg_Welcome);
  Loop;
end;

// skip linker removal of B function when debugging
procedure TryLink_B;
{$IFOPT D+}
var tmp : TNode;
{$ENDIF}
begin
  {$IFOPT D+}
  tmp:=TReturn.Create; // easy class
  try
    B(tmp);
  finally
    tmp.Free;
  end;
  {$ENDIF}
end;

procedure Vidi_Main(const AFile:String); overload;
begin
  TryLink_B;

  try
    FormatSettings.DecimalSeparator:='.';

    if IsParam('Q','Quiet') then
       Verbose:=False;

    if ParamCount>0 then
    begin
      if Verbose then
         Welcome;

       if IsCLI then
          DoCLI
       else
          DoCompile(AFile)
    end
    else
    begin
      Writeln(CLI_Error_NoFile);
      Writeln;
      Usage;
    end;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);

      WaitUserReturn;
    end;
  end;
end;

function IsValidParam(const AIndex:Integer):Boolean;
var tmp : String;
begin
  tmp:=ParamStr(AIndex);

  result:=IsParam(tmp,'P','Prompt') or
          IsParam(tmp,'Q','Quiet') or
          IsParam(tmp,'S','Search') or
          IsParam(tmp,'C','Compile');
end;

function FileParam:String;
var t : Integer;
begin
  result:='';

  for t:=1 to ParamCount do
      if not IsValidParam(t) then
         Exit(ParamStr(t));
end;

procedure Vidi_Main;
begin
  Vidi_Main(FileParam);
end;

end.
