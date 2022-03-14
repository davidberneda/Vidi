unit Runner;

interface

uses
  Classes, AST, Bee.Parser, Evaluator;

type
  TCodeRunner=record
  public
    class function Bee:TBee; static;

    class function RunCode(const ACode:TType):TRunException; overload; static;
    class function RunCode(const WithDebug:Boolean; const ACode:TType):TRunException; overload; static;

    class procedure RunFile(const AFileName:String); static;
    class procedure RunStream(const AStream:TStream); static;

    class procedure SwitchBee(const ABee:TBee); static;
  end;

implementation

uses
  SysUtils,
  Sys, Streamer, Constants, ModuleArray, IO, Exceptions, Module,
  Parser, Profiler, Plugin, Instance_Type;

type
  TCLIRunner=class(TRunner)
  private
    class procedure Put(const AText:String);
  end;

  TCLIBee=class(TBee)
  private
    procedure BeeError(const Sender:TParser; const AError:TNodeError);
  public
    Constructor Create; override;
  end;

{ TCLIBee }

constructor TCLIBee.Create;
begin
  inherited;
  OnError:=BeeError;
end;

procedure TCLIBee.BeeError(const Sender:TParser; const AError:TNodeError);
begin
  Writeln('['+AError.Module+'] '+AError.Position.LineColumn+' '+AError.Text);
end;

var
  _Bee : TBee;

{ TCLIRunner }

class procedure TCLIRunner.Put(const AText:String);
begin
  Write(AText);

  if AText=Chr($D) then
     Write(Chr($A));
end;

class function TCodeRunner.Bee: TBee;
begin
  result:=_Bee;
end;

class function TCodeRunner.RunCode(const WithDebug:Boolean; const ACode:TType):TRunException;
var Runner : TCLIRunner;
begin
  Runner:=TCLIRunner.Create;
  try
    Runner.Finder:=_Bee.Finder;
    TPlugin.Checker:=_Bee.Checker;
    TPlugin.Finder:=_Bee.Finder;

    if WithDebug then
       result:=Runner.Start(ACode)
    else
       result:=Runner.StartNoDebug(ACode);
  finally
    if Runner.RunException.Code<>TRunExceptionCode.None then
    begin
      if not WithDebug then
         Runner.Root.Free;

      try
        Runner.Stop;
      finally
        Runner.Free;
      end;
    end
    else
      Runner.Free;
  end;
end;

class function TCodeRunner.RunCode(const ACode:TType):TRunException;
begin
  result:=RunCode(True,ACode);
end;

class procedure TCodeRunner.RunStream(const AStream:TStream);
var Embedded : TModuleArray;

  procedure Relink_System_Module;
  var N : TNode;
  begin
    for N in Modules.SystemModule.Items do
        if N is TClassType then
        begin
          _Bee.Checker.TryCheckMagicTypes(TClassType(N));

          if _Bee.Checker.AllMagicTypes then
             break;
        end;
  end;

var tmp : TType;
begin
  tmp:=TCodeReader.ReadModule(AStream,Embedded);
  Modules.Items:=Embedded;
  Relink_System_Module;
  TCodeRunner.RunCode(tmp);
end;

class procedure TCodeRunner.SwitchBee(const ABee: TBee);
begin
  if ABee=nil then
     _Bee:=nil
  else
  begin
    _Bee.Free;
    _Bee:=ABee;
  end;
end;

class procedure TCodeRunner.RunFile(const AFileName:String);
var M : TStream;
begin
  M:=TFileStream.Create(TryAddExtension(AFileName,TVidiConstants.BinaryExtension),fmOpenRead or fmShareDenyNone);
  try
    TCodeRunner.RunStream(M);
  finally
    M.Free;
  end;
end;

initialization
  _Bee:=TCLIBee.Create;

  if IsConsole then
     TRunner.OnPut:=TCLIRunner.Put;

finalization
  if _Bee<>nil then
     if _Bee is TCLIBee then
        _Bee.Free;
end.
