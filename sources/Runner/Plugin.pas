unit Plugin;

interface

uses
  Sys, AST, Checker.AST, Find.AST, Instance_Type;

type
  TPluginClass=class of TPlugin;

  TPlugin=class
  protected
    Context : TNamedType;

    procedure DoHook; virtual; abstract;

    function FindMethod(const AModule:TType; const AName:String;
                    const AOutput:TType=nil;
                    const AParameter1:TNode=nil;
                    const AParameter2:TNode=nil;
                    const AParameter3:TNode=nil):TRoutine;
  public
    Module : String;

    class var
      Checker : TChecker;
      Finder : TFinder;

      Items : Array of TPlugin;

    class procedure Register(const AItem:TPlugin); static;
    class procedure TryHook(const AWith:TWith); static;
  end;

implementation

uses
  Module, Exceptions, IO;

function TPlugin.FindMethod(const AModule:TType; const AName:String;
                    const AOutput:TType=nil;
                    const AParameter1:TNode=nil;
                    const AParameter2:TNode=nil;
                    const AParameter3:TNode=nil):TRoutine;

var R : TChecker.TRoutineDefinition;
begin
  R.Name:=AName;
  R.Output:=AOutput;
  R.Parameters:=nil;

  if AParameter1<>nil then
  begin
    R.Parameters.Add(AParameter1);

    if AParameter2<>nil then
    begin
      R.Parameters.Add(AParameter2);

      if AParameter3<>nil then
         R.Parameters.Add(AParameter3);
    end;
  end;

  result:=Finder.FindMethod(AModule,R);

  if result=nil then
     Raise_Exception('Plugin Initialization error, routine: '+AName);
end;

class procedure TPlugin.Register(const AItem:TPlugin);
var L : Integer;
begin
  L:=Length(TPlugin.Items);
  SetLength(TPlugin.Items,L+1);
  TPlugin.Items[L]:=AItem;
end;

class procedure TPlugin.TryHook(const AWith:TWith);
var t : Integer;
    tmp : String;
begin
  tmp:=TFinder.WithName(AWith);

  for t:=Low(Items) to High(Items) do
  begin
    if Items[t].Context=nil then
       if Checker.TextIs(ExtractFileName(tmp),Items[t].Module) then
       begin
         Items[t].Context:=Modules.Find(tmp);
         Items[t].DoHook;
       end;
  end;
end;

initialization
finalization
  TPlugin.Items:=nil;
end.
