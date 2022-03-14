unit Module;

interface

uses
  Sys, AST, Map, Usage, ModuleArray;

type
  TModuleRemovedEvent=procedure(const AModule:TNode) of object;

  TFileStamp=record
  public
    LastModified : System.TDateTime;
    FileSize : Int64;
    // Hash : Int64
  end;

  TModules=record
  private
    // List of modules currently being parsed and not yet finished.
    // This list makes possible to detect self-referencing (cycle / circular)
    Parsing : Array of String;

    procedure Remove(const AIndex:Integer); overload;
  public
    const
      _SystemModule = 'sys';

    var
      Items : TModuleArray;
      Positions : Array of TNodePositions;
      FileStamps : Array of TFileStamp;
      Usages : Array of TNodeUsages;

      OnRemoved : TModuleRemovedEvent;

    class var
      ParsedLines:Integer;

    procedure Add(const AModule:TNamedType;
                  const APositions:TNodePositions;
                  const AUsages:TNodeUsages;
                  const AFileStamp:TFileStamp);

    procedure Clear;
    procedure DeleteFromArrays(const AIndex:Integer);
    function DependsOn(const AUser,AUsed:TNode):Boolean;
    function Find(const AName:String):TNamedType;
    function IndexOf(const AModule:TNamedType):Integer; overload;
    function IndexOf(const AName:String):Integer; overload;
    procedure Remove(const AModule:TType); overload;
    procedure Remove(const AName:String); overload;

    procedure ParsingAdd(const AName:String);
    function ParsingIndex(const AName:String):Integer;
    procedure ParsingRemove(const AName:String);

    function SystemModule:TType;

    class function UsesOf(const AModule:TNode; const Recursive:Boolean):TModuleArray; static;
  end;

var
  Modules : TModules;

implementation

uses
  SysUtils, // <-- inline only
  Text, Utils.AST, Checker.AST;

function TModules.IndexOf(const AModule:TNamedType):Integer;
begin
  result:=Items.IndexOf(AModule);
end;

function TModules.IndexOf(const AName:String):Integer;
begin
  result:=Items.IndexOf(AName);
end;

function TModules.ParsingIndex(const AName: String): Integer;
var t : Integer;
begin
  for t:=0 to High(Parsing) do
      if TTextUtils.Same(AName,Parsing[t]) then
         Exit(t);

  result:=-1;
end;

procedure TModules.ParsingAdd(const AName: String);
var L : Integer;
begin
  L:=Length(Parsing);
  SetLength(Parsing,L+1);
  Parsing[L]:=AName;
end;

procedure TModules.ParsingRemove(const AName: String);

  {$IFDEF FPC}
  procedure DeleteParsing(const AIndex:Integer);
  var t : Integer;
  begin
    for t:=AIndex to High(Parsing)-1 do
        Parsing[t]:=Parsing[t+1];

    SetLength(Parsing,High(Parsing));
  end;
  {$ENDIF}

var tmp : Integer;
begin
  tmp:=ParsingIndex(AName);

  {$IFDEF FPC}
  DeleteParsing(tmp);
  {$ELSE}
  Delete(Parsing,tmp,1);
  {$ENDIF}
end;

procedure TModules.Clear;

  procedure DestroyPositions;
  var P : TNodePositions;
  begin
    for P in Positions do
        P.Free;

    Positions:=nil;
  end;

  procedure DestroyUsages;
  var U : TNodeUsages;
  begin
    for U in Usages do
        U.Free;

    Usages:=nil;
  end;

begin
  Items.FreeAll;

  DestroyPositions;
  DestroyUsages;

  FileStamps:=nil;

  TModules.ParsedLines:=0;
end;

{
function TModules.NameOf(const AModule: TType): String;
var t : Integer;
begin
  for t:=0 to High(Items) do
      if Items[t]=AModule then
         Exit(Names[t]);

  if AModule is TNamedType then
     result:=TNamedType(AModule).Name
  else
     result:='';
end;
}

procedure TModules.Add(const AModule: TNamedType;
                       const APositions:TNodePositions;
                       const AUsages:TNodeUsages;
                       const AFileStamp:TFileStamp);
var L : Integer;
begin
  Items.Add(AModule);

  L:=Length(Positions);
  SetLength(Positions,L+1);
  Positions[L]:=APositions;

  L:=Length(Usages);
  SetLength(Usages,L+1);
  Usages[L]:=AUsages;

  L:=Length(FileStamps);
  SetLength(FileStamps,L+1);
  FileStamps[L]:=AFileStamp;
end;

function TModules.Find(const AName:String):TNamedType;
var tmp : Integer;
begin
  tmp:=IndexOf(AName);

  if tmp=-1 then
     result:=nil
  else
     result:=Items[tmp];
end;

function TModules.DependsOn(const AUser,AUsed:TNode):Boolean;

  function TypeDependsOn(const AType:TType):Boolean;
  var N : TNode;
  begin
    for N in AType.Items do
        if DependsOn(N,AUsed) then
           Exit(True);

    result:=False;
  end;

begin
  if AUsed=SystemModule then
     result:=True
  else
  if AUser is TType then
     result:=TypeDependsOn(TChecker.TheTypeOf(TType(AUser)))
  else
  if AUser is TWith then
     result:=TASTUtils.ModuleOfWith(TWith(AUser))=AUsed
  else
  if AUser is TBlockStatement then
     result:=TypeDependsOn(TBlockStatement(AUser).Block)
  else
     result:=False;
end;

// Do not destroy the module, just remove it from internal arrays
procedure TModules.DeleteFromArrays(const AIndex:Integer);

  {$IFDEF FPC}
  procedure DeletePositions(const AIndex:Integer);
  var t : Integer;
  begin
    for t:=AIndex to High(Positions)-1 do
        Positions[t]:=Positions[t+1];

    SetLength(Positions,High(Positions));
  end;

  procedure DeleteUsages(const AIndex:Integer);
  var t : Integer;
  begin
    for t:=AIndex to High(Usages)-1 do
        Usages[t]:=Usages[t+1];

    SetLength(Usages,High(Usages));
  end;

  procedure DeleteFileStamps(const AIndex:Integer);
  var t : Integer;
  begin
    for t:=AIndex to High(FileStamps)-1 do
        FileStamps[t]:=FileStamps[t+1];

    SetLength(FileStamps,High(FileStamps));
  end;
  {$ENDIF}

begin
  Items.Delete(AIndex);

  Positions[AIndex].Free;

  {$IFDEF FPC}
  DeletePositions(AIndex);
  {$ELSE}
  Delete(Positions,AIndex,1);
  {$ENDIF}

  Usages[AIndex].Free;

  {$IFDEF FPC}
  DeleteUsages(AIndex);
  {$ELSE}
  Delete(Usages,AIndex,1);
  {$ENDIF}

  {$IFDEF FPC}
  DeleteFileStamps(AIndex);
  {$ELSE}
  Delete(FileStamps,AIndex,1);
  {$ENDIF}
end;

// Remove also all modules from tmp+1 to Count, that are dependant on tmp
procedure TModules.Remove(const AIndex:Integer);
var tmp : Integer;
    tmpIndex : TNode;
begin
  tmp:=AIndex+1;

  tmpIndex:=Items[AIndex];

  while tmp<Length(Items) do
      if DependsOn(Items[tmp],tmpIndex) then
         Remove(tmp)
      else
         Inc(tmp);

  if Assigned(OnRemoved) then
     OnRemoved(tmpIndex);

  DeleteFromArrays(AIndex); // <-- before Free !!

  tmpIndex.Free;
end;

procedure TModules.Remove(const AName:String);
var tmp : Integer;
begin
  tmp:=IndexOf(AName);

  if tmp<>-1 then
     Remove(tmp);
end;

function TModules.SystemModule: TType;
begin
  result:=Find(TModules._SystemModule);
end;

procedure TModules.Remove(const AModule:TType);
var tmp : Integer;
begin
  tmp:=Items.IndexOf(AModule);

  if tmp<>-1 then
     Remove(tmp);
end;

class function TModules.UsesOf(const AModule: TNode; const Recursive:Boolean): TModuleArray;

  procedure FillRecursive(const AType:TType);

    procedure AddType(const AType:TType);
    var tmp : TModuleArray;
          M : TNamedType;
    begin
      tmp:=UsesOf(AType,True);

      for M in tmp do
          result.TryAdd(M);
    end;

  var N : TNode;
  begin
    for N in AType.Items do
        if N is TWith then
           result.TryAdd(TASTUtils.ModuleOfWith(TWith(N)))
        else
        if N is TType then
           AddType(TType(N))
        else
        if N is TBlockStatement then
           AddType(TBlockStatement(N).Block);
  end;

  procedure Fill(const AType:TType);
  var N : TNode;
  begin
    for N in AType.Items do
        if N is TWith then
           result.TryAdd(TASTUtils.ModuleOfWith(TWith(N)))
        else
        if N is TType then
           Fill(TType(N));
  end;

begin
  result:=nil;

  if Recursive then
     FillRecursive(AModule as TType)
  else
     Fill(AModule as TType);
end;

initialization
  TModules.ParsedLines:=0;
finalization
  Modules.Clear;
end.
