unit Profiler;

interface

uses
  Sys, AST;

type
  TCallerItem=record
  public
    Index : Integer;
    CallCount,
    Elapsed : Integer;
  end;

  TCallerItems=Array of TCallerItem;

  TCallerItemsHelper=record helper for TCallerItems
  private
    function Add(const AIndex:Integer): Integer;
    procedure DoExit(const AIndex:Integer; const Elapsed:Int64);
  public
  end;

  TCoverageItem=record
  public
    Node : TNode;
    Count : Integer;
    //Callers : TCallerItems;
  end;

  TCoverage=record
  public
    Enabled : Boolean;
    Count : Integer;
    Items : Array of TCoverageItem;

    function Add(const ANode:TNode):Integer;
    procedure Clear;
  end;

  TProfileItem=record
  public
    Routine : TType;
    CallCount : Integer;
    // MinTime,MaxTime,
    Elapsed : Integer;

    Callers : TCallerItems;
  end;

  TProfiler=record
  private
    function Add(const AType:TType):Integer;
  public
    Enabled : Boolean;
    Count : Integer;
    Items : Array of TProfileItem;

    Coverage : TCoverage;

    procedure Clear;
    function DoEnter(const AType:TType):Integer;
    procedure DoExit(const AParent,AIndex:Integer; const Elapsed:Int64);

    function TotalCount:Int64;
    function TotalElapsed:Int64;
  end;

var Profile : TProfiler;

implementation

{ TCallerItemsHelper }

function TCallerItemsHelper.Add(const AIndex:Integer): Integer;

  function Find:Integer;
  var t : Integer;
  begin
    for t:=0 to High(Self) do
        if Self[t].Index=AIndex then
           Exit(t);

    result:=-1;
  end;

begin
  result:=Find;

  if result=-1 then
  begin
    result:=Length(Self);
    SetLength(Self,result+1);

    Self[result].Index:=AIndex;
  end;
end;

procedure TCallerItemsHelper.DoExit(const AIndex:Integer; const Elapsed:Int64);
var tmp : Integer;
begin
  tmp:=Add(AIndex);

  Inc(Self[tmp].CallCount);
  Inc(Self[tmp].Elapsed,Elapsed);
end;

{ TCoverage }

function TCoverage.Add(const ANode:TNode):Integer;

  function Find(const ANode:TNode):Integer;
  var t : Integer;
  begin
    for t:=0 to Count-1 do
        if Items[t].Node=ANode then
           Exit(t);

    result:=-1;
  end;

begin
  result:=Find(ANode);

  if result=-1 then
  begin
    Inc(Count);

    if Length(Items)<Count then
       SetLength(Items,Count+100);

    result:=Count-1;
    Items[result].Node:=ANode;
  end;

  Inc(Items[result].Count); // Always increase usage
end;

procedure TCoverage.Clear;
begin
  Items:=nil;
  Count:=0;
end;

{ TProfiler }

function TProfiler.Add(const AType:TType): Integer;

  function Find(const ARoutine:TType):Integer;
  var t : Integer;
  begin
    for t:=0 to Count-1 do
        if Items[t].Routine=ARoutine then
           Exit(t);

    result:=-1;
  end;

begin
  result:=Find(AType);

  if result=-1 then
  begin
    Inc(Count);

    if Length(Items)<Count then
       SetLength(Items,Count+100);

    result:=Count-1;
    Items[result].Routine:=AType;
  end;
end;

procedure TProfiler.Clear;
begin
  Items:=nil;
  Count:=0;

  Coverage.Clear;
end;

function TProfiler.DoEnter(const AType:TType):Integer;
begin
  result:=Add(AType);

  Inc(Items[result].CallCount);
end;

procedure TProfiler.DoExit(const AParent,AIndex:Integer; const Elapsed:Int64);
begin
  Inc(Items[AIndex].Elapsed,Elapsed);

  if AParent<>-1 then
     Items[AIndex].Callers.DoExit(AParent,Elapsed);
end;

function TProfiler.TotalCount: Int64;
var t : Integer;
begin
  result:=0;

  for t:=0 to Count-1 do
      Inc(result,Items[t].CallCount);
end;

function TProfiler.TotalElapsed: Int64;
var t : Integer;
begin
  result:=0;

  for t:=0 to Count-1 do
      Inc(result,Items[t].Elapsed);
end;

end.
