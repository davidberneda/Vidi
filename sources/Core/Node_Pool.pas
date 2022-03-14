unit Node_Pool;

interface

uses Sys, AST;

type
  TNodeClass=class of TNode;

  TLiveNodes=record
    NodeClass : TNodeClass;
    Items : TNodes;

    procedure Add(const ANode:TNode);
    function IsEmpty:Boolean;
    procedure Remove(const ANode:TNode);
  end;

  TNodePool=record
  public
    Items : Array of TLiveNodes;

    procedure Add(const ANode:TNode);
    function AddClass(const ANode:TNodeClass):Integer;
    function CountOf(const ANode:TNode):Integer;
    procedure Init;
    function IndexOf(const AClass:TNodeClass):Integer;
    function IsEmpty:Boolean;
    procedure Remove(const ANode:TNode);
  end;

var
  NodePool : TNodePool;

implementation

uses
  SysUtils;

{ TNodePool }

procedure TNodePool.Add(const ANode: TNode);
var t : Integer;
    tmp : TNodeClass;
begin
  tmp:=TNodeClass(ANode.ClassType);

  t:=IndexOf(tmp);

  if t=-1 then
     t:=AddClass(tmp);

  Items[t].Add(ANode);
end;

function TNodePool.AddClass(const ANode: TNodeClass): Integer;
begin
  result:=Length(Items);
  SetLength(Items,result+1);

  Items[result].NodeClass:=ANode;
end;

function TNodePool.CountOf(const ANode: TNode): Integer;
var t : Integer;
begin
  t:=IndexOf(TNodeClass(ANode.ClassType));

  if t=-1 then result:=0 else result:=Length(Items[t].Items);
end;

function TNodePool.IndexOf(const AClass: TNodeClass): Integer;
var t : Integer;
begin
  for t:=0 to High(Items) do
    if Items[t].NodeClass=AClass then
       Exit(t);

  result:=-1;
end;

procedure TNodePool.Init;
var t : Integer;
begin
  for t:=0 to High(Items) do
      Items[t].Items:=nil;

  Items:=nil;
end;

function TNodePool.IsEmpty: Boolean;
var t : Integer;
begin
  result:=True;

  for t:=Low(Items) to High(Items) do
      if not Items[t].IsEmpty then
         Exit(False);
end;

procedure TNodePool.Remove(const ANode: TNode);

  procedure PoolError(const S:String);
  begin
    raise Exception.Create('Error NodePool does not contain class: '+S);
  end;

var t : Integer;
    tmp : TNodeClass;
begin
  tmp:=TNodeClass(ANode.ClassType);
  t:=IndexOf(tmp);

  if t=-1 then
     PoolError(tmp.ClassName);

  Items[t].Remove(ANode);
end;

{ TLiveNodes }

procedure TLiveNodes.Add(const ANode: TNode);
var t : Integer;
begin
  t:=Length(Items);
  SetLength(Items,t+1);
  Items[t]:=ANode;
end;

function TLiveNodes.IsEmpty: Boolean;
var t : Integer;
begin
  result:=True;

  for t:=Low(Items) to High(Items) do
      if Items[t]<>nil then
         Exit(False);
end;

procedure TLiveNodes.Remove(const ANode: TNode);
var t : Integer;
begin
  for t:=Low(Items) to High(Items) do
      if Items[t]=ANode then
      begin
        Items[t]:=nil;
        Exit;
      end;
end;

end.
