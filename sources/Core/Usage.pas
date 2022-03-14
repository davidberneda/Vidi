unit Usage;

interface

uses
  Sys;

type
  TNodeUsed=record
    Node : TNode;
  end;

  TNodeReferences=Array of TNodeUsed;

  TNodeUsage=record
  public
    Node : TNode;
    References : TNodeReferences;

    procedure Add(const AReference:TNode);
    function IndexOf(const ANode:TNode):Integer;
  end;

  TNodeUsages=class
  public
    Items : Array of TNodeUsage;

    procedure Add(const ANode,AReference:TNode);
    function IndexOf(const ANode:TNode):Integer;
    function UsageOf(const ANode:TNode):Integer;
  end;

implementation

procedure TNodeUsage.Add(const AReference:TNode);
var L : Integer;
begin
  L:=Length(References);
  SetLength(References,L+1);

  References[L].Node:=AReference;

  // TODO:
//  References[L].Position:=APosition;
end;

{ TNodeUsages }

procedure TNodeUsages.Add(const ANode,AReference:TNode);
var Index : Integer;
begin
  Index:=IndexOf(ANode);

  if Index=-1 then
  begin
    Index:=Length(Items);
    SetLength(Items,Index+1);

    Items[Index].Node:=ANode;
  end;

  Items[Index].Add(AReference);
end;

function TNodeUsages.IndexOf(const ANode:TNode):Integer;
var t : Integer;
begin
  for t:=0 to High(Items) do
      if Items[t].Node=ANode then
         Exit(t);

  result:=-1;
end;

function TNodeUsages.UsageOf(const ANode:TNode):Integer;
var tmp : Integer;
begin
  tmp:=IndexOf(ANode);

  if tmp=-1 then
     result:=0
  else
     result:=Length(Items[tmp].References);
end;

function TNodeUsage.IndexOf(const ANode: TNode): Integer;
var t : Integer;
begin
  for t:=0 to High(References) do
      if References[t].Node=ANode then
         Exit(t);

  result:=-1;
end;

end.
