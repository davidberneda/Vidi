unit Run_Breaks;

interface

uses
  Sys;

type
  TBreakHere=record
  public
    Enabled,
    StopExecution,
    OutputExpression : Boolean;

    Node : TNode;
    Line : Integer;

    Condition,
    Expression : String;
  end;

  TBreaks=class
  private
    procedure DeleteItem(const AIndex:Integer);
  public
    Items : Array of TBreakHere;

    procedure Add(const ANode:TNode; const ALine:Integer);
    function Find(const ANode:TNode):Integer;
    procedure Remove(const ANode:TNode);
    function RemoveIn(const AModule:TNode):Boolean;
  end;

implementation

uses
  Find.AST;

{ TBreaks }

procedure TBreaks.Add(const ANode: TNode; const ALine: Integer);
var L : Integer;
begin
  L:=Length(Items);
  SetLength(Items,L+1);

  Items[L].Enabled:=True;
  Items[L].Node:=ANode;
  Items[L].Line:=ALine;

  Items[L].StopExecution:=True;
end;

function TBreaks.Find(const ANode: TNode): Integer;
var t : Integer;
begin
  for t:=Low(Items) to High(Items) do
      if Items[t].Node=ANode then
         Exit(t);

  result:=-1;
end;

function TBreaks.RemoveIn(const AModule:TNode):Boolean;
var t : Integer;
begin
  result:=False;

  t:=Low(Items);

  while t<=High(Items) do
  begin
    if TFinder.ModuleOf(Items[t].Node)=AModule then
    begin
      result:=True;
      DeleteItem(t);
    end
    else
      Inc(t);
  end;
end;

procedure TBreaks.DeleteItem(const AIndex:Integer);
{$IFDEF FPC}
var t : Integer;
begin
  for t:=AIndex to High(Items)-1 do
      Items[t]:=Items[t+1];

  SetLength(Items,High(Items));
{$ELSE}
begin
  Delete(Items,AIndex,1);
{$ENDIF}
end;

procedure TBreaks.Remove(const ANode: TNode);
var tmp : Integer;
begin
  tmp:=Find(ANode);

  if tmp<>-1 then
     DeleteItem(tmp);

end;

end.
