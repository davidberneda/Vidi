unit Stops;

interface

uses
  Map, Gutter;

function CodeStopsOf(const P:TNodePositions; const AText:String):TStopsArray;

implementation

uses
  Sys, AST, Syntax;

function CodeStopsOf(const P:TNodePositions; const AText:String):TStopsArray;
var L : Integer;

  procedure AddStop(const APos:Integer);
  var tmpL : Integer;
  begin
    L:=APos;

    tmpL:=Length(result);
    SetLength(result,tmpL+1);
    result[tmpL]:=L;
  end;

var t,tmp : Integer;
    tmpNode : TNode;
begin
  result:=nil;

  if P=nil then
     Exit;

  L:=-1;

  for t:=0 to P.Count-1 do
  begin
    tmp:=P.Items[t].Position.Line;

    if tmp>L then
    begin
      tmpNode:=P.Items[t].Node;

      if tmpNode<>nil then
      begin
        if tmpNode is TRoutine then
        begin
          if (P.Items[t].Style=TPositionStyle.Symbol) and
             (AText[P.Items[t].Position.Position]=TSyntax._EndBlock) then
                AddStop(tmp);
        end
        else
        if (tmpNode is TClassType) or
           (tmpNode is TVariable) or
           (tmpNode is TNamedType) then
        else
           AddStop(tmp);
      end;
    end;
  end;
end;

end.
