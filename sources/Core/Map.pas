unit Map;

{$IFDEF INTERNAL}
// When sorting positions, make sure there are no duplicates
{$DEFINE CHECK_POSITIONS}
{$ENDIF}

interface

uses
  Sys, Position, Sort;

type
  TPositionStyle=(Identifier,Keyword,Symbol,Literal,Comment);

  TNodePosition=record
  public
    Node : TNode;
    Position : TPosition;
    Length : Integer;
    Style : TPositionStyle;
  end;

  TNodePositionItems=Array of TNodePosition;

  TNodePositions=class
  private
    {$IFDEF CHECK_POSITIONS}
    procedure CheckDuplicates;
    {$ENDIF}

    function Compare(const a,b:Integer):TComparison;
    function FindPastLine(const ARow: Integer): Integer;
    procedure Swap(const a,b:Integer);
  public
    Count : Integer;
    Items : TNodePositionItems;

    function Add(const APos:TNodePosition):Integer;

    {$IFDEF CHECK_POSITIONS}
    procedure CheckMissing(const FullText:String);
    procedure CheckNodes;
    {$ENDIF}

    procedure Clear;
    procedure DeleteFrom(const AStart:Integer);
    function Find(const ANode: TNode; out APosition:TNodePosition):Boolean;
    function FindPosition(const AIndex:Integer; out APosition:TNodePosition):Boolean;
    function IndexOf(const ANode: TNode):Integer; overload;
    function NodeAt(const ARow,AColumn: Integer): TNode; overload;
    function NodeAt(const ARow: Integer): TNode; overload;
    function NodeAtOrBefore(const ARow,AColumn: Integer): TNode;
    function NodeIndexAt(const ARow,AColumn: Integer): Integer;
    function NodeIndexAtOrBefore(const ARow,AColumn: Integer): Integer;
    procedure RemoveLastNode(const ANode:TNode);
    procedure RemoveNode(const FromPos:Integer);
    procedure ReplaceLastNode(const ANode:TNode);
    procedure Sort;
  end;

implementation

uses
  {$IFDEF CHECK_POSITIONS}
  Exceptions, Parser,
  {$ENDIF}
  AST,

  {$IFDEF INTERNAL}
  Internal,
  {$ENDIF}

  SysUtils;

{ TNodePositions }

function TNodePositions.Add(const APos:TNodePosition):Integer;
begin
  result:=Count;

  Inc(Count);

  if Length(Items)<Count then
     SetLength(Items,Count+64); // Delta

  Items[result]:=APos;
end;

function TNodePositions.FindPastLine(const ARow: Integer): Integer;
var t : Integer;
begin
  // TODO: Bubble search Line
  for t:=0 to Count-1 do
      if Items[t].Position.Line>ARow then
         Exit(t);

  result:=Count-1;
end;

// Return index of nearest position at (or before) ARow and AColumn
function TNodePositions.NodeIndexAtOrBefore(const ARow,AColumn: Integer): Integer;
begin
  result:=FindPastLine(ARow)-1;
end;

// Return index of position in ARow and containing AColumn
function TNodePositions.NodeIndexAt(const ARow,AColumn: Integer): Integer;
var t : Integer;
    i : TNodePosition;
    tmpPast : Integer;
begin
  tmpPast:=FindPastLine(ARow);

  for t:=tmpPast-1 downto 0 do // Backwards
  begin
    i:=Items[t];

    if i.Position.Line=ARow then
       if (AColumn>=i.Position.Column) and
          (AColumn<=i.Position.Column+i.Length) then
           Exit(t);
  end;

  result:=-1;
end;

// Owner of ANode that should be stopped the execution at:
function TopNodeOf(const ANode:TNode):TNode;
begin
  result:=ANode;

  while result<>nil do
     if result is TStatement then
        break
     else
        result:=result.Owner;

  {
  if result.Owner is TMember then
     result:=result.Owner;

  if result.Owner is TCallData then
     result:=result.Owner;
  }
end;

// Return first Block in ARow
function TNodePositions.NodeAt(const ARow: Integer): TNode;
var t,
    tmp : Integer;
begin
  result:=nil;

  // TODO: Speed optimization. Do bubble search Line instead of loop
  for t:=0 to Count-1 do
  begin
    if Items[t].Position.Line=ARow then
       if (result=nil) and (Items[t].Node<>nil) then
       begin
         tmp:=IndexOf(Items[t].Node);

         if Items[tmp].Position.Line=ARow then
         begin
           result:=TopNodeOf(Items[t].Node);

           if result<>nil then
              Exit;
         end;
       end;

    if Items[t].Position.Line>ARow then
       Exit;
  end;
end;

function TNodePositions.NodeAtOrBefore(const ARow,AColumn: Integer): TNode;
var tmp : Integer;
begin
  tmp:=NodeIndexAtOrBefore(ARow,AColumn);

  if tmp<=-1 then
     result:=nil
  else
     result:=Items[tmp].Node;
end;

function TNodePositions.NodeAt(const ARow,AColumn: Integer): TNode;
var tmp : Integer;
begin
  tmp:=NodeIndexAt(ARow,AColumn);

  if tmp=-1 then
     result:=nil
  else
     result:=Items[tmp].Node;
end;

function TNodePositions.Compare(const a,b:Integer):TComparison;
var PA,PB : Integer;
begin
  PA:=Items[a].Position.Position;
  PB:=Items[b].Position.Position;

  if PA<PB then result:=TComparison.Lower else
  if PA>PB then result:=TComparison.Greater else
     result:=TComparison.Equal;
end;

procedure TNodePositions.ReplaceLastNode(const ANode:TNode);
begin
  Items[Count-1].Node:=ANode;
end;

procedure TNodePositions.RemoveNode(const FromPos:Integer);
var t : Integer;
begin
  for t:=FromPos to Count-1 do
      Items[t].Node:=nil;
end;

// Ugly, but no alternative so far
procedure TNodePositions.RemoveLastNode(const ANode:TNode);
var t : Integer;
    tmpReplaced : Boolean;
begin
  tmpReplaced:=False;
  t:=Count-1;

  while t>=0 do
  begin
    if Items[t].Node=ANode then
    begin
      Items[t].Node:=nil;
      tmpReplaced:=True;
    end
    else
    if tmpReplaced then
       break;

    Dec(t);
  end;
end;

procedure TNodePositions.Swap(const a,b:Integer);
var tmp : TNodePosition;
begin
  tmp:=Items[a];
  Items[a]:=Items[b];
  Items[b]:=tmp;
end;

{$IFDEF CHECK_POSITIONS}
procedure TNodePositions.CheckDuplicates;
var t : Integer;
begin
  for t:=1 to Count-1 do
      if Items[t].Position.Position=Items[t-1].Position.Position then
         Raise_Exception('Error duplicate position: '+Items[t].Position.LineColumn+' '+IntToStr(Items[t].Position.Position));
end;

procedure TNodePositions.CheckNodes;
var t : Integer;
    tmp : TNode;
begin
  for t:=0 to Count-1 do
  begin
    tmp:=Items[t].Node;

    if tmp<>nil then
       try
         if (tmp.ClassType=TNodeError) or (tmp.ClassName='') then
       except
         on E:Exception do
         begin
           // Error AV, Node has already been freed / destroyed !
           raise;
         end;
       end;
  end;
end;

procedure TNodePositions.CheckMissing(const FullText:String);

  procedure DoError(const AFrom:Integer; const APosition:String);
  begin
    Raise_Exception('Error missing positioned text: '+IntToStr(AFrom)+' '+APosition);
  end;

var
  Prev : Integer;

  procedure SkipBlanks;
  begin
    while Prev<Length(FullText) do
          if CharInSet(FullText[Prev],TParser.Blanks) then
             Inc(Prev)
          else
             break;
  end;

var t : Integer;
    P : TNodePosition;
begin
  if Count>1 then
  begin
    Prev:=1;
    SkipBlanks;

    P:=Items[0];

    if Prev<>P.Position.Position then
       DoError(1,IntToStr(Prev))
    else
       Prev:=P.Position.Position+P.Length;

    for t:=1 to Count-1 do
    begin
      P:=Items[t];

      SkipBlanks;

      if Prev<>P.Position.Position then
         DoError(Prev,P.Position.LineColumn);

      Prev:=P.Position.Position+P.Length;
    end;
  end
  else
  if Count>0 then
     Prev:=1+Items[0].Length
  else
     Prev:=1; // ??

  SkipBlanks;

  if Prev<Length(FullText) then
     DoError(Prev,IntToStr(Length(FullText)-1));
end;
{$ENDIF}

procedure TNodePositions.Sort;
begin
  TSort.Sort(True,0,Count-1,Compare,Swap);

  {$IFDEF CHECK_POSITIONS}
  CheckDuplicates;
  {$ENDIF}
end;

procedure TNodePositions.Clear;
begin
  Items:=nil;
  Count:=0;
end;

procedure TNodePositions.DeleteFrom(const AStart: Integer);
begin
  if Count-AStart>0 then
  begin
    (*
    {$IFDEF FPC}
    SetLength(Items,AStart);
    {$ELSE}
    Delete(Items,AStart,Count-AStart);
    {$ENDIF}
    *)
    Count:=AStart;
  end
  {$IFDEF INTERNAL}
  else
//    InternalError('DeleteFrom Positions useless',nil);
  {$ENDIF}
end;

function TNodePositions.IndexOf(const ANode: TNode):Integer;
var t : Integer;
begin
  for t:=0 to Count-1 do
      if Items[t].Node=ANode then
         Exit(t);

  result:=-1;
end;

function TNodePositions.FindPosition(const AIndex:Integer; out APosition:TNodePosition):Boolean;
begin
  result:=AIndex<>-1;

  if result then
     APosition:=Items[AIndex];
end;

function TNodePositions.Find(const ANode: TNode; out APosition:TNodePosition):Boolean;
begin
  result:=FindPosition(IndexOf(ANode),APosition);
end;

end.
