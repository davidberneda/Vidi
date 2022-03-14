unit RunningNode;

interface

uses
  Sys, Instance_Type;

type
  TRunStack=record
  public
    Items : Array of TNode;

    procedure Clear;
    procedure Pop;
    procedure Push(const AItem:TNode);
  end;

  {$IFDEF INTERNAL}
  {$DEFINE RUNSTACK}
  {$ENDIF}

  TRunningNode=class(TNode)
  public
    Instance : TInstance;
    Caller : TRunningNode;

    {$IFDEF RUNSTACK}
    class var RunStack : Array of TRunningNode;

    Constructor Create(const ACaller:TRunningNode);
    Destructor Destroy; override;

    class function LastStack:TRunningNode; static;
    class function StackAsText(const AItem:TRunningNode):String; static;
    {$ENDIF}

    function CanDestroy(const ANode:TNode):Boolean; virtual; // <-- Remove this someday !!!
    function Next:TNode; virtual; abstract;
  end;

{$IFDEF RUNSTACK}
procedure FreeRunStack;
{$ENDIF}

implementation

{$IFDEF INTERNAL}
uses
  Internal, Utils.AST, SysUtils;
{$ENDIF}

{ TRunStack }

procedure TRunStack.Clear;
begin
  Items:=nil;
end;

procedure TRunStack.Pop;
begin
  Delete(Items,High(Items),1);
end;

procedure TRunStack.Push(const AItem: TNode);
var L : Integer;
begin
  L:=Length(Items);
  SetLength(Items,L+1);
  Items[L]:=AItem;
end;

{$IFDEF RUNSTACK}

function ExistsRunStack(const ANode:TRunningNode):Boolean;
var Item : TRunningNode;
begin
  for Item in TRunningNode.RunStack do
      if Item=ANode then
         Exit(True);

  result:=False;
end;

function GetStack(const AIndex:Integer):String;
var t : Integer;
begin
  result:=#10#13+#10#13;

  for t:=0 to High(TRunningNode.RunStack) do
  begin
    result:=result+#10#13;

    if t=AIndex then
       result:=result+'* ';

    result:=result+TRunningNode.StackAsText(TRunningNode.RunStack[t]);

//                     ' Caller: '+TRunningNode.StackAsText(TRunningNode.RunStack[t].Caller);
  end;

  result:=result+#10#13;
end;

procedure AddRunStack(const ANode:TRunningNode);
var L : Integer;
begin
  if ExistsRunStack(ANode) then
     InternalError('Runstack node already exists',ANode);

  L:=Length(TRunningNode.RunStack);

  if L>0 then
     if TRunningNode.RunStack[L-1]<>ANode.Caller then
         InternalError('Runstack node caller is not previous stack item.'+
               GetStack(L-1)
         ,ANode);

  SetLength(TRunningNode.RunStack,L+1);
  TRunningNode.RunStack[L]:=ANode;
end;

procedure FreeRunStack;
var t : Integer;
begin
  for t:=High(TRunningNode.RunStack) downto Low(TRunningNode.RunStack) do
      TRunningNode.RunStack[t].Free;

  TRunningNode.RunStack:=nil;
end;

procedure RemoveRunStack(const ANode:TRunningNode);
var H, t : Integer;
begin
  H:=High(TRunningNode.RunStack);

  for t:=H downto 0 do
    if TRunningNode.RunStack[t]=ANode then
    begin
      if t=H then
         SetLength(TRunningNode.RunStack,H)
      else
         InternalError('RunStack remove is not last item: '+GetStack(t),ANode);

      Exit;
    end;

  InternalError('RunStack remove item not found',ANode);
end;

{ TRunningNode }

Constructor TRunningNode.Create(const ACaller:TRunningNode);
begin
  inherited Create;

  Caller:=ACaller;
  AddRunStack(Self);
end;

Destructor TRunningNode.Destroy;
begin
  RemoveRunStack(Self);
  inherited;
end;

class function TRunningNode.LastStack:TRunningNode;
begin
  if Length(RunStack)=0 then
     result:=nil
  else
     result:=TRunningNode.RunStack[High(TRunningNode.RunStack)];
end;

class function TRunningNode.StackAsText(const AItem:TRunningNode):String;
begin
  result:=TASTUtils.RemoveTPrefix(AItem);

  if Copy(result,1,7)='Running' then
     Delete(result,1,7);

  result:=result+' ID: '+IntToStr(AItem.Instance.ID);
end;

(*
class function TRunningNode.StackParams(const AItem:TRunningNode):String;
begin
  if AItem is TRunningMember then
     result:=B(TRunningMember(AItem).Member)
  else
  if AItem is TRunningExpression then
     result:=B(TRunningExpression(AItem).Expression)
  else
     result:='';
end;
*)
{$ENDIF}

// Try to Remove this temporary !!!
function TRunningNode.CanDestroy(const ANode:TNode):Boolean;
begin
  result:=True;
end;

end.
