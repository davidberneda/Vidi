unit Checker.Shared;

interface

// Traverse AST nodes looking for at least one non-shared item
// (variable or routine) that is not owned by ARoutine.

uses
  Sys, AST;

type
  TSharedChecker=record
  private
    Routine : TRoutine;

    function AllShared(const ANodes:TNodes):Boolean;
    function IsShared(const ANode:TNode):Boolean;
    function OwnedByRoutine(const ANode:TNode):Boolean;
  public
    class function CheckIsShared(const ARoutine:TRoutine):Boolean; static;
  end;

implementation

uses
  SysUtils,
  {$IFDEF INTERNAL}
  Internal,
  {$ENDIF}
  Checker.AST;

function TSharedChecker.OwnedByRoutine(const ANode:TNode):Boolean;
var tmp : TNode;
begin
  tmp:=ANode.Owner;

  while tmp<>nil do
    if tmp=Routine then
       Exit(True)
    else
       tmp:=tmp.Owner;

  result:=False;
end;

class function TSharedChecker.CheckIsShared(const ARoutine:TRoutine):Boolean;
var S : TSharedChecker;
begin
  S.Routine:=ARoutine;
  result:=S.AllShared(ARoutine.Items);
end;

function TSharedChecker.AllShared(const ANodes:TNodes):Boolean;
var N : TNode;
begin
  for N in ANodes do
      if not IsShared(N) then
         Exit(False);

  result:=True;
end;

// TODO: Convert to a traverse of "used" items, looking for a
// non-local, non-shared item
function TSharedChecker.IsShared(const ANode:TNode):Boolean;

  {$IFDEF INTERNAL}
  procedure DoError(const ANode:TNode);
  begin
    InternalError('Cannot determine if node is shared: ',ANode);
  end;
  {$ENDIF}

  function IsSharedFor(const AFor:TFor):Boolean;
  begin
    if AFor.InExpression=nil then
       result:=IsShared(AFor.First) and IsShared(AFor.Last)
    else
       result:=IsShared(AFor.InExpression);

    if result then
       result:=IsShared(AFor.Block);
  end;

  function IsSharedWhen(const AWhen:TWhen):Boolean;
  var W : TWhenItem;
  begin
    result:=IsShared(AWhen.Expression);

    if result then
    begin
      for W in AWhen.Items do
          if IsShared(W.Expression) and IsShared(W.Block) then
          else
             Exit(False);

      if AWhen.ElseBlock<>nil then
         result:=IsShared(AWhen.ElseBlock);
    end;
  end;

  function IsSharedIf(const AIf:TIf):Boolean;
  begin
    result:=IsShared(AIf.Condition);

    if result then
    begin
      if AIf.ThenBlock<>nil then
         result:=IsShared(AIf.ThenBlock);

      if result then
         if AIf.ElseBlock<>nil then
            result:=IsShared(AIf.ElseBlock);
    end;
  end;

  function IsSharedTry(const ATry:TTry):Boolean;
  var C : TCatch;
  begin
    result:=IsShared(ATry.Block);

    if result then
    begin
      if ATry.TheFinally<>nil then
         result:=IsShared(ATry.TheFinally);

      if result then
         for C in ATry.Catch do
             if not IsShared(C.Block) then
                Exit(False);
    end;
  end;

  function IsDataCallShared(const ACall:TDataCall):Boolean;
  begin
    result:=ACall.Routine<>nil;

    if result then
       result:=
            ACall.Routine.Clauses.Shared or
            (ACall.Routine=Routine) // recursive, same routine
            or
            OwnedByRoutine(ACall.Routine);

    if result then
       result:=AllShared(ACall.Parameters);
  end;

  function IsMemberCallShared(const ACall:TDataCall):Boolean;
  begin
    result:=(ACall.Routine<>nil) and AllShared(ACall.Parameters);
  end;

  function IsSharedMember(const AMember:TMember):Boolean;
  begin
    result:=IsShared(AMember.Data);

    if result then
       if AMember.Member is TDataCall then
          result:=IsMemberCallShared(TDataCall(AMember.Member)) // <-- is .Member also necessary?
       {$IFDEF INTERNAL}
       else
       if AMember.Member is TVariable then // final Foo.Bar <-- Bar is shared because Foo is a final param, or it is a Type
       else
       if AMember.Member is TArrayExpression then
       else
          DoError(AMember.Member);
       {$ENDIF}
  end;

  function OwnedByModule(const ANode:TNode):Boolean;
  begin
    result:=(ANode.Owner is TNamedType) and (ANode.Owner.Owner=nil);
  end;

  // correct?
  function IsExtenderShared(const AExtender:TExtender):Boolean;
  begin
    result:=(AExtender.Extension<>nil) and
            AExtender.Extension.Clauses.Shared
  end;

begin
  result:=False;

  if ANode=nil then // incomplete means not shared
  else
  if ANode is TClassType then
     result:=TClassType(ANode).Clauses.Shared // <-- assert True !!, Shared must be always True for TClassTypes !
  else
  if ANode is TRoutine then
     result:=True // <-- always True, no matter if the Routine ANode is shared or not (it is only declared here, not called)
  else
  if ANode is TExtender then
     result:=IsExtenderShared(TExtender(ANode))
  else
   { unused:
  if ANode is TCall then
     result:=TCall(ANode).Routine.Clauses.Shared // or OwnedBy(ANode,ARoutine) ?
  else
  }
  if ANode is TAssignment then
     result:=IsShared(TAssignment(ANode).Variable) and
             IsShared(TAssignment(ANode).Value)
  else
  if ANode is TVariable then
  begin
    result:=TVariable(ANode).Clauses.Shared or
            TVariable(ANode).Clauses.Final or
            OwnedByRoutine(ANode);

    if not result then
    begin
      result:=OwnedByModule(ANode); // <-- Module-level non-final variables are also considered "shared"

      if result then
         Routine.AccessGlobals:=True;
    end;

    if result then
       if TVariable(ANode).ValueData<>nil then
          result:=IsShared(TVariable(ANode).ValueData);
  end
  else
  if ANode is TMember then
     result:=IsSharedMember(TMember(ANode))
  else
  if ANode is TDataCall then
     result:=IsDataCallShared(TDataCall(ANode))
  else
  if ANode is TCallData then
     result:=IsShared(TCallData(ANode).Value)
  else
  if ANode is TAncestor then
     result:=IsShared(TAncestor(ANode).DataCall)
  else
  if ANode is TCondition then // <-- before TOperand
     result:=IsShared(TCondition(ANode).Condition) and
             IsShared(TCondition(ANode).Left) and
             IsShared(TCondition(ANode).Right)
  else
  if ANode is TOperand then
     result:=IsShared(TOperand(ANode).Left) and
             IsShared(TOperand(ANode).Right)
  else
  if ANode is TIf then
     result:=IsSharedIf(TIf(ANode))
  else
  if ANode is TSelf then // Self needs instance !
  else
  if ANode is TGroup then
     result:=IsShared(TGroup(ANode).Expression)
  else
  if ANode is TCastingData then
     result:=IsShared(TCastingData(ANode).Data)
  else
  if ANode is TBlockStatement then
     result:=AllShared(TBlockStatement(ANode).Block.Items)
  else
  if ANode is TReturn then
     result:=IsShared(TReturn(ANode).Value)
  else
  if ANode is TRange then
     result:=IsShared(TRange(ANode).Min) and IsShared(TRange(ANode).Max)
  else
  if ANode is TLoopStatement then // while and repeat
     result:=IsShared(TLoopStatement(ANode).Condition) and IsShared(TLoopStatement(ANode).Block)
  else
  if ANode is TFor then
     result:=IsSharedFor(TFor(ANode))
  else
  if ANode is TWhen then
     result:=IsSharedWhen(TWhen(ANode))
  else
  if ANode is TTry then
     result:=IsSharedTry(TTry(ANode))
  else
  if ANode is TArrayExpression then
     result:=((TArrayExpression(ANode).Data=nil) or IsShared(TArrayExpression(ANode).Data)) and
             AllShared(TArrayExpression(ANode).Parameters)
  else
  // TODO: Invert logic. Return True by default, no errors
  if (ANode is TTypeCall) or
     (ANode is TBreak) or
     (ANode is TContinue) or
     (ANode is TWith) or
     TChecker.IsLiteral(ANode) then
       result:=True

  {$IFDEF INTERNAL}
  else
     DoError(ANode);
  {$ENDIF}
end;

end.
