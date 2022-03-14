unit Checker.Usage;

// Disable parameter usage warnings
{.$DEFINE SKIP_PARAM_USAGE}  // TODO: Maybe convert to "hints", not errors

interface

uses
  Sys, AST, Usage, Exceptions, Syntax;

type
  TOnUsageError=function(const AError:TErrors; const ANode:TNode):TNodeError of object;

  TUsageChecker=record
  public
    //Owner : TNode;

    Usages : TNodeUsages;

    DoError : TOnUsageError;

    procedure CheckParameterUsage(const ANodes:TNodes);
    procedure CheckUnusedItems(const ANodes:TNodes; const IsClass,IsMain:Boolean);
    procedure Process(const ANode:TNode);
    procedure ProcessBlock(const ABlock:TType);
    procedure ProcessNode(const AUsed:TNode);
  end;

implementation

uses
  {$IFDEF INTERNAL}
  Internal,
  {$ENDIF}
  Checker.AST;

procedure TUsageChecker.Process(const ANode:TNode);

  procedure CheckType(const ANode:TNode; const AUsedType:TType); overload;
  begin
    Usages.Add(TChecker.TheTypeOf(AUsedType),ANode);

    if AUsedType is TGenericType then
       Usages.Add(TGenericType(AUsedType).Variable,ANode);
  end;

  procedure CheckType(const AUsedType:TType); overload;
  begin
    CheckType(ANode,AUsedType);
  end;

  procedure Fill(const AUsed:TNode);
  begin
    if AUsed is TVariable then
       Usages.Add(AUsed,ANode)
    else
       Process(AUsed);
  end;

  procedure FillParameters(const AItems:TNodes);
  var N : TNode;
  begin
    for N in AItems do
        Fill(N);
  end;

  procedure FillItems(const AItems:TNodes);
  var N : TNode;
  begin
    for N in AItems do
        if N is TVariable then
        begin
          // declaration:  X:Integer:=1

          if TVariable(N).VariableType<>nil then
             CheckType(N,TVariable(N).VariableType);

          Fill(TVariable(N).ValueData);
        end
        else
          Fill(N);
  end;

  procedure FillWhen(const AWhen:TWhen);
  var W : TWhenItem;
  begin
    Fill(AWhen.Expression);

    for W in AWhen.Items do
        Fill(W);

    Fill(AWhen.ElseBlock);
  end;

  procedure FillDataCall(const ACall:TDataCall);
  begin
    CheckType(ACall.Routine);
    FillParameters(ACall.Parameters);
  end;

  procedure FillTry(const ATry:TTry);
  var C : TCatch;
  begin
    Fill(ATry.Block);

    for C in ATry.Catch do
    begin
      if C.Error is TType then
         CheckType(TType(C.Error)) // type: catch MyError {}
      else
         Fill(C.Error); // variable: catch X:MyError {}

      Fill(C.Block);
    end;

    Fill(ATry.TheFinally);
  end;

begin
  if ANode=nil then
     Exit;

  if ANode is TType then
  begin
    if ANode is TParametersType then
       FillItems(TParametersType(ANode).Parameters);

    if ANode is TSpecializedType then
       FillItems(TSpecializedType(ANode).Generics);

    if ANode is TExtender then
       Process(TExtender(ANode).Extension)
    else
    if ANode is TRoutine then
    begin
      if TRoutine(ANode).Output<>nil then
         CheckType(TRoutine(ANode).Output);
    end;
  end
  else
  if ANode is TBlockStatement then
 // DONT.   FillItems(TBlockStatement(ANode).Block.Items)
  else
  if ANode is TReturn then
     Fill(TReturn(ANode).Value)
  else
  if ANode is TIf then
  begin
    Fill(TIf(ANode).Condition);
    Fill(TIf(ANode).ThenBlock);
    Fill(TIf(ANode).ElseBlock);
  end
  else
  if ANode is TFor then
  begin
    if TFor(ANode).InExpression=nil then
    begin
      Fill(TFor(ANode).First);
      Fill(TFor(ANode).Last);
    end
    else
      Fill(TFor(ANode).InExpression);

    Fill(TFor(ANode).Block);
  end
  else
  if ANode is TLoopStatement then  // before TLogicalStatement !
  begin
    Fill(TLoopStatement(ANode).Condition);

    // For Repeat only, Block is a BlockStatement
    if TLoopStatement(ANode).Block is TBlockStatement then
       ProcessBlock(TBlockStatement(TLoopStatement(ANode).Block).Block)
    else
       Fill(TLoopStatement(ANode).Block);
  end
  else
  if ANode is TWhen then // before TLogicalStatement !
     FillWhen(TWhen(ANode))
  else
  if ANode is TLogicalStatement then
     Fill(TLogicalStatement(ANode).Condition)
  else
  if ANode is TWhenItem then
  begin
    Fill(TWhenItem(ANode).Expression);
    Fill(TWhenItem(ANode).Block);
  end
  else
  if ANode is TAssignment then
  begin
    Fill(TAssignment(ANode).Variable);
    Fill(TAssignment(ANode).Value);
  end
  else
{  if ANode is TVariable then
     Check(ANode)
  else}
  if ANode is TCallData then
     Fill(TCallData(ANode).Value)
  else
  if ANode is TVariableCall then // <-- Before TDataCall !
  begin
    Fill(TVariableCall(ANode).Variable);
    FillDataCall(TVariableCall(ANode));
  end
  else
  if ANode is TDataCall then
     FillDataCall(TDataCall(ANode))
  else
  if ANode is TTypeCall then
     CheckType(TTypeCall(ANode).TheType)
  else
  if ANode is TOperand then
  begin
    if ANode is TCondition then
       Fill(TCondition(ANode).Condition);

    Fill(TOperand(ANode).Left);
    Fill(TOperand(ANode).Right);
  end
  else
  if ANode is TGroup then
     Fill(TGroup(ANode).Expression)
  else
  if ANode is TMember then
  begin
    Fill(TMember(ANode).Data);
    Fill(TMember(ANode).Member);
  end
  else
  if ANode is TArrayExpression then
  begin
    Fill(TArrayExpression(ANode).Data);
    FillParameters(TArrayExpression(ANode).Parameters);
  end
  else
  if ANode is TCastingData then
  begin
    CheckType(TCastingData(ANode).TheType.TheType);
    Fill(TCastingData(ANode).Data);
  end
  else
  if ANode is TAncestor then
     Fill(TAncestor(ANode).DataCall)
  else
  if ANode is TRange then
  begin
    Fill(TRange(ANode).Min);
    Fill(TRange(ANode).Max);
  end
  else
  if ANode is TTry then
     FillTry(TTry(ANode))
  else
  // Not necessary to check these nodes:
  if TChecker.IsLiteral(ANode) or   // Boolean, Float, Integer, Text (literals)
     (ANode is TSelf) or
     (ANode is TBreak) or
     (ANode is TContinue) or
     (ANode is TWith) then

  {$IFDEF INTERNAL}
  else
    InternalError('Cannot check usage of: ',ANode); // Internal
  {$ENDIF}
end;

procedure TUsageChecker.ProcessNode(const AUsed:TNode);

  procedure TryAdd(const AData:TData);
  begin
    if AData<>nil then
       if AData is TVariable then
          Usages.Add(AData,AUsed)
       else
          ProcessNode(AData);
  end;

  procedure TryAddArrayType(const AType:TArrayType);
  begin
    Usages.Add(AType.TheType,AUsed);

    //ProcessNode(AType.ArrayType);

    TryAdd(AType.Size);
  end;

var tmp : TType;
begin
  if AUsed is TVariable then
  begin
    tmp:=TChecker.TheTypeOf(TVariable(AUsed).VariableType);

    Usages.Add(tmp,AUsed);

    {
    if tmp is TClassType then
       if TClassType(tmp).Generics<>nil then
          if TClassType(tmp).Ancestor<>nil then
             Usages.Add(TClassType(tmp).Ancestor,AUsed);
    }

    if tmp is TArrayType then
       TryAddArrayType(TArrayType(tmp))
    else
    if tmp is TSpecializedType then
       Usages.Add(TSpecializedType(tmp).TheType,AUsed);

    TryAdd(TVariable(AUsed).ValueData);
  end
  else
    Process(AUsed);
end;

procedure TUsageChecker.ProcessBlock(const ABlock:TType);
var N : TNode;
begin
  Process(ABlock);

  for N in ABlock.Items do
      ProcessNode(N);
end;

procedure TUsageChecker.CheckParameterUsage(const ANodes:TNodes);

  procedure NotUsedError(const ANode:TNode);
  begin
    DoError(TErrors._ParameterNotUsed,ANode).Style:=TErrorStyle.Warning;
  end;

var N : TNode;
begin
  {$IFDEF SKIP_PARAM_USAGE}
  Exit;
  {$ENDIF}

  for N in ANodes do
      if Usages.UsageOf(N)=0 then
         NotUsedError(N);
end;

// Check unused variables (hidden only for classes), and
// hidden methods and hidden subclasses (for both class and routine)
procedure TUsageChecker.CheckUnusedItems(const ANodes:TNodes; const IsClass,IsMain:Boolean);
var N : TNode;
begin
  for N in ANodes do
      if N is TVariable then
      begin
        if (not IsClass) or TVariable(N).Clauses.Hidden then
           if Usages.UsageOf(N)=0 then
              DoError(TErrors._VariableNotUsed,N).Style:=TErrorStyle.Warning;
      end
      else
      if (N is TRoutine) and
         (TRoutine(N).Clauses.Hidden or (TChecker.GetRoutineOf(N.Owner)<>nil) ) then
      begin
        if Usages.UsageOf(N)=0 then
           DoError(TErrors._RoutineNotUsed,N);
      end
      else
      if (N is TClassType) and (IsMain or TClassType(N).Clauses.Hidden) then
      begin
        if Usages.UsageOf(N)=0 then
           DoError(TErrors._ClassNotUsed,N).Style:=TErrorStyle.Warning;
      end
      else
      if N is TWith then
      begin
        if Usages.UsageOf(N)=0 then
// TODO !   DoError(TErrors._WithNotUsed,N);
      end;
end;

end.
