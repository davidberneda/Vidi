unit Evaluator.CompileTime;

interface

uses
  Sys;

type
  TEvaluateCompile=record
    class function AsData(const AData:TData; const AcceptParameters:Boolean):TData; static;

    class function AsBoolean(const AData:TData):Boolean; static;
    class function AsInteger(const AData:TData):Int64; static;

    class function Evaluable(const AData:TData; const AcceptParameters:Boolean):Boolean; static;

    class function EvaluableAsBoolean(const AData:TData;
                                      out AValue:Boolean):Boolean; static;
    class function EvaluableAsInteger(const AData:TData;
                                      out AValue:Int64):Boolean; static;

    class function IsInteger(const AData:TData; const AValue:Integer):Boolean; static;
    class function SameValue(const D1,D2:TData):Boolean; static;
  end;

implementation

uses
  AST, SysUtils, Evaluator,
  {$IFDEF INTERNAL}
  Internal,
  {$ENDIF}
  Checker.AST;

class function TEvaluateCompile.AsData(const AData:TData; const AcceptParameters:Boolean):TData;

  function IsAbstractCall(const ACall:TDataCall):Boolean;
  begin
    if ACall.Routine is TRoutine then
       result:= TChecker.IsAbstract(TRoutine(ACall.Routine))
    else
       result:=False;
  end;

  function MemberEvaluable(const AData,AMember:TData):TData;
  begin
    if Evaluable(AData,AcceptParameters) and
       Evaluable(AMember,AcceptParameters) then
       result:=AMember
    else
       result:=nil;
  end;

  function EvaluableNodes(const AItems:TNodes):Boolean;
  var N : TNode;
  begin
    for N in AItems do
        if (not (N is TData)) or (not TEvaluateCompile.Evaluable(TData(N),AcceptParameters)) then
           Exit(False);

    result:=True;
  end;

  function RoutineAccessGlobals(const ARoutine:TType):Boolean;
  begin
    if ARoutine is TRoutine then
       result:=TRoutine(ARoutine).AccessGlobals
    else
       result:=False;
  end;

  function CompileTimeCallable(const ACall:TDataCall):Boolean;

  begin
    result:=ACall.Routine.Clauses.Shared and
            (not IsAbstractCall(ACall)) and
            EvaluableNodes(ACall.Parameters) and
            (not RoutineAccessGlobals(ACall.Routine));
  end;

  function ArrayDataEvaluable(const AData:TData):Boolean;
  begin
    result:=(AData=nil) or // []
            Evaluable(AData,AcceptParameters);
  end;

  function ArrayOperable(const AOperand:TOperand):Boolean;
  begin
    result:=(AOperand is TIsEqual) or
            (AOperand is TIsNotEqual) or
            (AOperand is TAddition);  // and CompatibleArrays(L,R)
  end;

  function EnumerationOperable(const AOperand:TOperand):Boolean;
  begin
    result:=(AOperand is TIsEqual) or
            (AOperand is TIsNotEqual);
  end;

  type
    TOperandClass=class of TOperand;

  function IsOperable(const AOperand:TOperand; const AList:Array of TOperandClass):Boolean;
  var t : Integer;
  begin
    for t:=Low(AList) to High(AList) do
        if AList[t]=AOperand.ClassType then
           Exit(True);

    result:=False;
  end;

  const
    BooleanOperable: Array[0..4] of TOperandClass=(
         TLogicalAnd,
         TLogicalOr,
         TLogicalXor,
         TIsEqual,
         TIsNotEqual);

    NumberOperable: Array[0..12] of TOperandClass=(
         TAddition,
         TSubtraction,
         TMultiplication,
         TDivision,
         TLogicalAnd,
         TLogicalOr,
         TLogicalXor,
         TIsEqual,
         TIsNotEqual,
         TIsLower,
         TIsLowerOrEqual,
         TIsGreater,
         TIsGreaterOrEqual);

    TextOperable: Array[0..7] of TOperandClass=(
         TAddition,
         TIsEqual,
         TIsNotEqual,
         TContains,
         TIsLower,
         TIsLowerOrEqual,
         TIsGreater,
         TIsGreaterOrEqual);

  function IsArrayExpression(const AData:TData):Boolean;
  begin
    result:=(AData is TArrayExpression);

    if not result then
       result:=(AData is TVariable) and IsArrayExpression(TVariable(AData).ValueData);
  end;

  function Operable(const AOperand:TOperand; const L,R:TData):Boolean;
  begin
    if AOperand is TCondition then
       result:=Evaluable(TCondition(AOperand).Condition,AcceptParameters)
    else
    if IsArrayExpression(L) and IsArrayExpression(R) then
       result:=ArrayOperable(AOperand)
    else
    if IsArrayExpression(R) and (AOperand is TContains) then
       result:=True
    else
    if IsArrayExpression(L) and (AOperand is TAddition) then
       result:=True
    else
    if (L.Owner=R.Owner) and // Enumerations
       TChecker.IsEnumerationItem(L) and
       TChecker.IsEnumerationItem(R) then
    begin
      result:=EnumerationOperable(AOperand)
    end
    else
    if TChecker.IsLogical(L) and TChecker.IsLogical(R) then
       result:=IsOperable(AOperand,BooleanOperable)
    else
    if TChecker.IsInteger(L) and TChecker.IsInteger(R) then
       result:=IsOperable(AOperand,NumberOperable)
    else
    if TChecker.IsText(L) and TChecker.IsText(R) then
       result:=IsOperable(AOperand,TextOperable)
    else
    if TChecker.IsLogical(L) or TChecker.IsLogical(R) then // <-- True < 1.23
       result:=False
    else
    if TChecker.IsFloat(L) then // Promote to Float, after checking IsInteger
       result:=(TChecker.IsInteger(R) or TChecker.IsFloat(R)) and
               IsOperable(AOperand,NumberOperable)
    else
    if TChecker.IsFloat(R) then // Promote to Float, after checking IsInteger
       result:=(TChecker.IsInteger(L) or TChecker.IsFloat(L)) and
               IsOperable(AOperand,NumberOperable)
    else
       result:=False;
  end;

  function EvaluableOperand(const AOperand:TOperand):TData;
  var L, R : TData;
  begin
    L:=AsData(AOperand.Left,AcceptParameters);

    if L=nil then
       result:=nil
    else
    begin
      R:=AsData(AOperand.Right,AcceptParameters);

      if R=nil then
         result:=nil
      else
      if Operable(AOperand,L,R) then
         result:=AOperand
      else
         result:=nil;
    end;

    {
    if Evaluable(TOperand(AData).Left,AcceptParameters) and
       Evaluable(TOperand(AData).Right,AcceptParameters) then
       result:=AData
    else
       result:=nil;
    }
  end;

var tmp : Boolean;
begin
  if (AData is TInteger) or (AData is TBoolean) or
     (AData is TText) or (AData is TFloat) or
     (AData is TTypeCall) or (AData is TSelf) then
       result:=AData
  else
  if AData is TGroup then
  begin
    if TGroup(AData).Expression=nil then  // protection, ie:   X: := -(
       result:=nil
    else
       result:=AsData(TGroup(AData).Expression,AcceptParameters);
  end
  else
  if AData is TOperand then
     result:=EvaluableOperand(TOperand(AData))
  else
  if AData is TMember then
     result:=MemberEvaluable(TMember(AData).Data,TMember(AData).Member)
  else
  if AData is TVariable then
  begin
    tmp:=TVariable(AData).Clauses.Final;

    if tmp then
       if TVariable(AData).ValueData=nil then
          tmp:=AcceptParameters
       else
          tmp:=Evaluable(TVariable(AData).ValueData,AcceptParameters);

    if tmp then
       result:=AData
    else
       result:=nil;
  end
  else
  if AData is TArrayExpression then
     if ArrayDataEvaluable(TArrayExpression(AData).Data) and
        EvaluableNodes(TArrayExpression(AData).Parameters) then
          result:=AData
        else
          result:=nil
  else
  if AData is TRange then
     if (AData.Owner is TArrayExpression) and   // <-- x::= ['a'..'z']
        Evaluable(TRange(AData).Min,AcceptParameters) and
        Evaluable(TRange(AData).Max,AcceptParameters) then
          result:=AData
     else
          result:=nil  // a range cannot be an expression:   x::= 1..2
  else
  if AData is TDataCall then
  begin
    if CompileTimeCallable(TDataCall(AData)) then
       result:=AData
    else
       result:=nil;
  end
  else
  if AData is TCastingData then
     result:=AsData(TCastingData(AData).Data,AcceptParameters)
  else
  begin
    {$IFDEF INTERNAL}
    InternalError('Cannot check evaluable: ',AData);
    {$ENDIF}

    result:=nil;
  end;
end;

class function TEvaluateCompile.Evaluable(const AData:TData; const AcceptParameters:Boolean):Boolean;
begin
  result:=(AData<>nil) and (TEvaluateCompile.AsData(AData,AcceptParameters)<>nil);
end;

class function TEvaluateCompile.EvaluableAsInteger(const AData:TData;
                                                   out AValue:Int64):Boolean;
var tmp : TData;
begin
  tmp:=TEvaluateCompile.AsData(AData,False {True});
  result:=(tmp<>nil) and TChecker.IsIntegerOrNumber(tmp);

  if result then
     AValue:=TEvaluateCompile.AsInteger(tmp);
end;

class function TEvaluateCompile.EvaluableAsBoolean(const AData:TData;
                                                   out AValue:Boolean):Boolean;
var tmp : TData;
begin
  tmp:=TEvaluateCompile.AsData(AData,False);
  result:=(tmp<>nil) and TChecker.IsLogical(tmp);

  if result then
     AValue:=TEvaluateCompile.AsBoolean(tmp);
end;

class function TEvaluateCompile.IsInteger(const AData: TData;
  const AValue: Integer): Boolean;
var tmp : TData;
begin
  tmp:=TEvaluateCompile.AsData(AData,True);   // <-- should be AcceptParameters=False ??

  if tmp is TInteger then
     result:=AsInteger(tmp)=AValue
  else
  if (tmp is TVariable) and TVariable(tmp).Clauses.Final and
     (TVariable(tmp).ValueData<>nil) then
     result:=IsInteger(TVariable(tmp).ValueData,AValue)
  else
     result:=False;
end;

class function TEvaluateCompile.SameValue(const D1, D2: TData): Boolean;
begin
  result:=TEvaluateCompile.Evaluable(D1,True) and TEvaluateCompile.Evaluable(D2,True);

  if result then
     result:=TEvaluateCompile.AsInteger(D1)=TEvaluateCompile.AsInteger(D2);
end;

class function TEvaluateCompile.AsBoolean(const AData:TData):Boolean;
var tmp : TData;
begin
  if AData is TLogical then
     tmp:=TEvaluate.AsData(AData,nil)
  else
     tmp:=AData;

  if tmp is TBoolean then
     result:=TBoolean(tmp).Value
  else
  begin
    {$IFDEF INTERNAL}
    InternalError('Cannot evaluate as Boolean at compile-time: ',tmp);
    {$ENDIF}

    result:=False;
  end;

  if tmp.Owner=nil then
     tmp.Free;
end;

class function TEvaluateCompile.AsInteger(const AData:TData):Int64;

  function MemberAsInteger(const {AData,}AMember:TData):Int64;
  begin
    result:=AsInteger(AMember);
  end;

  function IsFloatInteger(const AFloat:Float):Boolean;
  begin
    result:=Trunc(AFloat)=AFloat;
  end;

begin
  //result:=TEvaluate.AsInteger(AData);
  //Exit;

  if AData is TInteger then
     result:=TInteger(AData).Value
  else
  if (AData is TFloat) and IsFloatInteger(TFloat(AData).Value) then
     result:=Trunc(TFloat(AData).Value)
  else
  if AData is TGroup then
     result:=AsInteger(TGroup(AData).Expression)
  else
  if AData is TAddition then
     result:=AsInteger(TAddition(AData).Left)+AsInteger(TAddition(AData).Right)
  else
  if AData is TSubtraction then
     result:=AsInteger(TSubtraction(AData).Left)-AsInteger(TSubtraction(AData).Right)
  else
  if AData is TMultiplication then
     result:=AsInteger(TMultiplication(AData).Left)*AsInteger(TMultiplication(AData).Right)
  else
  if AData is TDivision then
     result:=AsInteger(TDivision(AData).Left) div AsInteger(TDivision(AData).Right)
  else
  if AData is TMember then
     result:=MemberAsInteger({TMember(AData).Data,}TMember(AData).Member)
  else
  if AData is TVariable then
     result:=AsInteger(TVariable(AData).ValueData)
  else
  begin
    {$IFDEF INTERNAL}
    InternalError('Cannot evaluate as Integer at compile-time: ',AData);
    {$ENDIF}

    result:=0;
  end;
end;

end.
