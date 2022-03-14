unit Compare.AST;

interface

uses
  Sys,
  AST;

type
  TCodeCompare=record
  public
    class var InternalTesting:Boolean;

    class function SameArray(const L, R: TArrayType): Boolean; static;
    class function SameClassType(const L, R: TClassType): Boolean; static;
    class function SameClauses(const L, R: TClauses): Boolean; static;

    class function SameCode(const L,R:TNode):Boolean; overload; static;
    class function SameCode(const L,R:TNodes):Boolean; overload; static;

    class function SameDataCall(const L, R: TDataCall): Boolean; static;
    class function SameExtender(const L, R: TExtender): Boolean; static;
    class function SameGenericType(const L, R: TGenericType): Boolean; static;
    class function SameIntegerClass(const L, R: TIntegerClass): Boolean; static;
    class function SameNamedType(const L, R: TNamedType): Boolean; static;
    class function SameParametersType(const L, R: TParametersType): Boolean; static;
    class function SameRange(const L, R: TRange): Boolean; static;
    class function SameRoutineType(const L, R: TRoutine): Boolean; static;
    class function SameSpecializedType(const L, R: TSpecializedType): Boolean; static;
    class function SameTry(const L, R: TTry): Boolean; static;
    class function SameType(const L, R: TType): Boolean; static;
    class function SameTypeMember(const L, R: TTypeMember): Boolean; static;
    class function SameTypeReference(const L, R: TType): Boolean; static;
    class function SameVariable(const L, R: TVariable): Boolean; static;
    class function SameWhen(const L, R: TWhen): Boolean; static;
  end;

implementation

uses
  {$IFDEF INTERNAL}
  Internal,
  {$ENDIF}
  SysUtils;

{ TCodeCompare }

class function TCodeCompare.SameClauses(const L, R: TClauses): Boolean;
begin
  result:=(L.Indexed=R.Indexed) and
          (L.Hidden=R.Hidden) and
          (L.Shared=R.Shared) and
          (L.Final=R.Final);
end;

class function TCodeCompare.SameTry(const L, R: TTry): Boolean;

  function SameCatch(const L,R:TCatch):Boolean;
  begin
    result:=SameCode(L.Error,R.Error) and
            SameCode(L.Block,R.Block);
  end;

var t : Integer;
begin
  result:=SameCode(L.Block,R.Block) and
          SameCode(L.TheFinally,R.TheFinally) and
          (Length(L.Catch)=Length(R.Catch));

  if result then
     for t:=0 to High(L.Catch) do
         if not SameCatch(L.Catch[t],R.Catch[t]) then
            Exit(False);
end;

class function TCodeCompare.SameType(const L, R: TType): Boolean;
begin
  result:=SameClauses(L.Clauses,R.Clauses) and
          SameCode(L.Items,R.Items);
end;

class function TCodeCompare.SameParametersType(const L, R: TParametersType): Boolean;
begin
  result:=SameNamedType(L,R) and SameCode(L.Parameters,R.Parameters);
end;

class function TCodeCompare.SameGenericType(const L, R: TGenericType): Boolean;
begin
  result:=SameType(L,R) and
          SameTypeReference(L.FinalType,R.FinalType) and
          SameVariable(L.Variable,R.Variable); // <-- optimize: SameName(L.Variable.Name,R.Variable.Name)
end;

class function TCodeCompare.SameSpecializedType(const L, R: TSpecializedType): Boolean;
begin
  result:=SameType(L,R) and
          SameTypeReference(L.TheType,R.TheType) and
          SameCode(L.Generics,R.Generics);
end;

class function TCodeCompare.SameArray(const L, R: TArrayType): Boolean;
begin
  result:=SameType(L,R) and
          SameTypeReference(L.TheType,R.TheType) and
          SameCode(L.Size,R.Size) and
          SameCode(L.Values,R.Values);
end;

class function TCodeCompare.SameExtender(const L, R: TExtender): Boolean;
begin
  result:=SameTypeReference(L.TheType,R.TheType) and
          SameTypeReference(L.Extension,R.Extension)
end;

class function TCodeCompare.SameIntegerClass(const L, R: TIntegerClass): Boolean;
begin
  result:=SameRange(L.Size.Range,R.Size.Range);
end;

class function TCodeCompare.SameTypeMember(const L, R: TTypeMember): Boolean;
begin
  result:=TCodeCompare.SameTypeReference(L.TheType,R.TheType) and
          TCodeCompare.SameTypeReference(L.Member,R.Member);
end;

{$IFDEF INTERNAL}
procedure DoError(const ANode:TNode);
begin
  InternalError('Cannot samecode: ',ANode);
end;
{$ENDIF}

class function TCodeCompare.SameTypeReference(const L, R: TType): Boolean;
begin
  result:=L=R;

  if not result then
     if (L<>nil) and (R<>nil) and (L.ClassType=R.ClassType) then
        if (L is TTypeMember) and (R is TTypeMember) then
           result:=SameTypeMember(TTypeMember(L),TTypeMember(R))
        else
        if (L is TExtender) and (R is TExtender) then
           result:=SameExtender(TExtender(L),TExtender(R))
        else
        if (L is TNamedType) and (R is TNamedType) then
           result:=SameText(TNamedType(L).Name,TNamedType(R).Name)  // case-insensitive
        else
        if (L is TRangeType) and (R is TRangeType) then
           result:=SameRange(TRangeType(L).Range,TRangeType(R).Range)
        else
        if (L is TArrayType) and (R is TArrayType) then
           result:=SameArray(TArrayType(L),TArrayType(R))
        else
        if (L is TSpecializedType) and (R is TSpecializedType) then
           result:=SameSpecializedType(TSpecializedType(L),TSpecializedType(R))
        else
        if (L is TGenericType) and (R is TGenericType) then
           result:=SameGenericType(TGenericType(L),TGenericType(R))
        else
        if (L is TIntegerClass) and (R is TIntegerClass) then
           result:=SameIntegerClass(TIntegerClass(L),TIntegerClass(R))

        (*
        else
        if (L.Owner is TBlockStatement) and (R.Owner is TBlockStatement) then // Top-level module types, (this comes from TSelf.TheType, surely)
           result:=True
        else
        if (L.Owner.Owner is TNamedType) and (R.Owner.Owner is TNamedType) then // Top-level module types, (this comes from TSelf.TheType, surely)
        *)
        {$IFDEF INTERNAL}
        else
           DoError(L);
        {$ENDIF}
end;

class function TCodeCompare.SameVariable(const L, R: TVariable): Boolean;
begin
  result:=SameText(L.Name,R.Name) and
          SameClauses(L.Clauses,R.Clauses) and
          //Initialized : Boolean;
          SameTypeReference(L.VariableType,R.VariableType) and
          SameCode(L.ValueData,R.ValueData);
end;

class function TCodeCompare.SameWhen(const L, R: TWhen): Boolean;
var t : Integer;
begin
  result:=SameCode(L.Expression,R.Expression) and
          SameCode(L.ElseBlock,R.ElseBlock) and
          (Length(L.Items)=Length(R.Items));

  if result then
     for t:=0 to High(L.Items) do
         if not SameCode(L.Items[t],R.Items[t]) then
            Exit(False);
end;

class function TCodeCompare.SameNamedType(const L, R: TNamedType): Boolean;
begin
  result:=SameText(L.Name,R.Name) and SameType(L,R);
end;

class function TCodeCompare.SameClassType(const L, R: TClassType): Boolean;
begin
  result:=SameParametersType(L,R) and
          SameTypeReference(L.Ancestor,R.Ancestor);
end;

class function TCodeCompare.SameRoutineType(const L, R: TRoutine): Boolean;

   function SameSignature(const L, R: TRoutine): Boolean;
   begin
     result:=SameCode(L.Parameters,R.Parameters) and
             SameTypeReference(L.Ancestor,R.Ancestor) and
             SameTypeReference(L.Output,R.Output);
   end;

   function SameSignatureOrNil(const L, R: TRoutine): Boolean;
   begin
     if (L<>nil) and (R<>nil) then
        result:=SameText(L.Name,R.Name) and SameSignature(L,R)
     else
        result:=(L=nil) and (R=nil);
   end;

begin
  result:=SameParametersType(L,R) and
          SameTypeReference(L.Ancestor,R.Ancestor) and
          SameTypeReference(L.Output,R.Output) and
          SameSignatureOrNil(L.ForwardTo,R.ForwardTo) and
          SameSignatureOrNil(L.ForwardFrom,R.ForwardFrom);
end;

class function TCodeCompare.SameDataCall(const L, R: TDataCall): Boolean;
begin
  result:=SameTypeReference(L.Routine,R.Routine) and
          SameCode(L.Parameters,R.Parameters)
end;

function SameInteger(const L,R:TInteger):Boolean;
begin
  result:=L.Value=R.Value;

  if (L.Text<>'') and (R.Text<>'') then
     result:=(L.Text=R.Text) and result; // double-check, text and value
end;

// Epsilon precision comparison
function SameFloat(const L,R:TFloat):Boolean;
const Eps=1e-13;
begin
  if (L.Text<>'') and (R.Text<>'') then
     result:=L.Text=R.Text
  else
     result:=Abs(L.Value-R.Value)<=Eps;
end;

class function TCodeCompare.SameRange(const L, R: TRange): Boolean;
begin
  result:=SameCode(L.Min,R.Min) and SameCode(L.Max,R.Max);
end;

class function TCodeCompare.SameCode(const L, R: TNode): Boolean;
begin
  result:=L=R;

  if not result then
     if (L<>nil) and (R<>nil) and (L.ClassType=R.ClassType) then
        if L is TCallData then
           result:=SameCode(TCallData(L).Value,TCallData(R).Value)
        else
        if L is TMember then
           result:=SameCode(TMember(L).Data,TMember(R).Data) and
                   SameCode(TMember(L).Member,TMember(R).Member)
        else
        if L is TTypeCall then
           result:=SameTypeReference(TTypeCall(L).TheType,TTypeCall(R).TheType)
        else
        if L is TVariableCall then // <-- before TDataCall !
           result:=SameCode(TVariableCall(L).Variable,TVariableCall(R).Variable) and
                   SameDataCall(TVariableCall(L),TVariableCall(R))
        else
        if L is TDataCall then
           result:=SameDataCall(TDataCall(L),TDataCall(R))
        else
        if L is TText then
           result:=TText(L).Value=TText(R).Value
        else
        if L is TInteger then
           result:=SameInteger(TInteger(L),TInteger(R))
        else
        if L is TBoolean then
           result:=TBoolean(L).Value=TBoolean(R).Value
        else
        if L is TFloat then
           result:=SameFloat(TFloat(L),TFloat(R))
        else
        if L is TReturn then
           result:=SameCode(TReturn(L).Value,TReturn(R).Value)
        else
        if L is TVariable then
           result:=SameVariable(TVariable(L),TVariable(R))
        else
        if (L is TBreak) or
           // (L is TComment) or
           (L is TContinue) then // nothing
             result:=True
        else
        if L is TAssignment then
           result:=SameCode(TAssignment(L).Variable,TAssignment(R).Variable) and
                   SameCode(TAssignment(L).Value,TAssignment(R).Value)
        else
        if L is TOperand then
           result:=SameCode(TOperand(L).Left,TOperand(R).Left) and
                   SameCode(TOperand(L).Right,TOperand(R).Right)
        else
        if L is TBlockStatement then
           result:=SameCode(TBlockStatement(L).Block.Items,TBlockStatement(R).Block.Items)
        else
        if L is TIf then
           result:=SameCode(TIf(L).Condition,TIf(R).Condition) and
                   SameCode(TIf(L).ThenBlock,TIf(R).ThenBlock) and
                   SameCode(TIf(L).ElseBlock,TIf(R).ElseBlock)
        else
        if L is TLoopStatement then // while repeat
           result:=SameCode(TLoopStatement(L).Condition,TLoopStatement(R).Condition) and
                   SameCode(TLoopStatement(L).Block,TLoopStatement(R).Block)
        else
        if L is TFor then
           result:=SameCode(TFor(L).First,TFor(R).First) and
                   SameCode(TFor(L).Last,TFor(R).Last) and
                   SameCode(TFor(L).InExpression,TFor(R).InExpression) and
                   SameCode(TFor(L).Block,TFor(R).Block)

        else
        if L is TArrayExpression then
           result:=SameCode(TArrayExpression(L).Data,TArrayExpression(R).Data) and
                   SameCode(TArrayExpression(L).Parameters,TArrayExpression(R).Parameters)

        else
        if L is TCastingData then
           result:=SameCode(TCastingData(L).TheType,TCastingData(R).TheType) and
                   SameCode(TCastingData(L).Data,TCastingData(R).Data)

        else
        if L is TWhen then
            result:=SameWhen(TWhen(L),TWhen(R))
        else
        if L is TWhenItem then
            result:=SameCode(TWhenItem(L).Expression,TWhenItem(R).Expression) and
                    SameCode(TWhenItem(L).Block,TWhenItem(R).Block)

        else
        if L is TExtender  then // <-- before TNamedType !
           result:=SameExtender(TExtender(L),TExtender(R))
        else
        if L is TClassType then
           result:=SameClassType(TClassType(L),TClassType(R))
        else
        if L is TRoutine then
           result:=SameRoutineType(TRoutine(L),TRoutine(R))
        else
        if L is TParametersType then // <-- before TNamedType
           result:=SameParametersType(TParametersType(L),TParametersType(R))
        else
        if L is TSpecializedType then // <-- before TNamedType
           result:=SameSpecializedType(TSpecializedType(L),TSpecializedType(R))
        else
        if L is TArrayType then // <-- before TNamedType
           result:=SameArray(TArrayType(L),TArrayType(R))
        else
        if L is TNamedType then // At the bottom !
           result:=SameNamedType(TNamedType(L),TNamedType(R))
        else
        if L is TWith then
           result:=SameText(TWith(L).Alias,TWith(R).Alias) and
                   SameTypeReference(TWith(L).Module,TWith(R).Module)
        else
        if L is TRange then
           result:=SameRange(TRange(L),TRange(R))
        else
        if L is TGroup then
           result:=SameCode(TGroup(L).Expression,TGroup(R).Expression)
        else
        if L is TSelf then
           result:=SameTypeReference(TSelf(L).TheType,TSelf(R).TheType)
        else
        if L is TAncestor then
           result:=SameTypeReference(TAncestor(L).Ancestor,TAncestor(R).Ancestor) and
                   SameCode(TAncestor(L).DataCall,TAncestor(R).DataCall)
        else
        if L is TTry then
           result:=SameTry(TTry(L),TTry(R))
        else
        if L is TTypeMember then
           result:=SameTypeMember(TTypeMember(L),TTypeMember(R))

        {$IFDEF INTERNAL}
        else
           DoError(L);
        {$ENDIF}
end;

class function TCodeCompare.SameCode(const L,R:TNodes):Boolean;
var t : Integer;
begin
  result:=Length(L)=Length(R);

  if result then
     for t:=0 to High(L) do
         if not SameCode(L[t],R[t]) then
            Exit(False);
end;

end.
