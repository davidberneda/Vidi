unit Utils.AST;

interface

uses
  Sys, AST;

type
  TASTUtils=record
    class function AssigmentOf(const Assignment:TArithmetic_Assign):String; static;
    class procedure DestroyNodes(var Nodes:TNodes); static;
    class function IsLambda(const AType:TType):Boolean; static;
    class function ModuleOfWith(const AWith:TWith):TNamedType; static;
    class function NameOf(const ANode:TObject; const AddClass:Boolean):String; static;
    class function NodeName(const ANode:TNode):String; static;
    class function OperandToString(const AOperand:TOperand):String; static;
    class function QualifiedNameOf(const ANode:TNode):String; static;
    class function RemoveTPrefix(const AObject:TObject):String; static;
    class function TypeAndName(const AType:TType):String; static;
    class function TypeNameOf(const AType:TType):String; static;
    class function TypeOfOnly(const ANode: TNode): String; static;
  end;

implementation

uses
  {$IFDEF INTERNAL}
  Internal,
  {$ENDIF}
  Syntax, SysUtils;

{ Utils }

class function TASTUtils.AssigmentOf(const Assignment: TArithmetic_Assign): String;
begin
  case Assignment of
    None     : result:=TSyntax._Assignment;
    Add      : result:=TSyntax._Add_Assign;
    Subtract : result:=TSyntax._Subtract_Assign;
    Multiply : result:=TSyntax._Multiply_Assign;
    Divide   : result:=TSyntax._Divide_Assign;
  end;
end;

class procedure TASTUtils.DestroyNodes(var Nodes: TNodes);
var t : Integer;
begin
  for t:=High(Nodes) downto Low(Nodes) do
      Nodes[t].Free;
end;

class function TASTUtils.IsLambda(const AType: TType): Boolean;
begin
  // anonymous functions
  result:=(AType is TRoutine) and (TRoutine(AType).Name='');
end;

class function TASTUtils.ModuleOfWith(const AWith:TWith):TNamedType;
begin
  result:=AWith.Module;

  // with Foo.Bar.Lee
  while result.Owner<>nil do
     result:=result.Owner as TNamedType;
end;

class function TASTUtils.NodeName(const ANode:TNode):String;
begin
  if ANode is TTypeMember then
     result:=NodeName(TTypeMember(ANode).TheType)+
             TSyntax._DataMember+
             NodeName(TTypeMember(ANode).Member)
  else
  if ANode is TNamedType then
     result:=TNamedType(ANode).Name
  else
  if ANode is TMember then
     result:=NodeName(TMember(ANode).Data)+
             TSyntax._DataMember+
             NodeName(TMember(ANode).Member)
  else
  if ANode is TVariable then
     result:=TVariable(ANode).Name
  else
  if ANode is TTypeCall then
     result:=NodeName(TTypeCall(ANode).TheType)
  else
  if ANode is TDataCall then
     result:=NodeName(TDataCall(ANode).Routine)
  else
  if ANode is TWith then
     if TWith(ANode).Alias='' then
        result:=NodeName(TWith(ANode).Module)
     else
        result:=TWith(ANode).Alias
  else
  if ANode is TCatch then
     result:=NodeName(TCatch(ANode).Error)
  else
  if ANode is TSpecializedType then
     result:=NodeName(TSpecializedType(ANode).TheType)
  else
  if ANode is TSelf then
     result:=TSyntax._Reserved_self
  else
  if ANode is TArrayType then
     result:=NodeName(TArrayType(ANode).TheType)
  else
  if ANode is TGenericType then
     result:=NodeName(TGenericType(ANode).Variable)
  else
  if ANode is TArrayExpression then
     result:=Nodename(TArrayExpression(ANode).Data)
  else
  if ANode is TOperand then
     result:=OperandToString(TOperand(ANode))
  else
  if ANode is TFor then
     result:=TSyntax._Reserved_for
  else
  if ANode is TWhile then
     result:=TSyntax._Reserved_while
  else
  if ANode is TRepeat then
     result:=TSyntax._Reserved_repeat
  else
  if ANode is TIf then
     result:=TSyntax._Reserved_if
  else
  if ANode is TWhen then
     result:=TSyntax._Reserved_when
  else
  if ANode is TBoolean then
     result:=BoolToStr(TBoolean(ANode).Value,True)
  else
  if ANode is TInteger then
  begin
    result:=TInteger(ANode).Text;

    if result='' then
       result:=IntToStr(TInteger(ANode).Value);  // exponent?
  end
  else
  if ANode is TFloat then
  begin
    result:=TFloat(ANode).Text;

    if result='' then
       result:=FloatToStr(TFloat(ANode).Value); // exponent?
  end
  else
  if ANode is TText then
     result:=TText(ANode).Value
  else
  if ANode is TGroup then
     result:=TSyntax._BeginParameters+' '+TSyntax._EndParameters
  else
  if ANode is TType then
     result:=TSyntax._BeginBlock+' '+TSyntax._EndBlock
  else
  if ANode is TReturn then
     result:=TSyntax._Reserved_return
  else
  if ANode is TRange then
     result:=NodeName(TRange(ANode).Min)+TSyntax._RangeDelimiter+NodeName(TRange(ANode).Max)
  else
  if ANode is TCastingData then
     result:=NodeName(TCastingData(ANode).TheType)
  else
  if ANode is TAssignment then
     result:=NodeName(TAssignment(ANode).Variable)
  else
  if ANode is TCallData then
     result:=NodeName(TCallData(ANode).Value)
  else
  if ANode=nil then
     result:='?'
  else
  begin
    result:=ANode.ClassName+'?';

    {$IFDEF INTERNAL}
    //raise exception.Create('Unknown node class at NodeName: '+ANode.ClassName);
    {$ENDIF}
  end;
end;

class function TASTUtils.OperandToString(const AOperand: TOperand): String;
begin
  if AOperand is TIsEqual          then result:=TSyntax._Symbol_Equal else
  if AOperand is TIsNotEqual       then result:=TSyntax._Symbol_Different else
  if AOperand is TIsLower          then result:=TSyntax._Symbol_Lower else
  if AOperand is TIsLowerOrEqual   then result:=TSyntax._Symbol_Lower+TSyntax._Symbol_Equal else
  if AOperand is TIsGreater        then result:=TSyntax._Symbol_Greater else
  if AOperand is TIsGreaterOrEqual then result:=TSyntax._Symbol_Greater+TSyntax._Symbol_Equal else
  if AOperand is TAddition         then result:=TSyntax._Symbol_Add else
  if AOperand is TSubtraction      then result:=TSyntax._Symbol_Subtract else
  if AOperand is TMultiplication   then result:=TSyntax._Symbol_Multiply else
  if AOperand is TDivision         then result:=TSyntax._Symbol_Divide else
  if AOperand is TCondition        then result:=TSyntax._TypeDelimiter else
  if AOperand is TContains         then result:=TSyntax._Reserved_in
  else
    result:=''; // <-- Do not raise any exception here. Do it at callers.
end;

class function TASTUtils.RemoveTPrefix(const AObject:TObject):String;
begin
  if AObject=nil then
     result:='?'
  else
  begin
    result:=AObject.ClassName;

    {$IFDEF INTERNAL}
    if result='TNodeError' then
       InternalError('Corruption at position node',nil {AObject});
    {$ENDIF}

    if UpperCase(Copy(result,1,1))='T' then
       result:=Copy(result,2,Length(result));
  end;
end;

class function TASTUtils.NameOf(const ANode:TObject; const AddClass:Boolean):String;
begin
  if ANode=nil then
     result:='nil?'
  else
  if ANode is TTypeMember then
     if AddClass then
        result:=NameOf(TTypeMember(ANode).Member,AddClass)
     else
        result:=NameOf(TTypeMember(ANode).TheType,False)+
                TSyntax._DataMember+
                NameOf(TTypeMember(ANode).Member,False)
  else
  if ANode is TMember then
     result:=NameOf(TMember(ANode).Data,False)+TSyntax._DataMember+
             NameOf(TMember(ANode).Member,False)
  else
  if ANode is TExtender then
     result:=NameOf(TExtender(ANode).TheType,False)+
             TSyntax._DataMember+
             NameOf(TExtender(ANode).Extension,False)
  else
  if ANode is TTypeCall then
     result:=NameOf(TTypeCall(ANode).TheType,False)
  else
  if ANode is TNode then
  begin
    if AddClass then
       result:=RemoveTPrefix(ANode)+TSyntax._TypeDelimiter+NodeName(TNode(ANode))
    else
       result:=NodeName(TNode(ANode));
  end
  else
     result:='? '+ANode.ClassName;
end;

class function TASTUtils.QualifiedNameOf(const ANode:TNode):String;
begin
  result:=NodeName(ANode);
end;

class function TASTUtils.TypeOfOnly(const ANode:TNode): String;
begin
  if ANode=nil then
     result:=''
  else
  if ANode is TExtender then
     result:=TASTUtils.NodeName(TExtender(ANode).TheType)
  else
     result:=TASTUtils.NodeName(ANode);
end;

class function TASTUtils.TypeAndName(const AType: TType): String;
begin
  result:=TypeOfOnly(AType.Owner);

  if result<>'' then
     result:=result+TSyntax._DataMember;

  result:=result+TASTUtils.NodeName(AType);
end;

class function TASTUtils.TypeNameOf(const AType:TType):String;
begin
  if AType is TNamedType then
     result:=TNamedType(AType).Name
  else
     result:=AType.ClassName;
end;

end.
