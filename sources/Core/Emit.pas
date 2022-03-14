unit Emit;

interface

uses
  Sys, AST, Map;

type
  TSymbol=class
  end;

  TEmit=class
  public
    procedure Add(const ANode:TNode; const S:String); overload; virtual; abstract;
    procedure Add(const AStyle:TPositionStyle; const S:String); overload; virtual; abstract;
    procedure Clear; virtual; abstract;
    function GetText:String; virtual; abstract;
  end;

  TPlain=class(TEmit)
  private
    Text : String;
  public
    procedure Add(const ANode:TNode; const S:String); override;
    procedure Add(const AStyle:TPositionStyle; const S:String); override;
    procedure Clear; override;
    function GetText:String; override;
  end;

  TBaseEmit=class
  public
    const
      NewLine=#13#10;

    var
    Emit : TEmit;

    Constructor Create; virtual;
    Destructor Destroy; override;

    procedure Add(const ANode:TNode; const S: String); overload;
    procedure Add(const S: String); overload;
    procedure AddLine;
  end;

  TVidiEmit=class(TBaseEmit)
  private
    procedure AddError(const S:String);
    procedure AddKeyword(const S:String);
    procedure AddName(const ANode:TNode);
    procedure AddSymbol(const S:String);
    procedure AddSpace;

    procedure Block(const APrefix:String; const ANodes:TNodes);
    procedure Clauses(const AClauses:TClauses; const SkipShared:Boolean);
    procedure DataToString(const AData:TData; OnlyName:Boolean);
    procedure DoTypeToString(const AParent:TNode; const AType:TType);
    procedure EmitItems(const APrefix:String; const AItems:TNodes);
    procedure EmitRoutine(const APrefix:String; const ARoutine:TRoutine);
    procedure GetArrayExpression(const ANode:TArrayExpression);
    procedure Nodes(const APrefix:String; const ANodes:TNodes);
    procedure Parameters(const AParameters:TNodes; const OnlyName,AddOutClause:Boolean);
    procedure RangeToString(const ARange:TRange);
    procedure TextToString(const ANode:TNode; const S:String);
    procedure TryParameters(const AParameters:TNodes);
    procedure TryRoutineOutput(const ARoutine:TRoutine);
    procedure VariableName(const AVariable:TVariable);
    procedure VariableType(const AVariable:TVariable);

  protected
  public
    class var
      Indentation:String;
      RaiseErrors:Boolean;

    function AsString(const ANode:TNode):String; overload;
    function AsString(const ANodes:TNodes):String; overload;

    procedure DeclaringParameters(const AParameters:TNodes);

    procedure Node(const APrefix:String; const ANode:TNode); overload;
    procedure Node(const ANode:TNode); overload;

    procedure Operand(const AOperand:TOperand);

    function TypeToString(const AType:TType):String;
  end;

function B(const ANode:TNode):String; overload; // Helper method, useful for debugging
function B(const ANodes:TNodes):String; overload; // Helper method, useful for debugging

implementation

uses
  Exceptions,
  SysUtils,
  Syntax,
  Utils.AST,
  Checker.AST,
  Checker.Shared,

  {$IFDEF INTERNAL}
  Instance_Type,
  {$ENDIF}

  //Text,
  Module, Find.AST;

{$IFDEF INTERNAL}
procedure EmitError(const S:String; const ANode:TNode);
begin
  Raise_Exception(S+TASTUtils.NodeName(ANode));
end;
{$ENDIF}

{ TBaseEmit }

Constructor TBaseEmit.Create;
begin
  inherited;
  Emit:=TPlain.Create;
end;

Destructor TBaseEmit.Destroy;
begin
  Emit.Free;
  inherited;
end;

procedure TBaseEmit.Add(const ANode:TNode; const S: String);
begin
  if S<>'' then
     Emit.Add(ANode,S);
end;

procedure TBaseEmit.Add(const S: String);
begin
  Add(nil,S);
end;

procedure TBaseEmit.AddLine;
begin
  Add(nil,NewLine);
end;

{ TVidiEmit }

procedure TVidiEmit.RangeToString(const ARange:TRange);
begin
  DataToString(ARange.Min,True);
  AddSymbol(TSyntax._RangeDelimiter);
  DataToString(ARange.Max,True);
end;

procedure TVidiEmit.DeclaringParameters(const AParameters:TNodes);
begin
  if AParameters<>nil then
  begin
    AddSymbol(TSyntax._BeginParameters);
    Parameters(AParameters,False,True);
    AddSymbol(TSyntax._EndParameters);
  end;
end;

procedure TVidiEmit.TryParameters(const AParameters:TNodes);
begin
  if AParameters<>nil then
  begin
    AddSymbol(TSyntax._BeginParameters);
    AddSpace;
    Parameters(AParameters,True,True);
    AddSpace;
    AddSymbol(TSyntax._EndParameters);
  end;
end;

{
procedure TVidiEmit.GetTypeIndexes(const AType:TType);
begin
  if AType.Parameters=nil then
  else
  if AType is TFunctionType then
     TryParameters(AType.Parameters)
  else
  begin
    AddSymbol(TSyntax._BeginArray);
    Parameters(AType.Parameters,True);
    AddSymbol(TSyntax._EndArray);
  end;
end;
}

function TVidiEmit.TypeToString(const AType:TType):String;
begin
  Emit.Clear;
  DoTypeToString(nil,AType);
  result:=Emit.GetText;
end;

function HasSpaces(const S:String):Boolean;
begin
  result:=Pos(' ',S)>0;
end;

procedure TVidiEmit.DoTypeToString(const AParent:TNode;
                                   const AType:TType);

  procedure TypeMember(const AType:TTypeMember);
  begin
    DoTypeToString(AType,AType.TheType);
    AddSymbol(TSyntax._DataMember);
    DoTypeToString(AType,AType.Member);
  end;

  procedure AddSize(const AData:TData);
  begin
    AddSymbol(TSyntax._BeginArray); // [

    if AData<>nil then
       DataToString(AData,True);

    AddSymbol(TSyntax._EndArray);  // ]
  end;

  procedure ArrayType(const AType:TArrayType);
  var tmp : TArrayType;
      t,L : Integer;
      Indexes : Array of TArrayType;
  begin
    tmp:=AType;

    L:=0;

    while tmp.TheType is TArrayType do
    begin
      tmp:=TArrayType(tmp.TheType);

      {$IFDEF FPC}
      Indexes:=nil;
      {$ENDIF}

      SetLength(Indexes,L+1);
      Indexes[L]:=tmp;
      Inc(L);
    end;

    DoTypeToString(tmp,tmp.TheType);

    AddSize(AType.Size);

    for t:=High(Indexes) downto 0 do   // multi-dimension [ ] [ ] ...
        AddSize(Indexes[t].Size);
  end;

  // Costly. TTypeCall should allow TWith in addition to types?
  function FindWithAlias(const AModule:TNamedType):String;

    function FindWith(const AParent:TNode):TWith;
    var N : TNode;
    begin
      if AParent is TType then
         for N in TType(AParent).Items do
             if (N is TWith) and (TWith(N).Module=AModule) then
                Exit(TWith(N));

      result:=nil;
    end;

  var tmpParent : TNode;
      tmp : TWith;
  begin
    if AParent=nil then
       result:='' // error ?
    else
    begin
      tmpParent:=AParent;

      repeat
        tmp:=FindWith(tmpParent);

        if tmp=nil then
        begin
          if tmpParent.Owner=nil then
             break
          else
             tmpParent:=tmpParent.Owner
        end
        else
          break;

      until False;

      if (tmp<>nil) and (tmp.Alias<>'') then
         result:=tmp.Alias
      else
         result:=AModule.Name; // error !!
    end;
  end;

  procedure AddNamedType(const AType:TNamedType);
  begin
    if TNamedType(AType).Name='' then
       AddError(AType.ClassName+'?')
    else
    begin
      if AType.Owner=nil then
         Add(AType,FindWithAlias(TNamedType(AType)))
      else
      if (AType.Owner=nil) and HasSpaces(TNamedType(AType).Name) then
         Add(AType,FindWithAlias(TNamedType(AType)))
      else
         AddName(AType);
    end;
  end;

begin
  if AType is TManyValues then // <-- before TArrayType !!
  begin
    DoTypeToString(AType,TManyValues(AType).TheType);
    AddSymbol(TSyntax._ManyValues);
  end
  else
  if AType is TArrayType then
     ArrayType(TArrayType(AType))
  else
  if TChecker.IsInlineStruct(AType) then
  begin
    AddSpace;
    AddSymbol(TSyntax._BeginBlock);
    AddSpace;
    Parameters(AType.Items,False,False); //  EmitItems('',AType.Items);
    AddSpace;
    AddSymbol(TSyntax._EndBlock);
  end
  else
  if AType is TRangeType then
     RangeToString(TRangeType(AType).Range)
  else
  if AType is TTypeMember then
     TypeMember(TTypeMember(AType))
  else
  if AType is TNamedType then
  begin
    AddNamedType(TNamedType(AType));

    if not (AParent is TExtender) then
       if not (AType is TFunctionType) then
          if AType is TParametersType then
             if TParametersType(AType).Parameters<>nil then
                TryParameters(TParametersType(AType).Parameters);
  end
  else
  if AType is TSpecializedType then
  begin
    AddNamedType(TSpecializedType(AType).TheType);

    if TSpecializedType(AType).Generics<>nil then
       TryParameters(TSpecializedType(AType).Generics);
  end
  else
  if AType is TGenericType then
     AddName(TGenericType(AType).Variable)
  else
     AddError(TASTUtils.NodeName(AType)+'?');
end;

procedure TVidiEmit.AddError(const S:String);
begin
  Emit.Add(nil,S);

  if RaiseErrors then
     Raise_Exception(S);
end;

procedure TVidiEmit.Operand(const AOperand:TOperand);

  procedure AddOperand;
  var tmp : String;
  begin
    tmp:=TASTUtils.OperandToString(AOperand);

    if tmp='' then
       AddError('Cannot convert operand to string: '+TASTUtils.NameOf(AOperand,True))
    else
       AddSymbol(tmp);
  end;

begin
  if AOperand is TLogicalAnd then AddKeyword(TSyntax._Reserved_and) else
  if AOperand is TLogicalOr then AddKeyword(TSyntax._Reserved_or) else
  if AOperand is TLogicalXor then AddKeyword(TSyntax._Reserved_xor) else
     AddOperand;
end;

procedure TVidiEmit.AddName(const ANode:TNode);
begin
  if ANode=nil then
     AddError('?')
  else
  begin
    {
    if (ANode is TNamedType) and
       (TFinder.ModuleOf(ANode.Owner)<>CurrentModule) then
       Add(ANode,TNamedType(ANode).Name)
    else
    }
       Add(ANode,TASTUtils.NodeName(ANode));
  end;
end;

procedure TVidiEmit.AddSpace;
begin
  Add(nil,' ');
end;

procedure TVidiEmit.AddKeyword(const S: String);
begin
  Emit.Add(TPositionStyle.Keyword,S);
end;

procedure TVidiEmit.AddSymbol(const S: String);
begin
  Emit.Add(TPositionStyle.Symbol,S);
end;

function TVidiEmit.AsString(const ANode: TNode): String;
begin
  Emit.Clear;
  Node(ANode);
  result:=Emit.GetText;
end;

function TVidiEmit.AsString(const ANodes: TNodes): String;
begin
  Emit.Clear;
  TryParameters(ANodes);
  result:=Emit.GetText;
end;

procedure TVidiEmit.VariableType(const AVariable:TVariable);
begin
  AddSymbol(TSyntax._TypeDelimiter);

  if not AVariable.TypeInferred then
  begin
    AddSpace;
    DoTypeToString(AVariable,AVariable.VariableType);
  end;
end;

procedure TVidiEmit.Parameters(const AParameters:TNodes; const OnlyName,AddOutClause:Boolean);

  function HasType(const AParameter:TNode):Boolean;
  begin
    result:=(AParameter is TVariable) and
            (TVariable(AParameter).VariableType<>nil);
  end;

  function SameType(const A,B:TNode):Boolean;
  begin
    result:=(A is TVariable) and
            (TVariable(A).VariableType<>nil) and
            (B is TVariable) and
            (TVariable(A).VariableType=TVariable(B).VariableType);
  end;

  procedure ParameterToString(const AParam:TNode);
  begin
    if AParam=nil then
       Exit;

    if AParam is TVariable and (not TVariable(AParam).Clauses.Final) then
       if OnlyName then
          AddName(AParam)
       else
       begin
         if AddOutClause then
         begin
           AddKeyword(TSyntax._Reserved_out);
           AddSpace;
         end;

         AddName(AParam); // Add(AParam,TVariable(AParam).Name); better?
       end
    else
    if AParam is TData then
       DataToString(TData(AParam),True)
    else
       AddName(AParam);
  end;

var t : Integer;
begin
  for t:=0 to High(AParameters) do
  begin
    if t=0 then
       ParameterToString(AParameters[t])
    else
    begin
      AddSymbol(TSyntax._ItemDelimiter);
      AddSpace;
      ParameterToString(AParameters[t]);
    end;

    if not OnlyName then
    if HasType(AParameters[t]) then
       if (t=High(AParameters)) or (not SameType(AParameters[t],AParameters[t+1])) then
          VariableType(TVariable(AParameters[t]));
  end;
end;

procedure TVidiEmit.GetArrayExpression(const ANode:TArrayExpression);
begin
  if ANode.Data<>nil then
     DataToString(ANode.Data,True);

  AddSymbol(TSyntax._BeginArray);
  Parameters(ANode.Parameters,True,True);
  AddSymbol(TSyntax._EndArray);
end;

procedure TVidiEmit.TextToString(const ANode:TNode; const S:String);
var tmp : Char;
begin
  tmp:='''';

  if Pos(tmp,S)>0 then
     tmp:='"';

  AddSymbol(tmp);
  Add(ANode,S);
  AddSymbol(tmp);
end;

procedure TVidiEmit.TryRoutineOutput(const ARoutine:TRoutine);
begin
  if ARoutine.Output<>nil then
  begin
    AddSpace;
    AddSymbol(TSyntax._TypeDelimiter);
    AddSpace;
    DoTypeToString(ARoutine,ARoutine.Output);
  end;
end;

procedure TVidiEmit.EmitRoutine(const APrefix:String; const ARoutine:TRoutine);
var SkipShared : Boolean;
begin
  if TASTUtils.IsLambda(ARoutine) then
     SkipShared:=True
  else
     SkipShared:=(not TChecker.IsAbstract(ARoutine)) and
                 TSharedChecker.CheckIsShared(ARoutine);

  Clauses(ARoutine.Clauses,SkipShared);

  Add(ARoutine,ARoutine.Name);

  if ARoutine.Parameters=nil then
  begin
    if ARoutine.Output=nil then
    begin
      AddSymbol(TSyntax._BeginParameters);
      AddSymbol(TSyntax._EndParameters);
    end;
  end
  else
     DeclaringParameters(ARoutine.Parameters);

  TryRoutineOutput(ARoutine);

  {
  if ARoutine.Ancestor<>nil then
     Add(' // override ');
  }

  EmitItems(APrefix,ARoutine.Items);
end;

procedure TVidiEmit.VariableName(const AVariable:TVariable);
begin
  Add(AVariable,AVariable.Name);
end;

procedure TVidiEmit.DataToString(const AData:TData; OnlyName:Boolean);

  procedure VariableToString(const AVariable:TVariable);
  begin
    Clauses(AVariable.Clauses,False);

    Add(AVariable,AVariable.Name);

    VariableType(AVariable);

    if AVariable.ValueData<>nil then
    begin
      AddSpace;
      AddSymbol(TSyntax._Assignment);
      AddSpace;
      DataToString(AVariable.ValueData,True);
    end;
  end;

  procedure OperandToString(const AOperand:TOperand);
  begin
    DataToString(AOperand.Left,True);
    AddSpace;
    Operand(AOperand);
    AddSpace;
    DataToString(AOperand.Right,True);
  end;

  procedure EmitTypeCall(const ACall:TTypeCall);
  begin
    // lambda unnamed anonymous function callback
    if TASTUtils.IsLambda(ACall.TheType) then
       EmitRoutine(Indentation,TRoutine(ACall.TheType))
    else
       DoTypeToString(AData,ACall.TheType);
  end;

var tmpS : String;
begin
  if AData is TUnaryNot then  // <-- before TGroup !
  begin
    AddKeyword(TSyntax._Reserved_not);
    AddSpace;
    DataToString(TUnaryNot(AData).Expression,True);
  end
  else
  if AData is TUnarySign then // <-- before TGroup !
  begin
    if TUnarySign(AData).Positive then
       AddSymbol(TSyntax._Symbol_Add)
    else
       AddSymbol(TSyntax._Symbol_Subtract);

    DataToString(TUnarySign(AData).Expression,True);
  end
  else
  if AData is TGroup then
  begin
    AddSymbol(TSyntax._BeginParameters);
    DataToString(TGroup(AData).Expression,True);
    AddSymbol(TSyntax._EndParameters);
  end
  else
  if AData is TCondition then
  begin
    DataToString(TCondition(AData).Condition,True);
    AddSpace;
    AddSymbol(TSyntax._Reserved_condition);
    AddSpace;
    OperandToString(TOperand(AData));
  end
  else
  if AData is TOperand then
     OperandToString(TOperand(AData))
  else
  if AData is TVariable then
     if OnlyName then
        VariableName(TVariable(AData))
     else
        VariableToString(TVariable(AData))
  else
  if AData is TBoolean then
     Add(AData,BoolToStr(TBoolean(AData).Value,True))
  else
  if AData is TInteger then
  begin
    tmpS:=TInteger(AData).Text;

    if tmpS='' then
       tmpS:=IntToStr(TInteger(AData).Value);

    Add(AData,tmpS);
  end
  else
  if AData is TFloat then
  begin
    tmpS:=TFloat(AData).Text;

    if tmpS='' then
       tmpS:=FloatToStr(TFloat(AData).Value);

    Add(AData,tmpS);
  end
  else
  if AData is TText then
     TextToString(AData,TText(AData).Value)
  else
  if AData is TVariableCall then // <-- before TDataCall !!
  begin
    AddName(TVariableCall(AData).Variable);  // Foo:MyFunctionType Foo
    TryParameters(TDataCall(AData).Parameters);
  end
  else
  if AData is TDataCall then
  begin
    AddName(TDataCall(AData).Routine);
    TryParameters(TDataCall(AData).Parameters);
  end
  else
  {
  if AData is TUnaryNot then
  begin
    AddKeyword(TSyntax._Reserved_not);
    AddSpace;
    DataToString(TUnaryNot(AData).Expression,True);
  end
  else
  if AData is TUnarySign then
  begin
    if TUnarySign(AData).Positive then
       AddSymbol('+')
    else
       AddSymbol('-');

    DataToString(TUnarySign(AData).Expression,True);
  end
  else
  if AData is TUnary then
     DataToString(TUnary(AData).Expression,True)
  else
  }
  if AData is TArrayExpression then
     GetArrayExpression(TArrayExpression(AData))
  else
  if AData is TMember then
  begin
    DataToString(TMember(AData).Data,True);
    AddSymbol(TSyntax._DataMember);
    DataToString(TMember(AData).Member,True);
  end
  else
  if AData is TTypeCall then
     EmitTypeCall(TTypeCall(AData))
  else
  if AData is TRange then
     RangeToString(TRange(AData))
  else
  if AData is TCastingData then
  begin
    AddName(TCastingData(AData).TheType);

    AddSymbol(TSyntax._BeginParameters);
    DataToString(TCastingData(AData).Data,True);
    AddSymbol(TSyntax._EndParameters);
  end
  else
  if AData is TSelf then
     AddKeyword(TSyntax._Reserved_self)
  else
  if AData is TAncestor then
  begin
    AddKeyword(TSyntax._Reserved_ancestor);

    if TAncestor(AData).DataCall<>nil then
       if TChecker.TypeHasOutput(TAncestor(AData).DataCall.Routine) then
          AddSpace;
  end
  else
  if AData=nil then
     Add('?')
  else
     AddError('Cannot convert to string: '+TASTUtils.NameOf(AData,True));
end;

procedure TVidiEmit.Clauses(const AClauses:TClauses; const SkipShared:Boolean);
begin
  if AClauses.Indexed then
  begin
    AddSymbol(TSyntax._Reserved_indexed);
    AddSpace;
  end;

  if AClauses.Hidden then
  begin
    AddSymbol(TSyntax._Reserved_hidden);
    AddSpace;
  end;

  if (not SkipShared) and AClauses.Shared then
  begin
    AddSymbol(TSyntax._Reserved_shared);
    AddSpace;
  end;

  if AClauses.Final then
  begin
    AddSymbol(TSyntax._Reserved_final);
    AddSpace;
  end;
end;

procedure TVidiEmit.Nodes(const APrefix:String; const ANodes:TNodes);
var n : TNode;
begin
  for n in ANodes do
  begin
    Node(APrefix,n);

    //if n is TComment then
    //else
       AddLine;
  end;
end;

procedure TVidiEmit.Block(const APrefix:String; const ANodes:TNodes);

  procedure DoBlock(const ANodes:TNodes);
  begin
    if Length(ANodes)>0 then
    begin
      AddLine;

      Nodes(APrefix+Indentation,ANodes);

      Add(APrefix);
    end;
  end;

begin
  AddSymbol(TSyntax._BeginBlock);
  DoBlock(ANodes);
  AddSymbol(TSyntax._EndBlock);
end;

procedure TVidiEmit.EmitItems(const APrefix:String; const AItems:TNodes);
const
  BeginBlockSameLine=True;
begin
  if BeginBlockSameLine then
     AddSpace
  else
  begin
    AddLine;
    Add(APrefix);
  end;

  Block(APrefix,AItems);
end;

procedure TVidiEmit.Node(const APrefix:String; const ANode:TNode);

  procedure EmitClass(const AClass:TClassType);
  begin
    Clauses(AClass.Clauses,True);
    Add(AClass,AClass.Name);

    if TChecker.IsEnumerationClass(AClass) then
    begin
      AddSpace;
      AddKeyword(TSyntax._Reserved_is);
      AddSpace;

      AddSymbol(TSyntax._BeginBlock);
      AddSpace;

      Parameters(AClass.Items,True,True);

      AddSpace;
      AddSymbol(TSyntax._EndBlock);
    end
    else
    begin
      if AClass.Parameters<>nil then
         DeclaringParameters(AClass.Parameters);

      if AClass.Ancestor<>nil then
      begin
        AddSpace;
        AddKeyword(TSyntax._Reserved_is);
        AddSpace;

        if AClass is TIntegerClass then
           RangeToString(TIntegerClass(AClass).Size.Range)
        else
        if (AClass is TClassType) and (AClass.Ancestor is TClassType) then
           DoTypeToString(AClass,AClass.Ancestor)
        else
           DoTypeToString(AClass,AClass.Ancestor);
      end;

      EmitItems(APrefix,AClass.Items);
    end;
  end;

  procedure EmitFunctionType(const AType:TFunctionType);
  begin
    // skip "Shared" clause, because a function-type it is always shared
    // (its a type, and all types are always shared)
    Clauses(AType.Clauses,True);

    Add(AType,AType.Name);

    AddSpace;

    AddKeyword(TSyntax._Reserved_is);
    AddSpace;

    AddSymbol(TSyntax._BeginParameters);
    AddSpace;
    Parameters(AType.Parameters,False,True);
    AddSpace;
    AddSymbol(TSyntax._EndParameters);

    TryRoutineOutput(AType);

    AddSymbol(TSyntax._BeginBlock);
    AddSymbol(TSyntax._EndBlock);
  end;

  procedure MemberChain(const AMember:TMember);
  begin
    if AMember<>nil then
    begin
      if AMember.Data is TVariable then
         DataToString(AMember.Data,True)
      else
         Node(AMember.Data);

      AddSymbol(TSyntax._DataMember);

      if AMember.Member is TVariable then
         DataToString(AMember.Member,True)
      else
         Node(AMember.Member);
    end;
  end;

  function ParentBlock(ANode:TNode):TType;
  begin
    if ANode<>nil then
    repeat
      if ANode is TType then
         Exit(TType(ANode))
      else
         ANode:=ANode.Owner;

    until ANode=nil;

    result:=nil;
  end;

  procedure DoRepeat(const ARepeat:TRepeat);
  begin
    AddKeyword(TSyntax._Reserved_repeat);

    if ARepeat.Block<>nil then
       Node(APrefix+Indentation,ARepeat.Block);

    Add(APrefix);
    AddKeyword(TSyntax._Reserved_until);
    AddSpace;

    DataToString(ARepeat.Condition,True);
  end;

  procedure DoWhen(const AWhen:TWhen);
  var W : TWhenItem;
  begin
    AddKeyword(TSyntax._Reserved_when);
    AddSpace;
    DataToString(AWhen.Expression,True);
    AddSpace;

    AddSymbol(TSyntax._BeginBlock);
    AddLine;

    for W in AWhen.Items do
        Node(APrefix+Indentation,W);

    if AWhen.ElseBlock<>nil then
    begin
      AddLine;
      AddKeyword(TSyntax._Reserved_else);
      AddSpace;

      Node(AWhen.ElseBlock);

      AddLine;
    end;

    AddSymbol(TSyntax._EndBlock);
  end;

  procedure DoFor(const AFor:TFor);

    procedure AddFirstLast;
    begin
      DataToString(AFor.First,True);
      AddSpace;
      AddKeyword(TSyntax._Reserved_to);
      AddSpace;
      DataToString(AFor.Last,True);
    end;

  begin
    AddKeyword(TSyntax._Reserved_for);

    if AFor.Counter<>nil then
    begin
      AddSpace;
      Add(AFor.Counter,AFor.Counter.Name);

      if AFor.Counter.Owner=AFor then  // for x::=
         if not AFor.Counter.TypeInferred then
            VariableType(AFor.Counter);

      if AFor.InExpression=nil then
      begin
        AddSpace;

        if AFor.Counter.Owner=AFor then  // for x::=
           if AFor.Counter.TypeInferred then
              AddSymbol(TSyntax._TypeDelimiter);

        AddSymbol(TSyntax._Assignment);

        AddFirstLast;
      end
      else
      begin
        AddSpace;
        AddKeyword(TSyntax._Reserved_in);
        AddSpace;
        DataToString(AFor.InExpression,True);
      end;
    end
    else
    if AFor.InExpression=nil then
    begin
      AddSpace;
      AddFirstLast;
    end
    else
    begin
      AddSpace;
      DataToString(AFor.InExpression,True);
    end;

    Node(APrefix+Indentation,TFor(ANode).Block);
  end;

  procedure PartialOperand(const AOperand:TOperand);
  begin
    Operand(AOperand);
    AddSpace;
    DataToString(AOperand.Right,True);
  end;

  procedure DoWhenItem(const W:TWhenItem);
  begin
    Add(APrefix);

    if W.Expression is TOperand then
       PartialOperand(TOperand(W.Expression))
    else
       DataToString(W.Expression,True);

    AddSpace;

    //AddSymbol(TSyntax._BeginBlock);
    //AddLine;

    Node(W.Block);

    //AddSymbol(TSyntax._EndBlock);
  end;

  procedure DoTry(const ATry:TTry);
  var C : TCatch;
  begin
    AddKeyword(TSyntax._Reserved_try);
    AddLine;

    Node(ATry.Block);

    AddLine;

    for C in ATry.Catch do
    begin
      AddKeyword(TSyntax._Reserved_catch);
      AddLine;

      if C.Error<>nil then
      begin
        if C.Error is TVariable then
        begin
          Add(C.Error,TVariable(C.Error).Name);
          AddSpace;
          AddSymbol(TSyntax._TypeDelimiter);
          AddSpace;
          DoTypeToString(C.Error,TVariable(C.Error).VariableType);
        end
        else
          AddName(C.Error);

        AddSpace;
      end;

      Node(C.Block);
    end;

    if ATry.TheFinally<>nil then
    begin
      AddKeyword(TSyntax._Reserved_finally);
      AddLine;

      Node(ATry.TheFinally);
    end;
  end;

  procedure EmitAssignment(const A:TAssignment);
  begin
    //AddSpace;
    DataToString(A.Variable,True);
    AddSpace;

    if A.Arithmetic=TArithmetic_Assign.None then
       AddSymbol(TSyntax._Assignment)
    else
    begin
      case A.Arithmetic of
       TArithmetic_Assign.Add: AddSymbol(TSyntax._Symbol_Add);
  TArithmetic_Assign.Subtract: AddSymbol(TSyntax._Symbol_Subtract);
  TArithmetic_Assign.Multiply: AddSymbol(TSyntax._Symbol_Multiply);
    TArithmetic_Assign.Divide: AddSymbol(TSyntax._Symbol_Divide);
      end;

      AddSymbol(TSyntax._Symbol_Equal);
    end;

    AddSpace;
    DataToString(TAssignment(ANode).Value,True);
  end;

  procedure TryAdd(const ANodes:TNodes);
  begin
    Add(APrefix);
    Block(APrefix,ANodes);
    {
    if Length(ANodes)=1 then
    begin
      Add(APrefix);
      Node(APrefix,ANodes[0]);
    end
    else
    if Length(ANodes)>0 then
    begin
      Add(APrefix);
      Block(APrefix,ANodes);
    end
    else
    begin
      Add(APrefix);
      AddSymbol(TSyntax._BeginBlock);
      AddSymbol(TSyntax._EndBlock);
    end;
    }
  end;

  function HasNoIdentifierDigits(const S:String):Boolean;
  const
     Identifier = ['a'..'z','A'..'Z','_','0'..'9'];

  var C : Char;
  begin
    for C in S do
        if not CharInSet(C,Identifier) then
           Exit(True);

    result:=False;
  end;

  procedure EmitWith(const AWith:TWith);

    procedure AddQuotedName(const AModule:TNamedType);
    var tmp : String;
    begin
      tmp:=AModule.Name;

      if HasNoIdentifierDigits(tmp) then
         TextToString(AModule,tmp)
      else
         Add(AWith,tmp);
    end;

    procedure AddDottedModule(const AModule:TNamedType);

      function DottedNameOf(const AType:TType):String;
      begin
        if AType is TExtender then
           result:=DottedNameOf(TExtender(AType).TheType)+
                   TSyntax._DataMember+
                   DottedNameOf(TExtender(AType).Extension)
        else
        if AType is TTypeMember then
           result:=DottedNameOf(TTypeMember(AType).TheType)+
                   TSyntax._DataMember+
                   DottedNameOf(TTypeMember(AType).Member)
        else
        if AType is TNamedType then
           result:=TNamedType(AType).Name

        {$IFDEF INTERNAL}
        else
          EmitError('Cannot obtain dotted name of: ',AType);
        {$ENDIF}
      end;

    var tmp : TNamedType;
        tmpS : String;
    begin
      tmp:=AModule;
      tmpS:='';

      if tmp.Owner<>nil then
      begin
        while tmp.Owner<>nil do
        begin
          tmpS:=TSyntax._DataMember+DottedNameOf(tmp)+tmpS;

          if tmp.Owner is TNamedType then
             tmp:=TNamedType(tmp.Owner)
          else
             Raise_Exception('Module subitem owner is not a named type');
        end;
      end;

      AddQuotedName(tmp);

      if tmpS<>'' then
         Add(tmp,tmpS);
    end;

  begin
    // Do not add "with sys" because it is implicit, unless it comes with an alias

    if not AWith.SystemModule then
    begin
      AddKeyword(TSyntax._Reserved_with);
      AddSpace;

      if AWith.Alias='' then
         AddDottedModule(AWith.Module)
      else
      begin
        Add(ANode,TFinder.WithName(AWith));
        AddSpace;
        AddSymbol(TSyntax._Assignment);
        AddSpace;
        AddQuotedName(AWith.Module);
      end;
    end;
  end;

  procedure EmitExtender(const AExtender:TExtender);
  begin
    DoTypeToString(AExtender,AExtender.TheType);
    AddSymbol(TSyntax._DataMember);

    if AExtender.Extension is TFunctionType then // <-- before TRoutine !
       EmitFunctionType(AExtender.Extension as TFunctionType)
    else
    if AExtender.Extension is TRoutine then
       EmitRoutine(APrefix,AExtender.Extension as TRoutine)
    else
    if AExtender.Extension is TClassType then
       EmitClass(AExtender.Extension as TClassType)

    {$IFDEF INTERNAL}
    else
       EmitError('Cannot emit extender of type: ',AExtender.Extension);
    {$ELSE}
      ;
    {$ENDIF}

    AddLine;
  end;

  {$IFDEF INTERNAL}
  procedure EmitInstance(const AInstance:TInstance);
  var D : TInstance.TVariableValue;
      t : Integer;
  begin
    Add('$ '+IntToStr(Length(AInstance.Data)));
    AddLine;

    for t:=0 to High(AInstance.Data) do
    begin
      D:=AInstance.Data[t];
      Add(IntToStr(t)+': '+D.Variable.Name);

      if D.Value<>nil then
         Add('= '+B(D.Value));

      AddLine;
    end;

    if AInstance.Value<>nil then
       Add(' -> '+B(AInstance.Value));
  end;
  {$ENDIF}

  procedure EmitCallData(const ACall:TCallData);
  begin
    if ACall.Value is TVariable then
       VariableName(TVariable(ACall.Value))
    else
       Node(ACall.Value);
  end;

var tmp : TType;
begin
  Add(APrefix);

  {$IFDEF INTERNAL}
  if ANode is TInstance then
     EmitInstance(TInstance(ANode))
  else
  {$ENDIF}
  if ANode is TWith then
     EmitWith(TWith(ANode))
  else
  if ANode is TClassType then
  begin
    EmitClass(TClassType(ANode));
    AddLine;
  end
  else
  if ANode is TAssignment then
     EmitAssignment(TAssignment(ANode))
  else
  if ANode is TRange then
     RangeToString(TRange(ANode))
  else
  if ANode is TExtender then
     EmitExtender(TExtender(ANode))
  else
  if ANode is TFunctionType then // <-- before TRoutine !!
     EmitFunctionType(TFunctionType(ANode))
  else
  if ANode is TRoutine then
  begin
    EmitRoutine(APrefix,TRoutine(ANode));

    // New line only when routine has items:

    if Length(TRoutine(ANode).Items)>0 then
    begin
      // New line only for non-last routines inside class/sub-routines.
      tmp:=ParentBlock(ANode.Owner);

      if tmp=nil then
         AddLine
      else
      if (High(tmp.Items)>-1) and (tmp.Items[High(tmp.Items)]<>ANode) then
         AddLine;
    end;
  end
  else
  { unused:
  if ANode is TCall then
  begin
    AddName(TCall(ANode).Routine);
    TryParameters(TCall(ANode).Parameters);
  end
  else
  }
  if ANode is TArrayExpression then
     GetArrayExpression(TArrayExpression(ANode))
  else
  if ANode is TMember then
     MemberChain(TMember(ANode))
  else
  if ANode is TReturn then
  begin
    if TReturn(ANode).Value=nil then
    begin
      AddKeyword(TSyntax._Reserved_Return);
      AddSpace;
    end
    else
    begin
      tmp:=ParentBlock(ANode);

      if (tmp<>nil) and (Length(tmp.Items)=1) and (tmp.Items[0]=ANode) then
      else
      begin
        AddKeyword(TSyntax._Reserved_Return);
        AddSpace;
      end;

      DataToString(TReturn(ANode).Value,True);
    end;
  end
  else
  if ANode is TData then
     DataToString(TData(ANode),False)
  else
  {
  if ANode is TTypeMember then // <-- before TType !!
     TypeMember(TTypeMember(ANode))
  else
  }
  if ANode is TType then
     if ANode.Owner=nil then
        Nodes(APrefix,TType(ANode).Items) // <-- module
     else
        TryAdd(TType(ANode).Items)
  else
  if ANode is TBlockStatement then
  begin
    //AddSymbol(TSyntax._BeginBlock);
    //AddLine;
    Node(APrefix,TBlockStatement(ANode).Block);
    //AddLine;
    //AddSymbol(TSyntax._EndBlock);
    //AddLine;
  end
  else
  if ANode is TIf then
  begin
    AddKeyword(TSyntax._Reserved_if);
    AddSpace;

    DataToString(TIf(ANode).Condition,True);

    AddKeyword(TSyntax._Reserved_then);
    AddLine;

    if TIf(ANode).ThenBlock<>nil then
       Node(APrefix+Indentation,TIf(ANode).ThenBlock);

    if TIf(ANode).ElseBlock<>nil then
    begin
      AddLine;
      Add(APrefix);
      AddKeyword(TSyntax._Reserved_else);
      AddLine;

      if TIf(ANode).ElseBlock is TIf then
         Node(APrefix,TIf(ANode).ElseBlock)
      else
         Node(APrefix+Indentation,TIf(ANode).ElseBlock);
    end;
  end
  else
  if ANode is TWhile then
  begin
    AddKeyword(TSyntax._Reserved_while);
    AddSpace;

    DataToString(TWhile(ANode).Condition,True);

    if TWhile(ANode).Block<>nil then
       Node(APrefix+Indentation,TWhile(ANode).Block);
  end
  else
  if ANode is TRepeat then
     DoRepeat(TRepeat(ANode))
  else
  if ANode is TBreak then
     AddKeyword(TSyntax._Reserved_break)
  else
  if ANode is TContinue then
     AddKeyword(TSyntax._Reserved_continue)
  else
  if ANode is TFor then
     DoFor(TFor(ANode))
  else
  if ANode is TCallData then
     EmitCallData(TCallData(ANode))
  else
  {
  if ANode is TComment then
  begin
    Emit.Add(TPositionStyle.Comment,APrefix+TComment(ANode).Text);
    AddLine;
  end
  else
  }
  if ANode is TWhen then
     DoWhen(TWhen(ANode))
  else
  if ANode is TWhenItem then
     DoWhenItem(TWhenItem(ANode))
  else
  if ANode is TTry then
     DoTry(TTry(ANode))
  else
     AddError('Cannot emit: '+TASTUtils.NameOf(ANode,True));
end;

procedure TVidiEmit.Node(const ANode:TNode);
begin
  Node('',ANode);
end;

{ TPlain }

procedure TPlain.Add(const ANode:TNode; const S: String);
begin
  Text:=Text+S;
end;

procedure TPlain.Add(const AStyle:TPositionStyle; const S: String);
begin
  Text:=Text+S;
end;

procedure TPlain.Clear;
begin
  Text:='';
end;

function TPlain.GetText: String;
begin
  result:=Text;
end;

// Helper method, useful for debugging
function B(const ANode:TNode):String; overload;
var Emit : TVidiEmit;
begin
  Emit:=TVidiEmit.Create;
  try
    result:=Emit.AsString(ANode);
  finally
    Emit.Free;
  end;
end;

function B(const ANodes:TNodes):String; overload;
var Emit : TVidiEmit;
begin
  Emit:=TVidiEmit.Create;
  try
    result:=Emit.AsString(ANodes);
  finally
    Emit.Free;
  end;
end;

{$IFDEF INTERNAL}
procedure Force_B;
var Bo: TBoolean;
begin
  Bo:=TBoolean.Create(True);
  try
    B(Bo); // <-- this is just to avoid linker to remove the B function for debug
    B([Bo]);
  finally
    Bo.Free;
  end;
end;
{$ENDIF}

initialization
  TVidiEmit.Indentation:='  ';
  TVidiEmit.RaiseErrors:=True;

  {$IFDEF INTERNAL}
  Force_B;
  {$ENDIF}
end.
