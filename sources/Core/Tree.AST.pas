unit Tree.AST;

interface

uses
  Sys, AST, Emit;

type
  TTreeAST=class
  private
    function AddClasses(const AParent:TObject; const ANode:TType):TObject;
    procedure AddClassesOf(const AParent:TObject; const AItems:TNodes);
  protected
    function DoAdd(const ANode:TNode; const S:String; const AParent:TObject):TObject; virtual; abstract;
    function FindType(const AType:TType):TObject; virtual; abstract;
  public
    Emit : TVidiEmit;

    Constructor Create; virtual;
    Destructor Destroy; override;

    function Add(const AParent:TObject; const ANode:TNode):TObject; overload;
    procedure Add(const ANode:TNode); overload;

    procedure TreeModule(const AModule:TType; const AModuleName:String; const FullAST:Boolean); overload;
    procedure TreeModule(const AModule:String; const FullAST:Boolean); overload;
  end;

implementation

uses
  Module, Syntax,

  {$IFDEF INTERNAL}
  Internal,
  {$ENDIF}
  Utils.AST, Evaluator;

{ TTreeAST }

Constructor TTreeAST.Create;
begin
  inherited;
  Emit:=TVidiEmit.Create;
end;

Destructor TTreeAST.Destroy;
begin
  Emit.Free;
  inherited;
end;

function TTreeAST.Add(const AParent:TObject; const ANode:TNode):TObject;

  procedure AddOperand(const AParent:TObject; const AOperand:TOperand);
  begin
    Add(AParent,AOperand.Left);
    Add(AParent,AOperand.Right);
  end;

  procedure AddItems(const AParent:TObject; const ANodes:TNodes);
  var N : TNode;
  begin
    for N in ANodes do
        Add(AParent,N);
  end;

  procedure AddParameters(const AParent:TObject; const AParameters:TNodes);
  begin
    if AParameters<>nil then
       AddItems(AParent,AParameters);
  end;

  function Parameters(const AParameters:TNodes):String;
  var N : TNode;
  begin
    result:='';

    for N in AParameters do
        if result='' then
           result:=B(N)
        else
           result:=result+TSyntax._ItemDelimiter+' '+B(N);

    result:=TSyntax._BeginParameters+result+TSyntax._EndParameters;
  end;

  function TypeName(const AType:TType):String; overload;

    function ArrayTypeName(const AType:TArrayType):String;
    begin
      result:=TypeName(AType.TheType)+TSyntax._BeginArray;

      if AType.Size<>nil then
         result:=result+Emit.AsString(AType.Size);

      result:=result+TSyntax._EndArray;
    end;

  begin
    if AType=nil then
       result:='?'
    else
    if AType is TTypeMember then
       result:=TypeName(TTypeMember(AType).TheType)+
              TSyntax._DataMember+
              TypeName(TTypeMember(AType).Member)
    else
    if AType is TArrayType then
       result:=ArrayTypeName(TArrayType(AType))
    else
    if AType is TNamedType then
       result:=TNamedType(AType).Name
    else
    if AType is TRangeType then
       result:=Emit.AsString(TRangeType(AType).Range)
    else
    if AType is TSpecializedType then
       result:=TypeName(TSpecializedType(AType).TheType)+
               Parameters(TSpecializedType(AType).Generics)
    else
    if AType is TGenericType then
       if TGenericType(AType).Variable.VariableType is TGenericType then
          result:=TGenericType(AType).Variable.Name
       else
          result:=TypeName(TGenericType(AType).Variable.VariableType)
    else
//    if (AType is TVariable) and TChecker.IsVariableOfTypeType(TVariable(AType)) then
//       result:=TVariable(AType).Name
//    else
       result:=AType.ClassName+'?';
  end;

  function TypeName(const ACasting:TCastingData):String; overload;
  begin
    if ACasting.TheType=nil then
       result:='?'
    else
       result:=TypeName(ACasting.TheType.TheType);
  end;

  procedure AddWhen(const AParent:TObject; const AWhen:TWhen);
  var tmp,
      tmpItem : TObject;
      N : TWhenItem;
  begin
    tmp:=Add(AParent,AWhen.Expression);

    for N in AWhen.Items do
    begin
      tmpItem:=Add(tmp,N.Expression);
      Add(tmpItem,N.Block);
    end;

    if AWhen.ElseBlock<>nil then
       Add(tmp,AWhen.ElseBlock);
  end;

  procedure AddFor(const AParent:TObject; const AFor:TFor);
  begin
    Add(AParent,AFor.Counter);

    if AFor.InExpression=nil then
    begin
      Add(AParent,AFor.First);
      Add(AParent,AFor.Last);

      {
      if AFor.Step<>nil then
         Add(AParent,AFor.Step);
      }
    end
    else
      Add(AParent,AFor.InExpression);

    Add(AParent,AFor.Block);
  end;

  function ClausesOf(const AClauses:TClauses):String;

    function Clause(const AValue:Boolean; const AText:String):String;
    begin
      if AValue then
         result:=AText+' '
      else
         result:='';
    end;

  begin
    result:=Clause(AClauses.Indexed,'indexed')+
            Clause(AClauses.Hidden,'hidden')+
            Clause(AClauses.Shared,'shared')+
            Clause(AClauses.Final,'final');
  end;

  function AddSingleType(const AParent:TObject; const AType:TType):TObject;
  begin
    if AType is TArrayType then
       result:=DoAdd(AType,TypeName(AType),AParent)
    else
    if AType is TRangeType then
       result:=Add(AParent,AType)
    else
    // if TChecker.IsInlineStruct(AType.ArrayType) then ... <-- dependency on TChecker ! ?
    if (AType is TClassType) and (TClassType(AType).Name='') then
       result:=Add(AParent,AType)
    else
       result:=AParent;
  end;

  function AddFunction(const APrefix:String; const ANode:TRoutine):TObject;
  var tmp : String;
      tmpOut : TObject;
  begin
    tmp:=APrefix+' '+ANode.Name;

    result:=DoAdd(ANode,tmp,AParent);

    tmp:=ClausesOf(ANode.Clauses);

    if tmp<>'' then
       DoAdd(ANode,tmp,result);

    AddParameters(result,ANode.Parameters);

    if ANode.Output<>nil then
    begin
      tmpOut:=DoAdd(ANode.Output,'output',result);
      AddSingleType(tmpOut,ANode.Output);
    end;
  end;

  function AddType(const AParent:TObject; const AType:TType):TObject;

    function AddArrayType(const AParent:TObject; const AType:TArrayType):TObject;
    begin
      result:=AddSingleType(AParent,AType.TheType);

      if AType.Size<>nil then
         result:=Add(result,AType.Size);
    end;

  begin
    if AType is TArrayType then
       result:=AddArrayType(AParent,TArrayType(AType))
    else
       result:=Add(AParent,AType);
  end;

  procedure TryAdd(const AParent:TObject; const AType:TGenericType);
  begin
    if AType.FinalType<>nil then
       DoAdd(AType,TypeName(AType.FinalType),AParent);
  end;

  function AddVariable(const V:TVariable):TObject;
  var tmp : TObject;
  begin
    result:=DoAdd(ANode,V.Name,AParent);

    tmp:=DoAdd(V.VariableType,TypeName(V.VariableType),result);

    if V.VariableType<>nil then
       if (V.VariableType.Owner=V) or
          (V.VariableType is TArrayType) // <-- temporary until correct ownership
            then
              AddType(tmp,V.VariableType);

    if V.VariableType is TGenericType then
       TryAdd(tmp,TGenericType(V.VariableType));

    if V.TypeInferred then
       DoAdd(V,'type inferred',result);

    if V.ValueData<>nil then
    begin
      if V.ValueData is TArrayExpression then
         result:=DoAdd(V.ValueData,TSyntax._BeginParameters+TSyntax._EndParameters,result);

      Add(result,V.ValueData);
    end;
  end;

  function AddClassType(const AClass:TClassType):TObject;
  var tmp : TObject;
      tmpAncestor : TType;
  begin
    result:=DoAdd(ANode,'class '+AClass.Name,AParent);

    tmpAncestor:=AClass.Ancestor;

    if tmpAncestor<>nil then
    begin
      tmp:=DoAdd(tmpAncestor,TSyntax._Reserved_is+' '+TypeName(tmpAncestor),result);

      if tmpAncestor.Owner=ANode then
         AddType(tmp,tmpAncestor);

      if ANode is TIntegerClass then
         Add(tmp,TIntegerClass(ANode).Size);

//      if AClass.Generics<>nil then
//         AddParameters(tmp,AClass.Generics);
    end;

    if AClass.Parameters<>nil then
       AddParameters(result,AClass.Parameters);

    AddItems(result,AClass.Items);
  end;

  function AddTry(const ATry:TTry):TObject;
  var tmp : TObject;
      C : TCatch;
  begin
    result:=DoAdd(ANode,TSyntax._Reserved_try,AParent);

    if ATry.Block<>nil then
       Add(result,ATry.Block);

    for C in ATry.Catch do
    begin
      tmp:=DoAdd(ANode,TSyntax._Reserved_catch,result);

      if C.Error<>nil then
         Add(tmp,C.Error);

      if C.Block<>nil then
         Add(tmp,C.Block);
    end;

    if ATry.TheFinally<>nil then
    begin
      tmp:=DoAdd(ANode,TSyntax._Reserved_finally,result);
      Add(tmp,ATry.TheFinally);
    end;
  end;

  function AddRoutine(const ARoutine:TRoutine):TObject;
  begin
    result:=AddFunction('function',ARoutine);

    if ARoutine.Ancestor<>nil then
       DoAdd(ARoutine.Ancestor,'override '+Emit.AsString(ARoutine.Ancestor),result);

    if ARoutine.ForwardTo<>nil then
       DoAdd(ARoutine.ForwardTo,'forwarded',result);

    AddItems(result,ARoutine.Items);
  end;

begin
  if ANode=nil then
     result:=DoAdd(ANode,'nil?',AParent)
  else
  if ANode is TBlockStatement then
  begin
    result:=DoAdd(ANode,TSyntax._BeginBlock+' '+TSyntax._EndBlock,AParent);
    result:=DoAdd(TBlockStatement(ANode).Block,'...',result);
    AddItems(result,TBlockStatement(ANode).Block.Items);
  end
  else
  if ANode is TClassType then
     result:=AddClassType(TClassType(ANode))
  else
  if ANode is TUnarySign then
  begin
    if TUnarySign(ANode).Positive then
       result:=DoAdd(ANode,'+',AParent)
    else
       result:=DoAdd(ANode,'-',AParent);

    Add(result,TUnarySign(ANode).Expression);
  end
  else
  if ANode is TUnaryNot then
  begin
    result:=DoAdd(ANode,TSyntax._Reserved_not,AParent);
    Add(result,TUnaryNot(ANode).Expression);
  end
  else
  if ANode is TExtender then
  begin
    if TExtender(ANode).TheType=nil then
       result:=DoAdd(ANode,'?',AParent)
    else
       result:=DoAdd(ANode,TASTUtils.NameOf(ANode,False),AParent);

    Add(result,TExtender(ANode).Extension);
  end
  else
  if ANode is TRoutine then
     result:=AddRoutine(TRoutine(ANode))
  else
  if ANode is TRange then
  begin
    result:=DoAdd(ANode,'Range: '+Emit.AsString(ANode),AParent);

    Add(result,TRange(ANode).Min);
    Add(result,TRange(ANode).Max);
  end
  else
  if ANode is TVariable then
     result:=AddVariable(TVariable(ANode))
  else
  if ANode is TReturn then
  begin
    result:=DoAdd(ANode,TSyntax._Reserved_return,AParent);

    if TReturn(ANode).Value<>nil then
       Add(result,TReturn(ANode).Value);
  end
  else
  if ANode is TWith then
  begin
    result:=DoAdd(ANode,TSyntax._Reserved_with,AParent);

    if TWith(ANode).Alias<>'' then
       DoAdd(TWith(ANode).Module,TWith(ANode).Alias,result);

    if TWith(ANode).Module<>nil then
       DoAdd(TWith(ANode).Module,TWith(ANode).Module.Name,result);
  end
  else
  if ANode is TAssignment then
  begin
    result:=DoAdd(ANode,TASTUtils.AssigmentOf(TAssignment(ANode).Arithmetic),AParent);

    Add(result,TAssignment(ANode).Variable);
    Add(result,TAssignment(ANode).Value);
  end
  else
  if ANode is TIf then
  begin
    result:=DoAdd(ANode,TSyntax._Reserved_if,AParent);
    Add(result,TIf(ANode).Condition);

    // optional "then" part:
    if TIf(ANode).ThenBlock<>nil then
       Add(DoAdd(TIf(ANode).ThenBlock,TSyntax._Reserved_then,result),TIf(ANode).ThenBlock);

    // optional "else" part:
    if TIf(ANode).ElseBlock<>nil then
       Add(DoAdd(TIf(ANode).ElseBlock,TSyntax._Reserved_else,result),TIf(ANode).ElseBlock);
  end
  else
  if ANode is TWhile then
  begin
    result:=DoAdd(ANode,TSyntax._Reserved_while,AParent);
    Add(result,TWhile(ANode).Condition);
    Add(result,TWhile(ANode).Block);
  end
  else
  if ANode is TRepeat then
  begin
    result:=DoAdd(ANode,TSyntax._Reserved_repeat,AParent);
    Add(result,TRepeat(ANode).Block);
    Add(result,TRepeat(ANode).Condition);
  end
  else
  if ANode is TFor then
  begin
    result:=DoAdd(ANode,TSyntax._Reserved_for,AParent);
    AddFor(result,TFor(ANode));
  end
  else
  if ANode is TWhen then
  begin
    result:=DoAdd(ANode,TSyntax._Reserved_when,AParent);
    AddWhen(result,TWhen(ANode));
  end
  else
  { unused:
  if ANode is TCall then
  begin
    result:=DoAdd(ANode,TCall(ANode).Routine.Name,AParent);
    AddParameters(result,TCall(ANode).Parameters);
  end
  else
  }
  if ANode is TBreak then
     result:=DoAdd(ANode,TSyntax._Reserved_break,AParent)
  else
  if ANode is TContinue then
     result:=DoAdd(ANode,TSyntax._Reserved_continue,AParent)
  else
  if ANode is TSpecializedType then
  begin
    result:=Add(AParent,TSpecializedType(ANode).TheType);
    AddParameters(AParent,TSpecializedType(ANode).Generics);
  end
  else
  if ANode is TTypeCall then
  begin
    if TASTUtils.IsLambda(TTypeCall(ANode).TheType) then
       result:=AddRoutine(TRoutine(TTypeCall(ANode).TheType))
    else
       result:=DoAdd(ANode,TypeName(TTypeCall(ANode).TheType),AParent)
  end
  else
  if ANode is TFunctionType then
     result:=AddFunction('function type',TFunctionType(ANode))
  else
  if ANode is TCallData then
  begin
    result:=DoAdd(ANode,'->',AParent);
    Add(result,TCallData(ANode).Value);
  end
  else
  if ANode is TCondition then // before TOperand
  begin
    result:=DoAdd(ANode,TSyntax._Reserved_condition,AParent);
    Add(result,TCondition(ANode).Condition);
    AddOperand(result,TOperand(ANode));
  end
  else
  if ANode is TOperand then
  begin
    result:=DoAdd(ANode,Emit.AsString(TOperand(ANode)),AParent);
    AddOperand(result,TOperand(ANode));
  end
  else
  if ANode is TGroup then
  begin
    result:=DoAdd(ANode,'()',AParent);
    Add(result,TGroup(ANode).Expression);
  end
  else
  if ANode is TMember then
  begin
    result:=DoAdd(ANode,'.',AParent);
    Add(result,TMember(ANode).Data);
    Add(result,TMember(ANode).Member);
  end
  else
  if ANode is TCastingData then
  begin
    result:=DoAdd(ANode,TypeName(TCastingData(ANode))+
                   TSyntax._BeginParameters+
                   TASTUtils.NameOf(TCastingData(ANode).Data,False)+
                   TSyntax._EndParameters,AParent);

    result:=DoAdd(TCastingData(ANode).TheType,TypeName(TCastingData(ANode)),result);
  end
  else
  if ANode is TDataCall then
  begin
    if TDataCall(ANode).Routine=nil then // <-- very strange, assert !
       result:=DoAdd(ANode,'??',AParent)
    else
       result:=DoAdd(ANode,TypeName(TDataCall(ANode).Routine),AParent);

    AddParameters(result,TDataCall(ANode).Parameters);
  end
  else
  if ANode is TArrayExpression then
  begin
    if TArrayExpression(ANode).Data=nil then
       result:=DoAdd(ANode,TSyntax._BeginParameters+TSyntax._EndParameters,AParent)
    else
       result:=DoAdd(ANode,Emit.AsString(TArrayExpression(ANode).Data),AParent);

    AddParameters(result,TArrayExpression(ANode).Parameters);
  end
  else
  if ANode is TNumber then
     result:=DoAdd(ANode,TEvaluate.AsText(TNumber(ANode)),AParent)
  else
  if ANode is TText then
     result:=DoAdd(ANode,TEvaluate.AsText(TText(ANode)),AParent)
  else
  if ANode is TBoolean then
     result:=DoAdd(ANode,TEvaluate.AsText(TBoolean(ANode)),AParent)
  else
  if ANode is TSelf then
     result:=DoAdd(ANode,TSyntax._Reserved_self,AParent)
  else
  if ANode is TAncestor then // Before TData
  begin
    result:=DoAdd(ANode,'ancestor',AParent);
    DoAdd(TAncestor(ANode).Ancestor,TASTUtils.NameOf(TAncestor(ANode).Ancestor,False),result);
    Add(result,TAncestor(ANode).DataCall);
  end
  else
  if ANode is TData then
     result:=DoAdd(ANode,TASTUtils.NameOf(ANode,True),AParent)
  else
  {
  if ANode is TComment then
     result:=AParent //result:=DoAdd(ANode,'//',AParent)
  else
  }
  if ANode is TRangeType then
     result:=Add(AParent,TRangeType(ANode).Range)
  else
  if ANode is TGenericType then  // <-- before TType !
  begin
    result:=DoAdd(ANode,'generic type',AParent);
    DoAdd(TGenericType(ANode).Variable,TGenericType(ANode).Variable.Name,result);

    if TGenericType(ANode).FinalType<>nil then
       Add(result,TGenericType(ANode).FinalType);
  end
  else
  if ANode is TTypeMember then  // <-- before TType !
  begin
    result:=DoAdd(ANode,TSyntax._DataMember,AParent);
    DoAdd(TTypeMember(ANode).TheType,TypeName(TTypeMember(ANode).TheType),result);
    DoAdd(TTypeMember(ANode).Member,TypeName(TTypeMember(ANode).Member),result);
  end
  else
  if ANode is TType then // Module
  begin
    result:=DoAdd(ANode,'type',AParent);
    AddItems(result,TType(ANode).Items);
  end
  else
  if ANode is TTry then
     result:=AddTry(TTry(ANode))
  else
  {$IFDEF INTERNAL}
  begin
    result:=nil;
    InternalError('TreeVCL AddNode: ',ANode);
  end;
  {$ELSE}
  if ANode=nil then
     result:=DoAdd(ANode,'? nil!',AParent)
  else
     result:=DoAdd(ANode,'? '+TASTUtils.NameOf(ANode,True),AParent);
  {$ENDIF}
end;

procedure TTreeAST.Add(const ANode:TNode);
begin
  Add(nil,ANode);
end;

function TTreeAST.AddClasses(const AParent:TObject; const ANode:TType):TObject;

  function AddItemsOf(const ANode:TType; const AText:String; const AParent:TObject):TObject;
  begin
    result:=DoAdd(ANode,AText,AParent);

    if ANode<>nil then
       AddClassesOf(result,ANode.Items);
  end;

  function TextOf(const ARoutine:TRoutine):String;
  begin
    if ARoutine.Output=nil then
       result:='method'
    else
       result:='function';

    result:=result+' '+ARoutine.Name;
  end;

var S : String;
begin
  if ANode is TClassType then
  begin
    S:=TClassType(ANode).Name;

    if TClassType(ANode).Ancestor<>nil then
       S:=S+' is '+TASTUtils.TypeNameOf(TClassType(ANode).Ancestor);
  end
  else
  if ANode is TRoutine then
     S:=TextOf(TRoutine(ANode))
  else
  if ANode is TFunctionType then
     S:='function type '+TFunctionType(ANode).Name
  else
  if ANode is TExtender then
  begin
//    tmp:=FindType(TExtender(ANode).TheType); // why ??
//    if tmp=nil then
    S:=TASTUtils.TypeNameOf(TExtender(ANode).TheType)+TSyntax._DataMember+TASTUtils.TypeNameOf(ANode);

       //TextOf(TExtender(ANode).Routine)

    result:=AddItemsOf(TExtender(ANode).Extension,S,AParent);
    Exit;
  end
  else
     S:='? '+TASTUtils.TypeNameOf(ANode); // Error !!

  result:=AddItemsOf(ANode,S,AParent);
end;

procedure TTreeAST.AddClassesOf(const AParent:TObject; const AItems:TNodes);
var N : TNode;
begin
  for N in AItems do
      if N is TType then
         AddClasses(AParent,TType(N));
end;

procedure TTreeAST.TreeModule(const AModule:TType; const AModuleName:String; const FullAST:Boolean);

  procedure AddTopLevel(const AParent:TObject; const AType:TType);
  var N : TNode;
  begin
    for N in AType.Items do
        if N is TBlockStatement then
        begin
          if N.Owner=AModule then
             DoAdd(AModule,AModuleName,AParent);

          AddTopLevel(AParent,TBlockStatement(N).Block);
        end
        else
        if N is TType then
           AddClasses(AParent,TType(N));
  end;

begin
  if AModule<>nil then
     if FullAST then
        Add(AModule)
     else
        AddTopLevel(nil,AModule);
end;

procedure TTreeAST.TreeModule(const AModule:String; const FullAST:Boolean);
begin
  TreeModule(Modules.Find(AModule),AModule,FullAST);
end;

end.
