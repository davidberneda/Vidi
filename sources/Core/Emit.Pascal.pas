unit Emit.Pascal;

interface

uses
  Emit, Sys, AST;

type
  TPascalEmit=class(TBaseEmit)
  private
    var
    ToFree : Array of TVariable;

    procedure AddArrayTypes(const APrefix:String; const AItems:TNodes);

    procedure AddAssignCondition(const APrefix:String; const ACondition:TCondition; const AVariable:TVariable);
    procedure AddAssignment(const APrefix:String; const Assignment:TAssignment; InlineVar:Boolean);
    procedure AddCallData(const APrefix:String; const ACall:TCallData);
    procedure AddClass(const APrefix:String; const AClass:TClassType);
    procedure AddClauses(const AClauses:TClauses; const IsRoutine:Boolean);
    procedure AddComment(const AComment:String);
    procedure AddCondition(const APrefix:String; const ACondition:TCondition; const AddExits:Boolean);
    procedure AddCreate(const AClass:TClassType; const AtInterface:Boolean);
    procedure AddData(const AData:TData);
    procedure AddError(const S:String);
    procedure AddFullName(const AType:TType);
    procedure AddImplementation(const APrefix:String; const ANode:TNode);
    procedure AddImplementationClass(const APrefix:String; const AClass:TClassType);
    procedure AddImplementationItems(const APrefix:String; const AItems:TNodes);
    procedure AddImplementationClassNodes(const APrefix:String;
                                          const AClass:TClassType;
                                          const AItems:TNodes);
    procedure AddInterfaceItems(const APrefix:String; const AItems:TNodes; const AClass:TClassType);
    procedure AddKeyword(const S:String);
    procedure AddLocals(const APrefix:String; const ANode:TNode);
    procedure AddName(const ANode:TNode);
    procedure AddReturn(const AData:TData);
    procedure AddRoutine(const APrefix:String; const R:TRoutine; const IsDeclaration:Boolean; const AClass:TClassType);
    procedure AddSpace;
    procedure AddSymbol(const S:String);
    procedure AddToFree(const AVariable:TVariable);
    procedure AddType(const AType:TType);
    procedure AddTypeClause(const APrefix:String);
    procedure AddValueData(const AData:TData);
    procedure AddVariable(const APrefix:String; const AVariable:TVariable; const AtInterface:Boolean);
    procedure AddWhen(const APrefix:String; const AWhen:TWhen);

    procedure ArrayType(const AType:TArrayType);
    function CreateAliasType(const AType:TType; const ARange:String):String;
    procedure DataToString(const AData:TData; OnlyName:Boolean);
    procedure DeclaringParameters(const AParameters:TNodes);
    procedure DoTypeToString(const AType:TType);
    function FinalName(const ANode:TNode):String;
    procedure GetArrayExpression(const ANode:TArrayExpression);
    procedure GetTypeIndexes(const AType:TParametersType);
    procedure Nodes(const APrefix:String; const ANodes:TNodes);
    procedure Parameters(const AParameters:TNodes; const OnlyName:Boolean);
    procedure RangeToString(const ARange:TRange);
    procedure TryParameters(const AParameters:TNodes);
    procedure TypeMember(const AType:TTypeMember);
    procedure VariableAndTypes(const AVariable:TVariable);
  public
    class var
      InlineVariables,
      UniqueUnitName : Boolean;

      RaiseErrors : Boolean;
      Indentation : String;

    function AsString(const ANode:TNode):String;

    class function ModuleName(const AModule:TNamedType):String; static;

    procedure Node(const APrefix:String; const ANode:TNode); overload;
    procedure Node(const ANode:TNode); overload;

    procedure Operand(const AOperand:TOperand);
  end;

function P(const ANode:TNode):String; // Helper method, useful for debugging

{$IFDEF INTERNAL}
var
  LastEmitPascal:TNode;
{$ENDIF}

implementation

uses
  SysUtils, Map, Checker.AST, Find.AST,
  Syntax, PascalSyntax, Utils.AST, Evaluator.CompileTime,
  Module, Exceptions, StringArray;

procedure TPascalEmit.RangeToString(const ARange:TRange);
begin
  DataToString(ARange.Min,True);
  AddSymbol(TPascalSyntax._RangeDelimiter);
  DataToString(ARange.Max,True);
end;

procedure TPascalEmit.DeclaringParameters(const AParameters:TNodes);
begin
  if AParameters<>nil then
  begin
    AddSymbol(TPascalSyntax._BeginParameters);
    Parameters(AParameters,False);
    AddSymbol(TPascalSyntax._EndParameters);
  end;
end;

procedure TPascalEmit.TryParameters(const AParameters:TNodes);
begin
  if AParameters<>nil then
  begin
    AddSymbol(TPascalSyntax._BeginParameters);
    AddSpace;
    Parameters(AParameters,True);
    AddSpace;
    AddSymbol(TPascalSyntax._EndParameters);
  end;
end;

procedure TPascalEmit.GetTypeIndexes(const AType:TParametersType);
begin
  if AType.Parameters=nil then
  else
  if AType is TFunctionType then
     TryParameters(AType.Parameters)
  else
  begin
    AddSymbol('[');
    Parameters(AType.Parameters,True);
    AddSymbol(']');
  end;
end;

function IsReserved(const S:String):Boolean;
begin
  TPascalSyntax.Reserved.SortedFind(S,result,True);
end;

function CorrectName(const S:String):String;
begin
  if IsReserved(S) then
     result:='_'+S
  else
     result:=StringReplace(S,' ','_',[rfReplaceAll]);
end;

function CorrectParts(S:String):String;
var i : Integer;
begin
  result:='';

  repeat
    i:=Pos(TPascalSyntax._DataMember,S);

    if i>0 then
    begin
      result:=result+CorrectName(Copy(S,1,i-1))+TPascalSyntax._DataMember;
      Delete(S,1,i);
    end
    else
      result:=result+CorrectName(S);

  until i=0;
end;

class function TPascalEmit.ModuleName(const AModule: TNamedType): String;
begin
  result:=CorrectParts(AModule.Name);
end;

procedure TPascalEmit.TypeMember(const AType:TTypeMember);
begin
  DoTypeToString(AType.TheType);
  AddSymbol(TPascalSyntax._DataMember);
  DoTypeToString(AType.Member);
end;

procedure TPascalEmit.ArrayType(const AType:TArrayType);
begin
  AddKeyword(TPascalSyntax._Reserved_Array);

  if AType.Size<>nil then
  begin
    AddSymbol(TPascalSyntax._BeginArray);

    if AType.Size is TInteger then // TIntegerClass ?
    begin
      Add(AType.Size,'0');
      AddSymbol(TPascalSyntax._RangeDelimiter);
      Add(AType.Size,IntToStr(TInteger(AType.Size).Value-1));
    end
    else
      DataToString(AType.Size,False);

    AddSymbol(TPascalSyntax._EndArray);
  end;

  AddSpace;
  AddKeyword(TPascalSyntax._Reserved_Of);
  AddSpace;

  DoTypeToString(AType.TheType);
end;

function TPascalEmit.CreateAliasType(const AType:TType; const ARange:String):String;

  function TypeToString(const AType:TType):String;
  var tmp : TPascalEmit;
  begin
    tmp:=TPascalEmit.Create;
    try
      tmp.DoTypeToString(AType);
      result:=tmp.Emit.GetText;
    finally
      tmp.Free;
    end;
  end;

var tmpItem : String;
begin
  tmpItem:=TypeToString(AType);

  result:='T'+TPascalSyntax._Reserved_Array+ARange+
          TPascalSyntax._Reserved_Of+tmpItem;
end;

function IsArrayTypeMinMax(const AType:TArrayType; out AMin,AMax:Integer):Boolean;
begin
  if AType.Size=nil then
  begin
    AMin:=-1;
    AMax:=-1;

    result:=True;
  end
  else
  if AType.Size is TInteger then
  begin
    AMin:=0;
    AMax:=TInteger(AType.Size).Value-1;

    result:=True;
  end
  else
  if AType.Size is TRange then
  begin
    AMin:=TEvaluateCompile.AsInteger(TRange(AType.Size).Min);
    AMax:=TEvaluateCompile.AsInteger(TRange(AType.Size).Max);

    result:=True;
  end
  else
    result:=False;
end;

function ArrayTypeRange(const AMin,AMax:Integer):String;
begin
  if AMin=-1 then
     result:=''
  else
     result:=IntToStr(AMin)+'_'+IntToStr(AMax);
end;

procedure TPascalEmit.AddArrayTypes(const APrefix:String; const AItems:TNodes);
type
  TTypeAlias=record
  public
    ItemType : TType;  // of XXX (ie: Integer)
    Min,Max : Integer; // 0..10
    SizeType : TType;  // Radius:Float[Planets]
  end;

var
  TypeAliases : Array of TTypeAlias;
  First : Boolean;

  function NewAliasType(const AItemType:TType):Integer;
  begin
    result:=Length(TypeAliases);
    SetLength(TypeAliases,result+1);

    TypeAliases[result].ItemType:=AItemType;
    TypeAliases[result].Min:=-1;
    TypeAliases[result].Max:=-1;
  end;

  procedure DoAddTypeAlias(const AType:TArrayType; const AName:String);
  begin
    if First then
    begin
      AddTypeClause(APrefix);
      First:=False;
    end;

    Add(APrefix+Indentation);
    Add(AType.TheType,AName);
    AddSpace;
    AddSymbol(TPascalSyntax._Symbol_Equal);
    AddSpace;

    ArrayType(AType);

    AddSymbol(TPascalSyntax._EndOfStatement);

    AddLine;
  end;

  procedure AddTypeAlias(const AType:TArrayType; const AMin,AMax:Integer); overload;
  var L,t : Integer;
      tmp: String;
  begin
    for t:=Low(TypeAliases) to High(TypeAliases) do
        if (TypeAliases[t].ItemType=AType.TheType) and
           (TypeAliases[t].Min=AMin) and
           (TypeAliases[t].Max=AMax) then

           Exit;

    L:=NewAliasType(AType.TheType);
    TypeAliases[L].Min:=AMin;
    TypeAliases[L].Max:=AMax;

    tmp:=CreateAliasType(AType.TheType,ArrayTypeRange(AMin,AMax));

    DoAddTypeAlias(AType,tmp);
  end;

  procedure AddTypeAlias(const AType:TArrayType; const ASize:TType); overload;
  var L,t : Integer;
      tmp : String;
  begin
    for t:=Low(TypeAliases) to High(TypeAliases) do
        if (TypeAliases[t].ItemType=AType.TheType) and
           (TypeAliases[t].SizeType=ASize) then
           Exit;

    L:=NewAliasType(AType.TheType);
    TypeAliases[L].SizeType:=ASize;

    tmp:=CreateAliasType(AType.TheType,FinalName(ASize));

    DoAddTypeAlias(AType,tmp);
  end;

  procedure ArrayTypeAlias(const AType:TArrayType);
  var tmpMin,
      tmpMax : Integer;
  begin
    if IsArrayTypeMinMax(AType,tmpMin,tmpMax) then
       AddTypeAlias(AType,tmpMin,tmpMax)
    else
    if AType.Size is TTypeCall then
       AddTypeAlias(AType,TTypeCall(AType.Size).TheType);
  end;

  procedure TryAddType(const AType:TType);
  begin
    if AType is TArrayType then
       ArrayTypeAlias(TArrayType(AType));
  end;

  procedure TryAdd(const AItems:TNodes);
  var N : TNode;
  begin
    for N in AItems do
        if N is TArrayType then
           ArrayTypeAlias(TArrayType(N))
        else
        if N is TBlockStatement then
           TryAdd(TBlockStatement(N).Block.Items)
        else
        if N is TVariable then
           TryAddType(TVariable(N).VariableType)
        else
        if N is TClassType then
           TryAddType(TClassType(N).Ancestor)
  end;

begin
  First:=True;
  TryAdd(AItems);

  if not First then
     AddLine;
end;

procedure TPascalEmit.DoTypeToString(const AType:TType);

  procedure TryArrayType(const AType:TArrayType);
  var tmpMin,
      tmpMax : Integer;
  begin
    if IsArrayTypeMinMax(AType,tmpMin,tmpMax) then
       Add(AType,CreateAliasType(AType.TheType,ArrayTypeRange(tmpMin,tmpMax)))
    else
    if AType.Size is TTypeCall then
       Add(AType,CreateAliasType(AType.TheType,FinalName(AType.Size)))
    else
       ArrayType(AType);
  end;

begin
  if AType=nil then
     AddError('?')
  else
  if AType is TArrayType then
     TryArrayType(TArrayType(AType))
  else
  if AType is TRangeType then
     RangeToString(TRangeType(AType).Range)
  else
  if AType is TTypeMember then
     TypeMember(TTypeMember(AType))
  else
  if AType is TNamedType then
  begin
    if TChecker.IsInlineStruct(AType) then
       // !! TODO AddError(AType.ClassName+'?')
    else
    begin
      AddName(AType);

      if AType is TClassType then
         GetTypeIndexes(TClassType(AType));
    end;
  end;
end;

procedure TPascalEmit.AddError(const S:String);
begin
  Emit.Add(TPositionStyle.Comment,S); // TODO: TPositionStyle.Error

  if RaiseErrors then
     Raise_Exception(S);
end;

procedure TPascalEmit.Operand(const AOperand:TOperand);

  procedure AddOperand;
  var tmp : String;
  begin
    if AOperand is TIsEqual then tmp:='=' else
    if AOperand is TIsNotEqual then tmp:='<>' else
    if AOperand is TIsLower then tmp:='<' else
    if AOperand is TIsLowerOrEqual then tmp:='<=' else
    if AOperand is TIsGreater then tmp:='>' else
    if AOperand is TIsGreaterOrEqual then tmp:='>=' else
    if AOperand is TAddition then tmp:='+' else
    if AOperand is TSubtraction then tmp:='-' else
    if AOperand is TMultiplication then tmp:='*' else
    if AOperand is TDivision then tmp:='/' else
    if AOperand is TCondition then tmp:=':'
    else
    begin
      AddError('Cannot convert operand to string: '+TASTUtils.NameOf(AOperand,True));
      Exit;
    end;

    AddSymbol(tmp);
  end;

begin
  if AOperand is TContains then AddKeyword(TPascalSyntax._Reserved_in) else
  if AOperand is TLogicalAnd then AddKeyword(TPascalSyntax._Reserved_and) else
  if AOperand is TLogicalOr then AddKeyword(TPascalSyntax._Reserved_or) else
  if AOperand is TLogicalXor then AddKeyword(TPascalSyntax._Reserved_xor)
  else
     AddOperand;
end;

function IsClass(const ANode:TNode; const AName:String):Boolean;
begin
  result:=(ANode is TClassType) and SameText(TClassType(ANode).Name,AName);
end;

function IsRoutine(const ANode:TNode; const AName:String):Boolean;
begin
  result:=(ANode is TRoutine) and SameText(TRoutine(ANode).Name,AName);
end;

function TPascalEmit.FinalName(const ANode:TNode):String;

  function CheckRedeclared(const S:String):String;
  var tmp : TNamedType;
  begin
    tmp:=TFinder.ModuleOf(ANode);

    if (tmp<>nil) and SameText(S,ModuleName(tmp)) then
       result:='_'+S
    else
       result:=S;
  end;

var tmp : String;
begin
  tmp:=TASTUtils.NodeName(ANode);

  if UniqueUnitName and (ANode.Owner=TFinder.ModuleOf(ANode)) then
     tmp:=CheckRedeclared(tmp);

  result:=CorrectName(tmp);
end;

procedure TPascalEmit.AddName(const ANode:TNode);
begin
  // TODO: Make sure AsText is the AsText we want (not a duplicate! )
  if IsRoutine(ANode,'AsText') then
     Add(ANode,'ToString')
  else
  if IsClass(ANode,'Math') then
     Add(ANode,'_Math')
  else
     Add(ANode,FinalName(ANode));
end;

procedure TPascalEmit.AddSpace;
begin
  Add(' ');
end;

procedure TPascalEmit.AddKeyword(const S: String);
begin
  Emit.Add(TPositionStyle.Keyword,S);
end;

procedure TPascalEmit.AddSymbol(const S: String);
begin
  Emit.Add(TPositionStyle.Symbol,S);
end;

function TPascalEmit.AsString(const ANode: TNode): String;

  {
  function Aliases:String;
  var t : Integer;
  begin
    result:='type'+NewLine;

    for t:=0 to High(TypeAliases) do
        result:=result+'  '+TypeAliases[t].Alias+' = '+TypeAliases[t].Code+';'+NewLine
  end;
  }

//var tmp : String;
begin
  Emit.Clear;

  Node(ANode);

  result:=Emit.GetText;

  {
  if TypeAliases<>nil then
  begin
    tmp:=Copy(result,TypeAliasesPos+1,Length(result));
    result:=Copy(result,1,TypeAliasesPos)+Aliases+tmp;
  end;
  }
end;

procedure TPascalEmit.AddType(const AType:TType);
begin
  AddSymbol(TPascalSyntax._TypeDelimiter);
  AddSpace;
  DoTypeToString(AType);
end;

procedure TPascalEmit.VariableAndTypes(const AVariable:TVariable);
begin
  AddType(AVariable.VariableType);
end;

procedure TPascalEmit.Parameters(const AParameters:TNodes; const OnlyName:Boolean);

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
    if AParam is TVariable then
    begin
      if TVariable(AParam).Clauses.Final then
      begin
        if not OnlyName then
        begin
          AddKeyword(TPascalSyntax._Reserved_Const);
          AddSpace;
        end;

        AddName(AParam);
      end
      else
      if OnlyName then
         AddName(AParam)
      else
      begin
        AddKeyword(TPascalSyntax._Reserved_out);
        AddSpace;
        AddName(AParam);
      end
    end
    else
    if AParam is TData then
       DataToString(TData(AParam),True)
  end;

var t : Integer;
begin
  for t:=0 to High(AParameters) do
  begin
    if t=0 then
       ParameterToString(AParameters[t])
    else
    begin
      if OnlyName then
         AddSymbol(TPascalSyntax._ItemDelimiter)
      else
         AddSymbol(TPascalSyntax._EndOfStatement);

      AddSpace;
      ParameterToString(AParameters[t]);
    end;

    if not OnlyName then
    if HasType(AParameters[t]) then
       if (t=High(AParameters)) or (not SameType(AParameters[t],AParameters[t+1])) then
          VariableAndTypes(TVariable(AParameters[t]));
  end;
end;

procedure TPascalEmit.GetArrayExpression(const ANode:TArrayExpression);
begin
  if ANode.Data<>nil then
     DataToString(ANode.Data,True);

  AddSymbol(TPascalSyntax._BeginArray);
  Parameters(ANode.Parameters,True);
  AddSymbol(TPascalSyntax._EndArray);
end;

procedure TPascalEmit.AddComment(const AComment:String);
begin
{
  Emit.Add(TPositionStyle.Comment,TPascalSyntax._SingleLineComment+' '+AComment);
//  Emit.Add(TPositionStyle.Comment,TPascalSyntax._BeginComment+AComment+TPascalSyntax._EndComment);

  AddLine;
}
end;

procedure TPascalEmit.AddClauses(const AClauses:TClauses; const IsRoutine:Boolean);
begin
  if AClauses.Indexed then
  begin
    AddComment(TPascalSyntax._Reserved_indexed);
    AddSpace;
  end;

  if AClauses.Hidden then
  begin
    AddKeyword(TPascalSyntax._Reserved_Private);
    AddSpace;
  end;

  if AClauses.Shared then
  begin
    if AClauses.Final then
    begin
      AddKeyword(TPascalSyntax._Reserved_Const);
      AddSpace;
    end
    else
    if not IsRoutine then
    begin
      AddKeyword(TPascalSyntax._Reserved_Class);
      AddSpace;

      AddKeyword(TPascalSyntax._Reserved_Var);
      AddSpace;
    end;
  end
  else
  if AClauses.Final then
  begin
    AddKeyword(TPascalSyntax._Reserved_Const);
    AddSpace;
  end;
end;

procedure TPascalEmit.AddToFree(const AVariable:TVariable);
var L : Integer;
begin
  L:=Length(ToFree);
  SetLength(ToFree,L+1);
  ToFree[L]:=AVariable;
end;

procedure TPascalEmit.AddValueData(const AData:TData);
begin
  AddSpace;
  AddSymbol(TPascalSyntax._Assignment);
  AddSpace;

  DataToString(AData,True);
end;

procedure TPascalEmit.AddVariable(const APrefix:String; const AVariable:TVariable;
                                  const AtInterface:Boolean);

  procedure AddClassField;
  begin
    Add(APrefix);

    AddClauses(AVariable.Clauses,False);

    AddName(AVariable);
    AddSpace;

    if AVariable.Clauses.Final and (AVariable.ValueData<>nil) then
    begin
      AddSymbol(TPascalSyntax._Symbol_Equal);
      AddSpace;

      DataToString(AVariable.ValueData,True);
    end
    else
      VariableAndTypes(AVariable);

    AddSymbol(TPascalSyntax._EndOfStatement);

    if AVariable.Clauses.Hidden then
    begin
      AddLine;
      Add(APrefix);
      AddKeyword(TPascalSyntax._Reserved_Public);
    end;
  end;

  procedure InlineVariableNameAndType;
  begin
    Add(APrefix);

    AddKeyword(TPascalSyntax._Reserved_Var);
    AddSpace;

    AddName(AVariable);
    AddSpace;
    VariableAndTypes(AVariable);

//  AddSymbol(TPascalSyntax._EndOfStatement);
//  AddLine;
  end;

  // Foo := TFoo.Create;
  procedure AddCallToCreate;
  begin
    DoTypeToString(AVariable.VariableType);
    AddSymbol(TPascalSyntax._DataMember);
    Add(TPascalSyntax._Reserved_Create);

    AddToFree(AVariable);

    AddSymbol(TPascalSyntax._EndOfStatement);
  end;

  procedure AddDefaultValueOf(const AType:TType);
  begin
    if (AType=TChecker._Types[TChecker._Integer]) or (AType=TChecker._Types[TChecker._Float]) then
       Add('0')
    else
    if AType=TChecker._Types[TChecker._Boolean] then
       Add(TPascalSyntax._Reserved_false)
    else
    if AType=TChecker._Types[TChecker._Text] then
       Add(''''+'''')
    else
       AddError('Cannnot add default value for type: '+AType.ClassName);
  end;

begin
  if AtInterface then
     AddClassField
  else
  if AVariable.ValueData=nil then
  begin
    // var Foo : TFoo;

    if InlineVariables then
    begin
      InlineVariableNameAndType;
      AddSymbol(TPascalSyntax._EndOfStatement);
      AddLine;
    end;

    if AVariable.VariableType is TArrayType then
    else
    begin
      Add(APrefix);
      AddName(AVariable);
      AddSpace;

      AddSymbol(TPascalSyntax._Assignment);
      AddSpace;

      if TChecker.IsBasicType(AVariable.VariableType) then
         AddDefaultValueOf(AVariable.VariableType)
      else
         AddCallToCreate;
    end;
  end
  else
  begin
    if AVariable.ValueData is TCondition then
    begin
      if InlineVariables then
      begin
        InlineVariableNameAndType;
        AddSymbol(TPascalSyntax._EndOfStatement);
        AddLine;
      end;

      AddAssignCondition(APrefix,TCondition(AVariable.ValueData),AVariable);
    end
    else
    if AVariable.Clauses.Final then
       Exit
    else
    begin
      Add(APrefix);

      if InlineVariables then
      begin
        AddKeyword(TPascalSyntax._Reserved_Var);
        AddSpace;
      end;

      AddName(AVariable);
      AddValueData(AVariable.ValueData);
    end;

//    AddSymbol(TPascalSyntax._EndOfStatement);
  end;
end;

procedure TPascalEmit.AddData(const AData:TData);
begin
  if AData is TGroup then // skip ( )
     DataToString(TGroup(AData).Expression,True)
  else
     DataToString(AData,True);
end;

// if xx then p:=Foo else p:=Bar
procedure TPascalEmit.AddAssignCondition(const APrefix:String; const ACondition:TCondition; const AVariable:TVariable);

  procedure AddIndent;
  var t : Integer;
  begin
    AddLine;
    Add(APrefix{+Indentation});

    for t:=1 to 1+Length(TPascalSyntax._Reserved_if) do
        AddSpace;
  end;

  procedure AddAssign(const AData:TData);
  begin
    AddIndent;

    AddName(AVariable);
    AddSpace;
    AddSymbol(TPascalSyntax._Assignment);
    AddSpace;
    DataToString(AData,True);
  end;

begin
  Add(APrefix);

  AddKeyword(TPascalSyntax._Reserved_if);
  AddSpace;

  AddData(ACondition.Condition);

  AddSpace;
  AddKeyword(TPascalSyntax._Reserved_then);
  AddAssign(ACondition.Left);

  AddLine;
  Add(APrefix{+Indentation});

  AddKeyword(TPascalSyntax._Reserved_else);
  AddAssign(ACondition.Right);
end;

procedure TPascalEmit.AddCondition(const APrefix:String; const ACondition:TCondition; const AddExits:Boolean);
begin
  Add(APrefix);

  AddKeyword(TPascalSyntax._Reserved_if);
  AddSpace;

  AddData(ACondition.Condition);

  AddSpace;
  AddKeyword(TPascalSyntax._Reserved_then);
  AddSpace;

  if AddExits then
  begin
    AddLine;
    Add(APrefix+Indentation);
    AddReturn(ACondition.Left);
  end
  else
    DataToString(ACondition.Left,True);

  if AddExits then
  begin
    AddLine;
    Add(APrefix);
  end
  else
    AddSpace;

  AddKeyword(TPascalSyntax._Reserved_else);

  if AddExits then
  begin
    AddLine;
    Add(APrefix+Indentation);
    AddReturn(ACondition.Right);
  end
  else
  begin
    AddSpace;
    DataToString(ACondition.Right,True);
  end;
end;

function EscapeQuotes(const S:String):String;
const SingleQuote=#39#39#39#39;
var t : Integer;
begin
  result:='';

  for t:=1 to Length(S) do
      if S[t]='''' then
         result:=result+SingleQuote
      else
         result:=result+S[t];
end;

// Octal base 8 and binary base 2 not supported in Pascal
function PascalIntegerToString(const Base:Byte; const Value:Int64):String;
begin
  if Base=16 then
     result:='$'+IntToHex(Value {$IFDEF FPC},32{$ENDIF}) // Review FPC !
  else
     result:=IntToStr(Value);
end;

function IsArrayVariable(const AData:TData):Boolean;
begin
  result:=(AData is TVariable) and TChecker.IsArray(TVariable(AData).VariableType);
end;

function IsCallTo(const AData:TData; const AType:TType; const ARoutine:String):Boolean;
var tmp : TType;
begin
  result:=AData is TDataCall;

  if result then
  begin
    tmp:=TDataCall(AData).Routine;

    result:=(tmp.Owner=AType) and (tmp is TRoutine) and
            SameText(TRoutine(tmp).Name,ARoutine);
  end;
end;

procedure TPascalEmit.DataToString(const AData:TData; OnlyName:Boolean);

  function AddArrayMethod(const AMember:TMember):Boolean;

    function IsCall(const AName:String):Boolean;
    begin
      result:=IsCallTo(AMember.Member,TChecker._Types[TChecker._Array],AName);
    end;

    function TryMethod(const AName,APascalName:String):Boolean;
    begin
      result:=IsCall(AName);

      if result then
      begin
        Add(APascalName);
        TryParameters([AMember.Data]);
      end;
    end;

    function TryClear:Boolean;
    begin
      result:=IsCall('Clear');

      if result then
      begin
        DataToString(AMember.Data,True);
        AddSpace;
        AddSymbol(TPascalSyntax._Assignment);
        AddSpace;
        AddSymbol(TPascalSyntax._BeginArray);
        AddSymbol(TPascalSyntax._EndArray);
      end;
    end;

    function TryAppend:Boolean;
    begin
      result:=IsCall('Append') and (AMember.Member is TDataCall);

      if result then
      begin
        DataToString(AMember.Data,True);
        AddSpace;
        AddSymbol(TPascalSyntax._Assignment);
        AddSpace;
        DataToString(AMember.Data,True);
        AddSymbol(TPascalSyntax._Symbol_Add);

        AddSymbol(TPascalSyntax._BeginArray);
        Parameters(
        TDataCall(AMember.Member).Parameters,True);

        AddSymbol(TPascalSyntax._EndArray);

        //DataToString(AMember.Member,True);
      end;
    end;

  begin
    result:=IsArrayVariable(AMember.Data);

    if result then
       result:=TryMethod('Low','Low') or
               TryMethod('High','High') or
               TryMethod('Count','Length') or
               TryClear or
               TryAppend;
  end;

  procedure AddText(const AData:TData; const AText:String);
  begin
    Add(AData,EscapeQuotes(AText)); // <-- TODO: Escape single quotes
  end;

  procedure OperandToString(const AOperand:TOperand);
  begin
    DataToString(AOperand.Left,True);
    AddSpace;
    Operand(AOperand);
    AddSpace;
    DataToString(AOperand.Right,True);
  end;

begin
  if AData is TGroup then
  begin
    AddSymbol(TPascalSyntax._BeginParameters);
    AddSpace;
    DataToString(TGroup(AData).Expression,False);
    AddSpace;
    AddSymbol(TPascalSyntax._EndParameters);
  end
  else
  if AData is TCondition then
     AddCondition('',TCondition(AData),False)
  else
  if AData is TOperand then
     OperandToString(TOperand(AData))
  else
  if AData is TVariable then
     AddName(TVariable(AData))
  else
  if AData is TBoolean then
     Add(AData,BoolToStr(TBoolean(AData).Value,True))
  else
  if AData is TInteger then
     Add(AData,PascalIntegerToString(TInteger(AData).Base,TInteger(AData).Value))
  else
  if AData is TFloat then
     Add(AData,FloatToStr(TFloat(AData).Value))
  else
  if AData is TText then
  begin
    AddSymbol('''');
    AddText(AData,TText(AData).Value);
    AddSymbol('''');
  end
  else
  if AData is TDataCall then
  begin
    AddName(TDataCall(AData).Routine);
    TryParameters(TDataCall(AData).Parameters);
  end
  else
  if AData is TUnaryNot then
  begin
    AddKeyword(TPascalSyntax._Reserved_not);
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
  if AData is TArrayExpression then
     GetArrayExpression(TArrayExpression(AData))
  else
  if AData is TMember then
  begin
    if not AddArrayMethod(TMember(AData)) then
    begin
      DataToString(TMember(AData).Data,True);
      AddSymbol(TPascalSyntax._DataMember);
      DataToString(TMember(AData).Member,True);
    end;
  end
  else
  if AData is TTypeCall then
     DoTypeToString(TTypeCall(AData).TheType)
  else
  if AData is TRange then
     RangeToString(TRange(AData))
  else
  if AData is TCastingData then
  begin
    AddName(TCastingData(AData).TheType);
    AddSymbol(TPascalSyntax._BeginParameters);
    DataToString(TCastingData(AData).Data,True);
    AddSymbol(TPascalSyntax._EndParameters);
  end
  else
  if AData is TSelf then
     AddKeyword(TPascalSyntax._Reserved_self)
  else
  if AData is TAncestor then
     AddKeyword(TPascalSyntax._Reserved_Inherited)
  else
     AddError('Cannot convert to string: '+TASTUtils.NameOf(AData,True));
end;

procedure TPascalEmit.Nodes(const APrefix:String; const ANodes:TNodes);
var n : TNode;
begin
  for n in ANodes do
  begin
    Node(APrefix,n);

    //AddSymbol(TPascalSyntax._EndOfStatement);
    AddLine;
  end;
end;

procedure TPascalEmit.AddAssignment(const APrefix:String; const Assignment:TAssignment; InlineVar:Boolean);
begin
  Add(APrefix);

  if InlineVar then
  begin
    AddKeyword(TPascalSyntax._Reserved_Var);
    AddSpace;
  end;

  DataToString(Assignment.Variable,True);
  AddSpace;

  AddSymbol(TPascalSyntax._Assignment);  // :=

  AddSpace;

  if Assignment.Arithmetic<>TArithmetic_Assign.None then
  begin
    DataToString(Assignment.Variable,True);
    AddSpace;

    case Assignment.Arithmetic of
      TArithmetic_Assign.Add      : AddSymbol('+');
      TArithmetic_Assign.Subtract : AddSymbol('-');
      TArithmetic_Assign.Multiply : AddSymbol('*');
      TArithmetic_Assign.Divide   : AddSymbol('/');
    end;

    AddSpace;
  end;

  DataToString(Assignment.Value,True);

// duplicate  AddSymbol(TPascalSyntax._EndOfStatement);
end;

procedure TPascalEmit.AddReturn(const AData:TData);
begin
  AddKeyword(TPascalSyntax._Reserved_Exit);

  if AData<>nil then
  begin
    AddSymbol(TPascalSyntax._BeginParameters);
    DataToString(AData,True);
    AddSymbol(TPascalSyntax._EndParameters);
  end;
end;

function ForCounterName(const AFor:TFor):String;

  function HasVariable(const ANodes:TNodes; const S:String):Boolean;
  var N : TNode;
  begin
    for N in ANodes do
        if N=AFor then
           break
        else
        if N is TVariable then
        begin
          if SameText(TVariable(N).Name,S) then
             Exit(True);
        end
        else
        if N is TFor then
          if SameText(ForCounterName(TFor(N)),S) then
             Exit(True);

    result:=False;
  end;

  function GetBlockOf(ANode:TNode):TBlockStatement;
  begin
    result:=nil;

    while ANode<>nil do
    begin
      if ANode is TBlockStatement then
         Exit(TBlockStatement(ANode))
      else
         ANode:=ANode.Owner;
    end;
  end;

  function Exists(const S:String):Boolean;
  var B : TBlockStatement;
      C : TClassType;
  begin
    B:=GetBlockOf(AFor);

    if B=nil then
       result:=False
    else
       result:=HasVariable(B.Block.Items,S);

    if not result then
    begin
      C:=TChecker.GetClassTypeOf(AFor);

      if C<>nil then
         result:=HasVariable(C.Items,S);
    end;
  end;

begin
  if AFor.Counter=nil then
     result:='t'
  else
     result:=AFor.Counter.Name;

  result:=CorrectName(result);

  if not TPascalEmit.InlineVariables then
     while Exists(result) do
           result:='_'+result;
end;

function LastItem(const ANode:TNode):TNode;
begin
  result:=ANode;

  if result is TBlockStatement then
     result:=LastItem(TBlockStatement(result).Block)
  else
  if result is TType then
  begin
    if TType(result).Items<>nil then
       result:=TType(result).Items[High(TType(result).Items)];
  end;
end;

function BeforeElse(const ANode:TNode):Boolean;
begin
  result:=(ANode.Owner is TIf) and
          (TIf(ANode.Owner).ElseBlock<>nil) and
          (LastItem(TIf(ANode.Owner).ThenBlock)=ANode);
end;

procedure TPascalEmit.AddWhen(const APrefix:String; const AWhen:TWhen);

  function UniqueName(const ANode:TNode; const AName:String):String;

    function GetName(const AValue:Integer):String;
    begin
      result:=AName+'_'+IntToStr(AValue);
    end;

    function Exists(const AValue:Integer):Boolean;
    var tmp : String;
        tmpR : TRoutine;
        N : TNode;
    begin
      tmpR:=TChecker.GetRoutineOf(ANode);

      if tmpR<>nil then
      begin
        tmp:=GetName(AValue);

        for N in tmpR.Items do
          if N=ANode then
             break
          else
          if N is TVariable then
          begin
            if SameText(TVariable(N).Name,tmp) then
               Exit(True);
          end;
      end;

      result:=False;
    end;

  var t : Integer;
  begin
    t:=1;

    while Exists(t) do
      Inc(t);

    result:=GetName(t);
  end;

var t : Integer;
    tmp : String;
    tmpData : TData;
begin
  Add(APrefix);

  if InlineVariables then
  begin
    AddKeyword(TPascalSyntax._Reserved_Var);
    AddSpace;
  end;

  tmp:=UniqueName(AWhen,'when');

  Add(AWhen.Expression,tmp);
  AddValueData(AWhen.Expression);

  AddSymbol(TPascalSyntax._EndOfStatement);
  AddLine;

  // TODO
  // var temp := AWhen.Condition
  // if temp ... then ... else
  // if temp ... then ... else
  // ...
  for t:=0 to High(AWhen.Items) do
  begin
    Add(APrefix);

    AddKeyword(TPascalSyntax._Reserved_if);
    AddSpace;

    Add(AWhen.Expression,tmp);
    AddSpace;

    tmpData:=AWhen.Items[t].Expression;

    if tmpData is TOperand then
    begin
      Operand(TOperand(tmpData));
      AddSpace;
      DataToString(TOperand(tmpData).Right,True);
    end
    else
    begin
      AddSymbol(TPascalSyntax._Symbol_Equal);
      AddSpace;
      DataToString(tmpData,False);
    end;

    AddSpace;
    AddKeyword(TPascalSyntax._Reserved_Then);
    AddLine;

    Node(APrefix+Indentation,AWhen.Items[t].Block);
    AddLine;

    if (t=High(AWhen.Items)) and (AWhen.ElseBlock=nil) then
       AddSymbol(TPascalSyntax._EndOfStatement)
    else
    begin
      Add(APrefix);
      AddKeyword(TPascalSyntax._Reserved_else);
      AddLine;
    end;
  end;

  if AWhen.ElseBlock<>nil then
  begin
    Node(APrefix+Indentation,AWhen.ElseBlock);
    AddLine;
  end;
end;

procedure TPascalEmit.Node(const APrefix:String; const ANode:TNode);

  procedure Block(const ANodes:TNodes; BeforeElse:Boolean);
  begin
    AddKeyword(TPascalSyntax._Reserved_Begin);

    if Length(ANodes)>0 then
    begin
      AddLine;
      Nodes(APrefix+Indentation,ANodes);
      Add(APrefix);
    end;

    AddKeyword(TPascalSyntax._Reserved_End);

//    if not BeforeElse then
//       AddSymbol(TPascalSyntax._EndOfStatement);

//    AddLine;
  end;

  procedure TryAdd(const ANodes:TNodes; BeforeElse:Boolean);
  begin
    if Length(ANodes)=1 then
       Node(APrefix,ANodes[0])
    else
    if Length(ANodes)>0 then
    begin
      Add(APrefix);
      Block(ANodes,BeforeElse);
    end;
  end;

  {
  procedure EmitItems(const AItems:TNodes);
  begin
    AddLine;
    Add(Prefix);
    Block(AItems);
  end;

  procedure EmitRoutine(const ARoutine:TRoutine);
  begin
    Clauses(ARoutine);
    AddRoutine(APrefix,ARoutine,True,nil);
    EmitItems(ARoutine.Items);
  end;
  }

  procedure MemberChain(const AMember:TMember);
  begin
    if AMember<>nil then
    begin
      Node(AMember.Data);
      AddSymbol(TPascalSyntax._DataMember);
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
    Add(APrefix);
    AddKeyword(TPascalSyntax._Reserved_repeat);
    AddLine;

    if ARepeat.Block<>nil then
       Node(APrefix+Indentation,ARepeat.Block);

    Add(APrefix);
    AddKeyword(TPascalSyntax._Reserved_until);
    AddSpace;
    AddData(ARepeat.Condition);
    AddSymbol(TPascalSyntax._EndOfStatement);
    AddLine;
  end;

  procedure DoTry(const ATry:TTry);
  var C : TCatch;
  begin
    Add(APrefix);
    AddKeyword(TPascalSyntax._Reserved_try);
    AddLine;

    if ATry.Block<>nil then
    begin
      Node(APrefix+Indentation,ATry.Block);
      AddLine;
    end;

    // TODO:
    // if (ATry.Catch<>nil) and (ATry.TheFinally<>nil) then
    // emit two nested try..try..except..finally..end

    if ATry.Catch<>nil then
    begin
      Add(APrefix);
      AddKeyword(TPascalSyntax._Reserved_except);
      AddLine;

      for C in ATry.Catch do
      begin
        if C.Error<>nil then
        begin
          Add(APrefix);
          AddKeyword(TPascalSyntax._Reserved_On);
          AddSpace;

          if C.Error is TVariable then
          begin
            AddName(TVariable(C.Error));
            AddSymbol(TPascalSyntax._TypeDelimiter);
            AddSpace;
            AddName(TVariable(C.Error).VariableType);
          end
          else
          if C.Error is TType then
             AddType(TType(C.Error));

          AddSpace;
          AddKeyword(TPascalSyntax._Reserved_do);
          AddSpace;

          AddLine;
        end;

        if C.Block<>nil then
        begin
          Node(APrefix+Indentation,C.Block);
          AddLine;
        end;
      end;

      AddKeyword(TPascalSyntax._Reserved_End);
      AddSymbol(TPascalSyntax._EndOfStatement);
      AddLine;
    end;

    if ATry.TheFinally<>nil then
    begin
      Add(APrefix);
      AddKeyword(TPascalSyntax._Reserved_Finally);
      AddLine;

      Node(APrefix+Indentation,ATry.TheFinally);
      AddLine;
    end;
  end;

  procedure AddForRange(const AFirst,ALast:TData);
  begin
    AddSymbol(TSyntax._Assignment);
    AddSpace;

    DataToString(AFirst,True);
    AddSpace;
    AddKeyword(TSyntax._Reserved_to);
    AddSpace;
    DataToString(ALast,True);
  end;

  procedure DoFor(const AFor:TFor);
  var tmpName : String;
  begin
    Add(APrefix);
    AddKeyword(TPascalSyntax._Reserved_for);

    tmpName:=ForCounterName(AFor);

    AddSpace;

    if InlineVariables then
    begin
      AddKeyword(TPascalSyntax._Reserved_Var);
      AddSpace;
    end;

    Add(AFor.Counter,tmpName);

    AddSpace;

    if AFor.InExpression=nil then
       AddForRange(AFor.First,AFor.Last)
    else
    if AFor.InExpression is TRange then
       AddForRange(TRange(AFor.InExpression).Min,TRange(AFor.InExpression).Max)
    else
    begin
      AddKeyword(TSyntax._Reserved_in);
      AddSpace;
      DataToString(AFor.InExpression,True);
    end;

    AddSpace;
    AddKeyword(TPascalSyntax._Reserved_do);

    AddLine;

    if AFor.Block is TBlockStatement then
       Node(APrefix,AFor.Block)
    else
       Node(APrefix+Indentation,AFor.Block)
  end;

  procedure AddWhile(const APrefix:String; const AWhile:TWhile);
  begin
    Add(APrefix);
    AddKeyword(TPascalSyntax._Reserved_while);
    AddSpace;
    AddData(AWhile.Condition);
    AddSpace;
    AddKeyword(TPascalSyntax._Reserved_do);
    AddLine;

    if AWhile.Block<>nil then
       Node(APrefix,AWhile.Block);
  end;

  procedure AddIf(const APrefix:String; const AIf:TIf);
  begin
    Add(APrefix);
    AddKeyword(TPascalSyntax._Reserved_if);
    AddSpace;

    AddData(AIf.Condition);

    AddSpace;
    AddKeyword(TPascalSyntax._Reserved_Then);
    AddLine;

    if AIf.ThenBlock<>nil then
       Node(APrefix+Indentation,AIf.ThenBlock);

    if AIf.ElseBlock<>nil then
    begin
      AddLine;
      Add(APrefix);
      AddKeyword(TPascalSyntax._Reserved_else);
      AddLine;

      if AIf.ElseBlock is TIf then
         Node(APrefix,AIf.ElseBlock)
      else
         Node(APrefix+Indentation,AIf.ElseBlock);
    end;
  end;

  function IsSys(const AModule:TType):Boolean;
  begin
    result:=Modules.SystemModule=AModule;
  end;

  procedure AddAllUses(const AItems:TNodes);
  var First : Boolean;
      Counter : Integer;

    procedure TryAdd(const AItems:TNodes);
    var N : TNode;
    begin
      for N in AItems do
          if N is TWith then
          begin
            if First then
            begin
              AddKeyword(TPascalSyntax._Reserved_Uses);
              AddLine;
              Add(Indentation);

              First:=False;
            end
            else
            begin
              AddSymbol(TPascalSyntax._ItemDelimiter);
              AddSpace;
            end;

            if Counter=5 then
            begin
              AddLine;
              Add(Indentation);
              Counter:=0;
            end;

            if IsSys(TWith(N).Module) then
               Add(TWith(N).Module,'SysUtils, Vidi') // Vidi.pas as last unit, because Float Helper
            else
               Add(TWith(N).Module,ModuleName(TWith(N).Module));

            Inc(Counter);
          end
          else
          if N is TClassType then
             TryAdd(TClassType(N).Items);
    end;

  begin
    First:=True;
    Counter:=0;

    TryAdd(AItems);

    if not First then
    begin
      AddSymbol(TPascalSyntax._EndOfStatement);
      AddLine;
      AddLine;
    end;
  end;

  procedure AddModule(const AModule:TNamedType);
  begin
    AddKeyword(TPascalSyntax._Reserved_unit);
    AddSpace;
    Add(AModule,ModuleName(AModule));
    AddSymbol(TPascalSyntax._EndOfStatement);

    AddLine;
    AddLine;

    AddKeyword(TPascalSyntax._Reserved_interface);

    AddLine;
    AddLine;

    AddAllUses(AModule.Items);

    AddInterfaceItems(APrefix,AModule.Items,nil);

    AddKeyword(TPascalSyntax._Reserved_implementation);
    AddLine;
    AddLine;

    AddImplementationItems(APrefix,AModule.Items);

    AddKeyword(TPascalSyntax._Reserved_End);
    AddSymbol(TPascalSyntax._DataMember);
  end;

  function IsMemberCount(const AMember:TMember):Boolean;
  begin
    result:=IsArrayVariable(AMember.Data);

    if result then
       result:=IsCallTo(AMember.Member,TChecker._Types[TChecker._Array],'Count');
  end;

  function AddArraySetLength(const ANode:TAssignment):Boolean;
  var tmpMember : TMember;
  begin
    result:=(ANode.Variable is TMember);

    if result then
    begin
      tmpMember:=TMember(ANode.Variable);

      result:=IsMemberCount(tmpMember);

      if result then
      begin
        Add('SetLength');
        TryParameters([tmpMember.Data,ANode.Value]);
      end;
    end;
  end;

var tmp : TType;
begin
  if ANode is TVariable then
  begin
    AddVariable(APrefix,TVariable(ANode),False);
    AddSymbol(TPascalSyntax._EndOfStatement);
  end
  else
  if ANode is TWith then
  begin
    // uses
    AddKeyword(TPascalSyntax._Reserved_with);
    AddSpace;
    AddName(TWith(ANode).Module);
  end
  else
  if ANode is TClassType then
  begin
    AddClass(APrefix,TClassType(ANode));
    AddLine;
  end
  else
  if ANode is TAssignment then
  begin
    if not AddArraySetLength(TAssignment(ANode)) then
       AddAssignment(APrefix,TAssignment(ANode),False);

    AddSymbol(TPascalSyntax._EndOfStatement);
  end
  else
  if ANode is TRange then
  begin
    AddSpace;
    RangeToString(TRange(ANode));
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
    tmp:=ParentBlock(ANode);

    if (tmp<>nil) and (Length(tmp.Items)=1) and (tmp.Items[0]=ANode) then
    begin
      if TReturn(ANode).Value is TCondition then
         AddCondition(APrefix,TCondition(TReturn(ANode).Value),True)
      else
      begin
        Add(APrefix);
        AddReturn(TReturn(ANode).Value);
      end;
    end
    else
    begin
      Add(APrefix);
      AddReturn(TReturn(ANode).Value);
    end;
  end
  else
  if ANode is TData then
     AddError('Wrong Data node: '+ANode.ClassName)
  else
  if ANode is TTypeMember then // <-- before TType
     TypeMember(TTypeMember(ANode))
  else
  if ANode is TType then
  begin
    if (ANode is TNamedType) and (ANode.Owner=nil) then
       AddModule(TNamedType(ANode))
    else
       TryAdd(TType(ANode).Items,False);
  end
  else
  if ANode is TBlockStatement then
     Node(APrefix,TBlockStatement(ANode).Block)
  else
  if ANode is TIf then
     AddIf(APrefix,TIf(ANode))
  else
  if ANode is TWhile then
     AddWhile(APrefix,TWhile(ANode))
  else
  if ANode is TRepeat then
     DoRepeat(TRepeat(ANode))
  else
  if ANode is TWhen then
     AddWhen(APrefix,TWhen(ANode))
  else
  if ANode is TBreak then
  begin
    Add(APrefix);
    AddKeyword(TPascalSyntax._Reserved_break);

    if not BeforeElse(ANode) then
       AddSymbol(TPascalSyntax._EndOfStatement);
  end
  else
  if ANode is TContinue then
  begin
    Add(APrefix);
    AddKeyword(TPascalSyntax._Reserved_continue);

    if not BeforeElse(ANode) then
       AddSymbol(TPascalSyntax._EndOfStatement);
  end
  else
  if ANode is TFor then
     DoFor(TFor(ANode))
  else
  {
  if ANode is TComment then
     AddComment(TComment(ANode).Text)
  else
  }
  if ANode is TCallData then
  begin
    AddCallData(APrefix,TCallData(ANode));
    AddSymbol(TPascalSyntax._EndOfStatement);
  end
  else
  if ANode is TTry then
     DoTry(TTry(ANode))
  else
     AddError('Cannot emit Pascal for: '+TASTUtils.NameOf(ANode,True));
end;

function HasStatement(const ANodes:TNodes):Boolean;
var N : TNode;
begin
  for N in ANodes do
      if N is TStatement then
         Exit(True);

  result:=False;
end;

procedure TPascalEmit.AddFullName(const AType:TType);
var tmp : Boolean;
    tmpS : Array of TType;
    tmpClass : TType;
    t,L : Integer;
begin
  {$IFDEF FPC}
  tmpS:=nil;
  {$ENDIF}

  SetLength(tmpS,1);
  tmpS[0]:=AType;

  tmpClass:=AType;

  repeat
    tmp:=tmpClass.Owner is TClassType;

    if tmp then
    begin
      tmpClass:=TClassType(tmpClass.Owner);

      L:=Length(tmpS);
      SetLength(tmpS,L+1);
      tmpS[L]:=tmpClass;
    end;

  until not tmp;

  for t:=High(tmpS) downto Low(tmpS) do
  begin
    Add(tmpS[t],FinalName(tmpS[t]));

    if t>Low(tmpS) then
       AddSymbol(TPascalSyntax._DataMember);
  end;
end;

procedure TPascalEmit.AddCreate(const AClass:TClassType; const AtInterface:Boolean);
begin
  AddKeyword(TPascalSyntax._Reserved_Constructor);
  AddSpace;

  if not AtInterface then
  begin
    AddFullName(AClass);
    AddSymbol(TPascalSyntax._DataMember);
  end;

  Add(TPascalSyntax._Reserved_Create);
  AddSymbol(TPascalSyntax._EndOfStatement);

  if AtInterface then
  begin
    AddSpace;

    if AClass.Ancestor=nil then
       AddKeyword(TPascalSyntax._Reserved_Virtual)
    else
       AddKeyword(TPascalSyntax._Reserved_Override);

    AddSymbol(TPascalSyntax._EndOfStatement);
  end;
end;

function HasVariableInits(const ANodes:TNodes):Boolean;

  function HasInit(const AVariable:TVariable):Boolean;
  begin
    result:=AVariable.ValueData<>nil;

    if result then
       if AVariable.Clauses.Final and TChecker.IsBasicType(AVariable.VariableType) then
          result:=False
       else
       if AVariable.Clauses.Shared then
          result:=False
  end;

var N : TNode;
begin
  for N in ANodes do
      if N is TVariable then
         if HasInit(TVariable(N)) then
            Exit(True);

  result:=False;
end;

function HasManaged(const ANodes:TNodes):Boolean;
var N : TNode;
begin
  for N in ANodes do
      if (N is TVariable) and (not TChecker.IsBasicType(TVariable(N).VariableType)) then
         Exit(True);

  result:=False;
end;

function IsEnumeration(const AClass:TClassType):Boolean;
begin
  result:=(AClass.Ancestor=nil) and TChecker.OnlyDataItems(AClass);
end;

function HasCreate(const AClass:TClassType):Boolean;
begin
  result:=(not IsEnumeration(AClass)) and
          (
            HasStatement(AClass.Items) or
            HasManaged(AClass.Items) or
            HasVariableInits(AClass.Items)
          );
end;

procedure TPascalEmit.AddClass(const APrefix:String; const AClass:TClassType);
begin
//    Clauses(AClass);

  Add(APrefix);
  AddName(AClass);   // Foo
  AddSpace;
  AddSymbol(TPascalSyntax._Symbol_Equal);  // =
  AddSpace;

  if AClass.Ancestor is TArrayType then
  begin
    DoTypeToString(AClass.Ancestor);

    {
    AddKeyword(TPascalSyntax._Reserved_Array);  // array
    AddSpace;
    AddKeyword(TPascalSyntax._Reserved_Of);  // of
    AddSpace;

    AddName(AClass.Ancestor);            //   Bar
    }
  end
  else
  if IsEnumeration(AClass) then // Enum
     TryParameters(AClass.Items)
  else
  begin
    AddKeyword(TPascalSyntax._Reserved_Class);  // class

    if AClass.Clauses.Final then
    begin
      AddSpace;
      AddKeyword(TPascalSyntax._Reserved_Sealed);  // sealed
    end;

    if AClass.Parameters<>nil then
       DeclaringParameters(AClass.Parameters);

    if AClass.Ancestor<>nil then
    begin
      AddSymbol(TPascalSyntax._BeginParameters);  // (
      AddFullName(AClass.Ancestor);    //   Bar

      //DoTypeToString(AClass.Ancestor);            //   Bar

      AddSymbol(TPascalSyntax._EndParameters);    // )
    end;

    GetTypeIndexes(AClass);

    AddLine;
    AddInterfaceItems(APrefix+Indentation,AClass.Items,AClass);

    if HasCreate(AClass) then
    begin
      AddLine;
      Add(APrefix+Indentation);
      AddCreate(AClass,True);
      AddLine;
    end;

    Add(APrefix);
    AddKeyword(TPascalSyntax._Reserved_End);
  end;

  AddSymbol(TPascalSyntax._EndOfStatement);

  AddLine;
  AddLine;
end;

procedure TPascalEmit.AddCallData(const APrefix:String; const ACall:TCallData);
begin
  Add(APrefix);
  DataToString(ACall.Value,True);

//  AddSymbol(TPascalSyntax._EndOfStatement);
end;

procedure TPascalEmit.AddImplementation(const APrefix:String; const ANode:TNode);
begin
  if ANode is TVariable then
     AddVariable(APrefix,TVariable(ANode),False)
  else
  if ANode is TStatement then
     Node(APrefix,ANode)
  else
  if ANode is TClassType then
     AddImplementationClass(APrefix,TClassType(ANode))
  else
  if ANode is TWith then
     Exit
  else
  if ANode is TRoutine then
     AddRoutine(APrefix,TRoutine(ANode),False,nil)
  else
  if ANode is TExtender then
     if TExtender(ANode).Extension is TRoutine then
        AddRoutine(APrefix,TExtender(ANode).Extension as TRoutine,False,nil) // Pending
     else
        AddImplementationClass(APrefix,TExtender(ANode).Extension as TClassType)
  else
     AddError('Node not supported at implementation: '+ANode.ClassName);

  AddLine;
end;

procedure TPascalEmit.AddImplementationClassNodes(const APrefix:String;
                                     const AClass:TClassType;
                                     const AItems:TNodes);
var N : TNode;
begin
  for N in AItems do
      if (N is TRoutine) and (not TChecker.IsAbstract(TRoutine(N))) then
      begin
        AddRoutine(APrefix,TRoutine(N),False,AClass);
        AddLine;
      end
      else
      if N is TClassType then
         AddImplementationClass(APrefix,TClassType(N))
//      else
//      if (N is TVariable) and TChecker.IsBasicType(TVariable(N).VariableType) then
//      else
//         AddImplementation(APrefix+Indentation,N);
end;

procedure TPascalEmit.AddImplementationClass(const APrefix:String; const AClass:TClassType);

  procedure AddVariableInit(const AVariable:TVariable);
  begin
    // TODO : Optimize default basic types, eg:
    // "Bool:=False" <-- DO NOT EMIT, IT IS NOT NECESSARY

    if AVariable.ValueData is TCondition then
       AddAssignCondition(APrefix+Indentation,TCondition(AVariable.ValueData),AVariable)
    else
    begin
      Add(APrefix+Indentation);

      AddName(AVariable);

      AddSpace;
      AddSymbol(TPascalSyntax._Assignment);
      AddSpace;

      DataToString(AVariable.ValueData,True);
    end;

    AddSymbol(TPascalSyntax._EndOfStatement);
    AddLine;
  end;

  procedure AddCreateItems;
  var N : TNode;
  begin
    for N in AClass.Items do
        if (N is TRoutine) or (N is TClassType) then
        else
        if (N is TVariable) and TChecker.IsBasicType(TVariable(N).VariableType) then
        begin
          if TVariable(N).ValueData<>nil then
             if not TVariable(N).Clauses.Final then // constants
                if not TVariable(N).Clauses.Shared then // class variables
                   AddVariableInit(TVariable(N))
        end
        else
        if not (N is TVariable) then
           AddImplementation(APrefix+Indentation,N);
  end;

var tmp : Boolean;
begin
  tmp:=HasCreate(AClass);

  if tmp then
  begin
    AddCreate(AClass,False);
    AddLine;
    AddKeyword(TPascalSyntax._Reserved_Begin);
    AddLine;

    Add(APrefix+Indentation);

    AddKeyword(TPascalSyntax._Reserved_Inherited);
    AddSymbol(TPascalSyntax._EndOfStatement);
    AddLine;

    AddCreateItems;

    AddKeyword(TPascalSyntax._Reserved_End);
    AddSymbol(TPascalSyntax._EndOfStatement);
    AddLine;
    AddLine;
  end;

  AddImplementationClassNodes(APrefix,AClass,AClass.Items);
end;

procedure TPascalEmit.AddImplementationItems(const APrefix:String; const AItems:TNodes);

  procedure AddClassVariableInit(const APrefix:String; const AClass:TClassType; const AVariable:TVariable);
  begin
    Add(APrefix);

    AddName(AClass);
    AddSymbol(TPascalSyntax._DataMember);
    AddName(AVariable);

    AddSpace;
    AddSymbol(TPascalSyntax._Assignment);
    AddSpace;

    DataToString(AVariable.ValueData,True);

    AddSymbol(TPascalSyntax._EndOfStatement);
    AddLine;
  end;

  procedure AddClassVarInits(const AItems:TNodes);

    procedure TryAdd(const AClass:TClassType);
    var N : TNode;
    begin
      for N in AClass.Items do
          if N is TVariable then
             if TVariable(N).Clauses.Shared and (TVariable(N).ValueData<>nil) then
                AddClassVariableInit(APrefix+Indentation,AClass,TVariable(N));
    end;

  var N : TNode;
  begin
    for N in AItems do
        if N is TClassType then
           TryAdd(TClassType(N));
  end;

  procedure TryNodes(const ANodes:TNodes);
  begin
    if Length(ANodes)>0 then
    begin
      Nodes(APrefix+Indentation,ANodes);
      //Add(APrefix);
    end;
  end;

  procedure TryFrees;
  var t : Integer;
  begin
    for t:=High(ToFree) downto Low(ToFree) do
    begin
      Add(APrefix+Indentation);
      AddName(ToFree[t]);
      AddSymbol(TPascalSyntax._DataMember);
      Add('Free');
      AddSymbol(TPascalSyntax._EndOfStatement);
      AddLine;
    end;

    ToFree:=nil;
  end;

var N : TNode;
begin
  for N in AItems do
      if N is TWith then
      else
      //if N is TComment then
      //else
      if N is TClassType then
         AddImplementationClass(APrefix,TClassType(N))
      else
      if N is TBlockStatement then
      begin
        if not InlineVariables then
           AddLocals(APrefix,TBlockStatement(N).Block);

        //TypeAliasesPos:=Length(Emit.GetText);

        AddKeyword(TPascalSyntax._Reserved_initialization);
        AddLine;

        AddClassVarInits(AItems);

        TryNodes(TBlockStatement(N).Block.Items);

        TryFrees;

        AddKeyword(TPascalSyntax._Reserved_finalization);
        AddLine;
      end
      else
      if not (N is TVariable) then
        AddImplementation(APrefix,N);
end;

procedure TPascalEmit.AddLocals(const APrefix:String; const ANode:TNode);
var FirstVar : Boolean;

  procedure TryAdd(const AName:String; const ANode:TNode; const AType:TType);
  var t : Integer;
  begin
    if FirstVar then
    begin
      Add(APrefix);
      AddKeyword(TPascalSyntax._Reserved_Var);
      AddSpace;
      FirstVar:=False;
    end
    else
    begin
      Add(APrefix);

      for t:=1 to Length(TPascalSyntax._Reserved_Var)+1 do
          AddSpace;
    end;

    if AName='' then
       AddName(ANode)
    else
       Add(ANode,AName);

    AddSpace;
    AddType(AType);

    AddSymbol(TPascalSyntax._EndOfStatement);
    AddLine;
  end;

  procedure DoAdd(const ANode:TNode);

    procedure DoAddLocals(const AItems:TNodes);
    var N : TNode;
        tmp : TType;
    begin
      for N in AItems do
          if N is TVariable then
             TryAdd('',N,TVariable(N).VariableType)
          else
          if N is TFor then
          begin
            if TFor(N).Counter=nil then
               tmp:=TChecker._Types[TChecker._Integer] // Integer
            else
               tmp:=TFor(N).Counter.VariableType;

            TryAdd(ForCounterName(TFor(N)),TFor(N).Counter,tmp);

            DoAdd(TFor(N).Block);
          end;
    end;

  begin
    if ANode is TType then
       DoAddLocals(TType(ANode).Items)
    else
    if ANode is TBlockStatement then
       DoAdd(TBlockStatement(ANode).Block)
  end;

begin
  FirstVar:=True;
  DoAdd(ANode);
end;

procedure TPascalEmit.AddRoutine(const APrefix:String; const R:TRoutine; const IsDeclaration:Boolean; const AClass:TClassType);

  function IsOverload:Boolean;
  var C : TClassType;
      N : TNode;
  begin
    C:=TChecker.GetClassTypeOf(R);

    if C<>nil then
    for N in C.Items do
        if N<>R then
           if (N is TRoutine) and SameText(TRoutine(N).Name,R.Name) then
              Exit(True);

    result:=False;
  end;

  function IsClassRoutine:Boolean;
  begin
    result:=R.Clauses.Shared and (AClass<>nil) and (R.Items<>nil) and (R.Ancestor=nil);
  end;

begin
  Add(APrefix);

  AddClauses(R.Clauses,True);

  if IsClassRoutine then
  begin
    AddKeyword(TPascalSyntax._Reserved_Class);
    AddSpace;
  end;

  // TODO:  Other Clauses?

  if R.Output=nil then
     AddKeyword(TPascalSyntax._reserved_procedure)
  else
     AddKeyword(TPascalSyntax._reserved_function);

  AddSpace;

  if (not IsDeclaration) and (AClass<>nil) then
  begin
    AddFullName(AClass);
    AddSymbol(TPascalSyntax._DataMember);
  end;

  AddName(R);

  if R.Parameters<>nil then
     DeclaringParameters(R.Parameters);

  if R.Output<>nil then
     AddType(R.Output);

  AddSymbol(TPascalSyntax._EndOfStatement);

  if IsDeclaration then
  begin
    if R.Ancestor<>nil then
    begin
      AddSpace;
      AddKeyword(TPascalSyntax._Reserved_Override);
      AddSymbol(TPascalSyntax._EndOfStatement);
    end
    else
    if IsOverload then
    begin
      AddSpace;
      AddKeyword(TPascalSyntax._Reserved_Overload);
      AddSymbol(TPascalSyntax._EndOfStatement);
    end;

    if IsClassRoutine then
    begin
      AddSpace;
      AddKeyword(TPascalSyntax._Reserved_Static);
      AddSymbol(TPascalSyntax._EndOfStatement);
    end
    else
    if TChecker.IsAbstract(R) then
    begin
      AddSpace;
      AddKeyword(TPascalSyntax._Reserved_virtual);
      AddSymbol(TPascalSyntax._EndOfStatement);

      AddSpace;
      AddKeyword(TPascalSyntax._Reserved_abstract);
      AddSymbol(TPascalSyntax._EndOfStatement);
    end;
  end;

  if not IsDeclaration then
  begin
    AddLine;

    if not InlineVariables then
       AddLocals(APrefix,R);

    Add(APrefix);
    AddKeyword(TPascalSyntax._Reserved_Begin);
    AddLine;

    Nodes(APrefix+Indentation,R.Items);

    Add(APrefix);
    AddKeyword(TPascalSyntax._Reserved_End);
    AddSymbol(TPascalSyntax._EndOfStatement);
    AddLine;
  end;
end;

procedure TPascalEmit.AddTypeClause(const APrefix:String);
begin
  Add(APrefix);
  AddKeyword(TPascalSyntax._Reserved_Type);
  AddLine;
end;

procedure TPascalEmit.AddInterfaceItems(const APrefix:String; const AItems:TNodes; const AClass:TClassType);
var tmpVar,
    tmpType : Boolean;

  procedure SetType;
  begin
    if not tmpType then
    begin
      AddTypeClause(APrefix);

      tmpType:=True;
      tmpVar:=False;
    end;
  end;

  procedure AddInterfaceItem(const N:TNode);
  begin
    {$IFDEF INTERNAL}
    LastEmitPascal:=N;
    {$ENDIF}

    if N is TClassType then
    begin
      SetType;
      AddClass(APrefix+Indentation,TClassType(N));
    end
    else
    if N is TVariable then
    begin
      if not tmpVar then
      begin
        Add(APrefix);
        AddKeyword(TPascalSyntax._Reserved_Var);
        tmpVar:=True;
        tmpType:=False;

        AddLine;
      end;

      AddVariable(APrefix,TVariable(N),True);
      AddLine;
    end
    else
    if N is TRoutine then
    begin
      tmpType:=False;
      tmpVar:=False;

      AddRoutine(APrefix,TRoutine(N),True,AClass);
      AddLine;
    end
    else
    if N is TExtender then
    begin
      if TExtender(N).Extension is TRoutine then
      begin
        tmpType:=False;
        tmpVar:=False;

        AddRoutine(APrefix,TExtender(N).Extension as TRoutine,True,nil);
      end
      else
      begin
        SetType;

        AddClass(APrefix+Indentation,TExtender(N).Extension as TClassType);
      end;

      AddLine;
    end
    else
    // not at interface section
    if (N is TWith) or
       (N is TStatement) or
       //(N is TComment) or
       (N is TWhen) then
    else
       AddError('Node not supported at interface: '+N.ClassName);
  end;

var N : TNode;
begin
  tmpType:=False;
  tmpVar:=False;

  AddArrayTypes(APrefix,AItems);

  for N in AItems do
      AddInterfaceItem(N);
end;

procedure TPascalEmit.Node(const ANode:TNode);
begin
  Node('',ANode);
end;

// Helper method, useful for debugging
function P(const ANode:TNode):String;
var Emit : TPascalEmit;
begin
  Emit:=TPascalEmit.Create;
  try
    result:=Emit.AsString(ANode);
  finally
    Emit.Free;
  end;
end;

initialization
  TPascalEmit.InlineVariables:=True;
  TPascalEmit.RaiseErrors:=True;
  TPascalEmit.Indentation:='  ';
end.
