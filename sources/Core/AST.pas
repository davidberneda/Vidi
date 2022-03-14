unit AST;

interface

uses
  Sys;

type
  TNodes=Array of TNode;

  { TNodesHelper }

  TNodesHelper=record helper for TNodes
  public
    procedure Add(const ANode:TNode); inline;
    function Count:Integer; inline;
//    procedure Delete(const ANode:TNode); overload;
//    procedure Delete(const AIndex:Integer); overload;
    procedure Free(const AOwner:TNode);
    function IndexOf(const ANode:TNode):Integer;
  end;

  // Expression

  TGroup=class(TData)
  public
    Expression : TData;
    Destructor Destroy; override;

//    function Value:TData; override;
  end;

  Float=Extended;

  TOperand=class(TData)
  public
    Left,
    Right : TData;

    Destructor Destroy; override;
  end;

  TLogical=class(TOperand)
  public
    // function Join : TLogical;
  end;

  TLogicalAnd=class(TLogical)
  end;

  TLogicalOr=class(TLogical)
  end;

  TLogicalXor=class(TLogical)
  end;

  TCompareLogical=class(TLogical)
  end;

  TIsEqual=class(TCompareLogical)
  end;

  TIsNotEqual=class(TCompareLogical)
  end;

  TIsLower=class(TCompareLogical)
  end;

  TIsLowerOrEqual=class(TCompareLogical)
  end;

  TContains=class(TCompareLogical)
  end;

  TIsGreater=class(TCompareLogical)
  end;

  TIsGreaterOrEqual=class(TCompareLogical)
  end;

  TArithmetic=class(TOperand)
  end;

  TAddition=class(TArithmetic)
  end;

  TSubtraction=class(TArithmetic)
  end;

  TMultiplication=class(TArithmetic)
  end;

  TDivision=class(TArithmetic)
  end;

  // A ? B : C
  TCondition=class(TLogical)
  public
    Condition : TData;
    Destructor Destroy; override;
  end;

  TUnary=class abstract (TGroup)
  end;

  TUnarySign=class(TUnary)
  public
    Positive : Boolean;
  end;

  TUnaryNot=class(TUnary)
  end;

  TExpressionData=class abstract (TData)
  public
    Parameters : TNodes;

    Destructor Destroy; override;
  end;

  // Foo[Bar]
  TArrayExpression=class(TExpressionData) // Bar = Parameters (dimensions)
  public
    Data : TData;  // Foo

    {$IFDEF INTERNAL}
    Constructor Create;
    {$ENDIF}

    Destructor Destroy; override;

    //function Value:TData; override;
  end;

  // Parser

  {
  // Deprecated, comments should not be preserved / streamed / transpiled
  TComment=class(TNode)
  public
    Text : String;
  end;
  }

  TClauses=record
    Indexed,
    Hidden,
    Shared,
    Final : Boolean;

    function EqualsTo(const AClauses:TClauses):Boolean;
    procedure Reset;
  end;

  TNodeClass=class of TNode;

  TType=class(TNode)
  public
    Clauses : TClauses;
    Items : TNodes; // All Fields, methods, sub-types etc

    Plugin : TNodeClass;  // non-streamable

    class function NewType(const AOwner:TNode):TType; static;

    Destructor Destroy; override;
  end;

  TNamedType=class(TType)
  public
    Name : String;
  end;

  // 0..999  -123..456  1..3*4 etc
  TRange=class(TData)
  public
    Min,
    Max : TData;

    Destructor Destroy; override;

    //function Value:TData; override;
  end;

  // Foo:0..255
  TRangeType=class(TType)
  public
    Range : TRange;

    Destructor Destroy; override;
  end;

  // X:Type.Of(My_BaseType):=My_DerivedType
  // TMetadata=class(Type)
  // public
  //  TheType : TType;
  // end;

  // X:Sys.Integer
  TTypeMember=class(TType)
  public
    TheType,
    Member : TType;

    Destructor Destroy; override;
  end;

  {
  // TODO: set of XXX (ordered array with no duplicates)
  TSet=class(TType)
  public
    OfType : TType;
  end;
  }

  // Foo:Bar:=123
  TVariable=class(TData)
  public
    Clauses : TClauses;
    Initialized : Boolean;
    Name : String;
    TypeInferred : Boolean;    // X::=
    ValueData : TData;
    VariableType : TType;  // ie: Integer  // ie: Array

    Destructor Destroy; override;
    //function Value : TData; override;
  end;

  // X:T
  TGenericType=class(TType)
  public
    Variable : TVariable;
    FinalType : TType; // reificated
  end;

  TParametersType=class(TNamedType)
  public
    Parameters : TNodes; // routine/class parameters

    destructor Destroy; override;
  end;

  // Array(Text)
  TSpecializedType=class(TType)
  public
    TheType : TParametersType;
    Generics : TNodes;

    Destructor Destroy; override;
  end;

  // class/record/struct Foo {}
  TClassType=class(TParametersType)
  public
    Ancestor : TType;

    Destructor Destroy; override;
    function DefaultIndexed:TNode;
  end;

  // class/record Foo is 1..100
  TIntegerClass=class(TClassType)
  public
    Size : TRangeType; // 1..100     123..xyz

    Destructor Destroy; override;
  end;

  // Foo:Bar[]  Foo:Bar[0..9]  Foo:Bar[Enum] Foo:Bar[3][4+max][5]  Foo:Bar[3,4,1..5*2]
  TArrayType=class(TType)
  public
    TheType : TType;
    Size : TData; // TRange, TInteger, expression etc;
    Values : TNodes;

    Destructor Destroy; override;
  end;

  // Foo(D:Data...)   -->  Foo([1,2,3]) = Foo(1,2,3)
  TManyValues=class(TArrayType)
  end;

  TPluginHook=function(const Instance,Parameters:TNode):TData;

  // Foo {}
  TRoutine=class(TParametersType)
  public
    Ancestor : TRoutine; // optional override
    Output : TType;    // optional

    AccessGlobals : Boolean;

    ForwardFrom,
    ForwardTo : TRoutine;

    Hook : TPluginHook; // non-streamable

    Destructor Destroy; override;
  end;

  // FooProc is (A,B,C:Integer):Text {}  <-- function type reference declaration
  TFunctionType=class(TRoutine)
  end;

  // with ast, run
  TWith=class(TNode)
  public
    Module : TNamedType;
    Alias : String;  // with MyModule.MyClass is Foo  <-- alias Foo

    SystemModule : Boolean;

    // TODO: Block : TBlockStatement ? (restrict With scope to a block)  with Foo { a:xyz }
  end;

  // call Foo() used in expression
  TDataCall=class(TExpressionData)
  public
    Routine : TType; // can be: TRoutine, TTypeMember, Variable of TFunctionType, etc
    ResultValue : TData;

    Destructor Destroy; override;

    //function Value: TData; override;
  end;

  // AFindProc() calling variable, because it is a FunctionType
  TVariableCall=class(TDataCall)
  public
    Variable : TVariable;
  end;

  // Ancestor...
  TAncestor=class(TData)
  public
    Ancestor : TType;
    DataCall : TDataCall;

    Destructor Destroy; override;
  end;

  // call a class method MyType.Foo() used in expression (also as CastingData)
  TTypeCall=class(TExpressionData) // TData better?
  public
    TheType : TType;

    Constructor CreateNew(const AOwner:TNode);
    Destructor Destroy; override;

    //function Value: TData; override;
  end;

  // casting a type to another
  TCastingData=class(TData)
  public
    TheType : TTypeCall;
    Data : TData;

    Destructor Destroy; override;

//    function Value: TData; override;
  end;

  // Foo.Bar
  TMember=class(TData)
  public
    Data : TData;
    Member : TData;

    Destructor Destroy; override;

//    function Value: TData; override;
  end;

  // Self.xxx
  TSelf=class(TData)
  public
    TheType : TType;
  end;

  // abstract
  TStatement=class(TNode)
  end;

  // { }
  TBlockStatement=class(TStatement)
  public
    Block : TType;

    Constructor Create;
    Destructor Destroy; override;
  end;

  // Array.Sort(Ascending:Boolean) { ... }
  // Array.SubClass { ... }
  TExtender=class(TNamedType)
  public
    TheType : TType; // The Type we are extending (eg: Array, or subtype Foo.Bar...)

    // The extension on Type (eg: Sort method or SubClass class)
    Extension : TNamedType;

    Destructor Destroy; override;
  end;

  (*
  // Foo(A,B,C)
  TCall=class(TStatement)
  public
    Routine : TRoutine;   // Foo
    Parameters : TNodes;  // A,B,C

    Destructor Destroy; override;
  end;
  *)

  TArithmetic_Assign=(None,Add,Subtract,Multiply,Divide);

  // Foo:=Bar    Foo.Bar:=Lee
  TAssignment=class(TStatement)
  public
    Arithmetic : TArithmetic_Assign; // optional: += -= *= /=
    Variable : TData;
    Value : TData;

    Destructor Destroy; override;
  end;

  // A.B.C.Foo()   <-- expression to call
  TCallData=class(TStatement)
  public
    Value : TData;

    Destructor Destroy; override;
  end;

  // return Foo+Bar
  TReturn=class(TStatement)
  public
    Value : TData;

    Destructor Destroy; override;
  end;

  TLogicalStatement=class abstract(TStatement)
  public
    Condition : TData; // Logical expression

    Destructor Destroy; override;
  end;

  // if
  TIf=class(TLogicalStatement)
  public
    ThenBlock,
    ElseBlock : TStatement;

    Destructor Destroy; override;
  end;

  // abstract for TWhile and TRepeat
  TLoopStatement=class(TLogicalStatement)
  public
    Block : TStatement;

    Destructor Destroy; override;
  end;

  // while
  TWhile=class(TLoopStatement)
  end;

  // repeat
  TRepeat=class(TLoopStatement)
  end;

  // for
  TFor=class(TStatement)
  public
    Counter : TVariable;
    First, Last{, Step} : TData;  // for a::0 to 999 ...

    InExpression : TData;  // for Item in Items ...

    Block : TStatement;

    Destructor Destroy; override;
  end;

  TBreak=class(TStatement)
  end;

  TContinue=class(TStatement)
  end;

  TWhenItem=class(TStatement)
  public
    Expression: TData;
    Block : TStatement;

    Destructor Destroy; override;
  end;

  TWhenItems=Array of TWhenItem;

  // when Foo { 3 {}  5 {}  >10 {} else {} }
  TWhen=class(TStatement)
  public
    Expression : TData;
    Items : TWhenItems;
    ElseBlock : TStatement;

    function AddItem:TWhenItem;
    Destructor Destroy; override;
  end;

  TCatch=class(TStatement)
  public
    Error : TNode;  // type or variable  catch MyError {}  catch X:MyError {}
    Block : TStatement;

    Destructor Destroy; override;
  end;

  TTry=class(TStatement)
  public
    Block : TStatement;
    Catch : Array of TCatch;  // catch XYZ {} catch Foo {} ...
    TheFinally : TStatement;

    Destructor Destroy; override;
  end;

implementation

{$IFDEF NODEPOOL}
uses Node_Pool;
{$ENDIF}

procedure TNodesHelper.Add(const ANode:TNode);
var L : Integer;
begin
  L:=Length(Self);
  SetLength(Self,L+1);

  Self[L]:=ANode;
end;

{
procedure TNodesHelper.Delete(const ANode:TNode);
var t : Integer;
begin
  for t:=Low(Self) to High(Self) do
      if Self[t]=ANode then
      begin
        Delete(t);
        break;
      end;
end;
}

function TNodesHelper.Count: Integer;
begin
  result:=Length(Self);
end;

(*
procedure TNodesHelper.Delete(const AIndex: Integer);
{$IFDEF FPC}var t : Integer;{$ENDIF}
begin
  {$IFDEF FPC}
  for t:=AIndex to High(Self)-1 do Self[t]:=Self[t+1];
  SetLength(Self,High(Self));
  {$ELSE}
  System.Delete(Self,AIndex,1);
  {$ENDIF}
end;
*)
procedure TryFree(const AOwner,ANode:TNode);
begin
  if ANode<>nil then
     if ANode.Owner=AOwner then
        ANode.Free;
end;

procedure TNodesHelper.Free(const AOwner:TNode);
var t : Integer;
begin
  for t:=High(Self) downto Low(Self) do
      if (Self[t]<>nil) and (Self[t].Owner=AOwner) then
         Self[t].Free;

  Self:=nil;
end;

function TNodesHelper.IndexOf(const ANode: TNode): Integer;
var t : Integer;
begin
  for t:=Low(Self) to High(Self) do
      if Self[t]=ANode then
         Exit(t);

  result:=-1;
end;

{ TClauses }

function TClauses.EqualsTo(const AClauses: TClauses): Boolean;
begin
  result:=(Indexed=AClauses.Indexed) and
          (Hidden=AClauses.Hidden) and
          (Shared=AClauses.Shared) and
          (Final=AClauses.Final);
end;

procedure TClauses.Reset;
begin
  Indexed:=False;
  Hidden:=False;
  Shared:=False;
  Final:=False;
end;

{ TOperand }

Destructor TOperand.Destroy;
begin
  TryFree(Self,Left);
  TryFree(Self,Right);

  inherited;
end;

{ TType }

Destructor TType.Destroy;
begin
  if Items<>nil then
     Items.Free(Self);

  inherited;
end;

{ TClassType }

// indexed Items[foo]
function TClassType.DefaultIndexed: TNode;
var N : TNode;
begin
  for N in Items do
      // TODO: Allow other "indexed" items, not only variables (ie: methods)
      if N is TVariable then
         if TVariable(N).Clauses.Indexed then
            if TVariable(N).VariableType is TArrayType then
               Exit(N);

  result:=nil;
end;

class function TType.NewType(const AOwner: TNode): TType;
begin
  result:=TType.Create;
  result.Owner:=AOwner;
end;

(*
{ TCall }

Destructor TCall.Destroy;
begin
  if Parameters<>nil then
     Parameters.Free(Self);

  inherited;
end;
*)

{ TVariable }

destructor TVariable.Destroy;
begin
  {$IFDEF NODEPOOL}
//  NodePool.Remove(Self);
  {$ENDIF}

  TryFree(Self,ValueData);
  TryFree(Self,VariableType);

  inherited;
end;

{
function TVariable.Value: TData;
begin
  result:=ValueData;
end;
}

{ TExpressionData }

destructor TExpressionData.Destroy;
begin
  if Parameters<>nil then
     Parameters.Free(Self);

  inherited;
end;

{ TCondition }

destructor TCondition.Destroy;
begin
  TryFree(Self,Condition);

  inherited;
end;

{ TGroup }

Destructor TGroup.Destroy;
begin
  TryFree(Self,Expression);
  inherited;
end;

{
function TGroup.Value: TData;
begin
  result:=Expression;
end;
}

{ TIf }

Destructor TIf.Destroy;
begin
  ThenBlock.Free;
  ElseBlock.Free;

  inherited;
end;

{ TLoopStatement }

Destructor TLoopStatement.Destroy;
begin
  Block.Free;

  inherited;
end;

{ TDataCall }

destructor TDataCall.Destroy;
begin
  TryFree(Self,Routine);
  ResultValue.Free;

  {$IFDEF NODEPOOL}
//  NodePool.Remove(Self);
  {$ENDIF}

  inherited;
end;

{
function TDataCall.Value: TData;
begin
  result:=ResultValue.Value;
end;
}

{ TArrayExpression }

{$IFDEF INTERNAL}
Constructor TArrayExpression.Create;
begin
  inherited Create;
end;
{$ENDIF}

Destructor TArrayExpression.Destroy;
begin
  TryFree(Self,Data);
  inherited;
end;

{
function TArrayExpression.Value: TData;
begin
  result:=Data;
end;
}

{ TCastingData }

destructor TCastingData.Destroy;
begin
  TryFree(Self,TheType);
  TryFree(Self,Data);
  inherited;
end;

{
function TCastingData.Value: TData;
begin
  result:=Data;
end;
}

{ TMember }

destructor TMember.Destroy;
begin
  TryFree(Self,Data);
  TryFree(Self,Member);

  inherited;
end;

{
function TMember.Value: TData;
begin
  result:=Member;
end;
}

{ TRange }

destructor TRange.Destroy;
begin
  TryFree(Self,Max);
  TryFree(Self,Min);

  inherited;
end;

{
function TRange.Value: TData;
begin
  result:=Min; // ?
end;
}

{ TTypeCall }

constructor TTypeCall.CreateNew(const AOwner: TNode);
begin
  inherited Create;
  Owner:=AOwner;

  {$IFDEF NODEPOOL}
//  NodePool.Add(Self);
  {$ENDIF}
end;

destructor TTypeCall.Destroy;
begin
  {$IFDEF NODEPOOL}
//  NodePool.Remove(Self);
  {$ENDIF}

  TryFree(Self,TheType); // <-- lambda functions
  inherited;
end;

{
function TTypeCall.Value: TData;
begin
  result:=nil // ??
end;
}

{ TRangeType }

destructor TRangeType.Destroy;
begin
  Range.Free;
  inherited;
end;

{ TArrayType }

destructor TArrayType.Destroy;
begin
  TryFree(Self,Size);
  TryFree(Self,TheType);

  if Values<>nil then
     Values.Free(Self);

  inherited;
end;

{ TReturn }

destructor TReturn.Destroy;
begin
  TryFree(Self,Value);

  inherited;
end;

{ TSpecializedType }

destructor TSpecializedType.Destroy;
begin
  {$IFDEF NODEPOOL}
//  NodePool.Remove(Self);
  {$ENDIF}

  if Generics<>nil then
     Generics.Free(Self);

  inherited;
end;

{ TParametersType }

destructor TParametersType.Destroy;
begin
  if Parameters<>nil then
     Parameters.Free(Self);

  inherited;
end;

{ TClassType }

destructor TClassType.Destroy;
begin
  TryFree(Self,Ancestor);
  inherited;
end;

{ TIntegerClass }

destructor TIntegerClass.Destroy;
begin
  Size.Free;
  inherited;
end;

{ TAncestor }

destructor TAncestor.Destroy;
begin
  DataCall.Free;
  inherited;
end;

{ TBlockStatement }

constructor TBlockStatement.Create;
begin
  inherited;

  Block:=TType.NewType(Self);
end;

destructor TBlockStatement.Destroy;
begin
  Block.Free;
  inherited;
end;

{ TAssignment }

destructor TAssignment.Destroy;
begin
  TryFree(Self,Variable);
  TryFree(Self,Value);

  inherited;
end;

{ TCallData }

destructor TCallData.Destroy;
begin
  TryFree(Self,Value);
  inherited;
end;

{ TLogicalStatement }

destructor TLogicalStatement.Destroy;
begin
  TryFree(Self,Condition);

  inherited;
end;

{ TFor }

destructor TFor.Destroy;
begin
  Block.Free;

  TryFree(Self,Counter); // TODO: Counter should always be owned by TFor !

  TryFree(Self,First);
  TryFree(Self,Last);
  TryFree(Self,InExpression);

  inherited;
end;

{ TWhenItem }

destructor TWhenItem.Destroy;
begin
  Block.Free;
  TryFree(Self,Expression);

  inherited;
end;

{ TWhen }

function TWhen.AddItem: TWhenItem;
var L : Integer;
begin
  result:=TWhenItem.Create;

  L:=Length(Items);
  SetLength(Items,L+1);
  Items[L]:=result;
end;

destructor TWhen.Destroy;
var W : TWhenItem;
begin
  TryFree(Self,Expression);

  for W in Items do
      W.Free;

  ElseBlock.Free;

  inherited;
end;

{ TRoutine }

destructor TRoutine.Destroy;
begin
  TryFree(Self,Output);
  inherited;
end;

{ TExtender }

destructor TExtender.Destroy;
begin
  TryFree(Self,TheType);
  Extension.Free;
  inherited;
end;

{ TCatch }

destructor TCatch.Destroy;
begin
  TryFree(Self,Error);
  Block.Free;
  inherited;
end;

{ TTry }

destructor TTry.Destroy;
var C : TCatch;
begin
  Block.Free;

  for C in Catch  do
      C.Free;

  TheFinally.Free;

  inherited;
end;

{ TTypeMember }

destructor TTypeMember.Destroy;
begin
  TryFree(Self,TheType);
  TryFree(Self,Member);
  inherited;
end;

end.
