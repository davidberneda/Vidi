unit Expression.Parser;
{$IFDEF NEXTGEN}
{$ZEROBASEDSTRINGS OFF}
{$ENDIF}

{$DEFINE EXTRA_CHECKS}

interface

uses
  Sys, AST, Parser, Checker.AST, Find.AST,

  {$IFDEF NODEPOOL}
  Node_Pool,
  {$ENDIF}

  Exceptions, Map, Position, AST.Parser, Syntax;

type
  TOnError=procedure(const Sender:TParser; const AError:TNodeError) of object;

  TExpressionParser=class(TASTParser)
  private
    procedure CheckRangeIsAscending(const ARange:TRange);

    {$IFDEF EXTRA_CHECKS}
    procedure CheckUselessOperand(const AOperand:TOperand);
    {$ENDIF}

    function CreateCondition(const AParent,AOwner:TNode; const ACondition:TData):TCondition;
    function CreateTypeCall(const AOwner:TNode; const AType:TType; const AName:String):TTypeCall;
    function CreateUnarySign(const AParent,AOwner:TNode; const Positive:Boolean):TUnarySign;

    function DirectCreateDataCall(const AOwner:TNode;
                                  const ARoutine:TRoutine;
                                  const AddRoutine:Boolean):TDataCall;

    function DoCreateCall(const AOwner:TNode; const ARoutine:TRoutine; const AddRoutine:Boolean):TDataCall;
    function DoGetExpression(const AParent,AOwner:TNode; const DoError:Boolean=True):TData;

    function FindExtenderData(const AParent:TNode; AOwner:TNode; const AType:TType;
                              const AName:String; TopDown:Boolean):TData;

    procedure FinishDataCall(const ACall:TDataCall;
                             const ARoutine:TType;
                             const AddRoutine:Boolean);

    function GetArrayExpression(const AParent,AOwner:TNode;
                                const AData:TData;
                                const Expressions:Boolean;
                                const APosition:TPosition):TArrayExpression;

    function GetDataTypeArray(const AData:TArrayExpression):TType;
    function GetOperand{(out AText:String; out AStyle:TPositionStyle)}:TOperand;

    function NewManyValuesType(const AOwner:TNode; const AType:TType):TManyValues;

    procedure SetExpressionDataParameters(const AOwner:TNode; const AData:TExpressionData; const AParameters:TNodes);

  protected
    procedure CheckParameterTypes(const AParameters:TNodes);

    procedure Clear;

    function CreateDataCall(const AOwner:TNode;
                            const ARoutine:TRoutine;
                            const AddRoutine:Boolean):TDataCall;

    function CreateVariable(const AOwner:TNode;
                            const AName:String;
                            const AClauses:TClauses;
                            const AType:TType):TVariable;

    function Error(const APosition:TPosition;
                   const ALength:Integer;
                   const AError:TErrors;
                   const AParam1:String=''; const AParam2:String='';
                   const ANode:TNode=nil):TNodeError; overload;

    function Error(const APosition:TPosition;
                   const ALength:Integer;
                   const AError:TErrors; const ANode:TNode):TNodeError; overload;

    function Error(const AError:TErrors; const AParam:String):TNodeError; overload;

    function Error(const AError:TErrors;
                   //const ALength:Integer;
                   const ANode:TNode):TNodeError; overload;

    procedure ErrorFree(const ANode,AOwner:TNode; const APosCount:Integer;
                        const APosition:TPosition; const AError:TErrors);

    function ExpectSymbol(const AOwner:TNode; const ASymbol:String):Boolean;
    function FindData(const AParent,AOwner:TNode; const AName:String):TData;
    function FindDataRecursive(const AParent,AOwner:TNode; const AName:String):TData;
    function FinishOperand(const AParent,AOwner:TNode; const ALeft:TData):TData;
    function FinishMember(const AParent,AOwner:TNode; const AData:TData):TMember;
    class procedure FreeIfNotOwned(const ANode,AParent:TNode); static;
    function Get(const AParent,AOwner:TNode; const DoError:Boolean=True):TData;
    function GetArrayType(const AParent,AOwner:TNode; const AType:TType; const APosition:TPosition):TArrayType;

    function GetData(const AParent,AOwner:TNode; const AName:String;
                     Recursive:Boolean; const DoError:Boolean):TData;

    function GetDataMember(const AParent,AOwner:TNode; const AToken:String; const DoError:Boolean):TData; overload;
    function GetDeclaredParameters(const Stop:String;
                                   const AParent,AOwner:TNode;
                                   const AType:TType;
                                   out ErrorFlag:Boolean):TNodes;

    function GetExpression(const AParent,AOwner:TNode; const DoError:Boolean):TData; overload;
    function GetExpression(const AParent,AOwner:TNode):TData; overload;

    function GetExpressions(const Stop:String; const AParent,AOwner:TNode; out APosition:TPosition):TNodes;

    function GetParameters(const Stop:String;
                           const AParent,AOwner:TNode;
                           const AType:TType;
                           DoError:Boolean;
                           out ErrorFlag:Boolean):TNodes;

    function GetRange(const AParent,AOwner:TNode; const AMin:TData):TRange;
    function GetSubType(const AParent,AOwner:TNode; const AType:TType; const AllowRangeType:Boolean):TType;
    function GetType(const AOwner:TNode; const AType:String):TType; overload;
    function GetTypes(const AParent,AOwner:TNode):TType;
    procedure Init(const AText:String); override;

    {$IFDEF INTERNAL}
    procedure InternalError(const APosition:TPosition; const S:String); overload;
    procedure InternalError(const ANode:TNode; const S:String); overload;
    {$ENDIF}

    class function IsMagicTrueFalse(const AData:TData):Boolean; static;
    function IsValidCondition(const ACondition:TData; const AOwner:TNode;
                                    const ACount:Integer;
                                    const APosition:TPosition):Boolean;

    function NewArrayType(const AOwner:TNode; const AType:TType):TArrayType;
    procedure ReplacePositionOf(const AOld,ANew:TNode);
    function CreateSpecialized(const AParent,AOwner:TNode;
                               const AType:TParametersType):TSpecializedType;
    class procedure TryChangeOwnership(const ANode,AOwner,ANewOwner:TNode); static;

    function TryGetDeclaredParameters({const APosition:TPosition;}
                                     const AParent:TNode;
                                     out HasParameters:Boolean):TNodes;

    function TryGetLambdaFunction(const AParent,AOwner:TNode):TTypeCall; virtual; abstract;

    function TryGetType(const AParent,AOwner:TNode; const AllowRangeType:Boolean):TType; overload;
    function TryGetType(const APos:TPosition; const AParent,AOwner:TNode; const AName:String):TType; overload;
    function TryRangeOrMember(const AParent,AOwner:TNode; const AData:TData):TData;
//    procedure TypeNotFound(const APosition:TPosition; const AType:String);
    procedure Unknown(const AName:String);
    function VariableCall(const AOwner:TNode; const AVariable:TVariable;
                          const AType:TType;
                          const AddRoutine:Boolean):TVariableCall;

  public
    Finder : TFinder;
    OnError : TOnError;

    Errors : TNodeErrors;

    Constructor Create; virtual;
    Destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  {$IFDEF EXTRA_CHECKS}
  Compare.AST,
  {$ENDIF}
  //Text,
  Evaluator.CompileTime, Magic, Utils.AST, Creator.AST;

{ TExpressionParser }

Constructor TExpressionParser.Create;
begin
  inherited;

  Checker:=TChecker.Create;

  Finder:=TFinder.Create;
  Finder.Checker:=Checker;
end;

Destructor TExpressionParser.Destroy;
begin
  Finder.Free;
  Checker.Free;

  Clear;
  inherited;
end;

// =
// <>
// >
// <
// <=
// >=
// +
// -
// *
// /
// ?
// in
// and
// or
// xor
function TExpressionParser.GetOperand:TOperand;

  function CreateOperand(const Operand:String):TOperand;
  begin
    result:=nil;

    if Operand=TSyntax._Symbol_Equal then
    begin
      result:=TIsEqual.Create; // =
      AddPositionSymbol(result,TSyntax._Symbol_Equal);
    end
    else
    if Operand=TSyntax._Symbol_Lower then
    begin
      AddPositionSymbol(result,TSyntax._Symbol_Lower); // <-- problem: result is nil here !!

      PositionToAdd:=Position;

      if PeekSymbolGet(TSyntax._Symbol_Equal) then
      begin
        result:=TIsLowerOrEqual.Create; // <=
        AddPositionSymbol(result,TSyntax._Symbol_Equal);
      end
      else
      if PeekSymbolGet(TSyntax._Symbol_Greater) then
      begin
        result:=TIsNotEqual.Create; // <>
        AddPositionSymbol(result,TSyntax._Symbol_Greater);
      end
      else
        result:=TIsLower.Create; // <

    end
    else
    if Operand=TSyntax._Symbol_Greater then
    begin
      AddPositionSymbol(result,TSyntax._Symbol_Greater); // <-- problem: result is nil here !!

      PositionToAdd:=Position;

      if PeekSymbolGet(TSyntax._Symbol_Equal) then
      begin
        result:=TIsGreaterOrEqual.Create; // >=
        AddPositionSymbol(result,TSyntax._Symbol_Equal);
      end
      else
        result:=TIsGreater.Create; // >
    end
    else
    if Operand=TSyntax._Symbol_Add then
    begin
      if not PeekSymbolGet(TSyntax._Symbol_Equal) then
      begin
        result:=TAddition.Create;
        AddPositionSymbol(result,TSyntax._Symbol_Add);
      end;
    end
    else
    if Operand=TSyntax._Symbol_Subtract then
    begin
      if not PeekSymbolGet(TSyntax._Symbol_Equal) then
      begin
        result:=TSubtraction.Create;
        AddPositionSymbol(result,TSyntax._Symbol_Subtract);
      end;
    end
    else
    if Operand=TSyntax._Symbol_Multiply then
    begin
      if not PeekSymbolGet(TSyntax._Symbol_Equal) then
      begin
        result:=TMultiplication.Create;
        AddPositionSymbol(result,TSyntax._Symbol_Multiply);
      end;
    end
    else
    if Operand=TSyntax._Symbol_Divide then
    begin
      if not PeekSymbolGet(TSyntax._Symbol_Equal) then
      begin
        result:=TDivision.Create;
        AddPositionSymbol(result,TSyntax._Symbol_Divide);
      end;
    end
    else
    (* UNREACHABLE:
    if Operand=TSyntax._Reserved_Condition then
    begin
      result:=TCondition.Create;

      // AddPositionKeyword here??
      AddPositionSymbol(result,TSyntax._Reserved_condition);

      //AText:=TSyntax._Reserved_condition;
    end
    else
    *)
    if Checker.TextIs(Operand,TSyntax._Reserved_in) then
    begin
      result:=TContains.Create;
      AddPositionKeyword(result,TSyntax._Reserved_in);
    end
    else
    if Checker.TextIs(Operand,TSyntax._Reserved_and) then
    begin
      result:=TLogicalAnd.Create;
      AddPositionKeyword(result,TSyntax._Reserved_and);
    end
    else
    if Checker.TextIs(Operand,TSyntax._Reserved_or) then
    begin
      result:=TLogicalOr.Create;
      AddPositionKeyword(result,TSyntax._Reserved_or);
    end
    else
    if Checker.TextIs(Operand,TSyntax._Reserved_xor) then
    begin
      result:=TLogicalXor.Create;
      AddPositionKeyword(result,TSyntax._Reserved_xor);
    end;
  end;

var Old : TPosition;
    OldCount : Integer;
begin
  OldCount:=Positions.Count;
  Old:=Position;

  PositionToAdd:=Position;
  result:=CreateOperand(GetToken);

  if result=nil then
  begin
    // Reset back
    Positions.DeleteFrom(OldCount);
    Position:=Old;
  end;
end;

{$IFDEF INTERNAL}
procedure TExpressionParser.InternalError(const APosition:TPosition; const S:String);
begin
  RaiseException(APosition,Length(S),TErrorTexts.Texts[_InternalError]+S);
end;

procedure TExpressionParser.InternalError(const ANode:TNode; const S:String);
var tmp : TNodePosition;
begin
  if Positions.Find(ANode,tmp) then
     InternalError(tmp.Position,S)
  else
     InternalError(Position,S);
end;
{$ENDIF}

procedure TrySetVariableTypeOwner(const AVariable:TVariable; const AType:TType);
begin
  if AType.Owner=AVariable.Owner then
  begin
    if (AType is TRangeType) or
       (AType is TManyValues) or
       (AType is TArrayType) or
       (AType is TGenericType) or
       TChecker.IsInlineStruct(AType) then
          AType.Owner:=AVariable;
  end
  else
  if AType.Owner=nil then
     if AType is TGenericType then
        AType.Owner:=AVariable;
end;

function TExpressionParser.CreateVariable(const AOwner:TNode;
                             const AName:String;
                             const AClauses:TClauses;
                             const AType:TType):TVariable;
begin
  {$IFDEF INTERNAL}
  if AName='' then
     InternalError(Position,'Empty variable name');
  {$ENDIF}

  result:=TVariable.Create;
  result.Owner:=AOwner;

  result.Clauses:=AClauses;

  result.Name:=AName;

  if AType<>nil then
  begin
    result.VariableType:=AType;
    TrySetVariableTypeOwner(result,AType);
  end;

  {$IFDEF NODEPOOL}
//  NodePool.Add(result);
  {$ENDIF}
end;

procedure TExpressionParser.FinishDataCall(const ACall:TDataCall;
                                           const ARoutine:TType;
                                           const AddRoutine:Boolean);
var Clauses : TClauses;
    tmp : TType;
    tmpRoutine : TRoutine;
begin
  ACall.Routine:=ARoutine;

  tmp:=TChecker.GetFinalType(ARoutine);

  if AddRoutine then
     if tmp is TNamedType then // GetFinalType ?
        AddPosition(ARoutine,TNamedType(tmp).Name);

  // Flag to skip internal check, and set ResultValue as internal node (not in Text)
  PositionToAdd.Line:=0;

  {$IFDEF INTERNAL}
  if not (tmp is TRoutine) then
     InternalError(tmp,'Not a TRoutine at FinishDataCall');
  {$ENDIF}

  tmpRoutine:=TRoutine(tmp);

  Clauses.Reset;
  ACall.ResultValue:=CreateVariable(ACall,tmpRoutine.Name,Clauses,tmpRoutine.Output);

//    TVariable(ACall.ResultValue).VariableType:=tmpRoutine.Output;

  Positions.ReplaceLastNode(ACall);

  PositionToAdd:=Position;
end;

function NewDataCall(const AOwner:TNode):TDataCall;
begin
  result:=TDataCall.Create;

  {$IFDEF NODEPOOL}
//  NodePool.Add(result);  // breakpoint condition here: NodePool.CountOf(result)=39
  {$ENDIF}

  result.Owner:=AOwner;
end;

function TExpressionParser.DirectCreateDataCall(const AOwner:TNode;
                                                const ARoutine:TRoutine;
                                                const AddRoutine:Boolean):TDataCall;
begin
  result:=NewDataCall(AOwner);
  FinishDataCall(result,ARoutine,AddRoutine);
end;

// Call routine "Foo" inside a expression:   Foo+1
function TExpressionParser.CreateDataCall(const AOwner:TNode;
                                          const ARoutine:TRoutine;
                                          const AddRoutine:Boolean):TDataCall;
begin
  result:=DirectCreateDataCall(AOwner,ARoutine,AddRoutine);
end;

// General method to parse any complete expression, of any kind
function TExpressionParser.GetExpression(const AParent,AOwner:TNode; const DoError:Boolean):TData;
begin
  result:=Get(AParent,AOwner,DoError);

  { unreachable:
  if result<>nil then
     if PeekIs(TSyntax._RangeDelimiter) then
        result:=GetRange(AParent,AOwner,result);
  }
end;

function TExpressionParser.GetExpression(const AParent,AOwner:TNode):TData;
begin
  result:=GetExpression(AParent,AOwner,True);
end;

function TExpressionParser.TryGetDeclaredParameters(
                                     const AParent:TNode;
                                     out HasParameters:Boolean):TNodes;
var OldCount : Integer;
    Old : TPosition;

    tmpError : Boolean;
begin
  OldCount:=Positions.Count;
  Old:=Position;

  HasParameters:=GetSymbol(AParent,TSyntax._BeginParameters);

  if HasParameters then
  begin
    // TODO: Too early. AParent is not the owner or "("
    //AddPosition(AParent,APosition,TSyntax._BeginParameters,TPositionStyle.Symbol);

    result:=GetDeclaredParameters(TSyntax._EndParameters,AParent,AParent,nil,tmpError);

    if tmpError then
    begin
      Positions.DeleteFrom(OldCount);
      Position:=Old;
    end;
  end
  else
    result:=nil;
end;

// 123, 'abc', 4*5, foo(bar(15)), ...
function TExpressionParser.GetExpressions(const Stop:String; const AParent,AOwner:TNode; out APosition:TPosition):TNodes;
var tmp : TData;
begin
  result:=nil;

  APosition:=Position;

  PositionToAdd:=APosition;

  if not GetSymbol(AOwner,Stop) then
  repeat
    tmp:=GetExpression(AParent,AOwner);

    if tmp=nil then
       tmp:=TryGetLambdaFunction(AParent,AOwner);

    if tmp=nil then
       break
    else
       result.Add(tmp);

    APosition:=Position;
    PositionToAdd:=APosition;

    if not GetSymbol(AOwner,TSyntax._ItemDelimiter) then
       if GetSymbol(AOwner,Stop) then
          break;

  until False;
end;

// To access class methods (ie: call a type method)
function TExpressionParser.CreateTypeCall(const AOwner:TNode; const AType:TType; const AName:String):TTypeCall;
begin
  result:=TTypeCall.CreateNew(AOwner);

  result.TheType:=AType;

  AddPosition(result,PositionToAdd,AName,TPositionStyle.Identifier,False);
end;

// Returns type of array expression
function TExpressionParser.GetDataTypeArray(const AData:TArrayExpression):TType;
begin
  result:=Checker.GetDataTypeArray(AData);

  {
    // USELESS ???
  if result=nil then
     Error(TErrors._CannotFindArrayType,AData);
  }
end;

function IsTypeAutoInitialized(const AType:TType):Boolean;
begin
  // NOOO !!! Basic types: Integer,Boolean,Text,Float are auto-initialized (0, false, '')
  if TChecker.IsBasicType(AType) then
     result:=False
  else
  result:=(AType is TArrayType) or // Arrays are always initialized
          (AType is TSpecializedType) or // Generic specialized, always initialized
          (AType is TClassType) or // Class variables are always initialized at first use attempt
          (AType is TFunctionType) or // Function types (lambdas)
          ( (AType is TNamedType) and (AType.Owner=nil) ); // Modules
end;

function TExpressionParser.FindData(const AParent,AOwner:TNode; const AName:String):TData;

  function FindInType(const AType:TType):TData;
  begin
    result:=CreateTypeCall(AOwner,AType,AName);
  end;

  (*
  function AssigningVariable(const AVariable:TVariable):Boolean;
  begin
    result:=IsAssignment; // ???
  end;
  *)

  function DataFound(const AData:TData):TData;
  begin
    // TODO: Maybe here Usages.Add(AData) ? (problem: no Reference known yet here...)

    (*
    if AData is TVariable then
       //if not AssigningVariable(TVariable(AData)) then
       if not IsTypeAutoInitialized(TChecker.GetFinalType(TVariable(AData).VariableType)) then
       if not TVariable(AData).Initialized then

          // "out" parameters don't need to be initialized in advance
          { if (not TVariable(AData).Clauses.Final) and
             (TChecker.GetRoutineOf(AData)<>nil) then
          else
          }
          if (TChecker.GetRoutineOf(AData)<>nil) or // <-- is a local variable of a routine
             (TChecker.GetClassTypeOf(AData)=nil) then  // <-- or it is local to an unnamed module { } block
             Error(TErrors._VariableNotInitialized,AData);
    *)

    AddPosition(AData,AName);

    result:=AData;
  end;

  function FindDataIn(const ANodes:TNodes):TData;
  var Node : TNode;
  begin
    for Node in ANodes do
        if Checker.NodeIs(Node,AName) then
        begin
          if Node is TExtender then
          begin
            if TExtender(Node).Extension is TRoutine then
               Exit(CreateDataCall(AOwner,TExtender(Node).Extension as TRoutine,True))
            { unreachable:
            else
               Exit(CreateTypeCall(AOwner,TExtender(Node).Extension,AName));
            }
          end
          else
          if Node is TRoutine then
             Exit(CreateDataCall(AOwner,TRoutine(Node),True))
          else
          { unreachable:
          if Node is TTypeMember then // <-- before TType !!
             Exit(FindInType(TTypeMember(Node).Member))
          else
          }
          if Node is TType then
             Exit(FindInType(TType(Node)))
          else
          if Node is TData then
             Exit(DataFound(TData(Node)))
          {$IFDEF INTERNAL}
          else
             InternalError(Node,
                           'Found named node of unknown class: '+TASTUtils.NameOf(Node,True));
          {$ENDIF}
        end;

    result:=nil;
  end;

  function FindInClassType(const AClass:TClassType):TData;
  var tmpType : TType;
  begin
    if AClass.Ancestor=nil then
       result:=nil
    else
       result:=FindData(AClass.Ancestor,AOwner,AName);

    if result=nil then
    begin
      tmpType:=Checker.TypeOfIndexed(AClass.DefaultIndexed);

      if tmpType<>nil then
         if tmpType<>AClass then
            result:=FindData(tmpType,AOwner,AName);
    end;
  end;

  // When "Ancestor" refers to a routine-call, not a field of Class class:
  function CreateAncestor:TAncestor;
  var tmpClass : TClassType;
      tmpRoutine : TRoutine;
      tmpAncestor : TRoutine;
  begin
    result:=nil;

    tmpRoutine:=TChecker.GetRoutineOf(AParent);
    tmpClass:=TChecker.GetClassTypeOf(AOwner);

    // It must be found in both inside a routine and a class
    if (tmpClass=nil) or (tmpRoutine=nil) then
       Error(TErrors._CannotFindAncestor,AOwner)
    else
    if tmpClass.Ancestor=nil then
       Error(TErrors._ClassHasNoAncestor,tmpClass)
    else
    begin
      tmpAncestor:=Finder.FindInherited(TChecker.TheTypeOf(tmpClass.Ancestor) as TClassType,tmpRoutine);

      if tmpAncestor=nil then
         Error(Position,Length(TSyntax._Reserved_Ancestor),
               TErrors._AncestorMethodMissing,
               TASTUtils.NameOf(tmpClass.Ancestor,False),tmpRoutine.Name)
      else
      begin
        result:=TAncestor.Create;
        result.Owner:=AOwner;

        TAncestor(result).Ancestor:=TChecker.GetClassTypeOf(tmpAncestor);

        AddPositionKeyword(result,TSyntax._Reserved_ancestor);

        result.DataCall:=DoCreateCall(AOwner,tmpAncestor,False);
        SetExpressionDataParameters(AOwner,result.DataCall,tmpRoutine.Parameters);
      end;
    end;
  end;

  function FindDataWithCall(const AParent:TNode; const AName:String):TData;
  var tmp : TType;
  begin
    if AParent=nil then // <-- assert never
       result:=nil
    else
    begin
      if AParent is TDataCall then
         tmp:=TChecker.GetFinalType(TDataCall(AParent).Routine)
      else
         tmp:=nil;

      if (tmp is TRoutine) and (TRoutine(tmp).Output<>nil) then
         result:=FindData(TRoutine(tmp).Output,AOwner,AName)
      else
         result:=FindData(AParent,AOwner,AName);
    end;
  end;

  // Find shared item
  function FindDataInType(const AType:TType):TData;
  begin
    if Checker.TextIs(TSyntax._Reserved_Ancestor,AName) then
       result:=CreateAncestor
    else
       result:=FindDataIn(AType.Items);

    if result=nil then
    begin
      if AType is TParametersType then
         result:=FindDataIn(TParametersType(AType).Parameters);

      if result=nil then
         if AType is TRoutine then
         begin
           if Checker.TextIs(TRoutine(AType).Name,AName) then
              result:=CreateDataCall(AOwner,TRoutine(AType),True);
         end
         else
         begin
           if AType is TArrayType then
              if TChecker._Types[TChecker._Array]<>nil then // Array
                 result:=FindData(TChecker._Types[TChecker._Array],AOwner,AName);

           if result=nil then
              if AType is TClassType then
                 result:=FindInClassType(TClassType(AType));
         end;
    end;
  end;

  function FindFinalType(const AType:TType):TData;
  var tmp : TType;
  begin
    tmp:=TChecker.GetFinalType(AType);

    if tmp=nil then
       result:=nil
    else
       result:=FindData(tmp,AOwner,AName);
  end;

  function FindDataInSelf(const ASelf:TSelf):TData;
  var tmp : TType;
  begin
    tmp:=ASelf.TheType;

    if tmp=nil then
       tmp:=TFinder.FindParentTypeOf(ASelf); // {} blockstatement first

    result:=FindData(tmp,AOwner,AName);

    if result=nil then
       if ASelf.TheType=nil then
          result:=FindData(TFinder.ModuleOf(ASelf),AOwner,AName); // module second
  end;

  function FindInArray(const AArray:TArrayExpression):TData;
  var tmp : TType;
  begin
    tmp:=GetDataTypeArray(AArray);

    if tmp=nil then
       result:=nil
    else
       result:=FindData(tmp,AOwner,AName);
  end;

begin
  {$IFDEF INTERNAL}
  if (AParent=nil) or (AName='') then
     InternalError(AParent,'FindData empty owner or name');
  {$ENDIF}

  result:=nil;

  if AParent is TExtender then // before TType !
     result:=FindFinalType(TExtender(AParent).TheType)
  else
  { unreachable:
  if AParent is TRangeType then
     result:=FindData(TChecker._Types[TChecker._Integer],AOwner,AName)
  else
  }
  if AParent is TTypeMember then // <-- before TType !!
     result:=FindFinalType(TTypeMember(AParent).Member)
  else
  if AParent is TSpecializedType then // <-- before TType !!
     result:=FindDataInType(TSpecializedType(AParent).TheType)
  else
  if AParent is TType then
     result:=FindDataInType(TType(AParent))
  else
  if AParent is TVariable then
     result:=FindFinalType(TVariable(AParent).VariableType)
  else
  if AParent is TSelf then
     result:=FindDataInSelf(TSelf(AParent))
  else
  if AParent is TTypeCall then
     result:=FindFinalType(TTypeCall(AParent).TheType)
  else
  if AParent is TCastingData then
     result:=FindData(TCastingData(AParent).TheType,AOwner,AName)
  else
  //if AParent is TDataCall then  // <-- wrong !  Math.IsEven ... Modulo(Value <--
//       result:=FindData(TDataCall(AParent).Routine.Output,AOwner,AName)
//    else
  if AParent is TArrayExpression then
     result:=FindInArray(TArrayExpression(AParent))
  else
  if AParent is TBlockStatement then
     result:=FindData(TBlockStatement(AParent).Block,AOwner,AName)
  else
  if AParent is TMember then // Foo.Bar
     result:=FindDataWithCall(TMember(AParent).Member,AName)
  else
  { unreachable:
  if AParent is TWith then
     result:=FindData(TWith(AParent).Module,AOwner,AName)
  else
  }
  if AParent is TAncestor then
     result:=FindData(TAncestor(AParent).Ancestor,AOwner,AName)
  else
  if AParent is TFor then
     if Checker.NodeIs(TFor(AParent).Counter,AName) then
     begin
       result:=TFor(AParent).Counter;
       AddPosition(result,AName);
     end
     else
        result:=nil
  else
  if AParent is TCatch then
     if TCatch(AParent).Error is TVariable and
        Checker.NodeIs(TCatch(AParent).Error,AName) then
          result:=DataFound(TVariable(TCatch(AParent).Error))
     else
        result:=nil
  else
  if AParent is TVariableCall then
     result:=FindDataWithCall(AParent,AName)
  else
  if AParent is TWhen then // just to skip the error below
  else
  if AParent is TDataCall then // <-- ??  // just to skip the error below
  else
  if AParent is TOperand then // before is TData
     result:=nil
  else
  { unreachable:
  if AParent is TRange then // before is TData
     result:=nil
  else
  }
  if AParent is TGroup then // just to skip the error below
  else
  if AParent is TIf then // just to skip the error below
  else
  if AParent is TWhile then // just to skip the error below
  else
  if AParent is TRepeat then // just to skip the error below
  else
  if AParent is TReturn then // just to skip the error below
  else
  if AParent is TTry then // just to skip the error below
  else

  // Basic types
  if AParent is TNumber then // <-- 1.abc ?? just to skip the error below
  else
  if AParent is TBoolean then // <-- False.AsText ?? just to skip the error below
  else
  if AParent is TText then // "abc".Length ?? just to skip the error below
     // result:=FindDataInType(_Types[_TypesText])

  {$IFDEF INTERNAL}
  else
    InternalError(AParent,'Cannot find in : '+TASTUtils.NameOf(AParent,True));
  {$ENDIF}
end;

function TExpressionParser.FindDataRecursive(const AParent,AOwner:TNode; const AName:String):TData;
var tmp : TNode;
begin
  tmp:=AParent;

  while tmp<>nil do
  begin
    result:=FindData(tmp,AOwner,AName);

    if result=nil then
       tmp:=tmp.Owner
    else
       Exit
  end;

  result:=nil;
end;

procedure TExpressionParser.Unknown(const AName:String);
begin
  Error(Position,Length(AName),TErrors._Unknown,AName);
end;

function TExpressionParser.FindExtenderData(const AParent:TNode; AOwner:TNode; const AType:TType;
                                            const AName:String; TopDown:Boolean):TData;

  function TryFind(const AParent:TNode):TData;

    function TryFindIn(const ANodes:TNodes; TopDown:Boolean):TData;
    var t : Integer;
    begin
      result:=nil;

      if TopDown then
         for t:=Low(ANodes) to High(ANodes) do // top-down scope (with Module)
         begin
           result:=TryFind(ANodes[t]); // Subclasses

           if result<>nil then
              break;
         end
      else
         for t:=High(ANodes) downto Low(ANodes) do // bottom-up scope (same source file)
         begin
           result:=TryFind(ANodes[t]); // Subclasses

           if result<>nil then
              break;
         end;
    end;

    function TryCreateExtenderCall(const AExtender:TExtender):TData;
    begin
      result:=nil;

      if AExtender.Extension<>nil then
         if Checker.TextIs(AExtender.Extension.Name,AName) then
            if TChecker.CompatibleType(AExtender.TheType,AType) then
               if AExtender.Extension is TRoutine then
                  result:=CreateDataCall(AOwner,AExtender.Extension as TRoutine,True)
               { unreachable:
               else
                  result:=CreateTypeCall(AOwner,AExtender.Extension,AName);
               }
    end;

  begin
    if AParent is TExtender then
       result:=TryCreateExtenderCall(TExtender(AParent))
    else
    if AParent is TType then
       result:=TryFindIn(TType(AParent).Items,TopDown)
    else
    if (AParent is TWith) and (TWith(AParent).Module<>nil) then
       result:=TryFindIn(TWith(AParent).Module.Items,True) //,AType,AName,True);
    else
       result:=nil;
  end;

var tmp : TNode;
begin
  tmp:=AParent;

  while tmp<>nil do
  begin
    result:=TryFind(tmp);

    if result=nil then
       tmp:=tmp.Owner
    else
       Exit;
  end;

  result:=nil;
end;

function TExpressionParser.GetData(const AParent,AOwner:TNode;
                                   const AName:String;
                                   Recursive:Boolean;
                                   const DoError:Boolean):TData;

  function CreateSelf:TSelf;
  begin
    result:=TSelf.Create;
    result.Owner:=AOwner;

    result.TheType:=TChecker.GetClassTypeOf(AParent);

    AddPositionKeyword(result,TSyntax._Reserved_self);
  end;

var tmpType : TType;
begin
  {$IFDEF INTERNAL}
  if AName='' then
     InternalError(AOwner,'Empty name at GetData');
  {$ENDIF}

  if Recursive and (AParent<>nil) then
  begin
    if Checker.TextIs(TSyntax._Reserved_self,AName) then // <-- todo: try to remove this
       result:=CreateSelf
    else
       result:=FindDataRecursive(AParent,AOwner,AName);
  end
  else
    result:=FindData(AParent,AOwner,AName);

  if result=nil then
  begin
    if Recursive then
    begin
      tmpType:=Finder.FindType(AParent,AName);  // Owner.Owner... scope find?

      if tmpType=nil then
      begin
        if DoError then
           Error(Position,Length(AName),TErrors._IdentifierNotFound,AName);
      end
      else
         result:=CreateTypeCall(AOwner,tmpType,AName);
    end
    else
    // Try find in extenders full scope up
    if AParent is TData then
       result:=FindExtenderData(AParent,AOwner,TChecker.GetDataType(AParent,TData(AParent)),AName,False);

    if DoError then
       if result=nil then
          Unknown(AName);
  end;

  if result<>nil then
     if TChecker.BadHiddenAccess(AParent,result) then
        Error(TErrors._HiddenAccessScope,result);
end;

{ unused:
procedure TExpressionParser.TypeNotFound(const APosition:TPosition; const AType:String);
begin
  Error(APosition,Length(AType),TErrors._TypeNotFound,AType);
end;}

function TExpressionParser.GetType(const AOwner:TNode; const AType:String):TType;
begin
  result:=Finder.FindType(AOwner,AType);

  {
  if result=nil then
     TypeNotFound(Position,AType);}
end;


// Foo[xx]
function TExpressionParser.NewArrayType(const AOwner:TNode; const AType:TType):TArrayType;
//var tmpClauses : TClauses;
begin
  result:=TArrayType.Create;
  result.Owner:=AOwner;

//  if AType is TNamedType then
//     result.Name:=TNamedType(AType).Name;

//  result.Ancestor:=TChecker._Types[TChecker._Array]; // Array type in sys

//  tmpClauses.Reset;
//  result.Generics.Add(AType {CreateVariable(result,'T',tmpClauses,AType)});

  result.TheType:=AType; //TChecker._Types[TChecker._Array]; //AType;

  if (AType is TRangeType) or (AType is TGenericType) then
     AType.Owner:=result;
end;

// Free ANode only if AParent does not own it in any way:
// 1) As one of the AParent.Items
// 2) As one of the Parameters of AParent routine
class procedure TExpressionParser.FreeIfNotOwned(const ANode,AParent:TNode);

  function NodeIsAParameter:Boolean;
  begin
    result:=(AParent is TRoutine) and
            (TRoutine(AParent).Parameters.IndexOf(ANode)<>-1);
  end;

var tmp : Boolean;
begin
  if (ANode<>nil) and (ANode.Owner=AParent) then
  begin
    tmp:=AParent is TType;

    if tmp then
    begin
      tmp:=TType(AParent).Items.IndexOf(ANode)<>-1;

      if not tmp then
         tmp:=NodeIsAParameter;
    end;

    if not tmp then
       ANode.Free;
  end;
end;

procedure TExpressionParser.ErrorFree(const ANode,AOwner:TNode;
                                      const APosCount:Integer;
                                      const APosition:TPosition;
                                      const AError:TErrors);
var tmpS : String;
    tmpL : Integer;
begin
  tmpL:=Length(TASTUtils.NameOf(ANode,False));

  tmpS:=TASTUtils.NameOf(ANode,True);

  Positions.DeleteFrom(APosCount);

  FreeIfNotOwned(ANode,AOwner);

  Error(APosition,tmpL,AError,tmpS);
end;

// Foo[10] Foo[3..7]
function TExpressionParser.GetArrayType(const AParent,AOwner:TNode; const AType:TType; const APosition:TPosition):TArrayType;

  function TryGetRange(ANode:TNode):TRange;
  begin
    result:=nil;

    if ANode is TTypeCall then
    begin
      ANode:=TTypeCall(ANode).TheType;

      if ANode is TIntegerClass then
         ANode:=TIntegerClass(ANode).Size;

      if ANode is TRangeType then
         result:=TRangeType(ANode).Range
    end;
  end;

  procedure SetArraySize(const AArray:TArrayType;
                         const ANode,AExpressionOwner:TNode;
                         const APosCount:Integer);
  var tmp : TRange;
  begin
    if ANode is TRange then  // Foo[1..10]
    begin
      CheckRangeIsAscending(TRange(ANode));
      AArray.Size:=TRange(ANode);
    end
    else
    if (ANode is TData) and Checker.IsInteger(ANode as TData) then // Foo[123]
    begin
      if TEvaluateCompile.Evaluable(TData(ANode),True) then
         AArray.Size:=TData(ANode)
      else
         ErrorFree(ANode,AExpressionOwner,
                   APosCount,
                   APosition,
                   TErrors._ArraySizeNotEvaluable);
    end
    else
    if TChecker.IsEnumerationType(ANode) then // { Red,Blue,Green }
    begin
      AArray.Size:=TTypeCall(ANode);
      {
      AArray.Size:=TInteger.Create(Length(TTypeCall(ANode).TheType.Items));
      AArray.Size.Owner:=AArray;

      TTypeCall(ANode).Free;
      }
    end
    else
    begin
      tmp:=TryGetRange(ANode);

      if tmp=nil then
         ErrorFree(ANode,AExpressionOwner,APosCount,
                   APosition,TErrors._CannotArrayOfType)
      else
         AArray.Size:=TTypeCall(ANode); // MyRange is 1..5 {}   Foo:Text[MyRange]
    end;

    // Change ownership when parsing expressions:
    if AArray.Size<>nil then
       // AExpressionOwner is a temporary trick to make sure class fields
       // (eg: variables) aren't changed ownership by mistake
       if AArray.Size.Owner=AExpressionOwner then // ugly, but works.
          AArray.Size.Owner:=AArray;
  end;

  {$IFDEF FPC}
  function AddNodes(const A,B:TNodes):TNodes;
  var t,L : Integer;
  begin
    //result:=nil;

    L:=Length(A);
    SetLength(result,L+Length(B));

    for t:=0 to High(A) do
        result[t]:=A[t];

    for t:=0 to High(B) do
        result[L+t]:=B[t];
  end;
  {$ENDIF}

var tmp : TNodes;
    t : Integer;
    tmpFirst : TArrayType;
    Old,
    tmpPos : TPosition;
    tmpDimensions : TNodes;

    tmpOldCount: Integer;

    tmpExpressionOwner : TNode;
begin
  result:=NewArrayType(AOwner,AType);

  tmpExpressionOwner:=result;

  PositionToAdd:=APosition;

  tmpDimensions:=nil;

  tmpOldCount:=Positions.Count;

  repeat
    AddPositionSymbol(result,TSyntax._BeginArray);

    Old:=Position;

    if PeekIsGet(TSyntax._EndArray) then
    begin
      tmp:=[nil];

      PositionToAdd:=Old;
      AddPositionSymbol(result,TSyntax._EndArray);
    end
    else
    begin
      tmp:=GetExpressions(TSyntax._EndArray,AParent,tmpExpressionOwner,tmpPos); // [123]

      if Length(tmp)=0 then
      begin
        //tmp:=[nil];

        result.Free;
        result:=nil;
        Exit;
      end;
    end;

    if Length(tmp)>0 then
       tmpDimensions:={$IFDEF FPC}AddNodes(tmpDimensions,tmp){$ELSE}tmpDimensions+tmp{$ENDIF};

    PositionToAdd:=Position;

    // [123][34][67] is equivalent to: [123,34,67]

  until not PeekSymbolGet(TSyntax._BeginArray);

  // Optional
  if Length(tmpDimensions)>0 then
  begin
    tmpFirst:=result;

    for t:=High(tmpDimensions) downto 1 do
    begin
      if tmpDimensions[t]<>nil then
         SetArraySize(result,tmpDimensions[t],tmpExpressionOwner,tmpOldCount);

      result:=NewArrayType(result,result);

      if tmpFirst.Owner=AOwner then
         tmpFirst.Owner:=result;
    end;

    if tmpDimensions[0]<>nil then
       SetArraySize(result,tmpDimensions[0],tmpExpressionOwner,tmpOldCount);
  end;
end;

function TExpressionParser.NewManyValuesType(const AOwner:TNode; const AType:TType):TManyValues;
begin
  result:=TManyValues.Create;
  result.Owner:=AOwner;

//  if AType is TNamedType then
//     result.Name:=TNamedType(AType).Name;

//  result.Ancestor:=TChecker._Types[TChecker._Array]; // Array type in sys

  AddPositionSymbol(result,TSyntax._ManyValues);

  { unused:
  if AType.Owner=AOwner then
     AType.Owner:=result;
  }

  result.TheType:=AType; //TChecker._Types[TChecker._Array];

//  result.Generics.Add(AType);
end;

// is Foo(a,b,c)  // X:Foo(a)
function TExpressionParser.CreateSpecialized(const AParent,AOwner:TNode;
                               const AType:TParametersType):TSpecializedType;

  // Problem: GetExpressions might return TArrayExpression->Data=TTypeCall,
  // when the needed parameter is T:Type (TArrayType-> TTypeCall.TheType)

var tmpBad : Integer;
    tmpPosition : TPosition;
    tmpS : String;
begin
  result:=TSpecializedType.Create;
  result.Owner:=AOwner;

  {$IFDEF NODEPOOL}
//  NodePool.Add(result);
  {$ENDIF}

  result.TheType:=AType;

  result.Generics:=GetExpressions(TSyntax._EndParameters,result,result,tmpPosition);

  if not Checker.ValidParameters(nil,AType.Parameters,result.Generics,tmpBad) then
  begin
    result.Free;
    result:=nil;

    tmpS:=IntToStr(tmpBad);

    Error(tmpPosition,
          Length(tmpS), // <-- incorrect length !
          TErrors._IncompatibleGeneric,tmpS);
  end;
end;

procedure TExpressionParser.CheckRangeIsAscending(const ARange:TRange);
var tmpMin,
    tmpMax : Int64;
begin
  if TEvaluateCompile.EvaluableAsInteger(ARange.Min,tmpMin) and
     TEvaluateCompile.EvaluableAsInteger(ARange.Max,tmpMax) then
  begin
    if tmpMax<tmpMin then
       Error(TErrors._InvertedOrder,ARange);
  end;
end;

procedure TExpressionParser.CheckParameterTypes(const AParameters:TNodes);
var Parameter : TNode;
begin
  for Parameter in AParameters do
      if (Parameter as TVariable).VariableType=nil then
         Error(TErrors._MissingTypeParam,TVariable(Parameter).Name);
end;

function TExpressionParser.TryGetType(const AParent,AOwner:TNode; const AllowRangeType:Boolean):TType;

  procedure TryAddNamedTypePosition(const AType:TType; const APosition:TPosition; const AText:String);
  begin
    if AType is TGenericType then
       AddPosition(AType,APosition,AText)
    else
    if (AType is TNamedType) and (AText<>'') then // ArrayType can be anonymous, ie: "Podiums is 1..3[] {}"
    begin
      PositionToAdd:=APosition;

      if AText=TNamedType(AType).Name then
         AddPosition(AType,APosition,AText)
      else
         AddPosition(AType,APosition,AText,TPositionStyle.Literal); // with alias trick
    end;
  end;

  function GetRangeType:TRangeType;
  var tmp : TData;
      Old2 : TPosition;
      OldCount : Integer;
  begin
    result:=nil;

    OldCount:=Positions.Count;
    Old2:=Position;

    tmp:=GetExpression(AOwner,AOwner,False);

    if tmp is TRange then
    begin
      CheckRangeIsAscending(TRange(tmp));

      result:=TASTCreator.CreateRangeType(TRange(tmp));
      result.Owner:=AOwner;    // <-- unnecessary ?
    end
    else
    if tmp<>nil then
       ErrorFree(tmp,AOwner,OldCount,Old2,TErrors._WrongTypeExpression)
    else
       Position:=Old2;
  end;

  function CreateInlineType(const AOwner:TNode):TClassType;

    procedure SetNotFinalItems(const AItems:TNodes);
    var N : TNode;
    begin
      for N in AItems do
      begin
        {$IFDEF INTERNAL}
        if not (N is TVariable) then
        begin
          InternalError(N,'Class item is not a variable');
          break;
        end;
        {$ENDIF}

        TVariable(N).Clauses.Final:=False;
      end;
    end;

  var tmpError : Boolean;
  begin
    result:=TASTCreator.CreateClassType(AOwner);
    TClassType(result).Clauses.Final:=True;

    result.Items:=GetDeclaredParameters(TSyntax._EndBlock,AParent,result,nil,tmpError); // Data

    if tmpError then
       Error(TErrors._BadParameter,result)
    else
    if result.Items=nil then
       Error(TErrors._EmptyItem,result)
    else
    begin
      SetNotFinalItems(result.Items);
      CheckParameterTypes(result.Items);
    end;
  end;

  function AreManyValues(const AParams:TNodes):Boolean;

    function IsManyValues(const ANode:TNode):Boolean;
    begin
      result:=(ANode is TData) and
              (TChecker.GetDataType(AParent,TData(ANode)) is TManyValues);
    end;

  var N : TNode;
  begin
    for N in AParams do
        if not IsManyValues(N) then
           Exit(False);

    result:=AParams<>nil;
  end;

var T : String;
    Old,
    Old2 : TPosition;
    OldCount : Integer;

    tmpIsInline : Boolean;
    tmpInline : TClassType;
    tmpType : TType;
begin
  Old:=Position;
  OldCount:=Positions.Count;

  tmpIsInline:=PeekIsGet(TSyntax._BeginBlock);

  if tmpIsInline then
  begin
    // inline class struct:
    // X: { Name:Text, Age:Integer } := ('Leia', 29)

    result:=CreateInlineType(AOwner);

    PositionToAdd:=Old;
    AddPositionSymbol(result,TSyntax._BeginBlock);

    tmpInline:=TClassType(result);
  end
  else
  begin
    tmpInline:=nil;

    Old2:=Position;

    if PeekSymbolGet(TSyntax._BeginArray) then
       result:=GetArrayType(AParent,AOwner,TChecker._Types[TChecker._SomeThing],Old2)
    else
    begin
      T:=GetToken;

      if T='' then
         result:=nil
      else
         result:=Finder.FindType(AOwner,T);
    end;
  end;

  if result=nil then
  begin
    Position:=Old;
    Positions.DeleteFrom(OldCount);

    if AllowRangeType then
       result:=GetRangeType;
  end;

  if result<>nil then
  begin
    TryAddNamedTypePosition(result,Old,T);

    if not (result is TFunctionType) then

    // Class with parameters, or Routine returning a Type
    if (result is TParametersType) and
       (TParametersType(result).Parameters<>nil) then
    begin
      PositionToAdd:=Position;

      if result is TRoutine then
         if TChecker.GetFinalType(TRoutine(result).Output)<>TChecker._Types[TChecker._Type] then
            Exit(nil);  // <-- routine is not returning a Type, so we exit

      if GetSymbol(result,TSyntax._BeginParameters) then
         result:=CreateSpecialized(AParent,AOwner,TParametersType(result))
      else
      if not AreManyValues(TParametersType(result).Parameters) then
         Error(TErrors._MissingParameters,result);
    end;

    Old2:=Position;

    if PeekSymbolGet(TSyntax._BeginArray) then
    begin
      result:=GetArrayType(AParent,AOwner,result,Old2);

      if tmpIsInline then
         tmpInline.Owner:=result;
    end
    else
    if PeekSymbolGet(TSyntax._ManyValues) then
    begin
      PositionToAdd:=Old2;
      result:=NewManyValuesType(AOwner,result);
    end
    else
    begin
      tmpType:=GetSubType(AParent,AOwner,result,AllowRangeType);

      if tmpType<>nil then
         result:=tmpType;
    end;
  end;
end;

function TExpressionParser.GetSubType(const AParent,AOwner:TNode; const AType:TType; const AllowRangeType:Boolean):TType;

  procedure TryFixRangeType(const ARange:TRangeType; const AType:TType);
  var tmp : TMember;
      tmpTypeCall : TTypeCall;
  begin
    if ARange.Range.Min<>nil then
       if ARange.Range.Min.Owner=AType then
       begin
         tmp:=TMember.Create;
         tmp.Owner:=ARange.Range;

         // tmp.Data:=CreateTypeCall(tmp,AType); // issue: addposition AType
         tmpTypeCall:=TTypeCall.CreateNew(tmp);
         tmpTypeCall.TheType:=AType;

         tmp.Data:=tmpTypeCall;
         tmp.Member:=ARange.Range.Min;

         ARange.Range.Min:=tmp;
       end;
  end;

var Old : TPosition;
    OldCount : Integer;
    tmpMember : TType;
begin
  result:=nil;

  Old:=Position;
  OldCount:=Positions.Count;

  if PeekSymbolGet(TSyntax._DataMember) then
  begin
    result:=TryGetType(AParent,AType,AllowRangeType);

    if result=nil then
    begin
      Position:=Old;
      Positions.DeleteFrom(OldCount);
    end
    else
    begin
      if result is TRangeType then
         TryFixRangeType(TRangeType(result),AType)
      else
      begin
        tmpMember:=result;

        result:=TASTCreator.CreateTypeMember(AOwner,AType,tmpMember);

        // special case for generic specialized type member:  Test(Text).My_Array
        if AType.Owner=AOwner then
           if AType is TSpecializedType then
              AType.Owner:=result;

        if (tmpMember.Owner=nil) and (tmpMember is TGenericType) then
           tmpMember.Owner:=result
        else
        // special case for sub.sub.sub types
        if tmpMember.Owner=AType then
           if (tmpMember is TTypeMember) or
              (tmpMember is TSpecializedType) then  // special types just created
                tmpMember.Owner:=result;  // <-- reset Owner so when freeing result, tmpMember will be destroyed too
      end;

      PositionToAdd:=Old;
      AddPositionSymbol(result,TSyntax._DataMember);
    end;
  end;
end;

// Try to find AName type, and then try sub-types until not found
function TExpressionParser.TryGetType(const APos:TPosition; const AParent,AOwner:TNode; const AName:String):TType;
var tmp : TType;
    tmpWith : TWith;
begin
  result:=Finder.FindType(AOwner,AName,tmpWith);

  if result<>nil then
  begin
    PositionToAdd:=APos;

    if tmpWith=nil then
       AddPosition(result,AName)
    else
       AddPosition(tmpWith,AName);

    tmp:=GetSubType(AParent,AOwner,result,False);

    if tmp<>nil then
       result:=tmp;
  end;
end;

function TExpressionParser.GetTypes(const AParent,AOwner:TNode):TType;
var Old : TPosition;
    tmp : String;
    OldCount : Integer;
begin
  Old:=Position;
  OldCount:=Positions.Count;

  result:=TryGetType(AParent,AOwner,True);

  if result=nil then
  begin
    Position:=Old;
    Positions.DeleteFrom(OldCount);

    tmp:=GetToken;
    Error(Old,Length(tmp),TErrors._TypeNotFound,tmp);
  end;
end;

// (A:Byte,X,Y:Float,T:Text...)
function TExpressionParser.GetDeclaredParameters(const Stop:String;
                           const AParent,AOwner:TNode;
                           const AType:TType;
                           out ErrorFlag:Boolean):TNodes;
begin
  result:=GetParameters(Stop,AParent,AOwner,AType,False,ErrorFlag);
end;

// (1,2,3...)
function TExpressionParser.GetParameters(const Stop:String;
                                  const AParent,AOwner:TNode;
                                  const AType:TType;
                                  DoError:Boolean;
                                  out ErrorFlag:Boolean):TNodes;

  function ParameterOf(const AName:String; const ByReference:Boolean):TVariable;
  var Clauses:TClauses;
  begin
    Clauses.Reset;
    Clauses.Final:=not ByReference;

    result:=CreateVariable(AOwner,AName,Clauses,AType);

    AddPosition(result,AName);

    if not ByReference then
       result.Initialized:=True; // <-- assert ensured
  end;

  function GetTypeInResult(const ANodes:TNodes; const AName:String):TType;
  var t : Integer;
  begin
    for t:=High(ANodes) downto 0 do
        if ANodes[t] is TVariable then
           if Checker.VariableIsType(TVariable(ANodes[t]),AName) then
           begin
             result:=TASTCreator.CreateGenericType(nil,TVariable(ANodes[t]),nil);
             AddPosition(result,AName);
             Exit;
           end;

    result:=nil;
  end;

  function ParametersGetType(const ANodes:TNodes):TType;
  var //Old : TPosition;
      tmp : String;
  begin
    //Old:=Position;

    result:=TryGetType(AParent,AOwner,True);

    if result=nil then
    begin
      tmp:=GetToken;
      result:=GetTypeInResult(ANodes,tmp);  // <-- search in parameters (for Generics)

      { unused:
      if result=nil then
         TypeNotFound(Old,tmp);
      }
    end;
  end;

  function SetPendingTypes(const ANodes:TNodes):Boolean;
  var tmpType : TType;
      t : Integer;
      tmpLast : TVariable;
  begin
    PositionToAdd:=Position;

    tmpType:=ParametersGetType(ANodes);

    if tmpType=nil then
       result:=False
    else
    begin
      tmpLast:=nil;

      for t:=High(ANodes) downto 0 do
          {$IFDEF INTERNAL}
          if not (ANodes[t] is TVariable) then
             InternalError(ANodes[t],'Parameter is not a Variable: '+TASTUtils.NameOf(ANodes[t],True))
          else
          {$ENDIF}

          if TVariable(ANodes[t]).VariableType=nil then
          begin
            tmpLast:=TVariable(ANodes[t]);
            tmpLast.VariableType:=tmpType;

            // special case for TypeMember, reowning
            if tmpType.Owner=AOwner then
               if tmpType is TTypeMember then
                  tmpType.Owner:=tmpLast;

            // special case for TGenericType
            if tmpType.Owner=nil then
               if tmpType is TGenericType then
                  tmpType.Owner:=tmpLast;
          end
          else
            Break;

      // Reset ownership of special types
      if tmpLast<>nil then
         TrySetVariableTypeOwner(tmpLast,tmpType);

      result:=True;
    end;
  end;

  function GetOutParameter:TVariable;
  var Old : TPosition;
      tmp : String;
  begin
    Old:=PositionToAdd;
    PositionToAdd:=Position;

    tmp:=GetToken;

    if tmp='' then
       result:=nil
    else
    begin
      result:=ParameterOf(tmp,True);

      PositionToAdd:=Old;
      AddPositionKeyword(result,TSyntax._Reserved_out);
    end;
  end;

var T : String;
    tmp : TVariable;

    Old : TPosition;
    Old2 : TPosition;
    OldCount:Integer;
begin
  result:=nil;

  Old:=PositionToAdd;

  Old2:=Position;
  OldCount:=Positions.Count;

  PositionToAdd:=Old2;

  ErrorFlag:=False;

  if GetSymbol(AOwner,Stop) then
  begin
    PositionToAdd:=Old;
    Exit;
  end;

  repeat
    PositionToAdd:=Position;

    T:=GetIdentifier;

    if T='' then
       break;

    if Checker.TextIs(T,TSyntax._Reserved_Out) then
       tmp:=GetOutParameter
    else
       tmp:=ParameterOf(T,False); // <-- make this more smart: if T is not an identifier, then destroy and exit

    if tmp=nil then
       break;

    result.Add(tmp);

    PositionToAdd:=Position;

    if GetSymbol(tmp,TSyntax._ItemDelimiter) then // ,
    else
    begin
      if GetSymbol(tmp,TSyntax._TypeDelimiter) then // :
      begin
        if SetPendingTypes(result) then
           PositionToAdd:=Position
        else
        begin
          result.Free(AOwner);

          Positions.DeleteFrom(OldCount);
          Position:=Old2;

          ErrorFlag:=True;
          result:=nil;
          break;
        end;
      end;

      if GetSymbol(tmp,Stop) then
         break
      else
      if not GetSymbol(tmp,TSyntax._ItemDelimiter) then // ,
      begin
        result.Free(AOwner);

        Positions.DeleteFrom(OldCount);
        Position:=Old2;

        ErrorFlag:=True;
        result:=nil;

        if DoError then
           Unknown(GetToken);

        break;
      end;
    end;

  until not Pending;

  PositionToAdd:=Old;
end;

// [xxx]
function TExpressionParser.GetArrayExpression(const AParent,AOwner:TNode;
                                              const AData:TData;
                                              const Expressions:Boolean;
                                              const APosition:TPosition):TArrayExpression;

  function GetBounds(const AType:TArrayType; const ASize:TData; out AMin,AMax:Integer):Boolean;
  begin
    result:=False;

    if ASize=nil then
    begin
      // Runtime only ?
      AMin:=Low(AType.Values);
      AMax:=High(AType.Values);
      // result:=True;
    end
    else
    if ASize is TRange then
    begin
      result:=TEvaluateCompile.Evaluable(TRange(ASize).Min,False) and
              TEvaluateCompile.Evaluable(TRange(ASize).Max,False);

      if result then
      begin
        AMin:=TEvaluateCompile.AsInteger(TRange(ASize).Min);
        AMax:=TEvaluateCompile.AsInteger(TRange(ASize).Max);
      end;
    end
    else
    if ASize is TTypeCall then // Enumeration
    begin
      AMin:=0;
      AMax:=TTypeCall(ASize).TheType.Items.Count; // number of enumerated items

      if AMax>0 then
         Dec(AMax);

      result:=True;
    end
    else
    if TEvaluateCompile.Evaluable(ASize,False) then
    begin
      AMin:=0;
      AMax:=TEvaluateCompile.AsInteger(ASize);
      result:=True;
    end;
  end;

  function GetIndexedType(const AClass:TClassType):TType;
  var tmp : TNode;
  begin
    tmp:=AClass.DefaultIndexed;

    if tmp is TVariable then
       result:=TVariable(tmp).VariableType
    else
       result:=nil;
  end;

  procedure CheckRange(const AData:TData; const AMin,AMax:Integer);
  var tmpIndex : Integer;
  begin
    tmpIndex:=TEvaluateCompile.AsInteger(AData);

    if tmpIndex<AMin then
       Error(Position,
             Length(AMin.ToString),
             TErrors._ArrayOutOfRangeMin,tmpIndex.ToString,AMin.ToString,AData)
    else
    if tmpIndex>AMax then
       Error(Position,
             Length(AMax.ToString),
             TErrors._ArrayOutOfRangeMax,tmpIndex.ToString,AMax.ToString,AData);
  end;

  // Try to reuse TEvaluate.ArrayRange at compile-time instead of run-time
  procedure CheckArrayRange(const AType:TType; const AExpressions:TNodes);
  var AMin, AMax : Integer;
      tmp : TType;
      E : TNode;
  begin
    tmp:=TChecker.GetFinalSpecialized(AType);

    if tmp is TClassType then
       tmp:=GetIndexedType(TClassType(tmp));

    if tmp is TArrayType then
    begin
      if GetBounds(TArrayType(tmp),TArrayType(tmp).Size,AMin,AMax) then
      for E in AExpressions do
      begin
        // TODO: Multi-dimensional array expressions

        if TEvaluateCompile.Evaluable(E as TData,False) then
           CheckRange(TData(E),AMin,AMax);
      end;
    end
    {$IFDEF INTERNAL}
    else
    if tmp=nil then // []
    else
       InternalError(tmp,'Cannot check array range of type: ');
    {$ENDIF}
  end;

  function CreateArrayExpression(const ArrayType:TType):TArrayExpression;
  var tmpPosition : TPosition;
  begin
    result:=TArrayExpression.Create;
    result.Owner:=AOwner;

    PositionToAdd:=APosition;

    AddPositionSymbol(result,TSyntax._BeginArray);

    result.Data:=AData;  // Foo[Bar] <-- AData means the Foo part

    if AData is TSelf then
       AData.Owner:=result;

// unused:    if Expressions then
    begin
      result.Parameters:=GetExpressions(TSyntax._EndArray,AParent,result,tmpPosition);

      if result.Data<>nil then // []
         CheckArrayRange(ArrayType {Checker.GetDataType(result.Data)},result.Parameters);
    end
  { unused:
    else
      result.Parameters:=SetTypes(GetParameters(TSyntax._EndArray,AParent,result,nil,True,tmpError));
    }
  end;

var tmpError : Boolean;
   // tmpDataType : TType;
    tmpArray : TType;
begin
  tmpArray:=nil;

  tmpError:=False;

  if AData<>nil then
  begin
    {
    if AData is TTypeCall then
       tmpDataType:=Checker.GetDataType(AParent,AData)
    else
    }
    begin
      tmpArray:=Checker.GetDataAsArrayType(AParent,AData,Checker.GetDataType(AParent,AData));

      if tmpArray=nil then
      begin
        tmpError:=True;
        Error(TErrors._NotAnArray,AData);
      end;

//      else
//      if tmpArray is TArrayType then
//         tmpDataType:=TArrayType(tmpArray).TheType
      //else
      //   tmpDataType:=tmpArray;

      {
      tmpDataType:=Checker.GetDataTypeAsArray(AParent,AData,Checker.GetDataType(AData));

      if tmpDataType=nil then
         Error(TErrors._NotAnArray,AData);
      }
    end;
  end
//  else
//     tmpDataType:=nil
  ;

  if tmpError then
     result:=nil
  else
     result:=CreateArrayExpression(tmpArray);
end;

procedure TExpressionParser.SetExpressionDataParameters(const AOwner:TNode; const AData:TExpressionData;
                                                  const AParameters:TNodes);

  function IsParameterOf(const AParameter:TNode; const AOwner:TNode):Boolean;
  begin
    if AOwner is TRoutine then
       result:=TChecker.Contains(TRoutine(AOwner).Parameters,AParameter)
    else
    if AOwner is TFor then
       result:=TFor(AOwner).Counter=AParameter
    else
       result:=False;
  end;

var P : TNode;
begin
  for P in AParameters do
      if P.Owner=AOwner then
         if not IsParameterOf(P,AOwner) then
            TryChangeOwnership(P,AOwner,AData);
            //P.Owner:=AData;

  AData.Parameters:=AParameters;
end;

function TExpressionParser.VariableCall(const AOwner:TNode;
                                        const AVariable:TVariable;
                                        const AType:TType;
                                        const AddRoutine:Boolean):TVariableCall;
begin
  result:=TVariableCall.Create;
  result.Owner:=AOwner;

  result.Variable:=AVariable;

  FinishDataCall(result,AType,AddRoutine);

  {$IFDEF NODEPOOL}
//  NodePool.Add(result);
  {$ENDIF}
end;

function TExpressionParser.DoCreateCall(const AOwner:TNode; const ARoutine:TRoutine;
                                        const AddRoutine:Boolean):TDataCall;
begin
  result:=CreateDataCall(AOwner,ARoutine,AddRoutine);
{
  if result<>nil then
     if result.ResultValue<>nil then
        TVariable(result.ResultValue).VariableType:=ARoutine.Output;}
end;

// Carefully lookup Position for AOld node, to replace it with ANew.
// It must exist. It must exist only once.
procedure TExpressionParser.ReplacePositionOf(const AOld,ANew:TNode);
var t: Integer;
begin
  for t:=Positions.Count-1 downto 0 do
      if Positions.Items[t].Node=AOld then
      begin
        Positions.Items[t].Node:=ANew;
        Exit;
      end;

  {$IFDEF INTERNAL}
  //InternalError(AOld,'Cannot find Position for CallData replaced with Return');
  {$ENDIF}
end;

// Common routine to attempt changing ownership of ANode.
// It is only changed when the AOwner.Items does not contain ANode.
class procedure TExpressionParser.TryChangeOwnership(const ANode,AOwner,ANewOwner:TNode);

   function IsOwner(const AType:TType):Boolean;
   begin
     result:=AType.Items.IndexOf(ANode)<>-1;

     if not result then
        if AType is TRoutine then
           result:=TRoutine(AType).Parameters.IndexOf(ANode)<>-1;
   end;

begin
  if ANode.Owner=nil then
     ANode.Owner:=ANewOwner
  else
  if ANode.Owner=AOwner then
     if (AOwner is TType) and IsOwner(TType(AOwner)) then
     else
        ANode.Owner:=ANewOwner;
end;

// Foo.Bar[xxx].XYZ(A,B,C)...
function TExpressionParser.GetDataMember(const AParent,AOwner:TNode;
                                         const AToken:String;
                                         const DoError:Boolean):TData;

  procedure CheckAbstractCall(const ANode:TNode);
  var tmpRoutine : TRoutine;
      tmpType : TType;
      tmpClass : TClassType;
  begin
    if ANode is TDataCall then
       tmpType:=TDataCall(ANode).Routine
    else
    if ANode is TTypeCall then
       tmpType:=TChecker.GetFinalType(TTypeCall(ANode).TheType)
    else
       Exit;

    if not (tmpType is TFunctionType) then // <-- not for TVariableCall
    if tmpType is TRoutine then
    begin
      tmpRoutine:=TRoutine(tmpType);

      if TChecker.IsAbstract(tmpRoutine) then
      begin
        tmpClass:=TChecker.GetClassTypeOf(tmpRoutine);

        if tmpClass=nil then // only for module-level routines. Class methods might be overriden !!
           if TChecker.GetRoutineOf(ANode)<>tmpRoutine then // <-- Factorial() { Factorial (recursive call to self)
              Error(Position,Length(tmpRoutine.Name),TErrors._CannotCallAbstract,tmpRoutine);
      end;
    end;
  end;

  function CreateCasting(const AParent,AOwner:TNode; const ATypeCall:TTypeCall):TCastingData;

    procedure DoError(const AError:TErrors);
    begin
      result.TheType:=nil;
      result.Free;
      result:=nil;

      if ATypeCall=nil then
         Error(AError,'?')
      else
      if ATypeCall.TheType=nil then
         Error(AError,ATypeCall)
      else
         Error(AError,ATypeCall.TheType);
    end;

  var tmp : TData;
      OldCount : Integer;
      Old : TPosition;
  begin
    result:=TCastingData.Create;
    result.Owner:=AOwner;

    if ATypeCall.Owner=AOwner then
    begin
      ATypeCall.Owner:=result;
      result.Owner:=AOwner;
    end
    else
    if ATypeCall.Owner.Owner=AOwner then
    begin
      result.Owner:=ATypeCall.Owner;
      ATypeCall.Owner:=result;
    end;

    result.TheType:=ATypeCall;

    // NO, see CreateCasting !!! ExpectSymbol(result,TSyntax._BeginParameters);
    GetToken;

    OldCount:=Positions.Count;
    Old:=Position;

    tmp:=Get(AParent,result,False); // <-- do not use Result as parent, because Casting is not ownership

    if tmp=nil then
    begin
      Positions.DeleteFrom(OldCount);
      Position:=Old;
    end;

    if PeekIs(TSyntax._EndParameters) then
       ExpectSymbol(result,TSyntax._EndParameters);

    if tmp=nil then
       DoError(TErrors._WrongCastingParams)
    else
       result.Data:=tmp;
  end;

  function ParseNewCasting(const AType:TTypeCall; const Old:TPosition):TCastingData;
  var tmpType : TType;
      tmpDataType : TType;
  begin
    tmpType:=AType.TheType;

    result:=CreateCasting(AParent,AOwner,AType);

    if result=nil then
       AType.Free
    else
    begin
      PositionToAdd:=Old;
      AddPositionSymbol(result,TSyntax._BeginParameters);

      tmpDataType:=TChecker.GetFinalType(TChecker.GetDataType(AParent,result.Data));

      if TChecker.CompatibleType(tmpType,tmpDataType) then
         Error(TErrors._CastingUnnecessary,result);
    end;
  end;

  function GetDataAndIndex(const AParent,AOwner:TNode; const AToken:String; Recursive:Boolean):TData;

    function GetRoutine(const AData:TData):TRoutine;
    var tmp : TType;
    begin
      if AData is TDataCall then
         tmp:=TDataCall(AData).Routine
      else
      if AData is TTypeCall then
         tmp:=TTypeCall(AData).TheType
      else
      if TChecker.IsDataFunctionType(AData) then
         tmp:=TVariable(AData).VariableType
      else
         Exit(nil);

      tmp:=TChecker.GetFinalType(tmp);

      if tmp is TRoutine then
         result:=TRoutine(tmp)
      else
         result:=nil;
    end;

    procedure SetRoutine(const AData:TData; const ARoutine:TRoutine);
    begin
      if AData is TDataCall then
         TDataCall(AData).Routine:=ARoutine
      else
      if (AData is TTypeCall) and (TTypeCall(AData).TheType is TRoutine) then
         TTypeCall(AData).TheType:=ARoutine
      else
      if TChecker.IsDataFunctionType(AData) then
         TVariable(AData).VariableType:=ARoutine
    end;

    // (5*4).AsText <-- Find AsText
    procedure TrySetRoutine(const AData:TData);
    var tmpRoutine : TRoutine;
    begin
      // Try to find a parameter-less routine, or show an error
      tmpRoutine:=GetRoutine(AData);

      if tmpRoutine<>nil then
         if (tmpRoutine.Parameters<>nil) and (not TChecker.IsManyValues(tmpRoutine.Parameters[0])) then
         begin
           tmpRoutine:=Finder.FindOverload(AOwner,tmpRoutine,nil);

           if tmpRoutine=nil then
              Error(TErrors._MissingParameters,AData)
           else
              SetRoutine(AData,tmpRoutine);
         end;
    end;

    function GetCallableType(const AData:TData):TType;
    begin
      if AData is TVariableCall then
         result:=TVariableCall(AData).Variable.VariableType
      else
      if AData is TDataCall then
         result:=TDataCall(AData).Routine
      else
      if AData is TTypeCall then
         result:=TTypeCall(AData).TheType
      else
         Exit(nil);

      result:=TChecker.GetFinalType(result);
    end;

    procedure SetCallParameters(const AData:TExpressionData);

      function CallError(const AIndex:Integer; const AParams:TNodes):String;
      //var tmpBadParam : TNode;
      //    tmpError : String;
      begin
        if AIndex=-1 then
           result:=TASTUtils.NameOf(AData,True)
        else
        begin
          {
          if tmpRoutine is TParametersType then
             tmpBadParam:=TParametersType(TDataCall(AData).Routine).Parameters[AIndex] as TData
          else
             tmpBadParam:=nil;
          }
          result:=//TASTUtils.NameOf(tmpBadParam,True)+' '+
                    TASTUtils.NameOf(AParams[AIndex],True);
        end;
      end;

      (*
      function IsForCounter(const AVariable:TVariable):Boolean;
      begin
        result:=(AVariable.Owner is TFor) and (TFor(AVariable.Owner).Counter=AVariable);
      end;

      function CanCheckInitialized(const AVariable:TVariable):Boolean;
      begin
        result:=(TChecker.GetClassTypeOf(AVariable)=nil) // <-- not a class field
                and
                (not IsTypeAutoInitialized(TChecker.GetFinalType(AVariable.VariableType)))
                and
                (not IsForCounter(AVariable)) // for counters are auto-initialized (always have value)
      end;
      *)

      // Check against using the same P parameter twice or more, if any of them are "out" not final
      procedure CheckOutParameterTwice(const ANodes,AParams:TNodes);
      var t,
          tt,
          tmp : Integer;
      begin
        tmp:=High(ANodes);

        if tmp>High(AParams) then
           tmp:=High(AParams);

        for t:=Low(ANodes) to tmp do
            if (ANodes[t] as TVariable).Clauses.Final then // read-only param
            begin
            (*
              if (AParams[t] is TVariable) then
                 if not TVariable(AParams[t]).Initialized then
                 if CanCheckInitialized(TVariable(AParams[t])) then
                 begin
          // Problem: Analyze if AParams[t] has been passed to an "out" parameter to initialize it

          // EASIER WAY: Just try to find any access to AParams[t] up until its declaration, to determine
          // if it has been initialized or not.

          // X: Integer
          // X:=123
          // Foo(X)  <--- search for X:=123 to avoid error

                   Error('Variable must be initialized before using it as parameter',AParams[t]);
                   Exit;
                 end;
                 *)
            end
            else // out param
            begin
              // Check duplication
              for tt:=Low(AParams) to High(AParams) do
                 if tt<>t then // do not check current
                 begin
                   if AParams[tt]=AParams[t] then
                   begin
                     Error(TErrors._OutParameterUsedTwice,AParams[t]);
                     Exit;
                   end;
                 end;

              // Check, must be a variable
              if not (AParams[t] is TVariable) then
              begin
                Error(TErrors._OutParameterMustBeVariable,AParams[t]);
                Exit;
              end;
            end;
      end;

    var tmp : TNodes;
        tmpBad:Integer;
        tmpOk : Boolean;
        tmpRoutine : TRoutine;
        OldCount : Integer;
        tmpPosition : TPosition;
        tmpType : TType;
        tmpError : String;
        tmpWrongParamCount : Boolean;
        tmpText : TErrors;
    begin
      AddPositionSymbol(AData,TSyntax._BeginParameters);

      GetToken;

      OldCount:=Positions.Count;

      tmp:=GetExpressions(TSyntax._EndParameters,AData.Owner,AData,tmpPosition);

      tmpType:=GetCallableType(AData);

      if tmpType=nil then
      begin
        tmp.Free(AOwner);

        {$IFDEF INTERNAL}
        InternalError(AData,'Cannot check call parameters: ');
        {$ENDIF}

        Exit;
      end;

      tmpOk:=False;

      if tmpType is TParametersType then
      begin
         tmpOk:=Checker.ValidParameters(AParent,
                           TParametersType(tmpType).Parameters,tmp,tmpBad);

         if tmpOk then
            CheckOutParameterTwice(TParametersType(tmpType).Parameters,tmp);
      end
      {$IFDEF INTERNAL}
      else
        InternalError(tmpType,'Call type has no parameters: ')
      {$ENDIF}
      ;

      if tmpOk then
         SetExpressionDataParameters(AOwner,AData,tmp)
      else
      if AData is TDataCall then
      begin
        if tmpType is TRoutine then
           tmpRoutine:=Finder.FindOverload(AOwner,TRoutine(tmpType),tmp)
        else
           tmpRoutine:=nil;

        if tmpRoutine=nil then
        begin
          tmpError:=CallError(tmpBad,tmp);

          tmpWrongParamCount:=Length(TParametersType(tmpType).Parameters)<>Length(tmp);

          tmp.Free(AData);
          Positions.DeleteFrom(OldCount);

          if tmpWrongParamCount then
             tmpText:=TErrors._WrongParamCount
          else
             tmpText:=TErrors._BadParameter;

          Error(Position,1,tmpText,tmpError);
        end
        else
        begin
          TDataCall(AData).Routine:=tmpRoutine;

          if TDataCall(AData).ResultValue is TVariable then
             TVariable(TDataCall(AData).ResultValue).VariableType:=tmpRoutine.Output;

          SetExpressionDataParameters(AOwner,AData,tmp);

          CheckOutParameterTwice(tmpRoutine.Parameters,tmp);
        end;
      end
      else
      begin
        tmp.Free(AOwner);

        {$IFDEF INTERNAL}
        InternalError(AData,'Cannot check call parameters: ');
        {$ENDIF}
      end;
    end;

    function IsMagic(out AData:TData; const ARoutine:TRoutine):Boolean;
    begin
      result:=Checker.TextIs(AToken,ARoutine.Name);

      if result then
         AData:=DirectCreateDataCall(AParent,ARoutine,True);
    end;

    function OwnerNeedsType:Boolean;
    begin
      result:=(AOwner is TVariable) and
              (TChecker.GetFinalType(TVariable(AOwner).VariableType) is TRoutine);
    end;

  var tmpTypeCall : TTypeCall;
      Old : TPosition;
      tmpType : TType;
      OldCount : Integer;
      tmpData : TData;
  begin
    if IsMagic(result,TMagic.MagicTypeOf) or
       IsMagic(result,TMagic.MagicNameOf) then // Magic TypeOf etc
    else
    begin
      Old:=Position;
      OldCount:=Positions.Count;

      result:=GetData(AParent,AOwner,AToken,Recursive,DoError);

      if result=nil then
      begin
        Positions.DeleteFrom(OldCount);
        Position:=Old;
        Exit;
      end;
    end;

    Old:=Position;

    while PeekSymbolGet(TSyntax._BeginArray) do
    begin
      // special case for generics specialized "Array Generic type.vidi"
      if result is TTypeCall then
      begin
        tmpType:=GetArrayType(AParent,AOwner,TTypeCall(result).TheType,Old);

        if tmpType<>nil then
        begin
          TTypeCall(result).TheType:=tmpType;

          if tmpType.Owner=AOwner then
             tmpType.Owner:=result;
        end;
      end
      else
      begin
        tmpData:=result;

        result:=GetArrayExpression(AParent,AOwner,result,True,Old);

        if result=nil then
        begin
          result:=tmpData; // <-- improve this bad workaround
          Exit; // <-- exit on error
        end;
      end;

      Old:=Position;
    end;

    if PeekIs(TSyntax._BeginParameters) then
    begin
      if result is TDataCall then
      begin
        //try
          SetCallParameters(TExpressionData(result));
        {
        except
          on Exception do
          begin
            result.Free;
            raise;
          end;
        end;
        }
      end
      else
      if result is TTypeCall then
      begin
        if TTypeCall(result).TheType is TRoutine then
        begin
          tmpTypeCall:=TTypeCall(result);

          result:=DoCreateCall(AOwner,TRoutine(TTypeCall(result).TheType),False); // ? True?

          ReplacePositionOf(tmpTypeCall,result);
          tmpTypeCall.Free;

          if result<>nil then
             SetCallParameters(TExpressionData(result));
        end
        else
          result:=ParseNewCasting(TTypeCall(result),Old);
      end
      else
      if TChecker.IsCallableVariable(result) then
      begin
        result:=VariableCall(AOwner,TVariable(result),TVariable(result).VariableType,False);

        if result<>nil then
           SetCallParameters(TExpressionData(result));
      end
      else
      begin
        if result is TSelf then
           result.Free;

        result:=nil;
        Error(Position,Length(AToken),TErrors._DataIsNotCall,AToken);
        Exit;
      end;
    end
    else
    if TChecker.IsDataFunctionType(result) then // <-- passing functions as variables/parameters
    begin
      result:=result;  // <-- ??
    end
    else
    if not OwnerNeedsType then
       TrySetRoutine(result);  // parameterless (5*4).AsText <-- Find AsText

    CheckAbstractCall(result);
  end;

  function CanAccessTypeLevel(const AData:TData):Boolean;
  begin
    // Is it a constant? (final variable)
    result:=(AData is TVariable) and TVariable(AData).Clauses.Final;

    if not result then
       result:=TChecker.IsSharedData(AData); // is it shared? (class variable/constant)
  end;

  function IsAbstractTypeCall(const ACall:TData):Boolean;
  var tmpType : TType;
  begin
    result:=False;

    if ACall is TDataCall then
    begin
      tmpType:=TDataCall(ACall).Routine;

      if tmpType is TRoutine then
         result:=TChecker.IsAbstract(TRoutine(tmpType));
    end;
  end;

  function GetDataSubMember(const AData:TData):TData;
  var tmpSub : TData;
      tmpMember : TMember;
      OldCount : Integer;

    procedure DoFreeMember;
    begin
      if result.Owner=tmpMember then
         result.Owner:=AOwner;

      Positions.DeleteFrom(OldCount);

      if tmpSub<>nil then
         if tmpSub.Owner=tmpMember then
            tmpSub.Free;

      tmpMember.Free;
    end;

    procedure DoError(const AError:TErrors; const APosition:TPosition; const ALength:Integer);
    var tmp : String;
    begin
      tmp:=TASTUtils.QualifiedNameOf(tmpSub);
      DoFreeMember;

      Error(APosition,ALength,AError,tmp);
    end;

    function IsTypeVariable(const ANode:TData):Boolean;
    begin
      result:=(ANode is TVariable) and (TVariable(ANode).VariableType=TChecker._Types[TChecker._Type]);
    end;

    function AccessingTypeLevel(const ALeft,ARight:TData):Boolean;
    begin
      result:=(ALeft is TTypeCall) or

              // checker.usage ABlock:Type  -> "for N in ABlock.Items"
              (
                IsTypeVariable(ALeft) and
                (not (ARight.Owner=TVariable(ALeft).VariableType) )
              );
    end;

  var ErrorPosition : TPosition;
      ErrorLength : Integer;
      tmpS : String;
  begin
    result:=AData;

    repeat
      PositionToAdd:=Position;

      if (not PeekIs(TSyntax._RangeDelimiter)) and PeekSymbolGet(TSyntax._DataMember) then
      begin
        OldCount:=Positions.Count;

        tmpMember:=TMember.Create;
        tmpMember.Owner:=AOwner;

        TryChangeOwnership(result,AOwner,tmpMember);

        AddPositionSymbol(tmpMember,TSyntax._DataMember);

        tmpMember.Data:=result;

        PositionToAdd:=Position;
        ErrorPosition:=PositionToAdd;

        tmpS:=GetToken;

        ErrorLength:=Length(tmpS);

        if tmpS='' then
           tmpSub:=nil
        else
        begin
          tmpSub:=GetDataAndIndex(result,tmpMember,tmpS,False);
        end;

        if tmpSub=nil then
        begin
          DoFreeMember;

          {
          if result.Owner=tmpMember then
             result.Owner:=AOwner;

          Positions.DeleteFrom(OldCount);

          tmpMember.Free;
          }

          Unknown(tmpS);
          break // pass error
        end
        else
        if AccessingTypeLevel(result,tmpSub) and (not CanAccessTypeLevel(tmpSub)) then
           DoError(TErrors._CannotAccessNonShared,ErrorPosition,ErrorLength)
        else
        if (result is TTypeCall) and IsAbstractTypeCall(tmpSub) then
           DoError(TErrors._CannotCallAbstract,ErrorPosition,ErrorLength)
        else
        begin
          // FinishMember
          tmpMember.Member:=tmpSub;
          result:=tmpMember;
        end;
      end
      else
        break;

    until False;
  end;

begin
  result:=GetDataAndIndex(AParent,AOwner,AToken,True);

  if result=nil then
  begin
    if DoError then
       Unknown(AToken);
  end
  else
     result:=GetDataSubMember(result);
end;

function TExpressionParser.FinishMember(const AParent,AOwner:TNode; const AData:TData):TMember;
var tmpMember : TData;
    Old : TPosition;
    tmpS : String;
begin
  result:=TMember.Create;

  Old:=PositionToAdd;

  PositionToAdd:=Position;

  tmpS:=GetToken;

  if tmpS='' then
     tmpMember:=nil
  else
     tmpMember:=GetData(AData,result,tmpS,False,True);

  if tmpMember=nil then
  begin
    result.Free;
    result:=nil;
  end
  else
  begin
    result.Owner:=AOwner;

    AddPosition(result,Old,TSyntax._DataMember,TPositionStyle.Symbol);

    if AData.Owner=AOwner then
       AData.Owner:=result;

    result.Data:=AData;
    result.Member:=tmpMember;
  end;
end;

{$IFDEF EXTRA_CHECKS}
procedure TExpressionParser.CheckUselessOperand(const AOperand:TOperand);

  function SameLeftAndRight:Boolean;
  begin
    result:=TCodeCompare.SameCode(AOperand.Left,AOperand.Right);

//    if not result then
//       result:=TEvaluateCompile.SameValue(AOperand.Left,AOperand.Right);
  end;

begin
  if SameLeftAndRight then
  begin
    {if AOperand is TAddition then
    begin
      // TODO:  x + -x    // -x + x
    end
    else
    }
    if AOperand is TSubtraction then   // x-x
       Error(TErrors._ExpressionIsZero,AOperand)
    else
    if AOperand is TDivision then  // x/x
       Error(TErrors._ExpressionIsOne,AOperand)
    else
    if (AOperand is TIsEqual) or
       (AOperand is TIsLowerOrEqual) or
       (AOperand is TIsGreaterOrEqual) then  // x=x
       Error(TErrors._ExpressionIsAlwaysTrue,AOperand)
    else
    if AOperand is TCompareLogical then  // x<x
       Error(TErrors._ExpressionIsAlwaysFalse,AOperand)
    else
    // if AOperand is TLogicalAnd then // TODO: Logical and or xor
  end;
end;
{$ENDIF}

function TExpressionParser.FinishOperand(const AParent,AOwner:TNode; const ALeft:TData):TData;

  function CheckOperandLeftRight(const AOperand:TOperand; const APosition:TPosition):Boolean;
  begin
    result:=(AOperand.Left<>nil) and (AOperand.Right<>nil);

    if result then
    begin
      result:=Checker.CheckOperand(AParent,AOperand);

      if not result then
         Error(APosition,
               Length(TASTUtils.OperandToString(AOperand)),
               TErrors._IncompatibleLeftRight,AOperand);
    end;
  end;

  function ParseOperand(const Old:TPosition; var AOperand:TOperand; const ALeft:TData):Boolean;
  var tmp : TOperand;
  begin
    AOperand.Left:=ALeft;

    if not (ALeft is TVariable) then
       if ALeft.Owner=AOperand.Owner then
          ALeft.Owner:=AOperand;

    AOperand.Right:=DoGetExpression(AParent,AOperand);

    result:=AOperand.Right<>nil;

    if result then
    begin
      if AOperand is TDivision then
         if TEvaluateCompile.IsInteger(AOperand.Right,0) then
            Error(TErrors._DivisionByZero,AOperand);

      if AOperand.Right is TOperand then //  true / -0.008 - 1
      begin
        if CheckOperandLeftRight(TOperand(AOperand.Right),Old) then // -0.008 - 1
        begin
          tmp:=AOperand;
          AOperand:=Checker.FixPrecedence(AOperand);

          if AOperand<>tmp then
             if not CheckOperandLeftRight(tmp,Old) then  //  true / -0.008
             begin
               result:=False;
               Exit;
             end;
        end
        else
        begin
          result:=False;
          Exit;
        end;
      end;

      // Check type compatibility *after* fixing operator precedence
      result:=CheckOperandLeftRight(AOperand,Old);
    end;
  end;

var Old : TPosition;
    OldCount : Integer;
begin
  Old:=Position;
  OldCount:=Positions.Count;

  result:=GetOperand;

  if result=nil then
     result:=TryRangeOrMember(AParent,AOwner,ALeft)
  else
  begin
    PositionToAdd:=Old;

    if result.Owner=nil then
       result.Owner:=AOwner;

    if ParseOperand(Old,TOperand(result),ALeft) then
    {$IFDEF EXTRA_CHECKS}
       CheckUselessOperand(TOperand(result))
    {$ENDIF}
    else
    begin
      Positions.DeleteFrom(OldCount);

      result.Free;
      result:=nil;
    end
  end;
end;

function TExpressionParser.Error(const AError:TErrors; const AParam: String):TNodeError;
begin
  result:=Error(Position,Length(AParam),AError,AParam);
end;

function TExpressionParser.ExpectSymbol(const AOwner:TNode; const ASymbol:String):Boolean;
begin
  PositionToAdd:=Position;

  result:=GetSymbol(AOwner,ASymbol);

  if not result then
     Error(Position,Length(ASymbol),TErrors._Expected,ASymbol);
end;

// +Foo -Foo
function TExpressionParser.CreateUnarySign(const AParent,AOwner:TNode; const Positive:Boolean):TUnarySign;
var tmp : String;
begin
  result:=TUnarySign.Create;
  result.Owner:=AOwner;

  if Positive then
     tmp:=TSyntax._Symbol_Add
  else
     tmp:=TSyntax._Symbol_Subtract;

  AddPosition(result,PositionToAdd,tmp,TPositionStyle.Symbol);

  result.Positive:=Positive;

  result.Expression:=Get(AParent,result);

  if result.Expression=nil then
  begin
    result.Free;
    result:=nil;
  end;
end;

// 0..9      min..max*2
function TExpressionParser.GetRange(const AParent,AOwner:TNode; const AMin:TData):TRange;
var OldCount : Integer;

  procedure Clean(const ARange:TRange);
  begin
    Positions.DeleteFrom(OldCount);

//    ARange.Min:=nil;
    ARange.Free;
  end;

begin
  {$IFDEF INTERNAL}
  if AMin=nil then
     InternalError(nil,'Min Data is nil at create range');
  {$ENDIF}

  result:=TRange.Create;
  result.Owner:=AOwner;

  if not (AMin is TBoolean) then // <-- not for True False
     TryChangeOwnership(AMin,AMin.Owner,result);

  result.Min:=AMin;

  OldCount:=Positions.Count;

  if ExpectSymbol(result,TSyntax._RangeDelimiter) then
  begin
    result.Max:=GetExpression(AParent,result);

    if result.Max=nil then
    begin
      Error(TErrors._WrongExpression,TASTUtils.NameOf(result,True));

      Clean(result);
      result:=nil;
    end
    else
    if not Checker.CompatibleAssign(result.Min,result.Max,result) then
    begin
      Error(TErrors._IncompatibleLeftRight,TASTUtils.NameOf(result,True));

      Clean(result);
      result:=nil;
    end;
  end
  else
  begin
    Clean(result);
    result:=nil;
  end;
end;

function TExpressionParser.DoGetExpression(const AParent,AOwner:TNode; const DoError:Boolean):TData;

  function CreateNewGroup(const P:TPosition; out IsError:Boolean):TData;
  var tmp : TData;
      OldCount : Integer;
  begin
    IsError:=False;

    if PeekSymbolGet(TSyntax._BeginParameters) then
    begin
      result:=TGroup.Create;
      result.Owner:=AOwner;

      OldCount:=Positions.Count;

      PositionToAdd:=P;
      AddPositionSymbol(result,TSyntax._BeginParameters);

      tmp:=Get(AParent,result,False);

      if tmp=nil then
      begin
        Position:=P;
        Positions.DeleteFrom(OldCount);

        IsError:=True;
        result.Free;
        result:=nil;
      end
      else
      begin
        TGroup(result).Expression:=tmp;

        if ExpectSymbol(result,TSyntax._EndParameters) then
           if PeekOperand then
              if PeekIs(TSyntax._Reserved_condition) then
                 Exit
              else
                 result:=FinishOperand(AParent,AOwner,result);
      end;
    end
    else
      result:=nil;
  end;

  type
    TUnaryType=(None,Positive,Negative);

  function CreateUnary(const AType:TUnaryType; const Old:TPosition):TData;
  begin
    PositionToAdd:=Old;
    result:=CreateUnarySign(AParent,AOwner,AType=TUnaryType.Positive);
  end;

  function Negate(const AData:TData):TData;
  begin
    result:=AData;

    if result is TInteger then
       {$IFOPT Q+}
       {$DEFINE OVERFLOW_ON}
       {$Q-}
       {$ELSE}
       {$UNDEF OVERFLOW_ON}
       {$ENDIF}

       TInteger(result).Value:= -TInteger(result).Value

       {$IFDEF OVERFLOW_ON}
       {$Q+}
       {$UNDEF OVERFLOW_ON}
       {$ENDIF}
    else
    if result is TFloat then
       TFloat(result).Value:= -TFloat(result).Value;
  end;

  function TryGetNumber(const Old:TPosition; out AText:String):TData;
  var tmp : TUnaryType;
      Old2 : TPosition;
  begin
    if PeekSymbolGet(TSyntax._UnaryPositive) then
       tmp:=TUnaryType.Positive
    else
    if PeekSymbolGet(TSyntax._UnaryNegative) then
       tmp:=TUnaryType.Negative
    else
       tmp:=TUnaryType.None;

    Old2:=Position;

    AText:=GetTokenNumber;

    if AText='' then
       result:=nil
    else
    begin
      PositionToAdd:=Old2;

      result:=NumberToNode(tmp=TUnaryType.Negative,AText);

      if result=nil then
      begin
        PositionToAdd:=Old;
        AText:='';
      end
      else
        result.Owner:=AOwner;
    end;

    if tmp<>TUnaryType.None then
       if result=nil then
          result:=CreateUnary(tmp,PositionToAdd)
       else
       begin
         if Old2.Position>Old.Position+1 then
         begin
           PositionToAdd:=Old;

           if tmp=TUnaryType.Positive then
              AddPositionSymbol(nil,TSyntax._UnaryPositive)
           else
              AddPositionSymbol(nil,TSyntax._UnaryNegative);

           PositionToAdd:=Old2;
         end
         else
         begin
           // Add +- symbol again to Text

           if tmp=TUnaryType.Positive then
              AText:=TSyntax._Symbol_Add+AText
           else
              AText:=TSyntax._Symbol_Subtract+AText;

           if result is TNumber then
              TNumber(result).Text:=AText;

           PositionToAdd:=Old;
         end;
       end;
  end;

var T : String;
    Old,Old2 : TPosition;
    tmp : TData;
    tmpError : Boolean;
    OldCount : Integer;
    tmpMember : TMember;
begin
  Old:=Position;

  result:=CreateNewGroup(Old,tmpError);

  if (result<>nil) or tmpError then
     Exit;

  (* See TryGetLambdaFunction override
  if PeekSymbolGet(TSyntax._BeginBlock) then
  begin
    // Allow blocks inside any expression inline???
    // eg: 3*5 + Foo.Xyz - { if a=b return 4 else return 3 }
    result:=NewDataCall(AOwner);
    TDataCall(result).Routine:=TRoutine.Create;
    DoFillBlock(TDataCall(result).Routine);
  end
  else
  *)
  if PeekSymbolGet(TSyntax._BeginArray) then
  begin
    result:=GetArrayExpression(AParent,AOwner,nil,True,Old);

    if result<>nil then
    begin
      tmp:=FinishOperand(AParent,AOwner,result);

      if tmp=nil then
      begin
        FreeIfNotOwned(result,AOwner);
        result:=nil;
      end
      else
        result:=tmp;
    end;
  end
  else
  if PeekIsGet(TSyntax._Reserved_not) then // not Foo
  begin
    PositionToAdd:=Old;
    AddPositionSymbol(result,TSyntax._Reserved_not);

    result:=TUnaryNot.Create;
    result.Owner:=AOwner;

    TUnaryNot(result).Expression:=Get(AParent,result);
  end
  else
  begin
    PositionToAdd:=Position;

    if not GetBoolean(result) then
    begin
      if GetTokenText(T) then
      begin
        result:=TText.Create(T);

        if result is TText then // do not check "if T<>''" here ! (empty text is valid !)
        begin
          AddPosition(result,PositionToAdd,T,TPositionStyle.Literal);

          if result.Owner=nil then
             result.Owner:=AOwner;
        end;
      end
      else
      begin
        result:=TryGetNumber(Old,T);

        if result<>nil then
        begin
          if result.Owner=nil then
             result.Owner:=AOwner;

          // Not good solution:
          //tmpL:=Position.Position-PositionToAdd.Position;

          if T<>'' then // do not check "if T<>''" here ! (empty text is valid !)
             AddPosition(result,PositionToAdd,T,TPositionStyle.Literal);
        end;
      end;
    end;

    Old2:=Position;
    OldCount:=Positions.Count;

    if (not PeekIs(TSyntax._RangeDelimiter)) and
        PeekSymbolGet(TSyntax._DataMember) then
    begin
      if result=nil then
         Error(Position,Length(T),TErrors._WrongExpression)
      else
      begin
        PositionToAdd:=Old2;

        tmpMember:=FinishMember(AParent,AOwner,result);

        if tmpMember=nil then
        begin
          // FreeIfNotOwned(result,AOwner);

          // Suspicious, maybe it is not enough to check for AOwner
          if result.Owner=AOwner then
             result.Free;

          result:=nil;
        end
        else
          result:=tmpMember;
      end;
    end
    else
    if result=nil then
       if AOwner<>nil then
       begin
         PositionToAdd:=Position;

         T:=GetToken;

         if T<>'' then
            result:=GetDataMember(AParent,AOwner,T,DoError); // any identifier or member: Foo.Bar.Tau
       end;

    if result=nil then
    begin
      Position:=Old2;
      Positions.DeleteFrom(OldCount);

      if DoError then
         Error(TErrors._WrongExpression,AOwner);
    end
    else
    begin
      if result.Owner=nil then
         result.Owner:=AOwner;

      Old:=Position;

      if PeekIs(TSyntax._RangeDelimiter) then
      begin
        PositionToAdd:=Old;

        if not (AOwner is TOperand) then // 1+1..2..5
        begin
          result:=GetRange(AParent,AOwner,result);

          if result=nil then
             Exit;
        end;
      end;

      if PeekOperand then
         if PeekIs(TSyntax._Reserved_condition) then
            Exit
         else
         begin
           tmp:=FinishOperand(AParent,AOwner,result);

           if tmp=nil then
           begin
             FreeIfNotOwned(result,AOwner);

             result:=nil;
           end
           else
              result:=tmp;
         end;
    end;
  end;
end;

class function TExpressionParser.IsMagicTrueFalse(const AData:TData):Boolean;
begin
  result:=(AData=TMagic.MagicTrue) or (AData=TMagic.MagicFalse);
end;

// Foo ? A : B
function TExpressionParser.CreateCondition(const AParent,AOwner:TNode; const ACondition:TData):TCondition;
var OldCount : Integer;
    tmpOk : Boolean;
begin
  result:=TCondition.Create;
  result.Owner:=AOwner;

  AddPositionSymbol(result,TSyntax._Reserved_condition);

  // Comp::=Ascending ?   sort.vidi
  if ACondition.Owner=AOwner then
     if not (ACondition is TVariable) then  //  DO NOT Re-owner "A" -> F:FunctionType:=(A:Boolean) { A? 'abc' : 'xyz' }
        ACondition.Owner:=result; // <-- mem leak

  result.Condition:=ACondition;

  PositionToAdd:=Position;

  OldCount:=Positions.Count;

  result.Left:=Get(AParent,result);

  tmpOk:=(result.Left<>nil) and
         ExpectSymbol(result,TSyntax._TypeDelimiter);

  if tmpOk then
  begin
    OldCount:=Positions.Count;

    result.Right:=Get(AParent,result);

    tmpOk:=result.Right<>nil;
  end;

  if not tmpOk then
  begin
    Positions.DeleteFrom(OldCount);
    result.Free;
    result:=nil;
  end;

  {
  if result.Left=nil then
  begin
    Positions.DeleteFrom(OldCount);

    result.Free;
    result:=nil;
  end
  else
  if ExpectSymbol(result,TSyntax._TypeDelimiter) then
  begin
    OldCount:=Positions.Count;

    result.Right:=Get(AParent,result);

    if result.Right=nil then
    begin
      Positions.DeleteFrom(OldCount);

      result.Free;
      result:=nil;
    end;
  end;
  }
end;

procedure TExpressionParser.Clear;
begin
  Context:=nil;
  Errors.Clear;
end;

// Returns type of logical expression (TBoolean, TLogical, etc)
procedure TExpressionParser.Init(const AText: String);
begin
  inherited;
  Clear;
end;

function TExpressionParser.TryRangeOrMember(const AParent,AOwner:TNode; const AData:TData):TData;
begin
  if PeekIs(TSyntax._RangeDelimiter) then
     result:=GetRange(AParent,AOwner,AData) // (1+1)..(2*5)
  else
  begin
    PositionToAdd:=Position;

    if PeekSymbolGet(TSyntax._DataMember) then
       result:=FinishMember(AParent,AOwner,AData) // (a+b).AsText
    else
       result:=AData;
  end;
end;

function TExpressionParser.IsValidCondition(const ACondition:TData; const AOwner:TNode;
                                   const ACount:Integer; const APosition:TPosition):Boolean;
var tmp : Boolean;
    tmpError : TErrors;
begin
  result:=False;

  if TChecker.IsLogical(ACondition) then
  begin
    if ACondition is TVariable then
       result:=True
    else
    if (not IsMagicTrueFalse(ACondition)) //and TEvaluateCompile.Evaluable(ACondition,False) then
        and
        TEvaluateCompile.EvaluableAsBoolean(ACondition,tmp) then
    begin
      if tmp then
         tmpError:=TErrors._ConditionAlwaysTrue
      else
         tmpError:=TErrors._ConditionAlwaysFalse;

      ErrorFree(ACondition,AOwner,ACount,APosition,tmpError);
    end
    else
       result:=True;
  end
  else
    ErrorFree(ACondition,AOwner,ACount,APosition,TErrors._ConditionNotLogical);
end;

function TExpressionParser.Get(const AParent,AOwner:TNode; const DoError:Boolean):TData;

var
  OldPos : Integer;

  // Pending to test with other kinds of AOwner (routines? etc?)
  function IsItemOf(const ANode,AOwner:TNode):Boolean;
  begin
    if AOwner is TType then
       result:=TType(AOwner).Items.IndexOf(ANode)<>-1
    else
       result:=False;
  end;

  procedure TryFree(const AData:TData);
  begin
    Positions.DeleteFrom(OldPos);

    if AData.Owner=AOwner then
       if not IsItemOf(AData,AOwner) then
          AData.Free
  end;

var Old,
    OldCond : TPosition;

    tmp : TData;
begin
  OldCond:=Position;
  OldPos:=Positions.Count;

  result:=DoGetExpression(AParent,AOwner,DoError);

  if result<>nil then
  begin
    Old:=Position;

    if PeekSymbolGet(TSyntax._Reserved_condition) then
    begin
      if IsValidCondition(result,AOwner,OldPos,OldCond) then
      begin
        PositionToAdd:=Old;

        tmp:=CreateCondition(AParent,AOwner,result);

        if tmp=nil then
        begin
          TryFree(result);
          result:=nil;
        end
        else
           result:=tmp;
      end
      else
        result:=nil; // TryFree(result) ??
    end
    else
    begin
      tmp:=TryRangeOrMember(AParent,result.Owner,result);

      if tmp=nil then
      begin
        TryFree(result);
        result:=nil;
      end
      else
         result:=tmp;
    end;
  end;
end;

function TExpressionParser.Error(const APosition:TPosition;
                                 const ALength:Integer;
                                 const AError:TErrors;
                                 const AParam1,AParam2:String;
                                 const ANode:TNode):TNodeError;
var tmp : String;
begin
  {$IFDEF INTERNAL}
  if TErrorTexts.TestMode then
     TErrorTexts.Tested[AError]:=True;
  {$ENDIF}

  tmp:=TErrorTexts.ReplaceParams(AError,AParam1,AParam2);

  result:=TNodeError.From(tmp,ModuleName,ModulePath,APosition,ALength);

  if Assigned(OnError) then
  begin
    Errors.Add(result);
    OnError(Self,result);
  end
  else
    Raise_Exception(result);
end;

function TExpressionParser.Error(const APosition:TPosition;
                                 const ALength:Integer;
                                 const AError:TErrors;
                                 const ANode:TNode):TNodeError;
var //tmp : TNodePosition;
    tmpS : String;
begin
  tmpS:=TASTUtils.NameOf(ANode,True);

{  if Positions.Find(ANode,tmp) then
     result:=Error(tmp.Position,AError,tmpS)
  else}
     result:=Error(APosition,ALength,AError,tmpS);
end;

function TExpressionParser.Error(const AError:TErrors;
                                 //const ALength:Integer;
                                 const ANode:TNode):TNodeError;
begin
  result:=Error(Position,10, // <-- 10 temporary
  AError,ANode);
end;

end.
