unit Bee.Parser;
{$IFDEF NEXTGEN}
{$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

{$DEFINE EXTRA_CHECKS}

uses
  Sys, AST, Checker.AST, Position, Parser,
  Find.AST, Expression.Parser, Usage, Module;

type
  TOnFindModule=procedure(const AName:String; out ANode:TNamedType) of object;
  TOnParsed=procedure(const Sender:TParser; const ANode:TNode) of object;

  TBee=class(TExpressionParser)
  private
    function AddToNodes(const AOwner:TNode; var Nodes:TNodes; const Node:TNode; const OldPos:Integer):Boolean;
    procedure AddNewItem(const AOwner,ANode:TNode; var AItems:TNodes; const OldPos:Integer);
    function CheckAssign(const ALeft,ARight:TData; const CheckFinal:Boolean; const APosition:TPosition):Boolean;

    function CreateReturn(const AOwner:TNode; const AValue:TData; const HasPosition:Boolean):TReturn;
    function DoCreateReturn(const AOwner:TNode; const HasPosition:Boolean):TReturn;
    function DoParse(const AText:String):TNode;

    function Expect(const AToken:String):Boolean;

    procedure FillBlock(const ABlock:TType);
    function FinishModule(const AName,AText:String):TNamedType;
    procedure FinishReturn(const AOwner:TNode; const AReturn:TReturn;
                           const AValue:TData);

    function GetBlock(const AParent:TNode):TBlockStatement;
    function GetParsedModule(const AName:String):TNamedType;
    function GetVariable(const AParent,AOwner:TNode;
                         const AName:String; const AType:TType;
                         const AClauses:TClauses):TVariable;

    function ParseBlock(const AParent:TType; var Clauses:TClauses):TNode;
    procedure PerformChecks(const ANode:TNode);
    procedure SetVariableData(const AParent:TNode; const AVariable:TVariable);
    procedure TryClearMagicTypes(const AName:String);
  protected
    function Parse(const AText:String):TNode;
    function TryGetLambdaFunction(const AParent,AOwner:TNode):TTypeCall; override;
  public
    ParserPath : String;

    class var
      OnFindModule : TOnFindModule;

    var
    OnParsed : TOnParsed;

    Usages : TNodeUsages;

    function DirectParseModule(const APath,AName,AText:String):TNamedType;

    class function FindModule(const AName:String):TNamedType; static;
    class function FindModuleStamp(const AName:String; out AStamp:TFileStamp):TNamedType; static;

    function ParseModule(const APath,AName:String):TNamedType; overload;
    function ParseModule(const APath,AName,AText:String):TNamedType; overload;
    class function ParseExpression(const AParent:TNode; const AText:String):TData;
  end;

implementation

uses
  Syntax, Exceptions, IO, Evaluator, SysUtils, Map,
  Checker.Usage, Checker.Shared, Evaluator.CompileTime, Utils.AST,
  Constants, Magic, Compare.AST, Creator.AST;

type
  TAvoidEmpty=class(TNode)
  end;

{ TBee }

function TBee.CheckAssign(const ALeft,ARight:TData; const CheckFinal:Boolean; const APosition:TPosition):Boolean;

  procedure CheckBounds(const AValue:Int64; const ARange:TRange);

    procedure RangeError(const AError:TErrors; const ALimit:Integer);
    var tmp : String;
    begin
      tmp:=IntToStr(AValue);
      Error(APosition,Length(tmp),AError,tmp,IntToStr(ALimit));
    end;

  var tmpMin,
      tmpMax : Integer;
  begin
    tmpMin:=TEvaluate.AsInteger(ARange.Min);
    tmpMax:=TEvaluate.AsInteger(ARange.Max);

    if AValue<tmpMin then
       RangeError(TErrors._Underflow,tmpMin)
    else
    if AValue>tmpMax then
       RangeError(TErrors._Overflow,tmpMax);
  end;

  procedure AssignmentError;
  var tmp : TType;
      tmpS: String;
  begin
    tmp:=Checker.GetDataType(nil,ARight);

    if tmp=nil then
       tmpS:='?'+TASTUtils.NameOf(ARight,True)
    else
    if tmp is TArrayType then
       tmpS:=TASTUtils.NameOf(TArrayType(tmp).TheType,False)+TSyntax._BeginArray+TSyntax._EndArray
    else
       tmpS:=TASTUtils.TypeNameOf(tmp);

    Error(APosition,Length(tmpS),TErrors._CannotAssign,tmpS,TASTUtils.NameOf(ALeft,True));
  end;

  procedure CheckIntegerBounds(const AVariable:TVariable);
  var tmp : TType;
  begin
    tmp:=AVariable.VariableType;

    // TODO: Evaluate.AsInteger(ARight)
    if (tmp is TIntegerClass) and (ARight is TInteger) then
       CheckBounds(TInteger(ARight).Value,TIntegerClass(tmp).Size.Range);
  end;

  procedure ErrorLeft(const APosition:TPosition; const AError:TErrors);
  begin
    Error(APosition,
          Length(TASTUtils.NameOf(ALeft,False)),
          AError,ALeft);
  end;

begin
  if CheckFinal and TChecker.IsFinalData(ALeft) then
  begin
    ErrorLeft(APosition,TErrors._CannotModifyFinal);

    result:=False;
    Exit;
  end;

  // Check self-assignment A:=A
  if (ALeft=ARight) or (ALeft=TEvaluateCompile.AsData(ARight,True)) then
  begin
    ErrorLeft(APosition,TErrors._SameDataBothSides);

    result:=False;
    Exit;
  end;

  result:=Checker.CompatibleAssign(ALeft,ARight,nil);

  if result then
  begin
    // Overflow check for Integer
    if ALeft is TVariable then
       CheckIntegerBounds(TVariable(ALeft));
  end
  else
    AssignmentError;
end;

procedure TBee.SetVariableData(const AParent:TNode; const AVariable:TVariable);

  // X::=[ [1,2,3], [4,5,6] ]
  function ArrayTypeFromArrayExpression(const AExp:TArrayExpression; const AType:TType):TArrayType;
  var tmp : TArrayType;
  begin
    result:=NewArrayType(AVariable,AType);

    if AExp.Parameters<>nil then
       if AExp.Parameters[0] is TArrayExpression then
       begin
         tmp:=result;
         result:=ArrayTypeFromArrayExpression(TArrayExpression(AExp.Parameters[0]),result);
         tmp.Owner:=result;
       end;
  end;

  procedure InferType;
  var tmpType : TType;
      tmpData : TData;
  begin
    tmpData:=TEvaluateCompile.AsData(AVariable.ValueData,True);

    if tmpData is TTypeCall then  // Metadata
       tmpType:=TChecker._Types[TChecker._Type] // Type // TTypeCall(tmpData).TheType
    else
       tmpType:=TChecker.GetDataType(AParent,AVariable.ValueData);

    if tmpType<>nil then
    begin
      if (AVariable.ValueData is TArrayExpression) and
         (TArrayExpression(AVariable.ValueData).Data=nil) then
         AVariable.VariableType:=ArrayTypeFromArrayExpression(TArrayExpression(AVariable.ValueData),tmpType)
      else
         AVariable.VariableType:=tmpType;

      AVariable.TypeInferred:=True;
    end;
  end;

var tmpData : TData;
    Old : TPosition;
    tmpPosCount: Integer;
begin
  Old:=Position;
  tmpPosCount:=Positions.Count;

  tmpData:=GetExpression(AParent,AVariable,False);

  if tmpData=nil then
     tmpData:=TryGetLambdaFunction(AParent,AVariable);

  if tmpData=nil then
     Error(Old,10,TErrors._EmptyItem,nil)
  else
  begin
    if AVariable.Clauses.Final then
       if not TEvaluateCompile.Evaluable(tmpData,False) then
       begin
         ErrorFree(tmpData,AVariable,tmpPosCount,Old,TErrors._ExpressionNotEvaluable);
         Exit;
       end;

    if AVariable.VariableType=nil then
    begin
      // TODO: Strict type check of (multi-dim) array elements,
      // to be sure InferType is possible:
      // X::= [ 1, 'abc' ] <-- SomeThing[] ?

      AVariable.ValueData:=tmpData;

      // infer type:   Foo::=123
      if AVariable.ValueData<>nil then
         InferType;
    end
    else
    if CheckAssign(AVariable,tmpData,False,Old) then
    begin
      // TODO: Strict type check of (multi-dim) array elements:
      AVariable.ValueData:=tmpData;
    end
    else
    begin
      Positions.DeleteFrom(tmpPosCount);
      FreeIfNotOwned(tmpData,AVariable);
    end;

    AVariable.Initialized:=AVariable.ValueData<>nil;
  end;
end;

function TBee.GetVariable(const AParent,AOwner:TNode;
                          const AName:String;
                          const AType:TType;
                          const AClauses:TClauses):TVariable;
begin
  result:=CreateVariable(AOwner,AName,AClauses,AType);

  AddPosition(result,AName);

  if GetSymbol(result,TSyntax._Assignment) then
     SetVariableData(AParent,result);
end;

function FindReturnBlock(const AParent:TNode; out AOut:TType):Boolean;

  function FindBlockStatement(ANode:TNode):TBlockStatement;
  begin
    repeat
      if (ANode is TBlockStatement) and (ANode.Owner<>nil) and (ANode.Owner is TNamedType) then
          Exit(TBlockStatement(ANode))
      else
          ANode:=ANode.Owner;

    until ANode=nil;

    result:=nil;
  end;

var tmp : TRoutine;
begin
  tmp:=TChecker.GetRoutineOf(AParent);

  result:=tmp<>nil;

  if result then
     AOut:=tmp.Output
  else
  begin
    AOut:=nil;
    result:=FindBlockStatement(AParent)<>nil;
  end;
end;

procedure TBee.FinishReturn(const AOwner:TNode; const AReturn:TReturn;
                            const AValue:TData);
var tmpOut : TType;
begin
  AReturn.Value:=AValue;

  if FindReturnBlock(AOwner,tmpOut) then
  begin
    if AValue=nil then
    begin
      if tmpOut<>nil then
         Error(TErrors._ReturnNeedsType,TASTUtils.NameOf(tmpOut,True));
    end
    else
    begin
      {$IFDEF INTERNAL}
      if tmpOut=nil then
         InternalError(AValue,'Return must be empty')
      else
      {$ENDIF}
      if not Checker.CompatibleAssign(tmpOut,nil,AValue) then
         Error(TErrors._IncompatibleType,AValue);
    end;
  end
  {$IFDEF INTERNAL}
  else
     InternalError(AReturn,'Cannot find Return parent method')
  {$ENDIF}
  ;
end;

function TBee.DoCreateReturn(const AOwner:TNode; const HasPosition:Boolean):TReturn;
begin
  result:=TReturn.Create;
  result.Owner:=AOwner;

  if HasPosition then
     AddPositionKeyword(result,TSyntax._Reserved_return);
end;

function TBee.CreateReturn(const AOwner:TNode; const AValue:TData; const HasPosition:Boolean):TReturn;
begin
  result:=DoCreateReturn(AOwner,HasPosition);

  if AValue<>nil then
     TryChangeOwnership(AValue,AOwner,result);

  FinishReturn(AOwner,result,AValue);
end;

function TBee.Expect(const AToken:String):Boolean;
begin
  result:=PeekIsGet(AToken);

  if not result then
     Error(TErrors._Expected,AToken);
end;

function TBee.FinishModule(const AName,AText:String):TNamedType;

  function GetFileStamp:TFileStamp;
  var tmp : String;
  begin
    tmp:=CombineFile(ModulePath,ModuleName+TVidiConstants.Extension);

    if FileExists(tmp) then
    begin
      result.FileSize:=FileSize(tmp);
      result.LastModified:=FileLastModified(tmp);
    end
    else
    begin
      result.FileSize:=0;
      result.LastModified:=0;
    end;
  end;

  function DoTryFinishModule:TNamedType;
  begin
    result:=Parse(AText) as TNamedType;

    Positions.Sort;

    result.Name:=AName;

    Modules.Add(result,Positions,Usages,GetFileStamp);

    Inc(TModules.ParsedLines,Position.Line);

    if Assigned(OnParsed) then
       OnParsed(Self,result);
  end;

begin
  Positions:=TNodePositions.Create;
  Usages:=TNodeUsages.Create;

  try
    result:=DoTryFinishModule;
  finally
    Positions:=nil;
    Usages:=nil;
  end;
end;

procedure TBee.TryClearMagicTypes(const AName:String);
begin
  if Checker.TextIs(AName,TModules._SystemModule) then
     Checker.ClearMagicTypes;
end;

class function TBee.FindModule(const AName:String):TNamedType;
begin
  result:=Modules.Find(AName);

  if result=nil then
     if Assigned(OnFindModule) then
        OnFindModule(AName,result);
end;

class function TBee.FindModuleStamp(const AName:String; out AStamp:TFileStamp):TNamedType;
begin
  result:=Modules.Find(AName);

  if result<>nil then
     AStamp:=Modules.FileStamps[Modules.IndexOf(AName)];
end;

// Parse full AText
function TBee.DirectParseModule(const APath,AName,AText:String):TNamedType;
begin
  ModulePath:=APath;
  ModuleName:=AName;

  TryClearMagicTypes(AName);

  result:=FinishModule(AName,AText);
end;

// Parse full AText
function TBee.ParseModule(const APath,AName,AText:String):TNamedType;
begin
  result:=FindModule(AName);

  if result=nil then
     result:=DirectParseModule(APath,AName,AText);
end;

// Parse full AFileName from file
function TBee.ParseModule(const APath,AName:String):TNamedType;

  function CheckFileExists(const AFileName:String; out ModulePath:String):Boolean;
  begin
    if DoFileExists(APath,AFileName) then
       ModulePath:=APath
    else
    if DoFileExists(ParserPath,AFileName) then
       ModulePath:=ParserPath
    else
       Exit(False);

    result:=True;
  end;

  // Search dotted Foo.Bar.Lee type inside AType module.
  // Example:  "with Total.Animals.SubClass"
  function FindDottedTypedIn(const AType:TNamedType; Name:String):TNamedType;
  var i : Integer;
  begin
    result:=AType;

    repeat
       i:=Pos(TSyntax._NamespaceDelimiter,Name);

       if i>0 then
       begin
         result:=Finder.FindTypeOrExtender(AType,result,Copy(Name,1,i-1));

         if result=nil then
            Exit;

         Name:=Copy(Name,i+1,Length(Name));
       end;

    until i=0;

    result:=Finder.FindTypeOrExtender(AType,result,Name);
  end;

  // Try to parse a dotted name until a module is found.
  // Then, locate the sub.sub.subtype inside the module.
  function TryParsePartial:TNamedType;
  var tmp : String;
      tmpSub : String;
      i : Integer;
  begin
    result:=nil;

    tmp:=AName;

    repeat
       i:=Pos(TSyntax._NamespaceDelimiter,tmp);

       if i>0 then
       begin
         result:=ParseModule(APath,Copy(tmp,1,i-1));

         if result<>nil then
         begin
           tmpSub:=Copy(tmp,i+1,Length(tmp));
           result:=FindDottedTypedIn(result,tmpSub);

           break;
         end;

         tmp:=Copy(tmp,1,i-1);
       end;
    until i=0;
  end;

var tmp : String;
begin
  result:=FindModule(AName);

  if result=nil then
  begin
    ModuleName:=AName;

    TryClearMagicTypes(ModuleName);

    tmp:=AName+TVidiConstants.Extension;

    if CheckFileExists(tmp,ModulePath) then
       result:=FinishModule(AName,ReadFile(ModulePath,tmp))
    else
       result:=TryParsePartial;
  end;
end;

procedure TBee.PerformChecks(const ANode:TNode);

  // Make sure all "out" parameters are assigned (returned)
  procedure CheckOutParameters(const ARoutine:TRoutine);

    function FirstIsAssignment(const AVariable:TVariable; const References : TNodeReferences):Boolean;
    var tmp : TNode;
        t : Integer;
    begin
      for t:=0 to High(References) do
      begin
        tmp:=References[t].Node;

        if TChecker.GetRoutineOf(tmp)=ARoutine then
           if (tmp is TAssignment) and
              (TAssignment(tmp).Variable=AVariable) then
                 Exit(True)
           else
              break;
      end;

      result:=False;
    end;

  var P : TNode;
      V : TVariable;
      tmp : Integer;
  begin
    for P in ARoutine.Parameters do
      if P is TVariable then
      begin
        V:=TVariable(P);

        if not V.Clauses.Final then // out parameter
        begin
          tmp:=Usages.IndexOf(V);

          if tmp=-1 then
             Error(TErrors._OutParameterNotAssign,V)
          else
          if not FirstIsAssignment(V,Usages.Items[tmp].References) then
             Error(TErrors._OutParameterFirst,V);
        end;
      end
      {$IFDEF INTERNAL}
      else
        InternalError(ARoutine,'Parameter is not a variable: '+P.ClassName);
      {$ENDIF}
  end;

  function NeedsAnyReturn(const ARoutine:TRoutine):Boolean;
  begin
    result:=(ARoutine.Output<>nil) and // <-- should be a function returning something
            (ARoutine.Items<>nil);// <-- at least something (not abstract routine)
  end;

  // Make sure a function returns something
  procedure CheckNodeOutput(const ARoutine:TRoutine);

    // Convert last item in the block into a Return
    function CheckLast(const AParent:TNode; const ANodes:TNodes):Boolean;
    var tmp : Integer;
        tmpData : TCallData;
        tmpReturn : TReturn;
    begin
      result:=False;

      if Length(ANodes)>0 then
      begin
        tmp:=High(ANodes);

        { unreachable
        if ANodes[tmp] is TData then
        begin
          tmpReturn:=CreateReturn(AParent, ANodes[tmp] as TData,False);
          ANodes[tmp]:=tmpReturn;
          result:=True;
        end
        else
        }
        if ANodes[tmp] is TCallData then // replace TCallData with a new TReturn
        begin
          tmpData:=TCallData(ANodes[tmp]);

          tmpReturn:=CreateReturn(AParent, tmpData.Value,False);
          ANodes[tmp]:=tmpReturn;

          tmpData.Value.Owner:=tmpReturn;

          ReplacePositionOf(tmpData,tmpReturn);

          tmpData.Free;

          result:=True;
        end;
      end;
    end;


    // Foo() { return Foo } <-- infinite loop (error!)
    procedure CheckSelfReturnLoop;
    var tmp : TReturn;
        N : TNode;
    begin
      for N in ARoutine.Items do
          if N is TReturn then
          begin
            tmp:=TReturn(N);

            if tmp.Value is TDataCall then
               if TDataCall(tmp.Value).Routine=ARoutine then
                  Error(TErrors._InfiniteLoop,tmp);

            break;
          end;
    end;

  begin
    if NeedsAnyReturn(ARoutine) then
    begin
      if not Finder.FindReturn(ARoutine) then
         if not CheckLast(ARoutine,ARoutine.Items) then
            Error(TErrors._NoOutputReturn,ARoutine.Name);

      CheckSelfReturnLoop;
    end;
  end;

  function ClassAndMethod(const ARoutine:TRoutine):String;
  var tmp : TClassType;
  begin
    tmp:=TChecker.GetClassTypeOf(ARoutine);

    result:=tmp.Name+TSyntax._DataMember+ARoutine.Name;
  end;

  function IsAncestor(const ANode:TNode):Boolean;
  begin
    result:=(ANode is TCallData) and
            (TCallData(ANode).Value is TAncestor);
  end;

  function MustBeOverriden(const ARoutine:TRoutine):Boolean;
  begin
    // ARoutine.Ancestor ??
    result:=(ARoutine.Items=nil) and
            (not ARoutine.Clauses.Final);
  end;

  procedure CheckMustOverride(const AClass:TClassType);
  var N : TNode;
      tmp : TRoutine;
      tmpPos : TNodePosition;
      tmpPosition : TPosition;
  begin
    for N in AClass.Ancestor.Items do
        if not (N is TFunctionType) then
        if (N is TRoutine) and MustBeOverriden(TRoutine(N)) then
        begin
          tmp:=Finder.FindOverride(TRoutine(N),AClass.Items);

          if tmp=nil then
          begin
            if Positions.Find(TRoutine(N),tmpPos) then
               tmpPosition:=tmpPos.Position
            else
               tmpPosition:=Position;

            Error(tmpPosition,10,TErrors._MethodMustOverride,ClassAndMethod(TRoutine(N)));
            break;
          end
          else
          if tmp.Items=nil then
          begin
            Error(TErrors._MethodIsEmpty,N);
            break;
          end
          else
          if (tmp.Items.Count=1) and IsAncestor(tmp.Items[0]) then
          begin
            Error(TErrors._MethodIsEmptyAncestor,N);
            break;
          end;

        end;
  end;

var UsageChecker : TUsageChecker;

  procedure CheckRoutine(const ARoutine:TRoutine);
  begin
    // ABlock is a final method, and its empty? Error

    if TChecker.IsFinalRoutine(ARoutine) then
       if ARoutine.Items=nil then
          Error(TErrors._FinalMethodIsEmpty,ARoutine.Name);

    // Check unused variables (for Routine)
    UsageChecker.CheckUnusedItems(ARoutine.Items,False,False);

    // Make sure the routine (when a function) is returning in all cases
    CheckNodeOutput(ARoutine);

    if TChecker.IsAbstract(ARoutine) then
       ARoutine.Clauses.Shared:=True // doubt: all abstract routines are shared by default ?
    else
    begin
      // Make sure all "out" parameters are assigned (returned)
      CheckOutParameters(ARoutine);

      if not (ARoutine is TFunctionType) then // <-- dont check param usage for function as types
         UsageChecker.CheckParameterUsage(ARoutine.Parameters);

      // Try to determine if the routine is type-level shared
      if TSharedChecker.CheckIsShared(ARoutine) then
         ARoutine.Clauses.Shared:=True
      else
      if ARoutine.Clauses.Shared then
         Error(TErrors._MethodCannotBeShared,ARoutine.Name);
    end;
  end;

  function AnyAbstract(const AItems:TNodes):Boolean;
  var N : TNode;
  begin
    for N in AItems do
        if N is TRoutine then
           if TChecker.IsAbstract(TRoutine(N)) then
              Exit(True);

    result:=False;
  end;

  procedure CheckClass(const AClass:TClassType);
  begin
    // A final class cannot be empty (abstract)
    if AClass.Clauses.Final then
    begin
      if AClass.Items=nil then
         Error(TErrors._FinalClassIsEmpty,AClass)
      else
      if AnyAbstract(AClass.Items) then
         Error(TErrors._FinalClassHasAbstract,AClass);
    end;

    // Check Must-override methods.
    // Search ancestor for empty (abstract) methods,
    // and issue errors if they aren't overriden in ABlock

    if AClass.Ancestor<>nil then
       CheckMustOverride(AClass);

    // Check unused hidden variables (for ClassType)
    UsageChecker.CheckUnusedItems(AClass.Items,True,False);
  end;

  procedure CheckBlock(const ANode:TNode);
  var tmp : TNode;
  begin
    // Check for empty code block
    if (ANode=nil) or
       ( (ANode is TType) and (TType(ANode).Items=nil) ) then
    begin
      tmp:=ANode.Owner;

      while tmp is TBlockStatement do
        if tmp.Owner=nil then
           break
        else
           tmp:=tmp.Owner;

      if (tmp is TFor) or
         (tmp is TWhen) or  // TWhenItem.Owner
         (tmp is TLoopStatement) then
           Error(TErrors._CodeBlockEmpty,tmp);
    end;
  end;

  function CheckUselessAssignment(const AIndex:Integer;
                                  const ABlock:TType;
                                  const ANode:TNode;
                                  const ANodes:TNodes):TNode;

    function UsedIn(const AVariable:TData; const AData:TNode):Boolean;
    var tmpUsage : TUsageChecker;
        tmpUsages : TNodeUsages;
        tmp : TNode;
    begin
      if AData is TVariable then
         tmp:=TVariable(AData).ValueData
      else
         tmp:=AData;

      if tmp=nil then
         result:=False
      else
      begin
        result:=tmp=AVariable; // same variable? (it is used for sure !)

        if not result then
        if not (tmp is TVariable) then // <-- a variable cannot be passed to TNodeUsages
        begin
          tmpUsages:=TNodeUsages.Create;
          try
            tmpUsage.Usages:=tmpUsages;
            tmpUsage.DoError:=Error;

            tmpUsage.Process(tmp);

            result:=tmpUsages.IndexOf(AVariable)<>-1;
          finally
            tmpUsages.Free;
          end;
        end;
      end;
    end;

    function IsUseless(const AIndex:Integer; const A:TAssignment):TNode;

      function UselessForward(const AVariable:TVariable):TNode;
      var t : Integer;
          N : TNode;
      begin
        for t:=AIndex+1 to High(ANodes) do
        begin
          N:=ANodes[t];

          if UsedIn(AVariable,N) then
             Exit(nil);
        end;

        result:=A;
      end;

      function GetFinalVariable(const AData:TData):TData;
      begin
        if AData is TArrayExpression then
           result:=GetFinalVariable(TArrayExpression(AData).Data)
        else
        if AData is TMember then
           result:=GetFinalVariable(TMember(AData).Member)
        else
           result:=AData;
      end;

      function SameValue(const P1,P2:TNode):Boolean;
      begin
        result:=P1=P2;

        if not result then
        begin
          result:=(P1 is TData) and (P2 is TData);

          if result then
             result:=TEvaluateCompile.SameValue(TData(P1),TData(P2));
        end;
      end;

      function DifferentArrayIndex(const A1,A2:TArrayExpression):Boolean;
      var L, t : Integer;
      begin
        L:=Length(A1.Parameters);
        result:=L=Length(A2.Parameters);

        if result then
           for t:=0 to L-1 do
               if not SameValue(A1.Parameters[t],A2.Parameters[t]) then
                  Exit(False);
      end;

      function UselessBackwards(const AVariable:TData):TNode;
      var t : Integer;
          N : TNode;
          tmp : TData;
          tmpIsArray : Boolean;
      begin
        for t:=AIndex-1 downto 0 do
        begin
          N:=ANodes[t];

          if N is TAssignment then
          begin
            tmp:=GetFinalVariable(TAssignment(N).Variable);

            if tmp=AVariable then
            begin
              tmpIsArray:=TAssignment(N).Variable is TArrayExpression;

              if (not tmpIsArray) or
                 DifferentArrayIndex(A.Variable as TArrayExpression,
                                     TArrayExpression(TAssignment(N).Variable)) then

                   if not UsedIn(AVariable,TAssignment(N).Value) then
                      Exit(N);
            end
            else
            if UsedIn(AVariable,TAssignment(N).Value) then
               break;

          end
          else
          if N is TVariable then
          begin
            if TVariable(N).ValueData<>nil then
               if N=AVariable then
               begin
                 // unreachable
                 if not UsedIn(AVariable,TVariable(N).ValueData) then
                    Exit(N);
               end
               else
               if UsedIn(AVariable,TVariable(N).ValueData) then
                  break;
          end
          // TODO: Else check all expressions for calls with A.Variable as "out" parameter !
          else
          if UsedIn(AVariable,N) then
             break;
        end;

        result:=nil;
      end;

    var tmp : TData;
    begin
      result:=nil;

      tmp:=GetFinalVariable(A.Variable);

      if tmp is TVariable then
         if TChecker.IsLocalVariable(TVariable(tmp),ABlock,ANodes) then
         begin
           if UsedIn(tmp,A.Value) then
              result:=nil
           else
           begin
             result:=UselessBackwards(tmp);

             if result=nil then
                if not TChecker.IsOutParameter(TVariable(tmp)) then
                   if TChecker.GetClassTypeOf(tmp)=nil then  // <-- not for class instance variables
                      result:=UselessForward(TVariable(tmp));
           end;
         end;
    end;

  begin
    if (ANode is TAssignment) and
       (TAssignment(ANode).Arithmetic=TArithmetic_Assign.None) then
       result:=IsUseless(AIndex,TAssignment(ANode))
    else
       result:=nil;
  end;

  procedure CheckUselessAssignments(const AType:TType);

    procedure DoError(const ANode:TNode);
    var tmp : TNodePosition;
    begin
      if Positions.Find(ANode,tmp) then
         Error(tmp.Position,10,TErrors._AssignmentIsUseless,ANode)

      {$IFDEF INTERNAL}
      else
         InternalError(ANode,'Cannot find node position');
      {$ENDIF}
    end;

  var t : Integer;
      N : TNode;
      tmp : TNode;
  begin
    for t:=0 to High(AType.Items) do
    begin
      N:=AType.Items[t];

      tmp:=CheckUselessAssignment(t,AType,N,AType.Items);

      if tmp<>nil then
         DoError(tmp);
    end;
  end;

  {$IFDEF INTERNAL}
  // Verify all items Owner property is correct, ie, it has not been
  // changed by mistake at any of the ownership-change situations.
  procedure CheckItemsOwner(const ABlock:TType);
  var N : TNode;
  begin
    for N in ABlock.Items do
        if N.Owner<>ABlock then
           InternalError(N,'Node owner is not its type: ');
  end;
  {$ENDIF}

  procedure CheckExtender(const AExtender:TExtender);
  begin
    if AExtender.Extension is TRoutine then
       CheckRoutine(AExtender.Extension as TRoutine)
    else
       CheckClass(AExtender.Extension as TClassType);
  end;

begin
  // Gather all references to local symbols in ABlock
  UsageChecker.Usages:=Usages;
  UsageChecker.DoError:=Error;

  if ANode is TType then
     UsageChecker.ProcessBlock(TType(ANode));

  if ANode is TExtender then
     CheckExtender(TExtender(ANode))
  else
  if ANode is TRoutine then
     CheckRoutine(TRoutine(ANode))
  else
  if ANode is TClassType then
     CheckClass(TClassType(ANode))
  else
  begin
    // Check unused variables (for unnamed main block { })
    if ANode is TType then
       UsageChecker.CheckUnusedItems(TType(ANode).Items,False,True);

    CheckBlock(ANode);
  end;

  if ANode is TType then
  begin
    CheckUselessAssignments(TType(ANode));

    {$IFDEF INTERNAL}
    if ANode is TType then
       CheckItemsOwner(TType(ANode));
    {$ENDIF}
  end
  {$IFDEF INTERNAL}
  else
  //  CheckUselessAssignment(0,nil,ANode,nil); // <-- remove ?
    InternalError(ANode,'Check Useless Assignment of unknown node')
  {$ENDIF}
  ;
end;

// inline unnamed function declaration and body:
// (X:Integer):Text { return Foo(X) }
function TBee.TryGetLambdaFunction(const AParent,AOwner:TNode):TTypeCall;
var tmpParameters : TNodes;
    tmpRoutine : TRoutine;
    tmpHasParams : Boolean;
    OldCount : Integer;
begin
  OldCount:=Positions.Count;

  tmpParameters:=TryGetDeclaredParameters(AOwner,tmpHasParams);

  if tmpHasParams then
  begin
    result:=TTypeCall.CreateNew(AOwner);

    tmpRoutine:=TRoutine.Create;
    tmpRoutine.Owner:=result;

    // NO!!!  Emit fails: tmpRoutine.Clauses.Final:=True;

    TASTCreator.AssignParameters(tmpRoutine,tmpParameters);

    if GetSymbol(result,TSyntax._TypeDelimiter) then
    begin
      PositionToAdd:=Position;
      tmpRoutine.Output:=GetTypes(AParent,tmpRoutine);
    end;

    if ExpectSymbol(tmpRoutine,TSyntax._BeginBlock) then
    begin
      FillBlock(tmpRoutine);

      result.TheType:=tmpRoutine;
    end
    else
    begin
      tmpRoutine.Free;
      Positions.DeleteFrom(OldCount);
      result.Free;
      result:=nil;
    end;
  end
  else
    result:=nil;
end;

function TBee.AddToNodes(const AOwner:TNode; var Nodes:TNodes;
                         const Node:TNode; const OldPos:Integer):Boolean;

  procedure FreeAndError(const AError:TErrors);
  var tmpS : String;
  begin
    Positions.RemoveNode(OldPos); // RemoveLastNode?

    tmpS:=TASTUtils.NodeName(Node);
    Node.Free;

    Error(AError,tmpS);
  end;

  procedure DoAddNode(const ANode:TNode);
  begin
    Nodes.Add(ANode);

    {$IFDEF INTERNAL}
    if AOwner<>nil then
       if ANode.Owner=nil then
          InternalError(Position,'Parsed nil node');
    {$ENDIF}

    if ANode is TClassType then
       Checker.TryCheckMagicTypes(TClassType(ANode));
  end;

  function TryFixForward(Node,Existing:TNode):Boolean;
  begin
    result:=False;

    // Fix forward routine declarations, add a "return" in old one, to the new one
    if (Existing is TRoutine) and
       (TRoutine(Existing).Items=nil) and
       (Node is TRoutine) and
       (TRoutine(Node).Items<>nil) then
    begin
      // Special case, force set Shared because we don't know that in advance
      TRoutine(Existing).Clauses.Shared:=TRoutine(Node).Clauses.Shared;

      if TRoutine(Existing).Clauses.EqualsTo(TRoutine(Node).Clauses) then
      begin
        TRoutine(Existing).ForwardTo:=TRoutine(Node);
        TRoutine(Node).ForwardFrom:=TRoutine(Existing);

        DoAddNode(Node);

        result:=True;
      end
      else
        FreeAndError(TErrors._DifferentClauses);

    end
    else
      FreeAndError(TErrors._Duplicate);
  end;

var Existing : TNode;
begin
  {$IFDEF INTERNAL}
  if Node=nil then
     InternalError(Position,'Parsed nil node');
  {$ENDIF}

  if Checker.MemberExists(Nodes,Node,Existing) then
     result:=TryFixForward(Node,Existing)
  else
  begin
    DoAddNode(Node);
    result:=True;
  end;
end;

procedure TBee.AddNewItem(const AOwner,ANode:TNode; var AItems:TNodes; const OldPos:Integer);

  procedure CheckUnreachable(const ANode:TNode);
  var tmp : TNode;
  begin
    if (AOwner is TRoutine) then // and (TRoutine(AOwner).Output<>nil) then
       if AItems<>nil then
       begin
         tmp:=AItems[High(AItems)];

         if not (tmp is TRoutine) then
            if Finder.FindReturn(tmp) then
               Error(TErrors._UnreachableCode,ANode);
       end;
  end;

begin
  CheckUnreachable(ANode);

  AddToNodes(AOwner,AItems,ANode,OldPos);
end;

procedure TBee.FillBlock(const ABlock:TType);

  procedure DoFillBlock(const ABlock:TType);
  var tmp : TNode;
      tmpClauses : TClauses;
      Old : Integer;
  begin
    while Pending do
    begin
      tmpClauses.Reset;

      Old:=Positions.Count;

      tmp:=ParseBlock(ABlock,tmpClauses);

      if tmp=nil then
         Break
      else
      if tmp is TAvoidEmpty then
         tmp.Free
      else
         AddNewItem(ABlock,tmp,ABlock.Items,Old);
    end;
  end;

begin
  DoFillBlock(ABlock);

  ExpectSymbol(ABlock,TSyntax._EndBlock);

  // TODO: speed up, caching a boolean for default indexed
  // if ABlock.Owner is TClassType then
  //    TClassType(ABlock.Owner).FindDefaultIndexed

  PerformChecks(ABlock);
end;

// Load and parse a file
function TBee.GetParsedModule(const AName:String):TNamedType;

  procedure ErrorNotFound;
  var tmp : String;
  begin
    tmp:=ModulePath;

    if tmp='' then
       tmp:='.';

    Error(Position,Length(AName),TErrors._ModuleNotFound,AName,tmp);
  end;

var Bee : TBee;
begin
  if Modules.ParsingIndex(AName)<>-1 then
  begin
    result:=nil;
    Error(TErrors._ModuleSelfReference,AName);
  end
  else
  begin
    Bee:=TBee.Create;
    try
      // Copy customizations:
      Bee.OnError:=OnError;
      Bee.OnParsed:=OnParsed;
      Bee.ParserPath:=ParserPath;

      // ?? necessary also modulepath?
      Bee.ModulePath:=ModulePath;

      {
      if DoFileExists(ModulePath,AName+_Extension) then
         result:=Bee.ParseModule(ModulePath,AName)
      else
      }

      Modules.ParsingAdd(AName);
      try
        result:=Bee.ParseModule(ModulePath,AName);

        if result=nil then
           ErrorNotFound
        else
        if result.Owner=nil then // <-- set name only for modules, not sub.sub.types
           result.Name:=AName;

      finally
        Modules.ParsingRemove(AName);
      end;
    finally

      // Re-parent Items objects, because Bee.Free will destroy them too soon !
      Self.Errors.Add(Bee.Errors);
      Bee.Errors:=nil;

      Bee.Free;
    end;
  end;
end;

function CreateBlockStatement(const AOwner:TNode):TBlockStatement;
begin
  result:=TBlockStatement.Create;
  result.Owner:=AOwner;
end;

function TBee.GetBlock(const AParent:TNode):TBlockStatement;
begin
  result:=CreateBlockStatement(AParent);
  AddPositionSymbol(result,TSyntax._BeginBlock);
  FillBlock(TBlockStatement(result).Block);
end;

function TBee.ParseBlock(const AParent:TType; var Clauses:TClauses):TNode;

  type
    TNodeName=record
      Position : TPosition;
      Name : String;
    end;

    TNodeNames=Array of TNodeName;

  function GetNames(const AName:String):TNodeNames;

    procedure Add(const AName:String);
    var L : Integer;
    begin
      L:=Length(result);
      SetLength(result,L+1);

      result[L].Position:=PositionToAdd;
      result[L].Name:=AName;
    end;

  var tmp : String;
  begin
    Add(AName);

    repeat
      PositionToAdd:=Position;

      tmp:=GetIdentifier;

      if tmp='' then
      begin
        Error(PositionToAdd,1,TErrors._EmptyItem);
        break;
      end
      else
         Add(tmp);

    until not GetSymbol(AParent,TSyntax._ItemDelimiter);
  end;

  function GetMultipleVariables(const AName:String):TNodes;
  var
    tmpType : TType;
    tmpNames : TNodeNames;

    function DoCreateVariable(const AIndex:Integer):TNode;
    var tmpS : String;
    begin
      tmpS:=tmpNames[AIndex].Name;
      result:=CreateVariable(AParent,tmpS,Clauses,tmpType);

      AddPosition(result,tmpNames[AIndex].Position,tmpS);
    end;

  var Old,L,t : Integer;
  begin
    result:=nil;

    tmpNames:=GetNames(AName);

    if not ExpectSymbol(AParent,TSyntax._TypeDelimiter) then
       Exit;

    if PeekIs(TSyntax._Assignment) then
       tmpType:=nil // Infer type later
    else
    {
    if PeekBeginBlock then
    begin
      Error(Position,TErrors._MultipleRoutines);
      Exit;
    end
    else
    }
      tmpType:=GetTypes(AParent,AParent);

//    if PeekBeginBlock then
//       Error(Position,TErrors._MultipleRoutines)
//    else

    //if AParent is TType then
    //begin
      L:=Length(tmpNames);

      SetLength(result,L);

      result[L-1]:=DoCreateVariable(L-1);

      for t:=0 to L-2 do
      begin
        result[t]:=DoCreateVariable(t);

        Old:=Positions.Count;

        if not AddToNodes(AParent,TType(AParent).Items,result[t],Old) then
           result[t]:=nil;
      end;

      // Move inline type ownership from last variable to first variable.
      // This is to achieve correct streaming references of the inline type:

      if L>1 then
         if tmpType<>nil then
            if tmpType.Owner=result[L-1] then // last variable owns the inline type
               tmpType.Owner:=result[0];
    {
    end
    else
       Error(Position,TErrors._ParentNotAClass)
    }
  end;

  function FindDataCall(const AData:TData):TDataCall;
  begin
    if AData is TDataCall then
       result:=TDataCall(AData)
    else
    if AData is TMember then
       result:=FindDataCall(TMember(AData).Member)
    else
       result:=nil;
  end;

  // Special case, try to find overload setter routine for assignment
  // Temp.Fahrenheit:=99
  function FindSetter(const AOwner:TNode; const AData,AValue:TData):TData;
  var tmp : TDataCall;
      tmpParams : TNodes;
      tmpNew : TType;
      Old,
      tmpType : TType;
  begin
    result:=AData;

    tmp:=FindDataCall(AData);

    if tmp<>nil then
       if tmp.Routine is TParametersType then
       if Length(TParametersType(tmp.Routine).Parameters)<>1 then  // Same name but not with just one parameter
       begin
         tmpNew:=nil;

         tmpType:=TChecker.GetFinalType(tmp.Routine);

         if tmpType is TRoutine then
         begin
           Old:=TRoutine(tmpType).Output;
           TRoutine(tmpType).Output:=nil;

           tmpParams:=nil;
           tmpParams.Add(AValue);

           // Try to search an overloaded routine name that will be fine as a setter
           tmpNew:=Finder.FindOverload(AOwner,TRoutine(tmpType),tmpParams);

           TRoutine(tmpType).Output:=Old;
           tmp.Routine:=tmpNew;
         end;

         if tmpNew=nil then
            Error(TErrors._SetterMissing,result);
       end;
  end;

  function GetAssignment(const AParent,AOwner:TNode; const Arithmetic:TArithmetic_Assign; const AData:TData):TAssignment;

    function DoCreateAssignment(const AOwner:TNode; const ALeft:TData):TAssignment;

      function IsCorrectAssign(const AValue:TData; const Old:TPosition):Boolean;
      var tmp : TType;
      begin
        if Arithmetic=TArithmetic_Assign.None then
           result:=CheckAssign(ALeft,AValue,True,Old)
        else
        begin
          tmp:=TChecker.GetDataType(AParent,ALeft);

          if (tmp is TArrayType) or
              TChecker.DerivesFrom(tmp,TChecker._Types[TChecker._Array]) or // Array
             TChecker.DerivesFrom(tmp,TChecker._Types[TChecker._Text]) then  // Text

               result:=Arithmetic=TArithmetic_Assign.Add // addition only allowed
          else
          if TChecker.DerivesFromNumber(tmp) then
             result:=True // division only for Float?
          else
             result:=False;
        end;
      end;

    var tmp : TData;
        OldCount : Integer;
        Old : TPosition;
    begin
      result:=nil;

      OldCount:=Positions.Count;
      Old:=PositionToAdd;

      (*
        Not here. F:={...} should be done at GetExpression level
      if PeekBeginBlock then
      begin
        if IsVariableFunctionType(ALeft) then
        else
           Error !!
      end;
      *)

      tmp:=GetExpression(AParent,AOwner);

      if tmp=nil then
         Error(Position,10,TErrors._EmptyItem)
      else
      if IsCorrectAssign(tmp,Old) then
      begin
        PositionToAdd:=Old;
        result:=TASTCreator.CreateAssignment(AOwner,Arithmetic,FindSetter(AOwner,ALeft,tmp),tmp);
        AddPositionSymbol(result,TASTUtils.AssigmentOf(Arithmetic));
      end
      else
      begin
        Positions.DeleteFrom(OldCount);
        FreeIfNotOwned(tmp,AOwner);

        // DO NOT show any error !
        // Error('Internal, wrong assignment',AOwner);
      end;
    end;

  var Old : Integer;
  begin
    if AData is TDataCall then
    begin
      Old:=Positions.Count-1;

      {$IFDEF INTERNAL}
      if Positions.Items[Old].Node<>AData then
         InternalError(AData,'Wrong position of AData');
      {$ENDIF}

      result:=GetAssignment(AParent,AOwner,Arithmetic,TDataCall(AData).ResultValue); // Foo():=123  ??

      if result=nil then
      begin
        Positions.Items[Old].Node:=nil;
        AData.Free;  // mem leak
      end;
    end
    else
    begin
      if (AData is TMember) or   // Foo.Bar.XYZ:=123
         (AData is TVariable) or  // a:=123
         (AData is TSelf) or  // Self:=[]
         (AData is TArrayExpression)  // Foo[x]:=123
            then
      begin
        result:=DoCreateAssignment(AOwner,AData);

        if result=nil then
           Error(TErrors._IncompatibleLeftRight,AData);
      end
      else
      begin
        result:=nil;

        Error(TErrors._NotAVariable,AData);
      end;
    end;
  end;

  // Extender: methods Array.Sort(Ascending:Boolean) {} or
  // subclasses Array.SubClass {}
  function GetExtender(const AName:String;
                       const AType:TType):TExtender; overload;

    procedure FinishExtender(const AExtender:TExtender);
    begin
      AExtender.Name:=AExtender.Extension.Name;
      AExtender.Owner:=AParent;
      AExtender.TheType:=AType; // Array
      AExtender.Clauses:=AExtender.Extension.Clauses;
    end;

  var tmpExtension : TNode;
      Old : Integer;
  begin
    Old:=Positions.Count;

    result:=TExtender.Create;
    result.Owner:=AParent;

    result.TheType:=AType; // Array

    tmpExtension:=ParseBlock(result,Clauses); // Sort

    if tmpExtension is TRoutine then
    begin
      result.Extension:=TRoutine(tmpExtension);
      FinishExtender(result);
    end
    else
    if tmpExtension is TClassType then
    begin
      result.Extension:=TClassType(tmpExtension);
      FinishExtender(result);
    end
    else
    try
      Error(TErrors._ExtenderNotARoutine,tmpExtension);
    finally
      tmpExtension.Free;
      Positions.DeleteFrom(Old);
    end;

    PositionToAdd:=Position;
  end;

  function PeekAssignment(out Arithmetic:TArithmetic_Assign):Boolean;
  begin
    Arithmetic:=TArithmetic_Assign.None;

    if PeekSymbolGet(TSyntax._Assignment) then
       result:=True
    else
    begin
      result:=True;

      if PeekSymbolGet(TSyntax._Add_Assign) then Arithmetic:=TArithmetic_Assign.Add else
      if PeekSymbolGet(TSyntax._Subtract_Assign) then Arithmetic:=TArithmetic_Assign.Subtract else
      if PeekSymbolGet(TSyntax._Multiply_Assign) then Arithmetic:=TArithmetic_Assign.Multiply else
      if PeekSymbolGet(TSyntax._Divide_Assign) then Arithmetic:=TArithmetic_Assign.Divide else
         result:=False;
    end
  end;

  function GetStatement(const AParent:TNode):TStatement;

    procedure CheckIsEmpty(const ABlock:TStatement);
    begin
      if (ABlock=nil) or
         (
           (ABlock is TBlockStatement) and
           (TBlockStatement(ABlock).Block.Items.Count=0)
         ) then
      begin
        Error(TErrors._EmptyItem,AParent);
      end;
    end;

    function GetCondition(const AParent,AOwner:TNode):TData;
    var Old : TPosition;
        OldPos : Integer;
        tmp : TData;
    begin
      result:=nil;

      Old:=Position;
      OldPos:=Positions.Count;

      tmp:=GetExpression(AParent,AOwner);

      if tmp=nil then
      begin
        Position:=Old;
        Positions.DeleteFrom(OldPos);

        Error(Position,10,TErrors._EmptyItem);
      end
      else
      if IsValidCondition(tmp,AOwner,OldPos,Old) then
         result:=tmp;
    end;

    function GetIf:TIf;
    var OldCount : Integer;
        tmpS : String;
    begin
      result:=TIf.Create;
      result.Owner:=AParent;

      AddPositionKeyword(result,TSyntax._Reserved_if);

      result.Condition:=GetCondition(result,result);

      if TSyntax._Reserved_then<>'' then
      begin
        PositionToAdd:=Position;

        if Expect(TSyntax._Reserved_then) then
           AddPositionKeyword(result,TSyntax._Reserved_then);
      end;

      OldCount:=Positions.Count;
      tmpS:=PeekIdentifier;
      Positions.DeleteFrom(OldCount);

      if not Checker.TextIs(tmpS,TSyntax._Reserved_else) then
         if not PeekEndBlock then
         begin
           result.ThenBlock:=GetStatement(result);
           CheckIsEmpty(result.ThenBlock);
         end;

      OldCount:=Positions.Count;
      tmpS:=PeekIdentifier;
      Positions.DeleteFrom(OldCount);

      if Checker.TextIs(tmpS,TSyntax._Reserved_else) then
      begin
        PositionToAdd:=Position;

        if Expect(TSyntax._Reserved_else) then
           AddPositionKeyword(result,TSyntax._Reserved_else);

        if PeekEndBlock then
           Error(TErrors._EmptyElse,result)
        else
        begin
          result.ElseBlock:=GetStatement(result);
          CheckIsEmpty(result.ElseBlock);
        end;
      end;

      // Expensive performance cost:
      {$IFDEF EXTRA_CHECKS}
      if (result.ThenBlock<>nil) and (result.ElseBlock<>nil) then
         if TCodeCompare.SameCode(result.ThenBlock,result.ElseBlock) then
            Error(TErrors._IfThenElseAreIdentical,result);
      {$ENDIF}

      if (result.ThenBlock=nil) and (result.ElseBlock=nil) then
         Error(TErrors._EmptyThen,result);

      (*
      NO !!  Condition can be nil because a previous error, so this is not an
      internal error.
      {$IFDEF INTERNAL}
      if result.Condition=nil then
         InternalError(AParent,'No logical If condition found');
      {$ENDIF}
      *)
    end;

    procedure CheckWhile(const AWhile:TWhile);
    var tmp,
        tmpEvaluated : Boolean;
    begin
      tmpEvaluated:=TEvaluateCompileTime.AsBoolean(AWhile.Condition,tmp); // inmutable?

      if tmpEvaluated then
      begin
        if tmp then  // while True
        begin
          if TFinder.FindReturn(AWhile.Block) or
             TFinder.FindBreak(AWhile.Block) then
          else
            // A while loop without any return or break, is a bug when the while condition is inmutable
             Error(TErrors._WhileInfinite,AWhile)
        end
        else // while False
           Error(TErrors._NeverExecuted,AWhile)
      end;
    end;

    function GetWhile:TWhile;
    begin
      result:=TWhile.Create;
      result.Owner:=AParent;

      AddPositionKeyword(result,TSyntax._Reserved_while);

      result.Condition:=GetCondition(result,result);

      result.Block:=GetStatement(result);
      CheckIsEmpty(result.Block);

      CheckWhile(result);
    end;

    function GetWhen:TWhen;

      procedure CheckDuplicate(const AItems:TWhenItems; const AExp:TData);
      var N : TWhenItem;
      begin
        if AExp<>nil then
           for N in AItems do
              if (N.Expression<>nil) and (N.Expression<>AExp) then
                 if TCodeCompare.SameCode(AExp,N.Expression) then
                    Error(TErrors._Duplicate,N.Expression);
      end;

      function AddNewWhenItem(const AWhen:TWhen):TWhenItem;
      begin
        result:=AWhen.AddItem;

        if PeekOperand then
           result.Expression:=FinishOperand(AParent,result,AWhen.Expression)
        else
           result.Expression:=GetExpression(AParent,result);

        if result.Expression<>nil then
        begin
          result.Block:=GetStatement(AWhen);
          CheckIsEmpty(result.Block);

          CheckDuplicate(AWhen.Items,result.Expression);
        end;
      end;

    var tmp : TData;
        Old : TPosition;
        OldPos : Integer;
    begin
      result:=TWhen.Create;
      result.Owner:=AParent;

      AddPositionKeyword(result,TSyntax._Reserved_when);

      Old:=Position;
      OldPos:=Positions.Count;

      tmp:=GetExpression(AParent,result);

      if tmp=nil then
      begin
        Error(Position,10,TErrors._EmptyItem);
        Exit;
      end
      else
      if TEvaluateCompile.Evaluable(tmp,False) then
      begin
        ErrorFree(tmp,result,OldPos,Old,TErrors._ExpressionAlwaysSameValue);
        Exit;
      end
      else
         result.Expression:=tmp;

      if ExpectSymbol(result,TSyntax._BeginBlock) then

      repeat
        PositionToAdd:=Position;

        if GetSymbol(result,TSyntax._EndBlock) then
           break
        else
        if PeekIsGet(TSyntax._Reserved_else) then
        begin
          AddPositionKeyword(result,TSyntax._Reserved_else);
          result.ElseBlock:=GetStatement(result);
        end
        else
        if AddNewWhenItem(result).Expression=nil then
           break;

      until False;
    end;

    procedure CheckRepeat(const ARepeat:TRepeat);
    var tmp : Boolean;
    begin
      if TFinder.FindReturn(ARepeat.Block) or
         TFinder.FindBreak(ARepeat.Block) then
      else
      if TEvaluateCompileTime.AsBoolean(ARepeat.Condition,tmp) then  // inmutable?
         if not tmp then
            // A repeat loop without any return or break, is a bug when the repeat condition is inmutable
            Error(TErrors._RepeatInfinite,ARepeat);
    end;

    function GetRepeat:TRepeat;
    begin
      result:=TRepeat.Create;
      result.Owner:=AParent;

      AddPositionKeyword(result,TSyntax._Reserved_repeat);

      result.Block:=GetStatement(result);
      CheckIsEmpty(result.Block);

      if result.Block<>nil then
      begin
        PositionToAdd:=Position;

        if Expect(TSyntax._Reserved_until) then
           AddPositionKeyword(result,TSyntax._Reserved_until);

        result.Condition:=GetCondition(result,result);

        CheckRepeat(result);
      end;
    end;

    function ValidForExpression(const AExpression:TData; out AType:TType):Boolean;

      function CustomGetAsArray(const AType:TType):TType;
      var tmp : TNode;
      begin
        result:=AType;

        if result is TClassType then
        begin
          if TClassType(result).Ancestor is TArrayType then
             result:=TArrayType(TClassType(result).Ancestor)
          else
          begin
            tmp:=TClassType(result).DefaultIndexed; // C:=Text[123]

            if tmp is TData then
               result:=TChecker.GetDataType(AParent,TData(tmp))
          end;
        end;
      end;

      function IsValidForType(const AType:TType):Boolean;
      begin
        result:=(AType is TArrayType) or
                (AType=TChecker._Types[TChecker._Text]) or // Text
                (AType=TChecker._Types[TChecker._Range]); // Range 1..10
      end;

    begin
      result:=False;

      if AExpression=nil then
         Error(Position,10,TErrors._EmptyItem)
      else
      if AExpression is TArrayExpression then  // for N in [4,1,3] ...
         result:=True
      else
      begin
        AType:=Checker.GetDataType(AParent,AExpression);

        result:=AExpression is TRange;

        if not result then
        begin
          result:=IsValidForType(AType);

          if not result then
          begin
            if AType is TClassType then
            begin
              AType:=CustomGetAsArray(AType);

              result:=IsValidForType(AType);

              if not result then
                 result:=TChecker.IsEnumerationClass(AType); // <-- Letters is { A,B,C }
            end;
          end;
        end;
      end;
    end;

    function GetFor:TFor;
    var Counter : String;
        tmpType : TType;
        tmp : TData;
        CounterClauses : TClauses;

        Old,
        Old2,
        OldExp : TPosition;

        OldCount : Integer;
    begin
      result:=TFor.Create;
      result.Owner:=AParent;

      AddPositionKeyword(result,TSyntax._Reserved_for);

      PositionToAdd:=Position;
      Old2:=PositionToAdd;

      OldCount:=Positions.Count;
      Counter:=PeekIdentifier;
      Positions.DeleteFrom(OldCount);

      if Counter<>'' then
         GetIdentifier;

      CounterClauses.Reset;
      CounterClauses.Final:=True;

      Old:=Position;

      if PeekIsGet(TSyntax._Reserved_in) then // in Items (array), in 0..99 (range), in 'ABC' (text) etc
      begin
        {$IFDEF INTERNAL}
        if Counter='' then
           InternalError(result,'"for in" has no counter variable');
        {$ENDIF}

        PositionToAdd:=Old;
        AddPositionKeyword(result,TSyntax._Reserved_in);

        PositionToAdd:=Old2;

        if Counter<>'' then
        begin
          result.Counter:=CreateVariable(result,Counter,CounterClauses,nil);
          AddPosition(result.Counter,Counter);
        end;

        Old2:=Position;
        OldCount:=Positions.Count;

        tmp:=GetExpression(AParent,result);

        if ValidForExpression(tmp,tmpType) then
        begin
          result.InExpression:=tmp;

          result.Counter.TypeInferred:=True;

          if tmp is TArrayExpression then
             result.Counter.VariableType:=Checker.GetDataType(AParent,tmp)
          else
             result.Counter.VariableType:=Checker.GetDataTypeAsArray(nil,tmp,tmpType);
        end
        else
        if tmp=nil then
           Error(Old2,10,TErrors._EmptyItem)
        else
           ErrorFree(tmp,result,OldCount,Old2,TErrors._WrongForInExpression)
      end
      else
      begin
        // "for x"
        if Counter<>'' then
        begin
          // "for x:="
          if PeekSymbolGet(TSyntax._Assignment) then
          begin
            Old2:=PositionToAdd;

            PositionToAdd:=Old;
            AddPositionSymbol(result,TSyntax._Assignment);

            PositionToAdd:=Old2;

            OldCount:=Positions.Count;

            tmp:=GetData(result,AParent,Counter,True,True);

            if tmp<>nil then
            begin
              if tmp is TVariable then
              begin
                // get existing variable as "for" counter
                result.Counter:=TVariable(tmp);

                {unreachable:
                if TChecker.BadHiddenAccess(AParent,result.Counter) then
                   Error(TErrors._HiddenAccessScope,result.Counter);
                }
              end
              else
              begin
                ErrorFree(tmp,AParent,OldCount,Old2,TErrors._NotAVariable);
                Exit;
              end;
            end;
          end
          else
          if GetSymbol(result,TSyntax._TypeDelimiter) then // "for x:"
          begin
            Old:=Position;

            // "for x:Integer"
            // "for x::=123"   type inference
            if PeekIs(TSyntax._TypeDelimiter) then // Type inference, leave nil
               tmpType:=nil
            else
               tmpType:=GetTypes(AParent,AParent);

            result.Counter:=CreateVariable(result,Counter,CounterClauses,tmpType);

            PositionToAdd:=Position;

            // "for x:Integer in"
            if PeekIsGet(TSyntax._Reserved_in) then // in Items (array), in 0..99 (range), in 'ABC' (text) etc
            begin
              AddPositionKeyword(result,TSyntax._Reserved_in);

              OldExp:=Position;

              tmp:=GetExpression(AParent,result);

              if ValidForExpression(tmp,tmpType) then
              begin
                result.InExpression:=tmp;

                CheckAssign(result.Counter,tmp,False,OldExp);
              end;
            end
            else
              ExpectSymbol(result,TSyntax._Assignment);

            PositionToAdd:=Old2;
            AddPosition(result.Counter,Counter);
          end;
        end;

        if result.InExpression=nil then
        begin
          // Start
          OldExp:=Position;
          OldCount:=Positions.Count;

          tmp:=GetExpression(AParent,result);

          if tmp=nil then
          begin
            //Position:=OldExp;
            Positions.DeleteFrom(OldCount);
            Error(OldExp,10,TErrors._EmptyItem);
          end
          else
          begin
            // TODO: GetDataType(tmp)=TRange  // Foo() { 1..10 } for Foo {}
            if (tmp is TRange) and (result.Counter=nil) then
                result.InExpression:=tmp  // for 1..10 {}
            else
            begin
              if result.Counter<>nil then
              begin
                if result.Counter.VariableType=nil then
                begin
                  result.Counter.TypeInferred:=True;
                  result.Counter.VariableType:=Checker.GetDataType(AParent,tmp);
                end;

                CheckAssign(result.Counter,tmp,False,OldExp);
              end;

              result.First:=tmp;  // 1 to 10
            end;
          end;

          // Check again InExpression, it has maybe been created, eg: "for 1..10"
          if result.InExpression=nil then
          begin
            PositionToAdd:=Position;

            if Expect(TSyntax._Reserved_to) then
            begin
              AddPositionKeyword(result,TSyntax._Reserved_to);

              // Finish
              OldExp:=Position;
              OldCount:=Positions.Count;

              tmp:=GetExpression(AParent,result);

              if tmp=nil then
              begin
                //Position:=OldExp;
                Positions.DeleteFrom(OldCount);
                Error(OldExp,10,TErrors._EmptyItem);
              end
              else
              begin
                if result.Counter<>nil then
                   CheckAssign(result.Counter,tmp,False,OldExp);

                result.Last:=tmp;
              end;
            end;
          end;

          // TODO: STEP pending:  "for x:=Y to Z STEP W"

          // TODO: Check (compile time) Finish >= Start
          // Error: "For" loop will never be performed
        end;
      end;

      // pass result to enable "for X" counter to be accessible inside "for"
      result.Block:=GetStatement(result);
      CheckIsEmpty(result.Block);
    end;

    function IsLoop(const ANode:TNode):Boolean;
    begin
      result:=(ANode is TLoopStatement) or (ANode is TFor);
    end;

    procedure CheckIsLoop(const IsBreak:Boolean);
    var tmp : TNode;
        tmpL : Integer;
    begin
      tmp:=AParent;

      while tmp<>nil do
      begin
        if IsLoop(tmp) then
           Exit
        else
           tmp:=tmp.Owner;
      end;

      if IsBreak then
         tmpL:=Length(TSyntax._Reserved_break)
      else
         tmpL:=Length(TSyntax._Reserved_continue);

      Error(Position,tmpL,TErrors._CannotBreakContinue);
    end;

    function GetBreak:TBreak;
    begin
      CheckIsLoop(True);

      result:=TBreak.Create;
      result.Owner:=AParent;

      AddPositionKeyword(result,TSyntax._Reserved_break);
    end;

    function GetContinue:TContinue;
    begin
      CheckIsLoop(False);

      result:=TContinue.Create;
      result.Owner:=AParent;

      AddPositionKeyword(result,TSyntax._Reserved_continue);
    end;

    // Special case, use "result" return as the owner of GetExpression, so
    // anything attached to "return" is owned by it (avoid memory leaks when destroying "return")
    function CreateReturnFunction(const AParent:TNode):TReturn;
    begin
      result:=DoCreateReturn(AParent,True);

      FinishReturn(AParent,result,GetExpression(AParent,result,False));
    end;

    function GetReturn:TReturn;
    var tmpOut : TType;
    begin
      if FindReturnBlock(AParent,tmpOut) then
      begin
        if tmpOut=nil then
           result:=CreateReturn(AParent,nil,True)   // Empty return (method)
        else
           result:=CreateReturnFunction(AParent); // Function
      end
      else
      begin
        result:=nil;  // return keyword in wrong place (not inside a method)
        Error(Position,Length(TSyntax._Reserved_return),TErrors._CannotReturnHere)
      end;
    end;

    function GetCatch(const AOwner:TTry):TCatch;
    var tmp : String;
        tmpClauses : TClauses;
        Old : TPosition;
        tmpType : TType;
    begin
      result:=TCatch.Create;
      result.Owner:=AOwner;

      if not PeekBeginBlock then
      begin
        PositionToAdd:=Position;

        result.Error:=TryGetType(AParent,result,False);

        if result.Error=nil then
        begin
          Old:=Position;

          tmp:=GetToken;

          if tmp='' then
          begin
            result.Free;
            result:=nil;

            Error(TErrors._EmptyItem,nil);
            Exit;
          end
          else
          begin
            PositionToAdd:=Position;

            tmpClauses.Reset;
            tmpClauses.Final:=True;

            if GetSymbol(result,TSyntax._TypeDelimiter) then // :
               tmpType:=GetTypes(AParent,result)
            else
               tmpType:=TChecker._Types[TChecker._Type];

            result.Error:=CreateVariable(result,tmp,tmpClauses,tmpType); // Type

            AddPosition(result,Old,tmp);
          end;
        end
        //else
        //  AddPosition(result.Error,tmp);
      end;

      result.Block:=GetStatement(result);
    end;

    function GetTry:TTry;
    var L : Integer;
        AnyFound : Boolean;
        tmp : TCatch;
    begin
      result:=TTry.Create;
      result.Owner:=AParent;

      AddPositionKeyword(result,TSyntax._Reserved_try);

      result.Block:=GetStatement(result);
      CheckIsEmpty(result.Block);

      AnyFound:=False;

      repeat
        PositionToAdd:=Position;

        if PeekIsGet(TSyntax._Reserved_catch) then
        begin
          AddPositionKeyword(result,TSyntax._Reserved_catch);

          tmp:=GetCatch(result);

          if tmp=nil then
             break
          else
          begin
            L:=Length(result.Catch);
            SetLength(result.Catch,L+1);
            result.Catch[L]:=tmp;

            AnyFound:=True;
          end;
        end
        else
        if PeekIsGet(TSyntax._Reserved_finally) then
        begin
          AddPositionKeyword(result,TSyntax._Reserved_finally);

          result.TheFinally:=GetStatement(result);
          CheckIsEmpty(result.TheFinally);

          AnyFound:=True;
          break;
        end
        else
          break;

      until False;

      if not AnyFound then
         Error(TErrors._CatchOrFinallyExpected,result);
    end;

    function CanBeCalled(const AData:TData):Boolean;
    begin
      if AData is TMember then
         result:=CanBeCalled(TMember(AData).Member)
      else
         result:=(AData is TDataCall) or (AData is TAncestor) or
                 TChecker.IsDataFunctionType(AData);
    end;

    function DoCallData(const AOwner:TNode; const AData:TData):TCallData;
    begin
      result:=TASTCreator.CreateCallData(AOwner,AData); // no return
      TryChangeOwnership(AData,AOwner,result);
    end;

    function Call_or_Assign_or_Return(const AOwner:TNode):TStatement;

      procedure TryFreeArrayData(const AArray:TArrayExpression);
      begin
        if AArray.Data<>nil then
           if not (AArray.Data is TVariable) then
           if AArray.Data.Owner=AParent {AOwner} then
           begin
             AArray.Data.Free;   // Foo[3]:=123 (result of call cannot be assigned)
             AArray.Data:=nil;
           end;
      end;

    var tmpData : TData;
        tmp : TRoutine;
        Old : TPosition;
        OldPos : Integer;
        tmpArithmetic : TArithmetic_Assign;
    begin
      result:=nil;

      Old:=Position;
      OldPos:=Positions.Count;

      tmpData:=Get(AParent,AOwner,False);

      if tmpData<>nil then
      begin
        if PeekAssignment(tmpArithmetic) then // := += etc
           result:=GetAssignment(AParent,AOwner,tmpArithmetic,tmpData)
        else
        if CanBeCalled(tmpData) then
        begin
          if TChecker.IsCallableVariable(tmpData) then
             tmpData:=VariableCall(AOwner,TVariable(tmpData),TVariable(tmpData).VariableType,False);

          result:=DoCallData(AOwner,tmpData); // Foo()
        end
        else
        begin
          // Obtain routine we are inside at:
          tmp:=TChecker.GetRoutineOf(AParent);

          if (tmp<>nil) and (TRoutine(tmp).Output<>nil) then // procedure (not a function)
          begin
            result:=CreateReturn(AParent,tmpData,False);
            Positions.Items[OldPos].Node:=result; // <-- replace tmpData with Return
          end
          else
          begin
            Positions.DeleteFrom(OldPos);
            Error(Old,10,TErrors._ExpressionNotCallable,tmpData);
          end;
        end;
      end;

      if result=nil then
      begin
        // TODO : Improve more situations
        if tmpData<>nil then
        begin
          if tmpData is TArrayExpression then
             TryFreeArrayData(TArrayExpression(tmpData));

          if not (tmpData is TVariable) then
             FreeIfNotOwned(tmpData,AParent {AOwner?});
        end;

        Positions.DeleteFrom(OldPos);
        //Position:=Old;
      end;
    end;

  begin
    PositionToAdd:=Position;

    if PeekSymbolGet(TSyntax._BeginBlock) then result:=GetBlock(AParent) else
    if PeekIsGet(TSyntax._Reserved_if) then result:=GetIf else
    if PeekIsGet(TSyntax._Reserved_while) then result:=GetWhile else
    if PeekIsGet(TSyntax._Reserved_repeat) then result:=GetRepeat else
    if PeekIsGet(TSyntax._Reserved_for) then result:=GetFor else
    if PeekIsGet(TSyntax._Reserved_when) then result:=GetWhen else
    if PeekIsGet(TSyntax._Reserved_break) then result:=GetBreak else
    if PeekIsGet(TSyntax._Reserved_continue) then result:=GetContinue else
    if PeekIsGet(TSyntax._Reserved_return) then result:=GetReturn else
    if PeekIsGet(TSyntax._Reserved_try) then result:=GetTry
    else
      result:=Call_or_Assign_or_Return(AParent);
  end;

  // Foo is (X:Integer):Text {}    // Lambda/Routine declaration
  function GetFunctionType(const AParent:TNode; const AName:String):TFunctionType;
  var tmpError : Boolean;
  begin
    result:=TFunctionType.Create;
    result.Owner:=AParent;

    result.Name:=AName; // Foo

    AddPosition(result,AName);

    result.Parameters:=GetParameters(TSyntax._EndParameters,AParent,result,nil,True,tmpError);
    //GetDeclaredParameters(TSyntax._EndParameters,AParent,result,nil,tmpError);

    if not tmpError then
    begin
      CheckParameterTypes(result.Parameters);

      PositionToAdd:=Position;

      if GetSymbol(result,TSyntax._TypeDelimiter) then
      begin
        PositionToAdd:=Position;
        result.Output:=GetTypes(AParent,AParent);
      end;
    end;
  end;

  // with ast, ...
  function GetWiths(const AParent:TNode):Boolean;

    procedure AddPositionWith(const AWith:TWith; AName:String);
    var i : Integer;
        tmp : String;
    begin
      tmp:=TASTUtils.NodeName(AWith);

      if tmp<>AName then
      begin
        repeat
          i:=Pos(TSyntax._NamespaceDelimiter,AName);

          if i>0 then
          begin
            tmp:=Copy(AName,1,i-1);

            AddPosition(AWith,PositionToAdd,tmp,TPositionStyle.Identifier,False);
            PositionToAdd.Increment(Length(tmp));

            AddPositionSymbol(AWith,TSyntax._NamespaceDelimiter);
            PositionToAdd.Increment(1);

            AName:=Copy(AName,i+1,Length(AName));
          end;

        until i=0;
      end;

      AddPosition(AWith,AName);
    end;

    function ExistsWith(const ANodes:TNodes; const AWith:TWith):Boolean;
    var N : TNode;
    begin
      for N in ANodes do
          if N is TWith then
             if TWith(N).Module=AWith.Module then
                if Checker.TextIs(TWith(N).Alias,AWith.Alias) then
                   Exit(True);

      result:=False;
    end;

    function ParseWithModules(const AType:TType):TWith;

      function TryGetName(out Quoted:Boolean):String;
      begin
        Quoted:=GetTokenText(result);

        if not Quoted then  // with "Foo"
           result:=GetTokenWithDots;  // with A.B.C
      end;

    var tmpAlias,
        tmpName : String;

        tmpPosAssign,
        tmpPosAlias,
        tmp,
        tmpPos : TPosition;

        tmpModule : TNamedType;

        tmpQuoted,
        tmpFirst : Boolean;

        Old : Integer;
    begin
      result:=nil;

      tmp:=PositionToAdd;

      tmpFirst:=True;

      repeat
        Old:=Positions.Count;

        tmpPos:=Position;

        tmpName:=TryGetName(tmpQuoted);

        tmpPosAssign:=Position;

        if (not tmpQuoted) and PeekSymbolGet(TSyntax._Assignment) then     // with Foo:=A.B.C
        begin
          tmpAlias:=tmpName;

          tmpPosAlias:=tmpPos;
          tmpPos:=Position;

          tmpName:=TryGetName(tmpQuoted);
        end
        else
          tmpAlias:='';

        tmpModule:=GetParsedModule(ConvertPathDelimiter(tmpName));

        if tmpModule<>nil then
        begin
          result:=TASTCreator.CreateWith(AParent,tmpModule);

          if tmpFirst then
          begin
            PositionToAdd:=tmp;
            AddPositionKeyword(result,TSyntax._Reserved_with);

            tmpFirst:=False;
          end;

          if tmpAlias<>'' then
          begin
            PositionToAdd:=tmpPosAssign;
            AddPositionSymbol(result,TSyntax._Assignment);

            result.Alias:=tmpAlias;

            PositionToAdd:=tmpPosAlias;
            AddPosition(result,result.Alias);
          end;

          // Re-add quotes to include them in positions map
          if tmpQuoted then
             tmpName:=Text[tmpPos.Position]+tmpName+Text[tmpPos.Position+Length(tmpName)+1];

          PositionToAdd:=tmpPos;

          if result.Alias='' then
             if tmpQuoted then
                AddPosition(result,tmpPos,tmpName,TPositionStyle.Identifier,False)
             else
                AddPositionWith(result,tmpName)
          else
             AddPosition(result,tmpPos,tmpName,TPositionStyle.Literal);

          if result.Module<>Modules.SystemModule then // <-- "with sys" can coexist with implicit sys
             if ExistsWith(AType.Items,result) then
                Error(TErrors._Duplicate,result);

          if not AddToNodes(AParent,AType.Items,result,Old) then
             result:=nil;
        end
        else
          break;

        if not GetSymbol(result,TSyntax._ItemDelimiter) then
           break;

      until tmpName='';
    end;

  var //tmpWith : TWith;
      //tmpBlock : TBlockStatement;
      tmpType : TType;
  begin
    PositionToAdd:=Position;

    result:=PeekIsGet(TSyntax._Reserved_with);

    if result then
    begin
      tmpType:=Finder.FindParentTypeOf(AParent);

      {$IFDEF INTERNAL}
      if tmpType=nil then
         InternalError(Position,'Cannot find parent type of: '+TASTUtils.NameOf(AParent,True));
      {$ENDIF}

      {tmpWith:=}ParseWithModules(tmpType);

      {
      // TODO: Pending decide if "with" can support scoped block, or not

      PositionToAdd:=Position;

      if PeekIsGet(TSyntax._BeginBlock) then
      begin
        tmpBlock:=CreateBlockStatement(tmpWith);
        AddPositionSymbol(tmpBlock,TSyntax._BeginBlock);

        FillBlock(TBlockStatement(tmpBlock).Block);

        Add(AParent,tmpType.Items,tmpBlock);
      end;
      }
    end;
  end;

  function CreateClassType(const APosition:TPosition; const AName:String; const AParameters:TNodes):TClassType;

    // Set shared items (enable type-level access)
    procedure SetShared(const AItems:TNodes);
    var N : TNode;
        tmp : String;
    begin
      for N in AItems do
          if N is TVariable then
          begin
            if TVariable(N).Clauses.Final then
               TVariable(N).Clauses.Shared:=True
            else
            begin
              tmp:=TVariable(N).Name;
              Error(Position,Length(tmp),TErrors._EnumNotFinal,tmp);
              break;
            end;
          end
          {$IFDEF INTERNAL}
          else
             InternalError(N,'Enum item is not a variable');
          {$ENDIF}
   end;

    // is { A,B,C }  // enumeration
    function CreateEnumeration(const APosition:TPosition):TClassType;
    var tmpError : Boolean;
    begin
      result:=TASTCreator.CreateClassType(AParent,AName,Clauses,nil);
      AddPosition(result,AName);

      PositionToAdd:=APosition;
      AddPositionSymbol(result,TSyntax._BeginBlock);

      result.Items:=GetParameters(TSyntax._EndBlock,AParent,result,
                    TChecker._Types[TChecker._Data],True,tmpError); // Data

      if not tmpError then
         SetShared(result.Items);
    end;

    procedure SetAncestor(const AClass:TClassType; const AType:TType);
    begin
      if (AType is TArrayType) or
         (AType is TTypeMember) or
         (AType is TGenericType) then
         AType.Owner:=AClass;  // <-- fix leak

      AClass.Ancestor:=AType;
    end;

    function NewClassType(const AName:String;
                          const AParameters:TNodes):TClassType;
    begin
      result:=TASTCreator.CreateClassType(AParent,AName,Clauses,AParameters);
      AddPosition(result,AName);

      if TChecker._Types[TChecker._Boolean]=nil then
         Checker.SpecialBoolean(AName,result);
    end;

    function GetAncestorOfClass(const AClass:TClassType; const AType:TType):TType;
    begin
      PositionToAdd:=Position;

      if GetSymbol(AClass,TSyntax._BeginParameters) and (AType is TParametersType) then
         result:=CreateSpecialized(AParent,AClass,TParametersType(AType))
      else
         result:=AType;
    end;

    function VerifyAncestorRecursive(const AClass:TClassType; const AType:TType):Boolean;
    var tmp : TType;
    begin
      result:=False;

      tmp:=TChecker.GetFinalType(AType);

      if tmp=AClass then
      begin
        result:=True;
        Error(_BaseClassCannotbeSame,AType);  // class:  a is a {}
      end
      else
      if AType is TArrayType then
      begin
        tmp:=TArrayType(AType).TheType;
        tmp:=TChecker.GetFinalType(tmp);

        if tmp=AClass then
        begin
          result:=True;
          Error(_BaseClassCannotbeSame,AType); // class:  a is a[] {}
        end;
      end;
    end;

  var Old,
      Old3,
      OldType : TPosition;

      tmpType : TType;

      OldCount,
      tmpPos : Integer;
  begin
    Old:=Position;

    if (AParameters=nil) and PeekSymbolGet(TSyntax._BeginBlock) then
       result:=CreateEnumeration(Old)
    else
    begin
      // is XXX

      Old:=Position;

      //Old2:=APosition;

      Old3:=PositionToAdd;

      OldCount:=Positions.Count;

      result:=NewClassType(AName,AParameters);

      tmpPos:=Positions.Count-1;

      OldType:=Position;

      tmpType:=GetTypes(AParent,result);

      if tmpType=nil then  // no inheritance, no problem
      else
      if tmpType.Clauses.Final then
      begin
        Error(TErrors._CannotDeriveFromFinal,tmpType);

        {
         unreachable
        if tmpType.Owner=result then
           tmpType.Free;
        }

        Positions.RemoveLastNode(result);

        result.Free;
        result:=nil;
      end
      else
      if tmpType is TRangeType then // is 1..100
      begin
        result.Free;

        result:=TASTCreator.CreateIntegerClass(AParent,AName,TRangeType(tmpType),AParameters);

        Positions.Items[tmpPos].Node:=result;
      end
      else
      begin
        PositionToAdd:=Old3;

        // a is a {}
        // a is a[] {}
        if VerifyAncestorRecursive(result,tmpType) then
        begin
          Position:=OldType;
          Positions.DeleteFrom(OldCount);

          if tmpType<>result then
             result.Free;

          tmpType.Free;
          result:=nil;
        end
        else
          SetAncestor(result,GetAncestorOfClass(result,tmpType));
      end;
    end;
  end;

  function GetExtender(const AName:String):TExtender; overload;
  begin
    result:=GetExtender(AName,GetType(AParent,AName));
  end;

  function TryGetExtender(AName:String; AType:TType):TExtender;
  var Old2,
      Old3,
      Old4 : TPosition;

      tmpS : String;
      tmpData : TData;
      tmpOwner : TNode;
  begin
    result:=nil;

    Old3:=Position;

    if PeekSymbolGet(TSyntax._DataMember) then
    begin
      Old2:=Position;

      tmpS:=GetToken;

      if tmpS<>'' then
      begin
        Old4:=PositionToAdd;
        PositionToAdd:=Old2;

        tmpOwner:=TNode.Create; // dummy
        try
          tmpData:=FindDataRecursive(AType,tmpOwner,tmpS);

          if tmpData=nil then
          begin
            Position:=Old2;
            PositionToAdd:=Old4;

            // Extender:   Integer.Foo(a:Text):Boolean { ... }
            result:=GetExtender(AName,AType);

            PositionToAdd:=Old3;
            AddPositionSymbol(result,TSyntax._DataMember);
          end
          else
          if tmpData.Owner=tmpOwner then
             tmpData.Free;
        finally
          tmpOwner.Free;
        end;
      end;
    end;
  end;

  function NameOfType(const AType:TType):String;
  var tmp : TType;
  begin
    if AType is TNamedType then
       result:=TNamedType(AType).Name
    else
       result:='';

    if result='' then
       if AType is TArrayType then
       begin
         tmp:=TArrayType(AType).TheType;

         if tmp is TNamedType then
            result:=NameOfType(TNamedType(tmp))
         else
            result:='';
       end;
  end;

  // Foo :
  function RoutineOrVariable(const AName:String; const AParameters:TNodes; const HasParameters:Boolean):TNode;

    function SameNodes(const A,B:TNodes):Boolean;
    var t : Integer;
    begin
      result:=Length(A)=Length(B);

      if result then
         for t:=0 to High(A) do
             if A[t]<>B[t] then
                Exit(False);
    end;

    function TrySelf(const AClass:TClassType; const AType:TType):TType;
    begin
      result:=AType;

      if result is TSpecializedType then
         if TSpecializedType(result).TheType=AClass then
            if SameNodes(TSpecializedType(result).Generics,AClass.Parameters) then
            begin

              // Ugly workaround to replace Node before destroying it:
              if Positions.Items[Positions.Count-1].Node=AType then
                 Positions.Items[Positions.Count-1].Node:=AClass;

              AType.Free;
              result:=AClass;
            end;
    end;

  var tmpType : TType;
      Old,
      Old2 : TPosition;
      tmpReownType: Boolean;
  begin
    Old:=PositionToAdd;

    result:=TASTCreator.CreateRoutine(AParent,'',Clauses,AParameters);

    if PeekIs(TSyntax._TypeDelimiter) then // Type inference, leave nil
       tmpType:=nil
    else
    begin
      tmpType:=GetTypes(result,result);

      if tmpType<>nil then
         if tmpType=result then
            tmpType:=nil
         else
         if tmpType.Owner=result then
            tmpType:=TrySelf(TChecker.GetClassTypeOf(AParent),tmpType);
    end;

    Old2:=Position;

    if PeekSymbolGet(TSyntax._BeginBlock) then // New routine {
    begin
      PositionToAdd:=Old;

      TRoutine(result).Name:=AName;

      AddPosition(result,AName);

      TRoutine(result).Output:=tmpType;

      if tmpType<>nil then
         TryChangeOwnership(tmpType,AParent,result);

      AddPosition(result,Old2,TSyntax._BeginBlock,TPositionStyle.Symbol);

      FillBlock(TRoutine(result));
    end
    else
    if HasParameters then
       Expect(TSyntax._BeginBlock)
    else
    // New variable
    // Foo : Bar
    // Foo : Bar(Param1,Param2,...)
    begin
      tmpReownType:=(tmpType<>nil) and (tmpType.Owner=result);

      if tmpReownType then
         tmpType.Owner:=nil;

      result.Free;

      PositionToAdd:=Old;

      result:=GetVariable(AParent,AParent,AName,tmpType,Clauses);

      if tmpReownType then
         tmpType.Owner:=result
      else
      if (tmpType is TArrayType) or (tmpType is TTypeMember) then // <-- correct?
         tmpType.Owner:=result;
    end;
  end;

  // is (x:Integer) {}
  // is MyAncestor {}
  function FunctionTypeOrClass(const APosition:TPosition; const AName:String; const AParameters:TNodes):TType;
  var Old : TPosition;
  begin
    Old:=Position;

    if PeekSymbolGet(TSyntax._BeginParameters) then // function type
    begin
      result:=GetFunctionType(AParent,AName);

      PositionToAdd:=Old;
      AddPositionSymbol(result,TSyntax._BeginParameters);
    end
    else
      result:=CreateClassType(APosition,AName,AParameters); // class

    if result<>nil then
       if TChecker.IsEnumerationClass(result) then // <-- enumeration is { A,B,C }
       else
       begin
         if ExpectSymbol(result,TSyntax._BeginBlock) then
            if result is TFunctionType then
               ExpectSymbol(result,TSyntax._EndBlock)  // <-- block must be empty !
            else
               FillBlock(TType(result));
       end;
  end;

  procedure GetAncestorRoutine(const ARoutine:TRoutine);
  var tmp : TRoutine;
  begin
    if AParent is TClassType then
       if TClassType(AParent).Ancestor<>nil then
       begin
         tmp:=Finder.FindOverride(
                  TFinder.GetAncestorType(TClassType(AParent)),
                  ARoutine); // override

         if tmp<>nil then
            if TChecker.IsFinalRoutine(tmp) then
               Error(TErrors._CannotOverrideFinal,tmp.Name)
            else
               ARoutine.Ancestor:=tmp;
       end;
  end;

  function ProcedureOrClass(const Old:TPosition;
                            const AName:String;
                            const HasParameters:Boolean;
                            const AParameters:TNodes):TNode;
  begin
    if HasParameters then // Foo() {}
    begin
      CheckParameterTypes(AParameters);

      result:=TASTCreator.CreateRoutine(AParent,AName,Clauses,AParameters);
      AddPosition(result,AName);

      GetAncestorRoutine(TRoutine(result));
    end
    else
    begin
      result:=TASTCreator.CreateClassType(AParent,AName,Clauses,nil); // Foo {}
      AddPosition(result,AName);
    end;

    AddPosition(result,Old,TSyntax._BeginBlock,TPositionStyle.Symbol);

    FillBlock(TType(result));
  end;

  function TryGetStatement:TStatement;
  var Old : TPosition;
  begin
    Old:=Position;
    result:=GetStatement(AParent);

    if result=nil then
       Error(Old,10,TErrors._BadStatement);
  end;

  function CreateFinalVariable(const AName:String; const APosition:TPosition):TVariable;
  begin
    result:=CreateVariable(AParent,AName,Clauses,nil);

    AddPosition(result,AName);

    PositionToAdd:=APosition;
    AddPositionSymbol(result,TSyntax._Assignment);

    SetVariableData(AParent,result);
  end;

  // normal assignment a:=b, or += -= *= /= arithmetic assignments
  function FinishAssignment(const AParent:TNode;
                            const AName:String;
                            const AOld:TPosition;
                            const Arithmetic:TArithmetic_Assign):TAssignment;
  var tmpData : TData;
      OldCount : Integer;
  begin
    OldCount:=Positions.Count;
    tmpData:=GetData(AParent,AParent,AName,True,True);

    if tmpData=nil then
       result:=nil
    else
    begin
      PositionToAdd:=AOld;
      result:=GetAssignment(AParent,AParent,Arithmetic,tmpData);

      if result=nil then
         if tmpData.Owner=AParent then
             if (tmpData is TSelf) or (tmpData is TTypeCall) then
             begin
               Positions.DeleteFrom(OldCount);
               tmpData.Free;
             end;
    end;
  end;

  function TryParseMultipleVariables(const AName:String; out ANode:TNode):Boolean;

    procedure SetValues(const ANodes:TNodes);
    var t: Integer;
        tmp : TVariable;
        tmpLast : TVariable;
    begin
      tmpLast:=ANode as TVariable;

      SetVariableData(AParent,tmpLast);

      for t:=0 to High(ANodes)-1 do
      begin
        if ANodes[t]<>nil then // <-- might return nil, if duplicate variable name
        begin
          tmp:=ANodes[t] as TVariable;
          tmp.ValueData:=tmpLast.ValueData;

          if tmp.VariableType=nil then
          begin
            tmp.VariableType:=tmpLast.VariableType;
            tmp.TypeInferred:=True;
          end;
        end;
      end;
    end;

  var tmpNodes : TNodes;
      Old : TPosition;
  begin
    ANode:=nil;
    Old:=Position;

    result:=PeekSymbolGet(TSyntax._ItemDelimiter);

    if result then
    begin
      tmpNodes:=GetMultipleVariables(AName);

      PositionToAdd:=Old;

      if tmpNodes=nil then  // Free nodes?
         AddPositionSymbol(nil,TSyntax._ItemDelimiter)
      else
      begin
        ANode:=tmpNodes[High(tmpNodes)];

        AddPositionSymbol(ANode,TSyntax._ItemDelimiter);

        // :=
        if GetSymbol(ANode,TSyntax._Assignment) then
           SetValues(tmpNodes);
      end;
    end;
  end;

  function TryParseExtender(const AType:TType):TExtender;
  var tmpFinal : TType;
  begin
    if AType=nil then
       result:=nil
    else
    begin
      tmpFinal:=TChecker.GetFinalType(AType);

      // Only classes (so far) can be extended with new routines or subclasses
      if tmpFinal is TClassType then   // Array.Sort
         result:=TryGetExtender(TClassType(tmpFinal).Name,AType)
      else
         result:=nil;

      if (AType is TGenericType) and (AType.Owner=AParent) then
         AType.Free
      else
      if AType is TTypeMember then
         if result=nil then
            AType.Free
         else
         if AType.Owner=AParent then
            AType.Owner:=result;
    end;
  end;

var tmpType : TType;

    tmpParameters : TNodes;

    tmpValid,
    tmpHasParameters : Boolean;

    tmpName : String;

    tmpArithmetic : TArithmetic_Assign;

    Old,
    OldParam : TPosition;

    OldCount : Integer;
begin
  result:=nil;

  if PeekEndBlock then
     Exit;

  GetClauses(AParent,Clauses);

  if not Pending then
  begin
    Error(TErrors._EmptyItem,nil);
    Exit;
  end;

  // Important !! OldCount placement should be AFTER GetClauses
  OldCount:=Positions.Count;

  PositionToAdd:=Position;

  if GetWiths(AParent) then
  begin
    result:=TAvoidEmpty.Create;
    Exit;
  end;

  tmpName:=GetToken;

  {$IFDEF INTERNAL}
  if tmpName='' then
     InternalError(Position,'Empty token !');
  {$ENDIF}

  if Checker.TextIs(tmpName,TSyntax._Reserved_until) then // special case for "repeat" statement
  begin
    Position:=PositionToAdd;
    Exit;
  end;

  if Checker.IsStatementKeyword(tmpName) then // if (...  <--- not a function !
  begin
    Positions.DeleteFrom(OldCount); // <-- just in case there is a comment // after tmpName !
    Position:=PositionToAdd;
    result:=TryGetStatement;
  end
  else
  begin
    tmpValid:=ValidIdentifier(tmpName); // and (not TChecker.IsReservedKeyword(tmpName));

    if tmpValid and TryParseMultipleVariables(tmpName,result) then
       Exit;

    OldParam:=Position;

    tmpParameters:=TryGetDeclaredParameters(AParent,tmpHasParameters);

    Old:=Position;

    // :=
    if PeekSymbolGet(TSyntax._Assignment) then // <-- before PeekAssign !
    begin
      tmpParameters.Free(AParent);

      if tmpValid or (TChecker.TextIs(tmpName,TSyntax._Reserved_self)) then
      begin
        // final a:=b    (type is always inferred)
        if Clauses.Final and (AParent is TClassType) then
           result:=CreateFinalVariable(tmpName,Old)
        else
           result:=FinishAssignment(AParent,tmpName,Old,TArithmetic_Assign.None);
      end
      else
        Error(Old,Length(tmpName),TErrors._BadIdentifier,tmpName);

    end
    else
    // += -= *= /=
    if PeekAssignment(tmpArithmetic) then
    begin
      tmpParameters.Free(AParent);

      if tmpValid then
         result:=FinishAssignment(AParent,tmpName,Old,tmpArithmetic)
      else
         Error(Old,Length(tmpName),TErrors._BadIdentifier,tmpName);
    end
    else
    // :
    if PeekSymbolGet(TSyntax._TypeDelimiter) then
    begin
      if tmpValid then
      begin
        result:=RoutineOrVariable(tmpName,tmpParameters,tmpHasParameters);

        if result=nil then
           tmpParameters.Free(AParent);

        PositionToAdd:=Old;
        AddPositionSymbol(result,TSyntax._TypeDelimiter);
      end
      else
      begin
        tmpParameters.Free(AParent);
        Error(Old,Length(tmpName),TErrors._BadIdentifier,tmpName);
      end;

    end
    else
    // is
    if PeekIsGet(TSyntax._Reserved_is) then
    begin
      if tmpValid then
      begin
        result:=FunctionTypeOrClass(PositionToAdd,tmpName,tmpParameters);

        if result=nil then
           tmpParameters.Free(AParent);

        PositionToAdd:=Old;
        AddPositionKeyword(result,TSyntax._Reserved_is);
      end
      else
      begin
        tmpParameters.Free(AParent);
        Error(Old,Length(tmpName),TErrors._BadIdentifier,tmpName);
      end;

    end
    else
    // {
    if PeekSymbolGet(TSyntax._BeginBlock) then
    begin
      (*
       unreachable, due to IsStatementKeyword above
      if Checker.TextIs(tmpName,TSyntax._Reserved_repeat) then // special case:  repeat {} until true
      begin
        tmpParameters.Free(AParent);

        Position:=PositionToAdd;
        result:=GetStatement(AParent);
      end
      else
      *)
      if tmpValid then
      begin
        result:=ProcedureOrClass(Old,tmpName,tmpHasParameters,tmpParameters);

        if result=nil then
           tmpParameters.Free(AParent);
      end
      else
      begin
        tmpParameters.Free(AParent);

        result:=nil;
        Error(Old,Length(tmpName),TErrors._BadIdentifier,tmpName);
      end;
    end
    else
    begin
      // Not valid, destroy them
      tmpParameters.Free(AParent);

      if OldCount<Positions.Count then
         Positions.DeleteFrom(OldCount);

      Position:=OldParam;

      Old:=PositionToAdd;
      OldCount:=Positions.Count;

      if not Checker.TextIs(tmpName,TSyntax._BeginBlock) then // module-level { } main block
      begin
        // Try to parse an extender SomeType.SomeSubType.SomeProc  (or .SomeClass)

        if tmpValid then
        begin
          tmpType:=TryGetType(Old,AParent,AParent,tmpName);

          result:=TryParseExtender(tmpType);
        end
        else
          result:=nil;
      end;

      if result=nil then
      begin
        // Go back
        Position:=Old;
        Positions.DeleteFrom(OldCount);
        result:=TryGetStatement;
      end;
    end;
  end;
end;

function TBee.DoParse(const AText:String):TNode;

  procedure Loop; // <-- TODO: Merge with DoFillBlock
  var tmp : TNode;
      tmpClauses : TClauses;
      Old : Integer;
  begin
    while Pending do
    begin
      tmpClauses.Reset;

      Old:=Positions.Count;

      tmp:=ParseBlock(Context,tmpClauses);

      if tmp=nil then
         break
      else
      if tmp is TAvoidEmpty then
         tmp.Free
      else
         AddNewItem(Context,tmp,Context.Items,Old);
    end;
  end;

  function WithSys(const AParent:TNode):TWith;
  var tmp : TNamedType;
  begin
    // Do not use FindModule here, use Modules.Find
    tmp:=Modules.Find(TModules._SystemModule);

    if tmp=nil then
       tmp:=GetParsedModule(TModules._SystemModule);

    {$IFDEF INTERNAL}
    if tmp=nil then
       InternalError(nil,'Sys module not found');
    {$ENDIF}

    result:=TASTCreator.CreateWith(AParent,tmp);
    result.SystemModule:=True;
  end;

  procedure TryAddSystem;
  begin
    if not Checker.TextIs(ModuleName,TModules._SystemModule) then
       AddToNodes(Context,Context.Items,WithSys(Context),0);
  end;

begin
  Init(AText);

  Context:=TNamedType.Create;

  Context.Clauses.Shared:=True; // Allow: "with Z  X::=Z.Y"

  TryAddSystem;

  SkipBlanks;
  Loop;

  if Pending then
     Error(Position,10,TErrors._ExtraCodeNotAllowed,Context);

  result:=Context;
end;

function TBee.Parse(const AText:String):TNode;
begin
  result:=DoParse(AText);
end;

// 2*4+(5/6)-3*6
class function TBee.ParseExpression(const AParent: TNode; const AText: String): TData;

  function GetContext:TType;
  begin
    if AParent is TType then
       result:=TType(AParent)
    else
       result:=TType.NewType(AParent);
  end;

var P : TBee;
begin
  P:=TBee.Create;
  try
    P.Positions:=TNodePositions.Create;
    try
      P.Init(AText);

      P.Context:=GetContext;

      P.SkipBlanks;

      result:=P.GetExpression(AParent,AParent);

      if P.Context.Owner=AParent then
         P.Context.Free;
    finally
      P.Positions.Free;
    end;
  finally
    P.Free;
  end;
end;

end.
