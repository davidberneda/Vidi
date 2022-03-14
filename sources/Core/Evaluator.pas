unit Evaluator;

interface

uses
  Sys, AST,
  Find.AST,

  {$IFDEF FPC}
  FPC_StopWatch,
  {$ELSE}
  Diagnostics,
  {$ENDIF}

  Exceptions, Instance_Type, Run_Breaks, RunningNode, Syntax;

type
  TEvaluate=class
  private
    class function AsDataDirect(const AData: TData; const AInstance: TInstance): TData; static;
    class function OperandAsData(const AOperand:TOperand; const AInstance: TInstance): TData; static;
  public
    class function AsData(AData:TData; const AInstance:TInstance):TData; static;

    class function AsBoolean(AData:TData; const AInstance:TInstance=nil):Boolean; static;
    class function AsFloat(AData:TData; const AInstance:TInstance=nil):Float; static;
    class function AsInteger(AData:TData; const AInstance:TInstance=nil):Int64;
    class function AsText(AData:TData; const AInstance:TInstance=nil):String; overload; static;

    class procedure ArrayRange(const A:TArrayExpression;
                               const AInstance:TInstance;
                               const ArrayItems:TNodes;
                               const ADimension:Integer;
                               out AMin,AMax:Integer);

    class function DefaultArray(const AOwner:TNode; const AArray:TArrayType; const AInstance:TInstance): TArrayExpression; static;
    class function DefaultValueOf(const AOwner:TNode; const AType:TType; const AInstance:TInstance): TData; static;

    class function InstanceOf(const AInstance:TInstance; const AVariable:TVariable):TInstance; overload; static;
    class function InstanceOf(const AInstance:TInstance; const AData:TData):TInstance; overload; static;
  end;

  TOnClear=procedure of object;
  TOnPut=procedure(const AText:String) of object;

  TBreakEvent=procedure(Sender:TObject; const AIndex:Integer) of object;

  TRunning=(Running,Stopped,Paused);

  TRunner=class;

  {$IFDEF INTERNAL}
  {$DEFINE RUNSTACK}
  {$ENDIF}

  TRunningItem=class(TRunningNode)
  public
    Runner : TRunner;
  end;

  TRunExceptionCode=(None,
                     ErrorInternal,
                     DivisionByZero,
                     ArrayIndexGreater,
                     ArrayIndexLower);

  TRunException=record
  public
    Code : TRunExceptionCode;
    Text : String;
    // Node : TNode;  <-- problem, Node will be destroyed when this record might be used later

    function FullText:String;
  end;

  TRunner=class
  private
    FLast : TRunningNode;

    ProfileIndex : Integer;
    StartTime : TStopWatch;

    procedure BeginProfile(const AType:TType);
    procedure EndProfile;

    class function Call(const AInstance:TInstance; const AData:TData):TNode; overload; static;
    class function CallSetter(const AInstance,AValue:TInstance; const ACall:TDataCall):TData; overload; static;

    class procedure Call(const AInstance:TInstance; const ACall:TDataCall;
                         const ASetter:TInstance;
                         out AResult:TData); overload; static;

    procedure DoMonitor;
    function DoStart(const WithDebug:Boolean; const ANode:TNode):TRunException;

    class function ExecuteFor(const AInstance:TInstance; const AFor:TFor):TNode; static;
    class function Execute(const AInstance:TInstance; const AType:TType):TNode; overload; static;
    class function Execute(const AInstance:TInstance; const ANode:TNode):TNode; overload; static;

    function ExecuteNext(const AInstance:TInstance; const ANode:TNode):TNode;

    procedure Initiate_Run(const WithDebug:Boolean; const ANode:TNode);

    procedure RuntimeException(const ARuntimeError:TRunExceptionCode; const AText:String{; const ANode:TNode}); overload;
    procedure RuntimeException(const ARuntimeError:TRunExceptionCode; const AError:TErrors {; const ANode:TNode}); overload;

    procedure SetLast(const AValue:TRunningNode);
  public
    Finder : TFinder;

    Current : TNode;
    Root : TInstance;
    Stack : TRunStack;
    Status : TRunning;

    Breaks : TBreaks;
    OneTimeBreak : TNode;
    LastBreak : TNode;
    OnBreak : TBreakEvent;

    RunException : TRunException;

    type
      TOnMonitor=procedure(const Sender:TRunner) of object;

    class var

      {$IFDEF INTERNAL}
      CheckInstanceLeaks,
      ExtraMonitorRefresh : Boolean;
      {$ENDIF}

      OnClear : TOnClear;
      OnPut : TOnPut;

      OnMonitor : TOnMonitor;

    Constructor Create;
    Destructor Destroy; override;

    class procedure ClearHooks; static;

    function CurrentNode:TNode;

    procedure Loop;
    procedure Pause;
    function Start(const ANode:TNode):TRunException;
    function StartNoDebug(const ANode:TNode):TRunException;
    procedure Step(const ANode:TNode); overload;
    procedure Step(const IsStepOver:Boolean); overload;
    procedure Stop;

    property Last : TRunningNode read FLast write SetLast;
  end;

  TEvaluateCompileTime=class
  public
    class function AsBoolean(const AData:TData; out AValue:Boolean):Boolean; static;
  end;

  TRunningBlock=class(TRunningItem)
  private
    Index : Integer;
    Local : TInstance;
  public
    TheType : TType;

    Destructor Destroy; override;

    function Current:TNode;
    function CurrentNode:TNode;
    function Next:TNode; override;

    class function NextNode(const AType:TType; const AIndex:Integer):TNode; static;
  end;

  EVidiRunException=class(EBeeException)
  public
    class procedure RuntimeError(const AError:String {; const ANode:TNode}); overload; static;
    class procedure RuntimeError(const AError:TErrors {; const ANode:TNode}); overload; static;
  end;

function Runner_DataAsText(const AData:TData):String;

{$IFDEF INTERNAL}
procedure CheckNodeCounter;
function R(const ANode:TRunningNode):String;
{$ENDIF}

implementation

uses
  SysUtils, Math,

  {$IFDEF INTERNAL}
  Emit, Internal,
  {$ENDIF}

  Plugin,
  Module,
  Checker.AST,
  Magic,
  Utils.AST,
  Cloner,
  Profiler,
  Text,
  Sys_Plugin;

procedure Error(const AMessage:String; const ANode:TNode); overload;
begin
  Raise_Exception(AMessage+': '+TASTUtils.NameOf(ANode,True));
end;

procedure Error(const AError:TErrors; const ANode:TNode); overload;
begin
  Error(TErrorTexts.Texts[AError],ANode);
end;

{$IFDEF INTERNAL}
procedure CheckNodeCounter;
begin
  if TNode.Counter>0 then
     InternalError('Leaks: TNode.Counter = '+IntToStr(TNode.Counter),nil);
end;
{$ENDIF}

class procedure EVidiRunException.RuntimeError(const AError:String {; const ANode:TNode});
begin
  raise EVidiRunException.Create(AError); //+': '+TASTUtils.NameOf(ANode,True));
end;

class procedure EVidiRunException.RuntimeError(const AError:TErrors {; const ANode:TNode});
begin
  RuntimeError(TErrorTexts.Texts[AError] {,ANode} );
end;

function TRunException.FullText:String;
begin
  result:=Text;

  {
  if Node<>nil then
     result:=result+': '+TASTUtils.NameOf(Node,True);
  }
end;

procedure VariableNotFound(const AVariable:TVariable);
begin
  Error('Variable not found',AVariable);
end;

// Calculate and FREE AData !!
function CalcAsInteger(AData:TData; const AInstance:TInstance):Int64;
begin
  AData:=TEvaluate.AsData(AData,AInstance);

  if AData is TInteger then
  begin
    result:=TInteger(AData).Value;

    if AData.Owner=nil then
       AData.Free;
  end
  else
  if AData is TTypeCall then  // Enumeration:   Integer[Planets]
     result:=TTypeCall(AData).TheType.Items.Count
  else
  begin
    result:=0;

    {$IFDEF INTERNAL}
    InternalError('Cannot evaluate as Integer',AData);
    {$ENDIF}
  end;
end;

// MyClass is Integer[] {}   <-- returns ancestor as array type
function GetAncestorArrayType(const AType:TType):TArrayType;
begin
  if AType is TArrayType then
     result:=TArrayType(AType)
  else
  if AType is TClassType then
     result:=GetAncestorArrayType(TClassType(AType).Ancestor)
  else
     result:=nil;
end;

procedure GetRangeBounds(const ARange:TRange; const AInstance:TInstance; out AMin,AMax:Integer);
begin
  AMin:=CalcAsInteger(ARange.Min,AInstance);
  AMax:=CalcAsInteger(ARange.Max,AInstance);
end;

function GetArrayTypeOf(const AData:TData):TType;
begin
  if AData is TVariable then
     result:=TVariable(AData).VariableType
  else
  if AData is TSelf then
     result:=TChecker.GetClassTypeOf(AData)
  else
  begin
    result:=nil;

    {$IFDEF INTERNAL}
    InternalError('Cannot determine array type',AData);
    {$ENDIF}
  end;
end;

class procedure TEvaluate.ArrayRange(const A:TArrayExpression;
                     const AInstance:TInstance;
                     const ArrayItems:TNodes;
                     const ADimension:Integer;
                     out AMin,AMax:Integer);

  procedure GetBounds(const ASize:TData);
  begin
    if ASize is TRange then
       GetRangeBounds(TRange(ASize),AInstance,AMin,AMax)
    else
    if ASize is TInteger then
    begin
      AMin:=0;
      AMax:=CalcAsInteger(ASize,AInstance)-1;
    end
    else
    begin
      AMin:=0;
      AMax:=High(ArrayItems);
    end;
  end;

  function GetArrayType(const AType:TType):TType;
  var tmpNode : TNode;
  begin
    result:=GetAncestorArrayType(AType);

    if not (result is TArrayType) then
    begin
      result:=AType;

      if result is TClassType then
      begin
        tmpNode:=TClassType(result).DefaultIndexed;

        if tmpNode is TVariable then
           result:=TVariable(tmpNode).VariableType;
      end;
    end;
  end;

  function GetDimensionType(var AType:TType):Boolean;
  var t : Integer;
  begin
    for t:=0 to ADimension do
    begin
      AType:=GetArrayType(AType);

      {$IFDEF INTERNAL}
      if not (AType is TArrayType) then
      begin
        result:=False;
        InternalError('Expression is not an array. Cannot determine array bounds',A);
        Exit;
      end;
      {$ENDIF}

      if t<ADimension then
         AType:=TArrayType(AType).TheType;
    end;

    result:=True;
  end;

var tmp : TType;
begin
  if A.Data=nil then
  begin
    AMin:=0;
    AMax:=High(A.Parameters);
  end
  else
  begin
    tmp:=GetArrayTypeOf(A.Data);

    if GetDimensionType(tmp) then
       GetBounds(TArrayType(tmp).Size);
  end;
end;

function EnumerationIndexOf(const AData:TVariable):Integer;
var tmp : TNodes;
    t : Integer;
begin
  tmp:=TClassType(AData.Owner).Items;

  for t:=0 to High(tmp) do
      if tmp[t]=AData then
         Exit(t);

  result:=-1;

  {$IFDEF INTERNAL}
  InternalError('Cannot find enumeration item',AData);
  {$ENDIF}
end;

function DataSlotOf(const AInstance:TInstance; const AVariable: TVariable; out NewInst:TInstance): Integer;
begin
  result:=AInstance.Data.Find(AVariable);

  if result=-1 then
  begin
    // special case for no-debug run and calling overriden methods, see Call FindOverriden
    if AInstance.Value is TInstance then
       result:=DataSlotOf(TInstance(AInstance.Value),AVariable,NewInst)
    else
    if AInstance.Owner is TInstance then
       result:=DataSlotOf(TInstance(AInstance.Owner),AVariable,NewInst);
  end
  else
    NewInst:=AInstance;

  {$IFDEF INTERNAL}
  if result=-1 then
     VariableNotFound(AVariable);
  {$ENDIF}
end;

// TODO: Remove "forward" dependency
procedure Construct(const AInstance:TInstance; const AType:TType); forward;

function FromValue(const AOwner:TInstance; const AType:TType; const AValue:TData):TInstance;
begin
  if AType.Plugin=nil then
     result:=TInstance.Create
  else
     result:=TInstanceClass(AType.Plugin).Create as TInstance; // <-- force "as TInstance", to avoid dependency of Instance_type.pas unit

  result.Owner:=AOwner;
  Construct(result,AType);

  result.Value:=AValue;

  if AValue<>nil then
     if AValue.Owner=nil then
        AValue.Owner:=result;
end;

// Future: just let Value to be nil to indicate default value ?
function CheckValue(const AInstance:TInstance; const AOwner:TNode; const AVariable:Integer):TInstance;
var tmpType : TType;
    tmp : TData;
    tmpVariable : TVariable;
    tmpEnum : Boolean;
    tmpInst : TInstance;
begin
  if AInstance.Data[AVariable].Value=nil then
  begin
    tmpVariable:=AInstance.Data[AVariable].Variable;

    tmpEnum:=TChecker.IsSharedVariable(tmpVariable) or TChecker.IsEnumerationItem(tmpVariable);

    if tmpEnum then // <-- Colors.Red
    else
    begin
      tmpType:=tmpVariable.VariableType;

      tmp:=TEvaluate.DefaultValueOf(AOwner,tmpType,AInstance);

      tmpInst:=FromValue(AInstance,tmpType,tmp);

      if tmp<>nil then
         tmp.Owner:=tmpInst;

      AInstance.Data[AVariable].Value:=tmpInst;
    end;
  end;

  result:=AInstance.Data[AVariable].Value;
end;

function CloneData(const AInstance:TInstance; const AOwner:TNode; const AData:TData):TData;
begin
  result:=TCloner.Node(AOwner,TEvaluate.AsData(AData,AInstance)) as TData;
end;

function IsReference(const ANode:TNode):Boolean;
begin
  if ANode is TCastingData then
     result:=IsReference(TCastingData(ANode).Data)
  else
     result:=(ANode is TVariable) and TChecker.VariableIsClass(TVariable(ANode));
end;

function CloneOrReference(const AInstance:TInstance; const AData:TData; const AVariable:TVariable):TInstance;
begin
  (*  NEVER CALLED:
  if IsReference(AData) then
  begin
    result:=TEvaluate.InstanceOf(AInstance,AData as TVariable);

    {$IFDEF INTERNAL}
    if result=nil then
       Error('Instance not found for:',AData);
    {$ENDIF}
  end
  else
  *)
  begin
    result:=TInstance.Create;

    result.Owner:=AInstance;

    if AData.Owner=nil then
    begin
      result.Value:=AData;
      AData.Owner:=result;
    end
    else
    begin
      if (AData is TInstance) or
         (AData is TDataCall) or // <-- functiontype
         (AData is TTypeCall) // <-- inline functiontype
           then
         result.Value:=AData
      else
      begin
        result.Value:=CloneData(AInstance,result,AData) as TData;

        if AVariable<>nil then
           if AVariable.VariableType is TArrayType then
              Construct(result,AVariable.VariableType);
      end;
    end;
  end;
end;

// NOTE: TODO !!! IMPORTANT !!!
// This function is very costly, and the best solution would be to remove it
// completely. Replacing TDivision with a new TIntegerDivision AST node that knows
// to do "div" instead of "/".

// Convert from Float to Integer, ie:   X:Integer:=1/2
function TryRoundFloat(const ATarget,AValue:TData):TData;
begin
  if (AValue is TFloat) and TChecker.IsIntegerOrRangeType(ATarget) then
  begin
    result:=TInteger.Create(Trunc(TFloat(AValue).Value));

    if AValue.Owner=nil then
       AValue.Free;
  end
  else
    result:=AValue;
end;

function Instance_NewVariable(const AInstance:TInstance; const AVariable: TVariable; const AddValue:Boolean):TInstance;
var L : Integer;
    tmp : TData;
    tmpIsFunctionType : Boolean;
begin
  L:=AInstance.AddData(AVariable);

  if AVariable.ValueData=nil then
     { NEVER CALLED:
     if AddValue then
        result:=CheckValue(AInstance,AVariable,L)
     else
     }
        result:=nil
  else
  begin
    tmpIsFunctionType:=TChecker.IsVariableFunctionType(AVariable);

    if tmpIsFunctionType then
       tmp:=AVariable.ValueData
    else
    begin
      tmp:=TEvaluate.AsData(AVariable.ValueData,AInstance);

      tmp:=TryRoundFloat(AVariable,tmp);
    end;

    if TChecker.IsEnumerationItem(tmp) or tmpIsFunctionType then
    begin
      result:=TInstance.From(tmp);
      result.Owner:=AInstance;
    end
    else
      result:=CloneOrReference(AInstance,tmp,AVariable);

    AInstance.Data[L].Value:=result;
  end;
end;

function GetNodeOf(const ARunner:TRunner;
                   const AInstance:TInstance;
                   const AArray:TArrayExpression;
                   out ANodes:TNodes;
                   out AIndex:Integer;
                   out AOwner:TArrayExpression):Boolean; overload;

  function ValidRange(var AResult:Integer; const ADimension:Integer):Boolean;

    procedure ErrorMin(const AMin:Integer);
    var tmp : String;
    begin
      tmp:=TErrorTexts.ReplaceParams(TErrors._ArrayOutOfRangeMin,AResult.ToString,AMin.ToString);

      if ARunner=nil then
         EVidiRunException.RuntimeError(tmp)
      else
         ARunner.RuntimeException(TRunExceptionCode.ArrayIndexLower,tmp)
    end;

    procedure ErrorMax(const AMax:Integer);
    var tmp : String;
    begin
      tmp:=TErrorTexts.ReplaceParams(TErrors._ArrayOutOfRangeMax,AResult.ToString,AMax.ToString);

      if ARunner=nil then
         EVidiRunException.RuntimeError(tmp)
      else
         ARunner.RuntimeException(TRunExceptionCode.ArrayIndexGreater,tmp)
    end;

  var tmpMin,
      tmpMax : Integer;
  begin
    result:=False;

    TEvaluate.ArrayRange(AArray,AInstance,ANodes,ADimension,tmpMin,tmpMax);

    if AResult<tmpMin then
       ErrorMin(tmpMin)
    else
    if AResult>tmpMax then
       ErrorMax(tmpMax)
    else
    begin
      Dec(AResult,tmpMin);
      result:=True;
    end;
  end;

  // Make sure ARange min and max are within valid limits of array bounds
  function CorrectRange(const ARange:TRange; const ADimension:Integer):Boolean;
  var tmp : Integer;
  begin
    tmp:=CalcAsInteger(ARange.Min,AInstance);

    result:=ValidRange(tmp,ADimension);

    if result then
    begin
      tmp:=CalcAsInteger(ARange.Max,AInstance);
      result:=ValidRange(tmp,ADimension);
    end;
  end;

  (* NEVER CALLED:
  function GetFirstParam:Integer;
  var tmpData : TData;
  begin
    tmpData:=TEvaluate.AsData(AArray.Parameters[0] as TData,AInstance);
    result:=CalcAsInteger(tmpData,AInstance);
  end;

  function TryGetCharacter(const AVariable:TVariable; const AValue:TNode):TText;
  var tmpS : String;
      tmp : Integer;
  begin
    result:=nil;

    if AVariable.VariableType is TArrayType then
       if TArrayType(AVariable.VariableType).TheType=TChecker._Types[TChecker._Character] then // Character
       begin
         tmpS:=(AValue as TText).Value;

         tmp:=GetFirstParam;

         if (tmp>0) and (tmp<=Length(tmpS)) then
            result:=TText.Create(tmpS[tmp]);
       end;
  end;
  *)

  function GetArrayExpression(const AVariable:TVariable; tmpInst:TInstance; const ASlot:Integer):Boolean;
  var tmpInst2 : TInstance;

      //tmpChar,
      tmpNode : TNode;
  begin
    tmpInst:=CheckValue(tmpInst,AVariable,ASlot);

    if not (AVariable.VariableType is TArrayType) then
    if TChecker.VariableIsClass(AVariable) then
    begin
      tmpNode:=TClassType(AVariable.VariableType).DefaultIndexed; // C:=Text[123]

      if tmpNode is TVariable then
      begin
        tmpInst2:=TEvaluate.InstanceOf(tmpInst,TVariable(tmpNode));

        (* NEVER CALLED:
        // Special case for Text[x] , Characters array is not an instance yet
        if tmpInst2=nil then
        begin
          tmpChar:=TryGetCharacter(TVariable(tmpNode),tmpInst.Value);

          result:=tmpChar<>nil;

          if result then
             ANodes:=[tmpChar]
          else
             ANodes:=[];

          AIndex:=0;
          AOwner:=nil;

          Exit;
        end
        else
        *)
          tmpInst:=tmpInst2;
      end;
    end;

    {$IFDEF INTERNAL}
    if tmpInst=nil then
       InternalError('Instance of variable is nil',AVariable)
    else
    if not (tmpInst.Value is TArrayExpression) then
    begin
      (* NEVER CALLED:
       AOwner:=TEvaluate.AsData(tmpInst.Value,AInstance) as TArrayExpression;
       *)

       InternalError('Instance Value is not an Array Expression',tmpInst.Value);
    end
    else
    {$ENDIF}
    begin
      (* NEVER CALLED:
      // TODO: Temporary solution to access array from Array.Items:Node[]
      if (AInstance.Owner is TInstance) and (TInstance(AInstance.Owner).Value is TArrayExpression) then
         AOwner:=TArrayExpression(TInstance(AInstance.Owner).Value)
      else
      *)
         AOwner:=TArrayExpression(tmpInst.Value);
    end;

    result:=True;
  end;

  function GetArrayExpressionOfSelf:TArrayExpression;
  var tmp : TInstance;
  begin
    result:=nil;

    tmp:=AInstance;

    while tmp<>nil do
    begin
      if tmp.Value is TArrayExpression then
         Exit(TArrayExpression(tmp.Value))

      else
         tmp:=tmp.Owner as TInstance;
    end;
  end;

var t: Integer;
    tmpData : TData;
    tmpVariable : TVariable;

    tmpInst : TInstance;

    tmpDimensions,
    tmp : Integer;
begin
  if AArray.Data is TSelf then
     AOwner:=GetArrayExpressionOfSelf
  else
  if AArray.Data is TVariable then
  begin
    tmpVariable:=TVariable(AArray.Data);
    tmp:=DataSlotOf(AInstance,tmpVariable,tmpInst);

    {$IFDEF INTERNAL}
    if tmp=-1 then
    begin
      result:=False;

      InternalError('DataSlotOf cannot find variable',tmpVariable);
      Exit;
    end;
    {$ENDIF}

    result:=GetArrayExpression(tmpVariable,tmpInst,tmp);

    if not result then
       Exit;
  end;

  {$IFDEF INTERNAL}
  if AOwner=nil then
     InternalError('Array Owner is nil',AArray.Data);
  {$ENDIf}

  ANodes:=AOwner.Parameters;

  result:=True; // avoid hint

  AIndex:=0;

  tmpDimensions:=High(AArray.Parameters);

  for t:=0 to tmpDimensions do
  begin
    tmpData:=TEvaluate.AsData(AArray.Parameters[t] as TData,AInstance);

    if tmpData is TRange then // X[2..3] sub-range
    begin
      {$IFDEF INTERNAL}
      if not CorrectRange(TRange(tmpData),t) then
      begin
        // Should this be an error? Or simply return False ?
        InternalError('Range is not correct',tmpData);
        Exit(False);
      end
      else
      {$ENDIF}
         break;
    end
    else
    if TChecker.IsEnumerationItem(tmpData) then
       AIndex:=EnumerationIndexOf(TVariable(tmpData))  // X[Colors.Blue]
    else
       AIndex:=CalcAsInteger(tmpData,AInstance);  // X[123]

    if ValidRange(AIndex,t) then
    begin
      if t<tmpDimensions then
      begin
        AOwner:=TArrayExpression(ANodes[AIndex]);
        ANodes:=AOwner.Parameters;
      end;
    end
    else
      Exit(False);

    // ? break ??
  end;
end;

// TODO: Range assign  :    X[2..3] := ['a','b']
procedure AssignArray(const AInstance:TInstance; const AArray:TArrayExpression; const AValue:TNode);

  // TODO: REMOVE (SLOW !!!)
  // Workaround !! See "Bugs\Array Swap" bug (AV)
  // Returns true when the Item is found more than once in Items
  function Repeated(const AItem:TNode; const AItems:TNodes):Boolean;
  var t, C : Integer;
  begin
    C:=0;

    for t:=Low(AItems) to High(AItems) do
        if AItems[t]=AItem then
        begin
          Inc(C);

          if C>1 then
             break;
        end;

    result:=C>1;
  end;

var tmpItems : TNodes;
    tmpIndex : Integer;
    tmp : TNode;
    tmpOwner : TArrayExpression;
begin
  if GetNodeOf(nil,AInstance,AArray,tmpItems,tmpIndex,tmpOwner) then
  begin
    {$IFDEF INTERNAL}
    if AValue.Owner=tmpOwner then
    begin
      //InternalError('Array assign value is already in the array',AValue);
      //Exit;
    end;
    {$ENDIF}

    tmp:=tmpItems[tmpIndex];

    if tmp<>nil then
       if tmp.Owner=tmpOwner then
       begin
         if not Repeated(tmp,tmpItems) then
            tmp.Free;
       end;

    if AValue.Owner=nil then
       AValue.Owner:=tmpOwner;

    tmpItems[tmpIndex]:=AValue;
  end;
end;

function SharedInstanceOf(const AInstance:TInstance; const AData:TData):TInstance;
var tmpType : TType;
begin
  tmpType:=TChecker.GetClassTypeOf(AData);

  {$IFDEF INTERNAL}
  if tmpType=nil then
  begin
    result:=nil;
    InternalError('Cannot get class type of ',AData);
  end
  else
  {$ENDIF}

  result:=TInstance.TypeInstanceOf(AInstance,tmpType);
end;

procedure Instance_Assign(const AInstance:TInstance; const AData:TData; const AValue:TInstance);

  function GetLeft(const AData:TData):TInstance;
  begin
    (* NEVER CALLED:
    if AData is TInstance then
       result:=TInstance(AData)
    else
    *)
    if AData is TTypeCall then
       result:=TInstance.TypeInstanceOf(AInstance,TTypeCall(AData).TheType)
    else
       result:=TEvaluate.InstanceOf(AInstance,AData);
  end;

var tmp : Integer;
    tmpInst : TInstance;
    tmpInst2 : TInstance;
begin
  if AData is TVariable then
  begin
    if TVariable(AData).Clauses.Shared then
       tmpInst2:=SharedInstanceOf(AInstance,AData)
    else
       tmpInst2:=AInstance;

    tmp:=DataSlotOf(tmpInst2,TVariable(AData),tmpInst);

    if AInstance<>tmpInst then
    begin
      if AValue.Owner=AInstance then
         AValue.Owner:=tmpInst;
    end;

    // Check here for integer underflow / overflow of AValue ??

    tmpInst.Data[tmp].ReplaceValue(AValue);
  end
  else
  if AData is TMember then
  begin
    tmpInst:=GetLeft(TMember(AData).Data);

    {$IFDEF INTERNAL}
    if tmpInst=nil then
       InternalError('Variable not found',TMember(AData).Data)
    else
    {$ENDIF}
    begin
      if AValue.Owner=AInstance then
         AValue.Owner:=tmpInst;

      Instance_Assign(tmpInst,TMember(AData).Member,AValue);
    end;
  end
  else
  (* NEVER CALLED:
  if AData is TTypeCall then  // Shared
  begin
    tmpInst:=TInstance.TypeInstanceOf(AInstance,TTypeCall(AData).TheType);
    Instance_Assign(tmpInst,AData,AValue);
  end
  else
  *)
  if AData is TDataCall then
     TRunner.CallSetter(AInstance,AValue,TDataCall(AData))
  else
  if AData is TArrayExpression then
  begin
    if AValue.Value.Owner is TInstance then
       AValue.Value.Owner:=nil;

    AssignArray(AInstance,TArrayExpression(AData),AValue.Value);

    AValue.Free;
  end
  {$IFDEF INTERNAL}
  else
    InternalError(TErrors._CannotAssign,AData); // <-- pending: Format error string %s %s
  {$ENDIF}
end;

procedure Construct(const AInstance:TInstance; const AType:TType);

  procedure AddSpecializedParameters(const AClass:TClassType);
  var V : TNode;
      t : Integer;
      tmpData : TNode;
  begin
     for t:=0 to High(AClass.Parameters) do
     begin
       V:=AClass.Parameters[t];

       if V is TVariable then
       begin
         Instance_NewVariable(AInstance,TVariable(V),False);

         tmpData:=TSpecializedType(AType).Generics[t];

         if tmpData is TData then
            AInstance.Data[High(AInstance.Data)].Value:=FromValue(AInstance,
                        TChecker.GetDataType(nil,TData(tmpData)),TData(tmpData));
       end;
     end;
  end;

var tmp : TType;
begin
  if AType is TSpecializedType then
  begin
    tmp:=TSpecializedType(AType).TheType;

    if tmp is TClassType then
       AddSpecializedParameters(TClassType(tmp));
  end
  else
    tmp:=AType;

  if (tmp is TClassType) and (TClassType(tmp).Ancestor<>nil) then
     Construct(AInstance,TClassType(tmp).Ancestor);

  // TODO: Replace TRunner.Execute with a TRunningConstruct or similar, to enable debugging construction

  TRunner.Execute(AInstance,tmp);
end;

class function TEvaluate.InstanceOf(const AInstance:TInstance; const AVariable:TVariable):TInstance;

  function FindIn(const AInstance:TInstance):TInstance;
  var tmp : Integer;
  begin
    tmp:=AInstance.Data.Find(AVariable);

    if tmp=-1 then
       result:=nil
    else
       result:=CheckValue(AInstance,AVariable,tmp);
  end;

begin
  if AVariable.Clauses.Shared then
  begin
    result:=SharedInstanceOf(AInstance,AVariable);
    result:=FindIn(result);
  end
  else
  begin
    {$IFDEF INTERNAL}
    if AInstance=nil then
    begin
      result:=nil;
      InternalError('Instance of variable is nil at InstanceOf',AVariable);
    end
    else
    {$ENDIF}
    if AInstance.Value is TInstance then
       result:=FindIn(TInstance(AInstance.Value))
    else
       result:=nil;

    if result=nil then
       result:=FindIn(AInstance);
  end;

  if result=nil then
     if AInstance.Owner is TInstance then
        result:=InstanceOf(TInstance(AInstance.Owner),AVariable);
end;

class function TEvaluate.InstanceOf(const AInstance:TInstance; const AData:TData):TInstance;
begin
  {$IFDEF INTERNAL}
  if not (AData is TVariable) then
  begin
    result:=nil; // ?? error !!

    InternalError('Not found, instance of',AData);
  end
  else
  {$ENDIF}
    // do not check is TMember !
    result:=InstanceOf(AInstance,TVariable(AData))
end;

function ValueOf(const AInstance:TInstance; const AVariable: TVariable): TData;
var tmp : TInstance;
begin
  tmp:=TEvaluate.InstanceOf(AInstance,AVariable);

  if tmp=nil then
  begin
    if AInstance.Owner is TInstance then
       result:=ValueOf(TInstance(AInstance.Owner),AVariable)
    else
       result:=nil;

    {
    if result=nil then
       VariableNotFound(AVariable);
    }
  end
  else
  begin
    result:=tmp.Value;

    if result=nil then
       if TChecker.VariableIsClass(AVariable) then
          result:=tmp;

    {$IFDEF INTERNAL}
    if result=nil then
       // Class instances are empty (they have Data[] but no Value)
       //if not TChecker.VariableIsClass(AVariable) then
          InternalError('Variable Value is empty',AVariable);
    {$ENDIF}
  end;
end;

function DirectValueOf(const AInstance:TInstance; const AVariable: TVariable): TData;
begin
  result:=ValueOf(AInstance,AVariable);

  if result=nil then
     if AVariable.Clauses.Final or AVariable.Clauses.Shared then
        if AVariable.ValueData=nil then
        begin
          if TChecker.VariableIsEnumItem(AVariable) then
             result:=AVariable; // returning a simple enum item
        end
        else
           result:=AVariable.ValueData; // <-- last resort, for direct: "final Foo::=123"
  {
  else
     result:=ValueOf(AInstance,AVariable);
  }
end;

function CreateArrayExpression(const ACount:Integer):TArrayExpression;
begin
  result:=TArrayExpression.Create;
  SetLength(result.Parameters,ACount);
end;

function CloneArray(const AInstance:TInstance; const ANodes:TNodes; const ARange:TRange):TArrayExpression;
var t,
    tmpMin,
    tmpMax : Integer;
begin
  GetRangeBounds(ARange,AInstance,tmpMin,tmpMax);

  result:=CreateArrayExpression(tmpMax-tmpMin+1);

  for t:=tmpMin to tmpMax do
      result.Parameters[t-tmpMin]:=ANodes[t];
end;

function GetNodeOf(const ARunner:TRunner;
                   const AInstance:TInstance;
                   const AArray:TArrayExpression;
                   out AOwner:TArrayExpression):TNode; overload;

  function GetRange:TRange;
  var tmp : TNode;
  begin
    if AArray.Parameters.Count=1 then
    begin
      tmp:=AArray.Parameters[0];

      if tmp is TRange then
         result:=TRange(tmp)
      else
        result:=nil;
    end
    else
       result:=nil;
  end;

var tmpItems : TNodes;
    tmpIndex : Integer;
    tmpRange : TRange;
begin
  if AArray.Data=nil then
  begin
    AOwner:=nil; // <-- ??
    result:=AArray;
  end
  else
  begin
    tmpRange:=GetRange;

    if GetNodeOf(ARunner,AInstance,AArray,tmpItems,tmpIndex,AOwner) then
       if tmpRange=nil then
          result:=tmpItems[tmpIndex]
       else
          result:=CloneArray(AInstance,tmpItems,tmpRange)
    else
       result:=nil // Range Array Index error
  end;
end;

function ArithmeticAssign(const ANode: TAssignment):TData;
begin
  case ANode.Arithmetic of
         Add: result:=TAddition.Create;
    Subtract: result:=TSubtraction.Create;
    Multiply: result:=TMultiplication.Create;
      Divide: result:=TDivision.Create;
  else
       {None:} result:=ANode.Value;
  end;

  if ANode.Arithmetic<>TArithmetic_Assign.None then
  begin
    TOperand(result).Left:=ANode.Variable;
    TOperand(result).Right:=ANode.Value;
  end;
end;

procedure DoAssignment(const AInstance:TInstance; const ANode: TAssignment);
var tmp : TInstance;
    tmpData : TData;
    tmpValue : TData;
    tmpVariable : TVariable;
    tmpIsFunctionType : Boolean;
begin
  tmpValue:=ArithmeticAssign(ANode);

  tmpIsFunctionType:=TChecker.IsDataFunctionType(tmpValue);

  if tmpIsFunctionType then
     tmpData:=TEvaluate.InstanceOf(AInstance,TVariable(tmpValue))
  else
     tmpData:=TryRoundFloat(ANode.Variable,TEvaluate.AsData(tmpValue,AInstance));

  if tmpValue<>ANode.Value then
     tmpValue.Free;

  if tmpData is TInstance then
     tmp:=TInstance(tmpData)
  else
  begin
    if ANode.Variable is TVariable then
       tmpVariable:=TVariable(ANode.Variable)
    else
       tmpVariable:=nil;

    if TChecker.IsEnumerationItem(tmpData) or tmpIsFunctionType then
    begin
      tmp:=TInstance.From(tmpData);
      tmp.Owner:=AInstance;
    end
    else
      tmp:=CloneOrReference(AInstance,tmpData,tmpVariable);
  end;

  Instance_Assign(AInstance,ANode.Variable,tmp);

  {$IFDEF INTERNAL}
  if tmpData.Owner=nil then
     InternalError('DoAssignment, Owner of data is nil',tmpData);
  {$ENDIF}
end;

function IsNegative(const AData:TData):Boolean;
begin
  result:=(AData is TUnarySign) and (not TUnarySign(AData).Positive);
end;

function FloatModulus(const A,B:Float):Float;
var tmpB : Float;
begin
  if A<0 then
     result:=-A
  else
     result:=A;

  if B<0 then
     tmpB:=-B
  else
     tmpB:=B;

  while result>=tmpB do
        result:=result-tmpB;

  if A<0 then
     result:=-result;
end;

function CountOfRange(const ARange:TRange; const AInstance:TInstance):Integer;
var Min, Max : Integer;
begin
  Min:=CalcAsInteger(ARange.Min,AInstance);
  Max:=CalcAsInteger(ARange.Max,AInstance);

  result:=Max-Min+1;
end;

{ TEvaluate }

class function TEvaluate.AsBoolean(AData:TData; const AInstance:TInstance=nil):Boolean;
begin
  AData:=AsData(AData,AInstance);

  {$IFDEF INTERNAL}
  if not (AData is TBoolean) then
  begin
    result:=False;
    InternalError('Cannot evaluate as Boolean',AData);
  end
  else
  {$ENDIF}
     result:=TBoolean(AData).Value;
end;

class function TEvaluate.DefaultArray(const AOwner:TNode; const AArray:TArrayType; const AInstance:TInstance): TArrayExpression;
var t,tmp : Integer;
begin
  if AArray.Size=nil then
     tmp:=0
  else
  if AArray.Size is TRange then
     tmp:=CountOfRange(TRange(AArray.Size),AInstance)
  else
     tmp:=CalcAsInteger(AArray.Size,AInstance);

  result:=CreateArrayExpression(tmp);
  result.Owner:=AOwner;

  for t:=0 to tmp-1 do
      result.Parameters[t]:=DefaultValueOf(result,AArray.TheType,AInstance);
end;

class function TEvaluate.DefaultValueOf(const AOwner:TNode; const AType:TType; const AInstance:TInstance): TData;
begin
  if AType=TChecker._Types[TChecker._Integer] then
     result:=TInteger.Create(0)
  else
  if AType=TChecker._Types[TChecker._Float] then
     result:=TFloat.Create(0)
  else
  if AType=TChecker._Types[TChecker._Boolean] then
     result:=TBoolean.Create(False)
  else
  if AType=TChecker._Types[TChecker._Text] then
     result:=TText.Create('')
  else
  if AType is TArrayType then
     result:=DefaultArray(AOwner,TArrayType(AType),AInstance)
  else
  begin
    result:=nil;

    if AType is TClassType then
       if TClassType(AType).Ancestor<>nil then
          result:=DefaultValueOf(AOwner,TClassType(AType).Ancestor,AInstance);

      {
    tmpType:=GetAncestorArrayType(AType);

    if tmpType=nil then
       result:=nil
    else
       result:=DefaultArray(AOwner,tmpType,AInstance)
       }
  end;

  if result<>nil then
     if result.Owner=nil then
        result.Owner:=AOwner;
end;

function EqualNode(const L,R:TNode):Boolean;
begin
  if (L is TInteger) and (R is TInteger) then
     result:=TInteger(L).Value=TInteger(R).Value
  else
  if (L is TBoolean) and (R is TBoolean) then
     result:=TBoolean(L).Value=TBoolean(R).Value
  else
  if (L is TText) and (R is TText) then
     result:=TText(L).Value=TText(R).Value
  else
  if (L is TFloat) and (R is TFloat) then
     result:=TFloat(L).Value=TFloat(R).Value // Epsilon ?
  else
     result:=L=R; // <-- TODO: Compare Integer, Text, Boolean, Float
end;

function EqualNodes(const L,R:TNodes):Boolean;
var t : Integer;
begin
  result:=Length(L)=Length(R);

  if result then
     for t:=Low(L) to High(L) do
         if not EqualNode(L[t],R[t]) then
            Exit(False);
end;

function CompareArrays(const L,R:TArrayExpression; const Equal:Boolean):TBoolean;
var tmp : Boolean;
begin
  tmp:=L.Data=R.Data;

  if tmp then
     tmp:=EqualNodes(L.Parameters,R.Parameters);

  if not Equal then
     tmp:=not tmp;

  result:=TBoolean.Create(tmp);
end;

function CloneArrayExpression(const AInstance:TInstance; const AArray:TArrayExpression):TArrayExpression;
begin
  result:=TCloner.Node(AInstance.Value,TEvaluate.AsData(AArray,AInstance)) as TArrayExpression;
end;

function ConcatenateArrays(const L,R:TArrayExpression; const AInstance:TInstance):TArrayExpression; overload;
var t,
    tmpL,
    tmpR : Integer;
begin
  result:=CloneArrayExpression(AInstance,L);

  tmpL:=Length(result.Parameters);
  tmpR:=Length(R.Parameters);
  SetLength(result.Parameters,tmpL+tmpR);

  for t:=0 to tmpR-1 do
      result.Parameters[tmpL+t]:=TCloner.Node(result,TEvaluate.AsData(R.Parameters[t] as TData,AInstance));
end;

function ConcatenateArrays(const L:TArrayExpression;
                           const R:TData;
                           const AInstance:TInstance):TArrayExpression; overload;
begin
  result:=CloneArrayExpression(AInstance,L);
  result.Parameters.Add(TCloner.Node(result,TEvaluate.AsData(R,AInstance)));
end;

function FromBoolean(const AOperand:TOperand; const LB,RB:Boolean): TBoolean;
var tmp : Boolean;
begin
  if AOperand is TLogicalAnd then
     tmp:=LB and RB
  else
  if AOperand is TLogicalOr then
     tmp:=LB or RB
  else
  if AOperand is TLogicalXor then
     tmp:=LB xor RB
  else
  if AOperand is TIsEqual then
     tmp:=LB=RB
  else
  if AOperand is TIsNotEqual then
     tmp:=LB<>RB
  else
  begin
    result:=nil;

    {$IFDEF INTERNAL}
    InternalError('Internal. Boolean operand not supported',AOperand);
    {$ENDIF}

    Exit;
  end;

  result:=TBoolean.Create(tmp);
end;

function FromText(const AOperand:TOperand; const LT,RT:String): TData;
begin
  if AOperand is TAddition then
     result:=TText.Create(LT+RT)
  else
  if AOperand is TIsEqual then
     result:=TBoolean.Create(LT=RT)
  else
  if AOperand is TIsNotEqual then
     result:=TBoolean.Create(LT<>RT)
  else
  if AOperand is TContains then
     result:=TBoolean.Create(Pos(LT,RT)>0)
  else
  if AOperand is TIsLower then
     result:=TBoolean.Create(LT<RT)
  else
  if AOperand is TIsLowerOrEqual then
     result:=TBoolean.Create(LT<=RT)
  else
  if AOperand is TIsGreater then
     result:=TBoolean.Create(LT>RT)
  else
  if AOperand is TIsGreaterOrEqual then
     result:=TBoolean.Create(LT>=RT)
  else
  begin
    {$IFDEF INTERNAL}
    InternalError('Text arithmetic operand not supported',AOperand);
    {$ENDIF}

    result:=nil;
  end;
end;

function FromFloat(const AOperand:TOperand; const LF,RF:Float): TData;
begin
  if AOperand is TIsEqual then
     result:=TBoolean.Create(LF=RF)
  else
  if AOperand is TIsNotEqual then
     result:=TBoolean.Create(LF<>RF)
  else
  if AOperand is TIsLower then
     result:=TBoolean.Create(LF<RF)
  else
  if AOperand is TIsLowerOrEqual then
     result:=TBoolean.Create(LF<=RF)
  else
  if AOperand is TIsGreater then
     result:=TBoolean.Create(LF>RF)
  else
  if AOperand is TIsGreaterOrEqual then
     result:=TBoolean.Create(LF>=RF)
  else
  if AOperand is TAddition then
     result:=TFloat.Create(LF+RF)
  else
  if AOperand is TSubtraction then
     result:=TFloat.Create(LF-RF)
  else
  if AOperand is TMultiplication then
     result:=TFloat.Create(LF*RF)
  else
  if AOperand is TDivision then
  begin
    if RF=0 then
    begin
      result:=nil;
      EVidiRunException.RuntimeError(TErrors._DivisionByZero);
    end
    else
      result:=TFloat.Create(LF/RF) // Can raise: Division by zero. See callers.
  end
  else

  // TODO: Bit float
  // is TLogicalAnd
  // is TLogicalOr
  // is TLogicalXor

  begin
    result:=nil;

    {$IFDEF INTERNAL}
    InternalError('Float arithmetic operand not supported',AOperand);
    {$ENDIF}
  end;
end;

// TODO: Check for overflow / underflow exceptions
function FromInteger(const AOperand:TOperand; const LI,RI:Int64): TData;

  function DoMultiplication:TData;
  begin
    try
      result:=TInteger.Create(LI*RI);
    except
      on EOverflow do
      begin
        result:=nil;
        EVidiRunException.RuntimeError(TErrors._Overflow); // replace with: _ArithmeticOverflow
      end;
    end;
  end;

  function DoDivision:TData;
  var tmp : Integer;
  begin
    if RI=0 then
    begin
      result:=nil;
      EVidiRunException.RuntimeError(TErrors._DivisionByZero);
    end
    else
    begin
      tmp:=LI div RI;  // Can raise: Division by zero. See callers.

      if tmp=(LI/RI) then
         result:=TInteger.Create(tmp)
      else
         result:=TFloat.Create(LI/RI);
    end;
  end;

begin
  if AOperand is TAddition then
     result:=TInteger.Create(LI+RI)
  else
  if AOperand is TSubtraction then
     result:=TInteger.Create(LI-RI)
  else
  if AOperand is TMultiplication then
     result:=DoMultiplication
  else
  if AOperand is TDivision then
     result:=DoDivision
  else
  if AOperand is TLogicalAnd then
     result:=TInteger.Create(LI and RI)
  else
  if AOperand is TLogicalOr then
     result:=TInteger.Create(LI or RI)
  else
  if AOperand is TLogicalXor then
     result:=TInteger.Create(LI xor RI)
  else
  if AOperand is TIsEqual then
     result:=TBoolean.Create(LI=RI)
  else
  if AOperand is TIsNotEqual then
     result:=TBoolean.Create(LI<>RI)
  else
  if AOperand is TIsLower then
     result:=TBoolean.Create(LI<RI)
  else
  if AOperand is TIsLowerOrEqual then
     result:=TBoolean.Create(LI<=RI)
  else
  if AOperand is TIsGreater then
     result:=TBoolean.Create(LI>RI)
  else
  if AOperand is TIsGreaterOrEqual then
     result:=TBoolean.Create(LI>=RI)
  else
  begin
    result:=nil;

    {$IFDEF INTERNAL}
    InternalError('Integer arithmetic operand not supported',AOperand);
    {$ENDIF}
  end;
end;

function EnumerationBoolean(const AOperand:TOperand; const L,R:TData):TData;
begin
  if AOperand is TIsEqual then
     result:=TBoolean.Create(L=R)
  else
  if AOperand is TIsNotEqual then
     result:=TBoolean.Create(L<>R)
  else
  begin
    result:=nil;

    {$IFDEF INTERNAL}
    InternalError('Operand not supported for enumerations',AOperand);
    {$ENDIF}
  end;
end;

function ArrayContains(const AInstance:TInstance; const AValue:TData; const AArray:TArrayExpression):Boolean;
var P : TNode;
    tmp : TData;
    tmpResult : Boolean;
begin
  for P in AArray.Parameters do
  begin
    tmp:=TEvaluate.AsData(P as TData,AInstance);

    tmpResult:=EqualNode(AValue,tmp);

    if tmp.Owner=nil then
       tmp.Free;  // <-- NEVER CALLED

    if tmpResult then
       Exit(True);
  end;

  result:=False;
end;

// Calculate and FREE AData !!
function CalcAsBoolean(AData:TData; const AInstance:TInstance):Boolean;
begin
  AData:=TEvaluate.AsData(AData,AInstance);

  {$IFDEF INTERNAL}
  if not (AData is TBoolean) then
  begin
    result:=False;
    InternalError('Cannot evaluate as Boolean',AData);
  end
  else
  {$ENDIF}
  begin
    result:=TBoolean(AData).Value;

    if AData.Owner=nil then
       AData.Free;
  end;
end;

function ArrayOperand(const AOperand:TOperand; const AInstance:TInstance; const L,R:TArrayExpression):TData;
begin
  if AOperand is TIsEqual then
     result:=CompareArrays(L,R,True)
  else
  if AOperand is TIsNotEqual then
     result:=CompareArrays(L,R,False)
  else
  if AOperand is TAddition then
     result:=ConcatenateArrays(L,R,AInstance)
  else
  begin
    result:=nil;

    {$IFDEF INTERNAL}
    InternalError('Array operand not supported',AOperand);
    {$ENDIF}
  end;
end;

class function TEvaluate.OperandAsData(const AOperand:TOperand; const AInstance: TInstance): TData;
var L,R : TData;

  function Calculate:TData;
  begin
    if (L is TArrayExpression) and (R is TArrayExpression) then
       result:=ArrayOperand(AOperand,AInstance,TArrayExpression(L),TArrayExpression(R))
    else
    if (R is TArrayExpression) and (AOperand is TContains) then
       result:=TBoolean.Create(ArrayContains(AInstance,L,TArrayExpression(R)))
    else
    if (L is TArrayExpression) and (AOperand is TAddition) then
       result:=ConcatenateArrays(TArrayExpression(L),R,AInstance)
    else
    if (L.Owner=R.Owner) and // Enumerations
       TChecker.IsEnumerationItem(L) and
       TChecker.IsEnumerationItem(R) then
    begin
      result:=EnumerationBoolean(AOperand,L,R)
    end
    else
    if TChecker.IsLogical(L) then // and TChecker.IsLogical(R) <-- implicit
       result:=FromBoolean(AOperand,
                           TEvaluate.AsBoolean(L,AInstance),
                           TEvaluate.AsBoolean(R,AInstance))
    else
    if TChecker.IsInteger(L) and TChecker.IsInteger(R) then
       result:=FromInteger(AOperand,
                           TEvaluate.AsInteger(L,AInstance),
                           TEvaluate.AsInteger(R,AInstance))
    else
    if TChecker.IsText(L) then
       result:=FromText(AOperand,
                        TEvaluate.AsText(L,AInstance),
                        TEvaluate.AsText(R,AInstance))
    else
    if TChecker.IsFloat(L) or TChecker.IsFloat(R) then // Promote to Float
       result:=FromFloat(AOperand,
                         TEvaluate.AsFloat(L,AInstance),
                         TEvaluate.AsFloat(R,AInstance))
    else
    begin
      result:=nil;

      {$IFDEF INTERNAL}
      InternalError('Internal, Operand types not supported',AOperand);
      {$ENDIF}
    end;
  end;

begin
  if AOperand is TCondition then
     if CalcAsBoolean(TCondition(AOperand).Condition,AInstance) then
        result:=AsData(AOperand.Left,AInstance)
     else
        result:=AsData(AOperand.Right,AInstance)
  else
  begin
    L:=AsData(AOperand.Left,AInstance);
    R:=AsData(AOperand.Right,AInstance);

    try
      result:=Calculate;
    finally
      if L.Owner=nil then
         L.Free;

      if R.Owner=nil then
         R.Free;
    end;
  end;
end;

class function TEvaluate.AsDataDirect(const AData: TData; const AInstance: TInstance): TData;

  function AsVariable(const AVariable:TVariable):TData;
  var tmp : TInstance;
  begin
    if AVariable.Clauses.Final and (AVariable.ValueData<>nil) then
       result:=AsDataDirect(AVariable.ValueData,AInstance)
    else
    if TChecker.IsEnumerationItem(AVariable) then
       result:=AVariable
    else
    // Type-level variables or constants
    if AVariable.Clauses.Shared and
       (AVariable.ValueData=nil) then
    begin
      tmp:=SharedInstanceOf(AInstance,AData);
      result:=ValueOf(tmp,AVariable);
    end
    else
      result:=nil; // cannot evaluate variable without an instance, no problem.
  end;

  function GetSelfInstanceOf(const AData:TData):TInstance;
  var tmp : TInstance;
  begin
    tmp:=AInstance;

    while tmp<>nil do
    begin
      if tmp.Value<>nil then // ???
         Exit(tmp)
      else
      if tmp.Owner is TInstance then
         tmp:=TInstance(tmp.Owner)
      else
         break;
    end;

    result:=nil;
  end;

begin
  if (AData is TText) or (AData is TBoolean) or (AData is TNumber) or (AData is TRange) then
     result:=AData
  else
  if AData is TVariable then
     result:=AsVariable(TVariable(AData))
  else
  if AData is TTypeCall then // Type-calls do nothing
     result:=AData
  else
  if AData is TSelf then
     result:=GetSelfInstanceOf(AData)
  else
  if AData is TCastingData then
     result:=TCastingData(AData).Data    // <-- NEVER CALLED
  else
  if (AData is TDataCall) and TDataCall(AData).Routine.Clauses.Shared then
     TRunner.Call(AInstance,TDataCall(AData),nil,result) // Foo(Bar)
  else
     result:=nil;
end;

class function TEvaluate.AsData(AData: TData; const AInstance: TInstance): TData;

  function UnaryData(const AUnary:TUnarySign):TData;
  var tmp : TData;
      tmpInt : Int64;
      tmpFloat : Float;
  begin
    tmp:=AsData(AUnary.Expression,AInstance);

    if TChecker.IsInteger(tmp) then
    begin
      tmpInt:=AsInteger(tmp,AInstance);

      if not TUnarySign(AData).Positive then
         tmpInt:=-tmpInt;

      result:=TInteger.Create(tmpInt);
    end
    else
    if TChecker.IsFloat(tmp) then
    begin
      tmpFloat:=AsFloat(tmp,AInstance);

      if not TUnarySign(AData).Positive then
         tmpFloat:=-tmpFloat;

      result:=TFloat.Create(tmpFloat);
    end
    else
    begin
      result:=nil;

      {$IFDEF INTERNAL}
      InternalError('Not a number',AData);
      {$ENDIF}
    end;

    if tmp.Owner=nil then
       tmp.Free;
  end;

  function AsArrayExpression:TData;
  var tmpNode : TNode;
      tmpOwner : TArrayExpression;
  begin
    tmpNode:=GetNodeOf(nil,AInstance,TArrayExpression(AData),tmpOwner);

    {$IFDEF INTERNAL}
    if not (tmpNode is TData) then
    begin
      InternalError('Cannot evaluate',tmpNode);
      result:=nil;
    end
    else
    {$ENDIF}
      result:=TData(tmpNode);
  end;

  function AsMember(const AMember:TMember):TData;
  var tmpData : TData;
      tmp  : TInstance;
  begin
    if AMember.Data is TSelf then
       result:=AsData(AMember.Member,AInstance)
    else
    begin
      if AMember.Data is TVariable then
         tmpData:=AMember.Data
      else
         tmpData:=AsData(AMember.Data,AInstance);

      if tmpData is TVariable then
      begin
        tmp:=InstanceOf(AInstance,TVariable(tmpData));
        result:=AsData(AMember.Member,tmp);
      end
      else
      begin
        tmp:=TInstance.From(tmpData);
        try
          tmp.Owner:=AInstance;
          result:=AsData(AMember.Member,tmp);
        finally
          tmp.Free;
        end;
      end;

      if tmpData.Owner=nil then
         tmpData.Free;
    end;
  end;

  function AsUnaryNot(const ANot:TUnaryNot):TData;
  var tmpData : TData;
  begin
    tmpData:=AsData(ANot.Expression,AInstance);

    if TChecker.IsLogical(tmpData) then
       result:=TBoolean.Create(not AsBoolean(tmpData))
    else
    if TChecker.IsInteger(tmpData) then
       result:=TInteger.Create(not AsInteger(tmpData))
    else
    begin
      result:=nil;

      {$IFDEF INTERNAL}
      InternalError('Internal. Cannot apply "not" for data',tmpData);
      {$ENDIF}
    end;
  end;

  function Find_InstanceOf(const AVariable:TVariable):TData;
  var tmpData : TData;
  begin
    {$IFDEF INTERNAL}
    if AInstance=nil then
    begin
      result:=nil;

      InternalError('Instance for variable is nil',AVariable);
    end
    else
    {$ENDIF}
    begin
      tmpData:=ValueOf(AInstance,AVariable);

      if tmpData is TInstance then
         result:=tmpData
      else
      if tmpData is TTypeCall then
         result:=tmpData
      else
         result:=AsData(tmpData,AInstance);
    end;
  end;

  function AsVariable(const AVariable:TVariable):TData;
  begin
    // Type-level variables or constants
    if AVariable.Clauses.Shared then

       if TChecker.IsEnumerationItem(AData) then
          result:=AData
       else
       begin
         result:=TEvaluate.InstanceOf(AInstance,AVariable);

         if result=nil then
            result:=AsData(AVariable.ValueData,AInstance);
       end
    else
    begin
      if AVariable.Clauses.Final and (AVariable.ValueData<>nil) then
         result:=AsDataDirect(AVariable.ValueData,AInstance)
      else
         result:=nil;

      if result=nil then
         result:=Find_InstanceOf(AVariable)
    end;
  end;

begin
  result:=AsDataDirect(AData,AInstance);

  if result=nil then
  if AData is TVariable then
     result:=AsVariable(TVariable(AData))
  else
  if AData is TMember then
     result:=AsMember(TMember(AData))
  else
  if AData is TArrayExpression then
     result:=AsArrayExpression
  else
  if AData is TOperand then
     result:=OperandAsData(TOperand(AData),AInstance)
  else
  if AData is TUnaryNot then
     result:=AsUnaryNot(TUnaryNot(AData))
  else
  if AData is TUnarySign then
     result:=UnaryData(TUnarySign(AData))
  else
  if AData is TGroup then
     result:=AsData(TGroup(AData).Expression,AInstance)
  else
  if AData is TDataCall then
     TRunner.Call(AInstance,TDataCall(AData),nil,result) // Foo(Bar)
  else
  if AData is TAncestor then
     TRunner.Call(AInstance,TAncestor(AData).DataCall,nil,result)
  else
  begin
    result:=nil;

    {$IFDEF INTERNAL}
    InternalError('Cannot evaluate as Data',AData);
    {$ENDIF}
  end;
end;

class function TEvaluate.AsFloat(AData:TData; const AInstance:TInstance=nil):Float;
begin
  AData:=AsData(AData,AInstance);

  if AData is TFloat then
     result:=TFloat(AData).Value
  else
  if AData is TInteger then
     result:=TInteger(AData).Value
  else
  begin
    result:=0;

    {$IFDEF INTERNAL}
    InternalError('Cannot evaluate as Float',AData);
    {$ENDIF}
  end;
end;

class function TEvaluate.AsInteger(AData:TData; const AInstance:TInstance=nil):Int64;
begin
  AData:=AsData(AData,AInstance);

  {$IFDEF INTERNAL}
  if not (AData is TInteger) then
  begin
    result:=0;
    InternalError('Cannot evaluate as Integer',AData);
  end
  else
  {$ENDIF}
    result:=TInteger(AData).Value;
end;

function Runner_DataAsText(const AData:TData):String;

  {
  function RangeToStr(const ARange:TRange):String;
  begin
    result:=Runner_DataAsText(ARange.Min)+TSyntax._RangeDelimiter+
            Runner_DataAsText(ARange.Max);
  end;
  }

  function ArrayAsText(const AData:TArrayExpression):String;
  const ItemSeparator=' ';

  var tmp : String;
      N : TNode;
      t : Integer;
      tmpMax : Integer;
  begin
    result:='';

    tmpMax:=High(AData.Parameters);

    for t:=0 to tmpMax do
    begin
      N:=AData.Parameters[t];

      if N is TArrayExpression then
         tmp:=ArrayAsText(TArrayExpression(N))
      else
      if N is TData then
         tmp:=Runner_DataAsText(TData(N))
      else
      begin
        tmp:='?';

        {$IFDEF INTERNAL}
        InternalError('Cannot convert data as text',N);
        {$ENDIF}
      end;

      {if t=0 then
         result:=tmp
      else}
         result:=result+{ItemSeparator+}tmp;
    end;
  end;

  {$IFDEF INTERNAL}
  procedure DoError;
  begin
    InternalError('Cannot evaluate as Text',AData);
  end;
  {$ENDIF}

  function TypeName(const AType:TType):String;
  begin
    if AType is TNamedType then
       result:=TNamedType(AType).Name
    else
    begin
      result:='';

      {$IFDEF INTERNAL}
      DoError;
      {$ENDIF}
    end;
  end;

  function VariableValueOrName(const AVariable:TVariable):String;
  begin
    if AVariable.ValueData=nil then
       result:=AVariable.Name
    else
       result:=Runner_DataAsText(AVariable.ValueData);
  end;

  function InstanceValueOrVariable(const AInstance:TInstance):TData;
  begin
    result:=AInstance.Value;

    if result=nil then
       if AInstance.Owner is TInstance then
          result:=TInstance(AInstance.Owner).Data.VariableOfValue(AInstance);
  end;

begin
  if AData is TInstance then
     result:=Runner_DataAsText(InstanceValueOrVariable(TInstance(AData)))
  else
  if AData is TText then
     result:=TText(AData).Value
  else
  if AData is TInteger then
     result:=TTextUtils.IntegerToString(TInteger(AData).Base,TInteger(AData).Value)
  else
  if AData is TFloat then
     result:=FloatToStr(TFloat(AData).Value)
  else
  if AData is TBoolean then
     result:=BoolToStr(TBoolean(AData).Value,True)
  else
  if AData is TArrayExpression then
     result:=ArrayAsText(TArrayExpression(AData))
  else
  if AData is TVariable then
     result:=VariableValueOrName(TVariable(AData))
  else
  if AData is TTypeCall then
     result:=TypeName(TTypeCall(AData).TheType)
  else
  {
  if AData is TRange then
     result:=RangeToStr(TRange(AData))  // <-- NEVER CALLED
  else
  }
  begin
    {$IFDEF INTERNAL}
    DoError;
    {$ENDIF}

    //result:=TEvaluate.AsText(AData);
  end;
end;

class function TEvaluate.AsText(AData:TData; const AInstance:TInstance=nil):String;
begin
  AData:=AsData(AData,AInstance);

  result:=Runner_DataAsText(AData);

//  if AData.Owner=nil then
//     AData.Free;
end;

{ TRunner }

Constructor TRunner.Create;
begin
  inherited Create;

  {$IFDEF INTERNAL}
  CheckInstanceLeaks:=True;
  {$ENDIF}

  Breaks:=TBreaks.Create;
  Status:=TRunning.Stopped;
end;

Destructor TRunner.Destroy;
begin
  Breaks.Free;
  inherited;
end;

procedure TRunner.SetLast(const AValue:TRunningNode);
begin
  FLast:=AValue;
end;

procedure TRunner.RuntimeException(const ARuntimeError:TRunExceptionCode; const AText:String{; const ANode:TNode});
begin
  RunException.Code:=ARuntimeError;
  RunException.Text:=AText;
//  RunException.Node:=ANode;
end;

procedure TRunner.RuntimeException(const ARuntimeError:TRunExceptionCode; const AError:TErrors {; const ANode:TNode});
begin
  RuntimeException(ARuntimeError,TErrorTexts.Texts[AError]);
end;

function GetCallRoutine(const ACall:TDataCall):TType; overload;
begin
  result:=TChecker.GetFinalType(ACall.Routine);
end;

// Fahrenheit(Value:Float) {}  <-- setter of: Fahrenheit:=100
class function TRunner.CallSetter(const AInstance, AValue: TInstance;
                                  const ACall: TDataCall): TData;
var tmpParams : TNodes;
    tmpRoutine : TType;
begin
  tmpRoutine:=GetCallRoutine(ACall);

  {$IFDEF INTERNAL}
  if not (tmpRoutine is TParametersType) then
  begin
    result:=nil;
    InternalError('Routine is not a ParametersType',ACall.Routine);
    Exit;
  end;
  {$ENDIF}

  tmpParams:=TParametersType(tmpRoutine).Parameters;

  {$IFDEF INTERNAL}
  if tmpParams.Count<>1 then
  begin
    InternalError('Setter routine must have one parameter',ACall.Routine);
    result:=nil;
  end
  else
  {$ENDIF}
  begin
    ACall.Parameters:=[tmpParams[0]];

    try
      Call(AInstance,ACall,AValue,result);
    finally
      ACall.Parameters:=nil;
      AValue.Free;
    end;
  end;
end;

class function TRunner.Call(const AInstance:TInstance; const AData:TData):TNode;

  {$IFDEF INTERNAL}
  procedure CallError;
  begin
    InternalError('Not a call',AData); // Internal error
  end;
  {$ENDIF}

  function CallVariableMember(const AVariable:TVariable; const AData:TData):TNode;
  var tmp : TInstance;
  begin
    tmp:=TEvaluate.InstanceOf(AInstance,AVariable);

    {
    if tmp.Value is TInstance then
    begin
      if TInstance(tmp.Value).Value=nil then
         TInstance(tmp.Value).Value:=AVariable;
    end;
    }

    if tmp.Value is TInstance then
       result:=Call(TInstance(tmp.Value),AData) // UI/FormView demo
    else
       result:=Call(tmp,AData);
  end;

  (* NEVER CALLED:
  function CallVariable(const AVariable:TVariable):TNode;
  var tmp : TInstance;
  begin
    tmp:=TEvaluate.InstanceOf(AInstance,AVariable);
    result:=Call(tmp,tmp.Value);
  end;
  *)

var tmpData : TData;
begin
  (* NEVER CALLED:
  if AData is TGroup then
     result:=Call(AInstance,TGroup(AData).Expression)  // <-- NEVER CALLED
  else
  *)
  if AData is TMember then
  begin
    tmpData:=TMember(AData).Data;

    if tmpData is TVariable then
       result:=CallVariableMember(TVariable(tmpData),TMember(AData).Member)
    else
    begin
      tmpData:=TEvaluate.AsData(tmpData,AInstance);

      if tmpData is TTypeCall then
         result:=Call(AInstance,TMember(AData).Member)
      else
      (* NEVER CALLED:
      if tmpData is TVariable then
         result:=CallVariableMember(TVariable(tmpData),TMember(AData).Member)
      else
      if tmpData is TInstance then
         result:=Call(TInstance(tmpData),TMember(AData).Member)
      else
      *)
      begin
        result:=nil;

        {$IFDEF INTERNAL}
        CallError;
        {$ENDIF}
      end;
    end;
  end
  else
  if AData is TDataCall then
  begin
    Call(AInstance,TDataCall(AData),nil,tmpData);
    result:=tmpData;
  end
  else
  (* NEVER CALLED:
  if AData is TTypeCall then
     result:=AData // <-- should never pass here ?
  else
  *)
  if AData is TAncestor then
     result:=Call(AInstance,TAncestor(AData).DataCall)
  else
  (* NEVER CALLED:
  if TChecker.IsVariableFunctionType(AData) then
     result:=CallVariable(TVariable(AData))
  else
  *)
  begin
    result:=nil;

    {$IFDEF INTERNAL}
    CallError;
    {$ENDIF}
  end;
end;

class function TRunner.Execute(const AInstance:TInstance; const AType:TType):TNode;
var tmp : TNode;
    N : TNode;
begin
  for N in TChecker.GetFinalType(AType).Items do
      if not (N is TType) then
      begin
        tmp:=Execute(AInstance,N);

        if tmp<>nil then
           Exit(tmp);
      end;

  result:=nil;
end;

type
  TRunningExpression=class(TRunningItem)
  private
    Expression,
    Value : TData;

    ValueInstance : TInstance;

    RunningDone,
    ReturnFunctionType,
    ReturnVariable : Boolean;

    Running : TRunningItem;
  public
    function Next:TNode; override;
  end;

  TRunningArrayExpression=class(TRunningItem)
  private
    ItemsInstance : TInstance;

    Start,
    Item : Integer;

    Evaluator : TRunningExpression;

    Value : TArrayExpression;
    Items : TNodes;

    procedure SetResult(const AData:TData);
  public
    Destructor Destroy; override;

    function CanDestroy(const ANode:TNode):Boolean; override;
    function Next:TNode; override;
  end;

  TRunningDataCall=class(TRunningItem)
  private
    var
      ResultNode : TNode;
      ResultValue : TData;

      IsManyParams : Boolean;

      Parameter : Integer;
      ParametersDone : Boolean;
      ParamsInstance : TInstance;

      ParamArrays : TNodes;

      LocalInstance : TInstance;

      Evaluator : TRunningExpression;
      ArrayEvaluator : TRunningArrayExpression;

      ProfileIndex : Integer;
      StartTime : TStopWatch;

    procedure BeginProfile;
    procedure EndCall(const ASelf:TData);
    procedure EndProfile;
    procedure InitParams(const AddParams:Boolean);
  public
    Call : TDataCall;

    Destructor Destroy; override;

    function Next:TNode; override;
  end;

  TRunningBooleanCondition=class(TRunningItem)
  private
    Evaluator : TRunningExpression;
    DoCondition : Boolean;
  end;

  TRunningIf=class(TRunningBooleanCondition)
  private
    Value : TData;
    DoBlock : Boolean;
  public
    TheIf : TIf;

    function Next:TNode; override;
  end;

  TRunningLoopCondition=class(TRunningBooleanCondition)
  private
    function GetBool:Boolean;
  end;

  TRunningRepeat=class(TRunningLoopCondition)
  private
    RunnerLast : TNode;
  public
    Started : Boolean;
    TheRepeat : TRepeat;

    function CanDestroy(const ANode:TNode):Boolean; override;
    function Next:TNode; override;
  end;

  TRunningWhile=class(TRunningLoopCondition)
  public
    InBlock : Boolean;
    TheWhile : TWhile;

    function Next:TNode; override;
  end;

  TRunningLoop=class(TRunningItem)
  private
    Counter : TInstance;
    CounterVariable : TVariable;
  public
    TheFor : TFor;

    Destructor Destroy; override;

    procedure Prepare; virtual;
  end;

  TRunningNodesLoop=class(TRunningLoop)
  private
    Index : Integer;
    Items : TNodes;

  public
    function Next:TNode; override;
    procedure Prepare; override;
  end;

  TRunningArrayLoop=class(TRunningNodesLoop)
    AArray : TArrayExpression;
  public
    Destructor Destroy; override;

    procedure Prepare; override;
  end;

  TRunningWhen=class(TRunningItem)
  private
    Data : TData;
    Index : Integer;
    Done : Boolean;

    ItemEvaluator,
    Evaluator : TRunningExpression;
  public
    When : TWhen;

    function Next:TNode; override;
  end;

function RunningBlock(const ACaller:TRunningNode;
                      const AInstance:TInstance;
                      const AType:TType):TRunningBlock;
begin
  result:=TRunningBlock.Create {$IFDEF RUNSTACK}(ACaller {Last}){$ENDIF};
  result.Caller:=ACaller;

  result.Local:=TInstance.Create;
  result.Local.Owner:=AInstance;

  result.Instance:=result.Local;

  result.TheType:=AType;
end;

{ TRunningLoop }

procedure TRunningLoop.Prepare;
var tmp : Integer;
begin
  CounterVariable:=TheFor.Counter;

  if CounterVariable=nil then
     CounterVariable:=TVariable.Create;

  if (TheFor.Counter<>nil) and (TheFor.Counter.Owner<>TheFor) then
     tmp:=Instance.Data.Find(CounterVariable)
  else
     tmp:=Instance.AddData(CounterVariable);

  Counter:=TInstance.Create;
  Counter.Owner:=Instance;

  Instance.Data[tmp].Value:=Counter;
end;

Destructor TRunningLoop.Destroy;
begin
  Instance.Data.Remove(CounterVariable);

  if CounterVariable<>TheFor.Counter then
     CounterVariable.Free;

  inherited;
end;

function RunningExpression(const ACaller:TRunningNode;
                           const AInstance:TInstance;
                           const AData:TData):TRunningExpression;
begin
  {$IFDEF INTERNAL}
  if AData=nil then
     InternalError('Expression is nil at RunningExpression',AData);
  {$ENDIF}

  result:=TRunningExpression.Create {$IFDEF RUNSTACK}(ACaller){$ENDIF};
  result.Caller:=ACaller;
  result.Instance:=AInstance;
  result.Expression:=AData;
end;

{ TRunningNumberLoop }

type
  TRunningNumberLoop=class(TRunningLoop)
  private
    Expression : TData;

    Int : TInteger;

    Start,
    Finish : Integer;

    Evaluator : TRunningExpression;

    Descending,
    StartDone,
    FinishDone,
    DoneFirst : Boolean;
  public
    function CanDestroy(const ANode:TNode):Boolean; override; //<-- Try to Remove this !!!
    function Next:TNode; override;
    procedure Prepare; override;
  end;

function TRunningNumberLoop.CanDestroy(const ANode:TNode):Boolean; // <-- Try to Remove this !!!
begin
  result:=ANode<>Evaluator;
end;

procedure TRunningNumberLoop.Prepare;
begin
  inherited;

  Int:=TInteger.Create(Start);
  Int.Owner:=Counter;

  Counter.Value:=Int;
end;

function TRunningNumberLoop.Next:TNode;

  function Evaluate(const AData:TData):TNode;
  begin
    Evaluator:=RunningExpression(Self,Instance,AData);
    Evaluator.Runner:=Runner;

    Runner.Last:=Evaluator;
    result:=Evaluator.Next;
  end;

  function GetValue:Integer;
  var tmp : TData;
  begin
    tmp:=Evaluator.Value;

    //if tmp is TVariable then
    //   tmp:=ValueOf(Instance,TVariable(tmp));

    result:=(tmp as TInteger).Value;

    Runner.Last:=Self;

    Evaluator.Free;
    Evaluator:=nil;

    if tmp.Owner=nil then
       tmp.Free;
  end;

  procedure GetStart;
  begin
    Start:=GetValue;
    StartDone:=True;
  end;

  procedure GetFinish;
  begin
    Finish:=GetValue;
    FinishDone:=True;
  end;

  function KeepLoop:Boolean;
  begin
    if Descending then
       result:=Int.Value>Finish
    else
       result:=Int.Value<Finish;
  end;

  function Finished:Boolean;
  begin
    if Descending then
       result:=Int.Value<Finish
    else
       result:=Int.Value>Finish;
  end;

begin
  if StartDone and FinishDone then
  begin
    result:=nil;

    if DoneFirst then
    begin
      if KeepLoop then  // <
      begin
        if Descending then
           Dec(Int.Value)
        else
           Inc(Int.Value);

        result:=TheFor.Block;
      end;
    end
    else
    begin
      Int.Value:=Start;
      DoneFirst:=True;

      Descending:=Finish<Start;

      if not Finished then
         result:=TheFor.Block;
    end;
  end
  else
  if Evaluator<>nil then
  begin
    if StartDone then
       GetFinish
    else
       GetStart;

    result:=Self;
  end
  else
  if StartDone then
  begin
    if Expression=nil then
       result:=Evaluate(TheFor.Last)
    else
    if Expression is TRange then
       result:=Evaluate(TRange(Expression).Max)
    else
    begin
      result:=nil;

      {$IFDEF INTERNAL}
      InternalError('For Expression not supported',Expression);
      {$ENDIF}
    end;

    if result=nil then
    begin
      GetFinish;
      result:=Self;
    end;
  end
  else
  begin
    if Expression=nil then
       result:=Evaluate(TheFor.First)
    else
    if Expression is TRange then
       result:=Evaluate(TRange(Expression).Min)
    else
    begin
      result:=nil;

      {$IFDEF INTERNAL}
      InternalError('For Expression not supported',Expression);
      {$ENDIF}
    end;

    if result=nil then
    begin
      GetStart;
      result:=Self;
    end;
  end;
end;

{ TTextLoop }

type
  TRunningTextLoop=class(TRunningLoop)
  private
    Text : TText;

    Index : Integer;

    S : String;
  public
    function Next:TNode; override;
    procedure Prepare; override;
  end;

procedure TRunningTextLoop.Prepare;
begin
  inherited;
  Index:=0;

  Text:=TText.Create('');
  Text.Owner:=Counter;

  Text.Value:=S;

  Counter.Value:=Text;
end;

function TRunningTextLoop.Next: TNode;
begin
  if Index<High(S) then
  begin
    Inc(Index);
    Text.Value:=S[Index];
    result:=TheFor.Block;
  end
  else
    result:=nil;
end;

{ TRunningNodesLoop }

function TRunningNodesLoop.Next: TNode;
begin
  if Index<=High(Items) then
  begin
    Counter.Value:=Items[Index] as TData;

    Runner.Last:=Self;
    result:=TheFor.Block;
    Inc(Index);
  end
  else
    result:=nil;
end;

procedure TRunningNodesLoop.Prepare;
begin
  inherited;
  Index:=0;
end;

{ TRunningArrayLoop }

destructor TRunningArrayLoop.Destroy;
begin
  if AArray.Owner=nil then
     AArray.Free;

  inherited;
end;

procedure TRunningArrayLoop.Prepare;
begin
  inherited;
  Items:=AArray.Parameters;
end;

{ TRunningIf }

function TRunningIf.Next: TNode;

  function RunIf(const AValue:TData):TNode;
  begin
    if (AValue as TBoolean).Value then
       result:=TheIf.ThenBlock
    else
       result:=TheIf.ElseBlock;
  end;

  function ProcessBlock:TNode;
  begin
    DoBlock:=False;

    {$IFDEF INTERNAL}
    if Evaluator<>nil then
       InternalError('RunIf, Evaluator is not nil',Evaluator);
       //Value:=Evaluator.Value;
    {$ENDIF}

    result:=RunIf(Value);

    if Value.Owner=nil then
       Value.Free;
  end;

begin
  if DoCondition then
  begin
    DoCondition:=False;

    Evaluator:=RunningExpression(Self,Instance,TheIf.Condition);
    Evaluator.Runner:=Runner;

    Runner.Last:=Evaluator;
    result:=Evaluator.Next;

    DoBlock:=True;

    if result=nil then
    begin
      Value:=Evaluator.Value;

      Runner.Last:=Self;

      Evaluator.Free;
      Evaluator:=nil;

      result:=ProcessBlock;
    end;
  end
  else
  if DoBlock then
  begin
    Value:=Evaluator.Value;
    Evaluator:=nil;
    result:=ProcessBlock;
  end
  else
    result:=nil;
end;

{ TRunningLoopCondition }

function TRunningLoopCondition.GetBool: Boolean;
var tmpBool : TBoolean;
begin
  tmpBool:=(Evaluator.Value as TBoolean);
  result:=tmpBool.Value;

  if tmpBool.Owner=nil then
     tmpBool.Free;

  Runner.Last:=Self;

//  Evaluator.Free;
//  Evaluator:=nil;
end;

{
function TRunningLoopCondition.CanDestroy(const ANode:TNode):Boolean;
begin
  result:=ANode<>Evaluator;
end;
}

{ TRunningRepeat }

function TRunningRepeat.CanDestroy(const ANode:TNode):Boolean;
begin
  RunnerLast:=ANode; // <-- catch ANode
  result:=False;

  //result:=ANode<>Runner.Last; // <-- The RunningBlock of Repeat.TheBlock
end;

function TRunningRepeat.Next: TNode;

  function GetValue:TNode;
  begin
    if GetBool then
       result:=nil
    else
    begin
      result:=TheRepeat.Block;
      DoCondition:=True;
    end;

    Evaluator.Free;
    Evaluator:=nil;
  end;

begin
  if DoCondition then
  begin
    DoCondition:=False;

    RunnerLast.Free;
    Runner.Last:=Self;

    Evaluator:=RunningExpression(Runner.Last,Instance,TheRepeat.Condition);
    Evaluator.Runner:=Runner;

    Runner.Last:=Evaluator;
    result:=Evaluator.Next;

    if result=nil then
       result:=GetValue;
  end
  else
  if Evaluator<>nil then
     result:=GetValue
  else
  if Started then
  begin
    result:=TheRepeat.Block;
    DoCondition:=True;
  end
  else
  begin
    Started:=True;
    result:=Self;
  end;
end;

{ TRunningWhile }

function TRunningWhile.Next: TNode;

  function GetValue:TNode;
  begin
    if GetBool then
    begin
      InBlock:=True;
      result:=TheWhile.Block;
    end
    else
      result:=nil;
  end;

begin
  if InBlock then
  begin
    InBlock:=False;
    result:=Self;
  end
  else
  if Evaluator=nil then
  begin
    DoCondition:=False;

    Evaluator:=RunningExpression(Self,Instance,TheWhile.Condition);
    Evaluator.Runner:=Runner;

    Runner.Last:=Evaluator;
    result:=Evaluator.Next;

    if result=nil then
    begin
      result:=GetValue;
      Evaluator.Free;
      Evaluator:=nil;
    end;
  end
  else
  begin
    result:=GetValue;
    Evaluator:=nil;
  end;
end;

{ TRunningWhen }

function TRunningWhen.Next:TNode;

  procedure GetData;
  begin
    Data:=Evaluator.Value;

    if Data.Owner=nil then
       Data.Owner:=Self;

    Evaluator.Free;
    Evaluator:=nil;

    Index:=-1;
  end;

  function ItemEvaluatorTrue:Boolean;
  begin
    result:=(ItemEvaluator.Value as TBoolean).Value;
  end;

  function FinishBlock(const ABlock:TStatement):TStatement;
  begin
    Done:=True;
    Runner.Last:=Self;
    result:=ABlock;
  end;

  function GetEvaluatedItem:TNode;
  begin
    if ItemEvaluatorTrue then
       result:=FinishBlock(When.Items[Index].Block)
    else
       result:=Self;

    if ItemEvaluator.Expression.Owner=nil then
       ItemEvaluator.Expression.Free;

    if ItemEvaluator.Value.Owner=nil then
       ItemEvaluator.Value.Free;
  end;

  function GetWhenEvaluator(const AItem:TWhenItem):TRunningExpression;
  var tmp : TIsEqual;
  begin
    if AItem.Expression is TLogical then
    begin
      TLogical(AItem.Expression).Left:=Data;
      result:=RunningExpression(Self,Instance,AItem.Expression);
    end
    else
    begin
      tmp:=TIsEqual.Create;
      tmp.Left:=Data;
      tmp.Right:=AItem.Expression;

      result:=RunningExpression(Self,Instance,tmp);
    end;

    result.Runner:=Runner;

    Runner.Last:=result;
  end;

  function FindWhenItem:TNode;
  var W : TWhenItem;
  begin
    result:=nil;

    Inc(Index);

    if Index<=High(When.Items) then
    begin
      W:=When.Items[Index];

      ItemEvaluator:=GetWhenEvaluator(W);

      result:=ItemEvaluator.Next;

      if result=nil then
      begin
        result:=GetEvaluatedItem;

        ItemEvaluator.Free;
        ItemEvaluator:=nil;
      end;
    end
    else
    if When.ElseBlock<>nil then
       result:=FinishBlock(When.ElseBlock);
  end;

begin
  if Done then
  begin
    if Data.Owner=Self then
       Data.Free;

    result:=nil;
  end
  else
  if ItemEvaluator<>nil then
  begin
    result:=GetEvaluatedItem;
    ItemEvaluator:=nil;
  end
  else
  if Data<>nil then
     result:=FindWhenItem
  else
  if Evaluator=nil then
  begin
    Evaluator:=RunningExpression(Self,Instance,When.Expression);
    Evaluator.Runner:=Runner;

    Runner.Last:=Self; //Evaluator;

    result:=Evaluator.Next;

    if result=nil then
    begin
      GetData;
      result:=Next;
    end;
  end
  else
  begin
    (* NEVER CALLED:
    GetData;
    result:=Next;
    *)

    result:=nil;
    {$IFDEF INTERNAL}
    InternalError('Should not happen at Running When',When);
    {$ENDIF}
  end;
end;

function ArrayExpressionOf(const AInstance:TInstance; const AVariable:TVariable):TArrayExpression;

  {$IFDEF INTERNAL}
  procedure DoError(const ANode:TNode);
  begin
    InternalError('Cannot foreach of array variable',ANode);
  end;
  {$ENDIF}

var tmpInst : TInstance;
    tmp,
    tmpNode : TNode;
begin
  result:=nil;

  tmpInst:=AInstance;

  if AVariable.VariableType is TArrayType then // <-- Before is TClassType !!!
     tmpNode:=AVariable
  else
  if TChecker.VariableIsClass(AVariable) then
  begin
    tmpNode:=TClassType(AVariable.VariableType).DefaultIndexed;

    if tmpNode<>nil then
       tmpInst:=TEvaluate.InstanceOf(AInstance,AVariable);
  end
  else
     tmpNode:=nil;

  {$IFDEF INTERNAL}
  if tmpNode=nil then
     // TODO: Investigate ClassType looking for ForEach methods
     InternalError('Cannot foreach of variable',AVariable.VariableType)
  else
  {$ENDIF}
  begin
    tmp:=ValueOf(tmpInst,tmpNode as TVariable); // <-- necessary ??

    if tmp is TArrayExpression then
       result:=TArrayExpression(tmp)
    else
    if TChecker.IsManyValues(tmpNode) then // special case when there is only one value
    begin
      if tmp=nil then
         result:=nil
      else
      if tmp is TData then
      begin
        result:=TArrayExpression.Create;
        result.Parameters.Add(TCloner.Node(result,TEvaluate.AsData(TData(tmp),tmpInst)));
      end
      {$IFDEF INTERNAL}
      else
        DoError(tmp);
      {$ENDIF}
    end
    {$IFDEF INTERNAL}
    else
      DoError(tmp);
    {$ENDIF}
  end;
end;

function OwnerLoop(const ANode:TNode):TNode;
begin
  result:=ANode;

  repeat
    // repeat while for
    if (result is TLoopStatement) or (result is TFor) then
       Exit
    else
       result:=result.Owner;

  until result=nil;
end;

{ TRunner }

procedure TRunner.DoMonitor;
begin
  if Assigned(OnMonitor) then
     OnMonitor(Self);
end;

class function TRunner.ExecuteFor(const AInstance:TInstance; const AFor:TFor):TNode;

  function CounterInstance:TInstance;
  var tmp : Integer;
  begin
    if AFor.Counter.Owner=AFor then
       tmp:=AInstance.AddData(AFor.Counter)
    else
       tmp:=AInstance.Data.Find(AFor.Counter);

    result:=TInstance.Create;
    result.Owner:=AInstance;

    AInstance.Data[tmp].Value:=result;
  end;

  function NumberLoop(const Start,Finish:Integer):TNode;
  var tmp : TInteger;
      Descending : Boolean;

    function Finished:Boolean;
    begin
      if Descending then
         result:=tmp.Value<Finish
      else
         result:=tmp.Value>Finish;
    end;

    function KeepLoop:Boolean;
    begin
      if Descending then
         result:=tmp.Value>=Finish
      else
         result:=tmp.Value<=Finish;
    end;

  var Inst : TInstance;
  begin
    result:=nil;

    if AFor.Counter=nil then
    begin
      Inst:=nil;
      tmp:=TInteger.Create(Start);
    end
    else
    begin
      Inst:=CounterInstance;

      {$IFDEF INTERNAL}
      if (Inst.Value=nil) or (Inst.Value is TVariable) then
      else
        InternalError('For instance not a variable',Inst.Value);
      {$ENDIF}

      begin
        tmp:=TInteger.Create(Start);
        tmp.Owner:=Inst;
        Inst.Value:=tmp;
      end
      {
      else
      begin
        // NEVER CALLED:
        tmp:=(Inst.Value as TInteger);
        tmp.Value:=Start;
      end;
      }
    end;

    try
      if Inst<>nil then
         tmp.Owner:=Inst;

      Descending:=Finish<Start;

      tmp.Value:=Start;

      if not Finished then
      while KeepLoop do
      begin
        if Inst<>nil then
           Inst.Value:=tmp;

        result:=Execute(AInstance,AFor.Block);

        if Inst<>nil then
           Inst.Value:=nil;

        if result<>nil then
           if result is TBreak then
           begin
             result:=nil;
             break;
           end;

        if Descending then
           Dec(tmp.Value)
        else
           Inc(tmp.Value);
      end;
    finally
      if Inst=nil then
         tmp.Free
      else
      begin
        Inst.Value:=tmp;
        AInstance.Data.Remove(AFor.Counter);
      end;
    end;
  end;

  // TODO: Remove when TText implements enumerator for Character[]
  function TextLoop(const AText:String):TNode;
  var tmp : TText;
      Inst : TInstance;
      t : Integer;
  begin
    result:=nil;

    Inst:=CounterInstance;
    try
      tmp:=TText.Create('');
      tmp.Owner:=Inst;

      try
        for t:=Low(AText) to High(AText) do
        begin
          tmp.Value:=AText[t];
          Inst.Value:=tmp;

          result:=Execute(AInstance,AFor.Block);

          Inst.Value:=nil;
          AFor.Counter.ValueData:=nil; // <-- necessary?

          if result<>nil then
             if result is TBreak then
             begin
               result:=nil;
               break;
             end;
        end;
      finally
        tmp.Free;
      end;
    finally
      AInstance.Data.Remove(AFor.Counter);
    end;
  end;

  function ItemsLoop(const AItems:TNodes):TNode;
  var Inst : TInstance;
      t : Integer;
      tmp : TData;
  begin
    result:=nil;

    Inst:=CounterInstance;
    tmp:=Inst.Value;
    try
      for t:=0 to High(AItems) do
      begin
        Inst.Value:=AItems[t] as TData; // AFor.Counter.ValueData:=

        result:=Execute(AInstance,AFor.Block);

        Inst.Value:=nil;

        AFor.Counter.ValueData:=nil; // <-- necessary?

        if result<>nil then
           if result is TBreak then
           begin
             result:=nil;
             break;
           end;
      end;
    finally
      tmp.Free;

      AInstance.Data.Remove(AFor.Counter);
    end;
  end;

  function ArrayLoop(const AArray:TArrayExpression):TNode;
  begin
    result:=ItemsLoop(AArray.Parameters);
  end;

  function VariableLoop(const AVariable:TVariable):TNode;
  var tmp : TData;
      tmpNode : TNode;
      tmpInst : TInstance;
  begin
    {$IFDEF INTERNAL}
    result:=nil;
    {$ENDIF}

    tmpInst:=AInstance;

    if AVariable.VariableType is TArrayType then // <-- Before is TClassType !!!
       tmpNode:=AVariable
    else
    if AVariable.VariableType is TClassType then
    begin
      tmpNode:=TClassType(AVariable.VariableType).DefaultIndexed;

      if tmpNode<>nil then
         tmpInst:=TEvaluate.InstanceOf(AInstance,AVariable);
    end
    else
       tmpNode:=nil;

    {$IFDEF INTERNAL}
    if tmpNode=nil then
       // TODO: Investigate ClassType looking for ForEach methods
       InternalError('Cannot foreach of variable',AVariable.VariableType)
    else
    {$ENDIF}
    begin
      tmp:=ValueOf(tmpInst,tmpNode as TVariable); // <-- necessary ?

      {$IFDEF INTERNAL}
      if not (tmp is TArrayExpression) then
         InternalError('Cannot foreach of array variable',tmp)
      else
      {$ENDIF}
         result:=ArrayLoop(TArrayExpression(tmp));
    end;
  end;

var Start, Finish : Integer;
    tmpExp,tmp : TData;
begin
  if AFor.InExpression=nil then
  begin
    Start:=CalcAsInteger(AFor.First,AInstance);
    Finish:=CalcAsInteger(AFor.Last,AInstance);

    result:=NumberLoop(Start,Finish);
  end
  else
  begin
    tmpExp:=TEvaluate.AsData(AFor.InExpression,AInstance);

    if tmpExp is TInstance then
       if IsReference(AFor.InExpression) then
          tmpExp:=AFor.InExpression;

    if tmpExp is TRange then
    begin
      Start:=CalcAsInteger(TRange(tmpExp).Min,AInstance);
      Finish:=CalcAsInteger(TRange(tmpExp).Max,AInstance);

      result:=NumberLoop(Start,Finish);
    end
    else
    if tmpExp is TVariable then
       result:=VariableLoop(TVariable(tmpExp))
    else
    begin
      if tmpExp is TTypeCall then
         result:=ItemsLoop(TTypeCall(tmpExp).TheType.Items)
      else
      begin
        //tmp:=TEvaluate.AsData(AFor.InExpression,AInstance);

        tmp:=tmpExp;

        if tmp is TText then
           result:=TextLoop(TText(tmp).Value)
        else
        if tmp is TArrayExpression then
           result:=ArrayLoop(TArrayExpression(tmp))
        else
        if tmp is TSelf then
        begin
          // TODO !!!
          result:=nil;

          {$IFDEF INTERNAL}
          InternalError('Cannot run for in Self',AFor.InExpression);
          {$ENDIF}
        end
        else
        begin
          result:=nil;

          {$IFDEF INTERNAL}
          InternalError('Cannot run for in',AFor.InExpression);
          {$ENDIF}
        end;
      end;
    end;
  end;
end;

procedure TRunner.Pause;
begin
  Status:=TRunning.Paused;
  DoMonitor;
end;

class function TRunner.Execute(const AInstance:TInstance; const ANode:TNode):TNode;

  procedure RunWhile(const AWhile:TWhile);
  var tmp : TNode;
  begin
    while CalcAsBoolean(AWhile.Condition,AInstance) do
    begin
      tmp:=Execute(AInstance,AWhile.Block);

      if (tmp<>nil) and (tmp<>AWhile) then
         break;
    end;
  end;

  procedure RunRepeat(const ARepeat:TRepeat);
  var tmp : TNode;
  begin
    repeat
      tmp:=Execute(AInstance,ARepeat.Block);

      if (tmp<>nil) and (tmp<>ARepeat) then
         break;

    until CalcAsBoolean(ARepeat.Condition,AInstance);
  end;

  function RunIf(const AIf:TIf):TNode;
  var tmp : TNode;
  begin
    if CalcAsBoolean(AIf.Condition,AInstance) then
       tmp:=AIf.ThenBlock
    else
       tmp:=AIf.ElseBlock;

    result:=Execute(AInstance,tmp);
  end;

  function RunWhen(const AWhen:TWhen):TNode;

    function WhenItem(const AInstance:TInstance; const AValue:TData; const AItem:TWhenItem):Boolean;
    var tmp : TIsEqual;
    begin
      if AItem.Expression is TLogical then
      begin
        TLogical(AItem.Expression).Left:=AValue;

        result:=CalcAsBoolean(AItem.Expression,AInstance);
      end
      else
      begin
        tmp:=TIsEqual.Create;
        try
          tmp.Left:=AValue;
          tmp.Right:=AItem.Expression;

          result:=CalcAsBoolean(tmp,AInstance);
        finally
          tmp.Left:=nil;
          tmp.Right:=nil;
          tmp.Free;
        end;
      end;
    end;

  var W : TWhenItem;
      tmpData : TData;
  begin
    result:=nil;

    tmpData:=TEvaluate.AsData(AWhen.Expression,AInstance);

    for W in AWhen.Items do
        if WhenItem(AInstance,tmpData,W) then
        begin
          result:=Execute(AInstance,W.Block);
          Exit;
        end;

    if AWhen.ElseBlock<>nil then
       result:=Execute(AInstance,AWhen.ElseBlock);
  end;

  function RunBlock(const ABlock:TType):TNode;
  var tmp : TInstance;
  begin
    tmp:=TInstance.Create;
    try
      tmp.Owner:=AInstance;
      result:=Execute(tmp,ABlock);

      if result<>nil then
         if result.Owner is TInstance then
            result.Owner:=nil;
    finally
      tmp.Free;
    end;
  end;

  function RunTry(const ATry:TTry):TNode;
  begin
    result:=Execute(AInstance,ATry.Block);

    // TODO: ATry.Catch exceptions, run them

    if ATry.TheFinally<>nil then
       result:=Execute(AInstance,ATry.TheFinally);
  end;

begin
  result:=nil;

  if ANode=nil then
     Exit
  else
  if ANode is TAssignment then
     DoAssignment(AInstance,TAssignment(ANode))
  else
  if ANode is TIf then
     result:=RunIf(TIf(ANode))
  else
  if ANode is TWhile then
     RunWhile(TWhile(ANode))
  else
  if ANode is TRepeat then
     RunRepeat(TRepeat(ANode))
  else
  if ANode is TFor then
     result:=ExecuteFor(AInstance,TFor(ANode))
  else
  if ANode is TVariable then
  begin
    if not TVariable(ANode).Clauses.Shared then
       Instance_NewVariable(AInstance,TVariable(ANode),False);
       //AInstance.AddData(TVariable(ANode));
  end
  else
  if ANode is TWhen then
     result:=RunWhen(TWhen(ANode))
  else
  if ANode is TBlockStatement then
     result:=RunBlock(TBlockStatement(ANode).Block)
  else
  if ANode is TCallData then
     result:=Call(AInstance,TCallData(ANode).Value)
  else
  if ANode is TReturn then
  begin
    if TReturn(ANode).Value=nil then
       result:=ANode
    else
    if TReturn(ANode).Value is TSelf then
       result:=TReturn(ANode).Value
    else
       result:=TEvaluate.AsData(TReturn(ANode).Value,AInstance);
  end
  else
  if ANode is TContinue then
     result:=OwnerLoop(ANode)
  else
  if ANode is TBreak then
     result:=ANode
  else
  if ANode is TWith then  // <-- NOTHING ! (TWith is not executable)
     TPlugin.TryHook(TWith(ANode))
  else
  if ANode is TType then
     result:=Execute(AInstance,TType(ANode))
  else
  if ANode is TTry then
     result:=RunTry(TTry(ANode))

  {$IFDEF INTERNAL}
  else
     InternalError('Internal, cannot execute',ANode);
  {$ENDIF}
end;

procedure TRunner.Loop;

  function IsOneTimeBreak(const ANode:TNode):Boolean;
  begin
    result:=ANode=OneTimeBreak;

    if not result then
       if ANode is TRunningBlock then
          result:=TRunningBlock(ANode).TheType=OneTimeBreak;
  end;

  procedure DoBreak(const AIndex:Integer);
  begin
    Pause;

    if Assigned(OnBreak) then
       OnBreak(Self,AIndex);
  end;

var tmp : Integer;
    tmpNode : TNode;
    tmpInst : TInstance;
begin
  repeat
    if Last=nil then
       tmpInst:=Root
    else
       tmpInst:=Last.Instance;

    Current:=ExecuteNext(tmpInst,Current);

    // COSTLY: DoMonitor; <-- do only on program break !

    if RunException.Code<>TRunExceptionCode.None then
    begin
      Pause;
      break;
    end
    else
    if Current=nil then
       break
    else
    begin
      tmpNode:=CurrentNode;

      if (OneTimeBreak<>nil) and IsOneTimeBreak(tmpNode) then
      begin
        OneTimeBreak:=nil;

        Pause;
        break;
      end
      else
      begin
        tmp:=Breaks.Find(tmpNode);

        if tmp<>-1 then
        begin
          DoBreak(tmp);
          break;
        end;
      end;
    end;

  until Status<>TRunning.Running;

  if Current=nil then
     Stop;
end;

{ TRunningVariableInit }

type
  TRunningVariableInit=class(TRunningItem)
  private
    Evaluator : TRunningExpression;
    Variable : TVariable;
  public
    function Next:TNode; override;
  end;

function TRunningVariableInit.Next:TNode;

  procedure GetResult(const AValue:TData);
  var tmp : TInstance;
  begin
    if TChecker.IsEnumerationItem(AValue) then
    begin
      tmp:=TEvaluate.InstanceOf(Instance,Variable);
      tmp.Value:=AValue;
    end
    else
    if AValue is TCastingData then
       GetResult(TCastingData(AValue).Data)
    else
    begin
      if IsReference(AValue) then
      begin
        tmp:=TEvaluate.InstanceOf(Instance,TVariable(AValue));

        {$IFDEF INTERNAL}
        if tmp=nil then
           Error('Instance not found for:',AValue);
        {$ENDIF}
      end
      else
        tmp:=CloneOrReference(Instance,AValue,Variable);

      Instance_Assign(Instance,Variable,tmp);
    end;
  end;

begin
  if Evaluator=nil then
  begin
    Evaluator:=RunningExpression(Self,Instance,Variable.ValueData);

    Evaluator.ReturnFunctionType:=TChecker.IsVariableFunctionType(Variable);

    Evaluator.Runner:=Runner;

    result:=Evaluator.Next;

    if result=nil then
    begin
      Evaluator.Value:=TryRoundFloat(Variable,Evaluator.Value);

      GetResult(Evaluator.Value);
      Evaluator.Free;
      Runner.Last:=Self;
    end;
  end
  else
  begin
    GetResult(Evaluator.Value);

    result:=nil;
  end
end;

function TRunner.DoStart(const WithDebug:Boolean; const ANode:TNode):TRunException;
var tmp : TNode;
begin
  result.Code:=TRunExceptionCode.None;

  Initiate_Run(WithDebug,ANode);

  if WithDebug then
  begin
    Loop;

    result:=RunException;

    if result.Code<>TRunExceptionCode.None then
       Pause;
  end
  else
  begin
    tmp:=Execute(Root,Current);

    // What to do with this "tmp" output? (return to caller, instead of returning a TRunException?)
    if tmp<>nil then
       if tmp.Owner=nil then
          tmp.Free;

    Root.Free;
    Stop;

    result:=RunException;

  end;
end;

function TRunner.Start(const ANode:TNode):TRunException;
begin
  result:=DoStart(True,ANode);
end;

function TRunner.StartNoDebug(const ANode:TNode):TRunException;
begin
  try
    result:=DoStart(False,ANode);
  except
    on E:EBeeException do
    begin
      // Pending: Catch other exceptions
      RunException.Code:=TRunExceptionCode.ErrorInternal;
      RunException.Text:=E.Message;

      result:=RunException;
    end;
  end;
end;

//var DebugResult : TData;

procedure PushParameters(const AInstance,AParamsInstance:TInstance; const ACall:TDataCall);

  function DataFrom(const AStart:Integer):TData;
  var L, t : Integer;
      tmpData : TData;
  begin
    L:=Length(ACall.Parameters);

    if L-AStart=1 then
       result:=TEvaluate.AsData(ACall.Parameters[AStart] as TData,AInstance)
    else
    begin
      result:=CreateArrayExpression(L-AStart);

      for t:=AStart to High(ACall.Parameters) do
      begin
        tmpData:=TEvaluate.AsData(ACall.Parameters[t] as TData,AInstance);
        TArrayExpression(result).Parameters[t-AStart]:=tmpData;

        if tmpData.Owner=nil then
           tmpData.Owner:=result;
      end;
    end;
  end;

  procedure SetValue(const AIndex:Integer; const AData,AValue:TData);
  var tmp : TInstance;
      tmpType : TType;
  begin
    if AData is TInstance then
    begin
      {$IFDEF INTERNAL}
      if not (AValue is TInstance) then
         InternalError('Internal, setvalue is not TInstance',AValue);
      {$ENDIF}

      tmp:=TInstance.Create;
      tmp.Value:=AValue;
    end
    else
    begin
      if (AValue is TTypeCall) and (AData is TTypeCall) then
      begin
        //tmpType:=TChecker.TheTypeOf(TTypeCall(AData).TheType);

        tmp:=TInstance.Create;
        tmp.Value:=AData;
      end
      else
      begin
        tmpType:=TChecker.GetDataType(nil,AData);

        tmp:=FromValue(AInstance,tmpType,AValue);
      end;
    end;

    tmp.Owner:=AParamsInstance;

    AParamsInstance.Data[AIndex].Value:=tmp;
  end;

  procedure SetParameter(const AIndex:Integer; const AData:TData);
  var tmp : TData;
  begin
    tmp:=TEvaluate.AsData(AData,AInstance);

    if tmp is TInstance then
       if TInstance(tmp).Value=nil then
          TInstance(tmp).Value:=AData; // <-- See Call FindOverriden !!

    SetValue(AIndex,tmp,tmp);
  end;

var t : Integer;
    tmpParam : TNode;
    tmpRoutine : TParametersType;
begin
  tmpRoutine:=TParametersType(GetCallRoutine(ACall));

  for t:=0 to High(ACall.Parameters) do
  begin
    tmpParam:=tmpRoutine.Parameters[t];

    if TChecker.IsManyValues(tmpParam) then // ...
    begin
      SetValue(t,tmpParam as TData,DataFrom(t));
      break;
    end
    else
      SetParameter(t,ACall.Parameters[t] as TData);
  end;
end;

function FindOverriden(const AFinder:TFinder; const AData:TData; const ARoutine:TType):TType;
begin
  {$IFDEF INTERNAL}
  result:=ARoutine;

  if not (AData is TVariable) then
     InternalError('Find Override Data is not a variable',AData)
  else
  if not (ARoutine is TRoutine) then
     InternalError('Find Override Routine is not a routine',ARoutine)
  else
  {$ENDIF}
  begin
    result:=AFinder.FindOverride(TRoutine(ARoutine),TVariable(AData).VariableType.Items);

    {$IFDEF INTERNAL}
    if result=nil then
       InternalError('Find Override cannot find routine',ARoutine);
    {$ENDIF}
  end;
end;

function GetVariableCallRoutine(const ACall:TVariableCall; const AInstance:TInstance):TType;

  {$IFDEF INTERNAL}
  function IsValidRoutineCall(const AValue:TNode):Boolean;
  begin
    result:=(AValue is TDataCall) or (AValue is TTypeCall); // and (TTypeCall(AValue).TheType is TRoutine)
  end;
  {$ENDIF}

var tmp : TInstance;
begin
  tmp:=TEvaluate.InstanceOf(AInstance,ACall.Variable);

  {$IFDEF INTERNAL}
  if (tmp=nil) or (not IsValidRoutineCall(tmp.Value)) then
  begin
    result:=nil;
    InternalError('VariableCall is not a DataCall',ACall);
  end
  else
  {$ENDIF}
  if tmp.Value is TTypeCall then
     result:=TTypeCall(tmp.Value).TheType as TRoutine
  else
     result:=TDataCall(tmp.Value).Routine;   // <-- NEVER CALLED
end;

function GetCallRoutine(const AFinder:TFinder; const ACall:TDataCall; const AInstance:TInstance):TType; overload;

  function FindPolymorphic(const ARoutine:TRoutine; const AItems:TNodes):TRoutine;
  var tmpFinder : TFinder;
  begin
    if AFinder=nil then
       tmpFinder:=TFinder.Create
    else
       tmpFinder:=AFinder;

    // When ARoutine is called from an inherited descendant variable, try to find polymorphic:
    result:=tmpFinder.FindOverride(ARoutine,AItems);

    if AFinder=nil then
       tmpFinder.Free;
  end;

var tmpValue : TData;
    tmpFinder : TFinder;
    tmpVar : TVariable;
    tmpResult : TType;
begin
  if ACall is TVariableCall then
     result:=GetVariableCallRoutine(TVariableCall(ACall),AInstance)
  else
  begin
    { NEVER CALLED:
    if AInstance=nil then
       tmpValue:=nil
    else
    }
       tmpValue:=AInstance.Value;

    { NEVER CALLED:
    if tmpValue is TInstance then
       tmpValue:=TInstance(tmpValue).Value;
    }

    if tmpValue is TVariable then
    begin
      if AFinder=nil then
         tmpFinder:=TFinder.Create
      else
         tmpFinder:=AFinder;

      result:=FindOverriden(tmpFinder,tmpValue,GetCallRoutine(ACall));

      if AFinder=nil then
         tmpFinder.Free;
    end
    else
    begin
      result:=GetCallRoutine(ACall);

      if result is TRoutine then
         if (AInstance<>nil) and (AInstance.Owner is TInstance) then
         begin
           if AInstance.Value is TInstance then
              tmpValue:=AInstance.Value
           else
              tmpValue:=AInstance;

           tmpVar:=TInstance(AInstance.Owner).Data.VariableOfValue(tmpValue);

           if (tmpVar<>nil) and (tmpVar.VariableType<>result.Owner) then
           begin
             tmpResult:=FindPolymorphic(TRoutine(result),tmpVar.VariableType.Items);

             if tmpResult<>nil then
                result:=tmpResult;
           end;
         end;
    end;
  end;
end;

{
      if ACall is TVariableCall then
         tmpRoutine:=GetVariableCallRoutine(TVariableCall(ACall),AInstance)
      else
      begin
        tmpRoutine:=GetCallRoutine(ACall.Routine);

        tmpValue:=AInstance.Value;

        if tmpValue<>nil then
        begin
          // while ?
          if tmpValue is TInstance then
             tmpValue:=TInstance(tmpValue).Value;

          if tmpValue is TVariable then
          begin
            tmpFind:=TFinder.Create;
            try
              tmpRoutine:=FindOverriden(tmpFind,tmpValue,GetCallRoutine(ACall.Routine));
            finally
              tmpFind.Free;
            end;
          end;
        end;
      end;
}

class procedure TRunner.Call(const AInstance:TInstance;
                             const ACall:TDataCall;
                             const ASetter:TInstance; // <-- special case when called from CallSetter
                             out AResult:TData);
var R : TRunningDataCall;
    tmpInst : TInstance;
    tmpRoutine : TType;
//    tmpValue : TData;
    tmpHook : TPluginHook;
begin
  R:=TRunningDataCall.Create {$IFDEF RUNSTACK}(TRunningNode.LastStack){$ENDIF};
  try
    R.Instance:=AInstance;
    R.Call:=ACall;

    R.BeginProfile;

    R.InitParams(True);

    if ASetter=nil then
       PushParameters(AInstance,R.ParamsInstance,ACall)
    else
       R.ParamsInstance.Data[0].Value:=ASetter;

    tmpRoutine:=GetCallRoutine(ACall);

    {$IFDEF INTERNAL}
    if not (tmpRoutine is TRoutine) then
       InternalError('Routine is a type',ACall.Routine);
    {$ENDIF}

    tmpHook:=TRoutine(tmpRoutine).Hook;

    if Assigned(tmpHook) then
       R.ResultValue:=tmpHook(AInstance,R.ParamsInstance)
    else
    begin
      tmpInst:=R.ParamsInstance; //R.PrepareCall;
      tmpInst.ProfileParent:=R.ProfileIndex;

      tmpRoutine:=GetCallRoutine(nil,ACall,AInstance);

      R.ResultNode:=Execute(tmpInst,tmpRoutine);

      { NEVER CALLED:
      if AInstance=nil then
         R.EndCall(nil)
      else
      }
         R.EndCall(AInstance.Value);
    end;

    //R.EndProfile;

    AResult:=R.ResultValue;
//    DEBUGResult:=AResult;
  finally
    R.Free;
  end;
end;

function TRunner.CurrentNode:TNode;
begin
  result:=Current;

  if result is TRunningBlock then
     result:=TRunningBlock(result).CurrentNode
{  else
  if result is TReturn then
     result:=TReturn(result).Value
  else
  if result is TRunningDataCall then
     result:=TRunningDataCall(result).Call
  else
  if result is TRunningLoop then
     result:=TRunningLoop(result).TheFor;

  if result is TCallData then
     result:=TCallData(result).Value;
  }
end;

procedure TRunner.Step(const ANode:TNode);
begin
  Initiate_Run(True,ANode);
  Pause;
end;

procedure TRunner.Initiate_Run(const WithDebug:Boolean; const ANode:TNode);

  function InitRunning:TRunningNode;
  begin
    result:=RunningBlock(nil,nil,TNamedType(ANode));
    Root:=TRunningBlock(result).Local;
    Last:=result;
  end;

begin
  {$IFDEF INTERNAL}
  if CheckInstanceLeaks then
     Instances.CheckLeaks;

  TNode.Counter:=0;

  Instances.Reset;
  {$ENDIF}

  Status:=TRunning.Running;

  if WithDebug then
     Current:=InitRunning
  else
  begin
    Current:=ANode;
    Root:=TInstance.Create;
    Last:=nil;
  end;

  Stack.Clear;
  Stack.Push(ANode);

  BeginProfile(ANode as TType);

  DoMonitor;
end;

{ TRunningBlock }

// Returns True when N is not a variable, or it is a Variable and needs initialization
function ShouldRunNode(const N:TNode):Boolean;
begin
  if N is TVariable then
  begin
    result:=(not TVariable(N).Clauses.Shared) and
            (TVariable(N).ValueData<>nil)
  end
  else
    result:=True;
end;

class function TRunningBlock.NextNode(const AType:TType; const AIndex:Integer):TNode;
var tmp : Integer;
    N : TNode;
begin
  tmp:=AIndex;

  while tmp<=High(AType.Items) do
  begin
    N:=AType.Items[tmp];

    Inc(tmp);

    if N is TWith then
       TPlugin.TryHook(TWith(N))
    else
    if not (N is TType) then
       if ShouldRunNode(N) then
          Exit(N);
  end;

  result:=nil;
end;

// Returns next node to be executed, but it does not increase Index.
// Instead, it uses a "tmp" variable.
function TRunningBlock.Current: TNode;
begin
  result:=NextNode(TheType,Index);
end;

// Returns next item to execute, or Self is we're past the end of items (finished)
function TRunningBlock.CurrentNode: TNode;
begin
  result:=Current;

  if result=nil then
     result:=Self;
end;

destructor TRunningBlock.Destroy;
begin
  Local.Free;
  inherited;
end;

// Returns next item to execute, and increases Index
function TRunningBlock.Next:TNode;
var N : TNode;
begin
  while Index<=High(TheType.Items) do
  begin
    N:=TheType.Items[Index];

    Inc(Index);

    if N is TWith then
       TPlugin.TryHook(TWith(N))
    else
    if not (N is TType) then
    begin
      if (N is TVariable) and (not TVariable(N).Clauses.Shared) then
         Instance.AddData(TVariable(N));

      if ShouldRunNode(N) then
         Exit(N); //Runner.ExecuteNext(Instance,N));
    end;
  end;

  result:=nil;
end;

{ TRunningArrayExpression }

function TRunningArrayExpression.CanDestroy(const ANode: TNode): Boolean;
begin
  result:=ANode<>Evaluator;
end;

destructor TRunningArrayExpression.Destroy;
begin
  ItemsInstance.Free;
  inherited;
end;

procedure TRunningArrayExpression.SetResult(const AData:TData);
begin
  {$IFDEF INTERNAL}
  if AData=nil then
     InternalError('Runningarray result is nil',AData);

  if Value.Parameters[Item-Start]=AData then
     InternalError('Runningarray result is already set',AData);
  {$ENDIF}

  Value.Parameters[Item-Start]:=AData;

  // Avoid memory leak of AData
  if AData.Owner=nil then
//     if not (AData is TArrayExpression) then // <--??
        AData.Owner:=Value;
end;

function TRunningArrayExpression.Next:TNode;

  procedure DoSetResult(const AData:TData);
  var tmp : TInstance;
  begin
    if AData is TVariable then
    begin
      tmp:=Evaluator.ValueInstance;

      if tmp=nil then
         tmp:=Evaluator.Instance;

      SetResult(DirectValueOf(tmp,TVariable(AData))); // <-- ValueOf necessary ??
    end
    else
      SetResult(AData);
  end;

  procedure EndEvaluator;
  begin
    DoSetResult(Evaluator.Value);
    Runner.Last:=Caller;
  end;

  function ItemEvaluator(const AData:TData):TNode;
  begin
    Evaluator:=RunningExpression(Self,ItemsInstance,AData);
    Evaluator.Runner:=Runner;

    Runner.Last:=Evaluator;

    result:=Evaluator.Next;

    if result=nil then
       EndEvaluator;
  end;

  function NextItem:TNode;
  begin
    if ItemsInstance=nil then
    begin
      ItemsInstance:=TInstance.Create;
      ItemsInstance.Owner:=Instance;
    end;

    while Item<=High(Items) do
    begin
      result:=ItemEvaluator(Items[Item] as TData);

      if result<>nil then
         Exit;

      Evaluator.Free;

      Inc(Item);
    end;

    result:=nil;
  end;

begin
  if Evaluator<>nil then
  begin
    EndEvaluator;

    Evaluator.Free;
    Evaluator:=nil;

    if Item>=High(Items) then
       result:=nil
    else
    begin
      Inc(Item);
      result:=NextItem;
    end;
  end
  else
    result:=NextItem;
end;

{ TRunningDataCall }

procedure TRunningDataCall.BeginProfile;
begin
  if Profile.Enabled then
  begin
    ProfileIndex:=Profile.DoEnter(Call.Routine);
    StartTime:=TStopWatch.StartNew;
  end;
end;

Destructor TRunningDataCall.Destroy;
begin
  EndProfile;

  if Runner<>nil then
     Runner.Stack.Pop;

  ParamsInstance.Free;

  ParamArrays.Free(nil);

  inherited;
end;

procedure TRunningDataCall.EndCall(const ASelf:TData);
begin
  if ResultNode<>nil then
  begin
    if ResultNode is TReturn then   // ??
       ResultValue:=TReturn(ResultNode).Value
    else
       ResultValue:=ResultNode as TData;

    if ResultValue is TSelf then
    begin
      {$IFDEF INTERNAL}
      if ASelf=nil then
         InternalError('Cannot return Self',Call);
      {$ENDIF}

      ResultValue:=ASelf;
    end
    else
    if ResultValue<>nil then
       if ResultValue<>Instance then
          if ResultValue is TInstance then // <-- Return Records
             TInstance(ResultValue).Owner:=nil
          else
          if ResultValue.Owner is TInstance then
             ResultValue:=TCloner.Data(nil,ResultValue);  // <-- Do not reuse. Clone it
  end;
end;

procedure TRunningDataCall.EndProfile;
var tmp : Integer;
begin
  if Profile.Enabled then
  begin
    {$IFDEF INTERNAL}
    if Instance=nil then
    begin
      tmp:=-1;
      InternalError('Instance is nil at End Profile',nil);
    end
    else
    {$ENDIF}
       tmp:=Instance.ProfileParent;

    Profile.DoExit(tmp,ProfileIndex,StartTime.ElapsedMilliseconds);
  end;
end;

procedure TRunningDataCall.InitParams(const AddParams:Boolean);

  function IsLastManyValues(const AParams:TNodes):Boolean;
  begin
    result:=(AParams<>nil) and TChecker.IsManyValues(AParams[High(AParams)]);
  end;

var tmpParams : TNodes;
    tmpLastIsMany : Boolean;
    tmpRoutine : TType;
begin
  if Call is TVariableCall then // before is TDataCall
     tmpRoutine:=GetVariableCallRoutine(TVariableCall(Call),Instance)
  else
     tmpRoutine:=GetCallRoutine(Call);

  {$IFDEF INTERNAL}
  if not (tmpRoutine is TParametersType) then
  begin
    tmpParams:=nil; // TODO: other call types

    InternalError('Internal, InitParams not supported: ',tmpRoutine);
  end
  else
  {$ENDIF}
     tmpParams:=TParametersType(tmpRoutine).Parameters;

  if AddParams then
     ParamsInstance:=TInstance.From(tmpParams)
  else
     ParamsInstance:=TInstance.Create;

  ParamsInstance.Owner:=Instance;

  tmpLastIsMany:=IsLastManyValues(tmpParams);

  {$IFDEF INTERNAL}
  if Call.Parameters.Count<>tmpParams.Count then
     if not tmpLastIsMany then
        Raise_Exception('Internal: Wrong call parameters: '+
               Call.Parameters.Count.ToString+' vs '+tmpParams.Count.ToString);
  {$ENDIF}

  // Special case for Console.Putline without parameters:
  if Call.Parameters=nil then
     if tmpLastIsMany then
        ParamsInstance.AddData(tmpParams[0] as TVariable);
end;

function RunningArrayExpression(const ACaller:TRunningNode;
                                const AInstance: TInstance;
                                const AArray: TArrayExpression): TRunningArrayExpression;
begin
  result:=TRunningArrayExpression.Create {$IFDEF RUNSTACK}(ACaller){$ENDIF};
  result.Caller:=ACaller;

  result.Instance:=AInstance;
  result.Value:=AArray;
end;

function TRunningDataCall.Next:TNode;

  function EvaluateParam(const AData:TData):TNode;
  begin
    Evaluator:=RunningExpression(Self,ParamsInstance,AData);
    Evaluator.Runner:=Runner;

    Runner.Last:=Evaluator;

    result:=Evaluator.Next;
  end;

  function EvaluateArrayOfParams(const ACount,AStart:Integer; out AValue:TData):TNode;
  var ArrayExpression : TArrayExpression;
  begin
    ArrayExpression:=CreateArrayExpression(ACount);
    ParamArrays.Add(ArrayExpression);

    AValue:=ArrayExpression;

    ArrayEvaluator:=RunningArrayExpression(Self,Instance,ArrayExpression);
    ArrayEvaluator.Runner:=Runner;
    ArrayEvaluator.Items:=Call.Parameters;
    ArrayEvaluator.Start:=AStart;
    ArrayEvaluator.Item:=AStart;

    Runner.Last:=ArrayEvaluator;
    result:=ArrayEvaluator.Next;

    if result=nil then
    begin
      ArrayEvaluator.Free;
      ArrayEvaluator:=nil;
    end;
  end;

  function GetManyParameters(const AStart:Integer; out AValue:TData):TNode;
  var L, tmpCount : Integer;
  begin
    L:=Length(Call.Parameters);

    tmpCount:=L-AStart;

    if tmpCount=1 then
    begin
      result:=EvaluateParam(Call.Parameters[AStart] as TData);

      if result=nil then
      begin
        AValue:=Evaluator.Value;
        Runner.Last:=Caller;
        Evaluator.Free;
      end;
      // else !!!  wait for Evaluator to finish !
    end
    else
      result:=EvaluateArrayOfParams(tmpCount,AStart,AValue);
  end;

  procedure SetParameter(const AIndex:Integer; const AVariable,AData:TData);

    function IsFunctionType:Boolean;
    begin
      // passing a function as a parameter !
      result:=(AVariable is TTypeCall) and (AData=AVariable);
    end;

  var tmpValue : TInstance;
      tmpVariable : TVariable;
      tmpRoutine,
      tmpType : TType;
  begin
    if Call is TVariableCall then
       tmpRoutine:=GetVariableCallRoutine(TVariableCall(Call),ParamsInstance)
    else
       tmpRoutine:=GetCallRoutine(Call);

    {$IFDEF INTERNAL}
    if not (tmpRoutine is TParametersType) then
    begin
      tmpVariable:=nil;

      InternalError('Internal error, Call.Routine is not ParametersType',Call.Routine);
    end
    else
    {$ENDIF}
      tmpVariable:=TParametersType(tmpRoutine).Parameters[AIndex] as TVariable;

    ParamsInstance.AddData(tmpVariable);

    {$IFDEF INTERNAL}
    if High(ParamsInstance.Data)<>AIndex then
       InternalError('Length of ParamsInstance Data is not AIndex+1',Call.Routine);

    ParamsInstance.Data[AIndex].Owner:=ParamsInstance;
    {$ENDIF}

    if AData is TInstance then
       tmpValue:=TInstance(AData)  // <-- Intrinsic demo, fails !!!  Self
    else
    if TChecker.IsEnumerationItem(AData) or IsFunctionType then
    begin
      tmpValue:=TInstance.Create;
      tmpValue.Owner:=ParamsInstance;
      tmpValue.Value:=AData;
    end
    else
    if IsReference(AData) then
    begin
      tmpValue:=TEvaluate.InstanceOf(Instance,AData as TVariable);

      {$IFDEF INTERNAL}
      if tmpValue=nil then
         InternalError('Reference to variable not found',AData)
      else
      {$ENDIF}

      // WORKAROUND !!! See calls\override, setting Value as AData so FindOverriden might
      // get the real variable that belongs to tmpValue instance
      if tmpValue.Value=nil then
         tmpValue.Value:=AData;
    end
    else
    begin
      tmpType:=TChecker.GetDataType(nil,AVariable);
      tmpValue:=FromValue({Instance} ParamsInstance,tmpType,AData);
    end;

    ParamsInstance.Data[AIndex].Value:=tmpValue;
  end;

  procedure FinishCurrentParameter(const AData:TData);
  begin
    SetParameter(Parameter,AData,AData);
  end;

  function ParamEvaluator(const AIndex:Integer):TNode;
  var tmpParam : TNode;
      tmp : TData;
      tmpParams : TNodes;
      tmpRoutine : TType;
  begin
    tmpRoutine:=GetCallRoutine(Call);

    {$IFDEF INTERNAL}
    if not (tmpRoutine is TParametersType) then
       InternalError('Internal error, no call parameters',Call.Routine)
    else
    {$ENDIF}
       tmpParams:=TParametersType(tmpRoutine).Parameters;

    tmpParam:=tmpParams[AIndex];

    IsManyParams:=TChecker.IsManyValues(tmpParam);

    if IsManyParams then // ...
    begin
      result:=GetManyParameters(AIndex,tmp);

      if result=nil then
      begin
        SetParameter(AIndex,tmpParam as TData,tmp);
        Parameter:=High(Call.Parameters);
      end;
    end
    else
    begin
      result:=EvaluateParam(Call.Parameters[AIndex] as TData);

      if result=nil then
      begin
        tmp:=Evaluator.Value;
        SetParameter(AIndex,tmp,tmp);

        Runner.Last:=Caller;
        Evaluator.Free;
      end;
    end;

    //result:=nil; // <-- pending
  end;

  procedure NextParameter;
  begin
    if IsManyParams then
       ParametersDone:=True
    else
    begin
      Inc(Parameter);

      if Parameter>=Length(Call.Parameters) then
         ParametersDone:=True;
    end;
  end;

  function RunTheRoutine:TNode;
  var tmpBlock : TRunningBlock;
      tmpRoutine : TType;
  begin
    tmpRoutine:=GetCallRoutine(Runner.Finder,Call,Instance);
    tmpBlock:=RunningBlock(Self,LocalInstance,tmpRoutine);
    tmpBlock.Runner:=Runner;

    Runner.Last:=tmpBlock;
    result:=tmpBlock.Next;

    if result=nil then
    begin
      Runner.Last:=Caller;

      tmpBlock.Free;
    end;
  end;

var tmpHook : TPluginHook;
    tmpRoutine : TType;
begin
  if ParametersDone then
  begin
    if LocalInstance=nil then
    begin
      Runner.Stack.Push(Call);

      BeginProfile;

      LocalInstance:=ParamsInstance;

      tmpRoutine:=GetCallRoutine(Call);

      {$IFDEF INTERNAL}
      if not (tmpRoutine is TRoutine) then
         InternalError('Call.Routine is not a routine',Call.Routine);
      {$ENDIF}

      tmpHook:=(tmpRoutine as TRoutine).Hook;

      if Assigned(tmpHook) then
         ResultValue:=tmpHook(Instance,ParamsInstance)
      else
      begin
        LocalInstance.ProfileParent:=ProfileIndex;

        result:=RunTheRoutine;

        Exit;
      end;
    end
    else
      EndCall(Instance{.Value});

    EndProfile;

    result:=nil;
  end
  else
  if ArrayEvaluator<>nil then
  begin
    FinishCurrentParameter(ArrayEvaluator.Value);

    NextParameter;

    ArrayEvaluator:=nil;

    Runner.Last:=Caller;
    result:=Self;
  end
  else
  if Evaluator<>nil then
  begin
    {$IFDEF INTERNAL}
    if Evaluator.Value=nil then
       InternalError('Parameter evaluator is nil',Evaluator);
    {$ENDIF}

    FinishCurrentParameter(Evaluator.Value);

    NextParameter;

    Evaluator:=nil;

    Runner.Last:=Caller;
    result:=Self;
  end
  else
  begin
    if ParamsInstance=nil then
       InitParams(False);

    while Parameter<=High(Call.Parameters) do
    begin
      result:=ParamEvaluator(Parameter);

      if result<>nil then
         Exit;

      Inc(Parameter);
    end;

    ParametersDone:=True;

    result:=Next;
  end;
end;

{ TRunningMember }

type
  TRunningMember=class(TRunningItem)
  private
    Left : TNode;
    Member : TMember;

    RightRunning : Boolean;

    LeftRunning,
    Right : TRunningExpression;

    ReturnVariable : Boolean;
  public
    function CanDestroy(const ANode:TNode):Boolean; override;
    function Next:TNode; override;
  end;

// Temporary solution !!
function TRunningMember.CanDestroy(const ANode:TNode):Boolean;
begin
  result:=(ANode<>Right) and (ANode<>LeftRunning);
end;

function TRunningMember.Next:TNode;

  procedure TrySetCallerExpression(const AInstance:TInstance; const AData:TData);
  begin
    if Caller is TRunningExpression then
    begin
      TRunningExpression(Caller).Value:=AData;
      TRunningExpression(Caller).ValueInstance:=AInstance;
    end
    else
    if (AData<>nil) and (AData.Owner=nil) then
       AData.Free;

    Runner.Last:=Caller;
  end;

  procedure FinishRight(const AInstance:TInstance);
  begin
    TrySetCallerExpression(AInstance,Right.Value);

    Right.Free;
    Right:=nil;
  end;

  // UGLY !!!!
  // Search Member in AParent chain a.b.c....
  function InParent(const AParent:TNode):Boolean;
  var tmp : TNode;
  begin
    tmp:=AParent;

    repeat
      if tmp=Member then
         Exit(True)
      else
      if tmp is TMember then
         tmp:=TMember(tmp).Member
      else
         break;
    until False;

    result:=False;
  end;

  // z.call := y
  // a.b.call := x
  // Returns true if "call" member belongs to left-side of assignment
  function InAssignmentVariable:Boolean;
  begin
    // Not the good way to do this !!!
    result:=(Member.Owner is TAssignment) and
            InParent(TAssignment(Member.Owner).Variable);
  end;

  function GetRight(const ACaller:TRunningNode; const AInstance:TInstance):TNode;
  begin
    // T.Farenheit:=99   (property setter)
    if (Member.Member is TDataCall) and InAssignmentVariable then
    begin
      {$IFDEF INTERNAL}
      if not (Caller is TRunningExpression) then
        InternalError('DataCall in member not supported',Member)
      else
      {$ENDIF}
      begin
        TRunningExpression(Caller).Value:=Member.Member;
        TRunningExpression(Caller).ValueInstance:=AInstance;
      end;

      Runner.Last:=Caller;

      result:=nil;
    end
    else
    begin
      if Member.Member is TTypeCall then
      begin
        TrySetCallerExpression(AInstance,Member.Member);
        result:=nil;
      end
      else
      begin
        Right:=RunningExpression(ACaller,AInstance,Member.Member);
        Right.ReturnVariable:=ReturnVariable;
        Right.Runner:=Runner;

        Runner.Last:=Right;
        result:=Right.Next;

        if result=nil then
           FinishRight(AInstance);
      end;
    end;
  end;

  // 42.AsText
  // (5*4).AsText  etc
  function GetRightOfData(const ACaller:TRunningNode; const AData:TData):TNode;
  var tmp : TInstance;
  begin
    if AData is TTypeCall then
    begin
      tmp:=Instance.TypeInstanceOf(Instance,TTypeCall(AData).TheType);
      result:=GetRight(ACaller,tmp);
    end
    else
    begin
      if AData is TInstance then
         result:=GetRight(ACaller,TInstance(AData))
      else
      if AData is TVariable then
         result:=GetRight(ACaller,TEvaluate.InstanceOf(Instance,TVariable(AData)))
      else
      begin
        tmp:=TInstance.From(AData);  // tmp.Value <- AData
        tmp.Owner:=Instance;

        result:=GetRight(ACaller,tmp);

        if result=nil then
           tmp.Free
        {$IFDEF INTERNAL}
        else
           InternalError('Instance leak',AData);
        {$ENDIF}
      end;
    end;
  end;

  function LeftInstance:TInstance;
  begin
    if Left is TTypeCall then
       result:=Instance.TypeInstanceOf(Instance,TTypeCall(Left).TheType)
    else
    if Left is TVariable then
       result:=TEvaluate.InstanceOf(Instance,TVariable(Left))
    else
    begin
      result:=nil;

      // NO ERROR should happen !!
//      Error('Error, Left node unknown',Left);
    end;
  end;

  function GetLeftRunning(const AData:TData):TNode;
  begin
    LeftRunning:=RunningExpression(Self,Instance,AData);
    LeftRunning.Runner:=Runner;

    Runner.Last:=LeftRunning;
    result:=LeftRunning.Next;
  end;

  function ProcessLeft:TNode;
  begin
    Left:=Member.Data;

    if Left is TVariable then
       result:=GetRight(Self,LeftInstance)
    else
    if Left is TTypeCall then
       result:=GetRight(Self,Instance)
    else
    if Left is TData then
    begin
      result:=GetLeftRunning(TData(Left));

      if result=nil then
         result:=Next;
    end
    else
    begin
      result:=nil; // Pending !!

      {$IFDEF INTERNAL}
      InternalError('Cannot execute node',Left);
      {$ENDIF}
    end;
  end;

  function FinishLeft:TNode;
  begin
    if LeftRunning.Value=nil then
       result:=nil
    else
    begin
      result:=GetRightOfData(LeftRunning,LeftRunning.Value);

      if LeftRunning.Value.Owner=nil then
         LeftRunning.Value.Free;
    end;

    RightRunning:=result<>nil;

    if result=nil then
    begin
      if Runner.Last=LeftRunning then
         Runner.Last:=Caller;

      LeftRunning.Free; // ??
      LeftRunning:=nil;
    end;
  end;

begin
  if RightRunning then
  begin
    TrySetCallerExpression(LeftInstance,LeftRunning.Value);

    LeftRunning.Free;
    LeftRunning:=nil;

    result:=nil;
  end
  else
  if LeftRunning<>nil then
     result:=FinishLeft
  else
  if Left=nil then
     result:=ProcessLeft
  else
  if Right<>nil then
  begin
    FinishRight(LeftInstance);
    result:=nil;
  end
  else
    result:=nil;
end;

function RunningDataCall(const ACaller:TRunningNode;
                         const AInstance:TInstance;
                         const ACall:TDataCall):TRunningDataCall;
begin
  result:=TRunningDataCall.Create  {$IFDEF RUNSTACK}(ACaller {Last}){$ENDIF};
  result.Caller:=ACaller;
  result.Instance:=AInstance;
  result.Call:=ACall;
end;

{ TRunningAssignment }

type
  TRunningAssignment=class(TRunningItem)
  private
    Assignment : TAssignment;

    ParamToRemove : TNode;

    RunningCall : TRunningDataCall;

    Evaluator,
    EvaluatorTarget : TRunningExpression;

    Target,
    Evaluated : TData;

    TargetInstance : TInstance;
  public
    function CanDestroy(const ANode:TNode):Boolean; override; // <-- Try to remove this !!!
    function Next:TNode; override;
  end;

function TRunningAssignment.CanDestroy(const ANode:TNode):Boolean;
begin
  result:=ANode<>Evaluator;
end;

function TRunningAssignment.Next:TNode;

  procedure GetTarget;
  begin
    Target:=EvaluatorTarget.Value;
    TargetInstance:=EvaluatorTarget.ValueInstance;

    if (Target is TVariable) and TVariable(Target).Clauses.Shared then
       TargetInstance:=SharedInstanceOf(TargetInstance,Target);

    Runner.Last:=Caller;

    EvaluatorTarget.Free;
    EvaluatorTarget:=nil;
  end;

  procedure GetValue;
  begin
    Evaluated:=ArithmeticAssign(Assignment);
    Evaluator:=RunningExpression(Self,Instance,Evaluated);
    Evaluator.Runner:=Runner;

    Runner.Last:=Evaluator;
  end;

  procedure RemoveCallParameter;
  begin
    // Find a better way and remove this code, without using "Parameters" at all:
    if Target is TDataCall then
       if TDataCall(Target).Parameters.Count=1 then
          if TDataCall(Target).Parameters[0]=ParamToRemove {Evaluated} then
             TDataCall(Target).Parameters:=nil; // reset back property setter. UGLY !!!
  end;

  function GetCall(const AParam:TNode):TNode;
  begin
    {$IFDEF INTERNAL}
    if TDataCall(Target).Parameters.Count<>0 then
       InternalError('Property setting DataCall assignment has parameters',Target);
    {$ENDIF}

    ParamToRemove:=AParam;

    TDataCall(Target).Parameters:=[AParam]; // UGLY !!!

    RunningCall:=RunningDataCall(Self,TargetInstance,TDataCall(Target));

    RunningCall.Runner:=Runner;

    Runner.Last:=RunningCall;

    result:=RunningCall.Next;

    if result=nil then
    begin
      RemoveCallParameter;

      Runner.Last:=Self;
      RunningCall.Free;
      RunningCall:=nil;
    end;
  end;

  function InstanceOfVariable(const AVariable:TVariable):TInstance;
  begin
    if AVariable.Clauses.Shared then  // enumeration item, etc
    begin
      if TChecker.IsEnumerationItem(AVariable) then
      begin
        result:=TEvaluate.InstanceOf(TargetInstance,Target);
        result.Value:=AVariable;
      end
      else
        result:=SharedInstanceOf(Instance,AVariable);
    end
    else
    begin
      result:=TEvaluate.InstanceOf(Instance,AVariable); // Get Reference instance

      { NEVER CALLED:
      if AVariable.Owner=nil then
         AVariable.Owner:=result;
      }

      result.Owner:=TargetInstance;
    end;
  end;

  function AssignValue:TNode;
  var tmp : TData;
      tmpInst : TInstance;
  begin
    if Evaluated<>Assignment.Value then
       Evaluated.Free;

    if Target is TDataCall then
    begin
      Runner.Last:=Caller;

      tmp:=Evaluator.Value;

      Evaluator.Free;
      Evaluator:=nil;

      result:=GetCall(tmp);
    end
    else
    begin
      // TODO:  Mod division  Do this here?? Maybe better a new streamer AST token: ModDiv
      (*
      if (Evaluator.Value is TFloat) and TChecker.IsInteger(Assignment.Variable) then
      begin
        tmp:=TInteger.Create(Trunc(TFloat(Evaluator.Value).Value));
        Instance.Assign(Assignment.Variable,TInstance.From(tmp));
      end
      else
      *)
      if TargetInstance=nil then
         TargetInstance:=Instance;

      if Evaluator.Value is TVariable then
         tmpInst:=InstanceOfVariable(TVariable(Evaluator.Value))
      else
      begin
        {$IFDEF INTERNAL}
        if Evaluator.Value=nil then
           InternalError('Evaluator Value is nil at Assign Value',Assignment);
        {$ENDIF}

        if Evaluator.Value=nil then
           tmpInst:=nil
        else
        begin
          Evaluator.Value:=TryRoundFloat(Target,Evaluator.Value);

          if (Evaluator.Value.Owner=nil) or (not (Evaluator.Value.Owner is TInstance)) then
          begin
            tmpInst:=TInstance.From(Evaluator.Value); // Create new instance

            if Evaluator.Value.Owner=nil then
               Evaluator.Value.Owner:=tmpInst;
          end
          else
          begin
            tmpInst:=TInstance.Create;
            tmpInst.Value:=TCloner.Data(tmpInst,Evaluator.Value);
          end;

          tmpInst.Owner:=TargetInstance;
        end;
      end;

      {$IFDEF INTERNAL}
      if tmpInst=nil then
         InternalError('Instance is nil at Assign Value',Assignment);
      {$ENDIF}

      Instance_Assign(TargetInstance,Target,tmpInst);

      Runner.Last:=Caller;

      Evaluator.Free;
      Evaluator:=nil;

      if Target.Owner=nil then // <-- ie: Target is TArrayExpression
         Target.Free;

      result:=nil;
    end;
  end;

begin
  if RunningCall<>nil then
  begin
    RemoveCallParameter;
    RunningCall:=nil;
    result:=nil;
  end
  else
  if Target<>nil then
  begin
    if Evaluator=nil then
    begin
      GetValue;

      result:=Evaluator.Next;

      if result=nil then
         result:=AssignValue;
    end
    else
      result:=AssignValue;
  end
  else
  if EvaluatorTarget=nil then
  begin
    if Assignment.Variable is TVariable then // direct variable X:=1
    begin
      Target:=Assignment.Variable;
      TargetInstance:=Instance;

      Runner.Last:=Caller;
      result:=Next;
    end
    else
    begin
      // (Some.Expression.That.Returns.A.Variable) := 1
      EvaluatorTarget:=RunningExpression(Self,Instance,Assignment.Variable);
      EvaluatorTarget.Runner:=Runner;

      EvaluatorTarget.ReturnVariable:=True;

      Runner.Last:=EvaluatorTarget;

      result:=EvaluatorTarget.Next;

      if result=nil then
      begin
        GetTarget;
        result:=Next;
      end;
    end;
  end
  else
    result:=nil; // <-- Never called
end;

{ TRunningTry }

type
  TRunningTry=class(TRunningItem)
  private
    TryNode : TTry;

    DoingFinally,
    Started : Boolean;
  public
    function Next:TNode; override;
  end;

function TRunningTry.Next:TNode;
begin
  if DoingFinally then
     result:=nil
  else
  if Started then
  begin
    result:=TryNode.TheFinally;
    DoingFinally:=result<>nil;
  end
  else
  begin
    Started:=True;
    Runner.Last:=Self;
    result:=TryNode.Block;
  end;
end;

{ TRunningCondition }

type
  TRunningCondition=class(TRunningExpression)
  private
    Evaluator,
    Side : TRunningExpression;

    //Done : Boolean;

    Condition : TCondition;
  public
    function CanDestroy(const ANode:TNode):Boolean; override;
    function Next:TNode; override;
  end;

function TRunningCondition.CanDestroy(const ANode:TNode):Boolean;
begin
  result:=ANode<>Side;
end;

function TRunningCondition.Next:TNode;

  function GetEvaluator:TRunningExpression;
  begin
    result:=RunningExpression(Self,Instance,Condition.Condition);
    result.Runner:=Runner;

    Runner.Last:=result;
  end;

  function GetEvaluatorOf(const AData:TData):TRunningExpression;
  begin
    result:=RunningExpression(Self,Instance,AData);
    result.Runner:=Runner;

    Runner.Last:=result;
  end;

var tmp : TData;
begin
  if Side<>nil then //Done then
  begin
    Value:=Side.Value;

    Side.Free;
    Side:=nil;

    result:=nil;
  end
  else
  if Evaluator=nil then
  begin
    Evaluator:=GetEvaluator;

    result:=Evaluator.Next;

    if result=nil then
       result:=Next;
  end
  else
  if Side=nil then
  begin
    tmp:=Evaluator.Value;

    Evaluator.Free;
    Evaluator:=nil;

    if CalcAsBoolean(tmp,Instance) then
       Side:=GetEvaluatorOf(Condition.Left)
    else
       Side:=GetEvaluatorOf(Condition.Right);

    result:=Side.Next;

    if result=nil then
    begin
      //Done:=True;
      result:=Next;
    end;
  end
  else
  begin
    //Done:=True;
    result:=nil;

    {$IFDEF INTERNAL}
    InternalError('Internal, Condition returns nil',Self);
    {$ENDIF}
  end;
end;

{ TRunningOperand }

type
  TRunningOperand=class(TRunningExpression)
  private
    LeftValue : TData;

    LeftRunning,
    RightRunning : TRunningExpression;

    procedure SetRightValue;
  public
    function CanDestroy(const ANode:TNode):Boolean; override;
    function Next:TNode; override;
    function Operand:TOperand;
  end;

function TRunningOperand.Operand:TOperand;
begin
  {$IFDEF INTERNAL}
  if not (Expression is TOperand) then
     InternalError('Expression is not TOperand: ',Expression);
  {$ENDIF}

  result:=TOperand(Expression);
end;

// Temporary solution !!
function TRunningOperand.CanDestroy(const ANode:TNode):Boolean;
begin
  result:=ANode<>LeftRunning;
end;

procedure TRunningOperand.SetRightValue;
var RightValue : TData;

  procedure EvaluateInteger;
  var L,R : Int64;
  begin
    R:=(RightValue as TInteger).Value;

    if (Operand is TDivision) and (R=0) then
       Runner.RuntimeException(TRunExceptionCode.DivisionByZero,TErrors._DivisionByZero)
    else
    begin
      L:=(LeftValue as TInteger).Value;
      Value:=FromInteger(Operand,L,R);
    end;
  end;

  function GetAsFloat(const AData:TData):Extended;
  begin
    if AData is TInteger then
       result:=TInteger(AData).Value
    else
       result:=(AData as TFloat).Value;
  end;

  procedure EvaluateFloat;
  var R : Extended;
  begin
    R:=GetAsFloat(RightValue);

    if (Operand is TDivision) and (R=0) then
       Runner.RuntimeException(TRunExceptionCode.DivisionByZero,TErrors._DivisionByZero)
    else
       Value:=FromFloat(Operand,GetAsFloat(LeftValue),R);
  end;

begin
  RightValue:=RightRunning.Value;

  //if RightValue is TVariable then
  //   RightValue:=ValueOf(Instance,TVariable(RightValue));

  if (LeftValue is TArrayExpression) and (RightValue is TArrayExpression) then
     Value:=ArrayOperand(Operand,Instance,TArrayExpression(LeftValue),TArrayExpression(RightValue))
  else
  if (LeftValue is TArrayExpression) and (Operand is TAddition) then
     Value:=ConcatenateArrays(TArrayExpression(LeftValue),RightValue,Instance)
  else
  if (Operand is TContains) and (RightValue is TArrayExpression) then
     Value:=TBoolean.Create(ArrayContains(Instance,LeftValue,TArrayExpression(RightValue)))
  else
  if TChecker.IsLogical(LeftValue) then
     Value:=FromBoolean(Operand,
                        (LeftValue as TBoolean).Value,
                        (RightValue as TBoolean).Value)
  else
  if TChecker.IsInteger(LeftValue) and TChecker.IsInteger(RightValue) then
      EvaluateInteger
  else
  if TChecker.IsEnumerationItem(LeftValue) then  // (L.Owner=R.Owner) and IsEnumerationItem(L) and IsEnumerationItem(R) then
     Value:=EnumerationBoolean(Operand,LeftValue,RightValue)
  else
  if TChecker.IsText(LeftValue) then
     Value:=FromText(Operand,
                    Runner_DataAsText(LeftValue),
                    Runner_DataAsText(RightValue))
  else
  if TChecker.IsFloat(LeftValue) or TChecker.IsFloat(RightValue) then // Promote to Float
     EvaluateFloat
//  else
//  if IsReference(LeftValue) and IsReference(RightValue) then <-- NIL ??
//     Value:=FromReferences
  {$IFDEF INTERNAL}
  else
     InternalError('Operand Expression not yet supported',Operand)
  {$ENDIF}
  ;

  Runner.Last:=Self;

  if LeftValue.Owner=nil then
     LeftValue.Free;

  if RightValue.Owner=nil then
     RightValue.Free;
end;

function TRunningOperand.Next:TNode;

  function CreateSide(const AExpression:TData):TRunningExpression;
  begin
    result:=RunningExpression(Self,Instance,AExpression);
    result.Runner:=Runner;

    Runner.Last:=result;
  end;

  function GetLeft:TNode;
  begin
    LeftValue:=LeftRunning.Value;

    LeftRunning.Free;
    LeftRunning:=nil;

    {$IFDEF INTERNAL}
    if LeftValue=nil then
    begin
      result:=nil;
      InternalError('RunningOperand GetLeft, LeftValue is nil',Operand);
    end
    else
    {$ENDIF}
    begin
      RightRunning:=CreateSide(Operand.Right);
      result:=RightRunning.Next;

      if result=nil then
      begin
        if RightRunning.Value<>nil then
           SetRightValue;

        RightRunning.Free;
        RightRunning:=nil;
      end;
    end;
 end;

begin
  if LeftRunning<>nil then
     result:=GetLeft
  else
  if RightRunning=nil then
  begin
    LeftRunning:=CreateSide(Operand.Left);
    result:=LeftRunning.Next;

    if result=nil then
       result:=GetLeft;
  end
  else
  begin
    if RightRunning.Value<>nil then
       SetRightValue;

    RightRunning:=nil;

    result:=nil;
  end;
end;

function RunningMember(const ACaller:TRunningNode;
                 const AInstance:TInstance;
                 const AMember:TMember):TRunningMember;
begin
  result:=TRunningMember.Create  {$IFDEF RUNSTACK}(ACaller {Last}){$ENDIF};
  result.Caller:=ACaller; {Last;}

  result.Instance:=AInstance;
  result.Member:=AMember;
end;

function RunningOperand(const ACaller:TRunningNode;
                                const AInstance: TInstance;
                                const AOperand: TOperand): TRunningOperand;
begin
  result:=TRunningOperand.Create {$IFDEF RUNSTACK}(ACaller){$ENDIF};
  result.Caller:=ACaller;

  result.Instance:=AInstance;
  result.Expression:=AOperand;
end;

function RunningCondition(const ACaller:TRunningNode;
                          const AInstance: TInstance;
                          const ACondition: TCondition): TRunningCondition;
begin
  result:=TRunningCondition.Create {$IFDEF RUNSTACK}(ACaller){$ENDIF};
  result.Caller:=ACaller;

  result.Instance:=AInstance;
  result.Condition:=ACondition;
end;

{ TRunningExpression }

function TRunningExpression.Next:TNode;

  function Negate(const AValue:TData):TData;
  begin
    if TChecker.IsInteger(AValue) then
    begin
      result:=TInteger.Create(-(AValue as TInteger).Value);

      if AValue.Owner=nil then
         AValue.Free;
    end
    else
    if TChecker.IsFloat(AValue) then
    begin
      result:=TFloat.Create(-(AValue as TFloat).Value);

      if AValue.Owner=nil then
         AValue.Free;
    end
    else
    begin
      result:=nil;

      {$IFDEF INTERNAL}
      InternalError('Internal, cannot negate UnarySign data',AValue);
      {$ENDIF}
    end;
  end;

  function NotValue(const AValue:TData):TData;
  begin
    if TChecker.IsLogical(AValue) then
    begin
      result:=TBoolean.Create(not TBoolean(AValue).Value);

      if AValue.Owner=nil then
         AValue.Free;
    end
    else
    if TChecker.IsInteger(AValue) then
    begin
      result:=TInteger.Create(not TInteger(AValue).Value);

      if AValue.Owner=nil then
         AValue.Free;
    end
    else
    begin
      result:=nil;

      {$IFDEF INTERNAL}
      InternalError('Internal, Cannot apply "not" for data',AValue);
      {$ENDIF}
    end;
  end;

  function AsArrayExpression:TData;
  var tmp : TNode;
      tmpOwner : TArrayExpression;
      tmpData : TData;
  begin
    result:=TRunningArrayExpression(Running).Value;

    if not ReturnVariable then
    begin
      tmp:=GetNodeOf(Runner,Instance,result as TArrayExpression,tmpOwner);

      if tmp=nil then
         result:=nil
      else
         result:=tmp as TData;
    end;

    tmpData:=TRunningArrayExpression(Running).Value;

    if result<>tmpData then
       if tmpData.Owner=nil then
          tmpData.Free;
  end;

  procedure GetRunningResult;
  begin
    {$IFDEF INTERNAL}
    // Pending to fix everything !
    //if Value<>nil then
    //   Error('GetRunningResult, Value already set',Value);
    {$ENDIF}

    if Running is TRunningDataCall then
       Value:=TRunningDataCall(Running).ResultValue
    else
    if Running is TRunningArrayExpression then
       Value:=AsArrayExpression
    else
    if Running is TRunningExpression then
    begin
      Value:=TRunningExpression(Running).Value;

      if Expression is TUnarySign then
      begin
        if not TUnarySign(Expression).Positive then
           Value:=Negate(Value);
      end
      else
      if Expression is TUnaryNot then
         Value:=NotValue(Value);
    end
    else
    if Running is TRunningMember then
    {$IFDEF INTERNAL}
    else
       InternalError('Internal error, not supported running expression: ',Running);
    {$ENDIF}
  end;

  procedure FreeRunning;
  begin
    Runner.Last:=Self;
    Running.Free;
    Running:=nil;
    RunningDone:=True;
  end;

  function GetRunning(const ARunning:TRunningItem):TNode;
  begin
    Running:=ARunning;

    Runner.Last:=Running;

    result:=Running.Next;

    if result=nil then
    begin
      GetRunningResult;
      FreeRunning;
    end;
  end;

  function CreateRunningCall(const ACall:TDataCall):TNode;
  begin
    Running:=RunningDataCall(Self,Instance,ACall);

    Running.Runner:=Runner;
    Runner.Last:=Running;

    result:=Running.Next;

    if result=nil then
    begin
      GetRunningResult;
      FreeRunning;
    end;
  end;

  function CreateRunningMember(const AMember:TMember):TNode;
  begin
    Running:=RunningMember(Self,Instance,AMember);
    TRunningMember(Running).ReturnVariable:=ReturnVariable;
    Running.Runner:=Runner;

    Runner.Last:=Running;

    result:=Running.Next;

    if result=nil then
    begin
      // no?? GetRunningResult;
      FreeRunning;
    end;
  end;

  function CreateRunningArrayExpression(const AArray:TArrayExpression):TNode;
  var ArrayExpression : TArrayExpression;
  begin
    ArrayExpression:=CreateArrayExpression(Length(AArray.Parameters));

    ArrayExpression.Data:=AArray.Data;

    Running:=RunningArrayExpression(Self,Instance,ArrayExpression);
    Running.Runner:=Runner;
    TRunningArrayExpression(Running).Items:=AArray.Parameters;

    Runner.Last:=Running;

    result:=Running.Next;

    if result=nil then
    begin
      GetRunningResult;
      FreeRunning;
    end;
  end;

  function GetSelfInstance(const ASelf:TSelf):TData;
  var tmpType : TType;

    function IsOwnedByClass(const AInstance:TInstance):TInstance;
    var D : TInstance.TVariableValue;
    begin
      if AInstance<>nil then
      begin
        for D in AInstance.Data do
          if D.Variable.VariableType=tmpType then
          begin
            result:=D.Value;
            Exit;
          end;
      end;

      result:=nil;
    end;

    function FindRunningBlock:TInstance;
    var tmp : TRunningNode;
    begin
      tmp:=Caller;

      while tmp<>nil do
      begin
        if tmp is TRunningBlock then
           if TRunningBlock(tmp).TheType=tmpType then
              Exit(tmp.Instance);

        tmp:=tmp.Caller;
      end;

      result:=nil;

      {$IFDEF INTERNAL}
      InternalError('Cannot obtain Self instance',ASelf);
      {$ENDIF}
    end;

  var tmp : TRunningNode;
  begin
    tmpType:=ASelf.TheType;

    if tmpType=nil then
    begin
      tmpType:=TFinder.FindParentTypeOf(ASelf.Owner);
      Exit(FindRunningBlock);
    end;

    tmp:=Caller;

    while tmp<>nil do
    begin
      result:=IsOwnedByClass(tmp.Instance.Owner as TInstance);

      if result<>nil then
         Exit
      else
        tmp:=tmp.Caller;
    end;

    result:=nil;

    {$IFDEF INTERNAL}
    InternalError('Cannot obtain Self instance',ASelf);
    {$ENDIF}
  end;

  procedure ObtainValue;
  begin
    if Expression is TSelf then
       Value:=GetSelfInstance(TSelf(Expression)) // <-- Duck typing
    else
    begin
      if Expression is TVariable then
      begin
        if ReturnVariable then
           Value:=Expression
        else
        begin
          Value:=DirectValueOf(Instance,TVariable(Expression));

          // special case "for in in Data"  when Data is an empty many-values ...
          if Value=nil then
             if TVariable(Expression).VariableType is TManyValues then
                Value:=TVariable(Expression);
        end;
      end
      else
      {
      if Expression is TCastingData then
         Value:=TCastingData(Expression).Data  // <-- NEVER CALLED
      else
      }
         Value:=Expression;

      if (Value=nil) or (Value is TInstance) then
      begin
        if IsReference(Expression) then
           Value:=Expression

        {$IFDEF INTERNAL}
        else
           InternalError('Expression not yet supported',Expression);
        {$ENDIF}
      end;
    end;
  end;

var tmp : TRunningItem;
begin
  if RunningDone then
     result:=nil
  else
  if Running<>nil then
  begin
    GetRunningResult;

    // special case, see "Stack overflow.vidi" test: Test.Sub.Lee
    if Caller is TRunningExpression then
       TRunningExpression(Caller).Value:=Value;

    RunningDone:=True;
    Running:=nil; //<-- !! ????

    result:=nil;
  end
  else
  if Expression is TDataCall then
     if ReturnFunctionType then
     begin
       Value:=Expression;
       result:=nil;
     end
     else
        result:=CreateRunningCall(TDataCall(Expression))
  else
  if Expression is TAncestor then
     result:=CreateRunningCall(TAncestor(Expression).DataCall)
  else
  if Expression is TOperand then
  begin
    if Expression is TCondition then
       tmp:=RunningCondition(Self,Instance,TCondition(Expression))
    else
       tmp:=RunningOperand(Self,Instance,TOperand(Expression));

    tmp.Runner:=Runner;
    result:=GetRunning(tmp);
  end
  else
  if Expression is TGroup then
  begin
    tmp:=RunningExpression(Self,Instance,TGroup(Expression).Expression);
    TRunningExpression(tmp).ReturnVariable:=ReturnVariable;
    tmp.Runner:=Runner;

    result:=GetRunning(tmp);
  end
  else
  if Expression is TMember then
     result:=CreateRunningMember(TMember(Expression))
  else
  if Expression is TArrayExpression then
     result:=CreateRunningArrayExpression(TArrayExpression(Expression))
  else
  begin
    ObtainValue;

    result:=nil;

    if Profile.Coverage.Enabled then
       Profile.Coverage.Add(Expression);
  end;
end;

{ TRunningFor }

type
  TRunningFor=class(TRunningItem)
  private
    Evaluator : TRunningExpression;
    TheFor : TFor;
    DoneProcess : Boolean;
  public
    function CanDestroy(const ANode:TNode):Boolean; override;
    function Next:TNode; override;
  end;

function TRunningFor.CanDestroy(const ANode:TNode):Boolean;
begin
  result:=ANode<>Evaluator;
end;

function TRunningFor.Next:TNode;

  function CallerNode:TRunningNode;
  begin
    result:=Self;
  end;

  procedure PrepareRunning(const ARunning:TRunningItem);
  begin
    ARunning.Runner:=Runner;
    ARunning.Instance:=Instance;
    ARunning.Caller:=CallerNode; //Runner.Last;
  end;

  procedure PrepareForLoop(const AFor:TFor; const ALoop:TRunningLoop);
  begin
    PrepareRunning(ALoop);
    ALoop.TheFor:=AFor;
  end;

  function RunningTextLoop(const AFor:TFor; const S:String):TRunningTextLoop;
  begin
    result:=TRunningTextLoop.Create {$IFDEF RUNSTACK}(CallerNode){$ENDIF};
    PrepareForLoop(AFor,result);
    result.S:=S;
    result.Prepare;
  end;

  function RunningArrayLoop(const AFor:TFor; const AArray:TArrayExpression):TRunningArrayLoop;
  begin
    result:=TRunningArrayLoop.Create {$IFDEF RUNSTACK}(CallerNode){$ENDIF};
    PrepareForLoop(AFor,result);
    result.AArray:=AArray;
    result.Prepare;
  end;

  function RunningNodesLoop(const AFor:TFor; const AItems:TNodes):TRunningNodesLoop;
  begin
    result:=TRunningNodesLoop.Create {$IFDEF RUNSTACK}(CallerNode){$ENDIF};
    PrepareForLoop(AFor,result);
    result.Items:=AItems;
    result.Prepare;
  end;

  function RunningNumberLoop(const AFor:TFor; const AExpression:TData):TRunningNumberLoop;
  begin
    result:=TRunningNumberLoop.Create {$IFDEF RUNSTACK}(CallerNode){$ENDIF};
    PrepareForLoop(AFor,result);
    result.Expression:=AExpression;
    result.Prepare;
  end;

  function DoProcess(const AValue:TData):TNode;

    function TryArrayLoop(const AArray:TArrayExpression):TNode;
    begin
      if AArray=nil then
         result:=nil
      else
         result:=RunningArrayLoop(TheFor,AArray);
    end;

  begin
    DoneProcess:=True;

    Runner.Last:=Self;

    Evaluator.Free;
    Evaluator:=nil;    // <-- here at the end

    if AValue is TRange then
       result:=RunningNumberLoop(TheFor,AValue)
    else
    if AValue is TVariable then
       result:=TryArrayLoop(ArrayExpressionOf(Instance,TVariable(AValue)))
    else
    if AValue is TText then
       result:=RunningTextLoop(TheFor,TText(AValue).Value)
    else
    if AValue is TArrayExpression then
       result:=RunningArrayLoop(TheFor,TArrayExpression(AValue))
    else
    if AValue is TTypeCall then // enumeration: for Color in Colors ...
       result:=RunningNodesLoop(TheFor,TTypeCall(AValue).TheType.Items)
    else
    begin
      result:=nil;

      {$IFDEF INTERNAL}
      InternalError('Cannot run for expression:',TheFor.InExpression);
      {$ENDIF}
    end;
  end;

  function TryDoProcess:TNode;
  var tmp : TData;
  begin
    tmp:=Evaluator.Value;

    if tmp is TInstance then
       tmp:=TInstance(tmp).Value;

    result:=DoProcess(tmp);
  end;

begin
  if DoneProcess then
     result:=nil
  else
  if Evaluator=nil then
  begin
    if TheFor.InExpression=nil then
    begin
      result:=RunningNumberLoop(TheFor,nil);
      DoneProcess:=True;
    end
    else
    begin
      Evaluator:=RunningExpression(Self,Instance,TheFor.InExpression);
      Evaluator.Runner:=Runner;

      Runner.Last:=Evaluator;

      result:=Evaluator.Next;

      if result=nil then
         result:=TryDoProcess;
    end;
  end
  else
    result:=TryDoProcess;
end;

{ TRunningReturn }

type
  TRunningReturn=class(TRunningItem)
  private
    Evaluator : TRunningExpression;
    Return : TReturn;
  public
    function Next:TNode; override;
  end;

function TRunningReturn.Next:TNode;

  function GetCall:TRunningDataCall;
  var tmp : TRunningNode;
  begin
    tmp:=Caller;

    while tmp<>nil do
    begin
      if tmp is TRunningDataCall then
         Exit(TRunningDataCall(tmp))
      else
         tmp:=tmp.Caller;
    end;

    result:=nil;

    {$IFDEF INTERNAL}
    InternalError('Cannot get call for Return',Caller);
    {$ENDIF}
  end;

  procedure SetCallResult;
  var tmpCall : TRunningDataCall;
      tmp : TNode;
  begin
    tmpCall:=GetCall;

    {$IFDEF INTERNAL}
    if tmpCall=nil then
       InternalError('Call not found to set result',Self)
    else
    {$ENDIF}
    begin
      tmp:=Evaluator.Value;

      {$IFDEF INTERNAL}
      if tmp=nil then
         InternalError('Evaluator return value is nil',Self)
      else
      {$ENDIF}
      if tmp is TInstance then
         tmpCall.ResultNode:=tmp
      else
      if tmp.Owner=Return then
         tmpCall.ResultNode:=Return
      else
      if tmp.Owner=nil then
         tmpCall.ResultNode:=tmp
      else
      if IsReference(tmp) then // IsVariableOfClass(tmp) then
      begin
        tmpCall.ResultNode:=TEvaluate.InstanceOf(Instance,TVariable(tmp)); // <--- forced casting TVariable

        if tmpCall.ResultNode=nil then
           if TChecker.VariableIsEnumItem(TVariable(tmp)) then
              tmpCall.ResultNode:=tmp;
      end
      else
         tmpCall.ResultNode:=TCloner.Data(nil,tmp as TData);
    end;

    Runner.Last:=Caller; // Self;
  end;

begin
  if Evaluator=nil then
  begin
    Evaluator:=RunningExpression(Self,Instance,Return.Value);
    Evaluator.Runner:=Runner;

    Runner.Last:=Evaluator;
    result:=Evaluator.Next;

    if result=nil then
    begin
      SetCallResult;

      Evaluator.Free;
      Evaluator:=nil;
    end;
  end
  else
  begin
    SetCallResult;
    result:=nil;
  end;
end;

{ TRunner }

{$IFDEF INTERNAL}
var
  RunnerOld:TNode=nil;
{$ENDIF}

function TRunner.ExecuteNext(const AInstance:TInstance; const ANode:TNode):TNode;

  procedure PrepareRunning(const ARunning:TRunningItem);
  begin
    ARunning.Runner:=Self;
    ARunning.Instance:=AInstance;
    ARunning.Caller:=Last;
  end;

  function RunningAssignment:TRunningAssignment;
  begin
    result:=TRunningAssignment.Create {$IFDEF RUNSTACK}(Last){$ENDIF};
    PrepareRunning(result);
    result.Assignment:=TAssignment(ANode);
  end;

  function RunningTry:TRunningTry;
  begin
    result:=TRunningTry.Create {$IFDEF RUNSTACK}(Last){$ENDIF};
    PrepareRunning(result);
    result.TryNode:=TTry(ANode);
  end;

  function RunningVariableInit:TRunningVariableInit;
  begin
    result:=TRunningVariableInit.Create {$IFDEF RUNSTACK}(Last){$ENDIF};
    PrepareRunning(result);
    result.Variable:=TVariable(ANode);
  end;

  function ExecuteNextType(const AType:TType):TNode;
  begin
    Last:=RunningBlock(Last,AInstance,AType);
    (Last as TRunningItem).Runner:=Self;
    result:=ExecuteNext(AInstance,Last);
  end;

  function RunningFor:TRunningFor;
  begin
    result:=TRunningFor.Create {$IFDEF RUNSTACK}(Last){$ENDIF};
    PrepareRunning(result);
    result.TheFor:=TFor(ANode);
  end;

  function RunningIf:TRunningIf;
  begin
    result:=TRunningIf.Create {$IFDEF RUNSTACK}(Last){$ENDIF};
    PrepareRunning(result);
    result.DoCondition:=True;
    result.TheIf:=TIf(ANode);
  end;

  function RunningRepeat:TRunningRepeat;
  begin
    result:=TRunningRepeat.Create {$IFDEF RUNSTACK}(Last){$ENDIF};
    PrepareRunning(result);
    result.TheRepeat:=TRepeat(ANode);
  end;

  function RunningReturn:TRunningReturn;
  begin
    result:=TRunningReturn.Create {$IFDEF RUNSTACK}(Last){$ENDIF};
    PrepareRunning(result);
    result.Return:=TReturn(ANode);
  end;

  function RunningWhile:TRunningWhile;
  begin
    result:=TRunningWhile.Create {$IFDEF RUNSTACK}(Last){$ENDIF};
    PrepareRunning(result);
    result.TheWhile:=TWhile(ANode);
    result.DoCondition:=True;
  end;

  function RunningWhen:TRunningWhen;
  begin
    result:=TRunningWhen.Create {$IFDEF RUNSTACK}(Last){$ENDIF};
    PrepareRunning(result);
    result.When:=TWhen(ANode);
  end;

  // TODO: Substitute this GetBreakReturn with JumpToLoop(True)
  function GetBreakReturn:TNode;
  var tmp : TRunningNode;
  begin
    result:=ANode;

    while not ((result is TRunningRepeat) or (result is TRunningWhile) or (result is TRunningLoop)) do
    begin
      {$IFDEF INTERNAL}
      if not (result is TRunningNode) then
      begin
        result:=nil;
        InternalError('Error',nil);
        Exit;
      end
      else
      {$ENDIF}
      begin
        tmp:=TRunningNode(result).Caller;
        result.Free;
        result:=tmp;
      end;
    end;

    tmp:=TRunningNode(result).Caller;
    result.Free;
    result:=tmp;
  end;

  {$IFDEF RUNSTACK}
  procedure CheckRunStackDestroy(const ANode:TNode);

    procedure DoError(const N:TRunningNode; const ATitle:String);
    begin
      // Should not be an error
      //InternalError('RunningNode destroy contained by: '+ATitle,N)
    end;

  var N : TRunningNode;
      t : Integer;
  begin
    for t:=High(TRunningNode.RunStack)-1 downto 0 do
    begin
      N:=TRunningNode.RunStack[t];

      if N is TRunningExpression then
      begin
        if TRunningExpression(N).Running=ANode then
        begin
          DoError(N,'RunningExpression.Running');
          break;
        end;
      end
      else
        ///if N is xxxx then ...  <-- add other TRunningXX classes
    end;
  end;
  {$ENDIF}

  procedure DoDestroy(const ANode:TNode);
  begin
    if ANode=Last then
       Last:=TRunningNode(ANode).Caller; // Last:=result ?

    {$IFDEF RUNSTACK}
    //CheckRunStackDestroy(ANode);
    {$ENDIF}

    ANode.Free;
  end;

  function Rollback(const ANode:TNode; const CallNext:Boolean):TNode;
  var tmpCanDestroy : Boolean;

      {$IFDEF INTERNAL}
      tmpClass : String;
      ANodeDestroyed : Boolean;
      {$ENDIF}

      Old,
      tmpNext : TNode;
  begin
    result:=ANode;

    if ANode is TRunningBlock then
       Stack.Pop;

    {$IFDEF INTERNAL}
    ANodeDestroyed:=False;
    {$ENDIF}

    repeat
      if RunException.Code<>TRunExceptionCode.None then
         break;

      Old:=result;

      {$IFDEF INTERNAL}
      RunnerOld:=Old;

      if not (result is TRunningNode) then
      begin
        if result=nil then
           tmpClass:='nil'
        else
           tmpClass:=result.ClassName;

        InternalError('Not a running node: '+tmpClass,result);
      end;
      {$ENDIF}

      if TRunningNode(result).Caller=nil then
      begin
        if result=Last then
           Last:=nil;

        {$IFDEF INTERNAL}
        ANodeDestroyed:=result=Old;
        {$ENDIF}

        result.Free;

        result:=nil;
        break;
      end
      else
         result:=TRunningNode(result).Caller;

      {$IFDEF INTERNAL}
      if not ANodeDestroyed then
         if Old=ANode then
            ANodeDestroyed:=True;
      {$ENDIF}

      tmpCanDestroy:=TRunningNode(result).CanDestroy(Old);

      if CallNext then
         tmpNext:=TRunningNode(result).Next
      else
         tmpNext:=result; // <-- JumpToLoop needs this to escape here

      if tmpCanDestroy then
         DoDestroy(Old);

      if tmpNext<>nil then
      begin
        result:=tmpNext;
        break;
      end;

      {$IFDEF INTERNAL}
      //DoMonitor; // <-- extreme costly calling DoMonitor here
      {$ENDIF}

    until False;

    {$IFDEF INTERNAL}
    if result=ANode then
    begin
      result:=nil;
      Error('RollBack result equals to parameter ANode',ANode);
    end;

    if not ANodeDestroyed then
    begin
      Error('RollBack ANode is not destroyed',ANode);
      DoDestroy(ANode);
    end;
    {$ENDIF}
  end;

  // Rollback stack until method or top-level block is found
  function ExitMethod:TRunningNode;

    function IsRoutineOrTopBlock(const AType:TType):Boolean;
    begin
      result:=(AType is TRoutine) or
              ((AType.Owner is TBlockStatement) and
               (AType.Owner.Owner is TNamedType) and
               (AType.Owner.Owner.Owner=nil)
               );
    end;

  var tmp : Boolean;
  begin
    result:=Last;

    repeat
      tmp:=(result is TRunningBlock) and IsRoutineOrTopBlock(TRunningBlock(result).TheType);
      result:=Rollback(result,False) as TRunningNode;

    until tmp;
  end;

  // Rollback stack until For, While or Repeat is found
  function JumpToLoop(const IsBreak:Boolean):TRunningNode;
  //var //tmpCaller : TRunningNode;
  //    tmp : TNode;
  var tmp : Boolean;
  begin
    result:=Last;

    if IsBreak then
    begin
      repeat
        tmp:=(result is TRunningLoop) or (result is TRunningLoopCondition);
        result:=Rollback(result,False) as TRunningNode;
      until tmp;
    end
    else
    begin
      repeat
        if (result is TRunningLoop) or (result is TRunningLoopCondition) then
           Exit
        else
           result:=Rollback(result,False) as TRunningNode;

      until result=nil;

      {$IFDEF INTERNAL}
      InternalError('Cannot rollback to loop',nil);
      {$ENDIF}
    end;
  end;

  function ExecuteCall(const ACall:TDataCall):TNode;
  var tmp : TRunningItem;
  begin
    tmp:=RunningDataCall(Last,AInstance,ACall);
    tmp.Runner:=Self;
    result:=ExecuteNext(AInstance,tmp);
  end;

  function ExecuteMember(const AMember:TMember):TNode;
  var tmp : TRunningItem;
  begin
    tmp:=RunningMember(Last,AInstance,AMember);
    tmp.Runner:=Self;
    result:=ExecuteNext(AInstance,tmp);
  end;

begin
  result:=nil;

  {$IFDEF INTERNAL}
  if ExtraMonitorRefresh then
     DoMonitor;

  if ANode=nil then
     InternalError('Node is nil at ExecuteNext',ANode)
  else
  {$ENDIF}
  if ANode is TRunningNode then
  begin
    result:=TRunningNode(ANode).Next;

    if result is TBreak then
       result:=GetBreakReturn
    else
    if result=nil then
       result:=Rollback(ANode,True);
  end
  else
  if ANode is TBlockStatement then
     result:=ExecuteNextType(TBlockStatement(ANode).Block)
  else
  if (ANode is TExtender) and (TExtender(ANode).Extension is TRoutine) then
     result:=ExecuteNextType(TExtender(ANode).Extension as TRoutine)
  else
  { NEVER CALLED:
  if ANode is TRoutine then
     result:=ExecuteNextType(TRoutine(ANode))
  else
  }
  if ANode is TFor then
     result:=ExecuteNext(AInstance,RunningFor)
  else
  if ANode is TIf then
     result:=ExecuteNext(AInstance,RunningIf)
  else
  if ANode is TWhile then
     result:=ExecuteNext(AInstance,RunningWhile)
  else
  if ANode is TRepeat then
     result:=ExecuteNext(AInstance,RunningRepeat)
  else
  if ANode is TWhen then
     result:=ExecuteNext(AInstance,RunningWhen)
  else
  if ANode is TCallData then
     result:=ExecuteNext(AInstance,TCallData(ANode).Value)
  else
  if ANode is TDataCall then
     result:=ExecuteCall(TDataCall(ANode))
  else
  { NEVER CALLED:
  if ANode is TGroup then
     result:=ExecuteNext(AInstance,TGroup(ANode).Expression)
  else
  }
  if ANode is TMember then
     result:=ExecuteMember(TMember(ANode))
  else
  if ANode is TAssignment then
     result:=ExecuteNext(AInstance,RunningAssignment)
  else
  if ANode is TAncestor then
     result:=ExecuteNext(AInstance,TAncestor(ANode).DataCall)
  else
  if ANode is TBreak then
     result:=JumpToLoop(True)
  else
  if ANode is TContinue then
     result:=JumpToLoop(False)
  else
  if ANode is TTry then
     result:=ExecuteNext(AInstance,RunningTry)
  else
  if ANode is TVariable then
  begin
    { NEVER CALLED:
    if TVariable(ANode).ValueData=nil then
       result:=nil
    else
    }
       result:=ExecuteNext(AInstance,RunningVariableInit);
  end
  else
  if ANode is TReturn then
  begin
    if TReturn(ANode).Value=nil then
       result:=ExitMethod
    else
       result:=ExecuteNext(AInstance,RunningReturn);
  end
  {$IFDEF INTERNAL}
  else
    InternalError('Cannot execute node',ANode)
  {$ENDIF}
  ;

  if result is TRunningNode then
     Last:=TRunningNode(result);
end;

procedure TRunner.Step(const IsStepOver:Boolean);

  function NextNode:TNode;
  begin
    if Last is TRunningBlock then
    begin
      result:=TRunningBlock(Last).Current;

      if result=nil then
         result:=TRunningBlock(Last).TheType;
    end
    else
    begin
      result:=Current;

      if result is TBlockStatement then
         result:=TRunningBlock.NextNode(TBlockStatement(result).Block,0);
    end;
  end;

var tmp : TInstance;
begin
  if IsStepOver then
  begin
    OneTimeBreak:=NextNode;
    Status:=TRunning.Running;
    Loop;
  end
  else
  begin
    if Last=nil then
       tmp:=Root
    else
       tmp:=Last.Instance;

    Current:=ExecuteNext(tmp,Current);

    if Current=nil then
       Stop
    else
       Pause;
  end;
end;

procedure TRunner.BeginProfile(const AType:TType);
begin
  if Profile.Enabled then
  begin
    ProfileIndex:=Profile.DoEnter(AType);
    StartTime:=TStopWatch.StartNew;
  end;
end;

procedure TRunner.EndProfile;
begin
  if Profile.Enabled then
     Profile.DoExit(-1,ProfileIndex,StartTime.ElapsedMilliseconds);
end;

procedure TRunner.Stop;

  procedure CleanCallers;
  var tmp : TNode;
      tmpCaller : TRunningNode;
  begin
    tmp:=Last {Current};

    while tmp<>nil do
    begin
      if tmp is TRunningNode then
      begin
        tmpCaller:=TRunningNode(tmp).Caller;

        tmp.Free;
        tmp:=tmpCaller;
      end
      else
         break;
    end;
  end;

  procedure CleanUpVariables;
  begin
    CleanCallers;

    TInstance.FreeTypes;

    // Do empty Instances ?

    {$IFDEF RUNSTACK}
    FreeRunStack; // Empty RunStack
    {$ENDIF}

    //Release_Bug_Show:=True;

//    Root.Free;
    Root:=nil;

    Last:=nil;
    LastBreak:=nil;

    Stack.Clear;
  end;

begin
  OneTimeBreak:=nil;

  EndProfile;

  CleanUpVariables;

  Status:=TRunning.Stopped;

  RunException.Code:=TRunExceptionCode.None;
  RunException.Text:='';
//  RunException.Node:=nil;

  DoMonitor;

  {$IFDEF INTERNAL}
  if CheckInstanceLeaks then
     Instances.CheckLeaks;

  CheckNodeCounter;
  {$ENDIF}
end;

class procedure TRunner.ClearHooks;
begin
  TSysPlugin.ClearHooks;
end;

{ TEvaluateCompileTime }

// TODO: Implement this fully (AsData... AsInteger AsText AsFloat, etc)
// Accept calls to type-level shared evaluable routines, at compile-time too.
class function TEvaluateCompileTime.AsBoolean(const AData: TData;
                                              out AValue: Boolean): Boolean;
begin
  result:=AData is TBoolean;

  if result then
     AValue:=TBoolean(AData).Value
  else
  begin
    // ALWAYS FALSE:
    result:=(AData is TVariable) and (TVariable(AData).ValueData<>nil);

    if result then
       // NEVER CALLED:
       result:=AsBoolean(TVariable(AData).ValueData,AValue);
  end;
end;

{$IFDEF INTERNAL}
/// Debugging purposes, for control+f7 evaluation at debug time, stopped
function R(const ANode:TRunningNode):String;

  function Add(const ANode:TNode):String;
  begin
    if ANode=nil then
       result:=': ?'
    else
       result:=': '+B(ANode)
  end;

begin
  if ANode=nil then
     result:='nil?'
  else
  begin
    result:=ANode.ClassName;

    if ANode is TRunningMember then
       result:=result+Add(TRunningMember(ANode).Member)
    else
    if ANode is TRunningExpression then
       result:=result+Add(TRunningExpression(ANode).Expression)
    else
    if ANode is TRunningDataCall then
       result:=result+Add(TRunningDataCall(ANode).Call)
    else
    if ANode is TRunningOperand then
       result:=result+Add(TRunningOperand(ANode).Operand)
    else
    if ANode is TRunningCondition then
       result:=result+Add(TRunningCondition(ANode).Condition)
    else
    if ANode is TRunningArrayExpression then
       result:=result+Add(TRunningArrayExpression(ANode).Value)
    else
    if ANode is TRunningAssignment then
       result:=result+Add(TRunningAssignment(ANode).Assignment)
    else
    if ANode is TRunningReturn then
       result:=result+Add(TRunningReturn(ANode).Return)

    // TRunningFor
    // TRunningIf
    // TRunningLoop
    // TRunningWhile
    // TRunningWhen
    // TRunningRepeat
  end;
end;

initialization
  R(nil);
finalization
{$ENDIF}
end.
