unit Sys_Plugin;

interface

uses
  Sys, Plugin, AST, Instance_Type;

type
  TOnGet=function:String of object;

  TSysPlugin=class(TPlugin)
  protected
    procedure DoHook; override;
  public
    class var
      OnGet : TOnGet;
      TestMode : Boolean;

    class procedure ClearHooks; static;
  end;

implementation

uses
  {$IFDEF INTERNAL}
  Internal,
  {$ENDIF}
  Evaluator,
  Evaluator_Utils, Cloner, Checker.AST, Syntax, Magic, Utils.AST;

function GetFloat(const Instance:TNode):Float;
begin
  result:=(TInstance(Instance).Value as TFloat).Value; // TEvaluate.AsFloat(Instance.Value,Instance);
end;

function GetInteger(const Instance:TNode):Int64;
begin
  result:=(TInstance(Instance).Value as TInteger).Value;
end;

  procedure CallOnPut(const AData:TInstance.TInstanceData);
  var tmpData : TData;

      {$IFDEF INTERNAL}
      //tmpMax : Integer;
      {$ENDIF}

      tmp : String;
  begin
    {$IFDEF INTERNAL}
    {
    tmpMax:=High(AData);

    if tmpMax>0 then
    begin
      InternalError('ConsolePut more than one data param',nil);
      Exit;
    end;
    }
    {$ENDIF}

    if AData[0].Value<>nil then
    begin
      tmpData:=AData[0].Value.Value;

      tmp:=Runner_DataAsText(tmpData);

      TRunner.OnPut(tmp);
    end;
  end;

  function ParamData(const Parameters:TNode; const AIndex:Integer):TData;
  begin
    result:=TInstance(Parameters).Data[AIndex].Value.Value;
  end;

  function ParamAsInteger(const Parameters:TNode; const AIndex:Integer):Int64;
  begin
    result:=(ParamData(Parameters,AIndex) as TInteger).Value;
  end;

  procedure FreeItems(const AArray:TArrayExpression; const AStart:Integer);
  var t : Integer;
      tmp : TNodes;
  begin
    tmp:=AArray.Parameters;

    for t:=AStart to High(tmp) do
        if tmp[t]<>nil then
           if tmp[t].Owner=AArray then
              tmp[t].Free;
  end;

  function SetArrayCount(const Instance,Parameters:TNode):TData;
  var tmp : Integer;
      tmpArray : TArrayExpression;
  begin
    {$IFDEF INTERNAL}
    if TInstance(Instance).Value=nil then
       InternalError('SetArrayCount, array is nil',Instance);
    {$ENDIF}

    // See CallSetter: tinstance(instance.owner).value as TArrayExpression !!

    tmp:=ParamAsInteger(Parameters,0);
    tmpArray:=(TInstance(Instance).Value as TArrayExpression);

    FreeItems(tmpArray,tmp);  // alternative: ? tmpArray.Parameters.Free(tmpArray,tmp)

    SetLength(tmpArray.Parameters,tmp);
    result:=nil;
  end;

  procedure SwapArray(const Instance,Parameters:TNode);
  var tmpA,
      tmpB : Integer;
      tmpNodes : TNodes;
      tmp : TNode;
  begin
    {$IFDEF INTERNAL}
    if TInstance(Instance).Value=nil then
       InternalError('SwapArray, array is nil',Instance);
    {$ENDIF}

    tmpA:=ParamAsInteger(Parameters,0);
    tmpB:=ParamAsInteger(Parameters,1);

    tmpNodes:=(TInstance(Instance).Value as TArrayExpression).Parameters;

    tmp:=tmpNodes[tmpA];
    tmpNodes[tmpA]:=tmpNodes[tmpB];
    tmpNodes[tmpB]:=tmp;
  end;

  procedure ClearArray(const Instance:TInstance);
  var tmp : TArrayExpression;
  begin
    tmp:=(Instance.Value as TArrayExpression);

    tmp.Parameters.Free(tmp);
    tmp.Parameters:=nil;
  end;

function ArrayCopy(const Instance,Parameters:TNode):TData;
begin
  result:=TCloner.Node(nil,
            TEvaluate.AsData(GetArrayData(TInstance(Instance)),TInstance(Instance))) as TArrayExpression
end;

function ArrayGetCount(const Instance,Parameters:TNode):TData;
begin
  result:=GetArrayCount(TInstance(Instance));
end;

function ArraySetCount(const Instance,Parameters:TNode):TData;
begin
  SetArrayCount(Instance,Parameters);
  result:=nil;
end;

function ArrayAppend(const Instance,Parameters:TNode):TData;
begin
  (TInstance(Instance).Value as TArrayExpression).Parameters.Add(ParamData(Parameters,0));
  result:=nil;
end;

function ArrayClear(const Instance,Parameters:TNode):TData;
begin
  ClearArray(TInstance(Instance));
  result:=nil;
end;

function ArrayDelete(const Instance,Parameters:TNode):TData;
var tmp : Integer;
    tmpArray : TArrayExpression;
    tmpItem : TNode;
begin
  tmp:=ParamAsInteger(Parameters,0);
  tmpArray:=(TInstance(Instance).Value as TArrayExpression);

  tmpItem:=tmpArray.Parameters[tmp];

  if tmpItem<>nil then
           if tmpItem.Owner=tmpArray then
              tmpItem.Free;

  Delete(tmpArray.Parameters,tmp,1);

  result:=nil;
end;

function ArrayInsert(const Instance,Parameters:TNode):TData;
var tmp : Integer;
    tmpArray : TArrayExpression;
    tmpItem : TNode;
begin
  tmp:=ParamAsInteger(Parameters,0);
  tmpItem:=ParamData(Parameters,1);

  tmpArray:=(TInstance(Instance).Value as TArrayExpression);

  Insert(tmpItem,tmpArray.Parameters,tmp);

  result:=nil;
end;

function ArrayGetLow(const Instance,Parameters:TNode):TData;
begin
  result:=GetArrayLow(TInstance(Instance));
end;

function ArrayGetHigh(const Instance,Parameters:TNode):TData;
begin
  result:=GetArrayHigh(TInstance(Instance));
end;

function ArraySwap(const Instance,Parameters:TNode):TData;
begin
  SwapArray(Instance,Parameters);
  result:=nil;
end;

// Character.From(13)
function CharacterFrom(const Instance,Parameters:TNode):TData;
begin
  {$IFDEF INTERNAL}
  if TInstance(Parameters).Data { Call.Parameters}=nil then
  begin
    result:=nil;
    InternalError(TErrors._MissingParameters,nil)  // <-- necessary?
  end
  else
  {$ENDIF}
     result:=TText.Create(Chr(ParamAsInteger(Parameters,0) {(Call.Parameters[0] as TInteger).Value} ))
end;

// Console.Put(x)
function ConsolePut(const Instance,Parameters:TNode):TData;
begin
  CallOnPut(TInstance(Parameters).Data);
  result:=nil;
end;

// Console.Get
function ConsoleGet(const Instance,Parameters:TNode):TData;
var tmp : String;
begin
  if TSysPlugin.TestMode then
     tmp:='123'
  else
     tmp:=TSysPlugin.OnGet();

  result:=TText.Create(tmp);
end;

function ConsoleClear(const Instance,Parameters:TNode):TData;
begin
  TRunner.OnClear();
  result:=nil;
end;

// 4.AsText
function DataAsText(const Instance,Parameters:TNode):TData;
begin
  result:=TText.Create(Runner_DataAsText(TInstance(Instance).Value));
end;

// Future: Remove TextXXX intrinsic methods
function TextLength(const Instance,Parameters:TNode):TData;
begin
  result:=TInteger.Create(Length((TInstance(Instance).Value as TText).Value))
end;

function TextIndexOf(const Instance,Parameters:TNode):TData;
begin
  result:=TInteger.Create(Pos((ParamData(Parameters,0) {Call.Parameters[0]} as TText).Value,
                          (TInstance(Instance).Value as TText).Value))
end;

function NumberAbsolute(const Instance,Parameters:TNode):TData;
begin
  if TInstance(Instance).Value is TFloat then
     result:=TFloat.Create(Abs(GetFloat(Instance)))
  else
     result:=TInteger.Create(Abs(GetInteger(Instance)))
end;

function FloatTruncate(const Instance,Parameters:TNode):TData;
begin
  result:=TInteger.Create(Trunc(GetFloat(Instance)));
end;

function FloatRound(const Instance,Parameters:TNode):TData;
begin
  result:=TInteger.Create(Round(GetFloat(Instance)));
end;

function FloatFraction(const Instance,Parameters:TNode):TData;
begin
  result:=TFloat.Create(Frac(GetFloat(Instance)));
end;

function ShiftLeft(const Instance,Parameters:TNode):TData;
begin
  result:=TInteger.Create(ParamAsInteger(Parameters,0) shl ParamAsInteger(Parameters,1));
end;

function ShiftRight(const Instance,Parameters:TNode):TData;
begin
  result:=TInteger.Create(ParamAsInteger(Parameters,0) shr ParamAsInteger(Parameters,1));
end;

function MagicTypeOf(const Instance,Parameters:TNode):TData;
begin
  result:=TInstance(Instance).Value;
end;

function MagicNameOf(const Instance,Parameters:TNode):TData;
begin
  result:=TText.Create(TASTUtils.NodeName(TInstance(Instance).Value));
end;

function Exception_Raise(const Instance,Parameters:TNode):TData;
begin
  // TODO: Use "Parameters" for the exception !!
  EVidiRunException.RuntimeError('Custom Exception');
  result:=nil;
end;

procedure TSysPlugin.DoHook;
var Sys : TNamedType;
    tmp : TType;
    V,V2 : TVariable;
    tmpRoutine : TRoutine;

    _Integer : TClassType;
begin
  Sys:=Context;

  _Integer:=TChecker._Types[TChecker._Integer];

  tmpRoutine:=FindMethod({Sys} TChecker._Types[TChecker._Number],'AsText',TChecker._Types[TChecker._Text],nil);

  // Re-think: AsText should already not be a shared Number method in sys.vidi
  tmpRoutine.Clauses.Shared:=False;

  tmpRoutine.Hook:=DataAsText;

  // Console
  tmp:=Finder.FindType(Sys,'Console');

  // Console.Put
  V:=TVariable.Create;
  V.VariableType:=TChecker._Types[TChecker._Data];

  V2:=TVariable.Create;

  try
    FindMethod(tmp,'Put',nil,V).Hook:=ConsolePut;

    // Console.Get
    FindMethod(tmp,'Get',TChecker._Types[TChecker._Text],nil).Hook:=ConsoleGet;

    // Console.Clear
    FindMethod(tmp,'Clear').Hook:=ConsoleClear;

    // Array
    tmp:=Finder.FindType(Sys,'Array');

    // Array.Copy
    FindMethod(tmp,'Copy',TChecker._Types[TChecker._Array],nil).Hook:=ArrayCopy;
//      ArrayCopy.Clauses.Shared:=False;

    // Array.Count getter
    tmpRoutine:=FindMethod(tmp,'Count',_Integer);
    tmpRoutine.Clauses.Shared:=False;
    tmpRoutine.Hook:=ArrayGetCount;

    // Array.Count setter
    V.VariableType:=_Integer;

    tmpRoutine:=FindMethod(tmp,'Count',nil,V);
    tmpRoutine.Clauses.Shared:=False;
    tmpRoutine.Hook:=ArraySetCount;

    // Array.Low High getters
    tmpRoutine:=FindMethod(tmp,'Low',_Integer);
    tmpRoutine.Clauses.Shared:=False;
    tmpRoutine.Hook:=ArrayGetLow;

    tmpRoutine:=FindMethod(tmp,'High',_Integer);
    tmpRoutine.Clauses.Shared:=False;
    tmpRoutine.Hook:=ArrayGetHigh;

    // Array.Append
    V.VariableType:=TChecker._Types[TChecker._Type]; // Item:T

    FindMethod(tmp,'Append',nil,V).Hook:=ArrayAppend;

    // Array.Delete
    V.VariableType:=_Integer; // Index:Integer

    tmpRoutine:=FindMethod(tmp,'Delete',nil,V);
    tmpRoutine.Clauses.Shared:=False;
    tmpRoutine.Hook:=ArrayDelete;

    // Array.Insert

    V2.VariableType:=TChecker._Types[TChecker._Type]; // Index:Integer, Item:T

    tmpRoutine:=FindMethod(tmp,'Insert',nil,V,V2);
    tmpRoutine.Clauses.Shared:=False;
    tmpRoutine.Hook:=ArrayInsert;

    // Array.Clear
    tmpRoutine:=FindMethod(tmp,'Clear');
    tmpRoutine.Clauses.Shared:=False;
    tmpRoutine.Hook:=ArrayClear;

    // Array.Swap


    (* NOT YET READY !!!
    // Instance of self array type cannot be located (yet) inside ArraySwap

    V2.VariableType:=_Integer; // A,B:Integer
    ArraySwap:=FindMethod(tmp,'Swap',nil,V,V2);
    ArraySwap.Clauses.Shared:=False;
    *)

    // Float
    tmp:=Finder.FindType(Sys,'Float');

    tmpRoutine:=FindMethod(tmp,'Truncate',_Integer);
    tmpRoutine.Clauses.Shared:=False;
    tmpRoutine.Hook:=FloatTruncate;

    tmpRoutine:=FindMethod(tmp,'Round',_Integer);
    tmpRoutine.Clauses.Shared:=False;
    tmpRoutine.Hook:=FloatRound;

    tmpRoutine:=FindMethod(tmp,'Fraction',TChecker._Types[TChecker._Float]);
    tmpRoutine.Clauses.Shared:=False;
    tmpRoutine.Hook:=FloatFraction;

    // Number
    tmp:=Finder.FindType(Sys,'Number');

    tmpRoutine:=FindMethod(tmp,'Absolute',TChecker._Types[TChecker._Number]);
    tmpRoutine.Clauses.Shared:=False;
    tmpRoutine.Hook:=NumberAbsolute;

    // Character
    tmp:=Finder.FindType(Sys,'Character');

    V.VariableType:=_Integer;

    FindMethod(tmp,'From',TChecker._Types[TChecker._Character],V).Hook:=CharacterFrom;

    // Text
    tmp:=Finder.FindType(Sys,'Text');

    FindMethod(tmp,'Length',_Integer).Hook:=TextLength;

    V.VariableType:=TChecker._Types[TChecker._Text];
    FindMethod(tmp,'IndexOf',_Integer,V).Hook:=TextIndexOf;

    // BinaryShift
    tmp:=Finder.FindType(Sys,'BinaryShift');

    V.VariableType:=_Integer;
    V2.VariableType:=V.VariableType;

    FindMethod(tmp,'Left',_Integer,V,V2).Hook:=ShiftLeft;
    FindMethod(tmp,'Right',_Integer,V,V2).Hook:=ShiftRight;

    // Exception
    tmp:=Finder.FindType(Sys,'Exception');
    V.VariableType:=TChecker._Types[TChecker._SomeThing];
    FindMethod(tmp,'Raise',nil,V).Hook:=Exception_Raise;

  finally
    V2.Free;
    V.Free;
  end;
end;

var
  Plug : TSysPlugin;

class procedure TSysPlugin.ClearHooks;
begin
  //Plug.ConsolePut:=nil; // Flag to re-obtain hooks

  Plug.Context:=nil;
end;

initialization
  Plug:=TSysPlugin.Create;
  Plug.Module:='sys';

  TPlugin.Register(Plug);
finalization
  Plug.Free;
end.
