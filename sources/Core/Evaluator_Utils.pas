unit Evaluator_Utils;

interface

uses
  Sys, AST, Instance_Type;

function GetArrayData(const AInstance:TInstance):TArrayExpression;
function GetArrayCount(const AInstance:TInstance):TData;
function GetArrayLow(const AInstance:TInstance):TData;
function GetArrayHigh(const AInstance:TInstance):TData;

implementation

uses
  {$IFDEF INTERNAL}
  Internal,
  {$ENDIF}
  Evaluator;

function GetArrayData(const AInstance:TInstance):TArrayExpression;
var tmpInst : TInstance;
    tmpData : TData;
begin
  tmpInst:=AInstance;

  while tmpInst.Value=nil do
        if tmpInst.Owner is TInstance then
           tmpInst:=TInstance(tmpInst.Owner)  // <-- correct?
        else
           break;

  tmpData:=TEvaluate.AsData(tmpInst.Value,AInstance);

  if tmpData is TArrayExpression then
     result:=TArrayExpression(tmpData)
  else
  begin
    {$IFDEF INTERNAL}
    InternalError('Instance value is not Array Expression',tmpData);
    {$ENDIF}

    result:=nil;
  end;
end;

function GetArrayCount(const AInstance:TInstance):TData;
var tmpData : TArrayExpression;
begin
  {$IFDEF INTERNAL}
  if AInstance=nil then
  begin
    result:=nil;
    InternalError('GetArrayCount of nil instance',AInstance);
    Exit;
  end;
  {$ENDIF}

  tmpData:=GetArrayData(AInstance);

  {$IFDEF INTERNAL}
  if tmpData=nil then
  begin
    result:=nil;
    InternalError('GetArrayCount of nil Data',AInstance);
    Exit;
  end;
  {$ENDIF}

  result:=TInteger.Create(Length(tmpData.Parameters));
end;

procedure GetArrayRange(const AInstance:TInstance; out AMin,AMax:Integer);
var tmpData : TArrayExpression;
begin
  tmpData:=GetArrayData(AInstance);

  {$IFDEF INTERNAL}
  if tmpData=nil then
  begin
    AMin:=0;
    AMax:=0;
    InternalError('GetArrayRange of nil data',AInstance);
    Exit;
  end;
  {$ENDIF}

  TEvaluate.ArrayRange(tmpData,AInstance,tmpData.Parameters,0,AMin,AMax);
end;

function GetArrayLow(const AInstance:TInstance):TData;
var AMin, AMax : Integer;
begin
  GetArrayRange(AInstance,AMin,AMax);
  result:=TInteger.Create(AMin);
end;

function GetArrayHigh(const AInstance:TInstance):TData;
var AMin, AMax : Integer;
begin
  GetArrayRange(AInstance,AMin,AMax);
  result:=TInteger.Create(AMax);
end;

end.
