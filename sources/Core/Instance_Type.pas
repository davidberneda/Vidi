unit Instance_Type;

interface

uses
  Sys, AST;

type
  TInstance=class(TData)
  {$IFDEF INTERNAL}
  public
    ID: Integer;

    procedure MakeLastID;
  {$ELSE}
  private
  {$ENDIF}

    type
      // Global types (shared instance)
      TTypeValue=record
      public
        TheType : TType;
        Value : TInstance;
      end;

      TSharedInstances=Array of TTypeValue;

    class
      var Types : TSharedInstances;

    procedure AddShared(const ANodes:TNodes);
  public
    type
      TVariableValue=record
      private
        FValue : TInstance;

        {$IFDEF INTERNAL}
        procedure SetValue(const AInstance:TInstance);
        {$ENDIF}
      public
        {$IFDEF INTERNAL}
        Owner : TInstance;
        {$ENDIF}

        Variable : TVariable;

        procedure ReplaceValue(const AValue:TInstance);

        property Value:TInstance read FValue write {$IFDEF INTERNAL}SetValue{$ELSE}FValue{$ENDIF};
      end;

      TInstanceData=Array of TVariableValue;

      TDataHelper=record helper for TInstanceData
      public
        function Add(const AVariable: TVariable):Integer; overload;

        function Find(const AVariable:TVariable):Integer;
        procedure Remove(const AVariable: TVariable);
        function VariableOfValue(const AValue:TData):TVariable;
      end;

    var
      Data : TInstanceData;
      Value : TData;

      ProfileParent : Integer;

    Constructor Create; virtual;

    Constructor From(const AData:TData); overload;
    Constructor From(const ANodes:TNodes); overload;

    Destructor Destroy; override;

    {$IFNDEF FPC}
    class Destructor Destroy;
    {$ENDIF}

    function AddData(const AVariable:TVariable):Integer;
//    procedure AddItems(const AType:TType); overload;
    procedure AddItems(const ANodes:TNodes); overload;

    class procedure FreeTypes; static;

    class function TypeInstanceOf(const AOwner:TInstance; const AType:TType):TInstance; static;
  end;

  TInstanceClass=class of TInstance;

{$IFDEF INTERNAL}
var
  Instances_ID : Integer;

type
  TInstances=Array of TInstance;

  TInstancesHelper=record helper for TInstances
  private
    //procedure CheckAsChildren(const AOwner,AInstance:TInstance);
  public
    procedure Add(const AInstance:TInstance);
    procedure CheckLeaks;
    function Count:Integer; inline;
    function Find(const AInstance:TInstance):Integer;
    procedure FreeAll;
    procedure Remove(const AInstance:TInstance);
    procedure Reset;
  end;

var Instances : TInstances;

// Debug
procedure FindOtherInstances(const ASelf:Integer; const AValue:TInstance);
{$ENDIF}

{.$DEFINE RELEASE_BUG}

{$IFDEF RELEASE_BUG}
var Release_Bug_Show :Boolean=False;
{$ENDIF}

implementation

uses
  {$IFDEF RELEASE_BUG}
  Windows,
  {$ENDIF}
  {$IFDEF INTERNAL}
  Internal,
  {$ENDIF}
  SysUtils;

{ TInstance.TVariableValue }

{$IFDEF INTERNAL}

procedure Dummy;
begin
end;

procedure TInstance.TVariableValue.SetValue(const AInstance:TInstance);
begin
  if AInstance=Value then
     InternalError('Setting same Value at VariableValue',AInstance);

  {
  if AInstance.Owner<>Owner then
     InternalError('Setting Value with different Owner at VariableValue',AInstance);
  }

  if AInstance.ID<Owner.ID then
     //Error('Setting long-lived Value at VariableValue',AInstance);
     Dummy;

  FValue:=AInstance;
end;
{$ENDIF}

procedure TInstance.TVariableValue.ReplaceValue(const AValue:TInstance);
begin
  if Value=nil then
     Value:=AValue
  else
  if Value=AValue then
  {$IFDEF INTERNAL}
//     Error('Error same ReplaceValue',AValue)
  {$ENDIF}
  else
  begin
    Value.Free;
    Value:=AValue;
  end;
end;

{ TInstance }

Constructor TInstance.Create;
begin
  inherited Create;

  {$IFDEF INTERNAL}
  MakeLastID;

  Instances.Add(Self);
  {$ENDIF}
end;

{$IFDEF INTERNAL}
procedure TInstance.MakeLastID;
begin
  ID:=Instances_ID;
  Inc(Instances_ID);
end;

{$ENDIF}

Constructor TInstance.From(const AData: TData);
begin
  Create;

  {$IFDEF INTERNAL}
  if AData=nil then
     InternalError('Internal error, empty Data: TInstance.From',nil);
  {$ENDIF}

  Value:=AData;
end;

Constructor TInstance.From(const ANodes: TNodes);
begin
  Create;
  AddItems(ANodes);
end;

{$IFNDEF FPC}
// Just in case, destroy global shared types
class destructor TInstance.Destroy;
begin
  FreeTypes;
  inherited;
end;
{$ENDIF}

{$IFDEF RELEASE_BUG}
procedure Release_Show(const AValue:TNode);
var S :String;
begin
  S:=AValue.ClassName;

  if AValue is TText then
     S:=S+': '+TText(AValue).Value
  else
  if AValue is TInteger then
     S:=S+': '+TInteger(AValue).Value.ToString;

  OutputDebugString(PWideChar(S));
end;
{$ENDIF}

Destructor TInstance.Destroy;
var D : TVariableValue;
    t : Integer;
begin
  {$IFDEF RELEASE_BUG}
  if Release_Bug_Show then
     if Value<>nil then
        Release_Show(Value);
  {$ENDIF}

  for t:=High(Data) downto Low(Data) do
  begin
    D:=Data[t];

    // B:=A  !!  reference issue !

    if D.Value<>nil then
    begin
      if D.Value.Owner=Self then
         D.Value.Free

      {$IFDEF INTERNAL}
      else
      // TO REMOVE WHEN Instance References are solved.   a:=b
       //  InternalError('Internal, Data Value Owner is not Self Instance',D.Variable);
      {$ENDIF}
    end;
  end;

  Data:=nil;

  if Value<>nil then
     if Value.Owner=Self then
        Value.Free;

  {$IFDEF INTERNAL}
  Instances.Remove(Self);
  {$ENDIF}

  //Instances.CheckAsChildren(Owner as TInstance,Self);

  inherited;
end;

function TInstance.AddData(const AVariable:TVariable):Integer;
begin
  result:=Data.Add(AVariable);

  {$IFDEF INTERNAL}
  Data[result].Owner:=Self;
  {$ENDIF}
end;

procedure TInstance.AddItems(const ANodes:TNodes);
var N : TNode;
begin
  for N in ANodes do
      if N is TVariable then
         if not TVariable(N).Clauses.Shared then
            AddData(TVariable(N));
end;

{unused:
procedure TInstance.AddItems(const AType:TType);
begin
  AddItems(AType.Items);

  if AType is TClassType then
     if TClassType(AType).Ancestor<>nil then
        AddItems(TClassType(AType).Ancestor);
end;
}

class procedure TInstance.FreeTypes;
var t : Integer;
begin
  for t:=Low(Types) to High(Types) do
      Types[t].Value.Free;

  Types:=nil;
end;

class function TInstance.TypeInstanceOf(const AOwner:TInstance; const AType:TType):TInstance;

    // TODO: Sorted array hash
  function FindType(const AType:TType):TInstance;
  var t : Integer;
  begin
    for t:=0 to High(Types) do
        if Types[t].TheType=AType then
           Exit(Types[t].Value);

    result:=nil;
  end;

  {$IFDEF INTERNAL}
  // Set all Data items Owner to Self
  procedure SetDataOwner(const AInstance:TInstance);
  var t : Integer;
  begin
    for t:=Low(AInstance.Data) to High(AInstance.Data) do
        AInstance.Data[t].Owner:=AInstance;
  end;
  {$ENDIF}

  // Search module-level top instance owner
  function TopInstanceOf(const AInstance:TInstance):TInstance;
  begin
    result:=AInstance;

    while result.Owner<>nil do
          result:=TInstance(result.Owner);
  end;

var tmp : Integer;
begin
  result:=FindType(AType);

  if result=nil then
  begin
    tmp:=Length(Types);
    SetLength(Types,tmp+1);
    Types[tmp].TheType:=AType;

    result:=TInstance.Create;
    result.Owner:=TopInstanceOf(AOwner);
    result.AddShared(AType.Items);

    {$IFDEF INTERNAL}
    SetDataOwner(result);
    {$ENDIF}

    Types[tmp].Value:=result;
  end;
end;

procedure TInstance.AddShared(const ANodes:TNodes);
var N : TNode;
begin
  for N in ANodes do
      if N is TVariable then
         if TVariable(N).Clauses.Shared then
            AddData(TVariable(N));
end;

function TInstance.TDataHelper.Find(const AVariable: TVariable): Integer;
var t : Integer;
begin
  for t:=Low(Self) to High(Self) do
      if Self[t].Variable=AVariable then
         Exit(t);

  result:=-1;
end;

function TInstance.TDataHelper.Add(const AVariable: TVariable):Integer;
begin
  result:=Length(Self);
  SetLength(Self,result+1);

  Self[result].Variable:=AVariable;
end;

procedure TInstance.TDataHelper.Remove(const AVariable: TVariable);

  {$IFDEF FPC}
  procedure DeleteVariable(const AIndex:Integer);
  var t : Integer;
  begin
    for t:=AIndex to High(Self)-1 do
        Self[t]:=Self[t+1];

    SetLength(Self,High(Self));
  end;
  {$ENDIF}

var t : Integer;
begin
  for t:=Low(Self) to High(Self) do
      if Self[t].Variable=AVariable then
      begin
        {$IFDEF INTERNAL}
        if Self[t].FValue.Owner<>Self[t].Owner then
           InternalError('Removing Data Value, not its Owner',Self[t].FValue);
        {$ENDIF}

        Self[t].Value.Free;

        {$IFDEF FPC}
        DeleteVariable(t);
        {$ELSE}
        Delete(Self,t,1);
        {$ENDIF}

        break;
      end;
end;

function TInstance.TDataHelper.VariableOfValue(const AValue: TData): TVariable;
var D : TInstance.TVariableValue;
begin
  for D in Self do
      if D.Value=AValue then
         Exit(D.Variable);

  result:=nil;
end;

{$IFDEF INTERNAL}
procedure FindOtherInstances(const ASelf:Integer; const AValue:TInstance);
var t : Integer;
begin
  for t:=0 to High(Instances) do
      if Instances[t]=AValue then
         if t<>ASelf then
            InternalError('Instance cannot be destroyed '+IntToStr(t),AValue);
end;

procedure TInstancesHelper.Add(const AInstance:TInstance);
var L : Integer;
begin
  L:=Length(Self);
  SetLength(Self,L+1);
  Self[L]:=AInstance;
end;

function TInstancesHelper.Count: Integer;
begin
  result:=Length(Self);
end;

procedure TInstancesHelper.CheckLeaks;
begin
  if Count>0 then
     {
     if YesNo('CleanUp, leaking instances: '+IntToStr(Count)+'. Continue?') then
        FreeAll
     else
     }
        InternalError('Stop due to leak instances',nil);
end;

function TInstancesHelper.Find(const AInstance:TInstance):Integer;
var t : Integer;
begin
   for t:=Low(Self) to High(Self) do
       if Self[t]=AInstance then
          Exit(t);

   result:=-1;
end;

procedure TInstancesHelper.FreeAll;
begin
  while Count>0 do
        Instances[0].Free;
end;

{
procedure TInstancesHelper.CheckAsChildren(const AOwner,AInstance:TInstance);
var t, tt : Integer;
    tmp : TInstance;
begin
  for t:=0 to High(Self) do
  begin
    tmp:=Self[t];

    if tmp<>AOwner then
       for tt:=0 to High(tmp.Data) do
           if tmp.Data[tt].Value=AInstance then
              InternalError('Trying to destroy instance before its owner',AInstance);
  end;
end;
}

procedure TInstancesHelper.Remove(const AInstance:TInstance);
var t,tmp : Integer;
begin
  tmp:=Find(AInstance);

  if tmp=-1 then
     {$IFDEF INTERNAL}
     InternalError('Cannot remove instance: ',AInstance)
     {$ENDIF}
  else
  begin
    for t:=tmp to High(Self)-1 do
        Self[t]:=Self[t+1];

    SetLength(Self,High(Self));
  end;
end;

procedure TInstancesHelper.Reset;
begin
  Instances_ID:=0;
end;
{$ENDIF}

{$IFDEF FPC}
initialization
  {$IFDEF INTERNAL}
  Instances.Reset;
  {$ENDIF}
finalization
  TInstance.FreeTypes;

  {$IFDEF INTERNAL}
  Instances.CheckLeaks;
  {$ENDIF}
{$ENDIF}
end.
