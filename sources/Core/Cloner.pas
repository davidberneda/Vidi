unit Cloner;

interface

uses
  Sys, AST;

type
  TCloner=record
    class function ArrayExpression(const AOwner:TNode; const A:TArrayExpression):TArrayExpression; static;
    class function Data(const AOwner:TNode; const ANode:TData):TData; static;
    class function Node(const AOwner:TNode; const ANode:TNode):TNode; static;
    class function Nodes(const AOwner: TNode; const ANodes:TNodes): TNodes; static;
    class function Range(const AOwner:TNode; const ARange:TRange):TRange; static;
    class function Routine(const AOwner:TNode; const ARoutine:TRoutine):TRoutine; static;

    class procedure SetParametersType(const ANew,AOld:TParametersType); static;

    class function Variable(const AOwner:TNode; const AVariable:TVariable):TVariable; static;
  end;

implementation

uses
  Exceptions;

class function TCloner.Variable(const AOwner:TNode; const AVariable:TVariable):TVariable;
begin
  result:=TVariable.Create;
  result.Owner:=AOwner;

  result.Clauses:=AVariable.Clauses;
  result.Initialized:=AVariable.Initialized;
  result.Name:=AVariable.Name;

  if AVariable.ValueData<>nil then
     result.ValueData:=TCloner.Node(result,AVariable.ValueData) as TData;

  result.VariableType:=AVariable.VariableType;
end;

procedure DoError(S:String; const ANode:TNode);
begin
  if ANode=nil then
     S:=S+' (? nil)'
  else
     S:=S+' '+ANode.ClassName;

  Raise_Exception(S);
end;

class function TCloner.Data(const AOwner:TNode; const ANode:TData):TData;
begin
  if ANode is TText then
  begin
    result:=TText.Create(TText(ANode).Value);
    result.Owner:=AOwner;
  end
  else
  if ANode is TInteger then
  begin
    result:=TInteger.Create(TInteger(ANode).Value);
    TInteger(result).Text:=TInteger(ANode).Text;
    TInteger(result).Base:=TInteger(ANode).Base;

    result.Owner:=AOwner;
  end
  else
  if ANode is TFloat then
  begin
    result:=TFloat.Create(TFloat(ANode).Value);
    TFloat(result).Text:=TFloat(ANode).Text;

    result.Owner:=AOwner;
  end
  else
  if ANode is TBoolean then
  begin
    result:=TBoolean.Create(TBoolean(ANode).Value);
    result.Owner:=AOwner;
  end
  else
  if ANode is TArrayExpression then
     result:=TCloner.ArrayExpression(AOwner,TArrayExpression(ANode))
  else
  if ANode is TVariable then
     result:=TCloner.Variable(AOwner,TVariable(ANode))
  else
  if ANode is TRange then
     result:=TCloner.Range(AOwner,TRange(ANode))
  else
     result:=nil;
end;

class function TCloner.Range(const AOwner:TNode; const ARange:TRange):TRange;
begin
  result:=TRange.Create;

  result.Min:=TCloner.Data(result,ARange.Min);
  result.Max:=TCloner.Data(result,ARange.Max);
end;

class function TCloner.Node(const AOwner:TNode; const ANode:TNode):TNode;
begin
  if ANode is TData then
     result:=TCloner.Data(AOwner,TData(ANode))
  else
  if ANode is TRoutine then
     result:=TCloner.Routine(AOwner,TRoutine(ANode))
  else
  if ANode is TReturn then
  begin
    result:=TReturn.Create;
    result.Owner:=AOwner;

    TReturn(result).Value:=TCloner.Data(result,TReturn(ANode).Value);
  end
  else
    result:=nil; //ANode; // <-- TODO: Clone rest of all classes?

  if result=nil then
     DoError('Cannot clone:',ANode);
end;

class function TCloner.Nodes(const AOwner: TNode;
                             const ANodes:TNodes): TNodes;
var t,L :Integer;
    tmp : TNode;
begin
  {$IFDEF FPC}
  result:=nil;
  {$ENDIf}

  L:=Length(ANodes);
  SetLength(result,L);

  for t:=0 to L-1 do
  begin
    tmp:=ANodes[t];

    if tmp=nil then
       result[t]:=nil
    else
       result[t]:=Node(AOwner,tmp);
  end;
end;

class procedure TCloner.SetParametersType(const ANew,AOld:TParametersType);
begin
  ANew.Clauses:=AOld.Clauses;

  // Fix
  if ANew is TNamedType then
     TNamedType(ANew).Name:=TNamedType(AOld).Name;

  ANew.Parameters:=Nodes(ANew,AOld.Parameters);
  ANew.Items:=Nodes(ANew,AOld.Items);
end;

class function TCloner.Routine(const AOwner: TNode;
                               const ARoutine: TRoutine): TRoutine;
begin
  result:=TRoutine.Create;
  result.Name:=ARoutine.Name;

  result.Owner:=AOwner;

  result.Ancestor:=ARoutine.Ancestor;
  result.Output:=ARoutine.Output;

  SetParametersType(result,ARoutine);
end;

class function TCloner.ArrayExpression(const AOwner:TNode; const A:TArrayExpression):TArrayExpression;
begin
  result:=TArrayExpression.Create;
  result.Owner:=AOwner;

  result.Parameters:=TCloner.Nodes(result,A.Parameters);
end;

end.
