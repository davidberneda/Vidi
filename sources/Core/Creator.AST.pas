unit Creator.AST;

interface

uses
  Sys, AST;

type
  TASTCreator=record
  public
    class procedure AssignParameters(const AType:TParametersType; const ANodes:TNodes); static;

    class function CreateAssignment(const AOwner:TNode;
                                    const Arithmetic:TArithmetic_Assign;
                                    const AVariable,
                                    AValue:TData):TAssignment; static;

    class function CreateCallData(const AOwner:TNode;
                                  const AData:TData):TCallData; static;

    class function CreateClassType(const AOwner:TNode):TClassType; overload; static;

    class function CreateClassType(const AParent:TNode;
                                   const AName:String;
                                   const AClauses:TClauses;
                                   const AParameters:TNodes):TClassType; overload; static;

    class function CreateGenericType(const AOwner:TNode;
                                     const AVariable:TVariable;
                                     const AFinalType:TType):TGenericType; static;

    class function CreateIntegerClass(const AParent:TNode;
                                      const AName:String;
                                      const ARange:TRangeType;
                                      const AParameters:TNodes):TIntegerClass; static;

    class function CreateRangeType(const ARange:TRange):TRangeType; static;

    class function CreateRoutine(const AOwner:TNode;
                                 const AName:String;
                                 const AClauses:TClauses;
                                 const AParameters:TNodes):TRoutine; static;

    class function CreateTypeMember(const AOwner:TNode; const AType,AMember:TType):TTypeMember; static;

    class function CreateWith(const AOwner:TNode; const AModule:TNamedType):TWith; static;

    class procedure SetOwner(const AOwner:TNode; const ANodes:TNodes); static;
  end;

implementation

uses
  Checker.AST, Syntax;

class procedure TASTCreator.SetOwner(const AOwner:TNode; const ANodes:TNodes);
var N : TNode;
begin
  for N in ANodes do
      N.Owner:=AOwner;
end;

// Try to unify both assignment types
class function TASTCreator.CreateAssignment(const AOwner:TNode;
                                            const Arithmetic:TArithmetic_Assign;
                                            const AVariable,
                                            AValue:TData):TAssignment;
begin
  result:=TAssignment.Create;
  result.Owner:=AOwner;

  result.Arithmetic:=Arithmetic;

  result.Variable:=AVariable;
  result.Value:=AValue;

  if (not (AVariable is TVariable)) and (AVariable.Owner=AOwner) then
     AVariable.Owner:=result;

  if (not (AValue is TVariable)) and (AValue.Owner=AOwner) then
     AValue.Owner:=result;

  if result.Variable is TVariable then
     TVariable(result.Variable).Initialized:=True;
end;

// Call routine that is an expression:   A.B.C.D()
class function TASTCreator.CreateCallData(const AOwner:TNode;
                                          const AData:TData):TCallData;
begin
  result:=TCallData.Create;
  result.Owner:=AOwner;
  result.Value:=AData;
end;

class function TASTCreator.CreateClassType(const AOwner:TNode):TClassType;
begin
  result:=TClassType.Create;
  result.Owner:=AOwner;
end;

class procedure TASTCreator.AssignParameters(const AType:TParametersType; const ANodes:TNodes);
begin
  SetOwner(AType,ANodes);
  AType.Parameters:=ANodes;
end;

class function TASTCreator.CreateClassType(const AParent:TNode;
                                           const AName:String;
                                           const AClauses:TClauses;
                                           const AParameters:TNodes):TClassType;
begin
  result:=TASTCreator.CreateClassType(AParent);
  result.Name:=AName;

  result.Clauses:=AClauses;

  // Force Shared=True for classes (they are always shared, accessible at owner type-level)
  result.Clauses.Shared:=True;

  AssignParameters(result,AParameters);

//      Checker.TryCheckMagicTypes(result);
end;

class function TASTCreator.CreateIntegerClass(const AParent:TNode;
                                              const AName:String;
                                              const ARange:TRangeType;
                                              const AParameters:TNodes):TIntegerClass;
begin
  result:=TIntegerClass.Create;
  result.Owner:=AParent;

  result.Name:=AName;

  result.Parameters:=AParameters;

  result.Ancestor:=TChecker._Types[TChecker._Integer]; // Integer

  if result.Ancestor=nil then
     result.Ancestor:=TChecker._Types[TChecker._Number]; // Integer is Number

  ARange.Owner:=result;
  result.Size:=ARange;
end;

// Foo[0..9]  = TArrayType Foo[TRangeType]
class function TASTCreator.CreateRangeType(const ARange:TRange):TRangeType;
begin
  result:=TRangeType.Create;
  result.Owner:=ARange.Owner;
  ARange.Owner:=result;
  result.Range:=ARange;
end;

class function TASTCreator.CreateGenericType(const AOwner:TNode;
                                             const AVariable:TVariable;
                                             const AFinalType:TType):TGenericType;
begin
  result:=TGenericType.Create;
  result.Owner:=AOwner;

//  result.Name:=AVariable.Name;

  result.Variable:=AVariable;
  result.FinalType:=AFinalType;
end;

class function TASTCreator.CreateRoutine(const AOwner:TNode;
                                         const AName:String;
                                         const AClauses:TClauses;
                                         const AParameters:TNodes):TRoutine;
begin
  result:=TRoutine.Create;
  result.Owner:=AOwner;

  result.Name:=AName;

  result.Clauses:=AClauses;

  AssignParameters(result,AParameters);

  // Sub-routines should always be flagged as Final. (Impossible to override)
  if TChecker.GetRoutineOf(result.Owner)<>nil then
     result.Clauses.Final:=True;
end;

class function TASTCreator.CreateTypeMember(const AOwner: TNode; const AType,
  AMember: TType): TTypeMember;
begin
  result:=TTypeMember.Create;
  result.Owner:=AOwner;

  result.TheType:=AType;
  result.Member:=AMember;
end;

class function TASTCreator.CreateWith(const AOwner:TNode;
                                      const AModule:TNamedType):TWith;
begin
  result:=TWith.Create;
  result.Owner:=AOwner;

  result.Module:=AModule;
end;

end.
