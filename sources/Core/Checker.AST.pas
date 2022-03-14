unit Checker.AST;

interface

uses
  Sys, AST;

type
  TChecker=class
  private
    class function AllItemsAreOnly(const AType,ADataType:TType):Boolean; static;
    class function CharacterOrText(const ALeft,ARight:TType):Boolean; static;
    class function InternalGetDataType(const AParent:TNode; const AData:TData; const ValidArrays:Boolean):TType; static;
  public
    const
      _Reserved_Boolean='Boolean';

    class var
      AllMagicTypes : Boolean;   // default False

      _Types:Array[0..12] of TClassType {TNamedType};

      CaseSensitive : Boolean;   // default False

    const
       TypeNames:Array[0..High(TChecker._Types)] of String=
         ('Data','Array','Integer','Float',_Reserved_Boolean,'Logical','Text',
          'nil','Type','Number','Character','SomeThing','Range');

       _Data     = 0;
       _Array    = 1;
       _Integer  = 2;
       _Float    = 3;
       _Boolean  = 4;
       _Logical  = 5;
       _Text     = 6;
       _Nil      = 7;
       _Type     = 8;
       _Number   = 9;
       _Character= 10;
       _SomeThing= 11;
       _Range    = 12;

    type
      TRoutineDefinition=record
        Name : String;
        Output : TType;
        Parameters : TNodes;
      end;


    class function BadHiddenAccess(const AParent:TNode; const AData:TData):Boolean; static;

    procedure CheckMagicTypes(const AType:TClassType);
    function CheckOperand(const AParent:TNode; const AOperand:TOperand):Boolean;
    procedure ClearMagicTypes;

    function CompatibleAssign(const ALeft,ARight:TData; const AParent:TNode):Boolean; overload;
    function CompatibleAssign(ALeft:TType; const ARight:TType; const ALeftData,ARightData:TData):Boolean; overload;
    function CompatibleAssign(const ALeftType:TType; const ALeft,ARight:TData):Boolean; overload;

    class function CompatibleType(const A:TType; B:TType):Boolean; static;
    class function Contains(const ANodes:TNodes; const ANode:TNode):Boolean; static;

    class function DerivesFrom(const ANode:TNode; const AType:TType):Boolean; static;
    class function DerivesFromNumber(const ANode:TNode):Boolean; static;

    class function FinalTypeOf(const AParent:TNode;
                         const AData:TData; const ANode:TNode):TNode; static;

    class function FindGenericAncestor(AClass:TClassType):TGenericType; static;
    class function FindGenericInType(const AType:TGenericType; const ADerived:TType):TType; static;
    class function FindSpecializedAncestor(AClass:TClassType):TSpecializedType; static;

    function FixPrecedence(const AOperand:TOperand):TOperand;

    class function GenericTypeOf(const AParent:TNode;
                                 const AData:TData;
                                 const AType:TGenericType):TType; static;

    class function GetClassTypeOf(ANode:TNode):TClassType; static;

    class function GetDataAsArrayType(const AParent:TNode;
                                 const AData:TData;
                                 const AType:TType):TType; static;

    class function GetDataType(const AParent:TNode; const AData:TData):TType; static;
    class procedure GetDataTypes(const AParent:TNode;
                                 const ALeft,ARight:TData;
                                 out L,R:TType); static;

    class function GetDataTypeArray(const AData:TArrayExpression):TType; static;
    class function GetDataTypeAsArray(const AParent:TNode;
                                      const AData:TData;
                                      const AType:TType):TType; static;
    class function GetFinalClassType(const AType:TType):TType; static;
    class function GetFinalSpecialized(const AType:TType):TType; static;
    class function GetFinalType(const AType:TType):TType; static;
    class function GetRoutineOf(ANode:TNode):TRoutine; static;
    class function GetSpecialGenerics(const AParent:TNode;
                         const AData:TData;
                         const ASpecial:TSpecializedType):TNodes; static;
    class function GetTypeType(const AData:TData):TType; static;

    class function HasName(const ANode:TNode; out AName:String):Boolean; static;

    class function IsAbstract(const R:TRoutine):Boolean; static;
    class function IsArray(const AType:TType):Boolean; static;
    class function IsCallableVariable(const ANode:TNode):Boolean; static;
    class function IsFinalData(const AData:TData):Boolean; static;
    class function IsFinalRoutine(const ARoutine:TRoutine):Boolean; static;
    class function IsSharedData(const AData:TData):Boolean; static;

    class function IsBasicType(const AType:TType):Boolean; static;

    class function IsEnumerationType(const ANode:TNode):Boolean; static;
    class function IsEnumerationClass(const AType:TType):Boolean; static;
    class function IsEnumerationItem(const AData:TData):Boolean; static;

    class function IsFloat(const AType:TType):Boolean; overload; static;
    class function IsFloat(const AData:TData):Boolean; overload; static;

    class function IsHidden(const ANode:TNode):Boolean; static;

    class function IsInteger(const AData:TData):Boolean; static;
    class function IsIntegerOrNumber(const AData:TData):Boolean; static;
    class function IsIntegerOrRangeType(const AData:TData):Boolean; static;

    class function IsInlineStruct(const AType:TType):Boolean; static;

    class function IsLiteral(const ANode:TNode):Boolean; static;

    class function IsLocalVariable(const AVariable:TVariable;
                                   const ABlock:TType;
                                   const ANodes:TNodes):Boolean; static;

    class function IsLogical(const AData:TData):Boolean; static;
    class function IsLogicalType(const AType:TType):Boolean;
    class function IsManyValues(const ANode:TNode):Boolean; static;
    class function IsNumber(const AData:TData):Boolean; static;

    class function IsOutParameter(const AVariable:TVariable):Boolean; static;
    class function IsOwner(const AOwner:TNode; ANode:TNode):Boolean; static;

    class function IsReservedKeyword(const AName:String):Boolean; static;
    class function IsSharedVariable(const AData:TData):Boolean; static;
    class function IsStatementKeyword(const AName:String):Boolean; static;

    class function IsStructure(const AType:TType):Boolean; static;

    class function IsText(const AData:TData):Boolean; static;
    class function IsValidLogicalOperator(const AOperand:TOperand):Boolean; static;

    class function IsVariableOfTypeType(const AVariable:TVariable):Boolean; static;
    class function IsVariableFunctionType(const AVariable:TVariable):Boolean; static;
    class function IsDataFunctionType(const AData:TData):Boolean; static;

    class function VariableIsEnumItem(const AVariable:TVariable):Boolean; static;

    function MemberExists(const ANodes:TNodes; const ANode:TNode; out Existing:TNode):Boolean;
    function NodeIs(const ANode:TNode; const AName:String):Boolean;

    class function OnlyDataItems(const AType:TType):Boolean; static;

    function Precedence(const AOperand:TData):Integer;
    class function ReturnsSomething(const AData:TData):Boolean; static;

    function SameRoutineType(const A,B:TRoutine):Boolean;
    function SameRoutine(const A,B:TRoutine):Boolean; overload; inline;
    function SameRoutine(const A:TRoutine; const B:TRoutineDefinition):Boolean; overload;

    function SameSignature(A,B:TNode):Boolean;
    function SameStructure(const AItems,ANodes:TNodes; out Bad:Integer):Boolean;

    procedure SpecialBoolean(const AName:String; const AType:TClassType);

    class function TextIs(const A,B:String):Boolean; static;
    class function TheTypeOf(const AType:TType):TType; static;
    procedure TryCheckMagicTypes(const AType:TClassType);
    class function TypeHasOutput(const AType:TType):Boolean; static;
    class function TypeOfGeneric(const ANode:TNode):TType; static;
    class function TypeOfIndexed(const ANode:TNode):TType; static;
    function ValidParameters(const AParent:TNode; const P1,P2:TNodes; out Bad:Integer):Boolean;

    class function VariableIsClass(const AVariable:TVariable):Boolean; static;

    function VariableIsType(const AVariable:TVariable; const AType:String):Boolean;
  end;

implementation

uses
  {$IFDEF INTERNAL}
  Internal,
  {$ENDIF}

  {$IFDEF NODEPOOL}
  Node_Pool,
  {$ENDIF}

  SysUtils, Syntax, Magic, StringArray;

class function TChecker.TextIs(const A,B:String):Boolean;
begin
  if CaseSensitive then
     result:=A=B
  else
     result:=SameText(A,B);
end;

// Magic Types
procedure TChecker.CheckMagicTypes(const AType:TClassType);
var t : Integer;
begin
  for t:=0 to High(_Types) do
      if _Types[t]=nil then
         if TextIs(AType.Name,TypeNames[t]) then
            _Types[t]:=AType;

  for t:=0 to High(_Types) do
      if _Types[t]=nil then
      begin
        AllMagicTypes:=False;
        Exit;
      end;

  AllMagicTypes:=True;
end;

// Try catching "magic" basic types
procedure TChecker.TryCheckMagicTypes(const AType:TClassType);

  procedure AddParamSomething(const ARoutine:TRoutine);
  var tmp : TVariable;
  begin
    tmp:=TVariable.Create;
    tmp.Owner:=ARoutine;
    tmp.VariableType:=_Types[_SomeThing];

    ARoutine.Parameters.Add(tmp);

    {$IFDEF NODEPOOL}
//    NodePool.Add(tmp);
    {$ENDIF}
  end;

  procedure FinishMagicTypeOf;
  begin
    TMagic.MagicTypeOf.Output:=_Types[_Type]; // Type
    AddParamSomething(TMagic.MagicTypeOf);
  end;

  procedure FinishMagicNameOf;
  begin
    TMagic.MagicNameOf.Output:=_Types[_Text]; // Text
    AddParamSomething(TMagic.MagicNameOf);
  end;

begin
  if not AllMagicTypes  then
  begin
    CheckMagicTypes(AType);

    if (TMagic.MagicTypeOf.Output=nil) and (_Types[_Type]<>nil) and (_Types[_SomeThing]<>nil) then
       FinishMagicTypeOf;

    if (TMagic.MagicNameOf.Output=nil) and (_Types[_Text]<>nil) and (_Types[_SomeThing]<>nil) then
       FinishMagicNameOf;
  end;
end;

function TChecker.ValidParameters(const AParent:TNode;
                                  const P1, P2: TNodes;
                                  out Bad:Integer): Boolean;

  function IsGoodParameter(const P1,P2:TNode):Boolean;
  begin
    result:=(P1 is TData) and
            (P2 is TData) and
            CompatibleAssign(TData(P1),TData(P2),AParent);
  end;

  function CheckParameters(const AFrom,ATo:Integer):Boolean;
  var t : Integer;
  begin
    for t:=AFrom to ATo do
        if not IsGoodParameter(P1[t],P2[t]) then
        begin
          Bad:=t;
          Exit(False);
        end;

    result:=True;
  end;

var H : Integer;
begin
  result:=False;

  Bad:=-1;

  if Length(P1)=Length(P2) then
     result:=CheckParameters(Low(P1),High(P2))
  else
  if Length(P1)>0 then
  begin
    H:=High(P1);

    // X:Text, D:Data...
    if IsManyValues(P1[H]) then
       result:=CheckParameters(Low(P1),H-1);
  end;
end;

class function TChecker.IsManyValues(const ANode:TNode):Boolean;
begin
  result:=(ANode is TVariable) and (TVariable(ANode).VariableType is TManyValues);
end;

class function TChecker.CompatibleType(const A:TType; B:TType):Boolean;
begin
//  if (A=_Types[_Type]) and (B is TClassType) then // Type=XXX   (XXX is a Type)
//     result:=True
//  else
  begin
    repeat

      result:=A=B;

      if result then
         break
      else
      begin
        if B is TClassType then
           if TClassType(B).Ancestor=nil then
              break
           else
              B:=TClassType(B).Ancestor
        else
        begin
          if (B is TArrayType) and (A=_Types[_Array]) then
             result:=True  // Array
          else
          if (B is TRangeType) and ((A=_Types[_Number]) or (A=_Types[_Integer])) then
             result:=True; // 1..31  is a number

          break;
        end;
      end;

    until B=nil;

  end;
end;

class function TChecker.VariableIsClass(const AVariable: TVariable): Boolean;
begin
  result:=GetFinalType(AVariable.VariableType) is TClassType;
end;

function TChecker.VariableIsType(const AVariable:TVariable; const AType:String):Boolean;
begin
  result:=(AVariable.VariableType=_Types[_Type]) and // 'Type'
          TextIs(AVariable.Name,AType);
end;

// Traverse chain:   MyNamespace.MyType.MySubType.My...
class function TChecker.TheTypeOf(const AType:TType):TType;
begin
  if AType is TTypeMember then
     result:=TheTypeOf(TTypeMember(AType).Member)
  else
     result:=AType;
end;

class function TChecker.DerivesFrom(const ANode:TNode; const AType:TType):Boolean;
var tmp : TType;
begin
  result:=False;

  if ANode is TType then
  begin
    tmp:=TheTypeOf(TType(ANode));

    while tmp<>nil do
      if tmp=AType then
         Exit(True)
      else
      if tmp is TClassType then
         tmp:=TClassType(tmp).Ancestor
      else
         break;
  end;
end;

class function TChecker.DerivesFromNumber(const ANode:TNode):Boolean;
begin
  result:=DerivesFrom(ANode,_Types[_Integer]) or  // Integer
          DerivesFrom(ANode,_Types[_Float]) or  // Float
          DerivesFrom(ANode,_Types[_Number]); // Number
end;

// AType is a class that only has variables, all of them of type ADataType
class function TChecker.AllItemsAreOnly(const AType,ADataType:TType):Boolean;

  function IsDataVariable(const ANode:TNode):Boolean;
  begin
    result:=(ANode is TVariable) and
            (TVariable(ANode).VariableType=ADataType)
  end;

var t : TNode;
begin
  result:=(AType is TClassType) and (Length(AType.Items)>0);

  if result then
     for t in AType.Items do
         if not IsDataVariable(t) then
            Exit(False);
end;

class function TChecker.IsVariableOfTypeType(const AVariable:TVariable):Boolean;
var tmp : TType;
begin
  tmp:=AVariable.VariableType;

  result:=(tmp=_Types[_Type]) or (tmp is TGenericType);
end;

class function TChecker.IsVariableFunctionType(const AVariable:TVariable):Boolean;
begin
  result:=GetFinalType(AVariable.VariableType) is TFunctionType;
end;

class function TChecker.IsDataFunctionType(const AData:TData):Boolean;
begin
  result:=(AData is TVariable) and IsVariableFunctionType(TVariable(AData));
end;

class function TChecker.IsSharedVariable(const AData:TData):Boolean;
begin
  result:=(AData is TVariable) and TVariable(AData).Clauses.Shared;
end;

class function TChecker.VariableIsEnumItem(const AVariable:TVariable):Boolean;
begin
  result:=(AVariable.Owner is TClassType) and
          TChecker.IsEnumerationClass(TClassType(AVariable.Owner));
end;

class function TChecker.IsEnumerationItem(const AData:TData):Boolean;
begin
  result:=IsSharedVariable(AData) and VariableIsEnumItem(TVariable(AData));
end;

class function TChecker.OnlyDataItems(const AType:TType):Boolean;
begin
  result:=AllItemsAreOnly(AType,_Types[_Data]); // Data
end;

// Returns True when ANode is an enum :  "Colors is { Red,Green,Blue }"
// ANode being used in an expression, not in declaration
class function TChecker.IsEnumerationType(const ANode:TNode):Boolean;
begin
  result:=(ANode is TTypeCall) and OnlyDataItems(TheTypeOf(TTypeCall(ANode).TheType));
end;

// Returns True when ANode is a class type :  "Colors is { Red,Green,Blue }"
class function TChecker.IsEnumerationClass(const AType:TType):Boolean;

  function EnumerationAncestor:Boolean;
  begin
    result:=(TClassType(AType).Ancestor=nil) or // <-- no ancestor, or
            IsEnumerationClass(TClassType(AType).Ancestor);
  end;

begin
  // TODO: Check also: AType is TSpecializedType
  result:=(AType is TClassType) and
          EnumerationAncestor and  // ast: Group is Data { Expression:Data }
//          (TClassType(AType).Generics=nil) and // Not a class with generics "Foo is Bar(Integer) {...}"
           TChecker.OnlyDataItems(AType);
end;

// Returns True when data items match AType class structure:
// "X: { Name:Text, Age:Integer } := ['Leia',29]"
function TChecker.SameStructure(const AItems,ANodes:TNodes; out Bad:Integer):Boolean;
begin
  result:=ValidParameters(nil,AItems,ANodes,Bad);
end;

// Special internal case for Boolean type, too early for True/False variables,
// needs forcing it:
procedure TChecker.SpecialBoolean(const AName: String; const AType:TClassType);
begin
  if TextIs(AName,_Reserved_Boolean) then
     _Types[_Boolean]:=AType;
end;

// Returns True when AType is an inline structure:  "X: { Name:Text, Age:Integer }"
class function TChecker.IsStructure(const AType:TType):Boolean;
var t : TNode;
begin
  result:=(AType is TClassType) and
          (Length(AType.Items)>0) and
          (TClassType(AType).Ancestor=nil);

  if result then
     for t in AType.Items do
         if not (t is TVariable) then
            Exit(False);
end;

class function TChecker.CharacterOrText(const ALeft,ARight:TType):Boolean;
begin
  result:=( (ALeft=_Types[_Text]) and (ARight=_Types[_Character]) ) or
          ( (ALeft=_Types[_Character]) and (ARight=_Types[_Text]) );
end;

function TChecker.CompatibleAssign(ALeft:TType;
                                   const ARight:TType;
                                   const ALeftData,ARightData:TData):Boolean;

  // AType = Float ,  or AType[] = Float
  function IsLeftFloat(const ALeftType:TType):Boolean;
  begin
    result:=IsFloat(ALeft);

    if not result then
       if ALeftType is TArrayType then
          result:=IsFloat(TArrayType(ALeftType).TheType)
  end;

  // AType? = Integer
  function CompatibleWithInteger(const AType:TType):Boolean;
  begin
    result:=(AType is TIntegerClass) or
            DerivesFrom(AType,_Types[_Integer]) or // Integer
            DerivesFrom(AType,_Types[_Float]) or // Float
            // TODO: Special case for assigning an integer to a float array
            // or variable (it is allowed).  ie: MyFloat[]:=MyInteger
            IsLeftFloat(AType) or
            (AType is TRangeType); // Foo:1..12    Foo=Integer
  end;

  // Number = AType?
  function NumberCompatibleWith(const AType:TType):Boolean;
  begin
    result:=(AType=_Types[_Integer]) or  // Integer
            (AType=_Types[_Float]);    // Float
  end;

  // Data = AType?
  function DataCompatibleWith(const AType:TType):Boolean;
  begin
    result:=(AType=_Types[_Integer]) or  // Integer
            (AType=_Types[_Float]) or  // Float
            (AType=_Types[_Boolean]) or  // Boolean
            (AType=_Types[_Logical]) or  // Logical
            (AType=_Types[_Text]) or  // Text
            (AType=_Types[_Number]) or  // Number
            (AType=_Types[_Character]);   // Character
  end;

  function CompatibleArrays(const ArrayLeft,ArrayRight:TType):Boolean;
  begin
    result:=ArrayLeft=ArrayRight;

    if not result then
       // TODO: Review Char:=Text
       result:=CharacterOrText(ArrayLeft,ArrayRight);
  end;

  function CompatibleAsArray(const ARightType:TType):Boolean;
  var tmpType : TType;
  begin
    result:=False;

    if ARightType=_Types[_Text] then // Text
    begin
      // Character = Text[1]
      if (ALeft=_Types[_Character]) and (ARightData is TText) then
         result:=Length(TText(ARightData).Value)=1
      else
      // Character[] = Text
      if ALeft is TArrayType then
         result:=TArrayType(ALeft).TheType=_Types[_Character];
    end;

    if not result then
      // Text:=Character
      if ALeft=_Types[_Text] then
         if ARight=_Types[_Character] then
            result:=True;

    if not result then
    begin
      if (ALeft is TArrayType) and (ARightType is TArrayType) then  // Text[] = Text[]
         result:=CompatibleArrays(TArrayType(ALeft).TheType,TArrayType(ARightType).TheType)
      else
      begin
        // Text[] = Character,  Text[] = [Character,Character...]

        if ARightData is TArrayExpression then
        begin
          tmpType:=GetDataTypeAsArray(nil,ALeftData,ALeft);

          result:=CompatibleType(tmpType,ARightType);

          if not result then
             // Type = AType[]
             result:=(ALeft=_Types[_Type]) and
                     (TArrayExpression(ARightData).Data is TTypeCall);
        end;

        Exit;

        {
        if not result then
           result:=(ARight is TArrayExpression) and
                   CompatibleType(ALeft,GetDataTypeAsArray(ARightType)); // Character = Text[]
        }
      end;
    end;
  end;

  function IsSameStructure(const AType:TClassType; const AData:TArrayExpression):Boolean;
  var tmpBad : Integer;
  begin
    result:=SameStructure(AType.Items,AData.Parameters,tmpBad);
  end;

  function TrySameStructure(const AType:TClassType; const ANode:TNode):Boolean;
  begin
    result:=(ANode is TArrayExpression) and
            IsSameStructure(AType,TArrayExpression(ANode));
  end;

  function TryAsStruct:Boolean;
  var tmp : TType;
      N : TNode;
      tmpData : TData;
  begin
    if ALeft is TArrayType then
       tmp:=TArrayType(ALeft).TheType
    else
       tmp:=ALeft;

    result:=(tmp is TClassType) and IsStructure(TClassType(tmp));

    if result then
    begin
      if ARightData is TVariable then
         tmpData:=TVariable(ARightData).ValueData
      else
         tmpData:=ARightData;

      if ALeft is TArrayType then
      begin
        result:=tmpData is TArrayExpression;

        if result then
           for N in TArrayExpression(tmpData).Parameters do
               if not TrySameStructure(TClassType(tmp),N) then
                  Exit(False);
      end
      else
        result:=TrySameStructure(TClassType(tmp),tmpData);
    end;
  end;

begin
  result:=False;

  if ARight<>nil then
  begin
    // X:MyFunctionType:=MyRoutine
    if ALeft is TFunctionType then
    begin
      if ARight is TRoutine then
      begin
        result:=SameRoutineType(TFunctionType(ALeft),TRoutine(ARight));

        if result then
           Exit;
      end;
    end;

    if ALeft is TManyValues then
       ALeft:=TManyValues(ALeft).TheType;

    result:=(ALeft=_Types[_SomeThing]) or // SomeThing
            (ALeft=ARight);

    if not result then
    begin
      // Special magic for intrinsic types

      if ALeft=_Types[_Data] then // Data = ?
         result:=DataCompatibleWith(ARight)
      else
      if ALeft=_Types[_Number] then // Number = ?
         result:=NumberCompatibleWith(ARight)
      else
      if ARight=_Types[_Number] then
         result:=NumberCompatibleWith(ALeft);

      if not result then
         if ARight=_Types[_Integer] then // ? = Integer
            result:=CompatibleWithInteger(ALeft);

      // Failed experiment: if P ...  <-- object instance: P<>nil
//        else
//            result:=IsLogicalType(ARight) and (ALeft is TClassType);


      if not result then
      begin
         // TODO: Special case for Foo:=nil  and Foo:=[]
        if ARight=_Types[_Nil] then
        begin
          result:=ALeft is TArrayType; // Foo : Text[]  Foo:=[]

          if not result then
             result:=(ALeft is TClassType) and (not IsBasicType(ALeft)); // Foo : TFoo Foo:=nil
        end;

        if not result then
        begin
          // Type = AType
          result:=(ALeft=_Types[_Type]) and (ARightData is TTypeCall);

          if not result then
          begin
            // Class1 = Class2
            result:=CompatibleType(ALeft,ARight);

            if not result then
            begin
              // Generics
              if ALeft is TGenericType then
              begin
                if ARight is TGenericType then
                   result:=TGenericType(ALeft).Variable=TGenericType(ARight).Variable
                else
                if ARightData is TVariable then
                   result:=TGenericType(ALeft).Variable=ARightData;
              end;

              if not result then
              begin
                result:=CompatibleAsArray(ARight);

                if not result then  // enum values = Data
                begin
                  result:=(ARight=_Types[_Data]) and OnlyDataItems(ALeft);

                  if not result then
                  begin
                    if ALeft=_Types[_Range] then
                       result:=ARightData is TRange
                    else
                       result:=TryAsStruct;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TChecker.CompatibleAssign(const ALeftType:TType; const ALeft,ARight:TData):Boolean;
begin
  result:=CompatibleAssign(ALeftType,GetDataType(nil,ARight),ALeft,ARight);
end;

class function TChecker.FindGenericInType(const AType:TGenericType; const ADerived:TType):TType;

 (*
  function GetGenericTypeOf(const Derived:TClassType; const AType:TGenericType):TType;
  var tmp : Integer;
      Ancestor : TType;
  begin
    Ancestor:=Derived.Ancestor;

    if Ancestor is TArrayType then
       result:=GetDataType(TArrayType(Ancestor).Generics[0] as TData)
    else
    if (Ancestor=nil) or (Ancestor.Parameters=nil) then
       result:=nil
    else
    begin
      tmp:=Ancestor.Parameters.IndexOf(AType.Variable);

      if tmp=-1 then
         result:=nil
      else
      if Derived.Generics.Count>tmp then
         result:=GetDataType(Derived.Generics[tmp] as TData)
      else
      begin
        result:=nil;

        {$IFDEF INTERNAL}
        InternalError(TErrors._MissingParameters,Derived);
        {$ENDIF}
      end;
    end;
  end;
  *)

  function GetGenericTypeOfArray(const AArray:TArrayType):TType;
  var tmp : Integer;
  begin
    tmp:=_Types[_Array].Parameters.IndexOf(AType.Variable);

    if tmp=-1 then
       result:=nil
    else
    if tmp=0 then
       result:=AArray.TheType
    else
    begin
      result:=nil;

      {$IFDEF INTERNAL}
      InternalError(TErrors._MissingParameters,AArray);
      {$ENDIF}
    end;
  end;

  // Traverse class-> ancestor-> chain looking for AType
  //  X:Array(Array(Integer))
  function GetGenericTypeOf(const Derived:TSpecializedType; const AVariable:TVariable):TType;
  var tmp : Integer;
      tmpType : TType;
  begin
    tmpType:=Derived.TheType;

    repeat
      if tmpType is TParametersType then
         tmp:=TParametersType(tmpType).Parameters.IndexOf(AVariable)
      else
         tmp:=-1;

      if tmp=-1 then
      begin
        // TODO: while not is TSpecialized
        if tmpType is TClassType then
           tmpType:=GetFinalSpecialized(TClassType(tmpType).Ancestor)
        else
           break;
      end
      else
        // TODO: Unwind recursive chain<-chain<- AVariable
        ;

    until tmp<>-1;

    if tmp=-1 then
       result:=nil
    else
    if Derived.Generics.Count>tmp then
       result:=GetDataType(nil,Derived.Generics[tmp] as TData)
    else
    begin
      result:=nil;

      {$IFDEF INTERNAL}
      InternalError(TErrors._MissingParameters,Derived);
      {$ENDIF}
    end;
  end;

var tmp : TType;
begin
  if ADerived=nil then
     result:=nil
  else
  begin
    tmp:=ADerived;

    repeat
      if tmp is TSpecializedType then
         result:=GetGenericTypeOf(TSpecializedType(tmp),AType.Variable)
      else
      if tmp is TArrayType then
         result:=GetGenericTypeOfArray(TArrayType(tmp))
      else
         result:=nil;

      if result=nil then
         if tmp is TClassType then
            tmp:=TClassType(tmp).Ancestor
         else
            break;

    until result<>nil;
  end;
end;

{
class function TChecker.GuessGeneric(const AType:TGenericType; const AClass:TClassType):TType;
begin
  if AClass=nil then
     result:=nil
  else
     result:=FindGenericInClass(AType,AClass);
end;
}

// Loop class type ancestors until generic type is found
class function TChecker.GenericTypeOf(const AParent:TNode; const AData:TData; const AType:TGenericType):TType;

  // Test(Integer).My_Class
  function FindInTypeMember(const ATypeMember:TTypeMember):TType;
  begin
    result:=FindGenericInType(AType,ATypeMember.TheType);
  end;

var tmp : TType;
    tmpNew : TData;
//    tmpClass : TClassType;
    tmpNode : TNode;
begin
  if AData is TMember then
     tmpNew:=TMember(AData).Data
  else
  if AData is TArrayExpression then
     tmpNew:=TArrayExpression(AData).Data
  else
     tmpNew:=AData;

  tmp:=GetDataType(AParent,tmpNew);

  if tmp is TTypeMember then
  begin
    result:=FindInTypeMember(TTypeMember(tmp));

    if result<>nil then
       Exit;
  end;

  if tmp is TClassType then
     result:=FindGenericInType(AType,TClassType(tmp))
  else
  if (tmp is TGenericType) and (AParent<>nil) then
  begin
    tmpNode:=AParent;

    while tmpNode<>nil do
    begin
      if tmpNode is TData then
         tmp:=GetDataType(AParent,TData(tmpNode))
      else
      if tmpNode is TClassType then
         tmp:=TClassType(tmpNode);

      result:=FindGenericInType(AType,tmp);

      if result<>nil then
         Exit;

        {
        if tmp is TClassType then
        begin
          tmpClass:=TClassType(tmp); // GetClassTypeOf(tmpNode);

          result:=GuessGeneric(AType,tmpClass);

          if result<>nil then
             Exit
        end;
        }
      //end;

      tmpNode:=tmpNode.Owner;
    end;

    result:=nil;
  end
  else
  if (tmp is TSpecializedType) {and IsArray(tmp)} then
  begin
    result:=TypeOfGeneric(TSpecializedType(tmp).Generics[0]);   // why 0 ??

    if result is TArrayType then
       result:=TArrayType(result).TheType
  end
  else
     result:=nil;
end;

// Returns the Type of AData, without considering function return values in case AData is a function)
class function TChecker.GetTypeType(const AData:TData):TType;
begin
  if AData is TVariable then
     result:=GetFinalType(TVariable(AData).VariableType)
  else
  if AData is TDataCall then
     result:=GetFinalType(TDataCall(AData).Routine)
  else
  if AData is TTypeCall then
     result:=GetFinalType(TTypeCall(AData).TheType)
  else
  if AData is TMember then
     result:=GetFinalType(GetDataType(nil,TMember(AData).Member))
  else
  begin
    result:=nil;

    {$IFDEF INTERNAL}
    InternalError('Cannot guess type of type: ',AData);
    {$ENDIF}
  end;
end;

// Up class hierarchy until SpecializedType is found: MyClass is Array(T) {}
class function TChecker.FindSpecializedAncestor(AClass:TClassType):TSpecializedType;
begin
  repeat
    if AClass.Ancestor is TSpecializedType then
       Exit(TSpecializedType(AClass.Ancestor))
    else
    if AClass.Ancestor is TClassType then
       AClass:=TClassType(AClass.Ancestor)
    else
       break;

  until False;

  result:=nil;
end;

// Up class hierarchy until GenericType is found: MyClass is T {}
class function TChecker.FindGenericAncestor(AClass:TClassType):TGenericType;
begin
  repeat
    if AClass.Ancestor is TGenericType then
       Exit(TGenericType(AClass.Ancestor))
    else
    if AClass.Ancestor is TClassType then
       AClass:=TClassType(AClass.Ancestor)
    else
       break;

  until False;

  result:=nil;
end;

// TODO:
class function TChecker.FinalTypeOf(const AParent:TNode;
   const AData:TData; const ANode:TNode):TNode;
var tmpType : TType;
    tmp : Integer;
begin
  if AData is TVariable then
     tmpType:=TVariable(AData).VariableType
  else
  begin
    tmpType:=nil;

    {$IFDEF INTERNAL}
    InternalError('Cannot support finaltypeof:',AData)
    {$ENDIF}
  end;

  if tmpType=nil then
     result:=nil  // error !
  else
  if tmpType is TTypeMember then
  begin
    tmpType:=TTypeMember(tmpType).TheType;

    if tmpType is TSpecializedType then
        tmp:=TSpecializedType(tmpType).TheType.Parameters.IndexOf(ANode);

    result:=nil;
  end;
end;

class function TChecker.GetSpecialGenerics(const AParent:TNode;
                const AData:TData; const ASpecial:TSpecializedType):TNodes;
var L,t : Integer;
begin
  {$IFDEF FPC}
  result:=nil;
  {$ENDIf}

  L:=Length(ASpecial.Generics);
  SetLength(result,L);

  for t:=0 to L-1 do
      result[t]:=FinalTypeOf(AParent,AData,ASpecial.Generics[t]);
end;

class procedure TChecker.GetDataTypes(const AParent:TNode; const ALeft,ARight:TData; out L,R:TType);
var tmpType,
    tmpNew : TType;
//    tmpSpecial : TSpecializedType;
//    tmpParams : TNodes;
begin
  L:=GetFinalType(GetDataType(AParent,ALeft));

  if L is TClassType then  // Class(T:Type) { SubClass is T {} }
  begin
    tmpType:=FindGenericAncestor(TClassType(L));

    {
    if tmpType=nil then
    begin
      tmpSpecial:=FindSpecializedAncestor(TClassType(L));

      if tmpSpecial<>nil then
      begin
        tmpParams:=GetSpecialGenerics(AParent,ALeft,tmpSpecial);

        tmpType:=tmpSpecial.TheType;
      end;
    end;
    }

    if tmpType<>nil then
       L:=tmpType;
  end;

  if L is TGenericType then
  begin
    tmpNew:=GenericTypeOf(AParent,ALeft,TGenericType(L));

    if tmpNew=nil then
       L:=TGenericType(L).Variable.VariableType
    else
       L:=tmpNew;
  end;

  // MyFuncType:=DummyFunc
  if L is TFunctionType then
     R:=GetTypeType(ARight)
  else
  begin
    tmpType:=GetDataType(AParent,ARight);

    if tmpType=nil then
       R:=nil // <-- silent, no error
    else
    begin
      R:=GetFinalType(tmpType);

      // Workaround?
      if R=nil then
         R:=GetTypeType(ARight);
    end;
  end;

  if R is TGenericType then
  begin
    tmpNew:=GenericTypeOf(AParent,ALeft,TGenericType(R));

    if tmpNew=nil then
       R:=TGenericType(R).Variable.VariableType
    else
       R:=tmpNew;
  end;

  { Bad idea: ERROR:  Characters:=Characters+T.Characters

  if (R is TArrayType) and (L is TArrayType) then
  begin
    R:=TArrayType(R).ArrayType;
    L:=TArrayType(L).ArrayType;
  end;
  }
end;

function TChecker.CompatibleAssign(const ALeft,ARight:TData; const AParent:TNode):Boolean;
var tmpLeft,
    tmpRight : TType;
begin
  GetDataTypes(AParent,ALeft,ARight,tmpLeft,tmpRight);
  result:=CompatibleAssign(tmpLeft,tmpRight,ALeft,ARight);
end;

function TChecker.NodeIs(const ANode:TNode; const AName:String):Boolean;

  function NodeIsExtender(const AExtender:TExtender):Boolean;
  begin
    result:=(AExtender.Extension<>nil) and
            TextIs(AExtender.Extension.Name,AName);
  end;

begin
  result:=((ANode is TVariable) and TextIs(TVariable(ANode).Name,AName)) or
          ((ANode is TRoutine) and TextIs(TRoutine(ANode).Name,AName)) or
          ((ANode is TNamedType) and TextIs(TNamedType(ANode).Name,AName)) or
          ((ANode is TExtender) and NodeIsExtender(TExtender(ANode)));
end;

// Search bottom-up until class owning ANode is found
class function TChecker.GetClassTypeOf(ANode:TNode):TClassType;
var tmpType : TType;
begin
  while ANode<>nil do
  begin
    if ANode is TType then
    begin
      tmpType:=GetFinalClassType(TType(ANode));

      if tmpType is TClassType then
         Exit(TClassType(tmpType))
    end;

    ANode:=ANode.Owner;
  end;

  result:=nil;
end;

// Search bottom-up until routine of ANode is found
class function TChecker.GetRoutineOf(ANode:TNode):TRoutine;
begin
  result:=nil;

  while ANode<>nil do
  begin
    if (ANode is TExtender) and (TExtender(ANode).Extension is TRoutine) then
       Exit(TExtender(ANode).Extension as TRoutine)
    else
    if ANode is TRoutine then
       Exit(TRoutine(ANode))
    else
    if ANode is TClassType then
       Exit(nil) // routine owned by class, stop here
    else
       ANode:=ANode.Owner;
  end;
end;

procedure TChecker.ClearMagicTypes;
var t: Integer;
begin
  for t:=0 to High(_Types) do _Types[t]:=nil;

  AllMagicTypes:=False;

  TMagic.Clear;
end;

// Integer, Float, Boolean, Text
class function TChecker.IsBasicType(const AType:TType):Boolean;
begin
  result:=(AType=_Types[_Integer]) or
          (AType=_Types[_Float]) or
          (AType=_Types[_Boolean]) or
          (AType=_Types[_Text]);
end;

class function TChecker.IsFloat(const AType:TType):Boolean;
begin
  result:=(AType<>nil) and (AType=_Types[_Float]);
end;

function GetDataTypeNotArray(const AData:TData):TType;
begin
  result:=TChecker.InternalGetDataType(nil,AData,False);
end;

class function TChecker.IsFloat(const AData:TData):Boolean;
var tmp : TType;
begin
  result:=AData is TFloat;

  if not result then
  begin
    tmp:=GetDataTypeNotArray(AData);

    if tmp<>nil then
       result:=IsFloat(tmp);
  end;
end;

class function TChecker.IsHidden(const ANode: TNode): Boolean;
begin
  if ANode is TType then
     result:=TType(ANode).Clauses.Hidden
  else
  if ANode is TVariable then
     result:=TVariable(ANode).Clauses.Hidden
  else
     result:=False;
end;

class function TChecker.IsInlineStruct(const AType: TType): Boolean;
begin
  // unnamed inline class type struct:
  // X: { Name:Text, Age:Integer }
  result:=(AType is TClassType) and (TClassType(AType).Name='');
end;

class function TChecker.IsIntegerOrRangeType(const AData:TData):Boolean;
var tmp : TType;
begin
  result:=AData is TInteger; // or Derives from "Integer" type ?

  if not result then
  begin
    tmp:=GetDataTypeNotArray(AData);

    if tmp<>nil then
       result:=(tmp=_Types[_Integer]) or (tmp is TRangeType);
  end;
end;

class function TChecker.IsInteger(const AData:TData):Boolean;
var tmp : TType;
begin
  result:=AData is TInteger;

  if not result then
  begin
    tmp:=GetDataTypeNotArray(AData);

    if tmp<>nil then
       result:=tmp=_Types[_Integer];
  end;
end;

class function TChecker.IsIntegerOrNumber(const AData:TData):Boolean;
var tmp : TType;
begin
  result:=(AData is TInteger) or (AData is TNumber);

  if not result then
  begin
    tmp:=GetDataTypeNotArray(AData);

    result:=(tmp=_Types[_Integer]) or (tmp=_Types[_Number]);
  end;
end;

class function TChecker.IsLocalVariable(const AVariable:TVariable;
                                        const ABlock:TType;
                                        const ANodes:TNodes):Boolean;

  function GetBlockOwnerOf(const AVariable:TVariable):TType;
  var tmp : TNode;
  begin
    tmp:=AVariable.Owner;

    while tmp<>nil do
    begin
      if tmp is TType then
         Exit(TType(tmp))
      else
         tmp:=tmp.Owner;
    end;

    result:=nil;
  end;

var tmp : TClassType;
begin
  result:=GetBlockOwnerOf(AVariable)=ABlock;

  if not result then
  begin
    tmp:=TChecker.GetClassTypeOf(AVariable);

    // local class field in class constructor?
    result:=(tmp<>nil) and (tmp.Items=ANodes);
  end;
end;

class function TChecker.IsLogicalType(const AType:TType):Boolean;
begin
  result:=(AType=_Types[_Boolean]) or (AType=_Types[_Logical]);
end;

class function TChecker.IsLogical(const AData:TData):Boolean;
var tmp : TType;
begin
  result:=(AData is TBoolean) or (AData is TLogical);

  if not result then
  begin
    tmp:=GetDataTypeNotArray(AData);

    if tmp<>nil then
       result:=IsLogicalType(tmp); // failed experiment: or (tmp is TClassType); // any instance
  end;
end;

var
  StatementReserved : TStringArray;

// TODO: Use a Hash sorted list. Return index instead of Boolean (to reuse Index later)
class function TChecker.IsStatementKeyword(const AName:String):Boolean;
begin
  result:=TextIs(AName,TSyntax._BeginBlock);

  if not result then
     StatementReserved.SortedFind(AName,result,not TChecker.CaseSensitive);
end;

var
  ReservedNoStatement : TStringArray;

// Note: Statement reserved keywords are NOT included here:
class function TChecker.IsReservedKeyword(const AName:String):Boolean;
begin
  StatementReserved.SortedFind(AName,result,not TChecker.CaseSensitive);
end;

class function TChecker.IsAbstract(const R:TRoutine):Boolean;
begin
  result:=(R.Ancestor=nil) and  // <--- eliminate ???
          (R.Items=nil);
end;

// TODO: Maybe try to reuse TEvaluate.AsData instead of this chain of "ifs"
function ClausesOf(const ANode:TNode):TNode;
begin
  if ANode=nil then // <-- incomplete statements might return nil
     result:=ANode
  else
  if ANode is TVariable then
     result:=ANode
  else
  if ANode is TType then
     result:=ANode
  else
  if ANode is TMember then
     result:=ClausesOf(TMember(ANode).Member)
  else
  if ANode is TDataCall then
     result:=ClausesOf(TDataCall(ANode).Routine)
  else
  if ANode is TArrayExpression then
     result:=ClausesOf(TArrayExpression(ANode).Data)
  else
  if ANode is TTypeCall then
     result:=ClausesOf(TTypeCall(ANode).TheType)
  else
  if ANode is TCastingData then
     result:=ClausesOf(TCastingData(ANode).TheType)
  else
  if ANode is TSelf then
     result:=ClausesOf(TChecker.GetClassTypeOf(ANode))
  else
  begin
    result:=ANode;

    {$IFDEF INTERNAL}
    InternalError('Cannot get clauses of ',ANode);
    {$ENDIF}
  end;
end;

class function TChecker.IsCallableVariable(const ANode:TNode):Boolean;
var tmpVar : TVariable;
begin
  result:=ANode is TVariable;

  if result then
  begin
    tmpVar:=TVariable(ANode);

    result:=(TChecker.GetFinalType(tmpVar.VariableType) is TRoutine);

    // Not allowed calling variables that are directly routines.
    // They need to be functiontypes
    if result then
       //if tmpVar.VariableType is TRoutine then
          result:=IsVariableFunctionType(tmpVar);
  end;
end;

class function TChecker.IsFinalData(const AData:TData):Boolean;
var tmp : TNode;
begin
  tmp:=ClausesOf(AData);

  if tmp is TVariable then
     result:=TVariable(tmp).Clauses.Final
  else
  if tmp is TType then
     result:=TType(tmp).Clauses.Final
  else
     result:=False; // ??
end;

class function TChecker.IsFinalRoutine(const ARoutine:TRoutine):Boolean;
var tmp : TType;
begin
  tmp:=TheTypeOf(ARoutine);

  if tmp=nil then
     result:=False
  else
     result:=tmp.Clauses.Final;
end;

class function TChecker.IsSharedData(const AData:TData):Boolean;
var tmp : TNode;
begin
  tmp:=ClausesOf(AData);

  if tmp is TVariable then
     result:=TVariable(tmp).Clauses.Shared
  else
  if tmp is TType then
     result:=TType(tmp).Clauses.Shared
  else
     result:=False; // ??
end;

class function TChecker.TypeOfIndexed(const ANode:TNode):TType;
var tmpType : TType;
begin
  if ANode is TVariable then
  begin
    tmpType:=TVariable(ANode).VariableType;

    if tmpType is TArrayType then
       tmpType:=TArrayType(tmpType).TheType;

    result:=tmpType;
  end
  else
    result:=nil;
end;

class function TChecker.IsArray(const AType:TType):Boolean;
begin
  result:=(AType is TArrayType) or
          (
            (AType is TSpecializedType) and
            (TSpecializedType(AType).TheType=_Types[_Array])
          )
end;

class function TChecker.TypeOfGeneric(const ANode:TNode):TType;
begin
  if ANode is TTypeCall then
     result:=TTypeCall(ANode).TheType
  else
  if ANode is TData then
     result:=GetDataType(nil,TData(ANode))
  else
  begin
    result:=nil;

    {$IFDEF INTERNAL}
    InternalError(TErrors._Unknown,ANode);
    {$ENDIF}
  end;
end;

// Returns type of array expression
class function TChecker.GetDataTypeArray(const AData:TArrayExpression):TType;

  // Guess from first item in the array
  function GuessTypeFromData:TType;
  var tmp : TData;
  begin
    if AData.Parameters[0] is TData then
    begin
      tmp:=TData(AData.Parameters[0]); // Foo:=[a,b,c]  <-- a

      result:=GetDataType(nil,tmp);
      //result:=GetDataTypeAsArray(nil,tmp,GetDataType(tmp));
    end
    else
    begin
      result:=nil;

      {$IFDEF INTERNAL}
      InternalError('Cannot guess array type from first element',AData);
      {$ENDIF}
    end;
  end;

  function ArrayType_In_Hierarchy(AType:TType):TType;
  begin
    repeat
      if IsArray(AType) then
         Exit(AType)
      else
      if AType is TClassType then
         AType:=TClassType(AType).Ancestor
      else
         break;

    until False;

    result:=nil;
  end;

  function Indexed_In_Hierarchy(AClass:TClassType):TType;
  var tmp : TNode;
  begin
    repeat
      tmp:=AClass.DefaultIndexed;

      if tmp=nil then
         if AClass.Ancestor is TClassType then
            AClass:=TClassType(AClass.Ancestor)
         else
            break
      else
         Exit(TypeOfIndexed(tmp))

    until False;

    result:=nil;
  end;

  function InitialDimension(AType:TType):TType;
  begin
    if IsArray(AType) then
       result:=AType
    else
    begin
      result:=nil;

      AType:=GetFinalSpecialized(AType);

      if AType is TClassType then
      begin
        result:=ArrayType_In_Hierarchy(TClassType(AType));

        if result=nil then
           if Indexed_In_Hierarchy(TClassType(AType))<>nil then
              result:=AType;
      end
      else
      if AType is TGenericType then
         result:=AType;
    end;
  end;

  function NextDimension(const AType:TType):TType;
  begin
    if AType is TArrayType then
       result:=TArrayType(AType).TheType
    else
    if (AType is TSpecializedType) and (TSpecializedType(AType).TheType=_Types[_Array]) then
       result:=TypeOfGeneric(TSpecializedType(AType).Generics[0])
    else
    if AType is TClassType then
    begin
      result:=ArrayType_In_Hierarchy(AType);

      if result=nil then
         result:=Indexed_In_Hierarchy(TClassType(AType));
    end
    else
    if AType is TGenericType then
       result:=AType
    else
      result:=nil;
  end;

  function TypeofLastDimension(const AType:TType):TType;
  var t : Integer;
  begin
    result:=InitialDimension(AType);

    {$IFDEF INTERNAL}
    if result=nil then
       InternalError('Array InitialDimension not found',AType);
    {$ENDIF}

    for t:=0 to High(AData.Parameters) do
    begin
      result:=NextDimension(result);

      {$IFDEF INTERNAL}
      if result=nil then
         InternalError('Array NextDimension not found',AType);
      {$ENDIF}
    end;
  end;

var tmp : TData;
begin
  tmp:=AData.Data;

  if (tmp=nil) and (Length(AData.Parameters)=0) then
     result:=_Types[_Nil] // Foo:=[]  special case empty array  --> Foo.Clear
  else
  if tmp=nil then
     result:=GuessTypeFromData
  else
  if tmp is TVariable then
     result:=TypeOfLastDimension(TVariable(tmp).VariableType)
  else
  if tmp is TTypeCall then // Array(Integer[])   <-- sub array
     result:=TTypeCall(tmp).TheType
  else
  if tmp is TDataCall then
     result:=GetDataType(nil,TDataCall(tmp))
  else
  if tmp is TSelf then
  begin
    result:=TChecker.GetClassTypeOf(tmp);

    if result<>nil then
    begin
      result:=InitialDimension(result);

      if result<>nil then  // <-- check for error "Self is not an array"
         result:=TypeOfLastDimension(result);
    end;
  end
  else
  begin
    result:=nil;

    {$IFDEF INTERNAL}
    InternalError('Array Data is not a variable',tmp);
    {$ENDIF}
  end;
end;

class function TChecker.InternalGetDataType(const AParent:TNode;
                                            const AData:TData;
                                            const ValidArrays:Boolean):TType;

  procedure GetDataTypes(const AOperand:TOperand; out L,R:TType);
  begin
    TChecker.GetDataTypes(AParent,AOperand.Left,AOperand.Right,L,R);
  end;

  function OperandResultType(const AOperand:TOperand):TType;
  var L,R : TType;
  begin
    if AOperand is TLogical then
    begin
      if (AOperand is TLogicalAnd) or
         (AOperand is TLogicalOr) or
         (AOperand is TLogicalXor) or
         (AOperand is TCondition) then // <-- for "?" condition use Left
      begin
        if AOperand.Left=nil then
           result:=nil
        else
           result:=GetDataType(AParent,AOperand.Left);

        // failed experiment:
        //
        //if result is TClassType then  // <-- if P and Q ...  // object instances ie: -> if (P<>nil) and (Q<>nil) ...
        //   result:=_Types[_Boolean] // force Boolean
        //
      end
      else
         result:=_Types[_Boolean] // force Boolean
    end
    else
    begin
      GetDataTypes(AOperand,L,R);

      if // (AOperand is TDivision) or <--- integer division !
         TChecker.IsFloat(L) or
         TChecker.IsFloat(R) then // cast upgrade to Float
           result:=_Types[_Float] // Float
      else
         result:=L;
    end;
  end;

begin
  if AData is TVariable then
     result:=TVariable(AData).VariableType
  else
  if AData is TOperand then
     result:=OperandResultType(TOperand(AData))
  else
  if AData is TTypeCall then
  begin
    result:=TheTypeOf(TTypeCall(AData).TheType);

//    if result is TRoutineExtender then
//       result:=TRoutineExtender(result).Routine;

    if result is TRoutine then
       result:=TRoutine(result).Output

    (*
    else
    if result<>_Types[_Nil] then // nil, special case
       result:=nil; // TTypeCall should not return its own type ! ->>  c:MyType  if c=MyType
    *)
  end
  else
  if AData is TCastingData then
     result:=TCastingData(AData).TheType.TheType
  else
  { NO, not here !
  if AData is TVariableCall then
     result:=TVariableCall(AData).Variable.VariableType
  else
  }
  if AData is TDataCall then
     if TDataCall(AData).ResultValue=nil then
        result:=nil
     else
        result:=InternalGetDataType(AParent,TDataCall(AData).ResultValue,ValidArrays)
  else
  if AData is TArrayExpression then
     if ValidArrays then
        result:=GetDataTypeArray(TArrayExpression(AData))
     else
        result:=nil
  else
  if AData is TMember then
  begin
    result:=InternalGetDataType(AParent,TMember(AData).Member,ValidArrays);

    if result is TGenericType then
       if TGenericType(result).FinalType=nil then
          result:=GenericTypeOf(nil,TMember(AData).Data,TGenericType(result));
  end
  else
  if AData is TGroup then
  begin
    if TGroup(AData).Expression=nil then
       result:=nil // <-- silent , no error
    else
       result:=InternalGetDataType(AParent,TGroup(AData).Expression,ValidArrays);
  end
  else
  if AData is TUnary then
     result:=InternalGetDataType(AParent,TUnary(AData).Expression,ValidArrays)
  else
  if AData is TRange then
     result:=InternalGetDataType(AParent,TRange(AData).Min,ValidArrays)
  else
  if AData is TSelf then
     result:=TSelf(AData).TheType
  else
  if AData is TAncestor then
     result:=InternalGetDataType(AParent,TAncestor(AData).DataCall,ValidArrays)
  else
  if AData is TInteger then
  begin
    result:=_Types[_Integer];

    // Only for sys bootstrap:
    if result=nil then
       result:=_Types[_Number]; // Number
  end
  else
  if AData is TBoolean then
     result:=_Types[_Boolean]
  else
  if AData is TFloat then
     result:=_Types[_Float]
  else
  if AData is TText then
  begin
    result:=_Types[_Text];

    // Only for sys bootstrap:
    if result=nil then
       result:=_Types[_Character]; // Character
  end
  else
  begin
    result:=nil; // Error ? (TDataCall.ResultValue can be nil)

    {$IFDEF INTERNAL}
    InternalError('Cannot get data type of: ',AData);
    {$ENDIF}
  end;
end;

class function TChecker.GetDataType(const AParent:TNode; const AData:TData):TType;
begin
  result:=InternalGetDataType(AParent,AData,True);
end;

class function TChecker.GetFinalSpecialized(const AType:TType):TType;
begin
  if AType is TSpecializedType then
     result:=GetFinalSpecialized(TSpecializedType(AType).TheType)
  else
     result:=AType;
end;

class function TChecker.GetFinalType(const AType:TType):TType;
begin
  result:=TheTypeOf(AType);

  if result is TExtender then
     result:=GetFinalType(TExtender(result).Extension)
end;

class function TChecker.GetFinalClassType(const AType:TType):TType;
begin
  result:=TheTypeOf(AType);

  if result is TExtender then
     result:=GetFinalClassType(TExtender(result).TheType)
end;

class function TChecker.TypeHasOutput(const AType:TType):Boolean;
var tmp : TType;
begin
  result:=AType<>nil;

  if result then
  begin
    tmp:=TChecker.GetFinalType(AType);

    result:=(tmp is TRoutine) and (TRoutine(tmp).Output<>nil);
  end;
end;

class function TChecker.IsLiteral(const ANode:TNode):Boolean;
begin
  result:=(ANode is TNumber) or
          (ANode is TText) or
          (ANode is TBoolean);
end;

class function TChecker.ReturnsSomething(const AData:TData):Boolean;
begin
  result:=GetDataType(nil,AData)<>nil;
end;

class function TChecker.GetDataAsArrayType(const AParent:TNode;
                                    const AData:TData; const AType:TType):TType;
var tmp : TNode;
begin
  if AType is TArrayType then
     result:=AType
  else
  if AType is TClassType then
  begin
    result:=GetDataAsArrayType(AParent,AData,TClassType(AType).Ancestor);

    if result=nil then
    begin
      tmp:=TClassType(AType).DefaultIndexed; // C:=Text[123]

      if tmp is TData then
         result:=GetDataType(AParent,TData(tmp))
      else
         result:=nil;
    end;

    {
    if TClassType(AType).Ancestor is TArrayType then
       result:=TClassType(AType).Ancestor
    else
    begin
      tmp:=TClassType(AType).DefaultIndexed; // C:=Text[123]

      if tmp is TData then
         result:=GetDataType(TData(tmp))
      else
         result:=nil;
    end;
    }
  end
  else
  if AType is TGenericType then
     result:=GenericTypeOf(AParent,AData,TGenericType(AType))
  else
     result:=AType;
end;

class function TChecker.GetDataTypeAsArray(const AParent:TNode;
                                    const AData:TData; const AType:TType):TType;

  function FixArrayType(const AArray:TArrayType):TType;
  begin
    result:=AArray.TheType;

    if result is TGenericType then
       result:=GenericTypeOf(AParent,AData,TGenericType(result));
  end;

//var tmp : TNode;
//    tmpType : TType;
begin
  result:=GetDataAsArrayType(AParent,AData,AType);

  if result is TArrayType then
  begin
    result:=FixArrayType(TArrayType(result));

    while result is TArrayType do
          result:=FixArrayType(TArrayType(result))
  end
  else
    result:=AType;

  {
  if AType is TArrayType then
  begin
    tmpType:=GetDataTypeAsArray(AParent,AData,TArrayType(AType).TheType);

    if tmpType=nil then
       result:=AType
    else
       result:=tmpType;
  end
  else
  if AType is TClassType then
  begin
    if TClassType(AType).Ancestor is TArrayType then
       result:=GetDataTypeAsArray(AParent,AData,TArrayType(TClassType(AType).Ancestor).TheType)
    else
    begin
      tmp:=TClassType(AType).DefaultIndexed; // C:=Text[123]

      if tmp is TData then
         result:=GetDataTypeAsArray(AParent,AData,GetDataType(TData(tmp)))
      else
         result:=AType;
    end;
  end
  else
  if AType is TGenericType then
     result:=GetDataTypeAsArray(AParent,AData,GenericTypeOf(AParent,AData,TGenericType(AType)))
  else
     result:=AType;
  }
end;

class function TChecker.BadHiddenAccess(const AParent:TNode; const AData:TData):Boolean;

  function FinalData(const AData:TData):TData;
  begin
    if AData is TVariable then
       result:=AData
    else
    if AData is TMember then
       result:=FinalData(TMember(AData).Member)
    else
    if AData is TArrayExpression then
       Result:=FinalData(TArrayExpression(AData).Data)
    else
       result:=AData;
  end;

  function InScope(const AParent:TNode; const AData:TData):Boolean;
  var tmp : TNode;
  begin
    tmp:=AParent;

    repeat
      result:=tmp=AData.Owner;

      if result then
         Exit
      else
         tmp:=tmp.Owner;

    until tmp=nil;

    result:=False;
  end;

var tmp : TData;
begin
  tmp:=FinalData(AData);

  if tmp is TVariable then
     if TVariable(tmp).Clauses.Hidden then
        if not InScope(AParent,tmp) then
           Exit(True);

  result:=False;
end;

class function TChecker.IsOwner(const AOwner:TNode; ANode:TNode):Boolean;
begin
  repeat
    result:=AOwner=ANode;

    if result then
       Exit
    else
       ANode:=ANode.Owner;

  until ANode=nil;

  result:=False;
end;

class function TChecker.IsNumber(const AData:TData):Boolean;
var tmp : TType;
begin
  result:=AData is TNumber;

  if not result then
  begin
    tmp:=GetDataTypeNotArray(AData);

    result:=CompatibleType(_Types[_Integer],tmp) or
            CompatibleType(_Types[_Float],tmp);  // TODO: replace with _Types[Number]
  end;
end;

class function TChecker.IsOutParameter(const AVariable:TVariable):Boolean;
var tmp : TRoutine;
    P : TNode;
begin
  result:=False;

  tmp:=TChecker.GetRoutineOf(AVariable);

  if tmp<>nil then
     for P in tmp.Parameters do
         if P=AVariable then
            if not AVariable.Clauses.Final then // out parameter
               Exit(True);
end;

class function TChecker.IsText(const AData: TData): Boolean;
var tmp : TType;
begin
  result:=AData is TText;

  if not result then
  begin
    tmp:=GetDataTypeNotArray(AData);

    if tmp<>nil then
       result:=CompatibleType(_Types[_Text],tmp);
  end;
end;

class function TChecker.IsValidLogicalOperator(const AOperand:TOperand):Boolean;
begin
  result:=(AOperand is TIsEqual) or
          (AOperand is TIsNotEqual) or
          (AOperand is TLogicalAnd) or
          (AOperand is TLogicalOr) or
          (AOperand is TLogicalXor);

  if not result then
     if AOperand is TContains then
        result:=AOperand.Right is TArrayExpression; //  := False in [True,False,False]
end;

function TChecker.CheckOperand(const AParent:TNode; const AOperand:TOperand):Boolean;

  function CompatibleArraySingle(const AArray,ASingle:TData):Boolean;
  var tmpLeft,
      tmpRight : TType;
  begin
    tmpLeft:=GetDataTypeAsArray(nil,AArray,GetDataType(AParent,AArray));
    tmpRight:=GetDataType(AParent,ASingle);

    result:=CompatibleAssign(tmpLeft,tmpRight,AArray,ASingle);
  end;

  // TODO: Remove in the future.
  // Very special case for Text vs Character, only for = <> operands
  function Text_Character:Boolean;
  var tmpLeft,
      tmpRight : TType;
  begin
    tmpLeft:=GetDataType(AParent,AOperand.Left);
    tmpRight:=GetDataType(AParent,AOperand.Right);

    result:=CharacterOrText(tmpLeft,tmpRight);
  end;

begin
  if AOperand is TContains then
     result:=CompatibleArraySingle(AOperand.Right,AOperand.Left)
  else
     // Can right be assigned to left?
     result:=CompatibleAssign(AOperand.Left,AOperand.Right,AParent);

  if result then
  begin
    if TChecker.IsLogical(AOperand.Left) and TChecker.IsLogical(AOperand.Right) then
       result:=IsValidLogicalOperator(AOperand)
  end
  else
  begin
    if (AOperand is TCompareLogical) or
       (AOperand is TArithmetic) then // invert left and right
          result:=CompatibleAssign(AOperand.Right,AOperand.Left,nil); // eg: Character=Data ?

    if not result then
       if AOperand is TAddition then
       begin
         // Items[] + Item
         result:=CompatibleArraySingle(AOperand.Left,AOperand.Right);

         // Item + Items[]
         if not result then
            result:=CompatibleArraySingle(AOperand.Right,AOperand.Left);
       end
       else
       if AOperand is TContains then
          result:=CompatibleArraySingle(AOperand.Right,AOperand.Left)  // <-- TODO: Review inverted comparison right=left (necessary?) !
       else
       if (AOperand is TIsEqual) or (AOperand is TIsNotEqual) then
          result:=Text_Character;
  end;
end;

function TChecker.Precedence(const AOperand:TData):Integer;
begin
  if (AOperand is TIsEqual) or
     (AOperand is TIsNotEqual) or
     (AOperand is TIsLower) or
     (AOperand is TIsLowerOrEqual) or
     (AOperand is TIsGreater) or
     (AOperand is TIsGreaterOrEqual) then
        result:=0
  else
  if (AOperand is TAddition) or
     (AOperand is TSubtraction) or
     (AOperand is TLogicalOr) or
     (AOperand is TLogicalXor) then
        result:=1
  else
  if (AOperand is TUnaryNot) then
        result:=3
  else
        result:=2;
end;

function TChecker.FixPrecedence(const AOperand:TOperand):TOperand;
var tmp : TOperand;
    tmpPred : Boolean;
begin
  result:=AOperand;

  if result.Left is TOperand then
     tmpPred:=Precedence(result.Left)>Precedence(result.Right)
  else
     tmpPred:=Precedence(result)>Precedence(result.Right);

  if tmpPred then
  begin
    tmp:=TOperand(result.Right);

    result.Right:=TOperand(result.Right).Left;

    tmp.Left:=result;

    result:=tmp;

    // Swap ownership

    result.Owner:=result.Left.Owner;

    if AOperand.Right.Owner=result then
       AOperand.Right.Owner:=result.Left;

    result.Left.Owner:=result;

    if result.Right<>nil then
       if result.Right.Owner=result.Owner then
          result.Right.Owner:=result;
  end;
end;

class function TChecker.HasName(const ANode:TNode; out AName:String):Boolean;
begin
  result:=True;

  if ANode is TNamedType then
     AName:=TNamedType(ANode).Name
  else
  if ANode is TVariable then
     AName:=TVariable(ANode).Name
  else
     result:=False;
end;

function TChecker.SameRoutine(const A:TRoutine; const B:TRoutineDefinition):Boolean;
var tmpBad : Integer;
begin
  result:=TextIs(A.Name,B.Name);

  if result then
  begin
    result:=ValidParameters(nil,A.Parameters,B.Parameters,tmpBad);

    if result then
       result:=(A.Output=B.Output);
  end;
end;

function TChecker.SameRoutineType(const A,B:TRoutine):Boolean;
var tmpBad : Integer;
begin
  result:=(A.Output=B.Output);

  if result then
     result:=ValidParameters(nil,A.Parameters,B.Parameters,tmpBad);
end;

function TChecker.SameRoutine(const A,B:TRoutine):Boolean;
begin
  result:=TextIs(A.Name,B.Name);

  if result then
     result:=SameRoutineType(A,B);
end;

function TChecker.SameSignature(A,B:TNode):Boolean;
var tmpA,
    tmpB : String;
begin
  if (A is TExtender) and (TExtender(A).Extension is TRoutine) then
     A:=TExtender(A).Extension as TRoutine;

  if (B is TExtender) and (TExtender(B).Extension is TRoutine) then
     B:=TExtender(B).Extension as TRoutine;

  if (A is TRoutine) or (B is TRoutine) then
  begin
    if (A is TRoutine) and (B is TRoutine) then
       result:=SameRoutine(TRoutine(A),TRoutine(B))
    else
       result:=False
  end
  else
  if HasName(A,tmpA) and HasName(B,tmpB) then
     result:=TextIs(tmpA,tmpB)
  else
     result:=False;
end;

class function TChecker.Contains(const ANodes:TNodes; const ANode:TNode):Boolean;
var Node : TNode;
begin
  for Node in ANodes do
      if Node=ANode then
         Exit(True);

  result:=False;
end;

function TChecker.MemberExists(const ANodes:TNodes; const ANode:TNode; out Existing:TNode):Boolean;

  function GetOwnerTypeOf(ANode:TNode):TType;
  begin
    if ANode is TExtender then
       result:=TExtender(ANode).TheType
    else
    begin
      ANode:=ANode.Owner;

      result:=nil;

      while ANode<>nil do
      begin
        if ANode is TExtender then
           Exit(TExtender(ANode).TheType)
        else
        if ANode is TType then
           Exit(TType(ANode))
        else
           ANode:=ANode.Owner;
      end;
    end;
  end;

var Node : TNode;
    tmp : TNode;
begin
  tmp:=GetOwnerTypeOf(ANode);

  if tmp<>nil then

  for Node in ANodes do
      // only for nodes inside the same block / class
      if GetOwnerTypeOf(Node)=tmp then

      if SameSignature(Node,ANode) then
      begin
        Existing:=Node;
        result:=True;
        Exit;
      end;

  result:=False;
end;

// NOTE: MUST BE Alphabetically sorted !!!!! (content, not constant name !)
procedure AddStatementReserved;
begin
  with TSyntax do
  StatementReserved.Add(
    [
      _Reserved_ancestor,
      _Reserved_break,
      _Reserved_catch,
      _Reserved_continue,
      _Reserved_finally,
      _Reserved_for,
      _Reserved_if,
      _Reserved_repeat,
      _Reserved_return,
      _Reserved_try,
      _Reserved_when,
      _Reserved_while
// ???       _Reserved_raise
    ]
    );
end;

// NOTE: MUST BE Alphabetically sorted !!!!! (content, not constant name !)
procedure AddReservedKeywords;
begin
  with TSyntax do
  ReservedNoStatement.Add(
    [
       _Reserved_and,
       _Reserved_else,
       _Reserved_false,
       _Reserved_final,
       _Reserved_hidden,
       _Reserved_in,
       _Reserved_indexed,
       _Reserved_is,
       _Reserved_not,
       _Reserved_or,
       _Reserved_out,
       _Reserved_raise,
       _Reserved_self,
       _Reserved_shared,
//       _Reserved_then,  <--- its blank
       _Reserved_to,
       _Reserved_true,
       _Reserved_until,
       _Reserved_with,
       _Reserved_xor
    ]
    );
end;

initialization
  AddStatementReserved;
  AddReservedKeywords;
end.
