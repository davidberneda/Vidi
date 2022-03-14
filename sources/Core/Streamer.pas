unit Streamer;

{$IFDEF INTERNAL}

{.$DEFINE FIXFORWARD}
{.$DEFINE LOG_REFERENCES}
{$DEFINE CHECK_DUPLICATE_REFERENCES}

{.$DEFINE INTERNAL_NONAMES} // <-- Experimental, do not write strings identifiers

{$ENDIF}

interface

uses
  Classes,
  Sys, AST, ModuleArray, StringArray;

type
  TNodeStreamer=class
  private
    Cached : TModuleArray;
    References : TNodes;
    Stream : TStream;
  protected
    procedure AddNodeReference(const ANode:TNode);
  end;

  TCodeWriter=class(TNodeStreamer)
  private
    Writer : TWriter;

    procedure DoWriteModule(const AModule:TNamedType);

    procedure MaybeWrite(const AOwner,ANode:TNode);
    procedure MaybeWriteData(const AData:TData);

    procedure Write(const AOwner,ANode:TNode); overload;
    procedure Write(const MustReference:Boolean; const ANode:TNode); overload;
    procedure Write(const AOwner:TNode; const ANodes:TNodes); overload;

    procedure WriteAncestor(const Ancestor:TAncestor);
    procedure WriteArrayExpression(const AExpression:TArrayExpression);
    procedure WriteArrayType(const AType:TArrayType);
    procedure WriteAssignment(const Assignment:TAssignment);
    procedure WriteBlock(const ABlock:TBlockStatement);
    procedure WriteCallData(const ACallData:TCallData);
    procedure WriteCastingData(const ACasting:TCastingData);
    procedure WriteClass(const AClass:TClassType);
    procedure WriteClassPart(const AClass:TClassType);
    procedure WriteClauses(const AClauses:TClauses);
    procedure WriteCondition(const ACondition:TCondition);
    procedure WriteData(const AData:TData);
    procedure WriteDataCall(const ADataCall:TDataCall);
    procedure WriteExtender(const AExtender:TExtender);
    procedure WriteFloat(const AFloat: TFloat);
    procedure WriteFor(const AFor:TFor);
    procedure WriteGenericType(const AType:TGenericType);
    procedure WriteGenerics(const AGenerics:TNodes);
    procedure WriteGroup(const AGroup:TGroup);
    procedure WriteID(const ID:Byte);
    procedure WriteIf(const AIf:TIf);
    procedure WriteInlineType(const AOwner:TNode; const AType:TType);
    procedure WriteInteger(const AInteger: TInteger);
    procedure WriteIntegerClass(const AInteger: TIntegerClass);
    procedure WriteMember(const AMember:TMember);
    procedure WriteName(const AName:String);
    procedure WriteOperand(const AOperand:TOperand);
    procedure WriteParametersType(const AType:TParametersType);
    procedure WriteRange(const ARange:TRange);
    procedure WriteRangeType(const AType:TRangeType);
    procedure WriteReference(const ANode:TNode);
    procedure WriteRepeat(const ARepeat:TRepeat);
    procedure WriteRoutine(const ARoutine:TRoutine);
    procedure WriteSpecializedType(const AType:TSpecializedType);
    procedure WriteTry(const ATry:TTry);
    procedure WriteType(const AType:TType);
    procedure WriteTypeMember(const AType:TTypeMember);
    procedure WriteTypeCall(const ATypeCall:TTypeCall);
    procedure WriteUnarySign(const AUnarySign:TUnarySign);
    procedure WriteVariable(const AVariable:TVariable);
    procedure WriteVariableCall(const ACall:TVariableCall);
    procedure WriteWhen(const AWhen:TWhen);
    procedure WriteWhile(const AWhile:TWhile);
    procedure WriteWith(const AWith:TWith);
  public
    Constructor Create(const AStream:TStream);
    Destructor Destroy; override;

    class procedure WriteModule(const AModule:TNamedType; const AStream:TStream); overload; static;
    class procedure WriteModule(const AModule:TNamedType; const AFile:String); overload; static;

    class procedure WriteModules(const AModules:TModuleArray; const AStream:TStream); static;
  end;

  TCodeReader=class(TNodeStreamer)
  private
    CachedNames : TStringArray;
    Reader : TReader;

    procedure DoReadDataCall(const ACall:TDataCall);
    procedure DoReadGroup(const AGroup:TGroup);
    function DoReadModule:TNamedType;
    function DoReadModules:TModuleArray;
    procedure DoReadRoutine(const ARoutine:TRoutine);

    function MaybeRead:TNode;
    function MaybeReadData:TData;

    function NewClass:TClassType;
    function NewExtender:TExtender;
    function NewFunctionType:TFunctionType;
    function NewRoutine:TRoutine;
    function NewVariable: TVariable;

    function ReadName:String;

    function Read:TNode; overload;
    procedure Read(const AOwner:TNode; var ANodes:TNodes); overload;

    function ReadAncestor:TAncestor;
    function ReadArrayExpression:TArrayExpression;
    procedure ReadArrayType(const AType:TArrayType);
    function ReadAssignment:TAssignment;
    function ReadBlock:TBlockStatement;
    function ReadCallData:TCallData;
    function ReadCastingData:TCastingData;
    procedure ReadClass(const AClass:TClassType);
    procedure ReadClassPart(const AClass: TClassType);
    function ReadClauses:TClauses;
    function ReadCondition:TCondition;
    function ReadData:TData;
    function ReadDataCall:TDataCall;
    function ReadFloat: TFloat;
    function ReadFor:TFor;
    function ReadGroup:TGroup;
    function ReadID:Byte;
    function ReadIf:TIf;
    function ReadInlineType(const AOwner:TNode):TType;
    function ReadInteger: TInteger;
    function ReadIntegerClass: TIntegerClass;
    procedure ReadLeftRight(const AOperand: TOperand);
    function ReadMember:TMember;
    function ReadOperand(const AOperand:Byte):TOperand;
    procedure ReadParametersType(const AType: TParametersType);
    function ReadRange:TRange;
    function ReadRangeType:TRangeType;
    function ReadReference:TNode;
    function ReadRepeat:TRepeat;
    function ReadReturn: TReturn;
    procedure ReadRoutine(const ARoutine:TRoutine);
    function ReadSelf: TSelf;
    function ReadSpecializedType:TSpecializedType;
    function ReadTry: TTry;
    procedure ReadType(const AType:TType);
    function ReadTypeCall:TTypeCall;
    function ReadTypeMember:TTypeMember;
    function ReadUnaryNot: TUnaryNot;
    function ReadUnarySign:TUnarySign;
    procedure ReadVariable(const AVariable:TVariable);
    function ReadVariableCall:TVariableCall;
    function ReadWhen:TWhen;
    function ReadWhile:TWhile;
    function ReadWith:TWith;
  public
    Constructor Create(const AStream:TStream);
    Destructor Destroy; override;

    class function ReadModule(const AStream:TStream; out Embedded:TModuleArray):TType; static;
    class function ReadModules(const AStream:TStream):TModuleArray; static;
  end;

implementation

uses
  {$IFDEF INTERNAL}
  Internal,
  {$ENDIF}
  SysUtils, Magic, Utils.AST;

{$IFDEF INTERNAL}
procedure WrongID(const ID:Byte; const AText:String);
begin
  InternalError('Internal, wrong ID: '+IntToStr(ID)+' '+AText,nil);
end;
{$ENDIF}

const
  ClassType_ID = 2;
  Routine_ID   = 4;
  FunctionType_ID = 56;

  Ref_ID       = 254;
  No_ID        = 255;

function NodeID(const ANode:TNode):Byte;
begin
  if ANode is TVariable then result:=1 else

  if ANode is TIntegerClass then result:=6 else // before TClassType !

  if ANode is TClassType then result:=ClassType_ID else
//  if ANode is TGenericType then result:=3 else

  if ANode is TFunctionType then result:=56 else // before TRoutine !

  if ANode is TRoutine then result:=Routine_ID else
  if ANode is TExtender then result:=5 else

  if ANode is TTypeCall then result:=7 else
  if ANode=TMagic.MagicTrue then result:=8 else
  if ANode=TMagic.MagicFalse then result:=9 else
  // if ANode is TComment then result:=10 else
  if ANode is TFloat then result:=11 else
//  if ANode is TArrayType then result:=12 else
  if ANode is TReturn then result:=13 else

  if ANode is TAddition then result:=14 else
  if ANode is TSubtraction then result:=15 else
  if ANode is TMultiplication then result:=16 else
  if ANode is TDivision then result:=17 else
  if ANode is TLogicalAnd then result:=18 else
  if ANode is TLogicalOr then result:=19 else
  if ANode is TLogicalXor then result:=20 else
  if ANode is TIsEqual then result:=21 else
  if ANode is TIsNotEqual then result:=22 else
  if ANode is TIsLower then result:=23 else
  if ANode is TIsLowerOrEqual then result:=24 else
  if ANode is TContains then result:=25 else
  if ANode is TIsGreater then result:=26 else
  if ANode is TIsGreaterOrEqual then result:=27 else

  if ANode is TCondition then result:=28 else
  if ANode is TInteger then result:=29 else
  if ANode is TVariableCall then result:=57 else // <-- before TDataCall !
  if ANode is TDataCall then result:=30 else
  if ANode is TAssignment then result:=31 else
//  if ANode is TManyValues then result:=32 else
  if ANode is TArrayExpression then result:=33 else
  if ANode is TIf then result:=34 else
  if ANode is TFor then result:=35 else
  if ANode is TWhile then result:=36 else
  if ANode is TRepeat then result:=37 else
  if ANode is TWhen then result:=38 else
  if ANode is TMember then result:=39 else
//  if ANode is TRangeType then result:=40 else
  if ANode is TText then result:=41 else
  if ANode is TCallData then result:=42 else
  if ANode is TBreak then result:=43 else
  if ANode is TContinue then result:=44 else
  if ANode is TUnarySign then result:=45 else
  if ANode is TUnaryNot then result:=46 else
  if ANode is TWith then result:=47 else
  if ANode is TGroup then result:=48 else
  if ANode is TCastingData then result:=49 else
  if ANode is TRange then result:=50 else
  if ANode is TBlockStatement then result:=51 else
  if ANode is TSelf then result:=52 else
  if ANode is TAncestor then result:=53 else
  if ANode is TTry then result:=54 else
  if ANode is TTypeMember then result:=55 else

  // 56, 57 are used !

  //  if ANode is TSpecializedType then result:=58 else

  begin
    result:=No_ID;

    {$IFDEF INTERNAL}
    InternalError('Node not supported at NodeID',ANode);
    {$ENDIF}
  end;

  {$IFDEF INTERNAL}
  if (result=3) or
     (result=12) or
     (result=32) or
     (result=40)
     then
       InternalError('Wrong Node at NodeID',ANode);
  {$ENDIF}
end;

{ TNodeStreamer }

procedure TNodeStreamer.AddNodeReference(const ANode: TNode);
begin
  {$IFDEF CHECK_DUPLICATE_REFERENCES}
  if References.IndexOf(ANode)<>-1 then
     InternalError('Duplicate reference: ',ANode);
  {$ENDIF}

  References.Add(ANode);
end;

{ TCodeWriter }

Constructor TCodeWriter.Create(const AStream:TStream);
begin
  inherited Create;

  Stream:=AStream;
  Writer:=TWriter.Create(Stream,65536);
end;

Destructor TCodeWriter.Destroy;
begin
  Writer.Free;
  inherited;
end;

procedure TCodeWriter.WriteClauses(const AClauses:TClauses);
begin
  Writer.WriteBoolean(AClauses.Indexed);
  Writer.WriteBoolean(AClauses.Hidden);
  Writer.WriteBoolean(AClauses.Shared);
  Writer.WriteBoolean(AClauses.Final);
end;

procedure TCodeWriter.WriteName(const AName:String);
begin
  {$IFDEF INTERNAL_NONAMES}
  {$ELSE}
  Writer.WriteString(AName);
  {$ENDIF}
end;

procedure TCodeWriter.WriteType(const AType:TType);
begin
  WriteClauses(AType.Clauses);
  Write(AType,AType.Items);
end;

procedure TCodeWriter.WriteReference(const ANode:TNode);

  {$IFDEF INTERNAL}
  procedure ReferenceError;
  begin
    InternalError('Cannot find node reference',ANode);
  end;
  {$ENDIF}

var tmp : Integer;
begin
  if ANode=nil then
     Writer.WriteInteger(-1)
  else
  begin
    tmp:=References.IndexOf(ANode);

    if tmp=-1 then
    begin
      {$IFDEF FIXFORWARD}
      // TODO !! Temporary "fix" for FORWARD routines (eg: parsing.vidi Structures.vidi GetDataType)
      if (ANode is TRoutine) and
         (
           (TRoutine(ANode).Name='GetDataType') or
           (TRoutine(ANode).Name='GetExpression') or
           (TRoutine(ANode).Name='GetExpressions')

         ) then
         Writer.WriteInteger(tmp)
      else
      {$ENDIF}

      {$IFDEF INTERNAL}
      ReferenceError;
      {$ENDIF}
    end
    else
      Writer.WriteInteger(tmp);
  end;
end;

procedure TCodeWriter.WriteData(const AData:TData);
begin
  Write(nil,AData);
end;

procedure TCodeWriter.DoWriteModule(const AModule:TNamedType);
begin
  Cached.Add(AModule);

  AddNodeReference(AModule);

  Writer.WriteString(AModule.Name);
  WriteClauses(AModule.Clauses);
  Write(AModule,AModule.Items);
end;

{$IFDEF LOG_REFERENCES}
procedure LogReferences(const ANodes:TNodes; const Suffix:String);
var S : TStrings;
    t : Integer;
begin
  S:=TStringList.Create;
  try
    for t:=0 to High(ANodes) do
        S.Add(IntToStr(t)+' '+TASTParser.NodeName(ANodes[t]));

    S.SaveToFile('log_references'+Suffix+'.txt');
  finally
    S.Free;
  end;
end;
{$ENDIF}

class procedure TCodeWriter.WriteModule(const AModule: TNamedType;
                                        const AStream: TStream);
var tmp : TCodeWriter;
begin
  tmp:=TCodeWriter.Create(AStream);
  try
    tmp.DoWriteModule(AModule);
    tmp.Writer.FlushBuffer;

    {$IFDEF LOG_REFERENCES}
    LogReferences(tmp.References,'');
    {$ENDIF}
  finally
    tmp.Free;
  end;
end;

class procedure TCodeWriter.WriteModule(const AModule: TNamedType;
  const AFile: String);
var F : TFileStream;
begin
  F:=TFileStream.Create(AFile,fmCreate);
  try
    TCodeWriter.WriteModule(AModule,F);
  finally
    F.Free;
  end;
end;

class procedure TCodeWriter.WriteModules(const AModules: TModuleArray;
  const AStream: TStream);
var tmp : TCodeWriter;
    M : TNode;
begin
  tmp:=TCodeWriter.Create(AStream);
  try
    tmp.Writer.WriteInteger(Length(AModules));

    for M in AModules do
        if M is TNamedType then
           tmp.DoWriteModule(TNamedType(M));

    tmp.Writer.FlushBuffer;
  finally
    tmp.Free;
  end;
end;

procedure TCodeWriter.WriteOperand(const AOperand:TOperand);
begin
  WriteData(AOperand.Left);
  WriteData(AOperand.Right);
end;

procedure TCodeWriter.WriteCallData(const ACallData: TCallData);
begin
  WriteData(ACallData.Value);
end;

procedure TCodeWriter.WriteParametersType(const AType:TParametersType);
begin
  Write(AType,AType.Parameters); // <-- before WriteType !
  WriteType(AType);
end;

procedure TCodeWriter.WriteClassPart(const AClass:TClassType);
begin
  WriteName(AClass.Name);
  WriteParametersType(AClass);

  WriteInlineType(AClass,AClass.Ancestor);
end;

procedure TCodeWriter.WriteGenerics(const AGenerics:TNodes);
var N : TNode;
begin
  Writer.WriteInteger(Length(AGenerics));

  for N in AGenerics do
      Write(True,N);
end;

procedure TCodeWriter.WriteClass(const AClass:TClassType);
begin
  WriteClassPart(AClass);
end;

procedure TCodeWriter.WriteCondition(const ACondition:TCondition);
begin
  WriteData(ACondition.Condition);
  WriteOperand(ACondition);
end;

procedure TCodeWriter.MaybeWrite(const AOwner,ANode:TNode);
begin
  Writer.WriteBoolean(ANode<>nil);

  if ANode<>nil then
     Write(AOwner,ANode);
end;

procedure TCodeWriter.MaybeWriteData(const AData:TData);
begin
  Writer.WriteBoolean(AData<>nil);

  if AData<>nil then
     WriteData(AData);
end;

procedure TCodeWriter.WriteArrayType(const AType:TArrayType);
begin
  WriteType(AType);
  WriteInlineType(AType,AType.TheType); // <-- to support "array of array of..."

  MaybeWriteData(AType.Size);

// ?? runtime only?  Write(AType,AType.Values);
end;

procedure TCodeWriter.WriteRange(const ARange:TRange);
begin
  WriteData(ARange.Min);
  WriteData(ARange.Max);
end;

procedure TCodeWriter.WriteRangeType(const AType:TRangeType);
begin
  WriteType(AType);
  WriteRange(AType.Range);
end;

procedure TCodeWriter.WriteArrayExpression(const AExpression:TArrayExpression);
begin
  MaybeWriteData(AExpression.Data);
  Write(nil,AExpression.Parameters);
end;

procedure TCodeWriter.WriteRoutine(const ARoutine:TRoutine);
begin
  WriteName(ARoutine.Name);
  WriteParametersType(ARoutine);
  WriteReference(ARoutine.Ancestor);
  WriteInlineType(ARoutine,ARoutine.Output);
  WriteReference(ARoutine.ForwardFrom);
  Writer.WriteBoolean(ARoutine.AccessGlobals);
end;

procedure TCodeWriter.WriteTry(const ATry:TTry);
var C : TCatch;
begin
  Write(ATry,ATry.Block);

  Writer.WriteInteger(Length(ATry.Catch));

  for C in ATry.Catch do
  begin
    MaybeWrite(C,C.Error);
    MaybeWrite(C,C.Block);
  end;

  MaybeWrite(ATry,ATry.TheFinally);
end;

procedure TCodeWriter.WriteExtender(const AExtender:TExtender);
begin
  WriteName(AExtender.Name);
  WriteType(AExtender);

  WriteInlineType(AExtender,AExtender.TheType);
//  WriteReference(AExtender.TheType);

  AddNodeReference(AExtender.Extension);

  if AExtender.Extension is TFunctionType then // <-- before TRoutine !
  begin
    WriteID(FunctionType_ID);
    WriteRoutine(AExtender.Extension as TRoutine);
  end
  else
  if AExtender.Extension is TRoutine then
  begin
    WriteID(Routine_ID);
    WriteRoutine(AExtender.Extension as TRoutine);
  end
  else
  begin
    WriteID(ClassType_ID);
    WriteClass(AExtender.Extension as TClassType);
  end;
end;

procedure TCodeWriter.WriteTypeCall(const ATypeCall:TTypeCall);
var tmp : Boolean;
begin
  tmp:=TASTUtils.IsLambda(ATypeCall.TheType);

  Writer.WriteBoolean(tmp);

  if tmp then
     WriteRoutine(ATypeCall.TheType as TRoutine) // <-- lambda
  else
  begin
    WriteInlineType(ATypeCall,ATypeCall.TheType);
    //WriteReference(ATypeCall.TheType);
    Write(ATypeCall,ATypeCall.Parameters);
  end;
end;

procedure TCodeWriter.WriteDataCall(const ADataCall:TDataCall);
//var tmp : Boolean;
begin
  Write(ADataCall,ADataCall.Routine);

  {
  // prevent wrong re-owning at ReadDataCall (eg: Fibonacci Recursive is called twice when it has no owner yet)
  tmp:=ADataCall.Routine.Owner=ADataCall;
  Writer.WriteBoolean(tmp);
  }

  Write(ADataCall,ADataCall.Parameters);
end;

procedure TCodeWriter.WriteGenericType(const AType:TGenericType);
begin
  WriteReference(AType.Variable);
//  WriteName(AType.Variable.Name);
  WriteInlineType(AType,AType.FinalType);
end;

procedure TCodeWriter.WriteSpecializedType(const AType:TSpecializedType);
begin
  WriteType(AType);
  WriteInlineType(AType,AType.TheType);
  WriteGenerics(AType.Generics);
end;

procedure TCodeWriter.WriteVariableCall(const ACall:TVariableCall);
begin
  WriteReference(ACall.Variable);
  WriteDataCall(ACall);
end;

procedure TCodeWriter.WriteID(const ID:Byte);
begin
  Writer.Write(ID,1); // One byte
end;

procedure TCodeWriter.WriteInlineType(const AOwner:TNode; const AType:TType);

  procedure WriteInlineClass;
  var tmpInline : Boolean;
  begin
    WriteID(ClassType_ID);

    // Three situations here:
    // 1) Foo is Bar(Integer)   // generic inline
    // 2) X : { Name:Text } // inline structs
    // 3) X : { Name:Text } [ ]  // array of inline struct

    tmpInline:=(TClassType(AType).Name='') and
       (
         (AOwner is TVariable) or (AOwner is TArrayType)
       );

    Writer.WriteBoolean(tmpInline);

    if tmpInline then
    begin
      Write(AType,AType.Items);

      AddNodeReference(AType); // <-- just in case other multiple variables share the same inline class type
    end
    else
    begin
      {$IFDEF INTERNAL}
      InternalError('Cannot write inline type of: ',AType);
      {$ENDIF}

      {AddNodeReference(AType);
      WriteReference(TClassType(AType).Ancestor);
      Write(AType,TClassType(AType).Generics);}
    end;
  end;

begin
  if AType is TManyValues then // <-- Before TArrayType
  begin
    WriteID(32);
    WriteArrayType(TArrayType(AType));
  end
  else
  if AType is TArrayType then
  begin
    WriteID(12);
    WriteArrayType(TArrayType(AType));
  end
  else
  if AType is TRangeType then
  begin
    WriteID(40);
    WriteRangeType(TRangeType(AType));
  end
  else
  if AType is TGenericType then
  begin
    WriteID(3);
    WriteGenericType(TGenericType(AType));
  end
  else
  if AType is TTypeMember then
  begin
    WriteID(55);
    WriteTypeMember(TTypeMember(AType));
  end
  else
  if AType is TSpecializedType then
  begin
    WriteID(58);
    WriteSpecializedType(TSpecializedType(AType));
  end
  else
  if (AType<>nil) and (AOwner=AType.Owner) then
  begin
    if AType is TClassType then
       WriteInlineClass
    {$IFDEF INTERNAL}
    else
       InternalError('Cannot write inline type of: ',AType);
    {$ENDIF}
  end
  else
  begin
    WriteID(Ref_ID);
    WriteReference(AType);
  end;
end;

procedure TCodeWriter.WriteInteger(const AInteger: TInteger);
begin
  Writer.WriteInteger(AInteger.Value);
  Writer.WriteString(AInteger.Text);
  Writer.Write(AInteger.Base,1);
end;

procedure TCodeWriter.WriteFloat(const AFloat: TFloat);
begin
  Writer.WriteFloat(AFloat.Value);
  Writer.WriteString(AFloat.Text);
end;

procedure TCodeWriter.WriteIntegerClass(const AInteger: TIntegerClass);
begin
  WriteClass(AInteger);
  WriteRangeType(AInteger.Size);
end;

procedure TCodeWriter.WriteVariable(const AVariable:TVariable);
begin
  WriteClauses(AVariable.Clauses);
  WriteName(AVariable.Name);

  Writer.WriteBoolean(AVariable.TypeInferred);
  WriteInlineType(AVariable,AVariable.VariableType);

  MaybeWriteData(AVariable.ValueData);
end;

procedure TCodeWriter.WriteBlock(const ABlock:TBlockStatement);
begin
  // Not necessary to write ABlock.Block.Clauses etc
  Write(ABlock.Block,ABlock.Block.Items);
end;

procedure TCodeWriter.WriteIf(const AIf:TIf);
begin
  WriteData(AIf.Condition);
  MaybeWrite(AIf,AIf.ThenBlock);
  MaybeWrite(AIf,AIf.ElseBlock);
end;

procedure TCodeWriter.WriteFor(const AFor:TFor);
begin
  MaybeWrite(AFor,AFor.Counter);
  MaybeWriteData(AFor.First);
  MaybeWriteData(AFor.Last);
  MaybeWriteData(AFor.InExpression);

  Write(AFor,AFor.Block);
end;

procedure TCodeWriter.WriteWhile(const AWhile:TWhile);
begin
  WriteData(AWhile.Condition);
  Write(AWhile,AWhile.Block);
end;

procedure TCodeWriter.WriteRepeat(const ARepeat:TRepeat);
begin
  MaybeWriteData(ARepeat.Condition);
  Write(ARepeat,ARepeat.Block);
end;

procedure TCodeWriter.WriteWhen(const AWhen:TWhen);
var W : TWhenItem;
begin
  WriteData(AWhen.Expression);

  Writer.WriteInteger(Length(AWhen.Items));

  for W in AWhen.Items do
  begin
    WriteData(W.Expression);
    Write(W,W.Block);
  end;

  MaybeWrite(AWhen,AWhen.ElseBlock);
end;

procedure TCodeWriter.WriteMember(const AMember:TMember);
begin
  WriteData(AMember.Data);
  WriteData(AMember.Member);
end;

procedure TCodeWriter.WriteUnarySign(const AUnarySign:TUnarySign);
begin
  WriteData(AUnarySign.Expression);
  Writer.WriteBoolean(AUnarySign.Positive);
end;

procedure TCodeWriter.WriteAncestor(const Ancestor:TAncestor);
begin
  WriteReference(Ancestor.Ancestor);
  WriteDataCall(Ancestor.DataCall);
end;

procedure TCodeWriter.WriteGroup(const AGroup:TGroup);
begin
  WriteData(AGroup.Expression);
end;

procedure TCodeWriter.WriteTypeMember(const AType:TTypeMember);
begin
  WriteInlineType(AType,AType.TheType);
  WriteInlineType(AType,AType.Member);
end;

procedure TCodeWriter.WriteWith(const AWith:TWith);
var tmp : TNamedType;
    tmpCached : Boolean;
begin
  Writer.WriteBoolean(AWith.SystemModule);
  Writer.WriteString(AWith.Alias);

  tmp:=TASTUtils.ModuleOfWith(AWith);

  tmpCached:=Cached.IndexOf(tmp)<>-1;
  Writer.WriteBoolean(tmpCached);

  if tmpCached then
     Writer.WriteString(tmp.Name)
  else
     DoWriteModule(tmp);

  Writer.WriteBoolean(tmp<>AWith.Module);

  if tmp<>AWith.Module then
     WriteReference(AWith.Module);
end;

procedure TCodeWriter.WriteCastingData(const ACasting:TCastingData);
begin
  WriteTypeCall(ACasting.TheType);
  WriteData(ACasting.Data);
end;

procedure TCodeWriter.WriteAssignment(const Assignment:TAssignment);
begin
  Writer.WriteInteger(Ord(Assignment.Arithmetic));
  WriteData(Assignment.Variable);
  WriteData(Assignment.Value);
end;

procedure TCodeWriter.Write(const MustReference:Boolean; const ANode:TNode);

  {$IFDEF INTERNAL}
  procedure AddReferenceError;
  begin
    InternalError('AddReference NodeWriter',ANode);
  end;

  procedure ArrayTypeError;
  begin
    InternalError('ArrayType NodeWriter',ANode);
  end;

  procedure RangeTypeError;
  begin
    InternalError('RangeType NodeWriter',ANode);
  end;
  {$ENDIF}

  procedure AddReference(const ID:Byte);
  begin
    AddNodeReference(ANode);

    case ID of
                1 : WriteVariable(TVariable(ANode));
     ClassType_ID : WriteClass(TClassType(ANode));
                3 : WriteVariable(TGenericType(ANode).Variable);
  FunctionType_ID,
       Routine_ID : WriteRoutine(TRoutine(ANode));
                5 : WriteExtender(TExtender(ANode));
                6 : WriteIntegerClass(TIntegerClass(ANode));

    {$IFDEF INTERNAL}
    else
      AddReferenceError;
    {$ENDIF}
    end;
  end;

var tmp : Byte;
begin
  tmp:=NodeID(ANode);

  if ((tmp>=1) and (tmp<=4)) or
     (tmp=FunctionType_ID) then // Extenders do not write references !
     if MustReference then
        tmp:=Ref_ID;

  WriteID(tmp);

  case tmp of
     1..6 : AddReference(tmp);
        7 : WriteTypeCall(TTypeCall(ANode));
      8,9 : ; // True/False
//       10 : WriteName(TComment(ANode).Text);
       11 : WriteFloat(TFloat(ANode));
       12 : {$IFDEF INTERNAL}ArrayTypeError{$ENDIF}; //WriteArrayType(TArrayType(ANode));
       13 : MaybeWriteData(TReturn(ANode).Value);
   14..27 : WriteOperand(TOperand(ANode));
       28 : WriteCondition(TCondition(ANode));
       29 : WriteInteger(TInteger(ANode));
       30 : WriteDataCall(TDataCall(ANode));
       31 : WriteAssignment(TAssignment(ANode));
       32 : {$IFDEF INTERNAL}ArrayTypeError{$ENDIF}; //WriteArrayType(TArrayType(ANode));
       33 : WriteArrayExpression(TArrayExpression(ANode));
       34 : WriteIf(TIf(ANode));
       35 : WriteFor(TFor(ANode));
       36 : WriteWhile(TWhile(ANode));
       37 : WriteRepeat(TRepeat(ANode));
       38 : WriteWhen(TWhen(ANode));
       39 : WriteMember(TMember(ANode));
       40 : {$IFDEF INTERNAL}RangeTypeError{$ENDIF}; //WriteRangeType(TRangeType(ANode));
       41 : Writer.WriteString(TText(ANode).Value);
       42 : WriteCallData(TCallData(ANode));
       43 : ; // break
       44 : ; // continue
       45 : WriteUnarySign(TUnarySign(ANode));
       46 : WriteGroup(TUnaryNot(ANode));
       47 : WriteWith(TWith(ANode));
       48 : WriteGroup(TGroup(ANode));
       49 : WriteCastingData(TCastingData(ANode));
       50 : WriteRange(TRange(ANode));
       51 : WriteBlock(TBlockStatement(ANode));
       52 : WriteReference(TSelf(ANode).TheType);
       53 : WriteAncestor(TAncestor(ANode));
       54 : WriteTry(TTry(ANode));
       55 : WriteTypeMember(TTypeMember(ANode));
       56 : AddReference(tmp);
       57 : WriteVariableCall(TVariableCall(ANode));

   Ref_ID : WriteReference(ANode);
  end;
end;

procedure TCodeWriter.Write(const AOwner,ANode:TNode);
begin
  {$IFDEF INTERNAL}
  if ANode=nil then
     InternalError('CodeWriter Write ANode=nil',nil);
  {$ENDIF}

  Write(ANode.Owner<>AOwner,ANode);
end;

procedure TCodeWriter.Write(const AOwner:TNode; const ANodes:TNodes);
var N : TNode;
begin
  Writer.WriteInteger(Length(ANodes));

  for N in ANodes do
      Write(AOwner,N);
end;

{ TCodeReader }

constructor TCodeReader.Create(const AStream: TStream);
begin
  inherited Create;

  Stream:=AStream;
  Reader:=TReader.Create(Stream,65536);
end;

destructor TCodeReader.Destroy;
begin
  Reader.Free;
  inherited;
end;

function TCodeReader.ReadName:String;
begin
  {$IFDEF INTERNAL_NONAMES}
  result:='';
  {$ELSE}
  result:=Reader.ReadString;
  {$ENDIF}
end;

procedure TryOwner(const AOwner,AChild:TNode);
begin
  if AChild<>nil then
     if AChild.Owner=nil then
        AChild.Owner:=AOwner;
end;

procedure TCodeReader.ReadLeftRight(const AOperand: TOperand);
begin
  AOperand.Left:=ReadData;
  TryOwner(AOperand,AOperand.Left);

  AOperand.Right:=ReadData;
  TryOwner(AOperand,AOperand.Right);
end;

function TCodeReader.ReadOperand(const AOperand: Byte):TOperand;
type
  TOperandClass=class of TOperand;

const
  OperandClasses:Array[14..27] of TOperandClass=
   (
     TAddition,
     TSubtraction,
     TMultiplication,
     TDivision,
     TLogicalAnd,
     TLogicalOr,
     TLogicalXor,
     TIsEqual,
     TIsNotEqual,
     TIsLower,
     TIsLowerOrEqual,
     TContains,
     TIsGreater,
     TIsGreaterOrEqual
   );

begin
  result:=OperandClasses[AOperand].Create;
  ReadLeftRight(result);
end;

function TCodeReader.ReadRange: TRange;
begin
  result:=TRange.Create;

  result.Min:=ReadData;
  TryOwner(result,result.Min);

  result.Max:=ReadData;
  TryOwner(result,result.Max);
end;

function TCodeReader.ReadRangeType: TRangeType;
begin
  result:=TRangeType.Create;
  ReadType(result);

  result.Range:=ReadRange;
  TryOwner(result,result.Range);
end;

function TCodeReader.ReadReference: TNode;
var tmp : Integer;
begin
  tmp:=Reader.ReadInteger;

  if tmp=-1 then
     result:=nil
  else
  begin
    {$IFDEF LOG_REFERENCES}
    if tmp=259 then
       LogReferences(References,'_');
    {$ENDIF}

    result:=References[tmp];
  end;
end;

function TCodeReader.ReadRepeat: TRepeat;
begin
  result:=TRepeat.Create;

  result.Condition:=MaybeReadData;
  TryOwner(result,result.Condition);

  result.Block:=Read as TStatement;
  TryOwner(result,result.Block);
end;

procedure TCodeReader.ReadRoutine(const ARoutine: TRoutine);
begin
  ARoutine.Name:=ReadName;
  ReadParametersType(ARoutine);
  ARoutine.Ancestor:=ReadReference as TRoutine;

  ARoutine.Output:=ReadInlineType(ARoutine);

  ARoutine.ForwardFrom:=ReadReference as TRoutine;

  if ARoutine.ForwardFrom<>nil then
     ARoutine.ForwardFrom.ForwardTo:=ARoutine;

  ARoutine.AccessGlobals:=Reader.ReadBoolean;
end;

function TCodeReader.NewFunctionType:TFunctionType;
begin
  result:=TFunctionType.Create;
  DoReadRoutine(result);
end;

procedure TCodeReader.DoReadRoutine(const ARoutine:TRoutine);
begin
  AddNodeReference(ARoutine);
  ReadRoutine(ARoutine);
end;

function TCodeReader.NewRoutine:TRoutine;
begin
  result:=TRoutine.Create;
  DoReadRoutine(result);
end;

function TCodeReader.NewExtender:TExtender;
begin
  result:=TExtender.Create;
  AddNodeReference(result);

  result.Name:=ReadName;
  ReadType(result);

  result.TheType:=ReadInlineType(result);

  //TryOwner(result,result.TheType);
  //result.TheType:=ReadReference as TType;

  case ReadID of
 FunctionType_ID : result.Extension:=NewFunctionType;
      Routine_ID : result.Extension:=NewRoutine;
  else
     result.Extension:=NewClass;
  end;

  result.Extension.Owner:=result;
//  TryOwner(result,result.Extension);
end;

function TCodeReader.NewClass:TClassType;
begin
  result:=TClassType.Create;
  AddNodeReference(result);
  ReadClass(result);
end;

procedure TCodeReader.ReadType(const AType: TType);
begin
  AType.Clauses:=ReadClauses;

  Read(AType,AType.Items);
end;

procedure TCodeReader.ReadParametersType(const AType: TParametersType);
begin
  Read(AType,AType.Parameters); // <-- Before Items (needed References !)
  ReadType(AType);
end;

function TCodeReader.ReadTypeCall: TTypeCall;
begin
  result:=TTypeCall.Create;

  if Reader.ReadBoolean then // lambda
  begin
    result.TheType:=TRoutine.Create;
    result.TheType.Owner:=result;

    ReadRoutine(TRoutine(result.TheType));
  end
  else
  begin
    result.TheType:=ReadInlineType(result);
//    result.TheType:=ReadReference as TType;

    Read(result,result.Parameters);
  end;
end;

function TCodeReader.ReadUnarySign: TUnarySign;
begin
  result:=TUnarySign.Create;
  DoReadGroup(result);
  result.Positive:=Reader.ReadBoolean;
end;

{
function TCodeReader.ReadUsedModules:TModuleArray;
begin
  result:=DoReadModules;
end;
}

procedure TCodeReader.ReadVariable(const AVariable: TVariable);
begin
  AVariable.Clauses:=ReadClauses;
  AVariable.Name:=ReadName;

  AVariable.TypeInferred:=Reader.ReadBoolean;
  AVariable.VariableType:=ReadInlineType(AVariable);

  AVariable.ValueData:=MaybeReadData;
  TryOwner(AVariable,AVariable.ValueData);
end;

function TCodeReader.ReadWhen: TWhen;
var W : TWhenItem;
    t, tmp : Integer;
begin
  result:=TWhen.Create;

  result.Expression:=ReadData;
  TryOwner(result,result.Expression);

  tmp:=Reader.ReadInteger;

  for t:=0 to tmp-1 do
  begin
    W:=result.AddItem;

    W.Expression:=ReadData;
    TryOwner(W,W.Expression);

    W.Block:=Read as TStatement;
    TryOwner(W,W.Block);
  end;

  result.ElseBlock:=MaybeRead as TStatement;
  TryOwner(result,result.ElseBlock);
end;

function TCodeReader.ReadWhile: TWhile;
begin
  result:=TWhile.Create;

  result.Condition:=ReadData;
  TryOwner(result,result.Condition);

  result.Block:=Read as TStatement;
  TryOwner(result,result.Block);
end;

function TCodeReader.ReadWith: TWith;
begin
  result:=TWith.Create;
  result.SystemModule:=Reader.ReadBoolean;
  result.Alias:=Reader.ReadString;

  if Reader.ReadBoolean then
     result.Module:=Cached[CachedNames.IndexOf(Reader.ReadString)]
  else
    result.Module:=DoReadModule;

  if Reader.ReadBoolean then
     result.Module:=ReadReference as TNamedType;
end;

{
procedure TCodeReader.AddCached(const AUsed:TModuleArray);
var M : TNamedType;
begin
  for M in AUsed do
      Cached.TryAdd(M);
end;
}

function TCodeReader.DoReadModule:TNamedType;
begin
  result:=TNamedType.Create;
  result.Name:=Reader.ReadString;

  Cached.Add(result);
  AddNodeReference(result);

  CachedNames.Add(result.Name);

//  AddCached(ReadUsedModules);

  result.Clauses:=ReadClauses;
  Read(result,result.Items);
end;

function TCodeReader.DoReadModules:TModuleArray;
var t, tmp : Integer;
begin
  {$IFDEF FPC}
  result:=nil;
  {$ENDIf}

  tmp:=Reader.ReadInteger;

  SetLength(result,tmp);

  for t:=0 to tmp-1 do
      result[t]:=DoReadModule;
end;

function TCodeReader.MaybeRead: TNode;
begin
  if Reader.ReadBoolean then
     result:=Read
  else
     result:=nil;
end;

function TCodeReader.MaybeReadData: TData;
begin
  if Reader.ReadBoolean then
     result:=ReadData
  else
     result:=nil;
end;

procedure TCodeReader.Read(const AOwner:TNode; var ANodes: TNodes);
var t, tmp : Integer;
begin
  tmp:=Reader.ReadInteger;

  SetLength(ANodes,tmp);

  for t:=0 to tmp-1 do
  begin
    ANodes[t]:=Read;
    TryOwner(AOwner,ANodes[t]);
  end;
end;

function TCodeReader.ReadAncestor: TAncestor;
begin
  result:=TAncestor.Create;
  result.Ancestor:=ReadReference as TType;

  result.DataCall:=ReadDataCall;
  TryOwner(result,result.DataCall);
end;

function TCodeReader.ReadArrayExpression: TArrayExpression;
begin
  result:=TArrayExpression.Create;

  result.Data:=MaybeReadData;
  TryOwner(result,result.Data);

  Read(result,result.Parameters);
end;

procedure TCodeReader.ReadArrayType(const AType:TArrayType);
begin
  ReadType(AType);

  AType.TheType:=ReadInlineType(AType); // <-- to support "array of array of..."

  AType.Size:=MaybeReadData;
  TryOwner(AType,AType.Size);
end;

function TCodeReader.ReadAssignment:TAssignment;
begin
  result:=TAssignment.Create;

  result.Arithmetic:=TArithmetic_Assign(Reader.ReadInteger);

  result.Variable:=ReadData; //ReadReference(Assignment.Variable);
  TryOwner(result,result.Variable);

  result.Value:=ReadData;
  TryOwner(result,result.Value);
end;

function TCodeReader.ReadBlock: TBlockStatement;
begin
  result:=TBlockStatement.Create;
  // Not necessary to write ABlock.Block.Clauses etc
  Read(result.Block,result.Block.Items);
end;

function TCodeReader.ReadCallData: TCallData;
begin
  result:=TCallData.Create;

  result.Value:=ReadData;
  TryOwner(result,result.Value);
end;

function TCodeReader.ReadCastingData: TCastingData;
begin
  result:=TCastingData.Create;

  result.TheType:=ReadTypeCall;
  TryOwner(result,result.TheType);

  result.Data:=ReadData;
  TryOwner(result,result.Data);
end;

procedure TCodeReader.ReadClassPart(const AClass: TClassType);
begin
  AClass.Name:=ReadName;
  ReadParametersType(AClass);

  AClass.Ancestor:=ReadInlineType(AClass);
end;

procedure TCodeReader.ReadClass(const AClass: TClassType);
begin
  ReadClassPart(AClass);
end;

function TCodeReader.ReadClauses: TClauses;
begin
  result.Indexed:=Reader.ReadBoolean;
  result.Hidden:=Reader.ReadBoolean;
  result.Shared:=Reader.ReadBoolean;
  result.Final:=Reader.ReadBoolean;
end;

function TCodeReader.ReadCondition: TCondition;
begin
  result:=TCondition.Create;

  result.Condition:=ReadData;
  TryOwner(result,result.Condition);

  ReadLeftRight(result);
end;

function TCodeReader.ReadData: TData;
begin
  result:=Read as TData;
end;

procedure TCodeReader.DoReadDataCall(const ACall:TDataCall);
begin
  ACall.Routine:=Read as TType;

  if ACall.Routine is TTypeMember then
     TryOwner(ACall,ACall.Routine);

  {
  if Reader.ReadBoolean then
     TryOwner(ACall,ACall.Routine);
  }

  Read(ACall,ACall.Parameters);
end;

function TCodeReader.ReadVariableCall:TVariableCall;
begin
  result:=TVariableCall.Create;
  result.Variable:=ReadReference as TVariable;
  DoReadDataCall(result);
end;

function TCodeReader.ReadDataCall:TDataCall;
begin
  result:=TDataCall.Create;
  DoReadDataCall(result);
end;

function TCodeReader.ReadFor: TFor;
begin
  result:=TFor.Create;

  result.Counter:=MaybeRead as TVariable;
  TryOwner(result,result.Counter);

  result.First:=MaybeReadData;
  TryOwner(result,result.First);

  result.Last:=MaybeReadData;
  TryOwner(result,result.Last);

  result.InExpression:=MaybeReadData;
  TryOwner(result,result.InExpression);

  result.Block:=Read as TStatement;
  TryOwner(result,result.Block);
end;

procedure TCodeReader.DoReadGroup(const AGroup:TGroup);
begin
  AGroup.Expression:=ReadData;
  TryOwner(AGroup,AGroup.Expression);
end;

function TCodeReader.ReadGroup: TGroup;
begin
  result:=TGroup.Create;
  DoReadGroup(result);
end;

function TCodeReader.ReadID: Byte;
begin
  {$IFDEF FPC}
  result:=0;
  {$ENDIf}

  Reader.Read(result,1); // One byte
end;

function TCodeReader.ReadIf: TIf;
begin
  result:=TIf.Create;

  result.Condition:=ReadData;
  TryOwner(result,result.Condition);

  result.ThenBlock:=MaybeRead as TStatement;
  TryOwner(result,result.ThenBlock);

  result.ElseBlock:=MaybeRead as TStatement;
  TryOwner(result,result.ElseBlock);
end;

function TCodeReader.ReadInlineType(const AOwner:TNode): TType;

  {
  function ReadGenericClass:TClassType;
  begin
    result:=TClassType.Create;
    AddNodeReference(result);

    result.Ancestor:=ReadReference as TClassType;

    if result.Ancestor is TNamedType then
       result.Name:=TNamedType(result.Ancestor).Name;

    Read(result,result.Generics);
  end;
  }

  function ReadGenericType:TGenericType;
  begin
    result:=TGenericType.Create;
    result.Variable:=ReadReference as TVariable;
    //result.Name:=result.Variable.Name;
    result.FinalType:=ReadInlineType(result);
  end;

  function ReadInlineClass:TClassType;
  var tmpInline : Boolean;
  begin
    tmpInline:=Reader.ReadBoolean;

    if tmpInline then
    begin
      result:=TClassType.Create;
      Read(result,TClassType(result).Items);

      AddNodeReference(result);
    end
    else
    begin
      result:=nil;

      {$IFDEF INTERNAL}
      InternalError('Cannot read non inline class',AOwner);
      {$ENDIF}

      //result:=ReadGenericClass;
    end;

    result.Owner:=AOwner;
  end;

var tmp : Byte;
begin
  tmp:=ReadID;

  case tmp of
ClassType_ID : result:=ReadInlineClass;

        3 : begin
              result:=ReadGenericType;
              result.Owner:=AOwner;
            end;

       12 : begin
              result:=TArrayType.Create;
              ReadArrayType(result as TArrayType);
              result.Owner:=AOwner;
            end;

       32 : begin
              result:=TManyValues.Create;
              ReadArrayType(result as TArrayType);
              result.Owner:=AOwner;
            end;

       40 : begin
              result:=ReadRangeType;
              result.Owner:=AOwner;
            end;

       55 : begin
              result:=ReadTypeMember;
              result.Owner:=AOwner;
            end;

       58 : begin
              result:=ReadSpecializedType;
              result.Owner:=AOwner;
            end;

   Ref_ID : result:=ReadReference as TType;
  else
    begin
      result:=nil;

      {$IFDEF INTERNAL}
      WrongID(tmp,'Inline Type');
      {$ENDIF}
    end;
  end;
end;

function TCodeReader.ReadReturn: TReturn;
begin
  result:=TReturn.Create;

  result.Value:=MaybeReadData;
  TryOwner(result,result.Value);
end;

function TCodeReader.ReadSelf: TSelf;
begin
  result:=TSelf.Create;
  result.TheType:=ReadReference as TType;
end;

function TCodeReader.ReadSpecializedType: TSpecializedType;
begin
  result:=TSpecializedType.Create;

  ReadType(result);
  result.TheType:=ReadInlineType(result) as TParametersType;
  Read(result,result.Generics);
end;

function TCodeReader.ReadTry: TTry;
var t,tmp : Integer;
    C : TCatch;
begin
  result:=TTry.Create;

  result.Block:=Read as TStatement;
  TryOwner(result,result.Block);

  tmp:=Reader.ReadInteger;

  if tmp>0 then
  begin
    SetLength(result.Catch,tmp);

    for t:=0 to tmp-1 do
    begin
      C:=TCatch.Create;

      C.Error:=MaybeRead;
      TryOwner(C,C.Error);

      C.Block:=MaybeRead as TStatement;
      TryOwner(result,C.Block);

      result.Catch[t]:=C;
    end;
  end;

  if Reader.ReadBoolean then
  begin
    result.TheFinally:=Read as TStatement;
    TryOwner(result,result.TheFinally);
  end;
end;

function TCodeReader.ReadTypeMember:TTypeMember;
begin
  result:=TTypeMember.Create;
  result.TheType:=ReadInlineType(result);
  result.Member:=ReadInlineType(result);
end;

function TCodeReader.ReadFloat: TFloat;
begin
  result:=TFloat.Create(Reader.ReadFloat);
  result.Text:=Reader.ReadString;
end;

function TCodeReader.ReadInteger: TInteger;
begin
  result:=TInteger.Create(Reader.ReadInt64);
  result.Text:=Reader.ReadString;
  Reader.Read(result.Base,1);
end;

function TCodeReader.ReadIntegerClass: TIntegerClass; // 0..100
begin
  result:=TIntegerClass.Create;
  AddNodeReference(result);
  ReadClass(result);

  result.Size:=ReadRangeType;
  result.Size.Owner:=result;
end;

function TCodeReader.NewVariable: TVariable;
begin
  result:=TVariable.Create;
  AddNodeReference(result);
  ReadVariable(result);
end;

function TCodeReader.ReadUnaryNot: TUnaryNot;
begin
  result:=TUnaryNot.Create;
  DoReadGroup(result);
end;

function TCodeReader.Read: TNode;
var tmp : Byte;
begin
  tmp:=ReadID;

  case tmp of
      1 : result:=NewVariable;
 ClassType_ID : result:=NewClass;
      3 : begin result:=TGenericType.Create; TGenericType(result).Variable:=NewVariable; end;
 Routine_ID : result:=NewRoutine;
      5 : result:=NewExtender;
      6 : result:=ReadIntegerClass;
      7 : result:=ReadTypeCall;
      8 : result:=TMagic.MagicTrue;
      9 : result:=TMagic.MagicFalse;
{     10 : begin
            result:=TComment.Create;
            TComment(result).Text:=ReadName;
          end;
          }
     11 : result:=ReadFloat;
//     12 : result:=nil; // error ?
     13 : result:=ReadReturn;
 14..27 : result:=ReadOperand(tmp);
     28 : result:=ReadCondition;
     29 : result:=ReadInteger;
     30 : result:=ReadDataCall;
     31 : result:=ReadAssignment;
//     32 : result:=nil; // error ?
     33 : result:=ReadArrayExpression;
     34 : result:=ReadIf;
     35 : result:=ReadFor;
     36 : result:=ReadWhile;
     37 : result:=ReadRepeat;
     38 : result:=ReadWhen;
     39 : result:=ReadMember;
//     40 : result:=nil; // <-- Error ???
     41 : result:=TText.Create(Reader.ReadString);
     42 : result:=ReadCallData;
     43 : result:=TBreak.Create;
     44 : result:=TContinue.Create;
     45 : result:=ReadUnarySign;
     46 : result:=ReadUnaryNot;
     47 : result:=ReadWith;
     48 : result:=ReadGroup;
     49 : result:=ReadCastingData;
     50 : result:=ReadRange;
     51 : result:=ReadBlock;
     52 : result:=ReadSelf;
     53 : result:=ReadAncestor;
     54 : result:=ReadTry;
     55 : result:=ReadTypeMember;
 FunctionType_ID: result:=NewFunctionType;
     57 : result:=ReadVariableCall;

 Ref_ID : result:=ReadReference;
  else
    begin
      result:=nil;

      {$IFDEF INTERNAL}
      WrongID(tmp,'Read');
      {$ENDIF}
    end;
  end;
end;

function TCodeReader.ReadMember: TMember;
begin
  result:=TMember.Create;

  result.Data:=ReadData;
  TryOwner(result,result.Data);

  result.Member:=ReadData;
  TryOwner(result,result.Member);
end;

class function TCodeReader.ReadModule(const AStream: TStream; out Embedded:TModuleArray): TType;
var tmp : TCodeReader;
begin
  tmp:=TCodeReader.Create(AStream);
  try
    result:=tmp.DoReadModule;
    Embedded:=tmp.Cached;
  finally
    tmp.Free;
  end;
end;

class function TCodeReader.ReadModules(const AStream: TStream):TModuleArray;
var tmp : TCodeReader;
begin
  tmp:=TCodeReader.Create(AStream);
  try
    result:=tmp.DoReadModules;
  finally
    tmp.Free;
  end;
end;

end.
