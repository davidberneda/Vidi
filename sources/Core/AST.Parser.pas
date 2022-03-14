unit AST.Parser;
{$IFDEF NEXTGEN}
{$ZEROBASEDSTRINGS OFF}
{$ENDIF}

{$IFDEF INTERNAL}
{$DEFINE CHECK_POSITIONS}
{$ENDIF}

interface

uses
  Parser, Sys, AST, Position, Checker.AST, Map;

type
  TASTParser=class(TParser)
  protected
    Context : TType;

    PositionToAdd : TPosition; // used at AddPosition

    function AddPosition(const ANode:TNode; const AText:String):Integer; overload;
    function AddPosition(const ANode:TNode; const APosition:TPosition; const AText:String):Integer; overload;
    function AddPosition(const ANode:TNode; const APosition:TPosition;
                         const AText:String; const AStyle:TPositionStyle;
                         CheckIdentifier:Boolean=True):Integer; overload;

    function AddPositionKeyword(const ANode:TNode; const AKeyword:String):Integer;
    function AddPositionSymbol(const ANode:TNode; const ASymbol:String):Integer;

    function GetBoolean(out ABool:TData):Boolean; overload;
    procedure GetClauses(const AOwner:TNode; var Temp:TClauses);
    function GetSymbol(const AOwner:TNode; const ASymbol:String):Boolean;
    function GetTokenWithDots:String;

    procedure Init(const AText:String); override;

    procedure NewComment(const Start,Finish:TPosition); override;

    class function NumberToNode(const IsNegative:Boolean; const S:String):TNumber;

    function PeekBeginBlock:Boolean;
    function PeekEndBlock:Boolean;
    function PeekIs(const AToken:String):Boolean;
    function PeekIsGet(const AToken:String):Boolean; // call GetToken if PeekIs = True
    function PeekOperand:Boolean;
    function PeekSymbolGet(const AToken:String):Boolean;

    procedure RaiseException(const APosition:TPosition; const ALength:Integer; const AError:String);
    function ValidIdentifier(const AName:String):Boolean;
  public
    Checker : TChecker;
    Positions : TNodePositions;

    ModuleName : String;
    ModulePath : String;

    Destructor Destroy; override;
  end;

implementation

uses
  SysUtils,

  {$IFDEF CHECK_POSITIONS}
  Utils.AST,
  {$ENDIF}
  Syntax, Magic, Exceptions, Text;

{ TASTParser }

function TASTParser.AddPosition(const ANode:TNode; const AText:String):Integer;
begin
  result:=AddPosition(ANode,PositionToAdd,AText);
end;

function TASTParser.AddPosition(const ANode:TNode; const APosition:TPosition;
                                const AText:String):Integer;
begin
  result:=AddPosition(ANode,APosition,AText,TPositionStyle.Identifier);
end;

function TASTParser.AddPosition(const ANode:TNode; const APosition:TPosition;
                                const AText:String;
                                const AStyle:TPositionStyle;
                                CheckIdentifier:Boolean):Integer;

  {$IFDEF CHECK_POSITIONS}
  procedure VerifyPosition;
  var tmp : String;
  begin
    // internal check, verify AText is exactly at Position.Position

    if (AText='') and (not (ANode is TText)) then
       RaiseException(APosition,Length(AText),'Position Empty Error');

    if Positions.Count>1 then
       if APosition.Position=Positions.Items[Positions.Count-1].Position.Position then
          RaiseException(APosition,Length(AText),'Position Duplicate Error: '+IntToStr(APosition.Position));

    if ANode is TText then
       tmp:=Copy(Text,APosition.Position+1,Length(AText))
    else
       tmp:=Copy(Text,APosition.Position,Length(AText));

    if not Checker.TextIs(tmp,AText) then
       RaiseException(APosition,Length(AText),'Position Text Error: ['+tmp+'] <> ['+AText+']');

    // Now check the ANode text as identifier
    if (AStyle=TPositionStyle.Identifier) and CheckIdentifier then
    begin
      tmp:=TASTUtils.NodeName(ANode);

      if not Checker.TextIs(tmp,AText) then
         RaiseException(APosition,Length(AText),'Position Node Error: ['+tmp+'] <> ['+AText+']');
    end;
  end;
  {$ENDIF}

var P : TNodePosition;
begin
  {$IFDEF CHECK_POSITIONS}
  VerifyPosition;
  {$ENDIF}

  P.Node:=ANode;

  if ANode is TText then
     P.Length:=Length(AText)+2
  else
     P.Length:=Length(AText);

  P.Position:=APosition;
  P.Style:=AStyle;

  result:=Positions.Add(P);
end;

function TASTParser.AddPositionSymbol(const ANode:TNode; const ASymbol:String):Integer;
begin
  result:=AddPosition(ANode,PositionToAdd,ASymbol,TPositionStyle.Symbol);
end;

destructor TASTParser.Destroy;
begin
 // TASTUtils.DestroyNodes(Comments);
  inherited;
end;

function TASTParser.AddPositionKeyword(const ANode:TNode; const AKeyword:String):Integer;
begin
  result:=AddPosition(ANode,PositionToAdd,AKeyword,TPositionStyle.Keyword);
end;

function TASTParser.PeekBeginBlock:Boolean;
begin
  result:=PeekIs(TSyntax._BeginBlock); // {
end;

function TASTParser.PeekEndBlock:Boolean;
begin
  result:=PeekIs(TSyntax._EndBlock); // }
end;

{.$DEFINE FASTPEEK}

function TASTParser.PeekIs(const AToken:String):Boolean;
begin
  // Much faster
  {$IFDEF FASTPEEK}
  result:=
    {$IFDEF FPC}
    (Length(Text)>=Position.Position) and  // <-- FPC does not check for past Text length
    {$ENDIF}
    (String.Compare(Text,Position.Position-1,AToken,0,Length(AToken),not TChecker.CaseSensitive)=0);

  {$ELSE}
  result:=TChecker.TextIs(Copy(Text,Position.Position,Length(AToken)),AToken);
  {$ENDIF}
end;

// Any char after AToken is NOT allowed. Only blanks and symbols
function TASTParser.PeekIsGet(const AToken:String):Boolean;
var L : Integer;
begin
  result:=PeekIs(AToken);

  if result then
  begin
    L:=Length(AToken);

    // No digit or letter is allowed after token:
    if Position.Position+L<Finish then
       if CharInSet(Text[Position.Position+L],Identifier+Digits) then
          Exit(False);

    IncPosition(L);
    SkipBlanks;
  end;
end;

// Any char after AToken is allowed
function TASTParser.PeekSymbolGet(const AToken:String):Boolean;
begin
  result:=PeekIs(AToken);

  if result then
  begin
    IncPosition(Length(AToken));
    SkipBlanks;
  end;
end;

function TASTParser.GetSymbol(const AOwner:TNode; const ASymbol:String):Boolean;
var Old : TPosition;
begin
  Old:=Position;
  result:=PeekSymbolGet(ASymbol);

  if result then
     AddPosition(AOwner,Old,ASymbol,TPositionStyle.Symbol);
end;

procedure TASTParser.RaiseException(const APosition:TPosition; const ALength:Integer;
                                    const AError:String);
begin
  Raise_Exception(APosition,ALength,ModuleName,ModulePath,AError);
end;

// Returns True if AName is a valid identifier (starts with A..Z or _, and
// next chars are A..Z, _ or 0..9
function TASTParser.ValidIdentifier(const AName: String): Boolean;
const
  Identifier_And_Digits=Identifier+Digits;

var t : Integer;
begin
  result:=(AName<>'') and TTextUtils.CharInSet(AName[1],Identifier);

  if result then
     for t:=2 to Length(AName) do
         if not TTextUtils.CharInSet(AName[t],Identifier_And_Digits) then
            Exit(False);
end;

function TASTParser.GetBoolean(out ABool:TData):Boolean;

  function Get(const AConstant:String; const AValue:TBoolean):Boolean;
  begin
    result:=PeekIsGet(AConstant);

    if result then
    begin
      ABool:=AValue;
      AddPositionKeyword(ABool,AConstant);
    end;
  end;

begin
  result:=Get(TSyntax._Reserved_true,TMagic.MagicTrue) or
          Get(TSyntax._Reserved_false,TMagic.MagicFalse);
end;

// Indexed Shared Hidden Final
procedure TASTParser.GetClauses(const AOwner:TNode; var Temp:TClauses);

  function IsClause(const AClause:String):Boolean;
  begin
    result:=PeekIsGet(AClause);

    if result then
       AddPositionKeyword(AOwner,AClause);
  end;

var tmpClause : Boolean;
begin
  //Temp.Reset;

  repeat
    tmpClause:=False;

    PositionToAdd:=Position;

    if IsClause(TSyntax._Reserved_indexed) then
    begin
      Temp.Indexed:=True;
      tmpClause:=True;
    end
    else
    if IsClause(TSyntax._Reserved_shared) then
    begin
      Temp.Shared:=True;
      tmpClause:=True;
    end
    else
    if IsClause(TSyntax._Reserved_hidden) then
    begin
      Temp.Hidden:=True;
      tmpClause:=True;
    end
    else
    if IsClause(TSyntax._Reserved_final) then
    begin
      Temp.Final:=True;
      tmpClause:=True;
    end;

  until not tmpClause;
end;

// a.b.c
function TASTParser.GetTokenWithDots:String;
begin
  result:=GetToken;

  while PeekSymbolGet(TSyntax._NamespaceDelimiter) do
        result:=result+TSyntax._NamespaceDelimiter+GetToken;
end;

procedure TASTParser.Init(const AText:String);
begin
  inherited;

  FormatSettings.DecimalSeparator:='.';

  if Positions<>nil then
     Positions.Clear;
end;

// Try converting S to a TInteger or a TFloat
class function TASTParser.NumberToNode(const IsNegative:Boolean; const S:String):TNumber;

  function RemoveSeparators(const X:String):String;
  begin
    result:=StringReplace(X,'_','',[rfReplaceAll]);
  end;

var tmpInt : Int64;
    tmpFloat : Extended;
    tmpBase : Byte;
    tmpS : String;
begin
  // Preserve positive S.
  // This is to skip problem with -MinInt64, because it overflows Int64
  // (UInt64 cannot *maybe* be used here)
  if IsNegative then
     tmpS:='-'+S
  else
     tmpS:=S;

  // Remove "_", if existing
  if Pos(DigitSeparator,tmpS)>0 then
     tmpS:=RemoveSeparators(tmpS);

  if TTextUtils.IsInteger(tmpS,tmpInt,tmpBase) then  // try to convert to integer
  begin
    result:=TInteger.Create(tmpInt);
    result.Text:=S;
    TInteger(result).Base:=tmpBase;
  end
  else
  if TTextUtils.IsFloat(tmpS,tmpFloat) then // try to convert to float
  begin
    result:=TFloat.Create(tmpFloat);
    result.Text:=S;
  end
  else
    result:=nil;
end;

// Try remove PeekOperand, replace it with already existing GetOperand
function TASTParser.PeekOperand:Boolean;
var Operand : Char;
    tmp : String;
    Old : Integer;
begin
  result:=False;

  Old:=Positions.Count;

  tmp:=PeekChar;

  if tmp<>'' then
  begin
    Operand:=tmp[1];

    if CharInSet(Operand,['+','-','*','/']) then
    begin
      Inc(Position.Position);
      result:=PeekChar<>'=';
      Dec(Position.Position);
    end
    else
      result:=(Operand='=') or (Operand=TSyntax._Reserved_Condition);

    if not result then
    begin
      if Operand='<' then
         result:=True
      else
      if Operand='>' then
         result:=True
      else
      begin
        tmp:=PeekIdentifier;

        result:=Checker.TextIs(tmp,TSyntax._Reserved_in) or  // Contains = left in right
                Checker.TextIs(tmp,TSyntax._Reserved_and) or
                Checker.TextIs(tmp,TSyntax._Reserved_or) or
                Checker.TextIs(tmp,TSyntax._Reserved_xor);
      end;
    end;
  end;

  Positions.DeleteFrom(Old); // <-- always !
end;

procedure TASTParser.NewComment(const Start, Finish: TPosition);
var tmpLength : Integer;
//    tmp : TComment;
    tmpS : String;
begin
{
  tmp:=TComment.Create;
  tmp.Owner:=Context;

  Context.Items.Add(tmp);
//  Comments.Add(tmp);
  }

  tmpLength:=Finish.Position-Start.Position; // Do not increase +1 !!

  tmpS:=Copy(Text,Start.Position,tmpLength);

  // tmp.Text:=tmpS;

  AddPosition(nil,Start,tmpS,TPositionStyle.Comment);
end;

end.
