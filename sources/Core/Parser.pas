unit Parser;

{$IFDEF NEXTGEN}
{$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Position;

type
  TParser=class
  protected

    var
      Finish : Integer;

    function Current:Char; inline;

    function GetIdentifier:String;
    function GetToken:String;
    function GetTokenNumber:String;
    function GetTokenText(out S:String):Boolean;

    procedure IncPosition(const N:Integer); overload; inline;
    procedure IncPosition; overload; inline;

    procedure Init(const AText:String); virtual;

    procedure NewComment(const Start,Finish:TPosition); virtual; abstract;

    function PeekChar:String;
    function PeekIdentifier:String;
    function PeekToken(const Length:Integer):String; inline;
    function Pending:Boolean; inline;
    procedure SkipBlanks;
  public
    const
      Identifier = ['a'..'z','A'..'Z','_'];
      Digits = ['0'..'9'];
      DigitSeparator = '_';
      Plus='+';
      Minus='-';

      Blanks = [' ',#9,#10,#13,#160]; // 160 = non-breaking space // --> ? $200B = zero-width space

    var
    Position : TPosition;
    Text : String;
  end;

implementation

uses
  Text;

procedure TParser.IncPosition(const N:Integer);
begin
  Position.Increment(N);
end;

procedure TParser.IncPosition;
begin
  Position.Increment(1);
end;

procedure TParser.Init(const AText:String);
begin
  Position.Line:=1;
  Position.Column:=1;
  Position.Position:=1;

  Text:=AText;
  Finish:=Length(Text) + 1;
end;

function TParser.Pending:Boolean;
begin
  result:=Position.Position<Finish;
end;

function TParser.Current:Char;
begin
  result:=Text[Position.Position];
end;

procedure TParser.SkipBlanks;

  procedure TryIncLine;
  begin
    if (Current=#13) {$IFDEF LINUX}or (Current=#10){$ENDIF} then
    begin
      Inc(Position.Line);
      Position.Column:=0;
    end
    else
    if Current=#10 then
    begin
      Inc(Position.Position);
      Exit;
    end;

    IncPosition;
  end;

  const
    EndOfLine = [#10,#13];

  procedure UpToEndOfLine;
  begin
    while Pending and (not TTextUtils.CharInSet(Current,EndOfLine)) do
          IncPosition;
  end;

  procedure DoSingleLine;
  var Start : TPosition;
  begin
    Start:=Position;

    IncPosition;
    IncPosition;

    UpToEndOfLine;

    NewComment(Start,Position);

    SkipBlanks;
  end;

  procedure DoMultiLine;
  var Start : TPosition;
  begin
    Start:=Position;

    IncPosition;
    IncPosition;

    while Pending do
    begin
      if (Current='*') and (Text[Position.Position+1]='/') then
      begin
        IncPosition;
        IncPosition;

        break;
      end
      else
        TryIncLine;
    end;

    NewComment(Start,Position);

    SkipBlanks;
  end;

begin
  while Pending and TTextUtils.CharInSet(Current,Blanks) do
     TryIncLine;

  // Code Comment //

  if Position.Position+1<Finish then
     if Current='/' then
        if Text[Position.Position+1]='/' then // single line comment
           DoSingleLine
        else
        if Text[Position.Position+1]='*' then // /* multiple line comment */
           DoMultiLine
end;

function TParser.PeekChar:String;
begin
  if Position.Position<Finish then
     result:=Text[Position.Position]
  else
     result:='';
end;

function TParser.PeekToken(const Length:Integer):String;
begin
  result:=Copy(Text,Position.Position,Length);
end;

function TParser.GetTokenText(out S:String):Boolean;

  function GetText(const Quote:String):Boolean;
  var C : Char;
      Start, Length : Integer;
  begin
    //S:='';

    IncPosition;

    Start:=Position.Position;
    Length:=0;

    while Pending do
    begin
      C:=Current;
      IncPosition;

      if C=Quote then
      begin
        S:=Copy(Text,Start,Length);
        SkipBlanks;
        result:=True;

        Exit;
      end
      else
      begin
        Inc(Length);
        //S:=S+C;
      end;
    end;

    result:=False;
  end;

begin
  if PeekChar='"' then
     result:=GetText('"')
  else
  if PeekChar='''' then
     result:=GetText('''')
  else
     result:=False;
end;

// +1
// 123.45
// -1
// 0xFF  hexadecimal
// 2e-5  exponent
// -4.123e2
// 0b110110101101  binary
// 0c777 octal
function TParser.GetTokenNumber:String;

  function GetExponent:String;
  var C : Char;
  begin
    result:='';

    while Pending do
    begin
      C:=Current;

      if ( (result='') and ( (C=Plus) or (C=Minus) ) ) or // +- prefix
         TTextUtils.CharInSet(C,Digits) then // 0..9
      begin
        IncPosition;
        result:=result+C;
      end
      else
        Break;
    end;

    // empty exponent not allowed:   123e-   or  123e+
    if (result=Plus) or (result=Minus) then
    begin
      result:='';
      Position.Increment(-1);
    end;
  end;

const
  HexaDigits = Digits+['A'..'F','a'..'f'];

type
  TBase=(Ten,Hexa,Binary,Octal);

var C : Char;
    FirstZero: Boolean;
    Base:TBase;
    Valid : Boolean;
begin
  result:='';

  Base:=TBase.Ten;

  while Pending do
  begin
    C:=Current;

    Valid:=(result='') and TTextUtils.CharInSet(C,[Plus,Minus]);

    if not Valid then // +- prefix
       if Base=TBase.Hexa then
          Valid:=TTextUtils.CharInSet(C,HexaDigits)  // 0..9 A..F a..f
       else
       if Base=TBase.Binary then
          Valid:=(C='0') or (C='1')  // 0..1
       else
       if Base=TBase.Octal then
          Valid:=TTextUtils.CharInSet(C,TTextUtils.OctalDigits)  // 0..7
       else
          Valid:=TTextUtils.CharInSet(C,Digits);  // 0..9

     if not Valid then
     begin
       // "." dot is only valid when there are decimals after it.
       // 4.  <-- NOT valid
       // 4.AsText <-- NOT valid
       // 4.5 <-- VALID
       Valid:=(C='.') and
              (Pos('.',result)=0) and
              (
               (Position.Position+1=Finish) or
               TTextUtils.CharInSet(Text[Position.Position+1],Digits)
              ) and
              (PeekToken(2)<>'..')
              ; // decimal point
     end;

    if not Valid then
       if Base=TBase.Ten then
       begin
         FirstZero:=(result='0') or (result='-0') or (result='+0');

         if FirstZero then
         begin
           if (C='x') or (C='X') then
               Base:=TBase.Hexa  // hexadecimal 0xABCDE
           else
           if (C='b') or (C='B') then // binary 0b1001010101
              Base:=TBase.Binary
           else
           if (C='c') or (C='C') then // octal 0c777
              Base:=TBase.Octal;

           Valid:=Base<>TBase.Ten;
         end;
       end;

    // TODO: Check here for (C='_') and (result<>'') to allow digit separators:
    //  0x1_234_567_890

    if not Valid then
       Valid:=(result<>'') and (C=DigitSeparator);

    if Valid then
    begin
      IncPosition;
      result:=result+C;
    end
    else
      Break;
  end;

  if Pending and (result<>'') then
  begin
    C:=Current;

    if (C='e') or (C='E') then  //  1.23e2
    begin
      IncPosition;
      result:=result+C+GetExponent;
    end;
  end;

  { unreachable:
  // empty sign not allowed:  + or -
  if (result=Plus) or (result=Minus) then
     result:='';
  }

  SkipBlanks;
end;

// Identifier (letter or _, and then letter,_ or digit)
function TParser.GetIdentifier:String;
const
  Identifier_And_Digits=Identifier+Digits;

var C : Char;
    Start, Length : Integer;
begin
  if Pending then
  begin
    C:=Current;

    if TTextUtils.CharInSet(C,Identifier) then
    begin
      Start:=Position.Position;
      Length:=1;

      IncPosition;
      //result:=C;

      while Pending do
      begin
        C:=Current;

        if TTextUtils.CharInSet(C,Identifier_And_Digits) then
        begin
          IncPosition;
          //result:=result+C;

          Inc(Length);
        end
        else
          Break;
      end;

      result:=Copy(Text,Start,Length);
      SkipBlanks;
    end
    else
      result:='';
  end
  else
    result:='';

  {
  result:='';

  while Pending do
  begin
    C:=Current;

    if TTextUtils.CharInSet(C,Identifier) or
       (TTextUtils.CharInSet(C,Digits) and (result<>'')) then
    begin
      IncPosition;
      result:=result+C;
    end
    else
      Break;
  end;

  SkipBlanks;
  }
end;

// Identifier, or at least one character
function TParser.GetToken:String;
begin
  result:=GetIdentifier;

  if result='' then
  begin
    if Pending then
    begin
      result:=Current; // single character
      IncPosition;
    end
  end;

  SkipBlanks;
end;

// Peek Identifier
function TParser.PeekIdentifier:String;
var tmp : TPosition;
begin
  tmp:=Position;
  result:=GetIdentifier;
  Position:=tmp;
end;

end.
