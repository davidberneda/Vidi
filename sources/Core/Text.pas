unit Text;

interface

uses
  SysUtils;

type
  TTextUtils=record
  public
    const
      IntegerPrefix_Binary = '0b';  // 0b10101010111011001
      IntegerPrefix_Octal  = '0c';  // 0c777123
      IntegerPrefix_Hexa   = '0x';  // 0xFFABCBEEF

      OctalDigits = ['0'..'7'];

    class function CharInSet(const C:Char; const ASet:TSysCharSet):Boolean; static;
    class function IntegerToBinary(Value:Int64):String; static;
    class function IntegerToHexadecimal(Value:Int64):String; static;
    class function IntegerToOctal(Value:Int64):String; static;
    class function IntegerToString(const Base:Byte; const Value:Int64):String; static;
    class function IsInteger(const S:String; out Value:Int64; out Base:Byte):Boolean; static;
    class function IsFloat(const S:String; out Value:Extended):Boolean; static;
    class function Same(const S,Token:String):Boolean; static; inline;
    class function TryStrToBin(const S:String; out Value:Int64):Boolean; static;
    class function TryStrToOctal(const S:String; out Value:Int64):Boolean; static;
  end;

implementation

// Returns True when C character is any of the characters in ASet.
// Comparison is case-sensitive
class function TTextUtils.CharInSet(const C:Char; const ASet:TSysCharSet):Boolean;
begin
  result:=SysUtils.CharInSet(C,ASet);
end;

// Returns True if S=Token (case insensitive comparison)
class function TTextUtils.Same(const S,Token:String):Boolean;
begin
  result:=SysUtils.SameText(S,Token);
end;

// Converts string S in binary format (eg: 0110110111) to an integer
class function TTextUtils.TryStrToBin(const S:String; out Value:Int64):Boolean;
var tmp : Byte;
    c : Char;
begin
  result:=False;
  Value:=0;

  for c in S do
  begin
    if c='1' then tmp:=1 else
    if c='0' then tmp:=0 else
       Exit;

    Value:=(Value shl 1)+tmp;
  end;

  result:=True;
end;

// Converts string S in octal format (eg: 777) to an integer
class function TTextUtils.TryStrToOctal(const S:String; out Value:Int64):Boolean;
var tmp : Byte; // SmallInt
    c : Char;
begin
  result:=False;
  Value:=0;

  for c in S do
  begin
    tmp:=Ord(c);

    if (tmp>=48) and (tmp<=55) then
       Value:=(Value*8)+(tmp-48)
    else
       Exit;
  end;

  result:=True;
end;

// Returns True when string S is an integer number,
// in base 2, 8, 10 or 16
class function TTextUtils.IsInteger(const S:String; out Value:Int64; out Base:Byte):Boolean;
var tmp : String;
    tmpFloat : Extended;
    UValue : {$IFDEF FPC}QWord{$ELSE}UInt64{$ENDIF};
begin
  tmp:=Copy(S,1,2);

  if Same(tmp,IntegerPrefix_Hexa) then
  begin
    result:=TryStrToInt64('$'+Copy(S,3,Length(S)),Value);

    if result then
       Base:=16;
  end
  else
  if Same(tmp,IntegerPrefix_Binary) then
  begin
    result:=TryStrToBin(Copy(S,3,Length(S)),Value);

    if result then
       Base:=2;
  end
  else
  if Same(tmp,IntegerPrefix_Octal) then
  begin
    result:=TryStrToOctal(Copy(S,3,Length(S)),Value);

    if result then
       Base:=8;
  end
  else
  begin
    Base:=10;

    result:=TryStrToInt64(S,Value);

    if not result then
    begin
      {$IFDEF FPC}
      result:=TryStrToQWord(S,UValue);
      {$ELSE}
      result:=TryStrToUInt64(S,UValue);
      {$ENDIF}

      if result then
         Exit;

      // String might contain exponent.
      // Try to convert it to a float first.
      if IsFloat(S,tmpFloat) then
      begin
        if (tmpFloat<=Int64.MaxValue) and (tmpFloat>=Int64.MinValue) then
        begin
          Value:=Trunc(tmpFloat);

          result:=Value=tmpFloat;
        end;
      end;
    end;
  end;
end;

class function TTextUtils.IsFloat(const S:String; out Value:Extended):Boolean;
begin
  result:=TryStrToFloat(S,Value);
end;

class function TTextUtils.IntegerToBinary(Value:Int64):String;
const ZeroOne:Array[0..1] of Char=('0','1');
begin
  result:='';

  while Value>0 do
  begin
    result:=ZeroOne[Value and 1]+Result;
    Value:=Value shr 1;
  end;

  result:='0b'+result;
end;

class function TTextUtils.IntegerToOctal(Value:Int64):String;
const ZeroSeven:Array[0..7] of Char=('0','1','2','3','4','5','6','7');
begin
  result:='';

  while Value>0 do
  begin
    result:=ZeroSeven[Value and 7]+Result;
    Value:=Value shr 3;
  end;

  result:='0c'+result;
end;

class function TTextUtils.IntegerToHexadecimal(Value:Int64):String;
const ZeroFifteen:Array[0..15] of Char=('0','1','2','3','4','5','6','7',
                                        '8','9','A','B','C','D','E','F');
begin
  result:='';

  while Value>0 do
  begin
    result:=ZeroFifteen[Value and 15]+Result;
    Value:=Value shr 4;
  end;

  result:='0x'+result;
end;

class function TTextUtils.IntegerToString(const Base:Byte; const Value:Int64):String;
begin
  case Base of
     2: result:=IntegerToBinary(Value);
     8: result:=IntegerToOctal(Value);
    16: result:=IntegerToHexadecimal(Value);
  else
    // 10
    result:=IntToStr(Value);
  end;
end;

end.
