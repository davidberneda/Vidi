unit Vidi;

interface

uses
  Variants;

type
  Float=Double;

  FloatHelper=record helper for Float
  public
    function Truncate:Int64;
    function Round:Int64;
  end;

  //  _Boolean=Boolean;

  Text=String;
  Character=Char;

  SomeThing=class
    constructor Create; virtual;
  end;

  SomeThings=Array of SomeThing;

  Clauses=record
    _Indexed,
    _Hidden,
    _Shared,
    _Final : Boolean;

    procedure Reset;
  end;

  _Type=class(SomeThing)
    Clauses : Clauses;

    Items : SomeThings; // All Fields, methods, sub-types etc
    Parameters : SomeThings; // generics

//    class function _Is(const AItem:SomeThing; const AType:_Type):Boolean;
  end;

  Data=class(SomeThing)
  end;

  Operand=class(Data)
  public
    Left, Right : Data;
  end;

  Logical=class(Operand)
    //  Join : Logical {}
  end;

  // Types

  NamedType=class(_Type)
    Name : Text;

    class function NameOf(const AItem:SomeThing):Text;
  end;

  // [0..999,-123..456]
  Range=class(Data)
    Min,
    Max : Data;
  end;

  Console=record
  public
//    class procedure Put(const A); overload; static;
    class procedure Put(const A,B:Variant); overload; static;
    class procedure Put(const A,B,C:Variant); overload; static;
    class procedure Put(const I:Integer); overload; static;
    class procedure Put(const F:Float); overload; static;
    class procedure Put(const B:Boolean); overload; static;
    class procedure Put(const S:String); overload; static;
    class procedure Put(const A:Array of Integer); overload; static;

    class procedure PutLine; overload; static;
    class procedure PutLine(const S:String); overload; static;
    class procedure PutLine(const S:String; const F:Float); overload; static;
    class procedure PutLine(const I:Integer); overload; static;
    class procedure PutLine(const F:Float); overload; static;
    class procedure PutLine(const B:Boolean); overload; static;
    class procedure PutLine(const S:Array of String); overload; static;
    class procedure PutLine(const B:Array of Boolean); overload; static;
  end;

  _Math=record
  public
    class function Modulo(const Divisor,Dividend:Integer):Integer; static;
    class function Power(const Base,Exponent:Integer):Integer; overload; static;
    class function Power(const Base,Exponent:Float):Float; overload; static;
    class function PowerFraction(const Base,Exponent:Float):Float; static;
  end;

implementation

uses Math, SysUtils;

constructor SomeThing.Create;
begin
  inherited;
end;

procedure Clauses.Reset;
begin
  _Indexed:=False;
  _Hidden:=False;
  _Shared:=False;
  _Final:=False;
end;

{
class procedure Console.Put(const A);
begin
end;
}

class procedure Console.Put(const A,B:Variant);
begin
  Write(A);
  Write(B);
end;

class procedure Console.Put(const A,B,C:Variant);
begin
  Write(A);
  Write(B);
  Write(C);
end;

class procedure Console.Put(const I:Integer);
begin
  Write(I);
end;

class procedure Console.Put(const F:Float);
begin
  Write(F);
end;

class procedure Console.Put(const S:String);
begin
  Write(S);
end;

class procedure Console.Put(const B:Boolean);
begin
  Write(B);
end;

class procedure Console.PutLine;
begin
  WriteLn;
end;

class procedure Console.PutLine(const S:String);
begin
  WriteLn(S);
end;

class procedure Console.PutLine(const S:String; const F:Float);
begin
  WriteLn(S,' ',FloatTostr(F));
end;

class procedure Console.PutLine(const I: Integer);
begin
  WriteLn(I);
end;

class procedure Console.PutLine(const F: Float);
begin
  WriteLn(FloatToStr(F));
end;

class procedure Console.PutLine(const B: Boolean);
begin
  WriteLn(BoolToStr(B,True));
end;

class procedure Console.PutLine(const S: array of String);
var t : Integer;
begin
  for t:=Low(S) to High(S) do
  begin
    if t>Low(S) then
       Write(' ');
    Write(S[t]);
  end;

  WriteLn;
end;

class procedure Console.PutLine(const B:Array of Boolean);
var t : Integer;
begin
  for t:=Low(B) to High(B) do
  begin
    if t>Low(B) then
       Write(' ');
    Write(B[t]);
  end;

  WriteLn;
end;

{ _Math }

class function _Math.Modulo(const Divisor, Dividend: Integer): Integer;
begin
  result:=Divisor mod Dividend;
end;

class function _Math.Power(const Base,Exponent:Integer):Integer;
begin
  result:=Round(Math.IntPower(Base,Exponent));
end;

class function _Math.Power(const Base, Exponent: Float): Float;
begin
  result:=Math.Power(Base,Exponent);
end;

class function _Math.PowerFraction(const Base,Exponent:Float):Float;
begin
  result:=Math.Power(Base,Exponent);
end;

{ NamedType }

class function NamedType.NameOf(const AItem: SomeThing): Text;
begin
  if AItem is NamedType then
     result:=NamedType(AItem).Name
  else
     result:='?';
end;

{ FloatHelper }

function FloatHelper.Round: Int64;
begin
  result:=System.Round(Self);
end;

function FloatHelper.Truncate: Int64;
begin
  result:=System.Trunc(Self);
end;

class procedure Console.Put(const A: array of Integer);
begin
  for var I:Integer in A do
      Put(I);
end;

end.
