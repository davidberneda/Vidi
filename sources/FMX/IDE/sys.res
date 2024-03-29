        ��  ��                  �  $   ��
 S Y S       0         SomeThing {
  Owner : SomeThing
}

nil is SomeThing {}

Data is SomeThing {
  Value : Data { nil }
}

Boolean is Data {
  shared final True  : Boolean := True
  shared final False : Boolean := False
}

Number is Data {
 // TheNumber : Number
}

Number.Absolute:Number {
//  TheNumber>0 ? TheNumber : -TheNumber
}

Integer is -2147483648..2147483647 {}

Bit is 0..1 {}
SignedByte is  -128..127 {}
Byte is  0..255 {}
Word is -32768..32767 {}
UnsignedWord is  0..65535 {}
UnsignedInteger is  0..4294967295 {}
Integer64 is -9223372036854775808..9223372036854775807 {}
UnsignedInteger64 is 0..18446744073709551615 {}

// Workaround to skip "parameter not used" and "abstract call" warnings
hidden __Internal {
  shared Use(Item:SomeThing):Boolean { 
    Item=nil
  //  Item.Owner:=Item.Owner 
  }
}

Float is Number {
   Fraction :Float { }
   Round : Integer { 0 }
   Truncate: Integer { 0 }
}

Math {
  final Pi := 3.1415926535897932384626433
  final E :=2.71828182845904523536

  Sin : Number {}
  Cos : Number {}
  Tangent : Number {}
  Square(Value:Number) : Number { Value*Value }
  SquareRoot : Number {}
  Log : Number {}
  Ln : Number {}
  Exp : Number {} // ^?
  Sign(Value:Number) : Integer { Value<0 ? -1 : 1 }

  Modulo(Dividend,Divisor:Integer):Integer {
    a::=Dividend
    b::=Divisor

    if a<0 {
      if b>0 b:= -b

      while a<b a -=b
    }
    else {
      if b<0 b:= -b

      while a>b a -=b
    }

    return a
  }

  Power(Value,Exponent:Integer):Float {
    if Exponent=0
       return 1
    else
    if (Exponent=1) or (Value=0)
       return Value
    else
    {
      tmp:Float:=Value

      for 2..Exponent.Absolute
          tmp *=Value

      return Exponent<0 ? 1/tmp : tmp
    }
  }

  // TODO: Fraction Exponent (roots)
  PowerFraction(Value,Exponent:Float):Float {
    Power(Value.Truncate,Exponent.Truncate)
  }

  IsEven(Value:Integer):Boolean { Modulo(Value,2)=0 }
  IsOdd(Value:Integer):Boolean { Modulo(Value,2)<>0 }
}

Clauses {
  _Indexed,
  _Hidden,
  _Shared,
  _Final : Boolean

  Reset() {
    _Indexed:=False
    _Hidden:=False
    _Shared:=False
    _Final:=False
  }
}

SomeThings is SomeThing[] {}

Type is SomeThing {
  Clauses : Clauses

  Items : SomeThings // All Fields, methods, sub-types etc
  Parameters : SomeThings // generics
}

BinaryShift {
  Left(Value,Bits:Integer):Integer {  // <<
     __Internal.Use(Bits) 
     return Value
  }

  Right(Value,Bits:Integer):Integer { // >>
     __Internal.Use(Bits) 
     return Value
  }
}

// TODO:
// Metaclass: PersonClass:Type(Person)
//Type(T:Type) is Type {
//  Meta: T
//}

Array(T:Type) is Data {
  indexed Items:T[]

  Append(Item:T) { Items += Item }

  Clear() { Self:=[] }

  Copy:Array(T) { Self }

  Count:Integer { } // getter
  Count(Value:Integer) { __Internal.Use(Value) } // setter

  Delete(Index:Integer) { __Internal.Use(Index) }
  Delete(Index,Quantity:Integer) {
    __Internal.Use(Index)
    __Internal.Use(Quantity)
  }

  Insert(Index:Integer, Item:T) {
    __Internal.Use(Index)
    __Internal.Use(Item)
  }

  Low:Integer {0}
  High:Integer {0}

  Swap(a,b:Integer) {
    Temp::=Self[a]
    Self[a]:=Self[b]
    Self[b]:=Temp
  }
}

// TODO:
// Untyped array
//Array is Array(SomeThing) {}
//

Character is Data {

  ExistsIn(Text:Character[]):Boolean {
    for C in Text
        if C=Value
           return True
    return False
  }
}

shared Character.From(Code:Integer):Character {
  __Internal.Use(Code)
  C:Character
  return C
}

Type.Is(AItem:SomeThing, AType:Type):Boolean {
  __Internal.Use(AItem)
  __Internal.Use(AType)
  False
}

Text is Data {
  indexed Characters : Character[]

  Append(C:Character) // alias "+"
  {
    Characters += C
  }

  Append(T:Text) // alias "+"
  {
    Characters += T.Characters
  }

  // explicit(C:Character) { Characters:=[C] }

  Clear() { Characters:=[] }

  Lowercase : Text { //Characters.ForEach.Lowercase
    Self
  }

  Uppercase : Text { //Characters.ForEach.Uppercase
    Self
  }

  Length : Integer { Characters.Count }

  IsEmpty : Boolean { Length = 0 }
  Trim : Text { Self }
  StartsWith(S:Text) : Boolean { }
  EndsWith(S:Text) : Boolean { }
  IndexOf(S:Text) : Integer { }

  Contains(S:Text) : Boolean { IndexOf(S)>-1 }

  SameText(S:Text):Boolean {
    // for each character, compare case-insensitive
  }

  SubString(Start,Length : Integer) : Text { }
  SubString(Start:Integer): Text { } // overload

  IsFloat(S:Text, out Value:Float): Boolean {  } // TODO
  IsInteger(S:Text, out Value:Integer): Boolean { } // TODO
}

Number.AsText:Text {
  ""
}

Date {
  Day : 1..31
  Month : 1..12
  Year : Integer

  Today : Date {}

  Week : 1..53 {}
  DayOfYear : 1..366 {}
  WeekDay : 1..7 {}
  Quarter : 1..4 { 1+Math.Modulo(Month-1,4) }
  Decade : Integer {}
  DecadeOfYear : Integer {} // ?
  Century : Integer { 1+Math.Modulo(Year,100) }
  Millennium : Integer { 1+Math.Modulo(Year,1000) }
}

Time {
  Hour : 0..23
  Minute : 0..59
  Second : 0..59

  Now : Time { T:Time return T }

  Nanosecond : 0..99999 {}
  Microsecond : 0..9999 {}
  Millisecond : 0..999 {}
  HundredOfSecond : 0..99 {}
  TenthOfSecond : 0..9 {}
  QuarterHour : 1..4 {}
}

DateTime {
  Date : Date
  Time : Time
}

Operand is Data {
  Left, Right : Data
}

Logical is Operand {
//  Join : Logical {}
}

// [0..999,-123..456]
Range is Data {
  Min,
  Max : Data
}

// Types

NamedType is Type {
  Name : Text

  NameOf(AItem:SomeThing):Text {
    __Internal.Use(AItem)
    ""
  }
}

Console {

  Clear() { __Internal.Use(nil) }

  Put(Item:SomeThing...) { __Internal.Use(Item) }

  PutLine(Item:SomeThing...) {
     Put(Item,Character.From(13))
  }

  Get:Text { "" }
}

Exception {
  Text : Text
  Raise(Value:SomeThing) { __Internal.Use(Value) }
}

   s  0   ��
 V I D I D E M O         0         with UI

{
  Form1 : Form
  Form1.Title := 'Hello World !'

  Button1 : Button
  Button1.Title := 'Click'

  Form1.Add(Button1,10,60)

  Label1 : Label
  Form1.Add(Label1,10,10)

  //OnClick(C:Control) { Label1.Text:='Clicked !' C.Size.Width:=20 }

  //X:Control.ClickedMethod := OnClick
  //Button1.Clicked:= X // OnClick

  Form1.Modal
}

//OUT:
 