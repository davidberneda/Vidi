# Vidi Language Reference

@davidberneda v.007 June-2020

**Important:** 
DRAFT. EVERYTHING MIGHT CHANGE.

### General concepts

Vidi is a strict typing, object oriented language that *borrows* most of its features from existing languages like Java, C#, Delphi / Pascal and others.

### Basic data types

#### Numeric

Numbers can be expressed in several ways:

```javascript
123     // Integer
-456    // Negative
123.45  // Float
123e2   // Exponent
0xFF    // Hexadecimal
0b11011 // Binary
0c217   // Octal
```



#### Text

Double or single quotes can be used to delimit text.

```javascript
"Hello"        // Double-quotes
'World'        // Single-quotes
"How're you?"  // Single quote inside double quotes
'Say "abc" !'   // Double quotes inside single quotes
```



#### Booleans

```javascript
True
False
```



These data types (numeric, text and booleans) are "*value types*". They are always copied when assigning variables:

```javascript
A : Integer := 123
B : Integer := A

// A and B are independent. Modifying A does not change B.

```

The rest of types (objects, arrays and functions) are always assigned by reference.



### Other data types

#### Arrays

```javascript
[ 1, 2, 3 ]  // Simple array
[ ["a","b"], ["c", "d"] ]  // Array inside array
```

#### Integer Ranges

Ranges express minimum and maximum values:

```javascript
1..10
-12..-2
```

Ranges can be used in several places, like when declaring an array:

```
MyArray : Integer[1..10]
```

Or to specify custom `Integer` types to benefit from overflow checking:

```javascript
// A custom Integer class
Podium is 1..3 {}

P : Podium := 4  // <-- Error. Overflow

// The 'Podium' class can also be used as an array dimension:

MyArray : Integer[Podium]  // same as Integer[1..3]  

```

Or to use a range in a `for` loop:

`for Num in 0..1000 { }`

Or in function parameters and result types:

`MyFunction( MyParam : 20..1000): 4..10 { }`



### Expressions

#### Logical

Boolean operators:

`and or not xor`

Conditional operator:

`2>1 ? True : False   //  Ternary` 

Membership operator:

```javascript
'A' in 'ABC'
3 in [1,2,3]
```



#### Arithmetic

```javascript
2 + 3 - 5 * (6 / -7) // Basic math
5 ^ 2   // Power
10 % 3   // Modulus 
2 << 4   // Shift left
32768 >> 4  // Shift right

255 or 0xFF
128 and 255
64 xor 32
not 123

"Hello" + "World" // Text addition
```

#### Comparative

Equality operators:

`=  <>  >  <  >=  <=`

#### Grouping

Parenthesis are used to group expressions:

`(4+2) * 6 - (5/9) * (Abc - Xyz)`



### Identifiers

Identifiers might begin with an alpha character (`a` to `z`) or `_` (underline), and then any digit (`0` to `9`), alpha or underline.

Examples:

```javascript
Abc
X123
My_Name
```



### Variables

The `:` symbol (colon) is used to separate the variable identifier (variable name) and its type:

Simple variables:

```javascript
A : Integer
B : Text
```

A variable can optionally define a *default* value (initial value) using the `:=` symbol:

`F : Float := 123.45   // Value initialization`

The Variable type can be optionally omitted to infer it from its initial value:

```javascript
Data ::= True   // Type inference  (Data is Boolean)
Planet ::= Earth   // Planet variable is of the same type as Earth value
```

Arrays are declared using the `[]` bracket symbols, and can also be optionally initialized:

```javascript
Colors : Text[] := [ "Red", "Blue" ]

Matrix : Float[ 3,3 ]   // Alternative way: Float[3][3]
```

Ranges and expressions can also be used to declare array dimensions:

`Numbers : Integer[ 1..2*10 ]   // 20 elements`



### Constants

The `final` keyword is used to define variables that cannot be modified (*readonly*):

`final Pi : :=  3.1415`



### Classes

Structures, records, classes and interfaces are the same thing in Vidi.

```javascript
Person {
  Name : Text
}
```



#### Class inheritance

A class can be extended from another class using the `is` keyword:

```javascript
Customer is Person {
  Code : Integer
}
```

In the above example, the `Customer` class derives from the  `Person` class.
`Person` is the ancestor class of `Customer`.

#### Class as parameter

The `Self` keyword (equivalent to *this* or *it* or *base* in other languages) represents the class instance itself.

```javascript
Foo is Integer {
  Bar() {
    SomeClass.Test(Self)  // Passing ourselves as parameter
  }
}
```

#### Sub-classes and sub-methods

Class types and procedures / routines / methods / functions can be nested, unlimited.

```javascript
Life {

  Tree {    // subclass
  
    Plant( Quantity : Integer) {  // method
    
      Forest is Text[] {    // subclass inside method
      }
      
      MyForest : Forest   // variable
    }
  }
 
}
```

Declaring a variable of a sub-class type:

`Pine : Life.Tree`

#### Class parameters

Exactly like methods, class parameters can be used when variables are declared to initialize (construct) them.

```javascript
Customer(SomeName:Text) is Person {
  Name:= SomeName
}

Cust1 : Customer("John")
Cust2 : Customer("Anne") 
```

#### Generic types

There is no special syntax for generic types.
Class parameters of type `type` can be used to specialize generic classes.

```javascript
with Types

List(T:Type) is T[] {}    // Parameter of type: Type

Numbers is List(Float) {}  // List of Float
```

#### Casting expressions

As there are no pointers, casting is only allowed within types of the same class hierarchy.

```javascript
Class1 {}
Class2 is Class1 {}

C2 : Class2
C1 : Class1 := C2

//  C2_bis : Class2 := C1 // <-- Error. 

C2_bis : Class2 := Class2(C1) // <-- Casting is OK

```

### Methods

Also called *routines*, *procedures* or *functions*.

  `Area : Float { return 123 }`

#### Parameters

All parameters to a method are passed by default by value (as constants).

`Make( Wheels : Integer ) {}`

The `out` keyword in front of a parameter means the parameter must be assigned a value:

`Parts( Style:Text, out Price:Float ):Boolean { Price:=123 }`

#### Many-Values parameters

The last parameter of a method can be declared with the special `...` prefix, to allow passing an undetermined number of parameters.

This is just syntactic sugar of passing an array without the need of typing the `[ ]` symbols around values.

```javascript
Print( Values : Data...) { 
 for Value in Values Console.PutLine(Value)
}

// Call examples:
Print
Print('abc')
Print(123,'abc',True)  

TypedPrint( Values : Integer... ) {
 for Value in Values Console.PutLine(Value)
}

TypedPrint(7,8,9,10,11)  // similar to: [7,8,9,10,11]

```

#### Method Overloads

Routines can have the same name if they have different parameters and/or return values:

```javascript
Write( Number : Integer) {}
Write( Number : Float):Integer[] {}
Write( Number : Text, Other : Boolean) {}
```



#### Method inheritance

A child class can declare methods with exactly the same name, parameters and return values as its ancestor parent class.

```javascript
Class1 {
  Proc() {}
}

Class2 is Class1 {
  Proc() {
    Ancestor  // calls Class1.Proc
  }
}
```

The `Ancestor` keyword refers to its parent method.

#### Non-inheritable methods (final)

Methods can be declared `final` to forbid overriding them in derived classes.

`final Proc() {}`

#### Abstract methods

When a method body is empty, it is considered abstract. 
There is no special syntax.

That means the method cannot be called (an error at compile time) and that derived classes must implement (override) it and fill it with content.





### Interfaces

There is no special syntax to declare interfaces.
Simple classes that have no fields (no variables), and all their methods are abstract, are always considered interfaces.

```javascript
MyInterface {
   MyMethod( Data : Boolean ):Text {}   // abstract function
}
```

Classes can be derived from interfaces as usually:

```javascript
MyClass is MyInterface {   // Deriving from an interface
  MyMethod( Data : Boolean ):Text { return "abc" } // must implement abstract method
} 
```

#### Soft Interfaces

Classes that have methods with exactly the same name, parameters and return values of methods of an interface, can be used like instances of that interface.

```javascript
SomeClass {
  MyMethod( Data : Boolean ):Text { return "abc" }
}

// This method requires a MyInterface parameter
Example( Value : MyInterface) {
  Value.MyMethod(True)
}

Some1 : SomeClass
Example(Some1)  // Some1 variable can be considered of MyInterface type
```

In the above code, `SomeClass` class is not derived from `MyInterface` but can be used as if it was.

### Modules

The `with` keyword imports (loads) modules located in separate files.

It can be used anywhere on a file, not only at the top.
Imported symbols are only available at the scope after `with`.

```javascript
with Module1, Module2, Module3.MyClass

MyClass {
  with SomeModule  // inner scope with

  Test : SomeClass // Class declared inside SomeModule
}

// SomeModule symbols cannot be accessed here, outside MyClass scope
```

### Element visibility

The `hidden` keyword prefixing a class, field or method makes it unavailable outside its scope.

```javascript
hidden MyClass {
  hidden MyField : Integer
  hidden MyFunction : Boolean {}
  hidden MySubClass {}
}
```

Unused hidden items will produce an error at compile-time.

### Type-level shared elements

The `shared` keyword means an element (variable or method) belongs to type-level, not instance-level.
This is the equivalent of *class variables* in other languages.

```javascript
Colors {
  shared Default : Text := "Red"
}

Colors.Default := "Blue"   // Can be used at type-level, without any instance
```

Type-level methods are auto-discovered. There is no special syntax to declare them.
When a method do not access any non-shared field or non-type level methods, it is considered `shared`.

```javascript
// Type-level procedure, no shared keyword necessary
SetDefault( Value : Text) { Default:=Value }

Colors.SetDefault( "Green" )
```





### Namespaces

There is no special syntax to declare namespaces.
Classes with no fields and no methods are considered namespaces. 

```javascript
// Module1
MyNamespace {
  MyClass {}
}
```

Modules with duplicate namespace names can be merged, to aggregate (contribute) new classes to the same namespace

```javascript
// Module2
MyNamespace {
  OtherClass {}
}
```

The `with` keyword can also be used to reference just only a sub element instead of to everything in the module

```javascript
// Module3
with Module1.MyNameSpace,
     Module2.MyNameSpace

Test is OtherClass {
  Some : MyClass
}
```



### Strong Typing

Deriving one type from another just for the convenience of strict type checking:

```javascript
// Type alias

Year is Integer {}
Y : Year

Month is Integer {}
M : Month

Y:=M  // <-- Error. Different types.

Class1 {}
Class2 is Class1 {}

C1 : Class1
C2 : Class2 // := C1  <-- equivalent but forbidden (strict check)
```

Type discovery and reflection:

The `Type` class provides methods to inspect (reflect) existing types:

```javascript
if Type.is( C1, Class1 ) ...
Methods:Method[] := Type.Methods( C1 )
```



### Extenders

At any scope, including in other modules, types can be extended with new methods.
So for example we can declare this class:

```javascript
// Module 1
MyClass {
}
```

And then, inside the same module or in other external modules, we can declare new methods and subclasses of `MyClass` :

```javascript
// Module 2
with Module1

MyClass.MyProc() {}   // New extended Procedure
MyClass.MySub { X:Float }  // New extended Sub-class
```

These new extended elements can then be used as normal, also in different modules

```javascript
// Module 3
with Module1, Module2 

Foo : MyClass
Foo.MyProc()   // Calling an extension as if it was a normal method
```

These extensions are only available inside the scope where they are declared.



### Function types

Also called lambdas or anonymous methods, types can be used to define a function:

`MyProcType is (A:Text, B:Integer):Float {}`

This type can be used to declare a variable and give it an implementation:

`MyFunction : MyProcType := { A:=A + B.AsText }`

The `MyFunction` variable (of type function), can now be called or passed to other methods.

### Enumerable types

The `is {}` syntax is used to declare enumerations.

`Colors is { Red, Green, Blue, Yellow }`

Variables and constants can then use the enumeration items:

`MyColor : := Colors.Blue`

These enumerations can also be used as dimensions for arrays:

```javascript
Names : Text[Colors]  // Array of four text items
Names[Colors.Green] := "I Like Green"
```

And the `for in` statement can loop all the enumeration items:

```javascript
for Color in Colors {
  Console.PutLine(Color)
}
```



### Statements

#### Assignment

```javascript
a:=b
b:=c+d
```

#### If 

```javascript
if a=b
   foo
else
   bar
```

#### While

```javascript
while a=b {
  if a=0 break else a:= a - 1
}
```

#### Repeat

```javascript
repeat {
  b:=b+1

  if b=5 continue

} until a<>b
```

#### For

The `in` keyword can loop an enumerated type:

`for c in Colors {}`

Also the `in` keyword can be used to loop an array:

```javascript
Nums::=[ 6,2,9 ]
for i in Nums { Output.Write(i) }    // iterate an array
```

A Text expression is an array of characters so it can be also iterated:

`for c in "abc" {} // foreach character`

An integer range:

`for t in 1..10 {}`

Traditional loop using the `to` keyword:

`for x:=a to b {}`

The counter variable cannot be reused or accessed outside the `for` block.
It cannot be an already declared variable. Its type is inferred.

#### When

Also called *switch*, *select* or *case* in other languages.

```javascript
Name::="Jane"

when Name {
  "Jane" { }
  "Peter" { }
}
```

Comparison complex expressions can also be used:

```javascript
num ::= 5
abc ::= 3

when abc+num {
  <3 { num:=123 }

  4 { } // equals

  <>6 { } // not equal

  // otherwise
  else { }
}
```

#### Return

The return statement exits a method, with an optional value if the method is a function

```javascript
Test {
  Foo() { return }
  Bar:Text { return "abc" }
}

// The return keyword is optional with the last expression of a function 
Square(X:Float) { X*X }
```



### Recursivity

```javascript
Factorial(x:Integer):Float {
  x=0 ? 1 : x * Factorial(x-1)
}

Factorial(5)  // Returns 120
```



### Forward declarations

There are situations where methods should be called but are not yet declared.
These are handled automatically, no special syntax is necessary.

```javascript
TestForward() {
    TestInner(False)  // <-- not yet declared
}

TestInner(Work:Boolean) {
  if Work
     TestForward
}

TestInner(True)
```

### Properties

No special syntax for properties.
A property "getter" can be a field:

`MyFoo : Integer := 123`

Or a function:

`Foo : Integer { return MyFoo }`

And optionally, a property "setter" which is just a function with the same name:

`Foo(Value:Integer) { MyFoo:=Value } // Setter`

The compiler will handle property access transparently:

`Foo:=123  // will call the setter method: Foo(123)`



### Finalizers

Classes can define a single, unnamed, parameter-less `final` method that will be called when variables get out of scope.

```javascript
Shop {
  final {
     Console.PutLine( 'Closed!' )
   }
}
```



### Syntax

#### Reserved words

    ancestor
    and
    break
    continue
    else
    False
    final
    for
    hidden
    if
    in
    indexed
    is
    not
    or
    out
    repeat
    return
    self
    shared
    to
    True
    until
    when
    while
    xor
    with

### Reserved symbols

    { }
    .
    [ ]
    :=
    :
    ,
    ( )
    ..
    ?
    >
    >=
    <
    <=
    <>
    <<
    >>
    +
    -
    *
    /
    %
    ^
