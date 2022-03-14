unit PascalSyntax;

interface

uses
  Syntax, StringArray;

type
  TPascalSyntax=class(TSyntax)
  const
    _BeginBlock='{';
    _EndBlock='}';
    _DataMember='.';
    _BeginArray='[';
    _EndArray=']';
    _Assignment=':=';
    _TypeDelimiter=':';
    _ItemDelimiter=',';
    _BeginParameters='(';
    _EndParameters=')';
    _RangeDelimiter='..';

    _SingleLineComment='//';

    _EndOfStatement = ';';

    // Reserved

    _Reserved_true       = 'True';
    _Reserved_false      = 'False';
    _Reserved_Uses       = 'uses';
    _Reserved_abstract   = 'abstract';
    _Reserved_if         = 'if';
    _Reserved_then       = 'then';
    _Reserved_else       = 'else';
    _Reserved_for        = 'for';
    _Reserved_do         = 'do';
    _Reserved_while      = 'while';
    _Reserved_repeat     = 'repeat';
    _Reserved_until      = 'until';
    _Reserved_break      = 'break';
    _Reserved_continue   = 'continue';
    _Reserved_return     = 'return';  // return Foo
    _Reserved_in         = 'in';    // for Item in Items -- if C in Text
    _Reserved_and        = 'and';
    _Reserved_or         = 'or';
    _Reserved_xor        = 'xor';  // Booleand and Integer
    _Reserved_not        = 'not';  // not False
    _Reserved_out        = 'out';  // foo(out bar:Text)
    _Reserved_self       = 'self';
    _Reserved_nil        = 'nil'; // remove ?
    _Reserved_to         = 'to'; // for x::=1 to 10
    _Reserved_Inherited  = 'inherited';
    _Reserved_Case       = 'case';  // swith case
    _Reserved_shared     = 'class var';
    _Reserved_Private    = 'private';
    _Reserved_Public     = 'public';
    _Reserved_Const      = 'const';

    // Pascal

    _BeginComment        = '{';
    _EndComment          = '}';

    _Reserved_Array          = 'array';
    _Reserved_Begin          = 'begin';
    _Reserved_Boolean        = 'boolean';
    _Reserved_Class          = 'class';
    _Reserved_Constructor    = 'constructor';
    _Reserved_Create         = 'Create';
    _Reserved_End            = 'end';
    _Reserved_Except         = 'except';
    _Reserved_Exit           = 'exit';
    _Reserved_Finalization   = 'finalization';
    _Reserved_Finally        = 'finally';
    _Reserved_Function       = 'function';
    _Reserved_Interface      = 'interface';
    _Reserved_Implementation = 'implementation';
    _Reserved_Initialization = 'initialization';
    _Reserved_Label          = 'label';
    _Reserved_Of             = 'of';
    _Reserved_On             = 'on';
    _Reserved_Overload       = 'overload';
    _Reserved_Override       = 'override';
    _Reserved_Procedure      = 'procedure';
    _Reserved_Result         = 'result';
    _Reserved_Sealed         = 'sealed';
    _Reserved_Set            = 'set';
    _Reserved_Static         = 'static';
    _Reserved_Try            = 'try';
    _Reserved_Type           = 'type';
    _Reserved_Unit           = 'unit';
    _Reserved_Var            = 'var';
    _Reserved_Virtual        = 'virtual';

  class var
    Reserved :TStringArray;
  end;

implementation

procedure AddReserved;
begin
  with TPascalSyntax do
    Reserved.Add(
    [
          _Reserved_Array,
          _Reserved_Begin,
          _Reserved_Case,
          _Reserved_Class,
          _Reserved_Const,
          _Reserved_Constructor,
          _Reserved_Create,
          _Reserved_else,
          _Reserved_End,
          _Reserved_Except,
          _Reserved_Exit,
          _Reserved_Finalization,
          _Reserved_Finally,
          _Reserved_Function,
          _Reserved_Implementation,
          _Reserved_Inherited,
          _Reserved_Initialization,
          _Reserved_Interface,
          _Reserved_Label,
          _Reserved_nil,
          _Reserved_Of,
          _Reserved_On,
          _Reserved_Override,
          _Reserved_Private,
          _Reserved_Procedure,
          _Reserved_Public,
          _Reserved_Result,
          _Reserved_Sealed,
          _Reserved_Set,
          _Reserved_Static,
          _Reserved_then,
          _Reserved_Try,
          _Reserved_Type,
          _Reserved_Unit,
          _Reserved_Uses,
          _Reserved_Var,
          _Reserved_Virtual
    ]);
end;

initialization
  AddReserved;
end.
