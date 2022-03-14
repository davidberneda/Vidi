unit Syntax;

interface

type
  TSyntax=class
  const
    _BeginBlock       = '{';
    _EndBlock         = '}';
    _DataMember       = '.';
    _BeginArray       = '[';
    _EndArray         = ']';
    _Assignment       = ':=';
    _TypeDelimiter    = ':';
    _ItemDelimiter    = ',';
    _BeginParameters  = '(';
    _EndParameters    = ')';
    _RangeDelimiter   = '..';
    _ManyValues       = '...';
    _NamespaceDelimiter = _DataMember;

    _Symbol_Equal     = '=';
    _Symbol_Add       = '+';
    _Symbol_Subtract  = '-';
    _Symbol_Multiply  = '*';
    _Symbol_Divide    = '/';
    _Symbol_Lower     = '<';
    _Symbol_Greater   = '>';
    _Symbol_Different = _Symbol_Lower+_Symbol_Greater; // <>

    _Add_Assign       = _Symbol_Add + _Symbol_Equal; // +=
    _Subtract_Assign  = _Symbol_Subtract + _Symbol_Equal; // -=
    _Multiply_Assign  = _Symbol_Multiply + _Symbol_Equal; // *=
    _Divide_Assign    = _Symbol_Divide + _Symbol_Equal; // /=

    _UnaryPositive    = _Symbol_Add;
    _UnaryNegative    = _Symbol_Subtract;

    // Reserved keywords:
    _Reserved_true       = 'True';
    _Reserved_false      = 'False';
    _Reserved_with       = 'with';    // with ast, ...
    _Reserved_is         = 'is';      // Text is Data {}
    _Reserved_final      = 'final';
    _Reserved_indexed    = 'indexed';
    _Reserved_condition  = '?';       // x:= a=b ? y : z
    _Reserved_if         = 'if';
    _Reserved_then       = '';        // 'then'
    _Reserved_else       = 'else';
    _Reserved_for        = 'for';
    _Reserved_while      = 'while';
    _Reserved_repeat     = 'repeat';
    _Reserved_until      = 'until';
    _Reserved_break      = 'break';
    _Reserved_continue   = 'continue';
    _Reserved_return     = 'return';  // return Foo
    _Reserved_in         = 'in';    // for Item in Items -- if C in Text

    _Reserved_and        = 'and';
    _Reserved_or         = 'or';
    _Reserved_xor        = 'xor';  // Boolean and Integer
    _Reserved_not        = 'not';  // not False
    _Reserved_out        = 'out';  // foo(out bar:Text)
    _Reserved_self       = 'Self';
    _Reserved_to         = 'to'; // for x::=1 to 10
    _Reserved_ancestor   = 'Ancestor';
    _Reserved_when       = 'when';  // swith case
    _Reserved_hidden     = 'hidden';
    _Reserved_shared     = 'shared';

    _Reserved_try        = 'try';
    _Reserved_catch      = 'catch';
    _Reserved_finally    = 'finally';
    _Reserved_raise      = 'raise';

    // Used at Random Test Generator only:
    ReservedKeywords : Array[0..30] of String=(
       _Reserved_true,
       _Reserved_false,
       _Reserved_with,
       _Reserved_is,
       _Reserved_final,
       _Reserved_indexed,
//       _Reserved_condition,
       _Reserved_if,
//       _Reserved_then,
       _Reserved_else,
       _Reserved_for,
       _Reserved_while,
       _Reserved_repeat,
       _Reserved_until,
       _Reserved_break,
       _Reserved_continue,
       _Reserved_return,
       _Reserved_in,

       _Reserved_and,
       _Reserved_or,
       _Reserved_xor,
       _Reserved_not,
       _Reserved_out,
       _Reserved_self,
       _Reserved_to,
       _Reserved_ancestor,
       _Reserved_when,
       _Reserved_hidden,
       _Reserved_shared,

       _Reserved_try,
       _Reserved_catch,
       _Reserved_finally,
       _Reserved_raise
  );
  end;

  TErrors= (
    {$IFDEF INTERNAL}
    _InternalError,
    {$ENDIF}
    _AncestorMethodMissing,
    _ArrayOutOfRangeMax,
    _ArrayOutOfRangeMin,
    _ArraySizeNotEvaluable,
    _AssignmentIsUseless,
    _BadIdentifier,
    _BadParameter,
    _BadStatement,
    _BaseClassCannotbeSame,
    _CannotAccessNonShared,
    _CannotArrayOfType,
    _CannotAssign,
    _CannotBreakContinue,
    _CannotCallAbstract,
//    _CannotDeriveClassFrom,
    _CannotDeriveFromFinal,
    _CannotFindAncestor,
//    _CannotFindArrayType,
//    _CannotFindStart,
//    _CannotInferType,
    _CannotModifyFinal,
    _CannotOverrideFinal,
    _CannotReturnHere,
//    _CastingExpression,
    _CastingUnnecessary,
    _CatchOrFinallyExpected,
    _ClassHasNoAncestor,
    _ClassNotUsed,
    _CodeBlockEmpty,
    _ConditionAlwaysTrue,
    _ConditionAlwaysFalse,
    _ConditionNotLogical,
    _DataIsNotCall,
    _DifferentClauses,
    _DivisionByZero,
//    _DoubleAssignment,
    _Duplicate,
    _EmptyItem,
    _EmptyElse,
    _EmptyThen,
    _EnumNotFinal,
    _Expected,
    _ExpressionAlwaysSameValue,
    _ExpressionIsAlwaysFalse,
    _ExpressionIsAlwaysTrue,
    _ExpressionIsOne,
    _ExpressionIsZero,
    _ExpressionNotCallable,
    _ExpressionNotEvaluable,
//    _ExpressionNotInteger,
    _ExtenderNotARoutine,
    _ExtraCodeNotAllowed,
    _FinalClassHasAbstract,
    _FinalClassIsEmpty,
    _FinalMethodIsEmpty,
//    _ForNeedsCounterVariable,
//    _FunctionTypeEmpty,
    _HiddenAccessScope,
    _IdentifierNotFound,
    _IfThenElseAreIdentical,
    _IncompatibleLeftRight,
    _IncompatibleType,
    _IncompatibleGeneric,
    _InfiniteLoop,
    _InvertedOrder,
    _MethodCannotBeShared,
    _MethodIsEmpty,
    _MethodIsEmptyAncestor,
    _MethodMustOverride,
    _MissingParameters,
    _MissingTypeParam,
    _ModuleNotFound,
    _ModuleSelfReference,
//    _MultipleRoutines,
    _NeverExecuted,
    _NoOutputReturn,
    _NotAnArray,
    _NotAVariable,
//    _NotLogicalType,
    _OutParameterFirst,
    _OutParameterMustBeVariable,
    _OutParameterNotAssign,
    _OutParameterUsedTwice,
    _Overflow,
    _ParameterNotUsed,
//    _ParentNotAClass,
    _RepeatInfinite,
//    _ReturnMustBeEmpty,
    _ReturnNeedsType,
//    _ReturnNotValid,
    _RoutineNotUsed,
    _SameDataBothSides,
    _SetterMissing,
    _TypeNotFound,
    _Underflow,
    _Unknown,
    _UnreachableCode,
//    _VariableNotInitialized,
    _VariableNotUsed,
    _WhileInfinite,
    _WrongCastingParams,
    _WrongExpression,
    _WrongForInExpression,
    _WrongParamCount,
    _WrongTypeExpression);

  TErrorTexts=class
  public
    {$IFDEF INTERNAL}
    class var TestMode : Boolean;
    class var Tested : Array[TErrors] of Boolean;
    {$ENDIF}

    class var
       Texts: Array[TErrors] of String;

    class function ReplaceParams(const AError:TErrors; const AParam1,AParam2:String):String; static;
  end;

implementation

class function TErrorTexts.ReplaceParams(const AError:TErrors; const AParam1,AParam2:String):String;

  function Replace(const AText,AFrom,ATo:String):String;
  var i : Integer;
  begin
    i:=Pos(AFrom,AText);

    if i>0 then
       result:=Copy(AText,1,i-1)+ATo+Copy(AText,i+2,Length(AText))
    else
    if ATo='' then
       result:=AText
    else
       result:=AText+' '+ATo;
  end;

begin
  result:=TErrorTexts.Texts[AError];

  if AParam1<>'' then
     result:=Replace(result,'%1',AParam1);

  if AParam2<>'' then
     result:=Replace(result,'%2',AParam2);
end;

end.
