unit Vidi_Lang_English;

interface

procedure Change;

implementation

uses
  Syntax;

procedure Change;
begin
  with TErrorTexts do
  begin
    {$IFDEF INTERNAL}
    Texts[_InternalError]        := 'Internal error: ';
    {$ENDIF}

  Texts[_AncestorMethodMissing ] := 'Ancestor class %1 does not have method %2';
  Texts[_ArrayOutOfRangeMax    ] := 'Array index %1 greater than maximum %2';
  Texts[_ArrayOutOfRangeMin    ] := 'Array index %1 lower than minimum %2';
  Texts[_ArraySizeNotEvaluable ] := 'Array size cannot be evaluated';
  Texts[_AssignmentIsUseless   ] := 'Assignment is useless';
  Texts[_BadIdentifier         ] := 'Bad identifier: %1';
  Texts[_BadParameter          ] := 'Bad Parameter: %1';
  Texts[_BadStatement          ] := 'Bad statement';
  Texts[_BaseClassCannotbeSame ] := 'Base class cannot be the same class';
  Texts[_CannotAccessNonShared ] := 'Cannot access non-shared data from type: %1';
  Texts[_CannotArrayOfType     ] := 'Wrong array size of type: %1';
  Texts[_CannotAssign          ] := 'Cannot assign: %1 to: %2';
  Texts[_CannotBreakContinue   ] := 'Not inside a loop';
  Texts[_CannotCallAbstract    ] := 'Cannot call abstract routine';
  Texts[_CannotDeriveFromFinal ] := 'Cannot derive from final class';
  Texts[_CannotFindAncestor    ] := 'Ancestor is not accessible';
  Texts[_CannotModifyFinal     ] := 'Cannot modify final variable';
  Texts[_CannotOverrideFinal   ] := 'Cannot override final method: %1';
  Texts[_CannotReturnHere      ] := 'Cannot return here';
  Texts[_CastingUnnecessary    ] := 'Unnecessary casting. [%1]';
  Texts[_CatchOrFinallyExpected] := 'Catch or finally expected';
  Texts[_ClassHasNoAncestor    ] := 'Class has no Ancestor';
  Texts[_ClassNotUsed          ] := 'Class is never used';
  Texts[_CodeBlockEmpty        ] := 'Code block cannot be empty';
  Texts[_ConditionAlwaysTrue   ] := 'Condition is always True';
  Texts[_ConditionAlwaysFalse  ] := 'Condition is always False';
  Texts[_ConditionNotLogical   ] := 'Wrong condition, not logical: %1';
  Texts[_DataIsNotCall         ] := 'Data is not a call: %1';
  Texts[_DifferentClauses      ] := 'Different clause attributes';
  Texts[_DivisionByZero        ] := 'Division by zero';
  Texts[_Duplicate             ] := 'Duplicate: %1';
  Texts[_EmptyItem             ] := 'Error. Empty parsed item'; // parser error
  Texts[_EmptyElse             ] := 'Empty Else block';
  Texts[_EmptyThen             ] := 'Empty Then block';
  Texts[_EnumNotFinal          ] := 'Enumeration item %1 must be final';
  Texts[_Expected              ] := 'Expected: %1';
  Texts[_ExpressionAlwaysSameValue ] := 'Expression has always the same value';
  Texts[_ExpressionIsAlwaysFalse ] := 'Expression is always False';
  Texts[_ExpressionIsAlwaysTrue] := 'Expression is always True';
  Texts[_ExpressionIsOne       ] := 'Expression is one';
  Texts[_ExpressionIsZero      ] := 'Expression is zero';
  Texts[_ExpressionNotCallable ] := 'Expression cannot be called';
  Texts[_ExpressionNotEvaluable] := 'Expression cannot be evaluated at compile time';
  Texts[_ExtenderNotARoutine   ] := 'Extender is not a routine: %1';
  Texts[_ExtraCodeNotAllowed   ] :=  'Extra code not allowed';
  Texts[_FinalClassHasAbstract ] := 'Final class cannot contain abstract routines: %1';
  Texts[_FinalClassIsEmpty     ] := 'Final class cannot be empty';
  Texts[_FinalMethodIsEmpty    ] := 'Final method: %1 cannot be empty';
  Texts[_HiddenAccessScope     ] := 'Hidden item cannot be accessed outside its scope: %1';
  Texts[_IdentifierNotFound    ] := 'Identifier not found: %1';
  Texts[_IfThenElseAreIdentical] := 'Then and Else are identical';
  Texts[_IncompatibleLeftRight ] := 'Incompatible left and right sides of operand';
  Texts[_IncompatibleType      ] := 'Type not compatible';
  Texts[_IncompatibleGeneric   ] := 'Generic Type not compatible: %1';
  Texts[_InfiniteLoop          ] := 'Infinite loop';
  Texts[_InvertedOrder         ] := 'Inverted order';
  Texts[_MethodCannotBeShared  ] := 'Method %1 cannot be shared';
  Texts[_MethodIsEmpty         ] := 'Method must not be empty';
  Texts[_MethodIsEmptyAncestor ] := 'Method must be more than Ancestor';
  Texts[_MethodMustOverride    ] := 'Method must be overriden: %1';
  Texts[_MissingParameters     ] := 'Missing parameters';
  Texts[_MissingTypeParam      ] := 'Missing type for parameter: %1';
  Texts[_ModuleNotFound        ] := 'Cannot find module: %1 at: %2';
  Texts[_ModuleSelfReference   ] := 'Module [%1] cannot reference itself';
  Texts[_NeverExecuted         ] := 'Code is never executed';
  Texts[_NoOutputReturn        ] := 'Routine %1 is not returning in all cases';
  Texts[_NotAnArray            ] := 'Not an array';
  Texts[_NotAVariable          ] := 'Not a variable: %1';
  Texts[_OutParameterFirst     ] := '"Out" parameter must be assigned first';
  Texts[_OutParameterMustBeVariable ] := 'Out parameter must be passed as a variable';
  Texts[_OutParameterNotAssign ] := '"Out" parameter must be assigned';
  Texts[_OutParameterUsedTwice ] := 'Out parameter cannot be used twice';
  Texts[_Overflow              ] := 'Overflow %1 %2';
  Texts[_ParameterNotUsed      ] := 'Parameter is never used';
  Texts[_RepeatInfinite        ] := '"Repeat" loop is infinite';
  Texts[_ReturnNeedsType       ] := 'Return must be of type: %1';
  Texts[_RoutineNotUsed        ] := 'Routine is never used';
  Texts[_SameDataBothSides     ] := 'Same data at both sides';
  Texts[_SetterMissing         ] := 'Cannot find setter routine';
  Texts[_TypeNotFound          ] := 'Type not found: %1';
  Texts[_Underflow             ] := 'Underflow %1 %2';
  Texts[_Unknown               ] := 'Unknown: %1';
  Texts[_UnreachableCode       ] := 'Unreachable code';
  Texts[_VariableNotUsed       ] := 'Variable is never used';
  Texts[_WhileInfinite         ] := '"While" loop is infinite';
  Texts[_WrongCastingParams    ] := 'Wrong number of casting parameters. [%1]';
  Texts[_WrongExpression       ] := 'Wrong expression';
  Texts[_WrongForInExpression  ] := 'Wrong "in" expression';
  Texts[_WrongParamCount       ] := 'Wrong number of parameters';
  Texts[_WrongTypeExpression   ] := 'Wrong type expression';
  end;
end;

// Not used:
// Texts[_CannotDeriveClassFrom ] := 'Cannot derive class from type';
// Texts[_CannotFindArrayType   ] := 'Cannot obtain type of array expression';
// Texts[_CannotFindStart       ] := 'Missing starting routine {}';
// Texts[_CannotInferType       ] := 'Cannot determine variable type from value';
// Texts[_CastingExpression     ] := 'Casting parameter should be an expression. [%1]';
// Texts[_DoubleAssignment      ] := 'Double assignment in sequence';   ("useless assignment" wins)
// Texts[_ExpressionNotInteger  ] := 'Expression is not an Integer number';
// Texts[_ForNeedsCounterVariable ] :=  'For in needs counter variable';
// Texts[_FunctionTypeEmpty     ] := 'Function type must be initialized';
// Texts[_MultipleRoutines      ] := 'Multiple routines not allowed';
// Texts[_NotLogicalType        ] := 'Not logical type: %1';
// Texts[_ParentNotAClass       ] := 'Parent is not a class';
// Texts[_ReturnMustBeEmpty     ] := 'Return must be empty';
// Texts[_ReturnNotValid        ] := 'Cannot return from a non-routine';
// Texts[_VariableNotInitialized] := 'Variable not initialized';

initialization
  Change;
end.
