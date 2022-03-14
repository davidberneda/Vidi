unit Highlight;

interface

uses
  {$IFDEF FPC}
  Graphics,
  {$ELSE}
  UITypes,
  {$ENDIF}
  Sys, Map, AST;

type
  THighlightColors=record
  private
    class procedure Init; static;
  public
   class var
    AbstractRoutine,
    Comment,
    Identifier,
    Keyword,
    Number,
    Operand,
    RoutineCall,
    Statement,
    Symbol,
    Text,
    TypeClass,
    TypeNode  : TColor;
  public
    class procedure ChangeColors(Day:Boolean); static;
  end;

  THighlight=class
  class var
    Colors : THighlightColors;

    class function ColorOf(ANode:TNode; out ShouldBold,ShouldItalic:Boolean):TColor; static;
    class function ColorOfNode(const APosition:TNodePosition; out ShouldBold,ShouldItalic:Boolean):TColor; static;
    class function StyleOf(const Bold,Italic:Boolean): TFontStyles; static;
  end;

implementation

uses
  Checker.AST;

class function THighlight.ColorOf(ANode:TNode; out ShouldBold,ShouldItalic:Boolean):TColor;
begin
  if ANode is TClassType then
  begin
    result:=THighlight.Colors.TypeClass;
    Exit;
  end;

  if ANode is TTypeCall then
     ANode:=TTypeCall(ANode).TheType
  else
  if ANode is TDataCall then
     ANode:=TDataCall(ANode).Routine
  else
  if ANode is TType then
     ShouldBold:=True;


  if ANode is TStatement then
     result:=THighlight.Colors.Statement
  else
  if ANode is TType then
     if ANode is TRoutine then
        result:=THighlight.Colors.RoutineCall
     else
        result:=THighlight.Colors.TypeNode
  else
  //if ANode is TComment then
  //   result:=THighlight.Colors.Comment
  //else
  if (ANode is TInteger) or (ANode is TFloat) then
     result:=THighlight.Colors.Number
  else
  if ANode is TText then
     result:=THighlight.Colors.Text
  else
  if ANode is TOperand then
     result:=THighlight.Colors.Operand
  else
     result:=THighlight.Colors.Identifier;

  ShouldItalic:=False;

  if ANode is TRoutine then
  begin
    if TRoutine(ANode).Clauses.Shared then
       ShouldItalic:=True;

    if TChecker.IsAbstract(TRoutine(ANode)) then
       result:=THighlight.Colors.AbstractRoutine;
  end
  else
  if ANode is TVariable then
    if TVariable(ANode).Clauses.Shared then
       ShouldItalic:=True;
end;

class function THighlight.ColorOfNode(const APosition:TNodePosition; out ShouldBold,ShouldItalic:Boolean):TColor;
begin
  ShouldBold:=False;
  ShouldItalic:=False;

  if APosition.Style=TPositionStyle.Comment then
     result:=THighlight.Colors.Comment
  else
  if APosition.Style=TPositionStyle.Keyword then
  begin
    result:=THighlight.Colors.Keyword;
    ShouldBold:=True;
  end
  else
  if APosition.Style=TPositionStyle.Symbol then
     if APosition.Node is TOperand then
        result:=THighlight.Colors.Operand
     else
        result:=THighlight.Colors.Symbol
  else
     result:=THighlight.ColorOf(APosition.Node,ShouldBold,ShouldItalic);
end;

class function THighlight.StyleOf(const Bold,Italic:Boolean): TFontStyles;
begin
  if Bold then
     result:=[TFontStyle.fsBold]
  else
     result:=[];

  if Italic then
     result:=result+[TFontStyle.fsItalic];
end;

{ THighlightColors }

class procedure THighlightColors.ChangeColors(Day: Boolean);
const
  BlackColor:TColor={$IFDEF FPC}clBlack{$ELSE}TColors.Black{$ENDIF};
  WhiteColor:TColor={$IFDEF FPC}clWhite{$ELSE}TColors.White{$ENDIF};
begin
  if Day then
  begin
    Identifier:=BlackColor;
    Operand:=BlackColor;
    RoutineCall:=BlackColor;
  end
  else
  begin
    Identifier:=WhiteColor;
    Operand:=WhiteColor;
    RoutineCall:=WhiteColor;
  end;
end;

class procedure THighlightColors.Init;
begin
  AbstractRoutine := {$IFDEF FPC}clRed{$ELSE}TColors.Red{$ENDIF};
  Comment   := TColor($797979);
  Identifier := {$IFDEF FPC}clBlack{$ELSE}TColors.Black{$ENDIF};
  Keyword   := {$IFDEF FPC}clNavy{$ELSE}TColors.Navy{$ENDIF};
  Number    := {$IFDEF FPC}clPurple{$ELSE}TColors.Purple{$ENDIF};
  Operand   := {$IFDEF FPC}clBlack{$ELSE}TColors.Black{$ENDIF};
  RoutineCall := {$IFDEF FPC}clBlack{$ELSE}TColors.Black{$ENDIF};
  Statement := {$IFDEF FPC}clMaroon{$ELSE}TColors.Maroon{$ENDIF};
  Symbol    := {$IFDEF FPC}clBlue{$ELSE}TColors.Blue{$ENDIF};
  Text      := {$IFDEF FPC}clGreen{$ELSE}TColors.Green{$ENDIF};
  TypeClass := $161E64;
  TypeNode  := {$IFDEF FPC}clGreen{$ELSE}TColors.DarkGreen{$ENDIF};
end;

initialization
  THighlightColors.Init;
end.

