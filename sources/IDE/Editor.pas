unit Editor;

{$IFDEF INTERNAL}
{.$DEFINE INTERNAL_TIMING}
{$ENDIF}

interface

uses
  Controls, ExtCtrls,

  ComCtrls, // <-- InfoPanel
  Classes,

  Sys,

  Editor_VCL,

  Emit,
  Map, Bee.Parser,
  TheBee_VCL, Gutter, Error_Gutter,
  Usage, Editor_Pairs, Exceptions;

type
  TOnNodeClick=procedure(Sender:TObject; const ANode:TNode) of object;

  TCodeEditor=class(TCustomPanel)
  private
    FTree_Classes,
    FTree_AST : TVCLTreeAST;

    FOnChangeLineCol : TNotifyEvent;
    FOnNodeClick : TOnNodeClick;
    FOnNodeJump : TOnNodeClick;

    OldColumn,
    OldLine,
    OldSelected : Integer;

    PairBackup : TPair;

    procedure CloseCompletion;
    procedure CreateEdit;

    procedure EditClick(Sender: TObject);
    procedure EditInvalidate(Sender:TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditMouseMove(Sender:TObject; Shift: TShiftState; X, Y: Integer);
    procedure EditMouseUp(Sender:TObject; Button:TMouseButton; Shift: TShiftState; X, Y: Integer);

    function GetASTTree:TVCLTreeAST;
    function GetClassesTree:TVCLTreeAST;

    procedure PrepareRichText;

    procedure TryUnderLine(X, Y: Integer);
    procedure TryUnselect;
  public
    Edit : TCodeRichEdit;

    ErrorGutter : TErrorGutter;
    Gutter : TEditorGutter;

    ASTValid : Boolean;

    Context : TNode;
    Errors : TNodeErrors;
    Positions : TNodePositions;
    Usages : TNodeUsages;

    Completion : TControl;

    NeedsHighlight: Boolean;

    ModuleName : String;
    ModulePath : String;

    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure AddErrors(const AErrors:TNodeErrors);
    procedure BreakAt(const ALine:Integer);
    procedure ChangeLineCol;
    procedure ClearContext;

    class procedure DoHighlight(const AEdit:TCodeRichEdit; const APos:TNodePositions); static;

    procedure DoSelectPosition(const APosition:TNodePosition);
    function FileName:String;
    procedure Focus;
    procedure FromFile(const AFile:String);
    procedure FromText(const AText:String);
    procedure Init;
    function NodeAtCursor(const XDelta:Integer):TNode;
    function NodeAtOrBeforeCursor(const XDelta:Integer):TNode;
    function PartialWord:String;
    procedure RefreshBreakStops(const AStops:TStopsArray);
    procedure RecalcGutterWidth;
    procedure RefreshHighlight;
    procedure SelectNode(const ANode:TNode);
    procedure SelectPosition(const APosition:TNodePosition); overload;
    class procedure SelectPosition(const AEdit:TCodeRichEdit; const APosition:TNodePosition); overload; static;

    procedure SetParsed(const AParser:TBee; const ANode:TNode);

    procedure TryHighLight;

    property Tree_AST : TVCLTreeAST read GetASTTree;
    property Tree_Classes : TVCLTreeAST read GetClassesTree;

    property OnChangeLineCol : TNotifyEvent read FOnChangeLineCol write FOnChangeLineCol;
    property OnNodeClick : TOnNodeClick read FOnNodeClick write FOnNodeClick;
    property OnNodeJump : TOnNodeClick read FOnNodeJump write FOnNodeJump;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}

  SysUtils, Forms, StdCtrls, Buttons, Math,
  {$IFDEF FPC}
  Graphics, LCLType,
  {$ELSE}
  UITypes,
  {$ENDIF}

  {$IFDEF INTERNAL_TIMING}
  {$IFDEF FPC}
  FPC_StopWatch,
  {$ELSE}
  Diagnostics,
  {$ENDIF}
  {$ENDIF}

  IO, Constants, Parser, Highlight;

Constructor TCodeEditor.Create(AOwner:TComponent);
begin
  inherited;

  OldSelected:=-1;

  Gutter:=TEditorGutter.Create(Self);

  CreateEdit;
end;

Destructor TCodeEditor.Destroy;
begin
  ClearContext;
  inherited;
end;

procedure TCodeEditor.CreateEdit;
begin
  Edit:=TCodeRichEdit.Create(Self);

  Edit.OnClick:=EditClick;
  Edit.OnKeyDown:=EditKeyDown;
  Edit.OnKeyUp:=EditKeyUp;
  Edit.OnMouseMove:=EditMouseMove;
  Edit.OnMouseUp:=EditMouseUp;

  Gutter.Edit:=Edit;
end;

procedure TCodeEditor.EditMouseMove(Sender:TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ssCtrl in Shift then
     TryUnderline(X,Y)
  else
     TryUnselect;
end;

procedure TCodeEditor.EditMouseUp(Sender:TObject; Button:TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Completion<>nil then
     CloseCompletion;

  if OldSelected<>-1 then
     if Button=TMouseButton.mbLeft then
        if ssCtrl in Shift then
           FOnNodeJump(Self,Positions.Items[OldSelected].Node);
end;

function TCodeEditor.GetASTTree:TVCLTreeAST;
begin
  if FTree_AST=nil then
     FTree_AST:=TVCLTreeAST.Create;

  result:=FTree_AST;
end;

function TCodeEditor.GetClassesTree:TVCLTreeAST;
begin
  if FTree_Classes=nil then
     FTree_Classes:=TVCLTreeAST.Create;

  result:=FTree_Classes;
end;

procedure TCodeEditor.Init;
begin
  Gutter.Parent:=Self;
  Gutter.Align:=TAlign.alLeft;

  PrepareRichText;
end;

procedure TCodeEditor.RefreshBreakStops(const AStops:TStopsArray);
begin
  Gutter.Stops:=AStops;
  Gutter.Invalidate;
end;

procedure TCodeEditor.RefreshHighlight;
begin
  NeedsHighlight:=True;

  TryHighLight;
end;

procedure TCodeEditor.RecalcGutterWidth;
begin
  Gutter.AutoWidth(Edit.Lines.Count); // <-- slow?
  Gutter.Invalidate;
end;

procedure TCodeEditor.EditInvalidate(Sender:TObject);
begin
  RecalcGutterWidth;
end;

procedure TCodeEditor.SetParsed(const AParser:TBee; const ANode:TNode);
begin
  ClearContext;

  Context:=ANode;

  Positions:=AParser.Positions;
  Usages:=AParser.Usages;

// Leak?
//  AParser.Positions:=nil;
//  AParser.Usages:=nil;

  AddErrors(AParser.Errors);

  ASTValid:=True;

  NeedsHighlight:=True;
end;

function TCodeEditor.PartialWord: String;

  function DoBehind(P:Integer):String;
  var C: Char;
  begin
    result:='';

    while P>0 do
    begin
      C:=Edit.Text[P];

      if CharInSet(C,TParser.Identifier) or
         CharInSet(C,TParser.Digits) then
          result:=C+result
      else
          break;

      Dec(P);
    end;
  end;

  function DoForward(P:Integer):String;
  var C: Char;
  begin
    result:='';

    while P<Length(Edit.Text) do
    begin
      C:=Edit.Text[P];

      if CharInSet(C,TParser.Identifier) or
         CharInSet(C,TParser.Digits) then
          result:=result+C
      else
          break;

      Inc(P);
    end;
  end;

var tmp : Integer;
begin
  tmp:=Edit.CurrentPos;

  result:=DoBehind(tmp);

  if result='' then
     result:=DoForward(tmp+1);
end;

procedure TCodeEditor.PrepareRichText;
begin
  Edit.Parent:=Self;
  Edit.Align:=TAlign.alClient;

  Edit.ScrollBars:={$IFNDEF FPC}System.UITypes.{$ENDIF}TScrollStyle.ssBoth;
  Edit.HideSelection:=False;

  {$IFNDEF FPC}
  Edit.BevelInner:=bvNone;
  Edit.BevelOuter:=bvNone;
  {$ENDIF}

//   Edit.BorderStyle:=bsNone; <-- WRONG ! This recreatesWnd and looses Text because it is not in FMemStream

  Edit.Font.Name:='Courier New';
  Edit.Font.Size:=10;

  Edit.OnInvalidate:=EditInvalidate;
end;

function TCodeEditor.FileName: String;
begin
  result:=CombineFile(ModulePath,ModuleName+TVidiConstants.Extension);
end;

procedure TCodeEditor.Focus;
begin
  Edit.SelLength:=0;
  Edit.SetFocus;
end;

procedure TCodeEditor.FromFile(const AFile:String);
begin
  Edit.Lines.LoadFromFile(AFile);
end;

procedure TCodeEditor.FromText(const AText:String);
begin
  Edit.Text:=AText;
end;

class procedure TCodeEditor.DoHighlight(const AEdit:TCodeRichEdit;
                                        const APos:TNodePositions);
var tmp : TColor;
    P : TNodePosition;
    ShouldBold,
    ShouldItalic : Boolean;
    t : Integer;
begin
  for t:=0 to APos.Count-1 do
  begin
    P:=APos.Items[t];

    tmp:=THighlight.ColorOfNode(P,ShouldBold,ShouldItalic);

    {$IFDEF FPC}
    AEdit.SetRangeColor(P.Position.Position-1, P.Length, tmp);
    {$ELSE}
    SelectPosition(AEdit,P);

    AEdit.ChangeSelected(THighlight.StyleOf(ShouldBold,ShouldItalic),tmp);
    {$ENDIF}
  end;
end;

procedure TCodeEditor.TryHighLight;
{$IFDEF INTERNAL_TIMING}
var t1: TStopWatch;
{$ENDIF}

var Old : TCodeRichStatus;
    OldMask : Integer;
    //S : String;
begin
  if NeedsHighlight and (Positions<>nil) then
  begin
    // RichEditSuspendAll(Edit,True); undo suspend and wm_setredraw?

    Old:=Edit.CurrentStatus;
    Edit.OnChange:=nil;

    Edit.Lines.BeginUpdate;
    try
      OldMask:=Edit.GetEventMask;
      Edit.SetEventMask(0);

      {$IFDEF INTERNAL_TIMING}
      t1:=TStopWatch.StartNew;
      {$ENDIF}

      {
      S:=Edit.Text;
      Edit.Clear;
      Edit.Text:=S;
      }

      DoHighLight(Edit,Positions);

      {$IFDEF INTERNAL_TIMING}
      GetParentForm(Self).Caption:=GetParentForm(Self).Caption+' Highlight: '+t1.ElapsedMilliseconds.ToString;
      {$ENDIF}

      Edit.SetEventMask(OldMask);
    finally
      Edit.Lines.EndUpdate;
    end;

    NeedsHighlight:=False;

    // Reset back cursor position etc
    Edit.ResetBack(Old);
  end;
end;

procedure TCodeEditor.ChangeLineCol;
begin
  if Assigned(FOnChangeLineCol) then
     FOnChangeLineCol(Self);
end;

procedure TCodeEditor.ClearContext;
begin
  Context:=nil;

  ASTValid:=False;

  Positions:=nil;
  Usages:=nil;

  NeedsHighlight:=True;

  FTree_AST.Free;
  FTree_AST:=nil;

  FTree_Classes.Free;
  FTree_Classes:=nil;
end;

function TCodeEditor.NodeAtOrBeforeCursor(const XDelta:Integer):TNode;
begin
  if Positions=nil then
     result:=nil
  else
     result:=Positions.NodeAtOrBefore(Edit.CaretLine,Max(1,Edit.Column+XDelta));
end;

function TCodeEditor.NodeAtCursor(const XDelta:Integer):TNode;
begin
  if Positions=nil then
     result:=nil
  else
     result:=Positions.NodeAt(Edit.CaretLine,Max(1,Edit.Column+XDelta));
end;

procedure TCodeEditor.EditClick(Sender: TObject);
var tmp : TNode;
begin
  ChangeLineCol;

  PairBackup.TryHighLight(Edit);

  if Assigned(FOnNodeClick) then
  begin
    tmp:=NodeAtCursor(0);

    if tmp<>nil then
       FOnNodeClick(Self,tmp);
  end;
end;

procedure TCodeEditor.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

  function TryJumpNodeUnderCaret:Boolean;
  var tmp : TNode;
  begin
    tmp:=Positions.NodeAt(Edit.CaretLine,Edit.Column);

    result:=tmp<>nil;

    if result then
       FOnNodeJump(Self,tmp);
  end;

  procedure GotoEnd;
  begin
    if Completion is TListBox then
       TListBox(Completion).ItemIndex:=TListBox(Completion).Count-1;
  end;

  procedure GotoItem(const AIndex:Integer);
  begin
    if Completion is TListBox then
       TListBox(Completion).ItemIndex:=AIndex;
  end;

  procedure ChangeItem(const Amount:Integer);
  var L : TListBox;
  begin
    if Completion is TListBox then
    begin
      L:=TListBox(Completion);

      if Amount>0 then
         L.ItemIndex:=Min(L.Count-1,L.ItemIndex+Amount)
      else
         L.ItemIndex:=Max(0,L.ItemIndex+Amount);
    end;
  end;

  procedure CompletionKey;
  begin
    case Key of
      VK_UP: ChangeItem(-1);
    VK_DOWN: ChangeItem(1);
    VK_NEXT: ChangeItem(5);
   VK_PRIOR: ChangeItem(-5);
    VK_HOME: GotoItem(0);
     VK_END: GotoEnd;
    else
      exit;
    end;

    Key:=0;
  end;

begin
  OldColumn:=Edit.Column;
  OldLine:=Edit.CaretLine;

  if Key=VK_RETURN then
  begin
    if Shift=[ssCtrl] then
    begin
      if TryJumpNodeUnderCaret then
         Key:=0;
    end;
  end
  else
  if (Completion<>nil) and Completion.Visible then
     CompletionKey
end;

procedure TCodeEditor.CloseCompletion;
begin
  Completion.Parent.Hide;
  Completion:=nil;

  Edit.SetFocus;
end;

procedure TCodeEditor.EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

  procedure DeleteWord;
  const LetterDigits= ['a'..'z','A'..'Z','_','0'..'9'];
  var L : Integer;
  begin
    L:=0;

    if Edit.SelStart>=0 then
       while CharInSet(Edit.Text[L+1+Edit.SelStart],LetterDigits) do
             Inc(L);

    if L>0 then
    begin
      Edit.SelLength:=L;
      Edit.SelText:='';
    end;
  end;

  procedure FinishCompletion;
  begin
    case Key of
      VK_ESCAPE: CloseCompletion;
      VK_RETURN: TListBox(Completion).OnDblClick(Completion);
        VK_LEFT: if (not Edit.CurrentCharIsIndentifier) or (Edit.WordUnderCursor='') then
                     CloseCompletion;
    else
        if Edit.WordUnderCursor='' then
           CloseCompletion;
    end;
  end;

  function CharRelativeAt(const Offset:Integer):Char;
  begin
    result:=Edit.CharAt(Offset);
  end;

  function Spaces(const Amount:Integer):String;
  var t : Integer;
  begin
    {$IFDEF FPC}
    result:='';
    {$ENDIF}

    SetLength(result,Amount);
    for t:=1 to Amount do result[t]:=' ';
  end;

  procedure IndentSelected(const Amount:Integer);
  var OldLength: Integer;

    procedure DoIndent;
    var t, tt,
        tmp0, tmp1 : Integer;
        tmpAdd,
        S : String;
    begin
      tmp0:=Edit.LineOfPosition(Edit.SelStart)-1;
      tmp1:=Edit.LineOfPosition(Edit.SelStart+Edit.SelLength)-1;

      Edit.Lines.BeginUpdate;
      try
        if Amount>0 then
        begin
          tmpAdd:=Spaces(Amount);

          for t:=tmp0 to tmp1 do
              Edit.Lines[t]:=tmpAdd+Edit.Lines[t];

          OldLength:=OldLength+(tmp1-tmp0)*Amount;
        end
        else
        begin
          for t:=tmp0 to tmp1 do
          begin
            S:=Edit.Lines[t];

            for tt:=1 to -Amount do
                if Copy(S,1,1)=' ' then
                begin
                  Delete(S,1,1);
                  Dec(OldLength);
                end;

            Edit.Lines[t]:=S;
          end;
        end;
      finally
        Edit.Lines.EndUpdate;
      end;
    end;

  var OldStart : Integer;
  begin
    OldStart:=Edit.SelStart;
    OldLength:=Edit.SelLength;

    DoIndent;

    Edit.SelStart:=OldStart;
    Edit.SelLength:=OldLength;
  end;

  procedure MoveSelection(const AOffset:Integer);
  begin
    Edit.SelStart:=Edit.SelStart+AOffset;
  end;

  procedure TryKeepColumn;
  var Old : Boolean;
      S : String;
      OldStart : Integer;
  begin
    if Edit.Column<OldColumn then
    begin
      Old:=Edit.Modified;
      Edit.SelText:=Spaces(OldColumn-Edit.Column);
      Edit.Modified:=Old;
    end;

    if OldLine>0 then
    begin
      S:=TrimRight(Edit.Lines[OldLine-1]);

      if Edit.Lines[OldLine-1]<>S then
      begin
        Old:=Edit.Modified;
        OldStart:=Edit.SelStart;
        Edit.Lines[OldLine-1]:=S;
        Edit.Modified:=Old;
        Edit.SelStart:=OldStart;
      end;
    end;
  end;

  function IsCharKey(const C:Char):Boolean;
  begin
    result:=(Key=Ord(C)) or (Key=Ord(UpCase(C)));
  end;

begin
  if Completion<>nil then
     FinishCompletion;

  if Shift=[ssCtrl] then
  begin
    if IsCharKey('T') or (Key=VK_DELETE) then
       DeleteWord
    else
    {
    // Already done at Unit_TheBee level:
    if Key=VK_F8 then
    begin
      Gutter.ToggleBreak(Edit.CaretLine);
      Gutter.Invalidate;
    end
    else
    }
    if Key=VK_RIGHT then
    begin
      if CharRelativeAt(1)='.' then
         MoveSelection(1)
    end
    else
    if Key=VK_LEFT then
    begin
      if CharRelativeAt(0)='.' then
         MoveSelection(-1)
    end
    else
    if IsCharKey('M') then
       IndentSelected(2);
  end
  else
  if Shift=[ssCtrl,ssShift] then
  begin
    if IsCharKey('M') then
       IndentSelected(-2);
  end
  else
  if (Key=VK_UP) or (Key=VK_DOWN) then
{     TryKeepColumn};  // Fails

  ChangeLineCol;

  if Shift=[] then
     PairBackup.TryHighLight(Edit);
end;

procedure TCodeEditor.DoSelectPosition(const APosition:TNodePosition);
begin
  if APosition.Position.Line>-1 then
  begin
    SelectPosition(Edit,APosition);
    Edit.SetFocus;
  end;
end;

procedure TCodeEditor.SelectNode(const ANode: TNode);
var tmp : TNodePosition;
begin
  if Positions.Find(ANode,tmp) then
     DoSelectPosition(tmp);
end;

class procedure TCodeEditor.SelectPosition(const AEdit:TCodeRichEdit; const APosition:TNodePosition);
begin
//  AEdit.SelectText(APosition.Position.Line,APosition.Position.Column,APosition.Length);
  AEdit.SelectText(APosition.Position.Position
                   {$IFNDEF FPC}-APosition.Position.Line+1{$ENDIF}, // <-- CRLF Delphi / Windows vs LF-only FPC
                   APosition.Length);
end;

procedure TCodeEditor.SelectPosition(const APosition:TNodePosition);
begin
  SelectPosition(Edit,APosition);
end;

procedure TCodeEditor.TryUnderLine(X, Y: Integer);

  function PositionUnderMouse(out P:TNodePosition):Integer;
  var LineColumn: TPoint;
  begin
    if (Positions<>nil) and Edit.LineColumnAt(X,Y,LineColumn) then
    begin
      result:=Positions.NodeIndexAt(LineColumn.Y,LineColumn.X);

      if result<>-1 then
         P:=Positions.Items[result];
    end
    else
       result:=-1;
  end;

var P : TNodePosition;
    NewSelected : Integer;
begin
  NewSelected:=PositionUnderMouse(P);

  if NewSelected=-1 then
     TryUnselect
  else
  if NewSelected<>OldSelected then
  begin
    TryUnselect;

    OldSelected:=NewSelected;

    Edit.Underline(P.Position.Line,P.Position.Column,P.Length);
  end;
end;

procedure TCodeEditor.TryUnselect;
var //OldStart : Integer;
    //OldLength : Integer;
    tmp : TCodeRichStatus;
begin
  if OldSelected<>-1 then
  begin
    tmp:=Edit.CurrentStatus;
    Edit.SetOldUnderline;
    Edit.ResetBack(tmp);

    {
    OldStart:=Edit.SelStart;
    OldLength:=Edit.SelLength;

    Edit.SetOldUnderline;

    Edit.SelStart:=OldStart;
    Edit.SelLength:=OldLength;
    }

    OldSelected:=-1;

    Edit.Cursor:=crDefault;
  end;
end;

procedure TCodeEditor.AddErrors(const AErrors: TNodeErrors);

  procedure AddGutterSymbols;
  var E : TNodeError;
  begin
    Gutter.Symbols:=nil;

    for E in Errors do
        Gutter.AddSymbol(E.Position.Line,E.Text,E.Style);
  end;

  procedure AddErrorGutter;
  var t,L : Integer;
  begin
    L:=Length(Errors);

    if L>0 then
    begin
      if ErrorGutter=nil then
      begin
        ErrorGutter:=TErrorGutter.Create(Self);
        ErrorGutter.Parent:=Self;
        ErrorGutter.Align:=TAlign.alRight;
      end;

      ErrorGutter.Visible:=True;
      SetLength(ErrorGutter.Lines,L);
      ErrorGutter.TotalLines:=Edit.Lines.Count;

      for t:=0 to L-1 do
          ErrorGutter.Lines[t]:=Errors[t].Position.Line;

      ErrorGutter.Invalidate;
    end
    else
    if ErrorGutter<>nil then
    begin
      ErrorGutter.Visible:=False;
      ErrorGutter.Lines:=nil;
    end;
  end;

begin
  Errors:=AErrors;

  AddGutterSymbols;
  AddErrorGutter;
end;

procedure TCodeEditor.BreakAt(const ALine:Integer);
begin
  Gutter.SetCurrent(ALine);
end;

end.
