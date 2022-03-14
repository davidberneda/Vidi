unit Editor_VCL;

interface

uses
  {$IFDEF FPC}
  RichMemo, RichMemoHelpers, LMessages,
  {$IFDEF MSWINDOWS}
  //RichEdit,
  {$ENDIF}
  {$ELSE}
  Winapi.RichEdit,
  {$ENDIF}

  Graphics, Classes, ComCtrls,
  Messages, Controls, Types;

type
  {$IFDEF FPC}
  TRichEdit=TRichMemo;
  {$ENDIF}

  TCodeRichStatus=record
  public
    Start,
    Length,
    Top : Integer;
    Modified : Boolean;
    OnChange : TNotifyEvent;
  end;

  TUnderline=record
    Line, Column, Length : Integer;
    Style : TFontStyles;
    Color : TColor;
  end;

  TCodeRichEdit=class(TRichEdit)
  private
    OldUnderline : TUnderline;

    ErasingBack : Boolean;

    {$IFNDEF FPC}
    PrivateFormat: TCharFormat2;
    {$ENDIF}

    procedure EnablePaint(const Enable:Boolean);
    function LineOf(const APos:Integer):Integer;
    function PosOfLine(const ALine:Integer):Integer;
  protected
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMVScroll(var Message: TWMVScroll); message CN_VSCROLL;

    {$IFDEF FPC}
    procedure WndProc(var Message: TLMessage); override;
    {$ENDIF}
  public
    OnInvalidate:TNotifyEvent;

    Constructor Create(AOwner:TComponent); override;

    function CaretLine:Integer;
    procedure ChangeFont(const AFont:TFont);
    procedure ChangeSelected(const AStyle:TFontStyles; const AColor:TColor);
    procedure ChangeStyle(const AStyle:TFontStyles; const AColor:TColor);
    function CharAt(const AOffset:Integer):Char;
    function CharFromPos(const P:TPoint): Integer;
    function Column:Integer;
    function CurrentCharIsIndentifier:Boolean;
    function CurrentPos:Integer;
    function CurrentStatus:TCodeRichStatus;
    function GetEventMask:Integer;
    function GetFullText:String;
    function LineColumnAt(const X,Y:Integer; out LineColumn:TPoint):Boolean;
    function LineOfPosition(const APos:Integer):Integer;
    function PositionOf(const AIndex:Integer): TPoint;
    procedure ResetBack(const AStatus:TCodeRichStatus);
    procedure ScrollLines(const ALines:Integer);
    function ScrollPosition:TPoint;
    procedure SelectText(const APosition,ALength:Integer); overload;
    procedure SelectText(const ALine,AColumn,ALength:Integer); overload;
    procedure SetOldUnderline;
    procedure SetEventMask(const AMask:Integer);
    function TopLine: Integer;
    procedure Underline(const ALine,AColumn,ALength:Integer);
    function WordUnderCursor:String;
    function YPos(Line: Integer): Integer;
  end;

function Distance(const P1,P2:TPoint):Integer;

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}

  {$IFNDEF FPC}
  UITypes, // <-- inline hint only
  {$ENDIF}

  SysUtils;

function Distance(const P1,P2:TPoint):Integer;
begin
  result:=Round(Sqrt(Sqr(P1.X-P2.X)+Sqr(P1.Y-P2.Y)));
end;

{ TCodeRichEdit }

// From:
// https://codeverge.com/embarcadero.delphi.win32/richedit-unicode-fetch-selected-text/1052348
function TCodeRichEdit.GetFullText:String;
{$IFDEF FPC}
begin
  result:=Lines.Text; // super-fast
end;
{$ELSE}

  function GetNumberOfChars:Integer;
  var tmpLen: GETTEXTLENGTHEX;
  begin
    FillMemory(@tmpLen, SizeOf(tmpLen), 0);
    tmpLen.flags := GTL_NUMCHARS or GTL_USECRLF or GTL_PRECISE;
    tmpLen.codepage := 1200; // request unicode
    result := SendMessage(Handle, EM_GETTEXTLENGTHEX, Integer(@tmpLen), 0);
  end;

var tmpText : GETTEXTEX;
    NumChars: Integer;
    Buff: Pointer;
begin
  result := '';

  NumChars:=GetNumberOfChars;

  if NumChars <> E_INVALIDARG then
  begin
    // Prepare structure to get all text
    FillMemory(@tmpText, SizeOf(tmpText), 0);

    tmpText.cb := (NumChars + 1) * SizeOf(WideChar); // include room for a null terminator
    tmpText.flags := GT_USECRLF;
    tmpText.codepage := 1200; // request unicode

    Buff := GetMemory(tmpText.cb);
    try
      // Do the actual request
      NumChars := SendMessage(Handle, EM_GETTEXTEX, Integer(@tmpText), Integer(Buff));

      if NumChars > 0 then
         SetString(Result, PWideChar(Buff), NumChars);
    finally
      FreeMemory(Buff);
    end;
  end;
end;
{$ENDIF}

procedure TCodeRichEdit.ChangeFont(const AFont: TFont);
var OldStart,
    OldLength : Integer;
    OldModified : Boolean;
begin
  OldModified:=Modified;

  Font:=AFont;

  OldStart:=SelStart;
  OldLength:=SelLength;

  SelectAll;

  SelAttributes.Name:=Font.Name;
  SelAttributes.Size:=Font.Size;
  SelAttributes.Color:=Font.Color;
  SelAttributes.Style:=Font.Style;

  SelStart:=OldStart;
  SelLength:=OldLength;

  Modified:=OldModified;
end;

procedure TCodeRichEdit.ChangeSelected(const AStyle: TFontStyles;
  const AColor: TColor);
begin
  {$IFDEF FPC}
  SelAttributes.Style:=AStyle;
  SelAttributes.Color:=AColor;
  {$ELSE}

  with PrivateFormat do
  begin
    dwEffects:=0;

    if fsBold in AStyle then dwEffects := dwEffects or CFE_BOLD;
    if fsItalic in AStyle then dwEffects := dwEffects or CFE_ITALIC;
//    if fsUnderline in AStyle then dwEffects := dwEffects or CFE_UNDERLINE;
//    if fsStrikeOut in AStyle then dwEffects := dwEffects or CFE_STRIKEOUT;

    crTextColor := ColorToRGB(AColor);
  end;

  SendStructMessage(Handle, EM_SETCHARFORMAT, SCF_SELECTION, PrivateFormat);
  {$ENDIF}
end;

procedure TCodeRichEdit.ChangeStyle(const AStyle:TFontStyles; const AColor:TColor);
var OldChange : TNotifyEvent;
    OldLength : Integer;
begin
  OldChange:=OnChange;
  OnChange:=nil;
  try
    OldLength:=SelLength;

    SelAttributes.Style:=AStyle;
    SelAttributes.Color:=AColor;

    SelLength:=OldLength;
  finally
    OnChange:=OldChange;
  end;

  Modified:=False;
end;

function TCodeRichEdit.CharFromPos(const P:TPoint): Integer;
begin
  {$IFDEF MSWINDOWS}
  result:=SendMessage(Handle, EM_CHARFROMPOS, 0, NativeInt(@P));
  {$ELSE}
  result:=0 // TODO
  {$ENDIF}
end;

procedure TCodeRichEdit.SelectText(const APosition,ALength:Integer);
begin
  {$IFDEF MSWINDOWS}
  SendMessage(Handle, EM_SETSEL, APosition-1, APosition-1+ALength);

  {
  SelStart:=APosition-1;
  SelLength:=ALength;
  }

  //OLD: Perform(EM_SCROLLCARET, 0, 0);
  {$ELSE}
  // TODO
  {$ENDIF}
end;

procedure TCodeRichEdit.SelectText(const ALine,AColumn,ALength:Integer);
begin
  if ALine<=0 then
     SelectText(0,ALength)
  else
     SelectText(PosOfLine(ALine-1) + AColumn {-1},ALength);
end;

procedure TCodeRichEdit.SetEventMask(const AMask: Integer);
begin
  {$IFDEF MSWINDOWS}
  SendMessage(Handle, EM_SETEVENTMASK, 0, AMask);
  {$ELSE}
  // TODO
  {$ENDIF}
end;

procedure TCodeRichEdit.SetOldUnderline;
begin
  SelectText(OldUnderline.Line,OldUnderline.Column,OldUnderline.Length);
  ChangeStyle(OldUnderline.Style,OldUnderline.Color);
end;

procedure TCodeRichEdit.Underline(const ALine,AColumn,ALength:Integer);
var //OldStart : Integer;
    //OldLength : Integer;
    tmp : TCodeRichStatus;
begin
  tmp:=CurrentStatus;

  SelectText(ALine,AColumn,ALength);

  OldUnderline.Line:=ALine;
  OldUnderline.Column:=AColumn;
  OldUnderline.Length:=ALength;

  OldUnderline.Style:=SelAttributes.Style;
  OldUnderline.Color:=SelAttributes.Color;

  ChangeStyle(SelAttributes.Style+[fsUnderline],clBlue);

  ResetBack(tmp);

  {
  OldStart:=SelStart;
  OldLength:=SelLength;

  SelectText(ALine,AColumn,ALength);

  OldUnderline.Line:=ALine;
  OldUnderline.Column:=AColumn;
  OldUnderline.Length:=ALength;

  OldUnderline.Style:=SelAttributes.Style;
  OldUnderline.Color:=SelAttributes.Color;

  ChangeStyle(SelAttributes.Style+[fsUnderline],clBlue);

  SelStart:=OldStart;
  SelLength:=OldLength;
  }

  Cursor:=crHandPoint;
end;

procedure TCodeRichEdit.EnablePaint(const Enable:Boolean);
begin
  {$IFDEF MSWINDOWS}
//  SendMessage(this.Handle, EM_GETSCROLLPOS, 0, ref _ScrollPoint);
//  SendMessage(this.Handle, EM_SETSCROLLPOS, 0, ref _ScrollPoint);

  if Enable then
  begin
    SendMessage(Handle, WM_SETREDRAW, 1, 0);
//    Invalidate;
  end
  else
     SendMessage(Handle, WM_SETREDRAW, 0, 0);

//  _EventMask = SendMessage(this.Handle, EM_GETEVENTMASK, 0, IntPtr.Zero);
//  SendMessage(this.Handle, EM_SETEVENTMASK, 0, _EventMask);
  {$ELSE}
  {$ENDIF}
end;

function TCodeRichEdit.GetEventMask: Integer;
begin
  {$IFDEF MSWINDOWS}
  result:=SendMessage(Handle, EM_GETEVENTMASK, 0, 0);
  {$ELSE}
  result:=0; // TODO
  {$ENDIF}
end;

function TCodeRichEdit.CurrentStatus:TCodeRichStatus;
begin
  result.Start:=SelStart;
  result.Length:=SelLength;
  result.Top:=TopLine;
  result.Modified:=Modified;
  result.OnChange:=OnChange;

  EnablePaint(False);
  //CurrentLine:=LineOf(OldStart);
end;

{
procedure TCodeRichEdit.CNCommand(var Message: TWMCommand);
begin
  inherited;

  if (Message.NotifyCode = EN_VSCROLL) and Assigned(OnInvalidate) then
     OnInvalidate(Self);
end;
}

function TCodeRichEdit.PositionOf(const AIndex:Integer): TPoint;
begin
  result:=TPoint.Zero;

  {$IFDEF MSWINDOWS}
  SendMessage(Handle,EM_POSFROMCHAR, WPARAM(@result), AIndex);
//  Perform(EM_POSFROMCHAR, WPARAM(@result), AIndex);
  {$ELSE}
  // TODO
  {$ENDIF}
end;

procedure TCodeRichEdit.ResetBack(const AStatus: TCodeRichStatus);
begin
  Modified:=AStatus.Modified;
  OnChange:=AStatus.OnChange;
  SelStart:=AStatus.Start;
  SelLength:=AStatus.Length;
  ScrollLines(AStatus.Top-TopLine);

  EnablePaint(True);
end;

function TCodeRichEdit.YPos(Line: Integer): Integer;
begin
  result:=PosOfLine(Line);
  result:=PositionOf(result).Y
end;

{
procedure TCodeRichEdit.CreateWnd;
begin
  inherited;

  var EventMask := SendMessage(Handle, EM_GETEVENTMASK, 0, 0) or EN_VSCROLL;
  SendMessage(Handle, EM_SETEVENTMASK, 0, EventMask);
end;
}

// TODO: Remove this method, and use WMVScroll instead
procedure TCodeRichEdit.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  inherited;

  if not ErasingBack then
  begin
    ErasingBack:=True;
    try
      if Assigned(OnInvalidate) then
         OnInvalidate(Self);
    finally
      ErasingBack:=False;
    end;
  end;
end;

{$IFDEF FPC}
procedure TCodeRichEdit.WndProc(var Message: TLMessage);
begin
  inherited;

  if Message.msg=WM_VSCROLL then
     if Assigned(OnInvalidate) then
        OnInvalidate(Self);
end;
{$ENDIF}

procedure TCodeRichEdit.WMVScroll(var Message: TWMVScroll);
begin
  inherited;

  if Assigned(OnInvalidate) then
     OnInvalidate(Self);
end;

const
  Identifier = ['a'..'z','A'..'Z','_'];
  Digits = ['0'..'9'];
  IdentChars = Identifier + Digits;

function TCodeRichEdit.CurrentPos:Integer;
begin
  result:=SelStart {$IFNDEF FPC}+CaretLine-1{$ENDIF};
end;

function TCodeRichEdit.CharAt(const AOffset:Integer):Char;
var tmp : Integer;
begin
  tmp:=CurrentPos+AOffset+1;

  if (tmp<1) or (tmp>Length(Text)) then
     result:=#0
  else
     result:=Text[tmp];
end;

function TCodeRichEdit.CurrentCharIsIndentifier:Boolean;
begin
  result:=CharInSet(CharAt(0),IdentChars);
end;

function TCodeRichEdit.WordUnderCursor: String;
var tmpLine : Integer;
    Start,Finish : Integer;
begin
  tmpLine:=CaretLine-1;

  if tmpLine<Lines.Count then
  begin
    result:=Lines[tmpLine];

    Start:=Column;
    Finish:=Start;

    while (Start>1) and CharInSet(result[Start-1],IdentChars) do
      Dec(Start);

    while (Finish<=Length(result)) and CharInSet(result[Finish],IdentChars) do
      Inc(Finish);

    result:=Copy(result,Start,Finish-Start);
  end
  else
    result:='';
end;

function TCodeRichEdit.LineOf(const APos:Integer):Integer;
begin
  {$IFDEF MSWINDOWS}
  result:=SendMessage(Handle,EM_LINEFROMCHAR,APos,0);
  {$ELSE}
  result:=0;
  {$ENDIF}
end;

function TCodeRichEdit.LineOfPosition(const APos:Integer):Integer;
begin
  result:=1+LineOf(APos);
end;

function TCodeRichEdit.CaretLine:Integer;
begin
  result:=LineOfPosition(SelStart);
end;

procedure TCodeRichEdit.ScrollLines(const ALines:Integer);
begin
  {$IFDEF MSWINDOWS}
  SendMessage(Handle,EM_LINESCROLL,0,ALines);
  {$ELSE}
  // TODO
  {$ENDIF}
end;

function TCodeRichEdit.ScrollPosition: TPoint;
begin
  result:=TPoint.Create(0,0);

  {$IFNDEF FPC}
  {$IFDEF MSWINDOWS}
  {$IF CompilerVersion>34} // 35 = RAD 11.0
  if HandleAllocated then
     Perform(EM_GETSCROLLPOS,0,@result);
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
end;

function TCodeRichEdit.TopLine: Integer;
begin
  {$IFDEF MSWINDOWS}
  result:=SendMessage(Handle,EM_GETFIRSTVISIBLELINE,0,0);
  {$ELSE}
  result:=0; // TODO
  {$ENDIF}
end;

function PointOf(const X,Y:Integer):TPoint;
begin
  result.X:=X;
  result.Y:=Y;
end;

function TCodeRichEdit.PosOfLine(const ALine:Integer):Integer;
begin
  {$IFDEF MSWINDOWS}
  result:=SendMessage(Handle,EM_LINEINDEX,ALine,0);
  {$ELSE}
  result:=0;
  {$ENDIF}
end;

function TCodeRichEdit.LineColumnAt(const X,Y:Integer; out LineColumn:TPoint):Boolean;
var tmp : Integer;
    P : TPoint;
begin
  tmp:=CharFromPos(TPoint.Create(X,Y));

  P:=PositionOf(tmp);

  result:=Distance(P,PointOf(X,Y))<(Font.Size*2);

  if result then
  begin
    LineColumn.Y:=1+LineOf(tmp);
    LineColumn.X:=PosOfLine(LineColumn.Y-1);
    LineColumn.X:=1+ tmp - LineColumn.X;
  end;
end;

function TCodeRichEdit.Column: Integer;
var tmp : Integer;
begin
  tmp:=SelStart;

  result:=LineOf(tmp);
  result:=1 + tmp - PosOfLine(result);
end;

constructor TCodeRichEdit.Create(AOwner: TComponent);
begin
  inherited;

  {$IFNDEF FPC}
  FillChar(PrivateFormat, SizeOf(TCharFormat2), 0);
  PrivateFormat.cbSize := SizeOf(TCharFormat2);

  PrivateFormat.dwMask := CFM_BOLD or CFM_ITALIC or CFM_COLOR; // or CFM_UNDERLINE or CFM_STRIKEOUT;
  {$ENDIF}
end;

end.
