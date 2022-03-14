unit Gutter;

interface

uses
  Messages, Classes, Controls, Graphics,
  {$IFNDEF FPC}
  Types,
  {$ENDIF}
  Themes, Exceptions, Editor_VCL;

type
  TBaseGutter=class(TCustomControl)
  protected
    function GetBackgroundColor:TColor;

    {$IFNDEF FPC}
    function GetThemeColor(const APanel:TThemedPanel;
                           const AElement:TElementColor;
                           var AColor:TColor):Boolean;
    {$ENDIF}

    procedure Paint; override;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
    Constructor Create(AOwner: TComponent); override;
  end;

  TGutterSymbol=record
  public
    Line : Integer;
    Style : TErrorStyle;
    Text : String;
  end;

  TBreakStatus=(Enabled,Disabled,Wrong,Stopped);

  TGutterBreak=record
  public
    Status : TBreakStatus;
    Line : Integer;
  end;

  TStopsArray=Array of Integer;

  TGutterBreakEvent=procedure(Sender:TObject; const AIndex:Integer) of object;

  TEditorGutter=class(TBaseGutter)
  private
    TopLine,
    BottomLine : Integer;

    LineHeight : Integer;

    function BoundsOfLine(const ALine:Integer):TRect;
    function BoundsOfSymbol(const ALine:Integer):TRect;

    procedure EnableDisableBreakAt(const ALine:Integer);

    function GetLineNumColor:TColor;

    procedure PaintBreaks;
    procedure PaintCurrent;
    procedure PaintLineNumbers;
    procedure PaintStops;
    procedure PaintSymbols;
    procedure PrepareLineCanvas;
    procedure SetBrushColor(const AStatus:TBreakStatus);
    function YPos(const ALine:Integer):Integer;
  protected
    function LineHit(X, Y: Integer):Integer;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    function SymbolAt(X, Y: Integer):Integer;
  public
    Breaks : Array of TGutterBreak;
    Current : Integer;
    Stops : TStopsArray;
    Symbols : Array of TGutterSymbol;

    Edit : TCodeRichEdit;

    OnBreakAdded,
    OnBreakToggled,
    OnBreakRemoved : TGutterBreakEvent;

    Constructor Create(AOwner:TComponent); override;

    procedure AddSymbol(const ALine:Integer; const AText:String; const AStyle:TErrorStyle);
    function AutoWidth(const ACount:Integer):Boolean;
    function FindBreakAt(const ALine:Integer):Integer;
    procedure SetCurrent(const ALine:Integer);
    procedure ToggleBreak(const ALine:Integer);
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, Math;

{$IFDEF FPC}
{$IFNDEF MSWINDOWS}
function RGB(r,g,b : longint) : DWORD;
begin
   RGB:=DWORD(((DWORD(BYTE(r))) or ((DWORD(WORD(g))) shl 8)) or ((DWORD(BYTE(b))) shl 16));
end;
{$ENDIF}
{$ENDIF}

function ApplyBright(const Color:TColor; const HowMuch:Byte):TColor;

  function Add(const B:Byte):Byte;
  begin
    if (B+HowMuch)<256 then result:=B+HowMuch else result:=255;
  end;

var r : Byte;
    g : Byte;
    b : Byte;
Begin
  r:=Add(Byte(Color));
  g:=Add(Byte(Color shr 8));
  b:=Add(Byte(Color shr 16));

  result:= r or (g shl 8) or (b shl 16);
end;

{ TBaseGutter }

Constructor TBaseGutter.Create(AOwner: TComponent);
begin
  inherited;

  DoubleBuffered:=True;
  Color:=RGB(250,250,250);
end;

procedure TBaseGutter.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

{$IFNDEF FPC}
function TBaseGutter.GetThemeColor(const APanel:TThemedPanel;
                                     const AElement:TElementColor;
                                     var AColor:TColor):Boolean;
var LStyle   : TCustomStyleServices;
    LDetails : TThemedElementDetails;
begin
  LStyle := StyleServices{$IFDEF VER340}(Self){$ENDIF};

  if LStyle.Enabled then
  begin
    LDetails := LStyle.GetElementDetails(APanel);

    result:=LStyle.GetElementColor(LDetails, AElement, AColor) and (AColor<>clNone);
  end
  else
    result:=False;
end;
{$ENDIF}

function TBaseGutter.GetBackgroundColor:TColor;
begin
  result:=Color;

  {$IFNDEF FPC}
  if seClient in StyleElements then
     if not GetThemeColor(tpPanelBackground,ecFillColor,result) then
        result:=Color;
  {$ENDIF}
end;

procedure TBaseGutter.Paint;
begin
  inherited;

  if csDestroying in ComponentState then
     Exit;

  Canvas.Brush.Color:=GetBackgroundColor;
  Canvas.FillRect(ClientRect);
end;

{ TEditorGutter }

Constructor TEditorGutter.Create(AOwner: TComponent);
begin
  inherited;
  Current:=-1;
  Width:=40;
end;

function TEditorGutter.SymbolAt(X, Y: Integer):Integer;
var P : TPoint;
    t : Integer;
begin
  P:=TPoint.Create(X,Y);

  for t:=0 to High(Symbols) do
      if BoundsOfSymbol(Symbols[t].Line).Contains(P) then
         Exit(t);

  result:=-1;
end;

procedure TEditorGutter.MouseMove(Shift: TShiftState; X, Y: Integer);
var tmp : Integer;
begin
  inherited;

  Cursor:=crDefault;
  Hint:='';

  tmp:=SymbolAt(X,Y);

  if tmp<>-1 then
  begin
    Cursor:=crHandPoint;

    Hint:=Symbols[tmp].Text;

    ShowHint:=False;
    ShowHint:=True;
  end;
end;

function TEditorGutter.LineHit(X, Y: Integer):Integer;
var P : TPoint;
    t : Integer;
begin
  P:=TPoint.Create(X,Y);

  for t:=Edit.TopLine to BottomLine do
      if BoundsOfLine(t).Contains(P) then
         Exit(t);

  result:=-1;
end;

function TEditorGutter.FindBreakAt(const ALine:Integer):Integer;
var t : Integer;
begin
  for t:=0 to High(Breaks) do
      if Breaks[t].Line=ALine then
         Exit(t);

  result:=-1;
end;

procedure TEditorGutter.EnableDisableBreakAt(const ALine:Integer);
var t : Integer;
begin
  t:=FindBreakAt(ALine);

  if t<>-1 then
     OnBreakToggled(Self,t);
end;

procedure TEditorGutter.ToggleBreak(const ALine:Integer);
var t,L : Integer;
    {$IFDEF FPC}
    tt : Integer;
    {$ENDIF}
begin
  L:=Length(Breaks);

  t:=FindBreakAt(ALine);

  if t=-1 then
  begin
    SetLength(Breaks,L+1);
    Breaks[L].Line:=ALine;
    Breaks[L].Status:=TBreakStatus.Enabled;

    OnBreakAdded(Self,L);
  end
  else
  begin
    OnBreakRemoved(Self,t);

    {$IFDEF FPC}
    for tt:=t to L-1 do Breaks[tt]:=Breaks[tt+1];

    SetLength(Breaks,L-1);
    {$ELSE}
    Delete(Breaks,t,1);
    {$ENDIF}
  end;
end;

procedure TEditorGutter.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var tmp : Integer;
begin
  inherited;

  tmp:=SymbolAt(X,Y);

  if tmp=-1 then
  begin
    tmp:=LineHit(X,Y);

    if tmp<>-1 then
    begin
      if Button=TMouseButton.mbLeft then
         ToggleBreak(tmp)
      else
         EnableDisableBreakAt(tmp);

      Invalidate;
    end;
  end;
end;

procedure TEditorGutter.AddSymbol(const ALine:Integer; const AText:String; const AStyle:TErrorStyle);
var L : Integer;
begin
  L:=Length(Symbols);

  SetLength(Symbols,L+1);

  Symbols[L].Line:=ALine;
  Symbols[L].Text:=AText;
  Symbols[L].Style:=AStyle;
end;

function TEditorGutter.YPos(const ALine:Integer):Integer;
begin
  result:=Edit.YPos(ALine);
end;

function TEditorGutter.GetLineNumColor:TColor;
const Grey=180;
begin
  {$IFNDEF FPC}
  if seFont in StyleElements then
  begin
    if not GetThemeColor(tpPanelBackground,ecTextColor,result) then
       result:=ApplyBright(ColorToRGB(Font.Color),128);
  end
  else
  {$ENDIF}
    result:=RGB(Grey,Grey,Grey);
end;

procedure TEditorGutter.Paint;
var Y : Integer;
begin
  inherited;

  if csDestroying in ComponentState then
     Exit;

  TopLine:=Edit.TopLine;

  Y:=YPos(TopLine);
  LineHeight:=YPos(TopLine+1)-Y;

  PaintLineNumbers;
  PaintStops;
  PaintBreaks;
  PaintSymbols;
  PaintCurrent;
end;

procedure TEditorGutter.PrepareLineCanvas;
begin
  Canvas.Font:=Edit.Font;
//  Canvas.Font.Size:=Canvas.Font.Size-1;

  {$IFDEF MSWINDOWS}
  SetBkMode(Canvas.Handle,TRANSPARENT);
  SetTextAlign(Canvas.Handle,TA_Right+TA_Bottom);
  {$ENDIF}
end;

procedure TEditorGutter.PaintLineNumbers;
var X,Y,Line : Integer;

  procedure OutLine;
  begin
    Canvas.TextOut(X,Y+2,IntToStr(Line));
  end;

var NewY : Integer;
begin
  if LineHeight>0 then
  begin
    PrepareLineCanvas;

    BottomLine:=-1;

    Canvas.Font.Color:=GetLineNumColor;

    X:=Width-6;
    Y:=YPos(TopLine+1);

    Line:=TopLine+1;

    while Y<Height+LineHeight do
    begin
      OutLine;
      Inc(Line);

      NewY:=YPos(Line);

      if NewY<0 then
         break
      else
      if Y<NewY then
         Y:=NewY
      else
      begin
        Inc(Y,LineHeight);
        OutLine;
        break;
      end;
    end;

    BottomLine:=Line;
  end;
end;

function TEditorGutter.AutoWidth(const ACount: Integer):Boolean;
var tmp : Double;
    tmpChar,
    tmpW : Integer;
begin
  tmp:=2+Log10(1+ACount);

  if tmp<2 then
     tmp:=2;

  PrepareLineCanvas;

  tmpChar:=Canvas.TextWidth('W');

  tmpW:=Round(tmp*(tmpChar+4));

  result:=Width<>tmpW; // True, when width has changed

  if result then
     Width:=tmpW;
end;

function TEditorGutter.BoundsOfLine(const ALine:Integer):TRect;
begin
  result.Bottom:=YPos(ALine)+1;
  result.Top:=result.Bottom-LineHeight+2;

  result.Left:=2;
  result.Right:=Width-2;
end;

function TEditorGutter.BoundsOfSymbol(const ALine:Integer):TRect;
const SymbolSize=5;
var tmp : Integer;
begin
  result.Top:=YPos(ALine-1);
  result.Bottom:=result.Top+LineHeight;

  tmp:=3+((result.Top+result.Bottom) div 2);
  result.Top:=tmp-SymbolSize;
  result.Bottom:=tmp+SymbolSize;

  result.Left:=7-SymbolSize;
  result.Right:=7+SymbolSize;
end;

procedure TEditorGutter.SetBrushColor(const AStatus:TBreakStatus);
begin
  case AStatus of
    TBreakStatus.Enabled: Canvas.Brush.Color:=RGB(220,0,0);
    TBreakStatus.Disabled: Canvas.Brush.Color:=RGB(220,100,100);
    TBreakStatus.Wrong: Canvas.Brush.Color:=RGB(120,100,100);
    TBreakStatus.Stopped: Canvas.Brush.Color:=RGB(20,220,20);
  end;
end;

procedure TEditorGutter.SetCurrent(const ALine: Integer);
begin
  Current:=ALine;
  Invalidate;
end;

procedure TEditorGutter.PaintStops;
var t : Integer;
    R : TRect;
begin
  if Stops<>nil then
  begin
    Canvas.Pen.Style:=TPenStyle.psClear;
    Canvas.Brush.Style:=TBrushStyle.bsSolid;

    Canvas.Brush.Color:=RGB(0,50,230);

    for t:=0 to High(Stops) do
        if (Stops[t]>=TopLine+1) and (Stops[t]<=BottomLine+1) then
        begin
          R.Bottom:=YPos(Stops[t])+1-3;
          R.Top:=R.Bottom-LineHeight+10;
          R.Left:=2;
          R.Right:=2+5;

          Canvas.Rectangle(R);
        end;
  end;
end;

procedure TEditorGutter.PaintBreaks;
var t : Integer;
    R : TRect;
begin
  if Breaks<>nil then
  begin
    Canvas.Pen.Style:=TPenStyle.psClear;
    Canvas.Brush.Style:=TBrushStyle.bsSolid;

    PrepareLineCanvas;
    Canvas.Font.Color:=clWhite;

    for t:=0 to High(Breaks) do
        if (Breaks[t].Line>=TopLine+1) and (Breaks[t].Line<=BottomLine+1) then
        begin
          SetBrushColor(Breaks[t].Status);

          R:=BoundsOfLine(Breaks[t].Line);

          if Current=Breaks[t].Line then
             R.Left:=R.Left+(R.Bottom-R.Top);

          Canvas.Rectangle(R);

          {$IFDEF MSWINDOWS}
          SetBkMode(Canvas.Handle,TRANSPARENT);
          {$ENDIF}

          Canvas.TextOut(Width-6,R.Bottom,IntToStr(Breaks[t].Line));
        end;
  end;
end;

procedure TEditorGutter.PaintCurrent;
begin
  if Current<>-1 then
  if (Current>=TopLine+1) and (Current<=BottomLine+1) then
  begin
    Canvas.Pen.Style:=TPenStyle.psClear;
    Canvas.Brush.Style:=TBrushStyle.bsSolid;

    Canvas.Brush.Color:=clGreen;
    Canvas.Rectangle(BoundsOfSymbol(Current));
  end;
end;

procedure TEditorGutter.PaintSymbols;
var S : TGutterSymbol;
begin
  if Symbols<>nil then
  begin
    Canvas.Pen.Style:=TPenStyle.psClear;
    Canvas.Brush.Style:=TBrushStyle.bsSolid;

    for S in Symbols do
        if (S.Line>=TopLine+1) and (S.Line<=BottomLine+1) then
        begin
          if S.Style=TErrorStyle.Error then
             Canvas.Brush.Color:=clRed
          else
             Canvas.Brush.Color:=clMaroon; // Warnings

          Canvas.Rectangle(BoundsOfSymbol(S.Line));
        end;
  end;
end;

end.
