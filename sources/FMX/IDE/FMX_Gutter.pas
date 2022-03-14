unit FMX_Gutter;

interface

uses
  Classes, Types, FMX.Objects, FMX.Memo, FMX.Controls, FMX.Graphics;

type
  TGutter=class(TPaintBox)
  protected
    procedure Paint; override;
  public
    Memo : TMemo;
  end;

implementation

uses
  SysUtils, FMX.Types, UITypes, FMX.Memo.Types;

{ TGutter }

procedure TGutter.Paint;
var R,RR : TRectF;
    LineH,
    Y : Single;

    Line : Integer;

//    tmpCaret : TCaretPosition;
begin
  inherited;

  if not (csDesigning in ComponentState) and not FInPaintTo then
  begin
    R := LocalRect;
    InflateRect(R, -0.5, -0.5);

    Y:=R.Top;

    RR.Left:=R.Left;
    RR.Right:=R.Right-Canvas.TextWidth('W');

    Canvas.Font.Family:=Memo.Font.Family;
    Canvas.Font.Size:=Memo.Font.Size;

//    tmpCaret:=Memo.TextPosToPos(0);

    Line:=1;
    LineH:=Canvas.TextHeight('W');

    Canvas.Fill.Color:=TAlphaColorRec.Darkgray;

    while Y<R.Bottom-LineH do
    begin
      RR.Top:=Y;

      Y:=Y+LineH;

      RR.Bottom:=Y;

      Canvas.FillText(RR,IntToStr(Line),False,1,[],TTextAlign.Trailing);

      Inc(Line);
    end;
  end;
end;

end.
