unit Error_Gutter;

interface

uses
  //Messages,
  Classes, Controls, Graphics, Gutter;

type
  TErrorGutter=class(TBaseGutter)
  private
  protected
    procedure Paint; override;
  public
    Lines : Array of Integer;
    TotalLines : Integer;

    Constructor Create(AOwner:TComponent); override;
  end;

implementation

{ TErrorGutter }

constructor TErrorGutter.Create(AOwner: TComponent);
begin
  inherited;
  Width:=10;
end;

procedure TErrorGutter.Paint;
var X1,X2,Y, t : Integer;
begin
  inherited;

  if csDestroying in ComponentState then
     Exit;

  if TotalLines>0 then
  begin
    X1:=2;
    X2:=Width-2;

    Canvas.Pen.Style:=psSolid;
    Canvas.Pen.Color:=clRed;

    for t:=0 to High(Lines) do
    begin
      Y:=Round(Lines[t]*Height/TotalLines);
      Canvas.MoveTo(X1,Y);
      Canvas.LineTo(X2,Y);
    end;
  end;
end;

end.
