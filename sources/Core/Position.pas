unit Position;

interface

type
  TPosition=record
  public
    Line,
    Column,
    Position : Integer;

    procedure Increment(const Amount:Integer);
    function LineColumn:String;
  end;

  TPositions=Array of TPosition;

implementation

uses
  SysUtils; // <-- ToString  (TO REMOVE)

{ TPosition }

procedure TPosition.Increment(const Amount: Integer);
begin
  Inc(Position,Amount);
  Inc(Column,Amount);
end;

function TPosition.LineColumn: String;
begin
  result:='['+Line.ToString+','+Column.ToString+']';
end;

end.

