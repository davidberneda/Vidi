unit Code_Jumps;

interface

type
  TCodeJump=record
    Module : String;
    Line, Column, TopLine : Integer;
  end;

  TCodeJumps=record
    Current : Integer;
    Items : Array of TCodeJump;

    procedure Add(const AModule:String; const ALine,AColumn,ATopLine:Integer);

    function Count:Integer;
    function Empty:TCodeJump;

    function Next:TCodeJump;
    function Previous:TCodeJump;
  end;

var
  CodeJumps : TCodeJumps;

implementation

{ TCodeJumps }

procedure TCodeJumps.Add(const AModule: String; const ALine, AColumn,
  ATopLine: Integer);
begin
  Current:=Count;
  SetLength(Items,Current+1);

  Items[Current].Module:=AModule;
  Items[Current].Line:=ALine;
  Items[Current].Column:=AColumn;
  Items[Current].TopLine:=ATopLine;
end;

function TCodeJumps.Count: Integer;
begin
  result:=Length(Items);
end;

function TCodeJumps.Empty: TCodeJump;
begin
  result.Module:='';
  result.Line:=0;
  result.Column:=0;
  result.TopLine:=0;
end;

function TCodeJumps.Next: TCodeJump;
begin
  if Current<High(Items) then
  begin
    Inc(Current);
    result:=Items[Current];
  end
  else
    result:=Empty;
end;

function TCodeJumps.Previous: TCodeJump;
begin
  if Current>0 then
  begin
    Dec(Current);
    result:=Items[Current];
  end
  else
    result:=Empty;
end;

end.
