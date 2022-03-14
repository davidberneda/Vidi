unit StringArray;

interface

type
  TStringArray=Array of String;

  TStringArrayHelper=record helper for TStringArray
  private
  type
    TCompareProc=function(const S1, S2: string): Integer;
  public
    function Add(const S:String):Integer; overload;
    procedure Add(const S:TStringArray); overload;
    function SortedFind(const Value: String; out Exists:Boolean; const IgnoreCase:Boolean): Integer;
    function IndexOf(const S:String):Integer;
  end;

implementation

uses
  SysUtils;

{ TStringArrayHelper }

function TStringArrayHelper.Add(const S:String):Integer;
begin
  result:=Length(Self);
  SetLength(Self,result+1);
  Self[result]:=S;
end;

procedure TStringArrayHelper.Add(const S: TStringArray);
var t,C,L : Integer;
begin
  L:=Length(S);

  if L>0 then
  begin
    C:=Length(Self);
    SetLength(Self,C+L);

    for t:=0 to L-1 do
        Self[C+t]:=S[t];
  end;
end;

function TStringArrayHelper.IndexOf(const S:String):Integer;
var t : Integer;
begin
  for t:=Low(Self) to High(Self) do
      if SameText(Self[t],S) then
         Exit(t);

  result:=-1;
end;

function TStringArrayHelper.SortedFind(const Value: String; out Exists:Boolean; const IgnoreCase:Boolean): Integer;
var
  L, H: Integer;
  C : Integer;
  Compare : TCompareProc;
begin
  L := 0;
  H := High(Self);

  if IgnoreCase then
     Compare:=CompareText // case-insensitive
  else
     Compare:=CompareStr; // case-sensitive

  while L <= H do
  begin
    result:=(L + H) shr 1;

    C:=Compare(Self[result],Value);

    if C < 0 then
       L := Succ(result)
    else
    if C > 0 then
       H := Pred(result)
    else
    begin
      Exists:=True;
      Exit;
    end;
  end;

  result:=L;
  Exists:=False;
end;

end.
