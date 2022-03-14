unit Editor_Pairs;

interface

uses
  Classes,
  {$IFDEF FPC}
  Graphics,
  {$ELSE}
  UITypes,
  {$ENDIF}
  Editor_VCL;

type
  TPairItem=record
  public
    Position : Integer;
    Color : TColor;
  end;

  TPair=record
  public
    Enabled : Boolean;
    First, Last : TPairItem;

    procedure TryHighLight(const AEdit:TCodeRichEdit);
  end;

  TEditStatus=record
    Start,
    Length : Integer;
    Modified : Boolean;
    OnChange : TNotifyEvent;

    procedure Save(const AEdit:TCodeRichEdit);
    procedure Restore(const AEdit:TCodeRichEdit);
  end;

implementation

{$IFDEF FPC}
uses RichMemo, RichMemoHelpers;
{$ENDIF}

function IsPair(const C:Char; out First:Boolean; out Opposite:Char):Boolean;
begin
  if C='(' then
  begin
    First:=True;
    Opposite:=')';

    result:=True;
  end
  else
  if C=')' then
  begin
    First:=False;
    Opposite:='(';

    result:=True;
  end
  else
  if C='[' then
  begin
    First:=True;
    Opposite:=']';

    result:=True;
  end
  else
  if C=']' then
  begin
    First:=False;
    Opposite:='[';

    result:=True;
  end
  else
  if C='{' then
  begin
    First:=True;
    Opposite:='}';

    result:=True;
  end
  else
  if C='}' then
  begin
    First:=False;
    Opposite:='{';

    result:=True;
  end
  {
  else
  if C='"' then
  begin
    First:=True;
    Opposite:='"';

    result:=True;
  end
  else
  if C=')' then
  begin
    First:=False;
    Opposite:='(';

    result:=True;
  end;
  }
  else
    result:=False;
end;

procedure TEditStatus.Save(const AEdit:TCodeRichEdit);
begin
  Start:=AEdit.SelStart;
  Length:=AEdit.SelLength;
  Modified:=AEdit.Modified;

  OnChange:=AEdit.OnChange;
  AEdit.OnChange:=nil;
end;

procedure TEditStatus.Restore(const AEdit:TCodeRichEdit);
begin
  AEdit.SelStart:=Start;
  AEdit.SelLength:=Length;
  AEdit.Modified:=Modified;
  AEdit.OnChange:=OnChange;
end;

// TODO: Use TPositions instead of Edit.Text
// TPositions is much better/faster and has comments already skipped
procedure TPair.TryHighLight(const AEdit:TCodeRichEdit);
var C, Opposite : Char;
    IsFirst : Boolean;

  function Search(P:Integer; const AText:String):Integer;
  var tmp,
      Nested : Integer;
  begin
    tmp:=0;
    Nested:=0;

    if IsFirst then
    begin
      P:=P+1;

      while P<=Length(AText) do
      begin
        if AText[P]=Opposite then
        begin
          if Nested=0 then
             Exit(P-tmp)
          else
             Dec(Nested);
        end
        else
        begin
          if AText[P]=Chr(10) then
             Inc(tmp)
          else
          if AText[P]=C then
             Inc(Nested);

        end;

        Inc(P);
      end;
    end
    else
    begin
      P:=P-1;

      while P>0 do
      begin
        if AText[P]=Opposite then
        begin
          if Nested=0 then
             Exit(P+tmp)
          else
             Dec(Nested);
        end
        else
        begin
          if AText[P]=Chr(10) then
             Inc(tmp)
          else
          if AText[P]=C then
             Inc(Nested);

        end;

        Dec(P);
      end;
    end;

    result:=-1;
  end;

  procedure Highlight(const APosition:Integer; const AColor:TColor);
  begin
    AEdit.SelLength:=0;
    AEdit.SelStart:=APosition;
    AEdit.SelLength:=1;
    AEdit.SelAttributes.Color:=AColor;
  end;

  procedure TryResetBackup;
  var Old : TEditStatus;
  begin
    if Enabled then
    begin
      Old.Save(AEdit);

      Highlight(First.Position,First.Color);
      Highlight(Last.Position,Last.Color);

      Enabled:=False;

      Old.Restore(AEdit);
    end;
  end;

  function SamePair(const A,B:Integer):Boolean;
  begin
    result:=Enabled and (First.Position=A) and (Last.Position=B);
  end;

const
  PairColor={$IFDEF FPC}clRed{$ELSE}TColors.Red{$ENDIF};

var Old : TEditStatus;
    tmp,
    tmpLine,
    tmpOther : Integer;
begin
  tmpLine:=AEdit.CaretLine;
  tmp:=AEdit.SelStart+tmpLine;

  if (tmp>0) and (tmp<=Length(AEdit.Text)) then
  begin
    C:=AEdit.Text[tmp];

    if IsPair(C,IsFirst,Opposite) then
    begin
      tmpOther:=Search(tmp,AEdit.Text);

      if tmpOther<>-1 then
      begin
        Dec(tmpOther,tmpLine);

        if not SamePair(AEdit.SelStart,tmpOther) then
        begin
          TryResetBackup;

          Old.Save(AEdit);

          Enabled:=True;

          First.Position:=Old.Start;
          AEdit.SelStart:=Old.Start+1;
          First.Color:=AEdit.SelAttributes.Color;

          Last.Position:=tmpOther;
          AEdit.SelStart:=tmpOther+1;
          Last.Color:=AEdit.SelAttributes.Color;

          Highlight(Old.Start,PairColor);
          Highlight(tmpOther,PairColor);

          Old.Restore(AEdit);
        end;

        Exit;
      end;
    end;
  end;

  TryResetBackup;
end;

end.
