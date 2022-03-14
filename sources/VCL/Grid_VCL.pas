unit Grid_VCL;

interface

uses
  Classes, Controls, Grids, Types;

type
  TVidiGridDrawCellEvent=function(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState):Boolean of object;

  TVidiGrid=class(TStringGrid)
  private
  protected
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    procedure SizeChanged(OldColCount, OldRowCount: Longint); override;
  public
    RowObjects : Array of TObject;

    OnDrawCell: TVidiGridDrawCellEvent;

    Constructor Create(AOwner:TComponent); override;

    class function Add(const AOwner:TComponent; const AParent:TWinControl):TVidiGrid; static;

    function AppendRow(const AValues:Array of String; const AObject:TObject):Integer;
    procedure DeleteRow(ARow:Integer); override;

    function AsText:String;

    {$IFNDEF VER340}
    procedure BeginUpdate;
    procedure EndUpdate;
    {$ENDIF}

    procedure Clear;
    procedure EnableRowSelect;
    procedure Header(const ANames:Array of String);
    function IndexOfObject(const AObject:TObject):Integer;
  end;

implementation

uses
  Graphics;

{ TVidiGrid }

class function TVidiGrid.Add(const AOwner: TComponent;
  const AParent: TWinControl): TVidiGrid;
begin
  result:=TVidiGrid.Create(AOwner);
  result.Align:=TAlign.alClient;
  result.Parent:=AParent;

  {$IFNDEF FPC}
  result.BevelOuter:=bvNone;
  {$ENDIF}
end;

function TVidiGrid.AppendRow(const AValues: array of String;
  const AObject: TObject): Integer;
var t : Integer;
begin
  result:=RowCount;
  RowCount:=RowCount+1;

  if FixedRows<1 then
     FixedRows:=1;

  for t:=0 to High(AValues) do
      Cells[t,result]:=AValues[t];

  RowObjects[result]:=AObject;
end;

function TVidiGrid.AsText: String;

  function RowAsText(const ARow:Integer):String;
  var t : Integer;
  begin
    result:='';

    for t:=0 to ColCount-1 do
        if t=0 then
           result:=Cells[t,ARow]
        else
           result:=result+#9+Cells[t,ARow];
  end;

var t : Integer;
begin
  result:='';

  for t:=1 to RowCount-1 do
      result:=result+RowAsText(t)+#13#10;
end;

procedure TVidiGrid.Clear;
begin
  RowCount:=1;
end;

{$IFNDEF VER340}
procedure TVidiGrid.BeginUpdate;
begin
end;

procedure TVidiGrid.EndUpdate;
begin
end;
{$ENDIF}

constructor TVidiGrid.Create(AOwner: TComponent);
begin
  inherited;

  {$IFDEF FPC}
  GridLineColor:=RGBToColor(220,220,220);
  {$ELSE}
  DefaultRowHeight:=3+(Font.Size*2);
  {$ENDIF}

  FixedCols:=0;

  FixedRows:=1;
  RowCount:=1;

  Options:=Options+[goColsizing,goThumbTracking,
                    {$IFDEF FPC}
                    goHeaderHotTracking
                    {$ELSE}
                    goFixedHotTrack
                    {$ENDIF}
                    ]-[goEditing];

end;

procedure TVidiGrid.DeleteRow(ARow: Integer);
{$IFDEF FPC}
var t : Integer;
{$ENDIF}
begin
  {$IFDEF FPC}
  for t:=ARow to RowCount-2 do
      RowObjects[t]:=RowObjects[t+1];
  {$ELSE}
  Delete(RowObjects,ARow,1);
  {$ENDIF}

  inherited;
end;

procedure TVidiGrid.DrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
begin
  if (not Assigned(OnDrawCell)) or OnDrawCell(ACol,ARow,ARect,AState) then
     inherited;
end;

procedure TVidiGrid.Header(const ANames: array of String);
var t, L : Integer;
begin
  L:=Length(ANames);

  if RowCount<1 then
     RowCount:=1;

  ColCount:=L;

  for t:=0 to L-1 do
      Cells[t,0]:=ANames[t];
end;

function TVidiGrid.IndexOfObject(const AObject: TObject): Integer;
var t : Integer;
begin
  for t:=Low(RowObjects) to High(RowObjects) do
      if RowObjects[t]=AObject then
         Exit(t);

  result:=-1;
end;

procedure TVidiGrid.EnableRowSelect;
begin
  Options:=Options+[TGridOption.goRowSelect];
end;

procedure TVidiGrid.SizeChanged(OldColCount, OldRowCount: Longint);
begin
  inherited;
  SetLength(RowObjects,RowCount);
end;

end.
