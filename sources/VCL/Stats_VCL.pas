unit Stats_VCL;

interface

uses
  {$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  Windows, Messages,
  {$ENDIF}
  {$ENDIF}

  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Editor, ExtCtrls, Grid_VCL, Sys;

type
  TCodeStatsItem=(
    Classes,
    Methods,
    Variables,
    Withs,
    Arrays,
    Literals,
    Loops,
    Breaks,
    Continues,
    Calls,
    Ancestors,
    Assignments,
    Ifs,
    Operands,
    Ranges,
    Repeats,
    Selfs,
    Castings,
    Trys,
    Catches,
    Returns,
    Whiles,
    Whens,
    Extenders);

  TCodeStats=record
  public
    Count:Array[TCodeStatsItem] of Integer;

    procedure Calculate(const ACode:TNode; const Recursive:Boolean);
    procedure Reset;
    class function TextOf(const AItem:TCodeStatsItem):String; static;
    function Total:Integer;
  end;

  TFormStats = class(TForm)
    MemoStats: TMemo;
    PanelBottom: TPanel;
    Splitter1: TSplitter;
    Panel1: TPanel;
    CBAllModules: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CBAllModulesClick(Sender: TObject);
  private
    { Private declarations }

    Stats : TVidiGrid;

    LastCode : TNode;

    procedure PresentResults(const AStats: TCodeStats);
  public
    { Public declarations }

    Recursive : Boolean;

    procedure RefreshCode(const ACode:TCodeEditor);
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF INTERNAL}
  Internal,
  {$ENDIF}
  Exceptions,
  AST, IDE_Lang_English, Module;

procedure TCodeStats.Reset;
var t : TCodeStatsItem;
begin
  for t:=Low(TCodeStatsItem) to High(TCodeStatsItem) do
      Count[t]:=0;
end;

function TCodeStats.Total:Integer;
var t : TCodeStatsItem;
begin
  result:=0;

  for t:=Low(TCodeStatsItem) to High(TCodeStatsItem) do
      Inc(result,Count[t]);
end;

class function TCodeStats.TextOf(const AItem:TCodeStatsItem):String;
begin
  case AItem of
    Classes    : result:='Class';
    Methods    : result:='Method';
    Variables  : result:='Variable';
    Withs      : result:='With';
    Arrays     : result:='Array';
    Literals   : result:='Literal';
    Loops      : result:='Loop';
    Breaks     : result:='Break';
    Continues  : result:='Continue';
    Calls      : result:='Call';
    Ancestors  : result:='Ancestor';
    Assignments: result:='Assignment';
    Ifs        : result:='If';
    Operands   : result:='Operand';
    Ranges     : result:='Range';
    Repeats    : result:='Repeat';
    Selfs      : result:='Self';
    Castings   : result:='Casting';
    Trys       : result:='Try';
    Catches    : result:='Catch';
    Returns    : result:='Return';
    Whiles     : result:='While';
    Whens      : result:='When';
    Extenders  : result:='Extender';
  {$IFDEF FPC}
  else
    result:='?';
  {$ENDIF}
  end;
end;

procedure TCodeStats.Calculate(const ACode:TNode; const Recursive:Boolean);

  procedure Increment(const AItem:TCodeStatsItem);
  begin
    Inc(Count[AItem]);
  end;

  procedure Add(const ANodes:TNodes);

    procedure AddNode(const N:TNode);
    var W : TWhenItem;
        C : TCatch;
    begin
      if N is TRoutine then
      begin
        Increment(Methods);

        if Recursive then
           Add(TRoutine(N).Items)
      end
      else
      if N is TVariable then
         Increment(Variables)
      else
      if N is TClassType then
      begin
        Increment(Classes);

        if Recursive then
           Add(TClassType(N).Items)
      end
      else
      if N is TWith then
         Increment(Withs)
      else
      if (N is TInteger) or (N is TText) or (N is TBoolean) or (N is TFloat) then
         Increment(Literals)
      else
      if N is TFor then
      begin
        Increment(Loops);
        AddNode(TFor(N).Block);
      end
      else
      if N is TBreak then
         Increment(Breaks)
      else
      if N is TContinue then
         Increment(Continues)
      else
      if (N is TCallData) or (N is TDataCall) or (N is TTypeCall) then
         Increment(Calls)
      else
      if N is TAssignment then
         Increment(Assignments)
      else
      if N is TIf then
      begin
        Increment(Ifs);

        AddNode(TIf(N).ThenBlock);
        AddNode(TIf(N).ElseBlock);
      end
      else
      if N is TRepeat then
      begin
        Increment(Repeats);

        AddNode(TRepeat(N).Block);
      end
      else
      if N is TWhile then
      begin
        Increment(Whiles);
        AddNode(TWhile(N).Block);
      end
      else
      if N is TWhen then
      begin
        Increment(Whens);

        for W in TWhen(N).Items do
            AddNode(W.Block);

        AddNode(TWhen(N).ElseBlock);
      end
      else
      if N is TExtender then
      begin
        Increment(Extenders);
        AddNode(TExtender(N).TheType);
      end
      else
      if N is TTry then
      begin
        Increment(Trys);

        AddNode(TTry(N).Block);
        AddNode(TTry(N).TheFinally);

        for C in TTry(N).Catch do
            AddNode(C);
      end
      else
      if N is TCatch then
      begin
        Increment(Catches);
        AddNode(TCatch(N).Block);
      end
      else
      if N is TBlockStatement then
         Add(TBlockStatement(N).Block.Items)
      else
      if N is TReturn then
      begin
        Increment(Returns);

        if TReturn(N).Value<>nil then
           AddNode(TReturn(N).Value);
      end
      else
      if N is TOperand then
      begin
        Increment(Operands);
        AddNode(TOperand(N).Left);
        AddNode(TOperand(N).Right);
      end
      else
      if N is TGroup then
         AddNode(TGroup(N).Expression)
      else
      if N is TArrayExpression then
      begin
        Increment(Arrays);
        Add(TArrayExpression(N).Parameters);
      end
      else
      if N is TRange then
      begin
        Increment(Ranges);
        AddNode(TRange(N).Min);
        AddNode(TRange(N).Max);
      end
      else
      if N is TSelf then
         Increment(Selfs)
      else
      if N is TTypeMember then
      else
      if N is TMember then
      begin
        AddNode(TMember(N).Data);
        AddNode(TMember(N).Member);
      end
      else
      if N is TAncestor then
         Increment(Ancestors)
      else
      if N is TCastingData then
      begin
        Increment(Castings);
        AddNode(TCastingData(N).Data);
      end
      {$IFDEF INTERNAL}
      else
      if N<>nil then
         InternalError('Statistics of: ',N);
      {$ENDIF}
    end;

  var N : TNode;
  begin
    for N in ANodes do
       AddNode(N);
  end;

begin
  if ACode is TType then
     Add(TType(ACode).Items);
end;

function CountEmpty(const S:TStrings):Integer;
var t : Integer;
begin
  result:=0;

  for t:=0 to S.Count-1 do
      if S[t].Trim='' then
         Inc(result);
end;

{ TFormStats }

procedure TFormStats.CBAllModulesClick(Sender: TObject);
var tmp : TCodeStats;
    M : TNamedType;
begin
  tmp.Reset;

  if CBAllModules.Checked then
  begin
    for M in Modules.Items do
        tmp.Calculate(M,True);
  end
  else
  if LastCode<>nil then
     tmp.Calculate(LastCode,True);

  PresentResults(tmp);
end;

procedure TFormStats.FormCreate(Sender: TObject);
begin
  Recursive:=True;

  Stats:=TVidiGrid.Add(Self,PanelBottom);
  Stats.Header([Vidi_Lang._Type,Vidi_Lang.Count]);
end;

procedure TFormStats.RefreshCode(const ACode: TCodeEditor);
var L : TStrings;

  procedure Add;
  begin
    L.Add('');
  end;

var tmp, tmpTotal : Integer;
    tmpCode : TCodeStats;
begin
  MemoStats.Clear;

  if ACode=nil then
     Exit;

  L:=MemoStats.Lines;

  L.Add(Vidi_Lang.Module+': '+ACode.ModuleName);
  L.Add(Vidi_Lang.Folder+': '+ACode.ModulePath);

  Add;

  tmpTotal:=ACode.Edit.Lines.Count;

  L.Add('Lines: '+tmpTotal.ToString);

  if tmpTotal>0 then
  begin
    tmp:=CountEmpty(ACode.Edit.Lines);

    L.Add('Empty Lines: '+tmp.ToString+' ('+FormatFloat('##.##%',tmp*100/tmpTotal)+')');

    Add;
    L.Add('Errors: '+ACode.Errors.Count.ToString);

    LastCode:=ACode.Context;

    if ACode.Context<>nil then
    begin
      tmpCode.Reset;
      tmpCode.Calculate(ACode.Context,Recursive);

      PresentResults(tmpCode);
    end;
  end;
end;

procedure TFormStats.PresentResults(const AStats: TCodeStats);
var t : TCodeStatsItem;
begin
  Stats.Clear;

  for t:=Low(TCodeStatsItem) to High(TCodeStatsItem) do
      if AStats.Count[t]>0 then
         Stats.AppendRow([TCodeStats.TextOf(t),AStats.Count[t].ToString],nil);
end;

end.
