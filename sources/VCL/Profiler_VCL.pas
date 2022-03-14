unit Profiler_VCL;

interface

uses
  {$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  Windows, Messages,
  {$ENDIF}
  {$ENDIF}

  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, Grid_VCL, StdCtrls, AST,
  Utils_VCL, ComCtrls;

type
  TFormProfiler = class(TForm)
    Panel1: TPanel;
    CBEnabled: TCheckBox;
    PanelBottom: TPanel;
    Splitter1: TSplitter;
    PageTop: TPageControl;
    TabResults: TTabSheet;
    TabCoverage: TTabSheet;
    CBCoverage: TCheckBox;
    Panel2: TPanel;
    LTotals: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CBEnabledClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CBCoverageClick(Sender: TObject);
  private
    { Private declarations }

    Items,
    Coverage,
    Callers : TVidiGrid;

    procedure DoClicked(Sender: TObject);
    procedure DoSelectItem(Sender: TObject);
    procedure DoSelectCaller(Sender: TObject);
    procedure FillCallers(AIndex:Integer);
    procedure JumpTo(const AType:TType);
  public
    { Public declarations }

    OnGotoDeclaration : TOnGotoNode;

    procedure RefreshResults;
  end;

implementation

{$R *.dfm}

uses
  Find.AST, Utils.AST, Checker.AST, Profiler, IDE_Lang_English, Sys;

procedure TFormProfiler.CBCoverageClick(Sender: TObject);
begin
  Profile.Coverage.Enabled:=CBCoverage.Checked;
end;

procedure TFormProfiler.CBEnabledClick(Sender: TObject);
begin
  Profile.Enabled:=CBEnabled.Checked;
  CBCoverage.Enabled:=Profile.Enabled;
end;

procedure GetModuleType(const ANode:TType; out AModule,AType:String);
var tmp : TNamedType;
begin
  tmp:=TFinder.ModuleOf(ANode);

  if tmp=nil then AModule:='?' else
                  AModule:=TASTUtils.NodeName(tmp);

  AType:=TASTUtils.TypeOfOnly(TChecker.GetFinalType(ANode).Owner);
end;

procedure AppendCoverage(const AGrid:TVidiGrid; const ANode:TNode; const ACount:Integer);
var tmp : TNamedType;
    tmpModule : String;
begin
  tmp:=TFinder.ModuleOf(ANode);

  if tmp=nil then tmpModule:='?' else
                  tmpModule:=TASTUtils.NodeName(tmp);

  AGrid.AppendRow([tmpModule,TASTUtils.NodeName(ANode),IntToStr(ACount)],ANode);
end;

procedure AppendTo(const AGrid:TVidiGrid; const AType:TType;
                   const ACount,AElapsed:Integer);
var tmpModule, tmpType : String;
begin
  GetModuleType(AType,tmpModule,tmpType);

  AGrid.AppendRow([tmpModule,tmpType,TASTUtils.NodeName(AType),
                   IntToStr(ACount),IntToStr(AElapsed)],
                   AType);
end;

procedure TFormProfiler.FillCallers(AIndex:Integer);
var P : TCallerItem;
    tmp : TType;
begin
  Callers.BeginUpdate;
  try
    Callers.Clear;

    for P in Profile.Items[AIndex].Callers do
    begin
      tmp:=Profile.Items[P.Index].Routine;
      AppendTo(Callers,tmp,P.CallCount,P.Elapsed);
    end;
  finally
    Callers.EndUpdate;
  end;
end;

procedure TFormProfiler.DoClicked(Sender: TObject);
begin
  if Items.Row>0 then
     FillCallers(Items.Row-1);
end;

procedure TFormProfiler.JumpTo(const AType:TType);
begin
  if Assigned(OnGotoDeclaration) then
     OnGotoDeclaration(Self,AType);
end;

procedure TFormProfiler.DoSelectCaller(Sender: TObject);
begin
  if Callers.Row>0 then
     JumpTo(Callers.RowObjects[Callers.Row] as TType);
end;

procedure TFormProfiler.DoSelectItem(Sender: TObject);
begin
  if Items.Row>0 then
     JumpTo(Items.RowObjects[Items.Row] as TType);
end;

procedure TFormProfiler.FormCreate(Sender: TObject);
begin
  Items:=TVidiGrid.Add(Self,TabResults);
  Items.Header([Vidi_Lang.Module,Vidi_Lang._Type,Vidi_Lang.Routine,
                Vidi_Lang.Count,Vidi_Lang.Elapsed]); // Min, Max, Average

  Items.OnClick:=DoClicked;
  Items.OnDblClick:=DoSelectItem;

  Callers:=TVidiGrid.Add(Self,PanelBottom);
  Callers.Header([Vidi_Lang.Module,Vidi_Lang._Type,Vidi_Lang.Routine,
                  Vidi_Lang.Count,Vidi_Lang.Elapsed]); // Min, Max, Average

  Callers.OnDblClick:=DoSelectCaller;

  AddTitlePanel(PanelBottom,Vidi_Lang.Callers);

  Coverage:=TVidiGrid.Add(Self,TabCoverage);
  Coverage.Header([Vidi_Lang.Module,Vidi_Lang.Item,Vidi_Lang.Count]);
end;

procedure TFormProfiler.FormShow(Sender: TObject);
begin
  CBEnabled.Checked:=Profile.Enabled;
  CBCoverage.Enabled:=Profile.Enabled;

  CBCoverage.Checked:=Profile.Coverage.Enabled;
end;

procedure TFormProfiler.RefreshResults;

  procedure FillProfile;
  var tmp : TType;
      P : TProfileItem;
      t : Integer;
  begin
    Items.BeginUpdate;
    try
      Items.Clear;

      for t:=0 to Profile.Count-1 do
      begin
        P:=Profile.Items[t];
        tmp:=P.Routine;

        AppendTo(Items,tmp,P.CallCount,P.Elapsed);
      end;
    finally
      Items.EndUpdate;
    end;
  end;

  procedure FillCoverage;
  var P : TCoverageItem;
      t : Integer;
  begin
    Coverage.BeginUpdate;
    try
      Coverage.Clear;

      for t:=0 to Profile.Coverage.Count-1 do
      begin
        P:=Profile.Coverage.Items[t];
        AppendCoverage(Coverage,P.Node,P.Count);
      end;
    finally
      Coverage.EndUpdate;
    end;
  end;

  function ProfileTotals:String;
  begin
    result:='#'+Profile.Count.ToString+'  Calls: '+Profile.TotalCount.ToString+
            '  msec: '+Profile.TotalElapsed.ToString;
  end;
begin
  FillProfile;
  FillCoverage;

  LTotals.Caption:=ProfileTotals;
end;

end.
