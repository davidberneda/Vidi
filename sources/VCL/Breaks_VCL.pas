unit Breaks_VCL;

interface

uses
  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,
  Run_Breaks, ExtCtrls;

type
  TGotoBreakEvent=procedure(Sender:TObject; const ABreak:TBreakHere) of object;

  TFormBreaks = class(TForm)
    LBBreaks: TListBox;
    PanelBreak: TPanel;
    CBEnabled: TCheckBox;
    CBStop: TCheckBox;
    CBOutput: TCheckBox;
    EExpression: TEdit;
    Splitter1: TSplitter;
    Label1: TLabel;
    ECondition: TEdit;
    procedure LBBreaksDblClick(Sender: TObject);
    procedure CBOutputClick(Sender: TObject);
    procedure LBBreaksClick(Sender: TObject);
    procedure CBEnabledClick(Sender: TObject);
    procedure CBStopClick(Sender: TObject);
    procedure EExpressionChange(Sender: TObject);
    procedure EConditionChange(Sender: TObject);
  private
    { Private declarations }

    procedure CheckPanelBreak;
    procedure FillBreak(const ABreak:TBreakHere);
  public
    { Public declarations }
    Breaks : TBreaks;

    OnBreakChange : TGotoBreakEvent;
    OnGoToBreak : TGotoBreakEvent;

    procedure Fill;
  end;

implementation

{$R *.dfm}

uses
  Emit;

{ TFormBreaks }

procedure TFormBreaks.CBEnabledClick(Sender: TObject);
begin
  Breaks.Items[LBBreaks.ItemIndex].Enabled:=CBEnabled.Checked;
  // TODO: Notify editor gutter to repaint

  OnBreakChange(Self,Breaks.Items[LBBreaks.ItemIndex]);
end;

procedure TFormBreaks.CBOutputClick(Sender: TObject);
begin
  EExpression.Enabled:=CBOutput.Checked;

  Breaks.Items[LBBreaks.ItemIndex].OutputExpression:=CBOutput.Checked;
end;

procedure TFormBreaks.CBStopClick(Sender: TObject);
begin
  Breaks.Items[LBBreaks.ItemIndex].StopExecution:=CBStop.Checked;
end;

procedure TFormBreaks.Fill;
var t : Integer;
    tmp : String;
    Emit : TVidiEmit;
    Old : Integer;
begin
  Old:=LBBreaks.ItemIndex;

  if Old=-1 then
     Old:=0;

  LBBreaks.Items.BeginUpdate;
  try
    LBBreaks.Clear;

    Emit:=TVidiEmit.Create;
    try
      for t:=Low(Breaks.Items) to High(Breaks.Items) do
      begin
        tmp:=IntToStr(Breaks.Items[t].Line)+' '+Emit.AsString(Breaks.Items[t].Node);

        LBBreaks.Items.Add(tmp);
      end;
    finally
      Emit.Free;
    end;
  finally
    LBBreaks.Items.EndUpdate;
  end;

  CheckPanelBreak;

  if LBBreaks.Count>Old then
  begin
    LBBreaks.ItemIndex:=Old;
    LBBreaksClick(Self);
  end;
end;

procedure TFormBreaks.CheckPanelBreak;
begin
  PanelBreak.Visible:=LBBreaks.ItemIndex<>-1;
  Splitter1.Visible:=PanelBreak.Visible;
end;

procedure TFormBreaks.EConditionChange(Sender: TObject);
begin
  Breaks.Items[LBBreaks.ItemIndex].Condition:=ECondition.Text;
end;

procedure TFormBreaks.EExpressionChange(Sender: TObject);
begin
  Breaks.Items[LBBreaks.ItemIndex].Expression:=EExpression.Text;
end;

procedure TFormBreaks.LBBreaksClick(Sender: TObject);
begin
  CheckPanelBreak;

  if PanelBreak.Visible then
  begin
    FillBreak(Breaks.Items[LBBreaks.ItemIndex]);
  end;
end;

procedure TFormBreaks.FillBreak(const ABreak:TBreakHere);
begin
  CBEnabled.Checked:=ABreak.Enabled;
  CBStop.Checked:=ABreak.StopExecution;
  CBOutput.Checked:=ABreak.OutputExpression;

  EExpression.Text:=ABreak.Expression;
  EExpression.Enabled:=CBOutput.Enabled;

  ECondition.Text:=ABreak.Condition;
end;

procedure TFormBreaks.LBBreaksDblClick(Sender: TObject);
begin
  if Assigned(OnGotoBreak) then
     if LBBreaks.ItemIndex<>-1 then
        OnGotoBreak(Self,Breaks.Items[LBBreaks.ItemIndex]);
end;

end.
