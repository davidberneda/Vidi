unit Run_Monitor;

interface

{$IFNDEF FPC}
{$DEFINE FASTMM}
{$ENDIF}

uses
  {$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  Windows, Messages,
  {$ENDIF}
  {$ENDIF}

  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, Evaluator, StdCtrls, ExtCtrls, ComCtrls,
  Grid_VCL;

type
  TFormRunMonitor = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    CBStatus: TComboBox;
    Label2: TLabel;
    LCount: TLabel;
    Splitter1: TSplitter;
    Button1: TButton;
    Label3: TLabel;
    LabelCurrent: TLabel;
    PageControl1: TPageControl;
    TabItems: TTabSheet;
    Label4: TLabel;
    LabelLast: TLabel;
    LBrunStack: TListBox;
    CBExtraRefresh: TCheckBox;
    Splitter2: TSplitter;
    TabShared: TTabSheet;
    Splitter3: TSplitter;
    TabMemory: TTabSheet;
    Memory: TMemo;
    Timer1: TTimer;
    Panel2: TPanel;
    CBAutoRefresh: TCheckBox;
    TBRefresh: TTrackBar;
    LMsec: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure CBExtraRefreshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBAutoRefreshClick(Sender: TObject);
    procedure TBRefreshChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }

    FRunner : TRunner;

    Items,
    Shared,
    Children : TVidiGrid;

    procedure DoClicked(Sender: TObject);

    {$IFDEF INTERNAL}
    procedure FillChildren(const Index:Integer);
    {$ENDIF}

    procedure MonitorEvent(const Sender:TRunner);
    procedure RefreshMemory;
    procedure SetRunner(const Value: TRunner);
  public
    { Public declarations }

    procedure RefreshRunner;
    property Runner:TRunner read FRunner write SetRunner;
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF FASTMM}
  FastMM5,
  {$ENDIF}

  {$IFDEF INTERNAL}
  Sys,
  AST,
  RunningNode,
  {$ENDIF}

  Emit, Runner, Instance_Type,
  IDE_Lang_English;

procedure TFormRunMonitor.Button1Click(Sender: TObject);
begin
  RefreshRunner;
end;

procedure TFormRunMonitor.CBExtraRefreshClick(Sender: TObject);
begin
  {$IFDEF INTERNAL}
  TRunner.ExtraMonitorRefresh:=CBExtraRefresh.Checked;
  {$ENDIF}
end;

procedure TFormRunMonitor.CBAutoRefreshClick(Sender: TObject);
begin
  Timer1.Enabled:=CBAutoRefresh.Checked;
end;

{$IFDEF INTERNAL}
function IDOf(const ANode:TNode):String;
begin
  if ANode is TInstance then
     result:=IntToStr(TInstance(ANode).ID)
  else
     result:='';
end;

procedure TFormRunMonitor.FillChildren(const Index:Integer);

  procedure AddChildren(const AData:TInstance.TInstanceData);
  var D : TInstance.TVariableValue;
      t : Integer;
      tmpName,
      tmpValue : String;
  begin
    for t:=0 to High(AData) do
    begin
      D:=AData[t];

      if (D.Value=nil) or (D.Value.Value=nil) then
         tmpValue:=''
      else
         tmpValue:=B(D.Value.Value);

      if D.Variable=nil then
         tmpName:='?'
      else
         tmpName:=D.Variable.Name;

      Children.AppendRow([IntToStr(t),
                          IDOf(D.Value),
                          tmpName,
                          tmpValue
                       ],nil);
    end;
  end;

  procedure AddArrayItems(const AArray:TArrayExpression);
  var t : Integer;
      tmp : TNode;
      tmpValue : String;
  begin
    for t:=0 to High(AArray.Parameters) do
    begin
      tmp:=AArray.Parameters[t];

      if tmp=nil then
         tmpValue:=''
      else
         tmpValue:=B(tmp);

      Children.AppendRow([IntToStr(t),IDOf(tmp),'',tmpValue],nil);
    end;
  end;

begin
  Children.BeginUpdate;
  try
    Children.Clear;

    AddChildren(Instances[Index].Data);

    if Instances[Index].Value is TArrayExpression then
       AddArrayItems(TArrayExpression(Instances[Index].Value));
  finally
    Children.EndUpdate;
  end;
end;
{$ENDIF}

procedure TFormRunMonitor.DoClicked(Sender: TObject);
begin
  {$IFDEF INTERNAL}
  if Items.Row>0 then
     FillChildren(Items.Row-1);
  {$ENDIF}
end;

procedure TFormRunMonitor.FormCreate(Sender: TObject);
const
  ID_Width=36;
begin
  Items:=TVidiGrid.Add(Self,TabItems);
  Items.Header([Vidi_Lang.Index,Vidi_ID,Vidi_Lang.Owner,Vidi_Lang.Children,
                Vidi_Lang.Value,Vidi_Lang.FirstVariable]);

  Items.OnClick:=DoClicked;

  Items.ColWidths[0]:=ID_Width;
  Items.ColWidths[1]:=ID_Width;
  Items.ColWidths[2]:=ID_Width;
  Items.ColWidths[3]:=ID_Width;

  Children:=TVidiGrid.Add(Self,TabItems);
  Children.Header([Vidi_Lang.Index,Vidi_ID,Vidi_Lang.Name,Vidi_Lang.Value]);
  Children.Align:=TAlign.alRight;
  Children.Width:=200;

  Children.ColWidths[0]:=ID_Width;
  Children.ColWidths[1]:=ID_Width;

  Splitter2.Align:=TAlign.alRight;

  Shared:=TVidiGrid.Add(Self,TabShared);
  Shared.Header([Vidi_Lang.Index,Vidi_ID,Vidi_Lang._Type,Vidi_Lang.Children,
                Vidi_Lang.Value,Vidi_Lang.FirstVariable]);

  Shared.ColWidths[0]:=ID_Width;
  Shared.ColWidths[1]:=ID_Width;
  Shared.ColWidths[3]:=ID_Width;

  PageControl1.ActivePage:=TabItems;
end;

procedure TFormRunMonitor.MonitorEvent(const Sender: TRunner);
begin
  if Parent.Visible then
     RefreshRunner;
end;

procedure TFormRunMonitor.RefreshRunner;

{
  function AddInstance(const AParent:TTreeNode; const AText:String; const AInstance:TInstance):TTreeNode;

    procedure AddChilds(const AParent:TTreeNode; const AInstance:TInstance);
    var t : Integer;
        tmp : TInstance.TVariableValue;
        tmpName : String;
    begin
      for t:=Low(AInstance.Data) to High(AInstance.Data) do
      begin
        tmp:=AInstance.Data[t];

        if tmp.Variable=nil then
           tmpName:='?'
        else
           tmpName:=tmp.Variable.Name;

        if tmp.Value=nil then
           Tree1.Items.AddChildObject(AParent,tmpName,tmp.Variable)
        else
           AddChilds(AddInstance(AParent,tmpName+' : '+IntToStr(t),tmp.Value),tmp.Value);
      end;
    end;

  begin
    result:=Tree1.Items.AddChildObject(AParent,AText,AInstance);

    if AInstance<>nil then
       AddChilds(result,AInstance);
  end;
}

  {$IFDEF INTERNAL}
  function FirstVariableName(const AInstance:TInstance):String;
  var t : Integer;
  begin
    if AInstance.Data=nil then
       result:=''
    else
    for t:=Low(AInstance.Data) to High(AInstance.Data) do
        if AInstance.Data[t].Variable<>nil then
           Exit(AInstance.Data[0].Variable.Name);
  end;

  procedure Fill_Instances;

    function OwnerOf(const AIndex:Integer):Integer;
    var tmp : TInstance;
        t : Integer;
        tmpOwner : TNode;
    begin
      result:=-1;

      tmpOwner:=Instances[AIndex].Owner;

      if tmpOwner is TInstance then
      begin
        tmp:=tmpOwner as TInstance;

        if tmp<>nil then
           for t:=0 to High(Instances) do
               if Instances[t]=tmp then
               begin
                 result:=t;
                 break;
               end;
      end;
    end;

    procedure AddItems;
    var t : Integer;
        tmpValue : String;
        tmp : TInstance;
    begin
      for t:=0 to High(Instances) do
      begin
        tmp:=Instances[t];

        if tmp.Value=nil then
           tmpValue:=''
        else
           tmpValue:=B(tmp.Value);

        Items.AppendRow([IntToStr(t),
                         IDOf(tmp),
                         IntToStr(OwnerOf(t)),
                         IntToStr(Length(tmp.Data)),
                         tmpValue,
                         FirstVariableName(tmp)

                         ],tmp);
      end;
    end;

  var Old : Integer;
  begin
    Old:=Items.Row;

    Items.BeginUpdate;
    try
      Items.Clear;
      AddItems;
    finally
      Items.EndUpdate;
    end;

    Children.Clear;

    if Old>1 then
       if Items.RowCount>Old then
       begin
         Items.Row:=Old;
       end;
  end;

  procedure Fill_Shared;
  var t : Integer;
      tmp : TInstance.TTypeValue;
      tmpInst : TInstance;
      tmpValue:String;
  begin
    Shared.BeginUpdate;
    try
      Shared.Clear;

      for t:=0 to High(TInstance.Types) do
      begin
        tmp:=TInstance.Types[t];
        tmpInst:=tmp.Value;

        if tmpInst.Value=nil then
           tmpValue:=''
        else
           tmpValue:=B(tmpInst.Value);

        Shared.AppendRow([IntToStr(t),
                          IDOf(tmpInst),
                          B(tmp.TheType),

                          IntToStr(Length(tmp.Value.Data)),
                          tmpValue,
                          FirstVariableName(tmp.Value)
                         ],tmpInst);
      end;
    finally
      Shared.EndUpdate;
    end;
  end;

  procedure FillRunStack;

     function ItemAsText(const AItem:TRunningNode):String;
     begin
       result:=TRunningNode.StackAsText(AItem);
     end;

  var Item : TRunningNode;
  begin
    LBrunStack.Items.BeginUpdate;
    try
      LBrunStack.Clear;

      for Item in TRunningNode.RunStack do
          LBRunStack.Items.Add(ItemAsText(Item));
    finally
      LBrunStack.Items.EndUpdate;
    end;
  end;
  {$ENDIF}

  procedure RefreshLabels;
  begin
    //LCount.Caption:=Tree1.Items.Count.ToString;

    if Runner.Current=nil then
       LabelCurrent.Caption:=''
    else
       LabelCurrent.Caption:=Runner.Current.ClassName;

    if Runner.Last=nil then
       LabelLast.Caption:=''
    else
       LabelLast.Caption:=Runner.Last.ClassName;
  end;

begin
  case Runner.Status of
    Running: CBStatus.ItemIndex:=0;
    Stopped: CBStatus.ItemIndex:=1;
    Paused: CBStatus.ItemIndex:=2;
  end;

  {
  Tree1.Items.Clear;

  if Runner.Root<>nil then
     AddInstance(nil,'Root',Runner.Root);
  }

  RefreshLabels;

  {$IFDEF INTERNAL}
  Fill_Instances;
  FillRunStack;
  Fill_Shared;
  {$ENDIF}

  RefreshMemory;

  {
  if Tree1.Items.Count>0 then
     Tree1.Items[0].Expanded:=True;
  }
end;

procedure TFormRunMonitor.SetRunner(const Value: TRunner);
begin
  if FRunner<>Value then
  begin
    if FRunner<>nil then
       FRunner.OnMonitor:=nil;

    FRunner:=Value;

    if FRunner<>nil then
       FRunner.OnMonitor:=MonitorEvent;
  end;
end;

procedure TFormRunMonitor.TBRefreshChange(Sender: TObject);
var tmp : Integer;
begin
  tmp:=TBRefresh.Position;

  tmp:= 500+(tmp*500);

  LMsec.Caption:=IntToStr(tmp)+' msec';

  Timer1.Interval:=tmp;
end;

procedure TFormRunMonitor.RefreshMemory;

  procedure Add(const S:String);
  begin
    Memory.Lines.Add(S);
  end;

  function IntegerToStr(const AValue:Single):String;
  begin
    result:=Format('%.0n',[AValue]);
  end;

begin
  Memory.Clear;

  {$IFDEF FPC}
  with GetHeapStatus do
  begin
    Add('Total space: '+IntegerToStr(TotalAddrSpace));
    Add('Allocated: '+IntegerToStr(TotalAllocated));
    Add('Overhead: '+IntegerToStr(Overhead));
    Add('Committed: '+IntegerToStr(TotalCommitted));
    Add('Uncommitted: '+IntegerToStr(TotalUncommitted));
    Add('Total free: '+IntegerToStr(TotalFree));
  end;

  {$ELSE}
  {$IFDEF FASTMM}
  Add('Current: '+IntegerToStr(FastMM_GetCurrentMemoryUsage));
  Add('Allocated: '+IntegerToStr(FastMM_GetHeapStatus.TotalAllocated));
  Add('Free: '+IntegerToStr(FastMM_GetHeapStatus.TotalFree));
  Add('Overhead: '+IntegerToStr(FastMM_GetHeapStatus.Overhead));

  Add('Allocated: '+IntegerToStr(FastMM_GetUsageSummary.AllocatedBytes));
  Add('Overhead: '+IntegerToStr(FastMM_GetUsageSummary.OverheadBytes));
  Add('Efficiency: '+FormatFloat('#.00',FastMM_GetUsageSummary.EfficiencyPercentage)+'%');
  {$ENDIF}
  {$ENDIF}
end;

procedure TFormRunMonitor.Timer1Timer(Sender: TObject);
begin
  RefreshMemory;
end;

end.
