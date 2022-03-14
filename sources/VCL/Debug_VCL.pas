unit Debug_VCL;

interface

uses
  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, Breaks_VCL, Trace_VCL, Watch_VCL,
  Evaluator,
  ExtCtrls;

type
  TFormDebug = class(TForm)
    PanelBreak: TPanel;
    Splitter1: TSplitter;
    PanelTrace: TPanel;
    Splitter2: TSplitter;
    PanelWatch: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }

    Breaks : TFormBreaks;
    Trace : TFormTrace;
    Watch : TFormWatch;

    procedure Fill(const ARunner:TRunner);
  end;

implementation

{$R *.dfm}

uses
  Utils_VCL, IDE_Lang_English, Instance_Type;

procedure TFormDebug.Fill(const ARunner:TRunner);

  function CurrentInstance:TInstance;
  begin
    if ARunner.Last=nil then
       result:=ARunner.Root
    else
       result:=ARunner.Last.Instance;
  end;

begin
  Breaks.Breaks:=ARunner.Breaks;
  Breaks.Fill;

  Watch.Fill(CurrentInstance);

  Trace.Fill(ARunner.Stack);
end;

procedure TFormDebug.FormCreate(Sender: TObject);
begin
  Breaks:=TFormBreaks.Create(Self);
  AddForm(Breaks,PanelBreak,TAlign.alClient);
  Breaks.Show;

  Trace:=TFormTrace.Create(Self);
  AddForm(Trace,PanelTrace,TAlign.alClient);
  Trace.Show;

  Watch:=TFormWatch.Create(Self);
  AddForm(Watch,PanelWatch,TAlign.alClient);
  Watch.Show;

  AddTitlePanel(PanelBreak,Vidi_Lang.Breaks);
  AddTitlePanel(PanelTrace,Vidi_Lang.Trace);
  AddTitlePanel(PanelWatch,Vidi_Lang.Watch);
end;

end.
