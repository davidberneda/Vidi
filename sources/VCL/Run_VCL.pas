unit Run_VCL;

interface

uses
  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, ExtCtrls, StdCtrls,

  {$IFDEF FPC}
  RichMemo,
  {$ELSE}
  pngimage,
  {$ENDIF}

  ComCtrls;

type
  {$IFDEF FPC}
  TRichEdit=TRichMemo;
  {$ENDIF}

  { TFormRun }

  TFormRun = class(TForm)
    PanelButtons: TPanel;
    Label1: TLabel;
    CBModule: TComboBox;
    SBClear: TSpeedButton;
    CBClearBeforeRun: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure SBClearClick(Sender: TObject);
    procedure CBModuleDropDown(Sender: TObject);
    procedure CBModuleChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    Console: TRichEdit;

    OnChangeStart : TNotifyEvent;

    procedure FillModules;

    procedure MemoRunClear;
    function MemoRunGet:String;
    procedure MemoRunPut(const AText:String);
  end;

implementation

{$R *.dfm}

uses
  {$IFNDEF FPC}
  UITypes,
  {$ENDIF}
  Module, Constants;

procedure TFormRun.CBModuleChange(Sender: TObject);
begin
  OnChangeStart(Self);
end;

procedure TFormRun.CBModuleDropDown(Sender: TObject);
begin
  FillModules;
end;

procedure TFormRun.FillModules;
var Old : String;
    t : Integer;
begin
  Old:=CBModule.Text;

  CBModule.Items.BeginUpdate;
  try
    CBModule.Clear;

    for t:=0 to High(Modules.Items) do
        CBModule.Items.Add(Modules.Items[t].Name);

    CBModule.Sorted:=True;

    if Old<>'' then
       CBModule.ItemIndex:=CBModule.Items.IndexOf(Old);

  finally
    CBModule.Items.EndUpdate;
  end;
end;

procedure TFormRun.SBClearClick(Sender: TObject);
begin
  MemoRunClear;
end;

function TFormRun.MemoRunGet:String;
begin
  result:=InputBox(TVidiConstants.Vidi_Name,'Enter text:','')
end;

procedure TFormRun.MemoRunClear;
begin
  Console.Clear;
  SBClear.Enabled:=False;
end;

procedure TFormRun.MemoRunPut(const AText:String);
begin
  {$IFDEF FPC}
  Console.Text:=Console.Text+AText;
  {$ELSE}
  Console.SelText:=AText;
  {$ENDIF}

  // ?? Thread?
  Application.ProcessMessages;

  SBClear.Enabled:=True;
end;

procedure TFormRun.FormCreate(Sender: TObject);
begin
  Console:=TRichEdit.Create(Self);

  Console.Align:=TAlign.alClient;
  Console.ScrollBars:=TScrollStyle.ssBoth;
  Console.Parent:=Self;
end;

end.
