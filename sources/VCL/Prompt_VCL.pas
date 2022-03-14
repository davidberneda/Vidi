unit Prompt_VCL;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,
  Vidi_CLI;

type
  TPrompt = class(TForm)
    Memo1: TMemo;
    procedure Memo1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }

    procedure AddLine(const S:String);
    procedure AddPrompt;
    function LastLine:String;
    procedure Process(const S:String);
  public
    { Public declarations }

    CLI: TVidi_CLI;
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF FPC}
  LCLType,
  {$ENDIF}
  Sys, Exceptions;

procedure TPrompt.AddPrompt;
begin
  if (Length(Memo1.Text)>=3) and (Copy(Memo1.Text,Length(Memo1.Text)-2,3)=#13#10'>') then
  else
  begin
    if Copy(Memo1.Text,Length(Memo1.Text)-1,1)<>'>' then
       Memo1.Text:=Memo1.Text+'>';

    Memo1.SelStart:=Length(Memo1.Text);
  end;
end;

procedure TPrompt.AddLine(const S:String);
begin
  Memo1.Text:=Memo1.Text+S+#13#10;
end;

procedure TPrompt.FormCreate(Sender: TObject);
begin
  CLI:=TVidi_CLI.Create;
end;

procedure TPrompt.FormDestroy(Sender: TObject);
begin
  CLI.Free;
end;

procedure TPrompt.FormShow(Sender: TObject);
begin
  AddPrompt;
end;

procedure TPrompt.Process(const S:String);
var tmp : TNode;
begin
  if S<>'' then
  try
    tmp:=CLI.ParseAndEvaluate(S);

    if tmp=nil then
       AddLine('nil')
    else
    try
      AddLine(CLI.NodeTypeOf(tmp)+': '+CLI.AsString(tmp));
    finally
      tmp.Free;
    end;

  except
    on E:EBeeException do
       if E.Error=nil then
          AddLine('Internal prompt exception')
       else
          AddLine(E.Error.Text)
  end;
end;

function RemovePrompt(const S:String):String;
begin
  result:=Trim(S);
  result:=Copy(result,2,Length(result));
end;

function TPrompt.LastLine:String;
begin
  if Memo1.Lines.Count=0 then
     result:=''
  else
     result:=RemovePrompt(Memo1.Lines[Memo1.Lines.Count-1]);
end;

procedure TPrompt.Memo1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_RETURN then
  begin
    Process(LastLine);

    AddPrompt;

    {$IFDEF MSWINDOWS}
    SendMessage(Memo1.Handle, WM_VSCROLL, SB_BOTTOM, 0);
    {$ENDIF}
  end;
end;

end.
