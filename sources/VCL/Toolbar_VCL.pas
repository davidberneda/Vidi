unit Toolbar_VCL;

interface

uses
  {$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  Windows, Messages,
  {$ENDIF}
  {$ENDIF}

  SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls, Menus,
  Run_Controls_VCL;

type

  { TFormToolbar }

  TFormToolbar = class(TForm)
    PanelRun: TPanel;
    Toolbar: TPanel;
    PanelButtons: TPanel;
    Tool_New: TImage;
    Tool_Open: TImage;
    Tool_Save: TImage;
    Tool_SaveAll: TImage;
    Tool_Search: TImage;
    Tool_Compile: TImage;
    Tool_Prompt: TImage;
    Tool_Explorer: TImage;
    Tool_Log: TImage;
    Tool_Modules: TImage;
    Tool_Types: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }

    RunControls : TFormRunControls;

    procedure LoadIcons(const APrefix:String; Force:Boolean);
  end;

implementation

{$R *.dfm}
{$R tool_images.res}
{$R tool_white_images.res}

uses
  Utils_VCL;

procedure TFormToolbar.LoadIcons(const APrefix:String; Force:Boolean);
var t : Integer;
    B : TPanel;
    C : TControl;
begin
  B:=PanelButtons;

  for t:=0 to B.ControlCount-1 do
  begin
    C:=B.Controls[t];

    if C is TImage then
       if Force or (TImage(C).Picture.Width=0) then
          LoadPNG(TImage(C),APrefix+Copy(C.Name,6,255));
  end;
end;

procedure TFormToolbar.FormCreate(Sender: TObject);
begin
  LoadIcons('',False);

  RunControls:=TFormRunControls.Create(Self);
  AddForm(RunControls,PanelRun,TAlign.alLeft);
  RunControls.Show;
end;

end.
