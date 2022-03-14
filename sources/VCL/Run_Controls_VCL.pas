unit Run_Controls_VCL;

interface

uses
  {$IFDEF MSWINDOWS}
  //Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, Menus;

type
  TFormRunControls = class(TForm)
    Tool_Run: TImage;
    Tool_Pause: TImage;
    Tool_Stop: TImage;
    Tool_Stepin: TImage;
    Tool_Stepover: TImage;
    PopupRun: TPopupMenu;
    Runwithoutdebug1: TMenuItem;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ResetIcons(const IsDay:Boolean);
  end;

implementation

{$R *.dfm}

uses
  Utils_VCL;

procedure TFormRunControls.FormCreate(Sender: TObject);
begin
  ResetIcons(True);
end;

procedure TFormRunControls.ResetIcons(const IsDay:Boolean);
var S : String;

  procedure CheckTool(const ATool:TImage; const AIcon:String);
  begin
    if ATool.Enabled then
    begin
      if (ATool.Picture=nil) or (ATool.Picture.Width=0) then
         LoadPNG(ATool,S+AIcon);

      ATool.Visible:=True;
    end
    else
      ATool.Visible:=False;
  end;

begin
  if IsDay then
     S:=''
  else
     S:='white_';

  // TODO: Pending disabled icons for Run, StepIn and StepOver
  CheckTool(Tool_Run,'run');
  CheckTool(Tool_Stepin,'stepin');
  CheckTool(Tool_Stepover,'stepover');

  if Tool_Pause.Enabled then
     LoadPNG(Tool_Pause,S+'pause')
  else
     LoadPNG(Tool_Pause,S+'pause_disabled');

  if Tool_Stop.Enabled then
     LoadPNG(Tool_Stop,S+'stop')
  else
     LoadPNG(Tool_Stop,S+'stop_disabled');
end;

end.
