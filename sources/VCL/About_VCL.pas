unit About_VCL;

interface

uses
  {$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  Windows, Messages,
  {$ENDIF}
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons;

type
  TRestartEvent=procedure(Sender:TObject; const AFile:String) of object;

  TFormAbout = class(TForm)
    LVersion: TLabel;
    SpeedButton1: TSpeedButton;
    LWeb: TLabel;
    LGitHub: TLabel;
    LThanks: TLabel;
    BUpdate: TButton;
    procedure FormCreate(Sender: TObject);
    procedure LWebClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LThanksClick(Sender: TObject);
    procedure LVersionClick(Sender: TObject);
    procedure BUpdateClick(Sender: TObject);
    procedure LGitHubClick(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }

    const
      Vidi_Home='vidi.dev';
      Vidi_GitHub='github.com/davidberneda/Vidi';

    var
      OnRestart : TRestartEvent;

    class procedure GotoLanguageReference; static;
    class procedure Show(const AOwner:TComponent; const AOnRestart:TRestartEvent); static;
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF FPC}
  LclIntf, LCLType,
  {$ENDIF}
  Constants, IDE_Lang_English, Utils_VCL, Updater_VCL;

procedure TFormAbout.BUpdateClick(Sender: TObject);
var tmpFile : String;
    tmpLatest : String;
begin
  BUpdate.Enabled:=False;
  try
    if TUpdater.CheckUpdate(tmpLatest) then
    begin
      if TUpdater.WantsToUpdate(Self,tmpLatest,tmpFile) then
         OnRestart(Self,tmpFile)
    end
    else
      ShowMessage(Translate(Vidi_Lang.NoNewVersion));
  finally
    BUpdate.Enabled:=True;
  end;
end;

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  LVersion.Caption:=TVidiConstants.Vidi_Version +

  {$IFDEF FPC}
  {$IFDEF CPUX64}
  ' 64bit'
  {$ELSE}
  ' 32bit'
  {$ENDIF}
  {$ELSE}
  {$IFDEF CPU64BITS}
  ' 64bit'
  {$ELSE}
  ' 32bit'
  {$ENDIF}
  {$ENDIF}
  ;

  LWeb.Caption:=Vidi_Home;

  Caption:=Vidi_Lang.About;

  {$IFNDEF FPC}
  LThanks.Hide;
  {$ENDIF}
end;

procedure TFormAbout.LThanksClick(Sender: TObject);
begin
  TGoto.URL('https://www.getlazarus.org/');
end;

procedure TFormAbout.LVersionClick(Sender: TObject);
begin
  TGoto.URL(Vidi_GitHub+'/tree/master/download');
end;

procedure TFormAbout.LWebClick(Sender: TObject);
begin
  TGoto.URL((Sender as TLabel).Caption);
end;

procedure TFormAbout.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then
     Close;
end;

class procedure TFormAbout.GotoLanguageReference;
begin
  TGoto.URL(Vidi_GitHub+'/blob/master/documentation/Vidi_Language_Reference.md');
end;

class procedure TFormAbout.Show(const AOwner:TComponent; const AOnRestart:TRestartEvent);
begin
  with TFormAbout.Create(AOwner) do
  try
    OnRestart:=AOnRestart;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TFormAbout.LGitHubClick(Sender: TObject);
begin
  TGoto.URL(Vidi_GitHub);
end;

end.
