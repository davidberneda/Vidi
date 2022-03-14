unit Updater_VCL;

interface

uses
  {$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  Windows,
  {$ENDIF}
  {$ENDIF}

  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  {$IFNDEF FPC}
  Zip,
  {$ENDIF}
  ExtCtrls;

type
  TUpdater = class(TForm)
    ProgressBar1: TProgressBar;
    LStatus: TLabel;
    BCancel: TButton;
    Timer1: TTimer;
    LPercent: TLabel;
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
  private
    { Private declarations }

    PartialZipSize,
    TotalZipSize : Integer;

    ExeFile,
    Release : String;
    UpdateCancel : Boolean;

    procedure ChangeProgress(const AValue:Integer);

    procedure DownloadProgress(Sender:TObject; const ACurrent,ATotal:Int64; var Abort:Boolean);
    function DoUpdate(const AFile:String):Boolean;

    {$IFDEF FPC}
    procedure UnZipProgress(Sender : TObject; Const ATotPos, ATotSize: Int64);
    {$ELSE}
    procedure UnZipProgress(Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64);
    {$ENDIF}

    procedure UnZipFile(const AZip,ADest:String);
  public
    { Public declarations }
    class var
      InternalAlwaysUpdate:Boolean;

    class function CheckUpdate(out ALatest:String):Boolean; static;
    class function LatestRelease:String; static;
    class function UpdateLatest(const AOwner:TComponent; const Release:String; out AFile:String):Boolean; static;
    class function WantsToUpdate(const AOwner:TComponent; const ALatest:String; out AFile:String):Boolean; static;
  end;


implementation

{$R *.dfm}

uses
  {$IFDEF FPC}
  Zipper,
  {$ENDIF}
  IO, Utils_VCL, Options, Constants, IDE_Lang_English;

const
  HttpPrefix={$IFDEF FPC}'http'{$ELSE}'https'{$ENDIF};
  GitHubLatest='://raw.githubusercontent.com/davidberneda/Vidi/master/download/'+TVidiConstants.LatestRelease;
  GitHubRelease='://github.com/'+TVidiConstants.DownloadURL+'/';
{$IFNDEF FPC}

function CalcTotalZipSize(const AZip:TZipFile):Integer;
var t : Integer;
begin
  result:=0;

  for t:=0 to AZip.FileCount-1 do
      Inc(result,AZip.FileInfo[t].UncompressedSize);
end;

procedure TUpdater.UnZipProgress(Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64);
begin
  {
  if UpdateCancel then
     (Sender as TUnzipper).Terminate
  else
  }
  if TotalZipSize>0 then
     if Position=Header.UncompressedSize then
     begin
       Inc(PartialZipSize,Position);
       ChangeProgress(Round(PartialZipSize*100/TotalZipSize));
     end;
end;
{$ENDIF}

procedure TUpdater.ChangeProgress(const AValue:Integer);
begin
  if ProgressBar1.Position<>AValue then
  begin
    ProgressBar1.Position:=AValue;
    LPercent.Caption:=IntToStr(AValue)+'%';
    Application.ProcessMessages;
  end;
end;

procedure TUpdater.DownloadProgress(Sender:TObject; const ACurrent,ATotal:Int64; var Abort:Boolean);
begin
  Abort:=UpdateCancel;

  if not Abort then
     if ATotal>0 then
        ChangeProgress(Round(ACurrent*100/ATotal));
end;

procedure TUpdater.FormShow(Sender: TObject);
begin
  ProgressBar1.Max:=100;

  // Force form show
  Timer1.Enabled:=True;
end;

{$IFDEF FPC}
procedure TUpdater.UnZipProgress(Sender : TObject; Const ATotPos, ATotSize: Int64);
begin
  if UpdateCancel then
     (Sender as TUnzipper).Terminate
  else
     ChangeProgress(Round(ATotPos*100/ATotSize));
end;
{$ENDIF}

procedure TUpdater.UnZipFile(const AZip,ADest:String);
{$IFDEF FPC}
var UnZipper: TUnZipper;
{$ELSE}
var LZip: TZipFile;
{$ENDIF}
begin
  {$IFDEF FPC}
  UnZipper := TUnZipper.Create;
  try
    UnZipper.FileName := AZip;
    UnZipper.OutputPath := ADest;
    UnZipper.Examine;

    UnZipper.OnProgressEx:=UnZipProgress;

    UnZipper.UnZipAllFiles;
  finally
    UnZipper.Free;
  end;
  {$ELSE}
  LZip := TZipFile.Create;
  try
    LZip.Encoding := nil;

    LZip.OnProgress := UnZipProgress;

    LZip.Open(AZip, zmRead);

    // Problem: Progres is per-file, not per-full-zip-file
    PartialZipSize:=0;
    TotalZipSize:=CalcTotalZipSize(LZip);

    LZip.ExtractAll(ADest);
    LZip.Close;
  finally
    LZip.Free;
  end;
  {$ENDIF}
end;

function TUpdater.DoUpdate(const AFile:String):Boolean;

  function InternalFindIDE(APath:String):String;
  begin
    APath:=APath+PathDelimiter+'ide';

    {$IFDEF MSWINDOWS}
    APath:=APath+PathDelimiter+'windows';
    {$ELSE}
    APath:=APath+PathDelimiter+'linux';
    {$ENDIF}

    {$IFDEF CPU64BITS}
    APath:=APath+PathDelimiter+'64bit';
    {$ELSE}
    APath:=APath+PathDelimiter+'32bit';
    {$ENDIF}

    result:=CombineFile(APath,'vidi.exe'); // only for windows !!
  end;

  procedure RenameIDE(const OldName:String);
  var NewName : String;
  begin
    NewName:=RemoveExtension(OldName)+'.old';

    DeleteFile(NewName);

    if not RenameFile(OldName,NewName) then
       raise Exception.Create('Cannot rename file: '+OldName+#13#10+LastErrorMessage);
  end;

  function CanAccessFolder(const AFolder:String):Boolean;
  var tmp : TFileStream;
      S : String;
  begin
    result:=False;

    try
      repeat
        S:=CombineFile(AFolder,IntToStr(Random(10000000)));
      until not DoFileExists(S);

      tmp:=TFileStream.Create(S,fmCreate);
      try
        result:=True;
      finally
        tmp.Free;
      end;

      DoDeleteFile(S);
    except
      on E:Exception do
      begin
        ShowMessage('Please install manually. Cannot access folder: '+AFolder+#13#10#13#10+E.Message);
      end;
    end;
  end;

var tmp,
    OldName :String;
begin
  tmp:=TOptions.HomePath;

  if InternalAlwaysUpdate then
  begin
    // Fake test mode
    tmp:=tmp+PathDelimiter+'Install_Test';
    ForceDirectories(tmp);

    // Test access denied problem: tmp:='C:\Program Files (x86)\Vidi';
  end;

  if CanAccessFolder(tmp) then
  begin
    LStatus.Caption:=Vidi_Lang.Installing;

    ChangeProgress(0);

    OldName:=ParamStr(0);
    RenameIDE(OldName);

    UnZipFile(AFile,tmp);

    if UpdateCancel then
       result:=False
    else
    begin
      if InternalAlwaysUpdate then
         OldName:=InternalFindIDE(tmp);

      result:=DoFileExists(OldName);

      if result then
         ExeFile:=OldName;
    end;
  end
  else
    result:=False;
end;

class function TUpdater.LatestRelease:String;

  function TrimCRLF(const S:String):String;
  var Last : String;
      Found : Boolean;
  begin
    result:=Trim(S);

    repeat
      Last:=Copy(result,Length(result),1);

      Found:=(Last=#13) or (Last=#10);

      if Found then
         Delete(result,Length(result),1);

    until not Found;
  end;

const
  URL=HttpPrefix+GitHubLatest;

var S : String;
begin
  try
    S:=GetURL(URL);
    result:=TrimCRLF(S);
  except
    on Exception do
       result:='';
  end;
end;

procedure TUpdater.Timer1Timer(Sender: TObject);
var Zip,
    tmpFile,
    Local : String;
begin
  Timer1.Enabled:=False;

  UpdateCancel:=False;

  Caption:=Format(Translate(Vidi_Lang.Updating),[Release]);

  tmpFile:=TVidiConstants.ReleasePrefix+Release+'.zip';

  Zip:=HttpPrefix+GitHubRelease+tmpFile;

  LStatus.Caption:=Vidi_Lang.Downloading;

  Local:=CombineFile(GetTempPath,tmpFile);

//  if not DoFileExists(Local) then
     if not DownloadFile(Zip,Local,DownloadProgress) then
        Local:='';

  if UpdateCancel then
     Close
  else
  if Local='' then
  begin
    ShowMessage(Vidi_Lang.DownloadError+#13#10+#13#10+
                Zip);
    Close;
  end
  else
  if DoUpdate(Local) then
     ModalResult:=mrOk;
end;

class function TUpdater.UpdateLatest(const AOwner:TComponent; const Release: String; out AFile:String):Boolean;
var Updater : TUpdater;
    tmp : TModalResult;
begin
  Updater:=TUpdater.Create(AOwner);
  try
    Updater.Release:=Release;

    tmp:=Updater.ShowModal;

    result:=tmp=mrOk;

    if result then
       AFile:=Updater.ExeFile;
  finally
    Updater.Free;
  end;
end;

procedure TUpdater.BCancelClick(Sender: TObject);
begin
  UpdateCancel:=True;
end;

function AskUpdate(const NewVersion:String):Boolean;
begin
  result:=YesNo(Translate(Format(Vidi_Lang.SureToUpdate,[TVidiConstants.Vidi_Version,NewVersion])));
end;

class function TUpdater.CheckUpdate(out ALatest:String):Boolean;
begin
  ALatest:=LatestRelease;

  result:=(ALatest<>'') and (InternalAlwaysUpdate or (ALatest<>TVidiConstants.Vidi_Version));
end;

class function TUpdater.WantsToUpdate(const AOwner:TComponent; const ALatest:String; out AFile:String):Boolean;
begin
  result:=AskUpdate(ALatest) and UpdateLatest(AOwner,ALatest,AFile);
end;

initialization
  TUpdater.InternalAlwaysUpdate:=False;
end.
