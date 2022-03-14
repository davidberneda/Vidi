unit Utils_VCL;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls,
  {$IFNDEF FPC}
  UITypes,
  {$ENDIF}
  Sys;

procedure AddForm(const AForm:TForm; const AParent:TWinControl;
                  const AAlign:TAlign=TAlign.alClient);

procedure AddTitlePanel(const AParent:TWinControl; const ACaption:String);

procedure RemoveControls(const AParent:TWinControl);

function CreateTree(const AOwner:TComponent; const AParent:TWinControl):TTreeView;

procedure LoadPNG(const AImage:TImage; const AName:String);

function TextWidth(const AControl:TWinControl; const AText:String):Integer;

type
  TOnGotoNode=procedure(Sender:TObject; const ANode:TNode) of object;

  TGoto=record
  public
    class procedure ToFile(const AFile:String); static;
    class procedure Folder(const AFolder:String); static;
    class procedure URL(const AURL:String); static;
  end;

function GetURL(const URL:String):String;

type
  TProgressEvent=procedure(Sender:TObject; const ACurrent,ATotal:Int64; var Abort:Boolean) of object;

function DownloadFile(const URL,AFile:String; const OnProgress:TProgressEvent=nil):Boolean;

function ExecuteFile(const AFile,AStartFolder:String):Integer;

function YesNoCancel(const AMessage:String):TModalResult;
function YesNo(const AMessage:String):Boolean;

implementation

uses
  {$IFDEF MSWINDOWS}
  ShellAPI,
  {$ENDIF}

  Dialogs,

  {$IFDEF FPC}
  Graphics, LCLIntf, fpHttpClient, OpenSSLSockets
  {$ELSE}
  System.Net.HttpClient,
  System.Net.HttpClientComponent, IOUtils, pngimage
  {$ENDIF}
  ;

procedure AddForm(const AForm:TForm; const AParent:TWinControl;
                  const AAlign:TAlign=TAlign.alClient);
begin
  AForm.Parent:=AParent;

  AForm.Align:=AAlign;
  AForm.Position:=poDesigned;
  AForm.BorderStyle:=TFormBorderStyle.bsNone;
  AForm.BorderIcons:=[];
end;

procedure AddTitlePanel(const AParent:TWinControl; const ACaption:String);
var tmp : TPanel;
    tmpL : TLabel;
begin
  tmp:=TPanel.Create(AParent.Owner);
  tmp.Align:=TAlign.alTop;
  tmp.Caption:='';
  tmp.Height:=24;

  tmp.BevelInner:=TBevelCut.bvNone;
  tmp.BevelOuter:=TBevelCut.bvNone;

  tmpL:=TLabel.Create(AParent.Owner);
  tmpL.Caption:=ACaption;
  tmpL.Font.Style:=[TFontStyle.fsBold];
  tmpL.Left:=6;
  tmpL.Top:=6;
  tmpL.Parent:=tmp;

  tmp.Parent:=AParent;
end;

procedure RemoveControls(const AParent:TWinControl);
begin
  while AParent.ControlCount>0 do
        AParent.Controls[0].Parent:=nil;
end;

function CreateTree(const AOwner:TComponent; const AParent:TWinControl):TTreeView;
begin
  result:=TTreeView.Create(AOwner);

  result.Align:=TAlign.alClient;
  result.ReadOnly:=True; // ?? enable renaming ?
  result.HideSelection:=False;
  result.Parent:=AParent;

  result.Items.Clear;
end;

procedure LoadPNG(const AImage:TImage; const AName:String);
var S : TResourceStream;
    png : {$IFDEF FPC}TPortableNetworkGraphic{$ELSE}TPngImage{$ENDIF};
begin
  S:=TResourceStream.Create(HINSTANCE,AName,RT_RCDATA);
  try
    png:= {$IFDEF FPC}TPortableNetworkGraphic{$ELSE}TPngImage{$ENDIF}.Create;
    try
      {$IFDEF FPC}
      Png.LoadFromStream(S);
      AImage.Picture.Assign(png);
      {$ELSE}
      png.LoadFromResourceName(HInstance,AName);
      AImage.Picture.Graphic:=png;
      {$ENDIF}
    finally
      png.Free;
    end;
  finally
    S.Free;
  end;
end;

function TextWidth(const AControl:TWinControl; const AText:String):Integer;
var c : TControlCanvas;
begin
  C:=TControlCanvas.Create;
  try
    C.Control:=AControl;
    result:=C.TextWidth(AText);
  finally
    C.Free;
  end;
end;

class procedure TGoto.Folder(const AFolder:String);
begin
  {$IFDEF MSWINDOWS}
  ShellExecute(0,'open',PWideChar(AFolder),nil,nil,SW_SHOW);
  {$ELSE}
  OpenDocument(AFolder);
  {$ENDIF}
end;

class procedure TGoto.ToFile(const AFile:String);
begin
  {$IFDEF MSWINDOWS}
  ShellExecute(0, nil, 'explorer.exe', PWideChar('/select,"'+AFile+'"'), nil,SW_SHOWNORMAL)

//  ShellExecute(Application.Handle, 'open', 'explorer.exe', PChar('/select,"'+AFile+'"'), nil, SW_NORMAL);
  {$ELSE}
  OpenDocument(AFile {ExtractFilePath(AFile)} );
  {$ENDIF}
end;

class procedure TGoto.URL(const AURL:String);
const HTTPS='https://';
begin
  {$IFDEF FPC}
  OpenURL(HTTPS+AURL);
  {$ELSE}
  ShellExecute(0,'open',PWideChar(HTTPS+AURL),nil,nil,SW_SHOW);
  {$ENDIF}
end;

function GetURL(const URL:String):String;
{$IFDEF FPC}
var Http : TFPHttpClient;
begin
  Http:=TfpHttpClient.Create(nil);
  try
    Http.AllowRedirect:=True;
    result:=Http.Get(URL);
  finally
    Http.Free;
  end;
end;
{$ELSE}
var Http : TNetHttpClient;  // System.Net
begin
  Http:=TNetHTTPClient.Create(nil);
  try
    result:=Http.Get(URL).ContentAsString;
  finally
    Http.Free;
  end;
end;
{$ENDIF}

type
  TDownloader=class
    {$IFDEF FPC}
    procedure ReceivedData(Sender : TObject; Const ContentLength, CurrentPos : Int64);
    {$ELSE}
    procedure ReceivedData(const Sender: TObject; AContentLength,
                           AReadCount: Int64; var Abort: Boolean);
    procedure RequestCompleted(const Sender: TObject;
                           const AResponse: IHTTPResponse);
    {$ENDIF}
  public
    OnProgress: TProgressEvent;
  end;

{$IFDEF FPC}
procedure TDownloader.ReceivedData(Sender : TObject; Const ContentLength, CurrentPos : Int64);
var Abort : Boolean;
begin
  if Assigned(OnProgress) then
  begin
    Abort:=False;
    OnProgress(Self,CurrentPos,ContentLength,Abort);
  end;
end;

{$ELSE}
procedure TDownloader.ReceivedData(const Sender: TObject; AContentLength,
                           AReadCount: Int64; var Abort: Boolean);
begin
//  if FTask<>nil then
     if Assigned(OnProgress) then
        OnProgress(Sender,AReadCount,AContentLength,Abort);
end;

procedure TDownloader.RequestCompleted(const Sender: TObject;
  const AResponse: IHTTPResponse);
var Abort : Boolean;
begin
//  if FTask<>nil then
     if Assigned(OnProgress) then
     begin
       Abort:=False;
       OnProgress(Sender,0,0,Abort);
     end;
end;
{$ENDIF}

// WARNING !!! NOT FOR GENERAL DOWNLOAD USE !!! ERRORS ARE NOT MANAGED
function DownloadFile(const URL,AFile:String; const OnProgress:TProgressEvent=nil):Boolean;
var tmp : TStream;
    {$IFDEF FPC}
    Http : TFPHttpClient;
    {$ELSE}
    Http : TNetHttpClient;  // System.Net
    {$ENDIF}

    tmpDown : TDownloader;
begin
  tmp:=TFileStream.Create(AFile,fmCreate);
  try
    tmpDown:=TDownloader.Create;
    try
      tmpDown.OnProgress:=OnProgress;

      {$IFDEF FPC}
      Http:=TfpHttpClient.Create(nil);
      try
        Http.AllowRedirect:=True;
        Http.OnDataReceived:=tmpDown.ReceivedData;

        Http.Get(URL,tmp);
      finally
        Http.Free;
      end;

      {$ELSE}
      Http:=TNetHTTPClient.Create(nil);
      try
        Http.OnReceiveData:=tmpDown.ReceivedData;
        Http.OnRequestCompleted:=tmpDown.RequestCompleted;

        Http.Get(URL,tmp);
      finally
        Http.Free;
      end;

      {$ENDIF}

      result:=tmp.Size>1000; // Smaller files might contain "Not found" errors etc
    finally
      tmpDown.Free;
    end;
  finally
    tmp.Free;
  end;
end;

function ExecuteFile(const AFile,AStartFolder:String):Integer;
{$IFDEF MSWINDOWS}
var StartupInfo : TStartupInfo;
    Working : PChar;
    ProcessInfo: TProcessInformation;
begin
  ZeroMemory(@StartupInfo, SizeOf(StartupInfo));

  StartupInfo.cb := SizeOf(StartupInfo);

  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_SHOWMAXIMIZED;

  if AStartFolder='' then
     Working:=nil
  else
     Working:=PChar(AStartFolder);

  if CreateProcess(nil, PChar(AFile), nil, nil, False, 0, nil, Working, StartupInfo, ProcessInfo) then
     result:=0
  else
     result:=GetLastError;
end;
{$ELSE}
begin
  result:=-1; // TODO Linux etc
end;
{$ENDIF}

function YesNoCancel(const AMessage:String):TModalResult;
begin
  result:=MessageDlg(AMessage,mtConfirmation,[mbYes,mbNo,mbCancel],0);
end;

function YesNo(const AMessage:String):Boolean;
begin
  result:=MessageDlg(AMessage,mtConfirmation,[mbYes,mbNo],0)=mrYes;
end;

end.
