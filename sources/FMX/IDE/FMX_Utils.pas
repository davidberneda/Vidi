unit FMX_Utils;

interface

type
  TYesNoProc=reference to procedure(const Value:Boolean);

  TFMXUtils=record
  public
    class function GetTextResource(const AName:String):String; static;
    class function OnlyName(const AFile:String):String; static;
    class procedure ShareText(const S:String); static;
    class procedure YesNo(const S:String; const AProc:TYesNoProc); static;
  end;

implementation

uses
  SysUtils, Classes, Types, UITypes, IOUtils,
  FMX.Platform, FMX.MediaLibrary, FMX.DialogService;

class function TFMXUtils.OnlyName(const AFile: String): String;
begin
  result:=TPath.GetFileNameWithoutExtension(AFile);
end;

class procedure TFMXUtils.ShareText(const S:String);
var tmp : IFMXShareSheetActionsService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXShareSheetActionsService, tmp) then
     tmp.Share(nil, S, nil)
  else
     raise Exception.Create('Cannot obtain share service.');
end;

class procedure TFMXUtils.YesNo(const S: String; const AProc:TYesNoProc);
begin
  TDialogService.MessageDialog(S,TMsgDlgType.mtConfirmation,[
      TMsgDlgBtn.mbYes,TMsgDlgBtn.mbNo],TMsgDlgBtn.mbYes,0,
    procedure(const AResult: TModalResult)
    begin
      AProc(AResult=mrYes);
    end
  );
end;

class function TFMXUtils.GetTextResource(const AName:String):String;
var Stream: TCustomMemoryStream;
    tmp : TBytes;
begin
  result:='';

  if FindResource(HInstance, PChar(AName), RT_RCDATA)<>0 then
  begin
    Stream := TResourceStream.Create(HInstance, AName, RT_RCDATA);
    try
      SetLength(tmp,Stream.Size);

      if Stream.Read(tmp,Stream.Size)=Stream.Size then
         result:=TEncoding.ANSI.GetString(tmp);
    finally
      Stream.Free;
    end;
  end;
end;

end.
