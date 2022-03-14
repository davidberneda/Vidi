unit IO;

interface

uses
  {$IFNDEF FPC}
  IOUtils,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$ENDIF}

  Classes, SysUtils;

const
  PathDelimiter={$IFDEF LINUX}'/'{$ELSE}'\'{$ENDIF};
  AllFiles={$IFDEF LINUX}'*'{$ELSE}'*.*'{$ENDIF};

function ConvertPathDelimiter(const S:String):String;

function CombineFile(const APath,AFile:String):String;

function CorrectPath(const APath:String):String;

function ExtractFileName(const AFileName:String):String;

function VidiHomePath:String;

function ReadFile(const AFileName:String):String; overload;
function ReadFile(const APath,AFileName:String):String; overload; inline;

procedure WriteFile(const AFileName,AText:String; const AEncoding:TEncoding); overload;
procedure WriteFile(const AFileName,AText:String); overload;

procedure DoDeleteFile(const FileName:String);

procedure RemoveReadOnly(const FileName:String);

function DoFileExists(const AFileName:String):Boolean; overload;
function DoFileExists(const APath,AFileName:String):Boolean; overload;
function FileSize(const AFileName:String):Int64;
function FileLastModified(const AFileName:String):TDateTime;
function GetTempPath:String;

function SamePath(const A,B:String):Boolean;

function RemoveExtension(const S:String):String;
function TryAddExtension(const AFileName,AExtension:String):String;

function LastErrorMessage:String;

implementation

function ConvertPathDelimiter(const S:String):String;
const
  Bad={$IFDEF LINUX}'\'{$ELSE}'/'{$ENDIF};
begin
  result:=StringReplace(S,Bad,PathDelimiter,[rfReplaceAll]);
end;

function CombineFile(const APath,AFile:String):String;
begin
  {$IFDEF FPC}
  result:=APath+PathDelimiter+AFile;
  {$ELSE}
  result:=TPath.Combine(APath,AFile);
  {$ENDIF}
end;

function VidiHomePath:String;
begin
  {$IFDEF FPC}
  result:=GetUserDir;
  {$ELSE}
  result:=TPath.GetHomePath;
  {$ENDIF}
end;

function CorrectPath(const APath:String):String;
begin
  result:=APath;

  while Copy(Result,Length(result),1)=PathDelimiter do
     Delete(result,Length(result),1);
end;

function ExtractFileName(const AFileName:String):String;
begin
  result:=SysUtils.ExtractFileName(AFileName);
end;

{$IFDEF FPC}
{
function ReadFile(const AFileName:String):String;
var Stream : TFileStream;
    tmpReader : TReadBufStream;
begin
  Stream:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyNone);
  try
    Stream.Position:=0;

    tmpReader:=TReadBufStream.Create(Stream);
    try
      tmpReader.Position:=0;

      SetLength(result,Stream.Size);
      tmpReader.ReadBuffer(Pointer(result)^, Stream.Size);
    finally
      tmpReader.Ffree;
    end;
  finally
    Stream.Free;
  end;
end;
}
{$ENDIF}

function ReadFile(const AFileName:String):String;
var S : TStrings;
begin
//  result:=TFile.ReadAllText(AFileName,TEncoding.UTF8);

  S:=TStringList.Create;
  try
    S.LoadFromFile(AFileName);
    result:=S.Text;
  finally
    S.Free;
  end;
end;

procedure WriteFile(const AFileName,AText:String; const AEncoding:TEncoding);
var S : TStrings;
begin
  S:=TStringList.Create;
  try
    S.DefaultEncoding:=AEncoding;
    S.Text:=AText;
    S.SaveToFile(AFileName);
  finally
    S.Free;
  end;
end;

procedure WriteFile(const AFileName,AText:String);
begin
  WriteFile(AFileName,AText,TEncoding.ANSI);
end;

procedure RemoveReadOnly(const FileName:String);
begin
  // FileIsReadOnly is buggy. Returns True if FileName does not exists !!!
  if FileExists(FileName) and FileIsReadOnly(FileName) then
     FileSetAttr(FileName,0);
end;

Procedure DoDeleteFile(const FileName:String);
var tmpError : String;
begin
  if not DeleteFile(FileName) then
  if FileExists(FileName) then
  begin
    if FileIsReadOnly(FileName) then
       RemoveReadOnly(FileName);

    if not DeleteFile(FileName) then
    begin
      tmpError:=LastErrorMessage;

      // Do not raise any exception if we are not in Administrator UAC mode:

      // PENDING GOOD SOLUTION:
      // ShellExecute "runas" this very same exe to get admin elevated privileges,
      // with a special parameter to try deleting the file again.

      Raise Exception.Create('Error deleting file: '+FileName+#13+tmpError);
    end;
  end;
end;

Function SystemLastError(const Error:Integer):String;
begin
  result:=' Error Code: '+IntToStr(Error)+' Description: '+SysErrorMessage(Error);
end;

function LastErrorMessage:String;
begin
  result:=SystemLastError({$IFDEF FPC}GetLastOSError{$ELSE}GetLastError{$ENDIF});
end;

function ReadFile(const APath,AFileName:String):String;
begin
  if APath='' then
     result:=ReadFile(AFileName)
  else
     result:=ReadFile(APath+PathDelimiter+AFileName);
end;

function DoFileExists(const AFileName:String):Boolean;
begin
  result:=FileExists(AFileName);
end;

function DoFileExists(const APath,AFileName:String):Boolean;
begin
  if APath='' then
     result:=DoFileExists(AFileName)
  else
     result:=DoFileExists(APath+PathDelimiter+AFileName);
end;

// From TPath
function GetTempPath: string;
{$IFDEF FPC}
begin
  result:=GetTempDir;
{$ELSE}
{$IFDEF MSWINDOWS}
var Len: Integer;
begin
  SetLastError(ERROR_SUCCESS);
  SetLength(Result, MAX_PATH);
  Len := Windows.GetTempPath(MAX_PATH, PChar(Result));
  if Len <> 0 then
  begin
    Len := GetLongPathName(PChar(Result), nil, 0);
    GetLongPathName(PChar(Result), PChar(Result), Len);
    SetLength(Result, Len - 1);
  end
  else
    Result := '';
{$ELSE}
begin
  result:='Temp'; // TODO
{$ENDIF}
{$ENDIF}
end;

function FileSize(const AFileName:String):Int64;
var f : TFileStream;
begin
  f:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyNone);
  try
    result:=f.Size;
  finally
    f.Free;
  end;
end;

function FileLastModified(const AFileName:String):TDateTime;
begin
 if not FileAge(AFileName,result) then
    result:=0;
end;

function TryAddExtension(const AFileName,AExtension:String):String;
begin
  if ExtractFileExt(AFileName)=AExtension then
     result:=AFileName
  else
     result:=AFileName+AExtension;
end;

function RemoveExtension(const S:String):String;
var ext: String;
begin
  result:=S;

  ext:=ExtractFileExt(S);

  if ext<>'' then
     Delete(result,Length(result)-Length(ext)+1,Length(ext));
end;

function SamePath(const A,B:String):Boolean;
begin
  {$IFDEF FPC}
  result:=SameText(ExpandFileName(A),ExpandFileName(B));
  {$ELSE}
  result:=SameText(TPath.GetFullPath(A),TPath.GetFullPath(B));
  {$ENDIF}
end;

end.
