unit Compile.Pascal;

interface

uses
  Classes;

type
  TCompiler=class
  protected
    function CompilerOptions:String; virtual; abstract;
  public
    Verbose : Boolean;

    class var
      Sources : String;

    class function RunExecutable(const AFile:String; const ALog:TStrings):Integer; static;
  end;

  TPascalCompile=class(TCompiler)
  protected
    function CompilerOptions:String; override;
  public
    class var
       Lazarus : String;

    function Compile(const AUnitName,AText:String; const ALog:TStrings;
                     out AOutputPath,AFinalExe:String):Boolean;
  end;

  TDelphiCompile=class(TCompiler)
  protected
    function CompilerOptions:String; override;
  public
    class var
       RAD : String;

    class function CheckRAD:String; static;
    function Compile(const AUnitName,AText:String; const ALog:TStrings;
                     out AOutputPath,AFinalExe:String):Boolean;
  end;

  TCSharpCompile=class(TCompiler)
  protected
    function CompilerOptions:String; override;
  public
    class var
       NET : String;

    function Compile(const AUnitName,AText:String; const ALog:TStrings;
                     out AOutputPath,AFinalExe:String):Boolean;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  Windows,
  {$ENDIF}
  Registry, Compile,
  {$ENDIF}
  IO, SysUtils;

class function TCompiler.RunExecutable(const AFile:String; const ALog:TStrings):Integer;
{$IFDEF MSWINDOWS}
var tmpLines : TStrings;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  tmpLines:=TCompile.Execute(AFile,result);
  try
    ALog.AddStrings(tmpLines);
  finally
    tmpLines.Free;
  end;
  {$ELSE}
  result:=-1;
  {$ENDIF}
end;

function RunAndShow(const ACommand,AFinalExe:String; const ALog:TStrings):Boolean;
var tmpCode : Integer;
    tmpS : String;
begin
  tmpCode:=TCompiler.RunExecutable(ACommand,ALog);

  tmpS:='Compiler code: '+IntToStr(tmpCode);

  if tmpCode=0 then
     tmpS:=tmpS+' Executable size: '+IntToStr(FileSize(AFinalExe));

  ALog.Add(tmpS);

  result:=tmpCode=0;
end;

{ TPascalCompile }

const
  ConsoleApp = 'C';

  {$IFNDEF FPC}
  GUIApp     = 'G';
  {$ENDIF}

function TPascalCompile.CompilerOptions:String;
begin
  result:='-B -v0 -Mdelphi -W'+ConsoleApp+
//          ' -Fu%ROOT%\Core -Fu%ROOT%\VCL -Fu%ROOT%\CLI -Fu%ROOT%\Languages'+
          ' -Fu%ROOT%\Pascal'+
          ' -Fu'+Lazarus+'\lazarus\lcl\units\i386-win32\win32'+
          ' -Fu'+Lazarus+'\lazarus\lcl\units\i386-win32'+
          ' -Fu'+Lazarus+'\lazarus\components\lazutils\lib\i386-win32'+
          ' -Fu'+Lazarus+'\config_lazarus\onlinepackagemanager\packages\richmemo\lib\i386-win32';
end;

function TPascalCompile.Compile(const AUnitName,AText: String;
                                      const ALog: TStrings;
                                      out AOutputPath,AFinalExe:String):Boolean;
var tmp,
    tmpFile : String;

const
  tmpProgram='temp_program';
  CRLF=#13#10;

var FreePascalPath : String;
begin
  AOutputPath:=GetTempPath+'Vidi_output';

  ForceDirectories(AOutputPath);

  tmpFile:=AOutputPath+PathDelimiter+AUnitName+'.pas';
  WriteFile(tmpFile,AText);

  tmpFile:=AOutputPath+PathDelimiter+tmpProgram+'.lpr';
  WriteFile(tmpFile,'program temp_program; '+CRLF+

     '{$mode objfpc}{$H+}'+CRLF+
     'uses {$IFDEF UNIX}{$IFDEF UseCThreads} cthreads, {$ENDIF}{$ENDIF}'+CRLF+
     '  Classes, SysUtils, CustApp, '+AUnitName+';'+CRLF+
     'begin'+CRLF+
     'end.');

  FreePascalPath:=TPascalCompile.Lazarus+'\fpc';

  tmp:=FreePascalPath+'\bin\i386-win32\fpc.exe '+tmpFile+' '+
       StringReplace(CompilerOptions,'%ROOT%',Sources,[rfReplaceAll])+
       ' -FE'+AOutputPath;

  AFinalExe:=AOutputPath+PathDelimiter+tmpProgram+'.exe';
  DeleteFile(AFinalExe);

  DeleteFile(AOutputPath+PathDelimiter+AUnitName+'.dcu');

  if Verbose then
  begin
    ALog.Add(tmp);
    ALog.Add('');
  end;

  result:=RunAndShow(tmp,AFinalExe,ALog);
end;

{ TDelphiCompile }

function FindRadHome:String;
{$IFDEF MSWINDOWS}
var R : TRegistry;
    t : Integer;
begin
  result:='';

  //  'c:\Program Files (x86)\Embarcadero\Studio\21.0'
  R:=TRegistry.Create(KEY_READ);
  try
    for t:=22 downto 8 do
    begin
      if R.OpenKey('Software\Embarcadero\BDS\'+IntToStr(t)+'.0',False) then
      begin
        if R.ValueExists('RootDir') then
           result:=R.ReadString('RootDir');

        R.CloseKey;

        if result<>'' then
           Exit;
      end;
    end;
  finally
    R.Free;
  end;
{$ELSE}
begin
  result:='';
{$ENDIF}
end;

class function TDelphiCompile.CheckRAD:String;
begin
  if TDelphiCompile.RAD='' then
     TDelphiCompile.RAD:=FindRADHome;

  result:=TDelphiCompile.RAD;
end;

function TDelphiCompile.CompilerOptions:String;
begin
  result:='-B -$D+ -C'+ConsoleApp+
//          ' -U%ROOT%\Core;%ROOT%\VCL;%ROOT%\CLI;%ROOT%\Languages'+
          ' -U%ROOT%\Pascal'+
          ' -NSSystem';
end;

function TDelphiCompile.Compile(const AUnitName,AText: String;
                                const ALog: TStrings;
                                out AOutputPath,AFinalExe:String):Boolean;
var tmp,
    tmpFile : String;

const
  tmpProgram='temp_program';
  CRLF=#13#10;

var DCCPath : String;
begin
  AOutputPath:=GetTempPath+'Vidi_output';

  ForceDirectories(AOutputPath);

  tmpFile:=CombineFile(AOutputPath,AUnitName+'.pas');

  WriteFile(tmpFile,AText);

  tmpFile:=CombineFile(AOutputPath,tmpProgram+'.dpr');

  WriteFile(tmpFile,'program temp_program; '+CRLF+

     'uses Classes, SysUtils, '+AUnitName+';'+CRLF+
     'begin'+CRLF+
     ' {$IFOPT D+}'+CRLF+
     ' ReportMemoryLeaksOnShutdown:=True;'+CRLF+
     ' {$ENDIF}'+CRLF+
     'end.');

  TDelphiCompile.CheckRAD;

  DCCPath:=TDelphiCompile.RAD+'\bin\dcc32.exe';

  tmp:=DCCPath+' '+tmpFile+' '+
       StringReplace(CompilerOptions,'%ROOT%',Sources,[rfReplaceAll])+
       ' -E'+AOutputPath+' -NU'+AOutputPath;

  AFinalExe:=CombineFile(AOutputPath,tmpProgram+'.exe');
  DeleteFile(AFinalExe);

  DeleteFile(CombineFile(AOutputPath,AUnitName+'.dcu'));

  if Verbose then
  begin
    ALog.Add(tmp);
    ALog.Add('');
  end;

  result:=RunAndShow(tmp,AFinalExe,ALog);
end;

{ TCSharpCompile }

function TCSharpCompile.Compile(const AUnitName, AText: String;
  const ALog: TStrings; out AOutputPath,AFinalExe: String): Boolean;
const
  tmpProgram='temp_program';
  CRLF=#13#10;

var tmp,
    tmpFile,
    CSPath : String;
begin
  AOutputPath:=GetTempPath+'Vidi_output';

  ForceDirectories(AOutputPath);

  tmpFile:=AOutputPath+PathDelimiter+AUnitName+'.cs';
  WriteFile(tmpFile,AText);

  tmpFile:=AOutputPath+PathDelimiter+tmpProgram+'.cs';

  WriteFile(tmpFile,'using System, '+AUnitName+';'+CRLF+
    'namespace '+AUnitName+CRLF+
    '{'+CRLF+
    '  class Program'+CRLF+
    '  {'+CRLF+
    '    static void Main(string[] args)'+CRLF+
    '    {'+CRLF+
    '    }'+CRLF+
    '  }'+CRLF+
    '}');

  CSPath:=TCSharpCompile.NET+'\bin\cs.exe';

  tmp:=CSPath+' '+tmpFile+' '+
       StringReplace(CompilerOptions,'%ROOT%',Sources,[rfReplaceAll])+
       ' -E'+AOutputPath+' -NU'+AOutputPath;

  AFinalExe:=AOutputPath+PathDelimiter+tmpProgram+'.exe';
  DeleteFile(AFinalExe);

  DeleteFile(AOutputPath+PathDelimiter+AUnitName+'.dcu');

  if Verbose then
  begin
    ALog.Add(tmp);
    ALog.Add('');
  end;

  result:=RunAndShow(tmp,AFinalExe,ALog);
end;

function TCSharpCompile.CompilerOptions: String;
begin
  result:='';
end;

initialization
  TPascalCompile.Lazarus:='c:\lazarus';
end.
