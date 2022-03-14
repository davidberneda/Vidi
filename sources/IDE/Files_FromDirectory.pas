unit Files_FromDirectory;

interface

uses
  Classes;

function GetAllFilesFromDirRecursive(const PathOnly:String;
                                     const ExcludeDir:String='';
                                     const Extension:String=''):TStringList;

Function GetFilesFromDir(const PathAndWildcards:String):TStringList;

implementation

uses
  SysUtils, IO;

function GetAllFilesFromDirRecursive(const PathOnly:String;
                                     const ExcludeDir:String='';
                                     const Extension:String=''):TStringList;

var tmpExt : String;

  procedure SearchPath(const APath:String);
  var f : TSearchRec;
  begin
    if FindFirst(APath+PathDelimiter+AllFiles,faAnyFile,f)=0 then
    begin
      Repeat
        if (f.Attr and faDirectory)=faDirectory then
        begin
          if (f.Name<>'.') and (f.Name<>'..') then
          begin
            if (ExcludeDir='') or (Pos(UpperCase(ExcludeDir),UpperCase(f.Name))=0) then
               SearchPath(APath+PathDelimiter+f.Name);
          end;

        end
        else
        if (tmpExt='') or SameText(ExtractFileExt(f.Name),tmpExt) then
          result.Add(APath+PathDelimiter+f.Name);

      Until FindNext(f)<>0;

      FindClose(f);
    end;
  end;

begin
  result:=TStringList.Create;

  if Extension='' then
     tmpExt:=''
  else
     tmpExt:='.'+UpperCase(Extension);

  SearchPath(PathOnly);
end;

Function GetFilesFromDir(const PathAndWildcards:String):TStringList;
var f:TSearchRec;
begin
  result:=TStringList.Create;

  if FindFirst(PathAndWildcards,faAnyFile,f)=0 then
  begin
    Repeat
      if (f.Attr and faDirectory)=faDirectory then
      else
        result.Add(f.Name);

    Until FindNext(f)<>0;

    FindClose(f);
  end;
end;

end.
