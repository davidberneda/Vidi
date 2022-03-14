unit Search;

interface

uses
  Classes,
  Sys,
  AST,
  Module;

type
  TFoundItem=class
    Module : TNamedType;
    Node : TNode;
    Line,
    Column : Integer;
  end;

  TFoundItems=Array of TFoundItem;

  TFileFoundItem=class
    Module,
    Path : String;
    Line, Column : Integer;
    Text : String;
  end;

  TFileFoundItems=Array of TFileFoundItem;

  TSearch=class
  public
    class function NodeName(const ANode:TNode):String; static;

    class function Search(S:String; const IsWord,IsCase:Boolean):TFoundItems; static;
  end;

  TSearchFiles=class
  public
    class procedure SearchIn(const AFile:String; S:String;
                const IsWord,IsCase:Boolean; var AResult:TFileFoundItems); static;

    class function Search(const AFiles:TStrings; S: String; const IsWord,
                          IsCase: Boolean): TFileFoundItems; overload; static;

    class function Search(const APath:String; S:String;
                const IsWord,IsCase:Boolean):TFileFoundItems; overload; static;
  end;

implementation

uses
  SysUtils,

  Utils.AST, IO, Map, Constants, Files_FromDirectory;

{ TSearch }

class function TSearch.NodeName(const ANode:TNode):String;
begin
  result:=TASTUtils.NodeName(ANode);

  {
  if ANode is TNamedType then
     result:=TNamedType(ANode).Name
  else
  if ANode is TVariable then
     result:=TVariable(ANode).Name
  else
     result:='';
  }
end;

class function TSearch.Search(S: String; const IsWord,
                              IsCase: Boolean): TFoundItems;

  // These functions here for speed
  function TextIs(const A:String):Boolean;
  begin
    if IsCase then
       result:=A=S
    else
       result:=SameText(A,S);
  end;

  function PartialTextIs(const A:String):Boolean;
  begin
    if IsCase then
       result:=Pos(S,A)>0
    else
       result:=Pos(S,UpperCase(A))>0;
  end;

var
  Module : TNamedType;
  ModuleIndex : Integer;

  procedure AddResult(const AFound:TFoundItem);
  var L : Integer;
  begin
    L:=Length(result);
    SetLength(result,L+1);
    result[L]:=AFound;
  end;

  procedure SearchIn(const N:TNode);

    procedure AddFound{(const S:String)};
    var tmp : TFoundItem;
        P : TNodePosition;
    begin
      tmp:=TFoundItem.Create;

      tmp.Module:=Module;
      tmp.Node:=N;

      if Modules.Positions[ModuleIndex].Find(N,P) then
      begin
        tmp.Line:=P.Position.Line;
        tmp.Column:=P.Position.Column;
      end;

      AddResult(tmp);
    end;

  var S : String;
      NN : TNode;
      tmp : TType;
  begin
    S:=NodeName(N);

    if S<>'' then
       if (IsWord and TextIs(S)) or
          ((not IsWord) and PartialTextIs(S)) then
             AddFound{(S)};

    if N is TType then
       tmp:=TType(N)
    else
    if N is TBlockStatement then
       tmp:=TBlockStatement(N).Block
    else
       tmp:=nil;

    if tmp<>nil then
    begin
      for NN in tmp.Items do
          SearchIn(NN);

      if tmp is TSpecializedType then
         for NN in TSpecializedType(tmp).Generics do
             SearchIn(NN);
    end;
  end;

var t : Integer;
begin
  if not IsCase then
     S:=UpperCase(S);

  result:=[];

  for t:=0 to High(Modules.Items) do
  begin
    Module:=Modules.Items[t];
    ModuleIndex:=t;

    SearchIn(Module);
  end;
end;

{ TSearchFiles }

class procedure TSearchFiles.SearchIn(const AFile:String; S:String;
                const IsWord,IsCase:Boolean; var AResult:TFileFoundItems);

  // These functions here for speed
  function TextIs(const A:String):Boolean;
  begin
    if IsCase then
       result:=A=S
    else
       result:=SameText(A,S);
  end;

  function PartialTextPosition(const A:String):Integer;
  begin
    if IsCase then
       result:=Pos(S,A)
    else
       result:=Pos(S,UpperCase(A));
  end;

  procedure AddResult(const AFound:TFileFoundItem);
  var L : Integer;
  begin
    L:=Length(AResult);
    SetLength(AResult,L+1);

    AResult[L]:=AFound;
  end;

var tmp : TStrings;
    t : Integer;
    tmpModule : String;
    tmpPos : Integer;
    tmpFound : TFileFoundItem;
    tmpPath : String;
begin
  tmp:=TStringList.Create;
  try
    tmp.LoadFromFile(AFile);

    tmpPath:=ExtractFilePath(AFile);
    tmpModule:=RemoveExtension(ExtractFileName(AFile));

    for t:=0 to tmp.Count-1 do
     //if (IsWord and TextIs(S)) or
     //   ((not IsWord) and PartialTextIs(S)) then

    if not IsWord then
    begin
      tmpPos:=PartialTextPosition(tmp[t]);

      if tmpPos>0 then
      begin
        tmpFound:=TFileFoundItem.Create;

        tmpFound.Module:=tmpModule;
        tmpFound.Path:=tmpPath;

        tmpFound.Line:=t+1;
        tmpFound.Column:=tmpPos;
        tmpFound.Text:=Copy(tmp[t],tmpPos-10,tmpPos+10);

        AddResult(tmpFound);
      end;
    end;

  finally
    tmp.Free;
  end;
end;

class function TSearchFiles.Search(const AFiles:TStrings; S: String; const IsWord,
                                   IsCase: Boolean): TFileFoundItems;
var tmpFile : String;
begin
  result:=nil;

  if S='' then
     Exit;

  if not IsCase then
     S:=UpperCase(S);

  for tmpFile in AFiles do
      SearchIn(tmpFile,S,IsWord,IsCase,result);
end;

class function TSearchFiles.Search(const APath: String; S: String; const IsWord,
                                   IsCase: Boolean): TFileFoundItems;
var tmp : TStrings;
begin
  result:=nil;

  if S='' then
     Exit;

  tmp:=GetAllFilesFromDirRecursive(APath,'',TVidiConstants.NoDot_Extension);
  try
    result:=Search(tmp,S,IsWord,IsCase);
  finally
    tmp.Free;
  end;
end;

end.
