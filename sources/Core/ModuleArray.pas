unit ModuleArray;

interface

uses
  AST;

type
  TModuleArray=Array of TNamedType;

  TModuleArrayHelper=record helper for TModuleArray
  public
    function Add(const AModule:TNamedType):Integer;
    procedure Delete(const AIndex:Integer);
    procedure FreeAll;
    function IndexOf(const AModule:TType):Integer; overload;
    function IndexOf(const AName:String):Integer; overload;
    procedure TryAdd(const AModule:TNamedType);
  end;

implementation

uses
  SysUtils;

{ TModuleArrayHelper }

procedure TModuleArrayHelper.Delete(const AIndex: Integer);
var t : Integer;
begin
  for t:=AIndex to High(Self)-1 do
      Self[t]:=Self[t+1];

  SetLength(Self,High(Self));
end;

procedure TModuleArrayHelper.FreeAll;
var tmp : TNamedType;
begin
  for tmp in Self do
      tmp.Free;

  Self:=nil;
end;

function TModuleArrayHelper.IndexOf(const AName: String): Integer;
var t : Integer;
begin
  for t:=Low(Self) to High(Self) do
      if SameText(Self[t].Name,AName) then
         Exit(t);

  result:=-1;
end;

function TModuleArrayHelper.IndexOf(const AModule: TType): Integer;
var t : Integer;
begin
  for t:=Low(Self) to High(Self) do
      if Self[t]=AModule then
         Exit(t);

  result:=-1;
end;

function TModuleArrayHelper.Add(const AModule:TNamedType):Integer;
begin
  result:=Length(Self);
  SetLength(Self,result+1);
  Self[result]:=AModule;
end;

procedure TModuleArrayHelper.TryAdd(const AModule:TNamedType);
begin
  if IndexOf(AModule)=-1 then
     Add(AModule);
end;

end.
