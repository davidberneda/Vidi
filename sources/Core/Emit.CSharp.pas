unit Emit.CSharp;

interface

uses
  Emit, Sys, AST;

type
  TCSharpEmit=class(TBaseEmit)
  private
  public
    function AsString(const ANode:TNode):String;
    class function ModuleName(const AModule: TNamedType): String; static;
  end;

implementation

uses
  SysUtils,
  //Map,
  Checker.AST,
  //Find.AST,
  Syntax, PascalSyntax,
  //Utils.AST,
  //Exceptions, StringArray,
  Module;

{ TCSharpEmit }

function TCSharpEmit.AsString(const ANode: TNode): String;
begin
  result:='';
end;

class function TCSharpEmit.ModuleName(const AModule: TNamedType): String;
begin
  result:=AModule.Name;
end;

end.
