unit Internal;

interface

uses
  Sys, Syntax;

procedure InternalError(const S:String; const ANode:TNode); overload;
procedure InternalError(const S:TErrors; const ANode:TNode); overload;

implementation

uses
  Exceptions;

procedure InternalError(const S:String; const ANode:TNode); overload;
var tmp : String;
begin
  if ANode=nil then
     tmp:=''
  else
     tmp:=' '+ANode.ClassName;

  Raise_Exception('INTERNAL: '+TErrorTexts.Texts[_InternalError]+S+tmp);
end;

procedure InternalError(const S:TErrors; const ANode:TNode); overload;
begin
  InternalError(TErrorTexts.Texts[S],ANode);
end;

end.
