unit Magic;

interface

uses
  Sys, AST;

type
  TMagic=class
  class var
    MagicTrue,
    MagicFalse : TBoolean;

    MagicNameOf,
    MagicTypeOf : TRoutine;

    class procedure Clear; static;
    class procedure FreeAll; static;
  end;

implementation

{$IFDEF INTERNAL}
uses
  Exceptions;
{$ENDIF}

var
  _Magic : TNode;

{$IFDEF INTERNAL}
type
  TMagicBoolean=class(TBoolean) // only for debugging
  private
    class var Cleaning : Boolean;
  public
    Destructor Destroy; override;
  end;

Destructor TMagicBoolean.Destroy;
begin
  if not Cleaning then
     Raise_Exception('Cannot destroy Magic');

  inherited; // <-- break here to discover who is wrongly destroying this magic instance !
end;
{$ENDIF}

procedure InitMagic;
begin
  _Magic:=TNode.Create;

  {$IFDEF INTERNAL}
  TMagicBoolean.Cleaning:=False;
  {$ENDIF}

  TMagic.MagicTrue:={$IFDEF INTERNAL}TMagicBoolean{$ELSE}TBoolean{$ENDIF}.Create(True);
  TMagic.MagicTrue.Owner:=_Magic;

  TMagic.MagicFalse:={$IFDEF INTERNAL}TMagicBoolean{$ELSE}TBoolean{$ENDIF}.Create(False);
  TMagic.MagicFalse.Owner:=_Magic;

  TMagic.MagicTypeOf:=TRoutine.Create;
  TMagic.MagicTypeOf.Name:='__TypeOf';
  TMagic.MagicTypeOf.Clauses.Shared:=True;

  TMagic.MagicNameOf:=TRoutine.Create;
  TMagic.MagicNameOf.Name:='__NameOf';
  TMagic.MagicNameOf.Clauses.Shared:=True;
end;

class procedure TMagic.FreeAll;
begin
  if _Magic<>nil then
  begin
    TMagic.MagicNameOf.Free;
    TMagic.MagicTypeOf.Free;

    {$IFDEF INTERNAL}
    TMagicBoolean.Cleaning:=True;
    {$ENDIF}

    TMagic.MagicFalse.Free;
    TMagic.MagicTrue.Free;

    _Magic.Free;
    _Magic:=nil;

    {$IFDEF INTERNAL}
    TMagicBoolean.Cleaning:=False;
    {$ENDIF}
  end;
end;

{ TMagic }

class procedure TMagic.Clear;
begin
  MagicTypeOf.Parameters.Free(MagicTypeOf);
  MagicTypeOf.Output:=nil;

  MagicNameOf.Parameters.Free(MagicNameOf);
  MagicNameOf.Output:=nil;
end;

initialization
  InitMagic;
finalization
  TMagic.FreeAll;
end.
