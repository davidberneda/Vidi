unit Exceptions;

interface

uses
  SysUtils, Position;

type
  TStringArray=Array of String;

  TErrorStyle=(Error,Warning);

  TNodeError=class
    Position : TPosition;

    Length : Integer;

    Module : String;
    ModulePath : String;

    Text : String;
    Style : TErrorStyle;

    function AsText:String;
    function AsStrings:TStringArray;

    class function From(const AError,AModule,AModulePath:String;
                const APosition:TPosition;
                const ALength:Integer):TNodeError; static;

    Destructor Destroy; override;
  end;

  TNodeErrors=Array of TNodeError;

  TNodeErrorsHelper=record helper for TNodeErrors
  public
    procedure Add(const AErrors:TNodeErrors); overload;
    procedure Add(const AError:TNodeError); overload;
    procedure Clear;
    function Count:Integer;
  end;

  EBeeException=class(Exception)
  private
    OwnsError : Boolean;
  public
    Error : TNodeError;

    Constructor CreatePosition(const APosition:TPosition;
                               const ALength:Integer;
                               const AModule,AModulePath,AError:String);
    Constructor CreateError(const AError:TNodeError);

    Destructor Destroy; override;
  end;

procedure Raise_Exception(const AError: String); overload;
procedure Raise_Exception(const AError: TNodeError); overload;
procedure Raise_Exception(const APosition:TPosition;
                          const ALength:Integer;
                          const AModule,AModulePath,AError: String); overload;

implementation

procedure Raise_Exception(const AError: String); overload;
begin
  raise EBeeException.Create(AError);
end;

procedure Raise_Exception(const AError: TNodeError); overload;
begin
  raise EBeeException.CreateError(AError);
end;

procedure Raise_Exception(const APosition:TPosition;
                          const ALength:Integer;
                          const AModule,AModulePath,AError: String);
begin
  raise EBeeException.CreatePosition(APosition,ALength,AModule,AModulePath,AError);
end;

class function TNodeError.From(const AError,AModule,AModulePath:String;
                               const APosition:TPosition;
                               const ALength:Integer):TNodeError;
begin
  result:=TNodeError.Create;

  result.Text:=AError;
  result.Module:=AModule;
  result.ModulePath:=AModulePath;
  result.Position:=APosition;
  result.Length:=ALength;
end;

{ EBeeException }

constructor EBeeException.CreateError(const AError: TNodeError);
begin
  inherited Create(AError.AsText);
  Error:=AError;
  OwnsError:=True;
end;

constructor EBeeException.CreatePosition(const APosition: TPosition;
                            const ALength:Integer;
                            const AModule,AModulePath,AError: String);
begin
  inherited Create(AModule+' '+APosition.LineColumn+' '+AError);

  OwnsError:=True;

  Error:=TNodeError.From(AError,AModule,AModulePath,APosition,ALength);
end;

destructor EBeeException.Destroy;
begin
  if OwnsError then
     Error.Free;

  inherited;
end;

{ TNodeError }

function TNodeError.AsStrings: TStringArray;
begin
  {$IFDEF FPC}
  result:=nil;
  {$ENDIF}
  SetLength(result,4);

  result[0]:=Module;
  result[1]:=IntToStr(Position.Line);
  result[2]:=IntToStr(Position.Column);
  result[3]:=Text;
end;

function TNodeError.AsText: String;
begin
  result:='['+Module+'] '+Position.LineColumn+' '+Text;
end;

destructor TNodeError.Destroy;
begin
//
  inherited;
end;

{ TNodeErrors }

procedure TNodeErrorsHelper.Add(const AError: TNodeError);
var L : Integer;
begin
  L:=Count;
  SetLength(Self,L+1);
  Self[L]:=AError;
end;

procedure TNodeErrorsHelper.Add(const AErrors: TNodeErrors);
var E : TNodeError;
begin
  for E in AErrors do
      Add(E);
end;

procedure TNodeErrorsHelper.Clear;
var E : TNodeError;
begin
  for E in Self do
      E.Free;

  Self:=nil;
end;

function TNodeErrorsHelper.Count: Integer;
begin
  result:=Length(Self);
end;

end.
