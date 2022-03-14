program VidiCLI;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  Vidi_CLI;

type

  { Vidi }

  Vidi = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ Vidi }

procedure Vidi.DoRun;
begin
  { add your program here }
  Vidi_Main;

  // stop program loop
  Terminate;
end;

constructor Vidi.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor Vidi.Destroy;
begin
  inherited Destroy;
end;

var
  Application: Vidi;
begin
  Application := Vidi.Create(nil);
  Application.Title:='Vidi';
  Application.Run;
  Application.Free;
end.

