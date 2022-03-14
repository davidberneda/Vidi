program VidiCLI;

{$weaklinkrtti on}

{$APPTYPE CONSOLE}

{$R *.res}

uses
  FastMM5,
  Vidi_CLI in 'Vidi_CLI.pas';

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}

  Vidi_Main;
end.
