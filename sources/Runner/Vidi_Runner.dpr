program Vidi_Runner;

{$weaklinkrtti on}

{$APPTYPE CONSOLE}

{$R *.res}

uses
  FastMM5,
  Runner in 'Runner.pas';

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}

  if ParamCount>0 then
  begin
    TCodeRunner.RunFile(ParamStr(1));

    WriteLn;
    ReadLn;
  end
  else
     WriteLn('Usage: Vidi_Runner FileName');
end.
