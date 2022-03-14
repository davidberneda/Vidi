program Vidi_FMX;

{$R 'sys.res' 'sys.rc'}

uses
  System.StartUpCopy,
  FMX.Forms,
  UMain_FMX in 'UMain_FMX.pas' {VidiForm},
  FMX_Utils in 'FMX_Utils.pas',
  FMX_Gutter in 'FMX_Gutter.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}

  Application.Initialize;
  Application.CreateForm(TVidiForm, VidiForm);
  Application.Run;
end.
