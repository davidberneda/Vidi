unit Compile;

interface

uses
  Classes;

type
  TCompile=record
  public
    class function Execute(const ACommandLine:String;
                           out ExitCode:Integer;
                           CallbackEvent:TNotifyEvent=nil;
                           Sender:TObject=nil):TStringList; static;
  end;

implementation

uses
  DosCommand, Windows;

class function TCompile.Execute(const ACommandLine:String;
                                out ExitCode:Integer;
                                CallbackEvent:TNotifyEvent=nil;
                                Sender:TObject=nil):TStringList;

  procedure WaitToFinish(const ACommand: TDosCommand);
  var i : Integer;
  begin
    i:=0;

    while not ACommand.Terminated do
    begin
      Sleep(100);
      Inc(i);

      if Assigned(CallbackEvent) then
         if (i mod 10)=0 then
            CallbackEvent(Sender);
    end;
  end;

var Dos : TDosCommand;
begin
  Result:=TStringList.Create;

  Dos:=TDosCommand.Create(nil);
  try
    Dos.CommandLine:=ACommandLine;
    Dos.Execute;
    WaitToFinish(Dos);
    ExitCode:=Dos.DosExitCode;
    Result.Assign(Dos.Lines);
  finally
    Dos.Free;
  end;
end;

end.
