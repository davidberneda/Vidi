unit FPC_StopWatch;

interface

type
  TStopWatch=record
  public
    T : Int64;
    function ElapsedMilliseconds:Int64;
    class function StartNew:TStopWatch; static;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows
  {$ELSE}
  SysUtils
  {$ENDIF}
  ;

function TStopWatch.ElapsedMilliseconds:Int64;
begin
  result:=GetTickCount64-T;
end;

class function TStopWatch.StartNew:TStopWatch;
begin
  result.T:=GetTickCount64;
end;

end.
