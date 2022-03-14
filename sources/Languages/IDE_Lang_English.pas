unit IDE_Lang_English;

interface

uses
  Forms;

const
  CRLF=#13#10;
  Vidi_ID='ID';

type
  TVidi_Languages=(English,Catalan,Spanish);

  TVidi_Constants=record
    About,Name,_Type,Value,
    Module,Line,Column,Text,_Class,
    Fields,Types,Breaks,Trace,Watch,
    Changed,JumpToLine,Vidi_Files,

    DoYouWantToSave,
    SelectVidiSystemFolder,
    SureOverwrite,

    ShowAncestorItems,
    References,

    _Uses,UsedBy,Callers,Folder,

    StopRunAndClose,
    Routine,Count,Elapsed,Item,Index,Owner,
    Children,
    FirstVariable,Reference,
    Path,Found,

    NoNewVersion,
    Updating,
    DownloadError,
    Installing,
    Downloading,
    SureToUpdate

    :String;
  end;

  TVidi_Reserved=record
    _ancestor,
    _and,
    _break,
    _catch,
    _continue,
    _else,
    _False,
    _final,
    _finally,
    _for,
    _hidden,
    _if,
    _in,
    _indexed,
    _is,
    _not,
    _or,
    _out,
    _repeat,
    _return,
    _self,
    _shared,
    _to,
    _True,
    _try,
    _until,
    _when,
    _while,
    _xor,
    _with : String;
  end;

var
  Vidi_Language:TVidi_Languages=TVidi_Languages.English;
  Vidi_Lang:TVidi_Constants;
  Vidi_Reserved:TVidi_Reserved;

procedure Change(const AForm:TForm);
function Translate(const S:String):String;

implementation

uses
  {$IFDEF FPC}
  LConvEncoding,
  {$ENDIF}
  Unit_TheBee;

function Translate(const S:String):String;
begin
  {$IFDEF FPC}
  result:=CP1252ToUtf8(S);
  {$ELSE}
  result:=S;
  {$ENDIF}
end;

procedure InitConstants;
begin
  with Vidi_Lang do
  begin
    Name    := 'Name';
    _Type   := 'Type';
    Value   := 'Value';
    About   := 'About Vidi';
    Module  := 'Module';
    Line    := 'Line';
    Column  := 'Column';
    Text    := 'Text';
    _Class  := 'Class';
    Fields  := 'Fields';
    Types   := 'Types';
    Breaks  := 'Breaks';
    Trace   := 'Trace';
    Watch   := 'Watch';
    Changed := 'Changed';
 JumpToLine := 'Jump to line';
 Vidi_Files := 'Vidi files';

  DoYouWantToSave := 'Module: [%s] has been modified.'+CRLF+'Do you want to save it?';
  SelectVidiSystemFolder := 'Select folder with Vidi system sources';
  SureOverwrite   := 'Sure to overwrite file:'+CRLF+'[%s] ?';

    ShowAncestorItems := 'Show Ancestor Items';
    References  := 'References';

    _Uses  := 'Uses';
    UsedBy := 'Used By';

    Callers:= 'Callers';
    Folder := 'Folder';

    StopRunAndClose:='Stop running and close?';

    Routine:= 'Routine';
    Count  := 'Count';
    Elapsed:= 'Elapsed';
    Item   := 'Item';
    Index  := 'Index';
    Owner  := 'Owner';
    Children:='Children';
    FirstVariable:='First Variable';
    Reference := 'Reference';

    Path  := 'Path';
    Found := 'Found';

    // Auto-Updater
    NoNewVersion  := 'You are using already the latest version';
    Updating      := 'Updating to version: %s';
    DownloadError := 'Error downloading file: ';
    Installing    := 'Installing';
    Downloading   := 'Downloading';
    SureToUpdate  := 'Do you want to update?'#13#10#13#10'Current: %s'#13#10'New: %s';
  end;
end;

procedure Change(const AForm:TForm);
begin
  InitConstants;

  with TFormVidi(AForm) do
  begin
    Changed_Text:=       Vidi_Lang.Changed;

    TabAST.Caption:=     'AST';
    TabModules.Caption:= 'Modules';
    TabTypes.Caption:=   'Types';
    TabErrors.Caption:=  'Errors';
    TabPrompt.Caption:=  'Prompt';
    TabSearch.Caption:=  'Search';
    TabConsole.Caption:= 'Console';
    TabDebugger.Caption:='Debugger';
    TabFileExplorer.Caption:='Explorer';
    TabProfiler.Caption:='Profiler';
    TabStats.Caption:=   'Statistics';
    TabRecent.Caption:=  'Recent';

    File1.Caption:=      '&File';
     New1.Caption:=      '&New...';
     Open1.Caption:=     '&Open...';
     Reopen1.Caption:=   '&Reopen';
     Save1.Caption:=     '&Save';
     SaveAs1.Caption:=   'Sa&ve as...';
     SaveAll1.Caption:=  'Save &All';
     Close1.Caption:=    '&Close';
     CloseAll1.Caption:= 'Close A&ll';
     Exit1.Caption:=     '&Exit';

    Edit1.Caption:=      '&Edit';
     Cut1.Caption:=      'C&ut';
     Copy1.Caption:=     '&Copy';
     Paste1.Caption:=    '&Paste';
     Delete1.Caption:=   '&Delete';
     SelectAll1.Caption:='Select &All';
     Undo1.Caption:=     'U&ndo';
     Search1.Caption:=   '&Search...';

    Options1.Caption:=   '&Options';
     Font1.Caption:=      '&Font';
      Code1.Caption:=      '&Editor...';
      Run2.Caption:=       '&Console...';
     Language1.Caption:=  '&Language';
     Keyboard1.Caption:=  '&Keyboard';
     hemes1.Caption:=     '&Themes';
      Day1.Caption:=        '&Day';
      Night1.Caption:=      '&Night';
     Internal1.Caption:=  '&Internal';

    View1.Caption:=      '&View';
     Debugger1.Caption:=    '&Debugger';
     FileExplorer1.Caption:='&File Explorer';
     Log1.Caption:=         '&Log';
     Modules1.Caption:=     '&Modules';
     Profiler1.Caption:=    '&Profiler';
     Recent1.Caption:=      '&Recent';
     Statistics1.Caption:=  '&Statistics';
     ypes1.Caption:=        '&Types';
     oolBar1.Caption:=      'T&oolbar';

    Run1.Caption:=       '&Run';
     Start1.Caption:=     '&Start';
     Stop1.Caption:=      'St&op';
     Pause1.Caption:=     'P&ause';
     Stepin1.Caption:=    'St&ep in';
     Stepover1.Caption:=  'Step o&ver';
     Breakhere1.Caption:= '&Break here';
     ogglebreak1.Caption:= '&Toogle break';
     Prompt1.Caption:=    '&Prompt';
     CompileOnly1.Caption:='&Compile';

    Help1.Caption:=      '&Help';
     LanguageReference1.Caption:= '&Language Reference...';
     About1.Caption:=      '&About...';
  end;
end;

procedure ChangeReserved;
begin
  Vidi_Reserved._ancestor := 'ancestor';
  Vidi_Reserved._and      := 'and';
  Vidi_Reserved._break    := 'break';
  Vidi_Reserved._catch    := 'catch';
  Vidi_Reserved._continue := 'continue';
  Vidi_Reserved._else     := 'else';
  Vidi_Reserved._False    := 'False';
  Vidi_Reserved._final    := 'final';
  Vidi_Reserved._finally  := 'finally';
  Vidi_Reserved._for      := 'for';
  Vidi_Reserved._hidden   := 'hidden';
  Vidi_Reserved._if       := 'if';
  Vidi_Reserved._in       := 'in';
  Vidi_Reserved._indexed  := 'indexed';
  Vidi_Reserved._is       := 'is';
  Vidi_Reserved._not      := 'not';
  Vidi_Reserved._or       := 'or';
  Vidi_Reserved._out      := 'out';
  Vidi_Reserved._repeat   := 'repeat';
  Vidi_Reserved._return   := 'return';
  Vidi_Reserved._self     := 'self';
  Vidi_Reserved._shared   := 'shared';
  Vidi_Reserved._to       := 'to';
  Vidi_Reserved._True     := 'True';
  Vidi_Reserved._try      := 'try';
  Vidi_Reserved._until    := 'until';
  Vidi_Reserved._when     := 'when';
  Vidi_Reserved._while    := 'while';
  Vidi_Reserved._xor      := 'xor';
  Vidi_Reserved._with     := 'with';
end;

initialization
  InitConstants;
end.
