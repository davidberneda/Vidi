unit Trace_VCL;

interface

uses
  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,
  //Evaluator,
  AST, RunningNode;

type
  TGotoTraceEvent=procedure(Sender:TObject; const ACall:TDataCall) of object;

  TFormTrace = class(TForm)
    LBTrace: TListBox;
    procedure LBTraceDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    OnTraceJump : TGotoTraceEvent;

    procedure Fill(const AStack: TRunStack);

    {$IFDEF INTERNAL}
    class function StackAsText(const AStack:TRunStack):String; static;
    {$ENDIF}
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF INTERNAL}
  Internal,
  {$ENDIF}
  Utils.AST, Checker.AST, Sys, Find.AST;

function StackTypeOf(const AItem:TNode):TType;
begin
  if AItem is TDataCall then
     result:=TChecker.GetFinalType(TDataCall(AItem).Routine)
  else
  if AItem is TBlockStatement then
     result:=TFinder.ModuleOf(AItem)
  else
  if AItem is TNamedType then
     result:=TNamedType(AItem) // <-- top-level module
  else
  begin
    result:=nil; // Internal Error !!

    {$IFDEF INTERNAL}
    InternalError('Cannot find type of stack item:',AItem);
    {$ENDIF}
  end;
end;

{$IFDEF INTERNAL}
class function TFormTrace.StackAsText(const AStack:TRunStack):String;
var t : Integer;
begin
  result:='';

  for t:=High(AStack.Items) downto Low(AStack.Items) do
  begin
    if result<>'' then
       result:=result+#13#10;

    result:=result+TASTUtils.TypeAndName(StackTypeOf(AStack.Items[t]));
  end;
end;
{$ENDIF}

procedure TFormTrace.Fill(const AStack: TRunStack);
var t : Integer;
begin
  LBTrace.Items.BeginUpdate;
  try
    LBTrace.Clear;

    for t:=High(AStack.Items) downto Low(AStack.Items) do
        LBTrace.Items.AddObject(
                TASTUtils.TypeAndName(StackTypeOf(AStack.Items[t])),
                 AStack.Items[t]);
  finally
    LBTrace.Items.EndUpdate;
  end;
end;

procedure TFormTrace.LBTraceDblClick(Sender: TObject);
begin
  if LBTrace.ItemIndex<>-1 then
     OnTraceJump(Self,TDataCall(LBTrace.Items.Objects[LBTrace.ItemIndex]));
end;

end.
