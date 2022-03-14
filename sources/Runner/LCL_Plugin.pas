unit LCL_Plugin;

interface

uses
  Sys, AST, Plugin, Instance_Type;

type
  TLCLPlugin=class(TPlugin)
  protected
    procedure DoHook; override;
  public
    class var
      TestMode : Boolean;
  end;

implementation

uses
  Graphics, Forms, Controls, Checker.AST, StdCtrls;

type
  TControlInstance=class(TInstance)
  public
    Control : TControl;

    Destructor Destroy; override;
  end;

Destructor TControlInstance.Destroy;
begin
  Control.Free;
  inherited;
end;

type
  TFormInstance=class(TControlInstance)
  public
    Constructor Create; override;
  end;

Constructor TFormInstance.Create;
begin
  inherited;
  Control:=TForm.Create(nil);
end;

type
  TButtonInstance=class(TControlInstance)
  public
    Constructor Create; override;
  end;

  TEditInstance=class(TControlInstance)
  public
    Constructor Create; override;
  end;

  TLabelInstance=class(TControlInstance)
  public
    Constructor Create; override;
  end;

  TListViewInstance=class(TControlInstance)
  public
    Constructor Create; override;
  end;

  TGraphicsInstance=class(TInstance)
  public
    Graphics : TCanvas;

    Constructor Create; override;
    Destructor Destroy; override;
  end;

Constructor TButtonInstance.Create;
begin
  inherited;
  Control:=TButton.Create(nil);
end;

Constructor TEditInstance.Create;
begin
  inherited;
  Control:=TEdit.Create(nil);
end;

Constructor TLabelInstance.Create;
begin
  inherited;
  Control:=TLabel.Create(nil);
end;

Constructor TListViewInstance.Create;
begin
  inherited;
  Control:=TListBox.Create(nil);
end;

type
  TControlAccess=class(TControl);

  function GetControl(const Instance:TNode):TControl;
  begin
    result:=(Instance as TControlInstance).Control;
  end;

  function GetForm(const Instance:TNode):TForm;
  begin
    result:=GetControl(Instance) as TForm;
  end;

  function ParamData(const Parameters:TNode; const AIndex:Integer):TData;
  begin
    result:=TInstance(Parameters).Data[AIndex].Value.Value;
  end;

  function ParamAsInteger(const Parameters:TNode; const AIndex:Integer):Int64;
  begin
    result:=(ParamData(Parameters,AIndex) as TInteger).Value;
  end;

  function ParamAsText(const Parameters:TNode; const AIndex:Integer):String;
  begin
    result:=(ParamData(Parameters,AIndex) as TText).Value;
  end;

function FormShow(const Instance,Parameters:TNode):TData;
begin
  GetForm(Instance).Show;
  result:=nil;
end;

function FormClose(const Instance,Parameters:TNode):TData;
begin
  GetForm(Instance).Close;
  result:=nil;
end;

function FormModal(const Instance,Parameters:TNode):TData;
var tmp : Integer;
    tmpForm : TForm;
begin
  if TLCLPlugin.TestMode then
  begin
    tmpForm:=GetForm(Instance);
    tmpForm.Show;
    tmpForm.Close;
    tmp:=0;
  end
  else
     tmp:=GetForm(Instance).ShowModal;

  result:=TInteger.Create(tmp);
end;

function SetText(const Instance,Parameters:TNode):TData;
begin
  TControlAccess(GetControl(Instance)).Text:=ParamAsText(Parameters,0);
  result:=nil;
end;

function AddControl(const Instance,Parameters:TNode):TData;
var tmp : TInstance;
    tmpControl : TControl;
begin
  tmp:=(TInstance(Parameters).Data[0].Value as TInstance);

  if tmp is TControlInstance then
     tmpControl:=GetControl(tmp)
  else
     tmpControl:=GetControl(tmp.Value); // <-- run no-debug from TRunner.Call

  tmpControl.Left:=ParamAsInteger(Parameters,1);
  tmpControl.Top:=ParamAsInteger(Parameters,2);

  tmpControl.Parent:=GetControl(Instance) as TWinControl;

  result:=nil;
end;

{ TVCLPlugin }

procedure TLCLPlugin.DoHook;

  procedure SetAddControl(const AType:TType);
  var V1,V2 : TVariable;
  begin
    V1:=TVariable.Create;
    V2:=TVariable.Create;
    try
      V1.VariableType:=AType;
      V2.VariableType:=TChecker._Types[TChecker._Integer];

      FindMethod(AType,'Add',nil,V1,V2,V2).Hook:=AddControl;
    finally
      V2.Free;
      V1.Free;
    end;
  end;

var tmpType : TType;
    V : TVariable;
begin
  tmpType:=Finder.FindType(Context,'Form');
  tmpType.Plugin:=TFormInstance;

  FindMethod(tmpType,'Show').Hook:=FormShow;
  FindMethod(tmpType,'Close').Hook:=FormClose;

  FindMethod(tmpType,'Modal',TChecker._Types[TChecker._Integer]).Hook:=FormModal;

  tmpType:=Finder.FindType(Context,'Control');
  SetAddControl(tmpType);

  tmpType:=Finder.FindType(Context,'TextControl');

  V:=TVariable.Create;
  try
    V.VariableType:=TChecker._Types[TChecker._Text]; // Text

    FindMethod(tmpType,'Title',nil,V).Hook:=SetText;
  finally
    V.Free;
  end;

  tmpType:=Finder.FindType(Context,'Button');
  tmpType.Plugin:=TButtonInstance;

  tmpType:=Finder.FindType(Context,'Edit');
  tmpType.Plugin:=TEditInstance;

  tmpType:=Finder.FindType(Context,'Label');
  tmpType.Plugin:=TLabelInstance;

  tmpType:=Finder.FindType(Context,'ListView');
  tmpType.Plugin:=TListViewInstance;

  tmpType:=Finder.FindType(Context,'Graphics');
  tmpType.Plugin:=TGraphicsInstance;
end;

var
  Plug : TLCLPlugin;

{ TGraphicsInstance }

constructor TGraphicsInstance.Create;
begin
  inherited;
  Graphics:=TCanvas.Create;
end;

destructor TGraphicsInstance.Destroy;
begin
  Graphics.Free;
  inherited;
end;

initialization
  Plug:=TLCLPlugin.Create;
  Plug.Module:='UI';

  TPlugin.Register(Plug);
finalization
  Plug.Free;
end.
