unit Transpiler_VCL;

interface

uses
  {$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  Windows,
  {$ENDIF}
  {$ENDIF}

  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Editor_VCL, AST, Gutter;

type
  TNewCodeEvent=procedure(Sender:TObject; const ACode:String) of object;

  TFormTranspiler = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    CBLang: TComboBox;
    BRefresh: TButton;
    BAsNew: TButton;
    BCompile: TButton;
    Splitter1: TSplitter;
    PanelCompile: TPanel;
    Label2: TLabel;
    LabelPath: TLabel;
    BRun: TButton;
    procedure BRefreshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBLangChange(Sender: TObject);
    procedure BAsNewClick(Sender: TObject);
    procedure BCompileClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LabelPathClick(Sender: TObject);
    procedure BRunClick(Sender: TObject);
  private
    { Private declarations }

    Gutter : TEditorGutter;

    FinalExe : String;

    procedure CreateCompilerLog;
    procedure CreateEditor;
    procedure EditInvalidate(Sender:TObject);
    procedure RecalcGutterWidth;
  public
    { Public declarations }

    Edit, Log : TCodeRichEdit;
    Module : TNamedType;

    OnNewCode : TNewCodeEvent;

    procedure Transpile(const AModule:TNamedType);
  end;

implementation

{$R *.dfm}

uses
  {$IFNDEF FPC}
  UITypes,
  {$ENDIF}

  Emit, Emit.Pascal, Emit.CSharp,
  RichEmit, Compile.Pascal, Utils_VCL;

const
  Vidi_Lang       = 0;
  Delphi_Lang     = 1;
  FreePascal_Lang = 2;
  CSharp_Lang     = 3;

procedure TFormTranspiler.BAsNewClick(Sender: TObject);
begin
  OnNewCode(Self,Edit.Text);
end;

procedure TFormTranspiler.BRefreshClick(Sender: TObject);
begin
  if Module<>nil then
     Transpile(Module);
end;

procedure TFormTranspiler.BRunClick(Sender: TObject);
begin
  if FinalExe<>'' then
  begin
    if Log=nil then
       CreateCompilerLog
    else
       Log.Clear;

    TCompiler.RunExecutable(FinalExe,Log.Lines);
  end;
end;

procedure TFormTranspiler.BCompileClick(Sender: TObject);
var tmp : String;
    tmpResult : Boolean;

    tmpDelphi : TDelphiCompile;
    tmpFPC : TPascalCompile;
    tmpCSharp : TCSharpCompile;
begin
  BRun.Enabled:=False;

  if Log=nil then
     CreateCompilerLog
  else
     Log.Clear;

  FinalExe:='';

  if CBLang.ItemIndex=Delphi_Lang then
  begin
    tmpDelphi:=TDelphiCompile.Create;
    try
      tmpDelphi.Verbose:=True;

      tmpResult:=tmpDelphi.Compile(TPascalEmit.ModuleName(Module),Edit.Text,
                                   Log.Lines,tmp,FinalExe)
    finally
      tmpDelphi.Free;
    end;
  end
  else
  if CBLang.ItemIndex=FreePascal_Lang then
  begin
    tmpFPC:=TPascalCompile.Create;
    try
      tmpFPC.Verbose:=True;

      tmpResult:=tmpFPC.Compile(TPascalEmit.ModuleName(Module),Edit.Text,
                                Log.Lines,tmp,FinalExe)
    finally
      tmpFPC.Free;
    end;
  end
  else
  if CBLang.ItemIndex=CSharp_Lang then
  begin
    tmpCSharp:=TCSharpCompile.Create;
    try
      tmpCSharp.Verbose:=True;

      tmpResult:=tmpCSharp.Compile(TCSharpEmit.ModuleName(Module),Edit.Text,
                                   Log.Lines,tmp,FinalExe)
    finally
      tmpCSharp.Free;
    end;
  end
  else
     Exit;

  if tmpResult then
     LabelPath.Font.Color:=clNavy
  else
     LabelPath.Font.Color:=clRed;

  LabelPath.Caption:=tmp;

  PanelCompile.Visible:=True;

  if Log=nil then
     CreateCompilerLog;

  Log.Visible:=True;
  Splitter1.Visible:=True;

  BRun.Enabled:=FinalExe<>'';
end;

procedure TFormTranspiler.CBLangChange(Sender: TObject);
begin
  BAsNew.Enabled:=CBLang.ItemIndex=Vidi_Lang;

  TPascalEmit.InlineVariables:=CBLang.ItemIndex=Delphi_Lang;
  TPascalEmit.UniqueUnitName:=CBLang.ItemIndex=Delphi_Lang;

  BRefreshClick(Self);
end;

procedure TFormTranspiler.CreateEditor;
begin
  Edit:=TCodeRichEdit.Create(Self);
  Edit.Align:=TAlign.alClient;
  Edit.ScrollBars:={$IFNDEF FPC}System.UITypes.{$ENDIF}TScrollStyle.ssBoth;
  Edit.Parent:=Self;

  Edit.Font.Name:='Courier New';
  Edit.Font.Size:=10;
end;

procedure TFormTranspiler.CreateCompilerLog;
begin
  Log:=TCodeRichEdit.Create(Self);

  Log.Visible:=False;

  Log.Align:=TAlign.alBottom;
  Log.ScrollBars:={$IFNDEF FPC}System.UITypes.{$ENDIF}TScrollStyle.ssBoth;
  Log.Parent:=Self;
end;

procedure TFormTranspiler.FormCreate(Sender: TObject);
begin
  CreateEditor;

  Gutter:=TEditorGutter.Create(Self);
  Gutter.Edit:=Edit;

  CBLangChange(Self);

  Edit.OnInvalidate:=EditInvalidate;
end;

procedure TFormTranspiler.RecalcGutterWidth;
begin
  if Gutter<>nil then
  begin
    Gutter.AutoWidth(Edit.Lines.Count); // <-- slow?
    Gutter.Invalidate;
  end;
end;

procedure TFormTranspiler.EditInvalidate(Sender:TObject);
begin
  RecalcGutterWidth;
end;

procedure TFormTranspiler.FormShow(Sender: TObject);
begin
  Gutter.Parent:=Self;
  Gutter.Align:=TAlign.alLeft;
end;

procedure TFormTranspiler.LabelPathClick(Sender: TObject);
begin
  TGoto.ToFile(LabelPath.Caption);
end;

function EmitCSharp(const AModule:TNamedType; const AEditor:TCodeRichEdit):String;
var tmp : TCSharpEmit;
begin
  tmp:=TCSharpEmit.Create;
  try
    tmp.Emit.Free;
    tmp.Emit:=TRichEmit.Create;

    TRichEmit(tmp.Emit).RichEdit:=AEditor;

    result:=tmp.AsString(AModule);
  finally
    tmp.Free;
  end;
end;

function EmitPascal(const AModule:TNamedType; const AEditor:TCodeRichEdit):String;
var tmp : TPascalEmit;
begin
  tmp:=TPascalEmit.Create;
  try
    tmp.Emit.Free;
    tmp.Emit:=TRichEmit.Create;

    TRichEmit(tmp.Emit).RichEdit:=AEditor;

    result:=tmp.AsString(AModule);
  finally
    tmp.Free;
  end;
end;

function EmitVidi(const AModule:TNamedType; const AEditor:TCodeRichEdit):String;
var tmp : TVidiEmit;
begin
  tmp:=TVidiEmit.Create;
  try
    tmp.Emit.Free;
    tmp.Emit:=TRichEmit.Create;

    TRichEmit(tmp.Emit).RichEdit:=AEditor;

    result:=tmp.AsString(AModule);
  finally
    tmp.Free;
  end;
end;

procedure TFormTranspiler.Transpile(const AModule: TNamedType);
begin
  Module:=AModule;

  Edit.Lines.BeginUpdate;
  try
    Edit.Clear;

    case CBLang.ItemIndex of
      Vidi_Lang: EmitVidi(AModule,Edit);

      Delphi_Lang,
      FreePascal_Lang: EmitPascal(AModule,Edit);

      CSharp_Lang: EmitCSharp(AModule,Edit);
    end;

    BCompile.Enabled:=CBLang.ItemIndex<>Vidi_Lang;
  finally
    Edit.Lines.EndUpdate;
  end;
end;

end.
