unit Tests_VCL;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, ComCtrls, StdCtrls,

  Editor, Bee.Parser, Sys;

type
  TFormTests = class(TForm)
    PageTests: TPageControl;
    TabEmit: TTabSheet;
    TabSheet2: TTabSheet;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Memo2: TMemo;
    ComboBox1: TComboBox;
    Button2: TButton;
    Button4: TButton;
    Button6: TButton;
    TreeView2: TTreeView;
    Panel1: TPanel;
    EmitPascal: TButton;
    EmitVidi: TButton;
    procedure Button2Click(Sender: TObject);
    procedure EmitPascalClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button4Click(Sender: TObject);
    procedure EmitVidiClick(Sender: TObject);
  private
    { Private declarations }

    procedure CreateEmitEditor;
    function CurrentContext:TNode;
  public
    { Public declarations }

    Bee: TBee;
    CurrentEditor : TCodeEditor;
    EmitEditor : TCodeEditor;
  end;

implementation

{$R *.dfm}

uses
  TheBee_VCL, Emit, Emit.Pascal, Module,

  TheBee_Test_Expressions,

  Evaluator, Editor_VCL, RichEmit;

function TFormTests.CurrentContext:TNode;
begin
  result:=CurrentEditor.Context;
end;

procedure TFormTests.EmitPascalClick(Sender: TObject);
var tmp : TPascalEmit;
begin
  tmp:=TPascalEmit.Create;
  try
    tmp.Emit.Free;
    tmp.Emit:=TRichEmit.Create;

    TRichEmit(tmp.Emit).RichEdit:=EmitEditor.Edit;

    EmitEditor.Edit.Lines.Text:=tmp.AsString(CurrentContext);
  finally
    tmp.Free;
  end;
end;

procedure TFormTests.EmitVidiClick(Sender: TObject);
var tmp : TVidiEmit;
    tmpR : TCodeRichEdit;
begin
  tmp:=TVidiEmit.Create;
  try
    tmp.Emit.Free;
    tmp.Emit:=TRichEmit.Create;

    tmpR:=EmitEditor.Edit;

    TRichEmit(tmp.Emit).RichEdit:=tmpR;

    tmpR.Lines.Text:=tmp.AsString(CurrentContext);

    tmpR.SelStart:=0;
  finally
    tmp.Free;
  end;
end;

procedure TFormTests.Button2Click(Sender: TObject);
var tmp : TData;
    AST : TVCLTreeAST;
    tmpContext : TNode;
begin
  tmpContext:=CurrentContext;

  if tmpContext=nil then
     tmpContext:=Modules.SystemModule;

  tmp:=TBee.ParseExpression(tmpContext,ComboBox1.Text);

  Label2.Caption:='';
  Memo2.Clear;
  TreeView2.Items.Clear;

  if tmp<>nil then
  begin
    AST:=TVCLTreeAST.Create;
    try
      AST.Tree:=TreeView2;

      Memo2.Text:=AST.Emit.AsString(tmp);

      Memo2.Lines.Add('');
      Memo2.Lines.Add(TEvaluate.AsText(tmp));

      AST.AddExpression(tmp);

      if TreeView2.Items.Count>0 then
         TreeView2.Items[0].Expand(True);
    finally
      AST.Free;
    end;
  end;
end;

procedure TFormTests.Button4Click(Sender: TObject);
begin
  with TFormTestExpressions.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TFormTests.Button6Click(Sender: TObject);
begin
//  DoParseCurrent(Bee.ParserPath,Bee.ModuleName,EmitEditor.Edit.Text);
end;

procedure TFormTests.ComboBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then
     Button2Click(Self);
end;

procedure TFormTests.CreateEmitEditor;
begin
  EmitEditor:=TCodeEditor.Create(Self);
  EmitEditor.Parent:=TabEmit;

  EmitEditor.Init;

  EmitEditor.Align:=TAlign.alClient;
end;

procedure TFormTests.FormCreate(Sender: TObject);
begin
  CreateEmitEditor;
end;

end.
