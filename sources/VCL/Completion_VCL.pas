unit Completion_VCL;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, Classes, Graphics,
  {$IFDEF FPC}
  LCLType,
  {$ENDIF}
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Sys, Editor;

type
  TFormComplete = class(TForm)
    LBItems: TListBox;
    Panel1: TPanel;
    CBAlpha: TCheckBox;
    procedure LBItemsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LBItemsDblClick(Sender: TObject);
    procedure CBAlphaClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }

    procedure CloseCompletion;
    function ItemSelected:String;
    procedure MakeListVisible(const AParent:TWinControl);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }

    Editor : TCodeEditor;

    procedure ShowList(const AEditor:TCodeEditor; Partial:Boolean);
  end;

implementation

{$R *.dfm}

uses
  //AST, Utils.AST,
  Checker.AST,
  Search,
  Module, Syntax, Completion;

{ TFormComplete }

procedure TFormComplete.CBAlphaClick(Sender: TObject);
begin
  if CBAlpha.Checked then
     LBItems.Sorted:=True;
end;

procedure TFormComplete.CloseCompletion;
begin
  Hide;
  Editor.Focus;
end;

procedure TFormComplete.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style {or WS_BORDER} or WS_THICKFRAME;
end;

procedure TFormComplete.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 // Debug
end;

function TFormComplete.ItemSelected:String;
var Node : TNode;
begin
  Node:=TNode(LBItems.Items.Objects[LBItems.ItemIndex]);

  result:=TSearch.NodeName(Node);
end;

procedure TFormComplete.LBItemsDblClick(Sender: TObject);
begin
  if LBItems.ItemIndex<>-1 then
  begin
    Editor.Edit.SelText:=ItemSelected;
    CloseCompletion;
  end;
end;

procedure TFormComplete.LBItemsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then
     CloseCompletion
  else
  if Key=VK_RETURN then
     LBItemsDblClick(Self);
end;

procedure TFormComplete.MakeListVisible(const AParent:TWinControl);
var P : TPoint;
    EP : TPoint;
begin
  P:=Editor.Edit.PositionOf(Editor.Edit.SelStart);

  EP:=TPoint.Create(Editor.Left,Editor.Top);

//  EP:=Editor.ClientToScreen(EP);

  Align:=alNone;
  Parent:=AParent; //AParent;

  Left:=EP.X+P.X+56;
  Top:=EP.Y+P.Y+20;

  Editor.Completion:=LBItems;

  Show;
end;

procedure TFormComplete.ShowList(const AEditor:TCodeEditor; Partial:Boolean);
var S : String;
    tmpFound,
    tmpDelta : Integer;
    tmpNode : TNode;
begin
  Editor:=AEditor;

  if Partial then
     S:=Editor.PartialWord
  else
     S:='';

  if Editor.Edit.CharAt(0)='.' then
     tmpDelta:=-2
  else
     tmpDelta:=-1;

  tmpNode:=Editor.NodeAtOrBeforeCursor(tmpDelta);

  if tmpNode=nil then
     tmpNode:=Editor.Context;

  tmpFound:=TCompletion.FillItems(LBItems.Items,tmpNode,S);

  if LBItems.Count>0 then
  begin
    LBItems.ItemIndex:=tmpFound;

    MakeListVisible(AEditor);
  end;
end;

end.
