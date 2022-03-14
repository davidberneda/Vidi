unit Types_VCL;

interface

uses
  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Sys, Usage, Menus, Grid_VCL, AST, Map,
  Utils_VCL;

type
  TFormTypes = class(TForm)
    PanelTypes: TPanel;
    Splitter1: TSplitter;
    PanelOptions: TPanel;
    CBAncestor: TCheckBox;
    Splitter2: TSplitter;
    PageControl1: TPageControl;
    TabReferences: TTabSheet;
    PopupMenu1: TPopupMenu;
    Jumpto1: TMenuItem;
    PanelFields: TPanel;
    procedure CBAncestorClick(Sender: TObject);
    procedure FieldsClick(Sender: TObject);
    procedure LBReferencesDblClick(Sender: TObject);
    procedure FieldsDblClick(Sender: TObject);
    procedure Jumpto1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }

    Fields,
    References : TVidiGrid;

    procedure ChangeType(Sender: TObject; Node: TTreeNode);
    procedure ClearFields;
    function GetTree:TTreeView;
    procedure GotoTreeNode(Sender: TObject);
    function SelectedField:TVariable;
  public
    { Public declarations }

    Positions : TNodePositions;
    Usages : TNodeUsages;

    OnGotoNode : TOnGotoNode;
    OnGotoDeclaration : TOnGotoNode;

    procedure AddTree(const ATree:TTreeView);
    class procedure FillFields(const AGrid:TVidiGrid;
                               const ANode:TNode;
                               ShowAncestor:Boolean); static;
    procedure DoTranslate;
    procedure FillReferences(const ANode:TNode);

    procedure RemoveTree;
  end;

implementation

{$R *.dfm}

uses
  Syntax, Emit, Find.AST, Utils.AST, IDE_Lang_English;

{ TFormTypes }

procedure TFormTypes.ClearFields;
begin
  Fields.Clear;
  References.Clear;
end;

procedure TFormTypes.CBAncestorClick(Sender: TObject);
var Tree : TTreeView;
begin
  ClearFields;

  Tree:=GetTree;

  if Tree<>nil then
     if Tree.Selected<>nil then
        ChangeType(Tree,Tree.Selected);
end;

procedure TFormTypes.FieldsClick(Sender: TObject);
begin
  References.Clear;

  if Fields.Row>0 then
     FillReferences(SelectedField);
end;

function TFormTypes.SelectedField:TVariable;
var tmp : Integer;
begin
  tmp:=Fields.Row;

  if tmp>0 then
     result:=Fields.RowObjects[tmp] as TVariable
  else
     result:=nil;
end;

procedure TFormTypes.FieldsDblClick(Sender: TObject);
var tmp : TVariable;
begin
  tmp:=SelectedField;

  if tmp<>nil then
  begin
    if Fields.Col=1 then
       OnGotoDeclaration(Self,tmp.VariableType)
    else
       OnGotoNode(Self,tmp);
  end;
end;

class procedure TFormTypes.FillFields(const AGrid:TVidiGrid;
                                      const ANode:TNode;
                                      ShowAncestor:Boolean);
var Emit : TVidiEmit;

  function TryAdd(const B:Boolean; const AText:String):String;
  begin
    if B then result:=' '+AText else result:='';
  end;

  function ClausesToString(const Clauses:TClauses):String;
  begin
    result:=TryAdd(Clauses.Indexed,TSyntax._Reserved_indexed)+
            TryAdd(Clauses.Hidden,TSyntax._Reserved_hidden)+
            TryAdd(Clauses.Shared,TSyntax._Reserved_shared)+
            TryAdd(Clauses.Final,TSyntax._Reserved_final);
  end;

  procedure AddVariable(const AVariable:TVariable);
  var tmp : String;
  begin
    if AVariable.ValueData=nil then
       tmp:=''
    else
       tmp:=Emit.AsString(AVariable.ValueData);

    AGrid.AppendRow([AVariable.Name,
                     Emit.TypeToString(AVariable.VariableType),
                     tmp,
                     ClausesToString(AVariable.Clauses)],
                     AVariable);
  end;

  procedure AddVariables(const AItems:TNodes);
  var I : TNode;
  begin
    for I in AItems do
        if I is TVariable then
           AddVariable(TVariable(I));
  end;

  procedure AddFields(const AType:TType; Ancestor:Boolean);
  var I : TNode;
  begin
    if AType is TParametersType then
       AddVariables(TParametersType(AType).Parameters);

    if AType is TSpecializedType then
       AddVariables(TSpecializedType(AType).Generics);

    for I in AType.Items do
        if I is TBlockStatement then
           AddFields(TBlockStatement(I).Block,Ancestor)
        else
        if I is TVariable then
           AddVariable(TVariable(I));

    if Ancestor and (AType is TClassType) then
       if TClassType(AType).Ancestor<>nil then
          AddFields(TClassType(AType).Ancestor,True);
  end;

begin
  if ANode is TType then
  begin
    AGrid.BeginUpdate;
    try
      AGrid.Clear;

      Emit:=TVidiEmit.Create;
      try
        AddFields(TType(ANode),ShowAncestor);
      finally
        Emit.Free;
      end;
    finally
      AGrid.EndUpdate;
    end;
  end;
end;

procedure TFormTypes.FillReferences(const ANode:TNode);

  procedure Fill(const AUsage:TNodeUsage);
  var R : TNodeUsed;
      tmpPos : TNodePosition;
      tmpLine,
      tmpColumn : String;
  begin
    for R in AUsage.References do
    begin
      if Positions.Find(R.Node,tmpPos) then
      begin
        tmpLine:=IntToStr(tmpPos.Position.Line);
        tmpColumn:=IntToStr(tmpPos.Position.Column);
      end
      else
      begin
        tmpLine:='';
        tmpColumn:='';
      end;

      References.AppendRow([TFinder.ModuleNameOf(R.Node),
                            tmpLine,
                            tmpColumn,
                            '', // TODO: Extract snippet from source code file ?
                            TASTUtils.RemoveTPrefix(R.Node)
                           ],R.Node);
    end;
  end;

var tmp : Integer;
begin
  References.BeginUpdate;
  try
    References.Clear;

    if Usages<>nil then
    begin
      tmp:=Usages.IndexOf(ANode);

      if tmp<>-1 then
         Fill(Usages.Items[tmp]);
    end;
  finally
    References.EndUpdate;
  end;
end;

procedure TFormTypes.FormCreate(Sender: TObject);

  procedure CreateFieldsGrid;
  begin
    Fields:=TVidiGrid.Create(Self);
    Fields.Align:=TAlign.alClient;

    Fields.Header([Vidi_Lang.Name,Vidi_Lang._Type,Vidi_Lang.Value,'']);

    Fields.Parent:=PanelFields;

    Fields.OnClick:=FieldsClick;
    Fields.OnDblClick:=FieldsDblClick;

    Fields.ColWidths[1]:=150;
    Fields.ColWidths[2]:=150;

    Fields.PopupMenu:=PopupMenu1;
  end;

  procedure CreateReferencesGrid;
  begin
    References:=TVidiGrid.Create(Self);
    References.Align:=TAlign.alClient;

    References.Header([Vidi_Lang.Module,Vidi_Lang.Line,Vidi_Lang.Column,Vidi_Lang.Text,
                       Vidi_Lang._Class]);

    References.Parent:=TabReferences;

    References.OnDblClick:=LBReferencesDblClick;

    References.ColWidths[0]:=150;
    References.ColWidths[1]:=50;
    References.ColWidths[2]:=50;
    References.ColWidths[3]:=250;
    References.ColWidths[4]:=100;

    References.PopupMenu:=PopupMenu1;
  end;

begin
  CreateFieldsGrid;
  CreateReferencesGrid;

  AddTitlePanel(PanelFields,Vidi_Lang.Fields);
  AddTitlePanel(PanelTypes,Vidi_Lang.Types);
end;

procedure TFormTypes.DoTranslate;
begin
  CBAncestor.Caption:=Vidi_Lang.ShowAncestorItems;
  TabReferences.Caption:=Vidi_Lang.References;
end;

procedure TFormTypes.FormShow(Sender: TObject);
begin
  DoTranslate;
end;

procedure TFormTypes.ChangeType(Sender: TObject; Node: TTreeNode);
begin
  FillFields(Fields,TNode(Node.Data),CBAncestor.Checked);

  FillReferences(TNode(Node.Data));
end;

procedure TFormTypes.GotoTreeNode(Sender: TObject);
var tmp : TTreeNode;
begin
  tmp:=TTreeView(Sender).Selected;

  if tmp<>nil then
     if tmp.Count=0 then  // do not consider tree nodes with child nodes
        OnGotoNode(Self,TNode(tmp.Data));
end;

procedure TFormTypes.Jumpto1Click(Sender: TObject);
var tmp : TTreeNode;
begin
  if PopupMenu1.PopupComponent is TTreeView then
  begin
    tmp:=TTreeView(PopupMenu1.PopupComponent).Selected;

    if tmp<>nil then
       OnGotoNode(Self,TNode(tmp.Data));
  end
  else
  if PopupMenu1.PopupComponent=References then
     LBReferencesDblClick(Self)
  else
     FieldsDblClick(Self);
end;

procedure TFormTypes.AddTree(const ATree:TTreeView);
begin
  ATree.Parent:=PanelTypes;
  ATree.Align:=TAlign.alClient;

  ClearFields;

  ATree.OnChange:=ChangeType;
  ATree.OnDblClick:=GotoTreeNode;
  ATree.PopupMenu:=PopupMenu1;

  CBAncestor.Enabled:=True;
end;

function TFormTypes.GetTree:TTreeView;
begin
  if (PanelTypes.ControlCount>1) and (PanelTypes.Controls[1] is TTreeView) then
     result:=TTreeView(PanelTypes.Controls[1])
  else
     result:=nil;
end;

procedure TFormTypes.LBReferencesDblClick(Sender: TObject);
var tmp : Integer;
begin
  tmp:=References.Row;

  if tmp>0 then
     OnGotoNode(Self,TNode(References.RowObjects[tmp]));
end;

procedure TFormTypes.RemoveTree;
var Tree : TTreeView;
begin
  Tree:=GetTree;

  if Tree<>nil then
  begin
    Tree.OnChange:=nil;
    Tree.Parent:=nil;

    ClearFields;
  end;

  CBAncestor.Enabled:=False;
end;

end.
