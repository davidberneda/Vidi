unit TheBee_VCL;

interface

uses
  Sys, Tree.AST, ComCtrls, AST;

type
  TVCLTreeAST=class(TTreeAST)
  protected
    function DoAdd(const ANode:TNode; const S:String; const AParent:TObject):TObject; override;
    function FindType(const AType:TType):TObject; override;
  public
    Tree: TTreeView;

    procedure AddExpression(const ANode:TData);
    procedure FillTree(const AModule:String; const FullAST:Boolean);
  end;

implementation

uses
  Module;

function TVCLTreeAST.DoAdd(const ANode:TNode; const S:String; const AParent:TObject):TObject;
begin
  if AParent=nil then
     result:=Tree.Items.AddChildObject(nil,S,ANode)
  else
     result:=TTreeNode(AParent).Owner.AddChildObject(TTreeNode(AParent),S,ANode);
end;

procedure TVCLTreeAST.AddExpression(const ANode:TData);
begin
  Tree.Items.Clear;
  Add(Tree.Items.AddChildObject(nil,'Expression',ANode),ANode);
end;

procedure TVCLTreeAST.FillTree(const AModule:String; const FullAST:Boolean);
begin
  Tree.Items.BeginUpdate;
  try
    TreeModule(AModule,FullAST);

    if FullAST then
    begin
      if Tree.Items.Count>0 then
         Tree.Items[0].Expand(False);
    end
    else
      Tree.SortType:=TSortType.stText;

  finally
    Tree.Items.EndUpdate;
  end;
end;


function TVCLTreeAST.FindType(const AType: TType): TObject;
var tmp : TTreeNode;
begin
  tmp:=Tree.Items.GetFirstNode;

  while tmp<>nil do
     if tmp.Data=AType then
        Exit(tmp)
     else
        tmp:=tmp.GetNext;

  result:=nil;
end;

end.
