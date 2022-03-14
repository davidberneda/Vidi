unit AST_VCL;

// When sorting positions, make sure there are no duplicates
{$IFDEF INTERNAL}
{$DEFINE CHECK_POSITIONS}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  Windows,
  {$ENDIF}
  //Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Editor, TheBee_VCL, ComCtrls, Buttons, Grids, Sys, Map, Grid_VCL, Usage, Sort,
  AST;

type
  TSelectedPosition=procedure(Sender:TObject; const APosition:TNodePosition) of object;
  TGetNodeText=function(Sender:TObject; const APosition:TNodePosition):String of object;

  TIntegerArray=Array of Integer;

  { TFormAST }

  TFormAST = class(TForm)
    Panel1: TPanel;
    LClass: TLabel;
    LOwner: TLabel;
    Panel2: TPanel;
    Label1: TLabel;
    CBSearch: TComboBox;
    SBSearch: TSpeedButton;
    TestButton: TButton;
    LPosition: TLabel;
    PageControl1: TPageControl;
    TabTree: TTabSheet;
    TabMap: TTabSheet;
    Panel3: TPanel;
    CBFilter: TComboBox;
    LCount: TLabel;
    Button1: TButton;
    TabReferences: TTabSheet;
    Splitter1: TSplitter;
    PanelUsages: TPanel;
    procedure CBSearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SBSearchClick(Sender: TObject);
    procedure LOwnerClick(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
    procedure CBSearchChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MapSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure CBFilterChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }

    Sorted : TIntegerArray;

    function CalcNameOf(const ANode:TNode):String;
    procedure ClearStatus;
    function Compare(const a,b:Integer):TComparison;
    procedure FillPositions;
    procedure FillReferences(const AIndex:Integer);
    procedure FillUsed;
    function FindNode(const ANode:TNode):TTreeNode;
    procedure GotoNode(const ANode:TNode);
    function ItemAt(const AIndex:Integer):TNodeUsage;
    procedure Search(S:String);
    procedure Swap(const a,b:Integer);
    function TextOf(const APosition:TNodePosition):String;
    procedure TreeASTChange(Sender: TObject; Node: TTreeNode);

    procedure UsedSelected(Sender: TObject; ACol, ARow: Longint; var CanSelect: Boolean);
  public
    { Public declarations }

    Map: TVidiGrid;
    Tree : TVCLTreeAST;

    Used,
    References : TVidiGrid;

    FullText : String;

    Positions : TNodePositions;
    Usages : TNodeUsages;

    OnGetText : TGetNodeText;
    OnTreeChange : TSelectedPosition;

    procedure Clear;

    function GetAST(const AEditor:TCodeEditor):TVCLTreeAST;

    class function ModalShow(const AOwner:TComponent; const AContext:TType):Integer; static;

    procedure Reset(const APositions:TNodePositions; const AUsages:TNodeUsages);

    class procedure TryFillTree(const ATree:TVCLTreeAST;
                                const AOwner,AParent:TWinControl;
                                const AModule:String; const FullAST:Boolean); overload; static;

    procedure TryFillTree(const ATree: TVCLTreeAST;
                          const AParent: TWinControl;
                          const AModule: TType); overload;
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF FPC}
  LCLType,
  {$ENDIF}
  Utils_VCL, Utils.AST, Position, Emit,
  Checker.Usage, Syntax, IDE_Lang_English;

{ TFormAST }

procedure TFormAST.Clear;
begin
  Map.RowCount:=1;

  Positions:=nil;
  Usages:=nil;
  Tree:=nil;

 //FormAST.Positions.Clear;

  Used.RowCount:=1;
  References.RowCount:=1;
end;

procedure TFormAST.ClearStatus;
begin
  LOwner.Caption:='';
  LOwner.Tag:=0;
  LClass.Caption:='';
  LPosition.Caption:='';
end;

procedure TFormAST.Button1Click(Sender: TObject);
begin
  if CBFilter.ItemIndex<>0 then
  begin
    CBFilter.ItemIndex:=0;
    CBFilterChange(Self);
  end;

  {$IFDEF CHECK_POSITIONS}
  if Positions<>nil then
  begin
    // First check for corruption:
    Positions.CheckNodes;

    // Then check for code text not represented at any map node:
    Positions.CheckMissing(FullText);
  end;
  {$ENDIF}
end;

procedure TFormAST.CBFilterChange(Sender: TObject);
begin
  FillPositions;
end;

procedure TFormAST.TreeASTChange(Sender: TObject; Node: TTreeNode);

  function RowOfObject(const APos:Integer):Integer;
  var t : Integer;
  begin
    if CBFilter.ItemIndex=0 then
       result:=APos+1
    else
    begin
      for t:=1 to Map.RowCount-1 do
          if Integer(Map.RowObjects[t])=APos then
             Exit(t);

      result:=0;
    end;
  end;

  function SelectPosition(const ANode:TNode):Integer;
  var P : TNodePosition;
      tmp : Integer;
  begin
    result:=Positions.IndexOf(ANode);

    if result=-1 then
       LPosition.Caption:='[?]'
    else
    begin
      P:=Positions.Items[result];
      LPosition.Caption:=P.Position.LineColumn+' '+IntToStr(P.Length);

      tmp:=RowOfObject(result);

      if tmp<Map.RowCount then
         Map.Row:=tmp;
    end;
  end;

var tmp : TObject;
    tmpOwner : TNode;
    tmpPos : Integer;
begin
  tmp:=TObject(Node.Data);

  ClearStatus;

  if tmp<>nil then
  begin
    LClass.Caption:=TASTUtils.RemoveTPrefix(tmp);

    if tmp is TNode then
    begin
      tmpOwner:=TNode(tmp).Owner;

      if tmpOwner<>nil then
      begin
        LOwner.Caption:=TASTUtils.RemoveTPrefix(tmpOwner)+': '+TASTUtils.NameOf(tmpOwner,True);
        LOwner.Tag:=NativeInt(tmpOwner);
      end;

      if Positions<>nil then
      begin
        tmpPos:=SelectPosition(TNode(tmp));

        if tmpPos<>-1 then
           OnTreeChange(Sender,Positions.Items[tmpPos]);
      end;
    end;
  end;
end;

procedure TFormAST.TryFillTree(const ATree: TVCLTreeAST;
                               const AParent: TWinControl;
                               const AModule:TType);
begin
  if ATree.Tree=nil then
  begin
    ATree.Tree:=CreateTree(Self,AParent);
    ATree.Tree.OnChange:=TreeASTChange;

    ATree.Tree.Items.BeginUpdate;
    try
      ATree.TreeModule(AModule,'',True);
    finally
      ATree.Tree.Items.EndUpdate;
    end;
  end
  else
    ATree.Tree.Parent:=AParent;
end;

class procedure TFormAST.TryFillTree(const ATree:TVCLTreeAST;
                                     const AOwner,AParent:TWinControl;
                                     const AModule:String; const FullAST:Boolean);
begin
  if ATree.Tree=nil then
  begin
    ATree.Tree:=CreateTree(AOwner,AParent);
    ATree.FillTree(AModule,FullAST);
  end
  else
    ATree.Tree.Parent:=AParent;
end;

function FindNodeInTree(const ATree:TTreeView; const AText:String):TTreeNode;

  function FindNode(const AItems:TTreeNodes; const AStart:Integer):TTreeNode;

    function TextIn(const ANode:TTreeNode):Boolean;
    begin
      result:=Pos(AText,UpperCase(ANode.Text))>0;

      if not result then
         if ANode.Data<>nil then
            result:=Pos(AText,UpperCase(TNode(ANode.Data).ClassName))>0;
    end;

    function FindNodeIn(const ANode:TTreeNode):TTreeNode;
    var t : Integer;
    begin
      if TextIn(ANode) then
         result:=ANode
      else
      begin
        result:=nil;

        for t:=0 to ANode.Count-1 do
        begin
          result:=FindNodeIn(ANode[t]);

          if result<>nil then
             break;
        end;
      end;
    end;

  var t : Integer;
  begin
    result:=nil;

    for t:=AStart to AItems.Count-1 do
    begin
      result:=FindNodeIn(AItems[t]);

      if result<>nil then
         break;
    end;
  end;

begin
  if ATree.Selected=nil then
     result:=FindNode(ATree.Items,0)
  else
     result:=FindNode(ATree.Items,ATree.Selected.AbsoluteIndex+1);
end;

procedure TFormAST.Search(S:String);
var tmp : TTreeNode;
begin
  S:=Trim(S);

  if S<>'' then
     if CBSearch.Items.IndexOf(S)=-1 then
        CBSearch.Items.Add(S);

  if Tree<>nil then
  begin
    tmp:=FindNodeInTree(Tree.Tree,UpperCase(S));

    if tmp<>nil then
       Tree.Tree.Selected:=tmp;
  end;
end;

procedure TFormAST.SBSearchClick(Sender: TObject);
begin
  Search(CBSearch.Text);
end;

// Test code, search for TDataCall with wrong owner node
procedure TFormAST.TestButtonClick(Sender: TObject);
var tmp : TTreeNode;
    N : TNode;
begin
  if Tree=nil then Exit;

  tmp:=Tree.Tree.Selected;

  if tmp=nil then
     tmp:=Tree.Tree.Items.GetFirstNode
  else
     tmp:=tmp.GetNext;

  while tmp<>nil do
  begin
    if tmp.Data<>nil then
    begin
      N:=TNode(tmp.Data);

      if N is TDataCall{Routine{TGenericType} then
         if N.Owner<>TNode(tmp.Parent.Data) then
         if tmp.Text<>'shared ' then

         begin
           Tree.Tree.Selected:=tmp;
           break;
         end;
    end;

    tmp:=tmp.GetNext;
  end;
end;

procedure TFormAST.CBSearchChange(Sender: TObject);
begin
  SBSearch.Enabled:=CBSearch.Text<>''
end;

procedure TFormAST.CBSearchKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then
     Search(CBSearch.Text);
end;

function TFormAST.GetAST(const AEditor:TCodeEditor): TVCLTreeAST;
var tmp : TVCLTreeAST;
begin
  tmp:=AEditor.Tree_AST;

  if TabTree.ControlCount>0 then
     if tmp.Tree=TabTree.Controls[0] then
        Exit(tmp);

  RemoveControls(TabTree);

  TryFillTree(tmp,Self,TabTree,AEditor.ModuleName,True);

  tmp.Tree.OnChange:=TreeASTChange;

  Tree:=tmp;

  Map.Clear;

  Positions:=AEditor.Positions;
  FillPositions;

  Usages:=AEditor.Usages;

  if Usages<>nil then
  begin
    FillUsed;
    References.Clear;
  end;

  FullText:=AEditor.Edit.Text;

  result:=tmp;
end;

function TFormAST.FindNode(const ANode:TNode):TTreeNode;
begin
  result:=Tree.Tree.Items.GetFirstNode;

  repeat
    if result.Data=ANode then
       Exit
    else
       result:=result.GetNext;

  until result=nil;
end;

procedure TFormAST.FormCreate(Sender: TObject);

  procedure SetupPositionsGrid;
  const DefaultColWidth=56;
  begin
    Map.RowCount:=1;
    Map.ColCount:=8;
    Map.FixedCols:=1;

    Map.Cells[0,0]:='#';
    Map.Cells[1,0]:='Line';
    Map.Cells[2,0]:='Column';
    Map.Cells[3,0]:='Position';
    Map.Cells[4,0]:='Length';
    Map.Cells[5,0]:='Text';
    Map.Cells[6,0]:='Class';
    Map.Cells[7,0]:='Style';

    Map.ColWidths[0]:=40;
    Map.ColWidths[1]:=DefaultColWidth;
    Map.ColWidths[2]:=DefaultColWidth;
    Map.ColWidths[3]:=DefaultColWidth;
    Map.ColWidths[4]:=DefaultColWidth;
    Map.ColWidths[5]:=130;
    Map.ColWidths[6]:=130;
    Map.ColWidths[7]:=80;
  end;

  procedure CreateMapGrid;
  begin
    Map:=TVidiGrid.Add(Self,TabMap);
  end;

  procedure CreateReferencesGrid;
  begin
    References:=TVidiGrid.Add(Self,TabReferences);
    References.Header([Vidi_Lang.Reference,Vidi_Lang._Class,Vidi_Lang.Line,Vidi_Lang.Column]);
  end;

  procedure CreateUsedGrid;
  begin
    Used:=TVidiGrid.Add(Self,PanelUsages);

    Used.Header([Vidi_Lang.Item,Vidi_Lang._Class,Vidi_Lang.Count]);

    Used.OnSelectCell:=UsedSelected;
  end;

begin
  CreateMapGrid;

  Map.OnSelectCell:=MapSelectCell;

  PageControl1.ActivePageIndex:=0;
  SetupPositionsGrid;

  CreateUsedGrid;
  CreateReferencesGrid;
end;

procedure TFormAST.GotoNode(const ANode:TNode);
var tmp : TTreeNode;
begin
  tmp:=FindNode(ANode);

  if tmp<>nil then
     Tree.Tree.Selected:=tmp;
end;

procedure TFormAST.LOwnerClick(Sender: TObject);
begin
  if LOwner.Tag<>0 then
     GotoNode(TNode(LOwner.Tag));
end;

procedure TFormAST.MapSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  CanSelect:=True;

  if (ARow>0) and (ARow<Length(Map.RowObjects)) then
     OnTreeChange(Sender,Positions.Items[Integer(Map.RowObjects[ARow])]);
end;

class function TFormAST.ModalShow(const AOwner: TComponent;
                                  const AContext: TType): Integer;
var UsageChecker : TUsageChecker;

  procedure CheckUsage(const AItems:TNodes);
  var N : TNode;
  begin
   for N in AItems do
       //if N is TComment then
       //else
       if N is TBlockStatement then
          CheckUsage(TBlockStatement(N).Block.Items)
       else
          UsageChecker.ProcessNode(N);
  end;

var tmp : TVCLTreeAST;
begin
  with TFormAST.Create(AOwner) do
  try
    tmp:=TVCLTreeAST.Create;
    try
      TryFillTree(tmp,TabTree,AContext);

      Usages:=TNodeUsages.Create;
      try
        UsageChecker.Usages:=Usages;
        //UsageChecker.DoError:=Error;

        CheckUsage(AContext.Items);

        result:=ShowModal;
      finally
        Usages.Free;
      end;
    finally
      tmp.Free;
    end;
  finally
    Free;
  end;
end;

function TFormAST.TextOf(const APosition:TNodePosition):String;
begin
  result:=OnGetText(Self,APosition);
end;

function StyleToString(const AStyle:TPositionStyle):String;
begin
  case AStyle of
    TPositionStyle.Identifier: result:='Identifier';
    TPositionStyle.Keyword: result:='Keyword';
    TPositionStyle.Symbol: result:='Symbol';
    TPositionStyle.Literal: result:='Literal';
  else
    result:='Comment';
  end;
end;

procedure TFormAST.FillPositions;

  function FilterPass(const P:TPositionStyle):Boolean;
  begin
    case CBFilter.ItemIndex of
      2: result:=P=TPositionStyle.Identifier;
      3: result:=P=TPositionStyle.Keyword;
      4: result:=P=TPositionStyle.Literal;
      5: result:=P=TPositionStyle.Symbol;
    else
      result:=P=TPositionStyle.Comment;
    end;
  end;

var t : Integer;
    P : TPosition;
    RowPos,
    tmpTotal : Integer;
    Filter : Boolean;
    PNode : TNodePosition;
begin
  if Positions=nil then
  begin
    Map.RowCount:=1;
    LCount.Caption:='Count: 0';
    Exit;
  end;

  Map.RowCount:=1+Length(Positions.Items);

  if Map.RowCount>1 then
     Map.FixedRows:=1;

  tmpTotal:=0;

  Filter:=CBFilter.ItemIndex>0;

  for t:=0 to Positions.Count-1 do
  begin
    if (not Filter) or FilterPass(Positions.Items[t].Style) then
    begin
      Inc(tmpTotal);

      PNode:=Positions.Items[t];

      P:=PNode.Position;

      RowPos:=tmpTotal;

      Map.Cells[0,RowPos]:=t.ToString;
      Map.Cells[1,RowPos]:=P.Line.ToString;
      Map.Cells[2,RowPos]:=P.Column.ToString;
      Map.Cells[3,RowPos]:=P.Position.ToString;
      Map.Cells[4,RowPos]:=Positions.Items[t].Length.ToString;
      Map.Cells[5,RowPos]:=TextOf(Positions.Items[t]);

      if Positions.Items[t].Node=nil then
         Map.Cells[6,RowPos]:=''
      else
         Map.Cells[6,RowPos]:=TASTUtils.RemoveTPrefix(Positions.Items[t].Node);

      Map.Cells[7,RowPos]:=StyleToString(Positions.Items[t].Style);

      Map.RowObjects[RowPos]:=TObject(t);
    end;
  end;

  Map.RowCount:=1+tmpTotal;

  LCount.Caption:='Count: '+IntToStr(tmpTotal);
end;

function TFormAST.ItemAt(const AIndex:Integer):TNodeUsage;
begin
  result:=Usages.Items[Sorted[AIndex]];
end;

procedure TFormAST.FillReferences(const AIndex:Integer);
var R : TNodeUsed;
    S : String;
    P : TNodePosition;
    tmpLine,
    tmpCol : String;
begin
  References.BeginUpdate;
  try
    References.Clear;

    for R in ItemAt(AIndex).References do
    begin
      S:=TASTUtils.NodeName(R.Node);

      if S='?' then
         S:='';

      if (Positions<>nil) and Positions.Find(R.Node,P) then
      begin
        tmpLine:=IntToStr(P.Position.Line);
        tmpCol:=IntToStr(P.Position.Column);
      end
      else
      begin
        tmpLine:='';
        tmpCol:='';
      end;

      References.AppendRow([S,TASTUtils.RemoveTPrefix(R.Node),tmpLine,tmpCol],R.Node);
    end;

  finally
    References.EndUpdate;
  end;
end;

function TFormAST.CalcNameOf(const ANode:TNode):String;
begin
  if ANode is TRangeType then
     result:=B(TRangeType(ANode).Range)
  else
  begin
    result:=TASTUtils.NodeName(ANode);

    if ANode is TSpecializedType then
       if TSpecializedType(ANode).Generics<>nil then
          result:=result+B(TSpecializedType(ANode).Generics);
  end;
end;

function TFormAST.Compare(const a,b:Integer):TComparison;
var tmpA,tmpB : String;
begin
  tmpA:=CalcNameOf(ItemAt(a).Node);
  tmpB:=CalcNameOf(ItemAt(b).Node);

  if tmpA<tmpB then result:=TComparison.Lower else
  if tmpA>tmpB then result:=TComparison.Greater else
                    result:=TComparison.Equal;
end;

procedure TFormAST.Swap(const a,b:Integer);
var tmp : Integer;
begin
  tmp:=Sorted[a];
  Sorted[a]:=Sorted[b];
  Sorted[b]:=tmp;
end;

procedure TFormAST.FillUsed;

  procedure DoSort;
  var t,L : Integer;
  begin
    L:=Length(Usages.Items);
    SetLength(Sorted,L);

    for t:=0 to L-1 do Sorted[t]:=t;

    TSort.Sort(True,0,L-1,Compare,Swap);
  end;

var tmp : TNodeUsage;
    tmpNode : TNode;
    t : Integer;
begin
  Used.BeginUpdate;
  try
    Used.Clear;

    DoSort;

    for t:=0 to High(Sorted) do
    begin
      tmp:=ItemAt(t);

      tmpNode:=tmp.Node;

      if tmpNode=nil then
          // Error !!
        Used.AppendRow(['nil','?'],nil)
      else
        Used.AppendRow([CalcNameOf(tmpNode),
                        TASTUtils.RemoveTPrefix(tmpNode),
                        IntToStr(Length(tmp.References))],
                       tmpNode);
    end;
  finally
    Used.EndUpdate;
  end;
end;

procedure TFormAST.UsedSelected(Sender: TObject; ACol, ARow: Longint;
    var CanSelect: Boolean);
begin
  CanSelect:=True;

  if Usages<>nil then
     if ARow>0 then
        FillReferences(ARow-1);
end;

procedure TFormAST.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage=TabMap then
  begin
    if Map.RowCount<=1 then
       FillPositions;
  end
  else
  if PageControl1.ActivePage=TabReferences then
  begin
    if Usages<>nil then
    begin
      FillUsed;
      References.Clear;
    end;
  end;
end;

procedure TFormAST.Reset(const APositions: TNodePositions;
                         const AUsages: TNodeUsages);
begin
  Positions:=APositions;
  FillPositions;

  Usages:=AUsages;
  FillUsed;
  References.Clear;
end;

end.
