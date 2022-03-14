unit Watch_VCL;

interface

uses
  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, Buttons, Menus,

  Sys, Bee.Parser,
  Evaluator, Instance_Type, Grid_VCL;

type
  TWatchItem=record
  public
    Data : TData;
    Expression : String;
  end;

  TFormWatch = class(TForm)
    Splitter1: TSplitter;
    Panel1: TPanel;
    EWatch: TEdit;
    SbAdd: TSpeedButton;
    PopupMenu1: TPopupMenu;
    Refresh1: TMenuItem;
    procedure EWatchChange(Sender: TObject);
    procedure SbAddClick(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure Refresh1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    Watch,
    Fields : TVidiGrid;

    procedure WatchClick(Sender: TObject);
  public
    { Public declarations }

    Items : Array of TWatchItem;
    Instance : TInstance;

    procedure Fill(const AGrid:TVidiGrid; const AInstance: TInstance); overload;
    procedure Fill(const AInstance:TInstance); overload;
  end;

implementation

{$R *.dfm}

uses
  AST,

  {$IFDEF INTERNAL}
  Internal,
  {$ENDIF}

  Syntax, Checker.AST, Utils.AST, IDE_Lang_English;

{
function EmitAsString(const ANode:TNode):String;
var tmp : TVidiEmit;
begin
  tmp:=TVidiEmit.Create;
  try
    result:=tmp.AsString(ANode);
  finally
    tmp.Free;
  end;
end;
}

{ TFormWatch }

procedure TFormWatch.Fill(const AGrid:TVidiGrid; const AInstance: TInstance);

  function AsText(const AVariable:TVariable):String;
  begin
    if TChecker.IsBasicType(AVariable.VariableType) then
       result:=TEvaluate.AsText(AVariable,AInstance)
    else
    if TChecker.VariableIsClass(AVariable) then
       result:=TClassType(AVariable.VariableType).Name
    else
       result:=TEvaluate.AsText(AVariable,AInstance);
  end;

  function WatchAsText(const ANode:TNode):String;
  begin
    if ANode is TTypeCall then
       result:=WatchAsText(TTypeCall(ANode).TheType)
    else
    if ANode is TNamedType then
       result:=TNamedType(ANode).Name
    else
    if ANode is TInstance then
       result:=WatchAsText(TInstance(ANode).Value)
    else
    if ANode is TData then
       result:=Runner_DataAsText(TData(ANode))
    else
       result:=ANode.ClassName+'?';
  end;

  procedure Add(const AInstance: TInstance);
  var t : Integer;
      tmp : TVariable;
      tmpName,
      tmpType,
      tmpText : String;
      tmpValue : TInstance;
  begin
    for t:=Low(AInstance.Data) to High(AInstance.Data) do
    begin
      tmp:=AInstance.Data[t].Variable;

      if tmp=nil then
      begin
        tmpName:='?';
        tmpText:=Runner_DataAsText(AInstance.Data[t].Value.Value);
        tmpType:='?';
      end
      else
      begin
        tmpName:=tmp.Name;

        tmpType:=TASTUtils.NodeName(tmp.VariableType);

        tmpValue:=AInstance.Data[t].Value;

        if (tmpValue=nil) or
           (tmpValue.Value=nil) then
           tmpText:='' //AsText(tmp)
        else
           tmpText:=WatchAsText(tmpValue.Value);
      end;

      AGrid.AppendRow([tmpName,tmpType,tmpText],tmp);
    end;
  end;

var Old : TVariable;
    tmp : TInstance;
    tmpRow : Integer;
begin
  if AGrid.Row=0 then
     Old:=nil
  else
     Old:=TVariable(AGrid.RowObjects[AGrid.Row-1]);

  AGrid.BeginUpdate;
  try
    AGrid.Clear;

    tmp:=AInstance;

    while tmp<>nil do
    begin
      Add(tmp);
      tmp:=tmp.Owner as TInstance;
    end;

    if Old<>nil then
    begin
      tmpRow:=AGrid.IndexOfObject(Old);

      if tmpRow>-1 then
         AGrid.Row:=tmpRow;
    end;
  finally
    AGrid.EndUpdate;
  end;
end;

procedure TFormWatch.EWatchChange(Sender: TObject);
begin
  SBAdd.Enabled:=EWatch.Text<>'';
end;

procedure TFormWatch.Fill(const AInstance: TInstance);
begin
  Instance:=AInstance;

  Fields.Clear;

  Fill(Watch,AInstance);

  if Watch.Row>0 then
     WatchClick(Self);
end;

procedure TFormWatch.FormCreate(Sender: TObject);
begin
  Watch:=TVidiGrid.Add(Self,Self);
  Watch.Align:=TAlign.alLeft;
  Watch.Width:=210;
  Watch.Header([Vidi_Lang.Name,Vidi_Lang._Type,Vidi_Lang.Value]);

  Watch.OnClick:=WatchClick;

  Watch.PopupMenu:=PopupMenu1;

  Fields:=TVidiGrid.Add(Self,Self);
  Fields.Header([Vidi_Lang.Name,Vidi_Lang._Type,Vidi_Lang.Value]);

  Splitter1.Align:=TAlign.alRight;
  Splitter1.Align:=TAlign.alLeft;
end;

procedure TFormWatch.WatchClick(Sender: TObject);
var tmp : TVariable;
//    t : Integer;
    tmpInst : TInstance;
begin
  if Instance=nil then
     Exit;

  if Watch.Row<1 then
     Exit;

  tmp:=TVariable(Watch.RowObjects[Watch.Row]);

  {$IFDEF INTERNAL}
  if tmp=nil then
     InternalError('Watch variable is nil',nil);
  {$ENDIF}

  tmpInst:=TEvaluate.InstanceOf(Instance,tmp);

  //t:=Instance.Data.Find(tmp);
  //if t<>-1 then

  if tmpInst<>nil then
     Fill(Fields,tmpInst {Instance.Data[t].Value});
//  TFormTypes.FillFields(LBFields,tmp,True);
end;

procedure TFormWatch.Panel1Resize(Sender: TObject);
begin
  SBAdd.Left:=Panel1.Width-9-SBAdd.Width;
  EWatch.Width:=SBAdd.Left-18;
end;

procedure TFormWatch.Refresh1Click(Sender: TObject);
begin
  Fill(Instance);
end;

procedure TFormWatch.SbAddClick(Sender: TObject);
var tmp : TData;
    L : Integer;
begin
  tmp:=TBee.ParseExpression(nil,EWatch.Text);

  if tmp<>nil then
  begin
    L:=Length(Items);
    SetLength(Items,L+1);

    Items[L].Expression:=EWatch.Text;
    Items[L].Data:=tmp;

    Watch.AppendRow([EWatch.Text,TEvaluate.AsText(tmp,Instance)],tmp);
  end;
end;

end.
