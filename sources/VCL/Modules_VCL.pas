unit Modules_VCL;

interface

uses
  {$IFNDEF FPC}
  Windows,
  {$ENDIF}

  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Menus, Buttons,
  {$IFNDEF FPC}
  Types,
  {$ENDIF}

  Bee.Parser, Sys, Exceptions, Map, AST, Module_Usage_VCL, Grid_VCL, Grids;

type
  TParsedModule=class
  public
    Module : TNode;
    ModuleName:String;
    Path : String;
    Errors : TNodeErrors;
    Positions : TNodePositions;

    Destructor Destroy; override;
  end;

  TOnCloseModule=procedure(Sender: TObject; const ANode:TNode) of object;
  TOnParsedModule=procedure(Sender: TObject; const AModule:TParsedModule) of object;
  TOnReplaceErrors=procedure(Sender: TObject; {const AModule:TParsedModule;} const AParser:TBee) of object;

  TFormModules = class(TForm)
    Splitter4: TSplitter;
    Panel1: TPanel;
    LModulePath: TLabel;
    PopupModules: TPopupMenu;
    CloseModule: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Folderlocation1: TMenuItem;
    Panel2: TPanel;
    CBSearch: TComboBox;
    BSearch: TSpeedButton;
    PanelUsages: TPanel;
    LModuleCount: TLabel;
    procedure CloseModuleClick(Sender: TObject);
    procedure LBModulesClick(Sender: TObject);
    procedure LBModulesDblClick(Sender: TObject);
    procedure PopupModulesPopup(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Folderlocation1Click(Sender: TObject);
    procedure BSearchClick(Sender: TObject);
    procedure CBSearchChange(Sender: TObject);
    procedure CBSearchCloseUp(Sender: TObject);
    procedure CBSearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure LModulePathClick(Sender: TObject);
  private
    { Private declarations }

    procedure ClearModules;
    function DrawGridCell(ACol, ARow: Longint; ARect: TRect;
                          AState: TGridDrawState):Boolean;
    function IndexOfModule(const AModule:TType):Integer; overload;
    function IndexOfModule(const AModule:String):Integer; overload;
    procedure GotoModule(Sender:TObject; const AModule:TType);
    function ParsedModule(const AIndex:Integer):TParsedModule;
  public
    { Public declarations }

    Items : TVidiGrid;

    ModuleUsages : TFormModuleUsage;

    OnReplaceErrors:TOnReplaceErrors;
    OnCloseModule:TOnCloseModule;
    OnOpenModule:TOnParsedModule;

    procedure AddModule(const AParser:TBee; const ANode:TNode);
    procedure Fill;
    function GetErrors(const AName:String):TNodeErrors;
    procedure Removed(const AModule:TNode);
    function TryOpen(const AName:String):Boolean;
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF FPC}
  LCLType,
  {$ENDIF}
  Module,
  Utils_VCL, IDE_Lang_English;

// From 0 to RowCount-2
function TFormModules.IndexOfModule(const AModule:String):Integer;
var t : Integer;
begin
  for t:=1 to Items.RowCount-1 do
      if Items.Cells[0,t]=AModule then
         Exit(t-1);

  result:=-1;
end;

procedure TFormModules.AddModule(const AParser:TBee; const ANode:TNode);

  procedure SetParsedModule(const P:TParsedModule);
  begin
    P.Module:=ANode;
    P.Positions:=AParser.Positions;

    if Assigned(OnReplaceErrors) then
       OnReplaceErrors(Self,{P,}AParser);

    P.Errors.Clear;
    P.Errors:=AParser.Errors;

    AParser.Errors:=nil;
  end;

var tmp : Integer;
    P : TParsedModule;
begin
  tmp:=IndexOfModule(AParser.ModuleName);

  if tmp=-1 then
  begin
    P:=TParsedModule.Create;

    P.ModuleName:=AParser.ModuleName;
    P.Path:=AParser.ModulePath;

    SetParsedModule(P);

    Items.AppendRow([P.ModuleName,P.Path],P);
  end
  else
  begin
    P:=ParsedModule(tmp+1);

    SetParsedModule(P);

    Items.Invalidate;

    if Items.Row=tmp+1 then
       LBModulesClick(Self);
  end;
end;

procedure TFormModules.BSearchClick(Sender: TObject);
var S : String;
    t : Integer;
begin
  S:=UpperCase(CBSearch.Text);

  for t:=1 to Items.RowCount-1 do
      if Pos(S,UpperCase(Items.Cells[0,t]))>0 then
      begin
        Items.Row:=t;
        break;
      end;
end;

procedure TFormModules.CBSearchChange(Sender: TObject);
begin
  BSearch.Enabled:=Trim(CBSearch.Text)<>''
end;

procedure TFormModules.CBSearchCloseUp(Sender: TObject);
begin
  CBSearchChange(Self);
end;

procedure TFormModules.CBSearchKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then
     BSearchClick(Self);
end;

procedure TFormModules.CloseModuleClick(Sender: TObject);
var P : TParsedModule;
    tmp : Integer;
begin
  tmp:=Items.Row;

  P:=ParsedModule(tmp);

  if P.Module<>nil then
  begin
    OnCloseModule(Self,P.Module);
    Modules.Remove(P.Module as TType);
  end;

  P.Free;

  Items.DeleteRow(tmp);

  if Items.RowCount>1 then
     LModulePath.Caption:=''
  else
  begin
    if tmp=0 then
       Items.Row:=0
    else
       Items.Row:=tmp;

    LBModulesClick(Self);
  end;
end;

procedure TFormModules.Fill;

  // Remove modules not in cache
  procedure RemoveMissing;
  var t : Integer;
  begin
    t:=1;

    while t<Items.RowCount do
      if TBee.FindModule(Items.Cells[0,t])=nil then
      begin
        ParsedModule(t).Free;
        Items.DeleteRow(t);
      end
      else
         Inc(t);
  end;

  // Add modules not in list
  procedure AddNew;
  var t : Integer;
      P : TParsedModule;
      tmp : Integer;
  begin
    for t:=0 to High(Modules.Items) do
    begin
      // TODO: Problem with duplicate same-name modules at different paths !
      tmp:=IndexOfModule(Modules.Items[t].Name);

      if tmp=-1 then
      begin
        P:=TParsedModule.Create;
        P.ModuleName:=Modules.Items[t].Name;

        P.Module:=Modules.Items[t];

        Items.AppendRow([P.ModuleName,P.Path],P);
      end;
    end;
  end;

begin
  Items.BeginUpdate;
  try
    RemoveMissing;
    AddNew;

    LModuleCount.Caption:='Count: '+IntToStr(Length(Modules.Items));
  finally
    Items.EndUpdate;
  end;
end;

procedure TFormModules.Folderlocation1Click(Sender: TObject);
var P : TParsedModule;
begin
  P:=ParsedModule(Items.Row);
  TGoto.Folder(P.Path);
end;

procedure TFormModules.FormCreate(Sender: TObject);

  procedure CreateModulesGrid;
  begin
    Items:=TVidiGrid.Add(Self,Self);
    Items.Header([Vidi_Lang.Module,Vidi_Lang.Folder]);

    Items.PopupMenu:=PopupModules;
    Items.OnClick:=LBModulesClick;
    Items.OnDblClick:=LBModulesDblClick;

    Items.ColWidths[0]:=100;
    Items.ColWidths[1]:=300;

    Items.OnDrawCell:=DrawGridCell;
  end;

begin
  CreateModulesGrid;

  ModuleUsages:=TFormModuleUsage.Create(Self);
  AddForm(ModuleUsages,PanelUsages);

  ModuleUsages.OnGotoModule:=GotoModule;

  ModuleUsages.Show;
end;

procedure TFormModules.ClearModules;
var t : Integer;
begin
  for t:=1 to Items.RowCount-1 do
      ParsedModule(t).Free;

  LModulePath.Caption:='';
end;

procedure TFormModules.FormDestroy(Sender: TObject);
begin
  ClearModules;
end;

// From 0 to RowCount-2
function TFormModules.IndexOfModule(const AModule:TType):Integer;
var t : Integer;
begin
  for t:=1 to Items.RowCount-1 do
      if ParsedModule(t).Module=AModule then
         Exit(t-1);

  result:=-1;
end;

function TFormModules.GetErrors(const AName: String): TNodeErrors;
var tmp : Integer;
begin
  tmp:=IndexOfModule(AName);
  result:=ParsedModule(tmp+1).Errors;
end;

procedure TFormModules.GotoModule(Sender:TObject; const AModule:TType);
begin
  Items.Row:=IndexOfModule(AModule)+1;
  LBModulesClick(Self);
end;

function TFormModules.ParsedModule(const AIndex:Integer):TParsedModule;
begin
  result:=TParsedModule(Items.RowObjects[AIndex]);
end;

procedure TFormModules.PopupModulesPopup(Sender: TObject);
var tmp : Boolean;
begin
  tmp:=Items.Row>0;

  Open1.Enabled:=tmp;
  CloseModule.Enabled:=tmp;
  Folderlocation1.Enabled:=tmp;
end;

procedure TFormModules.Removed(const AModule: TNode);
var t : Integer;
    P : TParsedModule;
begin
  for t:=1 to Items.RowCount-1 do
  begin
    P:=ParsedModule(t);

    if P.Module=AModule then
    begin
      P.Module:=nil;
      Items.Invalidate;
      break;
    end;
  end;

end;

procedure TFormModules.LBModulesClick(Sender: TObject);
var I : Integer;
    P : TParsedModule;
begin
  I:=Items.Row;

  if I<1 then
  begin
    LModulePath.Caption:='';

    ModuleUsages.Clear;
  end
  else
  begin
    P:=ParsedModule(I);
    LModulePath.Caption:=P.Path;

    ModuleUsages.Fill(P.Module);
  end;
end;

procedure TFormModules.LBModulesDblClick(Sender: TObject);
begin
  Open1Click(Self);
end;

function TFormModules.DrawGridCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState):Boolean;
var tmp : TParsedModule;
begin
  result:=True;

  if (ACol=0) and (ARow>0) then
  begin
    tmp:=ParsedModule(ARow);

    if (tmp.Module=nil) or (tmp.Errors<>nil) then
       if gdSelected in AState then
          Items.Canvas.Font.Color:=clBlack
       else
          Items.Canvas.Font.Color:=clRed;
  end;
end;

procedure TFormModules.LModulePathClick(Sender: TObject);
begin
  if LModulePath.Caption<>'' then
     Folderlocation1Click(Self);
end;

procedure TFormModules.Open1Click(Sender: TObject);
begin
  if Items.Row>0 then
     OnOpenModule(Self,ParsedModule(Items.Row));
end;

function TFormModules.TryOpen(const AName:String):Boolean;
var tmp : Integer;
begin
  tmp:=IndexOfModule(AName);

  result:=tmp<>-1;

  if result then
     OnOpenModule(Self,ParsedModule(tmp+1));
end;

{ TParsedModule }

destructor TParsedModule.Destroy;
begin
  Errors.Clear;
  inherited;
end;

end.
