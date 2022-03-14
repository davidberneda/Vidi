unit Search_VCL;

interface

uses
  {$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  Windows,
  //Messages,
  {$ENDIF}
  {$ENDIF}

  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,

  //Types_VCL,

  Search, Grid_VCL;

type
  TFormSearch = class(TForm)
    Panel2: TPanel;
    BSearch: TSpeedButton;
    SearchWord: TCheckBox;
    SearchCase: TCheckBox;
    CBSearch: TComboBox;
    CBSearchIn: TComboBox;
    Label1: TLabel;
    procedure BSearchClick(Sender: TObject);
    procedure CBSearchChange(Sender: TObject);
    procedure CBSearchCloseUp(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBSearchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }

    procedure AddFoundItems;
    procedure FreeFound;
  public
    { Public declarations }

    Grid : TVidiGrid;

    CurrentFile : String;
    Found: TFoundItems;
    FoundFile : TFileFoundItems;

    SearchPath : String;

    class function CreateEditorGrid(const AOwner:TComponent; const AParent:TWinControl):TVidiGrid; static;
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF FPC}
  LCLType,
  {$ENDIF}
  Module, Utils.AST, IDE_Lang_English, Files_FromDirectory, Constants;

procedure TFormSearch.AddFoundItems;
var Item : TFoundItem;
    FileItem : TFileFoundItem;
begin
  Grid.BeginUpdate;
  try
    Grid.Clear;

    if CBSearchIn.ItemIndex=0 then
       for Item in Found do
           Grid.AppendRow([ Item.Module.Name,
                            IntToStr(Item.Line),
                            IntToStr(Item.Column),
                            TASTUtils.NameOf(Item.Node,True)
                          ],Item)
    else
       for FileItem in FoundFile do
           Grid.AppendRow([ FileItem.Module,
                            IntToStr(FileItem.Line),
                            IntToStr(FileItem.Column),
                            FileItem.Text
                          ],FileItem)
  finally
    Grid.EndUpdate;
  end;
end;

procedure TFormSearch.BSearchClick(Sender: TObject);

  function SearchInSources(const ASearch:String):TFileFoundItems;
  var tmp : TStrings;
  begin
    tmp:=GetAllFilesFromDirRecursive(SearchPath,'',TVidiConstants.NoDot_Extension);
    try
      //AppendAllFilesFromDirRecursive(APath,'',TVidiConstants.NoDot_Extension);
      result:=TSearchFiles.Search(tmp,ASearch,SearchWord.Checked,SearchCase.Checked);
    finally
      tmp.Free;
    end;
  end;

var tmp : String;
    tmpCase : Boolean;
begin
  tmp:=CBSearch.Text;

  if tmp='' then
     Exit;

  if CBSearch.Items.IndexOf(tmp)=-1 then
     CBSearch.Items.Add(tmp);

  FreeFound;

  tmpCase:=SearchCase.Checked;

  if not tmpCase then
     tmp:=UpperCase(tmp);

  case CBSearchIn.ItemIndex of
    0 : Found:=TSearch.Search(tmp,SearchWord.Checked,tmpCase);

    1 : if SearchPath<>'' then
           FoundFile:=SearchInSources(tmp);
  else
    if CurrentFile<>'' then
    begin
      FoundFile:=nil;
      TSearchFiles.SearchIn(CurrentFile,tmp,SearchWord.Checked,tmpCase,FoundFile);
    end
  end;

  AddFoundItems;
end;

procedure TFormSearch.FreeFound;
var I : TFoundItem;
    F : TFileFoundItem;
begin
  for I in Found do
      I.Free;

  Found:=nil;

  for F in FoundFile do
      F.Free;

  FoundFile:=nil;
end;

procedure TFormSearch.CBSearchChange(Sender: TObject);
begin
  BSearch.Enabled:=Trim(CBSearch.Text)<>''
end;

procedure TFormSearch.CBSearchCloseUp(Sender: TObject);
begin
  CBSearchChange(Self);
end;

procedure TFormSearch.CBSearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then
  begin
    Key:=0;

    BSearchClick(Self);
  end;
end;

class function TFormSearch.CreateEditorGrid(const AOwner:TComponent; const AParent:TWinControl):TVidiGrid;
begin
  result:=TVidiGrid.Add(AOwner,AParent);

  result.Header([Vidi_Lang.Module,Vidi_Lang.Line,Vidi_Lang.Column,Vidi_Lang.Text]);

  result.ColWidths[0]:=150;
  result.ColWidths[1]:=50;
  result.ColWidths[2]:=50;
  result.ColWidths[3]:=350;
end;

procedure TFormSearch.FormCreate(Sender: TObject);
begin
  Grid:=TFormSearch.CreateEditorGrid(Self,Self);
end;

procedure TFormSearch.FormDestroy(Sender: TObject);
begin
  FreeFound;
end;

end.
