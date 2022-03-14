unit RecentFiles_VCL;

interface

uses
  {$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  Windows, Messages,
  {$ENDIF}
  {$ENDIF}

  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Grid_VCL, Menus;

type
  TFormRecent = class(TForm)
    PanelBottom: TPanel;
    Panel2: TPanel;
    BOK: TButton;
    BCancel: TButton;
    Panel3: TPanel;
    LFilter: TLabel;
    EFilter: TEdit;
    CBViewFolders: TCheckBox;
    PopupMenuGrid: TPopupMenu;
    LocateFolder1: TMenuItem;
    Remove1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure EFilterChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EFilterKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CBViewFoldersClick(Sender: TObject);
    procedure LocateFolder1Click(Sender: TObject);
    procedure Remove1Click(Sender: TObject);
    procedure PopupMenuGridPopup(Sender: TObject);
  private
    { Private declarations }

    class var
       FRecentFiles : TStrings;

    Grid : TVidiGrid;

    procedure AddRows(const AFilter:String; const S:TStrings);
    procedure DoClicked(Sender: TObject);
    procedure DoSelected(Sender: TObject);
    class procedure SaveRecent; static;
  public
    { Public declarations }

    OnSelected : TNotifyEvent;

    class function RecentFiles:TStrings; static;
    class procedure AddFile(const AFile:String); static;
    class function Select(const AOwner:TComponent):String; static;

    procedure RefreshList;
    function SelectedFile:String;
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF FPC}
  LCLType,
  {$ENDIF}
  IO, Constants, IDE_Lang_English, Utils_VCL;

const
  Default_FoldersWidth=350;

function RecentFiles_File:String;
const
  TextFile='Vidi Recent Files.txt';

var tmp : String;
begin
  tmp:=VidiHomePath+PathDelimiter+TVidiConstants.Vidi_Name;

  ForceDirectories(tmp);

  result:=CombineFile(tmp,TextFile);
end;

{ TFormRecent }

procedure TFormRecent.DoClicked(Sender: TObject);
begin
  BOK.Enabled:=Grid.Row>0;
end;

procedure TFormRecent.DoSelected(Sender: TObject);
begin
  if Assigned(OnSelected) then
     OnSelected(Self)
  else
     ModalResult:=mrOk;
end;

procedure TFormRecent.EFilterChange(Sender: TObject);
begin
  AddRows(UpperCase(EFilter.Text),RecentFiles);
end;

procedure TFormRecent.EFilterKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then
     if Grid.RowCount=2 then
     begin
       Grid.Row:=1;
       ModalResult:=mrOk;
     end;
end;

procedure TFormRecent.FormCreate(Sender: TObject);
begin
  Grid:=TVidiGrid.Add(Self,Self);

  Grid.Header([Vidi_Lang.Name,Vidi_Lang.Path,Vidi_Lang.Found]);

  Grid.ColWidths[0]:=150;
  Grid.ColWidths[1]:=Default_FoldersWidth;
  Grid.ColWidths[2]:=50;

  Grid.OnClick:=DoClicked;
  Grid.OnDblClick:=DoSelected;

  Grid.EnableRowSelect;

  Grid.PopupMenu:=PopupMenuGrid;
end;

procedure TFormRecent.RefreshList;
begin
  AddRows('',RecentFiles);
end;

procedure TFormRecent.Remove1Click(Sender: TObject);
var tmp : Integer;
begin
  tmp:=Grid.Row;
  RecentFiles.Delete(tmp-1);
  SaveRecent;

  Grid.DeleteRow(tmp);

  if Grid.RowCount>tmp then
     Grid.Row:=tmp;
end;

procedure TFormRecent.FormShow(Sender: TObject);
begin
  RefreshList;
end;

procedure TFormRecent.LocateFolder1Click(Sender: TObject);
begin
  if Grid.Row>0 then
     TGoto.ToFile(SelectedFile+TVidiConstants.Extension);
end;

procedure TFormRecent.PopupMenuGridPopup(Sender: TObject);
begin
  LocateFolder1.Enabled:=Grid.Row>0;
  Remove1.Enabled:=LocateFolder1.Enabled;
end;

class procedure TFormRecent.AddFile(const AFile: String);

  function Found(const S:String):Integer;
  var t : Integer;
  begin
    for t:=0 to RecentFiles.Count-1 do
        if SameText(FRecentFiles[t],S) then
           Exit(t);

    result:=-1;
  end;

  // Maximum 1000 recent files
  procedure DeleteExcess;
  begin
    while RecentFiles.Count>1000 do
          FRecentFiles.Delete(FRecentFiles.Count-1);
  end;

var tmp : String;
    t : Integer;
begin
  tmp:=Trim(AFile); // <-- TODO: make absolute path

  t:=Found(tmp);

  if t<>0 then
  begin
    if t=-1 then
    begin
      DeleteExcess;
      FRecentFiles.Insert(0,tmp);
    end
    else
    begin
      // Move up !
      FRecentFiles.Insert(0,FRecentFiles[t]);
      FRecentFiles.Delete(t+1);
    end;

    SaveRecent;
  end;
end;

class procedure TFormRecent.SaveRecent;
begin
  FRecentFiles.SaveToFile(RecentFiles_File);
end;

procedure TFormRecent.AddRows(const AFilter:String; const S:TStrings);

  function MatchFilter(const AItem:String):Boolean;
  begin
    result:=Pos(AFilter,UpperCase(AItem))>0;
  end;

var tmp : String;
    t : Integer;
begin
  Grid.BeginUpdate;
  try
    Grid.Clear;

    for t:=0 to S.Count-1 do
    begin
      tmp:=S[t];

      if (AFilter='') or MatchFilter(tmp) then

      Grid.AppendRow([ RemoveExtension(ExtractFileName(tmp)),
                       CorrectPath(ExtractFilePath(tmp)),
                       BoolToStr(DoFileExists(tmp),True)
                          ],nil);
    end;
  finally
    Grid.EndUpdate;
  end;
end;

procedure TFormRecent.CBViewFoldersClick(Sender: TObject);
begin
  if CBViewFolders.Checked then
     Grid.ColWidths[1]:=Default_FoldersWidth
  else
     Grid.ColWidths[1]:=0;
end;

class function TFormRecent.RecentFiles: TStrings;
var tmpName : String;
begin
  if TFormRecent.FRecentFiles=nil then
  begin
    TFormRecent.FRecentFiles:=TStringList.Create;

    tmpName:=RecentFiles_File;

    if DoFileExists(tmpName) then
       TFormRecent.FRecentFiles.LoadFromFile(tmpName);
  end;

  result:=TFormRecent.FRecentFiles;
end;

class function TFormRecent.Select(const AOwner:TComponent): String;
begin
  with TFormRecent.Create(AOwner) do
  try
    if ShowModal=mrOk then
       result:=SelectedFile
    else
       result:='';
  finally
    Free;
  end;
end;

function TFormRecent.SelectedFile: String;
begin
  if Grid.Row>0 then
     result:=CombineFile(Grid.Cells[1,Grid.Row],Grid.Cells[0,Grid.Row])
  else
     result:='';
end;

initialization
finalization
  TFormRecent.FRecentFiles.Free;
end.
