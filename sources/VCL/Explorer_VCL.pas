unit Explorer_VCL;

interface

uses
  {$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  Windows, Messages,
  {$ENDIF}
  {$ENDIF}

  Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, ExtCtrls,
  {$IFNDEF FPC}
  ImageList, ImgList,
  {$ENDIF}
  Buttons, Menus;

type
  TOnOpenFileEvent=procedure(Sender:TObject; const APath:String) of object;

  TFormExplorer = class(TForm)
    Panel1: TPanel;
    CBRoot: TComboBox;
    Tree1: TTreeView;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    PopupFolder: TPopupMenu;
    New1: TMenuItem;
    Folder1: TMenuItem;
    Vidimodule1: TMenuItem;
    ShowinExplorer1: TMenuItem;
    PopupModule: TPopupMenu;
    Open1: TMenuItem;
    Locate1: TMenuItem;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure CBRootKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CBRootCloseUp(Sender: TObject);
    procedure Tree1DblClick(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Folder1Click(Sender: TObject);
    procedure Tree1Change(Sender: TObject; Node: TTreeNode);
    procedure ShowinExplorer1Click(Sender: TObject);
    procedure Vidimodule1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Locate1Click(Sender: TObject);
    procedure PopupFolderPopup(Sender: TObject);
    procedure PopupModulePopup(Sender: TObject);
  private
    { Private declarations }

    const
      ParentFolder='..';
      Folder_Tag=1;

    function AddTreeFolder(const AParent:TTreeNode; const AName:String):TTreeNode;
    function AddTreeModule(const AParent:TTreeNode; const AName:String):TTreeNode;

    function FullPathOf(const ANode:TTreeNode):String;
    function IndexOfPath(const APath:String):Integer;
    function SelectedFolder:String;
    procedure TryAddPath(const APath:String);
    procedure TryRefreshExplorer(const ARoot:String);
  public
    { Public declarations }

    Extension,
    Root : String;

    OnOpenFile : TOnOpenFileEvent;

    procedure Init(const ARoot:String);
    procedure RefreshExplorer(const ARoot:String);
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF FPC}
  LCLType,
  {$ENDIF}
  IO, Constants, Utils_VCL,
  {$IFNDEF FPC}
  FileCtrl,  // <-- SelectDirectory
  {$ENDIF}
  SysUtils;

function TFormExplorer.IndexOfPath(const APath:String):Integer;
var t : Integer;
begin
  for t:=0 to CBRoot.Items.Count-1 do
      if SameText(APath,CBRoot.Items[t]) then // TODO: match path folders (ExpandFileName !)
         Exit(t);

  result:=-1;
end;

procedure TFormExplorer.Init(const ARoot: String);
begin
  CBRoot.Text:=ARoot;
  RefreshExplorer(CBRoot.Text);
end;

procedure TFormExplorer.Locate1Click(Sender: TObject);
begin
  TGoto.ToFile(TryAddExtension(FullPathOf(Tree1.Selected),TVidiConstants.Extension))
end;

procedure TFormExplorer.Open1Click(Sender: TObject);
begin
  Tree1DblClick(Self);
end;

procedure TFormExplorer.Panel1Resize(Sender: TObject);
var tmp : Integer;
begin
  tmp:=Panel1.Width-Panel2.Width-20;

  if tmp<20 then
     tmp:=20;

  CBRoot.Width:=tmp;
end;

procedure TFormExplorer.PopupFolderPopup(Sender: TObject);
begin
  ShowinExplorer1.Enabled:=Tree1.Selected<>nil;
end;

procedure TFormExplorer.PopupModulePopup(Sender: TObject);
begin
  Open1.Enabled:=Tree1.Selected<>nil;
  Locate1.Enabled:=Open1.Enabled;
end;

function PathOf(ANode:TTreeNode):String;
begin
  result:=ANode.Text;

  while ANode.Parent<>nil do
  begin
    ANode:=ANode.Parent;

    if ANode.Text=TFormExplorer.ParentFolder then
       break
    else
       result:=CombineFile(ANode.Text,result);
  end;
end;

function IsFolder(const ANode:TTreeNode):Boolean;
begin
  result:=(ANode<>nil) and
          (NativeUInt(ANode.Data)=TFormExplorer.Folder_Tag);
end;

procedure TFormExplorer.Tree1Change(Sender: TObject; Node: TTreeNode);
begin
  if IsFolder(Node) then
     Tree1.PopupMenu:=PopupFolder
  else
     Tree1.PopupMenu:=PopupModule;
end;

procedure TFormExplorer.Tree1DblClick(Sender: TObject);

  procedure GoParent;
  var tmp : String;
      i : Integer;
  begin
    tmp:=Trim(CBRoot.Text);
    i:=LastDelimiter(PathDelimiter,tmp);

    if i>0 then
       Init(Copy(tmp,1,i-1));
  end;

var tmpNode : TTreeNode;
begin
  tmpNode:=Tree1.Selected;

  if tmpNode<>nil then
     if tmpNode.Text=ParentFolder then
        GoParent
     else
     if not IsFolder(tmpNode) then
        if Assigned(OnOpenFile) then
           OnOpenFile(Self,FullPathOf(tmpNode));
end;

function TFormExplorer.FullPathOf(const ANode:TTreeNode):String;
begin
  result:=CombineFile(Root,PathOf(ANode));
end;

procedure TFormExplorer.TryAddPath(const APath:String);
var tmp : Integer;
begin
  tmp:=IndexOfPath(APath);

  if tmp=-1 then
     CBRoot.Items.Add(APath);
end;

procedure TFormExplorer.CBRootCloseUp(Sender: TObject);
begin
  TryRefreshExplorer(CBRoot.Text);
end;

procedure TFormExplorer.CBRootKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then
     TryRefreshExplorer(CBRoot.Text);
end;

function TFormExplorer.SelectedFolder:String;
var tmp : TTreeNode;
begin
  result:='';

  tmp:=Tree1.Selected;

  if tmp<>nil then
     if IsFolder(tmp) then
        result:=FullPathOf(tmp);
end;

procedure TFormExplorer.ShowinExplorer1Click(Sender: TObject);
begin
  TGoto.Folder(FullPathOf(Tree1.Selected))
end;

function NewPathName(const IsFolder:Boolean; const APrefix,AParent,AExtension:String):String;
var tmp : Integer;
    tmpPath : String;
    tmpExists : Boolean;
begin
  tmp:=1;

  repeat
    result:=APrefix+IntToStr(tmp)+AExtension;

    tmpPath:=CombineFile(AParent,result);

    if IsFolder then
       tmpExists:=DirectoryExists(tmpPath)
    else
       tmpExists:=DoFileExists(tmpPath);

    if tmpExists then
       Inc(tmp)
    else
       break;

  until False;
end;

procedure TFormExplorer.Folder1Click(Sender: TObject);
var tmpName,
    tmpPath,
    tmpDir : String;
begin
  tmpPath:=SelectedFolder;

  if tmpPath<>'' then
  begin
    tmpName:=NewPathName(True,'Folder ',tmpPath,'');

    if InputQuery('New folder','Folder name',tmpName) then
    begin
      tmpName:=Trim(tmpName);

      if tmpName<>'' then
      begin
        tmpDir:=CombineFile(tmpPath,tmpName);

        if DirectoryExists(tmpDir) then
           ShowMessage('Folder already exists: '+tmpName)
        else
        begin
          if CreateDir(tmpDir) then
             AddTreeFolder(Tree1.Selected,tmpName).Selected:=True
          else
             ShowMessage('Cannot create folder: '+tmpName);

        end;
      end;
    end;
  end;
end;

procedure TFormExplorer.FormCreate(Sender: TObject);
begin
  Tree1.ReadOnly:=True;
  Tree1.HideSelection:=False;
  Tree1.HotTrack:=True;

  Extension:=TVidiConstants.Extension;
end;

function TFormExplorer.AddTreeFolder(const AParent:TTreeNode; const AName:String):TTreeNode;
begin
  result:=Tree1.Items.AddChild(AParent,AName);
  result.Data:=TObject(Folder_Tag);
  result.ImageIndex:=0;
  result.SelectedIndex:=0;
end;

function TFormExplorer.AddTreeModule(const AParent:TTreeNode; const AName:String):TTreeNode;
begin
  result:=Tree1.Items.AddChild(AParent,AName);
  result.ImageIndex:=1;
  result.SelectedIndex:=1;
end;

procedure TFormExplorer.RefreshExplorer(const ARoot:String);

  function IsExtension(const S:String):Boolean;
  var tmp : String;
  begin
    tmp:=ExtractFileExt(S);
    result:=SameText(tmp,Extension);
  end;

  procedure SearchPath(const AParent:TTreeNode; const APath:String);
  var f : TSearchRec;
      tmpNode : TTreeNode;
  begin
    if FindFirst(APath+PathDelimiter+AllFiles,faAnyFile,f)=0 then
    begin
      Repeat
        if (f.Attr and faDirectory)=faDirectory then
        begin
          if (f.Name<>'.') and (f.Name<>ParentFolder) then
          begin
            //if (ExcludeDir='') or (Pos(UpperCase(ExcludeDir),UpperCase(f.Name))=0) then
            begin
              tmpNode:=AddTreeFolder(AParent,f.Name);

              SearchPath(tmpNode,CombineFile(APath,f.Name));

              if tmpNode.Count=0 then
                 tmpNode.Free;
            end;
          end;

        end
        else
        if (Extension='') or IsExtension(f.Name) then
           AddTreeModule(AParent,RemoveExtension(f.Name));

      Until FindNext(f)<>0;

      FindClose(f);
    end;
  end;

var tmp : String;
    tmpNode : TTreeNode;
begin
  tmp:=Trim(ARoot);

  if tmp='' then
     Exit;

  Root:=tmp;

  TryAddPath(Root);

  Tree1.Items.BeginUpdate;
  try
    Tree1.Items.Clear;

    tmpNode:=Tree1.Items.AddChild(nil,ParentFolder);
    SearchPath(tmpNode,Root);

    if tmpNode.Count>0 then
       tmpNode.Expanded:=True;
  finally
    Tree1.Items.EndUpdate;
  end;
end;

procedure TFormExplorer.SpeedButton1Click(Sender: TObject);
begin
  if SelectDirectory('Select directory','',Root) then
     Init(Root);
end;

procedure TFormExplorer.TryRefreshExplorer(const ARoot:String);
begin
  if Trim(ARoot)<>Root then
     RefreshExplorer(ARoot);
end;

procedure TFormExplorer.Vidimodule1Click(Sender: TObject);
var tmpName,
    tmpPath,
    tmpModule : String;
    tmpNode : TTreeNode;
begin
  tmpPath:=SelectedFolder;

  if tmpPath<>'' then
  begin
    tmpName:=NewPathName(False,'Module ',tmpPath,TVidiConstants.Extension);

    tmpName:=RemoveExtension(tmpName);

    if InputQuery('New Module','Module name',tmpName) then
    begin
      tmpName:=Trim(tmpName);

      if tmpName<>'' then
      begin
        tmpName:=TryAddExtension(tmpName,TVidiConstants.Extension);

        tmpModule:=CombineFile(tmpPath,tmpName);

        if DoFileExists(tmpPath,tmpName) then
           ShowMessage('Module already exists: '+tmpName)
        else
        begin
          WriteFile(tmpModule,'');

          tmpNode:=AddTreeModule(Tree1.Selected,RemoveExtension(tmpName));
          tmpNode.Selected:=True;

          Tree1DblClick(Self);
        end;
      end;
    end;
  end;
end;

end.

