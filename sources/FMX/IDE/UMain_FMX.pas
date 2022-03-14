unit UMain_FMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Objects, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,

  Runner, Parser, Exceptions, Module, AST, FMX_Gutter;

type
  TVidiForm = class(TForm)
    Layout1: TLayout;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    Log: TListBox;
    Layout2: TLayout;
    SaveImage: TImage;
    ShareImage: TImage;
    LayoutTopPanel: TLayout;
    MenuImage: TImage;
    LayoutMenu: TLayout;
    LayoutTopMenu: TLayout;
    RunImage: TImage;
    NewImage: TImage;
    List: TListView;
    LayoutMain: TLayout;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure LogClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Memo1ChangeTracking(Sender: TObject);
    procedure SaveImageClick(Sender: TObject);
    procedure ShareImageClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure OpenImageClick(Sender: TObject);
    procedure MenuImageClick(Sender: TObject);
    procedure RunImageClick(Sender: TObject);
    procedure NewImageClick(Sender: TObject);
    procedure ListChange(Sender: TObject);
    procedure ListDeletingItem(Sender: TObject; AIndex: Integer;
      var ACanDelete: Boolean);
    procedure ListDeleteItem(Sender: TObject; AIndex: Integer);
  private
    { Private declarations }

    Modified : Boolean;

    ThePath,
    TheName : String;

    Gutter : TGutter;

    procedure AddDemo;
    procedure AddToList(const AFile:String);
    function CheckSave:Boolean;
    function CreateGutter:TGutter;
    procedure DoCompile;
    procedure DoOpen(const AFile:String);
    procedure ErrorProc(const Sender:TParser; const AError:TNodeError);
    function ExistInList(const AName:String):Boolean;
    procedure FillList;
    procedure FindModule(const AName:String; out ANode:TNamedType);
    function GetFileToSave:String;
    function ListFile(const AName:String):String;
    function NewModuleName:String;
    procedure SetPathName(const AFile:String);
    procedure SwitchMenu;
    function TrySave:Boolean;
  public
    { Public declarations }
  end;

var
  VidiForm: TVidiForm;

implementation

{$R *.fmx}

uses
  {$IFOPT D+}
  Sys, Emit,
  {$ENDIF}
  Diagnostics, IOUtils, Constants,
  Bee.Parser, FMX_Utils;

function TVidiForm.CheckSave:Boolean;
begin
  if Modified then
     Modified:=TrySave;

  result:=not Modified;
end;

procedure TVidiForm.ErrorProc(const Sender: TParser; const AError: TNodeError);
begin
  Log.Items.AddObject('['+AError.Module+'] '+AError.Position.LineColumn+' '+AError.Text,AError);
end;

procedure TVidiForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Modified then
     TFMXUtils.YesNo('Do you want to save '+TheName+'?',
       procedure(const Value:Boolean)
       begin
         if Value then
            Modified:=CheckSave
         else
            Modified:=False;
       end
       );

  CanClose:=not Modified;
end;

function ModulesPath:String;
begin
  result:=TPath.Combine(TPath.GetDocumentsPath,'Vidi');
end;

function TVidiForm.CreateGutter:TGutter;
begin
  result:=TGutter.Create(Self);
  result.Align:=TAlignLayout.Left;
  result.Size.Width:=30;
  result.Parent:=LayoutMain;
end;

procedure TVidiForm.FormCreate(Sender: TObject);
begin
  Gutter:=CreateGutter;
  Gutter.Memo:=Memo1;

  LayoutMenu.Visible:=False;

  ThePath:=ModulesPath;

  ForceDirectories(ThePath);
  OpenDialog1.InitialDir:=ThePath;

  Log.Items.Add(ThePath);

  TheName:='Module1';

  TCodeRunner.Bee.OnError:=ErrorProc;

  {$IFDEF MSWINDOWS}
  //TCodeRunner.Bee.ParserPath:='C:\David\Projects\Vidi\Source';
  {$ENDIF}

  TCodeRunner.Bee.OnFindModule:=FindModule;
end;

procedure TVidiForm.FindModule(const AName:String; out ANode:TNamedType);
var tmp : TBee;
    tmpText : String;
begin
  tmpText:=TFMXUtils.GetTextResource(AName);

  if tmpText='' then
     ANode:=nil
  else
  begin
    tmp:=TBee.Create;
    try
      tmp.OnError:=TCodeRunner.Bee.OnError;
      tmp.ParserPath:=TCodeRunner.Bee.ParserPath;
      tmp.OnFindModule:=TCodeRunner.Bee.OnFindModule;

      ANode:=tmp.DirectParseModule('',AName,tmpText);
    finally
      tmp.Free;
    end;
  end;
end;

// skip linker removal of B function when debugging
procedure TryLink_B;
{$IFOPT D+}
var tmp : TNode;
{$ENDIF}
begin
  {$IFOPT D+}
  tmp:=TReturn.Create; // easy class
  try
    B(tmp);
  finally
    tmp.Free;
  end;
  {$ENDIF}
end;

procedure TVidiForm.AddDemo;
begin
  Memo1.Text:=TFMXUtils.GetTextResource('VidiDemo');
  Modified:=False;

{
//  Memo1.StyledSettings:=Memo1.StyledSettings-[TStyledSetting.FontColor];

  Memo1.SelStart:=50;
  Memo1.SelLength:=20;
  Memo1.SelectionFill.Kind:=TBrushKind.Solid;
  Memo1.SelectionFill.Color:=TAlphaColorRec.Red;
}

end;

procedure TVidiForm.FormShow(Sender: TObject);
const DefaultFontName=
   {$IFDEF MSWINDOWS}
   'Lucida Console'
   {$ELSE}
   'Roboto'
   {$ENDIF}
   ;
begin
  TryLink_B;

  Memo1.StyledSettings:=Memo1.StyledSettings-[TStyledSetting.Family];
  Memo1.Font.Family:=DefaultFontName;

  AddDemo;
end;

procedure TVidiForm.SaveImageClick(Sender: TObject);
begin
  Modified:=TrySave;
end;

function TVidiForm.ListFile(const AName:String):String;
begin
  result:=TPath.Combine(ThePath,AName+TVidiConstants.Extension);
end;

procedure TVidiForm.ListChange(Sender: TObject);
begin
  if List.ItemIndex<>-1 then
     if CheckSave then
        DoOpen(ListFile(List.Items[List.ItemIndex].Text));
end;

procedure TVidiForm.ListDeleteItem(Sender: TObject; AIndex: Integer);
var tmp : String;
begin
  tmp:=List.Items[AIndex].Text;
  tmp:=ListFile(tmp);

  TFile.Delete(tmp);
end;

procedure TVidiForm.ListDeletingItem(Sender: TObject; AIndex: Integer;
  var ACanDelete: Boolean);
var tmp : Boolean;
begin
  tmp:=False;

  TFMXUtils.YesNo('Sure to delete?',
    procedure(const Value:Boolean)
    begin
      tmp:=Value;
    end
   );

   ACanDelete:=tmp;
end;

procedure TVidiForm.LogClick(Sender: TObject);
begin
  if Log.ItemIndex<>-1 then
  begin

  end;
end;

procedure TVidiForm.SetPathName(const AFile:String);
begin
  ThePath:=ExtractFilePath(AFile);
  TheName:=ExtractFileName(AFile);
end;

procedure TVidiForm.ShareImageClick(Sender: TObject);
begin
  TFMXUtils.ShareText(Memo1.Text);
end;

function TVidiForm.GetFileToSave:String;
begin
  SaveDialog1.FileName:=TheName;
  SaveDialog1.InitialDir:=ThePath;

  if SaveDialog1.Execute then
     result:=SaveDialog1.FileName
  else
     result:='';
end;

function TVidiForm.ExistInList(const AName:String):Boolean;
var S : TListViewItem;
begin
  for S in List.Items do
      if SameText(S.Text,AName) then
         Exit(True);

  result:=False;
end;

function TVidiForm.TrySave:Boolean;
var tmp : String;
begin
  {$IFDEF MSWINDOWS}
  tmp:=GetFileToSave;
  {$ELSE}
  tmp:=ListFile(TheName);
  {$ENDIF}

  result:=tmp<>'';

  if result then
  begin
    Memo1.Lines.SaveToFile(tmp);
    SetPathName(tmp);

    if not ExistInList(TFMXUtils.OnlyName(tmp)) then
       AddToList(tmp);

    Modified:=False;
  end;
end;

procedure TVidiForm.Memo1ChangeTracking(Sender: TObject);
begin
  Modified:=True;

  ShareImage.Enabled:=Memo1.Text<>'';
end;

type
  TLayoutAccess=class(TLayout);

procedure TVidiForm.SwitchMenu;
begin
  if MenuImage.Parent=LayoutTopPanel then
     MenuImage.Parent:=LayoutTopMenu
  else
     MenuImage.Parent:=LayoutTopPanel;

  MenuImage.Position.X:=0;
  TLayoutAccess(MenuImage.Parent).ReAlign;

  LayoutMenu.Visible:=MenuImage.Parent=LayoutTopMenu;

  if LayoutMenu.Visible then
     FillList;
end;

procedure TVidiForm.MenuImageClick(Sender: TObject);
begin
  SwitchMenu;
end;

procedure TVidiForm.FillList;
var tmp : TArray<String>;
    t : Integer;
begin
  tmp:=TDirectory.GetFiles(ThePath);

  List.BeginUpdate;
  try
    List.Items.Clear;

    for t:=Low(tmp) to High(tmp) do
        AddToList(tmp[t]);
  finally
    List.EndUpdate;
  end;
end;

procedure TVidiForm.AddToList(const AFile:String);
var tmp : TListViewItem;
begin
  tmp:=List.Items.Add;
  tmp.Text:=TFMXUtils.OnlyName(AFile);
  tmp.Accessory:=TAccessoryType.Detail;
end;

procedure TVidiForm.NewImageClick(Sender: TObject);
begin
  if CheckSave then
  begin
    Memo1.Text:='';

    SetPathName(ListFile(NewModuleName));

    Log.Clear;

    SwitchMenu;

    Memo1.SetFocus;
  end;
end;

function TVidiForm.NewModuleName: String;
var tmp : Integer;
begin
  tmp:=1;

  repeat
    result:='Module'+tmp.ToString;

    if TFile.Exists(ListFile(result)) then
       Inc(tmp)
    else
       break;

  until False;
end;

procedure TVidiForm.DoOpen(const AFile:String);
begin
  Memo1.Lines.LoadFromFile(AFile);
  SetPathName(AFile);
  Log.Clear;
  Modified:=False;
end;

procedure TVidiForm.OpenImageClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
     if CheckSave then
        DoOpen(TPath.Combine(ThePath,OpenDialog1.FileName));
end;

procedure TVidiForm.RunImageClick(Sender: TObject);
begin
  DoCompile;
end;

procedure TVidiForm.DoCompile;
var tmpText : String;
    t1 : TStopWatch;
begin
  tmpText:=Memo1.Text;

  Log.Clear;

  Modules.Remove(TheName);

  t1:=TStopWatch.StartNew;

  TCodeRunner.Bee.ParseModule(ThePath,TheName,tmpText);

  Log.Items.Add('Compiled: '+t1.ElapsedMilliseconds.ToString+' msec.');
  Log.Items.Add('RUN NOT IMPLEMENTED YET !');
end;

end.
