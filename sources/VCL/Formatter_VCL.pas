unit Formatter_VCL;

interface

uses
  //Windows, Messages,
  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Map, Editor_VCL, Gutter, Editor;

type
  TFormFormatter = class(TForm)
    Panel1: TPanel;
    RGBlock: TRadioGroup;
    RGAssign: TRadioGroup;
    RGIfThen: TRadioGroup;
    RGNotEqual: TRadioGroup;
    procedure RGBlockClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RGAssignClick(Sender: TObject);
    procedure RGIfThenClick(Sender: TObject);
    procedure RGNotEqualClick(Sender: TObject);
  private
    { Private declarations }

    Edit : TCodeRichEdit;
    Gutter : TEditorGutter;

    procedure CreateEditor;
    function MapToText:String;
    function TextOf(const AItem:TNodePosition):String;
  public
    { Public declarations }

    Map : TNodePositions;
    Text : String;

    procedure Reformat;
  end;

implementation

{$R *.dfm}

  {$IFNDEF FPC}
uses
  UITypes
  ;
  {$ENDIF}

function TFormFormatter.TextOf(const AItem:TNodePosition):String;
begin
  result:=Copy(Text,AItem.Position.Position,AItem.Length);
end;

procedure TFormFormatter.CreateEditor;
begin
  Edit:=TCodeRichEdit.Create(Self);
  Edit.Align:=TAlign.alClient;
  Edit.ScrollBars:={$IFNDEF FPC}System.UITypes.{$ENDIF}TScrollStyle.ssBoth;
  Edit.Parent:=Self;

  Edit.Font.Name:='Courier New';
  Edit.Font.Size:=10;
end;

procedure TFormFormatter.FormCreate(Sender: TObject);
begin
  CreateEditor;

  Gutter:=TEditorGutter.Create(Self);
  Gutter.Edit:=Edit;
end;

function TFormFormatter.MapToText:String;
const CRLF=#13#10;

var t : Integer;
    i : TNodePosition;
    tmp : String;
    Line : Integer;
begin
  result:='';

  Line:=1;

  for t:=0 to Map.Count-1 do
  begin
    i:=Map.Items[t];

    if t>0 then
       while i.Position.Line>Line do
       begin
         Inc(Line);
         result:=result+CRLF;
       end;

    tmp:=TextOf(i);

    case i.Style of
      TPositionStyle.Symbol:
        begin
          if RGBlock.ItemIndex=1 then
             if tmp='{' then
                tmp:='begin'
             else
             if tmp='}' then
                tmp:='end';

          if RGAssign.ItemIndex=1 then
             if tmp=':=' then
                tmp:='='
             else
             if tmp='=' then
                tmp:='==';

          if RGNotEqual.ItemIndex=1 then
             if tmp='<>' then
                tmp:='!=';
        end;

//      TPositionStyle.Keyword:  IFThen !
    end;

    result:=result+tmp+' ';
  end;
end;

procedure TFormFormatter.Reformat;
var tmp : String;
begin
  tmp:=MapToText;
  Edit.Text:=tmp;

  TCodeEditor.DoHighlight(Edit,Map);
end;

procedure TFormFormatter.RGAssignClick(Sender: TObject);
begin
  Reformat;
end;

procedure TFormFormatter.RGBlockClick(Sender: TObject);
begin
  Reformat;
end;

procedure TFormFormatter.RGIfThenClick(Sender: TObject);
begin
  Reformat;
end;

procedure TFormFormatter.RGNotEqualClick(Sender: TObject);
begin
  Reformat;
end;

end.
