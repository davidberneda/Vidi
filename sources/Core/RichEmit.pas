unit RichEmit;

interface

uses
  Sys,
  Emit, Editor_VCL,
  {$IFDEF FPC}
  Graphics,
  {$ELSE}
  UITypes,
  {$ENDIF}
  Map;

type
  TRichEmit=class(TEmit)
  private
    procedure DoAdd(const S:String; const AColor:TColor; const Bold,Italic:Boolean);
  public
    RichEdit : TCodeRichEdit;

    procedure Add(const ANode:TNode; const S:String); override;
    procedure Add(const AStyle:TPositionStyle; const S:String); override;
    procedure Clear; override;
    function GetText:String; override;
  end;

implementation

uses
  //AST,
  //Syntax,
  {$IFDEF FPC}
  RichMemoUtils,
  {$ENDIF}
  Highlight;

procedure TRichEmit.DoAdd(const S:String; const AColor:TColor; const Bold,Italic:Boolean);
begin
  {$IFDEF FPC}
  InsertColorStyledText(RichEdit,S,AColor,THighlight.StyleOf(Bold,Italic));
  {$ELSE}
  RichEdit.ChangeSelected(THighlight.StyleOf(Bold,Italic),AColor);
  RichEdit.SelText:=S;
  {$ENDIF}
end;

procedure TRichEmit.Add(const AStyle:TPositionStyle; const S: String);
var tmp : TColor;
    Bold,
    Italic : Boolean;
begin
  if S='' then Exit;

  Bold:=False;
  Italic:=False;

  case AStyle of
    TPositionStyle.Keyword:
      begin
        tmp:=THighlight.Colors.Keyword;
        Bold:=True;
      end;

    TPositionStyle.Symbol: tmp:=THighlight.Colors.Symbol;
    TPositionStyle.Comment: tmp:=THighlight.Colors.Comment;
  else
  begin
    tmp:=THighlight.Colors.Identifier;
  end;
  end;

  DoAdd(S,tmp,Bold,Italic);
end;

procedure TRichEmit.Add(const ANode:TNode; const S: String);
var ShouldBold,
    ShouldItalic : Boolean;
    tmp : TColor;
begin
  if S='' then Exit;

  if ANode=nil then
     DoAdd(S,THighlight.Colors.Identifier,False,False)
  else
  begin
    tmp:=THighlight.ColorOf(ANode,ShouldBold,ShouldItalic);
    DoAdd(S,tmp,ShouldBold,ShouldItalic);
  end;
end;

procedure TRichEmit.Clear;
begin
  RichEdit.Clear;
end;

function TRichEmit.GetText: String;
begin
  result:=RichEdit.Text;
end;

end.
