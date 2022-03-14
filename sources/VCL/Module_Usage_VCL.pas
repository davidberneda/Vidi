unit Module_Usage_VCL;

interface

uses
  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  Sys, AST;

type
  TOnGotoModule=procedure(Sender:TObject; const AModule:TType) of object;

  TFormModuleUsage = class(TForm)
    Panel1: TPanel;
    LabelUses: TLabel;
    LabelUsedBy: TLabel;
    LBUses: TListBox;
    Splitter1: TSplitter;
    LBUsedBy: TListBox;
    procedure LBUsesDblClick(Sender: TObject);
    procedure LBUsedByDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }

    procedure TryGoto(const AList:TListBox);
  public
    { Public declarations }

    OnGotoModule : TOnGotoModule;

    procedure Clear;
    procedure Fill(const ANode:TNode);
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF INTERNAL}
  Internal,
  {$ENDIF}
  IDE_Lang_English,
  Module;

procedure TFormModuleUsage.Clear;
begin
  LBUses.Clear;
  LBUsedBy.Clear;
end;

procedure TryAdd(const AList:TStrings; const AModule:TNamedType);
begin
  if AList.IndexOfObject(AModule)=-1 then
     AList.AddObject(AModule.Name,AModule);
end;

procedure TFormModuleUsage.Fill(const ANode: TNode);

  procedure FillUsedBy(const AModule,Another:TNamedType);
  begin
    if Modules.DependsOn(Another,AModule) then
       TryAdd(LBUsedBy.Items,Another);
  end;

  procedure TryFillUsedBy(const AModule:TNamedType);
  var t,
      tmp : Integer;
  begin
    tmp:=Modules.IndexOf(AModule);

    {$IFDEF INTERNAL}
    if tmp=-1 then
       InternalError('Cannot find module: ',AModule)
    else
    {$ENDIF}
       for t:=tmp+1 to High(Modules.Items) do
           FillUsedBy(AModule,Modules.Items[t] as TNamedType);
  end;

  procedure AddUses;
  var N : TNamedType;
      tmp : TNode;
  begin
    LBUses.Items.BeginUpdate;
    try
      LBUses.Clear;

      if ANode<>nil then
      begin
        tmp:=Modules.SystemModule;

        if tmp<>nil then
           if tmp<>ANode then
              TryAdd(LBUses.Items,TNamedType(tmp));

        for N in TModules.UsesOf(ANode,True) do
            TryAdd(LBUses.Items,N);
      end;
    finally
      LBUses.Items.EndUpdate;
    end;
  end;

  procedure AddUsedBy;
  begin
    LBUsedBy.Items.BeginUpdate;
    try
      LBUsedBy.Clear;

      if ANode is TNamedType then
         TryFillUsedBy(TNamedType(ANode));
    finally
      LBUsedBy.Items.EndUpdate;
    end;
  end;

begin
  AddUses;
  AddUsedBy;

  LBUsedBy.Enabled:=ANode<>nil;
  LBUses.Enabled:=ANode<>nil;
end;

procedure TFormModuleUsage.FormCreate(Sender: TObject);
begin
  LBUses.Sorted:=True;
  LBUsedBy.Sorted:=True;
end;

procedure TFormModuleUsage.FormShow(Sender: TObject);
begin
  LabelUses.Caption:=Vidi_Lang._Uses;
  LabelUsedBy.Caption:=Vidi_Lang.UsedBy;
end;

procedure TFormModuleUsage.TryGoto(const AList:TListBox);
var tmp : Integer;
begin
  tmp:=AList.ItemIndex;

  if tmp<>-1 then
     OnGotoModule(AList,TType(AList.Items.Objects[tmp]));
end;

procedure TFormModuleUsage.LBUsedByDblClick(Sender: TObject);
begin
  TryGoto(LBUsedBy);
end;

procedure TFormModuleUsage.LBUsesDblClick(Sender: TObject);
begin
  TryGoto(LBUses);
end;

procedure TFormModuleUsage.Splitter1Moved(Sender: TObject);
begin
  LabelUsedBy.Left:=LBUsedBy.Left;
end;

end.
