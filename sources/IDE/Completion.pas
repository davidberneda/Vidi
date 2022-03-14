unit Completion;

interface

uses
  Classes, Sys;

type
  TCompletion=record
  public
    class function FillItems(const AItems:TStrings;
                       const ANode: TNode;
                       const APrefix: String):Integer; static;
  end;

implementation

uses
  AST, Utils.AST, Checker.AST,
  //Search,
  Module, Syntax, Emit;

function DeclaringParameters(const AParams:TNodes):String;
var Emit : TVidiEmit;
begin
  Emit:=TVidiEmit.Create;
  try
    Emit.DeclaringParameters(AParams);
    result:=Emit.Emit.GetText;
  finally
    Emit.Free;
  end;
end;

class function TCompletion.FillItems(const AItems:TStrings;
                                     const ANode: TNode;
                                     const APrefix: String):Integer;
var
  Found : Integer;

  function FinalNode(const N:TNode):TNode;
  begin
    if N is TExtender then
       result:=TExtender(N).Extension
    else
       result:=N;
  end;

  function GetNames(N:TNode; out ACategory,AName,AParams:String):String;
  begin
    N:=FinalNode(N);

    if N is TVariable then
       ACategory:='Variable'
    else
    if N is TRoutine then
       ACategory:='Routine'
    else
    if N is TType then
       ACategory:='Type'
    else
       ACategory:=TASTUtils.RemoveTPrefix(N);

    AName:=TASTUtils.NodeName(N);

    if N is TParametersType then
    begin
      if TParametersType(N).Parameters<>nil then
         AParams:=DeclaringParameters(TParametersType(N).Parameters)
      else
         AParams:='';

      if N is TRoutine then
         if TRoutine(N).Output<>nil then
            AParams:=AParams+' '+TSyntax._TypeDelimiter+' '+TASTUtils.NodeName(TRoutine(N).Output);

    end;

    result:=ACategory+' '+TSyntax._TypeDelimiter+' '+AName;

    if AParams<>'' then
       result:=result+' '+AParams;
  end;

  procedure TryAddItem(const N:TNode);
  var tmpCategory,
      tmpName,
      tmpParams,
      tmpAll : String;
  begin
    if (N is TVariable) or (N is TType) then
    begin
      tmpAll:=GetNames(N,tmpCategory,tmpName,tmpParams);

      // TODO: Avoid Object N duplicates !
      AItems.AddObject(tmpAll,N);

      if Found=-1 then
         if Pos(APrefix,tmpName)>0 then
            Found:=AItems.Count-1;
    end;
  end;

  procedure AddItems(const AItems:TNodes; OnlyHidden:Boolean);
  var N : TNode;
  begin
    for N in AItems do
        if (not OnlyHidden) or (not TChecker.IsHidden(N)) then
           TryAddItem(N);
  end;

  procedure AddAllItemsFromNode;
  var tmp,
      tmpSearch: TNode;
      t,
      tmpPos : Integer;
      tmpType : TType;
      tmpWith : TWith;

      tmpIsSys,
      tmpSysAdded : Boolean;
  begin
    tmpSysAdded:=False;

    tmpSearch:=ANode;
    tmp:=ANode;

    while tmp<>nil do
    begin
      if tmp is TType then
      begin
        tmpPos:=TType(tmp).Items.IndexOf(tmpSearch);

        if tmpPos<>-1 then
        begin
          tmpType:=TType(tmp);

          for t:=tmpPos downto 0 do
              if tmpType.Items[t] is TWith then
              begin
                tmpWith:=TWith(tmpType.Items[t]);

                tmpIsSys:=tmpWith.Module=Modules.SystemModule;

                if tmpIsSys then
                begin
                  if not tmpSysAdded then
                  begin
                    AddItems(tmpWith.Module.Items,True);
                    tmpSysAdded:=True;
                  end;
                end
                else
                  AddItems(tmpWith.Module.Items,True);
              end
              else
                 TryAddItem(tmpType.Items[t]);
        end;
      end;

      tmpSearch:=tmp;
      tmp:=tmp.Owner;
    end;
  end;

var tmp : TNode;
begin
  Found:=-1;

  AItems.BeginUpdate;
  try
    AItems.Clear;

    tmp:=TChecker.GetClassTypeOf(ANode);

    if tmp is TType then
    begin
      repeat
        AddItems(TType(tmp).Items,False);

        if tmp is TClassType then
           tmp:=TClassType(tmp).Ancestor
        else
           break;

      until tmp=nil;
    end
    else
      AddAllItemsFromNode;
  finally
    AItems.EndUpdate;
  end;

  result:=Found;
end;

end.
