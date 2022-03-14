unit Find.AST;

interface

uses
  Sys, AST, Checker.AST;

type
  TFinder=class
  private
    function IsSameNamedType(const ANode:TNode; const AName:String):Boolean;
  public
    Checker : TChecker;

    function FindOverload(const AOwner:TNode; const ARoutine:TRoutine; const AParameters:TNodes):TRoutine;

    class function FindBlock(const AContext:TNode):TBlockStatement; static; // {}

    class function FindBreak(const ANode:TNode):Boolean; static;

    function FindClass(const AContext:TNode; const AName:String):TClassType;

    function FindInherited(const AClass:TClassType; const ARoutine:TRoutine):TRoutine;

    function FindMethod(AOwner:TNode; const ARoutine:TChecker.TRoutineDefinition):TRoutine; overload;
//    function FindMethod(AOwner:TNode; const ARoutine:String):TRoutine; overload;

    function FindMethodIn(const ANodes:TNodes; const ARoutine:TChecker.TRoutineDefinition):TRoutine; overload;
    function FindMethodIn(const ANodes:TNodes; const ARoutine:String):TRoutine; overload;

    function FindOverride(AOwner:TType; const ARoutine:TRoutine):TRoutine; overload;
    function FindOverride(const ARoutine:TRoutine; const AItems:TNodes):TRoutine; overload;

    class function FindParentTypeOf(ANode:TNode):TType; static;
    class function FindReturn(const ANode:TNode):Boolean; static;

    function FindType(AOwner:TNode; const AType:String; out AWithFound:TWith):TType; overload;
    function FindType(AOwner:TNode; const AType:String):TType; overload;

    function FindTypeIn(const AItems:TNodes; const AName:String):TNamedType;
    function FindTypeOrExtender(const AModule,AType:TType; const AName:String):TNamedType;

    class function GetAncestorType(const AType:TType):TType; static;

    class function ModuleOf(ANode:TNode):TNamedType; static;
    class function ModuleNameOf(const ANode:TNode):String; static;
    class function NodeOfDeclaration(const ANode:TNode):TNode; static;

    class function WithName(const AWith:TWith):String; static;
  end;

implementation

uses
  {$IFDEF INTERNAL}
  Internal,
  {$ENDIF}
  Module, Creator.AST;

// Returns owner module of ANode (NOT the module where ANode is declared, see: NodeOfDeclaration)
class function TFinder.ModuleOf(ANode:TNode):TNamedType;
begin
  if ANode<>nil then
  while ANode.Owner<>nil do
        ANode:=ANode.Owner;

  if ANode is TNamedType then
     result:=TNamedType(ANode)
  else
     result:=nil;

  {$IFDEF INTERNAL}
  if result=nil then
     InternalError('Cannot obtain module of: ',ANode);
  {$ENDIF}
end;

class function TFinder.ModuleNameOf(const ANode:TNode):String;
var tmp : TNamedType;
begin
  tmp:=ModuleOf(ANode);

  if tmp=nil then
     result:=''
  else
     result:=tmp.Name;
end;

// Returns type node where ANode (as an identifier) is declared
class function TFinder.NodeOfDeclaration(const ANode:TNode):TNode;
begin
  if ANode is TTypeCall then
     result:=TTypeCall(ANode).TheType
  else
  if ANode is TDataCall then
     result:=TDataCall(ANode).Routine
  else
     result:=ANode;
end;

function TFinder.FindOverload(const AOwner:TNode; const ARoutine: TRoutine;
  const AParameters: TNodes): TRoutine;

  function FindIn(const AItems:TNodes):TRoutine;
  var N : TNode;
      tmp : TRoutine;
      tmpBad : Integer;
  begin
    for N in AItems do
        if (N is TWith) and (TWith(N).Module<>nil) then
        begin
          result:=FindIn(TWith(N).Module.Items);

          if result<>nil then
             Exit;
        end
        else
        begin
          if N is TRoutine then
             tmp:=TRoutine(N)
          else
          if (N is TExtender) and (TExtender(N).Extension is TRoutine) then
             tmp:=TExtender(N).Extension as TRoutine
          else
             tmp:=nil;

          if (tmp<>nil) and Checker.TextIs(tmp.Name,ARoutine.Name) then
          begin
            // Check Parameters and Output type
            if Checker.ValidParameters(nil,tmp.Parameters,AParameters,tmpBad) then
               Exit(tmp);
          end;
        end;

    result:=nil;
  end;

  function FindExtender(const AOwner:TNode):TRoutine;
  var tmpOwner : TNode;
  begin
    result:=nil;

    tmpOwner:=AOwner.Owner;

    // It seems this while loop is unnecessary, too much matches
    while tmpOwner<>nil do
    begin
      if (tmpOwner is TExtender) and (TExtender(tmpOwner).Extension is TRoutine) then
         result:=FindIn(TExtender(tmpOwner).Extension.Items)
      else
      if tmpOwner is TType then
         result:=FindIn(TChecker.TheTypeOf(TType(tmpOwner)).Items)
      else
         result:=nil;

      if result=nil then
         tmpOwner:=tmpOwner.Owner
      else
         break;
    end;
  end;

var tmp : TClassType;
begin
  result:=nil;

  tmp:=TChecker.GetClassTypeOf(ARoutine);

  // TODO: Loop also in scope hierarchy (Owner.Owner.Owner...)
  // This is to find Extender Routines declared in scope

  if tmp<>nil then
  repeat
    result:=FindIn(tmp.Items);

    if result=nil then
    begin
      if tmp.Ancestor is TClassType then
         tmp:=TClassType(tmp.Ancestor)
      else
         tmp:=nil;
    end
    else
       Exit;

  until tmp=nil;

  // Try to find routine as an extender up at the scope hierarchy

  if AOwner<>nil then
     result:=FindExtender(AOwner); // <-- fix, this must be done only once, outside this "repeat" ?
end;

// Returns first unnamed block { } inside AContext node (usually a Module TType)
class function TFinder.FindBlock(const AContext:TNode):TBlockStatement;
var N : TNode;
begin
  if AContext is TType then
     for N in TType(AContext).Items do
         if N is TBlockStatement then
            Exit(TBlockStatement(N));

  result:=nil;
end;

function TFinder.FindClass(const AContext:TNode; const AName:String):TClassType;
var N : TNode;
begin
  if AContext is TType then
     for N in TType(AContext).Items do
         if N is TClassType then
            if Checker.TextIs(TClassType(N).Name,AName) then
               Exit(TClassType(N));

  result:=nil;
end;

{ unused:
function TFinder.FindMethod(AOwner:TNode; const ARoutine:String):TRoutine;
begin
  result:=nil;

  repeat
    if AOwner=nil then
       break;

    if AOwner is TType then
    begin
      result:=FindMethodIn(TType(AOwner).Items,ARoutine);

      if result=nil then
         if AOwner is TClassType then
            if TClassType(AOwner).Ancestor<>nil then
               result:=FindMethod(TClassType(AOwner).Ancestor,ARoutine);
    end;

    if result=nil then
       if AOwner.Owner<>nil then
       begin
         result:=FindMethod(AOwner.Owner,ARoutine);

         if result=nil then
            break;
       end;

  until (result<>nil) or (AOwner.Owner=nil);
end;
}

function TFinder.FindMethod(AOwner:TNode; const ARoutine:TChecker.TRoutineDefinition):TRoutine;
begin
  result:=nil;

  repeat
    if AOwner=nil then
       break;

    if AOwner is TType then
    begin
      result:=FindMethodIn(TType(AOwner).Items,ARoutine);

      if result=nil then
         if AOwner is TClassType then
            if TClassType(AOwner).Ancestor<>nil then
               result:=FindMethod(TClassType(AOwner).Ancestor,ARoutine);
    end;

    if result=nil then
       if AOwner.Owner<>nil then
       begin
         result:=FindMethod(AOwner.Owner,ARoutine);

         if result=nil then
            break;
       end;

  until (result<>nil) or (AOwner.Owner=nil);
end;

function IndexOfNode(const ANode:TNode; const ANodes:TNodes):Integer;
var t : Integer;
begin
  for t:=Low(ANodes) to High(ANodes) do
      if ANodes[t]=ANode then
         Exit(t);

  result:=-1;
end;

function TFinder.IsSameNamedType(const ANode:TNode; const AName:String):Boolean;
begin
  result:=(ANode is TNamedType) and
          Checker.TextIs(TNamedType(ANode).Name,AName);
end;

function TFinder.FindTypeIn(const AItems:TNodes; const AName:String):TNamedType;
var tmp : TNode;
begin
  for tmp in AItems do
      if not (tmp is TExtender) then // with Extenders.Bar <- should fail
         if IsSameNamedType(tmp,AName) then
            Exit(TNamedType(tmp));

  result:=nil;
end;

class function TFinder.WithName(const AWith:TWith):String;
begin
  result:=AWith.Alias;

  if result='' then
     result:=AWith.Module.Name;
end;

function TFinder.FindTypeOrExtender(const AModule,AType:TType; const AName:String):TNamedType;
var N : TNode;
begin
  result:=FindTypeIn(AType.Items,AName);

  if result=nil then
  begin
    // try find in extenders of AType module
    for N in AModule.Items do
        if N is TExtender then
           if TChecker.GetFinalType(TExtender(N).TheType)=AType then
              if Checker.TextIs(TExtender(N).Extension.Name,AName) then
                 Exit(TExtender(N));
  end;
end;

function TFinder.FindType(AOwner:TNode; const AType:String; out AWithFound:TWith):TType;

  // Search AType in AWith module
  function TryFindInWith(const AWith:TWith):TType;
  var tmpAlias : String;
  begin
    tmpAlias:=AWith.Alias;

    if tmpAlias='' then
       tmpAlias:=AWith.Module.Name;

    if Checker.TextIs(tmpAlias,AType) then
    begin
      AWithFound:=AWith;
      result:=AWith.Module  // <-- good for sys.integer, bad for basic.vidi
    end
    else
       result:=FindTypeOrExtender(TFinder.ModuleOf(AWith.Module),
                                  TChecker.GetFinalType(AWith.Module),
                                  AType);
  end;

  function IsVariableOfType(const ANode:TNode):Boolean;
  begin
     result:=(ANode is TVariable) and
             TVariable(ANode).Clauses.Shared and
             TChecker.IsVariableOfTypeType(TVariable(ANode)) and
             Checker.TextIs(TVariable(ANode).Name,AType);
  end;

  function TryFindBackwards(const ANodes:TNodes):TType;
  var t : Integer;
  begin
    for t:=High(ANodes) downto Low(ANodes) do
        if IsSameNamedType(ANodes[t],AType) then
           Exit(TNamedType(ANodes[t]))
        else
        if ANodes[t] is TWith then
        begin
          result:=TryFindInWith(TWith(ANodes[t]));

          if result<>nil then
             Exit;
        end
        else
        if IsVariableOfType(ANodes[t]) then
        begin
          result:=TGenericType.Create;
          TGenericType(result).Variable:=TVariable(ANodes[t]);

          Exit;
        end;

    result:=nil;
  end;

  function TryFind(const AOwner:TNode):TType;

    function TryFindGenerics(const ANodes:TNodes):TType;

      function TryFindGeneric(const ANode:TNode):TGenericType;
      begin
        if (ANode is TVariable) and Checker.VariableIsType(TVariable(ANode),AType) then // Generic types
        begin
          result:=TASTCreator.CreateGenericType(AOwner,TVariable(ANode),nil);
        end
        else
          result:=nil;
      end;

    var t : Integer;
    begin
      result:=nil;

      for t:=High(ANodes) downto Low(ANodes) do // right to left
      begin
        result:=TryFindGeneric(ANodes[t]);

        if result<>nil then
           break;
      end;
    end;

    // Try "Reification", discover ancestor "T" parameter and return child generic specialized
    function TryFindAncestorGenerics(const AClass:TParametersType):TGenericType;

      function FindInParams(const AParams:TNodes):Integer;
      var t : Integer;
      begin
        for t:=0 to High(AParams) do
            if AParams[t] is TVariable then
               if Checker.VariableIsType(TVariable(AParams[t]),AType) then
                  Exit(t);

        result:=-1;
      end;

    var tmp : TType;
        tmpParams : TNodes;
        tmpIndex : Integer;
    begin
      result:=nil;

      tmp:=AClass;

      repeat
        if tmp is TSpecializedType then
        begin
          tmpParams:=TSpecializedType(tmp).TheType.Parameters;

          tmpIndex:=FindInParams(tmpParams);

          if tmpIndex<>-1 then
          begin

            if Length(TSpecializedType(tmp).Generics)>tmpIndex then
            begin
              result:=TASTCreator.CreateGenericType(AOwner,
                                TVariable(tmpParams[tmpIndex]),
                                TTypeCall(TSpecializedType(tmp).Generics[tmpIndex]).TheType);
            end;

            Exit;
          end;
        end
        else
        if tmp is TParametersType then
        begin
          tmpParams:=TParametersType(tmp).Parameters;

          tmpIndex:=FindInParams(tmpParams);

          if tmpIndex<>-1 then
          begin
            // Always return Generic type when trying to find a type in
            // (class or routine) params:
            result:=TASTCreator.CreateGenericType(AOwner,
                                TVariable(tmpParams[tmpIndex]),
                                nil);

            Exit;
          end;
        end;


        if (result=nil) and (tmp is TClassType) then
           tmp:=TClassType(tmp).Ancestor
        else
           Exit;

      until tmp=nil;
    end;

  begin
    { unreachable:
    if AOwner is TTypeMember then
       result:=TryFind(TTypeMember(AOwner).Member)
    else
    }
    if AOwner is TType then
    begin
      result:=TryFindBackwards(TType(AOwner).Items);

      if result=nil then
      begin
        if AOwner is TClassType then
           if TClassType(AOwner).Ancestor<>nil then
              result:=FindTypeIn(TClassType(AOwner).Ancestor.Items,AType);

        if result=nil then
           if IsSameNamedType(AOwner,AType) then // Same type
              result:=TNamedType(AOwner)
           else
           begin
             // generics as specialized: Lee is Foo(Integer) {}
             if AOwner is TSpecializedType then
             begin
               result:=TryFindGenerics(TSpecializedType(AOwner).TheType.Parameters);

               if result=nil then
                  result:=FindType(TSpecializedType(AOwner).TheType,AType);
             end;

             if result=nil then
                if AOwner is TParametersType then
                   // generics as parameters: Foo(T:Type) is Bar {}
                   result:=TryFindAncestorGenerics(TParametersType(AOwner));

           end;
      end;
    end
    else
    {unreachable:
    if (AOwner is TWith) and (TWith(AOwner).Module<>nil) then
       result:=FindTypeIn(TWith(AOwner).Module.Items,AType)
    else
    }
       result:=nil;
  end;

begin
  AWithFound:=nil;

  while AOwner<>nil do
  begin
    result:=TryFind(AOwner);

    if result=nil then
       AOwner:=AOwner.Owner
    else
       Exit
  end;

  result:=nil;
end;

function TFinder.FindType(AOwner:TNode; const AType:String):TType;
var Dummy : TWith;
begin
  result:=FindType(AOwner,AType,Dummy);
end;

class function TFinder.FindParentTypeOf(ANode:TNode):TType;
begin
  repeat
    if ANode is TType then
       Exit(TType(ANode))
    else
       ANode:=ANode.Owner;

  until ANode=nil;

  result:=nil;
end;

class function TFinder.FindBreak(const ANode:TNode):Boolean;

   function FindIn(const ANodes:TNodes):Boolean; overload;
   var Node : TNode;
   begin
     for Node in ANodes do
         if FindBreak(Node) then
            Exit(True);

     result:=False;
   end;

   function FindIn(const AItems:TWhenItems):Boolean; overload;
   var Item : TWhenItem;
   begin
     for Item in AItems do
         if FindBreak(Item.Block) then
            Exit(True);

     result:=False;
   end;

begin
 if ANode=nil then
    result:=False
 else
 if ANode is TBreak then
    result:=True
 else
 if ANode is TIf then
    result:=FindBreak(TIf(ANode).ThenBlock) or // and
            FindBreak(TIf(ANode).ElseBlock)
 else
 {
   Cannot search for a "break" inside sub-loops !
 if ANode is TWhile then
    result:=FindReturn(TWhile(ANode).Block)
 else
 if ANode is TRepeat then
    result:=FindReturn(TRepeat(ANode).Block)
 else
 if ANode is TFor then
    result:=FindReturn(TFor(ANode).Block)
 else
 }
 if ANode is TWhen then
 begin
   result:=FindBreak(TWhen(ANode).ElseBlock);

   if not result then
      result:=FindIn(TWhen(ANode).Items);
 end
 else
// if ANode is TRoutine then
//    result:=FindIn(TRoutine(ANode).Items)
// else
 if ANode is TBlockStatement then
    result:=FindIn(TBlockStatement(ANode).Block.Items)
 else
    result:=False;
end;

class function TFinder.FindReturn(const ANode:TNode):Boolean;

  function FindIn(const ANodes:TNodes):Boolean; overload;
  var Node : TNode;
  begin
    for Node in ANodes do
        if FindReturn(Node) then
           Exit(True);

    result:=False;
  end;

  function FindIn(const AItems:TWhenItems):Boolean; overload;
  var Item : TWhenItem;
  begin
    for Item in AItems do
        if FindReturn(Item.Block) then
           Exit(True);

    result:=False;
  end;

  function DoFind(const ANode:TNode):Boolean;
  begin
   if ANode=nil then
      result:=False
   else
   if ANode is TReturn then
      result:=True
   else
   if ANode is TIf then
      result:=DoFind(TIf(ANode).ThenBlock) and
              DoFind(TIf(ANode).ElseBlock)
   else
   if ANode is TWhile then
      result:=DoFind(TWhile(ANode).Block)
   else
   if ANode is TRepeat then
      result:=DoFind(TRepeat(ANode).Block)
   else
   if ANode is TFor then
      result:=DoFind(TFor(ANode).Block)
   else
   if ANode is TWhen then
   begin
     result:=DoFind(TWhen(ANode).ElseBlock);

     if not result then
        result:=FindIn(TWhen(ANode).Items);
   end
   else
   if ANode is TBlockStatement then
      result:=FindIn(TBlockStatement(ANode).Block.Items)
   else
      result:=False;
  end;

begin
  if ANode is TRoutine then
     result:=FindIn(TRoutine(ANode).Items)
  else
     result:=DoFind(ANode);
end;

function TFinder.FindInherited(const AClass: TClassType; const ARoutine: TRoutine): TRoutine;
var tmp : TClassType;
begin
  result:=nil;

  tmp:=AClass;

  while tmp<>nil do
  begin
    result:=FindOverride(ARoutine,tmp.Items);

    if result=nil then
       tmp:=tmp.Ancestor as TClassType
    else
       break;
  end;
end;

function TFinder.FindMethodIn(const ANodes:TNodes; const ARoutine:String):TRoutine;
var N : TNode;
begin
  for N in ANodes do
    if (N is TRoutine) and Checker.TextIs(TRoutine(N).Name,ARoutine) then
       Exit(TRoutine(N));

  result:=nil;
end;

function TFinder.FindMethodIn(const ANodes:TNodes; const ARoutine:TChecker.TRoutineDefinition):TRoutine;
var N : TNode;
    tmp : TRoutine;
begin
  for N in ANodes do
  begin
    if N is TRoutine then
       tmp:=TRoutine(N)
    else
    if (N is TExtender) and (TExtender(N).Extension is TRoutine) then
       tmp:=TExtender(N).Extension as TRoutine
    else
       tmp:=nil;

    if tmp<>nil then
       if Checker.SameRoutine(tmp,ARoutine) then
          Exit(tmp)
  end;

  result:=nil;
end;

class function TFinder.GetAncestorType(const AType:TType):TType;
var tmp : TType;
begin
  tmp:=TChecker.GetFinalSpecialized(AType);

  if tmp is TClassType then
     result:=TClassType(tmp).Ancestor
  else
     result:=nil;
end;

// Return identical routine in ancestor class type
function TFinder.FindOverride(AOwner: TType; const ARoutine: TRoutine): TRoutine;
begin
  result:=nil;

  if AOwner<>nil then
  repeat
    result:=FindOverride(ARoutine,AOwner.Items);

    if result=nil then
       AOwner:=GetAncestorType(AOwner)
    else
       break;

  until AOwner=nil;
end;

function TFinder.FindOverride(const ARoutine:TRoutine; const AItems:TNodes):TRoutine;
var N : TNode;
begin
  for N in AItems do
      if (N is TRoutine) and Checker.SameSignature(ARoutine,TRoutine(N)) then
         Exit(TRoutine(N));

  result:=nil;
end;

end.
