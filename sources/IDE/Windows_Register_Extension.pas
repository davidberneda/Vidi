unit Windows_Register_Extension;

interface

{
  Register the *.vidi file extension to allow using Vidi IDE
  when double-clicking a file in Windows Explorer
}

procedure RegisterFileExtension(const AExtension:String);

implementation

uses
  Registry, Windows, ShlObj, SysUtils,
  Constants;

procedure RegisterFileExtension(const AExtension:String);
const
  App_Vidi='application/'+TVidiConstants.NoDot_Extension;
  Vidi=TVidiConstants.Vidi_Name;
  Vidi_MIME='\MIME\Database\Content Type\'+App_Vidi;

var tmp : TRegistry;

  function ExistExtension:Boolean;
  begin
    result:=tmp.KeyExists(AExtension);

    if result then
       if tmp.OpenKey(Vidi+'\DefaultIcon',True) then
          result:=tmp.ReadString('')=ParamStr(0)+',0';
  end;

begin
  tmp:=TRegistry.Create(KEY_ALL_ACCESS);

  with tmp do
  try
    RootKey:=HKEY_CLASSES_ROOT;

    if not ExistExtension then { check if exists... }
    begin
      { extension }
      if OpenKey(AExtension,True) then
      try
        WriteString('',Vidi);
        WriteString('Content Type',App_Vidi);
        WriteString('PerceivedType','text');
      finally
        CloseKey;
      end;

      { application }
      if OpenKey(Vidi,True) then
      begin
        WriteString('',Vidi+' file');

        if OpenKey('DefaultIcon',True) then
        begin
          WriteString('',ParamStr(0)+',0');
          CloseKey;
        end;

        if OpenKey(Vidi,True) then
           if OpenKey('Shell',True) then
              if OpenKey('Open',True) then
              begin
                 WriteString('','Open with '+Vidi);

                 if OpenKey('Command',True) then
                    WriteString('',ParamStr(0)+' "%1"');
              end;
      end;

      // notify Windows on creating the file association
      SHChangeNotify(SHCNE_ASSOCCHANGED,SHCNF_IDLIST,nil,nil);
    end;

    { Add MIME content-type to registry database... }
    if not KeyExists(Vidi_MIME) then
    begin
      if OpenKey(Vidi_MIME,True) then
         WriteString('Extension',AExtension);
    end;
  finally
    Free;
  end;
end;

end.
