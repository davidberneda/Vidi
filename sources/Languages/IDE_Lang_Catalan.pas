unit IDE_Lang_Catalan;

interface

uses
  Forms;

procedure Change(const AForm:TForm);

implementation

uses
  Unit_TheBee, IDE_Lang_English;

procedure Change(const AForm:TForm);
begin
  with Vidi_Lang do
  begin
    Name:=  'Nom';
    _Type:= 'Tipus';
    Value:= 'Valor';
    About:= 'Sobre Vidi';
    Module := Translate('Mòdul');
    Line   := Translate('Línia');
    Column := 'Columna';
    Text   := 'Texte';
    _Class := 'Classe';
    Fields := 'Camps';
    Types  := 'Tipus';
    Breaks := Translate('Punts d''interrupció');
    Trace  := 'Rastreig';
    Watch  := 'Observar';
    Changed := 'Modificat';
 JumpToLine := Translate('Saltar a línia');
 Vidi_Files := 'Arxius Vidi';

  DoYouWantToSave := Translate('El mòdul: [%s] ha estat modificat.'+CRLF+'Voleu guardar-lo?');
  SelectVidiSystemFolder := 'Seleccioneu la carpeta on estan els arxius fonts del sistema Vidi';
  SureOverwrite   := 'Esteu segurs de sobre-escriure l''arxiu:'+CRLF+'[%s] ?';

    ShowAncestorItems := 'Mostrar items heredats';
    References  := Translate('Referències');

    _Uses  := 'Usos';
    UsedBy := 'Usat per';

    Callers:= 'Cridats';
    Folder := 'Carpeta';
    StopRunAndClose:=Translate('Parar la execució i tancar?');

    Routine:= Translate('Funció');
    Count  := 'Total';
    Elapsed:= 'Temps';
    Item   := 'Element';
    Index  := Translate('Índex');
    Owner  := 'Propietari';
    Children:='Fills';
    FirstVariable:='1ª Variable';
    Reference := Translate('Referència');
    Path  := 'Carpeta';
    Found := 'Existeix';

    NoNewVersion  := 'Ja esteu fent servir l''última versiò';
    Updating      := 'Actualitzant a la versiò: %s';
    DownloadError := 'Error descarregant l''arxiu: ';
    Installing    := 'Instal.lant';
    Downloading   := 'Descarregant';
    SureToUpdate  := 'Vols actualitzar la versió?'#13#10#13#10'Actual: %s'#13#10'Nova: %s';
  end;

  with TFormVidi(AForm) do
  begin
    Changed_Text:=       Vidi_Lang.Changed;

    TabAST.Caption:=     'AST';
    TabModules.Caption:= Translate('Mòduls');
    TabTypes.Caption:=   'Tipus';
    TabErrors.Caption:=  'Errors';
    TabPrompt.Caption:=  'Comandament';
    TabSearch.Caption:=  'Cerca';
    TabConsole.Caption:= 'Consola';
    TabDebugger.Caption:='Depurador';
    TabFileExplorer.Caption:='Explorador';
    TabProfiler.Caption:='Medidor';
    TabStats.Caption:=   Translate('Estadístiques');
    TabRecent.Caption:=  'Recents';

    File1.Caption:=      '&Arxiu';
     New1.Caption:=      '&Nou...';
     Open1.Caption:=     '&Obrir...';
     Reopen1.Caption:=   '&Reobrir';
     Save1.Caption:=     '&Guardar';
     SaveAs1.Caption:=   'G&uardar com...';
     SaveAll1.Caption:=  'Guardar T&ot';
     Close1.Caption:=    '&Tancar';
     CloseAll1.Caption:= 'Tanca&r Tot';
     Exit1.Caption:=     '&Sortir';

    Edit1.Caption:=      '&Editar';
     Cut1.Caption:=      '&Tallar';
     Copy1.Caption:=     '&Copiar';
     Paste1.Caption:=    '&Enganxar';
     Delete1.Caption:=   '&Borrar';
     SelectAll1.Caption:='&Seleccionar Tot';
     Undo1.Caption:=     '&Desfer';
     Search1.Caption:=   'B&uscar...';

    Options1.Caption:=   '&Opcions';
     Font1.Caption:=      '&Font';
      Code1.Caption:=      '&Editor...';
      Run2.Caption:=       '&Consola...';
     Language1.Caption:=  '&Idioma';
     Keyboard1.Caption:=  'Te&clat';
     hemes1.Caption:=     '&Temes';
      Day1.Caption:=        '&Dia';
      Night1.Caption:=      '&Nit';
     Internal1.Caption:=  '&Intern';

    View1.Caption:=      '&Mostrar';
     Debugger1.Caption:=    '&Depurador';
     FileExplorer1.Caption:='&Explorador';
     Log1.Caption:=         '&Registre';
     Modules1.Caption:=     Translate('&Mòduls');
     Profiler1.Caption:=    'Med&idor';
     Recent1.Caption:=      'Re&cents';
     Statistics1.Caption:=  Translate('E&stadístiques');
     ypes1.Caption:=        '&Tipus';
     oolBar1.Caption:=      '&Barra d''eines';

    Run1.Caption:=       Translate('E&xecució');
     Start1.Caption:=     '&Iniciar';
     Stop1.Caption:=      '&Aturar';
     Pause1.Caption:=     '&Pausar';
     Stepin1.Caption:=    'Pas &entrant';
     Stepover1.Caption:=  'Pas per &sobre';
     Breakhere1.Caption:= Translate('At&ura aquí');
     ogglebreak1.Caption:= 'Canvia pu&nt';
     Prompt1.Caption:=    'C&omandaments';
     CompileOnly1.Caption:='&Compilar';

    Help1.Caption:=      '&Ajuda';
     LanguageReference1.Caption:= Translate('&Manual de Referència...');
     About1.Caption:=      '&Sobre...';
  end;
end;

procedure ChangeReserved;
begin
  Vidi_Reserved._ancestor := 'ancestre';
  Vidi_Reserved._and      := 'i';
  Vidi_Reserved._break    := 'trenca';
  Vidi_Reserved._catch    := 'captura';
  Vidi_Reserved._continue := 'continua';
  Vidi_Reserved._else     := 'sino';
  Vidi_Reserved._False    := 'Fals';
  Vidi_Reserved._final    := 'final';
  Vidi_Reserved._finally  := 'finalment';
  Vidi_Reserved._for      := 'per';
  Vidi_Reserved._hidden   := 'ocult';
  Vidi_Reserved._if       := 'si';
  Vidi_Reserved._in       := 'en';
  Vidi_Reserved._indexed  := 'indexat';
  Vidi_Reserved._is       := 'és';
  Vidi_Reserved._not      := 'no';
  Vidi_Reserved._or       := 'o';
  Vidi_Reserved._out      := 'torna';
  Vidi_Reserved._repeat   := 'repeteix';
  Vidi_Reserved._return   := 'retorna';
  Vidi_Reserved._self     := 'jo';
  Vidi_Reserved._shared   := 'compartit';
  Vidi_Reserved._to       := 'a';
  Vidi_Reserved._True     := 'Cert';
  Vidi_Reserved._try      := 'intenta';
  Vidi_Reserved._until    := 'fins';
  Vidi_Reserved._when     := 'quan';
  Vidi_Reserved._while    := 'mentres';
  Vidi_Reserved._xor      := 'exo';
  Vidi_Reserved._with     := 'amb';
end;

end.
