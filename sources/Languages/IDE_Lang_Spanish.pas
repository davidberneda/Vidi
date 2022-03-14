unit IDE_Lang_Spanish;

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
    Name:=  'Nombre';
    _Type:= 'Tipo';
    Value:= 'Valor';
    About:= 'Acerca de Vidi';
    Module := 'Módulo';
    Line   := 'Línea';
    Column := 'Columna';
    Text   := 'Texto';
    _Class := 'Clase';
    Fields := 'Campos';
    Types  := 'Tipos';
    Breaks := 'Puntos de interrupción';
    Trace  := 'Rastreo';
    Watch  := 'Observar';
    Changed := 'Modificado';
 JumpToLine := 'Saltar a línea';
 Vidi_Files := 'Archivos Vidi';

  DoYouWantToSave := 'El módulo: [%s] ha sido modificado.'+CRLF+'Desea guardarlo?';
  SelectVidiSystemFolder := 'Seleccione la carpeta donde estan los archivos fuentes del sistema Vidi';
  SureOverwrite   := 'Está seguro de sobreescribir el archivo:'+CRLF+'[%s] ?';

    ShowAncestorItems := 'Mostrar items heredados';
    References  := 'Referencias';

    _Uses  := 'Usa';
    UsedBy := 'Usado por';

    Callers:= 'Llamantes';
    Folder := 'Carpeta';

    StopRunAndClose:='¿Terminar la ejecución y cerrar?';

    Routine:= 'Función';
    Count  := 'Total';
    Elapsed:= 'Tiempo';
    Item   := 'Elemento';
    Index  := 'Indice';
    Owner  := 'Propietario';
    Children:='Hijos';
    FirstVariable:='1ª Variable';
    Reference := 'Referencia';
    Path  := 'Carpeta';
    Found := 'Existe';

    NoNewVersion  := 'Ya estás usando la última versión';
    Updating      := 'Actualizando a la versión: %s';
    DownloadError := 'Error al descargar el archivo: ';
    Installing    := 'Instalando';
    Downloading   := 'Descargando';
    SureToUpdate  := 'Quieres actualizar la versión?'#13#10#13#10'Actual: %s'#13#10'Nueva: %s';
  end;

  with TFormVidi(AForm) do
  begin
    Changed_Text:=       Vidi_Lang.Changed;

    TabAST.Caption:=     'AST';
    TabModules.Caption:= Translate('Módulos');
    TabTypes.Caption:=   'Tipos';
    TabErrors.Caption:=  'Errores';
    TabPrompt.Caption:=  'Comando';
    TabSearch.Caption:=  Translate('Búsqueda');
    TabConsole.Caption:= 'Consola';
    TabDebugger.Caption:='Depurador';
    TabFileExplorer.Caption:='Explorador';
    TabProfiler.Caption:='Medidor';
    TabStats.Caption:=   Translate('Estadísticas');
    TabRecent.Caption:=  'Recientes';

    File1.Caption:=      '&Archivo';
     New1.Caption:=      '&Nuevo...';
     Open1.Caption:=     '&Abrir...';
     Reopen1.Caption:=   '&Reabrir';
     Save1.Caption:=     '&Guardar';
     SaveAs1.Caption:=   'G&uardar como...';
     SaveAll1.Caption:=  'Guardar T&odo';
     Close1.Caption:=    '&Cerrar';
     CloseAll1.Caption:= 'Cerra&r Todo';
     Exit1.Caption:=     '&Salir';

    Edit1.Caption:=      '&Editar';
     Cut1.Caption:=      'C&ortar';
     Copy1.Caption:=     '&Copiar';
     Paste1.Caption:=    '&Pegar';
     Delete1.Caption:=   '&Eliminar';
     SelectAll1.Caption:='&Seleccionar Todo';
     Undo1.Caption:=     '&Deshacer';
     Search1.Caption:=   '&Buscar...';

    Options1.Caption:=   '&Opciones';
     Font1.Caption:=      '&Fuente';
      Code1.Caption:=      '&Editor...';
      Run2.Caption:=       '&Consola...';
     Language1.Caption:=  '&Idioma';
     Keyboard1.Caption:=  'Te&clado';
     hemes1.Caption:=     '&Temas';
      Day1.Caption:=        '&Dia';
      Night1.Caption:=      '&Noche';
     Internal1.Caption:=  '&Interno';

    View1.Caption:=      '&Ver';
     Debugger1.Caption:=    '&Depurador';
     FileExplorer1.Caption:='&Explorador';
     Log1.Caption:=         '&Registro';
     Modules1.Caption:=     Translate('&Módulos');
     Profiler1.Caption:=    'Med&idor';
     Recent1.Caption:=      'Re&cientes';
     Statistics1.Caption:=  Translate('E&stadísticas');
     ypes1.Caption:=        '&Tipos';
     oolBar1.Caption:=      '&Barra de herramientas';

    Run1.Caption:=       Translate('&Ejecución');
     Start1.Caption:=     '&Iniciar';
     Stop1.Caption:=      '&Parar';
     Pause1.Caption:=     'P&ausar';
     Stepin1.Caption:=    'Paso &entrante';
     Stepover1.Caption:=  'Paso p&or encima';
     Breakhere1.Caption:= Translate('Para aq&uí');
     ogglebreak1.Caption:= 'Cambia pu&nto';
     Prompt1.Caption:=    'Co&mandos';
     CompileOnly1.Caption:='&Compilar';

    Help1.Caption:=      '&Ayuda';
     LanguageReference1.Caption:= '&Referencia del Lenguaje...';
     About1.Caption:=      '&Acerca de...';
  end;
end;

procedure ChangeReserved;
begin
  Vidi_Reserved._ancestor := 'ancestro';
  Vidi_Reserved._and      := 'y';
  Vidi_Reserved._break    := 'rompe';
  Vidi_Reserved._catch    := 'captura';
  Vidi_Reserved._continue := 'continua';
  Vidi_Reserved._else     := 'sino';
  Vidi_Reserved._False    := 'Falso';
  Vidi_Reserved._final    := 'final';
  Vidi_Reserved._finally  := 'finalmente';
  Vidi_Reserved._for      := 'para';
  Vidi_Reserved._hidden   := 'oculto';
  Vidi_Reserved._if       := 'si';
  Vidi_Reserved._in       := 'en';
  Vidi_Reserved._indexed  := 'indexado';
  Vidi_Reserved._is       := 'es';
  Vidi_Reserved._not      := 'no';
  Vidi_Reserved._or       := 'o';
  Vidi_Reserved._out      := 'devuelve';
  Vidi_Reserved._repeat   := 'repite';
  Vidi_Reserved._return   := 'retorna';
  Vidi_Reserved._self     := 'yo';
  Vidi_Reserved._shared   := 'compartido';
  Vidi_Reserved._to       := 'a';
  Vidi_Reserved._True     := 'Cierto';
  Vidi_Reserved._try      := 'intenta';
  Vidi_Reserved._until    := 'hasta';
  Vidi_Reserved._when     := 'cuando';
  Vidi_Reserved._while    := 'mientras';
  Vidi_Reserved._xor      := 'exo';
  Vidi_Reserved._with     := 'con';
end;

end.
