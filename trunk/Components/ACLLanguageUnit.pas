Unit ACLLanguageUnit;

Interface

uses
  OS2Def,
  Classes,
  Forms;

type
  TLanguageItem = record
    pValue: pstring;
    Used: boolean;
  end;
  TPLanguageItem = ^ TLanguageItem;

  TLanguageFile = class
  protected
    FItems: TStringList;
    FFilename: string;
    FPrefix: string;

    // only if saving...
    FOutputFile: TextFile;
    FSaving: boolean;

    procedure GetValue( const Index: longint;
                        var Value: string );

    procedure LoadComponentLanguageInternal( Component: TComponent;
                                             const Path: string;
                                             const DoUpdates: boolean );

    procedure SaveItem( const Name: string;
                        const Value: string;
                        const Marker: string; );

  public
    constructor Create( const Filename: string );
    destructor Destroy; override;

    // if DoUpdates is true, then the component and it's
    // owned components will be updated. If false, then
    // it will only be checked and missing items noted.

    // Also sets Prefix to name of component with a dot, for convenience
    // in loading strings manually, related to the component
    procedure LoadComponentLanguage( Component: TComponent;
                                     DoUpdates: boolean );

    // Starts saving to the file
    procedure StartUpdate;
    procedure EndUpdate;

    // Looks for <prefix>.<name>
    function GetString( const Name: string;
                        const Default: string ): string;

    // If apply is true, then assign S the string called title
    // Or, if not found, use Default.
    // If apply is false, just look it up but don't assign to S
    procedure LL( const Apply: boolean;
                  Var S: string;
                  const Title: string;
                  const Default: string );

    property Prefix: string read FPrefix write FPrefix;

  end;

  // callback for when language events occur
  // If apply is true, then the specified language has been loaded
  // If apply is false, then the specified language file is being
  // saved, so you should access any strings you need,
  // but not actually change anything
  TLanguageEvent = procedure( Language: TLanguageFile;
                              Apply: boolean ) of object;

  // non-object version
  TLanguageProc = procedure( Language: TLanguageFile;
                             Apply: boolean );

var
  g_CurrentLanguageFile: TLanguageFile;

// Register that you want to know when the current language changes
// Will immediately call you back, if there is a current language
procedure RegisterForLanguages( Callback: TLanguageEvent );

// Register a procedure callback (not-object)
procedure RegisterProcForLanguages( Callback: TLanguageProc );


// Register a procedure that will be called only when a language file
// is being updated (use to load forms if needed)
procedure RegisterUpdateProcForLanguages( Callback: TProcedure );


// Change current language to given, and tell everybody who has registered
procedure ApplyLanguage( Language: TLanguageFile );

// Tell everybody who has registered to access the given file,
// but don't apply strings from it
procedure UpdateLanguage( Language: TLanguageFile );

// Load a string.
// If Language is nil then just assign default.
// If apply is true, then assign S the string called title
// Or, if not found, use Default.
// If apply is false, just look it up but don't assign to S
procedure LoadString( Language: TLanguageFile;
                      const Apply: boolean;
                      Var S: string;
                      const Title: string;
                      const Default: string );

// Load and apply specified language file
procedure LoadLanguage( const FilePath: string );

// load a language from the standard location.
// AppName is a short name for the app, e.g. 'newview'
// language spec is e.g. 'es' or 'es_es'
function LoadAutoLanguage( const AppName: string;
                           const LanguageSpec: string ): boolean;

// load default language based on LANG environment var
Procedure LoadDefaultLanguage( const AppName: string );

Implementation

uses
  Dos, SysUtils, // system
  StdCtrls,
  Buttons,
  ExtCtrls,
  TabCtrls,
  Dialogs,
  Coolbar2,
  Multicolumnlistbox,
  ACLUtility,
  ACLStringUtility,
  FileUtilsUnit;

var
  g_LanguageCallbacks: TList;
  g_LanguageUpdateCallbacks: TList;

Type
  TLanguageCallback = class
    FCallbackMethod: TLanguageEvent;
    FCallbackProc: TLanguageProc;
    constructor CreateMethod( CallbackMethod: TLanguageEvent );
    constructor CreateProc( CallbackProc: TLanguageProc );
  end;

constructor TLanguageCallback.CreateMethod( CallbackMethod: TLanguageEvent );
begin
  FCallbackMethod := CallbackMethod;
  FCallbackProc := nil;
end;

constructor TLanguageCallback.CreateProc( CallbackProc: TLanguageProc );
begin
  FCallbackProc := CallbackProc;
  FCallbackMethod := nil;
end;

procedure AddLanguageCallback( CallbackObject: TLanguageCallback );
begin
  if g_LanguageCallbacks = nil then
    g_LanguageCallbacks := TList.Create;

  g_LanguageCallbacks.Add( CallbackObject );
end;

procedure RegisterForLanguages( Callback: TLanguageEvent );
begin
  AddLanguageCallback( TLanguageCallback.CreateMethod( Callback ) );

  if g_CurrentLanguageFile <> nil then
    Callback( g_CurrentLanguageFile, true );
end;

procedure RegisterProcForLanguages( Callback: TLanguageProc );
begin
  AddLanguageCallback( TLanguageCallback.CreateProc( Callback ) );

  if g_CurrentLanguageFile <> nil then
    Callback( g_CurrentLanguageFile, true );
end;

procedure RegisterUpdateProcForLanguages( Callback: TProcedure );
begin
  if g_LanguageUpdateCallbacks = nil then
    g_LanguageUpdateCallbacks := TList.Create;

  g_LanguageUpdateCallbacks.Add( TObject( Callback ) );
  // since this is for when updating a language only, we don't immediately call it
end;

procedure ApplyLanguage( Language: TLanguageFile );
var
  i: longint;
  Callback: TLanguageCallback;
begin
  if g_CurrentLanguageFile <> nil then
    g_CurrentLanguageFile.Destroy;

  g_CurrentLanguageFile := Language;

  // do language callbacks to everyone
  for i := 0 to g_LanguageCallbacks.Count - 1 do
  begin
    Callback := g_LanguageCallbacks[ i ];
    if Assigned( Callback.FCallbackMethod ) then
      Callback.FCallbackMethod( g_CurrentLanguageFile, true );
    if Assigned( Callback.FCallbackProc ) then
      Callback.FCallbackProc( g_CurrentLanguageFile, true );
  end;
end;

procedure UpdateLanguage( Language: TLanguageFile );
var
  i: longint;
  Callback: TLanguageCallback;
  UpdateProc: TProcedure;
begin
  if g_LanguageUpdateCallbacks <> nil then
  begin
    // first call all update callbacks so dynamically created
    // things can be loaded (i.e. forms)
    // Note: this will cause them to load their strings from
    // the current language if any. This is fine, necessary even.
    for i := 0 to g_LanguageUpdateCallbacks.Count - 1 do
    begin
      UpdateProc := TProcedure( g_LanguageUpdateCallbacks[ i ] );
      UpdateProc;
    end;
  end;

  Language.StartUpdate;

  if g_LanguageCallbacks <> nil then
  begin
    // now call the language events
    for i := 0 to g_LanguageCallbacks.Count - 1 do
    begin
      Callback := g_LanguageCallbacks[ i ];
      if Assigned( Callback.FCallbackMethod ) then
        Callback.FCallbackMethod( Language, false );
      if Assigned( Callback.FCallbackProc ) then
        Callback.FCallbackProc( Language, false );
    end;
  end;

  Language.EndUpdate;
end;

constructor TLanguageFile.Create( const Filename: string );
var
  F: TextFile;
  S: string;
  Name: string;
  Value: string;
  p: longint;
  pItem: TPLanguageItem;
begin
  FSaving := false;

  FFilename := Filename;

  FPrefix := '';

  FItems := TStringList.Create;
  FItems.Sorted := true; // for lookup speed
  FItems.CaseSensitive := true; // also for speed. We manually convert to uppercase.
  FItems.Duplicates := dupAccept;

  if not FileExists( Filename ) then
    exit;

  FileMode := fmInput;
  AssignFile( F, Filename );
  Reset( F );

  while not Eof( F ) do
  begin
    ReadLn( F, S );

    p := 1;
    GetNextQuotedValue( S, p, Name, DoubleQuote );

    if Name <> '' then
    begin
      if Name[ 1 ] <> '#' then
      begin
        // Got a name, read the value and store.
        GetNextQuotedValue( S, p, Value, DoubleQuote );

        New( pItem );
        pItem ^. pValue := NewStr( Value );
        pItem ^. Used := false;
        FItems.AddObject( UpperCase( Name ),
                          TObject( pItem ) );
      end;
    end;
  end;

  CloseFile( F );
end;

destructor TLanguageFile.Destroy;
var
  i: longint;
  pItem: TPLanguageItem;
begin
  for i := 0 to FItems.Count - 1 do
  begin
    pItem := TPLanguageItem( FItems.Objects[ i ] );
    DisposeStr( pItem ^. pValue );
    Dispose( pItem );
  end;
  FItems.Destroy;
end;

procedure TLanguageFile.GetValue( const Index: longint;
                                  var Value: string );
var
  pItem: TPLanguageItem;
begin
  pItem := TPLanguageItem( FItems.Objects[ Index ] );
  pItem ^. Used := true;
  Value := pItem ^. pValue ^;
end;

// Magical procedure that does certain things...
// If Apply is true, then it looks up title
// and if found, assigns it's value to S
// If not found, then assigns Default to S
// If Apply is false, then does lookup only, does not assign S
// In either case, if the string is not found,
// it will be added to missing items list
procedure TLanguageFile.LL( const Apply: boolean;
                            Var S: string;
                            const Title: string;
                            const Default: string );
begin
  if Apply then
    S := GetString( Title, Default )
  else
    GetString( Title, Default );
end;

procedure TLanguageFile.LoadComponentLanguage( Component: TComponent;
                                               DoUpdates: boolean );
begin
  LoadComponentLanguageInternal( Component, '', DoUpdates );
  Prefix := Component.Name + '.';
end;

procedure TLanguageFile.StartUpdate;
var
  BackupFilename: string;
begin
  BackupFilename := ChangeFileExt( FFilename, '.bak' );
  if FileExists( BackupFilename ) then
    if not DeleteFile( BackupFilename ) then
      raise Exception.Create( 'Unable to delete backup language file: '
                              + BackupFilename );
  if FileExists( FFilename ) then
    // backup
    if not CopyFile( FFilename, BackupFilename ) then
      raise Exception.Create( 'Unable to copy to backup language file: '
                              + BackupFilename );

  AssignFile( FOutputFile, FFilename );

  Rewrite( FOutputFile );

  FSaving := true;
end;

procedure TLanguageFile.EndUpdate;
var
  i: longint;
  pItem: TPLanguageItem;
  Notified: boolean;
begin
  Notified := false;

  for i := 0 to FItems.Count - 1 do
  begin
    pItem := TPLanguageItem( FItems.Objects[ i ] );
    if not pItem ^. Used then
    begin
      if not Notified then
      begin
        Writeln( FOutputFile,
                 '# *** The following items are no longer needed.' );
        Writeln( FOutputFile,
                 '# You can delete them after checking they are of no use.' );
        Notified := true;
      end;

      SaveItem( '# ' + FItems[ i ],
                pItem ^. pValue^,
                '' );
    end;
  end;

  FSaving := false;
  CloseFile( FOutputFile );
end;

procedure TLanguageFile.SaveItem( const Name: string;
                                  const Value: string;
                                  const Marker: string );

var
  QuotedValue: string;
begin
  QuotedValue :=
    StrDoubleQuote(
      InsertDuplicateChars( Value, DoubleQuote ) );
  WriteLn( FOutputFile,
           Name + ' ' + QuotedValue + ' ' + Marker );
end;

procedure TLanguageFile.LoadComponentLanguageInternal( Component: TComponent;
                                                       const Path: string;
                                                       const DoUpdates: boolean );
var
  i : longint;
  ComponentPath: string;
  Value: string;

  MenuItem: TMenuItem;
  Button: TButton;
  TheLabel: TLabel;
  RadioGroup: TRadioGroup;
  TabSet: TTabSet;
  TabbedNotebook: TTabbedNotebook;
  Form: TForm;
  RadioButton: TRadioButton;
  CheckBox: TCheckBox;
  CoolBar2: TCoolBar2;
  GroupBox: TGroupBox;
  MultiColumnListBox: TMultiColumnListBox;
  SystemOpenDialog: TSystemOpenDialog;
  SystemSaveDialog: TSystemSaveDialog;

  // searches for componentpath + name, sets value if found
  function FindIt( const Name: string;
                   const Default: string ): boolean;
  var
    Index: longint;
  begin
    result := FItems.Find( UpperCase( ComponentPath + Name ), Index );

    if result then
    begin
      GetValue( Index, Value );
      if FSaving then
        // save the specified value
        SaveItem( ComponentPath + Name, Value, '' );
    end
    else
    begin
      if FSaving then
        // save the default.
        SaveItem( ComponentPath + Name, Default, '***' );
    end;

    if result then
      // found
      if not DoUpdates then
        // not doing updates, so pretend we didn't, so we don't apply it
        // (this is a local hack only)
        result := false;
  end;

Begin
  ComponentPath := Path + Component.Name + '.';

  // Components sorted with most common at top, ish...

  if Component is TMenuItem then
  begin
    MenuItem := TMenuItem( Component );
    if MenuItem.Caption <> '-' then
    begin
      // skip separators
      if FindIt(  'Caption', MenuItem.Caption ) then
        MenuItem.Caption := Value;
      if FindIt( 'Hint', MenuItem.Hint ) then
        MenuItem.Hint := Value;
    end;
  end

  else if Component is TButton then
  begin
    Button := TButton( Component );
    if FindIt( 'Caption', Button.Caption ) then
      Button.Caption := Value;
    if FindIt( 'Hint', Button.Hint ) then
      Button.Hint := Value;
  end

  else if Component is TLabel then
  begin
    TheLabel := TLabel( Component );
    if FindIt( 'Caption', TheLabel.Caption ) then
      TheLabel.Caption := Value;
    if FindIt( 'Hint', TheLabel.Hint ) then
      TheLabel.Hint := Value;
  end

  else if Component is TRadioGroup then
  begin
    RadioGroup := TRadioGroup( Component );
    if FindIt( 'Caption', RadioGroup.Caption ) then
      RadioGroup.Caption := Value;
    for i := 0 to RadioGroup.Items.Count - 1 do
      if FindIt( 'Item' + IntToStr( i ),
                 RadioGroup.Items[ i ] ) then
        RadioGroup.Items[ i ] := Value;
  end

  else if Component is TTabSet then
  begin
    TabSet := TTabSet( Component );
    for i := 0 to TabSet.Tabs.Count - 1 do
      if FindIt( 'Tab' + IntToStr( i ),
                 TabSet.Tabs[ i ] ) then
        TabSet.Tabs[ i ] := Value;
  end

  else if Component is TTabbedNotebook then
  begin
    TabbedNotebook := TTabbedNotebook( Component );
    for i := 0 to TabbedNotebook.Pages.Count - 1 do
    begin
      if FindIt( 'Tab' + IntToStr( i ) + '.Caption',
                 TabbedNotebook.Pages[ i ] ) then
        TabbedNotebook.Pages[ i ] := Value;

      if FindIt( 'Tab' + IntToStr( i ) + '.Hint',
                 TabbedNotebook.Pages.Pages[ i ].Hint ) then
        TabbedNotebook.Pages.Pages[ i ].Hint := Value;
    end;
  end

  else if Component is TForm then
  begin
    Form := TForm( Component );
    if FindIt( 'Caption', Form.Caption ) then
      Form.Caption := Value;

    // load owned controls
    for i := 0 to Component.ComponentCount - 1 do
      LoadComponentLanguageInternal( Component.Components[ i ],
                                     ComponentPath,
                                     DoUpdates );

  end

  else if Component is TRadioButton then
  begin
    RadioButton := TRadioButton( Component );
    if FindIt( 'Caption', RadioButton.Caption ) then
      RadioButton.Caption := Value;
    if FindIt( 'Hint', RadioButton.Hint ) then
      RadioButton.Hint := Value;
  end

  else if Component is TCheckBox then
  begin
    CheckBox := TCheckBox( Component );
    if FindIt( 'Caption', CheckBox.Caption ) then
      CheckBox.Caption := Value;
    if FindIt( 'Hint', CheckBox.Hint ) then
      CheckBox.Hint := Value;
  end

  else if Component is TCoolBar2 then
  begin
    CoolBar2 := TCoolBar2( Component );
    for i := 0 to CoolBar2.Sections.Count - 1 do
    begin
      if FindIt( 'Item' + IntToStr( i ),
                 CoolBar2.Sections[ i ].Text ) then
        CoolBar2.Sections[ i ].Text := Value;
//      if FindIt( 'Hint' + IntToStr( i ) ) then
//        TCoolBar2( Component ).Sections[ i ].Hint := Value;
    end;
  end

  else if Component is TGroupBox then
  begin
    GroupBox := TGroupBox( Component );
    if FindIt( 'Caption', GroupBox.Caption ) then
      GroupBox.Caption := Value;
    if FindIt( 'Hint', GroupBox.Hint ) then
      GroupBox.Hint := Value;
  end
  else if Component is TMultiColumnListBox then
  begin
    MultiColumnListBox := TMultiColumnListBox( Component );
    for i := 0 to MultiColumnListBox.HeaderColumns.Count - 1 do
      if FindIt( 'Column' + IntToStr( i ),
                 MultiColumnListBox.HeaderColumns[ i ].Text ) then
        MultiColumnListBox.HeaderColumns[ i ].Text := Value;
  end
  else if Component is TSystemOpenDialog then
  begin
    SystemOpenDialog := TSystemOpenDialog( Component );
    if FindIt( 'OKName', SystemOpenDialog.OKName ) then
      SystemOpenDialog.OKName := Value;
    if FindIt( 'Title', SystemOpenDialog.Title ) then
      SystemOpenDialog.Title := Value;
  end
  else if Component is TSystemSaveDialog then
  begin
    SystemSaveDialog := TSystemSaveDialog( Component );
    if FindIt( 'OKName', SystemSaveDialog.OKName ) then
      SystemSaveDialog.OKName := Value;
    if FindIt( 'Title', SystemSaveDialog.Title ) then
      SystemSaveDialog.Title := Value;

  end;

end;

function TLanguageFile.GetString( const Name: string;
                                  const Default: string ): string;
var
  Index: longint;
  Found: boolean;
begin
  Found := FItems.Find( UpperCase( Prefix + Name ), Index );

  if Found then
  begin
    GetValue( Index, Result );
    if FSaving then
      SaveItem( Prefix + Name, Result, '' );
  end
  else
  begin
    Result := Default;
    if FSaving then
      SaveItem( Prefix + Name, Default, '***' );
  end;

end;

procedure LoadString( Language: TLanguageFile;
                      const Apply: boolean;
                      Var S: string;
                      const Title: string;
                      const Default: string );
begin
  if Language = nil then
  begin
    if Apply then
    begin
      S := Default
    end
  end
  else
  begin
    Language.LL( Apply, S, Title, Default );
  end;
end;

procedure LoadLanguage( const FilePath: string );
var
  NewLanguage: TLanguageFile;
begin
  try
    NewLanguage := TLanguageFile.Create( FilePath );
  except
    exit;
  end;

  ApplyLanguage( NewLanguage );

end;

function LoadAutoLanguage( const AppName: string;
                           const LanguageSpec: string ): boolean;
var
  FilePath: string;
  Filename: string;
  OSDir: string;
begin
  // Filenames loaded will be <AppName>.<Language>.lng
  Filename :=   AppName
              + '_'
              + LanguageSpec
              + '.lng';

  // eCS 1.1+ look in x:\ecs\lang
  OSDir := GetEnv( 'OSDIR' );
  if OSDir <> '' then
  begin
    FilePath := AddDirectorySeparator( OSDir )
                + 'lang\'
                + Filename;
    if FileExists( FilePath ) then
    begin
      LoadLanguage( FilePath );
      result := true;
      exit;
    end;
  end;

  // something or rather, maybe: look in %ULSPATH%
  if SearchPath( 'ULSPATH',
                 Filename,
                 Filepath ) then
  begin
    LoadLanguage( FilePath );
    result := true;
    exit;
  end;

  // Standalone/compatibility: look in own dir
  FilePath := GetApplicationDir
              + Filename;

  if FileExists( FilePath ) then
  begin
    LoadLanguage( FilePath );
    result := true;
    exit;
  end;

  Result := false;
end;

Procedure LoadDefaultLanguage( const AppName: string );
var
  LanguageVar: string;
  MajorLanguage: string;
  MinorLanguage: string;
begin
//  ProfileEvent( 'LoadDefaultLanguage' );

  LanguageVar := GetEnv( 'LANG' );

//  ProfileEvent( '  LANG=' + LanguageVar );
  if LanguageVar = '' then
    LanguageVar := 'EN_US';

  MajorLanguage := ExtractNextValue( LanguageVar, '_' );
  MinorLanguage := ExtractNextValue( LanguageVar, '_' );

//  ProfileEvent( '  MajorLanguage=' + MajorLanguage );
//  ProfileEvent( '  MinorLanguage=' + MinorLanguage );

  // note there might be some other stuff on the end of LANG
  // such as ES_ES_EURO...

  if MinorLanguage <> '' then
  begin
//    ProfileEvent( '  Trying Major_Minor' );
    if LoadAutoLanguage( AppName,
                         MajorLanguage
                         + '_'
                         + MinorLanguage ) then
    begin
      // found a specifc language
//      ProfileEvent( '    Found' );
      exit;
    end;
  end;

//  ProfileEvent( '  Trying Major only' );
  // try generic language?
  if LoadAutoLanguage( AppName, MajorLanguage ) then
  begin
//    ProfileEvent( '    Found' );
  end
  else
  begin
//    ProfileEvent( '    No language found, using default' );

    // load defaults
    LoadLanguage( '' );
  end;

end;

Initialization
  g_LanguageCallbacks := nil;
  g_CurrentLanguageFile := nil;

Finalization
  DestroyListAndObjects( g_LanguageCallbacks );
  if g_LanguageUpdateCallbacks <> nil then
    g_LanguageUpdateCallbacks.Destroy;

End.
