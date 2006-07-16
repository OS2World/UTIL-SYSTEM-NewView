Unit DirectoryEdit;
// An edit box that will act as a directory
// changer, with tab completion and enter
// selecting a dir.
Interface

Uses
  Classes, Forms, StdCtrls, SysUtils;

Type
  TNotifyDirChange = procedure( NewDir: string ) of object;

  TDirectoryEdit=Class(TEdit)
  Protected
    FDirectory: string;
    FCompletionBasePath: string;
    FCompletionIndex: longint;
    FCompletionStart: string;
    FOnChangeDir: TNotifyDirChange;
    FCompletions: TStringList;
    Procedure SetupComponent; Override;
    Procedure ScanEvent( Var KeyCode: TKeyCode;
                         RepeatCount: Byte ); override;
    Procedure CharEvent( Var Key: Char;
                         RepeatCount: Byte ); override;
    procedure ReadCompletions;
  Public
    Destructor Destroy; Override;
  Published
    property OnChangeDirectory: TNotifyDirChange read FOnChangeDir write FOnChangeDir;
    procedure SetDirectory( const Directory: string );
  End;

Exports
  TDirectoryEdit,'User','diredit.bmp';

Implementation

Uses
  PMWIN, BseDos, ACLFileUtility, ACLFindFunctions;

Procedure TDirectoryEdit.SetupComponent;
Begin
  Inherited SetupComponent;
  Name:= 'DirectoryEdit';
  FCompletionIndex:= 0;
  FCompletions:= TStringList.Create;
End;

Destructor TDirectoryEdit.Destroy;
Begin
  Inherited Destroy;
End;

procedure TDirectoryEdit.SetDirectory( const Directory: string );
begin
  FDirectory := Directory;
  Text := Directory;

  // move cursor to end
  SelStart := Length( Directory );
end;

procedure TDirectoryEdit.ReadCompletions;
Var
  SearchResults: TSearchData;
  rc:integer;
  Drive: string;
Begin
  FCompletions.Clear;
  Drive := ExtractFileDrive( FCompletionBasePath );
  if Length( Drive ) < 2 then
    // must have a drive spec
    exit;

  rc:= MyFindFirst( FCompletionBasePath
                    + FCompletionStart
                    + '*',
                    SearchResults );
  while rc = 0 do     
  begin
    if ( SearchResults.Attr and faDirectory ) > 0 then
      if     ( SearchResults.Name <> '.' )
         and ( SearchResults.Name <> '..' ) then
        FCompletions.Add( SearchResults.Name );
    rc:= MyFindNext( SearchResults );
  end;
  MyFindClose( SearchResults );
  FCompletions.Sort;
End;

Procedure TDirectoryEdit.ScanEvent( Var KeyCode: TKeyCode;
                                    RepeatCount: Byte );
Var
  Entry: string;
  rc: longint;
  Dir: string;
  NewDirectory: string;
Begin
  if KeyCode = kbTab then
  begin
    KeyCode:= kbNull;
    // want to use tab for completion
    if FCompletions.Count = 0 then
    begin
      // First time, read possible completions
      FCompletionStart := ExtractFileName( Text );
      FCompletionBasePath:= ExpandPath( FDirectory,
                                        ExtractFilePath( Text ) );
      ReadCompletions;
    end;

    if FCompletionIndex < FCompletions.Count then
    begin
      // Display next completion
      Text:= FCompletionBasePath + FCompletions[ FCompletionIndex ];
      // Place cursor at end
      SelStart := Length( Text );
      inc( FCompletionIndex );
    end
    else
    begin
      // nothing more
      Beep( 1000, 50 );
    end;
    exit;
  end;

  // not a tab
  FCompletionIndex:= 0;
  FCompletions.Clear;

  if KeyCode = kb_VK + VK_NEWLINE then
  begin
    KeyCode:= kbNull;
    // enter key pressed - change dir
    NewDirectory := ExpandPath( FDirectory,
                                  Text );
    if DirectoryExists( NewDirectory ) then
    begin
      SetDirectory( NewDirectory );
      if Assigned( FOnChangeDir ) then
         FOnChangeDir( NewDirectory )
    end
    else
    begin
      Beep( 1000, 50 );
    end;
    exit;
  end;

  if KeyCode = kbCtrlTab then
  begin
     // fake a normal focus shift
     KeyCode:= kbTab;
     Parent.ScanEvent( KeyCode, 1 );
     KeyCode:= kbNull;
  end;

End;

Procedure TDirectoryEdit.CharEvent( Var Key: Char;
                                    RepeatCount: Byte );
Begin
  FCompletions.Clear;
  FCompletionIndex:= 0;
End;

Initialization
  {Register classes}
  RegisterClasses([TDirectoryEdit]);
End.

