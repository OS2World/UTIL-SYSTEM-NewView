Unit ACLFileUtility;

Interface

uses
  SysUtils, Classes,
  ACLUtility;

const
  InvalidFilenameCharacters =
    '* ? \ / : < > |"';
const
  HelpPathEnvironmentVar = 'HELP';
  BookshelfEnvironmentVar = 'BOOKSHELF';

Function IsValidFilename( const Filename: string ): boolean;

// Returns true for e.g.
// C: or C:\
Function IsRootDir( const Path: string ): boolean;

// Returns true if the dir is not . or ..
Function IsNotDots( const Filename: string ): boolean;

// Expands the path given, relative to BaseDir
// Handles leading \ for root dir,
// .. for parent, . (ignored),
// drive spec at start,
// ignores repeated \ e.g. \\
Function ExpandPath( BaseDir: string;
                     Path: string ): string;

Function ParentDir( Dir: string ): string;

Function StripDrive( Path: string ): string;

Function ChangeDrive( Path: string;
                      NewDrive: string ): string;
                      
Procedure MakeFileReadOnly( Filename: string );

Procedure MakeFileReadWrite( Filename: string );

// Deletes files incl readonly
Function MyDeleteFile( Path: string ): boolean;

// Adds a slash to dir if not present
Function AddSlash( Dir: string ): string;

// same, but if empty doesn't add slash
function AddSlashNoRoot( Dir: string ): string;

// Remove slash from end of dir if present
Function RemoveSlash( Dir: string ): string;

// Remove leading slashes from dir if present
Function RemoveLeadingSlashes( Dir: string ): string;

// Returns true if it succeeds in removing the directory
// Always removes readonly files
Function DeleteTree( path: string ): boolean;

Procedure ClearDirectory( Directory: string );

// Get the TMP directory
Function TempDir: string;

// Return a list of files in the given Dir
Procedure GetFilesForDir( Dir: string; List: TStrings );

// Return a list of files in the given Dir. using the given filter
Procedure GetFilteredFilesForDir( Dir: string;
                                  Filter: string;
                                  List: TStrings );

// Returns list of files given dir, using given mask (
// Mask may contain more than one filter e.g. *.c;*.h
Procedure GetMaskedFilesForDir( Dir: string;
                                Mask: string;
                                List: TStrings );

Procedure ListDirectory( const Dir: string;
                         const Filter: string;
                         Files: TStrings;
                         SubDirectories: TStrings );

Procedure ListDirectoryAdditive( const Dir: string;
                                 const Filter: string;
                                 const RelativePath: string;
                                 Files: TStrings;
                                 SubDirectories: TStrings );

// If you don't want subdirectories, leave it nil
Procedure ListDirectoryRecursive( const Dir: string;
                                  const Filter: string;
                                  Files: TStrings;
                                  SubDirectories: TStrings );

Procedure ListDirectoryRecursiveAdditive( const Dir: string;
                                          const Filter: string;
                                          const RelativePath: string;
                                          Files: TStrings;
                                          SubDirectories: TStrings );

Procedure ListDirectoryRecursiveAdditive2( const Dir: string;
                                           const Filter: string;
                                           const RelativePath: string;
                                           Files: TStrings;
                                           SubDirectories: TStrings;
                                           const TerminateCheck: TTerminateCheck;
                                           const UseTerminateCheck: boolean );

// Return a list of files in the given path
Procedure GetFilesForPath( PathEnvVar: string;
                           Mask: string;
                           List: TStrings );

// Breaks up specified Env var path
Procedure GetDirsInPath( PathEnvVar: string;
                         List: TStrings );

// Finds the first matching file
Function GetFirstMatchingFile( Dir: string;
                               Filter: string ): string;

// In the directory startpath, create directory and subdirectories
// specified in DirsString
// e.g. bob\bill\fred will make bob, then bill in bob, then fred in bob
// returns path to lowest dir created
Function MakeDirs( FullPath: string ):string;

// Returns current path incl drive
Function GetCurrentDir: string;

// Returns date/time of last modification of file
// Returns 0.0 if error
Function FileDateTime( filename: string ): TDateTime;

// Returns true if file exists and is read only
Function FileIsReadOnly( filename: string ):Boolean;

{$ifdef os2}
Function ExtractFileDrive( Path: string ): string;

Function DirectoryExists( Dir: string ):boolean;

Procedure AnsiReadLn( Var TheFile: Text;
                      Var Line: AnsiString );

Function GetFileSize( Filename: string ): longint;

{$endif}

Function SearchPath( PathEnvVar: string;
                     Filename: string;
                     Var FilenameFound: string ): boolean;

{$ifdef os2}
Function SearchHelpPaths( const Filename: string;
                          var ResultFilename: string;
                          const IncludeAppDir: boolean ): boolean;

// Find the help file for the current app based on LANG
Function FindDefaultLanguageHelpFile( const AppName: string ): string;
{$endif}

Function RunProgram( FileName: string;
                     Parameters: string ): boolean;

function DriveLetterToNumber( const Drive: char ): longint;

function DriveNumberToLetter( const DriveNumber: longint ): char;

function GetVolumeLabel( Drive: char ): string;

function GetBootDrive: char;

Function GetLogFilesDir: string;

Implementation

uses
{$ifdef os2}
  BseDos, DOS, Os2Def,
{$else}
  Windows, FileCtrl,
{$endif}
  ACLFindFunctions, ACLStringUtility,
  ACLString;

Function IsValidFilename( const Filename: string ): boolean;
var
  i: longint;
begin
  Result := false;

  for i := 1 to Length( Filename ) do
  begin
    if Filename[ i ]
       in [ '*','?','\','/',':','<','>','|','"' ] then
      exit;
  end;

  Result := true;
end;


Function IsRootDir( const Path: string ): boolean;
begin
  Result := false;
  case Length( Path ) of
    2:
      Result := IsAlpha( Path[ 1 ] )
                and ( Path[ 2 ] = ':' );

    3:
      Result := IsAlpha( Path[ 1 ] )
                and ( Path[ 2 ] = ':' )
                and ( Path[ 3 ] = '\' );
  end;
end;

Function IsNotDots( const Filename: string ): boolean;
begin
  Result :=     ( Filename <> '.' )
            and ( Filename <> '..' );
end;

Function ParentDir( Dir: string ): string;
var
  SlashPos: integer;
begin
  Dir := RemoveSlash( Dir );
  for SlashPos := Length( Dir ) downto 1 do
  begin
    if Dir[ SlashPos ] = '\' then
    begin
      result:= StrLeft( Dir, SlashPos );
      exit;
    end;
  end;
  result:= '';
end;

Function ExpandPath( BaseDir: string;
                     Path: string ): string;
var
  Dir: string;
begin
  Result:= AddSlash( BaseDir );
  Path := trim( path );
  if Length( Path ) > 1 then
  begin
    // check for drive spec
    if Path[ 2 ] = ':' then
    begin
      Result := StrLeft( Path, 2 ) + '\';
      Delete( Path, 1, 2 );
      Path := RemoveLeadingSlashes( Path );
    end
  end;

  if Length( Path ) > 0 then
  begin
    // check for root dir spec
    if Path[ 1 ] = '\' then
    begin
      // take just the drive from the basedir
      Result := StrLeft( BaseDir, 2 );
      Path := RemoveLeadingSlashes( Path );
    end;
  end;

  while Length( Path ) > 0 do
  begin
    Dir := ExtractNextValue( Path, '\' );
    if Dir = '..' then
    begin
      Result := ParentDir( Result );
    end
    else if Dir = '.' then
    begin
      ; // nothing to do
    end
    else
    begin
      Result := Result + Dir + '\';
    end;

    // strip any extra leading slashes
    Path := RemoveLeadingSlashes( Path );

  end;
  if Length( Result ) = 2 then
    if Result[ 2 ] = ':' then
      // just a drive spec X:, so add a slash
      Result := Result + '\';
end;

{$ifdef os2}
Function ExtractFileDrive( Path: string ): string;
begin
  Result:= '';
  if Length( Path ) < 2 then
    exit;
  if Path[ 2 ] = ':' then
    Result:= Copy( Path, 1, 2 );
end;
{$endif}

Function ChangeDrive( Path: string;
                      NewDrive: string ): string;
var
  CurrentDrive: string;
begin
  Result:= Path;
  CurrentDrive:= ExtractFileDrive( Path );
  Result:= RemoveSlash( NewDrive )
           + StrRightFrom( Path, Length( CurrentDrive ) + 1 );
end;

Function StripDrive( Path: string ): string;
begin
  Result:= ChangeDrive( Path, '' );
end;

Procedure MakeFileReadOnly( Filename: string );
var
  Attributes: longint;
begin
  Attributes:= FileGetAttr( FileName );
  Attributes:= Attributes or faReadonly;
  FileSetAttr( FileName, Attributes );
end;

Procedure MakeFileReadWrite( Filename: string );
var
  Attributes: longint;
begin
  Attributes:= FileGetAttr( FileName );
  Attributes:= Attributes and not faReadonly;
  FileSetAttr( FileName, Attributes );
end;

// Deletes files incl readonly
Function MyDeleteFile( Path: string ): boolean;
begin
  MakeFileReadWrite( Path );
  {$ifdef os2}
  Result:= DeleteFile( Path );
  {$else}
  Result:= DeleteFile( PChar( Path ) );
  {$endif}
end;

// Adds a slash if need to Dir
function AddSlash( Dir: string ): string;
begin
  if Dir='' then
    Result:= '\'
  else
    if Dir[ length( Dir ) ]<>'\' then
      Result:= Dir + '\'
    else
      Result:= Dir;
end;

function AddSlashNoRoot( Dir: string ): string;
begin
  if Dir='' then
    Result := ''
  else
    if Dir[ length( Dir ) ]<>'\' then
      Result:= Dir + '\'
    else
      Result:= Dir;
end;

// Remove slash from end of dir if present
function RemoveSlash( Dir: string ): string;
begin
  Result:= Dir;
  if Result <> '' then
    if Result[ length( Result ) ]='\' then
      Delete( Result, length( Result ), 1 );
end;

Function RemoveLeadingSlashes( Dir: string ): string;
begin
  Result := Dir;
  while Length( Result ) > 0 do
  begin
    if Result[ 1 ] <> '\' then
      break;
    Delete( Result, 1, 1 );
  end;
end;

Function DeleteTree( path: string ): boolean;
Var
  SearchResults: TSearchData;
  rc:integer;
  Directories: TStringList;
  DirectoryIndex: longint;
  FullPath: string;
Begin
  path:= AddSlash( path );
  Directories:= TStringList.Create;
  rc:= MyFindFirst( path+'*', SearchResults );
  while rc = 0 do
  begin
    if IsNotDots( SearchResults.Name ) then
    begin
      FullPath:= path + SearchResults.Name;
      if SearchResults.Attr And faDirectory > 0 then
        Directories.Add( FullPath )
      else
        MyDeleteFile( FullPath );
    end;
    rc:= MyFindNext( SearchResults );
  end;

  SysUtils.FindClose( SearchResults );

  // Now delete directories
  for DirectoryIndex:= 0 to Directories.Count-1 do
    DeleteTree( Directories[ DirectoryIndex ] );

  Directories.Destroy;

  // Finally remove the directory itself
  RmDir( StrLeftWithout( path, 1 ) );
  Result:= (IOResult=0);
End;

Procedure ClearDirectory( Directory: string );
Var
  SearchResults: TSearchData;
  rc:integer;
  FileName: string;
Begin
  Directory:= AddSlash( Directory );
  rc:= MyFindFirst( Directory + '*', SearchResults );
  while rc=0 do
  begin
    FileName:= Directory + SearchResults.Name;
    if SearchResults.Attr and faDirectory = 0 then
      MyDeleteFile( FileName );
    rc:= MyFindNext( SearchResults );
  end;
  SysUtils.FindClose( SearchResults );
End;

{$ifdef win32}
Function GetEnv( VariableName: string ): string;
var
  RequiredSize: integer;
begin
  RequiredSize := GetEnvironmentVariable( PChar( VariableName ), nil, 0 );
  if RequiredSize = 0 then
  begin
    // not defined ?
    Result := '';
    exit;
  end;
  SetLength( Result, RequiredSize + 1 );
  GetEnvironmentVariable( PChar( VariableName ),
                          PChar( Result ),
                          RequiredSize );
  SetLength( Result, StrLen( PChar( Result ) ) );                          
end;
{$endif}

Function TempDir: string;
Begin
//   GetTempPath( sizeof( Buffer ), Buffer ); // doesn't work on W2k
  Result:= GetEnv( 'TMP' );
  if result <> '' then
    if not DirectoryExists( Result ) then
      result := '';

  if Result = '' then
    Result := GetEnv( 'TEMP' );

  if result <> '' then
    if not DirectoryExists( Result ) then
      result := '';
  Result:= AddSlash( Result );
end;

// Return a list of files in the given Dir. using the given filter
Procedure GetFilteredFilesForDir( Dir: string;
                                  Filter: string;
                                  List: TStrings );
Var
  SearchResults: TSearchData;
  rc:integer;
Begin
  Dir:= AddSlash( Dir );
  rc:= MyFindFirst( Dir+Filter, SearchResults );
  while rc=0 do
  begin
    if SearchResults.Attr and faDirectory = 0 then
      List.Add( dir + SearchResults.Name );
    rc:= MyFindNext( SearchResults );
  end;
  MyFindClose( SearchResults );
End;

Procedure GetFilesForDir( Dir: string; List: TStrings );
Begin
  GetFilteredFilesForDir( Dir, '*', List );
End;

Procedure GetMaskedFilesForDir( Dir: string;
                                Mask: string;
                                List: TStrings );
Var
  Filter: string;
Begin
  while Mask <> '' do
  begin
    Filter:= ExtractNextValue( Mask, ';' );
    GetFilteredFilesForDir( Dir, Filter, List );
  end;
End;

Procedure ListDirectoryAdditive( const Dir: string;
                                 const Filter: string;
                                 const RelativePath: string;
                                 Files: TStrings;
                                 SubDirectories: TStrings );
Var
  SearchResults: TSearchData;
  rc: integer;
  Mask: string;
  RemainingFIlter: string;
Begin
  if Assigned( Files ) then
  begin
    RemainingFilter := Filter;;

    while RemainingFIlter <> '' do
    begin
      Mask:= ExtractNextValue( RemainingFilter, ';' );
      rc:= MyFindFirst( AddSlash( Dir ) + Mask, SearchResults );
      while rc = 0 do
      begin
        if SearchResults.Attr And faDirectory = 0 then
        begin
          Files.Add( AddSlashNoRoot( RelativePath )
                     + SearchResults.Name );
        end;
        rc:= MyFindNext( SearchResults );
      end;

      MyFindClose( SearchResults );
    end;
  end;

  if Assigned( SubDirectories ) then
  begin
    rc:= MyFindFirst( AddSlash( Dir ) + '*', SearchResults );
    while rc = 0 do
    begin
      if SearchResults.Attr And faDirectory > 0 then
      begin
        if IsNotDots( SearchResults.Name ) then
        begin
          SubDirectories.Add( SearchResults.Name )
        end
      end;
      rc:= MyFindNext( SearchResults );
    end;
    MyFindClose( SearchResults );
  end;

End;

Procedure ListDirectory( const Dir: string;
                         const Filter: string;
                         Files: TStrings;
                         SubDirectories: TStrings );
begin
  if Assigned( Files ) then
    Files.Clear;
  if Assigned( SubDirectories ) then
    SubDirectories.Clear;
  ListDirectoryAdditive( Dir,
                         Filter,
                         '', // no relative path
                         Files,
                         SubDirectories );
end;

Procedure ListDirectoryRecursiveAdditive2( const Dir: string;
                                           const Filter: string;
                                           const RelativePath: string;
                                           Files: TStrings;
                                           SubDirectories: TStrings;
                                           const TerminateCheck: TTerminateCheck;
                                           const UseTerminateCheck: boolean );
var
  i: integer;
  Directories: TStringList;
  Directory: string;
begin
  Directories := TStringList.Create;
  ListDirectoryAdditive( Dir,
                         Filter,
                         RelativePath,
                         Files,
                         Directories );
  for i := 0 to Directories.Count - 1 do
  begin
    // if Assigned( TerminateCheck ) then - doesn't work in sibyl
    if UseTerminateCheck then
      if TerminateCheck then
        break;
    Directory := Directories[ i ];

    if Assigned( SubDirectories ) then
      SubDirectories.Add( AddSlashNoRoot( RelativePath )
                          + Directory );

    ListDirectoryRecursiveAdditive2( AddSlash( Dir  )
                                     + Directory,
                                     Filter,
                                     AddSlashNoRoot( RelativePath )
                                     + Directory,
                                     Files,
                                     SubDirectories,
                                     TerminateCheck,
                                     UseTerminateCheck );
  end;
  Directories.Destroy;
end;

Procedure ListDirectoryRecursiveAdditive( const Dir: string;
                                          const Filter: string;
                                          const RelativePath: string;
                                          Files: TStrings;
                                          SubDirectories: TStrings );
begin
  ListDirectoryRecursiveAdditive2( Dir,
                                   Filter,
                                   RelativePath,
                                   Files,
                                   SubDirectories,
                                   nil,
                                   false );
end;

Procedure ListDirectoryRecursive( const Dir: string;
                                  const Filter: string;
                                  Files: TStrings;
                                  SubDirectories: TStrings );
begin
  if Assigned( Files ) then
    Files.Clear;
  if Assigned( SubDirectories ) then
    SubDirectories.Clear;
  ListDirectoryRecursiveAdditive( Dir,
                                  Filter,
                                  '',
                                  Files,
                                  SubDirectories );
end;

Procedure GetFilesForPath( PathEnvVar: string;
                           Mask: string;
                           List: TStrings );
var
  rc: longint;
  Path: TAString;
  Dir: TAstring;
  NextDir: longint;
{$ifdef os2}
  pszPath: PChar;
  szEnvVar: cstring;
{$else}
  pszPath: array[ 0..2000 ] of char;
{$endif}
begin
{$ifdef os2}
  szEnvVar:= PathEnvVar;
  rc := DosScanEnv( szEnvVar, pszPath );
{$else}
  rc := GetEnvironmentVariable( Pchar( PathEnvVar ),
                                pszPath,
                                sizeof( pszPath ) );
{$endif}
  if rc <> 0 then
    exit;
  Path:= TAString.CreateFromPChar( pszPath );
  Dir:= TAstring.Create;

  NextDir:= 0;

  while NextDir < Path.Length do
  begin
    Path.ExtractNextValue( NextDir, Dir, ';' );
    GetMaskedFilesForDir( ExpandFileName( Dir.AsString ),
                          Mask,
                          List );
  end;

  Dir.Destroy;
  Path.Destroy;

end;

Procedure GetDirsInPath( PathEnvVar: string;
                         List: TStrings );
var
  rc: longint;
  Path: TAString;
  Dir: TAstring;
  NextDir: longint;
{$ifdef os2}
  pszPath: PChar;
  szEnvVar: cstring;
{$else}
  pszPath: array[ 0..2000 ] of char;
{$endif}
begin
{$ifdef os2}
  szEnvVar:= PathEnvVar;
  rc := DosScanEnv( szEnvVar, pszPath );
{$else}
  rc := GetEnvironmentVariable( Pchar( PathEnvVar ),
                                pszPath,
                                sizeof( pszPath ) );
{$endif}
  if rc <> 0 then
    exit;
  Path:= TAString.CreateFromPChar( pszPath );
  Dir:= TAstring.Create;
  List.Clear;

  NextDir:= 0;

  while NextDir < Path.Length do
  begin
    Path.ExtractNextValue( NextDir, Dir, ';' );
    List.Add( Dir.AsString );
  end;

  Dir.Destroy;
  Path.Destroy;

end;

Function GetFirstMatchingFile( Dir: string;
                               Filter: string ): string;
Var
  SearchResults: TSearchData;
  rc:integer;
Begin
  result := '';
  Dir:= AddSlash( Dir );
  rc:= MyFindFirst( Dir+Filter, SearchResults );
  while rc=0 do
  begin
    if SearchResults.Attr and faDirectory = 0 then
    begin
      result := dir + SearchResults.Name;
      break;
    end;
    rc:= MyFindNext( SearchResults );
  end;
  MyFindClose( SearchResults );
End;

Function MakeDirs( FullPath: string ): string;
Var
  RemainingDirs: string;
  NewDir: string;
  CreatePath:string;
Begin
  CreatePath:= '';

  // Iterate thru specified dirs
  RemainingDirs:= FullPath;
  while trim( RemainingDirs )<>'' do
  begin
    NewDir:= ExtractNextValue( RemainingDirs, '\' );
    if NewDir<>'' then
    begin
      CreatePath:= CreatePath + NewDir;
      if not DirectoryExists( CreatePath ) then
      begin
        MkDir( CreatePath );
      end;
      CreatePath:= CreatePath + '\';
    end;
  end;
  // Remove the end \
  Result:= RemoveSlash( CreatePath );
end;

// Returns current path incl drive
{$ifdef os2}
Function GetCurrentDir: string;
Var
  CurrentDir: cstring[ 200 ];
  CurrentDirLen: longword;
  CurrentDisk: longword;
  DiskMap: longword;
Begin
  CurrentDirLen:= sizeof( CurrentDir );
  DosQueryCurrentDisk( CurrentDisk, DiskMap );
  DosQueryCurrentDir( CurrentDisk,
                      CurrentDir,
                      CurrentDirLen );

  // Form drive part
  Result:= Chr( Ord( 'A' ) + CurrentDisk - 1 ) + ':\';
  // Add directory
  Result:= AddSlash( Result + CurrentDir );
End;
{$else}
Function GetCurrentDir: string;
begin
  GetDir( 0, Result );
end;
{$endif}

Function FileDateTime( filename: string ):TDateTime;
Var
  FileDate: longint;
Begin
  FileDate:=FileAge( filename );
  if FileDate=-1 then
  begin
    Result:=0.0;
    exit;
  end;
  Result:=FileDateToDateTime( FileDate );
end;

Function FileIsReadOnly( filename: string ):Boolean;
Begin
  Result:=( FileGetAttr( filename ) and faReadonly ) >0;
End;

Procedure AnsiReadLn( Var TheFile: Text;
                      Var Line: AnsiString );
Var
  C: Char;
  FoundCR: boolean;
Begin
  Line:= '';
  FoundCR:= false;
  while not eof( TheFile ) do
  begin
    Read( TheFile, C );
    if ( C=#10 ) then
    begin
      if FoundCR then
        exit; // reached end of line
    end
    else
    begin
      if FoundCR then
        // last CR was not part of CR/LF so add to string
        line:= line+#13;
    end;
    FoundCR:= (C=#13);
    if not FoundCR then // don't handle 13's till later
    begin
      line:= line+C;
    end;
  end;

  if FoundCR then
  // CR was last char of file, but no LF so add to string
    line:= line+#13;
End;

{$ifdef os2}
Function DirectoryExists( Dir: string ):boolean;
Var
  SearchRec: TSearchData;
  rc: longint;
  DriveMap: LongWord;
  ActualDrive: LongWord;
  Drive: Char;
  DriveNum: longword;
  DriveBit: longword;
Begin
  Result:= false;
  Dir:= RemoveSlash( Dir );
  if Dir = '' then
  begin
    Result:= true;
    exit;
  end;
  if length( Dir ) = 2 then
    if Dir[ 2 ] = ':' then
    begin
      // a drive only has been specified
      Drive:= UpCase( Dir[ 1 ] );
      if ( Drive < 'A' ) or ( Drive > 'Z' ) then
        exit;
      DosQueryCurrentDisk( ActualDrive, DriveMap );
      DriveNum:= Ord( Drive ) - Ord( 'A' ) + 1; // A -> 1, B -> 2...
      DriveBit:= 1 shl (DriveNum-1); // 2^DriveNum
      if ( DriveMap and ( DriveBit ) > 0 ) then
        Result:= true;
      exit;
    end;

  rc:= MyFindFirst( Dir, SearchRec );
  if rc = 0 then
    if ( SearchRec.Attr and faDirectory )>0 then
      Result:= true;
  MyFindClose( SearchRec );
End;

Function GetFileSize( Filename: string ): longint;
var
  szFilename: Cstring;
  FileInfo: FILESTATUS3;     /* File info buffer */
  rc: APIRET;                   /* Return code */
begin
  szFilename:= FileName;
  rc := DosQueryPathInfo( szFilename,
                          1,
                          FileInfo,
                          sizeof( FileInfo ) );
  if rc = 0 then
    Result:= FileInfo.cbFile
  else
    Result:= -1;
end;
{$endif}

{$ifdef os2}
Function SearchPath( PathEnvVar: string;
                     Filename: string;
                     Var FilenameFound: string ): boolean;
var
  szEnvVar: cstring;
  szFilename: cstring;
  szFilenameFound: cstring;
  rc: APIRET;
begin
  Result:= false;
  FilenameFound:= '';

  szEnvVar:= PathEnvVar;
  szFilename:= Filename;
  rc:= DosSearchPath( SEARCH_IGNORENETERRS
                      + SEARCH_ENVIRONMENT
                      + SEARCH_CUR_DIRECTORY,
                      szEnvVar,
                      szFilename,
                      szFilenameFound,
                      sizeof( szFilenameFound ) );

  if rc = 0 then
  begin
    Result:= true;
    FilenameFound:= szFilenameFound;
  end
end;
{$else}

Function SearchPath( PathEnvVar: string;
                     Filename: string;
                     Var FilenameFound: string ): boolean;
var
  Buffer: array[ 0.._MAX_PATH ] of char;
  rc: DWORD;
  FilePart: PChar;
begin
  Result:= false;
  FilenameFound:= '';

  rc:= Windows.SearchPath( PChar( PathEnvVar ),
                           PChar( Filename ),
                           nil,
                           sizeof( Buffer ),
                           Buffer,
                           FilePart );

  if rc = 0 then
  begin
    Result:= true;
    FilenameFound:= Buffer;
  end
end;
{$endif}

{$ifdef os2}
Function RunProgram( FileName: string;
                     Parameters: string ): boolean;
var
  Dir: string;
  Found: boolean;
  Dummy: string;
  Extension: string;
begin
  Dir:= ExtractFilePath( FileName );
  if Dir = '' then
    Found:= SearchPath( 'PATH',
                        Filename,
                        Dummy )
  else
    // file path specified...
    Found:= FileExists( FileName );

  if not Found then
  begin
    Result:= false;
    exit;
  end;

  Result:= true;

  Extension:= ExtractFileExt( FileName );
  if StringsSame( Extension, '.exe' ) then
    Exec( FileName,
          Parameters )
  else
    Exec( 'cmd.exe',
          '/c '
          + FileName
          + ' '
          + Parameters );

end;
{$else}
Function RunProgram( FileName: string;
                     Parameters: string ): boolean;
Var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  NameAndArgs: string;
Begin
  NameAndArgs:= FileName+' '+Parameters;

  // Initialize some variables to create a process
  ZeroMemory( @StartupInfo, SizeOf( StartupInfo ) );

  StartupInfo.cb := SizeOf( StartupInfo );
  StartupInfo.dwFlags := STARTF_USESTDHANDLES;

  // Create the process
  Result:= CreateProcess( Nil, // use next param for exe name
                          PChar( NameAndArgs ), // command line
                          Nil, // no security attributes
                          Nil, // no thread security attributes
                          True, // do inherit handles
                          CREATE_NEW_PROCESS_GROUP, // so we can send
                          // it Ctrl signals
                          Nil, // no new environment
                          Nil, // use current directory
                          StartupInfo,
                          ProcessInfo );
  if not Result then
    exit;

end;
{$endif}

function DriveLetterToNumber( const Drive: char ): longint;
begin
  if     ( Drive >= 'a' )
     and ( Drive <= 'z' ) then
    Result := Ord( Drive ) - Ord( 'a' ) + 1

  else if     ( Drive >= 'A' )
          and ( Drive <= 'Z' ) then
    Result := Ord( Drive ) - Ord( 'A' ) + 1

  else
    // not a valid drive letter
    Result := 0;

end;

function DriveNumberToLetter( const DriveNumber: longint ): char;
begin
  Result := Chr( DriveNumber - 1 + Ord( 'A' ) );
end;

function GetVolumeLabel( Drive: char ): string;
{$ifdef os2}
var
  rc: APIRET;
  FileSystemInfo: FSINFO;
  e: EInOutError;
begin
  DosErrorAPI( FERR_DISABLEHARDERR );
  result := '';
  rc := DosQueryFSInfo( DriveLetterToNumber( Drive ),
                        FSIL_VOLSER,
                        FileSystemInfo,
                        sizeof( FileSystemInfo ) );
  if rc = 0 then
  begin
    Result := StrNPas( Addr( FileSystemInfo.vol.szVolLabel ),
                       FileSystemInfo.vol.cch );
    Result := LowerCase( Result );
  end;
  DosErrorAPI( FERR_ENABLEHARDERR );

  if rc <> 0 then
  begin
    e := EInOutError.Create( 'Cannot read drive '
                             + Drive
                             + ':' );
    e.ErrorCode := rc;
    raise e;
  end;
end;
{$endif}
{$ifdef win32}
var
  VolumeName: array[ 0..MAX_PATH ] of char;
  FileSystemName: array[ 0..MAX_PATH ] of char;
  SerialNumber: DWORD;
  MaximumFilenameComponentLength: DWORD;
  FileSystemFlags: DWORD;
  OldErrorMode: DWORD;
  e: EInOutError;
begin
  OldErrorMode := SetErrorMode( SEM_FAILCRITICALERRORS );
  if GetVolumeInformation(
    PChar( Drive ),        // root directory of the file system
    VolumeName,            // name of the volume
    sizeof( VolumeName ),  // length of VolumeNameBuffer
    @ SerialNumber,          // volume serial number
    MaximumFilenameComponentLength, // system's maximum filename length
    FileSystemFlags,       // address of file system flags
    FileSystemName,        // address of name of file system
    sizeof( FileSystemName ) // length of lpFileSystemNameBuffer
   ) then
  begin
    Result := VolumeName;
  end
  else
  begin
    e := EInOutError.Create( 'Cannot read drive '
                             + Drive
                             + ':' );
    e.ErrorCode := GetLastError;
    raise e;
  end;
  SetErrorMode( OldErrorMode );
end;
{$endif}

function GetBootDrive: char;
{$ifdef os2}
var
  buffer: longword;
begin
  DosQuerySysInfo( QSV_BOOT_DRIVE, 
                   QSV_BOOT_DRIVE,
                   buffer,
                   sizeof( buffer ) );
  Result := chr( ord( 'A' ) + buffer - 1 );
end;
{$endif}
{$ifdef win32}
var
  WindowsDir: array[ 0..MAX_PATH ] of char;
begin
  GetWindowsDirectory( WindowsDir,
                       sizeof( WindowsDir ) );
  if Strlen( WindowsDir ) > 0 then
    Result := WindowsDir[ 0 ] // not it's an array not a string!
  else
    Result := 'C'; // what the ...!
end;
{$endif}

Function GetLogFilesDir: string;
begin
{$ifdef os2}
  // ecomstation 1.1 compat
  Result := GetEnv( 'LOGFILES' );
  if Result <> '' then
  begin
    Result := AddSlash( Result );
    exit;
  end;
{$endif}
  Result := AddSlash( GetApplicationDir )
end;

{$ifdef os2}
Function SearchHelpPaths( const Filename: string;
                          var ResultFilename: string;
                          const IncludeAppDir: boolean ): boolean;
begin
  Result := SearchPath( HelpPathEnvironmentVar,
                        FileName,
                        ResultFilename );
  if not Result then
    Result := SearchPath( BookshelfEnvironmentVar,
                          FileName,
                          ResultFilename );
  if ( not Result ) and IncludeAppDir then
  begin
    ResultFilename := AddSlash( GetApplicationDir )
                      + Filename;
    Result := FileExists( ResultFilename );
    if not Result then
      ResultFilename := '';
  end;

end;

Function FindDefaultLanguageHelpFile( const AppName: string ): string;
var
  LanguageVar: string;
  MajorLanguage: string;
  MinorLanguage: string;
begin
  LanguageVar := GetEnv( 'LANG' );

  result := '';

  if LanguageVar = '' then
    LanguageVar := 'EN_US';

  MajorLanguage := ExtractNextValue( LanguageVar, '_' );
  MinorLanguage := ExtractNextValue( LanguageVar, '_' );

  // note there might be some other stuff on the end of LANG
  // such as ES_ES_EURO...

  if MinorLanguage <> '' then
  begin
    if SearchHelpPaths(   AppName
                        + '_'
                        + MajorLanguage
                        + '_'
                        + MinorLanguage
                        + '.hlp',
                        Result,
                        true ) then
    begin
      // found a specifc language
      exit;
    end;
  end;

  // try generic language?
  if SearchHelpPaths(   AppName
                      + '_'
                      + MajorLanguage
                      + '.hlp',
                      Result,
                      true ) then
  begin
    exit;
  end;

  SearchHelpPaths( AppName + '.hlp', Result, true );

end;
{$endif}

Initialization
End.

