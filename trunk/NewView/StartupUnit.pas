Unit StartupUnit;

// NewView - a new OS/2 Help Viewer
// Copyright 2003 Aaron Lawrence (aaronl at consultant dot com)
// This software is released under the Gnu Public License - see readme.txt

// Code related to startup and finding help files.
// Shared between NewView.exe and ViewStub.exe

Interface

uses
  OS2Def,
  Classes,
  ACLString,
  GlobalFilelistUnit,
  SharedMemoryUnit,
  CmdLineParameterUnit;

const
  OWN_HELP_MARKER = '[NVHELP]';


function AccessSharedMemory: TSuballocatedSharedMemory;

// Returns true if the program should be started as normal.
// False if it should immediately exit.
function Startup: boolean;

function GetOwnHelpFileName: string;

// Look for any items that are actually specifiying environment
// variables, and expand them to the contents of the variables
Procedure TranslateIPFEnvironmentVars( Items: TStrings;
                                       ExpandedItems: TStrings );

// Given a filename, which may or may not contain a path or extension,
// finds the actual file. This can involve searching
// the help and bookshelf paths.
Function FindHelpFile( FileName: string ): string;

var
  CmdLineParameters: TCmdLineParameters;
  SharedMemory: TSubAllocatedSharedMemory;
  GlobalFilelist: TGlobalFilelist;

Implementation

uses
  Dos,
  SysUtils,
  PMWin,
  ACLUtility,
  ACLStringUtility,
  ACLFileUtility,
  AStringUtilityUnit,
  ACLProfile,
  HelpManagerUnit;

// Look for any items that are actually specifiying environment
// variables, and expand them to the contents of the variables
Procedure TranslateIPFEnvironmentVars( Items: TStrings;
                                       ExpandedItems: TStrings );
var
  i: longint;
  Item: string;
  EnvironmentVarValue: string;
begin
  ProfileEvent( 'Translating environment vars' );
  for i := 0 to Items.Count - 1 do
  begin
    Item := Items[ i ];

    Item := StrUnQuote( Item ); // remove single quotes
    Item := StrUnDoubleQuote( Item ); // remove double quotes

    ProfileEvent( '  Item: ' + Item );
    EnvironmentVarValue := GetEnv( Uppercase( Item ) );
    if DosError = 0 then
    begin
      // environment var exists - use it's value
      ProfileEvent( '    Translated: ' + EnvironmentVarValue );
      while EnvironmentVarValue <> '' do
      begin
         Item := ExtractNextValue( EnvironmentVarValue, '+' );
         ExpandedItems.Add( Item );
      end;
    end
    else
    begin
      // not an environment var
      ExpandedItems.Add( Item );
    end;
  end;
end;

// Given a filename, which may or may not contain a path or extension,
// finds the actual file. This can involve searching
// the help and bookshelf paths.
Function FindHelpFile( FileName: string ): string;
var
  AlternativeFileName: string;
begin
  if FileName = OWN_HELP_MARKER then
  begin
    Result := GetOwnHelpFileName;
    exit;
  end;

  Result := '';

  AlternativeFileName := '';
  if ExtractFileExt( Filename ) = '' then
  begin
    Filename := ChangeFileExt( Filename, '.inf' );
    AlternativeFileName := ChangeFileExt( Filename, '.hlp' );
  end;

  if ExtractFilePath( FileName ) <> '' then
  begin
    // Path specified; just see if it exists

    // expand out relative paths
    Filename := ExpandFileName( FileName );
    AlternativeFilename := ExpandFileName( AlternativeFilename );

    if FileExists( Filename ) then
      Result := Filename
    else if FileExists( AlternativeFilename ) then
      Result := AlternativeFilename;

  end
  else
  begin
    // Path not specified; search current
    if FileExists( ExpandFileName( FileName ) ) then
    begin
      Result := ExpandFileName( FileName );
      exit;
    end;

    if FileExists( ExpandFileName( AlternativeFilename ) ) then
    begin
      Result := ExpandFileName( AlternativeFilename );
      exit;
    end;

    // Search help paths

    if not SearchHelpPaths( FileName,
                            Result,
                            false // don't search our app dir
                             ) then
    begin
      // Didn't find as specified or as .inf, try .hlp
      if AlternativeFilename <> '' then
      begin
        if not SearchHelpPaths( AlternativeFileName,
                                Result,
                                false // don't search our app dir
                                ) then
        begin
          Result := '';
        end;
      end;
    end;
  end;
end;

function GetOwnHelpFileName: string;
begin
  Result := FindDefaultLanguageHelpFile( 'NewView' );
end;

// Extract a single element of a window position spec
// - take a value from comma-separated list
// - convert to numeric
// - if the number ends with P then take as
//   a percentage of given dimension
Function ExtractPositionElement( Var ParamValue: string;
                                 ScreenDimension: longint ): longint;
var
  Element: string;
begin
  Element := ExtractNextValue( ParamValue, ',' );
  if Element = '' then
    raise Exception.Create( 'Missing position element' );
  if StrEnds( 'P', Element ) then
  begin
    Delete( Element, Length( Element ), 1 );
    if Element = '' then
      raise Exception.Create( 'Missing position element' );
    Result := StrToInt( Element );
    if Result < 0 then
      Result := 0;
    if Result > 100 then
      Result := 100;
    Result := Round( Result / 100 * ScreenDimension );
  end
  else
  begin
    Result := StrToInt( Element );
  end;
end;

Function SystemMetrics(sm:LONG):LongInt;
Begin
  Result := WinQuerySysValue(HWND_DESKTOP,sm);
end;

// Extract a specified window position:
// X,Y,W,H
Function ExtractPositionSpec( ParamValue: string;
                              Var Position: TWindowPosition ): boolean;
begin
  try
    Position.Left := ExtractPositionElement( ParamValue, SystemMetrics(SV_CXSCREEN) );
    Position.Bottom := ExtractPositionElement( ParamValue, SystemMetrics(SV_CYSCREEN) );
    Position.Width := ExtractPositionElement( ParamValue, SystemMetrics(SV_CXSCREEN) );
    if Position.Width < 50 then
      Position.Width := 50;
    Position.Height := ExtractPositionElement( ParamValue, SystemMetrics(SV_CYSCREEN) );
    if Position.Height < 50 then

      Position.Height := 50;
    Result := true;
  except
    Result := false;
  end;
end;


// If another instance already has the files open
// activate it and return true.
function FindExistingWindow: HWND;
var
  FileItems: TStringList;
  Filenames: TStringList;
  FullFilePath: string;
  i: longint;

  FileWindow: HWND;
begin
  Result := NULLHANDLE;

  if length(CmdLineParameters.getFileNames) = 0 then
    // not loading files; nothing to check
    exit;

  FileItems := TStringList.Create;
  Filenames := TStringList.Create;

  StringToList(cmdLineParameters.getFileNames, FileItems, '+' );
  TranslateIPFEnvironmentVars( FileItems, FileNames );

  for i := 0 to FileNames.Count - 1 do
  begin
    FullFilePath := FindHelpFile( Filenames[ i ] );
    if FullFilePath <> '' then
    begin
      FileWindow := GlobalFilelist.FindFile( FullFilePath );

      if FileWindow = NULLHANDLE then
      begin
        // not found - stop searching.
        Result := NULLHANDLE; // no match
        break;
      end;

      // found it

      // is it the same as any previous match?
      if Result <> NULLHANDLE then
      begin
        if FileWindow <> Result then
        begin
          // no, so we don't have a match.
          // NOTE: We just excluded something: if the same file is
          // open in multiple windows then we may not check all combinations
          Result := NULLHANDLE; // no match
          break;
        end;
      end
      else
      begin
        // no match yet - store this one
        Result := FileWindow;
      end;

    end;
  end;

  Filenames.Destroy;
  FileItems.Destroy;

end;

function AccessSharedMemory: TSuballocatedSharedMemory;
begin
  Result := TSuballocatedSharedMemory.Create( SharedMemName,
                                              SharedMemSize,
                                              SharedMemReserveSize );
end;

procedure PostNewViewTextMessage( Window: HWND;
                                  MessageType: ULONG;
                                  s: string );
var
  ps: pchar;
begin
  SharedMemory.Allocate( ps, length( s ) + 1 );
  ps ^ := s;
  WinPostMsg( Window,
              MessageType,
              LONG( ps ),
              0 );
end;

function Startup: boolean;
var
  tmpCmdLine: String;
  tmpSplittedCmdLine : TStringList;
  tmpRc : Integer;

  ExistingWindow: HWND;
begin
  // open shared memory
  SharedMemory := AccessSharedMemory;

  // get access to the system-global filelist
  GlobalFilelist := TGlobalFilelist.Create;

  // parse parameters into Parameters object
  tmpCmdLine := nativeOS2GetCmdLineParameter;
  tmpSplittedCmdLine := TStringList.Create;
  tmpRc := splitCmdLineParameter(tmpCmdLine, tmpSplittedCmdLine);

  CmdLineParameters := TCmdLineParameters.Create;
  CmdLineParameters.parseCmdLine(tmpSplittedCmdLine);

  tmpSplittedCmdLine.Destroy;

  ExistingWindow := FindExistingWindow;

  if ExistingWindow <> NULLHANDLE then
  begin
    // want to exit without running fully
    Result := false;

    // destroy global list - nobody else will
    GlobalFilelist.Destroy;
    // TODO rbri maybe we need the next line
    // Parameters.FilenamesParam.Destroy;

    WinSetFocus( HWND_DESKTOP, ExistingWindow );

    if CmdLineParameters.getTopics <> '' then
    begin
      PostNewViewTextMessage( ExistingWindow,
                              NHM_SEARCH,
                              CmdLineParameters.getTopics);
    end;

    if CmdLineParameters.getGlobalSearchTextFlag then
    begin
      PostNewViewTextMessage( ExistingWindow,
                              NHM_GLOBAL_SEARCH,
                              CmdLineParameters.getGlobalSearchText );
    end;

    if CmdLineParameters.getShowUsageFlag then
    begin
      WinPostMsg( ExistingWindow,
               NHM_SHOW_USAGE,
               0,
               0 );
    end;

    if CmdLineParameters.getHelpManagerFlag then
    begin
      // tell the new help manager instance to talk to the
      // other viewer
      WinPostMsg( CmdLineParameters.getHelpManagerWindow,
               NHM_VIEWER_READY,
               ExistingWindow,
               0 );
    end;

  end
  else
  begin
    // run as normal
    Result := true;
  end;
end;

Initialization
End.
