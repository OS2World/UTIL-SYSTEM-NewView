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
  SharedMemoryUnit;

const
  OWN_HELP_MARKER = '[NVHELP]';

type
  TWindowPosition = record
    Left: longint;
    Bottom: longint;
    Width: longint;
    Height: longint;
  end;

  TCommandLineParameters = record
    ShowUsageFlag: boolean; // *
    TopicParam: string; // *
    FilenamesParam: TAString;
    SearchText: string;
    SearchFlag: boolean;
    GlobalSearchText: string; // *
    GlobalSearchFlag: boolean; // *
    OwnerWindow: HWND;
    HelpManagerWindow: HWND;
    IsHelpManager: boolean;
    WindowTitle: string;
    Position: TWindowPosition;
    SetPosition: boolean;
    Language: string;
  end;
  // * posted to re-used windows

Procedure ParseCommandLineParameters;

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
  Parameters: TCommandLineParameters;
  SharedMemory: TSubAllocatedSharedMemory;
  GlobalFilelist: TGlobalFilelist;

Implementation

uses
  //Forms,
  Dos, SysUtils, PMWin,
  ACLUtility, ACLStringUtility, ACLFileUtility, AStringUtilityUnit, ACLProfile,
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

// Parse command line parameters newview was launched with.
// Store them into the Parameters. variables for later processing.
Procedure ParseCommandLineParameters;
var
  ParamIndex: longint;
  Param: string;
  ParamValue: string;
begin
  ProfileEvent( 'ParseCommandLineParameters started' );

  Parameters.FilenamesParam := TAString.Create;
  Parameters.TopicParam := '';
  Parameters.ShowUsageFlag := false;
  Parameters.GlobalSearchFlag := false;
  Parameters.SearchFlag := false;
  Parameters.OwnerWindow := 0;
  Parameters.IsHelpManager := false;
  Parameters.HelpManagerWindow := 0;
  Parameters.WindowTitle := '';
  Parameters.SetPosition := false;
  Parameters.Language := '';

  for ParamIndex := 1 to ParamCount do
  begin
    Param := ParamStr( ParamIndex );
    if    MatchFlagParam( Param, '?' )
       or MatchFlagParam( Param, 'H' )
       or MatchFlagParam( Param, 'HELP' ) then
    begin
      Parameters.ShowUsageFlag := true
    end
    else if MatchValueParam( Param, 'LANG', Parameters.Language ) then
    begin
    end
    else if MatchValueParam( Param, 'G', Parameters.GlobalSearchText ) then
    begin
      Parameters.GlobalSearchFlag := true;
    end
    else if MatchValueParam( Param, 'S', Parameters.SearchText ) then
    begin
      Parameters.SearchFlag := true;
    end
    else if MatchValueParam( Param, 'HM', ParamValue ) then
    begin
      try
        Parameters.HelpManagerWindow := StrToInt( ParamValue );
        Parameters.IsHelpManager := true;
      except
        // ignore invalid window value
      end;
    end
    else if MatchValueParam( Param, 'OWNER', ParamValue ) then
    begin
      Parameters.OwnerWindow := StrToInt( ParamValue );
    end
    else if MatchValueParam( Param, 'TITLE', ParamValue ) then
    begin
      Parameters.WindowTitle := ParamValue;
    end
    else if MatchFlagParam( Param, 'PROFILE' ) then
    begin
      StartProfile( GetLogFilesDir + 'newview.prf' );
    end
    else if MatchValueParam( Param, 'POS', ParamValue ) then
    begin
      // set window position/size
      if ExtractPositionSpec( ParamValue,
                              Parameters.Position ) then
      begin
        Parameters.SetPosition := true;
      end
      else
      begin
        // invalid...
        Parameters.ShowUsageFlag := true;
      end;
    end
    else
    begin
      if Parameters.FilenamesParam.Length = 0 then
      begin
        // filename(s)
        AString_ParamStr( ParamIndex, Parameters.FilenamesParam );
      end
      else
      begin
        // search (topic) parameter... append all remaining thingies
        if Parameters.TopicParam <> '' then
        begin
          Parameters.TopicParam := Parameters.TopicParam + ' ';
        end;
        Parameters.TopicParam := Parameters.TopicParam + Param;
      end;
    end;
  end;

  ProfileEvent( 'Parameters parsed' );
  ProfileEvent( '  Filenames: '
                + Parameters.FilenamesParam.AsString );
  ProfileEvent( '  Topic: '
                + Parameters.TopicParam );

  // params will be acted on later...
  ProfileEvent( '...done' );

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

  if Parameters.FilenamesParam.Length = 0 then
    // not loading files; nothing to check
    exit;

  FileItems := TStringList.Create;
  Filenames := TStringList.Create;

  AStringToList( Parameters.FilenamesParam, FileItems, '+' );
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
  ExistingWindow: HWND;
begin
  // open shared memory
  SharedMemory := AccessSharedMemory;

  // get access to the system-global filelist
  GlobalFilelist := TGlobalFilelist.Create;

  // parse parameters into Parameters object
  ParseCommandLineParameters;

  ExistingWindow := FindExistingWindow;

  if ExistingWindow <> NULLHANDLE then
  begin
    // want to exit without running fully
    Result := false;

    // destroy global list - nobody else will
    GlobalFilelist.Destroy;
    Parameters.FilenamesParam.Destroy;

    WinSetFocus( HWND_DESKTOP, ExistingWindow );

    if Parameters.TopicParam <> '' then
    begin
      PostNewViewTextMessage( ExistingWindow,
                              NHM_SEARCH,
                              Parameters.TopicParam );
    end;

    if Parameters.GlobalSearchFlag then
    begin
      PostNewViewTextMessage( ExistingWindow,
                              NHM_GLOBAL_SEARCH,
                              Parameters.GlobalSearchText );
    end;

    if Parameters.ShowUsageFlag then
    begin
      WinPostMsg( ExistingWindow,
               NHM_SHOW_USAGE,
               0,
               0 );
    end;

    if Parameters.IsHelpManager then
    begin
      // tell the new help manager instance to talk to the
      // other viewer
      WinPostMsg( Parameters.HelpManagerWindow,
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
