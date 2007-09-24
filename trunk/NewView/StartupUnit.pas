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
  GlobalFilelistUnit,
  SharedMemoryUnit,
  CmdLineParameterUnit;

const
  OWN_HELP_MARKER = '[NVHELP]';
  HELP_FILE_DELIMITER = '+';


function AccessSharedMemory: TSuballocatedSharedMemory;

// Look for any items that are actually specifiying environment
// variables, and expand them to the contents of the variables
Procedure TranslateIPFEnvironmentVars(const Items: TStrings; ExpandedItems: TStrings );

// Given a filename, which may or may not contain a path or extension,
// finds the actual file. This can involve searching
// the help and bookshelf paths.
Function FindHelpFile( FileName: string ): string;
Procedure PostNewViewTextMessage( Window: HWND;
                                  MessageType: ULONG;
                                  s: string );

var
  SharedMemory: TSubAllocatedSharedMemory;
  GlobalFilelist: TGlobalFilelist;

Implementation

uses
  Dos,
  SysUtils,
  DebugUnit,
  PMWin,
  HelpManagerUnit,
  StringUtilsUnit,
  FileUtilsUnit;

// Look for any items that are actually specifiying environment
// variables, and expand them to the contents of the variables
Procedure TranslateIPFEnvironmentVars(const Items: TStrings; ExpandedItems: TStrings );
var
  i : longint;
  tmpItem: string;
  tmpEnvironmentVarValue: string;
begin
  LogEvent(LogStartup, 'Translating environment vars' );
  for i := 0 to Items.Count - 1 do
  begin
    tmpItem := Items[ i ];

    tmpItem := StrTrimChars(tmpItem, [StrSingleQuote]); // remove single quotes
    tmpItem := StrTrimChars(tmpItem, [StrDoubleQuote]); // remove double quotes

    LogEvent(LogStartup, '  Checking for environment var: ' + tmpItem );
    tmpEnvironmentVarValue := GetEnv( Uppercase( tmpItem ) );
    if DosError = 0 then
    begin
      // environment var exists - use it's value
      LogEvent(LogStartup, '    Environment var found; translated to: ' + tmpEnvironmentVarValue);
      StrExtractStrings(ExpandedItems, tmpEnvironmentVarValue, [HELP_FILE_DELIMITER], #0);
    end
    else
    begin
      // not an environment var
      ExpandedItems.Add( tmpItem );
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


Initialization
End.
