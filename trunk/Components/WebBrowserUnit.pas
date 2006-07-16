Unit WebBrowserUnit;

// NewView - a new OS/2 Help Viewer
// Copyright 2003 Aaron Lawrence (aaronl at consultant dot com)
// This software is released under the Gnu Public License - see readme.txt

Interface

// Code for running the default browser.
// Doesn't implement DDE!

procedure LaunchURL( const URL: string );

Implementation

uses
  PMSHL, OS2Def,
  ACLFileUtility, ACLUtility, ACLDialogs, SysUtils, RunProgramUnit;

function GetDefaultBrowserPath: string;
begin
  Result := GetUserProfileString( 'WPURLDEFAULTSETTINGS',
                                  'DefaultBrowserExe',
                                  '' );
  if Result = '' then
  begin
    // try Web Explorer
    SearchPath( 'PATH', 'explore.exe', Result );
  end;
end;

function GetDefaultBrowserWorkingDir: string;
begin
  Result := GetUserProfileString( 'WPURLDEFAULTSETTINGS',
                                  'DefaultWorkingDir',
                                  '' );
  if Result = '' then
  begin
    Result := ExtractFilePath( GetDefaultBrowserPath );
  end;

end;

procedure LaunchURL( const URL: string );
var
  BrowserPath: string;
  BrowserWorkingDir: string;
begin
  BrowserPath := GetDefaultBrowserPath;
  BrowserWorkingDir := GetDefaultBrowserWorkingDir;
  if BrowserPath = '' then
  begin
    DoErrorDlg( 'Error',
                'You don''t have a default browser configured.' );
    exit;
  end;

  if not FileExists( BrowserPath ) then
  begin
    DoErrorDlg( 'Error',
                'Browser program doesn''t exist: '
                + BrowserPath  );
    exit;
  end;

  ChDir( RemoveSlash( BrowserWorkingDir ) );

  LaunchProgram( BrowserPath,
                 URL,
                 BrowserWorkingDir );
end;

Initialization
End.
