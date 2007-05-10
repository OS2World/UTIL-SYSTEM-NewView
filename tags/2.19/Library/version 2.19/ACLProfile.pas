Unit ACLProfile;
// Crude profiling functions. Accurate to single milliseconds
// (not just 1/18s). Writes profile to text file called 'profile' in
// current directory.
// Call ProfileEvent to log an event with time.

// Now logs delta times (time used) as well.
Interface

procedure StartProfile( const Filename: string );

procedure ProfileEvent( const Event: string );

procedure StopProfile;

Implementation

uses
{$ifdef os2}
  OS2Def, PMWin,
{$else}
  Windows,
{$endif}
  SysUtils;

var
  ProfileStartTime: ULONG;
  LastProfileTime: ULONG;
  ProfileFile: TextFile;

const
  Profiling: boolean = false;

function GetSystemMSCount: ULONG;
begin
{$ifdef os2}
  Result:= WinGetCurrentTime( AppHandle );
{$else}
  Result:= GetTickCount;
{$endif}
end;

procedure StartProfile( const Filename: string );
begin
  if Profiling then
  begin
    ProfileEvent( 'Attempt to start profiling to: ' + Filename );
    exit;
  end;

  ProfileStartTime:= GetSystemMSCount;
  LastProfileTime:= ProfileStartTime;
  Assign( ProfileFile, Filename );
  Rewrite( ProfileFile );
  WriteLn( ProfileFile,
           '---------------------------------------------------' );
  Write( ProfileFile,
         'Profile Start' );
  Close( ProfileFile );
  Profiling:= true;
end;

procedure ProfileEvent( const Event: string );
var
  ThisProfileTime: ULONG;
begin
  if not Profiling then
    exit;
{$ifdef win32}
  FileMode := fmOpenReadWrite;
{$else}
  FileMode := fmInOut;
{$endif}
  Append( ProfileFile );
  ThisProfileTime := GetSystemMSCount;

  WriteLn( ProfileFile,
           ', Used: '
           + IntToStr( ThisProfileTime - LastProfileTime ) );

  Write( ProfileFile,
         Event + ': '
         + IntToStr( ThisProfileTime - ProfileStartTime ) );

  LastProfileTime:= ThisProfileTime;

  Close( ProfileFile );
end;

procedure StopProfile;
begin
  if not Profiling then
    exit;
  ProfileEvent( 'Profile stop' );
  Profiling:= false;
end;

Initialization
End.
