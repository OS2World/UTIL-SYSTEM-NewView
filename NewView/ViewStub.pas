Program ViewStub;

// Small stub program that uses the GlobalFilelist to decide
// whether to launch a new instance of newview.exe, or activate
// an existing one.

// Becomes view.exe.

Uses
  OS2def,
  PMShl,
  PmWin,
  SysUtils,
  DebugUnit,
  CmdLineParameterUnit,
  StartupUnit;

{$R ViewStub}

var
  Details: PROGDETAILS;
  Parameters: pchar;

Const
  Vendor = 'Aaron Lawrence';
  BldLevelVersion = '1.1.1';
  Description = 'NewView Stub';

  // BLDLevel - compatible - mostly
  EmbeddedVersion: string =
      '@#'
    + Vendor
    + ':'
    + BldLevelVersion
    + '#@'
    + Description
    + #0;

imports
  // Alternative declaration of WinStartApp with a more useful type for pszParams
  FUNCTION _WinStartApp(hwndNotify:HWND;VAR pDetails:PROGDETAILS;pszParams:pchar;
                     Reserved:POINTER;fbOptions:ULONG):HAPP;
                    APIENTRY;         'PMSHAPI' index 119;
end;

var
    tmpCmdLine : String;
    tmpPCharCmdLine : PChar;

Begin
  LogEvent(LogViewStub, 'Starting' );

  if Startup then
  begin
    // Want a new instance.

    tmpCmdLine := nativeOS2GetCmdLineParameter;
    tmpPCharCmdLine := StrAlloc(length(tmpCmdLine) + 1);
    StrPCopy(tmpPCharCmdLine, tmpCmdLine);

    // set up details for launching newview
    With Details do
    begin
      Length := Sizeof( Details );
      progt.progc := PROG_PM;
      progt.fbVisible := SHE_VISIBLE;
      pszTitle := 'Help Viewer';
      pszExecutable := 'newview.exe';
      pszParameters := nil; // Parameters; // copy our parameters
      pszStartupDir := ''; // current dir
      pszIcon := nil; // default
      pszEnvironment := nil; // default
      swpInitial.fl := SWP_ACTIVATE;
      swpInitial.hwndInsertBehind := HWND_TOP;
    end;

    _WinStartApp( NULLHANDLE, // no notify window
                  Details,
                  tmpPCharCmdLine, // use these rather than Details parameters,
                              // cause PM was swallowing /? the other way!!
                  nil, // reserved
                  0 );

    StrDispose(tmpPCharCmdLine);

  end;
  LogEvent(LogViewStub, 'Finished' );
End.
