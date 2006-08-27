Program ViewStub;

// Small stub program that uses the GlobalFilelist to decide
// whether to launch a new instance of newview.exe, or activate
// an existing one.

// Becomes view.exe.

Uses
  OS2def,
  PMShl,
  PmWin,
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

Begin
  if Startup then
  begin
    // Want a new instance.

    // Get address of parameters, ignoring first (exename).
    ASM
      MOV ESI, SYSTEM.ArgStart    // Get start of parameters

      // read over exe name (until first null byte)
!rrloop:
      LODSB
      CMP AL,0
      JNE !rrloop

      MOV Parameters, ESI
    End;

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
                  Parameters, // use these rather than Details parameters,
                              // cause PM was swallowing /? the other way!!
                  nil, // reserved
                  0 );
  end;
End.
