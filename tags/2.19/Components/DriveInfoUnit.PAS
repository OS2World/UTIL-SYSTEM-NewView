unit DriveInfoUnit;

interface

const
  MinDriveNumber = 1;
  MaxDriveNumber = 26;

type
  TDriveType =
  (
    dtNone,
    dtFloppy,
    dtHard,
    dtCD,
    dtNetwork,
    dtRemovable
  );

// Drive numbers are one based

Function IsFloppyDrive( DriveNumber: longint ): Boolean;

Function GetLocalDriveType( DriveNumber: longint ): TDriveType;

Function GetDriveType( DriveNumber: longint ): TDriveType;

Function GetNetworkDriveRemotePath( DriveNumber: longint ): string;

implementation

uses
  BseDos, OS2def, BseErr, BseDev,
  SysUtils,
  ACLUtility, ACLFileUtility, ACLStringUtility;

imports
FUNCTION _DosQueryFSAttach( VAR pszDeviceName: CSTRING;
                            ulOrdinal: ULONG;
                            ulFSAInfoLevel:ULONG;
                            pfsqb: PFSQBUFFER2;
                            VAR pcbBuffLength: ULONG ): APIRET; APIENTRY;
  'DOSCALLS' index 277;
end;

type
  TWord = Record                    // Bytes of a Word
    LoByte, HiByte : Byte;
  End;

  TBPB = Array[0..30] Of Byte;    // Puffer fuer BPB-Struktur

  TDeviceParameters = Record
    BPB: TBPB;
    Cylinders: word;
    DeviceType: Byte;
    Attributes: Word;
  End;

Function IsFloppyDrive( DriveNumber: longint ): Boolean;
Var
  bResult : Byte;
Begin
  DosDevConfig( bResult, DEVINFO_FLOPPY );
  Result := ( Abs( DriveNumber ) <= bResult);
End;

// -------------------------------------------------------------------------
// Funktion/Function: QueryCDRoms()
//
// Beschreibung:
//   Die Funktion QueryCDRom ermittelt ueber eine nicht dokumentierte
//   Schnittstelle die Anzahl der CDRom-Laufwerke und den ersten, fuer
//   ein CDRom-Laufwerk, vergebenen UnitIdentifier.
//   Der Treiber OS2CDROM.DMD stellt dem System zwei Devices (CD-ROM1$
//   und CD-ROM2$) zur Verfuegung. Die beiden Devices unterscheiden sich
//   durch DeviceAttribute. Beide Devices unterstuetzen (zumindest unter
//   Warp) den undokumentierten Generic IOCtl 0x82/0x60, welcher Infor-
//   mationen ueber die angeschlossenen CDRom-Laufwerke liefert.
//
// Description:
//   This Functions finds out how many CD-Rom Drives are present in System
//   and which Drive Letter is the first occupied by a CD-Rom. It uses an
//   undocumented Interface to OS2CDROM.DMD.
//   OS2CDROM.DMD presents two Devices (CD-ROM1$ and CD-ROM2$). These De-
//   vices are distinguished by their Device-Attributes. Both Devices sup-
//   port (under Warp) the undocumented generic IOCtl-Call 0x82/0x60 which
//   deliver some Information about the connected CD-Rom Drives.
//
// Parameter:
//   Var ulCDRomCount        ULONG     Anzahl CD-Rom Laufwerke im System
//                                     Number of CD-Rom Drives in System
//
//   Var ulFirstCDRomDiskNo  ULONG     erste Laufwerksnummer, die an ein
//                                     CD-Rom vergeben ist
//                                     first Drive-Letter occupied by a
//                                     CD-Rom Drive
//
// Rueckgabe/Returnvalue:  keine/none
// -------------------------------------------------------------------------
Procedure QueryCDRoms(Var ulCDRomCount, ulFirstCDRomDiskNo: ULONG);
 
Const cszDriverName : CSTRING = 'CD-ROM?$';
 
Var cCurDriver : Char;                    // Indexvariable fuer aktuell bearbeites Device (1 oder 2)
                                          // Index for current Device (1 or 2)

    hDevHandle : HFILE;                   // Handle fuer Device
                                          // Device handle

    ulAction   : ULONG;                   // Aktionscode (DosOpen())
                                          // Actioncode (DosOpen())
                                          
    ulParams   : ULONG;                   // Anzahl Bytes von IOCtl gelieferter Parameterdaten
                                          // Number of Bytes for delivered Parameterdata
                                          
    ulData     : ULONG;                   // Anzahl Bytes von IOCtl gelieferter Daten
                                          // Number of Bytes delivered by IOCtl
                                          
    rCDInfo    : Record                   // Ergebnisstruktur der IOCtl-Funktion (s.o.)
                                          // Record for Results of IOCtl-Call (see above)
                   usCDRomCount : USHORT; // Anzahl CD-Roms                   / Number of CD-Rom Drives
                   usFirstUnitNo: USHORT; // erste vergebene Laufwerksnummer  / first Driver Letter
                 End;

Begin (* uQueryCDRom *)
                                       /************************************
                                        * Vorbelegungen
                                        *
                                        * initial assignments
                                        ************************************/
  ulCDRomCount := 0;
  ulFirstCDRomDiskNo := 0;

  ulParams := 0;

                                       /************************************
                                        * die beiden Devices abarbeiten
                                        *
                                        * accessing both Devices
                                        ************************************/
  For cCurDriver := '1' To '2' Do
    Begin
                                       /************************************
                                        * Device oeffnen
                                        *
                                        * open Device
                                        ************************************/
      cszDriverName[6] := cCurDriver;
      If (DosOpen(cszDriverName,              // Devicename
                  hDevHandle,                 // Handle
                  ulAction,                   // Aktionscode
                  0,                          // Dateigr”áe
                  FILE_NORMAL,                // Attribute: read/write
                  OPEN_ACTION_OPEN_IF_EXISTS, // OpenFlag: ”ffnen, wenn vorhanden
                  OPEN_FLAGS_FAIL_ON_ERROR Or // Modus: Fehlermeldung per Returncode
                    OPEN_SHARE_DENYNONE Or    //        keine Einschr„nkungen fr Dritte
                    OPEN_ACCESS_READONLY,     //        nur lesender Zugriff
                  NIL)=NO_ERROR) Then         // keine EA
        Begin
                                       /************************************
                                        * IOCtl-Funktion aufrufen
                                        *
                                        * Call to IOCtl
                                        ************************************/
          If (DosDevIOCtl(hDevHandle,             // Handle                 / Handle
                          $82,                    // Kategorie              / Category
                          $60,                    // Funktion               / Function
                          NIL,                    // keine Parameterliste   / No Parameterlist
                          0,                      // Laenge Parameterliste  / Length of Parameterlist
                          ulParams,               // Groesse der gelieferten Parameterdaten
                                                  //                        / Number of Bytes for Parameterdata
                          rCDInfo,                // Puffer fuer gelieferte Daten
                                                  //                        / Buffer for returned Data
                          SizeOf(rCDInfo),        // Groesse des Datenpuffers
                                                  //                        / Size of Databuffer
                          ulData)=NO_ERROR) Then  // Groesse der gelieferten Daten
                                                  //                        / Number of Bytes for returned Data
            Begin
              ulCDRomCount := rCDInfo.usCDRomCount;
              ulFirstCDRomDiskNo := Succ(rCDInfo.usFirstUnitNo);
            End;

          DosClose(hDevHandle);
        End;

   End; (* For *)

End; (* uQueryCDRom *)


Function GetLocalDriveType( DriveNumber: longint ): TDriveType;
var
  IOCtlParameters: Word;
  rc: APIRET;
  ParameterLength: longWord;
  DataLength: longword;
  DeviceData: TDeviceParameters;
  Fixed: boolean;
  FirstCDDrive: ULONG;
  NumCDDrives: ULONG;
begin

  TWord( IOCtlParameters ).LoByte := 0;  // BPB of physical Device
  TWord( IOCtlParameters ).HiByte := DriveNumber - 1; // drive number, zero base

  ParameterLength := SizeOf( IOCtlParameters ); // input length of parameters
  DataLength := 0;                              // input length of data (none)

  rc := DosDevIOCTL( HFILE(-1),                 // Open Device (not a file)
                     IOCTL_DISK,                // Category
                     DSK_GETDEVICEPARAMS,       // Function
                     IOCtlParameters,           // Parameters
                     SizeOf( IOCtlParameters ), // (max) size of parameters
                     ParameterLength,           // parameters length
                     DeviceData,                // results
                     SizeOf( DeviceData ),      // (max) size of data block
                     DataLength );              // data block length

  Fixed := ( DeviceData.Attributes and 1 ) > 0;    // bit 0 indicates fixed (1) or removable (0)
  if not Fixed then
  begin
    result := dtRemovable;

    QueryCDRoms( FirstCDDrive,
                 NumCDDrives );

    if     ( DriveNumber >= FirstCDDrive )
       and ( DriveNumber < FirstCDDrive + NumCDDrives ) then
      result := dtCD;

    exit;
  end;

  result := dtHard;
end;

// Takes a one-based drive number
Function GetDriveType( DriveNumber: longint ): TDriveType;
var
  szDrive: CString;

  FSData: array[ 0..sizeof( FSQBuffer) + 3*_MAX_PATH ] of char;
  pBuffer: PFSQBUFFER2;
  FSDataLength: ULONG;

  rc: APIRET;
begin
  assert( DriveNumber >= 1 );
  assert( DriveNumber <= 26 );

  if ( DriveNumber >=1 ) and ( DriveNumber <= 2 ) then
  begin
    if IsFloppyDrive( DriveNumber ) then
    begin
      result := dtFloppy;
      exit;
    end;

    result := dtNone; // don't let OS/2 try a fake B: drive
    exit;
  end;

  DosErrorAPI( FERR_DISABLEHARDERR );

  szDrive := DriveNumberToLetter( DriveNumber ) + ':';
  FSDataLength := sizeof( FSData );
  pBuffer := Addr( FSData );
  rc := _DosQueryFSAttach( szDrive,
                           0, // ignored
                           FSAIL_QUERYNAME,
                           pBuffer,
                           FSDataLength );

  if rc = 0 then
  begin
    case pBuffer^.iType of
      FSAT_REMOTEDRV:
        result := dtNetwork;

      FSAT_LOCALDRV:
        // Figure out what kind of local drive it is...
        result := GetLocalDriveType( DriveNumber );

      else
      begin
        // should never happen
        result := dtNone;
        exit;
      end;
    end;
  end
  else if rc = ERROR_NOT_READY then
  begin
    // No media?
    // Have a look for a local disk anyway.
    result := GetLocalDriveType( DriveNumber );
  end
  else
  begin
    result := dtNone;
  end;

  DosErrorAPI( FERR_ENABLEHARDERR );
end;

const
  DEVLEN = 8;
  CNLEN  = 15;               // Computer name length
  UNCLEN = (CNLEN+2);        // UNC computer name length
  NNLEN  = 12;               // 8.3 Net name length  (share name length)
  RMLEN  = (UNCLEN+1+NNLEN); // Maximum remote name length

type
  use_info_0 = record
    ui0_local: cstring[ DEVLEN ]; // note this is of size DEVLEN + 1
    ui0_pad_1: char;
    ui0_remote: pchar;
    space: array[ 0..RMLEN ] of char; // remote path is written to somewhere in here
  end;

  use_info_1 = record
    ui0_local: cstring[ DEVLEN ];
    ui0_pad_1: char;
    ui0_remote: pchar; // address of a buffer to hold remote path
    ui1_password: pchar; //
    ui1_status: USHORT;
    ui1_asg_type: SHORT;
    ui1_refcount: USHORT;
    ui1_usecount: USHORT;
    space: array[ 0..RMLEN ] of char; // remote path is written to somewhere in here
  end;

  TNet32UseGetInfo = Function( pszServer: pchar;
                               pszUseName: pchar; // e.g. drive x:
                               ulLevel: ULONG;
                               pbBuffer: pointer; // pointer to output buffer
                               ulBuffer: ULONG; // size of output in buffer
                               Var pulTotalAvail: ULONG )
                               : word; CDecl;

Var
  Net32UseGetInfo: TNet32UseGetInfo;
  hNetAPI32DLL: HMODULE;
  TriedLoading: boolean;

//   129 Net32UseGetInfo
Function GetNetworkDriveRemotePath( DriveNumber: longint ): string;
var
  ErrorName: array[ 0..255 ] of char;
  dummy: cstring;
  rc: word;
  UseName: array[ 0..255 ] of char;
  UseInfo: use_info_0;
  pUseInfo: pointer;
  TotalBytesNeeded: ULONG;
  RemotePath: array[ 0..255 ] of char;
  Dummy2: array[ 0..4096 ] of char; // try to fix stack probs
begin
  Result := '';

  if not TriedLoading then
  begin
    TriedLoading := true;
    rc := DosLoadModule( ErrorName,
                         sizeof( ErrorName ),
                         'NETAPI32',
                         hNetAPI32DLL );
    if rc = NO_ERROR then
    begin
      // NetAPI32.DLL loaded OK
      rc := DosQueryProcAddr( hNetAPI32DLL,
                              129,
                              dummy,
                              pointer( Net32UseGetInfo ) );
      if rc <> 0 then
        Net32UseGetInfo := nil;
    end;
  end;

  if Assigned( Net32UseGetInfo ) then
  begin
    UseName[ 0 ] := DriveNumberToLetter( DriveNumber );
    UseName[ 1 ] := ':';
    UseName[ 2 ] := #0;

    RemotePath[ 0 ] := #0;
//    UseInfo.ui0_remote := Addr( RemotePath );

    pUseInfo := Addr( UseInfo );
    rc := Net32UseGetInfo( nil, // server - always nil
                           Addr( UseName ),
                           0, // info level 0
                           pUseInfo,
                           sizeof( UseInfo ),
                           TotalBytesNeeded );

    if rc = 0 then
      Result := StrPas( UseInfo.ui0_remote );

  end;
end;

end.