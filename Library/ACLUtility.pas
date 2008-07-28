Unit ACLUtility;

Interface

Uses
  Classes, SysUtils,
{$ifdef os2}
  Os2Def, IniFiles;
{$endif}
{$ifdef win32}
  Registry;
{$endif}

function GetACLLibraryVersion: string;

Type
  TPrintOutput = procedure( TheText: string ) of object;
  TTerminateCheck = function: boolean of object;
  TProgressCallback = procedure( n, outof: integer;
                                 Message: string ) of object;

const
   _MAX_PATH = 260;    // max. length of full pathname
   _MAX_DRIVE = 3;     // max. length of drive component
   _MAX_DIR = 256;     // max. length of path component
   _MAX_FNAME = 256;   // max. length of file name component
   _MAX_EXT = 256;     // max. length of extension component

   mrFailed = $c000 + 1;

{$ifdef win32}
Function GetAPIErrorString( ErrorCode: integer ): string;
Function GetLastAPIErrorString: string;
{$endif}

Type
  TMyIniFile =
{$ifdef os2}
    class( TAsciiIniFile )
{$else}
    class( TRegIniFile )
{$endif}
    constructor CreateMe( const Path: string );
    destructor Destroy; override;
  end;

{$ifdef os2}
function AllocateMemory( const Size: longint ): pointer;
procedure DeallocateMemory( Var P: pointer );
procedure Sleep( const milliseconds: longint );
{$endif}

// allocate a new copy of the given source memory
procedure AllocMemCopy( const Source: pointer;
                        var Dest: pointer;
                        const Size: longint );

procedure MemCopy( const Source: pointer;
                   const Dest: pointer;
                   const Size: longint );

procedure FillMem( Dest: pointer;
                   Size: longint;
                   Data: Byte );

// Returns A - B
function PtrDiff( A, B: pointer ): longword;

function Min( const a: longint;
              const b: longint ): longint;

function Max( const a: longint;
              const b: longint ): longint;

function Between( const Value: longint;
                  const Limit1: longint;
                  const Limit2: longint ): boolean;

function PtrBetween( const Value: pointer;
                     const Limit1: pointer;
                     const Limit2: pointer ): boolean;

Procedure AddList( Source, Dest: TList );

Procedure AssignList( Source, Dest: TList );

// Destroy the objects stored in List
// and clear the list.
Procedure ClearListAndObjects( List: TList );

// Destroy the objects stored in the list
// and then destroy the list itself
// And set the reference to nil
Procedure DestroyListAndObjects( Var List: TList );

// Destroy the objects stored in the list.
// You probably want to use one of the two functions above.
Procedure DestroyListObjects( List: TList );

// Returns the filename of the running app
// (including drive/directory)
Function GetApplicationFilename: string;

// Returns the starting directory of the app
Function GetApplicationDir: string;

{$ifdef win32}
type
  TDaylightSavingStatus =
  (
    dssDisabled,
    dssDaylightSaving,
    dssNormal
  );

function GetDaylightSavingStatus: TDaylightSavingStatus;
{$endif}

type
  TDataList = TList; // to help distinguish from TObjectList below
  
type
  TObjectListSortCompare = function ( Item1, Item2: TObject ): Integer;

  // TObjectList has exactly the same functionality as TList,
  // except that rather than using untyped pointers it expects
  // object references. This means that incorrect typecasts can
  // be detected when using the list
  TObjectList = class( TList )
  protected
    function Get( index: integer ): TObject;
    procedure Put( index: integer; Item: TObject );
  public
    function Add( item: TObject ): integer;
    function First: TObject;
    function IndexOf( item: TObject ): integer;
    procedure Insert( Index: Integer; Item: TObject );
    function Last: TObject;
    function Remove( Item: TObject ): Integer;
    procedure Sort( Compare: TObjectListSortCompare );
    property Items[ index: integer ]: TObject read Get write Put; default;

    // Additional:
    // Copy the given list to this one.
    procedure Assign( Source: TObjectList );
    // Add the objects in the given list to this one
    procedure AddList( Source: TObjectList );

  end;

// Raise an exception if the given Code is <> 0
procedure CheckSystemError( Code: longword;
                            Message: string );

{$ifdef os2}
function GetUserProfileString( const AppName: string;
                               const KeyName: string;
                               const Default: string ): string;

Procedure SetUserProfileString( const AppName: string;
                                const KeyName: string;
                                const Value: string );

Procedure LoadDLLFunction( const DLLName: string;
                           const FunctionName: string;
                           var hDLL: HMODULE;
                           var F: pointer );

{$endif}

Implementation

// Implementation ------------------------------------------

Uses
{$ifdef os2}
  Dos, BseDos, BseTib, PmWin, PmGpi, PmDev, PmShl;
{$else}
  Windows, FileCtrl;
{$endif}

constructor TMyIniFile.CreateMe( const Path: string );
begin
{$ifdef os2}
  Inherited Create( Path );
{$else}
  Inherited Create( Path );
{$endif}
end;

destructor TMyIniFile.Destroy;
begin
{$ifdef os2}
  Refresh;
{$endif}
  inherited Destroy;
end;

{$ifdef os2}
function AllocateMemory( const Size: longint ): pointer;
begin
  GetMem( Result, size + sizeof( Size ) );
  pLong( Result )^ := Size;
  inc( Result, sizeof( Size ) );
end;

procedure DeallocateMemory( Var P: pointer );
var
  Size: longint;
begin
  if P = nil then
    exit;

  dec( P, sizeof( Size ) );
  Size := pLong( P )^;
  FreeMem( P, Size + sizeof( Size ) );
  P := nil;
end;
{$endif}

{$ifdef win32}
Function GetAPIErrorString( ErrorCode: integer ): string;
var
  buffer: array[ 0..1000 ] of char;
begin
  if FormatMessage( FORMAT_MESSAGE_FROM_SYSTEM,
                    nil, // no special message source
                    ErrorCode,
                    0, // use default language
                    Buffer,
                    Sizeof( Buffer ),
                    nil ) > 0
  then // no arguments
    Result:= IntToStr( ErrorCode ) + ': ' + Buffer
  else
    Result:= '(Unknown error)';

end;

Function GetLastAPIErrorString: string;
begin
  Result := GetAPIErrorString( GetLastError );
end;
{$endif}

procedure AllocMemCopy( const Source: pointer;
                        var Dest: pointer;
                        const Size: longint );
begin
  GetMem( Dest, Size );
  MemCopy( Source, Dest, Size );
end;

procedure MemCopy( const Source: pointer;
                   const Dest: pointer;
                   const Size: longint );
begin
  Move( Source^, Dest^, Size );
end;

procedure FillMem( Dest: pointer;
                   Size: longint;
                   Data: Byte );
begin
  FillChar( Dest^, Size, Data );
end;

function PtrDiff( A, B: pointer ): longword;
begin
  result:= longword( A ) - longword( B );
end;

Procedure AddList( Source, Dest: TList );
var
  i: longint;
begin
  // expand the destination list
  // to what's required
  Dest.Capacity := Dest.Capacity
                   + Source.Capacity;
  for i:= 0 to Source.Count - 1 do
    Dest.Add( Source[ i ] );
end;

Procedure AssignList( Source, Dest: TList );
begin
  Dest.Clear;
  AddList( Source, Dest );
end;

{$ifdef win32}
function GetDaylightSavingStatus: TDaylightSavingStatus;
var
  TimeZoneInfo: TIME_ZONE_INFORMATION;
  ZoneID: DWORD;
Begin
  ZoneID:= GetTimeZoneInformation( TimeZoneInfo );
  if TimeZoneInfo.DaylightBias = 0 then
    Result:= dssDisabled
  else if ZoneID = TIME_ZONE_ID_DAYLIGHT then
    Result:= dssDaylightSaving
  else
    Result:= dssNormal;
end;
{$endif}

// Destroy the objects stored in List
// and clear the list.
Procedure ClearListAndObjects( List: TList );
begin
  DestroyListObjects( List );
  List.Clear;
end;

// Destroy the objects stored in the list
// and then destroy the list itself.
Procedure DestroyListAndObjects( Var List: TList );
begin
  if not Assigned( List ) then
    exit;

  DestroyListObjects( List );
  List.Destroy;
  List := nil;
end;

Procedure DestroyListObjects( List: TList );
var
  Index: longint;
begin
  for Index := 0 to List.Count - 1 do
  begin
    if List[ Index ] <> nil then
    begin
      TObject( List[ Index ] ).Destroy;
      List[ Index ] := nil;
    end;
  end;
end;

function Min( const a: longint;
              const b: longint ): longint;
begin
  if a<b then
   result:= a
  else
   result:= b;
end;

function Max( const a: longint;
              const b: longint ): longint;
begin
  if a>b then
   result:= a
  else
   result:= b;
end;

function Between( const Value: longint;
                  const Limit1: longint;
                  const Limit2: longint ): boolean;
begin
  if Limit1 < Limit2 then
    Result:= ( Value >= Limit1 ) and ( Value <= Limit2 )
  else
    Result:= ( Value >= Limit2 ) and ( Value <= Limit1 )
end;

function PtrBetween( const Value: pointer;
                     const Limit1: pointer;
                     const Limit2: pointer ): boolean;
var
  v, p1, p2: pchar;
begin
  v := Value;
  p1 := Limit1;
  p2 := Limit2;
  if p1 < p2 then
    Result:= ( V >= p1 ) and ( V <= p2 )
  else
    Result:= ( V >= p2 ) and ( V <= p1 )
end;

{$ifdef os2}
Function GetApplicationFilename: string;
var
  pThreadInfo: PTIB;
  pProcessInfo: PPIB;
  ProcessName: cstring[ _MAX_PATH ];
begin
  DosGetInfoBlocks( pThreadInfo,
                    pProcessInfo );
  DosQueryModuleName( pProcessInfo^.pib_hmte,
                      sizeof( ProcessName ),
                      ProcessName );
  Result := ProcessName;
end;
{$else}
Function GetApplicationFilename: string;
var
  ProcessName: array[ 0.._MAX_PATH ] of char;
begin
  GetModuleFileName( 0, // our own process
                     ProcessName,
                     sizeof( ProcessName ) );

  Result := ProcessName;
end;
{$endif}

Function GetApplicationDir: string;
begin
  Result := ExtractFilePath( GetApplicationFilename );
end;

{ TObjectList }

function TObjectList.Add( item: TObject ): integer;
begin
  result := inherited Add( item );
end;

function TObjectList.First: TObject;
begin
  result := inherited First;
end;

function TObjectList.Get( index: integer ): TObject;
begin
  result := Items[ Index ];
end;

function TObjectList.IndexOf( item: TObject ): integer;
begin
  result := inherited IndexOf( item );
end;

procedure TObjectList.Insert( Index: Integer; Item: TObject );
begin
  inherited Insert( Index, Item );
end;

function TObjectList.Last: TObject;
begin
  result := inherited Last;
end;

procedure TObjectList.Put( index: integer; Item: TObject );
begin
  Items[ Index ] := Item;
end;

function TObjectList.Remove( Item: TObject ): Integer;
begin
  result := inherited Remove( Item );
end;

procedure QuickSortObjectList( SortList: PPointerList;
                               L, R: Integer;
                               CompareFunction: TObjectListSortCompare );
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while CompareFunction( TObject( SortList^[I] ), P ) < 0 do
        Inc(I);
      while CompareFunction( TObject( SortList^[J] ), P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSortObjectList( SortList, L, J, CompareFunction );
    L := I;
  until I >= R;
end;

procedure TObjectList.Sort( Compare: TObjectListSortCompare );
begin
  if ( List <> nil ) and ( Count > 0 ) then
    QuickSortObjectList( List, 0, Count - 1, Compare );
end;

procedure TObjectList.Assign( Source: TObjectList );
begin
  Clear;
  AddList( Source );
end;

procedure TObjectList.AddList( Source: TObjectList );
var
  i: integer;
begin
  for i := 0 to Source.Count - 1 do
    Add( Source[ i ] );
end;

{$ifdef os2}
procedure Sleep( const milliseconds: longint );
begin
  DosSleep( milliseconds );
end;
{$endif}

// Raise an exception if the given Code is <> 0
procedure CheckSystemError( Code: longword;
                            Message: string );
begin
  if Code <> 0 then
    raise Exception.Create( Message
                            + ': ['
                            + IntToStr( Code )
                            + '] '
                            + SysErrorMessage( Code ) );
end;

{$ifdef os2}
function GetUserProfileString( const AppName: string;
                               const KeyName: string;
                               const Default: string ): string;
var
  Buffer: array[ 0..255 ] of char;
  Len: longint;
  i: longint;
  szDefault: cstring;
begin
  szDefault := Default;
  Len := PrfQueryProfileString( HINI_USERPROFILE, // user profile
                                AppName, // application
                                KeyName, // key
                                szDefault,
                                Buffer,
                                sizeof( Buffer ) );

  // remove null terminator, if present
  if Len > 0 then
    if Buffer[ Len - 1 ] = #0 then
      dec( Len );
  Result := '';
  for i := 0 to Len - 1 do
    Result := Result + Buffer[ i ];
end;

Procedure SetUserProfileString( const AppName: string;
                                const KeyName: string;
                                const Value: string );
begin
  if not PrfWriteProfileString( HINI_USERPROFILE, // user profile
                                AppName, // application
                                KeyName, // key
                                Value ) then
    raise Exception.Create(
                'Error writing INI ['
                + AppName
                + '/'
                + KeyName
                + '] rc = '
                + IntToHex( WinGetLastError( AppHandle ), 8 ) );
end;

procedure LoadDLLFunction( const DLLName: string;
                           const FunctionName: string;
                           var hDLL: HMODULE;
                           var F: pointer );
var
  ActualDLLName: string;
  csErrorObject: cstring;
  csFunctionName: cstring;
  rc: APIRET;
begin
  F := nil;

  if hDLL = NullHandle then
  begin
    ActualDLLName := GetApplicationDir + DLLName;
    if not FileExists( ActualDLLName ) then
      ActualDLLName := DLLName;

    rc := DosLoadModule( csErrorObject,
                         sizeof( csErrorObject ),
                         ActualDLLName,
                         hDLL );
    if rc <> 0 then
      raise Exception.Create( DLLName
                              + ' could not be loaded: '
                              + SysErrorMessage( rc ) );

  end;

  csFunctionName := FunctionName;
  rc := DosQueryProcAddr( hDLL,
                          0, // using by name
                          csFunctionName,
                          F );
  if rc <> 0 then
    raise Exception.Create( FunctionName
                            + ' in '
                            + DLLName
                            + ': '
                            + SysErrorMessage( rc ) );
end;

{$endif}

const
  LibVersion = 'V1.5.14'; // $SS_REQUIRE_NEW_VERSION$

function GetACLLibraryVersion: string;
begin
  Result := LibVersion;
end;

Initialization
End.

