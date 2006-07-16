Unit SyncObjects;

/*********************************
  Thread Synchronization Classes
  for OS/2 and Win32

  1998.12.07 by Jens Schiffler

                Lochg„áchen 16
                86152 Augsburg Germany

                jensja@rz.fh-augsburg.de

**********************************/

/*
  IN WIN95 MUSS CRIPPLED32 DEFINIERT SEIN
  IN Win98 wohl nicht mehr???
 */

//{$define CRIPPLED32}

Interface

{$ifdef OS2}
Uses
  BseDos, BseErr, Os2Def;
{$endif}
{$ifdef WIN32}
Uses
  WinBase, WinNT, WIn32Add;
{$endif}

Const
  INDEFINITE_WAIT= -1;
  NO_WAIT= 0;

  Procedure Delay(ms: LONGWORD);

Type

  TMutexSem= Class
    {
      Mutex Semaphor
    }
    Protected
      FHandle: LONGWORD;
      FName: CString;
    Public
      Constructor Create(shared: Boolean; name: CString; owned: Boolean);
      Destructor Destroy; Override;
      Function Request (TimeOut: LONGWORD): Boolean; virtual;
      Function Release: Boolean; virtual;

      Property Name: CString read FName;
  End; { TMutexSem }

  TCritSec= Class (TMutexSem)
    {
      Critical Section
    }
    {$ifdef WIN32}
      FCRITICAL_SECTION: CRITICAL_SECTION;
    {$endif}
    Public
      Constructor Create(name: CString);
      Destructor Destroy; override;
      Function Request (TimeOut: LONGWORD): Boolean; override;
      Function Release: Boolean; override;
  End;

  TEventSem= Class
    {
      Event Semaphor (no count, only Set/Reset !)
    }
    Protected
      FEvent: LONGWORD;
      FName: CString;
      FPostCount: LONGWORD;
    Public
      Constructor Create(shared: Boolean; name: CString; posted: Boolean);
      Destructor Destroy; override;
      Function Post: Boolean; virtual;
      Function WaitFor(TimeOut: LONGWORD): Boolean; virtual;
      Function Reset: Boolean; virtual;
      Property LastPostCount: LONGWORD read FPostCount;
  End;

  TPeriodicTimer= Class(TEventSem)
    {
      Periodic Timer
    }
    Protected
      FTimer: LONGWORD;
      FInterval: LONGWORD; //millisekunden
    Public
      Constructor Create(shared: Boolean; name: CString; interval: LONGWORD);
      Destructor Destroy; Override;
      Function WaitFor(TimeOut: LONGWORD): Boolean; virtual;
      Procedure Start; virtual;
      Procedure Stop; virtual;
  End;


  Var
    GlobalCritSec: TCritSec;
    GlobalMutex: TMutexSem;

Implementation

/*--------------------
  TMutexSem
*/

  Constructor TMutexSem.Create(shared: Boolean; name: CString; owned: Boolean);
  {$ifdef OS2}
  Var
    rc: LONGWORD;
    flAttr: LONGWORD;
  Begin

    If Shared Then flAttr:= BseDos.DC_SEM_SHARED
    Else flAttr:= 0;

    If Length(name) = 0 Then
    Begin
      rc:= BseDos.DosCreateMutexSem(
                                    NIL,
                                    FHandle,
                                    flAttr,
                                    owned
                                   );
    End
    Else
    Begin
      FName:= name;
      rc:= BseDos.DosCreateMutexSem(
                                    name,
                                    FHandle,
                                    flAttr,
                                    owned
                                   );
    End;

  End;
  {$endif}{OS2}
  {$ifdef WIN32}
  Begin
    If Length(name) = 0 Then
    Begin
      FHandle:= WinBase.CreateMutex(
                                    NIL, //lpSecurityAttributes
                                    owned,
                                    NIL  //name
                                   );
    End
    Else
    Begin
      FName:= name;
      FHandle:= WinBase.CreateMutex(
                                    NIL, //lpSecurityAttributes
                                    owned,
                                    name
                                   );
    End;
//    Result:= (FHandle <> 0);
  End;
  {$endif}{WIN32}

  Destructor TMutexSem.Destroy;
  {$ifdef OS2}
  Begin
    BseDos.DosCloseMutexSem(FHandle);
  End;
  {$endif}{OS2}
  {$ifdef WIN32}
  Begin
    WinBase.CLoseHandle(FHandle);
  End;
  {$endif}{WIN32}

  Function TMutexSem.Request (TimeOut: LONGWORD): Boolean;
  {$ifdef OS2}
  Var
    rc: LONGWORD;
  Begin
    rc:= BseDos.DosRequesTMutexSem(FHandle, TimeOut);
    Result:= (rc= BseErr.NO_ERROR);
  End;
  {$endif OS2}
  {$ifdef WIN32}
  Var
    rc: LONGWORD;
  Begin
    rc:= WinBase.WaitForSingleObject(FHandle, TimeOut);
    Result:= (rc= WAIT_OBJECT_0) OR (rc= WAIT_ABANDONED);
  End;
  {$endif WIN32}

  Function TMutexSem.Release: Boolean;
  {$ifdef OS2}
  Var
    rc: LONGWORD;
  Begin
    rc:= BseDos.DosReleaseMutexSem(FHandle);
    Result:= (rc= BseErr.NO_ERROR);
  End;
  {$endif}{OS2}
  {$ifdef WIN32}
  Begin
    Result:= WinBase.ReleaseMutex(FHandle);
  End;
  {$endif}{WIN32}

/*----------------------
  TCritSec
*/
  Constructor TCritSec.Create(name: CString);
  {$ifdef OS2}
  Begin
  End;
  {$endif}
  {$ifdef WIN32}
  Begin
    WinBase.InitializeCriticalSection(FCRITICAL_SECTION);
  End;
  {$endif}

  Destructor TCritSec.Destroy;
  {$ifdef OS2}
  Begin
  End;
  {$endif}
  {$ifdef WIN32}
  Begin
    WinBase.DeleteCriticalSection(FCRITICAL_SECTION);
  End;
  {$endif}

  Function TCritSec.Request (TimeOut: LONGWORD): Boolean;
  {$ifdef OS2}
  Begin
    Result:= (BseDos.DosEnterCritSec= BseErr.NO_ERROR);
  End;
  {$endif}
  {$ifdef WIN32}
  Begin
    WinBase.EnterCriticalSection(FCRITICAL_SECTION);
    Result:= TRUE;
  End;
  {$endif}

  Function TCritSec.Release: Boolean;
  {$ifdef OS2}
  Begin
    Result:= (BseDos.DosExitCritSec= BseErr.NO_ERROR);
  End;
  {$endif}
  {$ifdef WIN32}
  Begin
    WinBase.LeaveCriticalSection(FCRITICAL_SECTION);
    Result:= TRUE;
  End;
  {$endif}

/*
  TEventSem
*/
  Constructor TEventSem.Create(shared: Boolean; name: CString; posted: Boolean);
  {$ifdef OS2}
  Var
    rc,
    flAttr: LONGWORD;
  Begin

    If shared Then
      flAttr:= BseDos.DC_SEM_SHARED
    Else
      flAttr:= 0;

    If Length(name) = 0 Then
    Begin
      rc:= BseDos.DosCreateEventSem(
                                    NIL,
                                    FEvent,
                                    flAttr,
                                    posted
                                   );

    End
    Else
    Begin
      FName:= name;
      rc:= BseDos.DosCreateEventSem(
                                    name,
                                    FEvent,
                                    flAttr,
                                    posted
                                   );
    End;

//    Result:= (rc= BseErr.NO_ERROR);
  End;
  {$endif}{OS2}
  {$ifdef WIN32}
  Begin
    If Length(name) = 0 Then
    Begin
      FEvent:= WinBase.CreateEvent(
                                    NIL, //SecurityAttributes
                                    TRUE, //Manual Reset
                                    posted,
                                    NIL  //Name
                                   );
    End
    Else
    Begin
      FName:= name;
      FEvent:= WinBase.CreateEvent(
                                    NIL, //SecurityAttributes
                                    TRUE, //Manual Reset
                                    posted,
                                    name
                                   );
    End;

//    Result:= (FHandle <> 0);
  End;
  {$endif}{WIN32}

  Destructor TEventSem.Destroy;
  {$ifdef OS2}
  Begin
    BseDos.DosCloseEventSem(FEvent);
  End;
  {$endif}{OS2}
  {$ifdef WIN32}
  Begin
    WinBase.CloseHandle(FEvent);
  End;
  {$endif}{WIN32}

  Function TEventSem.Post: Boolean;
  {$ifdef OS2}
  Begin
    Result:= (BseDos.DosPostEventSem(FEvent) <> BseErr.ERROR_INVALID_HANDLE);
  End;
  {$endif}{OS2}
  {$ifdef WIN32}
  Begin
    Result:= WinBase.SetEvent(FEvent);
  End;
  {$endif}{WIN32}

  Function TEventSem.WaitFor(TimeOut: LONGWORD): Boolean;
  {$ifdef OS2}
  Begin
    Result:= (BseDos.DosWaitEventSem(FEvent, TimeOut)= BseErr.NO_ERROR);
  End;
  {$endif}{OS2}
  {$ifdef WIN32}
  Var
    rc: LONGWORD;
  Begin
    rc:= WinBase.WaitForSingleObject(FEvent, TimeOut);
    Result:= (rc= WAIT_OBJECT_0) OR (rc= WAIT_ABANDONED);
  End;
  {$endif}{WIN32}

  Function TEventSem.Reset: Boolean;
  {$ifdef OS2}
  Var
    rc: LONGWORD;
  Begin
    rc:= BseDos.DosResetEventSem(FEvent, FPostCount);
    Result:= (rc <> BseErr.ERROR_INVALID_HANDLE) AND (rc <> BseErr.ERROR_ALREADY_RESET);
  End;
  {$endif}{OS2}
  {$ifdef WIN32}
  Begin
    Result:= WinBase.ResetEvent(FEvent);
  End;
  {$endif}{WIN32}

/*
  TPeriodicTimer.
  NUR IN OS/2, WIN NT 4.0, WIN98
  NICHT IN WIN95 -> Workaround mit Sleep
*/
  Constructor TPeriodicTimer.Create(shared: Boolean; name: CString; interval: LONGWORD);
  {$ifdef OS2}
  Begin
    Inherited.Create(TRUE, name, FALSE); //TEventSem
    BseDos.DosStartTimer(
                         interval,
                         FEvent,
                         FTimer
                        );
  End;
  {$endif}{OS2}
  {$ifdef WIN32}
    {$ifdef CRIPPLED32}
    Begin
      FInterval:= Interval;
    End;
    {$else}
    Var
      DUE_TIME: WinBase.FileTime;
    Begin
      If Length(name) = 0 Then
      Begin
        FTimer:= Win32Add.CreateWaitableTimer(
                                    NIL, //Attribute
                                    FALSE, //Manual Reset
                                    NIL
                                   );
      End
      Else
      Begin
        FName:= name;
        FTimer:= Win32Add.CreateWaitableTimer(
                                    NIL, //Attribute
                                    FALSE, //Manual Reset
                                    PChar(name)
                                   );
      End;

      DUE_TIME.dwLowDateTime:= -1;
      DUE_TIME.dwHighDateTime:= -1;

      Win32Add.SetWaitableTimer(
                               FTimer,
                               @DUE_TIME, //NIL, //DueTime (64Bit)
                               Interval,
                               NIL, //Completion Routine
                               NIL, //Completion Routine Data
                               TRUE //Resume upon Power Suspend
                              );

    End;
    {$endif}{CRIPPLED32}
  {$endif}{WIN32}

  Destructor TPeriodicTimer.Destroy;
  {$ifdef OS2}
  Begin
    Stop;
    Inherited.Destroy;
  End;
  {$endif}{OS2}
  {$ifdef WIN32}
    {$ifdef CRIPPLED32}
    Begin
    End;
    {$else}
    Begin
      Stop;
      WinBase.CloseHandle(FTimer);
    End;
    {$endif}{CRIPPLED32}
  {$endif}{WIN32}

  Function TPeriodicTimer.WaitFor(TimeOut: LONGWORD): Boolean;
  {$ifdef OS2}
  Begin
    Result:= Inherited.WaitFor(TimeOut);
    If Result Then Result:= Inherited.Reset;
  End;
  {$endif}{OS2}
  {$ifdef WIN32}
    {$ifdef CRIPPLED32}
    Begin
      /*
        Mieser Workaround!!!
      */
      WinBase.Sleep(FInterval);
      Result:= TRUE;
    End;
    {$else}
    Var
      rc: LONGWORD;
    Begin
      rc:= WinBase.WaitForSingleObject(FTimer, TimeOut);
      Result:= (rc= WAIT_OBJECT_0);// OR (rc= WAIT_ABANDONED); {only for mutex?}
(*
{ no reset necessary }
      If Result Then
      Begin

        {rc:=} Win32Add.ResetWaitableTimer(FTimer);

      End;
*)
    End;
    {$endif}{CRIPPLED32}
  {$endif}{WIN32}

  Procedure TPeriodicTimer.Start;
  {$ifdef OS2}
  Begin
  End;
  {$endif}{OS2}
  {$ifdef WIN32}
  Begin
  End;
  {$endif}{WIN32}

  Procedure TPeriodicTimer.Stop;
  {$ifdef OS2}
  Begin
    BseDos.DosStopTimer(FTimer);
  End;
  {$endif}{OS2}
  {$ifdef WIN32}
    {$ifdef CRIPPLED32}
    Begin
    End;
    {$else}
    Begin
      Win32Add.CancelWaitableTimer(FTimer);
    End;
    {$endif}{CRIPPLED32}
  {$endif}{WIN32}

  Procedure Delay(ms: LONGWORD);
  Begin
    {$ifdef OS2}
    BseDos.DosSleep(ms);
    {$endif}
    {$ifdef WIN32}
    WinBase.Sleep(ms);
    {$endif}
  End; {Delay}

Initialization
  GlobalCritSec.Create('');
  GlobalMutex.Create(False, '', False);
Finalization
  GlobalMutex.Destroy;
  GlobalCritSec.Destroy;
End.

