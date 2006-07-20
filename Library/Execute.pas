Unit Execute;

Interface

Type
  Results=record
  RunResult: integer;
  ProgramResult: integer;
  end;

  TermQResults=record
    SessionID: WORD;
    ResultCode: WORD;
    end;

Const
  COULD_NOT_START=-7;
  INTERNAL_ERROR=-77;

Var
  DosStartSessionResult: integer;

Function RunProgram( name:string; parameters: string ):integer;

Function RunCommand( command: string ):integer;

Implementation

Uses
  BseDos, OS2Def, BseErr, Forms;

Function RunCommand( command: string ):integer;
Begin
  Result:=RunProgram( 'cmd.exe', '/c '+command );
End;

Function RunProgram( name:string; parameters: string ): integer;
Type
  pByte=^BYTE;
Var
  psd:STARTDATA;
  SessID:LONGWORD;
  apid:LONGWORD;
  PgmTitle:CSTRING;
  PgmName:CSTRING;
  ObjBuf:CSTRING;
  rc:Integer;

  Args, ReportTypeStr:CSTRING;

  TerminationQueue:HQueue;
  QueueName: CSTRING;
  QueueRequest: REQUESTDATA;
  DataLength: ULONG;
  DataAddress: ^TermQResults;
  ElementCode: ULONG;
  NoWait: BOOL;
  ElemPriority: BYTE;
  SemName: CString;
  SemHandle: HEV;
  OwningPID: PID;

  SemPostCount: ULONG;
Begin
  DosStartSessionResult:=0;

  QueueName:='\QUEUES\SIBYL_EXECUTE_TERMQ';
  rc:= DosCreateQueue( TerminationQueue,
                       QUE_FIFO, // normal queue
                       QueueName );
  if (rc<>0) then
  begin
    Result:=INTERNAL_ERROR;
    exit;
  end;

  psd.Length := SizeOf( psd );
  psd.Related := SSF_RELATED_CHILD; // Yes we want to know about it
  psd.FgBg := SSF_FGBG_FORE; // run in foreground
  psd.TraceOpt := SSF_TRACEOPT_NONE; // no tracing!

  PgmName := name; // copy to a cstring
  psd.PgmName := @PgmName; // program
  psd.PgmTitle := @PgmName; // window title

  Args:=parameters; // copy to a cstring
  psd.PgmInputs := @Args;  //arguments
  psd.TermQ := @QueueName; // Termination Queue name
  psd.Environment := NIL; // no special environment to pass
  psd.InheritOpt := SSF_INHERTOPT_PARENT;
    // use parent file handles
    // AND (more importantly) parent's current drive and dir
  psd.SessionType := SSF_TYPE_DEFAULT; // whatever the exe says
  psd.IconFile := NIL; // no icon file
  psd.PgmHandle := 0; // no program handle
  psd.PgmControl := 0; // SSF_CONTROL_MINIMIZE; // run minimized
  psd.InitXPos :=0; // position x
  psd.InitYPos :=0; // position y
  psd.Reserved := 0; // blah
  psd.ObjectBuffer := @ObjBuf; // put errors here
  psd.ObjectBuffLen := 100; // up to 100 chars

  rc := DosStartSession(  psd, SessID, apid );

  if (rc<>0)
     // but we don't care if it just started in the background!
     and (rc<>ERROR_SMG_START_IN_BACKGROUND)
     then
  begin
    Result:=COULD_NOT_START;
    DosStartSessionResult:=rc;
    DosCloseQueue( TerminationQueue );
    exit;
  end;

  // DosCloseQueue( TerminationQueue );
  // exit;

  // create a semaphore so we can check the queue!!
  SemName := '\SEM32\SIBYL_EXECUTE_TERMQ';
  rc := DosCreateEventSem( SemName,
                           SemHandle,
                           0,
                           FALSE );
  if ( rc <> 0 ) then
  begin
    Result:= INTERNAL_ERROR;
    DosStartSessionResult:= rc;
    DosCloseQueue( TerminationQueue );
    exit;
  end;

  ElementCode:=0; // get element at front of queue
  NoWait:=True; // don't wait for data: so we supply semaphore instead

  // assocaite the semaphore with the queue, don't remove item if it's already there
  rc:= DosPeekQueue( TerminationQueue,
                     QueueRequest,
                     DataLength,
                     DataAddress,
                     ElementCode,
                     NoWait,
                     ElemPriority,
                     SemHandle );

  repeat
    rc := DosQueryEventSem( SemHandle, SemPostCount );

    // Handle PM messages (including to our own app!)
    Application.ProcessMessages;

    // Give up CPU briefly so we don't appear too greedy when idle.
    DosSleep( 1 );

  until SemPostCount > 0; // until semaphore is posted, indicating an item in the TermQ

  // The program has terminated.
  // Now read the item
  rc:= DosReadQueue( TerminationQueue,
                     QueueRequest,
                     DataLength,
                     DataAddress,
                     ElementCode,
                     NoWait,
                     ElemPriority,
                     SemHandle );

  Result:=DataAddress^.ResultCode;
  DosCloseQueue( TerminationQueue );
  // Free the memory used by the queue element
  DosFreeMem( DataAddress );

  // Close semaphore
  rc := DosCloseEventSem( SemHandle );

End;

Initialization
End.
