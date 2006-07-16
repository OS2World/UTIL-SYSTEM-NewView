Unit HT;
// Helperthread and supporting classes - Erik Huelsmann
// Vandalised by Aaron Lawrence
// V1.2
// Add simple string message send method.
// Seems that Erik forgot to set FScheduledOn anywhere!!
//
// V1.1
// Removed OnSchedule complexities. Now TStdTask.Schedule takes
// parameters object.
Interface

Uses
  Classes, Forms, SyncObjects;

Type
  TTaskThread = class;
  EThreadingException = class(SysException);

  // A running instance of a TTask, basically
  TTaskItem = class(TObject)  // used to schedule tasks in the TaskThread
  protected
    FCanceled: boolean;
    FErrorNr: longint;
    FFinished: boolean;
    FScheduledOn: TTaskThread;
    FFreeOnFinish: boolean;
    function GetScheduled: boolean;
    function GetRunning: boolean;
    procedure SetCanceled( C: boolean);
    procedure SetErrorNr( E: longint);
    procedure SetFreeOnFinish( F: boolean);
  public
    procedure Execute; virtual;
    procedure TaskProcedure; virtual; abstract;

    procedure Cancel; virtual; abstract;
    procedure Error; virtual; abstract;
    procedure Finish; virtual; abstract;
    procedure Start; virtual; abstract;

    property Canceled: boolean
      read FCanceled write SetCanceled;
    property ErrorNr: longint
      read FErrorNr write SetErrorNr;
    property Finished: boolean
      read FFinished write FFinished;
    property FreeOnFinish: boolean
      read FFreeOnFinish write SetFreeOnFinish;
    property Scheduled: boolean
      read GetScheduled;
    property Running: boolean
      read GetRunning;
    property Thread: TTaskThread
      read FScheduledOn;
  end;

  TTaskEvent = class(TObject) // used to send events to the primary thread
  public
    procedure Event; virtual; abstract;
  end;

//  TTaskList = class(TList)
//  end;

  // Actual THread for running TTasks
  TTaskThread = class(TThread)
  protected
    FTasks: TList;
    FTasksMutex: TMutexSem;
    FTasksChanged: TEventSem;
    FStop: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    procedure ExecuteTask; virtual;
    function FindTask( Task: TTaskItem): longint; virtual;
    procedure Schedule( Task: TTaskItem); virtual;
    procedure Stop; virtual;
    procedure UnSchedule( Task: TTaskItem); virtual;
 end;


type
 TBasicTask = class;
 TEventWait = (None,All,AllButProgress);

{Declare new class}
Type
  // Component which owns the actual thread
  THelperThread=Class(TComponent)
  Protected
    FThread: TTaskThread;
    FPriority: TThreadPriority;
    function GetThread: TTaskThread;
    procedure SetPriority( Prio: TThreadPriority);
    Procedure SetupComponent; Override;
  Public
    constructor Create(Parent: TComponent); override;
    Destructor Destroy; Override;
    procedure Schedule(Task: TTaskItem);

    property Thread: TTaskThread
      read GetThread;
  published
    property Priority: TThreadPriority
      read FPriority write SetPriority;
  End;

Type
  // Component which encapsulates a task
  TBasicTask = class(TComponent)
  private
    FThread: THelperThread;
    FFreeTasksOnComplete: boolean;

    procedure SetThread(N: THelperThread);
  Protected
    Procedure SetupComponent; Override;
    procedure Notification(AComponent:TComponent;Operation:TOperation); override;
  Public
    procedure Schedule;
  published
    property Thread: THelperThread
      read FThread write SetThread;
    property FreeTasksOnComplete: boolean
      read FFreeTasksOnComplete write FFreeTasksOnComplete;

  end;

{Declare new class}
Type
 TStdTaskItem = class;
 TTaskEventProc = procedure (Sender: TStdTaskItem) of object;
 TTaskProgressProc = procedure (Sender: TStdTaskItem; I: longint; const S: String) of object;
 TTaskProcedureProc = procedure (Parameters: TObject; Context: TStdTaskItem) of object;

 TStdTaskEventProc = class (TTaskEvent)
 public
   FOnTaskEvent: TTaskEventProc;
   FContext: TStdTaskItem;
   constructor Create( T: TStdTaskItem; FOTE: TTaskEventProc );
   procedure Event; override;
 end;

 TStdTaskProgressProc = class (TTaskEvent)
 public
   FOnTaskProgress: TTaskProgressProc;
   FContext: TStdTaskItem;
   FI: longint;
   Str: String;
   constructor Create( T: TStdTaskItem;
                       FOTP: TTaskProgressProc;
                       I: longint;
                       const S: String);
   procedure Event; override;
 end;

  TStringEvent = procedure( S: String ) of Object;
  TDataEvent = procedure( Data: TObject ) of Object;


  TStdTaskItem = class(TTaskItem)
  protected
    // AaronL - add temp storage for passing a string message
    FMessageString: string;
    FStringMessageHandler: TStringEvent;
    FDataMessageHandler: TDataEvent;

    FData: TObject;

    // Procedure executed in main thread context to do string callback
    Procedure DoStringMessage;

    Procedure DoDataMessage;

  public
    FOnStart,
    FOnFinish,
    FOnCancel,
    FOnError: TTaskEventProc;
    FOnProgress: TTaskProgressProc;
    FOnExecute: TTaskProcedureProc;
    Parameters: TObject;
    Errors: TObject;
    FEventWait: TEventWait;
    FEventWaitSem: TEventSem;
    destructor Destroy; override;
    procedure Cancel; override;
    procedure Error; override;
    procedure Finish; override;
    procedure Progress( I: longint; const S: String);
    procedure Start; override;
    procedure TaskProcedure; override;

    // Simple string message sender. 
    procedure SendStringMessage( const S: string;
                                 EventHandler: TStringEvent );
    procedure SendData( Data: TObject;
                        EventHandler: TDataEvent );
  end;

  TStdTask=Class(TBasicTask)
  private
    FDefaultCaption: PString;
    FOnStart,
    FOnFinish,
    FOnCancel,
    FOnError: TTaskEventProc;
    FOnProgress: TTaskProgressProc;
    FOnExecute: TTaskProcedureProc;
    FEventWait: TEventWait;
    FEventWaitSem: TEventSem;
    function GetDefaultCaption: String;
    procedure SetDefaultCaption( C: String);
    procedure SetEventWait( E: TEventWait);
  public
    constructor Create(Parent: TComponent); override;
    procedure Schedule( Parameters: TObject );
  published
    property DefaultCaption: String
      read GetDefaultCaption write SetDefaultCaption;
    property EventWait: TEventWait
      read FEventWait write SetEventWait;

    property OnStart: TTaskEventProc
      read FOnStart write FOnStart;
    property OnFinish: TTaskEventProc
      read FOnFinish write FOnFinish;
    property OnError: TTaskEventProc
      read FOnError write FOnError;
    property OnCancel: TTaskEventProc
      read FOnCancel write FOnCancel;
    property OnProgress: TTaskProgressProc
      read FOnProgress write FOnProgress;
    property OnExecute: TTaskProcedureProc
      read FOnExecute write FOnExecute;
  End;


procedure SendTaskEvent( Event: TTaskEvent; Complete: TEventSem);

var
  PrimaryThread: TTaskThread;

{Define components to export}
{You may define a page of the component palette and a component bitmap file}
Exports
  THelperThread,'User','HT.BMP',
  TBasicTask,'User','',
  TStdTask,'User','Task.BMP';

Implementation

uses Messages, SysUtils;

/***
 *
 *    THiddenWindow  - internal class for the HelperThread library
 *
 */

Const
  WM_TaskEvent    = WM_USER + 600; { not sent outside this unit }
  WM_TaskData     = WM_USER + 601; { not sent outside this unit }


type
  THiddenWindow = class(TControl)
  private
    procedure WMTaskEvent( var Msg: TMessage); message WM_TaskEvent;
  public
    procedure CreateWnd; override;
  end;


procedure THiddenWindow.WMTaskEvent;
begin
  TTaskEvent (Msg.Param1).Event;

  if (Msg.Param2<>0) then
    TObject(Msg.Param2) as TEventSem.Post;
  TTaskEvent.Destroy;
end;

procedure THiddenWindow.CreateWnd;
begin
 inherited CreateWnd;
end;

var
  EventsWindow: THiddenWindow;


procedure SendTaskEvent( Event: TTaskEvent; Complete: TEventSem);
begin
  PostMsg (EventsWindow.Handle, WM_TaskEvent, longint(Event), longint(Complete));
  if Complete<>Nil then
  begin
    Complete.WaitFor(INDEFINITE_WAIT);
    Complete.Reset;
  end;
end;


/***
 *
 *    TTaskItem  - class to schedule tasks on the TTaskThread class
 *
 */



function TTaskItem.GetScheduled: boolean;
begin
  GetScheduled := FScheduledOn<>Nil
end;


function TTaskItem.GetRunning: boolean;
begin
  GetRunning := (FScheduledOn<>Nil) and (FScheduledOn.FindTask(Self)=0);
end;


procedure TTaskItem.SetCanceled( C: boolean);
begin
  if C then
  begin
    Cancel;
    FCanceled := C
  end
end;


procedure TTaskItem.SetErrorNr( E: longint);
begin
  FErrorNr := E;
  if E<>0 then Error;
end;


procedure TTaskItem.SetFreeOnFinish( F: boolean);
begin
  if Finished and F then
    Free
  else
    FFreeOnFinish := F;
end;


procedure TTaskItem.Execute;
begin
  Start;
  TaskProcedure;
  if (not Canceled) and (not ErrorNr<>0) then
    Finish;
end;


/***
 *
 *    TTaskThread  - internal class for the HelperThread library
 *
 */




constructor TTaskThread.Create;
begin
  inherited Create( true);

  FTasks := TList.Create;
  FTasksMutex := TMutexSem.Create(false,'',false);
  FTasksChanged := TEventSem.Create(false,'',false);

  FStop := False;

  Suspended := false;
end;

destructor TTaskThread.Destroy;
begin
  if FTasks<>Nil then FTasks.Destroy;
  if FTasksMutex<>Nil then FTasksMutex.Destroy;
  if FTasksChanged<>Nil then FTasksChanged.Destroy;

  inherited Destroy;
end;



procedure TTaskThread.Schedule(Task: TTaskItem);
begin
  FTasksMutex.Request(INDEFINITE_WAIT);
  FTasks.Add(Task);
  FTasksMutex.Release;
  FTasksChanged.Post;
  // AaronL - set the tasks' thread to ourselves.
  Task.FScheduledOn:= self;
end;


procedure TTaskThread.UnSchedule( Task: TTaskItem);
begin
  FTasksMutex.Request(INDEFINITE_WAIT);
  if not Task.Running then
    FTasks.Delete(FindTask( Task))
  else
    raise EThreadingException.Create('Cannot unschedule a running task!');
  FTasksMutex.Release;
end;


FUNCTION TTASKTHREAD.FINDTASK( Task: TTaskItem): LONGINT;
VAR
  I: LONGINT;
BEGIN
  I := 0;
  WHILE (TTaskItem(FTASKS[I]) <> Task) DO
    INC(I);
  IF (TTaskItem(FTASKS[I]) <> Task) THEN
    RESULT := -1
  ELSE
    RESULT := I
END;


procedure TTaskThread.Execute;
begin
  {Place thread code here}
  while (not FStop) do
  begin

    if (FTasks.Count > 0) then // tasks to be run
    begin
      FTasksChanged.Reset;

      ExecuteTask;
      // AaronL - set the task's thread to nil so
      // it can be unscheduled. This seems a bit wrong but it works.
      TTaskItem( FTasks[0] ).FScheduledOn:= nil;
      UnSchedule(FTasks[0]);
    end
    else
      FTasksChanged.WaitFor(INDEFINITE_WAIT);
  end;
end;


procedure TTaskThread.ExecuteTask;
var
  Task: TTaskItem;
begin
  FTasksMutex.Request(INDEFINITE_WAIT);
  Task := FTasks.Items[0];
  FTasksMutex.Release;
  if not Task.Canceled then
    Task.Execute;
end;



procedure TTaskThread.Stop;
begin
  FStop := true;
  FTasksChanged.Post;
end;


/***
 *
 *    THelperThread  -
 *
 *       Component which encapsulates a HelperThread type of thread
 *
 */


function THelperThread.GetThread: TTaskThread;
begin
  Result := FThread;
end;


procedure THelperThread.SetPriority( Prio: TThreadPriority);
begin
  FPriority := Prio;
  if FThread<>Nil then
    FThread.Priority := Prio;
end;


constructor THelperThread.Create;
begin
  inherited Create(Parent);

  FPriority := tpNormal;
end;

Procedure THelperThread.SetupComponent;
Begin
  Inherited SetupComponent;

  Include(ComponentState,csHandleLinks);

  FThread := TTaskThread.Create;
  FThread.Priority := FPriority;
end;


Destructor THelperThread.Destroy;
Begin
  if FThread<>Nil then FThread.Stop;

  Inherited Destroy;
End;


procedure THelperThread.Schedule;
begin
  if FThread<>Nil then
    FThread.Schedule(Task)
  else
    raise EThreadingException.Create(Name+': Helper thread not started!');
end;


/***
 *
 *    TStdTaskXXXXX  -
 *
 *       Events sent by TStdTask
 *
 */


constructor TStdTaskEventProc.Create( T: TStdTaskItem; FOTE: TTaskEventProc );
begin
  inherited Create;

  FOnTaskEvent := FOTE;
  FContext := T;
end;


procedure TStdTaskEventProc.Event;
begin
  FOnTaskEvent(FContext);
end;



constructor TStdTaskProgressProc.Create( T: TStdTaskItem;
                                         FOTP: TTaskProgressProc;
                                         I: longint;
                                         const S: String);
begin
  inherited Create;

  FOnTaskProgress := FOTP;
  FContext := T;
  FI:= I;
  Str := S;
end;

procedure TStdTaskProgressProc.Event;
begin
  FOnTaskProgress(FContext,FI,Str);
end;



/***
 *
 *    TStdTaskItem  -
 *
 *       Component to administer parameters sent from a StdTask
 *
 */


destructor TStdTaskItem.Destroy;
begin
  if Errors<>Nil then Errors.Destroy;
  if Parameters<>Nil then Parameters.Destroy;

  inherited Destroy;
end;


procedure TStdTaskItem.Cancel;
begin
  if FOnCancel<>Nil then
    SendTaskEvent(TStdTaskEventProc.Create(Self,FOnCancel),FEventWaitSem);
end;

procedure TStdTaskItem.Error;
begin
  if FOnError<>Nil then
    SendTaskEvent(TStdTaskEventProc.Create(Self,FOnError),FEventWaitSem);
end;


procedure TStdTaskItem.Finish;
begin
  if FOnFinish<>Nil then
    SendTaskEvent(TStdTaskEventProc.Create(Self,FOnFinish),FEventWaitSem);
end;


procedure TStdTaskItem.Progress( I: longint; const S: String);
var
  ES: TEventSem;
begin
  if FOnProgress<>Nil then
  begin
    if (FEventWait<>All) then
      ES := Nil
    else
      ES := FEventWaitSem;
    SendTaskEvent(TStdTaskProgressProc.Create(Self,FOnProgress,I,S),ES);
  end;
end;

procedure TStdTaskItem.SendStringMessage( const S: string;
                                          EventHandler: TStringEvent );
begin
  FMessageString:= S;
  FStringMessageHandler:= EventHandler;
  FScheduledOn.Synchronize( DoStringMessage );
end;

procedure TStdTaskItem.SendData( Data: TObject;
                                 EventHandler: TDataEvent );
begin
  FData:= Data;
  FDataMessageHandler:= EventHandler;
  FScheduledOn.Synchronize( DoDataMessage );
end;

Procedure TStdTaskItem.DoStringMessage;
begin
  FStringMessageHandler( FMessageString );
end;

Procedure TStdTaskItem.DoDataMessage;
begin
  FDataMessageHandler( FData );
  FData.Free;
  FData:= nil;
end;

procedure TStdTaskItem.Start;
begin
  if FOnStart<>Nil then
    SendTaskEvent(TStdTaskEventProc.Create(Self,FOnStart),FEventWaitSem);
end;


procedure TStdTaskItem.TaskProcedure;
begin
  if FOnExecute<>Nil then
    FOnExecute(Parameters,Self);
end;


/***
 *
 *    TBasicTask  -
 *
 *       Component to schedule tasks on a TTaskThread (using the THelperThread)
 *
 */


Procedure TBasicTask.SetupComponent;
Begin
  Inherited SetupComponent;

  Include(ComponentState,csHandleLinks);
End;


procedure TBasicTask.Notification;
begin
  inherited Notification(AComponent,Operation);

  if (Operation=opRemove) and (AComponent=TComponent(FThread)) then
    FThread := NIL;
end;


procedure TBasicTask.SetThread(N: THelperThread);
begin
  if FThread=N then Exit;
  if FThread<>Nil then FThread.Notification(Self,opRemove);
  fthread := n;
  if N<>Nil then N.FreeNotification(Self);
end;

procedure TBasicTask.Schedule;
var
  T: TTaskItem;
begin
 if FThread=NIL then
  raise EThreadingException.Create('Task: '+Name+' not bound to thread context')
 else
 begin
   T := Nil;
   if T<>Nil then
     Thread.Schedule(T);
 end;
end;



/***
 *
 *    TStdTask  -
 *
 *       Component to schedule procedures on a TTaskThread. Allows the use of the IDE
 *          code generator to generate event-handlers.
 *
 */


CONSTRUCTOR TSTDTASK.CREATE(PARENT: TCOMPONENT);
BEGIN
  INHERITED CREATE(PARENT);

  EVENTWAIT := ALL;
END;

procedure TStdTask.Schedule( Parameters: TObject );
var
  T: TStdTaskItem;
begin
  if FThread=NIL then
    raise EThreadingException.Create('Task: '+Name+' not bound to thread context');

  T := TStdTaskItem.Create;

  T.Parameters:= Parameters;
  T.FOnStart := FOnStart;
  T.FOnFinish := FOnFinish;
  T.FOnCancel := FOnCancel;
  T.FOnError := FOnError;
  T.FOnProgress := FOnProgress;
  T.FOnExecute := FOnExecute;
  T.FEventWait := FEventWait;
  T.FEventWaitSem := FEventWaitSem;

  Thread.Schedule(T);

end;

function TStdTask.GetDefaultCaption: String;
begin
  if FDefaultCaption=Nil then
    Result := ''
  else
    Result := FDefaultCaption^;
end;


procedure TStdTask.SetEventWait( E: TEventWait);
begin
  FEventWait := E;
  if (E=None) and (FEventWaitSem<>Nil) then
  begin
    FEventWaitSem.Destroy;
    FEventWaitSem := Nil;
  end
  else if (E<>None) and (FEventWaitSem=Nil) then
         FEventWaitSem := TEventSem.Create(false,'',false);
end;


procedure TStdTask.SetDefaultCaption( C: String);
begin
  if FDefaultCaption<>Nil then DisposeStr(FDefaultCaption);
  FDefaultCaption := NewStr( C);
end;

{$HINTS OFF}

type
  TPrimaryTaskThread = class(TTaskThread)
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    procedure ExecuteTask; override;
    function FindTask( Task: TTaskItem): longint; override;
    procedure Schedule( Task: TTaskItem); override;
    procedure Stop; override;
    procedure UnSchedule( Task: TTaskItem); override;
  end;

constructor TPrimaryTaskThread.Create;
begin
end;

destructor TPrimaryTaskThread.Destroy;
begin
end;

procedure TPrimaryTaskThread.Execute;
begin
end;

procedure TPrimaryTaskThread.ExecuteTask;
begin
end;

function TPrimaryTaskThread.FindTask( Task: TTaskItem): longint;
begin
  Result := -1;
end;

procedure TPrimaryTaskThread.Schedule;
begin
  Task.Execute;
end;

procedure TPrimaryTaskThread.UnSchedule;
begin
end;

procedure TPrimaryTaskThread.Stop;
begin
end;

{$HINTS ON}

Initialization
  EventsWindow := THiddenWindow.Create(nil);
  EventsWindow.CreateWnd;
  PrimaryThread := TPrimaryTaskThread.Create;

  {Register classes}
  RegisterClasses([THelperThread,TBasicTask,TStdTask]);
End.

