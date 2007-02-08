Unit DebugUnit;

// NewView - a new OS/2 Help Viewer
// Copyright 2006 Ronald Brill (rbri at rbri dot de)
// This software is released under the GNU Public License - see readme.txt

// Helper functions for debugging

Interface

{$define DEBUG}


uses
  Classes,
  OS2Def,
  PMWin,
  SysUtils;

{$ifdef DEBUG}
imports
  Function PmPrintfString(aString:PChar):BYTE; APIENTRY; 'PMPRINTF' NAME 'PmPrintfString';
end;
{$endif}

  // -- Logging --
  Type
    LogAspect = (       LogStartup,
                        LogShutdown,
                        LogSettings,
                        LogParse,
                        LogDisplay,
                        LogSearch,
                        LogNHM,
                        LogViewStub,
                        LogObjConstDest,
                        LogDebug
    );
    LogAspects = SET OF LogAspect;

  Procedure LogEvent(const aLogAspect: LogAspect; const anEventDescription: String);


  // -- Profiling --

  // Starts the timer
  Procedure PrfStartTimer;

  // Stops the timer
  Procedure PrfStopTimer;

  // Writes the eventDesctiption together with
  // the time since timer start to PMPrintF
  // Procedure PrfTraceEvent(const anEventDescription: String);

const
  activeLogAspects : LogAspects = [
//                                        LogStartup,
//                                        LogShutdown,
//                                        LogSettings,
//                                        LogParse,
//                                        LogDisplay,
//                                        LogSearch,
                                          LogNHM,
//                                        LogViewStub,
//                                        LogObjConstDest,
                                          LogDebug
                                  ];

var
  startTime : ULONG;
  lastTime : ULONG;

Implementation

  Function GetAspectPrefix(const aLogAspect: LogAspect): String;
  Begin
    Case aLogAspect of
      LogStartup      :  result := 'Startup';
      LogShutdown     :  result := 'Start';
      LogSettings     :  result := 'Settings';
      LogParse        :  result := 'Parse';
      LogDisplay      :  result := 'Display';
      LogSearch       :  result := 'Search';
      LogNHM          :  result := 'NewHelpManager';
      LogViewStub     :  result := 'ViewStub';
      LogObjConstDest :  result := 'ObjConstDest';
      LogDebug        :  result := 'Debug';
      else               result := 'Unknown';
      end;
  End;


  Procedure LogEvent(const aLogAspect: LogAspect; const anEventDescription: String);
{$ifdef DEBUG}
  Var
    tmpMessage : String;
    tmpPCharMessage : PChar;
{$endif}
  Begin
{$ifdef DEBUG}
    if (aLogAspect IN activeLogAspects) then
    begin
      tmpMessage := 'Log[' + GetAspectPrefix(aLogAspect) + ']  ' + anEventDescription;

      tmpPCharMessage := StrAlloc(length(tmpMessage) + 1);
      StrPCopy(tmpPCharMessage, tmpMessage);

      PmPrintfString(tmpPCharMessage);
      StrDispose(tmpPCharMessage);
    end;
{$endif}
  end;




  Function GetSystemMSCount: ULONG;
  Begin
    result:= WinGetCurrentTime(AppHandle);
  End;


  Procedure PrfStartTimer;
  Begin
{$ifdef DEBUG}
    startTime := GetSystemMSCount;
    lastTime := startTime;
{$endif}
  End;

  Procedure PrfStopTimer;
  Begin
  End;


  Procedure PrfTraceEvent(const anEventDescription: String);
{$ifdef DEBUG}
  Var
    tmpTime : ULONG;
    tmpMessage : String;
    tmpPCharMessage : PChar;
{$endif}
  Begin
{$ifdef DEBUG}
    tmpTime := GetSystemMSCount;
    tmpMessage := 'Prf: ' + IntToStr(tmpTime - lastTime) + 'ms ' + anEventDescription + IntToStr(tmpTime - startTime) + 'ms since start';

    tmpPCharMessage := StrAlloc(length(tmpMessage) + 1);
    StrPCopy(tmpPCharMessage, tmpMessage);

    PmPrintfString(tmpPCharMessage);
    StrDispose(tmpPCharMessage);

    lastTime := GetSystemMSCount;
{$endif}
  end;
END.
