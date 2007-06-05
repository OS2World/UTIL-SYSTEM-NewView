program Main;

uses
  Classes,
  SysUtils,
  TestAssert,
  CmdLineParameterUnitTests,
  CharUtilsUnitTests,
  StringUtilsUnitTests,
  FileUtilsUnitTests,
  HelpTopicTests,
  VersionUnit;

IMPORTS
  FUNCTION PmPrintfString(aString:PChar):BYTE; APIENTRY; 'PMPRINTF' NAME 'PmPrintfString';
END;

VAR
  tmpSuites : TList;
  tmpTests : TList;
  tmpAllTests : TList;
  tmpAllExceptions : TStringList;
  tmpFunction : FUNCTION : TList;
  tmpTestNoParam : String;
  tmpTest : PROCEDURE;
  i,j,tmpTestCount,tmpFailureCount,tmpErrorCount,tmpTestNo : integer;
  tmpStartTime, tmpEndTime : TDateTime;

BEGIN
  tmpAllTests := TList.Create;
  tmpSuites := TList.Create;
  tmpAllExceptions := TStringList.Create;

  write('UnitTest for NewView version ' + GetAppVersion);

  tmpTestNoParam := ParamStr(1);
  writeln(tmpTestNoParam);

  tmpSuites.Add(@getCmdLineParameterUnitTests);
  tmpSuites.Add(@getCharUtilsUnitTests);
  tmpSuites.Add(@getStringUtilsUnitTests);
//  tmpSuites.Add(@getHelpTopicTests);
  tmpSuites.Add(@getFileUtilsUnitTests);

  tmpTestNo := -1;
  try
    tmpTestNo := StrToInt(tmpTestNoParam);
    tmpTestNo := tmpTestNo;
    // no parameter or empty
    if 0 = tmpTestNo then tmpTestNo := -1;
  except
  end;

  tmpTestCount := 0;
  tmpFailureCount := 0;
  tmpErrorCount := 0;
  tmpStartTime := now;

  for i := 0 to tmpSuites.Count-1 do
  begin
    tmpFunction := tmpSuites.items[i];
    tmpTests := tmpFunction;

    for j := 0 to tmpTests.Count-1 do
    begin
      tmpTestCount := tmpTestCount + 1;
      tmpTest := tmpTests.items[j];
      if (0 > tmpTestNo)
         OR (tmpTestNo = tmpTestCount)
      then
      begin
        try
          tmpTest;
          Write('.');
        except
          on e:EAssertFailed do
          begin
            tmpFailureCount := tmpFailureCount + 1;
            tmpAllExceptions.add('# ' + IntToStr(tmpTestCount) + ' ' + e.message);
          end;
          on e:Exception do
          begin
            tmpErrorCount := tmpErrorCount + 1;
            tmpAllExceptions.add('# ' + IntToStr(tmpTestCount) + ' ' + e.message);
          end;
        end;
        if (0 = tmpTestCount MOD 50) then writeln;
      end;
    end;
  end;

  tmpEndTime := now;

  writeln;
  for i := 0 to tmpAllExceptions.count-1 do
  begin
    writeln(tmpAllExceptions[i]);
  end;

  if (0 > tmpTestNo) then
    write('Tests run: ' + IntToStr(tmpTestCount))
  else
    write('Test #' + IntToStr(tmpTestNo) + ':');
  write(', Failures: ' + IntToStr(tmpFailureCount));
  write(', Errors: ' + IntToStr(tmpErrorCount));
  write(', Time elapsed: ' + FormatDateTime('hh:nn:ss', tmpEndTime - tmpStartTime));
  writeln;

END.
