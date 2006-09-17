program Main;

uses
  Classes,
  SysUtils,
  TestAssert,
  CmdLineParameterUnitTests,
  StringUtilsUnitTests,
  HelpTopicTests;


VAR
  tmpSuites : TList;
  tmpTests : TList;
  tmpAllTests : TList;
  tmpFunction : FUNCTION:TList;
  tmpTest : PROCEDURE;
  i,j,tmpTestCount,tmpFailureCount : integer;

BEGIN
  tmpAllTests := TList.Create;
  tmpSuites := TList.Create;

  tmpSuites.Add(@getCmdLineParameterUnitTests);
  tmpSuites.Add(@getStringUtilsUnitTests);
  tmpSuites.Add(@getHelpTopicTests);

  tmpTestCount := 0;
  tmpFailureCount := 0;
  for i := 0 to tmpSuites.Count-1 do
  begin
    tmpFunction := tmpSuites.items[i];
    tmpTests := tmpFunction;
    for j := 0 to tmpTests.Count-1 do
    begin
      tmpTest := tmpTests.items[j];
      try
        tmpTest;
        Write('.');

      except
        on e:Exception do
        begin
          tmpFailureCount := tmpFailureCount + 1;
          writeln;
          writeln(e.message);
        end;
      end;
      tmpTestCount := tmpTestCount + 1;
      if (0 = tmpTestCount MOD 50) then writeln;
    end;
  end;

  writeln;
  write('Running ' + IntToStr(tmpTestCount) + ' tests');
  if (0 < tmpFailureCount) then write(' ' + IntToStr(tmpFailureCount) + ' failures');
  writeln;

END.
