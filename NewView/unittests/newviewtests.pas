program Main;

uses
  Classes,
  SysUtils,
  CmdLineParameterUnit,
  TestAssert,
  CmdLineParameterUnitTests;


VAR
  tmpSuites : TList;
  tmpTests : TList;
  tmpAllTests : TList;
  tmpFunction : FUNCTION:TList;
  tmpTest : PROCEDURE;
  i,j,tmpTestCount : integer;

BEGIN
  tmpAllTests := TList.Create;
  tmpSuites := TList.Create;

  tmpSuites.Add(@getCmdLineParameterUnitTests);

  tmpTestCount := 0;
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
          writeln;
          Writeln(e.message);
        end;
      end;
      tmpTestCount := tmpTestCount + 1;
      if (0 = tmpTestCount MOD 50) then writeln;
    end;
  end;
END.
