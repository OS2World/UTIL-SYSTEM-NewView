program Main;

uses
  Classes,
  CmdLineParameterUnit;


PROCEDURE assertEqualsString(aTestDescription : String; anExpectedValue : String; aRealValue : String);
VAR
   tmpMessage : String;
BEGIN
     if (aRealValue = anExpectedValue) then
     begin
          write('.');
     end
     else
     begin
          tmpMessage := 'Failed: ' + aTestDescription + ' Expected: ''';
          tmpMessage := tmpMessage + anExpectedValue;
          tmpMessage := tmpMessage + ''' but it was: ''';
          tmpMessage := tmpMessage + aRealValue;
          tmpMessage := tmpMessage + '''';
          writeln('');
          writeln(tmpMessage);
     end;
END;

PROCEDURE assertEqualsInt(aTestDescription : String; anExpectedValue : INTEGER; aRealValue : INTEGER);
VAR
   tmpMessage : String;
   tmpIntString : String;
BEGIN
     if (aRealValue = anExpectedValue) then
     begin
          write('.');
     end
     else
     begin
          tmpMessage := 'Failed: ' + aTestDescription + ' Expected: ''';
          Str(anExpectedValue, tmpIntString);
          tmpMessage := tmpMessage + tmpIntString;
          tmpMessage := tmpMessage + ''' but it was: ''';
          Str(aRealValue, tmpIntString);
          tmpMessage := tmpMessage + tmpIntString;
          tmpMessage := tmpMessage + '''';
          writeln('');
          writeln(tmpMessage);
     end;
END;

PROCEDURE assertTrue(aTestDescription : String; aRealValue : Boolean);
VAR
   tmpMessage : String;
BEGIN
     if (aRealValue) then
     begin
          write('.');
     end
     else
     begin
          tmpMessage := 'Failed: ' + aTestDescription + ' Expected: ''True';
          tmpMessage := tmpMessage + ''' but it was: ''False''';
          writeln('');
          writeln(tmpMessage);
     end;
END;


PROCEDURE assertFalse(aTestDescription : String; aRealValue : Boolean);
VAR
   tmpMessage : String;
BEGIN
     if (!aRealValue) then
     begin
          write('.');
     end
     else
     begin
          tmpMessage := 'Failed: ' + aTestDescription + ' Expected: ''False';
          tmpMessage := tmpMessage + ''' but it was: ''True''';
          writeln('');
          writeln(tmpMessage);
     end;
END;


FUNCTION testCmdLineParameterUnit : INTEGER;
VAR
   tmpResult : TStringList;
   tmpRC     : Integer;
   tmpParams : TStringList;
   tmpCmdLineParameters : TCmdLineParameters;
   tmpWindowPosition : TWindowPosition;

BEGIN
     writeln('CmdLineParameterUnit Tests ''' + nativeOS2GetCmdLineParameter + '''');

     tmpResult := splitCmdLineParameter('', tmpRC);
     assertEqualsInt('CmdLine split empy string', 0, tmpRC);
     assertEqualsInt('CmdLine split empy string', 0, tmpResult.Count);

     tmpResult := splitCmdLineParameter('abc', tmpRC);
     assertEqualsInt('CmdLine split single string', 0, tmpRC);
     assertEqualsInt('CmdLine split single string', 1, tmpResult.Count);
     assertEqualsString('CmdLine split single string', 'abc', tmpResult[0]);

     tmpResult := splitCmdLineParameter(' abc', tmpRC);
     assertEqualsInt('CmdLine split single string with leading blank', 0, tmpRC);
     assertEqualsInt('CmdLine split single string with leading blank', 1, tmpResult.Count);
     assertEqualsString('CmdLine split single string with leading blankCmdLine split many strings', 'abc', tmpResult[0]);

     tmpResult := splitCmdLineParameter('abc def ghi', tmpRC);
     assertEqualsInt('CmdLine split empy string', 0, tmpRC);
     assertEqualsInt('CmdLine split many strings', 3, tmpResult.Count);
     assertEqualsString('CmdLine split many strings', 'abc', tmpResult[0]);
     assertEqualsString('CmdLine split many strings', 'def', tmpResult[1]);
     assertEqualsString('CmdLine split many strings', 'ghi', tmpResult[2]);

     tmpResult := splitCmdLineParameter('"abc def"', tmpRC);
     assertEqualsInt('CmdLine split quoted (1)', 0, tmpRC);
     assertEqualsInt('CmdLine split quoted (1)', 1, tmpResult.Count);
     assertEqualsString('CmdLine split quoted (1)', 'abc def', tmpResult[0]);

     tmpResult := splitCmdLineParameter('ab"abc def"', tmpRC);
     assertEqualsInt('CmdLine split quoted (2)', 0, tmpRC);
     assertEqualsInt('CmdLine split quoted (2)', 1, tmpResult.Count);
     assertEqualsString('CmdLine split quoted (2)', 'ababc def', tmpResult[0]);

     tmpResult := splitCmdLineParameter('ab"""abc def"', tmpRC);
     assertEqualsInt('CmdLine split quoted (3)', 0, tmpRC);
     assertEqualsInt('CmdLine split quoted (3)', 1, tmpResult.Count);
     assertEqualsString('CmdLine split quoted (3)', 'ab"abc def', tmpResult[0]);

     tmpResult := splitCmdLineParameter('ab"abc""def"', tmpRC);
     assertEqualsInt('CmdLine split quoted (4)', 0, tmpRC);
     assertEqualsInt('CmdLine split quoted (4)', 1, tmpResult.Count);
     assertEqualsString('CmdLine split quoted (4)', 'ababc"def', tmpResult[0]);

     tmpResult := splitCmdLineParameter('ab"abc""def" "ghi"', tmpRC);
     assertEqualsInt('CmdLine split quoted (5)', 0, tmpRC);
     assertEqualsInt('CmdLine split quoted (5)', 2, tmpResult.Count);
     assertEqualsString('CmdLine split quoted (5)', 'ababc"def', tmpResult[0]);
     assertEqualsString('CmdLine split quoted (5)', 'ghi', tmpResult[1]);

     tmpResult := splitCmdLineParameter('ab"abc""def" "ghi', tmpRC);
     assertEqualsInt('CmdLine split quoted (6)', -1, tmpRC);
     assertEqualsInt('CmdLine split quoted (6)', 2, tmpResult.Count);
     assertEqualsString('CmdLine split quoted (6)', 'ababc"def', tmpResult[0]);
     assertEqualsString('CmdLine split quoted (6)', 'ghi', tmpResult[1]);

     // parser Tests
     tmpParams := TStringList.Create;
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertFalse('parseCmdLine [getShowUsageFlag](1)', tmpCmdLineParameters.getShowUsageFlag);
     assertFalse('parseCmdLine [getSearchFlag](1)', tmpCmdLineParameters.getSearchTextFlag);
     assertEqualsString('parseCmdLine [getSearchText](1)', '', tmpCmdLineParameters.getSearchText);
     assertFalse('parseCmdLine [getGlobalSearchFlag](1)', tmpCmdLineParameters.getGlobalSearchTextFlag);
     assertEqualsString('parseCmdLine [getGlobalSearchText](1)', '', tmpCmdLineParameters.getGlobalSearchText);
     assertEqualsString('parseCmdLine [getLanguage](1)', '', tmpCmdLineParameters.getLanguage);
     assertFalse('parseCmdLine [getHelpManagerFlag](1)', tmpCmdLineParameters.getHelpManagerFlag);
     assertEqualsInt('parseCmdLine [getHelpManagerWindow](1)', 0, tmpCmdLineParameters.getHelpManagerWindow);
     assertEqualsInt('parseCmdLine [getOwnerWindow](1)', 0, tmpCmdLineParameters.getOwnerWindow);
     assertEqualsString('parseCmdLine [getWindowTitle](1)', '', tmpCmdLineParameters.getWindowTitle);

     tmpParams := TStringList.Create;
     tmpParams.add('-?');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertTrue('parseCmdLine [getShowUsageFlag](2)', tmpCmdLineParameters.getShowUsageFlag);
     assertFalse('parseCmdLine [getSearchFlag](2)', tmpCmdLineParameters.getSearchTextFlag);
     assertEqualsString('parseCmdLine [getSearchText](2)', '', tmpCmdLineParameters.getSearchText);
     assertFalse('parseCmdLine [getGlobalSearchFlag](2)', tmpCmdLineParameters.getGlobalSearchTextFlag);
     assertEqualsString('parseCmdLine [getGlobalSearchText](2)', '', tmpCmdLineParameters.getGlobalSearchText);
     assertEqualsString('parseCmdLine [getLanguage](2)', '', tmpCmdLineParameters.getLanguage);
     assertFalse('parseCmdLine [getHelpManagerFlag](2)', tmpCmdLineParameters.getHelpManagerFlag);
     assertEqualsInt('parseCmdLine [getHelpManagerWindow](2)', 0, tmpCmdLineParameters.getHelpManagerWindow);
     assertEqualsInt('parseCmdLine [getOwnerWindow](2)', 0, tmpCmdLineParameters.getOwnerWindow);
     assertEqualsString('parseCmdLine [getWindowTitle](2)', '', tmpCmdLineParameters.getWindowTitle);

     tmpParams := TStringList.Create;
     tmpParams.add('-h');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertTrue('parseCmdLine [getShowUsageFlag](3)', tmpCmdLineParameters.getShowUsageFlag);
     assertFalse('parseCmdLine [getSearchFlag](3)', tmpCmdLineParameters.getSearchTextFlag);
     assertEqualsString('parseCmdLine [getSearchText](3)', '', tmpCmdLineParameters.getSearchText);
     assertFalse('parseCmdLine [getGlobalSearchFlag](3)', tmpCmdLineParameters.getGlobalSearchTextFlag);
     assertEqualsString('parseCmdLine [getGlobalSearchText](3)', '', tmpCmdLineParameters.getGlobalSearchText);
     assertEqualsString('parseCmdLine [getLanguage](3)', '', tmpCmdLineParameters.getLanguage);
     assertFalse('parseCmdLine [getHelpManagerFlag](3)', tmpCmdLineParameters.getHelpManagerFlag);
     assertEqualsInt('parseCmdLine [getHelpManagerWindow](3)', 0, tmpCmdLineParameters.getHelpManagerWindow);
     assertEqualsInt('parseCmdLine [getOwnerWindow](3)', 0, tmpCmdLineParameters.getOwnerWindow);
     assertEqualsString('parseCmdLine [getWindowTitle](3)', '', tmpCmdLineParameters.getWindowTitle);

     tmpParams := TStringList.Create;
     tmpParams.add('-H');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertTrue('parseCmdLine [getShowUsageFlag](4)', tmpCmdLineParameters.getShowUsageFlag);
     assertFalse('parseCmdLine [getSearchFlag](4)', tmpCmdLineParameters.getSearchTextFlag);
     assertEqualsString('parseCmdLine [getSearchText](4)', '', tmpCmdLineParameters.getSearchText);
     assertFalse('parseCmdLine [getGlobalSearchFlag](4)', tmpCmdLineParameters.getGlobalSearchTextFlag);
     assertEqualsString('parseCmdLine [getGlobalSearchText](4)', '', tmpCmdLineParameters.getGlobalSearchText);
     assertEqualsString('parseCmdLine [getLanguage](4)', '', tmpCmdLineParameters.getLanguage);
     assertFalse('parseCmdLine [getHelpManagerFlag](4)', tmpCmdLineParameters.getHelpManagerFlag);
     assertEqualsInt('parseCmdLine [getHelpManagerWindow](4)', 0, tmpCmdLineParameters.getHelpManagerWindow);
     assertEqualsInt('parseCmdLine [getOwnerWindow](4)', 0, tmpCmdLineParameters.getOwnerWindow);
     assertEqualsString('parseCmdLine [getWindowTitle](4)', '', tmpCmdLineParameters.getWindowTitle);

     tmpParams := TStringList.Create;
     tmpParams.add('-HELP');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertTrue('parseCmdLine [getShowUsageFlag](5)', tmpCmdLineParameters.getShowUsageFlag);
     assertFalse('parseCmdLine [getSearchFlag](5)', tmpCmdLineParameters.getSearchTextFlag);
     assertEqualsString('parseCmdLine [getSearchText](5)', '', tmpCmdLineParameters.getSearchText);
     assertFalse('parseCmdLine [getGlobalSearchFlag](5)', tmpCmdLineParameters.getGlobalSearchTextFlag);
     assertEqualsString('parseCmdLine [getGlobalSearchText](5)', '', tmpCmdLineParameters.getGlobalSearchText);
     assertEqualsString('parseCmdLine [getLanguage](5)', '', tmpCmdLineParameters.getLanguage);
     assertFalse('parseCmdLine [getHelpManagerFlag](5)', tmpCmdLineParameters.getHelpManagerFlag);
     assertEqualsInt('parseCmdLine [getHelpManagerWindow](5)', 0, tmpCmdLineParameters.getHelpManagerWindow);
     assertEqualsInt('parseCmdLine [getOwnerWindow](5)', 0, tmpCmdLineParameters.getOwnerWindow);
     assertEqualsString('parseCmdLine [getWindowTitle](5)', '', tmpCmdLineParameters.getWindowTitle);

     tmpParams := TStringList.Create;
     tmpParams.add('-help');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertTrue('parseCmdLine [getShowUsageFlag](6)', tmpCmdLineParameters.getShowUsageFlag);
     assertFalse('parseCmdLine [getSearchFlag](6)', tmpCmdLineParameters.getSearchTextFlag);
     assertEqualsString('parseCmdLine [getSearchText](6)', '', tmpCmdLineParameters.getSearchText);
     assertFalse('parseCmdLine [getGlobalSearchFlag](6)', tmpCmdLineParameters.getGlobalSearchTextFlag);
     assertEqualsString('parseCmdLine [getGlobalSearchText](6)', '', tmpCmdLineParameters.getGlobalSearchText);
     assertEqualsString('parseCmdLine [getLanguage](6)', '', tmpCmdLineParameters.getLanguage);
     assertFalse('parseCmdLine [getHelpManagerFlag](6)', tmpCmdLineParameters.getHelpManagerFlag);
     assertEqualsInt('parseCmdLine [getHelpManagerWindow](6)', 0, tmpCmdLineParameters.getHelpManagerWindow);
     assertEqualsInt('parseCmdLine [getOwnerWindow](6)', 0, tmpCmdLineParameters.getOwnerWindow);
     assertEqualsString('parseCmdLine [getWindowTitle](6)', '', tmpCmdLineParameters.getWindowTitle);

     tmpParams := TStringList.Create;
     tmpParams.add('-s:search');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertFalse('parseCmdLine [getShowUsageFlag](7)', tmpCmdLineParameters.getShowUsageFlag);
     assertTrue('parseCmdLine [getSearchFlag](7)', tmpCmdLineParameters.getSearchTextFlag);
     assertEqualsString('parseCmdLine [getSearchText](7)', 'search', tmpCmdLineParameters.getSearchText);
     assertFalse('parseCmdLine [getGlobalSearchFlag](7)', tmpCmdLineParameters.getGlobalSearchTextFlag);
     assertEqualsString('parseCmdLine [getGlobalSearchText](7)', '', tmpCmdLineParameters.getGlobalSearchText);
     assertEqualsString('parseCmdLine [getLanguage](7)', '', tmpCmdLineParameters.getLanguage);
     assertFalse('parseCmdLine [getHelpManagerFlag](7)', tmpCmdLineParameters.getHelpManagerFlag);
     assertEqualsInt('parseCmdLine [getHelpManagerWindow](7)', 0, tmpCmdLineParameters.getHelpManagerWindow);
     assertEqualsInt('parseCmdLine [getOwnerWindow](7)', 0, tmpCmdLineParameters.getOwnerWindow);
     assertEqualsString('parseCmdLine [getWindowTitle](7)', '', tmpCmdLineParameters.getWindowTitle);

     tmpParams := TStringList.Create;
     tmpParams.add('-S:Search');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertFalse('parseCmdLine [getShowUsageFlag](8)', tmpCmdLineParameters.getShowUsageFlag);
     assertTrue('parseCmdLine [getSearchFlag](8)', tmpCmdLineParameters.getSearchTextFlag);
     assertEqualsString('parseCmdLine [getSearchText](8)', 'Search', tmpCmdLineParameters.getSearchText);
     assertFalse('parseCmdLine [getGlobalSearchFlag](8)', tmpCmdLineParameters.getGlobalSearchTextFlag);
     assertEqualsString('parseCmdLine [getGlobalSearchText](8)', '', tmpCmdLineParameters.getGlobalSearchText);
     assertEqualsString('parseCmdLine [getLanguage](8)', '', tmpCmdLineParameters.getLanguage);
     assertFalse('parseCmdLine [getHelpManagerFlag](8)', tmpCmdLineParameters.getHelpManagerFlag);
     assertEqualsInt('parseCmdLine [getHelpManagerWindow](8)', 0, tmpCmdLineParameters.getHelpManagerWindow);
     assertEqualsInt('parseCmdLine [getOwnerWindow](8)', 0, tmpCmdLineParameters.getOwnerWindow);
     assertEqualsString('parseCmdLine [getWindowTitle](8)', '', tmpCmdLineParameters.getWindowTitle);

     tmpParams := TStringList.Create;
     tmpParams.add('-S');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertFalse('parseCmdLine [getShowUsageFlag](9)', tmpCmdLineParameters.getShowUsageFlag);
     assertTrue('parseCmdLine [getSearchFlag](9)', tmpCmdLineParameters.getSearchTextFlag);
     assertEqualsString('parseCmdLine [getSearchText](9)', '', tmpCmdLineParameters.getSearchText);
     assertFalse('parseCmdLine [getGlobalSearchFlag](9)', tmpCmdLineParameters.getGlobalSearchTextFlag);
     assertEqualsString('parseCmdLine [getGlobalSearchText](9)', '', tmpCmdLineParameters.getGlobalSearchText);
     assertEqualsString('parseCmdLine [getLanguage](9)', '', tmpCmdLineParameters.getLanguage);
     assertFalse('parseCmdLine [getHelpManagerFlag](9)', tmpCmdLineParameters.getHelpManagerFlag);
     assertEqualsInt('parseCmdLine [getHelpManagerWindow](9)', 0, tmpCmdLineParameters.getHelpManagerWindow);
     assertEqualsInt('parseCmdLine [getOwnerWindow](9)', 0, tmpCmdLineParameters.getOwnerWindow);
     assertEqualsString('parseCmdLine [getWindowTitle](9)', '', tmpCmdLineParameters.getWindowTitle);

     tmpParams := TStringList.Create;
     tmpParams.add('-S:');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertFalse('parseCmdLine [getShowUsageFlag](10)', tmpCmdLineParameters.getShowUsageFlag);
     assertTrue('parseCmdLine [getSearchFlag](10)', tmpCmdLineParameters.getSearchTextFlag);
     assertEqualsString('parseCmdLine [getSearchText](10)', '', tmpCmdLineParameters.getSearchText);
     assertFalse('parseCmdLine [getGlobalSearchFlag](10)', tmpCmdLineParameters.getGlobalSearchTextFlag);
     assertEqualsString('parseCmdLine [getGlobalSearchText](10)', '', tmpCmdLineParameters.getGlobalSearchText);
     assertEqualsString('parseCmdLine [getLanguage](10)', '', tmpCmdLineParameters.getLanguage);
     assertFalse('parseCmdLine [getHelpManagerFlag](10)', tmpCmdLineParameters.getHelpManagerFlag);
     assertEqualsInt('parseCmdLine [getHelpManagerWindow](10)', 0, tmpCmdLineParameters.getHelpManagerWindow);
     assertEqualsInt('parseCmdLine [getOwnerWindow](10)', 0, tmpCmdLineParameters.getOwnerWindow);
     assertEqualsString('parseCmdLine [getWindowTitle](10)', '', tmpCmdLineParameters.getWindowTitle);

     tmpParams := TStringList.Create;
     tmpParams.add('-sSeaRch');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertFalse('parseCmdLine [getShowUsageFlag](11)', tmpCmdLineParameters.getShowUsageFlag);
     assertTrue('parseCmdLine [getSearchFlag](11)', tmpCmdLineParameters.getSearchTextFlag);
     assertEqualsString('parseCmdLine [getSearchText](11)', 'SeaRch', tmpCmdLineParameters.getSearchText);
     assertFalse('parseCmdLine [getGlobalSearchFlag](11)', tmpCmdLineParameters.getGlobalSearchTextFlag);
     assertEqualsString('parseCmdLine [getGlobalSearchText](11)', '', tmpCmdLineParameters.getGlobalSearchText);
     assertEqualsString('parseCmdLine [getLanguage](11)', '', tmpCmdLineParameters.getLanguage);
     assertFalse('parseCmdLine [getHelpManagerFlag](11)', tmpCmdLineParameters.getHelpManagerFlag);
     assertEqualsInt('parseCmdLine [getHelpManagerWindow](11)', 0, tmpCmdLineParameters.getHelpManagerWindow);
     assertEqualsInt('parseCmdLine [getOwnerWindow](11)', 0, tmpCmdLineParameters.getOwnerWindow);
     assertEqualsString('parseCmdLine [getWindowTitle](11)', '', tmpCmdLineParameters.getWindowTitle);

     tmpParams := TStringList.Create;
     tmpParams.add('-s::Sea rch');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertFalse('parseCmdLine [getShowUsageFlag](12)', tmpCmdLineParameters.getShowUsageFlag);
     assertTrue('parseCmdLine [getSearchFlag](12)', tmpCmdLineParameters.getSearchTextFlag);
     assertEqualsString('parseCmdLine [getSearchText](12)', ':Sea rch', tmpCmdLineParameters.getSearchText);
     assertFalse('parseCmdLine [getGlobalSearchFlag](12)', tmpCmdLineParameters.getGlobalSearchTextFlag);
     assertEqualsString('parseCmdLine [getGlobalSearchText](12)', '', tmpCmdLineParameters.getGlobalSearchText);
     assertEqualsString('parseCmdLine [getLanguage](12)', '', tmpCmdLineParameters.getLanguage);
     assertFalse('parseCmdLine [getHelpManagerFlag](12)', tmpCmdLineParameters.getHelpManagerFlag);
     assertEqualsInt('parseCmdLine [getHelpManagerWindow](12)', 0, tmpCmdLineParameters.getHelpManagerWindow);
     assertEqualsInt('parseCmdLine [getOwnerWindow](12)', 0, tmpCmdLineParameters.getOwnerWindow);
     assertEqualsString('parseCmdLine [getWindowTitle](12)', '', tmpCmdLineParameters.getWindowTitle);

     tmpParams := TStringList.Create;
     tmpParams.add('-g:globalsearch');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertFalse('parseCmdLine [getShowUsageFlag](13)', tmpCmdLineParameters.getShowUsageFlag);
     assertFalse('parseCmdLine [getSearchFlag](13)', tmpCmdLineParameters.getSearchTextFlag);
     assertEqualsString('parseCmdLine [getSearchText](13)', '', tmpCmdLineParameters.getSearchText);
     assertTrue('parseCmdLine [getGlobalSearchFlag](13)', tmpCmdLineParameters.getGlobalSearchTextFlag);
     assertEqualsString('parseCmdLine [getGlobalSearchText](13)', 'globalsearch', tmpCmdLineParameters.getGlobalSearchText);
     assertEqualsString('parseCmdLine [getLanguage](13)', '', tmpCmdLineParameters.getLanguage);
     assertFalse('parseCmdLine [getHelpManagerFlag](13)', tmpCmdLineParameters.getHelpManagerFlag);
     assertEqualsInt('parseCmdLine [getHelpManagerWindow](13)', 0, tmpCmdLineParameters.getHelpManagerWindow);
     assertEqualsInt('parseCmdLine [getOwnerWindow](13)', 0, tmpCmdLineParameters.getOwnerWindow);
     assertEqualsString('parseCmdLine [getWindowTitle](13)', '', tmpCmdLineParameters.getWindowTitle);

     tmpParams := TStringList.Create;
     tmpParams.add('-G:globalSearch');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertFalse('parseCmdLine [getShowUsageFlag](14)', tmpCmdLineParameters.getShowUsageFlag);
     assertFalse('parseCmdLine [getSearchFlag](14)', tmpCmdLineParameters.getSearchTextFlag);
     assertEqualsString('parseCmdLine [getSearchText](14)', '', tmpCmdLineParameters.getSearchText);
     assertTrue('parseCmdLine [getGlobalSearchFlag](14)', tmpCmdLineParameters.getGlobalSearchTextFlag);
     assertEqualsString('parseCmdLine [getGlobalSearchText](14)', 'globalSearch', tmpCmdLineParameters.getGlobalSearchText);
     assertEqualsString('parseCmdLine [getLanguage](14)', '', tmpCmdLineParameters.getLanguage);
     assertFalse('parseCmdLine [getHelpManagerFlag](14)', tmpCmdLineParameters.getHelpManagerFlag);
     assertEqualsInt('parseCmdLine [getHelpManagerWindow](14)', 0, tmpCmdLineParameters.getHelpManagerWindow);
     assertEqualsInt('parseCmdLine [getOwnerWindow](14)', 0, tmpCmdLineParameters.getOwnerWindow);
     assertEqualsString('parseCmdLine [getWindowTitle](14)', '', tmpCmdLineParameters.getWindowTitle);

     tmpParams := TStringList.Create;
     tmpParams.add('-gGlobalSearch');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertFalse('parseCmdLine [getShowUsageFlag](15)', tmpCmdLineParameters.getShowUsageFlag);
     assertFalse('parseCmdLine [getSearchFlag](15)', tmpCmdLineParameters.getSearchTextFlag);
     assertEqualsString('parseCmdLine [getSearchText](15)', '', tmpCmdLineParameters.getSearchText);
     assertTrue('parseCmdLine [getGlobalSearchFlag](15)', tmpCmdLineParameters.getGlobalSearchTextFlag);
     assertEqualsString('parseCmdLine [getGlobalSearchText](15)', 'GlobalSearch', tmpCmdLineParameters.getGlobalSearchText);
     assertEqualsString('parseCmdLine [getLanguage](15)', '', tmpCmdLineParameters.getLanguage);
     assertFalse('parseCmdLine [getHelpManagerFlag](15)', tmpCmdLineParameters.getHelpManagerFlag);
     assertEqualsInt('parseCmdLine [getHelpManagerWindow](15)', 0, tmpCmdLineParameters.getHelpManagerWindow);
     assertEqualsInt('parseCmdLine [getOwnerWindow](15)', 0, tmpCmdLineParameters.getOwnerWindow);
     assertEqualsString('parseCmdLine [getWindowTitle](15)', '', tmpCmdLineParameters.getWindowTitle);

     tmpParams := TStringList.Create;
     tmpParams.add('-g::Global Search');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertFalse('parseCmdLine [getShowUsageFlag](16)', tmpCmdLineParameters.getShowUsageFlag);
     assertFalse('parseCmdLine [getSearchFlag](16)', tmpCmdLineParameters.getSearchTextFlag);
     assertEqualsString('parseCmdLine [getSearchText](16)', '', tmpCmdLineParameters.getSearchText);
     assertTrue('parseCmdLine [getGlobalSearchFlag](16)', tmpCmdLineParameters.getGlobalSearchTextFlag);
     assertEqualsString('parseCmdLine [getGlobalSearchText](16)', ':Global Search', tmpCmdLineParameters.getGlobalSearchText);
     assertEqualsString('parseCmdLine [getLanguage](16)', '', tmpCmdLineParameters.getLanguage);
     assertFalse('parseCmdLine [getHelpManagerFlag](16)', tmpCmdLineParameters.getHelpManagerFlag);
     assertEqualsInt('parseCmdLine [getHelpManagerWindow](16)', 0, tmpCmdLineParameters.getHelpManagerWindow);
     assertEqualsInt('parseCmdLine [getOwnerWindow](16)', 0, tmpCmdLineParameters.getOwnerWindow);
     assertEqualsString('parseCmdLine [getWindowTitle](16)', '', tmpCmdLineParameters.getWindowTitle);

     tmpParams := TStringList.Create;
     tmpParams.add('-lang:DE');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertFalse('parseCmdLine [getShowUsageFlag](17)', tmpCmdLineParameters.getShowUsageFlag);
     assertFalse('parseCmdLine [getSearchFlag](17)', tmpCmdLineParameters.getSearchTextFlag);
     assertEqualsString('parseCmdLine [getSearchText](17)', '', tmpCmdLineParameters.getSearchText);
     assertFalse('parseCmdLine [getGlobalSearchFlag](17)', tmpCmdLineParameters.getGlobalSearchTextFlag);
     assertEqualsString('parseCmdLine [getGlobalSearchText](17)', '', tmpCmdLineParameters.getGlobalSearchText);
     assertEqualsString('parseCmdLine [getLanguage](17)', 'DE', tmpCmdLineParameters.getLanguage);
     assertFalse('parseCmdLine [getHelpManagerFlag](17)', tmpCmdLineParameters.getHelpManagerFlag);
     assertEqualsInt('parseCmdLine [getHelpManagerWindow](17)', 0, tmpCmdLineParameters.getHelpManagerWindow);
     assertEqualsInt('parseCmdLine [getOwnerWindow](17)', 0, tmpCmdLineParameters.getOwnerWindow);
     assertEqualsString('parseCmdLine [getWindowTitle](17)', '', tmpCmdLineParameters.getWindowTitle);

     tmpParams := TStringList.Create;
     tmpParams.add('-Hm:123');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertFalse('parseCmdLine [getShowUsageFlag](18)', tmpCmdLineParameters.getShowUsageFlag);
     assertFalse('parseCmdLine [getSearchFlag](18)', tmpCmdLineParameters.getSearchTextFlag);
     assertEqualsString('parseCmdLine [getSearchText](18)', '', tmpCmdLineParameters.getSearchText);
     assertFalse('parseCmdLine [getGlobalSearchFlag](18)', tmpCmdLineParameters.getGlobalSearchTextFlag);
     assertEqualsString('parseCmdLine [getGlobalSearchText](18)', '', tmpCmdLineParameters.getGlobalSearchText);
     assertEqualsString('parseCmdLine [getLanguage](18)', '', tmpCmdLineParameters.getLanguage);
     assertTrue('parseCmdLine [getHelpManagerFlag](18)', tmpCmdLineParameters.getHelpManagerFlag);
     assertEqualsInt('parseCmdLine [getHelpManagerWindow](18)', 123, tmpCmdLineParameters.getHelpManagerWindow);
     assertEqualsInt('parseCmdLine [getOwnerWindow](18)', 0, tmpCmdLineParameters.getOwnerWindow);
     assertEqualsString('parseCmdLine [getWindowTitle](18)', '', tmpCmdLineParameters.getWindowTitle);

     tmpParams := TStringList.Create;
     tmpParams.add('-HM:1zwei3');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertFalse('parseCmdLine [getShowUsageFlag](19)', tmpCmdLineParameters.getShowUsageFlag);
     assertFalse('parseCmdLine [getSearchFlag](19)', tmpCmdLineParameters.getSearchTextFlag);
     assertEqualsString('parseCmdLine [getSearchText](19)', '', tmpCmdLineParameters.getSearchText);
     assertFalse('parseCmdLine [getGlobalSearchFlag](19)', tmpCmdLineParameters.getGlobalSearchTextFlag);
     assertEqualsString('parseCmdLine [getGlobalSearchText](19)', '', tmpCmdLineParameters.getGlobalSearchText);
     assertEqualsString('parseCmdLine [getLanguage](19)', '', tmpCmdLineParameters.getLanguage);
     assertFalse('parseCmdLine [getHelpManagerFlag](19)', tmpCmdLineParameters.getHelpManagerFlag);
     assertEqualsInt('parseCmdLine [getHelpManagerWindow](19)', 0, tmpCmdLineParameters.getHelpManagerWindow);
     assertEqualsInt('parseCmdLine [getOwnerWindow](19)', 0, tmpCmdLineParameters.getOwnerWindow);
     assertEqualsString('parseCmdLine [getWindowTitle](19)', '', tmpCmdLineParameters.getWindowTitle);

     tmpParams := TStringList.Create;
     tmpParams.add('-oWner:123');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertFalse('parseCmdLine [getShowUsageFlag](20)', tmpCmdLineParameters.getShowUsageFlag);
     assertFalse('parseCmdLine [getSearchFlag](20)', tmpCmdLineParameters.getSearchTextFlag);
     assertEqualsString('parseCmdLine [getSearchText](20)', '', tmpCmdLineParameters.getSearchText);
     assertFalse('parseCmdLine [getGlobalSearchFlag](20)', tmpCmdLineParameters.getGlobalSearchTextFlag);
     assertEqualsString('parseCmdLine [getGlobalSearchText](20)', '', tmpCmdLineParameters.getGlobalSearchText);
     assertEqualsString('parseCmdLine [getLanguage](20)', '', tmpCmdLineParameters.getLanguage);
     assertFalse('parseCmdLine [getHelpManagerFlag](20)', tmpCmdLineParameters.getHelpManagerFlag);
     assertEqualsInt('parseCmdLine [getHelpManagerWindow](20)', 0, tmpCmdLineParameters.getHelpManagerWindow);
     assertEqualsInt('parseCmdLine [getOwnerWindow](20)', 123, tmpCmdLineParameters.getOwnerWindow);
     assertEqualsString('parseCmdLine [getWindowTitle](20)', '', tmpCmdLineParameters.getWindowTitle);

     tmpParams := TStringList.Create;
     tmpParams.add('-owner:1zwei3');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertFalse('parseCmdLine [getShowUsageFlag](21)', tmpCmdLineParameters.getShowUsageFlag);
     assertFalse('parseCmdLine [getSearchFlag](21)', tmpCmdLineParameters.getSearchTextFlag);
     assertEqualsString('parseCmdLine [getSearchText](21)', '', tmpCmdLineParameters.getSearchText);
     assertFalse('parseCmdLine [getGlobalSearchFlag](21)', tmpCmdLineParameters.getGlobalSearchTextFlag);
     assertEqualsString('parseCmdLine [getGlobalSearchText](21)', '', tmpCmdLineParameters.getGlobalSearchText);
     assertEqualsString('parseCmdLine [getLanguage](21)', '', tmpCmdLineParameters.getLanguage);
     assertFalse('parseCmdLine [getHelpManagerFlag](21)', tmpCmdLineParameters.getHelpManagerFlag);
     assertEqualsInt('parseCmdLine [getHelpManagerWindow](21)', 0, tmpCmdLineParameters.getHelpManagerWindow);
     assertEqualsInt('parseCmdLine [getOwnerWindow](21)', 0, tmpCmdLineParameters.getOwnerWindow);
     assertEqualsString('parseCmdLine [getWindowTitle](21)', '', tmpCmdLineParameters.getWindowTitle);

     tmpParams := TStringList.Create;
     tmpParams.add('-tiTle:testTitle');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertFalse('parseCmdLine [getShowUsageFlag](22)', tmpCmdLineParameters.getShowUsageFlag);
     assertFalse('parseCmdLine [getSearchFlag](22)', tmpCmdLineParameters.getSearchTextFlag);
     assertEqualsString('parseCmdLine [getSearchText](22)', '', tmpCmdLineParameters.getSearchText);
     assertFalse('parseCmdLine [getGlobalSearchFlag](22)', tmpCmdLineParameters.getGlobalSearchTextFlag);
     assertEqualsString('parseCmdLine [getGlobalSearchText](22)', '', tmpCmdLineParameters.getGlobalSearchText);
     assertEqualsString('parseCmdLine [getLanguage](22)', '', tmpCmdLineParameters.getLanguage);
     assertFalse('parseCmdLine [getHelpManagerFlag](22)', tmpCmdLineParameters.getHelpManagerFlag);
     assertEqualsInt('parseCmdLine [getHelpManagerWindow](22)', 0, tmpCmdLineParameters.getHelpManagerWindow);
     assertEqualsInt('parseCmdLine [getOwnerWindow](22)', 0, tmpCmdLineParameters.getOwnerWindow);
     assertEqualsString('parseCmdLine [getWindowTitle](22)', 'testTitle', tmpCmdLineParameters.getWindowTitle);

     // can't test start profile


     tmpParams := TStringList.Create;
     tmpParams.add('-pos:20,40,60,80');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertTrue('parseCmdLine [getWindowPositionFlag](40)', tmpCmdLineParameters.getWindowPositionFlag);
     tmpWindowPosition := tmpCmdLineParameters.getWindowPosition;
     assertEqualsInt('parseCmdLine [getWindowPosition/left](40)', 20, tmpWindowPosition.left);
     assertEqualsInt('parseCmdLine [getWindowPosition/bottom](40)', 40, tmpWindowPosition.bottom);
     assertEqualsInt('parseCmdLine [getWindowPosition/width](40)', 60, tmpWindowPosition.width);
     assertEqualsInt('parseCmdLine [getWindowPosition/height](40)', 80, tmpWindowPosition.height);

     tmpParams := TStringList.Create;
     tmpParams.add('-pos:20p,40p,60p,70p');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertTrue('parseCmdLine [getWindowPositionFlag](41)', tmpCmdLineParameters.getWindowPositionFlag);
     tmpWindowPosition := tmpCmdLineParameters.getWindowPosition;
     assertEqualsInt('parseCmdLine [getWindowPosition/left](41)', 205, tmpWindowPosition.left);
     assertEqualsInt('parseCmdLine [getWindowPosition/bottom](41)', 307, tmpWindowPosition.bottom);
     assertEqualsInt('parseCmdLine [getWindowPosition/width](41)', 614, tmpWindowPosition.width);
     assertEqualsInt('parseCmdLine [getWindowPosition/height](41)', 538, tmpWindowPosition.height);

     tmpParams := TStringList.Create;
     tmpParams.add('ab c');
     tmpParams.add('topi1');
     tmpParams.add('topi2');
     tmpCmdLineParameters := TCmdLineParameters.Create;
     tmpCmdLineParameters.parseCmdLine(tmpParams);
     assertEqualsString('parseCmdLine [getFileName](70)', 'ab c', tmpCmdLineParameters.getFileName);
     assertEqualsString('parseCmdLine [getTopic](70)', 'topi1 topi2', tmpCmdLineParameters.getTopic);
END;




BEGIN
     writeln('NewView Tests');
     testCmdLineParameterUnit;
END.
