Unit CmdLineParameterUnitTests;

Interface

uses
  Classes,
  TestAssert,
  CmdLineParameterUnit;

FUNCTION getCmdLineParameterUnitTests : TList;
PROCEDURE testSplitCmdLineParameter_Empty;
PROCEDURE testSplitCmdLineParameter_simpleOne;
PROCEDURE testSplitCmdLineParameter_simpleOneWithLeadingBlanks;
PROCEDURE testSplitCmdLineParameter_simpleThreeParts;
PROCEDURE testSplitCmdLineParameter_quoted;
PROCEDURE testSplitCmdLineParameter_quotedPart;
PROCEDURE testSplitCmdLineParameter_quotedPartIncludingQuoteAtStart;
PROCEDURE testSplitCmdLineParameter_quotedPartIncludingQuoteInside;
PROCEDURE testSplitCmdLineParameter_quotedPartIncludingQuoteAtEnd;
PROCEDURE testSplitCmdLineParameter_TwoQuotedParts;
PROCEDURE testSplitCmdLineParameter_TwoQuotesAtStartEnd;
PROCEDURE testSplitCmdLineParameter_TwoQuotedPartsMissingClosedQuote;

PROCEDURE testParseCmdLine_Empty;

PROCEDURE testParseCmdLine_QuestionMark;
PROCEDURE testParseCmdLine_lowerH;
PROCEDURE testParseCmdLine_upperH;
PROCEDURE testParseCmdLine_lowerHelp;
PROCEDURE testParseCmdLine_upperHELP;

PROCEDURE testParseCmdLine_lowerS;
PROCEDURE testParseCmdLine_upperS;
PROCEDURE testParseCmdLine_emptyS;
PROCEDURE testParseCmdLine_emptyColonS;
PROCEDURE testParseCmdLine_withoutColonS;
PROCEDURE testParseCmdLine_doubleColonS;

PROCEDURE testParseCmdLine_lowerG;
PROCEDURE testParseCmdLine_upperG;
PROCEDURE testParseCmdLine_emptyG;
PROCEDURE testParseCmdLine_emptyColonG;
PROCEDURE testParseCmdLine_withoutColonG;
PROCEDURE testParseCmdLine_doubleColonG;

PROCEDURE testParseCmdLine_Language;
PROCEDURE testParseCmdLine_HelpManagerNumber;
PROCEDURE testParseCmdLine_HelpManagerText;
PROCEDURE testParseCmdLine_OwnerNumber;
PROCEDURE testParseCmdLine_OwnerTest;
PROCEDURE testParseCmdLine_Title;
PROCEDURE testParseCmdLine_WindowPos;
PROCEDURE testParseCmdLine_WindowPosPercentage;
PROCEDURE testParseCmdLine_Topic;

Implementation

  PROCEDURE testSplitCmdLineParameter_Empty;
  VAR
    tmpResult : TStringList;
    tmpRC : Integer;
  BEGIN
    tmpResult := TStringList.Create;
    tmpRC := splitCmdLineParameter('', tmpResult);

    assertEqualsInt('testSplitCmdLineParameter_Empty', 0, tmpRC);
    assertEqualsInt('testSplitCmdLineParameter_Empty', 0, tmpResult.Count);

    tmpResult.Destroy;
  END;


  PROCEDURE testSplitCmdLineParameter_simpleOne;
  VAR
    tmpResult : TStringList;
    tmpRC : Integer;
  BEGIN
    tmpResult := TStringList.Create;
    tmpRC := splitCmdLineParameter('abc', tmpResult);

    assertEqualsInt('testSplitCmdLineParameter_simpleOne', 0, tmpRC);
    assertEqualsInt('testSplitCmdLineParameter_simpleOne', 1, tmpResult.Count);
    assertEqualsString('testSplitCmdLineParameter_simpleOne', 'abc', tmpResult[0]);

    tmpResult.Destroy;
  END;


  PROCEDURE testSplitCmdLineParameter_simpleOneWithLeadingBlanks;
  VAR
    tmpResult : TStringList;
    tmpRC : Integer;
  BEGIN
    tmpResult := TStringList.Create;
    tmpRC := splitCmdLineParameter(' abc', tmpResult);

    assertEqualsInt('testSplitCmdLineParameter_simpleOneWithLeadingBlanks', 0, tmpRC);
    assertEqualsInt('testSplitCmdLineParameter_simpleOneWithLeadingBlanks', 1, tmpResult.Count);
    assertEqualsString('testSplitCmdLineParameter_simpleOneWithLeadingBlanks', 'abc', tmpResult[0]);

    tmpResult.Destroy;
  END;


  PROCEDURE testSplitCmdLineParameter_simpleThreeParts;
  VAR
    tmpResult : TStringList;
    tmpRC : Integer;
  BEGIN
    tmpResult := TStringList.Create;
    tmpRC := splitCmdLineParameter('abc def ghi', tmpResult);

    assertEqualsInt('testSplitCmdLineParameter_simpleThreeParts', 0, tmpRC);
    assertEqualsInt('testSplitCmdLineParameter_simpleThreeParts', 3, tmpResult.Count);
    assertEqualsString('testSplitCmdLineParameter_simpleThreeParts', 'abc', tmpResult[0]);
    assertEqualsString('testSplitCmdLineParameter_simpleThreeParts', 'def', tmpResult[1]);
    assertEqualsString('testSplitCmdLineParameter_simpleThreeParts', 'ghi', tmpResult[2]);

    tmpResult.Destroy;
  END;


  PROCEDURE testSplitCmdLineParameter_quoted;
  VAR
    tmpResult : TStringList;
    tmpRC : Integer;
  BEGIN
    tmpResult := TStringList.Create;
    tmpRC := splitCmdLineParameter('"abc def"', tmpResult);

    assertEqualsInt('testSplitCmdLineParameter_quoted', 0, tmpRC);
    assertEqualsInt('testSplitCmdLineParameter_quoted', 1, tmpResult.Count);
    assertEqualsString('testSplitCmdLineParameter_quoted', 'abc def', tmpResult[0]);

    tmpResult.Destroy;
  END;


  PROCEDURE testSplitCmdLineParameter_quotedPart;
  VAR
    tmpResult : TStringList;
    tmpRC : Integer;
  BEGIN
    tmpResult := TStringList.Create;
    tmpRC := splitCmdLineParameter('ab"abc def"', tmpResult);

    assertEqualsInt('testSplitCmdLineParameter_quotedPart', 0, tmpRC);
    assertEqualsInt('testSplitCmdLineParameter_quotedPart', 1, tmpResult.Count);
    assertEqualsString('testSplitCmdLineParameter_quotedPart', 'ababc def', tmpResult[0]);

    tmpResult.Destroy;
  END;


  PROCEDURE testSplitCmdLineParameter_quotedPartIncludingQuoteAtStart;
  VAR
    tmpResult : TStringList;
    tmpRC : Integer;
  BEGIN
    tmpResult := TStringList.Create;
    tmpRC := splitCmdLineParameter('ab"""abc def"', tmpResult);

    assertEqualsInt('testSplitCmdLineParameter_quotedPartIncludingQuoteAtStart', 0, tmpRC);
    assertEqualsInt('testSplitCmdLineParameter_quotedPartIncludingQuoteAtStart', 1, tmpResult.Count);
    assertEqualsString('testSplitCmdLineParameter_quotedPartIncludingQuoteAtStart', 'ab"abc def', tmpResult[0]);

    tmpResult.Destroy;
  END;


  PROCEDURE testSplitCmdLineParameter_quotedPartIncludingQuoteInside;
  VAR
    tmpResult : TStringList;
    tmpRC : Integer;
  BEGIN
    tmpResult := TStringList.Create;
    tmpRC := splitCmdLineParameter('ab"ab c""d ef"', tmpResult);

    assertEqualsInt('testSplitCmdLineParameter_quotedPartIncludingQuoteInside', 0, tmpRC);
    assertEqualsInt('testSplitCmdLineParameter_quotedPartIncludingQuoteInside', 1, tmpResult.Count);
    assertEqualsString('testSplitCmdLineParameter_quotedPartIncludingQuoteInside', 'abab c"d ef', tmpResult[0]);

    tmpResult.Destroy;
  END;


  PROCEDURE testSplitCmdLineParameter_quotedPartIncludingQuoteAtEnd;
  VAR
    tmpResult : TStringList;
    tmpRC : Integer;
  BEGIN
    tmpResult := TStringList.Create;
    tmpRC := splitCmdLineParameter('ab"abc def"""', tmpResult);

    assertEqualsInt('testSplitCmdLineParameter_quotedPartIncludingQuoteAtEnd', 0, tmpRC);
    assertEqualsInt('testSplitCmdLineParameter_quotedPartIncludingQuoteAtEnd', 1, tmpResult.Count);
    assertEqualsString('testSplitCmdLineParameter_quotedPartIncludingQuoteAtEnd', 'ababc def"', tmpResult[0]);

    tmpResult.Destroy;
  END;


  PROCEDURE testSplitCmdLineParameter_TwoQuotedParts;
  VAR
    tmpResult : TStringList;
    tmpRC : Integer;
  BEGIN
    tmpResult := TStringList.Create;
    tmpRC := splitCmdLineParameter('ab"abc""def" "ghi"', tmpResult);

    assertEqualsInt('testSplitCmdLineParameter_TwoQuotedParts', 0, tmpRC);
    assertEqualsInt('testSplitCmdLineParameter_TwoQuotedParts', 2, tmpResult.Count);
    assertEqualsString('testSplitCmdLineParameter_TwoQuotedParts', 'ababc"def', tmpResult[0]);
    assertEqualsString('testSplitCmdLineParameter_TwoQuotedParts', 'ghi', tmpResult[1]);

    tmpResult.Destroy;
  END;


  PROCEDURE testSplitCmdLineParameter_TwoQuotesAtStartEnd;
  VAR
    tmpResult : TStringList;
    tmpRC : Integer;
  BEGIN
    tmpResult := TStringList.Create;
    tmpRC := splitCmdLineParameter('""abc def""', tmpResult);

    assertEqualsInt('testSplitCmdLineParameter_TwoQuotesAtStartEnd', 0, tmpRC);
    assertEqualsInt('testSplitCmdLineParameter_TwoQuotesAtStartEnd', 2, tmpResult.Count);
    assertEqualsString('testSplitCmdLineParameter_TwoQuotesAtStartEnd', 'abc', tmpResult[0]);
    assertEqualsString('testSplitCmdLineParameter_TwoQuotesAtStartEnd', 'def', tmpResult[1]);

    tmpResult.Destroy;
  END;


  PROCEDURE testSplitCmdLineParameter_TwoQuotedPartsMissingClosedQuote;
  VAR
    tmpResult : TStringList;
    tmpRC : Integer;
  BEGIN
    tmpResult := TStringList.Create;
    tmpRC := splitCmdLineParameter('ab"abc""def" "ghi', tmpResult);

    assertEqualsInt('testSplitCmdLineParameter_TwoQuotedPartsMissingClosedQuote', -1, tmpRC);
    assertEqualsInt('testSplitCmdLineParameter_TwoQuotedPartsMissingClosedQuote', 2, tmpResult.Count);
    assertEqualsString('testSplitCmdLineParameter_TwoQuotedPartsMissingClosedQuote', 'ababc"def', tmpResult[0]);
    assertEqualsString('testSplitCmdLineParameter_TwoQuotedPartsMissingClosedQuote', 'ghi', tmpResult[1]);

    tmpResult.Destroy;
  END;


  //////////////////
  // parser Tests
  //////////////////


  PROCEDURE testParseCmdLine_Empty;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertFalse('testParseCmdLine_Empty', tmpCmdLineParameters.getShowUsageFlag);
    assertFalse('testParseCmdLine_Empty', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_Empty', '', tmpCmdLineParameters.getSearchText);
    assertFalse('testParseCmdLine_Empty', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('testParseCmdLine_Empty', '', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_Empty', '', tmpCmdLineParameters.getLanguage);
    assertFalse('testParseCmdLine_Empty', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_Empty', 0, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_Empty', 0, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_Empty', '', tmpCmdLineParameters.getWindowTitle);
  END;


  PROCEDURE testParseCmdLine_QuestionMark;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-?');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertTrue('testParseCmdLine_QuestionMark', tmpCmdLineParameters.getShowUsageFlag);
    assertFalse('testParseCmdLine_QuestionMark', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_QuestionMark', '', tmpCmdLineParameters.getSearchText);
    assertFalse('testParseCmdLine_QuestionMark', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('testParseCmdLine_QuestionMark', '', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_QuestionMark', '', tmpCmdLineParameters.getLanguage);
    assertFalse('testParseCmdLine_QuestionMark', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_QuestionMark', 0, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_QuestionMark', 0, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_QuestionMark', '', tmpCmdLineParameters.getWindowTitle);
  END;


  PROCEDURE testParseCmdLine_lowerH;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-h');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertTrue('testParseCmdLine_lowerH', tmpCmdLineParameters.getShowUsageFlag);
    assertFalse('testParseCmdLine_lowerH', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_lowerH', '', tmpCmdLineParameters.getSearchText);
    assertFalse('testParseCmdLine_lowerH', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('testParseCmdLine_lowerH', '', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_lowerH', '', tmpCmdLineParameters.getLanguage);
    assertFalse('testParseCmdLine_lowerH', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_lowerH', 0, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_lowerH', 0, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_lowerH', '', tmpCmdLineParameters.getWindowTitle);
  END;


  PROCEDURE testParseCmdLine_upperH;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-H');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertTrue('testParseCmdLine_upperH', tmpCmdLineParameters.getShowUsageFlag);
    assertFalse('testParseCmdLine_upperH', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_upperH', '', tmpCmdLineParameters.getSearchText);
    assertFalse('testParseCmdLine_upperH', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('testParseCmdLine_upperH', '', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_upperH', '', tmpCmdLineParameters.getLanguage);
    assertFalse('testParseCmdLine_upperH', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_upperH', 0, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_upperH', 0, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_upperH', '', tmpCmdLineParameters.getWindowTitle);
  END;


  PROCEDURE testParseCmdLine_lowerHelp;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-help');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertTrue('testParseCmdLine_lowerHelp', tmpCmdLineParameters.getShowUsageFlag);
    assertFalse('testParseCmdLine_lowerHelp', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_lowerHelp', '', tmpCmdLineParameters.getSearchText);
    assertFalse('testParseCmdLine_lowerHelp', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('testParseCmdLine_lowerHelp', '', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_lowerHelp', '', tmpCmdLineParameters.getLanguage);
    assertFalse('testParseCmdLine_lowerHelp', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_lowerHelp', 0, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_lowerHelp', 0, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_lowerHelp', '', tmpCmdLineParameters.getWindowTitle);
  END;


  PROCEDURE testParseCmdLine_upperHELP;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-HELP');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertTrue('testParseCmdLine_upperHELP', tmpCmdLineParameters.getShowUsageFlag);
    assertFalse('testParseCmdLine_upperHELP', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_upperHELP', '', tmpCmdLineParameters.getSearchText);
    assertFalse('testParseCmdLine_upperHELP', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('testParseCmdLine_upperHELP', '', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_upperHELP', '', tmpCmdLineParameters.getLanguage);
    assertFalse('testParseCmdLine_upperHELP', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_upperHELP', 0, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_upperHELP', 0, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_upperHELP', '', tmpCmdLineParameters.getWindowTitle);
  END;


  PROCEDURE testParseCmdLine_lowerS;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-s:search');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertFalse('testParseCmdLine_lowerS', tmpCmdLineParameters.getShowUsageFlag);
    assertTrue('testParseCmdLine_lowerS', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_lowerS', 'search', tmpCmdLineParameters.getSearchText);
    assertFalse('testParseCmdLine_lowerS', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('testParseCmdLine_lowerS', '', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_lowerS', '', tmpCmdLineParameters.getLanguage);
    assertFalse('testParseCmdLine_lowerS', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_lowerS', 0, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_lowerS', 0, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_lowerS', '', tmpCmdLineParameters.getWindowTitle);
  END;


  PROCEDURE testParseCmdLine_upperS;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-S:seArch');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertFalse('testParseCmdLine_upperS', tmpCmdLineParameters.getShowUsageFlag);
    assertTrue('testParseCmdLine_upperS', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_upperS', 'seArch', tmpCmdLineParameters.getSearchText);
    assertFalse('testParseCmdLine_upperS', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('testParseCmdLine_upperS', '', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_upperS', '', tmpCmdLineParameters.getLanguage);
    assertFalse('testParseCmdLine_upperS', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_upperS', 0, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_upperS', 0, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_upperS', '', tmpCmdLineParameters.getWindowTitle);
  END;


  PROCEDURE testParseCmdLine_emptyS;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-S');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertFalse('testParseCmdLine_emptyS', tmpCmdLineParameters.getShowUsageFlag);
    assertTrue('testParseCmdLine_emptyS', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_emptyS', '', tmpCmdLineParameters.getSearchText);
    assertFalse('testParseCmdLine_emptyS', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('testParseCmdLine_emptyS', '', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_emptyS', '', tmpCmdLineParameters.getLanguage);
    assertFalse('testParseCmdLine_emptyS', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_emptyS', 0, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_emptyS', 0, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_emptyS', '', tmpCmdLineParameters.getWindowTitle);
  END;


  PROCEDURE testParseCmdLine_emptyColonS;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-S:');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertFalse('testParseCmdLine_emptyColonS', tmpCmdLineParameters.getShowUsageFlag);
    assertTrue('testParseCmdLine_emptyColonS', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_emptyColonS', '', tmpCmdLineParameters.getSearchText);
    assertFalse('testParseCmdLine_emptyColonS', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('testParseCmdLine_emptyColonS', '', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_emptyColonS', '', tmpCmdLineParameters.getLanguage);
    assertFalse('testParseCmdLine_emptyColonS', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_emptyColonS', 0, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_emptyColonS', 0, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_emptyColonS', '', tmpCmdLineParameters.getWindowTitle);
  END;


  PROCEDURE testParseCmdLine_withoutColonS;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-ssEarRch');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertFalse('testParseCmdLine_withoutColonS', tmpCmdLineParameters.getShowUsageFlag);
    assertTrue('testParseCmdLine_withoutColonS', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_withoutColonS', 'sEarRch', tmpCmdLineParameters.getSearchText);
    assertFalse('testParseCmdLine_withoutColonS', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('testParseCmdLine_withoutColonS', '', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_withoutColonS', '', tmpCmdLineParameters.getLanguage);
    assertFalse('testParseCmdLine_withoutColonS', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_withoutColonS', 0, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_withoutColonS', 0, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_withoutColonS', '', tmpCmdLineParameters.getWindowTitle);
  END;


  PROCEDURE testParseCmdLine_doubleColonS;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-s::sEarRch');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertFalse('testParseCmdLine_doubleColonS', tmpCmdLineParameters.getShowUsageFlag);
    assertTrue('testParseCmdLine_doubleColonS', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_doubleColonS', ':sEarRch', tmpCmdLineParameters.getSearchText);
    assertFalse('testParseCmdLine_doubleColonS', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('testParseCmdLine_doubleColonS', '', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_doubleColonS', '', tmpCmdLineParameters.getLanguage);
    assertFalse('testParseCmdLine_doubleColonS', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_doubleColonS', 0, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_doubleColonS', 0, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_doubleColonS', '', tmpCmdLineParameters.getWindowTitle);
  END;


  PROCEDURE testParseCmdLine_lowerG;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-g:search');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertFalse('testParseCmdLine_lowerG', tmpCmdLineParameters.getShowUsageFlag);
    assertFalse('testParseCmdLine_lowerG', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_lowerG', '', tmpCmdLineParameters.getSearchText);
    assertTrue('testParseCmdLine_lowerG', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('testParseCmdLine_lowerG', 'search', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_lowerG', '', tmpCmdLineParameters.getLanguage);
    assertFalse('testParseCmdLine_lowerG', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_lowerG', 0, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_lowerG', 0, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_lowerG', '', tmpCmdLineParameters.getWindowTitle);
  END;


  PROCEDURE testParseCmdLine_upperG;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-G:seArch');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertFalse('testParseCmdLine_upperG', tmpCmdLineParameters.getShowUsageFlag);
    assertFalse('testParseCmdLine_upperG', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_upperG', '', tmpCmdLineParameters.getSearchText);
    assertTrue('testParseCmdLine_upperG', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('testParseCmdLine_upperG', 'seArch', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_upperG', '', tmpCmdLineParameters.getLanguage);
    assertFalse('testParseCmdLine_upperG', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_upperG', 0, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_upperG', 0, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_upperG', '', tmpCmdLineParameters.getWindowTitle);
  END;


  PROCEDURE testParseCmdLine_emptyG;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-G');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertFalse('testParseCmdLine_emptyG', tmpCmdLineParameters.getShowUsageFlag);
    assertFalse('testParseCmdLine_emptyG', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_emptyG', '', tmpCmdLineParameters.getSearchText);
    assertTrue('testParseCmdLine_emptyG', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('testParseCmdLine_emptyG', '', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_emptyG', '', tmpCmdLineParameters.getLanguage);
    assertFalse('testParseCmdLine_emptyG', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_emptyG', 0, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_emptyG', 0, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_emptyG', '', tmpCmdLineParameters.getWindowTitle);
  END;


  PROCEDURE testParseCmdLine_emptyColonG;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-G:');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertFalse('testParseCmdLine_emptyColonG', tmpCmdLineParameters.getShowUsageFlag);
    assertFalse('testParseCmdLine_emptyColonG', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_emptyColonG', '', tmpCmdLineParameters.getSearchText);
    assertTrue('testParseCmdLine_emptyColonG', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('testParseCmdLine_emptyColonG', '', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_emptyColonG', '', tmpCmdLineParameters.getLanguage);
    assertFalse('testParseCmdLine_emptyColonG', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_emptyColonG', 0, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_emptyColonG', 0, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_emptyColonG', '', tmpCmdLineParameters.getWindowTitle);
  END;


  PROCEDURE testParseCmdLine_withoutColonG;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-gsEarRch');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertFalse('testParseCmdLine_withoutColonG', tmpCmdLineParameters.getShowUsageFlag);
    assertFalse('testParseCmdLine_withoutColonG', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_withoutColonG', '', tmpCmdLineParameters.getSearchText);
    assertTrue('testParseCmdLine_withoutColonG', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('testParseCmdLine_withoutColonG', 'sEarRch', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_withoutColonG', '', tmpCmdLineParameters.getLanguage);
    assertFalse('testParseCmdLine_withoutColonG', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_withoutColonG', 0, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_withoutColonG', 0, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_withoutColonG', '', tmpCmdLineParameters.getWindowTitle);
  END;


  PROCEDURE testParseCmdLine_doubleColonG;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-g::sEarRch');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertFalse('testParseCmdLine_doubleColonG', tmpCmdLineParameters.getShowUsageFlag);
    assertFalse('testParseCmdLine_doubleColonG', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_doubleColonG', '', tmpCmdLineParameters.getSearchText);
    assertTrue('testParseCmdLine_doubleColonG', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('testParseCmdLine_doubleColonG', ':sEarRch', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_doubleColonG', '', tmpCmdLineParameters.getLanguage);
    assertFalse('testParseCmdLine_doubleColonG', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_doubleColonG', 0, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_doubleColonG', 0, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_doubleColonG', '', tmpCmdLineParameters.getWindowTitle);
  END;


  PROCEDURE testParseCmdLine_Language;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-lang:DE');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertFalse('testParseCmdLine_Language', tmpCmdLineParameters.getShowUsageFlag);
    assertFalse('testParseCmdLine_Language', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_Language', '', tmpCmdLineParameters.getSearchText);
    assertFalse('testParseCmdLine_Language', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('testParseCmdLine_Language', '', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_Language', 'DE', tmpCmdLineParameters.getLanguage);
    assertFalse('testParseCmdLine_Language', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_Language', 0, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_Language', 0, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_Language', '', tmpCmdLineParameters.getWindowTitle);
  END;


  PROCEDURE testParseCmdLine_HelpManagerNumber;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-Hm:123');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertFalse('testParseCmdLine_HelpManagerNumber', tmpCmdLineParameters.getShowUsageFlag);
    assertFalse('testParseCmdLine_HelpManagerNumber', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_HelpManagerNumber', '', tmpCmdLineParameters.getSearchText);
    assertFalse('testParseCmdLine_HelpManagerNumber', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('testParseCmdLine_HelpManagerNumber', '', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_HelpManagerNumber', '', tmpCmdLineParameters.getLanguage);
    assertTrue('testParseCmdLine_HelpManagerNumber', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_HelpManagerNumber', 123, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_HelpManagerNumber', 0, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_HelpManagerNumber', '', tmpCmdLineParameters.getWindowTitle);
  END;


  PROCEDURE testParseCmdLine_HelpManagerText;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-HM:1zwei3');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertFalse('testParseCmdLine_HelpManagerText', tmpCmdLineParameters.getShowUsageFlag);
    assertFalse('testParseCmdLine_HelpManagerText', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_HelpManagerText', '', tmpCmdLineParameters.getSearchText);
    assertFalse('testParseCmdLine_HelpManagerText', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('parseCmdLine [getGlobalSearchText](19)', '', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_HelpManagerText', '', tmpCmdLineParameters.getLanguage);
    assertFalse('testParseCmdLine_HelpManagerText', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_HelpManagerText', 0, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_HelpManagerText', 0, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_HelpManagerText', '', tmpCmdLineParameters.getWindowTitle);
  END;


  PROCEDURE testParseCmdLine_OwnerNumber;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-oWner:123');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertFalse('testParseCmdLine_OwnerNumber', tmpCmdLineParameters.getShowUsageFlag);
    assertFalse('testParseCmdLine_OwnerNumber', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_OwnerNumber', '', tmpCmdLineParameters.getSearchText);
    assertFalse('testParseCmdLine_OwnerNumber', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('testParseCmdLine_OwnerNumber', '', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_OwnerNumber', '', tmpCmdLineParameters.getLanguage);
    assertFalse('testParseCmdLine_OwnerNumber', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_OwnerNumber', 0, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_OwnerNumber', 123, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_OwnerNumber', '', tmpCmdLineParameters.getWindowTitle);
  END;


  PROCEDURE testParseCmdLine_OwnerTest;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-oWner:1zwei3');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertFalse('testParseCmdLine_OwnerTest', tmpCmdLineParameters.getShowUsageFlag);
    assertFalse('testParseCmdLine_OwnerTest', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_OwnerTest', '', tmpCmdLineParameters.getSearchText);
    assertFalse('testParseCmdLine_OwnerTest', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('testParseCmdLine_OwnerTest', '', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_OwnerTest', '', tmpCmdLineParameters.getLanguage);
    assertFalse('testParseCmdLine_OwnerTest', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_OwnerTest', 0, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_OwnerTest', 0, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_OwnerTest', '', tmpCmdLineParameters.getWindowTitle);
  END;


  PROCEDURE testParseCmdLine_Title;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-tiTle:testTitle');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertFalse('testParseCmdLine_Title', tmpCmdLineParameters.getShowUsageFlag);
    assertFalse('testParseCmdLine_Title', tmpCmdLineParameters.getSearchTextFlag);
    assertEqualsString('testParseCmdLine_Title', '', tmpCmdLineParameters.getSearchText);
    assertFalse('testParseCmdLine_Title', tmpCmdLineParameters.getGlobalSearchTextFlag);
    assertEqualsString('testParseCmdLine_Title', '', tmpCmdLineParameters.getGlobalSearchText);
    assertEqualsString('testParseCmdLine_Title', '', tmpCmdLineParameters.getLanguage);
    assertFalse('testParseCmdLine_Title', tmpCmdLineParameters.getHelpManagerFlag);
    assertEqualsInt('testParseCmdLine_Title', 0, tmpCmdLineParameters.getHelpManagerWindow);
    assertEqualsInt('testParseCmdLine_Title', 0, tmpCmdLineParameters.getOwnerWindow);
    assertEqualsString('testParseCmdLine_Title', 'testTitle', tmpCmdLineParameters.getWindowTitle);
  END;


  // can't test start profile

  PROCEDURE testParseCmdLine_WindowPos;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
    tmpWindowPosition : TWindowPosition;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-pos:20,40,60,80');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertTrue('parseCmdLine [getWindowPositionFlag](40)', tmpCmdLineParameters.getWindowPositionFlag);
    tmpWindowPosition := tmpCmdLineParameters.getWindowPosition;
    assertEqualsInt('testParseCmdLine_WindowPos', 20, tmpWindowPosition.left);
    assertEqualsInt('testParseCmdLine_WindowPos', 40, tmpWindowPosition.bottom);
    assertEqualsInt('testParseCmdLine_WindowPos', 60, tmpWindowPosition.width);
    assertEqualsInt('testParseCmdLine_WindowPos', 80, tmpWindowPosition.height);
  END;


  PROCEDURE testParseCmdLine_WindowPosPercentage;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
    tmpWindowPosition : TWindowPosition;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('-pos:100p,100p,50p,50p');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertTrue('parseCmdLine [getWindowPositionFlag](41)', tmpCmdLineParameters.getWindowPositionFlag);
    tmpWindowPosition := tmpCmdLineParameters.getWindowPosition;
    assertEqualsInt('testParseCmdLine_WindowPosPercentage [left](41)', 1280, tmpWindowPosition.left);
    assertEqualsInt('testParseCmdLine_WindowPosPercentage [bottom](41)', 1024, tmpWindowPosition.bottom);
    assertEqualsInt('testParseCmdLine_WindowPosPercentage [width](41)', 640, tmpWindowPosition.width);
    assertEqualsInt('testParseCmdLine_WindowPosPercentage [height](41)', 512, tmpWindowPosition.height);
  END;


  PROCEDURE testParseCmdLine_Topic;
  VAR
    tmpParams : TStringList;
    tmpCmdLineParameters : TCmdLineParameters;
  BEGIN
    tmpParams := TStringList.Create;
    tmpParams.add('ab c');
    tmpParams.add('topi1');
    tmpParams.add('topi2');
    tmpCmdLineParameters := TCmdLineParameters.Create;
    tmpCmdLineParameters.parseCmdLine(tmpParams);

    assertEqualsString('testParseCmdLine_Topic', 'ab c', tmpCmdLineParameters.getFileNames);
    assertEqualsString('testParseCmdLine_Topic', 'topi1 topi2', tmpCmdLineParameters.getTopics);
  END;


  FUNCTION getCmdLineParameterUnitTests : TList;
  BEGIN
    result := TList.Create;
    result.add(@testSplitCmdLineParameter_Empty);
    result.add(@testSplitCmdLineParameter_simpleOne);
    result.add(@testSplitCmdLineParameter_simpleOneWithLeadingBlanks);
    result.add(@testSplitCmdLineParameter_simpleThreeParts);
    result.add(@testSplitCmdLineParameter_quoted);
    result.add(@testSplitCmdLineParameter_quotedPart);
    result.add(@testSplitCmdLineParameter_quotedPartIncludingQuoteAtStart);
    result.add(@testSplitCmdLineParameter_quotedPartIncludingQuoteInside);
    result.add(@testSplitCmdLineParameter_quotedPartIncludingQuoteAtEnd);
    result.add(@testSplitCmdLineParameter_TwoQuotedParts);
    result.add(@testSplitCmdLineParameter_TwoQuotesAtStartEnd);
    result.add(@testSplitCmdLineParameter_TwoQuotedPartsMissingClosedQuote);

    result.add(@testParseCmdLine_Empty);

    result.add(@testParseCmdLine_QuestionMark);
    result.add(@testParseCmdLine_lowerH);
    result.add(@testParseCmdLine_upperH);
    result.add(@testParseCmdLine_lowerHelp);
    result.add(@testParseCmdLine_upperHELP);

    result.add(@testParseCmdLine_lowerS);
    result.add(@testParseCmdLine_upperS);
    result.add(@testParseCmdLine_emptyS);
    result.add(@testParseCmdLine_emptyColonS);
    result.add(@testParseCmdLine_emptyColonS);
    result.add(@testParseCmdLine_withoutColonS);
    result.add(@testParseCmdLine_doubleColonS);

    result.add(@testParseCmdLine_lowerG);
    result.add(@testParseCmdLine_upperG);
    result.add(@testParseCmdLine_emptyG);
    result.add(@testParseCmdLine_emptyColonG);
    result.add(@testParseCmdLine_emptyColonG);
    result.add(@testParseCmdLine_withoutColonG);
    result.add(@testParseCmdLine_doubleColonG);

    result.add(@testParseCmdLine_Language);
    result.add(@testParseCmdLine_HelpManagerNumber);
    result.add(@testParseCmdLine_HelpManagerText);
    result.add(@testParseCmdLine_OwnerNumber);
    result.add(@testParseCmdLine_OwnerTest);
    result.add(@testParseCmdLine_Title);
    result.add(@testParseCmdLine_WindowPos);
    result.add(@testParseCmdLine_WindowPosPercentage);
    result.add(@testParseCmdLine_Topic);
  END;

END.