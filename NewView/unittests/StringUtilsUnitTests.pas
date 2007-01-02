Unit StringUtilsUnitTests;

Interface

uses
  Classes,
  TestAssert,
  StringUtilsUnit;

 FUNCTION getStringUtilsUnitTests : TList;


Implementation

  PROCEDURE testTSerializableStringList_Construction;
  VAR
    tmpResult : TSerializableStringList;
  BEGIN
    tmpResult := TSerializableStringList.Create;

    assertEqualsInt('testTSerializableStringList_Construction', 0, tmpResult.getCount);

    tmpResult.Destroy;
  END;


  PROCEDURE testTSerializableStringList_Add;
  VAR
    tmpResult : TSerializableStringList;
  BEGIN
    tmpResult := TSerializableStringList.Create;

    tmpResult.add('Test');
    assertEqualsInt('testTSerializableStringList_Add', 1, tmpResult.getCount);

    tmpResult.Destroy;
  END;


  PROCEDURE testTSerializableStringList_Get_Empty;
  VAR
    tmpResult : TSerializableStringList;
  BEGIN
    tmpResult := TSerializableStringList.Create;

    assertEqualsInt('testTSerializableStringList_Get_Empty', 0, tmpResult.getCount);

    try
      tmpResult.get(0);
    except
      on e:EListError do
      begin
        assertEqualsString('testTSerializableStringList_Get', 'TList error exception (EListError) occured', e.message);
      end;
    end;

    tmpResult.Destroy;
  END;


  PROCEDURE testTSerializableStringList_Get;
  VAR
    tmpResult : TSerializableStringList;
  BEGIN
    tmpResult := TSerializableStringList.Create;

    tmpResult.add('First');
    tmpResult.add('Second');
    assertEqualsInt('testTSerializableStringList_Get', 2, tmpResult.getCount);
    assertEqualsString('testTSerializableStringList_Get', 'First', tmpResult.get(0));

    tmpResult.Destroy;
  END;


  PROCEDURE testTSerializableStringList_getSerializedString_Empty;
  VAR
    tmpResult : TSerializableStringList;
  BEGIN
    tmpResult := TSerializableStringList.Create;

    assertEqualsString('testTSerializableStringList_getSerializedString_Empty', '', tmpResult.getSerializedString);

    tmpResult.Destroy;
  END;


  PROCEDURE testTSerializableStringList_getSerializedString;
  VAR
    tmpResult : TSerializableStringList;
  BEGIN
    tmpResult := TSerializableStringList.Create;

    tmpResult.add('First');
    tmpResult.add('Second');
    assertEqualsString('testTSerializableStringList_getSerializedString', 'First&Second', tmpResult.getSerializedString);

    tmpResult.Destroy;
  END;


  PROCEDURE testTSerializableStringList_getSerializedString_WithBlanks;
  VAR
    tmpResult : TSerializableStringList;
  BEGIN
    tmpResult := TSerializableStringList.Create;

    tmpResult.add('Fi rst');
    tmpResult.add('Second');
    assertEqualsString('testTSerializableStringList_getSerializedString_WithBlanks', 'Fi rst&Second', tmpResult.getSerializedString);

    tmpResult.Destroy;
  END;


  PROCEDURE testTSerializableStringList_getSerializedString_Escaping;
  VAR
    tmpResult : TSerializableStringList;
  BEGIN
    tmpResult := TSerializableStringList.Create;

    tmpResult.add('F&i"r''st');
    tmpResult.add('Sec&ond');
    assertEqualsString('testTSerializableStringList_getSerializedString_WithBlanks', 'F\&i"r''st&Sec\&ond', tmpResult.getSerializedString);

    tmpResult.Destroy;
  END;


  PROCEDURE testTSerializableStringList_readValuesFromSerializedString;
  VAR
    tmpResult : TSerializableStringList;
    tmpSerialized : String;
  BEGIN
    tmpResult := TSerializableStringList.Create;

    tmpSerialized := 'First&Second';

    tmpResult.readValuesFromSerializedString(tmpSerialized);

    assertEqualsInt('testTSerializableStringList_readValuesFromSerializedString', 2, tmpResult.getCount);
    assertEqualsString('testTSerializableStringList_readValuesFromSerializedString', 'First', tmpResult.get(0));
    assertEqualsString('testTSerializableStringList_readValuesFromSerializedString', 'Second', tmpResult.get(1));

    tmpResult.Destroy;
  END;


  PROCEDURE testTSerializableStringList_readValuesFromSerializedString_quoted;
  VAR
    tmpResult : TSerializableStringList;
    tmpSerialized : String;
  BEGIN
    tmpResult := TSerializableStringList.Create;

    tmpSerialized := 'Fi\\nrst&Se\&cond';

    tmpResult.readValuesFromSerializedString(tmpSerialized);

    assertEqualsInt('testTSerializableStringList_readValuesFromSerializedString_quoted', 2, tmpResult.getCount);
    assertEqualsString('testTSerializableStringList_readValuesFromSerializedString_quoted', 'Fi\nrst', tmpResult.get(0));
    assertEqualsString('testTSerializableStringList_readValuesFromSerializedString_quoted', 'Se&cond', tmpResult.get(1));

    tmpResult.Destroy;
  END;


  PROCEDURE testTSerializableStringList_readValuesFromSerializedString_empty;
  VAR
    tmpResult : TSerializableStringList;
    tmpSerialized : String;
  BEGIN
    tmpResult := TSerializableStringList.Create;

    tmpSerialized := '';

    tmpResult.readValuesFromSerializedString(tmpSerialized);

    assertEqualsInt('testTSerializableStringList_readValuesFromSerializedString_empty', 0, tmpResult.getCount);

    tmpResult.Destroy;
  END;


  PROCEDURE testTSerializableStringList_readValuesFromSerializedString_firstempty;
  VAR
    tmpResult : TSerializableStringList;
    tmpSerialized : String;
  BEGIN
    tmpResult := TSerializableStringList.Create;

    tmpResult.add('');
    tmpResult.add('test');

    tmpSerialized := tmpResult.getSerializedString;

    tmpResult.readValuesFromSerializedString(tmpSerialized);

    assertEqualsInt('testTSerializableStringList_readValuesFromSerializedString_firstempty', 2, tmpResult.getCount);
    assertEqualsString('testTSerializableStringList_readValuesFromSerializedString_firstempty', '', tmpResult.get(0));
    assertEqualsString('testTSerializableStringList_readValuesFromSerializedString_firstempty', 'test', tmpResult.get(1));

    tmpResult.Destroy;
  END;


  PROCEDURE testTSerializableStringList_readValuesFromSerializedString_2;
  VAR
    tmpResult : TSerializableStringList;
    tmpSerialized : String;
  BEGIN
    tmpResult := TSerializableStringList.Create;

    tmpResult.add('&test&');
    tmpResult.add('"test"');
    tmpResult.add('');
    tmpResult.add('abc\&');
    tmpResult.add('');

    tmpSerialized := tmpResult.getSerializedString;

    tmpResult.readValuesFromSerializedString(tmpSerialized);

    assertEqualsInt('testTSerializableStringList_readValuesFromSerializedString_2', 5, tmpResult.getCount);
    assertEqualsString('testTSerializableStringList_readValuesFromSerializedString_2', '&test&', tmpResult.get(0));
    assertEqualsString('testTSerializableStringList_readValuesFromSerializedString_2', '"test"', tmpResult.get(1));
    assertEqualsString('testTSerializableStringList_readValuesFromSerializedString_2', '', tmpResult.get(2));
    assertEqualsString('testTSerializableStringList_readValuesFromSerializedString_2', 'abc\&', tmpResult.get(3));
    assertEqualsString('testTSerializableStringList_readValuesFromSerializedString_2', '', tmpResult.get(4));

    tmpResult.Destroy;
  END;


  // ------------------------------------------------------


  PROCEDURE testEscapeAllCharsBy_Empty;
  VAR
    tmpResult : String;
  BEGIN
    tmpResult := StrEscapeAllCharsBy('', ['b'], '\');

    assertEqualsString('testEscapeAllCharsBy_Empty', '', tmpResult);
  END;


  PROCEDURE testEscapeAllCharsBy_EmptyChars;
  VAR
    tmpResult : String;
  BEGIN
    tmpResult := StrEscapeAllCharsBy('abcd', [], '\');

    assertEqualsString('testEscapeAllCharsBy_EmptyChars', 'abcd', tmpResult);
  END;


  PROCEDURE testEscapeAllCharsBy;
  VAR
    tmpResult : String;
  BEGIN
    tmpResult := StrEscapeAllCharsBy('abc', ['b'], '\');

    assertEqualsString('testEscapeAllCharsBy', 'a\bc', tmpResult);
  END;


  PROCEDURE testEscapeAllCharsBy_EscapeEscape;
  VAR
    tmpResult : String;
  BEGIN
    tmpResult := StrEscapeAllCharsBy('a\bc', ['b'], '\');

    assertEqualsString('testEscapeAllCharsBy_EscapeEscape', 'a\\\bc', tmpResult);
  END;


  PROCEDURE testStrExtractStrings_EmptyReceiver;
  VAR
    tmpResult : TStringList;
  BEGIN
    tmpResult := TStringList.Create;
    StrExtractStrings(tmpResult, '', ['x'], '\');

    assertEqualsInt('testStrExtractStrings_EmptyReceiver', 0, tmpResult.count);
    tmpResult.Destroy;
  END;


  PROCEDURE testStrExtractStrings_OnlyOnePart;
  VAR
    tmpResult : TStringList;
  BEGIN
    tmpResult := TStringList.Create;
    StrExtractStrings(tmpResult, 'abcd', ['x'], '\');

    assertEqualsInt('testStrExtractStrings_OnlyOnePart', 1, tmpResult.count);
    assertEqualsString('testStrExtractStrings_OnlyOnePart', 'abcd', tmpResult[0]);

    tmpResult.Destroy;
  END;


  PROCEDURE testStrExtractStrings_ManyParts;
  VAR
    tmpResult : TStringList;
  BEGIN
    tmpResult := TStringList.Create;
    StrExtractStrings(tmpResult, 'abxcd', ['x'], '\');

    assertEqualsInt('testStrExtractStrings_ManyParts', 2, tmpResult.count);
    assertEqualsString('testStrExtractStrings_ManyParts', 'ab', tmpResult[0]);
    assertEqualsString('testStrExtractStrings_ManyParts', 'cd', tmpResult[1]);

    tmpResult.Destroy;
  END;


  PROCEDURE testStrExtractStrings_StartWithDelimiter;
  VAR
    tmpResult : TStringList;
  BEGIN
    tmpResult := TStringList.Create;
    StrExtractStrings(tmpResult, 'xab', ['x'], '\');

    assertEqualsInt('testStrExtractStrings_StartWithDelimiter', 2, tmpResult.count);
    assertEqualsString('testStrExtractStrings_StartWithDelimiter', '', tmpResult[0]);
    assertEqualsString('testStrExtractStrings_StartWithDelimiter', 'ab', tmpResult[1]);

    tmpResult.Destroy;
  END;


  PROCEDURE testStrExtractStrings_EndWithDelimiter;
  VAR
    tmpResult : TStringList;
  BEGIN
    tmpResult := TStringList.Create;
    StrExtractStrings(tmpResult, 'abx', ['x'], '\');

    assertEqualsInt('testStrExtractStrings_EndWithDelimiter', 2, tmpResult.count);
    assertEqualsString('testStrExtractStrings_EndWithDelimiter', 'ab', tmpResult[0]);
    assertEqualsString('testStrExtractStrings_EndWithDelimiter', '', tmpResult[1]);

    tmpResult.Destroy;
  END;


  PROCEDURE testStrExtractStrings_EmptyPartInside;
  VAR
    tmpResult : TStringList;
  BEGIN
    tmpResult := TStringList.Create;
    StrExtractStrings(tmpResult, 'axxb', ['x'], '\');

    assertEqualsInt('testStrExtractStrings_EmptyPartInside', 3, tmpResult.count);
    assertEqualsString('testStrExtractStrings_EmptyPartInside', 'a', tmpResult[0]);
    assertEqualsString('testStrExtractStrings_EmptyPartInside', '', tmpResult[1]);
    assertEqualsString('testStrExtractStrings_EmptyPartInside', 'b', tmpResult[2]);

    tmpResult.Destroy;
  END;


  PROCEDURE testStrExtractStrings_NoDelimiter;
  VAR
    tmpResult : TStringList;
  BEGIN
    tmpResult := TStringList.Create;
    StrExtractStrings(tmpResult, 'axxb', [], '\');

    assertEqualsInt('testStrExtractStrings_NoDelimiter', 1, tmpResult.count);
    assertEqualsString('testStrExtractStrings_NoDelimiter', 'axxb', tmpResult[0]);

    tmpResult.Destroy;
  END;


  PROCEDURE testStrExtractStrings_EscapedDelimiter;
  VAR
    tmpResult : TStringList;
  BEGIN
    tmpResult := TStringList.Create;
    StrExtractStrings(tmpResult, 'a\xb', ['x'], '\');

    assertEqualsInt('testStrExtractStrings_EscapedDelimiter', 1, tmpResult.count);
    assertEqualsString('testStrExtractStrings_EscapedDelimiter', 'axb', tmpResult[0]);

    tmpResult.Destroy;
  END;

  PROCEDURE testStrExtractStrings_EscapedEscapeChar;
  VAR
    tmpResult : TStringList;
  BEGIN
    tmpResult := TStringList.Create;
    StrExtractStrings(tmpResult, 'a\\xb', ['x'], '\');

    assertEqualsInt('testStrExtractStrings_EscapedEscapeChar', 2, tmpResult.count);
    assertEqualsString('testStrExtractStrings_EscapedEscapeChar', 'a\', tmpResult[0]);
    assertEqualsString('testStrExtractStrings_EscapedEscapeChar', 'b', tmpResult[1]);

    tmpResult.Destroy;
  END;


  PROCEDURE testTrimChars_Empty;
  VAR
    tmpResult : String;
  BEGIN
    tmpResult := StrTrimChars('', ['b', 'x']);

    assertEqualsString('testTrimChars_Empty', '', tmpResult);
  END;


  PROCEDURE testTrimChars_RemoveAll;
  VAR
    tmpResult : String;
  BEGIN
    tmpResult := StrTrimChars('bxxxbx', ['b', 'x']);

    assertEqualsString('testTrimChars_RemoveAll', '', tmpResult);
  END;


  PROCEDURE testTrimChars_LeftOnly;
  VAR
    tmpResult : String;
  BEGIN
    tmpResult := StrTrimChars('bxbxxay', ['b', 'x']);

    assertEqualsString('testTrimChars_LeftOnly', 'ay', tmpResult);
  END;


  PROCEDURE testTrimChars_RightOnly;
  VAR
    tmpResult : String;
  BEGIN
    tmpResult := StrTrimChars('aybxbxx', ['b', 'x']);

    assertEqualsString('testTrimChars_LeftOnly', 'ay', tmpResult);
  END;


  PROCEDURE testTrimChars_CharsInside;
  VAR
    tmpResult : String;
  BEGIN
    tmpResult := StrTrimChars('bxabxvvx', ['b', 'x']);

    assertEqualsString('testTrimChars_CharsInside', 'abxvv', tmpResult);
  END;


  PROCEDURE testTrimChars_Nothing;
  VAR
    tmpResult : String;
  BEGIN
    tmpResult := StrTrimChars('acdef', ['b', 'x']);

    assertEqualsString('testTrimChars_Nothing', 'acdef', tmpResult);
  END;


  PROCEDURE testTrim;
  VAR
    tmpResult : String;
  BEGIN
    tmpResult := StrTrim('  a bc ');

    assertEqualsString('testTrim', 'a bc', tmpResult);
  END;


  PROCEDURE testStrEndsWith_BothEmpty;
  VAR
    tmpResult : Boolean;
  BEGIN
    tmpResult := StrEndsWith('', '');

    assertTrue('testStrEndsWith_BothEmpty', tmpResult);
  END;


  PROCEDURE testStrEndsWith_StringEmpty;
  VAR
    tmpResult : Boolean;
  BEGIN
    tmpResult := StrEndsWith('', 'end');

    assertFalse('testStrEndsWith_StringEmpty', tmpResult);
  END;


  PROCEDURE testStrEndsWith_MatchEmpty;
  VAR
    tmpResult : Boolean;
  BEGIN
    tmpResult := StrEndsWith('test', '');

    assertTrue('testStrEndsWith_MatchEmpty', tmpResult);
  END;


  PROCEDURE testStrEndsWith_StringToShort;
  VAR
    tmpResult : Boolean;
  BEGIN
    tmpResult := StrEndsWith('test', 'test1');

    assertFalse('testStrEndsWith_StringToShort', tmpResult);
  END;


  PROCEDURE testStrEndsWith_StringEqualLength;
  VAR
    tmpResult : Boolean;
  BEGIN
    tmpResult := StrEndsWith('test', 'best');

    assertFalse('testStrEndsWith_StringEqualLength', tmpResult);
  END;


  PROCEDURE testStrEndsWith_StringEqualLengthMatch;
  VAR
    tmpResult : Boolean;
  BEGIN
    tmpResult := StrEndsWith('test', 'test');

    assertTrue('testStrEndsWith_StringEqualLengthMatch', tmpResult);
  END;


  PROCEDURE testStrEndsWith_StringMatch;
  VAR
    tmpResult : Boolean;
  BEGIN
    tmpResult := StrEndsWith('a simple test', 'test');

    assertTrue('testStrEndsWith_StringMatch', tmpResult);
  END;


  PROCEDURE testStrEndsWith_StringMatchCaseSensitive;
  VAR
    tmpResult : Boolean;
  BEGIN
    tmpResult := StrEndsWith('a simple tEst', 'test');

    assertFalse('testStrEndsWith_StringMatchCaseSensitive', tmpResult);
  END;


  PROCEDURE testStrEndsWithIgnoringCase_BothEmpty;
  VAR
    tmpResult : Boolean;
  BEGIN
    tmpResult := StrEndsWithIgnoringCase('', '');

    assertTrue('testStrEndsWithIgnoringCase_BothEmpty', tmpResult);
  END;


  PROCEDURE testStrEndsWithIgnoringCase_StringEmpty;
  VAR
    tmpResult : Boolean;
  BEGIN
    tmpResult := StrEndsWithIgnoringCase('', 'end');

    assertFalse('testStrEndsWithIgnoringCase_StringEmpty', tmpResult);
  END;


  PROCEDURE testStrEndsWithIgnoringCase_MatchEmpty;
  VAR
    tmpResult : Boolean;
  BEGIN
    tmpResult := StrEndsWithIgnoringCase('test', '');

    assertTrue('testStrEndsWithIgnoringCase_MatchEmpty', tmpResult);
  END;


  PROCEDURE testStrEndsWithIgnoringCase_StringToShort;
  VAR
    tmpResult : Boolean;
  BEGIN
    tmpResult := StrEndsWithIgnoringCase('test', 'test1');

    assertFalse('testStrEndsWithIgnoringCase_StringToShort', tmpResult);
  END;


  PROCEDURE testStrEndsWithIgnoringCase_StringEqualLength;
  VAR
    tmpResult : Boolean;
  BEGIN
    tmpResult := StrEndsWithIgnoringCase('test', 'best');

    assertFalse('testStrEndsWithIgnoringCase_StringEqualLength', tmpResult);
  END;


  PROCEDURE testStrEndsWithIgnoringCase_StringEqualLengthMatch;
  VAR
    tmpResult : Boolean;
  BEGIN
    tmpResult := StrEndsWithIgnoringCase('test', 'test');

    assertTrue('testStrEndsWithIgnoringCase_StringEqualLengthMatch', tmpResult);
  END;


  PROCEDURE testStrEndsWithIgnoringCase_StringMatch;
  VAR
    tmpResult : Boolean;
  BEGIN
    tmpResult := StrEndsWithIgnoringCase('a simple test', 'test');

    assertTrue('testStrEndsWithIgnoringCase_StringMatch', tmpResult);
  END;


  PROCEDURE testStrEndsWithIgnoringCase_StringMatchCaseInSensitive;
  VAR
    tmpResult : Boolean;
  BEGIN
    tmpResult := StrEndsWithIgnoringCase('a simple tEst', 'test');

    assertTrue('testStrEndsWithIgnoringCase_StringMatchCaseSensitive', tmpResult);
  END;


  // ----------------------------------------------------------


  FUNCTION getStringUtilsUnitTests : TList;
  BEGIN
    result := TList.Create;

    result.add(@testTSerializableStringList_Construction);
    result.add(@testTSerializableStringList_Add);
    result.add(@testTSerializableStringList_Get_Empty);
    result.add(@testTSerializableStringList_Get);
    result.add(@testTSerializableStringList_getSerializedString_Empty);
    result.add(@testTSerializableStringList_getSerializedString);
    result.add(@testTSerializableStringList_getSerializedString);
    result.add(@testTSerializableStringList_getSerializedString_WithBlanks);
    result.add(@testTSerializableStringList_getSerializedString_Escaping);
    result.add(@testTSerializableStringList_readValuesFromSerializedString);
    result.add(@testTSerializableStringList_readValuesFromSerializedString_quoted);
    result.add(@testTSerializableStringList_readValuesFromSerializedString_empty);
    result.add(@testTSerializableStringList_readValuesFromSerializedString_firstempty);
    result.add(@testTSerializableStringList_readValuesFromSerializedString_2);

    result.add(@testEscapeAllCharsBy);
    result.add(@testEscapeAllCharsBy_Empty);
    result.add(@testEscapeAllCharsBy_EmptyChars);
    result.add(@testEscapeAllCharsBy_EscapeEscape);

    result.add(@testStrExtractStrings_EmptyReceiver);
    result.add(@testStrExtractStrings_OnlyOnePart);
    result.add(@testStrExtractStrings_ManyParts);
    result.add(@testStrExtractStrings_StartWithDelimiter);
    result.add(@testStrExtractStrings_EndWithDelimiter);
    result.add(@testStrExtractStrings_EmptyPartInside);
    result.add(@testStrExtractStrings_NoDelimiter);
    result.add(@testStrExtractStrings_EscapedDelimiter);
    result.add(@testStrExtractStrings_EscapedEscapeChar);

    result.add(@testTrimChars_Empty);
    result.add(@testTrimChars_RemoveAll);
    result.add(@testTrimChars_LeftOnly);
    result.add(@testTrimChars_RightOnly);
    result.add(@testTrimChars_CharsInside);
    result.add(@testTrimChars_Nothing);
    result.add(@testTrim);

    result.add(@testStrEndsWith_BothEmpty);
    result.add(@testStrEndsWith_StringEmpty);
    result.add(@testStrEndsWith_MatchEmpty);
    result.add(@testStrEndsWith_StringToShort);
    result.add(@testStrEndsWith_StringEqualLength);
    result.add(@testStrEndsWith_StringEqualLengthMatch);
    result.add(@testStrEndsWith_StringMatch);
    result.add(@testStrEndsWith_StringMatchCaseSensitive);

    result.add(@testStrEndsWithIgnoringCase_BothEmpty);
    result.add(@testStrEndsWithIgnoringCase_StringEmpty);
    result.add(@testStrEndsWithIgnoringCase_MatchEmpty);
    result.add(@testStrEndsWithIgnoringCase_StringToShort);
    result.add(@testStrEndsWithIgnoringCase_StringEqualLength);
    result.add(@testStrEndsWithIgnoringCase_StringEqualLengthMatch);
    result.add(@testStrEndsWithIgnoringCase_StringMatch);
    result.add(@testStrEndsWithIgnoringCase_StringMatchCaseInSensitive);

  END;

END.