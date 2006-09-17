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
    tmpResult := escapeAllCharsBy('', 'b', '\');

    assertEqualsString('testEscapeAllCharsBy_Empty', '', tmpResult);
  END;


  PROCEDURE testEscapeAllCharsBy_EmptyChars;
  VAR
    tmpResult : String;
  BEGIN
    tmpResult := escapeAllCharsBy('abcd', '', '\');

    assertEqualsString('testEscapeAllCharsBy_EmptyChars', '', tmpResult);
  END;


  PROCEDURE testEscapeAllCharsBy;
  VAR
    tmpResult : String;
  BEGIN
    tmpResult := escapeAllCharsBy('abc', 'b', '\');

    assertEqualsString('testEscapeAllCharsBy', 'a\bc', tmpResult);
  END;


  PROCEDURE testEscapeAllCharsBy_EscapeEscape;
  VAR
    tmpResult : String;
  BEGIN
    tmpResult := escapeAllCharsBy('a\bc', 'b', '\');

    assertEqualsString('testEscapeAllCharsBy', 'a\\\bc', tmpResult);
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
  END;

END.