Unit TestAssert;

Interface

uses
  SysUtils;

  TYPE EAssertFailed=CLASS(Exception);

  PROCEDURE assertEqualsString(aTestDescription : String; anExpectedValue : String; aRealValue : String);
  PROCEDURE assertEqualsInt(aTestDescription : String; anExpectedValue : INTEGER; aRealValue : INTEGER);
  PROCEDURE assertEqualsLongWord(aTestDescription : String; anExpectedValue : LongWord; aRealValue : LongWord);
  PROCEDURE assertTrue(aTestDescription : String; aRealValue : Boolean);
  PROCEDURE assertFalse(aTestDescription : String; aRealValue : Boolean);


Implementation

  PROCEDURE assertEqualsString(aTestDescription : String; anExpectedValue : String; aRealValue : String);
  VAR
    tmpMessage : String;
  BEGIN
    if (aRealValue <> anExpectedValue) then
    begin
      tmpMessage := 'Failed: ' + aTestDescription + ' Expected: ''';
      tmpMessage := tmpMessage + anExpectedValue;
      tmpMessage := tmpMessage + ''' but it was: ''';
      tmpMessage := tmpMessage + aRealValue;
      tmpMessage := tmpMessage + '''';
      raise EAssertFailed.Create(tmpMessage);
    end;
  END;


  PROCEDURE assertEqualsInt(aTestDescription : String; anExpectedValue : INTEGER; aRealValue : INTEGER);
  VAR
    tmpMessage : String;
    tmpIntString : String;
  BEGIN
    if (aRealValue <> anExpectedValue) then
    begin
      tmpMessage := 'Failed: ' + aTestDescription + ' Expected: ''';
      Str(anExpectedValue, tmpIntString);
      tmpMessage := tmpMessage + tmpIntString;
      tmpMessage := tmpMessage + ''' but it was: ''';
      Str(aRealValue, tmpIntString);
      tmpMessage := tmpMessage + tmpIntString;
      tmpMessage := tmpMessage + '''';
      raise EAssertFailed.Create(tmpMessage);
    end;
  END;


  PROCEDURE assertEqualsLongWord(aTestDescription : String; anExpectedValue : LongWord; aRealValue : LongWord);
  VAR
    tmpMessage : String;
    tmpIntString : String;
  BEGIN
    if (aRealValue <> anExpectedValue) then
    begin
      tmpMessage := 'Failed: ' + aTestDescription + ' Expected: ''';
      Str(anExpectedValue, tmpIntString);
      tmpMessage := tmpMessage + tmpIntString;
      tmpMessage := tmpMessage + ''' but it was: ''';
      Str(aRealValue, tmpIntString);
      tmpMessage := tmpMessage + tmpIntString;
      tmpMessage := tmpMessage + '''';
      raise EAssertFailed.Create(tmpMessage);
    end;
  END;


  PROCEDURE assertTrue(aTestDescription : String; aRealValue : Boolean);
  VAR
    tmpMessage : String;
  BEGIN
    if (not aRealValue) then
    begin
      tmpMessage := 'Failed: ' + aTestDescription + ' Expected: ''True';
      tmpMessage := tmpMessage + ''' but it was: ''False''';
      raise EAssertFailed.Create(tmpMessage);
    end;
  END;


  PROCEDURE assertFalse(aTestDescription : String; aRealValue : Boolean);
  VAR
    tmpMessage : String;
  BEGIN
    if (aRealValue) then
    begin
      tmpMessage := 'Failed: ' + aTestDescription + ' Expected: ''False';
      tmpMessage := tmpMessage + ''' but it was: ''True''';
      raise EAssertFailed.Create(tmpMessage);
    end;
  END;

end.
