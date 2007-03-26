Unit StringUtilsUnit;

// NewView - a new OS/2 Help Viewer
// Copyright 2006-2007 Ronald Brill (rbri at rbri dot de)
// This software is released under the GNU Public License - see readme.txt

// Helper functions to work with strings

Interface

uses
 Classes;

const
  StrCR = chr(13);
  StrLF = chr(10);
  StrCRLF = StrCR + StrLF;
  Quote = '''';
  DoubleQuote = '"';


  TYPE
     TSerializableStringList = class
     private
       stringList : TStringList;

     public
       CONSTRUCTOR Create;
       DESTRUCTOR Destroy; override;
       FUNCTION getCount : LongInt;
       PROCEDURE add(const aString : String);
       FUNCTION get(const anIndex : LongInt) : String;
       FUNCTION getSerializedString : String;
       PROCEDURE readValuesFromSerializedString(const aSerializedString : String);
  end;

  TYPE
    TSetOfChars = set of char;

  // prefices all occurences of one of the chars in aStringWithChars with anEscape char
  // if the escapeChar itself is found, then it is doubled
  Function StrEscapeAllCharsBy(Const aReceiver: String; const aSetOfChars: TSetOfChars; const anEscapeChar: char): String;

  // Extract all fields in a String given a set of delimiter characters and
  // an optional escape character usable to escape field delimits.
  // Example:
  //     StrExtractStrings('1x2x3\x4', 'x', '\') ->
  //     returns 4 strings: '1', '', '2' and '3x4'
  Procedure StrExtractStrings(Var aResult : TStrings; Const aReceiver: String; const aSetOfChars: TSetOfChars; const anEscapeChar: char);

  // same as StrExtractStrings but ignores empty strings
  Procedure StrExtractStringsIgnoreEmpty(Var aResult : TStrings; Const aReceiver: String; const aSetOfChars: TSetOfChars; const anEscapeChar: char);

  // removes all occurences of char from aSetOfChars from the beginning
  // of a String.
  Function StrTrimLeftChars(const aReceiver: String; const aSetOfChars: TSetOfChars): String;

  // removes all occurences of char from aSetOfChars from the end
  // of a String.
  Function StrTrimRightChars(const aReceiver: String; const aSetOfChars: TSetOfChars): String;

  // removes all occurences of char from aSetOfChars from the beginning
  // end the end of a String.
  Function StrTrimChars(const aReceiver: String; const aSetOfChars: TSetOfChars): String;

  // removes all blanks from beginning and end
  Function StrTrim(const aReceiver: String): String;

  // Returns the aCount leftmost chars of aString
  Function StrLeft(const aString : String; const aCount : Integer) : String;

  // Returns a copy of the string without aCount chars from right
  Function StrLeftWithout(const aString : String; const aCount : Integer) : String;

  // Returns a copy of the string including all characters until one from aSetOfChars found
  Function StrLeftUntil(const aReceiver: String; const aSetOfChars: TSetOfChars) : String;

  // returns true if the String starts with the provides one
  // this is case SENSITIVE
  Function StrStartsWith(const aReceiver: String; const aStartString: String): Boolean;

  // returns true if the String starts with the provides one
  // this is case INsensitive
  Function StrStartsWithIgnoringCase(const aReceiver: String; const aStartString: String): Boolean;

  // returns true if the String ends with the provides one
  // this is case SENSITIVE
  Function StrEndsWith(const aReceiver: String; const anEndString: String): Boolean;

  // returns true if the String ends with the provides one
  // this is case INsensitive
  Function StrEndsWithIgnoringCase(const aReceiver: String; const anEndString: String): Boolean;

  // the IntToStr generates wrong results
  // in normal cases IntToStr returns a negative value
  // and somtimes completly wrong values
  Function LongWordToStr(const aLongWord: LongWord) : String;

  Function BoolToStr(const aBoolean : boolean ): string;

  // Returns aString enclosed in double quotes
  Function StrInDoubleQuotes(const aString : String) : String;


Implementation

  uses
    DebugUnit;

  constructor TSerializableStringList.Create;
  begin
    LogEvent(LogObjConstDest, 'TSerializableStringList createdestroy');

    inherited Create;
    stringList := TStringList.Create;
  end;


  destructor TSerializableStringList.Destroy;
  begin
    LogEvent(LogObjConstDest, 'TSerializableStringList destroy');
    if Nil <> stringList then stringList.Destroy;

    inherited Destroy;
  end;


  FUNCTION TSerializableStringList.getCount : LongInt;
  begin
    result := stringList.count;
  end;


  PROCEDURE TSerializableStringList.add(const aString : String);
  begin
    stringList.add(aString);
  end;

  FUNCTION TSerializableStringList.get(const anIndex : LongInt) : String;
  begin
    result := stringList[anIndex];
  end;

  FUNCTION TSerializableStringList.getSerializedString : String;
  Var
    i : Integer;
  begin
    result := '';
    for i := 0 to stringList.count-1 do
    begin
      if (i > 0) then result := result + '&';
      result := result + StrEscapeAllCharsBy(stringList[i], ['&'], '\');
    end;
  end;


  PROCEDURE TSerializableStringList.readValuesFromSerializedString(const aSerializedString : String);
  Begin
    if (length(aSerializedString) < 1) then exit;

    LogEvent(LogObjConstDest, 'readValuesFromSerializedString');
    stringList.Destroy;
    LogEvent(LogObjConstDest, 'readValuesFromSerializedString destroy done');
    stringList := TStringList.Create;
    StrExtractStrings(stringList, aSerializedString, ['&'], '\');
  end;


  // ----------------------------------------------------------


  Function StrEscapeAllCharsBy(Const aReceiver: String; const aSetOfChars: TSetOfChars; const anEscapeChar: char): String;
  Var
    i : Integer;
    tmpChar : Char;
  Begin
    Result := '';

    for i := 1 To length(aReceiver) do
    begin
      tmpChar := aReceiver[i];

      if (tmpChar = anEscapeChar) or (tmpChar IN aSetOfChars) then
        result := result + anEscapeChar + tmpChar
      else
        result := result + tmpChar;
    end;
  end;


  Procedure PrivateStrExtractStrings(   Var aResult: TStrings;
                                        const aReceiver: String;
                                        const aSetOfChars: TSetOfChars;
                                        const anEscapeChar: char;
                                        const anIgnoreEmptyFlag : boolean);
  Var
    i : Integer;
    tmpChar,tmpNextChar : Char;
    tmpPart: String;
  Begin
    if (length(aReceiver) < 1) then exit;

    tmpPart := '';

    i := 1;
    while i <= length(aReceiver) do
    begin
      tmpChar := aReceiver[i];
      if i < length(aReceiver) then
        tmpNextChar := aReceiver[i+1]
      else
        tmpNextChar := #0;

      if (tmpChar = anEscapeChar) and (tmpNextChar = anEscapeChar) then
      begin
        tmpPart := tmpPart + anEscapeChar;
        i := i + 2;
      end
      else
        if (tmpChar = anEscapeChar) and (tmpNextChar IN aSetOfChars) then
        begin
          tmpPart := tmpPart + tmpNextChar;
          i := i + 2;
        end
        else
          if (tmpChar IN aSetOfChars) then
          begin
            if (NOT anIgnoreEmptyFlag) OR ('' <> tmpPart) then
            begin
              aResult.add(tmpPart);
            end;
            tmpPart := '';
            i := i + 1;
          end
          else
            begin
            tmpPart := tmpPart + tmpChar;
            i := i + 1;
          end;
    end;

    if (NOT anIgnoreEmptyFlag) OR ('' <> tmpPart) then
    begin
      aResult.add(tmpPart);
    end;
  end;


  Procedure StrExtractStrings(Var aResult: TStrings; Const aReceiver: String; const aSetOfChars: TSetOfChars; const anEscapeChar: char);
  Begin
    PrivateStrExtractStrings(aResult, aReceiver, aSetOfChars, anEscapeChar, false);
  end;


  Procedure StrExtractStringsIgnoreEmpty(Var aResult: TStrings; Const aReceiver: String; const aSetOfChars: TSetOfChars; const anEscapeChar: char);
  Begin
    PrivateStrExtractStrings(aResult, aReceiver, aSetOfChars, anEscapeChar, true);
  end;


  Function StrTrimLeftChars(const aReceiver: String; const aSetOfChars: TSetOfChars): String;
  Var
    i : Longint;
  Begin
    i := 1;
    // mem optimization
    if aReceiver[i] in aSetOfChars then
    begin
      while i <= Length(aReceiver) do
      begin
        if aReceiver[i] in aSetOfChars then
          inc(i)
        else
          break;
      end;
      result := Copy(aReceiver, i, Length(aReceiver)-i+1);
    end
    else
    begin
      result := aReceiver;
    end;
  end;


  Function StrTrimRightChars(const aReceiver: String; const aSetOfChars: TSetOfChars): String;
  Var
    i : Longint;
  Begin
    i := Length(aReceiver);

    // mem optimization
    if aReceiver[i] in aSetOfChars then
    begin
      while i > 0 do
      begin
        if aReceiver[i] in aSetOfChars then
          dec(i)
        else
          break;
      end;
      result := Copy(aReceiver, 1, i);
    end
    else
    begin
      result := aReceiver;
    end;
  end;


  Function StrTrimChars(const aReceiver: String; const aSetOfChars: TSetOfChars): String;
  Var
    i : Longint;
    j : Longint;
    tmpNeedCopy : boolean;
  Begin
    tmpNeedCopy := false;
    i := 1;
    while i < Length(aReceiver) do
    begin
      if aReceiver[i] in aSetOfChars then
      begin
        inc(i);
        tmpNeedCopy := true;
      end
      else
      begin
        break;
      end;
    end;

    j := Length(aReceiver);
    while j >= i do
    begin
      if aReceiver[j] in aSetOfChars then
      begin
        dec(j);
        tmpNeedCopy := true;
      end
      else
      begin
        break;
      end;
    end;

    if tmpNeedCopy then
    begin
      result := Copy(aReceiver, i, j-i+1);
    end
    else
    begin
      result := aReceiver;
    end;
  end;


  Function StrTrim(const aReceiver: String): String;
  Begin
    result := StrTrimChars(aReceiver, [' ']);
  end;


  Function StrLeft(const aString : String; const aCount : Integer) : String;
  Begin
    if aCount >= Length(aString) then
      Result := aString
    else
      Result := copy(aString, 1, aCount);
  end;


  Function StrLeftWithout(const aString : String; const aCount : Integer) : String;
  Begin
    Result:= copy(aString, 1, length(aString) - aCount );
  End;


  Function StrLeftUntil(const aReceiver: String; const aSetOfChars: TSetOfChars) : String;
  Var
    i : integer;
  Begin
    Result := aReceiver;

    for i := 1 To Length(aReceiver) do
    begin
      if aReceiver[i] in aSetOfChars then
      begin
        Result := Copy(aReceiver, 1, i-1 );
        break;
      end;
    end;
  end;


  Function StrStartsWith(const aReceiver: String; const aStartString: String) : Boolean;
  Var
    tmpStringPos : integer;
    tmpStartStringLength : integer;
  Begin
    tmpStartStringLength := Length(aStartString);

    if Length(aReceiver) < tmpStartStringLength then
    begin
      result := false;
      exit;
    end;

    for tmpStringPos := 1 to tmpStartStringLength do
    begin
      if aReceiver[tmpStringPos] <> aStartString[tmpStringPos] then
      begin
        result := false;
        exit;
      end;
    end;

    result := true;
  end;


  Function StrStartsWithIgnoringCase(const aReceiver: String; const aStartString: String) : Boolean;
  Var
    tmpStringPos : integer;
    tmpStartStringLength : integer;
  Begin
    tmpStartStringLength := Length(aStartString);

    if Length(aReceiver) < tmpStartStringLength then
    begin
      result := false;
      exit;
    end;

    for tmpStringPos := 1 to tmpStartStringLength do
    begin
      if UpCase(aReceiver[tmpStringPos]) <> UpCase(aStartString[tmpStringPos]) then
      begin
        result := false;
        exit;
      end;
    end;

    result := true;
  end;


  Function StrEndsWith(const aReceiver: String; const anEndString: String): Boolean;
  Var
    tmpStringPos : Longint;
    tmpMatchPos : Longint;
  Begin
    tmpStringPos := length(aReceiver);
    tmpMatchPos := length(anEndString);

    if tmpMatchPos > tmpStringPos then
    begin
      result := false;
      exit;
    end;

    while tmpMatchPos > 0 do
    begin
      if aReceiver[tmpStringPos] <> anEndString[tmpMatchPos] then
      begin
        result := false;
        exit;
      end;
      dec(tmpMatchPos);
      dec(tmpStringPos);
    end;

    result := true;
  end;


  Function StrEndsWithIgnoringCase(const aReceiver: String; const anEndString: String): Boolean;
  Var
    tmpStringPos : Longint;
    tmpMatchPos : Longint;
  Begin
    tmpStringPos := length(aReceiver);
    tmpMatchPos := length(anEndString);

    if tmpMatchPos > tmpStringPos then
    begin
      result := false;
      exit;
    end;

    while tmpMatchPos > 0 do
    begin
      if upcase(aReceiver[tmpStringPos]) <> upcase(anEndString[tmpMatchPos]) then
      begin
        result := false;
        exit;
      end;
      dec(tmpMatchPos);
      dec(tmpStringPos);
    end;

    result := true;
  end;


  Function LongWordToStr(const aLongWord: LongWord) : String;
  Var
    l : LongWord;
    i : Integer;
  Begin
    Result := '';
    l := aLongWord;

    if l = 0 then
    begin
      result := '0';
      exit;
    end;

    while l > 0 do
    begin
      i := l mod 10;
      l := l div 10;
      Case i of
           0 : result := '0' + result;
           1 : result := '1' + result;
           2 : result := '2' + result;
           3 : result := '3' + result;
           4 : result := '4' + result;
           5 : result := '5' + result;
           6 : result := '6' + result;
           7 : result := '7' + result;
           8 : result := '8' + result;
           9 : result := '9' + result;
      end;
    end;

  end;


  Function BoolToStr(const aBoolean : boolean ): string;
  begin
    if aBoolean then
      Result := 'True'
    else
      Result := 'False';
  end;

  Function StrInDoubleQuotes(const aString : String) : String;
  begin
    Result := DoubleQuote + aString + DoubleQuote;
  end;


END.
