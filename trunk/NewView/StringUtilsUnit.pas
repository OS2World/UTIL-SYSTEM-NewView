Unit StringUtilsUnit;

// NewView - a new OS/2 Help Viewer
// Copyright 2006 Ronald Brill (rbri at rbri dot de)
// This software is released under the Gnu Public License - see readme.txt

// Helper functions to work with strings

Interface

uses
 Classes;

const
  StrCR = chr(13);
  StrLF = chr(10);
  StrCRLF = StrCR + StrLF;


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
  //     StrExtractStrings('1x2x3\x4', "x", '\') ->
  //     returns 4 strings: "1", "", "2" and "3x4"
  Procedure StrExtractStrings(Var aResult : TStringList; Const aReceiver: String; const aSetOfChars: TSetOfChars; const anEscapeChar: char);

  // removes all occurences of char from aSetOfChars from the beginning
  // end the end of a String.
  Function StrTrimChars(const aReceiver: String; const aSetOfChars: TSetOfChars): String;

  // removes all blanks from beginning and end
  Function StrTrim(const aReceiver: String): String;

  // returns true if the String ends with the provides one
  // this is case SENSITIVE
  Function StrEndsWith(const aReceiver: String; const anEndString: String): Boolean;

  // returns true if the String ends with the provides one
  // this is case INsensitive
  Function StrEndsWithIgnoringCase(const aString: String; const anEndString: String): Boolean;


Implementation

  constructor TSerializableStringList.Create;
  begin
    inherited Create;
    stringList := TStringList.Create;
  end;


  destructor TSerializableStringList.Destroy;
  begin
    stringList.Destroy;
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
    for i := 0 To stringList.count-1 do
    begin
      if (i > 0) then result := result + '&';
      result := result + StrEscapeAllCharsBy(stringList[i], ['&'], '\');
    end;
  end;


  PROCEDURE TSerializableStringList.readValuesFromSerializedString(const aSerializedString : String);
  Begin
    if (length(aSerializedString) < 1) then exit;

    stringList.destroy;
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


  Procedure StrExtractStrings(Var aResult: TStringList; Const aReceiver: String; const aSetOfChars: TSetOfChars; const anEscapeChar: char);
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
            aResult.add(tmpPart);
            tmpPart := '';
            i := i + 1;
          end
          else
            begin
            tmpPart := tmpPart + tmpChar;
            i := i + 1;
          end;
    end;
    aResult.add(tmpPart);
  end;


  Function StrTrimChars(const aReceiver: String; const aSetOfChars: TSetOfChars): String;
  Var
    i : Longint;
    j : Longint;
  Begin
    i := 1;
    while i < Length(aReceiver) do
    begin
      if aReceiver[i] in aSetOfChars then
        inc(i)
       else
         break;
    end;

    j := Length(aReceiver);
    while j >= i do
    begin
      if aReceiver[j] in aSetOfChars then
        dec(j)
      else
        break;
    end;

    result := Copy(aReceiver, i, j-i+1);
  end;


  Function StrTrim(const aReceiver: String): String;
  Begin
    result := StrTrimChars(aReceiver, [' ']);
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
    tmpStringPos := length(aString);
    tmpMatchPos := length(anEndString);

    if tmpMatchPos > tmpStringPos then
    begin
      result := false;
      exit;
    end;

    while tmpMatchPos > 0 do
    begin
      if upcase(aString[tmpStringPos]) <> upcase(anEndString[tmpMatchPos]) then
      begin
        result := false;
        exit;
      end;
      dec(tmpMatchPos);
      dec(tmpStringPos);
    end;

    result := true;
  end;

END.
