Unit StringUtilsUnit;

// NewView - a new OS/2 Help Viewer
// Copyright 2006-2007 Ronald Brill (rbri at rbri dot de)
// This software is released under the GNU Public License - see readme.txt

// Helper functions to work with String and AnsiString

Interface

uses
  Classes,
  CharUtilsUnit;

const
  StrTAB = CharTAB;
  StrCR = CharCR;
  StrLF = CharLF;
  StrCRLF = StrCR + StrLF;
  StrSingleQuote = '''';
  StrDoubleQuote = '"';


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

  // prefices all occurences of one of the chars in aStringWithChars with anEscape char
  // if the escapeChar itself is found, then it is doubled
  Function StrEscapeAllCharsBy(const aReceiver: String; const aSetOfChars: TSetOfChars; const anEscapeChar: char): String;

  // Extract all fields in a String given a set of delimiter characters and
  // an optional escape character usable to escape field delimits.
  // Example:
  //     StrExtractStrings('1x2x3\x4', 'x', '\') ->
  //     returns 4 strings: '1', '', '2' and '3x4'
  Procedure StrExtractStrings(Var aResult : TStrings; const aReceiver: String; const aSetOfChars: TSetOfChars; const anEscapeChar: char);

  // same as StrExtractStrings but ignores empty strings
  Procedure StrExtractStringsIgnoreEmpty(Var aResult : TStrings; const aReceiver: String; const aSetOfChars: TSetOfChars; const anEscapeChar: char);

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

  // Returns a copy of the string starting at aPos
  Function StrSubstringFrom(const aReceiver: String; const aPos : Integer) : String;

  // returns true if the String starts with the provided one
  // this is case SENSITIVE
  Function StrStartsWith(const aReceiver: String; const aStartString: String): Boolean;

  // returns true if the String starts with the provided one
  // this is case INsensitive
  Function StrStartsWithIgnoringCase(const aReceiver: String; const aStartString: String): Boolean;

  // returns true if the String ends with the provides one
  // this is case SENSITIVE
  Function StrEndsWith(const aReceiver: String; const anEndString: String): Boolean;

  // returns true if the String ends with the provided one
  // this is case INsensitive
  Function StrEndsWithIgnoringCase(const aReceiver: String; const anEndString: String): Boolean;

  // returns true if the Strings are the same
  // this is case INsensitive
  Function StrEqualIgnoringCase(const aReceiver: String; const aSecondString: String): Boolean;

  // the IntToStr generates wrong results
  // in normal cases IntToStr returns a negative value
  // and somtimes completly wrong values
  Function LongWordToStr(const aLongWord: LongWord) : String;

  Function BoolToStr(const aBoolean : boolean ): string;

  // Returns aString enclosed in double quotes
  Function StrInDoubleQuotes(const aString : String) : String;

  // Extract all fields in a String delimited by whitespace (blank or tab).
  // use double quotes if you need blanks in the strings
  Procedure StrExtractStringsQuoted(Var aResult: TStrings; const aReceiver: String );


  // returns the position of aPart in aString
  // case insensitive
  Function CaseInsensitivePos(const aPart: String; const aString: String ): longint;


  // --------------------
  // ---- AnsiString ----
  // --------------------

  
  // removes all occurences of char from aSetOfChars from the beginning
  // of a String.
  Function AnsiStrTrimLeftChars(const aReceiver: AnsiString; const aSetOfChars: TSetOfChars): AnsiString;

  // removes all occurences of char from aSetOfChars from the end
  // of a String.
  Function AnsiStrTrimRightChars(const aReceiver: AnsiString; const aSetOfChars: TSetOfChars): AnsiString;

  // removes all occurences of char from aSetOfChars from the beginning
  // end the end of a String.
  Function AnsiStrTrimChars(const aReceiver: AnsiString; const aSetOfChars: TSetOfChars): AnsiString;

  // removes all blanks from beginning and end
  Function AnsiStrTrim(const aReceiver: AnsiString): AnsiString;




Implementation

  uses
    SysUtils,
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
    tmpLength : integer;
    i : integer;
  Begin
    tmpLength := Length(aReceiver);

    if 1 > tmpLength then
    begin
      result := aReceiver;
      exit;
    end;

    i := 1;
    // mem optimization
    if aReceiver[i] in aSetOfChars then
    begin
      while i <= tmpLength do
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
    i : integer;
  Begin
    i := Length(aReceiver);

    if 1 > i then
    begin
      result := aReceiver;
      exit;
    end;

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
    i,j : integer;
    tmpNeedCopy : boolean;
  Begin
    j := Length(aReceiver);

    if 1 > j then
    begin
      result := aReceiver;
      exit;
    end;

    tmpNeedCopy := false;
    i := 1;
    while i < j do
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


  Function StrSubstringFrom(const aReceiver: String; const aPos : Integer) : String;
  Begin
    Result := copy(aReceiver, aPos, length(aReceiver) - aPos + 1);
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


  Function StrEqualIgnoringCase(const aReceiver: String; const aSecondString: String): Boolean;
  begin
    Result := CompareText(aReceiver, aSecondString) = 0;
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
    Result := StrDoubleQuote + aString + StrDoubleQuote;
  end;


  Procedure StrExtractStringsQuoted(Var aResult: TStrings; const aReceiver: String );
  Var
    tmpState : (WHITESPACE, INSIDE, START_QUOTE, INSIDE_QUOTED, INSIDE_QUOTED_START_QUOTE);
    tmpCurrentParsePosition : Integer;
    tmpCurrentChar : Char;
    tmpPart : String;

  Begin
    if (length(aReceiver) < 1) then exit;

    tmpState := WHITESPACE;
    tmpPart := '';

    tmpCurrentParsePosition := 1;

    for tmpCurrentParsePosition:=1 to length(aReceiver) do
    begin
      tmpCurrentChar := aReceiver[tmpCurrentParsePosition];

      Case tmpCurrentChar of
        ' ', StrTAB :
        begin

          Case tmpState of

            WHITESPACE :
            begin
              // nothing
            end;

            INSIDE :
            begin
              aResult.add(tmpPart);
              tmpPart := '';
              tmpState := WHITESPACE;
            end;

            INSIDE_QUOTED :
            begin
              tmpPart := tmpPart + tmpCurrentChar;
            end;

            START_QUOTE :
            begin
              tmpPart := tmpPart + tmpCurrentChar;
              tmpState := INSIDE_QUOTED;
            end;

            INSIDE_QUOTED_START_QUOTE :
            begin
              aResult.add(tmpPart);
              tmpPart := '';
              tmpState := WHITESPACE;
            end;
          end;
        end;

        StrDoubleQuote :
        begin

          Case tmpState of

            WHITESPACE :
            begin
              tmpState := START_QUOTE;
            end;

            INSIDE :
            begin
              aResult.add(tmpPart);
              tmpPart := '';
              tmpState := START_QUOTE;
            end;

            INSIDE_QUOTED :
            begin
              tmpState := INSIDE_QUOTED_START_QUOTE;
            end;

            START_QUOTE :
            begin
              tmpState := INSIDE_QUOTED_START_QUOTE;
            end;

            INSIDE_QUOTED_START_QUOTE :
            begin
              tmpPart := tmpPart + tmpCurrentChar;
              tmpState := INSIDE_QUOTED;
            end;
          end;
        end;

        else
        begin
          Case tmpState of

            WHITESPACE :
            begin
              tmpPart := tmpPart + tmpCurrentChar;
              tmpState := INSIDE;
            end;

            INSIDE, INSIDE_QUOTED :
            begin
              tmpPart := tmpPart + tmpCurrentChar;
            end;

            START_QUOTE :
            begin
              tmpPart := tmpPart + tmpCurrentChar;
              tmpState := INSIDE_QUOTED;
            end;

            INSIDE_QUOTED_START_QUOTE :
            begin
              aResult.add(tmpPart);
              tmpPart := tmpCurrentChar;
              tmpState := INSIDE;
            end;
          end;
        end;

      end;
    end;

    Case tmpState of
      WHITESPACE, START_QUOTE : {nothing to do};

      INSIDE, INSIDE_QUOTED, INSIDE_QUOTED_START_QUOTE :
      begin
        aResult.add(tmpPart);
      end;
    end;
  end;


  Function CaseInsensitivePos(const aPart: String; const aString: String) : longint;
  Var
    EndOfPart: longword;
  Begin
    // Result := Pos(UpperCase(aPart), Uppercase(aString));

    // Aarons assembler version :-)
    Asm
    //Locals:
    //a at [EBP+12]
    //b at [EBP+8]

    // First get and check lengths
    MOV   ESI, aPart     // get address of aPart into ESI
    MOV   CL,  [ESI]     // get length of aPart
    CMP   CL, 0          // if aPart is empty then return null to simulate the behavior of POS
    JE    !CIP_NoMatch

    MOV   EDI, aString   // get address of aString into EDI
    MOV   DL,  [EDI]     // get length of aString
    CMP   CL,  DL
    JBE   !CIP_PartFitsInString

    // aParta longer than aString so aPart can't be in aString

    !CIP_NoMatch:
      MOV   EAX, 0
      LEAVE
      RETN32 8

    !CIP_PartFitsInString:
      INC   ESI            // skip length byte in aPart
      INC   EDI            // skip length byte of aString

    // get ending address of b into EDX
      MOVZX EDX, DL        // widen DL
      ADD   EDX, EDI       // add start of aString

    // get ending address of a into EndOfA
      MOVZX ECX, CL        // widen CL
      ADD   ECX, ESI       // add start of aPart
      MOV   EndOfPart, ECX    // store to EndOfPart

      MOV   ECX, EDI       // set start of current match to start of b

      // ESI: current search point in a
      // EDI: current search point in b
      // EDX: end of b
      // ECX: start of current match
      // available: eax, ebx

      JMP   !CIP_Loop

    !CIP_LoopStart:
      CMP   EDI, EDX
      JE    !CIP_NoMatch   // run out of b

      MOV   AL,  [ESI]     // get next char of a
      INC   ESI            // next in a

      MOV   BL,  [EDI]     // get next char of b
      INC   EDI            // next in b

    // Convert chars to uppercase
      CMP   AL,  97
      JB    !CIP_Upcase1
      CMP   AL,  122
      JA    !CIP_Upcase1
      SUB   AL,  32         // convert lower to upper
    !CIP_Upcase1:

      CMP   BL,97
      JB    !CIP_Upcase2
      CMP   BL,122
      JA    !CIP_Upcase2
      SUB   BL,32          // convert lower to upper
    !CIP_Upcase2:

    // Compare uppercased chars
      CMP   AL,BL
      JE    !CIP_Loop

    // different.

    // Back to start of match + 1
      INC   ECX            // inc start of match
      MOV   EDI, ECX       // back to start of match in b
      MOV   ESI, aPart     // back to start of aPart
      INC   ESI            // skip length
      JMP   !CIP_LoopStart

    !CIP_Loop:

    // same
      CMP   ESI, EndOfPart    // have we reached the end of a
      JB    !CIP_LoopStart

      // Match, return position
      SUB   ECX, [EBP+8]   // position = ( start of match ) - ( start of b ) + 1
      MOV   EAX, ECX
      LEAVE
      RETN32 8
    end;
  end;


  // --------------------
  // ---- AnsiString ----
  // --------------------


  Function AnsiStrTrimLeftChars(const aReceiver: AnsiString; const aSetOfChars: TSetOfChars): AnsiString;
  Var
    tmpLength : integer;
    i : integer;
  Begin
    tmpLength := Length(aReceiver);

    if 1 > tmpLength then
    begin
      result := aReceiver;
      exit;
    end;

    i := 1;
    // mem optimization
    if aReceiver[i] in aSetOfChars then
    begin
      while i <= tmpLength do
      begin
        if aReceiver[i] in aSetOfChars then
          inc(i)
        else
          break;
      end;
      result := AnsiCopy(aReceiver, i, Length(aReceiver)-i+1);
    end
    else
    begin
      result := aReceiver;
    end;
  end;


  Function AnsiStrTrimRightChars(const aReceiver: AnsiString; const aSetOfChars: TSetOfChars): AnsiString;
  Var
    i : integer;
  Begin
    i := Length(aReceiver);

    if 1 > i then
    begin
      result := aReceiver;
      exit;
    end;

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
      result := AnsiCopy(aReceiver, 1, i);
    end
    else
    begin
      result := aReceiver;
    end;
  end;


  Function AnsiStrTrimChars(const aReceiver: AnsiString; const aSetOfChars: TSetOfChars): AnsiString;
  Var
    i,j : integer;
    tmpNeedCopy : boolean;
  Begin
    tmpNeedCopy := false;

    j := Length(aReceiver);

    if 1 > j then
    begin
      result := aReceiver;
      exit;
    end;

    i := 1;
    while i < j do
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
      result := AnsiCopy(aReceiver, i, j-i+1);
    end
    else
    begin
      result := aReceiver;
    end;
  end;


  Function AnsiStrTrim(const aReceiver: AnsiString): AnsiString;
  Begin
    result := AnsiStrTrimChars(aReceiver, [' ']);
  end;


END.
