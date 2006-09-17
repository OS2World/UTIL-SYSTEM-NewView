Unit StringUtilsUnit;

// NewView - a new OS/2 Help Viewer
// Copyright 2006 Ronald Brill (rbri at rbri dot de)
// This software is released under the Gnu Public License - see readme.txt

// Helper functions to work with strings

Interface

uses
 Classes;

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
  Function escapeAllCharsBy(Const aReceiver: String; const aStringWithChars: String; const anEscapeChar: char): String;

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
      result := result + escapeAllCharsBy(stringList[i], '&', '\');
    end;
  end;


  PROCEDURE TSerializableStringList.readValuesFromSerializedString(const aSerializedString : String);
  Var
    i : Integer;
    tmpChar,tmpNextChar : Char;
    tmpPart: String;
  begin
    if (length(aSerializedString) < 1) then exit;

    stringList.destroy;
    stringList := TStringList.Create;
    tmpPart := '';

    i := 1;
    while i <= length(aSerializedString) do
    begin
      tmpChar := aSerializedString[i];
      if i < length(aSerializedString) then
        tmpNextChar := aSerializedString[i+1]
      else
        tmpNextChar := #0;

      if (tmpChar = '\') and (tmpNextChar = '\') then
      begin
        tmpPart := tmpPart + '\';
        i := i + 2;
      end
      else
        if (tmpChar = '\') and (tmpNextChar = '&') then
        begin
          tmpPart := tmpPart + '&';
          i := i + 2;
        end
        else
          if (tmpChar = '&') then
          begin
            stringList.add(tmpPart);
            tmpPart := '';
            i := i + 1;
          end
          else
            begin
            tmpPart := tmpPart + tmpChar;
            i := i + 1;
          end;
    end;
    stringList.add(tmpPart);
  end;

  // ----------------------------------------------------------

  Function escapeAllCharsBy(Const aReceiver: String; const aStringWithChars: String; const anEscapeChar: char): String;
  Var
    i : Integer;
    tmpChar : Char;
  Begin
    Result := '';

    if (length(aStringWithChars) > 0) then
    begin
      for i := 1 To length(aReceiver) do
      begin
        tmpChar := aReceiver[i];

        if ((tmpChar = anEscapeChar) or (pos(tmpChar, aStringWithChars) > 0)) then
          result := result + anEscapeChar + tmpChar
        else
          result := result + tmpChar;
      end;
    end;
  end;

END.
