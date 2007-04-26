Unit CharUtilsUnit;

// NewView - a new OS/2 Help Viewer
// Copyright 2006-2007 Ronald Brill (rbri at rbri dot de)
// This software is released under the GNU Public License - see readme.txt

// Helper functions to work with characters

Interface

uses
  Classes;

const
  CharTAB = chr(9);
  CharCR = chr(13);
  CharLF = chr(10);


  TYPE
    TSetOfChars = set of char;

  // Returns true if aChar is a digit 0..9
  Function CharIsDigit(const aChar : char) : boolean;

  // Returns true if aChar is an alphabetic character a..z A..Z
  Function CharIsAlpha(const aChar : char) : boolean;


  // ---------------
  // ---- PChar ----
  // ---------------

  
  // Converts a PChar into a String like StrPas
  // but conversts at least the first aLength chars
  Function StrPasWithLength(const aPChar: PChar; const aLength: integer) : String;

  // Returns the difference of the pointers
  Function PCharPointerDiff(const aMinuend: PChar; const aSubtrahend : PChar)  : Longword;

Implementation

  uses
    SysUtils;

  Function CharIsDigit(const aChar : char) : boolean;
  begin
    Result := (aChar >= '0') and (aChar <= '9');
  end;

  Function CharIsAlpha(const aChar : char) : boolean;
  begin
    Result := ( (aChar >= 'A') and (aChar <= 'Z') ) or ((aChar >= 'a') and (aChar <= 'z'));
  end;


  // ---------------
  // ---- PChar ----
  // ---------------


  // Converts a PChar into a String like StrPas
  // but conversts at least the first aLength chars
  Function StrPasWithLength(const aPChar: PChar; const aLength: integer) : String;
  var
    i: integer;
  begin
    Result := '';
    i := 0;
    while (aPChar[i] <> #0) and (i < aLength) do
    begin
      Result := Result + aPChar[i];
      inc( i );
    end;
  end;


  Function PCharPointerDiff(const aMinuend: PChar; const aSubtrahend : PChar) : Longword;
  begin
    Result := Longword(aMinuend) - Longword(aSubtrahend);
  end;


END.
