Unit RichTextDocumentUnit;

// Declarations of tags, and parsing functions

Interface

uses
  Classes;

type
  TTagType = ( ttInvalid,
               ttBold, ttBoldOff,
               ttItalic, ttItalicOff,
               ttUnderline, ttUnderlineOff,
               ttFixedWidthOn, ttFixedWidthOff,
               ttHeading1, ttHeading2, ttHeading3, ttHeadingOff,
               ttColor, ttColorOff,
               ttBackgroundColor, ttBackgroundColorOff,
               ttRed, ttBlue, ttGreen, ttBlack,
               ttWrap,
               ttAlign,
               ttBeginLink, ttEndLink,
               ttSetLeftMargin, ttSetRightMargin,
               ttImage,
               ttFont, ttFontOff,
               ttEnd );

  TStandardColor = record
    Name: string[ 32 ];
    Color: TColor;
  end;

  TTag = record
    TagType: TTagType;
    Arguments: string;
  end;

  TTextElementType = ( teText,              // a character
                       teWordBreak,
                       teLineBreak,         // end of para
                       teTextEnd,
                       teImage,
                       teStyle,
                       teWrapChar,          // A non-whitespace wrappable character (ALT)
                       teLeadByte,          // DBCS lead byte (ALT)
                       teSecondByte );      // DBCS secondary byte (ALT)

  TTextElement = record
    ElementType: TTextElementType;
    Character: Char;
    Tag: TTag;
  end;

  TTextAlignment = ( taLeft,
                     taRight,
                     taCenter );

// Returns tag pointed to by TextPointer and
// moves TextPointer to the first char after the tag.
Function ExtractTag( Var TextPointer: PChar ): TTag;

// Returns tag ending at TextPointer
// (Expects textpointer is currently pointing at the >)
// and moves TextPointer to the first char of the tag
Function ExtractPreviousTag( const TextStart: PChar;
                             Var TextPointer: PChar ): TTag;

function ExtractNextTextElement( TextPointer: PChar;
                                 Var NextElement: PChar ): TTextElement;

function ExtractPreviousTextElement( const TextStart: PChar;
                                     TextPointer: PChar;
                                     Var NextElement: PChar ): TTextElement;

// Parse a color name or value (#hexval). Returns true if valid
function GetTagColor( const ColorParam: string;
                      var Color: TColor ): boolean;

function GetTagTextAlignment( const AlignParam: string;
                              const Default: TTextAlignment ): TTextAlignment;

function GetTagTextWrap( const WrapParam: string ): boolean;

// Search within a rich text document for the given text
// if found, returns true, pMatch is set to the first match,
//   and MatchLength returns the length of the match
//   (which may be greater than the length of Text due to
//    to skipping tags)
// if not found, returns false, pMatch is set to nil
function RichTextFindString( pRichText: PChar;
                             const Text: string;
                             var pMatch: PChar;
                             var MatchLength: longint ): boolean;

// Returns the start of the previous word,
// or the current word if pStart is in the middle of the word
function RichTextWordLeft( pRichText: PChar;
                           pStart: PChar ): PChar;

// Returns the start of the next word.
function RichTextWordRight( pStart: PChar ): PChar;

// If pStart is in the middle of a word, then
// returns true and sets the start and length of the word
function RichTextWordAt( pRichText: PChar;
                         pStart: PChar;
                         Var pWordStart: PChar;
                         Var WordLength: longint ): boolean;

// Copies plaintext of richtext starting at StartP
// to the given buffer. Returns number of characters copied.
// Buffer may be nil
// If BufferLength is negative, it is effectively ignored
function CopyPlainTextToBuffer( StartP: PChar;
                                EndP: PChar;
                                Buffer: PChar;
                                BufferLength: longint ): longint;


// ALT begins
//

// Check for special text element types and adjust value accordingly.
procedure CheckSpecialElementType( const Character:   Char;
                                   var   ElementType: TTextElementType;
                                   var   InsideDBC:   Boolean;
                                   const Codepage:    LongInt );

// Returns true if the given byte value is a legally-wrappable single-byte
// character under the given Asian codepage.
function IsAsianWrapChar( const CharByte: Byte;
                          const Codepage: LongInt ): boolean;

// Returns true if the given byte value is the leading byte of a multi-byte
// character under the given Asian codepage.
function IsDBCSLeadByte( const CharByte: Byte;
                         const Codepage: LongInt ): boolean;

// Returns true if the given byte value is valid as a possible second byte of
// a multi-byte character (this does not guarantee that it IS one, just that
// it COULD be).
function IsDBCSSecondByte( const CharByte: Byte;
                           const Codepage: LongInt ): boolean;
//
// ALT ends


Implementation

uses
  BseDOS, // for NLS/case mapping
  SysUtils,
  ACLStringUtility;

const
  TagStr: array[ ttInvalid .. ttEnd ] of string =
  (
    '', //
    'b',
    '/b',
    'i',
    '/i',
    'u',
    '/u',
    'tt',
    '/tt',
    'h1',
    'h2',
    'h3',
    '/h',
    'color',
    '/color',
    'backcolor',
    '/backcolor',
    'red',
    'blue',
    'green',
    'black',
    'wrap',
    'align',
    'link',
    '/link',
    'leftmargin',
    'rightmargin',
    'image',
    'font',
    '/font',
    ''
  );

  StandardColors: array[ 0..7 ] of TStandardColor =
  (
    ( Name : 'white' ; Color: clWhite   ),
    ( Name : 'black' ; Color: clBlack   ),
    ( Name : 'red'   ; Color: clRed     ),
    ( Name : 'blue'  ; Color: clBlue    ),
    ( Name : 'green' ; Color: clLime    ),
    ( Name : 'purple'; Color: clFuchsia ),
    ( Name : 'yellow'; Color: clYellow  ),
    ( Name : 'cyan'  ; Color: clAqua    )
  );

Procedure ParseTag( const Text: string;
                    Var Tag: TTag );
var
  TagType: TTagType;
  TagTypeText: string;
  SpacePos: longint;
begin
  SpacePos := Pos( ' ', Text );
  if SpacePos <> 0 then
  begin
    Tag.Arguments := trim( Copy( Text, SpacePos + 1, 255 ) );
    TagTypeText := LowerCase( Copy( Text, 1, SpacePos - 1 ) );
  end
  else
  begin
    Tag.Arguments := ''; // to save time copying when not needed
    TagTypeText := LowerCase( Text );
  end;

  for TagType := ttBold to ttEnd do
  begin
    if TagStr[ TagType ] = TagTypeText  then
    begin
      Tag.TagType := TagType;
      exit;
    end;
  end;

  // not found
  Tag.TagType := ttInvalid;
end;

var
  TagText: string;
  TagArgText: string;

Function ExtractTag( Var TextPointer: PChar ): TTag;
var
  CurrentChar: Char;
  TagTooLong: boolean;
  InQuote: boolean;
begin
//  assert( TextPointer[ 0 ] = '<' );
  TagText := '';
  TagTooLong := false;
  InQuote := false;

  repeat
    CurrentChar := TextPointer^;

    if     ( CurrentChar = '>' )
       and ( not InQuote ) then
    begin
      // found tag end.
      if TagTooLong then
        Result.TagType := ttInvalid
      else
        ParseTag( TagText, Result );
      inc( TextPointer );
      exit;
    end;

    if CurrentChar = #0 then
    begin
      // if we reach here we have reached the end of text
      // during a tag. invalid tag.
      Result.TagType := ttInvalid;
      exit;
    end;

    if CurrentChar = DoubleQuote then
    begin
      if not InQuote then
      begin
        InQuote := true
      end
      else
      begin
        // Could be escaped quote ""
        if ( TextPointer + 1 ) ^ = DoubleQuote then
        begin
          // yes it is
          inc( TextPointer ); // skip second one
        end
        else
        begin
          // no, not an escaped quote
          InQuote := false;
        end;
      end;

    end;

    if not TagTooLong then
      if Length( TagText ) < 200 then
        TagText := TagText + CurrentChar
      else
        TagTooLong := true; // but keep going until the end

    inc( TextPointer );
  until false;

end;

// Expects textpointer is currently pointing at the >
Function ExtractPreviousTag( const TextStart: PChar;
                             Var TextPointer: PChar ): TTag;
var
  CurrentChar: Char;
  TagTooLong: boolean;
  InQuote: boolean;
begin
  TagText := '';
  TagTooLong := false;
  InQuote := false;

  repeat
    dec( TextPointer );
    if TextPointer < TextStart then
    begin
      // if we reach here we have reached the end of text
      // during a tag. invalid tag.
      Result.TagType := ttInvalid;
      exit;
    end;
    CurrentChar := TextPointer^;

    if     ( CurrentChar = '<' )
       and ( not InQuote ) then
    begin
      // found tag end.
      if TagTooLong then
        Result.TagType := ttInvalid
      else
        ParseTag( TagText, Result );
      exit;
    end;

    if CurrentChar = DoubleQuote then
    begin
      if not InQuote then
      begin
        InQuote := true
      end
      else
      begin
        // Could be escaped quote ""
        if TextPointer <= TextStart then
        begin
          // start of text... somethin weird
          InQuote := false;
        end
        else if ( TextPointer - 1 ) ^ = DoubleQuote then
        begin
          // yes it is
          dec( TextPointer ); // skip second one
        end
        else
        begin
          // no, not an escaped quote
          InQuote := false;
        end;
      end;

    end;

    if not TagTooLong then
      if Length( TagText ) < 200 then
        TagText := CurrentChar + TagText
      else
        TagTooLong := true; // but keep going until the end

  until false;

end;

function ExtractNextTextElement( TextPointer: PChar;
                                 Var NextElement: PChar ): TTextElement;
var
  TheChar: Char;
  NextChar: Char;
begin
  with Result do
  begin
    TheChar := TextPointer^;
    Character := TheChar;
    inc( TextPointer );


    case TheChar of
      ' ': // ---- Space (word break) found ----
        ElementType := teWordBreak;

      #10, #13: // ---- End of line found ----
      begin
        ElementType := teLineBreak;
        if TheChar = #13 then
        begin
          TheChar := TextPointer^;
          if TheChar = #10 then
            // skip CR following LF
            inc( TextPointer );
        end;
      end;

      #0: // ---- end of text found ----
        ElementType := teTextEnd;

      '<': // ---- tag found? ----
      begin
        NextChar := TextPointer^;
        if NextChar = '<' then
        begin
          // no. just a literal <
          ElementType := teText;
          inc( TextPointer );
        end
        else
        begin
          Tag := ExtractTag( TextPointer );
          if Tag.TagType = ttImage then
            ElementType := teImage
          else
            ElementType := teStyle;
        end;

      end;

      '>': // check - should be double
      begin
        ElementType := teText;
        NextChar := TextPointer^;
        if NextChar = '>' then
          inc( TextPointer );
      end;

//    '-': // ---- Hyphen (ALT)
//      ElementType := teWrapChar;

      else
        ElementType := teText;
    end;

  end; // with

  NextElement := TextPointer;
end;

function ExtractPreviousTextElement( const TextStart: PChar;
                                     TextPointer: PChar;
                                     Var NextElement: PChar ): TTextElement;
var
  TheChar: Char;
  PreviousChar: Char;
  FoundTag: boolean;
begin
  with Result do
  begin
    dec( TextPointer );
    TheChar := TextPointer^;
    Character := TheChar;
    if TextPointer < TextStart then
    begin
      ElementType := teTextEnd;
      exit;
    end;

    case TheChar of
      ' ': // ---- Space (word break) found ----
        ElementType := teWordBreak;

      #10, #13: // ---- End of line found ----
      begin
        ElementType := teLineBreak;
        if TheChar = #10 then
        begin
          dec( TextPointer );
          TheChar := TextPointer^;
          if TheChar = #13 then
          begin
            // skip CR preceeding LF
          end
          else
            inc( TextPointer );
        end;
      end;

      '>': // ---- tag found ----
      begin
        FoundTag := true;
        if TextPointer > TextStart then
        begin
          PreviousChar := ( TextPointer - 1 )^;
          if PreviousChar = '>' then
          begin
            // no. just a literal >
            FoundTag := false;
            ElementType := teText;
            dec( TextPointer );
          end
        end;

        if FoundTag then
        begin
          Tag := ExtractPreviousTag( TextStart, TextPointer );
          if Tag.TagType = ttImage then
            ElementType := teImage
          else
            ElementType := teStyle;
        end;
      end;

      '<': // should be double
      begin
        ElementType := teText;
        if TextPointer > TextStart then
        begin
          PreviousChar := TextPointer^;
          if PreviousChar = '<' then
            dec( TextPointer );
        end;
      end

//    '-': // ---- Hyphen (ALT)
//      ElementType := teWrapChar;

      else
        ElementType := teText;
    end;
  end; // with
  NextElement := TextPointer;
end;

function GetTagColor( const ColorParam: string;
                      var Color: TColor ): boolean;
var
  ColorIndex: longint;
begin
  Result := false;
  if ColorParam <> '' then
  begin
    if ColorParam[ 1 ] = '#' then
    begin
      try
        Color := HexToInt( StrRightFrom( ColorParam, 2 ) );
        Result := true;
      except
      end;
    end
    else
    begin
      for ColorIndex := 0 to High( StandardColors ) do
      begin
        if StringsSame( ColorParam, StandardColors[ ColorIndex ].Name ) then
        begin
          Color := StandardColors[ ColorIndex ].Color;
          Result := true;
          break;
        end;
      end;
    end;
  end;
end;

function GetTagTextAlignment( const AlignParam: string;
                              const Default: TTextAlignment ): TTextAlignment;
begin
  if StringsSame( AlignParam, 'left' ) then
    Result := taLeft
  else if StringsSame( AlignParam, 'center' ) then
    Result := taCenter
  else if StringsSame( AlignParam, 'right' ) then
    Result := taRight
  else
    Result := Default;
end;

function GetTagTextWrap( const WrapParam: string ): boolean;
begin
  Result := StringsSame( WrapParam, 'yes' );
end;

function RichTextFindString( pRichText: PChar;
                             const Text: string;
                             var pMatch: PChar;
                             var MatchLength: longint ): boolean;
var
  P: PChar;
  NextP: PChar;
  Element: TTextElement;
  pMatchStart: pchar;
  pMatchStartNext: pchar;
  MatchIndex: longint;

  CountryData: COUNTRYCODE;
  CaseMap: array[ Low( Char )..High( Char ) ] of char;
  C: Char;
begin
  if Length( Text ) = 0 then
  begin
    // null string always matches
    Result := true;
    pMatch := pRichText;
    MatchLength := 0;
    exit;
  end;

  P := pRichText;

  MatchIndex := 1;

  // Get case mapping of all chars (only SBCS)

  CountryData.Country := 0; // default country
  CountryData.CodePage := 0; // default codepage

  // fill array with all chars
  for C := Low( CaseMap ) to High( CaseMap ) do
    CaseMap[ C ] := C;

  DosMapCase( sizeof( CaseMap ),
              CountryData,
              CaseMap );

  // Now search, case insensitively

  while true do
  begin
    Element := ExtractNextTextElement( P, NextP );

    case Element.ElementType of
      teTextEnd:
        // end of text
        break;

      teImage,
      teLineBreak:
        // breaks a potential match
        MatchIndex := 1;

      teStyle:
        ; // ignore, matches can continue

      else
      begin
        if   CaseMap[ Element.Character ]
           = CaseMap[ Text[ MatchIndex ] ] then
        begin
          // found a match
          if MatchIndex = 1 then
          begin
            pMatchStart := P; // store start of match
            pMatchStartNext := NextP;
          end;

          inc( MatchIndex );
          if MatchIndex > Length( Text ) then
          begin
            // found a complete match
            Result := true;
            pMatch := pMatchStart;
            MatchLength := PCharDiff( P, pMatchStart )
                           + 1; // include this char
            exit;
          end;
        end
        else
        begin
          // not a match
          if MatchIndex > 1 then
          begin
            // go back to start of match, + 1
            NextP := pMatchStartNext;
            MatchIndex := 1;
          end;
        end;
      end;
    end;

    P := NextP;
  end;

  // no match found
  Result := false;
  pMatch := nil;
  MatchLength := 0;
end;

function RichTextWordLeft( pRichText: PChar;
                           pStart: PChar ): PChar;
Var
  P: PChar;
  NextP: PChar;
  Element: TTextElement;
begin
  P := pStart;

  // skip whitespace/tags...
  Element := ExtractPreviousTextElement( pRichText, P, NextP );
  P := NextP;
  while Element.ElementType in [ teWordBreak, teLineBreak, teImage, teStyle ] do
  begin
    Element := ExtractPreviousTextElement( pRichText, P, NextP );
    P := NextP;
  end;
  if Element.ElementType = teTextEnd then
  begin
    Result := P;
    // out of text
    exit;
  end;

  // back to start of word, skip text/tags
  while true do
  begin
    Element := ExtractPreviousTextElement( pRichText, P, NextP );
    if not ( Element.ElementType in [ teText, teStyle ] ) then
      break;
    P := NextP;
  end;
  Result := P;
end;

function RichTextWordRight( pStart: PChar ): PChar;
Var
  P: PChar;
  NextP: PChar;
  Element: TTextElement;
begin
  P := pStart;

  // skip text/tags...
  Element := ExtractNextTextElement( P, NextP );
  while Element.ElementType in [ teStyle, teText ] do
  begin
    P := NextP;
    Element := ExtractNextTextElement( P, NextP );
  end;
  if Element.ElementType <> teTextEnd then
  begin
    // skip whitespace
    Element := ExtractNextTextElement( P, NextP );
    while Element.ElementType in [ teWordBreak, teLineBreak, teImage, teStyle ] do
    begin
      P := NextP;
      Element := ExtractNextTextElement( P, NextP );
    end;
  end;

  Result := P;
end;

function RichTextWordAt( pRichText: PChar;
                         pStart: PChar;
                         Var pWordStart: PChar;
                         Var WordLength: longint ): boolean;
Var
  P: PChar;
  NextP: PChar;
  Element: TTextElement;
  pWordEnd: PChar;
begin
  P := pStart;
  Element := ExtractNextTextElement( P, NextP );
  if not ( Element.ElementType in [ teStyle, teText ] ) then
  begin
    // not in a word.
    result := false;
    pWordStart := nil;
    WordLength := 0;
    exit;
  end;
  // find end of the word
  while Element.ElementType in [ teStyle, teText ] do
  begin
    P := NextP;
    Element := ExtractNextTextElement( P, NextP );
  end;
  pWordEnd := P;

  P := pStart;
  Element := ExtractPreviousTextElement( pRichText, P, NextP );
  while Element.ElementType in [ teStyle, teText ] do
  begin
    P := NextP;
    Element := ExtractPreviousTextElement( pRichText, P, NextP );
  end;
  pWordStart := P;
  WordLength := PCharDiff( pWordEnd, pWordStart );
  Result := true;
end;

function CopyPlainTextToBuffer( StartP: PChar;
                                EndP: PChar;
                                Buffer: PChar;
                                BufferLength: longint ): longint;
var
  Q: PChar;
  EndQ: Pchar;
  P: PChar;
  NextP: PChar;
  Element: TTextElement;
begin
  P := StartP;
  Q := Buffer;
  EndQ := Buffer + BufferLength;

  while P < EndP do
  begin
    Element := ExtractNextTextElement( P, NextP );
    case Element.ElementType of
      teText, teWordBreak:
      begin
        // copy char
        if Buffer <> nil then
          Q[ 0 ] := Element.Character;
        inc( Q );
      end;

      teLineBreak:
      begin
        if Buffer <> nil then
          Q[ 0 ] := #13;
        inc( Q );
        if Q = EndQ then
          // end of buffer
          break;

        if Buffer <> nil then
          Q[ 0 ] := #10;
        inc( Q );
      end;
    end;

   if Q = EndQ then
     // end of buffer
     break;

    P := NextP;
  end;

  Q[ 0 ] := #0;         // ALT - make sure string is terminated

  result := PCharDiff( Q, Buffer );
end;

// ALT begins
//
// Check for special text element types that depend on context.
//
procedure CheckSpecialElementType( const Character:   Char;
                                   var   ElementType: TTextElementType;
                                   var   InsideDBC:   Boolean;
                                   const Codepage:    LongInt );
var
  CharByte: Byte;
begin
  if Codepage in [ 874, 932, 936, 942, 943, 949, 950, 1381, 1386 ] then
  begin
    CharByte := ord( Character );
    if InsideDBC then
    begin
        InsideDBC := false;
        // sanity check for corrupt text sequence (definitely not foolproof)
        if IsDBCSSecondByte( CharByte, Codepage ) then
          ElementType := teSecondByte
        else
          ElementType := teText;
    end
    else
    begin
      if IsAsianWrapChar( CharByte, Codepage ) then
      begin
        ElementType := teWrapChar;
        InsideDBC := false;
      end
      else if IsDBCSLeadByte( CharByte, Codepage ) then
      begin
        ElementType := teLeadByte;
        InsideDBC := true;
      end;
    end;
  end;
end;

// Check if this (single-byte) character is a legal wrap point under certain
// Asian codepages. This is really only used for Thai and for Japanese
// half-width katakana; other DBCS languages use double-byte characters for all
// their native glyphs.
//
function IsAsianWrapChar( const CharByte: Byte;
                          const Codepage: LongInt ): boolean;
begin
    Result := false;

    if ( CharByte < $80) then
      exit;

    case Codepage of
      932, 942, 943:        // Japanese
        if CharByte in [ $A2, $A6, $B1..$DD ] then
          Result := true;
      874:                  // Thai
        Result := true;
    end;
end;

// Check if this is the lead byte of a double-byte character. This is essential
// to know in certain cases:
//  - Nothing must ever be inserted between such a byte and the next byte
//    (e.g. line break, tag, etc).
//  - Cursor position must never be set between such a byte and the next byte.
//  - Selection state must never change between such a byte and the next byte.
//
function IsDBCSLeadByte( const CharByte: Byte;
                         const Codepage: LongInt ): boolean;
begin
    Result := false;

    case Codepage of
      932, 942, 943:        // Japanese
        if CharByte in [ $81..$9F, $E0..$FC ] then
          Result := true;
      949:                  // Korean KSC
        if CharByte in [ $85..$FE ] then
          Result := true;
      1381:                 // Chinese GB2312
        if CharByte in [ $8C..$FE ] then
          Result := true;
      936, 950, 1386:       // Chinese BIG-5 or GBK
        if CharByte in [ $81..$FE ] then
          Result := true;
    end;
end;

// Check to see if this byte is a valid second byte in a double-byte character.
// (This doesn't guarantee that it IS such a byte, only that it COULD be. The
// caller is assumed to know whether we're in a double byte character or not.)
//
function IsDBCSSecondByte( const CharByte: Byte;
                           const Codepage: LongInt ): boolean;
begin
    Result := false;

    case Codepage of
      932, 936, 942, 943, 949, 950, 1386:
        if CharByte >= $40 then
          Result := true;
      1381:
        if CharByte >= $A1 then
          Result := true;
    end;
end;
//
// ALT ends

Initialization
End.
