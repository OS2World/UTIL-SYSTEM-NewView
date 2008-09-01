Unit TextSearchQuery;

// NewView - a new OS/2 Help Viewer
// Copyright 2003 Aaron Lawrence (aaronl at consultant dot com)
// This software is released under the Gnu Public License - see readme.txt

Interface

// Encapsulates a parsed search query.

uses
  Classes,
  SysUtils;

Type
  ESearchSyntaxError = class( Exception )
  end;

  TSearchTermCombineMethod =
  (
    cmOptional,
    cmRequired,
    cmExcluded
  );

  TSearchTerm = class
    Text: string;
    Parts: TStringList;
    CombineMethod: TSearchTermCombineMethod;

    constructor Create( const aText: string;
                        const aCombineMethod: TSearchTermCombineMethod );
    destructor Destroy; override;

    function AsLogText: String;
  end;

  TTextSearchQuery = class
  protected
    Terms: TList;
    function GetTerm( Index: longint ): TSearchTerm;
    function GetTermCount: longint;
  public
    constructor Create(const aSearchString: String);
    destructor Destroy; override;

    property Term[ Index: longint ]: TSearchTerm read GetTerm;
    property TermCount: longint read GetTermCount;

    procedure Log;
  end;

Implementation

uses
  ACLUtility,
  ACLLanguageUnit,
  CharUtilsUnit,
  StringUtilsUnit,
  DebugUnit;

var
  QueryErrorMissingWord1: string;
  QueryErrorMissingWord2: string;

Procedure OnLanguageEvent( Language: TLanguageFile;
                           const Apply: boolean );
begin
  Language.Prefix := 'SearchQuery.';
  Language.LL( Apply, QueryErrorMissingWord1, 'QueryErrorMissingWord1', 'No search word given after ' );
  Language.LL( Apply, QueryErrorMissingWord2, 'QueryErrorMissingWord2', ' before ' );
end;


constructor TTextSearchQuery.Create(const aSearchString: string);
var
  tmpTermText: string;
  tmpCombineMethod: TSearchTermCombineMethod;
  Term: TSearchTerm;
  tmpTerms : TStringList;
  i : integer;
begin
  Terms := TList.Create;
  try
    tmpTerms := TStringList.Create;
    StrExtractStringsQuoted(tmpTerms, aSearchString);

    for i := 0 to tmpTerms.count-1 do
    begin
      tmpTermText := tmpTerms[i];

      // Check for modifiers:
      //  + word must be matched
      //  - word must not be matched
      case tmpTermText[ 1 ] of
       '+':
         tmpCombineMethod := cmRequired;
       '-':
         tmpCombineMethod := cmExcluded;
       else
         tmpCombineMethod := cmOptional;
      end;

      if tmpCombineMethod <> cmOptional then
      begin
        // delete + or -
        if Length( tmpTermText ) = 1 then
          if (i < tmpTerms.count-1) then
            raise ESearchSyntaxError.Create( QueryErrorMissingWord1
                                             + StrInDoubleQuotes(tmpTermText)
                                             + QueryErrorMissingWord2
                                             + StrInDoubleQuotes(tmpTerms[i+1]) )
          else
            raise ESearchSyntaxError.Create( QueryErrorMissingWord1
                                             + StrInDoubleQuotes(tmpTermText));
        Delete( tmpTermText, 1, 1 );
      end;

      Term := TSearchTerm.Create( tmpTermText, tmpCombineMethod );
      Terms.Add( Term );
    end;
    tmpTerms.Destroy;
  except
    tmpTerms.Destroy;
    Destroy; // clean up
    raise; // reraise exception
  end;
end;


destructor TTextSearchQuery.Destroy;
begin
  DestroyListObjects( Terms );
  Terms.Destroy;
end;


function TSearchTerm.AsLogText: String;
var
  i : integer;
begin
  result := 'TSearchTerm: ''' + text + '''  (' + IntToStr(Parts.Count) + ' parts; ';

  case CombineMethod of
  cmRequired :
    result := result + 'cmRequired';
  cmExcluded :
    result := result + 'cmExcluded';
  cmOptional :
    result := result + 'cmOptional';
  end;

  result := result + ')';
end;



function TTextSearchQuery.GetTerm( index: longint ): TSearchTerm;
begin
  Result := Terms[ Index ];
end;


function TTextSearchQuery.GetTermCount: longint;
begin
  Result := Terms.Count;
end;


constructor TSearchTerm.Create( const aText: string;
                                const aCombineMethod: TSearchTermCombineMethod );
var
  TermParseIndex: longint;
  TermChar: char;
  TermPart: string;
begin
  Parts := TStringList.Create;

  Text := aText;
  CombineMethod := aCombineMethod;

  // Break out each part of the term as IPF does:
  // consecutive alphanumeric chars become a "word"
  // but each symbol is a separate word, and symbols break
  // up alphanumerics into multiple words. e.g.
  // CAKE_SAUSAGE becomes three words in IPF,
  // one each for "CAKE" "_" and "SAUSAGE"

  TermParseIndex := 1;
  while TermParseIndex <= Length( Text ) do
  begin
    // collect alphanumeric chars
    TermPart := '';
    while TermParseIndex <= Length( Text ) do
    begin
      TermChar := Text[ TermParseIndex ];
      if  (    CharIsAlpha( TermChar )
            or CharIsDigit( TermChar ) ) then
      begin
        // alpha numeric, collect it
        TermPart := TermPart + TermChar;
        inc( TermParseIndex );
      end
      else
      begin
        // not alpha numeric, so stop
        break;
      end;
    end;
    if Length( TermPart ) > 0 then
    begin
      Parts.Add( TermPart ); // add collected alphanumeric part
    end;

    if TermParseIndex <= Length( Text ) then
    begin
      // must be a symbol,
      // each symbol (excluding space) is an individual item
      if Text[ TermParseIndex ] <> ' ' then
        Parts.Add( Text[ TermParseIndex ] );
      inc( TermParseIndex );
    end;

  end;

end;

destructor TSearchTerm.Destroy;
begin
  Parts.Destroy;
end;


procedure TTextSearchQuery.Log;
var
  i : longint;
begin
  if IsLogAspectsEnabled(LogSearch) then
  begin
    LogEvent(LogSearch, ' TTextSearchQuery:');

    for i := 0 to TermCount - 1 do
    begin
      LogEvent(LogSearch, '  Term ' + IntToStr(0) + ' [' + Term[i].AsLogText + ']');
    end;
  end;
end;



Initialization
   RegisterProcForLanguages( OnLanguageEvent );
End.
