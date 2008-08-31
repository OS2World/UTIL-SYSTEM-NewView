Unit SearchUnit;

// NewView - a new OS/2 Help Viewer
// Copyright 2003 Aaron Lawrence (aaronl at consultant dot com)
// This software is released under the Gnu Public License - see readme.txt

Interface

// Contains code to search help files.

uses
  Classes,
  HelpFile,
  TextSearchQuery,
  IPFFileFormatUnit;

const
  // match weightings
  mwOnlyTitleWord = 200;
  mwFirstTitleWord = 50;
  mwTitleWord = 20;

  mwOnlyIndexWord = 100;
  mwFirstIndexWord = 20;
  mwIndexWord = 10;
  mwTopicTextWord = 1;

  // best case match weighting of a word
  mwExactWord = 20;


// note on weightings. The title/index weightings
// are multipled by word weightings.
// Topic text matches are equal to word weighting
// times word weighting.

procedure SearchHelpFile( HelpFile: THelpFile;
                          Query: TTextSearchQuery;
                          Results: TList;
                          WordSequences: TList );

// clear a lsit of word sequences (as produced by above)
procedure ClearWordSequences( WordSequences: TList;
                              DictionaryCount: longint );

Implementation

uses
  SysUtils,
  DebugUnit,
  StringUtilsUnit,
  HelpTopic;

type
  TSearchType = ( stGeneral, stStarts, stExactMatch, stEnds );

procedure ClearWordSequence( WordSequence: TList;
                             DictionaryCount: longint );
var
  StepIndex: longint;
  DictionaryRelevances: UInt32ArrayPointer;
begin
  for StepIndex := 0 to WordSequence.Count - 1 do
  begin
    DictionaryRelevances := WordSequence[ StepIndex ];
    FreeUInt32Array( DictionaryRelevances, DictionaryCount );
  end;
  WordSequence.Clear;
end;

procedure ClearWordSequences( WordSequence: TList;
                              DictionaryCount: longint );
var
  SequenceIndex: longint;
  WordSequence: TList;
begin
  for SequenceIndex := 0 to WordSequences.Count - 1 do
  begin
    WordSequence := WordSequences[ SequenceIndex ];
    ClearWordSequence( WordSequence,
                       DictionaryCount );
    WordSequence.Destroy;
  end;
  WordSequences.Clear;
end;


// given a search word which is known to matche Reference word,
// return the relevance
function MatchedWordRelevance( const SearchWord: string;
                               const ReferenceWord: string ): longint;
begin
  Result := mwExactWord
            * Length( SearchWord )
            div Length( ReferenceWord );
  if Result = 0 then
    Result := 1;
end;

// Compares the given search word against the given
// reference word. Returns a value indicating how well the
// search word matches, 0 = not at all.
function CompareWord( const SearchWord: string;
                      const ReferenceWord: string ): longint;
var
  OccurrencePos: longint;
begin
  Result := 0;
  OccurrencePos := CaseInsensitivePos( SearchWord, ReferenceWord );
  if OccurrencePos = 0 then
  begin
    // no match
    exit;
  end;

  Result := MatchedWordRelevance( SearchWord, ReferenceWord );
end;

// Search the help file dictionary for words that match
// the given search word. Partial matches are considered.
// Results returns the matching word indexes.
procedure SearchDictionary( HelpFile: THelpFile;
                            SearchWord: string;
                            Results: UInt32ArrayPointer );
var
  tmpDictIndex: integer;
  pDictWord: pstring;
begin
  for tmpDictIndex := 0 to HelpFile.DictionaryCount - 1 do
  begin
    pDictWord := HelpFile.DictionaryWordPtrs[ tmpDictIndex ];
    Results[ tmpDictIndex ] := CompareWord( SearchWord, pDictWord^ );
  end;
end;

// Search the help file dictionary for words that
// match the given search word exactly (except for case-insensitive)
procedure SearchDictionaryExact( HelpFile: THelpFile;
                                 SearchWord: string;
                                 Results: UInt32ArrayPointer );
var
  DictIndex: integer;
  pDictWord: pstring;
begin
  FillUInt32Array( Results, HelpFile.DictionaryCount, 0 );

  for DictIndex := 0 to HelpFile.DictionaryCount - 1 do
  begin
    pDictWord := HelpFile.DictionaryWordPtrs[ DictIndex ];
    if StrEqualIgnoringCase( SearchWord, pDictWord^ ) then
      Results[ DictIndex ] := mwExactWord;
  end;
end;

// Search the help file dictionary for words that
// start with the given word
procedure SearchDictionaryStarts( HelpFile: THelpFile;
                                  SearchWord: string;
                                  Results: UInt32ArrayPointer );
var
  DictIndex: integer;
  DictWord: string;
begin
  if IsLogAspectsEnabled(LogSearch) then
  begin
    LogEvent(LogSearch, '  calling SearchDictionaryStarts "' + SearchWord + '"');
  end;
  
  FillUInt32Array( Results, HelpFile.DictionaryCount, 0 );

  for DictIndex := 0 to HelpFile.DictionaryCount - 1 do
  begin
    DictWord := HelpFile.DictionaryWords[ DictIndex ];
    if StrStartsWithIgnoringCase(DictWord, SearchWord) then
    begin
      Results[ DictIndex ] := MatchedWordRelevance( SearchWord, DictWord )
    end;
  end;
end;


// Search the help file dictionary for words that
// end with the given word
procedure SearchDictionaryEnds( HelpFile: THelpFile;
                                SearchWord: string;
                                Results: UInt32ArrayPointer );
var
  DictIndex: integer;
  DictWord: string;
begin
  if IsLogAspectsEnabled(LogSearch) then
  begin
    LogEvent(LogSearch, ' calling SearchDictionaryEnds for "' + SearchWord + '"');
  end;
  FillUInt32Array( Results, HelpFile.DictionaryCount, 0 );

  for DictIndex := 0 to HelpFile.DictionaryCount - 1 do
  begin
    DictWord := HelpFile.DictionaryWords[ DictIndex ];
    if StrEndsWithIgnoringCase(DictWord, SearchWord ) then
    begin
      Results[ DictIndex ] := MatchedWordRelevance( SearchWord, DictWord );
    end;
  end;
end;

// Search titles of topics for given searchword
procedure SearchTopicTitles( HelpFile: THelpFile;
                             SearchWord: string;
                             Results: UInt32ArrayPointer );
var
  TopicIndex: longint;
  pTitle: pstring;
  TitleWord: string;
  Topic: TTopic;
  TitleWordIndex: longint;
  WordRelevance: longint;
  TitleWordRelevance: longint;
  tmpTitleWords : TStringList;
  i : integer;
begin
  if IsLogAspectsEnabled(LogSearch) then
  begin
    LogEvent(LogSearch, ' calling SearchTopicTitles for "' + SearchWord + '"');
  end;

  tmpTitleWords := TStringList.Create;

  // Search topic titles
  for TopicIndex:= 0 to HelpFile.TopicCount - 1 do
  begin
    Topic:= HelpFile.Topics[ TopicIndex ];
    pTitle:= Topic.TitlePtr;
    TitleWordIndex := 0;

    tmpTitleWords.Clear;
    StrExtractStringsQuoted(tmpTitleWords, pTitle^);

    for i := 0 to tmpTitleWords.count-1 do
    begin
      TitleWord := tmpTitleWords[i];

      WordRelevance := CompareWord( SearchWord, TitleWord );
      if WordRelevance > 0 then
      begin
        if TitleWordIndex = 0 then
        begin
          // matching the first word is best
          if i = tmpTitleWords.count-1 then
          begin
            // in fact it's the only word
            TitleWordRelevance := mwOnlyTitleWord * WordRelevance
          end
          else
            TitleWordRelevance := mwFirstTitleWord * WordRelevance
        end
        else
        begin
          TitleWordRelevance := mwTitleWord * WordRelevance;
        end;
        inc( Results[ TopicIndex ],
             TitleWordRelevance );
      end;
      inc( TitleWordIndex );
    end;
  end;
  tmpTitleWords.Destroy;
end;

// Search index entries for given searchword
procedure SearchIndex( HelpFile: THelpFile;
                       SearchWord: string;
                       Results: UInt32ArrayPointer );
var
  IndexIndex: longint;
  pIndexEntry: pstring;
  IndexEntryWord: string;
  Topic: TTopic;
  IndexEntryWordIndex: longint;
  WordRelevance: longint;
  IndexEntryWordRelevance: longint;
  tmpIndexWords : TStringList;
  i : integer;
begin
  if IsLogAspectsEnabled(LogSearch) then
  begin
    LogEvent(LogSearch, ' calling SearchIndex for "' + SearchWord + '"');
  end;
  
  tmpIndexWords := TStringList.Create;

  for IndexIndex := 0 to HelpFile.Index.Count - 1 do
  begin
    Topic := HelpFile.Index.Objects[ IndexIndex ] as TTopic;
    pIndexEntry := HelpFile.IndexEntryPtr[ IndexIndex ];
    IndexEntryWordIndex := 0;

    tmpIndexWords.Clear;
    StrExtractStringsQuoted(tmpIndexWords, pIndexEntry^);

    for i := 0 to tmpIndexWords.count-1 do
    begin
      IndexEntryWord := tmpIndexWords[i];

      WordRelevance := CompareWord( SearchWord, IndexEntryWord );
      if WordRelevance > 0 then
      begin
        if IndexEntryWordIndex = 0 then
        begin
          // matching the first word is best
          if i = tmpIndexWords.count-1 then
          begin
            // in fact it's the only word
            IndexEntryWordRelevance := mwOnlyIndexWord * WordRelevance
          end
          else
            IndexEntryWordRelevance := mwFirstIndexWord * WordRelevance
        end
        else
        begin
          IndexEntryWordRelevance := mwIndexWord * WordRelevance;
        end;
        inc( Results[ Topic.Index ],
             IndexEntryWordRelevance );
      end;
      inc( IndexEntryWordIndex );
    end;
  end;

  tmpIndexWords.Destroy;
end;

// ------------------------------------------------------

// Master search function. Given a search query,
// searches topic text, titles, index entries.
// Matching topics are added to TList, with their
// SearchRelevance set appropriately.
procedure SearchHelpFile( HelpFile: THelpFile;
                          Query: TTextSearchQuery;
                          Results: TList;
                          WordSequences: TList );
var
  tmpTopicCount: longint;
  tmpTopic: TTopic;
  tmpTopicIndex: longint;
  tmpTermIndex: longint;
  tmpTerm: TSearchTerm;

  DictionaryRelevances: UInt32ArrayPointer;

  TopicsMatchingDictWord: UInt32ArrayPointer; // flags
  TopicsMatchingTermPart: UInt32ArrayPointer; // flags
  TopicsMatchingTerm: UInt32ArrayPointer; // flag then relevances
  TopicRelevances: UInt32ArrayPointer;
  TopicsExcluded: UInt32ArrayPointer;

  TopicRelevanceForTerm: longint;

  WordRelevance: longint;
  DictIndex: longint;

  TermPartIndex: longint;
  TermPart: string;

  s: string;

  TermWordSequence: TList;
begin
  LogEvent(LogSearch, 'SearchHelpFile');
  Query.Log;

  if HelpFile.SearchTable = nil then
  begin
    exit;
  end;

  // Reset flags per topic
  tmpTopicCount := HelpFile.TopicCount;

  // Get memory for topic relevance arrays

  AllocUInt32Array( TopicsMatchingDictWord, tmpTopicCount );
  AllocUInt32Array( TopicsMatchingTermPart, tmpTopicCount );
  AllocUInt32Array( TopicsMatchingTerm, tmpTopicCount );
  AllocUInt32Array( TopicRelevances, tmpTopicCount ); // functions as a flag and a cumulative relevance

  AllocUInt32Array( TopicsExcluded, tmpTopicCount ); // Exclusions are treated as boolean only


  ClearUInt32Array( TopicRelevances, tmpTopicCount );
  ClearUInt32Array( TopicsExcluded, tmpTopicCount );

  for tmpTermIndex := 0 to Query.TermCount - 1 do
  begin
    tmpTerm := Query.Term[ tmpTermIndex ];

    if IsLogAspectsEnabled(LogSearch) then
    begin
      LogEvent(LogSearch, 'Searching for term "'
                  + tmpTerm.Text
                  + '", '
                  + IntToStr( tmpTerm.Parts.Count )
                  + ' parts' );
    end;

    // look thru all parts of the term.           eg. CAKE_SAUSAGE

    TermWordSequence := TList.Create;

    if WordSequences <> nil then
      if tmpTerm.CombineMethod <> cmExcluded then
      begin
        // this term is an inclusive one, so we want to remember the matches
        WordSequences.Add( TermWordSequence );
      end;

    for TermPartIndex := 0 to tmpTerm.Parts.Count - 1 do
    begin
      TermPart := tmpTerm.Parts[ TermPartIndex ];

      if IsLogAspectsEnabled(LogSearch) then
      begin
        LogEvent(LogSearch, '  Searching for TermPart [' + TermPart + ']' );
      end;

      AllocUInt32Array( DictionaryRelevances, HelpFile.DictionaryCount );

      TermWordSequence.Add( DictionaryRelevances );

      // Search the dictionary for matches.
      // alpha numeric match

      if tmpTerm.Parts.Count = 1 then
      begin
        if IsLogAspectsEnabled(LogSearch) then
        begin
          LogEvent(LogSearch, '  Term has only one part...' );
          LogEvent(LogSearch, '  SearchDictionary [' + TermPart + ']' );
        end;

        // general match allowing all kinds of partial matches
        SearchDictionary( HelpFile, TermPart, DictionaryRelevances )
      end

      else if TermPartIndex = 0 then
      begin
        if IsLogAspectsEnabled(LogSearch) then
        begin
          LogEvent(LogSearch, '  Term has more then one part... we are at first' );
          LogEvent(LogSearch, '  SearchDictionaryEnd [' + TermPart + ']' );
        end;

        // first term part: word must match end of a topic word e.g. must end in "cake"
        SearchDictionaryEnds( HelpFile, TermPart, DictionaryRelevances )
      end

      else if TermPartIndex = tmpTerm.Parts.Count - 1 then
      begin
        if IsLogAspectsEnabled(LogSearch) then
        begin
          LogEvent(LogSearch, '  Term has more then one part... we are at last' );
          LogEvent(LogSearch, '  SearchDictionaryEnd [' + TermPart + ']' );
        end;

        // last term part: word must match start of a topic word e.g. must start with "sausage"
        SearchDictionaryStarts( HelpFile, TermPart, DictionaryRelevances )
      end

      else
      begin
        if IsLogAspectsEnabled(LogSearch) then
        begin
          LogEvent(LogSearch, '  Term has more then one part... we are inside' );
          LogEvent(LogSearch, '  SearchDictionaryEnd [' + TermPart + ']' );
        end;

        // intermediate term part: word must match exactly  e.g. must be "_"
        SearchDictionaryExact( HelpFile, TermPart, DictionaryRelevances )
      end;

      // For each word in the dictionary that matches
      // this search term part, search topic texts

      LogEvent(LogSearch, '  Dictionary search done' );
      ClearUInt32Array( TopicsMatchingTermPart, tmpTopicCount );

      for DictIndex := 0 to HelpFile.DictionaryCount - 1 do
      begin
        WordRelevance := DictionaryRelevances[ DictIndex ];
        if WordRelevance > 0 then
        begin
          // Search for occurrences of this word
          // within the text of topics
          HelpFile.SearchTable.Search( DictIndex,
                                       TopicsMatchingDictWord );

          // debug
          s := HelpFile.DictionaryWords[ DictIndex ];
          // TopicRelevancesForDictWord now contains 1
          // for topics that contain this word.

          OrUInt32Array( TopicsMatchingDictWord,
                         TopicsMatchingTermPart,
                         tmpTopicCount );
        end
      end;

      LogEvent(LogSearch, '  Topic searches done' );

      if TermPartIndex = 0 then
        // first part, just copy
        CopyUInt32Array( TopicsMatchingTermPart,
                         TopicsMatchingTerm,
                         tmpTopicCount )
      else
        // and with previous term part results
        AndUInt32Array( TopicsMatchingTermPart,
                        TopicsMatchingTerm,
                        tmpTopicCount );

      // loop for next term part (IPF word)
    end;

    // Now we have searched the dictionary and worked out matching topics
    // for all parts of the term. Now combine all together

    LogEvent(LogSearch, 'Checking for sequences' );
    for tmpTopicIndex := 0 to tmpTopicCount - 1 do
    begin
      if TopicsMatchingTerm[ tmpTopicIndex ] > 0 then
      begin
        tmpTopic := HelpFile.Topics[ tmpTopicIndex ];
        // Topic text contained a match for the all the parts
        // of the term.
        // Now we need to:
        // - verify that they actually occur all in a sequence (if it's a multi-part term)
        // - count occurrences for relevance.

        TopicRelevanceForTerm := tmpTopic.SearchForWordSequences( TermWordSequence, false ); // don't stop at first match

        TopicRelevanceForTerm := TopicRelevanceForTerm div tmpTerm.Parts.Count; // divide to bring back into scale

        TopicsMatchingTerm[ tmpTopicIndex ] := TopicRelevanceForTerm;

      end;
    end;

    if WordSequences = nil then
    begin
      // we don't need to keep the sequence
      ClearWordSequence( TermWordSequence,
                         HelpFile.DictionaryCount );
      TermWordSequence.Destroy;
    end;

    // Search titles and index

    LogEvent(LogSearch, '  Searching titles' );
    SearchTopicTitles( HelpFile, tmpTerm.Text, TopicsMatchingTerm );

    LogEvent(LogSearch, '  Searching index' );
    SearchIndex( HelpFile, tmpTerm.Text, TopicsMatchingTerm );

    LogEvent(LogSearch, '  Combining' );
    case tmpTerm.CombineMethod of
      cmOptional:
      begin
        LogEvent(LogSearch, '  Combining optional');
        AddUInt32Array( TopicsMatchingTerm,
                        TopicRelevances,
                        tmpTopicCount );
      end;

      cmRequired:
      begin
        LogEvent(LogSearch, '  Combining required');
        // if zero then add to exclusions
        NotOrUInt32Array( TopicsMatchingTerm,
                          TopicsExcluded,
                          tmpTopicCount );

        AddUInt32Array( TopicsMatchingTerm,
                        TopicRelevances,
                        tmpTopicCount );
      end;

      cmExcluded:
      begin
        LogEvent(LogSearch, '  Combining excluded');
        OrUInt32Array( TopicsMatchingTerm,
                       TopicsExcluded,
                       tmpTopicCount );
      end;
    end;

//    Term.ClearMatches;

    // loop for next term...
  end;

  LogEvent(LogSearch, 'Search completed, converting to list' );

  // Now convert to list form.

  for tmpTopicIndex := 0 to tmpTopicCount - 1 do
  begin
    if TopicsExcluded[ tmpTopicIndex ] = 0 then
    begin
      tmpTopic := HelpFile.Topics[ tmpTopicIndex ];
      tmpTopic.SearchRelevance := TopicRelevances[ tmpTopicIndex ];
      if tmpTopic.SearchRelevance > 0 then
      begin
        Results.Add( tmpTopic );
      end;
    end;
  end;

  LogEvent(LogSearch, 'Freeing arrays' );
  FreeUInt32Array( TopicRelevances, tmpTopicCount );
  FreeUInt32Array( TopicsExcluded, tmpTopicCount );
  FreeUInt32Array( TopicsMatchingTerm, tmpTopicCount );
  FreeUInt32Array( TopicsMatchingTermPart, tmpTopicCount );
  FreeUInt32Array( TopicsMatchingDictWord, tmpTopicCount );

  LogEvent(LogSearch, 'Done' );
end;

Initialization
End.