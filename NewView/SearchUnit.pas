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
  ACLStringUtility,
  ACLProfile,
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
    // no match
    exit;

  Result := MatchedWordRelevance( SearchWord, ReferenceWord );
end;

// Search the help file dictionary for words that match
// the given search word. Partial matches are considered.
// Results returns the matching word indexes.
procedure SearchDictionary( HelpFile: THelpFile;
                            SearchWord: string;
                            Results: UInt32ArrayPointer );
var
  DictIndex: integer;
  pDictWord: pstring;
begin
  for DictIndex := 0 to HelpFile.DictionaryCount - 1 do
  begin
    pDictWord := HelpFile.DictionaryWordPtrs[ DictIndex ];
    Results[ DictIndex ] := CompareWord( SearchWord,
                                         pDictWord^ );
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
    if StringsSame( SearchWord, pDictWord^ ) then
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
  FillUInt32Array( Results, HelpFile.DictionaryCount, 0 );

  for DictIndex := 0 to HelpFile.DictionaryCount - 1 do
  begin
    DictWord := HelpFile.DictionaryWords[ DictIndex ];
    if StrStarts( SearchWord, DictWord ) then
      Results[ DictIndex ] := MatchedWordRelevance( SearchWord, DictWord );
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
  FillUInt32Array( Results, HelpFile.DictionaryCount, 0 );

  for DictIndex := 0 to HelpFile.DictionaryCount - 1 do
  begin
    DictWord := HelpFile.DictionaryWords[ DictIndex ];
    if StrEnds( SearchWord, DictWord ) then
      Results[ DictIndex ] := MatchedWordRelevance( SearchWord, DictWord );
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
  P: longint;
begin
  // Search topic titles
  for TopicIndex:= 0 to HelpFile.TopicCount - 1 do
  begin
    Topic:= HelpFile.Topics[ TopicIndex ];
    pTitle:= Topic.TitlePtr;
    TitleWordIndex := 0;
    P := 1;
    while P < Length( pTitle^ ) do
    begin
      GetNextValue( pTitle^, P, TitleWord, ' ' );
      WordRelevance := CompareWord( SearchWord,
                                    TitleWord );
      if WordRelevance > 0 then
      begin
        if TitleWordIndex = 0 then
        begin
          // matching the first word is best
          if P >= Length( pTitle^ ) then
            // in fact it's the only word
            TitleWordRelevance := mwOnlyTitleWord
                                  * WordRelevance
          else
            TitleWordRelevance := mwFirstTitleWord
                                  * WordRelevance
        end
        else
        begin
          TitleWordRelevance := mwTitleWord
                                * WordRelevance;
        end;
        inc( Results[ TopicIndex ],
             TitleWordRelevance );
      end;
      inc( TitleWordIndex );
    end;
  end;
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
  P: longint;
begin
  for IndexIndex := 0 to HelpFile.Index.Count - 1 do
  begin
    Topic := HelpFile.Index.Objects[ IndexIndex ] as TTopic;
    pIndexEntry := HelpFile.IndexEntryPtr[ IndexIndex ];
    IndexEntryWordIndex := 0;
    P := 1;
    while P < Length( pIndexEntry^ ) do
    begin
      GetNextValue( pIndexEntry^, P, IndexEntryWord, ' ' );
      WordRelevance := CompareWord( SearchWord,
                                    IndexEntryWord );
      if WordRelevance > 0 then
      begin
        if IndexEntryWordIndex = 0 then
        begin
          // matching the first word is best
          if P >= Length( pIndexEntry^ ) then
            // in fact it's the only word
            IndexEntryWordRelevance := mwOnlyIndexWord
                                       * WordRelevance
          else
            IndexEntryWordRelevance := mwFirstIndexWord
                                    * WordRelevance
        end
        else
        begin
          IndexEntryWordRelevance := mwIndexWord
                                  * WordRelevance;
        end;
        inc( Results[ Topic.Index ],
             IndexEntryWordRelevance );
      end;
      inc( IndexEntryWordIndex );
    end;
  end;
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
  TopicCount: longint;
  Topic: TTopic;
  TopicIndex: longint;
  TermIndex: longint;
  Term: TSearchTerm;

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
  if HelpFile.SearchTable = nil then
  begin
    exit;
  end;

  // Reset flags per topic
  TopicCount := HelpFile.TopicCount;

  // Get memory for topic relevance arrays

  AllocUInt32Array( TopicsMatchingDictWord,
                    TopicCount );
  AllocUInt32Array( TopicsMatchingTermPart,
                    TopicCount );
  AllocUInt32Array( TopicsMatchingTerm,
                    TopicCount );
  AllocUInt32Array( TopicRelevances,  // functions as a flag and a cumulative relevance
                    TopicCount );
  AllocUInt32Array( TopicsExcluded, // Exclusions are treated as boolean only
                    TopicCount );

  ClearUInt32Array( TopicRelevances,
                    TopicCount );
  ClearUInt32Array( TopicsExcluded,
                    TopicCount );

  for TermIndex := 0 to Query.TermCount - 1 do
  begin
    Term := Query.Term[ TermIndex ];

    ProfileEvent( 'Searching for term "'
                  + Term.Text
                  + '", '
                  + IntToStr( Term.Parts.Count )
                  + ' parts' );

    // look thru all parts of the term.           eg. CAKE_SAUSAGE

    TermWordSequence := TList.Create;

    if WordSequences <> nil then
      if Term.CombineMethod <> cmExcluded then
        // this term is an inclusive one, so we want to remember the matches
        WordSequences.Add( TermWordSequence );

    for TermPartIndex := 0 to Term.Parts.Count - 1 do
    begin
      TermPart := Term.Parts[ TermPartIndex ];

      ProfileEvent( '  Searching for [' + TermPart + ']' );

      AllocUInt32Array( DictionaryRelevances,
                        HelpFile.DictionaryCount );

      TermWordSequence.Add( DictionaryRelevances );

      // Search the dictionary for matches.
      // alpha numeric match

      if Term.Parts.Count = 1 then
        // general match allowing all kinds of partial matches
        SearchDictionary( HelpFile,
                          TermPart,
                          DictionaryRelevances )

      else if TermPartIndex = 0 then
        // first term part: word must match end of a topic word e.g. must end in "cake"
        SearchDictionaryEnds( HelpFile,
                              TermPart,
                              DictionaryRelevances )

      else if TermPartIndex = Term.Parts.Count - 1 then
        // last term part: word must match start of a topic word e.g. must start with "sausage"
        SearchDictionaryStarts( HelpFile,
                                TermPart,
                                DictionaryRelevances )

      else
        // intermediate term part: word must match exactly  e.g. must be "_"
        SearchDictionaryExact( HelpFile,
                               TermPart,
                               DictionaryRelevances );

      // For each word in the dictionary that matches
      // this search term part, search topic texts

      ProfileEvent( '  Dictionary search done' );
      ClearUInt32Array( TopicsMatchingTermPart,
                        TopicCount );

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
                         TopicCount );
        end
      end;

      ProfileEvent( 'TOpic searches done' );

      if TermPartIndex = 0 then
        // first part, just copy
        CopyUInt32Array( TopicsMatchingTermPart,
                         TopicsMatchingTerm,
                         TopicCount )
      else
        // and with previous term part results
        AndUInt32Array( TopicsMatchingTermPart,
                        TopicsMatchingTerm,
                        TopicCount );

      // loop for next term part (IPF word)
    end;

    // Now we have searched the dictionary and worked out matching topics
    // for all parts of the term. Now combine all together

    ProfileEvent( 'Checking for sequences' );
    for TopicIndex := 0 to TopicCount - 1 do
    begin
      if TopicsMatchingTerm[ TopicIndex ] > 0 then
      begin
        Topic := HelpFile.Topics[ TopicIndex ];
        // Topic text contained a match for the all the parts
        // of the term.
        // Now we need to:
        // - verify that they actually occur all in a sequence (if it's a multi-part term)
        // - count occurrences for relevance.

        TopicRelevanceForTerm :=
          Topic.SearchForWordSequences( TermWordSequence,
                                        false ); // don't stop at first match

        TopicRelevanceForTerm :=
          TopicRelevanceForTerm div Term.Parts.Count; // divide to bring back into scale

        TopicsMatchingTerm[ TopicIndex ] := TopicRelevanceForTerm;

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

    ProfileEvent( '  Searching titles' );
    SearchTopicTitles( HelpFile, Term.Text, TopicsMatchingTerm );

    ProfileEvent( '  Searching index' );
    SearchIndex( HelpFile, Term.Text, TopicsMatchingTerm );

    ProfileEvent( '  Combining' );
    case Term.CombineMethod of
      cmOptional:
        AddUInt32Array( TopicsMatchingTerm,
                        TopicRelevances,
                        TopicCount );

      cmRequired:
      begin
        // if zero then add to exclusions
        NotOrUInt32Array( TopicsMatchingTerm,
                          TopicsExcluded,
                          TopicCount );

        AddUInt32Array( TopicsMatchingTerm,
                        TopicRelevances,
                        TopicCount );
      end;

      cmExcluded:
        OrUInt32Array( TopicsMatchingTerm,
                       TopicsExcluded,
                       TopicCount );
    end;

//    Term.ClearMatches;

    // loop for next term...
  end;

  ProfileEvent( 'Search completed, converting to list' );

  // Now convert to list form.

  for TopicIndex := 0 to TopicCount - 1 do
  begin
    if TopicsExcluded[ TopicIndex ] = 0 then
    begin
      Topic := HelpFile.Topics[ TopicIndex ];
      Topic.SearchRelevance := TopicRelevances[ TopicIndex ];
      if Topic.SearchRelevance > 0 then
      begin
        Results.Add( Topic );
      end;
    end;
  end;

  ProfileEvent( 'Freeing arrays' );
  FreeUInt32Array( TopicRelevances, TopicCount );
  FreeUInt32Array( TopicsExcluded, TopicCount );
  FreeUInt32Array( TopicsMatchingTerm, TopicCount );
  FreeUInt32Array( TopicsMatchingTermPart, TopicCount );
  FreeUInt32Array( TopicsMatchingDictWord, TopicCount );

  ProfileEvent( 'Done' );
end;

Initialization
End.
