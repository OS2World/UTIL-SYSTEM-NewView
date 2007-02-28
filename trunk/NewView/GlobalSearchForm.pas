Unit GlobalSearchForm;

// NewView - a new OS/2 Help Viewer
// Copyright 2003 Aaron Lawrence (aaronl at consultant dot com)
// This software is released under the Gnu Public License - see readme.txt

Interface

// Global search uses a thread to load helpfiles, search them, and
// send the results to be displayed.

Uses
  PmWin,
  Classes,
  Forms,
  Buttons,
  StdCtrls,
  ComCtrls,
  Outline2,
  GenericThread,
  ACLLanguageUnit,
  TextSearchQuery,
  HelpFile,
  ExtCtrls,
  LED;

const
  // Custom window messages for this form
  // NOTE! Sibyl uses WM_USER+1 and +2!
  WM_OPENED     = WM_USER + 10;

Type
  TViewTopicCallback = procedure( Filename: string;
                                  TopicIndex: longint ) of object;

  TSearchParameters = class
    Query: TTextSearchQuery;
    Directories: TStringList;
  end;

  // Returned by thread. (Note - helpfiles and topics are destroyed dynamically.)
  TSearchResult = class
    Filename: string;
    FileTitle: string;
    MatchingTopics: TList;
    constructor Create;
    destructor Destroy; override;
  end;

  TGlobalSearchForm = Class (TForm)
    SearchTextEdit: TEdit;
    SearchTextLabel: TLabel;
    SelectDrivesButton: TButton;
    SearchTextLabel1: TLabel;
    ResultsLabel: TLabel;
    Led: TLed;
    LEDTimer: TTimer;
    SearchLocationComboBox: TComboBox;
    Bevel: TBevel;
    HelpButton: TButton;
    ResultsOutline: TOutline2;
    SearchButton: TButton;
    ViewTopicButton: TButton;
    CancelButton: TButton;
    ProgressBar: TProgressBar;
    ProgressLabel: TLabel;
    Procedure SearchLocationComboBoxOnItemSelect (Sender: TObject;
      Index: LongInt);
    Procedure LEDTimerOnTimer (Sender: TObject);
    Procedure SearchInComboBoxOnChange (Sender: TObject);
    Procedure ResultsOutlineOnEnter (Sender: TObject);
    Procedure SearchTextEditOnEnter (Sender: TObject);
    Procedure ViewHelpPathsButtonOnClick (Sender: TObject);
    Procedure SelectDrivesButtonOnClick (Sender: TObject);
    Procedure GlobalSearchFormOnSetupShow (Sender: TObject);
    Procedure CancelButtonOnClick (Sender: TObject);
    Procedure GlobalSearchFormOnClose (Sender: TObject;
      Var Action: TCloseAction);
    Procedure ResultsOutlineOnItemDblClick (Node: TNode);
    Procedure ViewTopicButtonOnClick (Sender: TObject);
    Procedure GlobalSearchFormOnCreate (Sender: TObject);
    Procedure GlobalSearchFormOnCloseQuery (Sender: TObject;
      Var CanClose: Boolean);
    Procedure SearchButtonOnClick (Sender: TObject);
    Procedure GlobalSearchFormOnShow (Sender: TObject);
  Protected
    Procedure OnSearchProgress ( n, outof: integer;
                                 S: String );
    Procedure OnMatchFound( SearchResult: TSearchResult );
    Procedure OnThreadData( S: string; Data: TObject );
    Procedure OnSearchFinished( Result: TObject );
    Procedure SetProgressLabel( const S: String );

    // Handle our own WM_OPENED message
    Procedure WMOpened( Var Msg: TMessage ); Message WM_OPENED;
    Procedure ClearResults;
    Procedure ViewTopic;

    ThreadManager: TGenericThreadManager;

    Procedure UpdateButtons;


    Procedure GetSelectedDirectories( List: TStringList );

    // search thread context

    Function Search( Parameters: TObject ): TObject;

  protected
    Procedure OnLanguageEvent( Language: TLanguageFile;
                               const Apply: boolean );

    SearchCaption: string;
    StopCaption: string;
    NoResultsMsg: string;
    ScanDirectoriesMsg: string;
    SearchingFileMsg: string;
    OfMsg: string;
    DoneMsg: string;
    SearchErrorTitle: string;
    SearchError: string;

    StandardHelpPathsLocation: string;
    FixedDrivesLocation: string;
    SelectedHelpPathsLocation: string;
    CustomPathsLocation: string;

  public
    // set by caller
    ViewTopicCallback: TViewTopicCallback;
    Procedure DoSearch;
  End;

Var
  GlobalSearchForm: TGlobalSearchForm;

procedure EnsureGlobalSearchFormLoaded;

Implementation

uses
  SysUtils,
  DebugUnit,
  ACLDialogs,
  ControlsUtility,
  DriveInfoUnit,
  ACLStringUtility,
  IPFFileFormatUnit,
  HelpTopic,
  SearchUnit,
  SearchDirectoriesFormUnit,
  SettingsUnit,
  InformationFormUnit,
  FileUtilsUnit;

type
  // Used to store filenames in outline
  THelpFileInfo = class
    FileName: string;
  end;

constructor TSearchResult.Create;
begin
  inherited Create;
  MatchingTopics := TList.Create;
end;

destructor TSearchResult.Destroy;
begin
  MatchingTopics.Destroy;
  inherited Destroy;
end;

Procedure TGlobalSearchForm.SearchLocationComboBoxOnItemSelect (Sender: TObject; Index: LongInt);
Begin
  Settings.GlobalSearchLocation := TGlobalSearchLocation( SearchLocationComboBox.ItemIndex );
End;

Procedure TGlobalSearchForm.LEDTimerOnTimer (Sender: TObject);
Begin
  LED.LedCondition := not LED.LedCondition;
End;

Procedure TGlobalSearchForm.SearchInComboBoxOnChange (Sender: TObject);
Begin
End;

Procedure TGlobalSearchForm.ResultsOutlineOnEnter (Sender: TObject);
Begin
  ViewTopicButton.Default := true;
End;

Procedure TGlobalSearchForm.SearchTextEditOnEnter (Sender: TObject);
Begin
  SearchButton.Default := true;
End;

Procedure TGlobalSearchForm.ViewHelpPathsButtonOnClick (Sender: TObject);
var
  Dirs: TStringList;
Begin
  Dirs := TStringList.Create;

  with InformationForm.InformationMemo do
  begin
    Lines.Clear;

    Lines.Add( 'HELP:' );
    GetDirsInPath( 'HELP', Dirs );
    Lines.AddStrings( Dirs );

    Lines.Add( '' );
    Lines.Add( 'BOOKSHELF:' );
    GetDirsInPath( 'BOOKSHELF', Dirs );
    Lines.AddStrings( Dirs );
  end;
  InformationForm.ShowModal;

  Dirs.Destroy;
End;

Procedure TGlobalSearchForm.GetSelectedDirectories( List: TStringList );
Var
  DriveNumber: longint;
  DriveType: TDriveType;
  DriveLetter: char;
  Dirs: TStringList;
  i: longint;
  Dir: string;
  FoundIndex: longint;
begin
  List.Clear;
  case SearchLocationComboBox.ItemIndex of
    -1:
    begin
      // one custom dir...
      List.Add( SearchLocationComboBox.Text );
    end;

    Ord( gsHelpPaths ),
    Ord( gsSelectedHelpPaths ): // standard or selected help paths
    Begin
      Dirs := TStringList.Create;
      List.Sorted := true;
      List.Duplicates := dupIgnore;

      GetDirsInPath( 'HELP', Dirs );
      List.AddStrings( Dirs );

      GetDirsInPath( 'BOOKSHELF', Dirs );
      List.AddStrings( Dirs );

      Dirs.Destroy;

      if SearchLocationComboBox.ItemIndex = Ord( gsSelectedHelpPaths ) then
      begin
        // now mark some as non-selected...
        for i := 0 to List.Count - 1 do
        begin
          Dir := List[ i ];
          if not Settings.SearchDirectories.Find( Dir, FoundIndex ) then
            List.Objects[ i ] := pointer( 1 );
        end;
      end;
    end;

    Ord( gsFixedDrives ):
    begin
      // drives
      For DriveNumber := MinDriveNumber To MaxDriveNumber Do
      Begin
        DriveType := GetDriveType( DriveNumber );
        DriveLetter := Chr( DriveNumber + Ord( 'A' ) - 1 );

        if DriveType = dtHard then
        begin
          List.Add( DriveLetter + ':\...' );
        end;
      end;
    end;

    Ord( gsCustom ):
    begin
      // already custom...
      List.Assign( Settings.SearchDirectories );
    end;
  end;
end;

Procedure TGlobalSearchForm.SelectDrivesButtonOnClick (Sender: TObject);
Begin
  GetSelectedDirectories( SearchDirectoriesForm.SelectedFolders );

  SearchDirectoriesForm.ShowModal;
  if SearchDirectoriesForm.ModalResult <> mrOK then
    exit;

  if SearchLocationComboBox.ItemIndex = Ord( gsHelpPaths ) then
    SearchLocationComboBox.ItemIndex := Ord( gsSelectedHelpPaths );

  if SearchLocationComboBox.ItemIndex = Ord( gsFixedDrives ) then
    SearchLocationComboBox.ItemIndex := Ord( gsCustom );

  if SearchDirectoriesForm.CustomDirAdded then
    SearchLocationComboBox.ItemIndex := Ord( gsCustom );

  Settings.SearchDirectories.Assign( SearchDirectoriesForm.SelectedFolders );

  SaveSettings;
End;

Procedure TGlobalSearchForm.UpdateButtons;
Begin
  // HelpPathsRadioButton.Checked := SearchType = stHelpPaths;
  // EverywhereRadioButton.Checked := SearchType = stDrives;
//  FolderRadioButton.Checked := SearchType = stFolder;

End;

Procedure TGlobalSearchForm.GlobalSearchFormOnSetupShow (Sender: TObject);
Begin
  ScaleForm( self, 11, 16 );
  SearchButton.Align := alFixedRightTop;

  if SearchLocationComboBox.Items.Count = 0 then
  begin
    // must match teh global search location enum...
    SearchLocationComboBox.Items.Add( StandardHelpPathsLocation );
    SearchLocationComboBox.Items.Add( FixedDrivesLocation );
    SearchLocationComboBox.Items.Add( SelectedHelpPathsLocation );
    SearchLocationComboBox.Items.Add( CustomPathsLocation );

    SearchLocationComboBox.ItemIndex := Ord( Settings.GlobalSearchLocation );
  end;

End;

Procedure TGlobalSearchForm.OnLanguageEvent( Language: TLanguageFile;
                                             const Apply: boolean );
begin
  Language.LoadComponentLanguage( self, Apply );

  Language.LL( Apply, SearchCaption, 'SearchCaption', '~Search' );
  Language.LL( Apply, StopCaption, 'StopCaption', '~Stop' );
  Language.LL( Apply, NoResultsMsg, 'NoResultsMsg', '(No results found)' );
  Language.LL( Apply, ScanDirectoriesMsg, 'ScanDirectoriesMsg', 'Finding help files...' );
  Language.LL( Apply, SearchingFileMsg, 'SearchingFileMsg', 'Searching ' );
  Language.LL( Apply, OfMsg, 'OfMsg', ' of ' );
  Language.LL( Apply, DoneMsg, 'DoneMsg', 'Done' );
  Language.LL( Apply, SearchErrorTitle, 'SearchErrorTitle', 'Search' );
  Language.LL( Apply, SearchError, 'SearchError', 'Error in search syntax: ' );

  Language.LL( Apply, StandardHelpPathsLocation, 'StandardHelpPathsLocation', 'Standard Help Paths' );
  Language.LL( Apply, FixedDrivesLocation, 'FixedDrivesLocation', 'All Hard Drives' );
  Language.LL( Apply, SelectedHelpPathsLocation, 'SelectedHelpPathsLocation', 'Selected Help Paths' );
  Language.LL( Apply, CustomPathsLocation, 'CustomPathsLocation', 'Directory List' );

end;

Procedure TGlobalSearchForm.CancelButtonOnClick (Sender: TObject);
Begin
  Close;
End;

Procedure TGlobalSearchForm.GlobalSearchFormOnClose (Sender: TObject;
  Var Action: TCloseAction);
Begin
  ClearResults;

  if SearchLocationComboBox.ItemIndex = -1 then
  begin
    Settings.GlobalSearchLocation := gsCustom;
    Settings.SearchDirectories.Clear;
    Settings.SearchDirectories.Add( SearchLocationComboBox.Text );
  end;

  Action := caFreeHandle; // DON'T release the form! (Default action for non-modal forms)
End;

Procedure TGlobalSearchForm.ResultsOutlineOnItemDblClick (Node: TNode);
Begin
  ViewTopic;
End;

Procedure TGlobalSearchForm.ViewTopicButtonOnClick (Sender: TObject);
begin
  ViewTopic;
end;

Procedure TGlobalSearchForm.ViewTopic;
var
  Node: TNode;
  HelpFileInfo: THelpFileInfo;
  TopicIndex: longint;
Begin
  Node := ResultsOutline.SelectedNode;
  if Node = nil then
    exit;
  case Node.Level of
    0:
    begin
      // file node
      HelpFileInfo := Node.Data as THelpFileInfo;
      TopicIndex := -1;
    end;

    1:
    begin
      // topic node
      HelpFileInfo := Node.Parent.Data as THelpFileInfo;
      TopicIndex := longint( Node.Data );
    end;

    else
      assert( false, 'Invalid node level in ViewTopic!: ' + IntToStr( Node.Level ) );
  end;

  ViewTopicCallback( HelpFileInfo.FileName, TopicIndex );
End;

Procedure TGlobalSearchForm.GlobalSearchFormOnCreate (Sender: TObject);
Begin
  RegisterForLanguages( OnLanguageEvent );

  UpdateButtons;

  ThreadManager := TGenericThreadManager.Create( self );
  ThreadManager.OnProgressUpdate := OnSearchProgress;
  ThreadManager.OnDataFromThread := OnThreadData;
  ThreadManager.OnJobComplete := OnSearchFinished;
End;

Procedure TGlobalSearchForm.OnSearchFinished( Result: TObject );
Begin
  SearchButton.Caption := SearchCaption;
  ProgressBar.Hide;
  LED.LedCondition := false;
  LEDTimer.Stop;

  if ResultsOutline.ChildCount > 0 then
  begin
    ResultsOutline.SelectedNode:= ResultsOutline.Children[ 0 ];
    ResultsOutline.Focus;
    SetProgressLabel( '' );
  end
  else
  begin
    SetProgressLabel( NoResultsMsg );
  end;

End;

Procedure TGlobalSearchForm.GlobalSearchFormOnCloseQuery (Sender: TObject;
  Var CanClose: Boolean);
Begin
  if ThreadManager.IsRunning then
  begin
    ThreadManager.Stop;
  end;
End;

Procedure TGlobalSearchForm.SetProgressLabel( const S: String );
begin
  ProgressLabel.Text := S;
  ProgressLabel.Refresh;
end;

Procedure TGlobalSearchForm.OnSearchProgress ( n, outof: integer;
                                               S: String );
Begin
  ProgressBar.Position := n * 100 div outof;
  SetProgressLabel( S );
End;

Function TGlobalSearchForm.Search( Parameters: TObject ): TObject;
var
  Files: TStringList;
  FileIndex: longint;
  Filename: string;
  HelpFile: THelpFile;
  SearchResult: TSearchResult;
  MatchingTopics: TList;

  SearchParameters: TSearchParameters;
  Query: TTextSearchQuery;
  i: longint;
  Dir: string;

Begin
//  StartProfile( GetLogFilesDir + 'NewViewSearch.prf' );

  SearchParameters := Parameters as TSearchParameters;

  Query := SearchParameters.Query;
  Files := TStringList.Create;

  // MatchingTopics.Add( nil ); // artificial crash
  MatchingTopics := TList.Create;

  LogEvent(LogParse, 'Getting files');

  // make sure we ignore duplicate files...
  Files.Sorted := true;
  Files.CaseSensitive := false;
  Files.Duplicates := dupIgnore;

  for i := 0 to SearchParameters.Directories.Count - 1 do
  begin
    if ThreadManager.StopRequested then
      break;
    ThreadManager.UpdateProgress( i * 10 div SearchParameters.Directories.Count,
                                  100,
                                  ScanDirectoriesMsg );
    Dir := SearchParameters.Directories[ i ];
    if StrEnds( '...', Dir ) then
    begin
      Dir := StrLeftWithout( Dir, 3 );
      ListFilesInDirectoryRecursiveWithTermination(
                                       Dir,
                                       '*.inf;*.hlp',
                                       Files,
                                       ThreadManager.StopRequested,
                                       true ); // check termination
    end
    else
    begin
      ListFilesInDirectory( Dir,
                            '*.inf;*.hlp',
                            Files);
    end;
  end;

  LogEvent(LogParse, ' Searching ' + IntToStr( Files.Count ) + ' files');
  for FileIndex := 0 to Files.Count - 1 do
  begin
    if ThreadManager.StopRequested then
      break;
    Filename := Files[ FileIndex ];
    LogEvent(LogParse, Filename);
    ThreadManager.UpdateProgress( 10 + FileIndex * 95 div Files.Count,
                                  100,
                                  SearchingFileMsg
                                  + Filename
                                  + ' ('
                                  + IntToStr( FileIndex + 1 )
                                  + OfMsg
                                  + IntToStr( Files.Count )
                                  + ')...' );

    try
      LogEvent(LogParse, ' Create THelpFile');
      HelpFile := THelpFile.Create( FileName );

      LogEvent(LogParse, ' Helpfile created');
      MatchingTopics.Clear;

      LogEvent(LogParse, ' Search file');
      SearchHelpFile( HelpFile,
                      Query,
                      MatchingTopics,
                      nil // don't care about words matched
                      );

      if MatchingTopics.Count > 0 then
      begin
        LogEvent(LogParse, '  Sort results');
        // Create a searchresult object to send back to main thread.
        SearchResult := TSearchResult.Create;
        SearchResult.Filename := HelpFile.Filename;
        SearchResult.FileTitle := HelpFile.Title;

        SearchResult.MatchingTopics.Assign( MatchingTopics );

        SearchResult.MatchingTopics.Sort( TopicRelevanceCompare );
        LogEvent(LogParse, '  Display results');

        ThreadManager.SendData( '', SearchResult );
      end;

      LogEvent(LogParse, 'Unload helpfile');
      HelpFile.Destroy;

    except
      on E: EHelpFileException do
      begin
        ; // ignore exceptions
      end;

      on E: EWindowsHelpFormatException do
      begin
        ; // ignore Windows help files
      end;
    end;

  end;
  LogEvent(LogParse, 'search completed');
  ThreadManager.UpdateProgress( 100, 100, DoneMsg );
  Files.Destroy;

  Query.Destroy;

  SearchParameters.Directories.Destroy;
  SearchParameters.Destroy;

  Result := nil;
  LogEvent(LogParse, 'done');
End;

Procedure TGlobalSearchForm.ClearResults;
var
  FileIndex: longint;
begin
  for FileIndex := 0 to ResultsOutline.ChildCount - 1 do
    ResultsOutline.Children[ FileIndex ].Data.Free;
  ResultsOutline.Clear;
  ViewTopicButton.Enabled := false;
end;

Procedure TGlobalSearchForm.SearchButtonOnClick (Sender: TObject);
begin
  DoSearch;
end;

Procedure TGlobalSearchForm.DoSearch;
var
  SearchText: string;
  Query: TTextSearchQuery;
  SearchParameters: TSearchParameters;
Begin
  if ThreadManager.IsRunning then
  begin
    ThreadManager.Stop;
    exit;
  end;

  SearchText := trim( SearchTextEdit.Text );
  if SearchText = '' then
    exit;

  try
    Query := TTextSearchQuery.Create( SearchText );
  except
    on e: ESearchSyntaxError do
    begin
      DoErrorDlg( SearchErrorTitle,
                  SearchError
                  + e.Message );
      exit;
    end;
  end;

  ClearResults;

  SearchParameters := TSearchParameters.Create;
  SearchParameters.Directories := TStringList.Create;

  SearchParameters.Query := Query;

  GetSelectedDirectories( SearchParameters.Directories );

  ThreadManager.StartJob( Search, SearchParameters );
  SearchButton.Caption := StopCaption;
  ProgressBar.Show;

  LED.LedCondition := true;
  LEDTimer.Start;
End;

Procedure TGlobalSearchForm.GlobalSearchFormOnShow (Sender: TObject);
Begin
  // make search button default
  SearchButton.Focus;
  SearchTextEdit.Focus;
  SetProgressLabel( '' );
  PostMsg( Handle, WM_OPENED, 0, 0 );
  SearchButton.Caption := SearchCaption;
  ProgressBar.Hide;
  ViewTopicButton.Enabled := false;
End;

Procedure TGlobalSearchForm.OnThreadData( S: string; Data: TObject );
var
  SearchResult: TSearchResult;
begin
  SearchResult := Data as TSearchResult;
  OnMatchFound( SearchResult );
  SearchResult.Destroy;
end;

Procedure TGlobalSearchForm.OnMatchFound( SearchResult: TSearchResult );
var
  Topic: TTopic;
  HelpFileInfo: THelpFileInfo;
  FileNode: TNode;
  TopicIndex: longint;
begin
  ViewTopicButton.Enabled := true;

  HelpFileInfo := THelpFileInfo.Create;
  HelpFileInfo.FileName := SearchResult.FileName;
  FileNode := ResultsOutline.AddChild( SearchResult.FileTitle
                                       + ' ('
                                       + SearchResult.FileName
                                       + ')',
                                       HelpFileInfo );
  for TopicIndex := 0 to SearchResult.MatchingTopics.Count - 1 do
  begin
    Topic := SearchResult.MatchingTopics[ TopicIndex ];
    FileNode.AddChild( Topic.Title,
                       TObject( Topic.Index ) );
  end;
end;

Procedure TGlobalSearchForm.WMOpened( Var Msg: TMessage );
begin
  SearchTextLabel.XAlign := xaLeft;
  SearchTextLabel.YAlign := yaTop;

  SearchTextEdit.XStretch := xsFrame;
  SearchTextEdit.YAlign := yaTop;
  SearchTextEdit.Focus;

  SearchButton.XAlign := xaRight;
  SearchButton.YAlign := yaTop;

  SearchTextLabel1.XAlign := xaLeft;
  SearchTextLabel1.YAlign := yaTop;

  SearchLocationComboBox.XStretch := xsFrame;
  SearchLocationComboBox.YAlign := yaTop;

  SelectDrivesButton.XAlign := xaRight;
  SelectDrivesButton.YAlign := yaTop;

  Bevel.XStretch := xsFrame;
  Bevel.YAlign := yaTop;

  LED.XAlign := xaRight;
  LED.YAlign := yaTop;

  ResultsLabel.XAlign := xaLeft;
  ResultsLabel.YAlign := yaTop;

  ProgressLabel.XStretch := xsFrame;
  ProgressLabel.YAlign := yaTop;

  ResultsOutline.XStretch := xsFrame;
  ResultsOutline.YStretch := ysFrame;

  ProgressBar.XStretch := xsFrame;
  ProgressBar.YAlign := yaBottom;

end;

procedure EnsureGlobalSearchFormLoaded;
begin
  if GlobalSearchForm = nil then
    GlobalSearchForm := TGlobalSearchForm.Create( nil );
end;

Initialization
  RegisterClasses ([TGlobalSearchForm, TEdit, TLabel,
    TProgressBar, TButton, TOutline2, TComboBox, TBevel, TLed, TTimer]);
  RegisterUpdateProcForLanguages( EnsureGlobalSearchFormLoaded );
End.
