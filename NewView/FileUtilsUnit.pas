Unit FileUtilsUnit;

// NewView - a new OS/2 Help Viewer
// Copyright 2006/2007 Ronald Brill (rbri at rbri dot de)
// This software is released under the GNU Public License - see readme.txt

// Helper functions for file handling

Interface

uses
  Classes,
  ACLUtility;


const
  DirectorySeparator = '\';
  PATH_SEPARATOR = ';';

  // TODO
  HelpPathEnvironmentVar = 'HELP';
  BookshelfEnvironmentVar = 'BOOKSHELF';
  LanguageEnvironmentVar = 'LANG';
  DEFAULT_LANGUAGE = 'EN_US';
  HELP_FILE_EXTENSION = '.hlp';


  // Adds a slash to the end of dir if not present
  // if aDir is empty this returns '\'
  Function AddDirectorySeparator(aDirectory : String) : String;

  // Adds a slash to the end of dir if not present
  // if aDir is empty this returns ''
  Function AddDirectorySeparatorIfNotEmpty(aDirectory: String) : String;

  // Removes a directory seperator from the end of aDirectory
  // (if present)
  Function RemoveRightDirectorySeparator(aDirectory : String) : String;

  // Expands the path given, relative to aBaseDirectory
  // Handles leading \ for root dir,
  // .. for parent, . (ignored),
  // drive spec at start,
  // ignores repeated \ e.g. \\
  Function ExpandPath(aBaseDirectory : String; aPath : String): String;

  Function GetLogFilesDir: String;

  Function SearchPath(const aPathEnvVar: String; const aFilename: String; var aResultFilename: String) : boolean;

  Function SearchHelpPaths(const aFilename: String; var aResultFilename: String; const anIncludeAppDir: boolean) : boolean;

  // Find the help file for the current app based on LANG
  Function FindDefaultLanguageHelpFile(const anApplicationName: String; const aLanguage : String) : String;

  // Breaks up specified Env var path
  Procedure GetDirsInPath(const aPathEnvVar: String; var aList: TStrings);

  // searches for all files in aDirectory matching aFilter and add
  // them to aList
  // it is possible to define different filter if you separate them by semicolon
  Procedure ListFilesInDirectory(const aDirectory: String; const aFilter: String; var aList: TStrings);

  // searches for all directories in aDirectory and add them to aList
  Procedure ListSubDirectories(const aDirectory: String; var aList: TStrings);

  Procedure ListFilesInDirectoryRecursiveWithTermination(const aDirectory : String;
                                                         const aFilter : String;
                                                         var aList : TStrings;
                                                         const aTerminateCheck : TTerminateCheck;
                                                         const aUseTerminateCheck : boolean);

  Function ParentDir(const aDirectory : String) : String;

  Function DirectoryExists(const aDirectory : String) : boolean;

Implementation

uses
  Dos,
  BseDos,
  Os2Def,
  SysUtils,
  StringUtilsUnit;

  Function AddDirectorySeparator(aDirectory : String) : String;
  begin
    if aDirectory = '' then
    begin
      Result:= DirectorySeparator;
      exit;
    end;

    if aDirectory[length(aDirectory)] <> DirectorySeparator then
    begin
      Result := aDirectory + DirectorySeparator;
      exit;
    end;

    Result := aDirectory;
  end;


  Function AddDirectorySeparatorIfNotEmpty(aDirectory: String): String;
  begin
    if aDirectory = '' then
    begin
      Result := '';
      exit;
    end;
    Result := AddDirectorySeparator(aDirectory);
  end;


  Function RemoveRightDirectorySeparator(aDirectory : String) : String;
  begin
    Result := StrTrimRightChars(aDirectory, [DirectorySeparator]);
  end;


  Function ExpandPath(aBaseDirectory : String; aPath : String): String;
  var
    tmpDirectory: String;
    tmpDirectories : TStringList;
    i : integer;
  begin
    Result:= aBaseDirectory;

    if aPath = '' then
    begin
      Result := StrTrimRightChars(Result, [DirectorySeparator]);
      exit;
    end;

    aPath := trim(aPath);
    if Length(aPath) > 1 then
    begin
      // check for drive spec
      if aPath[2] = ':' then
      begin
        Result := AddDirectorySeparator(aPath);
        if Length(aPath) > 3 then
        begin
          Result := StrTrimRightChars(Result, [DirectorySeparator]);
        end;
        exit;
      end
    end;

    if Length(aPath) > 0 then
    begin
      // check for root dir spec
      if aPath[1] = DirectorySeparator then
      begin
        // take just the drive from the basedir
        if aBaseDirectory[2] = ':' then
        begin
          Result := StrLeft(aBaseDirectory, 2);
        end
        else
        begin
          Result := DirectorySeparator;
        end;
          aPath := StrTrimLeftChars(aPath, [DirectorySeparator]);
      end;
    end;

    tmpDirectories := TStringList.Create;
    StrExtractStringsIgnoreEmpty(tmpDirectories, aPath, [DirectorySeparator], #0);
    for i := 0 to tmpDirectories.count-1 do
    begin
      tmpDirectory := tmpDirectories[i];
      if tmpDirectory = '..' then
      begin
        if NOT ((Length(Result) = 2) AND (Result[2] = ':')) then
        begin
          Result := ParentDir(Result);
        end;
      end
      else if tmpDirectory = '.' then
      begin
        ; // nothing to do
      end
      else
      begin
        Result := AddDirectorySeparator(Result) + tmpDirectory;
      end;

      // strip any extra leading slashes
      aPath := StrTrimLeftChars(aPath, [DirectorySeparator]);
    end;
    tmpDirectories.Destroy;

//    Result := StrTrimRightChars(Result, [DirectorySeparator]);

    if Length(Result) = 2 then
    begin
      if Result[2] = ':' then
      begin
        // just a drive spec X:, so add a slash
        Result := Result + DirectorySeparator;
      end;
    end;
  end;

  Function GetLogFilesDir: String;
  begin
    // ecomstation 1.1 compat
    Result := GetEnv('LOGFILES');
    if Result <> '' then
    begin
      Result := AddDirectorySeparator(Result);
      exit;
    end;
    // TODO
    Result := AddDirectorySeparator(GetApplicationDir);
  end;


  Function SearchPath(const aPathEnvVar: String;
                      const aFilename: String;
                      var   aResultFilename: String) : boolean;
  var
    tmpSzEnvVar : CString;
    tmpSzFilename : CString;
    tmpSzFilenameFound : CString;
    tmpRC: APIRET;
  begin
    Result := false;
    aResultFilename := '';

    tmpSzEnvVar := aPathEnvVar;
    tmpSzFilename := aFilename;
    tmpRC := DosSearchPath( SEARCH_IGNORENETERRS
                            + SEARCH_ENVIRONMENT
                            + SEARCH_CUR_DIRECTORY,
                            tmpSzEnvVar,
                            tmpSzFilename,
                            tmpSzFilenameFound,
                            sizeof(tmpSzFilenameFound));
    if tmpRC = 0 then
    begin
      Result := true;
      aResultFilename := tmpSzFilenameFound;
    end;
  end;


  Function SearchHelpPaths(const aFilename: String;
                           var   aResultFilename: String;
                           const anIncludeAppDir: boolean) : boolean;
  begin
    Result := SearchPath(HelpPathEnvironmentVar, aFileName, aResultFilename);
    if not Result then
    begin
      Result := SearchPath(BookshelfEnvironmentVar, aFileName, aResultFilename);
    end;

    if (not Result) and anIncludeAppDir then
    begin
      aResultFilename := AddDirectorySeparator(GetApplicationDir) + aFilename;
      Result := FileExists(aResultFilename);
      if not Result then
      begin
        aResultFilename := '';
      end;
    end;
  end;


  Function FindDefaultLanguageHelpFile(const anApplicationName: String; const aLanguage : String) : String;
  var
    tmpLanguage : String;
    tmpLanguageParts : TStringList;
    tmpMajorLanguage : String;
    tmpMinorLanguage : String;
  begin
    Result := '';

    tmpLanguage := aLanguage;
    if aLanguage = '' then
    begin
      tmpLanguage := DEFAULT_LANGUAGE;
    end;

    tmpLanguageParts := TStringList.Create;
    StrExtractStrings(tmpLanguageParts, tmpLanguage, ['_'], #0);

    tmpMajorLanguage := '';
    if tmpLanguageParts.count > 0 then
    begin
      tmpMajorLanguage := tmpLanguageParts[0];
    end;

    tmpMinorLanguage := '';
    if tmpLanguageParts.count > 1 then
    begin
      tmpMinorLanguage := tmpMinorLanguage[1];
    end;

    tmpLanguageParts.Destroy;

    // note there might be some other stuff on the end of LANG
    // such as ES_ES_EURO...
    if tmpMinorLanguage <> '' then
    begin
      if SearchHelpPaths( anApplicationName
                          + '_' + tmpMajorLanguage
                          + '_' + tmpMinorLanguage
                          + HELP_FILE_EXTENSION,
                          Result,
                          true ) then
      begin
        // found a specifc language
        exit;
      end;
    end;

    // try generic language?
    if SearchHelpPaths( anApplicationName
                        + '_' + tmpMajorLanguage
                        + HELP_FILE_EXTENSION,
                        Result,
                        true ) then
    begin
      exit;
    end;

    // nothing specific, search for default
    SearchHelpPaths(anApplicationName + HELP_FILE_EXTENSION, Result, true);
  end;


  Procedure GetDirsInPath(const aPathEnvVar: String; var aList: TStrings);
  var
    tmpRC : APIRET;
    tmpPszPathEnvVar : PChar;
    tmpSzEnvVar : CString;
  begin
    // do this in any case also if there is an error
    // to garantie a defined behavior
    aList.Clear;

    tmpSzEnvVar := aPathEnvVar;
    tmpRC := DosScanEnv(tmpSzEnvVar, tmpPszPathEnvVar);

    if tmpRC <> 0 then
    begin
      exit;
    end;

    StrExtractStringsIgnoreEmpty(aList, StrPas(tmpPszPathEnvVar), [PATH_SEPARATOR], #0);
  end;


  Procedure ListFilesInDirectory(const aDirectory: String; const aFilter: String; var aList: TStrings);
  var
    tmpRC : APIRET;
    tmpSearchResults: TSearchRec;
    tmpMask: String;
    tmpFilterParts : TStringList;
    i : integer;
  begin
    tmpFilterParts := TStringList.Create;
    StrExtractStrings(tmpFilterParts, aFilter, [PATH_SEPARATOR], #0);

    for i:=0 to tmpFilterParts.count-1 do
    begin
      tmpMask := tmpFilterParts[i];
      tmpRC := FindFirst(AddDirectorySeparator(aDirectory) + tmpMask, faAnyFile, tmpSearchResults);

      while tmpRC = 0 do
      begin
        if tmpSearchResults.Attr And faDirectory = 0 then
        begin
          aList.Add(AddDirectorySeparatorIfNotEmpty(aDirectory) + tmpSearchResults.Name );
        end;

        tmpRC := FindNext(tmpSearchResults);
      end;

      FindClose(tmpSearchResults);
    end;
    tmpFilterParts.Destroy;
  end;


  Procedure ListSubDirectories(const aDirectory: String; var aList: TStrings);
  var
    tmpRC : APIRET;
    tmpSearchResults: TSearchRec;
    tmpName : String;
  begin

    tmpRC := FindFirst(AddDirectorySeparator(aDirectory) + '*', faDirectory or faMustDirectory, tmpSearchResults);
    if (tmpRC <> 0) then
    begin
      exit;
    end;

    while tmpRC = 0 do
    begin
      tmpName := tmpSearchResults.Name;
      if (tmpName <> '.') AND (tmpName <> '..') then
      begin
        aList.Add(AddDirectorySeparatorIfNotEmpty(aDirectory) + tmpSearchResults.Name );
      end;
      tmpRC := FindNext(tmpSearchResults);
    end;
    FindClose(tmpSearchResults);
  end;


  Procedure ListFilesInDirectoryRecursiveWithTermination(const aDirectory : String;
                                                         const aFilter : String;
                                                         var aList : TStrings;
                                                         const aTerminateCheck : TTerminateCheck;
                                                         const aUseTerminateCheck : boolean);
  var
    i : integer;
    tmpSubDirectories : TStringList;
    tmpSubDirectory : String;
  begin
    // at first add all files from the directory itself
    ListFilesInDirectory(aDirectory, aFilter, aList);

    // now determine all subdirectories
    tmpSubDirectories := TStringList.Create;
    ListSubDirectories(aDirectory, tmpSubDirectories);

    for i := 0 to tmpSubDirectories.Count - 1 do
    begin
      // if Assigned( TerminateCheck ) then - doesn't work in sibyl
      if aUseTerminateCheck then
        if aTerminateCheck then
          break;

      tmpSubDirectory := tmpSubDirectories[i];

      ListFilesInDirectoryRecursiveWithTermination(tmpSubDirectory, aFilter, aList, aTerminateCheck, aUseTerminateCheck);
    end;
    tmpSubDirectories.Destroy;
  end;


  Function ParentDir(const aDirectory : String) : String;
  var
    tmpPos: integer;
  begin
    tmpPos := Length(aDirectory);

    // ends with slash
    while (aDirectory[tmpPos] = DirectorySeparator) AND (tmpPos > 0) do
    begin
      dec(tmpPos);
    end;

    // find slash
    while (aDirectory[tmpPos] <> DirectorySeparator) AND (tmpPos > 0) do
    begin
      dec(tmpPos);
    end;

    result:= StrLeft(aDirectory, tmpPos-1);
  end;


  Function DirectoryExists(const aDirectory : String) : boolean;
  Var
    tmpRC : APIRET;
    tmpSearchResults : TSearchRec;
    tmpDriveMap : ULONG;
    tmpActualDrive : ULONG;
    tmpDrive : Char;
    tmpDriveNum : integer;
    tmpDriveBit : longword;
    tmpDirectory : String;
  Begin
    Result := false;
    tmpDirectory := RemoveRightDirectorySeparator(aDirectory);
    if tmpDirectory = '' then
    begin
      Result:= true;
      exit;
    end;

    if Length(tmpDirectory) = 2 then
    begin
      if tmpDirectory[2] = ':' then
      begin
        // a drive only has been specified
        tmpDrive:= UpCase(tmpDirectory[1] );
        if (tmpDrive < 'A') or (tmpDrive > 'Z') then
        begin
          // invalid drive; return false;
          exit;
        end;

        DosQueryCurrentDisk(tmpActualDrive, tmpDriveMap);
        tmpDriveNum := Ord(tmpDrive) - Ord('A') + 1; // A -> 1, B -> 2...
        tmpDriveBit := 1 shl (tmpDriveNum-1); // 2^DriveNum

        Result := tmpDriveMap and (tmpDriveBit) > 0;
        exit;
      end;
    end;

    tmpRC := FindFirst(tmpDirectory, faDirectory or faMustDirectory, tmpSearchResults);
    if tmpRC = 0 then
    begin
      Result:= true;
      FindClose(tmpSearchResults);
    end;
  end;


Initialization
End.
