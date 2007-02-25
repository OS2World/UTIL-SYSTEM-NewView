Unit FileUtilsUnitTests;

Interface

uses
  Classes,
  TestAssert,
  FileUtilsUnit;


  FUNCTION getFileUtilsUnitTests : TList;


Implementation

  Procedure testAddDirectorySeparator_Empty;
  var
    tmpResult : String;
  begin
    tmpResult := AddDirectorySeparator('');

    assertEqualsString('testAddDirectorySeparator_Empty', '\', tmpResult);
  end;

  Procedure testAddDirectorySeparator_SingleChar;
  var
    tmpResult : String;
  begin
    tmpResult := AddDirectorySeparator('x');

    assertEqualsString('testAddDirectorySeparator_SingleChar', 'x\', tmpResult);
  end;

  Procedure testAddDirectorySeparator_ManyChars;
  var
    tmpResult : String;
  begin
    tmpResult := AddDirectorySeparator('xyz dkdj ');

    assertEqualsString('testAddDirectorySeparator_ManyChars', 'xyz dkdj \', tmpResult);
  end;


  Procedure testAddDirectorySeparator_SlashAtEnd;
  var
    tmpResult : String;
  begin
    tmpResult := AddDirectorySeparator('xy\');

    assertEqualsString('testAddDirectorySeparator_SlashAtEnd', 'xy\', tmpResult);
  end;


  // ----------------------------------------------------------


  Procedure testAddDirectorySeparatorIfNotEmpty_Empty;
  var
    tmpResult : String;
  begin
    tmpResult := AddDirectorySeparatorIfNotEmpty('');

    assertEqualsString('testAddDirectorySeparatorIfNotEmpty_Empty', '', tmpResult);
  end;

  Procedure testAddDirectorySeparatorIfNotEmpty_SingleChar;
  var
    tmpResult : String;
  begin
    tmpResult := AddDirectorySeparatorIfNotEmpty('x');

    assertEqualsString('testAddDirectorySeparatorIfNotEmpty_SingleChar', 'x\', tmpResult);
  end;

  Procedure testAddDirectorySeparatorIfNotEmpty_ManyChars;
  var
    tmpResult : String;
  begin
    tmpResult := AddDirectorySeparatorIfNotEmpty('xyz dkdj ');

    assertEqualsString('testAddDirectorySeparatorIfNotEmpty_ManyChars', 'xyz dkdj \', tmpResult);
  end;


  Procedure testAddDirectorySeparatorIfNotEmpty_SlashAtEnd;
  var
    tmpResult : String;
  begin
    tmpResult := AddDirectorySeparatorIfNotEmpty('xy\');

    assertEqualsString('testAddDirectorySeparatorIfNotEmpty_SlashAtEnd', 'xy\', tmpResult);
  end;


  // ----------------------------------------------------------


  Procedure testRemoveRightDirectorySeparator_Empty;
  var
    tmpResult : String;
  begin
    tmpResult := RemoveRightDirectorySeparator('');

    assertEqualsString('testRemoveRightDirectorySeparator_Empty', '', tmpResult);
  end;


  Procedure testRemoveRightDirectorySeparator_WithoutSlash;
  var
    tmpResult : String;
  begin
    tmpResult := RemoveRightDirectorySeparator('abc');

    assertEqualsString('testRemoveRightDirectorySeparator_WithoutSlash', 'abc', tmpResult);
  end;


  Procedure testRemoveRightDirectorySeparator_WithSlash;
  var
    tmpResult : String;
  begin
    tmpResult := RemoveRightDirectorySeparator('abc\');

    assertEqualsString('testRemoveRightDirectorySeparator_WithSlash', 'abc', tmpResult);
  end;


  // ----------------------------------------------------------

  Procedure testExpandPath_BothEmpty;
  var
    tmpResult : String;
  begin
    tmpResult := ExpandPath('', '');

    assertEqualsString('testExpandPath_BothEmpty', '', tmpResult);
  end;


  Procedure testExpandPath_PathEmpty;
  var
    tmpResult : String;
  begin
    tmpResult := ExpandPath('\abc\def', '');

    assertEqualsString('testExpandPath_PathEmpty', '\abc\def', tmpResult);
  end;


  Procedure testExpandPath_PathEmptyDirEndsWithSlash;
  var
    tmpResult : String;
  begin
    tmpResult := ExpandPath('\abc\def\', '');

    assertEqualsString('testExpandPath_PathEmpty', '\abc\def', tmpResult);
  end;


  Procedure testExpandPath_AbsolutePath;
  var
    tmpResult : String;
  begin
    tmpResult := ExpandPath('\abc\def', 'c:\test');

    assertEqualsString('testExpandPath_AbsolutePath', 'c:\test', tmpResult);
  end;


  Procedure testExpandPath_DriveWithSlash;
  var
    tmpResult : String;
  begin
    tmpResult := ExpandPath('\abc\def', 'c:\');

    assertEqualsString('testExpandPath_DriveWithSlash', 'c:\', tmpResult);
  end;


  Procedure testExpandPath_DriveWithoutSlash;
  var
    tmpResult : String;
  begin
    tmpResult := ExpandPath('\abc\def', 'c:');

    assertEqualsString('testExpandPath_DriveWithoutSlash', 'c:\', tmpResult);
  end;


  Procedure testExpandPath_RootPathForDrive;
  var
    tmpResult : String;
  begin
    tmpResult := ExpandPath('d:\abc\def', '\');

    assertEqualsString('testExpandPath_RootPathForDrive', 'd:\', tmpResult);
  end;


  Procedure testExpandPath_RootDirForDrive;
  var
    tmpResult : String;
  begin
    tmpResult := ExpandPath('d:\abc\def', '\xy');

    assertEqualsString('testExpandPath_RootDirForDrive', 'd:\xy', tmpResult);
  end;


  Procedure testExpandPath_RootPathWithoutDrive;
  var
    tmpResult : String;
  begin
    tmpResult := ExpandPath('abc\def', '\');

    assertEqualsString('testExpandPath_RootPathWithoutDrive', '\', tmpResult);
  end;


  Procedure testExpandPath_RootDirWithoutDrive;
  var
    tmpResult : String;
  begin
    tmpResult := ExpandPath('abc\def', '\xyz');

    assertEqualsString('testExpandPath_RootDirWithoutDrive', '\xyz', tmpResult);
  end;


  Procedure testExpandPath_AppendWithSlash;
  var
    tmpResult : String;
  begin
    tmpResult := ExpandPath('abc\def\', 'xyz');

    assertEqualsString('testExpandPath_AppendWithSlash', 'abc\def\xyz', tmpResult);
  end;


  Procedure testExpandPath_AppendWithoutSlash;
  var
    tmpResult : String;
  begin
    tmpResult := ExpandPath('abc\def', 'xyz');

    assertEqualsString('testExpandPath_AppendWithoutSlash', 'abc\def\xyz', tmpResult);
  end;


  Procedure testExpandPath_AppendWithSlashAtEnd;
  var
    tmpResult : String;
  begin
    tmpResult := ExpandPath('abc\def', 'xyz\');

    assertEqualsString('testExpandPath_AppendWithSlashAtEnd', 'abc\def\xyz', tmpResult);
  end;


  Procedure testExpandPath_WithDot;
  var
    tmpResult : String;
  begin
    tmpResult := ExpandPath('abc\def', 'xyz\.\abc');

    assertEqualsString('testExpandPath_WithDot', 'abc\def\xyz\abc', tmpResult);
  end;


  Procedure testExpandPath_WithDotAtEnd;
  var
    tmpResult : String;
  begin
    tmpResult := ExpandPath('abc\def', 'xyz\.');

    assertEqualsString('testExpandPath_WithDotAtEnd', 'abc\def\xyz', tmpResult);
  end;


  Procedure testExpandPath_WithDots;
  var
    tmpResult : String;
  begin
    tmpResult := ExpandPath('abc\def', 'xyz\..\abc');

    assertEqualsString('testExpandPath_WithDots', 'abc\def\abc', tmpResult);
  end;


  Procedure testExpandPath_WithDotsAtEnd;
  var
    tmpResult : String;
  begin
    tmpResult := ExpandPath('abc\def', 'xyz\..');

    assertEqualsString('testExpandPath_WithDotsAtEnd', 'abc\def', tmpResult);
  end;


  Procedure testExpandPath_WithDotsInFront;
  var
    tmpResult : String;
  begin
    tmpResult := ExpandPath('abc\def', '..\xyz');

    assertEqualsString('testExpandPath_WithDotsInFront', 'abc\xyz', tmpResult);
  end;


  Procedure testExpandPath_WithDotsInFrontReachingRoot;
  var
    tmpResult : String;
  begin
    tmpResult := ExpandPath('abc\def', '..\..\xyz');

    assertEqualsString('testExpandPath_WithDotsInFrontReachingRoot', '\xyz', tmpResult);
  end;


  Procedure testExpandPath_WithDotsInFrontReachingDrive;
  var
    tmpResult : String;
  begin
    tmpResult := ExpandPath('c:\abc\def', '..\..\xyz');

    assertEqualsString('testExpandPath_WithDotsInFront', 'c:\xyz', tmpResult);
  end;


  Procedure testExpandPath_WithDotsInFrontLeavingRoot;
  var
    tmpResult : String;
  begin
    tmpResult := ExpandPath('abc\def', '..\..\..\xyz');

    assertEqualsString('testExpandPath_WithDotsInFrontLeavingRoot', '\xyz', tmpResult);
  end;


  Procedure testExpandPath_WithDotsInFrontLeavingDrive;
  var
    tmpResult : String;
  begin
    tmpResult := ExpandPath('c:\abc\def', '..\..\..\xyz');

    assertEqualsString('testExpandPath_WithDotsInFrontLeavingDrive', 'c:\xyz', tmpResult);
  end;


  // ----------------------------------------------------------


  Procedure testGetLogFilesDir;
  var
    tmpResult : String;
  begin
    tmpResult := GetLogFilesDir;

    assertEqualsString('testGetLogFilesDir', 'C:\var\log\', tmpResult);
  end;


  // ----------------------------------------------------------

  Procedure testSearchPath_Found;
  var
    tmpResult : Boolean;
    tmpResultFilename : String;
  begin
    tmpResult := SearchPath('BOOKSHELF', 'CMDREF.INF', tmpResultFilename);

    assertTrue('testSearchPath_Found', tmpResult);
    assertEqualsString('testSearchPath_Found', 'C:\OS2\BOOK\CMDREF.INF', tmpResultFilename);
  end;


  Procedure testSearchPath_FoundMixedCase;
  var
    tmpResult : Boolean;
    tmpResultFilename : String;
  begin
    tmpResult := SearchPath('BOOKSHELF', 'cMdRef.iNf', tmpResultFilename);

    assertTrue('testSearchPath_FoundMixedCase', tmpResult);
    assertEqualsString('testSearchPath_FoundMixedCase', 'C:\OS2\BOOK\cMdRef.iNf', tmpResultFilename);
  end;


  Procedure testSearchPath_NotFound;
  var
    tmpResult : Boolean;
    tmpResultFilename : String;
  begin
    tmpResult := SearchPath('BOOKSHELF', 'RBRi.INF', tmpResultFilename);

    assertFalse('testSearchPath_NotFound', tmpResult);
    assertEqualsString('testSearchPath_NotFound', '', tmpResultFilename);
  end;


  Procedure testSearchPath_NotExistingEnvironment;
  var
    tmpResult : Boolean;
    tmpResultFilename : String;
  begin
    tmpResult := SearchPath('BUECHER', 'RBRi.INF', tmpResultFilename);

    assertFalse('testSearchPath_NotExistingEnvironment', tmpResult);
    assertEqualsString('testSearchPath_NotExistingEnvironment', '', tmpResultFilename);
  end;

  // ----------------------------------------------------------

  Procedure testSearchHelpPaths_FoundBookshelf;
  var
    tmpResult : Boolean;
    tmpResultFilename : String;
  begin
    tmpResult := SearchHelpPaths('CMDREF.INF', tmpResultFilename, false);

    assertTrue('testSearchHelpPaths_FoundBookshelf', tmpResult);
    assertEqualsString('testSearchHelpPaths_FoundBookshelf', 'C:\OS2\BOOK\CMDREF.INF', tmpResultFilename);
  end;


  Procedure testSearchHelpPaths_FoundHelp;
  var
    tmpResult : Boolean;
    tmpResultFilename : String;
  begin
    tmpResult := SearchHelpPaths('WPHELP.HLP', tmpResultFilename, false);

    assertTrue('testSearchHelpPaths_FoundHelp', tmpResult);
    assertEqualsString('testSearchHelpPaths_FoundHelp', 'C:\OS2\HELP\WPHELP.HLP', tmpResultFilename);
  end;

{ . is part of the helppath
  Procedure testSearchHelpPaths_DontSearchInAppDir;
  var
    tmpResult : Boolean;
    tmpResultFilename : String;
  begin
    tmpResult := SearchHelpPaths('NewViewTests.EXE', tmpResultFilename, False);

    assertFalse('testSearchHelpPaths_DontSearchInAppDir', tmpResult);
    assertEqualsString('testSearchHelpPaths_DontSearchInAppDir', '', tmpResultFilename);
  end;
}

  Procedure testSearchHelpPaths_FoundInAppDir;
  var
    tmpResult : Boolean;
    tmpResultFilename : String;
  begin
    tmpResult := SearchHelpPaths('NewViewTests.EXE', tmpResultFilename, True);

    assertTrue('testSearchHelpPaths_FoundInAppDir', tmpResult);
    assertEqualsString('testSearchHelpPaths_FoundInAppDir', 'P:\newview_dev\build\unittest\NewViewTests.EXE', tmpResultFilename);
  end;


  Procedure testSearchHelpPaths_NotFoundInAppDir;
  var
    tmpResult : Boolean;
    tmpResultFilename : String;
  begin
    tmpResult := SearchHelpPaths('NewView.EXE', tmpResultFilename, True);

    assertFalse('testSearchHelpPaths_NotFoundInAppDir', tmpResult);
    assertEqualsString('testSearchHelpPaths_NotFoundInAppDir', '', tmpResultFilename);
  end;

  // ----------------------------------------------------------

  Procedure testFindDefaultLanguageHelpFile;
  var
    tmpResult : String;
  begin
    tmpResult := FindDefaultLanguageHelpFile('NewView', '');

    assertEqualsString('testFindDefaultLanguageHelpFile', 'C:\ecs\help\NewView.hlp', tmpResult);
  end;

  Procedure testFindDefaultLanguageHelpFile_it;
  var
    tmpResult : String;
  begin
    tmpResult := FindDefaultLanguageHelpFile('NewView', 'it');

    assertEqualsString('testFindDefaultLanguageHelpFile_it', 'C:\ecs\help\NewView_it.hlp', tmpResult);
  end;

  Procedure testFindDefaultLanguageHelpFile_it_UpperCase;
  var
    tmpResult : String;
  begin
    tmpResult := FindDefaultLanguageHelpFile('NewView', 'IT');

    assertEqualsString('testFindDefaultLanguageHelpFile_it_UpperCase', 'C:\ecs\help\NewView_IT.hlp', tmpResult);
  end;


  // ----------------------------------------------------------


  Procedure testGetDirsInPath_Unknown;
  var
    tmpResult : TStringList;
  begin
    tmpResult := TStringList.Create;
    tmpResult.Add('Tester');

    GetDirsInPath('Unknown', tmpResult);

    assertEqualsInt('testGetDirsInPath_Unknown', 0, tmpResult.count);

    tmpResult.Destroy;
  end;


  Procedure testGetDirsInPath_Help;
  var
    tmpResult : TStringList;
  begin
    tmpResult := TStringList.Create;
    tmpResult.Add('Tester');

    GetDirsInPath('HELP', tmpResult);

    assertEqualsInt('testGetDirsInPath_Help', 15, tmpResult.count);
    assertEqualsString('testGetDirsInPath_Help', 'D:\progs\watcom\BINP\HELP', tmpResult[0]);
    assertEqualsString('testGetDirsInPath_Help', 'd:\progs\SIBYL\BIN', tmpResult[14]);

    tmpResult.Destroy;
  end;

  // ----------------------------------------------------------

  Procedure testListFilesInDirectory_NoFiles;
  var
    tmpResult : TStringList;
  begin
    tmpResult := TStringList.Create;

    ListFilesInDirectory('P:\newview_dev', '*.jonas', tmpResult);
    ListFilesInDirectory('P:\newview_dev', '*.jonas', tmpResult);

    assertEqualsInt('testListFilesInDirectory_NoFiles', 0, tmpResult.count);
  end;


  Procedure testListFilesInDirectory_EmptyFilter;
  var
    tmpResult : TStringList;
  begin
    tmpResult := TStringList.Create;

    ListFilesInDirectory('P:\newview_dev', '', tmpResult);

    assertEqualsInt('testListFilesInDirectory_EmptyFilter', 0, tmpResult.count);

    tmpResult.Destroy;
  end;


  Procedure testListFilesInDirectory_OneFile;
  var
    tmpResult : TStringList;
  begin
    tmpResult := TStringList.Create;

    ListFilesInDirectory('P:\newview_dev', '*.txt', tmpResult);

    assertEqualsInt('testListFilesInDirectory_OneFile', 1, tmpResult.count);
    assertEqualsString('testListFilesInDirectory_OneFile', 'P:\newview_dev\__readme.txt', tmpResult[0]);

    tmpResult.Destroy;
  end;


  Procedure testListFilesInDirectory_ManyFiles;
  var
    tmpResult : TStringList;
  begin
    tmpResult := TStringList.Create;

    ListFilesInDirectory('P:\newview_dev', '*.*', tmpResult);

    assertEqualsInt('testListFilesInDirectory_ManyFiles', 3, tmpResult.count);
    assertEqualsString('testListFilesInDirectory_ManyFiles', 'P:\newview_dev\env.cmd', tmpResult[0]);
    assertEqualsString('testListFilesInDirectory_ManyFiles', 'P:\newview_dev\med.cmd', tmpResult[1]);
    assertEqualsString('testListFilesInDirectory_ManyFiles', 'P:\newview_dev\__readme.txt', tmpResult[2]);

    tmpResult.Destroy;
  end;


  Procedure testListFilesInDirectory_ManyFilter;
  var
    tmpResult : TStringList;
  begin
    tmpResult := TStringList.Create;

    ListFilesInDirectory('P:\newview_dev', '*.txt;*v.cmd', tmpResult);

    assertEqualsInt('testListFilesInDirectory_ManyFilter', 2, tmpResult.count);
    assertEqualsString('testListFilesInDirectory_ManyFilter', 'P:\newview_dev\__readme.txt', tmpResult[0]);
    assertEqualsString('testListFilesInDirectory_ManyFilter', 'P:\newview_dev\env.cmd', tmpResult[1]);

    tmpResult.Destroy;
  end;

  // ----------------------------------------------------------

  Procedure testListSubDirectories_None;
  var
    tmpResult : TStringList;
  begin
    tmpResult := TStringList.Create;

    ListSubDirectories('P:\newview_dev\dll', tmpResult);

    assertEqualsInt('testListSubDirectories_None', 0, tmpResult.count);

    tmpResult.Destroy;
  end;


  Procedure testListSubDirectories_Many;
  var
    tmpResult : TStringList;
  begin
    tmpResult := TStringList.Create;

    ListSubDirectories('P:\newview_dev\i18n', tmpResult);

    assertEqualsInt('testListSubDirectories_Many', 14, tmpResult.count);
    assertEqualsString('testListSubDirectories_Many', 'P:\newview_dev\i18n\.svn', tmpResult[0]);
    assertEqualsString('testListSubDirectories_Many', 'P:\newview_dev\i18n\cz', tmpResult[1]);
    assertEqualsString('testListSubDirectories_Many', 'P:\newview_dev\i18n\de', tmpResult[2]);
    assertEqualsString('testListSubDirectories_Many', 'P:\newview_dev\i18n\eo', tmpResult[3]);

    tmpResult.Destroy;
  end;


  // ----------------------------------------------------------


  Procedure testListFilesInDirectoryRecursiveWithTermination;
  var
    tmpResult : TStringList;
  begin
    tmpResult := TStringList.Create;

    ListFilesInDirectoryRecursiveWithTermination('P:\newview_dev\i18n', '*.ipf;*.lng', tmpResult, nil, false);

    assertEqualsInt('testListFilesInDirectoryRecursiveWithTermination', 16, tmpResult.count);
    assertEqualsString('testListFilesInDirectoryRecursiveWithTermination', 'P:\newview_dev\i18n\NewView.ipf', tmpResult[0]);
    assertEqualsString('testListFilesInDirectoryRecursiveWithTermination', 'P:\newview_dev\i18n\sv\newview_sv.lng', tmpResult[15]);

    tmpResult.Destroy;
  end;


  // ----------------------------------------------------------


  Procedure testParentDir_Empty;
  var
    tmpResult : String;
  begin
    tmpResult := ParentDir('');

    assertEqualsString('testParentDir_Empty', '', tmpResult);
  end;


  Procedure testParentDir_Root;
  var
    tmpResult : String;
  begin
    tmpResult := ParentDir('C:\');

    assertEqualsString('testParentDir_Root', '', tmpResult);
  end;


  Procedure testParentDir_UnixRoot;
  var
    tmpResult : String;
  begin
    tmpResult := ParentDir('\');

    assertEqualsString('testParentDir_UnixRoot', '', tmpResult);
  end;


  Procedure testParentDir;
  var
    tmpResult : String;
  begin
    tmpResult := ParentDir('\abc\def');

    assertEqualsString('testParentDir', '\abc', tmpResult);
  end;


  Procedure testParentDir_OnlyOne;
  var
    tmpResult : String;
  begin
    tmpResult := ParentDir('\abc');

    assertEqualsString('testParentDir_OnlyOne', '', tmpResult);
  end;


  Procedure testParentDir_SlashAtEnd;
  var
    tmpResult : String;
  begin
    tmpResult := ParentDir('\abc\def\');

    assertEqualsString('testParentDir_SlashAtEnd', '\abc', tmpResult);
  end;


  Procedure testParentDir_NoSlashAtStart;
  var
    tmpResult : String;
  begin
    tmpResult := ParentDir('abc\def\');

    assertEqualsString('testParentDir_NoSlashAtStart', 'abc', tmpResult);
  end;


  Procedure testParentDir_NoSlash;
  var
    tmpResult : String;
  begin
    tmpResult := ParentDir('abc');

    assertEqualsString('testParentDir_NoSlash', '', tmpResult);
  end;


  Procedure testParentDir_GoToRootDrive;
  var
    tmpResult : String;
  begin
    tmpResult := ParentDir('c:\abc');

    assertEqualsString('testParentDir_GoToRootDrive', 'c:', tmpResult);
  end;


  // ----------------------------------------------------------


  Procedure testDirectoryExists_Empty;
  var
    tmpResult : Boolean;
  begin
    tmpResult := DirectoryExists('');

    assertTrue('testDirectoryExists_Empty', tmpResult);
  end;


  Procedure testDirectoryExists_DriveOnlyLowercase;
  var
    tmpResult : Boolean;
  begin
    tmpResult := DirectoryExists('c:');

    assertTrue('testDirectoryExists_DriveOnlyLowercase', tmpResult);
  end;


  Procedure testDirectoryExists_DriveOnlyUppercase;
  var
    tmpResult : Boolean;
  begin
    tmpResult := DirectoryExists('C:');

    assertTrue('testDirectoryExists_DriveOnlyUppercase', tmpResult);
  end;


  Procedure testDirectoryExists_DriveOnlySlashAtEnd;
  var
    tmpResult : Boolean;
  begin
    tmpResult := DirectoryExists('C:\');

    assertTrue('testDirectoryExists_DriveOnlySlashAtEnd', tmpResult);
  end;


  Procedure testDirectoryExists_DriveAndPath;
  var
    tmpResult : Boolean;
  begin
    tmpResult := DirectoryExists('C:\os2\bitmap');

    assertTrue('testDirectoryExists_DriveAndPath', tmpResult);
  end;


  Procedure testDirectoryExists_DriveAndPathSlashAtEnd;
  var
    tmpResult : Boolean;
  begin
    tmpResult := DirectoryExists('C:\os2\bitmap\');

    assertTrue('testDirectoryExists_DriveAndPathSlashAtEnd', tmpResult);
  end;


  Procedure testDirectoryExists_InvalidDrive;
  var
    tmpResult : Boolean;
  begin
    tmpResult := DirectoryExists('y:');

    assertFalse('testDirectoryExists_InvalidDrive', tmpResult);
  end;


  Procedure testDirectoryExists_InvalidDriveAndPath;
  var
    tmpResult : Boolean;
  begin
    tmpResult := DirectoryExists('y:\test');

    assertFalse('testDirectoryExists_InvalidDriveAndPath', tmpResult);
  end;


  Procedure testDirectoryExists_NotExistent;
  var
    tmpResult : Boolean;
  begin
    tmpResult := DirectoryExists('C:\os2\bit\');

    assertFalse('testDirectoryExists_NotExistent', tmpResult);
  end;



  // ----------------------------------------------------------


  Function getFileUtilsUnitTests : TList;
  Begin
    result := TList.Create;

    result.add(@testAddDirectorySeparator_Empty);
    result.add(@testAddDirectorySeparator_SingleChar);
    result.add(@testAddDirectorySeparator_ManyChars);
    result.add(@testAddDirectorySeparator_SlashAtEnd);

    result.add(@testAddDirectorySeparatorIfNotEmpty_Empty);
    result.add(@testAddDirectorySeparatorIfNotEmpty_SingleChar);
    result.add(@testAddDirectorySeparatorIfNotEmpty_ManyChars);
    result.add(@testAddDirectorySeparatorIfNotEmpty_SlashAtEnd);

    result.add(@testRemoveRightDirectorySeparator_Empty);
    result.add(@testRemoveRightDirectorySeparator_WithoutSlash);
    result.add(@testRemoveRightDirectorySeparator_WithSlash);

    result.add(@testExpandPath_BothEmpty);
    result.add(@testExpandPath_PathEmpty);
    result.add(@testExpandPath_PathEmptyDirEndsWithSlash);
    result.add(@testExpandPath_AbsolutePath);
    result.add(@testExpandPath_DriveWithSlash);
    result.add(@testExpandPath_DriveWithoutSlash);
    result.add(@testExpandPath_RootPathForDrive);
    result.add(@testExpandPath_RootDirForDrive);
    result.add(@testExpandPath_RootPathWithoutDrive);
    result.add(@testExpandPath_RootDirWithoutDrive);
    result.add(@testExpandPath_AppendWithSlash);
    result.add(@testExpandPath_AppendWithoutSlash);
    result.add(@testExpandPath_AppendWithSlashAtEnd);
    result.add(@testExpandPath_WithDot);
    result.add(@testExpandPath_WithDotAtEnd);
    result.add(@testExpandPath_WithDots);
    result.add(@testExpandPath_WithDotsAtEnd);
    result.add(@testExpandPath_WithDotsInFront);
    result.add(@testExpandPath_WithDotsInFrontReachingRoot);
    result.add(@testExpandPath_WithDotsInFrontReachingDrive);
    result.add(@testExpandPath_WithDotsInFrontLeavingRoot);
    result.add(@testExpandPath_WithDotsInFrontLeavingDrive);

    result.add(@testGetLogFilesDir);

    result.add(@testSearchPath_Found);
    result.add(@testSearchPath_FoundMixedCase);
    result.add(@testSearchPath_NotFound);
    result.add(@testSearchPath_NotExistingEnvironment);

    result.add(@testSearchHelpPaths_FoundBookshelf);
    result.add(@testSearchHelpPaths_FoundHelp);
//    result.add(@testSearchHelpPaths_DontSearchInAppDir);
    result.add(@testSearchHelpPaths_FoundInAppDir);
    result.add(@testSearchHelpPaths_NotFoundInAppDir);

    result.add(@testFindDefaultLanguageHelpFile);
    result.add(@testFindDefaultLanguageHelpFile_it);
    result.add(@testFindDefaultLanguageHelpFile_it_UpperCase);

    result.add(@testGetDirsInPath_Unknown);
    result.add(@testGetDirsInPath_Help);

    result.add(@testListFilesInDirectory_NoFiles);
    result.add(@testListFilesInDirectory_EmptyFilter);
    result.add(@testListFilesInDirectory_OneFile);
    result.add(@testListFilesInDirectory_ManyFiles);
    result.add(@testListFilesInDirectory_ManyFilter);

    result.add(@testListSubDirectories_None);
    result.add(@testListSubDirectories_Many);

    result.add(@testListFilesInDirectoryRecursiveWithTermination);

    result.add(@testParentDir_Empty);
    result.add(@testParentDir_Root);
    result.add(@testParentDir_UnixRoot);
    result.add(@testParentDir);
    result.add(@testParentDir_OnlyOne);
    result.add(@testParentDir_SlashAtEnd);
    result.add(@testParentDir_NoSlashAtStart);
    result.add(@testParentDir_NoSlash);
    result.add(@testParentDir_GoToRootDrive);

    result.add(@testDirectoryExists_Empty);
    result.add(@testDirectoryExists_DriveOnlyLowercase);
    result.add(@testDirectoryExists_DriveOnlyUppercase);
    result.add(@testDirectoryExists_DriveOnlySlashAtEnd);
    result.add(@testDirectoryExists_DriveAndPath);
    result.add(@testDirectoryExists_DriveAndPathSlashAtEnd);
    result.add(@testDirectoryExists_InvalidDrive);
    result.add(@testDirectoryExists_InvalidDriveAndPath);
    result.add(@testDirectoryExists_NotExistent);
  end;

End.
