Unit CompatibleFileUtilsUnitTests;

Interface

uses
  Classes,
  TestAssert,
  ACLFileUtility;


  FUNCTION getFileUtilsUnitTests : TList;




Implementation

  Procedure testAddSlash_Empty;
  var
    tmpResult : String;
  begin
    tmpResult := AddSlash('');

    assertEqualsString('testAddSlash_Empty', '\', tmpResult);
  end;

  Procedure testAddSlash_SingleChar;
  var
    tmpResult : String;
  begin
    tmpResult := AddSlash('x');

    assertEqualsString('testAddSlash_SingleChar', 'x\', tmpResult);
  end;

  Procedure testAddSlash_ManyChars;
  var
    tmpResult : String;
  begin
    tmpResult := AddSlash('xyz dkdj ');

    assertEqualsString('testAddSlash_ManyChars', 'xyz dkdj \', tmpResult);
  end;


  Procedure testAddSlash_SlashAtEnd;
  var
    tmpResult : String;
  begin
    tmpResult := AddSlash('xy\');

    assertEqualsString('testAddSlash_SlashAtEnd', 'xy\', tmpResult);
  end;


  // ----------------------------------------------------------

{
  Procedure testAddSlashIfNotEmpty_Empty;
  var
    tmpResult : String;
  begin
    tmpResult := AddSlashIfNotEmpty('');

    assertEqualsString('testAddSlashIfNotEmpty_Empty', '', tmpResult);
  end;

  Procedure testAddSlashIfNotEmpty_SingleChar;
  var
    tmpResult : String;
  begin
    tmpResult := AddSlashIfNotEmpty('x');

    assertEqualsString('testAddSlashIfNotEmpty_SingleChar', 'x\', tmpResult);
  end;

  Procedure testAddSlashIfNotEmpty_ManyChars;
  var
    tmpResult : String;
  begin
    tmpResult := AddSlashIfNotEmpty('xyz dkdj ');

    assertEqualsString('testAddSlashIfNotEmpty_ManyChars', 'xyz dkdj \', tmpResult);
  end;


  Procedure testAddSlashIfNotEmpty_SlashAtEnd;
  var
    tmpResult : String;
  begin
    tmpResult := AddSlashIfNotEmpty('xy\');

    assertEqualsString('testAddSlashIfNotEmpty_SlashAtEnd', 'xy\', tmpResult);
  end;
}
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
{
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

 }
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

    assertEqualsInt('testGetDirsInPath_Help', 16, tmpResult.count);
    assertEqualsString('testGetDirsInPath_Help', 'D:\progs\watcom\BINP\HELP', tmpResult[0]);
    assertEqualsString('testGetDirsInPath_Help', 'd:\progs\SIBYL\BIN', tmpResult[14]);
    assertEqualsString('testGetDirsInPath_Help', '', tmpResult[15]);

    tmpResult.Destroy;
  end;

  // ----------------------------------------------------------

  Procedure testListFilesInDirectory_NoFiles;
  var
    tmpResult : TStringList;
  begin
    tmpResult := TStringList.Create;

    ListDirectoryAdditive('P:\newview_dev', '*.jonas', 'P:\newview_dev', tmpResult, nil);

    assertEqualsInt('testListFilesInDirectory_NoFiles', 0, tmpResult.count);
  end;


  Procedure testListFilesInDirectory_EmptyFilter;
  var
    tmpResult : TStringList;
  begin
    tmpResult := TStringList.Create;

    ListDirectoryAdditive('P:\newview_dev', '', 'P:\newview_dev', tmpResult, nil);

    assertEqualsInt('testListFilesInDirectory_EmptyFilter', 0, tmpResult.count);

    tmpResult.Destroy;
  end;


  Procedure testListFilesInDirectory_OneFile;
  var
    tmpResult : TStringList;
  begin
    tmpResult := TStringList.Create;

    ListDirectoryAdditive('P:\newview_dev', '*.txt', 'P:\newview_dev', tmpResult, nil);

    assertEqualsInt('testListFilesInDirectory_OneFile', 1, tmpResult.count);
    assertEqualsString('testListFilesInDirectory_OneFile', 'P:\newview_dev\__readme.txt', tmpResult[0]);

    tmpResult.Destroy;
  end;


  Procedure testListFilesInDirectory_ManyFiles;
  var
    tmpResult : TStringList;
  begin
    tmpResult := TStringList.Create;

    ListDirectoryAdditive('P:\newview_dev', '*.*', 'P:\newview_dev', tmpResult, nil);

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

    ListDirectoryAdditive('P:\newview_dev', '*.txt;*v.cmd', 'P:\newview_dev', tmpResult, nil);

    assertEqualsInt('testListFilesInDirectory_ManyFilter', 2, tmpResult.count);
    assertEqualsString('testListFilesInDirectory_ManyFilter', 'P:\newview_dev\__readme.txt', tmpResult[0]);
    assertEqualsString('testListFilesInDirectory_ManyFilter', 'P:\newview_dev\env.cmd', tmpResult[1]);

    tmpResult.Destroy;
  end;

  // ----------------------------------------------------------
{
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
}
  // ----------------------------------------------------------

  Procedure testListFilesInDirectoryRecursiveWithTermination;
  var
    tmpResult : TStringList;
  begin
    tmpResult := TStringList.Create;

//    ListFilesInDirectoryRecursiveWithTermination('P:\newview_dev\i18n', '*.ipf;*.lng', tmpResult, nil, false);
    ListDirectoryRecursiveAdditive2('P:\newview_dev\i18n', '*.ipf;*.lng', 'P:\newview_dev\i18n', tmpResult, nil, nil, false);

    assertEqualsInt('testListFilesInDirectoryRecursiveWithTermination', 16, tmpResult.count);
    assertEqualsString('testListFilesInDirectoryRecursiveWithTermination', 'P:\newview_dev\i18n\NewView.ipf', tmpResult[0]);
    assertEqualsString('testListFilesInDirectoryRecursiveWithTermination', 'P:\newview_dev\i18n\sv\newview_sv.lngx', tmpResult[15]);

    tmpResult.Destroy;
  end;

  // ----------------------------------------------------------

  Function getFileUtilsUnitTests : TList;
  Begin
    result := TList.Create;

    result.add(@testAddSlash_Empty);
    result.add(@testAddSlash_SingleChar);
    result.add(@testAddSlash_ManyChars);
    result.add(@testAddSlash_SlashAtEnd);

//    result.add(@testAddSlashIfNotEmpty_Empty);
//    result.add(@testAddSlashIfNotEmpty_SingleChar);
//    result.add(@testAddSlashIfNotEmpty_ManyChars);
//    result.add(@testAddSlashIfNotEmpty_SlashAtEnd);

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

//    result.add(@testFindDefaultLanguageHelpFile);
//    result.add(@testFindDefaultLanguageHelpFile_it);
//    result.add(@testFindDefaultLanguageHelpFile_it_UpperCase);

    result.add(@testGetDirsInPath_Unknown);
    result.add(@testGetDirsInPath_Help);

    result.add(@testListFilesInDirectory_NoFiles);
    result.add(@testListFilesInDirectory_EmptyFilter);
    result.add(@testListFilesInDirectory_OneFile);
    result.add(@testListFilesInDirectory_ManyFiles);
    result.add(@testListFilesInDirectory_ManyFilter);

//    result.add(@testListSubDirectories_None);
//    result.add(@testListSubDirectories_Many);

    result.add(@testListFilesInDirectoryRecursiveWithTermination);
  end;

End.
