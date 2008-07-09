Unit WebBrowserUnit;

// NewView - a new OS/2 Help Viewer
// Copyright 2003 Aaron Lawrence (aaronl at consultant dot com)
// Copyright 2006-2007 Ronald Brill (rbri at rbri dot de)
// This software is released under the Gnu Public License - see readme.txt

Interface

  // Code for running the default browser.
  // Doesn't implement DDE!
  Procedure LaunchURL(const aURL: String);


Implementation

uses
  PMSHL,
  OS2Def,
  ACLUtility,
  SysUtils,
  RunProgramUnit,
  FileUtilsUnit,
  StringUtilsUnit,
  DebugUnit;

const
  PROFILE_APPLICATION_WPURLDEFAULTSETTINGS = 'WPURLDEFAULTSETTINGS';
  // Browser
  PROFILE_KEY_DEFAULT_BROWSER_EXE = 'DefaultBrowserExe';
  PROFILE_KEY_DEFAULT_WORKING_DIR = 'DefaultWorkingDir';
  PROFILE_KEY_DEFAULT_PARAMETERS = 'DefaultParameters';

  PROFILE_KEY_DEFAULT_MAIL_EXE = 'DefaultMailExe';
  PROFILE_KEY_DEFAULT_MAIL_WORKING_DIR = 'DefaultMailWorkingDir';
  PROFILE_KEY_DEFAULT_MAIL_PARAMETERS = 'DefaultMailParameters';

  PROFILE_KEY_DEFAULT_NEWS_EXE = 'DefaultNewsExe';
  PROFILE_KEY_DEFAULT_NEWS_WORKING_DIR = 'DefaultNewsWorkingDir';
  PROFILE_KEY_DEFAULT_NEWS_PARAMETERS = 'DefaultNewsParameters';

  PROFILE_KEY_DEFAULT_FTP_EXE = 'DefaultFTPExe';
  PROFILE_KEY_DEFAULT_FTP_WORKING_DIR = 'DefaultFTPWorkingDir';
  PROFILE_KEY_DEFAULT_FTP_PARAMETERS = 'DefaultFTPParameters';


  Function GetDefaultBrowserExe : String;
  begin
    Result := GetUserProfileString(PROFILE_APPLICATION_WPURLDEFAULTSETTINGS, PROFILE_KEY_DEFAULT_BROWSER_EXE, '');
    if Result = '' then
    begin
      // try Web Explorer
      SearchPath('PATH', 'explore.exe', Result);
    end;
  end;


  Function GetDefaultBrowserWorkingDir : String;
  begin
    Result := GetUserProfileString(PROFILE_APPLICATION_WPURLDEFAULTSETTINGS, PROFILE_KEY_DEFAULT_WORKING_DIR, '');
    if Result = '' then
    begin
      Result := ExtractFilePath(GetDefaultBrowserExe);
    end;
  end;


  Function GetDefaultParameters : String;
  begin
    Result := GetUserProfileString(PROFILE_APPLICATION_WPURLDEFAULTSETTINGS, PROFILE_KEY_DEFAULT_PARAMETERS, '');
  end;


  Function GetDefaultMailExe : String;
  begin
    Result := GetUserProfileString(PROFILE_APPLICATION_WPURLDEFAULTSETTINGS, PROFILE_KEY_DEFAULT_MAIL_EXE, '');
  end;


  Function GetDefaultMailWorkingDir : String;
  begin
    Result := GetUserProfileString(PROFILE_APPLICATION_WPURLDEFAULTSETTINGS, PROFILE_KEY_DEFAULT_MAIL_WORKING_DIR, '');
    if Result = '' then
    begin
      Result := ExtractFilePath(GetDefaultMailExe);
    end;
  end;


  Function GetDefaultMailParameters : String;
  begin
    Result := GetUserProfileString(PROFILE_APPLICATION_WPURLDEFAULTSETTINGS, PROFILE_KEY_DEFAULT_MAIL_PARAMETERS, '');
  end;


  Function GetDefaultNewsExe : String;
  begin
    Result := GetUserProfileString(PROFILE_APPLICATION_WPURLDEFAULTSETTINGS, PROFILE_KEY_DEFAULT_NEWS_EXE, '');
  end;


  Function GetDefaultNewsWorkingDir : String;
  begin
    Result := GetUserProfileString(PROFILE_APPLICATION_WPURLDEFAULTSETTINGS, PROFILE_KEY_DEFAULT_NEWS_WORKING_DIR, '');
    if Result = '' then
    begin
      Result := ExtractFilePath(GetDefaultNewsExe);
    end;
  end;


  Function GetDefaultNewsParameters : String;
  begin
    Result := GetUserProfileString(PROFILE_APPLICATION_WPURLDEFAULTSETTINGS, PROFILE_KEY_DEFAULT_NEWS_PARAMETERS, '');
  end;


  Function GetDefaultFtpExe : String;
  begin
    Result := GetUserProfileString(PROFILE_APPLICATION_WPURLDEFAULTSETTINGS, PROFILE_KEY_DEFAULT_FTP_EXE, '');
  end;


  Function GetDefaultFtpWorkingDir : String;
  begin
    Result := GetUserProfileString(PROFILE_APPLICATION_WPURLDEFAULTSETTINGS, PROFILE_KEY_DEFAULT_FTP_WORKING_DIR, '');
    if Result = '' then
    begin
      Result := ExtractFilePath(GetDefaultFtpExe);
    end;
  end;


  Function GetDefaultFtpParameters : String;
  begin
    Result := GetUserProfileString(PROFILE_APPLICATION_WPURLDEFAULTSETTINGS, PROFILE_KEY_DEFAULT_FTP_PARAMETERS, '');
  end;


  Procedure LaunchURL(const aURL: String);
  var
    tmpExe : String;
    tmpAlternativeExe : String;
    tmpWorkingDir : String;
    tmpParameters : String;
  begin
    // default is to start the browser
    tmpExe := GetDefaultBrowserExe;
    tmpWorkingDir := GetDefaultBrowserWorkingDir;
    tmpParameters := GetDefaultParameters;

    if not FileExists(tmpExe) then
    begin
      raise Exception.Create('Browser program doesn''t exist: ' + tmpExe);
    end;

    if tmpExe = '' then
    begin
      raise Exception.Create('You don''t have a default browser configured.');
    end;

    if StrStartsWithIgnoringCase(aURL, '"ftp')
       or StrStartsWithIgnoringCase(aURL, 'ftp')
    then
    begin
      tmpAlternativeExe := GetDefaultFtpExe;
      if tmpAlternativeExe <> '' then
      begin
        tmpExe := tmpAlternativeExe;
        tmpWorkingDir := GetDefaultFtpWorkingDir;
        tmpParameters := GetDefaultFtpParameters;
      end
    end
    else if StrStartsWithIgnoringCase(aURL, '"mailto')
            or StrStartsWithIgnoringCase(aURL, 'mailto')
    then
    begin
      tmpAlternativeExe := GetDefaultMailExe;
      if tmpAlternativeExe <> '' then
      begin
        tmpExe := tmpAlternativeExe;
        tmpWorkingDir := GetDefaultMailWorkingDir;
        tmpParameters := GetDefaultMailParameters;
      end
    end
    else if StrStartsWithIgnoringCase(aURL, '"news')
            or StrStartsWithIgnoringCase(aURL, '"news')
    then
    begin
      tmpAlternativeExe := GetDefaultNewsExe;
      if tmpAlternativeExe <> '' then
      begin
        tmpExe := tmpAlternativeExe;
        tmpWorkingDir := GetDefaultNewsWorkingDir;
        tmpParameters := GetDefaultNewsParameters;
      end
    end;

    ChDir(RemoveRightDirectorySeparator(tmpWorkingDir));
    LaunchProgram(tmpExe, tmpParameters + ' ' + aURL, tmpWorkingDir);
  end;

Initialization
End.
