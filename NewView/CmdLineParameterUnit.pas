Unit CmdLineParameterUnit;

// NewView - a new OS/2 Help Viewer
// Copyright 2006 Ronald Brill (rbri at rbri dot de)
// This software is released under the GNU Public License - see readme.txt

// Helper functions to address the command line parameters newview
// is started with

Interface

uses
  Os2Def,
  BseTib,
  BseDos,
  SysUtils,
  Classes,
  PMWIN,
  StringUtilsUnit,
  DebugUnit;

 CONST
     SUCCESS = 0;
     ERROR_UNMATCHED_QUOTE = -1;

 TYPE EParsingFailed=CLASS(Exception);

 TYPE
     TWindowPosition = record
         left: longint;
         bottom: longint;
         width: longint;
         height: longint;
     end;
 TYPE
     TCmdLineParameters = class
     private
       commandLine : String;
       showUsageFlag : boolean;
       searchFlag : boolean;
       globalSearchFlag : boolean;
       language : string;
       helpManagerFlag : boolean;
       helpManagerWindow : integer;
       windowPositionFlag: boolean;
       windowPosition: TWindowPosition;
       ownerWindow : integer;
       windowTitle : string;
       fileNames : string;
       fileNamesRaw : string;
       searchText : string;

       currentParsePosition : integer;

       FUNCTION ReadNextPart(const aParseString : String; const aSetOfDelimiterChars : TSetOfChars): String;
       FUNCTION handleParamWithValue(const aCmdLineString : String; const aSwitch : String; var aValue : String) : Boolean;

     public
       PROPERTY getCommandLine : String read commandLine;
       PROPERTY getShowUsageFlag : boolean read showUsageFlag;
       PROPERTY getSearchFlag : boolean read searchFlag;
       PROPERTY getGlobalSearchFlag : boolean read globalSearchFlag;
       PROPERTY getLanguage : string read language;
       PROPERTY getHelpManagerFlag : boolean read helpManagerFlag;
       FUNCTION setHelpManagerFlag(aNewValue : boolean) : boolean;
       PROPERTY getHelpManagerWindow : integer read helpManagerWindow;
       PROPERTY getWindowPositionFlag : boolean read windowPositionFlag;
       PROPERTY getWindowPosition : TWindowPosition read windowPosition;
       PROPERTY getOwnerWindow : integer read ownerWindow;
       PROPERTY getWindowTitle : string read windowTitle;
       PROPERTY getFileNames : string read fileNames;
       PROPERTY getFileNamesRaw : string read fileNamesRaw;
       PROPERTY getSearchText : string read searchText;

       PROCEDURE parseCmdLine(aCmdLineString : String);

       FUNCTION getInterpretedFileNames: String;
       FUNCTION getInterpretedSearchText: String;
     private
       PROCEDURE parseSwitch(aCmdLineString : String);
  end;

  FUNCTION getOwnHelpFileName: String;

  // returns a string containing the whole
  // command line parametes
  FUNCTION nativeOS2GetCmdLineParameter : String;


Implementation
uses
  ACLFileUtility;

  FUNCTION TCmdLineParameters.getInterpretedFileNames: String;
  var
    tmpOwnHelpFileName : String;
  begin
    result := getFileNames;

    if getGlobalSearchFlag
       AND (getSearchText = '')
    then
    begin
      result := '';
      exit;
    end;


    tmpOwnHelpFileName := FindDefaultLanguageHelpFile('NewView');
    if (result = '') AND
      FileExists(tmpOwnHelpFileName)
    then
      result := tmpOwnHelpFileName;
  end;


  FUNCTION TCmdLineParameters.getInterpretedSearchText: String;
  begin
    result := getSearchText;

    if getGlobalSearchFlag
       AND (result = '')
    then
      result := getFileNamesRaw;
  end;


  FUNCTION TCmdLineParameters.setHelpManagerFlag(aNewValue : boolean) : boolean;
  begin
       helpManagerFlag := aNewValue;
       result := helpManagerFlag;
  end;


  procedure TCmdLineParameters.parseCmdLine(aCmdLineString : String);
  var
    tmpState : (SWITCH, FILENAME, FILENAME_QUOTE, TEXT);
    tmpCurrentChar : char;
  begin
     LogEvent(LogStartup, 'ParseCommandLine: "' + aCmdLineString + '"');

     // store the original string for debugging
     commandLine := aCmdLineString;

     // reset the whole object
     showUsageFlag := false;
     searchFlag := false;
     globalSearchFlag := false;
     language := '';
     helpManagerFlag := false;
     helpManagerWindow := 0;
     windowPositionFlag := false;
     ownerWindow := 0;
     windowTitle := '';
     searchText := '';
     fileNames := '';
     fileNamesRaw := '';

     try
       // start parsing
       tmpState := FILENAME;
       currentParsePosition := 1;
       while currentParsePosition <= length(aCmdLineString) do
       begin
         tmpCurrentChar := aCmdLineString[currentParsePosition];

         Case tmpCurrentChar of
           ' ' :
             begin
               Case tmpState of
                 SWITCH :
                 begin
                   tmpState := FILENAME;
                   inc(currentParsePosition);
                 end;
                 FILENAME :
                 begin
                   if length(fileNames) > 0 then
                   begin
                     tmpState := TEXT;
                   end;
                   inc(currentParsePosition);
                 end;
                 FILENAME_QUOTE :
                 begin
                   fileNames := fileNames + tmpCurrentChar;
                   fileNamesRaw := fileNamesRaw + tmpCurrentChar;
                   inc(currentParsePosition);
                 end;
                 TEXT :
                 begin
                   searchText := searchText + tmpCurrentChar;
                   inc(currentParsePosition);
                 end;
               end;
             end;

           '/', '-' :
             begin
               Case tmpState of
                 SWITCH :
                 begin
                   tmpState := SWITCH;
                   parseSwitch(aCmdLineString);
                 end;
                 FILENAME :
                 begin
                   if length(fileNames) < 1 then
                   begin
                     tmpState := SWITCH;
                     parseSwitch(aCmdLineString);
                   end
                   else
                   begin
                     fileNames := fileNames + tmpCurrentChar;
                     fileNamesRaw := fileNamesRaw + tmpCurrentChar;
                     inc(currentParsePosition);
                   end;
                 end;
                 FILENAME_QUOTE :
                 begin
                   fileNames := fileNames + tmpCurrentChar;
                   fileNamesRaw := fileNamesRaw + tmpCurrentChar;
                   inc(currentParsePosition);
                 end;
               else
                 begin
                   searchText := searchText + tmpCurrentChar;
                   inc(currentParsePosition);
                 end;
               end;
             end;

           '"' :
             begin
               Case tmpState of
                 SWITCH :
                 begin
                   // syntax error
                   raise EParsingFailed.Create('Unsupported switch');
                 end;
                 FILENAME :
                 begin
                   tmpState := FILENAME_QUOTE;
                   fileNamesRaw := fileNamesRaw + tmpCurrentChar;
                   inc(currentParsePosition);
                 end;
                 FILENAME_QUOTE :
                 begin
                   tmpState := FILENAME;
                   fileNamesRaw := fileNamesRaw + tmpCurrentChar;
                   inc(currentParsePosition);
                 end;
                 TEXT :
                 begin
                   searchText := searchText + tmpCurrentChar;
                   inc(currentParsePosition);
                 end;
               end;
             end;

           else
             begin
               Case tmpState of
                 SWITCH :
                 begin
                   // syntax error
                   raise EParsingFailed.Create('Unsupported switch');
                 end;
                 FILENAME :
                 begin
                   fileNames := fileNames + tmpCurrentChar;
                   fileNamesRaw := fileNamesRaw + tmpCurrentChar;
                   inc(currentParsePosition);
                 end;
                 FILENAME_QUOTE :
                 begin
                   fileNames := fileNames + tmpCurrentChar;
                   fileNamesRaw := fileNamesRaw + tmpCurrentChar;
                   inc(currentParsePosition);
                 end;
               else
               begin
                 searchText := searchText + tmpCurrentChar;
                 inc(currentParsePosition);
               end;
             end;
           end;
        end;
      end;

    except
        on e:EParsingFailed do
        begin
          showUsageFlag := true;
        end;
    end;

    // remove leading blanks from search text
    searchText := StrTrim(searchText);

    LogEvent(LogStartup, 'Parameters parsed');
    LogEvent(LogStartup, '  Filename(s): "' + fileNames + '"');
    LogEvent(LogStartup, '  Search Text: "' + searchText + '"');
  end;


  FUNCTION TCmdLineParameters.ReadNextPart(const aParseString : String; const aSetOfDelimiterChars : TSetOfChars): String;
  VAR
    i : integer;
    tmpChar : char;
  BEGIN
    result := '';
    for i:= currentParsePosition to length(aParseString) do
    begin
      tmpChar := aParseString[i];
      if tmpChar in aSetOfDelimiterChars then
      begin
        i := length(aParseString); // stop parsing
      end
      else
      begin
        result := result + tmpChar;
      end;
    end;
  END;



  Function TCmdLineParameters.handleParamWithValue(const aCmdLineString : String; const aSwitch : String; var aValue : String) : Boolean;
  var
    tmpText : String;
    tmpSwitchLength : integer;
  begin
    tmpSwitchLength := Length(aSwitch);
    tmpText := copy(aCmdLineString, currentParsePosition + 1, tmpSwitchLength);
    tmpText := lowercase(tmpText);

    if (lowercase(aSwitch) = tmpText) then
    begin
      currentParsePosition := currentParsePosition + 1 + tmpSwitchLength;
      if aCmdLineString[currentParsePosition] = ':' then
      begin
        inc(currentParsePosition);
      end;

      aValue := readNextPart(aCmdLineString, [' ', '-', '/']);
      currentParsePosition := currentParsePosition + length(aValue);
      result := true;
      exit;
    end;
    result := false;
  end;


  Function ParseWindowPositionPart(const aPart: String; const aScreenDimension: longint): longint;
  Var
    tmpPart : String;
  Begin
    if aPart = '' then
      raise EParsingFailed.Create('Missing position element');

    if StrEndsWithIgnoringCase(aPart, 'P') then
    begin
      tmpPart := copy(aPart, 1, length(aPart)-1);
      if tmpPart = '' then
        raise EParsingFailed.Create('Missing position element');

      Result := StrToInt(tmpPart);
      if Result < 0 then
        Result := 0;
      if Result > 100 then
        Result := 100;
      Result := Round(Result / 100 * aScreenDimension);
    end
    else
    begin
      Result := StrToInt(aPart);
    end;
  end;

  Function ParseWindowPosition(const aParamValue: String): TWindowPosition;
  Var
    tmpParts : TStringList;
  Begin
    tmpParts := TStringList.Create;
    StrExtractStrings(tmpParts, aParamValue, [','], '\');

    result.Left := ParseWindowPositionPart(tmpParts[0], WinQuerySysValue(HWND_DESKTOP, SV_CXSCREEN));
    result.Bottom := ParseWindowPositionPart(tmpParts[1], WinQuerySysValue(HWND_DESKTOP, SV_CYSCREEN));

    result.Width := ParseWindowPositionPart(tmpParts[2], WinQuerySysValue(HWND_DESKTOP, SV_CXSCREEN));
    if result.Width < 50 then
      result.Width := 50;

    result.Height := ParseWindowPositionPart(tmpParts[3], WinQuerySysValue(HWND_DESKTOP, SV_CYSCREEN));
    if result.Height < 50 then
      result.Height := 50;

    tmpParts.Destroy;
  end;


  Procedure TCmdLineParameters.parseSwitch(aCmdLineString : String);
  var
    tmpCurrentChar : char;
    tmpText : String;
    tmpValue : String;
  begin
    // lang
    if handleParamWithValue(aCmdLineString, 'lang', tmpValue) then
    begin
      language := tmpValue;
      exit;
    end;

    // title
    if handleParamWithValue(aCmdLineString, 'title', tmpValue) then
    begin
      windowTitle := tmpValue;
      exit;
    end;

    // HM
    if handleParamWithValue(aCmdLineString, 'hm', tmpValue) then
    begin
      try
        helpManagerWindow := StrToInt(tmpValue);
        helpManagerFlag := true;
      except
        on e:Exception do
        begin
          showUsageFlag := true;
        end;
      end;
      exit;
    end;

    // owner
    if handleParamWithValue(aCmdLineString, 'owner', tmpValue) then
    begin
      try
        ownerWindow := StrToInt(tmpValue);
      except
        on e:Exception do
        begin
          showUsageFlag := true;
        end;
      end;
      exit;
    end;

    // pos
    if handleParamWithValue(aCmdLineString, 'pos', tmpValue) then
    begin
      windowPosition := ParseWindowPosition(tmpValue);
      windowPositionFlag := true;
      exit;
    end;

    // check the next char
    tmpCurrentChar := aCmdLineString[currentParsePosition + 1];
    Case tmpCurrentChar of
      'h', 'H', '?' :
        begin
          currentParsePosition := currentParsePosition + 2;
          showUsageFlag := true;

          // check for 'help'
          tmpText := copy(aCmdLineString, currentParsePosition, 3);
          tmpText := lowercase(tmpText);

          if ('elp' = tmpText) then
          begin
            currentParsePosition := currentParsePosition + 3;
          end;
        end;

      's', 'S' :
        begin
          currentParsePosition := currentParsePosition + 2;
          searchFlag := true;
        end;

      'g', 'G' :
        begin
          currentParsePosition := currentParsePosition + 2;
          globalSearchFlag := true;
        end;

      else
        begin
          raise EParsingFailed.Create('Unsupported switch');
        end;
      end;
  end;


  FUNCTION getOwnHelpFileName: String;
  begin
    result := FindDefaultLanguageHelpFile('NewView');
  end;


  FUNCTION nativeOS2GetCmdLineParameter : STRING;
  VAR
    tmpPtib : PTIB;       // thread information block
    tmpPpib : PPIB;       // process information block
    tmpCmd  : PCHAR;
    tmpResult : PCHAR;

  BEGIN
    // ask the system
    DosGetInfoBlocks(tmpPtib, tmpPpib);
    tmpCmd := tmpPpib^.pib_pchcmd;
    // the fist element (null terminated) is the
    // called command itself
    // skip to the next null terminated string
    // these are the parameters
    tmpResult := tmpCmd + StrLen(tmpCmd) + 1;
    result := StrPas(tmpResult);
  END;
END.
