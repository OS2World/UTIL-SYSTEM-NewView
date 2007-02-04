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

//       FUNCTION ReadNextPart(const aParseString : String; const aSetOfDelimiterChars : TSetOfChars): String;
       FUNCTION handleSwitchWithValue(const aSwitchString : String; const aSwitch : String; var aValue : String) : Boolean;
       PROCEDURE parseSwitch(aSwitchString : String);
       PROPERTY getFileNames : string read fileNames;
       PROPERTY getSearchText : string read searchText;

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
       PROPERTY getFileNamesRaw : string read fileNamesRaw;

       PROCEDURE writeDetailsTo(aStrings : TStrings);
       PROCEDURE parseCmdLine(aCmdLineString : String);

       FUNCTION getInterpretedFileNames: String;
       FUNCTION getInterpretedSearchText: String;
  end;

  FUNCTION getOwnHelpFileName: String;

  // returns a string containing the whole
  // command line parametes
  FUNCTION nativeOS2GetCmdLineParameter : String;


Implementation
uses
  ACLFileUtility;

  PROCEDURE TCmdLineParameters.writeDetailsTo(aStrings : TStrings);
  var
    tmpWindowPosition : TWindowPosition;
  begin
    aStrings.Add('''' + getCommandLine + '''');
    aStrings.Add('parsed infos:');

    aStrings.Add('getShowUsageFlag: ' + boolToStr(getShowUsageFlag));
    aStrings.Add('getSearchFlag: ' + boolToStr(getSearchFlag));
    aStrings.Add('getSearchText: ' + getSearchText);
    aStrings.Add('getGlobalSearchFlag: ' + boolToStr(getGlobalSearchFlag));
    aStrings.Add('getLanguage: ' + getLanguage);
    aStrings.Add('getHelpManagerFlag: ' + boolToStr(getHelpManagerFlag));
    aStrings.Add('getHelpManagerFlag: ' + boolToStr(getHelpManagerFlag));
    aStrings.Add('getHelpManagerWindow: ' + intToStr(getHelpManagerWindow));
    aStrings.Add('getWindowPositionFlag: ' + boolToStr(getWindowPositionFlag));
    aStrings.Add('getFileNames: ' + getFileNames);
    aStrings.Add('getInterpretedSearchText: ' + getInterpretedSearchText);
    aStrings.Add('getInterpretedFileNames: ' + getInterpretedFileNames);

    tmpWindowPosition := getWindowPosition;
    aStrings.Add('getWindowPosition: '
                        + intToStr(tmpWindowPosition.left) + ', '
                        + intToStr(tmpWindowPosition.bottom) + ', '
                        + intToStr(tmpWindowPosition.width) + ', '
                        + intToStr(tmpWindowPosition.height)
                );
    aStrings.Add('getOwnerWindow: ' + intToStr(getOwnerWindow));
    aStrings.Add('getWindowTitle: ' + getWindowTitle);
  end;


  Function TCmdLineParameters.getInterpretedFileNames: String;
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


  Function TCmdLineParameters.getInterpretedSearchText: String;
  begin
    result := getSearchText;

    if getGlobalSearchFlag
       AND (result = '')
    then
      result := getFileNamesRaw;

    if not getGlobalSearchFlag
       AND (not getSearchFlag)
    then
    begin
      result := StrTrim(result);
      result := StrTrimChars(result, ['"']);
    end;

  end;


  Function TCmdLineParameters.setHelpManagerFlag(aNewValue : boolean) : boolean;
  begin
       helpManagerFlag := aNewValue;
       result := helpManagerFlag;
  end;


  Procedure TCmdLineParameters.parseCmdLine(aCmdLineString : String);
  var
    tmpState : (WHITESPACE, QUOTE, SWITCH, FILENAME, TEXT);
    tmpCurrentParsePosition : integer;
    tmpQuoted : boolean;
    tmpCurrentChar : char;
    tmpWhitespace : String;
    tmpQuote : String;
    tmpSwitch : String;
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
       tmpState := WHITESPACE;
       tmpWhitespace := '';
       tmpSwitch := '';
       tmpQuote := '';
       tmpQuoted := false;
       tmpCurrentParsePosition := 1;
       while tmpCurrentParsePosition <= length(aCmdLineString) do
       begin
         tmpCurrentChar := aCmdLineString[tmpCurrentParsePosition];

         Case tmpCurrentChar of
           ' ' :
           begin
             Case tmpState of

               WHITESPACE :
               begin
                 tmpWhitespace := tmpWhitespace + tmpCurrentChar;
               end;

               QUOTE :
               begin
                 tmpQuote := tmpQuote + tmpCurrentChar;
               end;

               SWITCH :
               begin
                 if tmpQuoted then
                 begin
                   tmpSwitch := tmpSwitch + tmpCurrentChar;
                 end
                 else
                 begin
                   parseSwitch(tmpSwitch);
                   tmpState := WHITESPACE;
                   tmpWhitespace := tmpCurrentChar;
                 end
               end;

               FILENAME :
               begin
                 if tmpQuoted then
                 begin
                   fileNames := fileNames + tmpCurrentChar;
                   fileNamesRaw := fileNamesRaw + tmpCurrentChar;
                 end
                 else
                 begin
                   tmpState := WHITESPACE;
                   tmpWhitespace := tmpCurrentChar;
                 end
               end;

               TEXT :
               begin
                 if tmpQuoted then
                 begin
                   searchText := searchText + tmpCurrentChar;
                 end
                 else
                 begin
                   tmpState := WHITESPACE;
                   tmpWhitespace := tmpCurrentChar;
                 end
               end;
             end;
           end;

           '/', '-' :
           begin
             Case tmpState of
               WHITESPACE :
               begin
                 tmpState := SWITCH;
                 tmpSwitch := '';
               end;

               QUOTE :
               begin
                 tmpState := SWITCH;
                 tmpSwitch := '';
               end;

               SWITCH :
               begin
                 parseSwitch(tmpSwitch);
                 tmpSwitch := '';
               end;

               FILENAME :
               begin
                 fileNames := fileNames + tmpCurrentChar;
                 fileNamesRaw := fileNamesRaw + tmpCurrentChar;
               end;

               TEXT :
               begin
                 searchText := searchText + tmpCurrentChar;
               end;
             end;
           end;

           '"' :
           begin
             if tmpQuoted then
             begin
               tmpQuoted := false;
               Case tmpState of
                 FILENAME :
                 begin
                   fileNamesRaw := fileNamesRaw + tmpCurrentChar;
                 end;
               end;
             end
             else
             begin
               Case tmpState of
                 WHITESPACE :
                 begin
                   tmpState := QUOTE;
                   tmpQuote := tmpCurrentChar;
                 end;
                 FILENAME :
                 begin
                   fileNamesRaw := fileNamesRaw + tmpCurrentChar;
                 end;
               end;
               tmpQuoted := true;
             end;
           end;

           // any other char
           else
           begin
             Case tmpState of

               WHITESPACE :
               begin
                 if length(fileNames) > 0 then
                 begin
                   tmpState := TEXT;
                   searchText := searchText + tmpWhitespace + tmpCurrentChar;
                 end
                 else
                 begin
                   tmpState := FILENAME;
                   fileNames := fileNames + tmpCurrentChar;
                   fileNamesRaw := fileNamesRaw + tmpCurrentChar;
                 end;
               end;

               QUOTE :
               begin
                 if length(fileNames) > 0 then
                 begin
                   tmpState := TEXT;
                   searchText := searchText + tmpWhitespace + tmpCurrentChar;
                 end
                 else
                 begin
                   tmpState := FILENAME;
                   fileNames := fileNames + tmpCurrentChar;
                   fileNamesRaw := fileNamesRaw + tmpQuote + tmpCurrentChar;
                 end;
               end;

               SWITCH :
               begin
                 tmpSwitch := tmpSwitch + tmpCurrentChar;
               end;

               FILENAME :
               begin
                 fileNames := fileNames + tmpCurrentChar;
                 fileNamesRaw := fileNamesRaw + tmpCurrentChar;
               end;

               TEXT :
               begin
                 searchText := searchText + tmpCurrentChar;
               end;
             end;
           end;
        end;
        inc(tmpCurrentParsePosition);
      end;

      // ok all chars are processed, but maybe we have something to do
      Case tmpState of
        SWITCH :
        begin
          parseSwitch(tmpSwitch);
        end;
     end;


     // TODO remove interpreted

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


{
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
}


  Function TCmdLineParameters.handleSwitchWithValue(const aSwitchString : String; const aSwitch : String; var aValue : String) : Boolean;
  var
    tmpText : String;
    tmpValueStartPos : integer;
    tmpSwitchLength : integer;
  begin
    tmpSwitchLength := Length(aSwitch);
    tmpText := copy(aSwitchString, 1, tmpSwitchLength);
    tmpText := lowercase(tmpText);

    if (lowercase(aSwitch) = tmpText) then
    begin
      tmpValueStartPos := tmpSwitchLength;
      inc(tmpValueStartPos);
      if aSwitchString[tmpValueStartPos] = ':' then
      begin
        inc(tmpValueStartPos);
      end;

      aValue := copy(aSwitchString, tmpValueStartPos, Length(aSwitchString) - tmpValueStartPos+ 1);
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


  Procedure TCmdLineParameters.parseSwitch(aSwitchString : String);
  var
    tmpCurrentChar : char;
    tmpText : String;
    tmpValue : String;
  begin
    // lang
    if handleSwitchWithValue(aSwitchString, 'lang', tmpValue) then
    begin
      language := tmpValue;
      exit;
    end;

    // title
    if handleSwitchWithValue(aSwitchString, 'title', tmpValue) then
    begin
      windowTitle := tmpValue;
      exit;
    end;

    // HM
    if handleSwitchWithValue(aSwitchString, 'hm', tmpValue) then
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
    if handleSwitchWithValue(aSwitchString, 'owner', tmpValue) then
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
    if handleSwitchWithValue(aSwitchString, 'pos', tmpValue) then
    begin
      windowPosition := ParseWindowPosition(tmpValue);
      windowPositionFlag := true;
      exit;
    end;

    // check the next char
    // TODO check for other invalid chars
    tmpCurrentChar := aSwitchString[1];
    Case tmpCurrentChar of
      'h', 'H', '?' :
        begin
          showUsageFlag := true;

          // check for 'help'
//          tmpText := copy(aCmdLineString, 2, 3);
//          tmpText := lowercase(tmpText);

//          if ('elp' = tmpText) then
//          begin
//          end;
        end;

      's', 'S' :
        begin
          searchFlag := true;
        end;

      'g', 'G' :
        begin
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
