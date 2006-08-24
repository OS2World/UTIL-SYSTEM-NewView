Unit CmdLineParameterUnit;

// NewView - a new OS/2 Help Viewer
// Copyright 2006 Ronald Brill (rbri at rbri dot de)
// This software is released under the Gnu Public License - see readme.txt

// Helper functions to address the command line parameters newview
// is started with

Interface

uses
  Os2Def, BseTib, BseDos,
  SysUtils,
  Classes,

  PMWIN,

  ACLStringUtility,
  ACLProfile,
  ACLFileUtility;

 CONST
     SUCCESS = 0;
     ERROR_UNMATCHED_QUOTE = -1;

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
       showUsageFlag : boolean;
       searchTextFlag : boolean;
       searchText : string;
       globalSearchTextFlag : boolean;
       globalSearchText : string;
       language : string;
       helpManagerFlag : boolean;
       helpManagerWindow : integer;
       windowPositionFlag: boolean;
       windowPosition: TWindowPosition;
       ownerWindow : integer;
       windowTitle : string;
       fileNames : string;
       topics : string;

     public
       FUNCTION getShowUsageFlag : boolean;
       FUNCTION getSearchTextFlag : boolean;
       FUNCTION getSearchText : string;
       FUNCTION getGlobalSearchTextFlag : boolean;
       FUNCTION getGlobalSearchText : string;
       FUNCTION getLanguage : string;
       FUNCTION getHelpManagerFlag : boolean;
       FUNCTION setHelpManagerFlag(aNewValue : boolean) : boolean;
       FUNCTION getHelpManagerWindow : integer;
       FUNCTION getWindowPositionFlag : boolean;
       FUNCTION getWindowPosition : TWindowPosition;
       FUNCTION getOwnerWindow : integer;
       FUNCTION getWindowTitle : string;
       FUNCTION getFileNames : string;
       FUNCTION getTopics : string;
       PROCEDURE parseCmdLine(aSplittedCmdLine : TStringList);
  end;

 // returns a string containing the whole
 // command line parametes
 FUNCTION nativeOS2GetCmdLineParameter : STRING;

 // returns a string containing the whole
 // command line parametes
 FUNCTION splitCmdLineParameter(aCmdLineString : String; var aResult : TStringList) : integer;

 // Return true if param matches the form
 // /Flag:value
 // dash (-) can be used instead of slash (/)
 // colon can be omitted
 FUNCTION MatchValueParam( const aParam: string; const aFlag: string; var aValue: string): boolean;

 // Return true if param matches the form
 // /Flag
 // dash (-) can be used instead of slash (/)
 FUNCTION MatchFlagParam( const aParam: string; const aFlag: string): boolean;

 // Extract a single element of a window position spec
 // - take a value from comma-separated list
 // - convert to numeric
 // - if the number ends with P then take as
 //   a percentage of given dimension
 FUNCTION ExtractPositionElement(Var aParamValue: string; aScreenDimension: longint) : longint;

 // Extract a specified window position:
 // X,Y,W,H
 FUNCTION ExtractPositionSpec(aParamValue: string; Var aPosition: TWindowPosition ): boolean;
Implementation

  FUNCTION TCmdLineParameters.getShowUsageFlag : boolean;
  begin
       result := showUsageFlag;
  end;


  FUNCTION TCmdLineParameters.getSearchTextFlag : boolean;
  begin
       result := searchTextFlag;
  end;


  FUNCTION TCmdLineParameters.getSearchText : string;
  begin
       result := searchText;
  end;


  FUNCTION TCmdLineParameters.getGlobalSearchTextFlag : boolean;
  begin
       result := globalSearchTextFlag;
  end;


  FUNCTION TCmdLineParameters.getGlobalSearchText : string;
  begin
       result := globalSearchText;
  end;


  FUNCTION TCmdLineParameters.getLanguage : string;
  begin
       result := language;
  end;


  FUNCTION TCmdLineParameters.getHelpManagerFlag : boolean;
  begin
       result := helpManagerFlag;
  end;


  FUNCTION TCmdLineParameters.setHelpManagerFlag(aNewValue : boolean) : boolean;
  begin
       helpManagerFlag := aNewValue;
       result := helpManagerFlag;
  end;


  FUNCTION TCmdLineParameters.getHelpManagerWindow : integer;
  begin
       result := helpManagerWindow;
  end;


  FUNCTION TCmdLineParameters.getWindowPositionFlag : boolean;
  begin
       result := windowPositionFlag;
  end;


  FUNCTION TCmdLineParameters.getWindowPosition : TWindowPosition;
  begin
       result := windowPosition;
  end;


  FUNCTION TCmdLineParameters.getOwnerWindow : integer;
  begin
       result := ownerWindow;
  end;


  FUNCTION TCmdLineParameters.getWindowTitle : string;
  begin
       result := windowTitle;
  end;


  FUNCTION TCmdLineParameters.getFileNames : string;
  begin
       result := fileNames;
  end;


  FUNCTION TCmdLineParameters.getTopics : string;
  begin
       result := topics;
  end;


  procedure TCmdLineParameters.parseCmdLine(aSplittedCmdLine : TStringList);
  var
      tmpParamIndex : integer;
      tmpParameter  : string;
      tmpParameterValue : string;
  begin
      ProfileEvent( 'ParseCommandLineParameters started' );

      // reset the whole object
      showUsageFlag := false;
      searchTextFlag := false;
      searchText := '';
      globalSearchTextFlag := false;
      globalSearchText := '';
      language := '';
      helpManagerFlag := false;
      helpManagerWindow := 0;
      windowPositionFlag := false;
      // windowPosition;
      ownerWindow := 0;
      windowTitle := '';

      filenames := '';
      topics := '';

      // start parsing
      for tmpParamIndex := 0 to aSplittedCmdLine.Count -1 do
      begin
          tmpParameter := aSplittedCmdLine[tmpParamIndex];

          if    MatchFlagParam(tmpParameter, '?')
                or MatchFlagParam(tmpParameter, 'H')
                or MatchFlagParam(tmpParameter, 'HELP') then
          begin
              showUsageFlag := true
          end
          else if MatchValueParam(tmpParameter, 'G', globalSearchText) then
          begin
              globalSearchTextFlag := true;
          end
          else if MatchValueParam(tmpParameter, 'S', searchText) then
          begin
              searchTextFlag := true;
          end
          else if MatchValueParam(tmpParameter, 'LANG', language) then
          begin
              // nothing to do
          end
          else if MatchValueParam(tmpParameter, 'HM', tmpParameterValue) then
          begin
              try
                  helpManagerWindow := StrToInt(tmpParameterValue);
                  helpManagerFlag := true;
              except
                  // ignore invalid window value
              end;
          end
          else if MatchValueParam(tmpParameter, 'OWNER', tmpParameterValue) then
          begin
              try
                  ownerWindow := StrToInt(tmpParameterValue);
              except
                  // ignore invalid owner value
              end;
          end
          else if MatchValueParam(tmpParameter, 'TITLE', windowTitle) then
          begin
              // nothing to do
          end
          else if MatchFlagParam(tmpParameter, 'PROFILE') then
          begin
              StartProfile(GetLogFilesDir + 'newview.prf' );
          end
          else if MatchValueParam(tmpParameter, 'POS', tmpParameterValue ) then
          begin
               // set window position/size
               if ExtractPositionSpec(tmpParameterValue, windowPosition) then
               begin
                   windowPositionFlag := true;
               end
               else
               begin
                   // invalid...
                   showUsageFlag := true;
               end;
          end
          else
          begin
               if length(filenames) = 0 then
               begin
                   // filename
                   fileNames := tmpParameter;
               end
               else
               begin
                   // search (topic) parameter... append all remaining thingies
                   if topics <> '' then
                   begin
                       topics := topics + ' ';
                   end;
                   topics := topics + tmpParameter;
               end;
          end;
      end;

      ProfileEvent('Parameters parsed');
      ProfileEvent('  Filename(s): ' + fileNames);
      ProfileEvent('  Topic(s): ' + topics);
      ProfileEvent( '...done' );
  end;


FUNCTION nativeOS2GetCmdLineParameter : STRING;
  VAR
     tmpPtib : PTIB;       /* thread information block */
     tmpPpib : PPIB;       /* process information block */
     tmpCmd  : PCHAR;
     tmpResult : PCHAR;

  BEGIN
     DosGetInfoBlocks(tmpPtib, tmpPpib);
     tmpCmd := tmpPpib^.pib_pchcmd;
     tmpResult := tmpCmd + StrLen(tmpCmd) + 1;
     nativeOS2GetCmdLineParameter := StrPas(tmpResult);
  END;


FUNCTION splitCmdLineParameter(aCmdLineString : String; var aResult : TStringList) : integer;
 CONST
     STATE_BEFORE = 0;
     STATE_INSIDE = 1;
     STATE_START_QUOTE = 2;
     STATE_INSIDE_QUOTED = 3;
     STATE_INSIDE_QUOTED_START_QUOTE = 4;
  VAR
     i : Integer;
     tmpCurrentChar : char;
     tmpState : INTEGER;
     tmpCurrentCommand : String;

  BEGIN
     result := SUCCESS;
     aResult.Clear;

     tmpState := STATE_BEFORE;
     tmpCurrentCommand := '';
     for i:=1 to length(aCmdLineString) do
     begin
          tmpCurrentChar := aCmdLineString[i];

          Case tmpCurrentChar of
          ' ' :
            begin
               Case tmpState of
               STATE_BEFORE : {do nothing};
               STATE_INSIDE :
                 begin
                    aResult.add(tmpCurrentCommand);
                    tmpCurrentCommand := '';
                    tmpState := STATE_BEFORE;
                 end;
               STATE_INSIDE_QUOTED_START_QUOTE :
                 begin
                    aResult.add(tmpCurrentCommand);
                    tmpCurrentCommand := '';
                    tmpState := STATE_BEFORE;
                 end;
               ELSE
                 begin
                    tmpCurrentCommand := tmpCurrentCommand + tmpCurrentChar;
                 end;
               end;
            end;

          '"' :
            begin
               Case tmpState of
               STATE_START_QUOTE :
                 begin
                    tmpState := STATE_INSIDE_QUOTED_START_QUOTE;
                 end;
               STATE_INSIDE_QUOTED :
                    tmpState := STATE_INSIDE_QUOTED_START_QUOTE;
               STATE_INSIDE_QUOTED_START_QUOTE :
                 begin
                    tmpState := STATE_INSIDE_QUOTED;
                    tmpCurrentCommand := tmpCurrentCommand + tmpCurrentChar;
                 end;
               ELSE
                    tmpState := STATE_START_QUOTE;
               end;
            end;
          ELSE
            begin
               Case tmpState of
               STATE_BEFORE :
                 begin
                    tmpState := STATE_INSIDE;
                    tmpCurrentCommand := tmpCurrentCommand + tmpCurrentChar;
                 end;
               STATE_INSIDE :
                 begin
                    tmpState := STATE_INSIDE;
                    tmpCurrentCommand := tmpCurrentCommand + tmpCurrentChar;
                 end;
               STATE_START_QUOTE :
                 begin
                    tmpState := STATE_INSIDE_QUOTED;
                    tmpCurrentCommand := tmpCurrentCommand + tmpCurrentChar;
                 end;
               STATE_INSIDE_QUOTED :
                 begin
                    tmpCurrentCommand := tmpCurrentCommand + tmpCurrentChar;
                 end;
               STATE_INSIDE_QUOTED_START_QUOTE :
                 begin
                    tmpState := STATE_INSIDE;
                    tmpCurrentCommand := tmpCurrentCommand + tmpCurrentChar;
                 end;
               end;
            end;
          end;
     end;

     Case tmpState of
     STATE_BEFORE : { nothing to do};
     STATE_INSIDE :
       begin
          aResult.add(tmpCurrentCommand);
       end;
     STATE_START_QUOTE :
       begin
          result := ERROR_UNMATCHED_QUOTE;
       end;
     STATE_INSIDE_QUOTED_START_QUOTE :
       begin
          if (0 < length(tmpCurrentCommand)) then
          begin
            aResult.add(tmpCurrentCommand);
          end;
       end;
     STATE_INSIDE_QUOTED :
       begin
          result := ERROR_UNMATCHED_QUOTE;
          if (0 < length(tmpCurrentCommand)) then
          begin
            aResult.add(tmpCurrentCommand);
          end;
       end;
     ELSE
       begin
          result := ERROR_UNMATCHED_QUOTE;
          if (0 < length(tmpCurrentCommand)) then
          begin
            aResult.add(tmpCurrentCommand);
          end;
       end;
     end;
  END;


FUNCTION MatchValueParam( const aParam: string; const aFlag: string; var aValue: string ): boolean;
begin
  Result := false;

  if aParam = '' then
    exit;

  if     ( aParam[ 1 ] <> '/' )
     and ( aParam[ 1 ] <> '-' ) then
    exit;

  if CompareText(copy(aParam, 2, length(aFlag)), aFlag) <> 0 then
    exit;

  Result := true;

  aValue := copy(aParam, 2 + length(aFlag), length(aParam));
  if aValue <> '' then
    if aValue[ 1 ] = ':' then
      delete(aValue, 1, 1 );
end;


FUNCTION MatchFlagParam(const aParam: string; const aFlag: string ): boolean;
begin
  Result := false;

  if aParam = '' then
    exit;

  if     (aParam[ 1 ] <> '/' )
     and (aParam[ 1 ] <> '-' ) then
    exit;
  Result := CompareText(copy(aParam, 2, length(aParam)-1), aFlag) = 0;
end;


FUNCTION ExtractPositionElement(Var aParamValue: string; aScreenDimension: longint ): longint;
var
  tmpElement: string;
begin
  tmpElement := ExtractNextValue(aParamValue, ',' );
  if tmpElement = '' then
    raise Exception.Create('Missing position element');
  if StrEnds('P', tmpElement) then
  begin
    Delete(tmpElement, length(tmpElement), 1);
    if tmpElement = '' then
      raise Exception.Create('Missing position element');
    Result := StrToInt(tmpElement);
    if Result < 0 then
      Result := 0;
    if Result > 100 then
      Result := 100;
    Result := Round(Result / 100 * aScreenDimension);
  end
  else
  begin
    Result := StrToInt(tmpElement);
  end;
end;

FUNCTION SystemMetrics(aSystemMetric : LONG) : LongInt;
Begin
  Result := WinQuerySysValue(HWND_DESKTOP, aSystemMetric);
end;

FUNCTION ExtractPositionSpec(aParamValue: string; Var aPosition: TWindowPosition ): boolean;
begin
  try
    aPosition.Left := ExtractPositionElement(aParamValue, SystemMetrics(SV_CXSCREEN));
    aPosition.Bottom := ExtractPositionElement(aParamValue, SystemMetrics(SV_CYSCREEN));
    aPosition.Width := ExtractPositionElement(aParamValue, SystemMetrics(SV_CXSCREEN));
    if aPosition.Width < 50 then
      aPosition.Width := 50;
    aPosition.Height := ExtractPositionElement(aParamValue, SystemMetrics(SV_CYSCREEN));
    if aPosition.Height < 50 then
      aPosition.Height := 50;
    Result := true;
  except
    Result := false;
  end;
end;


END.
