Unit RichTextLayoutUnit;

// Dynamically created layout class.
// Represents a laid out rich text document

Interface

Uses
  Os2Def,
  Classes, Forms, Messages, Graphics,
  CanvasFontManager,
  RichTextDocumentUnit, RichTextStyleUnit;

Type
  TLayoutLine = record
    Text: PChar;
    Length: longint;

    Height: longint;
    Width: longint;

    MaxDescender: longint;
    MaxTextHeight: longint; // maximum height of text, doesn't include images

    LinkIndex: longint; // link index at start of line, if any

    Style: TTextDrawStyle;

    Wrapped: boolean;
  end;

  TLinesArray = array[ 0..0 ] of TLayoutLine;

Type
  TTextPosition =
  (
    tpAboveTextArea,
    tpAboveText,
    tpWithinText,
    tpBelowText,
    tpBelowTextArea
  );


  TRichTextLayout = class;

  TLinkEvent = procedure( Sender: TRichTextLayout; Link: string ) of object;

  TRichTextLayout = class
  Protected
    FFontManager: TCanvasFontManager;

    // Text
    FText: PChar;
    FImages: TImageList;

    FAllocatedNumLines: Longint;

    FLayoutWidth: longint; // The target width for the layout. Used for centreing/right align
    FWidth: longint;       // The actual width of the text. May be wider due to unaligned
                           // parts or bitmaps or width so small individual characters don't fit.
    FHeight: longint;

    FLinks: TStringList;

    FHorizontalImageScale: double;
    FVerticalImageScale: double;

  public
    // Internal layout data
    FLines: ^TLinesArray;
    FNumLines: longint;

    FRichTextSettings: TRichTextSettings;

    // Drawing functions

    Procedure PerformStyleTag( Const Tag: TTag;
                               Var Style: TTextDrawStyle;
                               const X: longint );

    function GetElementWidth( Element: TTextElement ): longint;

    // Queries

    Function GetStartX( Style: TTextDrawStyle;
                        Line: TLayoutLine ): longint;

    Procedure GetXFromOffset( const Offset: longint;
                              const LineIndex: longint;
                              Var X: longint );
    Procedure GetOffsetFromX( const XToFind: longint;
                              const LineIndex: longint;
                              Var Offset: longint;
                              Var Link: string );
    function FindPoint( XToFind, YToFind: longint;
                        Var LineIndex: longint;
                        Var Offset: longint;
                        Var Link: string ): TTextPosition;
    function GetLineFromCharIndex( Index: longint ): longint;
    function GetOffsetFromCharIndex( Index: longint;
                                     Line: longint ): longint;
    function GetLinePosition( Line: longint ): longint;
    function GetLineFromPosition( YToFind: longint;
                                  Var LineIndex: longint;
                                  Var Remainder: longint ): TTextPosition;

    // Layout functions

    Procedure AddLineStart( Const Line: TLayoutLine );
    Procedure CheckFontHeights( Var Line: TLayoutLine );
    Procedure Layout;

    function IsValidBitmapIndex( Index: longint ): boolean;

    // property handlers

    Function GetCharIndex( P: PChar ): longint;
    Function GetTextEnd: longint;

  Public
    constructor Create( Text: PChar;
                        Images: TImageList;
                        RichTextSettings: TRichTextSettings;
                        FontManager: TCanvasFontManager;
                        Width: longint );

    Destructor Destroy; Override;

    property TextEnd: longint read GetTextEnd;

    function LinkFromIndex( const CharIndexToFind: longint): string;

    property Images: TImageList read FImages;

    property Width: longint read FWidth;
    property Height: longint read FHeight;

    property HorizontalImageScale: double read FHorizontalImageScale;
    property VerticalImageScale: double read FVerticalImageScale;

  End;

Implementation

Uses
  SysUtils, PMWin, BseDos, Dos, ClipBrd, Printers,
  ACLUtility, ACLStringUtility, ACLString,
  ControlScrolling;

Function TRichTextLayout.GetTextEnd: longint;
begin
  Result := StrLen( FText );
end;

// Create a layout of the specified rich text.
constructor TRichTextLayout.Create( Text: PChar;
                                    Images: TImageList;
                                    RichTextSettings: TRichTextSettings;
                                    FontManager: TCanvasFontManager;
                                    Width: longint );
var
  DefaultFontSpec: TFontSpec;
Begin
  Inherited Create;

  FRichTextSettings := RichTextSettings;

  FImages := Images;

  FText := Text;

  FAllocatedNumLines := 10;
  GetMem( FLines, FAllocatedNumLines * sizeof( TLayoutLine ) );
  FNumLines := 0;

  FLinks := TStringList.Create;
  FLinks.Duplicates := dupIgnore;

  FFontManager := FontManager;

  FLayoutWidth := Width
                  * FontWidthPrecisionFactor;

  FHorizontalImageScale :=   FFontManager.Canvas.HorizontalResolution
                           / Screen.Canvas.HorizontalResolution;
  FVerticalImageScale   :=   FFontManager.Canvas.VerticalResolution
                           / Screen.Canvas.VerticalResolution;

  // use normal font for default font when specified fonts can't be found
  SibylFontToFontSpec( RichTextSettings.NormalFont,
                       DefaultFontSpec );
  FFontManager.DefaultFontSpec := DefaultFontSpec;

  Layout;
End;

Destructor TRichTextLayout.Destroy;
Begin
  FreeMem( Flines, FAllocatedNumLines * sizeof( TLayoutLine ) );
  FLinks.Destroy;

  Inherited Destroy;
End;

Procedure TRichTextLayout.AddLineStart( Const Line: TLayoutLine );
var
  NewAllocation: longint;
begin
  if FNumLines >= FAllocatedNumLines then
  begin
    // reallocate the array twice the size
    NewAllocation := FAllocatedNumLines * 2;
    FLines := ReAllocMem( FLines,
                          FAllocatedNumLines * sizeof( TLayoutLine ),
                          NewAllocation * sizeof( TLayoutLine ) );
    FAllocatedNumLines := NewAllocation;
  end;
  FLines^[ FNumLines ] := Line;
  inc( FNumLines );
end;

Procedure TRichTextLayout.PerformStyleTag( Const Tag: TTag;
                                           Var Style: TTextDrawStyle;
                                           const X: longint );
begin
  ApplyStyleTag( Tag,
                 Style,
                 FFontManager,
                 FRichTextSettings,
                 X );
end;

// Check the current font specifications and see if the
// give line needs updating for max height/descender
Procedure TRichTextLayout.CheckFontHeights( Var Line: TLayoutLine );
var
  FontHeight: longint;
  Descender: longint;
begin
  FontHeight := FFontManager.CharHeight;
  Descender := FFontManager.CharDescender;

  if FontHeight > Line.Height then
    Line.Height := FontHeight;

  if FontHeight > Line.MaxTextHeight then
    Line.MaxTextHeight := FontHeight;

  if Descender > Line.MaxDescender then
    Line.MaxDescender := Descender;
end;

function TRichTextLayout.IsValidBitmapIndex( Index: longint ): boolean;
begin
  if FImages = nil then
    Result := false
  else if FImages.Count = 0 then
    Result := false
  else
    Result := Between( Index, 0, FImages.Count - 1 );
end;

// Main procedure: reads through the whole text currently stored
// and breaks up into lines - each represented as a TLayoutLine in
// the array FLines[ 0.. FNumLines ]
Procedure TRichTextLayout.Layout;
Var
  CurrentLine: TLayoutLine;

  CurrentLinkIndex: longint;

  WrapX: longint; // X to wrap at

  WordX: longint; // width of word so far
  P: PChar;
  NextP: PChar;
  NextP2: PChar;

  WordStart: PChar;
  WordStarted: boolean; // if false, just skipping spaces..
  WordStartX: longint; // X position of word start

  LineWordsCompleted: longint; // how many words draw so far this line

  CurrentElement: TTextElement;
  NextElement: TTextElement;

  CurrentCharWidth: longint;

  Style: TTextDrawStyle;

  DisplayedCharsSinceFontChange: boolean;

  BitmapIndex: longint;
  Bitmap: TBitmap;
  BitmapHeight: longint;

  OnBreak: boolean;

  DoWrap: boolean;

  // Nested procedure

  Procedure DoLine( EndPoint: PChar;
                    NextLine: PChar;
                    EndX: longint );
  begin
    // check if the max font
    // height needs updating for the last string of the line
    CheckFontHeights( CurrentLine );

    inc( FHeight, CurrentLine.Height );

    CurrentLine.Length := PCharDiff( EndPoint, CurrentLine.Text );

    CurrentLine.Width := EndX;

    if CurrentLine.Width > FWidth then
      FWidth := CurrentLine.Width;

    assert( CurrentLine.Height > 0 ); // we must have set the line height!

    AddLineStart( CurrentLine );
    CurrentLine.Text := NextLine;
    CurrentLine.Style := Style;
    CurrentLine.Height := 0;
    CurrentLine.MaxDescender := 0;
    CurrentLine.MaxTextHeight := 0;
    CurrentLine.Width := 0;
    CurrentLine.LinkIndex := CurrentLinkIndex;
    CurrentLine.Wrapped := false;

    assert( CurrentLinkIndex >= -1 );
    assert( CurrentLinkIndex < FLinks.Count );

    WordStartX := Style.LeftMargin * FontWidthPrecisionFactor;
    // next line

    // reset words completed count
    LineWordsCompleted := 0;

    WordStarted := false;
  end;

begin
  FNumLines := 0;
  FWidth := 0;
  FHeight := FRichTextSettings.Margins.Top;

  Style := GetDefaultStyle( FRichTextSettings );

  ApplyStyle( Style, FFontManager );

  CurrentLinkIndex := -1;

  P := FText; // P is the current search position
  CurrentLine.Text := P;
  CurrentLine.Style := Style;
  CurrentLine.Height := 0;
  CurrentLine.MaxDescender := 0;
  CurrentLine.MaxTextHeight := 0;
  CurrentLine.Width := 0;
  CurrentLine.LinkIndex := -1;
  CurrentLine.Wrapped := false;

  WordStartX := Style.LeftMargin * FontWidthPrecisionFactor;
  WordX := 0;

  WrapX := FLayoutWidth
           - FRichTextSettings.Margins.Right
             * FontWidthPrecisionFactor;

  LineWordsCompleted := 0;

  WordStarted := false;
  DisplayedCharsSinceFontChange := false;

  repeat
    CurrentElement := ExtractNextTextElement( P, NextP );
    assert( NextP > P );

    OnBreak := false;

    case CurrentElement.ElementType of
      teWordBreak:
      begin
        CurrentCharWidth := FFontManager.CharWidth( ' ' );
        OnBreak := true;
      end;

      teLineBreak:
      begin
        DoLine( P, NextP, WordStartX + WordX );

        // remember start of line
        WordStart := NextP;
        WordX := 0;

        P := NextP;

        continue;
      end;

      teTextEnd:
      begin
        DoLine( P, NextP, WordStartX + WordX );

        // end of text, done
        break;
      end;

      teImage:
      begin
        BitmapHeight := 0;
        try
          BitmapIndex := StrToInt( CurrentElement.Tag.Arguments );
        except
          BitmapIndex := -1;
        end;
        Bitmap := nil;
        if IsValidBitmapIndex( BitmapIndex ) then
        begin
          Bitmap := FImages.GetBitmapReference( BitmapIndex );

          CurrentCharWidth := Bitmap.Width
                              * FontWidthPrecisionFactor
                              * FHorizontalImageScale;

          WordStarted := true;
          BitmapHeight := Bitmap.Height * FVerticalImageScale;
        end;

      end;

      teText:
      begin
        // Normal (non-leading-space) character
        CurrentCharWidth := FFontManager.CharWidth( CurrentElement.Character );
        WordStarted := true;
      end;

      teStyle:
      begin
        case CurrentElement.Tag.TagType of
          ttBeginLink:
          begin
            CurrentLinkIndex := FLinks.Add( CurrentElement.Tag.Arguments );
            P := NextP;
            continue;
          end;

          ttEndLink:
          begin
            CurrentLinkIndex := -1;
            P := NextP;
            continue;
          end;

          ttSetLeftMargin: // SPECIAL CASE... could affect display immediately
          begin
            PerformStyleTag( CurrentElement.Tag,
                             Style,
                             WordstartX + WordX );
            if Style.LeftMargin * FontWidthPrecisionFactor < WordStartX then
            begin
              // we're already past the margin being set

              if pos( 'breakifpast', CurrentElement.Tag.Arguments ) > 0 then
              begin
                // this argument means, do a line break
                // if the margin is already past
                // Seems unusual for most purposes, but needed for IPF rendering.
                DoLine( P, NextP, WordStartX + WordX );

                // remember start of line
                WordStart := NextP;
                WordX := 0;

                P := NextP;

                continue;
              end;

              // so ignore it for now.
              P := NextP;
              continue;
            end;

            // skip across to the new margin
            CurrentCharWidth := Style.LeftMargin * FontWidthPrecisionFactor
                                - WordStartX
                                - WordX;
            // BUT! Don't treat it as a space, because you would not
            // expect wrapping to take place in a margin change...
            // at least not for IPF (NewView) :)

          end;

          else
          begin
            // before processing the tag see if font height needs updating
            if DisplayedCharsSinceFontChange then
              CheckFontHeights( CurrentLine );

            if     ( CurrentElement.Tag.TagType = ttItalicOff )
               and ( faItalic in Style.Font.Attributes ) then
              if not FFontManager.IsFixed then
                // end of italic; add a space
                inc( WordX, FFontManager.CharWidth( ' ' ) );

            PerformStyleTag( CurrentElement.Tag,
                             Style,
                             WordX );

            DisplayedCharsSinceFontChange := false;
            P := NextP;
            continue; // continue loop
          end;
        end;

      end

    end;

    if OnBreak then
    begin
      // we just processed a space
      if WordStarted then
      begin
        DisplayedCharsSinceFontChange := true;
        // remember that we have now completed a word on this line
        inc( LineWordsCompleted );
        WordStarted := false;

        // Add the word width, and the space width,
        // to get the start of the next word
        inc( WordStartX, WordX + CurrentCharWidth );
        WordX := 0;

        // remember the start of the next word
        WordStart := NextP;

        P := NextP;

        continue;
      end;
      // else - starting spaces - fall through like normal char
    end;

    // if we're still going here we have a normal char
    // (or leading spaces)

    if not Style.Wrap then
    begin
      // No alignment
      // We don't care about how wide it gets
      inc( WordX, CurrentCharWidth );
      DisplayedCharsSinceFontChange := true;

      if CurrentElement.ElementType = teImage then
        if Bitmap <> nil then
          if BitmapHeight > CurrentLine.Height then
            CurrentLine.Height := BitmapHeight;

      P := NextP;
      continue;
    end;

    DoWrap := false;

    // Calculate position of end of character
    // see if char would exceed width
    if   WordStartX
       + WordX
       + CurrentCharWidth
       >= WrapX then
    begin
      // reached right hand side before finding end of word
      if LineWordsCompleted > 0 then
        // always wrap after at least one word displayed
        DoWrap := true
      else if not FRichTextSettings.AtLeastOneWordBeforeWrap then
        // only wrap during the first word, if the "at least 1 word" flag is not set.
        DoWrap := true;

    end;

    if DoWrap then
    begin
      if LineWordsCompleted = 0 then
      begin
        // the first word did not fit on the line. so draw
        // as much as will fit
        if WordX = 0 then
        begin
          // even the first char doesn't fit,
          // but draw it anyway (otherwise, infinite loop)

          NextElement := ExtractNextTextElement( NextP, NextP2 );
          if NextElement.ElementType <> teLineBreak then
            // there is still more on the line...
            CurrentLine.Wrapped := true
          else
            // the line ends after this one char or image, we can skip the line end
            NextP := NextP2;

          if CurrentElement.ElementType = teImage then
          begin
            // the only thing on the line is the image. so check height
            if Bitmap <> nil then
              if BitmapHeight > CurrentLine.Height then
                CurrentLine.Height := BitmapHeight;
          end;

          DoLine( NextP,
                  NextP,
                  WordStartX + WordX + CurrentCharWidth );
          WordStart := NextP;

          WordX := 0;
        end
        else
        begin
          CurrentLine.Wrapped := true;
          // at least 1 char fits
          // so draw up to, but not including this char
          DoLine( P,
                  P,
                  WordStartX + WordX );
          WordStart := P;
          WordX := CurrentCharWidth;
        end;
      end
      else
      begin
        // Normal wrap; at least one word fitted on the line
        CurrentLine.Wrapped := true;

        // take the width of the last space of the
        // previous word off the line width
        DoLine( WordStart, // current line ends at start of this word
                WordStart, // next line starts at start of this word
                WordStartX - FFontManager.CharWidth( ' ' ) );
        if CurrentElement.ElementType = teImage then
          if Bitmap <> nil then
            if BitmapHeight > CurrentLine.Height then
              CurrentLine.Height := BitmapHeight;

        // do NOT reset WordX to zero; as we are continuing
        // from partway thru the word on the next line.
        inc( WordX, CurrentCharWidth );
      end;
      WordStarted := true; // by definition, for wrapping
    end
    else
    begin
      // Character fits.
      inc( WordX, CurrentCharWidth );

      DisplayedCharsSinceFontChange := true;

      if CurrentElement.ElementType = teImage then
        if Bitmap <> nil then
          if BitmapHeight > CurrentLine.Height then
            CurrentLine.Height := BitmapHeight;
    end;

    P := NextP;

  until false; // loop is exited by finding end of text

  inc( FHeight, FRichTextSettings.Margins.Bottom );
End;

Function TRichTextLayout.GetStartX( Style: TTextDrawStyle;
                                    Line: TLayoutLine ): longint;
var
  SpaceOnLine: longint;
begin
  case Style.Alignment of
    taLeft:
      Result := Style.LeftMargin * FontWidthPrecisionFactor;

    taRight:
      Result :=   Style.LeftMargin * FontWidthPrecisionFactor
                + FLayoutWidth
                - Style.RightMargin * FontWidthPrecisionFactor
                - Line.Width;

    taCenter:
      begin
        // |<------layout width------------------>|
        // |                                      |
        // |<-lm->[aaaaaaaaaaaaaaa]<-space-><-rm->|
        // |<-----line width------>               |
        // space = layoutw-rm-linew
        SpaceOnLine :=   FLayoutWidth
                       - Style.RightMargin * FontWidthPrecisionFactor
                       - Line.Width; // Note: line width includes left margin
        Result :=   Style.LeftMargin * FontWidthPrecisionFactor
                  + SpaceOnLine div 2;
      end;
  end;
end;

Procedure TRichTextLayout.GetOffsetFromX( const XToFind: longint;
                                          const LineIndex: longint;
                                          Var Offset: longint;
                                          Var Link: string );
Var
  X: longint;
  P: PChar;
  NextP: PChar;
  EndP: PChar;
  Element: TTextElement;
  CurrentLink: string;
  Line: TLayoutLine;
  Style: TTextDrawStyle;
  NewMarginX: longint;
  StartedDrawing: boolean;
begin
  Line := FLines[ LineIndex ];
  P := Line.Text;
  EndP := Line.Text + Line.Length;

  Style := Line.Style;
  FFontManager.SetFont( Style.Font );

  StartedDrawing := false;

  Link := '';
  if Line.LinkIndex <> -1 then
    CurrentLink := FLinks[ Line.LinkIndex ]
  else
    CurrentLink := '';

  while P < EndP do
  begin
    Element := ExtractNextTextElement( P, NextP );

    case Element.ElementType of
      teWordBreak,
      teText,
      teImage:
      begin
        if not StartedDrawing then
        begin
          // we haven't yet started drawing:
          // so work out alignment
          X := GetStartX( Style, Line );

          if X div FontWidthPrecisionFactor
             > XToFind then
          begin
            // found before the start of the line
            // don't set link
            Offset := 0;
            exit;
          end;

          StartedDrawing := true;

        end;

        // Now find out how wide the thing is
        inc( X, GetElementWidth( Element ) );

        if X div FontWidthPrecisionFactor
           > XToFind then
        begin
          // found
          Offset := PCharDiff( P, Line.Text );
          Link := CurrentLink;
          exit;
        end;

      end;

      teStyle:
        case Element.Tag.TagType of
          ttBeginLink:
            CurrentLink := Element.Tag.Arguments;
          ttEndLink:
            CurrentLink := '';
          else
          begin
            if     ( Element.Tag.TagType = ttItalicOff )
               and ( faItalic in Style.Font.Attributes )
               and ( not FFontManager.IsFixed ) then
              // end of italic; add a space
              inc( X, FFontManager.CharWidth( ' ' )  );

            PerformStyleTag( Element.Tag,
                             Style,
                             X );
            NewMarginX := Style.LeftMargin * FontWidthPrecisionFactor;
            if NewMarginX > X then
            begin
              //skip across...
              X := NewMarginX;
            end;
          end;
        end;
    end;

    P := NextP;
  end;
  Offset := Line.Length;
end;

Procedure TRichTextLayout.GetXFromOffset( const Offset: longint;
                                          const LineIndex: longint;
                                          Var X: longint );
Var
  P: PChar;
  NextP: PChar;
  EndP: PChar;
  Element: TTextElement;
  StartedDrawing: boolean;
  Line: TLayoutLine;
  Style: TTextDrawStyle;
  NewMarginX: longint;
begin
  Line := FLines[ LineIndex ];
  P := Line.Text;
  EndP := Line.Text + Line.Length;

  Style := Line.Style;
  FFontManager.SetFont( Style.Font );

  StartedDrawing := false;

  while P < EndP do
  begin
    Element := ExtractNextTextElement( P, NextP );

    case Element.ElementType of
      teWordBreak,
      teText,
      teImage:
      begin
        if not StartedDrawing then
        begin
          // we haven't yet started drawing:
          // so work out alignment
          X := GetStartX( Style, Line );
          StartedDrawing := true;
        end;

        if GetCharIndex( P ) - GetCharIndex( Line.Text ) >= Offset then
        begin
          X := X div FontWidthPrecisionFactor;
          // found
          exit;
        end;

        // Now find out how wide the thing is
        inc( X, GetElementWidth( Element ) );

      end;

      teStyle:
      begin
        if     ( Element.Tag.TagType = ttItalicOff )
           and ( faItalic in Style.Font.Attributes )
           and ( not FFontManager.IsFixed ) then
          // end of italic; add a space
          inc( X, FFontManager.CharWidth( ' ' )  );

        PerformStyleTag( Element.Tag,
                         Style,
                         X );

        NewMarginX := Style.LeftMargin * FontWidthPrecisionFactor;
        if NewMarginX > X then
        begin
          //skip across...
          X := NewMarginX;
        end;
      end;
    end;

    P := NextP;
  end;
  // went thru the whole line without finding the point,
  if not StartedDrawing then
    X := GetStartX( Style, Line );

  X := X div FontWidthPrecisionFactor;
end;

function TRichTextLayout.GetLineFromPosition( YToFind: longint;
                                              Var LineIndex: longint;
                                              Var Remainder: longint ): TTextPosition;
var
  Y: longint;
  LineHeight: longint;
begin
  LineIndex := 0;
  Remainder := 0;

  Y := FRichTextSettings.Margins.Top;

  if YToFind < Y then
  begin
    Result := tpAboveText;
    exit;
  end;

  while LineIndex < FNumLines do
  begin
    LineHeight := FLines[ LineIndex ].Height;
    if     ( YToFind >= Y )
       and ( YToFind < Y + LineHeight ) then
      begin
        // YToFind is within the line
        Result := tpWithinText;
        Remainder := YToFind - Y;
        exit;
      end;

    inc( Y, FLines[ LineIndex ].Height );
    inc( LineIndex );
  end;

  LineIndex := FNumLines - 1;
  Remainder := FLines[ LineIndex ].Height;

  Result := tpBelowText;
end;

function TRichTextLayout.FindPoint( XToFind, YToFind: longint;
                                    Var LineIndex: longint;
                                    Var Offset: longint;
                                    Var Link: string ): TTextPosition;
var
  Remainder: longint;
begin
  Link := '';
  Result := GetLineFromPosition( YToFind,
                                 LineIndex,
                                 Remainder );
  case Result of
    tpAboveText:
    begin
      Offset := 0;
      exit;
    end;

    tpBelowText:
    begin
      Offset := FLines[ LineIndex ].Length;
      exit;
    end;
  end;

  // found the line
  GetOffsetFromX( XToFind,
                  LineIndex,
                  Offset,
                  Link );
end;

function TRichTextLayout.GetLineFromCharIndex( Index: longint ): longint;
var
  LineCharIndex: longint;
  LineLength: longint;
begin
  Result := 0;
  if Index <= 0 then
    exit;

  while Result < FNumLines do
  begin
    LineCharIndex := GetCharIndex( FLines[ Result ].Text );
    LineLength := FLines[ Result ].Length;
    if LineCharIndex + LineLength
       > Index then
    begin
      // found
      exit;
    end;
    inc( Result );
  end;
  Result := FNumLines - 1;
end;

function TRichTextLayout.GetOffsetFromCharIndex( Index: longint;
                                                 Line: longint ): longint;
begin
  Result := Index - GetCharIndex( TLayoutLine( FLines[ Line ] ).Text );
end;

function TRichTextLayout.GetElementWidth( Element: TTextElement ): longint;
var
  Bitmap: TBItmap;
  BitmapIndex: longint;
begin
  // Now find out how wide the thing is
  case Element.ElementType of
    teImage:
    begin
      try
        BitmapIndex := StrToInt( Element.Tag.Arguments );
      except
        BitmapIndex := -1;
      end;
      if IsValidBitmapIndex( BitmapIndex ) then
      begin
        Bitmap := FImages.GetBitmapReference( BitmapIndex );
        Result := Bitmap.Width
                  * FontWidthPrecisionFactor
                  * FHorizontalImageScale;
      end;
    end;

    teText, teWordBreak:
      Result := FFontManager.CharWidth( Element.Character );

    else
      Assert( False ); // should never be trying to find the width of a style, etc

  end;
end;

Function TRichTextLayout.GetCharIndex( P: PChar ): longint;
begin
  Result := PCharDiff( P, FText );
end;

function TRichTextLayout.GetLinePosition( Line: longint ): longint;
begin
  Result := FRichTextSettings.Margins.Top;
  dec( line );
  while line >= 0 do
  begin
    inc( Result,
         Flines[ Line ].Height );
    dec( line );
  end;
end;

function TRichTextLayout.LinkFromIndex( const CharIndexToFind: longint): string;
Var
  P: PChar;
  NextP: PChar;
  EndP: PChar;
  Element: TTextElement;
  LineIndex: longint;
  Line: TLayoutLine;
begin
  if FNumLines = 0 then
  begin
    Result := '';
    exit;
  end;

  LineIndex := GetLineFromCharIndex( CharIndexToFind );

  Line := FLines[ LineIndex ];
  P := Line.Text;
  EndP := Line.Text + Line.Length;

  if Line.LinkIndex <> -1 then
    Result := FLinks[ Line.LinkIndex ]
  else
    Result := '';

  while P < EndP do
  begin
    if GetCharIndex( P ) >= CharIndexToFind then
      exit;

    Element := ExtractNextTextElement( P, NextP );

    case Element.ElementType of
      teStyle:
        case Element.Tag.TagType of
          ttBeginLink:
            Result := Element.Tag.Arguments;
          ttEndLink:
            Result := '';
        end;
    end;

    P := NextP;
  end;
end;

Initialization
End.

