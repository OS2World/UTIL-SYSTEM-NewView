Unit IPFFileFormatUnit;

// NewView - a new OS/2 Help Viewer
// Copyright 2003 Aaron Lawrence (aaronl at consultant dot com)
// This software is released under the Gnu Public License - see readme.txt

Interface

// Definition of IPF file header and other structures

uses
  SysUtils;

type
  uint32 = longword;
  uint16 = word;
  uint8 = byte;
  pUInt16 = ^ uint16;
  pUInt32 = ^ uint32;
  pUInt8 = ^ uint8;

  PCharArray = array[ 0..0 ] of PCHar;
  UInt32Array = array[ 0..0 ] of UInt32;
  UInt16Array = array[ 0..0 ] of UInt16;
  UInt8Array = array[ 0..0 ] of UInt8;

  PCharArrayPointer = ^ PCharArray;
  UInt32ArrayPointer = ^ UInt32Array;
  UInt16ArrayPointer = ^ UInt16Array;
  UInt8ArrayPointer = ^ UInt8Array;

  TBooleanArray = array[ 0..0 ] of boolean;
  BooleanArrayPointer = ^TBooleanArray;

  EHelpFileException = class( Exception )
  end;

  EWindowsHelpFormatException = class( Exception )
  end;

var
  ErrorCorruptHelpFile: string;

const
  INF_HEADER_ID = $5348;

Type
  THelpFileHeader = record
    ID: uint16;           // ID magic word (5348h = "HS")
    unknown1: uint8;      // unknown purpose, could be third letter of ID
    flags: uint8;         // probably a flag word...
                          //  bit 0: set if INF style file
                          //  bit 4: set if HLP style file
                          // patching this byte allows reading HLP files
                          // using the VIEW command, while help files
                          // seem to work with INF settings here as well.
    hdrsize: uint16;      // total size of header
    unknown2: uint16;     // unknown purpose

    ntoc: uint16;         // number of entries in the tocarray
    tocstart: uint32;     // file offset of the start of the toc
    toclen: uint32;       // number of bytes in file occupied by the toc
    tocoffsetsstart: uint32;     // file offset of the start of array of toc offsets
    nres: uint16;         // number of panels with ressource numbers
    resstart: uint32;     // 32 bit file offset of ressource number table
    nname: uint16;        // number of panels with textual name
    namestart: uint32;    // 32 bit file offset to panel name table
    nindex: uint16;       // number of index entries
    indexstart: uint32;   // 32 bit file offset to index table
    indexlen: uint32;     // size of index table
    unknown3: array[ 0..9 ] of uint8; // unknown purpose
    searchstart: uint32;  // 31 bit file offset of full text search table
                          // Note: top bit indicates 32 bit search record!
    searchlen: uint32;    // size of full text search table
    nslots: uint16;       // number of "slots"
    slotsstart: uint32;   // file offset of the slots array
    dictlen: uint32;      // number of bytes occupied by the "dictionary"
    ndict: uint16;        // number of entries in the dictionary
    dictstart: uint32;    // file offset of the start of the dictionary
    imgstart: uint32;     // file offset of image data
    unknown4: uint8;      // unknown purpose
    nlsstart: uint32;     // 32 bit file offset of NLS table
    nlslen: uint32;       // size of NLS table
    extstart: uint32;     // 32 bit file offset of extended data block
    reserved: array[ 0..2 ] of uint32; // for future use. set to zero.
    title: array[ 0..47 ] of char;    // ASCII title of database
  end;
  TPHelpFileHeader = ^ THelpFileHeader;

  TExtendedHelpFileHeader = record
    NumFontEntry: uint16;             // FONT TABLE:   Number entries
    FontTableOffset: uint32;          // FONT TABLE:   Offset in file
    NumDataBase: uint16;              // DATA BASE:    Number of files
    DataBaseOffset: uint32;           // DATA BASE:    Offset in file
    DataBaseSize: uint32;             // DATA BASE:    Size in bytes
    EntryInGNameTable: uint16;        // GLOBAL NAMES: Number entries
    HelpPanelGNameTblOffset: uint32;  // GLOBAL NAMES: Offset in file
    StringsOffset: uint32;            // STRINGS : Offset in file
    StringsSize: uint16;              // STRINGS : Total bytes of all strings
    ChildPagesOffset: uint32;         // CHILDPAGES : Offset in file
    ChildPagesSize: uint32;           // CHILDPAGES : Total bytes of all strings
    NumGIndexEntry: uint32;           // Total number of Global Index items
    CtrlOffset: uint32;               // CTRL BUTTONS : offset in file
    CtrlSize: uint32;                 // CTRL BUTTONS : size in bytes
    Reserved: array[0..3] of uint32;  // For future use. Set to zero
  end;
  TPExtendedHelpFileHeader = ^ TExtendedHelpFileHeader;

Type
  TTOCEntryStart = record
    length: uint8; // length of the entry including this byte
    flags: uint8; // flag byte, description folows (MSB first)
             // bit1 haschildren;  // following nodes are a higher level
             // bit1 hidden;       // this entry doesn't appear in VIEW.EXE's
                                   // presentation of the toc
             // bit1 extended;     // extended entry format
             // bit1 stuff;        // ??
             // int4 level;        // nesting level
    numSlots: uint8; // number of "slots" occupied by the text for
                                // this toc entry
  end;
  pTTOCEntryStart = ^TTOCEntryStart;

  TExtendedTOCEntry = record
    w1: uint8;
    w2: uint8;
  end;
  pExtendedTOCEntry = ^TExtendedTOCEntry;

  TTOCEntryOffsetArray =  array[ 0..0 ] of uint32;
  pTTOCEntryOffsetArray = ^ TTOCEntryOffsetArray;

Const
  TOCEntryExtended = 32;
  TOCEntryHidden = 64;
  TOCEntryHasChildren = 128;

type
  THelpXYPair = record
    Flags: uint8;
    X: uint16;
    Y: uint16;
  end;
  pHelpXYPair = ^ THelpXYPair;

  TSlotHeader = record
    stuff: uint8; // always 0??
    localdictpos: uint32; // file offset of the local dictionary
    nlocaldict: uint8; // number of entries in the local dict
    ntext: uint16; // number of bytes in the text
  end;
  pSlotHeader = ^TSlotHeader;

  THelpFontSpec = record
    FaceName: array[ 0..32 ] of char;
    Height: uint16;
    Width: uint16;
    Codepage: uint16;
  end;
  pTHelpFontSpec = ^ THelpFontSpec;

// List of IPF escape codes. 

const
  // Basic byte codes
  IPF_END_PARA = $fa;
  IPF_CENTER = $fb;
  IPF_INVERT_SPACING = $fc;
  IPF_LINEBREAK = $fd;
  IPF_SPACE = $fe;
  IPF_ESC = $ff; // followed by one of the ecXXX codes below

  // FF XX
  ecSetLeftMargin = $02;
  ecHighlight1 = $04; // hp1,2,3,5,6,7
  ecLinkStart = $05;
  ecFootnoteLinkStart = $07;
  ecLinkEnd = $08;
  ecStartCharGraphics = $0b;
  ecEndCharGraphics = $0c;
  ecHighlight2 = $0d; // hp4,8,9
  ecImage = $0e;
  ecLinkedImage = $0f;
  ecProgramLink = $10;
  ecSetLeftMarginNewLine = $11;
  ecSetLeftMarginFit = $12;
  ecForegroundColor = $13;
  ecBackgroundColor = $14;
  ecFontChange = $19;
  ecStartLines = $1a;
  ecEndLines = $1b;
  ecSetLeftMarginHere = $1c;
  ecStartLinkByResourceID = $1d;
  ecExternalLink = $1f;

  // Subescape codes of
  HPART_DEFINE = 0;
  HPART_PT_HDREF = 1;
  HPART_PT_FNREF = 2;
  HPART_PT_SPREF = 3;
  HPART_HDREF = 4;
  HPART_FNREF = 5;
  HPART_SPREF = 6;
  HPART_LAUNCH = 7;
  HPART_PT_LAUNCH = 8;
  HPART_INFORM = 9;
  HPART_PT_INFORM = 10;
  // ?? 11 ??
  HPART_EXTERN_PT_HDREF = 12;
  HPART_EXTERN_PT_SPREF = 13;
  HPART_EXTERN_HDREF = 14;
  HPART_EXTERN_SPREF = 15;
  HPART_GLOBAL_HDREF = 16;
  HPART_GLOBAL_PT_HDREF = 17;

// -----------------------------------------------------------
// Operations on Int32 arrays, used for searching
// These could be optimised heavily if needed.
procedure AllocUInt32Array( Var pArray: UInt32ArrayPointer;
                            Size: longint );
procedure FreeUInt32Array( Var pArray: UInt32ArrayPointer;
                           Size: longint );

procedure FillUInt32Array( pArray: UInt32ArrayPointer;
                           Size: longint;
                           Value: UInt32 );

procedure AddUInt32Array( pSource: UInt32ArrayPointer;
                          pDest: UInt32ArrayPointer;
                          Size: longint );

// Dest = Dest + source * Multiplier
procedure AddMultConstUInt32Array( pSource: UInt32ArrayPointer;
                                   Multiplier: longint;
                                   pDest: UInt32ArrayPointer;
                                   Size: longint );

procedure AndUInt32Array( pSource: UInt32ArrayPointer;
                          pDest: UInt32ArrayPointer;
                          Size: longint );

// If both source and dest > 0 then
//   add source to dest
procedure AndAddUInt32Array( pSource: UInt32ArrayPointer;
                             pDest: UInt32ArrayPointer;
                             Size: longint );

// if Source > 0 then dest is set to 0
procedure AndNotUInt32Array( pSource: UInt32ArrayPointer;
                             pDest: UInt32ArrayPointer;
                             Size: longint );

// dest = dest or source;
// if source > 0  then set dest to  > 0
procedure OrUInt32Array( pSource: UInt32ArrayPointer;
                         pDest: UInt32ArrayPointer;
                         Size: longint );

// if source = 0 then dest set to >0
procedure NotOrUInt32Array( pSource: UInt32ArrayPointer;
                            pDest: UInt32ArrayPointer;
                            Size: longint );

procedure CopyUInt32Array( pSource: UInt32ArrayPointer;
                           pDest: UInt32ArrayPointer;
                           Size: longint );

procedure ClearUInt32Array( pArray: UInt32ArrayPointer;
                            Size: longint );
procedure SetUInt32Array( pArray: UInt32ArrayPointer;
                          Size: longint );

// returns the result of ORing every array element.
// Can be useful for debugging e.g. seeing at a glance
// if any element is non-zero
function OrAllUInt32Array( pArray: UInt32ArrayPointer;
                           Size: longint ): longint;


Implementation

uses
  ACLUtility;

// Operations on int32 arrays
// -----------------------------------------------------------

procedure AllocUInt32Array( Var pArray: UInt32ArrayPointer;
                            Size: longint );
begin
  GetMem( pArray,
          Size
          * sizeof( UInt32 ) );
end;

procedure FreeUInt32Array( Var pArray: UInt32ArrayPointer;
                           Size: longint );
begin
  FreeMem( pArray,
           Size
           * sizeof( UInt32 ) );
end;

// This is a nice fast implementation of filling an
// array of dwords (Int32/longword)
procedure FillUInt32Array( pArray: UInt32ArrayPointer;
                           Size: longint;
                           Value: UInt32 );
begin
  assert( Size >= 0 );
  Asm
    Mov EAX, Value
    Mov EDI, pArray
    Mov ECX, Size
    CLD        // direction = up
    REP STOSD   // store double word, until ECX = 0
  End;
end;

procedure ClearUInt32Array( pArray: UInt32ArrayPointer;
                            Size: longint );
begin
  FillUInt32Array( pArray, Size, 0 );
end;

procedure SetUInt32Array( pArray: UInt32ArrayPointer;
                          Size: longint );
begin
  FillUInt32Array( pArray, Size, $ffffffff );
end;

procedure AddUInt32Array( pSource: UInt32ArrayPointer;
                          pDest: UInt32ArrayPointer;
                          Size: longint );
var
  i: longint;
begin
  for i := 0 to Size - 1 do
    inc( pDest^[ i ], pSource^[ i ] );
end;

procedure AddMultConstUInt32Array( pSource: UInt32ArrayPointer;
                                   Multiplier: longint;
                                   pDest: UInt32ArrayPointer;
                                   Size: longint );
var
  i: longint;
begin
  for i := 0 to Size - 1 do
    inc( pDest^[ i ], pSource^[ i ] * Multiplier );
end;

procedure OrUInt32Array( pSource: UInt32ArrayPointer;
                         pDest: UInt32ArrayPointer;
                         Size: longint );
var
  i: longint;
begin
  for i := 0 to Size - 1 do
    pDest^[ i ] := pDest^[ i ] or pSource^[ i ];
end;

procedure CopyUInt32Array( pSource: UInt32ArrayPointer;
                           pDest: UInt32ArrayPointer;
                           Size: longint );
begin
  MemCopy( pSource, pDest, Size * sizeof( uint32 ) );
end;

procedure NotOrUInt32Array( pSource: UInt32ArrayPointer;
                            pDest: UInt32ArrayPointer;
                            Size: longint );
var
  i: longint;
begin
  for i := 0 to Size - 1 do
    if pSource^[ i ] = 0 then
      pDest^[ i ] := 1;
end;

procedure AndUInt32Array( pSource: UInt32ArrayPointer;
                          pDest: UInt32ArrayPointer;
                          Size: longint );
var
  i: longint;
begin
  for i := 0 to Size - 1 do
    pDest^[ i ] := pDest^[ i ] and pSource^[ i ];
end;

procedure AndAddUInt32Array( pSource: UInt32ArrayPointer;
                             pDest: UInt32ArrayPointer;
                             Size: longint );
var
  i: longint;
begin
  for i := 0 to Size - 1 do
    if     ( pSource^[ i ] > 0 )
       and ( pDest^[ i ] > 0 ) then
      inc( pDest^[ i ], pSource^[ i ] )
    else
      pDest^[ i ] := 0;
end;

procedure AndNotUInt32Array( pSource: UInt32ArrayPointer;
                             pDest: UInt32ArrayPointer;
                             Size: longint );
var
  i: longint;
begin
  for i := 0 to Size - 1 do
    if pSource^[ i ] > 0 then
      pDest^[ i ] := 0;
end;

function OrAllUInt32Array( pArray: UInt32ArrayPointer;
                           Size: longint ): longint;
var
  i: longint;
begin
  Result := 0;
  for i := 0 to Size - 1 do
    Result := Result or pArray^[ i ];
end;

Initialization
End.
