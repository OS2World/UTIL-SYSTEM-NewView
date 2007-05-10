unit ACLFindFunctions;
// Replacements/wrappers for file find functions
// The main problem is that I have had problems
// with the Delphi functions.
interface

uses
  SysUtils;

type
  TSearchData = SysUtils.TSearchRec;

function MyFindFirst( const Path: string;
                      var F: TSearchData ): integer;
function MyFindNext( var F: TSearchData ): Integer;
procedure MyFindClose(var F: TSearchData);

implementation

{$ifdef win32}
uses
  Windows;
  
procedure TranslateFindData( var F: TSearchData );
var
  LocalFileTime: TFileTime;
begin
  with F do
  begin
    FileTimeToLocalFileTime( FindData.ftLastWriteTime,
                             LocalFileTime );
    FileTimeToDosDateTime( LocalFileTime,
                           LongRec(Time).Hi,
                           LongRec(Time).Lo );
    Size := FindData.nFileSizeLow;
    Attr := FindData.dwFileAttributes;
    Name := FindData.cFileName;
  end;
end;

function MyFindFirst( const Path: string;
                      var F: TSearchData ): integer;
begin
  F.FindHandle := FindFirstFile( PChar( Path ), F.FindData );

  if F.FindHandle = INVALID_HANDLE_VALUE then
    Result:= ERROR_NO_MORE_FILES
  else
  begin
    TranslateFindData( F );
    Result:= 0;
  end;
end;

function MyFindNext( var F: TSearchData ): Integer;
begin
  if FindNextFile( F.FindHandle, F.FindData ) then
  begin
    Result:= 0;
    TranslateFindData( F );
  end
  else
    Result:= 1;
end;

procedure MyFindClose(var F: TSearchData);
begin
  Windows.FindClose( F.FindHandle );
end;
{$else}
// OS/2 versions: just pass thru to Sibyl versions.
function MyFindFirst( const Path: string;
                      var F: TSearchData ): integer;
begin
  Result:= SysUtils.FIndFirst( Path, faAnyFile, F );
end;

function MyFindNext( var F: TSearchData ): Integer;
begin
  Result:= SysUtils.FindNext( F );
end;

procedure MyFindClose(var F: TSearchData);
begin
  SysUtils.FindClose( F );
end;
{$endif}


end.
