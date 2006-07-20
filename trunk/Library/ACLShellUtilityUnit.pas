unit ACLShellUtilityUnit;

interface

// Win32 only

uses
  Forms;

// Retrieves the "My Documents" folder
function GetPersonalFolder: string;

// Encapsulates Windows shell folder browser.
// Unlike the Delphi SelectDirectory function, this
// takes an owner form so that you cannot select the
// owner form!
function BrowseForFolder( const Owner: TForm;
                          const Caption: string;
                          const Root: WideString;
                          out Directory: string): Boolean;

// Launch explorer for the specified directory
procedure Explore( Directory: string;
                   TreeView: boolean = true );

implementation

uses
  ShlObj, ActiveX, Windows,
  Dialogs,
  ACLFileUtility, ACLUtility;

function GetPersonalFolder: string;
var
  pIDList: PItemIDList;
  Path: array[ 0..MAX_PATH ] of char;
begin
  SHGetSpecialFolderLocation( HWND_DESKTOP,
                              CSIDL_PERSONAL,
                              pIDList );
  SHGetPathFromIDList( pIDList, path );
  Result := path;
end;

function BrowseForFolder( const Owner: TForm;
                          const Caption: string;
                          const Root: WideString;
                          out Directory: string): Boolean;
var
  BrowseInfo: TBrowseInfo;
  Buffer: PChar;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Eaten, Flags: LongWord;
begin
  Result := False;
  Directory := '';
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      SHGetDesktopFolder(IDesktopFolder);
      IDesktopFolder.ParseDisplayName(Application.Handle, nil,
        POleStr(Root), Eaten, RootItemIDList, Flags);
      with BrowseInfo do
      begin
        hwndOwner := Owner.Handle;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PChar(Caption);
        ulFlags := BIF_RETURNONLYFSDIRS;
      end;
      ItemIDList := ShBrowseForFolder(BrowseInfo);
      Result :=  ItemIDList <> nil;
      if Result then
      begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Directory := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

procedure Explore( Directory: string;
                   TreeView: boolean );
var
  Parameters: string;
begin
  if TreeView then
    Parameters := '/e,' + Directory
  else
    Parameters := Directory;

  if not RunProgram( 'Explorer',
                     Parameters ) then
  begin
    ShowMessage( 'Could not run Windows Explorer: '
                 + GetLastAPIErrorString );
  end;
end;

end.
