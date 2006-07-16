Unit CustomDirOutline;

// This is a small enhancement of the sample TDirectoryOutline
// Changes are:
// 1) Leaf/open/close bitmaps are used as inherited from TOutline
//    instead of being specially loaded. THerefore they can be changed
// 2) Fix to Click method to make root directory selectable
// 3) Added Reload method
// 4) Does not change or use current directory
// 5) Has ChangeToParent, AtRoot, and ChangeToRoot methods
Interface

Uses
  SysUtils, Classes, Graphics, StdCtrls, Forms, Dialogs,
  CustomOutline, Outline;

type
  TCustomDirOutline = Class( TCustomOutline )
     Protected
        FDirectory: String;
        FDrive: Char;
        FOnChange: TNotifyEvent;
        FLookAhead: boolean;
        Procedure SetDrive( NewDrive: Char );
        Procedure SetDirectory( Const NewDir: String );
        Procedure FillLevel( Node: TOutlineNode );
        Procedure CheckForSomeDirs( Node: TOutlineNode );
        Procedure BuildTree; Virtual;
        Procedure WalkTree( Const Dir: String );
        Procedure SetupShow; Override;
        Procedure BuildOneLevel( ParentLevel: Longint ); Virtual;
        Procedure Change; Virtual;
     Public
        Procedure Expand( Index: Longint ); Override;
        Procedure SetupComponent; Override;
        Destructor Destroy; Override;
        Procedure ItemFocus( Index: longint ); Override;
        Procedure Reload;
     Public
        Property Drive: Char read FDrive write SetDrive;
        // Note unlike original TDirOutline, setting this property
        // does *not* allow relative paths.
        Property Directory: String read FDirectory write SetDirectory;

        // Returns true if already at a root dir
        Function AtRoot: boolean;
        // Returns true if could be done
        Function ChangeToParent: boolean;
        Function Parent: string;
        Procedure ChangeToRoot;

        Property Lines;
     published
        // If this property is false, all dirs will have a + symbol
        // until they are expanded
        // If true, the control will look into each dir and see if there
        // are any subdirs to correct show or hide the +
        property LookAhead: boolean read FLookAhead write FLookAhead;
        Property OnChange:TNotifyEvent read FOnChange write FOnChange;
  End;

Exports TCustomDirOutline, 'User', 'CustomDirOutline.bmp';

Implementation

// Returns true if already at a root dir
Function TCustomDirOutline.AtRoot: boolean;
Var
  TestString: string;
Begin
  TestString:= Directory;
  System.Delete( TestString, 1, 2 ); // remove x: off the start

  Result:= ( TestString='' ) or ( TestString='\' );
End;

Function TCustomDirOutline.Parent: string;
Var
  i: longint;
Begin
  Result:= '';
  if AtRoot then
    exit;
  Result:= Directory;
  if Result[ length( Result ) ]='\' then
    System.Delete( Result, length( Result ), 1 );
  for i:= length( Result ) downto 2 do
  begin
    if Result[ i ]='\' then
    begin
      Result:= copy( Result, 1, i );
      exit;
    end;
  end;
End;

// Returns true if could be done
Function TCustomDirOutline.ChangeToParent: boolean;
Begin
  Result:= false;
  if AtRoot then
    exit;
  Directory:= Parent;
  Result:= true;
End;

Procedure TCustomDirOutline.ChangeToRoot;
Begin
  Directory:= copy( Directory, 1, 3 );
End;

Procedure TCustomDirOutline.Change;
Begin
   If FOnChange <> Nil Then
     FOnChange( Self );
End;

// Looks at the path for the given node and adds one directory
// if there is one.
Procedure TCustomDirOutline.CheckForSomeDirs(Node:TOutlineNode);
Var
  Root:ShortString;
  SearchRec: TSearchRec;
  Status:Integer;
Begin
   Node.Clear;
   Root:=Node.FullPath;
   If Root[Length(Root)] In ['\','/'] Then dec(Root[0]);

   Status:=FindFirst(Root+'\*.*',faDirectory,SearchRec);
   While Status=0 Do
   Begin
     If SearchRec.Attr And faDirectory = faDirectory Then
     Begin
       If ((SearchRec.Name<>'.')And(SearchRec.Name<>'..')) Then //no .. and .
       Begin
         // Found a directory
         // All we care about is adding one node if needed
         AddChild(Node.Index,SearchRec.Name);
         FindClose( SearchRec );
         exit;
       End;
     End;
     Status:=FindNext(SearchRec);
   End;

end;

Procedure TCustomDirOutline.FillLevel(Node:TOutlineNode);
Var
  TempIndex:Longint;
  Root:ShortString;
  SearchRec: TSearchRec;
  Status:Integer;
  s,s1:String;
Begin
  // We always start from scratch. So it's up to date.
   Node.Clear;
   Root:=Node.FullPath;
   If Root[Length(Root)] In ['\','/'] Then dec(Root[0]);

   Status:=FindFirst(Root+'\*.*',faDirectory,SearchRec);
   While Status=0 Do
   Begin
     If SearchRec.Attr And faDirectory = faDirectory Then
     Begin
       If ((SearchRec.Name<>'.')And(SearchRec.Name<>'..')) Then //no .. and .
       Begin
          If Node.HasItems Then //must sort
          Begin
            TempIndex:=Node.GetFirstChild;
            s:=SearchRec.Name;
            UpcaseStr(s);
            If TempIndex<>-1 Then
            Begin
                 s1:=Items[TempIndex].Text;
                 UpcaseStr(s1);
            End;
            While (TempIndex<>-1)And(s1<s) Do
            Begin
                TempIndex:=Node.GetNextChild(TempIndex);
                If TempIndex<>-1 Then
                Begin
                   s1:=Items[TempIndex].Text;
                   UpcaseStr(s1);
                End;
            End;
            If TempIndex<>-1 Then Insert(TempIndex, SearchRec.Name)
            Else Add(Node.GetLastChild, SearchRec.Name);
          End
          Else AddChild(Node.Index,SearchRec.Name);
        End;
     End;
     Status:=FindNext(SearchRec);
   End;

end;


Procedure TCustomDirOutline.BuildOneLevel(ParentLevel:Longint);
Var Index:LongInt;
    RootNode:TOutlineNode;
    FList:TList;
    t:longint;
Begin
   FillLevel(Items[ParentLevel]);

   RootNode := Items[ParentLevel];
   FList:= TList.Create;
   Index:= RootNode.GetFirstChild;
   While Index<>-1 Do
   Begin
        FList.Add(Items[Index]);
        Index:=RootNode.GetNextChild(Index);
   End;

   // Depending on look ahead, either look for any directories at the
   // next level to correctly set the +, or
   // go and put dummy entries so the + will always show up
   For t:=0 To FList.Count-1 Do
     if FLookAhead then
       CheckForSomeDirs(TOutlineNode(FList[t]))
     else
       AddChild( TOutlineNode( FList[t] ).Index, 'dummy');

   FList.Destroy;
End;

Procedure TCustomDirOutline.SetupComponent;
Begin
  Inherited SetupComponent;
  BorderStyle:= bsNone;
  PlusMinusSize.CX:= 14;
  PlusMinusSize.CY:= 14;
  ShowPlusMinus:= False;
  FLookAhead:= false;
  Name:='DirectoryOutline';
End;

Destructor TCustomDirOutline.Destroy;
Begin
   Inherited Destroy;
End;

Procedure TCustomDirOutline.ItemFocus( Index: longint );
Begin
  inherited Click;
  Try
    If SelectedItem=-1 Then
      Beep(1200,400);
    if SelectedItem=1 then
      // Selecting root dir... FullPath will not be quite enough...
      Directory:=FDrive+':\'
    else
      Directory :=Items[SelectedItem].FullPath;
  Except
  End;
End;

Procedure TCustomDirOutline.SetDrive(NewDrive:Char);
Begin
   FDrive:=Upcase(NewDrive);
   FDirectory:=FDrive+':\';
   If Not (csLoading In ComponentState) Then
     BuildTree;
End;

Procedure TCustomDirOutline.SetDirectory(Const NewDir:String);
Var
  TempPath: ShortString;
  Node:TOutlineNode;

  Function FindNode(Node:TOutlineNode):TOutlineNode;
  Var s:String;
      t:LongInt;
      Node1:TOutlineNode;
  Begin
      s:=Node.FullPath;
      UpcaseStr(s);
      If s=TempPath Then
      Begin
          result:=Node;
          exit;
      End;

      For t:=0 To Node.ItemCount-1 Do
      Begin
          Node1:=Node.Items[t];
          Node1:=FindNode(Node1);
          If Node1<>Nil Then
          Begin
               Result:=Node1;
               exit;
          End;
      End;
      Result:=Nil;
  End;

Begin
  If NewDir = '' Then
    exit;

  If NewDir[ Length( NewDir ) ] In ['\','/'] Then
    Dec( NewDir[ 0 ] ) ;

  TempPath := NewDir;

  FDirectory:= TempPath;
  If FDirectory[ 1 ] <> Drive Then
    Drive:= FDirectory[ 1 ]
  Else
  Begin
    WalkTree( TempPath );
    Change;
  End;

  TempPath:= FDirectory;
  UpcaseStr( TempPath );
  Node:= FindNode( Items[ 1 ] ); // start at drive
  If Node <> Nil Then
    If SelectedNode <> Node Then
      SetAndShowSelectedItem( Node.Index );

// WARNING! Because when an item is expanded, all it's subnodes will be
// deleted and recreated (re-reading the directory) it is very important
// that node of the parent nodes not be expanded while setting the selected node

// This code works because WalkTree has already expanded all the nodes
// (except the one we actually want to select, and expanding that one
// does not matter). And I changed TCustomOutline.SetAndShowSelectedItem
// to not expand if already expanded.

End;

Procedure TCustomDirOutline.SetupShow;
Var CurDir:String;
Begin
  Inherited SetupShow;

  If FDrive=#0 Then  //test if unassigned
  Begin
    {$I-}
    GetDir(0, CurDir);
    {$I+}
    If IoResult<>0 Then exit;
    FDrive := Upcase(CurDir[1]);
    FDirectory := CurDir;
  End;

  BuildTree;
End;

Procedure TCustomDirOutline.BuildTree;
Var
  RootIndex: Longint;
Begin
  Clear;
  If FDrive=#0 Then
    exit;
  RootIndex:= Add( 0, Drive+':' );
  WalkTree( FDirectory );
  Change;
End;

Procedure TCustomDirOutline.WalkTree(Const Dir:String);
Var
  b:LongInt;
  CurPath,NextDir,s:ShortString;
  TempItem,TempIndex: Longint;
begin
  TempItem := 1; { start at root }

  CurPath := Dir;
  b:=Pos(':',CurPath);
  If b>0 then
    CurPath:=Copy(CurPath,b+1,255);
  If CurPath<>'' Then
    If CurPath[1]='\' Then
      System.Delete(CurPath,1,1);

  NextDir := CurPath;
  Repeat
    b:=Pos('\',CurPath);
    If b=0 Then
      b:=Pos('/',CurPath);
    If b > 0 then
    Begin
      NextDir:=Copy(CurPath,1,b-1);
      CurPath:=Copy(CurPath,b+1,255);
    End
    Else
    Begin
      NextDir:=CurPath;
      CurPath:='';
    End;

    // Expands this dir, forcing it's subdirs to be read
    Items[TempItem].Expanded:=True;

    TempIndex:=Items[TempItem].GetFirstChild;
    UpcaseStr(NextDir);
    If CurPath='' Then
      TempIndex:=-1
    Else While TempIndex<>-1 Do
    Begin
      s:=Items[TempIndex].Text;
      UpcaseStr(s);
      If s=NextDir Then Break;
      TempIndex:=Items[TempItem].GetNextChild(TempIndex);
    End;
    If TempIndex<>-1 Then
      TempItem:=TempIndex
    Else
      CurPath:=''; //break
  Until CurPath='';
End;

Procedure TCustomDirOutline.Expand(Index:Longint);
Begin
  BuildOneLevel(Index);
  Inherited Expand(Index);
End;

Procedure TCustomDirOutline.Reload;
Var
  OldDir: string;
Begin
  OldDir:= Directory;
  BuildTree;
  Directory:= OldDir;
End;

initialization
  RegisterClasses( [ TCustomDirOutline ] );

end.
