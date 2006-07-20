Unit ObjectListTemplate;

{#TemplateParameter ContainedClass}
{#TemplateParameter OutputClass}
{#TemplateParameter ContainedClassUnit}

Interface

uses
  ContainedClassUnit,
  Classes;

type
  // To use sort, you must pass in a compar method as below
  ObjectSortCompare = function ( Item1, Item2: ContainedClass ): Integer;

  OutputClass = class( TList )
  protected
    function Get( index: integer ): ContainedClass;
    procedure Put( index: integer; Item: ContainedClass );
  public
    function Add( item: ContainedClass ): integer;
    function First: ContainedClass;
    function IndexOf( item: ContainedClass ): integer;
    procedure Insert( Index: Integer; Item: ContainedClass );
    function Last: ContainedClass;
    function Remove( Item: ContainedClass ): Integer;
    procedure Sort( CompareFunction: ObjectSortCompare );
    property Items[ index: integer ]: ContainedClass read Get write Put; default;
    procedure DestroyContents;
    procedure Assign( OtherList: OutputClass );
    procedure AddList( OtherList: OutputClass );
  end;

Implementation

{ ContainedClassList }

function OutputClass.Add( item: ContainedClass ): integer;
begin
  result := inherited Add( item );
end;

function OutputClass.First: ContainedClass;
begin
  result := inherited First;
end;

function OutputClass.Get( index: integer ): ContainedClass;
begin
  result := inherited Get( Index );
end;

function OutputClass.IndexOf( item: ContainedClass ): integer;
begin
  result := inherited IndexOf( item );
end;

procedure OutputClass.Insert( Index: Integer; Item: ContainedClass );
begin
  inherited Insert( Index, Item );
end;

function OutputClass.Last: ContainedClass;
begin
  result := inherited Last;
end;

procedure OutputClass.Put( index: integer; Item: ContainedClass );
begin
  inherited Put( index, Item );
end;

function OutputClass.Remove( Item: ContainedClass ): Integer;
begin
  result := inherited Remove( Item );
end;

procedure QuickSort( SortList: PPointerList;
                     L, R: Integer;
                     CompareFunction: ObjectSortCompare );
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while CompareFunction( ContainedClass( SortList^[I] ),
                             ContainedClass( P ) ) < 0 do
        Inc(I);
      while CompareFunction( ContainedClass( SortList^[J] ),
                             ContainedClass( P ) ) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort( SortList, L, J, CompareFunction );
    L := I;
  until I >= R;
end;

procedure OutputClass.Sort( CompareFunction: ObjectSortCompare );
begin
  if ( List <> nil ) and ( Count > 0 ) then
    QuickSort( List, 0, Count - 1, CompareFunction );
end;

Procedure OutputClass.DestroyContents;
var
  Index: longint;
begin
  for Index := 0 to Count - 1 do
    ContainedClass( Items[ Index ] ).Destroy;
  Clear;
end;

procedure OutputClass.Assign( OtherList: OutputClass );
begin
  Clear;
  AddList( OtherList );
end;

procedure AddList( OtherList: OutputClass );
var
  Index: longint;
begin
  SetCapacity( Capacity + OtherList.Capacity );
  for Index := 0 to OtherList.Count - 1 do
    inherited Add( OtherList[ Index ] );
end;

Initialization
End.

