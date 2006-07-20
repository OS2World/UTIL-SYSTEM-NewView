Procedure IncreaseArraySize( Var pArray: pointer;
                             Var CurrentSize: longint;
                             ElementSize: longint );
var
  pNewArray: pointer;
  NewSize: longint;
begin
  NewSize := CurrentSize * 2;
  GetMem( pNewArray, NewSize * ElementSize );
  MemCopy( pArray, pNewArray, CurrentSize * ElementSize );
  FreeMem( pArray, CurrentSize * ElementSize );

  pArray := pNewArray;
  CurrentSize := NewSize;
end;
