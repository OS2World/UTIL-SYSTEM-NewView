Unit CustomBitmapButton;
// Custom bitrmap button is now an alias of TBitBtn, 
// for backwards compatibility; all the fixes in it are now
// in TBitBtn in my fixed SPCC.

Interface

Uses
  Classes, Buttons;

Type
  TCustomBitmapButton = class( TBitBtn )
  End;

Exports
  TCustomBitmapButton,'User','CustomBitmapButton.bmp';

Implementation


Initialization
  {Register classes}
  RegisterClasses([TCustomBitmapButton]);
End.

