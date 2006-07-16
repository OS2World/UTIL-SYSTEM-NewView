Unit NonSizableDialogUnit;

Interface

Uses
  Classes, Forms, Graphics, Buttons;

Type
  TNonSizableDialog = Class (TForm)
    OKButton: TBitBtn;
    CancelButton: TBitBtn;
  Private
    {Insert private declarations here}
  Public
    {Insert public declarations here}
  End;

Var
  NonSizableDialog: TNonSizableDialog;

Implementation

Initialization
  RegisterClasses ([TNonSizableDialog, TBitBtn]);
End.
