Program Components;

Uses
  Forms,
  Graphics,
  ComponentsTestForm;
//  NonSizableDialogUnit;

{$r Components.scu}

Begin
  Application.Create;
  Application.CreateForm (TComponentsTestForm, ComponentsTestForm);
  Application.Run;
  Application.Destroy;
End.
