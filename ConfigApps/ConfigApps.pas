Program ConfigApps;

Uses
  Forms, Graphics, MainFormUnit;

{$r ConfigApps.scu}

Begin
  Application.Create;
  Application.CreateForm (TMainForm, MainForm);
  Application.Run;
  Application.Destroy;
End.
