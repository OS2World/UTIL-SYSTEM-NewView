Program NewView;

// NewView - a new OS/2 Help Viewer
// Copyright 2003 Aaron Lawrence (aaronl at consultant dot com)
// This software is released under the Gnu Public License - see readme.txt

// Main program
// Becomes NewView.exe

Uses
  Forms,
  MainForm;

{$r NewView.scu}

Begin
  Application.Create;
  Application.CreateForm (TMainForm, MainForm);
  Application.Run;
  Application.Destroy;
End.

