Unit InformationFormUnit;

// NewView - a new OS/2 Help Viewer
// Copyright 2003 Aaron Lawrence (aaronl at consultant dot com)
// This software is released under the Gnu Public License - see readme.txt

Interface

Uses
  Classes, Forms, Graphics, StdCtrls, Buttons,
  ACLLanguageUnit;

Type
  TInformationForm = Class (TForm)
    InformationMemo: TMemo;
    OKButton: TButton;
    Procedure InformationFormOnSetupShow (Sender: TObject);
    Procedure InformationFormOnShow (Sender: TObject);
    Procedure InformationFormOnCreate (Sender: TObject);
  Private
    {Insert private declarations here}
  Public
    FText: pchar; // for messages with long lines
    Procedure OnLanguageEvent( Language: TLanguageFile;
                               const Apply: boolean );
  End;

Var
  InformationForm: TInformationForm;

Implementation

Uses
  ControlsUtility;

Procedure TInformationForm.InformationFormOnSetupShow (Sender: TObject);
Begin
  ScaleForm( self, 11, 16 );

  InformationMemo.XStretch := xsFrame;
  InformationMemo.YStretch := ysFrame;
End;

Procedure TInformationForm.InformationFormOnShow (Sender: TObject);
Begin
  if FText <> nil then
    InformationMemo.Lines.SetText( FText );
  FText := nil;
End;

Procedure TInformationForm.InformationFormOnCreate (Sender: TObject);
Begin
  RegisterForLanguages( OnLanguageEvent );
End;

Procedure TInformationForm.OnLanguageEvent( Language: TLanguageFile;
                                            const Apply: boolean );
begin
  Language.LoadComponentLanguage( self, Apply );
end;

Initialization
  RegisterClasses ([TInformationForm, TMemo, TButton]);
End.
