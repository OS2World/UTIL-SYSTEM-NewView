Unit PrintDialogUnit;

// NewView - a new OS/2 Help Viewer
// Copyright 2003 Aaron Lawrence (aaronl at consultant dot com)
// This software is released under the Gnu Public License - see readme.txt

Interface

Uses
  Classes,
  Forms,
  Buttons,
  ExtCtrls,
  StdCtrls,
  ACLLanguageUnit;

Type
  TPrintSelection =
  (
    ptCurrentTopic,
    ptVisibleTopics,
    ptAllTopics
  );

  TNewViewPrintDialog = Class (TForm)
    PrinterComboBox: TComboBox;
    PrinterLabel: TLabel;
    WhatToPrintRadioGroup: TRadioGroup;
    OKButton: TButton;
    SetupPrinterButton: TButton;
    CancelButton: TButton;
    Procedure OKButtonOnClick (Sender: TObject);
    Procedure NewViewPrintDialogOnSetupShow (Sender: TObject);
    Procedure NewViewPrintDialogOnCreate (Sender: TObject);
    Procedure PrinterComboBoxOnItemSelect (Sender: TObject; Index: LongInt);
    Procedure SetupPrinterButtonOnClick (Sender: TObject);
    Procedure PrintDialogOnShow (Sender: TObject);
  Public
  Protected
    Procedure OnLanguageEvent( Language: TLanguageFile;
                               const Apply: boolean );

    SetupPrinterErrorTitle: string;
    SetupPrinterError: string;
  End;

Var
  NewViewPrintDialog: TNewViewPrintDialog;

Implementation

uses
  SysUtils,
  Printers,
  ACLDialogs,
  ControlsUtility;

Procedure TNewViewPrintDialog.OKButtonOnClick (Sender: TObject);
Begin

End;

Procedure TNewViewPrintDialog.NewViewPrintDialogOnSetupShow (Sender: TObject);
Begin
  ScaleForm( self, 11, 16 );
End;

Procedure TNewViewPrintDialog.NewViewPrintDialogOnCreate (Sender: TObject);
Begin
  RegisterForLanguages( OnLanguageEvent );
End;

Procedure TNewViewPrintDialog.OnLanguageEvent( Language: TLanguageFile;
                                               const Apply: boolean );
begin
  Language.LoadComponentLanguage( self, Apply );

  Language.LL( Apply, SetupPrinterErrorTitle, 'SetupPrinterErrorTitle', 'Setup Printer' );
  Language.LL( Apply, SetupPrinterError, 'SetupPrinterError', 'Error displaying printer options: ' );
end;

Procedure TNewViewPrintDialog.PrinterComboBoxOnItemSelect (Sender: TObject;
  Index: LongInt);
Begin
  Printer.PrinterIndex := PrinterComboBox.ItemIndex;
End;

Procedure TNewViewPrintDialog.SetupPrinterButtonOnClick (Sender: TObject);
Begin
  try
    Printer.OptionsDlg;
  except
    on E: Exception do
    begin
      DoErrorDlg( SetupPrinterErrorTitle,
                  SetupPrinterError + E.Message );
    end;
  end;
End;

Procedure TNewViewPrintDialog.PrintDialogOnShow (Sender: TObject);
Begin
  PrinterComboBox.Items.Assign( Printer.Printers );
  PrinterComboBox.ItemIndex := Printer.PrinterIndex;
  OKButton.Default := true;
End;

Initialization
  RegisterClasses ([TNewViewPrintDialog, TComboBox, TLabel, TButton, TRadioGroup]);
End.
