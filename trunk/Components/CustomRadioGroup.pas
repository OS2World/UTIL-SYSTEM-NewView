Unit CustomRadioGroup;

// TCustomRadioGroup is not really based on TCustomRadioGroup it replaces it
// the only change is a tiny bug fix to make setting itemindex work without
// the control being shown (as when it is on an initially concealed notebook page)
Interface

Uses
  Classes, Forms, StdCtrls;

{Declare new class}
Type

  TCustomRadioGroup=Class(TGroupBox)
    Private
      FItems:TStrings;
      FRadios:TList;
      FItemIndex:LongInt;
      FColumns:LongInt;
      FOnClick:TNotifyEvent;
      Procedure SetItemIndex(Value:LongInt);
      Procedure SetColumns(Value:LongInt);
      Procedure SetItems(Value:TStrings);
      Procedure SetRadioCount(Value:LongInt);
      Function GetItemsEnabled(Index:LongInt):Boolean;
      Procedure SetItemsEnabled(Index:LongInt;Value:Boolean);
      Procedure UpdateRadios;
      Procedure ArrangeRadios;
      Procedure EvItemsChange(Sender:TObject);
      Procedure EvRadioClick(Sender:TObject);
    Protected
      Procedure SetupComponent;Override;
      Procedure SetupShow;Override;
      Procedure Resize;Override;
      Procedure FontChange;Override;
      Procedure Click;Virtual;
      Procedure ItemIndexChange;Virtual;
    Public
      Destructor Destroy;Override;
      Procedure ReadSCUResource(Const ResName:TResourceName;Var Data;DataLen:LongInt);Override;
      Function WriteSCUResource(Stream:TResourceStream):Boolean;Override;
      Property ItemsEnabled[Index:LongInt]:Boolean Read GetItemsEnabled Write SetItemsEnabled;
      Property XAlign;
      Property XStretch;
      Property YAlign;
      Property YStretch;
    Published
      Property Align;
      Property Caption;
      Property Color;
      Property Columns:LongInt Read FColumns Write SetColumns;
      Property DragCursor;
      Property DragMode;
      Property Enabled;
      Property Font;
      Property ItemIndex:LongInt Read FItemIndex Write SetItemIndex;
      Property Items:TStrings Read FItems Write SetItems;
      Property ParentColor;
      Property ParentPenColor;
      Property ParentFont;
      Property ParentShowHint;
      Property PenColor;
      Property ShowHint;
      Property TabOrder;
      Property TabStop;
      Property Visible;
      Property ZOrder;

      Property OnCanDrag;
      Property OnClick:TNotifyEvent Read FOnClick Write FOnClick;
      Property OnDragDrop;
      Property OnDragOver;
      Property OnEndDrag;
      Property OnEnter;
      Property OnExit;
      Property OnFontChange;
      Property OnSetupShow;
      Property OnStartDrag;
    End;

{Define components to export}
{You may define a page of the component palette and a component bitmap file}
Exports
  TCustomRadioGroup,'User','CustomRadioGroup.bmp';

Implementation

Uses
  Buttons, SysUtils;

Procedure TCustomRadioGroup.SetupComponent;
Begin
     Inherited SetupComponent;

     Name := 'RadioGroup';
     Caption := Name;

     FRadios:= TList.Create;
     FItems := TStringList.Create;
     TStringList(FItems).OnChange := EvItemsChange;
     FItemIndex := -1;
     FColumns := 1;
End;


Procedure TCustomRadioGroup.SetupShow;
Begin
     Inherited SetupShow;

     UpdateRadios;
     SetItemIndex(FItemIndex);
     ArrangeRadios;
End;


Destructor TCustomRadioGroup.Destroy;
Begin
     SetRadioCount(0);
     FRadios.Destroy;
     FRadios := Nil;
     TStringList(FItems).OnChange := Nil;
     FItems.Destroy;
     FItems := Nil;

     Inherited Destroy;
End;


Procedure TCustomRadioGroup.Resize;
Begin
     Inherited Resize;

     ArrangeRadios;
End;


Procedure TCustomRadioGroup.FontChange;
Var  I:LongInt;
     Radio:TRadioButton;
Begin
     Inherited FontChange;

     For I := 0 To FRadios.Count-1 Do
     Begin
          Radio := TRadioButton(FRadios.Items[I]);
          Radio.Font := Font;
     End;
     ArrangeRadios;
End;


Procedure TCustomRadioGroup.Click;
Begin
     If FOnClick <> Nil Then FOnClick(Self);
End;


Procedure TCustomRadioGroup.SetItemIndex(Value:LongInt);
Begin
     If ComponentState * [csReading] <> [] Then
     Begin
          FItemIndex := Value;
          Exit;
     End;

     // Bug fix over SPCC: create radios so we can set the item index
     // Thanks Martin Vieregg <vrb@compuserve.com>
     SetRadioCount(FItems.Count);

     If Value < 0 Then Value := -1;
     If Value >= FRadios.Count Then Value := FRadios.Count-1;

     {deselect old because New Value can be < 0}
     If (FItemIndex >= 0) And (FItemIndex < FRadios.Count)
     Then TRadioButton(FRadios[FItemIndex]).Checked := False;

     FItemIndex := Value;
     If FItemIndex >= 0 {Select New}
     Then TRadioButton(FRadios[FItemIndex]).Checked := True;

     ItemIndexChange;
End;


Procedure TCustomRadioGroup.ItemIndexChange;
Begin
End;


Procedure TCustomRadioGroup.SetColumns(Value:LongInt);
Begin
     If Value <= 0 Then Value := 1;
     If FColumns <> Value Then
     Begin
          FColumns := Value;
          ArrangeRadios;
     End;
End;


Procedure TCustomRadioGroup.SetItems(Value:TStrings);
Begin
     If Value <> FItems Then FItems.Assign(Value);
End;


Procedure TCustomRadioGroup.SetRadioCount(Value:LongInt);
Var  Radio:TRadioButton;
Begin
     While FRadios.Count < Value Do
     Begin
          Radio:= TRadioButton.Create(Self);
          Include(Radio.ComponentState, csDetail);
          Radio.Font := Font;
          Radio.OnClick := EvRadioClick;
          FRadios.Add(Radio);
     End;
     While FRadios.Count > Value Do
     Begin
          Radio := TRadioButton(FRadios.Last);
          FRadios.Remove(Radio);
          Radio.Destroy;
     End;
End;


Function TCustomRadioGroup.GetItemsEnabled(Index:LongInt):Boolean;
Var  Radio:TRadioButton;
Begin
     Radio := TRadioButton(FRadios.Items[Index]);
     Result := Radio.Enabled;
End;


Procedure TCustomRadioGroup.SetItemsEnabled(Index:LongInt;Value:Boolean);
Var  Radio:TRadioButton;
Begin
     Radio := TRadioButton(FRadios.Items[Index]);
     Radio.Enabled := Value;
End;


Procedure TCustomRadioGroup.UpdateRadios;
Var  I:LongInt;
     Radio:TRadioButton;
Begin
     SetRadioCount(FItems.Count);
     For I := 0 To FItems.Count- 1 Do
     Begin
          Radio := TRadioButton(FRadios.Items[I]);
          Radio.Caption := FItems[I];
     End;
     SetItemIndex(FItemIndex);
     ArrangeRadios;
End;


Procedure TCustomRadioGroup.ArrangeRadios;
Const Margin=10;
Var  I:LongInt;
     Radio:TRadioButton;
     RadioWidth:LongInt;
     RadioHeight:LongInt;
     RadiosPerColumn:LongInt;
     XPos,YPos:LongInt;
     rc:TRect;
Begin
     If Handle = 0 Then Exit;
     If FRadios.Count = 0 Then Exit;

     rc := ClientRect;
     Inc(rc.Left,Margin);
     Inc(rc.Bottom,Margin);
     Dec(rc.Right,Margin);
     Dec(rc.Top,Margin+Font.Height);

     RadiosPerColumn :=  (FRadios.Count + FColumns - 1) Div FColumns;
     RadioWidth := (rc.Right - rc.Left) Div FColumns;
     RadioHeight := (rc.Top - rc.Bottom) Div RadiosPerColumn;

     XPos := rc.Left;
     YPos := rc.Top - RadioHeight;
     For I := 0 To FRadios.Count-1 Do
     Begin
          Radio := TRadioButton(FRadios[I]);
          Radio.SetWindowPos(XPos,YPos,RadioWidth,RadioHeight);
          If Radio.parent = Nil Then Radio.parent := Self;

          If ((I+1) Mod RadiosPerColumn) = 0 Then
          Begin
               YPos := rc.Top - RadioHeight;
               Inc(XPos,RadioWidth);
          End
          Else Dec(YPos,RadioHeight);
     End;
End;


{$HINTS OFF}
Procedure TCustomRadioGroup.EvItemsChange(Sender:TObject);
Begin
     If ComponentState * [csReading] = [] Then
     Begin
          UpdateRadios;
     End;
End;
{$HINTS ON}


Procedure TCustomRadioGroup.EvRadioClick(Sender:TObject);
Begin
     FItemIndex := FRadios.IndexOf(Sender);
     ItemIndexChange;
     Click;
End;


Function TCustomRadioGroup.WriteSCUResource(Stream:TResourceStream):Boolean;
Var  aText:PChar;
Begin
     Result := Inherited WriteSCUResource(Stream);
     If Not Result Then Exit;

     aText := Items.GetText;
     If aText <> Nil Then
     Begin
          Result := Stream.NewResourceEntry(rnItems,aText^,Length(aText^)+1);
          StrDispose(aText);
     End;
End;


Procedure TCustomRadioGroup.ReadSCUResource(Const ResName:TResourceName;Var Data;DataLen:LongInt);
Var  aText:PChar;
Begin
     If ResName = rnItems Then
     Begin
          aText := @Data;
          Items.SetText(aText);
     End
     Else Inherited ReadSCUResource(ResName,Data,DataLen)
End;

Initialization
  {Register classes}
  RegisterClasses([TCustomRadioGroup]);
End.

