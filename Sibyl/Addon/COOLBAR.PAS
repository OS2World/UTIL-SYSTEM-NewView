{***********************************************************************
 *                                                                     *
 * This example implements a Win32 IExplore stlye "CoolBar" and also   *
 * shows how to implement a Class property editor                      *
 *                                                                     *
 * (C) 1998 SpeedSoft                                                  *
 *                                                                     *
 ***********************************************************************}

Unit CoolBar;

Interface

Uses Classes,Forms,Graphics,Buttons,StdCtrls,ComCtrls,Dialogs;

Type
    TCoolSection=Class(THeaderSection)
      Private
         FImage:LongInt;
         FHotImage:LongInt;
         FDisabledImage:LongInt;
         FDisabled:Boolean;
      Private
         Procedure SetDisabled(NewValue:Boolean);
         Procedure SetImage(NewValue:LongInt);
         Procedure SetHotImage(NewValue:LongInt);
         Procedure SetDisabledImage(NewValue:LongInt);
      Public
         Constructor Create(ACollection:TCollection);Override;
         Procedure Assign(Source:TCollectionItem);Override;
      Public
         Property Disabled:Boolean read FDisabled write SetDisabled;
         Property Image:LongInt read FImage write SetImage;
         Property HotImage:LongInt read FHotImage write SetHotImage;
         Property DisabledImage:LongInt read FDisabledImage write SetDisabledImage;
    End;

    TCoolSections=Class(THeaderSections)
      Public
         Procedure SetupComponent;Override;
    End;

    TDrawCoolSectionEvent=Procedure(HeaderControl:THeaderControl;Section:TCoolSection;
                                    Const rc:TRect;Pressed,Hot,Enabled:Boolean) Of Object;

    TCoolBar=Class(THeaderControl)
      Private
         FBackgroundBitmap:TBitmap;
         FActiveSection:TCoolSection;
         FImages:TImageList;
         FFlat:Boolean;
         FBackgroundOffset:LongWord;
         FMouseTimer:TTimer;
         FOnDrawSection:TDrawCoolSectionEvent;
      Private
         Procedure EvFMouseTimer(Sender:TObject);
         Procedure SetBackgroundBitmap(NewValue:TBitmap);
         Procedure UpdateHeader(Header:THeaderSection);Override;
         Procedure SetImages(NewValue:TImageList);
         Procedure SetFlat(NewValue:Boolean);
         Procedure SetBackgroundOffset(NewValue:LongWord);
         Function GetSections:TCoolSections;
         Procedure SetSections(NewValue:TCoolSections);
      Protected
         Procedure DrawSection(Section:TCoolSection;Const rc:TRect;Pressed,Hot,Enabled:Boolean);Virtual;
         Procedure MouseDown(Button:TMouseButton;ShiftState:TShiftState;X,Y:LongInt);Override;
         Procedure MouseDblClick(Button:TMouseButton;ShiftState:TShiftState;X,Y:LongInt);Override;
         Procedure MouseMove(ShiftState:TShiftState;X,Y:LongInt);Override;
         Procedure Notification(AComponent:TComponent;Operation:TOperation);Override;
      Public
         Procedure SetupComponent;Override;
         Procedure Redraw(Const rec:TRect);Override;
         Procedure ReadSCUResource(Const ResName:TResourceName;Var Data;DataLen:LongInt);Override;
         Function WriteSCUResource(Stream:TResourceStream):Boolean;Override;
      Published
         Property BackgroundBitmap:TBitmap read FBackgroundBitmap write SetBackgroundBitmap;
         Property BackgroundOffset:LongWord read FBackgroundOffset write SetBackgroundOffset;
         Property Images:TImageList read FImages write SetImages;
         Property Flat:Boolean read FFlat write SetFlat;
         Property Sections:TCoolSections Read GetSections Write SetSections;
      Published
         Property OnDrawSection:TDrawCoolSectionEvent Read FOnDrawSection Write FOnDrawSection;
    End;

Implementation

{$R CoolBar}

{
浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
�                                                                           �
� Speed-Pascal/2 Version 2.0                                                �
�                                                                           �
� Speed-Pascal Component Classes (SPCC)                                     �
�                                                                           �
� This section: TCoolSection Class Implementation                           �
�                                                                           �
� (C) 1995,97 SpeedSoft. All rights reserved. Disclosure probibited !       �
�                                                                           �
藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
}

Constructor TCoolSection.Create(ACollection:TCollection);
Begin
     Inherited Create(ACollection);
     FImage:=-1;
     FHotImage:=-1;
     FDisabledImage:=-1;
End;

Procedure TCoolSection.Assign(Source:TCollectionItem);
Begin
     Inherited Assign(Source);
     If Source Is TCoolSection Then
       If Source<>Self Then
     Begin
          FImage:=TCoolSection(Source).Image;
          FHotImage:=TCoolSection(Source).HotImage;
          FDisabledImage:=TCoolSection(Source).DisabledImage;
          FDisabled:=TCoolSection(Source).Disabled;
     End;
End;

Procedure TCoolSection.SetDisabled(NewValue:Boolean);
Begin
     If NewValue=FDisabled Then exit;
     FDisabled:=NewValue;
     Changed(False);
End;

Procedure TCoolSection.SetImage(NewValue:LongInt);
Begin
     If NewValue=FImage Then exit;
     FImage:=NewValue;
     Changed(False);
End;

Procedure TCoolSection.SetHotImage(NewValue:LongInt);
Begin
     If NewValue=FHotImage Then exit;
     FHotImage:=NewValue;
     Changed(False);
End;

Procedure TCoolSection.SetDisabledImage(NewValue:LongInt);
Begin
     If NewValue=FDisabledImage Then exit;
     FDisabledImage:=NewValue;
     Changed(False);
End;

{
浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
�                                                                           �
� Speed-Pascal/2 Version 2.0                                                �
�                                                                           �
� Speed-Pascal Component Classes (SPCC)                                     �
�                                                                           �
� This section: TCoolSections Class Implementation                          �
�                                                                           �
� (C) 1995,97 SpeedSoft. All rights reserved. Disclosure probibited !       �
�                                                                           �
藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
}

Procedure TCoolSections.SetupComponent;
Begin
    Inherited SetupComponent;
    Name:='CoolSections';
    ItemClass:=TCoolSection;
End;

{
浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
�                                                                           �
� Speed-Pascal/2 Version 2.0                                                �
�                                                                           �
� Speed-Pascal Component Classes (SPCC)                                     �
�                                                                           �
� This section: TCoolBar Class Implementation                               �
�                                                                           �
� (C) 1995,97 SpeedSoft. All rights reserved. Disclosure probibited !       �
�                                                                           �
藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
}

Procedure TCoolBar.SetupComponent;
Begin
     Inherited SetupComponent;

     FBackgroundBitmap.Create;
     FBackgroundBitmap.LoadFromResourceName('Cool');
     FFlat:=True;
     FMouseTimer.Create(Self);
     Include(FMouseTimer.ComponentState, csDetail);
     FMouseTimer.Interval := 50;
     FMouseTimer.OnTimer := EvFMouseTimer;
     Name:='CoolBar';
     SectionsClass:=TCoolSections;
End;

Procedure TCoolBar.SetFlat(NewValue:Boolean);
Begin
     If NewValue=FFlat Then exit;
     FFlat:=NewValue;
     Invalidate;
End;

Function TCoolBar.GetSections:TCoolSections;
Begin
     Result:=TCoolSections(Inherited Sections);
End;

Procedure TCoolBar.SetSections(NewValue:TCoolSections);
Begin
     Inherited Sections:=NewValue;
End;


Procedure TCoolBar.SetImages(NewValue:TImageList);
Begin
     If NewValue=FImages Then exit;
     If FImages<>Nil Then FImages.Notification(Self,opRemove);
     FImages:=NewValue;
     If FImages <> Nil Then FImages.FreeNotification(Self);
     Invalidate;
End;

Procedure TCoolBar.Notification(AComponent:TComponent;Operation:TOperation);
Begin
     Inherited Notification(AComponent,Operation);

     If Operation = opRemove Then
       If AComponent = FImages Then FImages := Nil;
End;


Procedure TCoolBar.SetBackgroundBitmap(NewValue:TBitmap);
Begin
     If NewValue=FBackgroundBitmap Then exit;
     If FBackgroundBitmap<>Nil Then FBackgroundBitmap.Destroy;
     If NewValue<>Nil Then FBackgroundBitmap:=NewValue.Copy
     Else FBackgroundBitmap:=Nil;
End;

Procedure TCoolBar.SetBackgroundOffset(NewValue:LongWord);
Begin
     If NewValue=FBackgroundOffset Then exit;
     FBackgroundOffset:=NewValue;
     Invalidate;
End;

Procedure TCoolBar.DrawSection(Section:TCoolSection;Const rc:TRect;Pressed,Hot,Enabled:Boolean);
Var
   Align:TAlignment;
   S:String;
   CX,CY,CX1,CY1,H,X,Y:LongInt;
   rec,bmprec:TRect;
   PointsArray:Array[0..5] Of TPoint;
   offs:LongInt;
   FBitmap,FMask:TBitmap;

   Procedure DrawMasked(Bitmap,Mask:TBitmap;X,Y:LongInt);
   Var Source,Dest:TRect;
   Begin
        If Bitmap=Nil Then exit;
        Source.Left:=0;
        Source.Bottom:=0;
        Dest.Left:=X;
        Dest.Bottom:=Y;

        If Mask<>Nil Then
        Begin
             Source.Right:=Mask.Width;
             Source.Top:=Mask.Height;
             Dest.Top:=Dest.Bottom+Mask.Height;
             Dest.Right:=Dest.Left+Mask.Width;
             Mask.Canvas.BitBlt(Canvas,Dest,Source,cmSrcAnd,bitfIgnore);
        End;

        Source.Right:=Bitmap.Width;
        Source.Top:=Bitmap.Height;
        Dest.Top:=Dest.Bottom+Bitmap.Height;
        Dest.Right:=Dest.Left+Bitmap.Width;
        If Mask<>Nil Then
          Bitmap.Canvas.BitBlt(Canvas,Dest,Source,cmSrcPaint,bitfIgnore)
        Else
          Bitmap.Canvas.BitBlt(Canvas,Dest,Source,cmSrcCopy,bitfIgnore);

        Canvas.ExcludeClipRect(Dest);
   End;

Begin
     Align:=section.Alignment;
     S:=section.Text;

     If S='' Then
     Begin
          CX:=0;
          CY:=0;
     End
     Else Canvas.GetTextExtent(S,CX,CY);

     FBitmap:=Nil;
     FMask:=Nil;
     If FImages<>Nil Then If FImages.Count>0 Then
     Begin
          If not Enabled Then
          Begin
              If Section.DisabledImage>=0 Then
              If FImages.Count>Section.DisabledImage Then
              Begin
                   FBitmap.Create;
                   FImages.GetBitmap(Section.DisabledImage,FBitmap);
                   FMask.Create;
                   FImages.GetMask(Section.DisabledImage,FMask);
              End;
          End
          Else If Hot Then
          Begin
              If Section.HotImage>=0 Then
              If FImages.Count>Section.HotImage Then
              Begin
                   FBitmap.Create;
                   FImages.GetBitmap(Section.HotImage,FBitmap);
                   FMask.Create;
                   FImages.GetMask(Section.HotImage,FMask);
              End;
          End;

          If FBitmap<>Nil Then If FBitmap.Empty Then
          Begin
               FBitmap.Destroy;
               FBitmap:=Nil;
          End;

          If FMask<>Nil Then If FMask.Empty Then
          Begin
               FMask.Destroy;
               FMask:=Nil;
          End;

          If FBitmap=Nil Then
          Begin
               If Section.Image>=0 Then
               If FImages.Count>Section.Image Then
               Begin
                    FBitmap.Create;
                    FImages.GetBitmap(Section.Image,FBitmap);
                    FMask.Create;
                    FImages.GetMask(Section.Image,FMask);
               End;
          End;

          If FBitmap<>Nil Then If FBitmap.Empty Then
          Begin
               FBitmap.Destroy;
               FBitmap:=Nil;
          End;

          If FMask<>Nil Then If FMask.Empty Then
          Begin
               FMask.Destroy;
               FMask:=Nil;
          End;
     End;

     CX1:=CX;
     CY1:=CY;

     If FBitmap<>Nil Then
     Begin
          If Align=taCenter Then inc(CY1,FBitmap.Height+3)
          Else inc(CX1,FBitmap.Width+3);
     End;

     Case Align Of
        taLeftJustify:
        Begin
             H:=rc.Right-rc.Left;
             rec.Left:=rc.Left+((H-CX1) Div 2);
        End;
        taRightJustify:
        Begin
             H:=rc.Right-rc.Left;
             rec.Left:=rc.Left+((H-CX1) Div 2);
             If FBitmap<>Nil Then inc(rec.Left,FBitmap.Width);
        End
        Else //taCenter
        Begin
             H:=rc.Right-rc.Left;
             rec.Left:=rc.Left+((H-CX) Div 2);
        End;
     End; //Case

     If rec.Left<rc.Left+3 Then rec.Left:=rc.Left+3;

     H:=rc.Top-rc.Bottom;
     rec.Bottom:=rc.Bottom+((H-CY1) Div 2);
     If rec.Bottom<rc.Bottom+3 Then rec.Bottom:=rc.Bottom+3;
     rec.Right:=rec.Left+CX-1;
     rec.Top:=rec.Bottom+CY-1;

     //Draw Bitmap
     If FBitmap<>Nil Then
     Begin
        H:=rc.Top-rc.Bottom;
        Y:=rc.Bottom+((H-CY1) Div 2);
        If Y<rc.Bottom+3 Then Y:=rc.Bottom+3;

        Case Align Of
            taLeftJustify:
            Begin
                 DrawMasked(FBitmap,FMask,rec.Right+3,Y);
            End;
            taRightJustify:
            Begin
                 DrawMasked(FBitmap,FMask,rec.Left-3-FBitmap.Width,Y);
            End;
            Else //taCenter
            Begin
                 H:=rc.Right-rc.Left;
                 X:=rc.Left+((H-FBitmap.Width) Div 2);
                 If X<rc.Left+3 Then X:=rc.Left+3;
                 DrawMasked(FBitmap,FMask,X,Y+CY+3);
            End;
         End; //Case

         FBitmap.Destroy;
         If FMask<>Nil Then FMask.Destroy;
     End;

     If S<>'' Then
     Begin
        Canvas.Brush.Mode:=bmTransparent;
        If not Enabled Then Canvas.Pen.Color:=clDkGray
        Else Canvas.Pen.Color:=PenColor;

        Canvas.TextOut(rec.Left,rec.Bottom,S);
        Canvas.Brush.Mode:=bmOpaque;

        Canvas.ExcludeClipRect(rec);
     End;

     If ((not Flat)Or(Hot And Enabled)) Then
     Begin
         If BevelWidth > 1 Then
         Begin
              offs := BevelWidth-1;
              PointsArray[0] := Point(rc.Left,rc.Bottom);
              PointsArray[1] := Point(rc.Left+offs,rc.Bottom+offs);
              PointsArray[2] := Point(rc.Left+offs,rc.Top-offs);
              PointsArray[3] := Point(rc.Right-offs,rc.Top-offs);
              PointsArray[4] := Point(rc.Right,rc.Top);
              PointsArray[5] := Point(rc.Left,rc.Top);
              Canvas.Pen.color := clWhite;
              Canvas.Polygon(PointsArray);
              PointsArray[2] := Point(rc.Right-offs,rc.Bottom+offs);
              PointsArray[3] := Point(rc.Right-offs,rc.Top-offs);
              PointsArray[4] := Point(rc.Right,rc.Top);
              PointsArray[5] := Point(rc.Right,rc.Bottom);
              Canvas.Pen.color := clDkGray;
              Canvas.Polygon(PointsArray);
              Canvas.Pen.color:=PenColor;
         End
         Else Canvas.ShadowedBorder(rc,clWhite,clDkGray);
     End;
End;

Type
    PHeaderItem=^THeaderItem;
    THeaderItem=Record
        Image:LongInt;
        HotImage:LongInt;
        DisabledImage:LongInt;
        Disabled:Boolean;
    End;

Const rnCoolHeaders='rnCoolHeaders';

Procedure TCoolBar.ReadSCUResource(Const ResName:TResourceName;Var Data;DataLen:LongInt);
Var
   Count:^LongInt;
   Items:PHeaderItem;
   section:TCoolSection;
   T:LongInt;
   ps:^String;
Begin
     If ResName = rnCoolHeaders Then
     Begin
          Count:=@Data;
          Items:=@Data;
          Inc(Items,4);
          For T:=1 To Count^ Do
          Begin
               Section:=TCoolSection(Sections[t-1]);
               section.Image:=Items^.Image;
               section.HotImage:=Items^.HotImage;
               section.DisabledImage:=Items^.DisabledImage;
               section.Disabled:=Items^.Disabled;
               Inc(Items,SizeOf(THeaderItem));
          End;
     End
     Else Inherited ReadSCUResource(ResName,Data,DataLen);
End;


Function TCoolBar.WriteSCUResource(Stream:TResourceStream):Boolean;
Var MemStream:TMemoryStream;
    T:LongInt;
    Item:THeaderItem;
    section:TCoolSection;
    S:String;
Begin
     Result := Inherited WriteSCUResource(Stream);
     If Not Result Then Exit;

     If Sections<>Nil Then If Sections.Count>0 Then
     Begin
          MemStream.Create;
          T:=Sections.Count;
          MemStream.Write(T,4);
          For T:=0 To Sections.Count-1 Do
          Begin
               section:=TCoolSection(Sections[T]);
               Item.Image:=section.Image;
               Item.HotImage:=section.HotImage;
               Item.DisabledImage:=section.DisabledImage;
               Item.Disabled:=section.Disabled;
               MemStream.Write(Item,SizeOf(THeaderItem));
          End;

          Result:=Stream.NewResourceEntry(rnCoolHeaders,MemStream.Memory^,MemStream.Size);
          MemStream.Destroy;
     End;
End;


Procedure TCoolBar.Redraw(Const rec:TRect);
Var X,Y:LongInt;
    rc,rc2,Src:TRect;
    FSections:TCoolSections;
    Section:TCoolSection;
    IsPressed:Boolean;
    t,W:LongInt;
Begin
    Canvas.ClipRect:=rec;

    If FBackgroundBitmap<>Nil Then
    Begin
         Y:=0;
         While Y<Height Do
         Begin
             X:=0;
             While X<Width Do
             Begin
                  rc.Left:=X;
                  rc.Right:=rc.Left+FBackgroundBitmap.Width;
                  rc.Bottom:=Y;
                  rc.Top:=rc.Bottom+FBackgroundBitmap.Height;
                  If ((BackgroundOffset<>0)And(Y=0)And(X=0)) Then
                  Begin
                       W:=BackgroundOffset;
                       If W>BackgroundBitmap.Width Then W:=BackgroundBitmap.Width;
                       Src.Left:=W;
                       Src.Bottom:=0;
                       Src.Right:=FBackGroundBitmap.Width;
                       Src.Top:=FBackGroundBitmap.Height;
                       rc.Right:=rc.Left+(BackgroundBitmap.Width-W);
                       FBackgroundBitmap.PartialDraw(Canvas,Src,rc);
                       inc(X,BackgroundBitmap.Width-W);
                  End
                  Else
                  Begin
                       FBackgroundBitmap.Draw(Canvas,rc);
                       inc(X,FBackgroundBitmap.Width);
                  End;
             End;
             inc(Y,FBackgroundBitmap.Height);
         End;
    End
    Else Canvas.FillRect(rec,Color);

    FSections:=TCoolSections(Sections);
    rc:=ClientRect;
    Inc(rc.Bottom);
    For T:=0 To FSections.Count-1 Do
     Begin
          Section:=TCoolSection(FSections[T]);
          rc.Right:=rc.Left+section.Width;
          If rc.Right>Width-1 Then rc.Right:=Width-1;

          IsPressed:=Section=ClickSection;
          If IsPressed Then
          Begin
               Inc(rc.Left);
               Inc(rc.Right);
               Dec(rc.Bottom);
               Dec(rc.Top);
          End;

          rc2:=Forms.IntersectRect(rc,rec);
          If Not Forms.IsRectEmpty(rc2) Then
          Begin
               inc(rc2.Right);
               Canvas.ClipRect:=rc2;

               If Section.Style=hsOwnerDraw Then
               Begin
                    If OnDrawSection<>Nil Then OnDrawSection(Self,section,rc,IsPressed,Section=FActiveSection,Section.Disabled=False)
                    Else DrawSection(Section,rc,IsPressed,Section=FActiveSection,Section.Disabled=False);
               End
               Else DrawSection(section,rc,IsPressed,Section=FActiveSection,Section.Disabled=False);
          End;

          If IsPressed Then
          Begin
               Dec(rc.Left);
               Dec(rc.Right);
               Inc(rc.Bottom);
               Inc(rc.Top);
          End;
          Inc(rc.Left,Section.Width+Spacing+1);
     End;

     Canvas.DeleteClipRegion;
End;

Procedure TCoolBar.MouseDown(Button:TMouseButton;ShiftState:TShiftState;X,Y:LongInt);
Var T:LongInt;
    section:TCoolSection;
    FSections:TCoolSections;
Begin
     TControl.MouseDown(Button,ShiftState,X,Y);

     If Button <> mbLeft Then Exit;

     FSections:=TCoolSections(Sections);
     For T:=0 To FSections.Count-1 Do
     Begin
          section:=TCoolSection(FSections[T]);
          If ((section.AllowSize)And(X>section.Right-2)And(X<section.Right+2)) Then
          Begin
               Inherited MouseDown(Button,ShiftState,X,Y);
               exit;
          End;
     End;

     If Designed Then Exit;

     //Test Press
     Section:=TCoolSection(GetMouseHeader(X,Y));
     If Section<>Nil Then If section.AllowClick Then If not Section.Disabled Then
       Inherited MouseDown(Button,ShiftState,X,Y);
End;

Procedure TCoolBar.MouseDblClick(Button:TMouseButton;ShiftState:TShiftState;X,Y:LongInt);
Var section:TCoolSection;
Begin
     If Button=mbLeft Then
     Begin
          Section:=TCoolSection(GetMouseHeader(X,Y));
          If Section<>Nil Then If section.AllowClick Then If not Section.Disabled Then
            Inherited MouseDblClick(Button,ShiftState,X,Y);
     End
     Else Inherited MouseDblClick(Button,ShiftState,X,Y);
End;

Procedure TCoolBar.UpdateHeader(Header:THeaderSection);
Var T:LongInt;
    rc:TRect;
    FSections:TCoolSections;
Begin
     //Get Rectangle For the Panel
     rc:=ClientRect;
     FSections:=TCoolSections(Sections);
     For T:=0 To FSections.Count-1 Do
     Begin
          If FSections[T]=Header Then break
          Else Inc(rc.Left,FSections[T].Width+Spacing+1);
     End;

     rc.Right:=rc.Left+Header.Width+1;
     If not Flat Then InflateRect(rc,-BevelWidth*2,-BevelWidth*2);
     InvalidateRect(rc);
     Update;
End;


Procedure TCoolBar.MouseMove(ShiftState:TShiftState;X,Y:LongInt);
Var
    Section,OldActiveSection:TCoolSection;
Begin
     FMouseTimer.Stop;

     OldActiveSection:=FActiveSection;
     Section:=TCoolSection(GetMouseHeader(X,Y));
     If ((Section=Nil)Or(not Section.Disabled)) Then FActiveSection:=Section;

     Inherited MouseMove(ShiftState,X,Y);

     If FActiveSection<>OldActiveSection Then
     Begin
          If OldActiveSection<>Nil Then UpdateHeader(OldActiveSection);
          If FActiveSection<>Nil Then UpdateHeader(FActiveSection);
     End;

     If FFlat Then FMouseTimer.Start;
End;


Procedure TCoolBar.EvFMouseTimer(Sender:TObject);
Var
  AControl:TControl;
  OldActiveSection:TCoolSection;
Begin
  FMouseTimer.Stop;

  AControl := Screen.GetControlFromPoint(Screen.MousePos);

  If AControl <> Self Then
  Begin
    OldActiveSection:=FActiveSection;
    FActiveSection := Nil;
    If OldActiveSection<>Nil Then UpdateHeader(OldActiveSection);
  End
  Else FMouseTimer.Start;
End;

{
浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
�                                                                           �
� Sibyl Version 2.0                                                         �
�                                                                           �
� This section: TCoolSectionsPropertyEditor Class implementation            �
�                                                                           �
�                                                                           �
藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
}

Type
    TCoolSectionsPropertyEditor=Class(TClassPropertyEditor)
      Public
         Function Execute(Var ClassToEdit:TObject):TClassPropertyEditorReturn;Override;
    End;

    TCoolSectionsPropEditDialog=Class(TDialog)
      Private
         FSections:TCoolSections;
         FListBox:TListBox;
         FText:TEdit;
         FWidth:TEdit;
         FMinWidth,FMaxWidth:TEdit;
         FImage,FHotImage,FDisabledImage:TEdit;
         FStyle:TComboBox;
         FAlignment:TComboBox;
         FCurrentSection:TCoolSection;
         FCurrentIndex:LongInt;
         FAllowClick:TCheckBox;
         FAllowSize:TCheckBox;
         FDisabled:TCheckBox;
      Protected
         Procedure SetupComponent;Override;
         Procedure NewClicked(Sender:TObject);
         Procedure DeleteClicked(Sender:TObject);
         Procedure UpdateClicked(Sender:TObject);
         Procedure ListItemFocus(Sender:TObject;Index:LongInt);
         Procedure StoreItem;
         Procedure TextChange(Sender:TObject);
         Procedure WidthChange(Sender:TObject);
         Procedure MinWidthChange(Sender:TObject);
         Procedure MaxWidthChange(Sender:TObject);
         Procedure ImageChange(Sender:TObject);
         Procedure HotImageChange(Sender:TObject);
         Procedure DisabledClick(Sender:TObject);
         Procedure DisabledImageChange(Sender:TObject);
         Procedure StyleSelect(Sender:TObject;Index:LongInt);
         Procedure AlignmentSelect(Sender:TObject;Index:LongInt);
    End;


{$HINTS OFF}
Procedure TCoolSectionsPropEditDialog.TextChange(Sender:TObject);
Begin
     If FCurrentSection=Nil Then exit;
     FCurrentSection.Text:=FText.Text;
End;

Procedure TCoolSectionsPropEditDialog.WidthChange(Sender:TObject);
Var i:LongInt;
    c:Integer;
Begin
     If FCurrentSection=Nil Then exit;
     VAL(FWidth.Text,i,c);
     If c<>0 Then exit;
     FCurrentSection.Width:=i;
End;

Procedure TCoolSectionsPropEditDialog.MinWidthChange(Sender:TObject);
Var i:LongInt;
    c:Integer;
Begin
     If FCurrentSection=Nil Then exit;
     VAL(FMinWidth.Text,i,c);
     If c<>0 Then exit;
     FCurrentSection.MinWidth:=i;
End;

Procedure TCoolSectionsPropEditDialog.MaxWidthChange(Sender:TObject);
Var i:LongInt;
    c:Integer;
Begin
     If FCurrentSection=Nil Then exit;
     VAL(FMaxWidth.Text,i,c);
     If c<>0 Then exit;
     FCurrentSection.MaxWidth:=i;
End;

Procedure TCoolSectionsPropEditDialog.DisabledClick(Sender:TObject);
Begin
     If FCurrentSection=Nil Then exit;
     FCurrentSection.Disabled:=FDisabled.Checked;
End;

Procedure TCoolSectionsPropEditDialog.ImageChange(Sender:TObject);
Var i:LongInt;
    c:Integer;
Begin
     If FCurrentSection=Nil Then exit;
     VAL(FImage.Text,i,c);
     If c<>0 Then exit;
     FCurrentSection.Image:=i;
End;

Procedure TCoolSectionsPropEditDialog.HotImageChange(Sender:TObject);
Var i:LongInt;
    c:Integer;
Begin
     If FCurrentSection=Nil Then exit;
     VAL(FHotImage.Text,i,c);
     If c<>0 Then exit;
     FCurrentSection.HotImage:=i;
End;

Procedure TCoolSectionsPropEditDialog.DisabledImageChange(Sender:TObject);
Var i:LongInt;
    c:Integer;
Begin
     If FCurrentSection=Nil Then exit;
     VAL(FDisabledImage.Text,i,c);
     If c<>0 Then exit;
     FCurrentSection.DisabledImage:=i;
End;


Procedure TCoolSectionsPropEditDialog.StyleSelect(Sender:TObject;Index:LongInt);
Begin
     If FCurrentSection=Nil Then exit;
     If FStyle.Text='OwnerDraw' Then FCurrentSection.Style:=hsOwnerDraw
     Else FCurrentSection.Style:=hsText;
End;

Procedure TCoolSectionsPropEditDialog.AlignmentSelect(Sender:TObject;Index:LongInt);
Begin
     If FCurrentSection=Nil Then exit;
     If FAlignment.Text='Center' Then FCurrentSection.Alignment:=taCenter
     Else If FAlignment.Text='Right justify' Then FCurrentSection.Alignment:=taRightJustify
     Else FCurrentSection.Alignment:=taLeftJustify;
End;

Procedure TCoolSectionsPropEditDialog.UpdateClicked(Sender:TObject);
Begin
     StoreItem;
     FSections.Update(Nil);
End;

Procedure TCoolSectionsPropEditDialog.NewClicked(Sender:TObject);
Var Section:THeaderSection;
Begin
     Section:=FSections.Add;
     If Section.Text='' Then FListBox.Items.Add(tostr(Section.Index)+' - (Untitled)')
     Else FListBox.Items.Add(tostr(Section.Index)+' - '+Section.Text);
     FListBox.ItemIndex:=Section.Index;
     FSections.Update(Nil);
End;

Procedure TCoolSectionsPropEditDialog.DeleteClicked(Sender:TObject);
Var Section:THeaderSection;
    Index:LongInt;
Begin
     Index:=FListBox.ItemIndex;
     If Index<0 Then exit;
     FListBox.Items.Delete(Index);
     Section:=FSections[Index];
     Section.Destroy;
     FCurrentSection:=Nil;
     FCurrentIndex:=-1;
     If FListBox.Items.Count>0 Then FListBox.ItemIndex:=0;
End;
{$HINTS ON}

Procedure TCoolSectionsPropEditDialog.StoreItem;
Var c:Integer;
    i:LongInt;
Begin
     If FCurrentSection<>Nil Then //store values
     Begin
          FCurrentSection.Text:=FText.Text;
          If FText.Text='' Then FListBox.Items[FCurrentIndex]:=tostr(FCurrentIndex)+' - (Untitled)'
          Else FListBox.Items[FCurrentIndex]:=tostr(FCurrentIndex)+' - '+FText.Text;

          VAL(FWidth.Text,i,c);
          If c<>0 Then i:=100;
          FCurrentSection.Width:=i;

          VAL(FMinWidth.Text,i,c);
          If c<>0 Then i:=0;
          FCurrentSection.MinWidth:=i;

          VAL(FMaxWidth.Text,i,c);
          If c<>0 Then i:=10000;
          FCurrentSection.MaxWidth:=i;

          VAL(FImage.Text,i,c);
          If c<>0 Then i:=10000;
          FCurrentSection.Image:=i;

          VAL(FHotImage.Text,i,c);
          If c<>0 Then i:=10000;
          FCurrentSection.HotImage:=i;

          VAL(FDisabledImage.Text,i,c);
          If c<>0 Then i:=10000;
          FCurrentSection.DisabledImage:=i;

          FCurrentSection.Disabled:=FDisabled.Checked;

          If FStyle.Text='OwnerDraw' Then FCurrentSection.Style:=hsOwnerDraw
          Else FCurrentSection.Style:=hsText;

          If FAlignment.Text='Center' Then FCurrentSection.Alignment:=taCenter
          Else If FAlignment.Text='Right justify' Then FCurrentSection.Alignment:=taRightJustify
          Else FCurrentSection.Alignment:=taLeftJustify;

          FCurrentSection.AllowClick:=FAllowClick.Checked;
          FCurrentSection.AllowSize:=FAllowSize.Checked;
     End;
End;

Procedure TCoolSectionsPropEditDialog.ListItemFocus(Sender:TObject;Index:LongInt);
Begin
     StoreItem;

     FCurrentSection:=TCoolSection(FSections[Index]);
     FCurrentIndex:=Index;
     FText.Text:=FCurrentSection.Text;
     FWidth.Text:=tostr(FCurrentSection.Width);
     FMinWidth.Text:=tostr(FCurrentSection.MinWidth);
     FMaxWidth.Text:=tostr(FCurrentSection.MaxWidth);
     FImage.Text:=tostr(FCurrentSection.Image);
     FHotImage.Text:=tostr(FCurrentSection.HotImage);
     FDisabledImage.Text:=tostr(FCurrentSection.DisabledImage);
     FDisabled.Checked:=FCurrentSection.Disabled;
     If FCurrentSection.Style=hsText Then FStyle.Text:='Text'
     Else FStyle.Text:='OwnerDraw';

     Case FCurrentSection.Alignment Of
        taRightJustify:FAlignment.Text:='Right justify';
        taCenter:FAlignment.Text:='Center';
        Else FAlignment.Text:='Left justify';
     End;

     FAllowClick.Checked:=FCurrentSection.AllowClick;
     FAllowSize.Checked:=FCurrentSection.AllowSize;
End;

Procedure TCoolSectionsPropEditDialog.SetupComponent;
Var Button:TButton;
    ComboBox:TComboBox;
Begin
     Inherited SetupComponent;

     Caption:='CoolBar sections editor';
     Width:=445;
     Height:=380;

     InsertGroupBox(Self,10,50,180,290,'Sections');
     FListBox:=InsertListBox(Self,20,100,160,220,'');
     FListBox.OnItemFocus:=ListItemFocus;

     Button:=InsertButton(Self,20,60,70,30,'New','New Section');
     Button.OnClick:=NewClicked;
     Button:=InsertButton(Self,100,60,70,30,'Delete','Delete Section');
     Button.OnClick:=DeleteClicked;

     InsertGroupBox(Self,200,50,230,290,'Section Properties');

     InsertLabel(Self,210,290,50,20,'Text');
     FText:=InsertEdit(Self,280,295,140,20,'','');
     FText.OnChange:=TextChange;

     InsertLabel(Self,210,260,100,20,'Width');
     FWidth:=InsertEdit(Self,280,265,140,20,'','');
     FWidth.OnChange:=WidthChange;
     FWidth.NumbersOnly:=TRUE;

     InsertLabel(Self,210,230,60,20,'Min/Max');
     FMinWidth:=InsertEdit(Self,280,235,65,20,'','');
     FMinWidth.OnChange:=MinWidthChange;
     FMinWidth.NumbersOnly:=TRUE;
     FMaxWidth:=InsertEdit(Self,355,235,65,20,'','');
     FMaxWidth.OnChange:=MaxWidthChange;
     FMaxWidth.NumbersOnly:=TRUE;

     InsertLabel(Self,210,200,100,20,'Style');
     FStyle:=InsertComboBox(Self,280,205,140,20,csDropDownList);
     FStyle.Items.Add('Text');
     FStyle.Items.Add('OwnerDraw');
     FStyle.OnItemSelect:=StyleSelect;

     InsertLabel(Self,210,170,100,20,'Alignment');
     FAlignment:=InsertComboBox(Self,280,175,140,20,csDropDownList);
     FAlignment.Items.Add('Left justify');
     FAlignment.Items.Add('Right justify');
     FAlignment.Items.Add('Center');
     FAlignment.OnItemSelect:=AlignmentSelect;

     FAllowClick:=InsertCheckBox(Self,210,135,100,20,'Allow Click','');
     FAllowSize:=InsertCheckBox(Self,210,115,100,20,'Allow Size','');
     FDisabled:=InsertCheckBox(Self,210,95,100,20,'Disabled','');
     FDisabled.OnClick:=DisabledClick;

     InsertLabel(Self,300,133,100,20,'Image');
     FImage:=InsertEdit(Self,400,138,20,20,'','');
     FImage.OnChange:=ImageChange;
     FImage.NumbersOnly:=TRUE;

     InsertLabel(Self,300,113,100,20,'HotImage');
     FHotImage:=InsertEdit(Self,400,118,20,20,'','');
     FHotImage.OnChange:=HotImageChange;
     FHotImage.NumbersOnly:=TRUE;

     InsertLabel(Self,300,93,100,20,'DisabledImage');
     FDisabledImage:=InsertEdit(Self,400,98,20,20,'','');
     FDisabledImage.OnChange:=DisabledImageChange;
     FDisabledImage.NumbersOnly:=TRUE;

     Button:=InsertButton(Self,210,60,200,30,'Update','Update Section');
     Button.OnClick:=UpdateClicked;

     InsertBitBtn(Self,10,10,90,30,bkOk,'~Ok','Click here to accept');
     InsertBitBtn(Self,110,10,90,30,bkCancel,'~Cancel','Click here to cancel');
     InsertBitBtn(Self,210,10,90,30,bkHelp,'~Help','Click here to get help');
End;

Function TCoolSectionsPropertyEditor.Execute(Var ClassToEdit:TObject):TClassPropertyEditorReturn;
Var  HeaderSections:TCoolSections;
     FDialog:TCoolSectionsPropEditDialog;
     SaveHeaders:TCoolSections;
     Section:TCoolSection;
     t:LongInt;
Begin
     HeaderSections:=TCoolSections(ClassToEdit);
     If HeaderSections.HeaderControl=Nil Then
     Begin
          result:=peNoEditor;
          exit;
     End;

     SaveHeaders.Create(Nil);
     SaveHeaders.Assign(HeaderSections);

     FDialog.Create(Nil);
     FDialog.FSections:=HeaderSections;

     For t:=0 TO HeaderSections.Count-1 Do
     Begin
          Section:=TCoolSection(HeaderSections[t]);
          If Section.Text='' Then FDialog.FListBox.Items.Add(tostr(t)+' - (Untitled)')
          Else FDialog.FListBox.Items.Add(tostr(t)+' - '+Section.Text);
     End;
     If FDialog.FListBox.Items.Count>0 Then FDialog.FListBox.ItemIndex:=0;

     FDialog.ShowModal;

     //Modify ClassToEdit here
     result:=peCancel;
     Case FDialog.ModalResult Of
        cmOk:
        Begin
             FDialog.StoreItem;
             result:=peOk;
        End;
        Else
        Begin
             HeaderSections.Assign(SaveHeaders);
             result:=peCancel;
        End;
     End; {Case}

     SaveHeaders.Destroy;
     FDialog.Destroy;
End;

Initialization
   RegisterClasses([TCoolBar]);
   //Register property editor
   AddClassPropertyEditor(TCoolSections,TCoolSectionsPropertyEditor);
End.
