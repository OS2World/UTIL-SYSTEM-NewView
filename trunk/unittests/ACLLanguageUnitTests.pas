Unit ACLLanguageUnitTests;

// NewView - a new OS/2 Help Viewer
// Copyright 2006-2007 Ronald Brill (rbri at rbri dot de)
// This software is released under the GNU Public License - see readme.txt

// UnitTests for ACLLanguageUnitTests

Interface

uses
  Classes,
  TestAssert,
  ACLLanguageUnit;

  FUNCTION getACLLanguageUnitTests : TList;


Implementation

  PROCEDURE testTLanguageItemList_ConstructorDestructor;
  VAR
    tmpItems : TLanguageItemList;
  BEGIN
    tmpItems := TLanguageItemList.Create;
    tmpItems.Destroy;
  END;


  PROCEDURE testTLanguageItemList_Empty_NotFound;
  VAR
    tmpItems : TLanguageItemList;
    tmpFound : String;
  BEGIN
    tmpItems := TLanguageItemList.Create;

    tmpFound := tmpItems.getValue('label1', 'default');
    assertEqualsString('testTLanguageItemList_Empty_NotFound', '', tmpFound);

    tmpFound := tmpItems.getValue('label1', 'default');
    assertEqualsString('testTLanguageItemList_Empty_NotFound (2)', '', tmpFound);

    tmpItems.Destroy;
  END;


  PROCEDURE testTLanguageItemList_OneValue_NotFound;
  VAR
    tmpItems : TLanguageItemList;
    tmpFound : String;
  BEGIN
    tmpItems := TLanguageItemList.Create;

    tmpItems.setValue('label1', 'value1');

    tmpFound := tmpItems.getValue('unknown', 'default');
    assertEqualsString('testTLanguageItemList_OneValue_NotFound', '', tmpFound);

    tmpFound := tmpItems.getValue('unknown', 'default');
    assertEqualsString('testTLanguageItemList_OneValue_NotFound (2)', '', tmpFound);

    tmpItems.Destroy;
  END;


  PROCEDURE testTLanguageItemList_OneValue;
  VAR
    tmpItems : TLanguageItemList;
    tmpFound : String;
  BEGIN
    tmpItems := TLanguageItemList.Create;

    tmpItems.setValue('label1', 'value1');

    tmpFound := tmpItems.getValue('label1', 'default');
    assertEqualsString('testTLanguageItemList_OneValue label1', 'value1', tmpFound);

    tmpFound := tmpItems.getValue('LABEL1', 'default');
    assertEqualsString('testTLanguageItemList_OneValue LABEL1', 'value1', tmpFound);

    tmpFound := tmpItems.getValue('LaBel1', 'default');
    assertEqualsString('testTLanguageItemList_OneValue LaBel1', 'value1', tmpFound);

    tmpItems.Destroy;
  END;


  PROCEDURE testTLanguageItemList_ManyValues;
  VAR
    tmpItems : TLanguageItemList;
    tmpFound : String;
  BEGIN
    tmpItems := TLanguageItemList.Create;
{
    tmpItems.setValue('label1', 'value1');
    tmpItems.setValue('Label2', 'vaLue2');

    tmpFound := tmpItems.getValue('label1', 'default');
    assertEqualsString('testTLanguageItemList_ManyValues label1', 'value1', tmpFound);

    tmpFound := tmpItems.getValue('LABEL1', 'default');
    assertEqualsString('testTLanguageItemList_ManyValues LABEL1', 'value1', tmpFound);

    tmpFound := tmpItems.getValue('LaBel1', 'default');
    assertEqualsString('testTLanguageItemList_ManyValues LaBel1', 'value1', tmpFound);

    tmpFound := tmpItems.getValue('label2', 'default');
    assertEqualsString('testTLanguageItemList_ManyValues label2', 'vaLue2', tmpFound);

    tmpFound := tmpItems.getValue('LABEL2', 'default');
    assertEqualsString('testTLanguageItemList_ManyValues LABEL2', 'vaLue2', tmpFound);

    tmpFound := tmpItems.getValue('LaBel2', 'default');
    assertEqualsString('testTLanguageItemList_ManyValues LaBel2', 'vaLue2', tmpFound);
}
    tmpFound := tmpItems.getValue('unknown', 'default');
    assertEqualsString('testTLanguageItemList_ManyValues unknown', '', tmpFound);

    tmpFound := tmpItems.getValue('unknown', 'default');
    assertEqualsString('testTLanguageItemList_ManyValues unknown (2)', '', tmpFound);

    tmpItems.Destroy;
  END;


  // ----------------------------------------------------------


  FUNCTION getACLLanguageUnitTests : TList;
  BEGIN
    result := TList.Create;

    result.add(@testTLanguageItemList_ConstructorDestructor);
    result.add(@testTLanguageItemList_Empty_NotFound);
    result.add(@testTLanguageItemList_OneValue_NotFound);
    result.add(@testTLanguageItemList_OneValue);
    result.add(@testTLanguageItemList_ManyValues);
  END;

END.