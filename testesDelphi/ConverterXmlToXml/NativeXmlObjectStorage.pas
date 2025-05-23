﻿{ unit NativeXmlObjectStorage

  This unit provides functionality to store any TObject descendant to an XML file
  or stream. Internally it makes full use of RTTI (runtime type information) in
  order to store all published properties and events.

  It can even be used to copy forms, but form inheritance is not exploited, so
  child forms descending from parent forms store everything that the parent already
  stored.

  All published properties and events of objects are stored. This includes
  the "DefineProperties". These are stored in binary form in the XML, encoded
  as BASE64.

  Known limitations:
  - The method and event lookup will not work correctly across forms.

  Please see the "ObjectToXML" demo for example usage of this unit.

  Copyright (c) 2004 - 2006 Simdesign B.V., Author Nils Haeck M.Sc.

  It is NOT allowed under ANY circumstances to publish or copy this code
  without prior written permission of the Author!

  This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied.

  Please visit http://www.simdesign.nl/xml.html for more information.
}

// Delphi and BCB versions

// Delphi 3
{$IFDEF VER110}
  {$DEFINE D3UP}
{$ENDIF}
// Delphi 4
{$IFDEF VER120}
  {$DEFINE D3UP}
  {$DEFINE D4UP}
{$ENDIF}
// BCB 4
{$IFDEF VER125}
  {$DEFINE D4UP}
{$ENDIF}
// Delphi 5
{$IFDEF VER130}
  {$DEFINE D3UP}
  {$DEFINE D4UP}
  {$DEFINE D5UP}
{$ENDIF}
//Delphi 6
{$IFDEF VER140}
  {$DEFINE D3UP}
  {$DEFINE D4UP}
  {$DEFINE D5UP}
  {$DEFINE D6UP}
{$ENDIF}
//Delphi 7
{$IFDEF VER150}
  {$DEFINE D3UP}
  {$DEFINE D4UP}
  {$DEFINE D5UP}
  {$DEFINE D6UP}
  {$DEFINE D7UP}
{$ENDIF}
//Delphi 8
{$IFDEF VER160}
  {$DEFINE D3UP}
  {$DEFINE D4UP}
  {$DEFINE D5UP}
  {$DEFINE D6UP}
  {$DEFINE D7UP}
  {$DEFINE D8UP}
{$ENDIF}
// Delphi 2005
{$IFDEF VER170}
  {$DEFINE D3UP}
  {$DEFINE D4UP}
  {$DEFINE D5UP}
  {$DEFINE D6UP}
  {$DEFINE D7UP}
  {$DEFINE D8UP}
  {$DEFINE D9UP}
{$ENDIF}
// above Delphi 2005
{$IFDEF VER180}
  {$DEFINE D3UP}
  {$DEFINE D4UP}
  {$DEFINE D5UP}
  {$DEFINE D6UP}
  {$DEFINE D7UP}
  {$DEFINE D8UP}
  {$DEFINE D9UP}
  {$DEFINE D10UP}
{$ENDIF}


unit NativeXmlObjectStorage;

interface

uses
  Classes, Forms, SysUtils, Controls, NativeXml, TypInfo, RTLConsts
  {$IFDEF D6UP}
  , Variants
  {$ENDIF};

type

  // Use TsdXmlObjectWriter to write any TPersistent descendant's published properties
  // to an XML node.
  TsdXmlObjectWriter = class(TPersistent)
  protected
    procedure WriteProperty(ANode: TXmlNode; AObject: TObject; AParent: TComponent; PropInfo: PPropInfo);
  public
    // Call WriteObject to write the published properties of AObject to the TXmlNode
    // ANode. Specify AParent in order to store references to parent methods and
    // events correctly.
    procedure WriteObject(ANode: TXmlNode; AObject: TObject; AParent: TComponent = nil);
    // Call WriteComponent to write the published properties of AComponent to the TXmlNode
    // ANode. Specify AParent in order to store references to parent methods and
    // events correctly.
    procedure WriteComponent(ANode: TXmlNode; AComponent: TComponent; AParent: TComponent = nil);
  end;

  // Use TsdXmlObjectReader to read any TPersistent descendant's published properties
  // from an XML node.
  TsdXmlObjectReader = class(TPersistent)
  protected
    procedure ReadProperty(ANode: TXmlNode; AObject: TObject; AParent: TComponent; PropInfo: PPropInfo);
  public
    // Call CreateComponent to first create AComponent and then read its published
    // properties from the TXmlNode ANode. Specify AParent in order to resolve
    // references to parent methods and events correctly. In order to successfully
    // create the component from scratch, the component's class must be registered
    // beforehand with a call to RegisterClass. Specify Owner to add the component
    // as a child to Owner's component list. This is usually a form. Specify Name
    // as the new component name for the created component.
    function CreateComponent(ANode: TXmlNode; AOwner, AParent: TComponent; AName: string = ''): TComponent;
    // Call ReadObject to read the published properties of AObject from the TXmlNode
    // ANode. Specify AParent in order to resolve references to parent methods and
    // events correctly.
    procedure ReadObject(ANode: TXmlNode; AObject: TObject; AParent: TComponent = nil);
    // Call ReadComponent to read the published properties of AComponent from the TXmlNode
    // ANode. Specify AParent in order to resolve references to parent methods and
    // events correctly.
    procedure ReadComponent(ANode: TXmlNode; AComponent: TComponent; AParent: TComponent);
  end;

// High-level create methods

// Create and read a component from the XML file with FileName. In order to successfully
// create the component from scratch, the component's class must be registered
// beforehand with a call to RegisterClass. Specify Owner to add the component
// as a child to Owner's component list. This is usually a form. Specify Name
// as the new component name for the created component.
function ComponentCreateFromXmlFile(const FileName: string; Owner: TComponent;
  const Name: string): TComponent;

// Create and read a component from the TXmlNode ANode. In order to successfully
// create the component from scratch, the component's class must be registered
// beforehand with a call to RegisterClass. Specify Owner to add the component
// as a child to Owner's component list. This is usually a form. Specify Name
// as the new component name for the created component.
function ComponentCreateFromXmlNode(ANode: TXmlNode; Owner: TComponent;
  const Name: string): TComponent;

// Create and read a component from the XML stream S. In order to successfully
// create the component from scratch, the component's class must be registered
// beforehand with a call to RegisterClass. Specify Owner to add the component
// as a child to Owner's component list. This is usually a form. Specify Name
// as the new component name for the created component.
function ComponentCreateFromXmlStream(S: TStream; Owner: TComponent;
  const Name: string): TComponent;

// Create and read a component from the XML in string in Value. In order to successfully
// create the component from scratch, the component's class must be registered
// beforehand with a call to RegisterClass. Specify Owner to add the component
// as a child to Owner's component list. This is usually a form. Specify Name
// as the new component name for the created component.
function ComponentCreateFromXmlString(const Value: string; Owner: TComponent;
  const Name: string): TComponent;

// Create and read a form from the XML file with FileName. In order to successfully
// create the form from scratch, the form's class must be registered
// beforehand with a call to RegisterClass. Specify Owner to add the form
// as a child to Owner's component list. For forms this is usually Application.
// Specify Name as the new form name for the created form.
function FormCreateFromXmlFile(const FileName: string; Owner: TComponent;
  const Name: string): TForm;

// Create and read a form from the XML stream in S. In order to successfully
// create the form from scratch, the form's class must be registered
// beforehand with a call to RegisterClass. Specify Owner to add the form
// as a child to Owner's component list. For forms this is usually Application.
// Specify Name as the new form name for the created form.
function FormCreateFromXmlStream(S: TStream; Owner: TComponent;
  const Name: string): TForm;

// Create and read a form from the XML string in Value. In order to successfully
// create the form from scratch, the form's class must be registered
// beforehand with a call to RegisterClass. Specify Owner to add the form
// as a child to Owner's component list. For forms this is usually Application.
// Specify Name as the new form name for the created form.
function FormCreateFromXmlString(const Value: string; Owner: TComponent;
  const Name: string): TForm;

// High-level load methods

// Load all the published properties of AObject from the XML file in Filename.
// Specify AParent in order to resolve references to parent methods and
// events correctly.
procedure ObjectLoadFromXmlFile(AObject: TObject; const FileName: string;
  AParent: TComponent = nil);

// Load all the published properties of AObject from the TXmlNode ANode.
// Specify AParent in order to resolve references to parent methods and
// events correctly.
procedure ObjectLoadFromXmlNode(AObject: TObject; ANode: TXmlNode; AParent: TComponent = nil);

// Load all the published properties of AObject from the XML stream in S.
// Specify AParent in order to resolve references to parent methods and
// events correctly.
procedure ObjectLoadFromXmlStream(AObject: TObject; S: TStream; AParent: TComponent = nil);

// Load all the published properties of AObject from the XML string in Value.
// Specify AParent in order to resolve references to parent methods and
// events correctly.
procedure ObjectLoadFromXmlString(AObject: TObject; const Value: string; AParent: TComponent = nil);

// High-level save methods

// Save all the published properties of AObject as XML to the file in Filename.
// Specify AParent in order to store references to parent methods and
// events correctly.
procedure ObjectSaveToXmlFile(AObject: TObject; const FileName: string;
  AParent: TComponent = nil);

// Save all the published properties of AObject to the TXmlNode ANode.
// Specify AParent in order to store references to parent methods and
// events correctly.
procedure ObjectSaveToXmlNode(AObject: TObject; ANode: TXmlNode; AParent: TComponent = nil);

// Save all the published properties of AObject as XML in stream S.
// Specify AParent in order to store references to parent methods and
// events correctly.
procedure ObjectSaveToXmlStream(AObject: TObject; S: TStream; AParent: TComponent = nil);

// Save all the published properties of AObject as XML in string Value.
// Specify AParent in order to store references to parent methods and
// events correctly.
function ObjectSaveToXmlString(AObject: TObject; AParent: TComponent = nil): string;

// Save all the published properties of AComponent as XML in the file in Filename.
// Specify AParent in order to store references to parent methods and
// events correctly.
procedure ComponentSaveToXmlFile(AComponent: TComponent; const FileName: string;
  AParent: TComponent = nil);

// Save all the published properties of AComponent to the TXmlNode ANode.
// Specify AParent in order to store references to parent methods and
// events correctly.
procedure ComponentSaveToXmlNode(AComponent: TComponent; ANode: TXmlNode;
  AParent: TComponent = nil);

// Save all the published properties of AComponent as XML in the stream in S.
// Specify AParent in order to store references to parent methods and
// events correctly.
procedure ComponentSaveToXmlStream(AComponent: TComponent; S: TStream;
  AParent: TComponent = nil);

// Save all the published properties of AComponent as XML in the string Value.
// Specify AParent in order to store references to parent methods and
// events correctly.
function ComponentSaveToXmlString(AComponent: TComponent; AParent: TComponent = nil): string;

// Save the form AForm as XML to the file in Filename. This method also stores
// properties of all child components on the form, and can therefore be used
// as a form-storage method.
procedure FormSaveToXmlFile(AForm: TForm; const FileName: string);

// Save the form AForm as XML to the stream in S. This method also stores
// properties of all child components on the form, and can therefore be used
// as a form-storage method.
procedure FormSaveToXmlStream(AForm: TForm; S: TStream);

// Save the form AForm as XML to a string. This method also stores
// properties of all child components on the form, and can therefore be used
// as a form-storage method.
function FormSaveToXmlString(AForm: TForm): string;

resourcestring
  sxwIllegalVarType        = 'Illegal variant type';
  sxrInvalidPropertyValue  = 'Invalid property value';
  sxwInvalidMethodName     = 'Invalid method name';

implementation

{$IFDEF TRIALXML}
uses
  Dialogs;
{$ENDIF}

type

  THackPersistent = class(TPersistent);
  THackComponent = class(TComponent)
  public
    procedure SetComponentState(const AState: TComponentState);
  published
    property ComponentState;
  end;

  THackReader = class(TReader);

function ComponentCreateFromXmlFile(const FileName: string; Owner: TComponent;
  const Name: string): TComponent;
var
  S: TStream;
begin
  S := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := ComponentCreateFromXmlStream(S, Owner, Name);
  finally
    S.Free;
  end;
end;

function ComponentCreateFromXmlNode(ANode: TXmlNode; Owner: TComponent;
  const Name: string): TComponent;
var
  AReader: TsdXmlObjectReader;
begin
  Result := nil;
  if not Assigned(ANode) then exit;
  // Create reader
  AReader := TsdXmlObjectReader.Create;
  try
    // Read the component from the node
    Result := AReader.CreateComponent(ANode, Owner, nil, Name);
  finally
    AReader.Free;
  end;
end;

function ComponentCreateFromXmlStream(S: TStream; Owner: TComponent;
  const Name: string): TComponent;
var
  ADoc: TNativeXml;
begin
  Result := nil;
  if not Assigned(S) then exit;
  // Create XML document
  ADoc := TNativeXml.Create;
  try
    // Load XML
    ADoc.LoadFromStream(S);
    // Load from XML node
    Result := ComponentCreateFromXmlNode(ADoc.Root, Owner, Name);
  finally
    ADoc.Free;
  end;
end;

function ComponentCreateFromXmlString(const Value: string; Owner: TComponent;
  const Name: string): TComponent;
var
  S: TStream;
begin
  S := TStringStream.Create(Value);
  try
    Result := ComponentCreateFromXmlStream(S, Owner, Name);
  finally
    S.Free;
  end;
end;

function FormCreateFromXmlFile(const FileName: string; Owner: TComponent;
  const Name: string): TForm;
var
  S: TStream;
begin
  S := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := FormCreateFromXmlStream(S, Owner, Name);
  finally
    S.Free;
  end;
end;

function FormCreateFromXmlStream(S: TStream; Owner: TComponent;
  const Name: string): TForm;
var
  ADoc: TNativeXml;
begin
  Result := nil;
  if not Assigned(S) then exit;
  // Create XML document
  ADoc := TNativeXml.Create;
  try
    // Load XML
    ADoc.LoadFromStream(S);

    // Load from XML node
    Result := TForm(ComponentCreateFromXmlNode(ADoc.Root, Owner, Name));
  finally
    ADoc.Free;
  end;
end;

function FormCreateFromXmlString(const Value: string; Owner: TComponent;
  const Name: string): TForm;
var
  S: TStream;
begin
  S := TStringStream.Create(Value);
  try
    Result := FormCreateFromXmlStream(S, Owner, Name);
  finally
    S.Free;
  end;
end;

procedure ObjectLoadFromXmlFile(AObject: TObject; const FileName: string;
  AParent: TComponent = nil);
var
  S: TStream;
begin
  S := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    ObjectLoadFromXmlStream(AObject, S, AParent);
  finally
    S.Free;
  end;
end;

procedure ObjectLoadFromXmlNode(AObject: TObject; ANode: TXmlNode; AParent: TComponent = nil);
var
  AReader: TsdXmlObjectReader;
begin
  if not Assigned(AObject) or not Assigned(ANode) then exit;
  // Create writer
  AReader := TsdXmlObjectReader.Create;
  try
    // Write the object to the document
    if AObject is TComponent then
      AReader.ReadComponent(ANode, TComponent(AObject), AParent)
    else
      AReader.ReadObject(ANode, AObject, AParent);
  finally
    AReader.Free;
  end;
end;

procedure ObjectLoadFromXmlStream(AObject: TObject; S: TStream; AParent: TComponent = nil);
var
  ADoc: TNativeXml;
begin
  if not Assigned(S) then exit;
  // Create XML document
  ADoc := TNativeXml.Create;
  try
    // Load XML
    ADoc.LoadFromStream(S);
    // Load from XML node
    ObjectLoadFromXmlNode(AObject, ADoc.Root, AParent);
  finally
    ADoc.Free;
  end;
end;

procedure ObjectLoadFromXmlString(AObject: TObject; const Value: string; AParent: TComponent = nil);
var
  S: TStringStream;
begin
  S := TStringStream.Create(Value);
  try
    ObjectLoadFromXmlStream(AObject, S, AParent);
  finally
    S.Free;
  end;
end;

procedure ObjectSaveToXmlFile(AObject: TObject; const FileName: string;
  AParent: TComponent = nil);
var
  S: TStream;
begin
  S := TFileStream.Create(FileName, fmCreate);
  try
    ObjectSaveToXmlStream(AObject, S, AParent);
  finally
    S.Free;
  end;
end;

procedure ObjectSaveToXmlNode(AObject: TObject; ANode: TXmlNode; AParent: TComponent = nil);
var
  AWriter: TsdXmlObjectWriter;
begin
  if not Assigned(AObject) or not Assigned(ANode) then exit;
  // Create writer
  AWriter := TsdXmlObjectWriter.Create;
  try
    // Write the object to the document
    if AObject is TComponent then
      AWriter.WriteComponent(ANode, TComponent(AObject), AParent)
    else begin
      ANode.Name := AObject.ClassName;
      AWriter.WriteObject(ANode, AObject, AParent);
    end;
  finally
    AWriter.Free;
  end;
end;

procedure ObjectSaveToXmlStream(AObject: TObject; S: TStream; AParent: TComponent = nil);
var
  ADoc: TNativeXml;
begin
  if not Assigned(S) then exit;
  // Create XML document
  ADoc := TNativeXml.Create;
  try
    ADoc.Utf8Encoded := True;
    ADoc.EncodingString := 'UTF-8';
    ADoc.ExternalEncoding := seUTF8;
    ADoc.XmlFormat := xfReadable;
    // Save to XML node
    ObjectSaveToXmlNode(AObject, ADoc.Root, AParent);
    // Save to stream
    ADoc.SaveToStream(S);
  finally
    ADoc.Free;
  end;
end;

function ObjectSaveToXmlString(AObject: TObject; AParent: TComponent = nil): string;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    ObjectSaveToXmlStream(AObject, S, AParent);
    Result := S.DataString;
  finally
    S.Free;
  end;
end;

procedure ComponentSaveToXmlFile(AComponent: TComponent; const FileName: string;
  AParent: TComponent = nil);
begin
  ObjectSaveToXmlFile(AComponent, FileName, AParent);
end;

procedure ComponentSaveToXmlNode(AComponent: TComponent; ANode: TXmlNode;
  AParent: TComponent = nil);
begin
  ObjectSaveToXmlNode(AComponent, ANode, AParent);
end;

procedure ComponentSaveToXmlStream(AComponent: TComponent; S: TStream;
  AParent: TComponent = nil);
begin
  ObjectSaveToXmlStream(AComponent, S, AParent);
end;

function ComponentSaveToXmlString(AComponent: TComponent; AParent: TComponent = nil): string;
begin
  Result := ObjectSaveToXmlString(AComponent, AParent);
end;

procedure FormSaveToXmlFile(AForm: TForm; const FileName: string);
begin
  ComponentSaveToXmlFile(AForm, FileName, AForm);
end;

procedure FormSaveToXmlStream(AForm: TForm; S: TStream);
begin
  ComponentSaveToXmlStream(AForm, S, AForm);
end;

function FormSaveToXmlString(AForm: TForm): string;
begin
  Result := ComponentSaveToXmlString(AForm, AForm);
end;


{ TsdXmlObjectWriter }

procedure TsdXmlObjectWriter.WriteComponent(ANode: TXmlNode; AComponent,
  AParent: TComponent);
begin
  if not Assigned(ANode) or not Assigned(AComponent) then exit;
  ANode.Name := AComponent.ClassName;
  if Length(AComponent.Name) > 0 then
    ANode.AttributeAdd('Name', AComponent.Name);
  WriteObject(ANode, AComponent, AParent);
end;

procedure TsdXmlObjectWriter.WriteObject(ANode: TXmlNode; AObject: TObject;
  AParent: TComponent);
var
  i, Count: Integer;
  PropInfo: PPropInfo;
  PropList: PPropList;
  S: TStringStream;
  AWriter: TWriter;
  AChildNode: TXmlNode;
  AComponentNode: TXmlNode;
begin
  if not Assigned(ANode) or not Assigned(AObject) then exit;

  // If this is a component, store child components
  if AObject is TComponent then with TComponent(AObject) do begin
    if ComponentCount > 0 then begin
      AChildNode := ANode.NodeNew('Components');
      for i := 0 to ComponentCount - 1 do begin
        AComponentNode := AChildNode.NodeNew(Components[i].ClassName);
        if Length(Components[i].Name) > 0 then
          AComponentNode.AttributeAdd('Name', Components[i].Name);
        WriteObject(AComponentNode, Components[i], TComponent(AObject));
      end;
    end;
  end;

  // Save all regular properties that need storing
  Count := GetTypeData(AObject.ClassInfo)^.PropCount;
  if Count > 0 then begin
    GetMem(PropList, Count * SizeOf(Pointer));
    try
      GetPropInfos(AObject.ClassInfo, PropList);
      for i := 0 to Count - 1 do begin
        PropInfo := PropList^[i];
        if PropInfo = nil then continue;
        if IsStoredProp(AObject, PropInfo) then
          WriteProperty(ANode, AObject, AParent, PropInfo);
      end;
    finally
      FreeMem(PropList, Count * SizeOf(Pointer));
    end;
  end;

  // Save defined properties
  if AObject is TPersistent then begin
    S := TStringStream.Create('');
    try
      AWriter := TWriter.Create(S, 4096);
      try
        THackPersistent(AObject).DefineProperties(AWriter);
      finally
        AWriter.Free;
      end;
      // Do we have data from DefineProperties?
      if S.Size > 0 then begin
        // Yes, add a node with binary data
        ANode.NodeNew('DefinedProperties').BinaryString := S.DataString;
      end;
    finally
      S.Free;
    end;
  end;
end;

procedure TsdXmlObjectWriter.WriteProperty(ANode: TXmlNode; AObject: TObject;
  AParent: TComponent; PropInfo: PPropInfo);
var
  PropType: PTypeInfo;
  AChildNode: TXmlNode;
  ACollectionNode: TXmlNode;

  procedure WritePropName;
  begin
    AChildNode := ANode.NodeNew(PPropInfo(PropInfo)^.Name);
  end;

  procedure WriteInteger(Value: Int64);
  begin
    AChildNode.ValueAsString := IntToStr(Value);
  end;

  procedure WriteString(Value: string);
  begin
    AChildNode.ValueAsString := Value;
  end;

  procedure WriteSet(Value: Longint);
  var
    I: Integer;
    BaseType: PTypeInfo;
    S, Enum: string;
  begin
    BaseType := GetTypeData(PropType)^.CompType^;
    for i := 0 to SizeOf(TIntegerSet) * 8 - 1 do begin
      if i in TIntegerSet(Value) then begin
        Enum := GetEnumName(BaseType, i);
        if i > 0 then
          S := S + ',' + Enum
        else
          S := Enum;
      end;
    end;
    AChildNode.ValueAsString := Format('[%s]', [S]);
  end;

  procedure WriteIntProp(IntType: PTypeInfo; Value: Longint);
  var
    Ident: string;
    IntToIdent: TIntToIdent;
  begin
    IntToIdent := FindIntToIdent(IntType);
    if Assigned(IntToIdent) and IntToIdent(Value, Ident) then
      WriteString(Ident)
    else
      WriteInteger(Value);
  end;

  procedure WriteCollectionProp(Collection: TCollection);
  var
    i: integer;
  begin
    if Assigned(Collection) then begin
      for i := 0 to Collection.Count - 1 do
      begin
        ACollectionNode := AChildNode.NodeNew(Collection.Items[i].ClassName);
        WriteObject(ACollectionNode, Collection.Items[I], AParent);
      end;
    end;
  end;

  procedure WriteOrdProp;
  var
    Value: Longint;
  begin
    Value := GetOrdProp(AObject, PropInfo);
    if not (Value = PPropInfo(PropInfo)^.Default) then begin
      WritePropName;
      case PropType^.Kind of
      tkInteger:     WriteIntProp(PPropInfo(PropInfo)^.PropType^, Value);
      tkChar:        WriteString(Chr(Value));
      tkSet:         WriteSet(Value);
      tkEnumeration: WriteString(GetEnumName(PropType, Value));
      end;
    end;
  end;

  procedure WriteFloatProp;
  var
    Value: Extended;
  begin
    Value := GetFloatProp(AObject, PropInfo);
    if not (Value = 0) then
      ANode.WriteFloat(PPropInfo(PropInfo)^.Name, Value);
  end;

  procedure WriteInt64Prop;
  var
    Value: Int64;
  begin
    Value := GetInt64Prop(AObject, PropInfo);
    if not (Value = 0) then
      ANode.WriteInt64(PPropInfo(PropInfo)^.Name, Value);
  end;

  procedure WriteStrProp;
  var
    Value: string;
  begin
    Value := GetStrProp(AObject, PropInfo);
    if not (Length(Value) = 0) then
      ANode.WriteString(PPropInfo(PropInfo)^.Name, Value);
  end;

  {$IFDEF D6UP}
  procedure WriteWideStrProp;
  var
    Value: WideString;
  begin
    Value := GetWideStrProp(AObject, PropInfo);
    if not (Length(Value) = 0) then
      ANode.WriteWidestring(PPropInfo(PropInfo)^.Name, Value);
  end;
  {$ENDIF}

  procedure WriteObjectProp;
  var
    Value: TObject;
    ComponentName: string;
    function GetComponentName(Component: TComponent): string;
    begin
      if Component.Owner = AParent then
        Result := Component.Name
      else if Component = AParent then
        Result := 'Owner'
      else if Assigned(Component.Owner) and (Length(Component.Owner.Name) > 0)
        and (Length(Component.Name) > 0) then
        Result := Component.Owner.Name + '.' + Component.Name
      else if Length(Component.Name) > 0 then
        Result := Component.Name + '.Owner'
      else Result := '';
    end;

  begin
    Value := TObject(GetOrdProp(AObject, PropInfo));
    if not Assigned(Value) then exit;
    WritePropName;
    if Value is TComponent then begin
      ComponentName := GetComponentName(TComponent(Value));
      if Length(ComponentName) > 0 then
        WriteString(ComponentName);
    end else begin
      WriteString(Format('(%s)', [Value.ClassName]));
      if Value is TCollection then
        WriteCollectionProp(TCollection(Value))
      else begin
        if AObject is TComponent then
          WriteObject(AChildNode, Value, TComponent(AObject))
        else
          WriteObject(AChildNode, Value, AParent)
      end;
      // No need to store an empty child.. so check and remove
      if AChildNode.NodeCount = 0 then
        ANode.NodeRemove(AChildNode);
    end;
  end;

  procedure WriteMethodProp;
  var
    Value: TMethod;
    function IsDefaultValue: Boolean;
    begin
      Result := (Value.Code = nil) or
        ((Value.Code <> nil) and Assigned(AParent) and (AParent.MethodName(Value.Code) = ''));
    end;
  begin
    Value := GetMethodProp(AObject, PropInfo);
    if not IsDefaultValue then begin
      if Assigned(Value.Code) then begin
        WritePropName;
        if Assigned(AParent) then
          WriteString(AParent.MethodName(Value.Code))
        else
          AChildNode.ValueAsString := '???';
      end;
    end;
  end;

  procedure WriteVariantProp;
  var
    AValue: Variant;
    ACurrency: Currency;
  var
    VType: Integer;
  begin
    AValue := GetVariantProp(AObject, PropInfo);
    if not VarIsEmpty(AValue) then begin
      if VarIsArray(AValue) then
        raise Exception.Create(sxwIllegalVarType);
      WritePropName;
      VType := VarType(AValue);
      AChildNode.AttributeAdd('VarType', IntToHex(VType, 4));
      case VType and varTypeMask of
      varOleStr:  AChildNode.ValueAsWideString := AValue;
      varString:  AChildNode.ValueAsString := AValue;
      varByte,
      varSmallInt,
      varInteger: AChildNode.ValueAsInteger := AValue;
      varSingle,
      varDouble:  AChildNode.ValueAsFloat := AValue;
      varCurrency:
        begin
          ACurrency := AValue;
          AChildNode.BufferWrite(ACurrency, SizeOf(ACurrency));
        end;
      varDate:    AChildNode.ValueAsDateTime := AValue;
      varBoolean: AChildNode.ValueAsBool := AValue;
      else
        try
          ANode.ValueAsString := AValue;
        except
          raise Exception.Create(sxwIllegalVarType);
        end;
      end;//case
    end;
  end;

begin
  if {(PPropInfo(PropInfo)^.SetProc <> nil) and}
    (PPropInfo(PropInfo)^.GetProc <> nil) then
  begin
    PropType := PPropInfo(PropInfo)^.PropType^;
    case PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet: WriteOrdProp;
    tkFloat:                                 WriteFloatProp;
    tkString, tkLString:                     WriteStrProp;
    {$IFDEF D6UP}
    tkWString:                               WriteWideStrProp;
    {$ENDIF}
    tkClass:                                 WriteObjectProp;
    tkMethod:                                WriteMethodProp;
    tkVariant:                               WriteVariantProp;
    tkInt64:                                 WriteInt64Prop;
    end;
  end;
end;

{ TsdXmlObjectReader }

function TsdXmlObjectReader.CreateComponent(ANode: TXmlNode;
  AOwner, AParent: TComponent; AName: string): TComponent;
var
  AClass: TComponentClass;
begin
  AClass := TComponentClass(GetClass(Trim(ANode.Name)));
  if not Assigned(AClass) then
    raise Exception.CreateFmt(SClassNotFound, [Trim(ANode.Name)]);
  Result := AClass.Create(AOwner);
  if Length(AName) = 0 then
    Result.Name := ANode.AttributeByName['Name']
  else
    Result.Name := AName;
  if not Assigned(AParent) then
    AParent := Result;
  ReadComponent(ANode, Result, AParent);
end;

procedure TsdXmlObjectReader.ReadComponent(ANode: TXmlNode; AComponent,
  AParent: TComponent);
begin
  ReadObject(ANode, AComponent, AParent);
end;

procedure TsdXmlObjectReader.ReadObject(ANode: TXmlNode; AObject: TObject; AParent: TComponent);
var
  i, Count: Integer;
  PropInfo: PPropInfo;
  PropList: PPropList;
  S: TStringStream;
  AReader: TReader;
  AChildNode: TXmlNode;
  AComponentNode: TXmlNode;
  AClass: TComponentClass;
  AComponent: TComponent;
begin
  if not Assigned(ANode) or not Assigned(AObject) then exit;

  // Start loading
  if AObject is TComponent then with THackComponent(AObject) do begin
    THackComponent(AObject).Updating;
    SetComponentState(ComponentState + [csLoading, csReading]);
  end;
  try

    // If this is a component, load child components
    if AObject is TComponent then with TComponent(AObject) do begin
      AChildNode := ANode.NodeByName('Components');
      if Assigned(AChildNode) then begin
        for i := 0 to AChildNode.NodeCount - 1 do begin
          AComponentNode := AChildNode.Nodes[i];
          AComponent := FindComponent(AComponentNode.AttributeByName['Name']);
          if not Assigned(AComponent) then begin
            AClass := TComponentClass(GetClass(Trim(AComponentNode.Name)));
            if not Assigned(AClass) then
              raise Exception.CreateFmt(SClassNotFound, [Trim(AComponentNode.Name)]);
            AComponent := AClass.Create(TComponent(AObject));
            AComponent.Name := AComponentNode.AttributeByName['Name'];
            // In case of new (visual) controls we set the parent
            if (AComponent is TControl) and (AObject is TWinControl) then
              TControl(AComponent).Parent := TWinControl(AObject);
          end;
          ReadComponent(AComponentNode, AComponent, TComponent(AObject));
        end;
      end;
    end;

    // Load all loadable regular properties
    Count := GetTypeData(AObject.ClassInfo)^.PropCount;
    if Count > 0 then begin
      GetMem(PropList, Count * SizeOf(Pointer));
      try
        GetPropInfos(AObject.ClassInfo, PropList);
        for i := 0 to Count - 1 do begin
          PropInfo := PropList^[i];
          if PropInfo = nil then continue;
          if IsStoredProp(AObject, PropInfo) then
            ReadProperty(ANode, AObject, AParent, PropInfo);
        end;
      finally
        FreeMem(PropList, Count * SizeOf(Pointer));
      end;
    end;

    // Load defined properties
    if AObject is TPersistent then begin
      AChildNode := ANode.NodeByName('DefinedProperties');
      if Assigned(AChildNode) then begin
        S := TStringStream.Create(AChildNode.BinaryString);
        try
          AReader := TReader.Create(S, 4096);
          try
            THackReader(AReader).ReadProperty(TPersistent(AObject));
          finally
            AReader.Free;
          end;
        finally
          S.Free;
        end;
      end;
    end;

  finally
    // End loading
    if AObject is TComponent then with THackComponent(AObject) do begin
      SetComponentState(ComponentState - [csReading]);
      THackComponent(AObject).Loaded;
      THackComponent(AObject).Updated;
    end;
  end;
end;

procedure TsdXmlObjectReader.ReadProperty(ANode: TXmlNode;
  AObject: TObject; AParent: TComponent; PropInfo: PPropInfo);
var
  PropType: PTypeInfo;
  AChildNode: TXmlNode;
  Method: TMethod;
  PropObject: TObject;

  procedure SetSetProp(const AValue: string);
  var
    S: string;
    P: integer;
    ASet: integer;
    EnumType: PTypeInfo;

    procedure AddToEnum(const EnumName: string);
    var
      V: integer;
    begin
      if Length(EnumName) = 0 then exit;
      V := GetEnumValue(EnumType, EnumName);
      if V = -1 then
        raise Exception.Create(sxrInvalidPropertyValue);
      Include(TIntegerSet(ASet), V);
    end;
  begin
    ASet := 0;
    EnumType := GetTypeData(PropType)^.CompType^;
    S := copy(AValue, 2, Length(AValue) - 2);
    repeat
      P := Pos(',', S);
      if P > 0 then begin
        AddToEnum(copy(S, 1, P - 1));
        S := copy(S, P + 1, Length(S));
      end else begin
        AddToEnum(S);
        break;
      end;
    until False;
    SetOrdProp(AObject, PropInfo, ASet);
  end;

  procedure SetIntProp(const AValue: string);
  var
    V: Longint;
    IdentToInt: TIdentToInt;
  begin
    IdentToInt := FindIdentToInt(PropType);
    if Assigned(IdentToInt) and IdentToInt(AValue, V) then
      SetOrdProp(AObject, PropInfo, V)
    else
      SetOrdProp(AObject, PropInfo, StrToInt(AValue));
  end;

  procedure SetCharProp(const AValue: string);
  begin
    if Length(AValue) <> 1 then
      raise Exception.Create(sxrInvalidPropertyValue);
    SetOrdProp(AObject, PropInfo, Ord(AValue[1]));
  end;

  procedure SetEnumProp(const AValue: string);
  var
    V: integer;
  begin
    V := GetEnumValue(PropType, AValue);
    if V = -1 then
      raise Exception.Create(sxrInvalidPropertyValue);
    SetOrdProp(AObject, PropInfo, V)
  end;

  procedure ReadCollectionProp(ACollection: TCollection);
  var
    i: integer;
    Item: TPersistent;
  begin
    ACollection.BeginUpdate;
    try
      ACollection.Clear;
      for i := 0 to AChildNode.NodeCount - 1 do begin
        Item := ACollection.Add;
        ReadObject(AChildNode.Nodes[i], Item, AParent);
      end;
    finally
      ACollection.EndUpdate;
    end;
  end;

  procedure SetObjectProp(const AValue: string);
  var
    AClassName: string;
    PropObject: TObject;
    Reference: TComponent;
  begin
    if Length(AValue) = 0 then
      Exit;
    if AValue[1] = '(' then begin
      // Persistent class
      AClassName := Trim(Copy(AValue, 2, Length(AValue) - 2));
      AClassName := StringReplace(AClassName, ')', '', [rfReplaceAll]);
      PropObject := TObject(GetOrdProp(AObject, PropInfo));
      if Assigned(PropObject) and SameText(PropObject.ClassName, AClassName) then begin
        if PropObject is TCollection then
          ReadCollectionProp(TCollection(PropObject))
        else begin
          if AObject is TComponent then
            ReadObject(AChildNode, PropObject, TComponent(AObject))
          else
            ReadObject(AChildNode, PropObject, AParent);
        end;
      end else
        raise Exception.CreateFmt(SClassNotFound, [AClassName]);
    end else begin
      // Component reference
      if Assigned(AParent) then begin
        Reference := FindNestedComponent(AParent, AValue);
        SetOrdProp(AObject, PropInfo, Longint(Reference));
      end;
    end;
  end;

  procedure SetMethodProp(const AValue: string);
  var
    Method: TMethod;
  begin
    // to do: add OnFindMethod
    if not Assigned(AParent) then exit;
    Method.Code := AParent.MethodAddress(AValue);
    if not Assigned(Method.Code) then
      raise Exception.Create(sxwInvalidMethodName);
    Method.Data := AParent;
    TypInfo.SetMethodProp(AObject, PropInfo, Method);
  end;

  procedure SetVariantProp(const AValue: string);
  var
    VType: integer;
    Value: Variant;
    ACurrency: Currency;
  begin
    VType := StrToInt('$' + AChildNode.AttributeByName['VarType']);

    case VType and varTypeMask of
    varOleStr:  Value := AChildNode.ValueAsWideString;
    varString:  Value := AChildNode.ValueAsString;
    varByte,
    varSmallInt,
    varInteger: Value := AChildNode.ValueAsInteger;
    varSingle,
    varDouble:  Value := AChildNode.ValueAsFloat;
    varCurrency:
      begin
        AChildNode.BufferWrite(ACurrency, SizeOf(ACurrency));
        Value := ACurrency;
      end;
    varDate:    Value := AChildNode.ValueAsDateTime;
    varBoolean: Value := AChildNode.ValueAsBool;
    else
      try
        Value := ANode.ValueAsString;
      except
        raise Exception.Create(sxwIllegalVarType);
      end;
    end;//case

    TVarData(Value).VType := VType;
    TypInfo.SetVariantProp(AObject, PropInfo, Value);
  end;

begin
  if {(PPropInfo(PropInfo)^.SetProc <> nil) and}
    (PPropInfo(PropInfo)^.GetProc <> nil) then
  begin
    PropType := PPropInfo(PropInfo)^.PropType^;
    AChildNode := ANode.NodeByName(PPropInfo(PropInfo)^.Name);
    if Assigned(AChildNode) then begin
      // Non-default values from XML
      case PropType^.Kind of
      tkInteger:     SetIntProp(AChildNode.ValueAsString);
      tkChar:        SetCharProp(AChildNode.ValueAsString);
      tkSet:         SetSetProp(AChildNode.ValueAsString);
      tkEnumeration: SetEnumProp(AChildNode.ValueAsString);
      tkFloat:       SetFloatProp(AObject, PropInfo, AChildNode.ValueAsFloat);
      tkString,
      tkLString:     SetStrProp(AObject, PropInfo, AChildNode.ValueAsString);
      {$IFDEF D6UP}
      tkWString:     SetWideStrProp(AObject, PropInfo, AChildNode.ValueAsWideString);
      {$ENDIF}
      tkClass:       SetObjectProp(AChildNode.ValueAsString);
      tkMethod:      SetMethodProp(AChildNode.ValueAsString);
      tkVariant:     SetVariantProp(AChildNode.ValueAsString);
      tkInt64:       SetInt64Prop(AObject, PropInfo, AChildNode.ValueAsInt64);
      end;//case
    end else begin
      // Set Default value
      case PropType^.Kind of
      tkInteger:     SetOrdProp(AObject, PropInfo, PPropInfo(PropInfo)^.Default);
      tkChar:        SetOrdProp(AObject, PropInfo, PPropInfo(PropInfo)^.Default);
      tkSet:         SetOrdProp(AObject, PropInfo, PPropInfo(PropInfo)^.Default);
      tkEnumeration: SetOrdProp(AObject, PropInfo, PPropInfo(PropInfo)^.Default);
      tkFloat:       SetFloatProp(AObject, PropInfo, 0);
      tkString,
      tkLString,
      tkWString:     SetStrProp(AObject, PropInfo, '');
      tkClass:
        begin
          PropObject := TObject(GetOrdProp(AObject, PropInfo));
          if PropObject is TComponent then
            SetOrdProp(AObject, PropInfo, 0);
        end;
      tkMethod:
        begin
          Method := TypInfo.GetMethodProp(AObject, PropInfo);
          Method.Code := nil;
          TypInfo.SetMethodProp(AObject, PropInfo, Method);
        end;
      tkInt64:       SetInt64Prop(AObject, PropInfo, 0);
      end;//case
    end;
  end;
end;

{ THackComponent }

procedure THackComponent.SetComponentState(const AState: TComponentState);
type
  PInteger = ^integer;
var
  PSet: PInteger;
  AInfo: PPropInfo;
begin
  // This is a "severe" hack in order to set a non-writable property value,
  // also using RTTI
  PSet := PInteger(@AState);
  AInfo := GetPropInfo(THackComponent, 'ComponentState');
  if Assigned(AInfo.GetProc) then
    PInteger(Integer(Self) + Integer(AInfo.GetProc) and $00FFFFFF)^ := PSet^;
end;

initialization

  {$IFDEF TRIALXML}
  ShowMessage('ObjectToXml demo.'#13#10'For more information please visit:'#13#10 +
    'http://www.simdesign.nl/xml.html');
  {$ENDIF}

end.

