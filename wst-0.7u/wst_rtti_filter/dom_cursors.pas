{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit dom_cursors;

interface

uses
  Classes, SysUtils,
  cursor_intf,
{$IFNDEF FPC}
  xmldom, wst_delphi_xml
{$ELSE}
  DOM, wst_fpc_xml
{$ENDIF}
  ;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

const

  s_LOCAL_NAME = 'LocalName';
  s_NODE_NAME  = 'NodeName';
  s_NODE_VALUE = 'NodeValue';
  s_NS_URI     = 'NamespaceURI';

type

  TFreeAction = ( faNone, faFreeOnDestroy );

  { TDOMNodeListCursor }

  TDOMNodeListCursor = class(
    TInterfacedObject,
    ICursor,
    IDefaultTypedCursor
  )
  private
    FList : TDOMNodeList;
    FCurrent : TDOMNode;
    FFreeListOnDestroy : TFreeAction;
    FHasItem : Boolean;
  protected
    procedure Reset();
    function MoveNext() : Boolean;
    function Clone():ICursor;
    function GetCount() : PtrInt;
    function GetCurrent() : IDefaultItemType;virtual;
  public
    constructor Create(
            ADataList          : TDOMNodeList;
      const AFreeListOnDestroy : TFreeAction
    );
    destructor Destroy();override;
  end;

  { TDOMNamedNodeMapCursor }

  TDOMNamedNodeMapCursor = class(
    TInterfacedObject,
    ICursor,
    IDefaultTypedCursor
  )
  private
    FList : TDOMNamedNodeMap;
    FCurrent : Integer;
    FFreeListOnDestroy : TFreeAction;
  protected
    procedure Reset();
    function MoveNext() : Boolean;
    function Clone():ICursor;
    function GetCount() : PtrInt;
    function GetCurrent() : IDefaultItemType;
  public
    constructor Create(
            ADataList : TDOMNamedNodeMap;
      const AFreeListOnDestroy : TFreeAction
    );
    destructor Destroy();override;
  end;

  { TDOMNodeRttiExposer }

  TDOMNodeRttiExposer = class(TPersistent)
  private
    FInnerObject: TDOMNode;
    function GetLocalName : DOMString;
    function GetNamespaceURI : DOMString;
    function GetNodeName: DOMString;
    function GetNodeValue: DOMString;
    procedure SetInnerObject(const AValue: TDOMNode);
  public
    constructor Create(AInnerObject : TDOMNode);
    property InnerObject : TDOMNode read FInnerObject write SetInnerObject;
  published
    property NodeName: DOMString read GetNodeName;
    property NodeValue: DOMString read GetNodeValue;
    property LocalName : DOMString read GetLocalName;
    property NamespaceURI : DOMString read GetNamespaceURI;
  end;

  { TDOMNodeRttiExposerCursor }

  TDOMNodeRttiExposerCursor = class(
    TInterfacedObject,
    ICursor,
    IObjectCursor
  )
  private
    FCurrentExposer : TDOMNodeRttiExposer;
    FBaseCursor : IDefaultTypedCursor;
  protected
    procedure Reset();
    function MoveNext() : Boolean;
    function Clone():ICursor;
    function GetCount() : PtrInt;
    function GetCurrent() : TObject;virtual;
  public
    constructor Create(ADataList : IDefaultTypedCursor);
    destructor Destroy();override;
  end;

  TCursorExposedType = ( cetRttiNode {$IFDEF WST_INTF_DOM},cetDomNode{$ENDIF} );


  function CreateChildrenCursor(
          ANode : TDOMNode;
    const AExposedType : TCursorExposedType
  ) : IObjectCursor;

  function CreateAttributesCursor(
          ANode : TDOMNode;
    const AExposedType : TCursorExposedType
  ) : IObjectCursor;


implementation
{$IFDEF WST_INTF_DOM}
  function CreateChildrenCursor(
          ANode : TDOMNode;
    const AExposedType : TCursorExposedType
  ) : IObjectCursor;
  var
    locCrs : IInterfaceCursor;
  begin
    Result := nil;
    if ( ANode <> nil ) and ANode.hasChildNodes() then begin
      locCrs := TDOMNodeListCursor.Create(ANode.ChildNodes,faNone) ;
      Result := TDOMNodeRttiExposerCursor.Create(locCrs);
    end;
  end;

  function CreateAttributesCursor(ANode : TDOMNode; const AExposedType : TCursorExposedType):IObjectCursor;
  var
    locCrs : IInterfaceCursor;
  begin
    Result := nil;
    if ( ANode <> nil ) and ( ANode.Attributes <> nil ) and ( ANode.Attributes.Length > 0 ) then begin
      locCrs := TDOMNamedNodeMapCursor.Create(ANode.Attributes,faNone) ;
      Result := TDOMNodeRttiExposerCursor.Create(locCrs);
    end;
  end;
{$ENDIF WST_INTF_DOM}

{$IFNDEF WST_INTF_DOM}
  function CreateChildrenCursor(
          ANode : TDOMNode;
    const AExposedType : TCursorExposedType
  ) : IObjectCursor;
  begin
    Result := nil;
    if ( ANode <> nil ) and ANode.HasChildNodes() then begin
      Result := TDOMNodeListCursor.Create(ANode.ChildNodes,faNone) ;
      if ( AExposedType = cetRttiNode ) then
        Result := TDOMNodeRttiExposerCursor.Create(Result);
    end;
  end;

  function CreateAttributesCursor(ANode : TDOMNode; const AExposedType : TCursorExposedType):IObjectCursor;
  begin
    Result := nil;
    if ( ANode <> nil ) and ( ANode.Attributes <> nil ) and ( ANode.Attributes.Length > 0 ) then begin
      Result := TDOMNamedNodeMapCursor.Create(ANode.Attributes,faNone) ;
      if ( AExposedType = cetRttiNode ) then
        Result := TDOMNodeRttiExposerCursor.Create(Result);
    end;
  end;
{$ENDIF !WST_INTF_DOM}


{ TDOMNodeListCursor }

procedure TDOMNodeListCursor.Reset();
begin
  FCurrent := nil;
end;

function TDOMNodeListCursor.MoveNext(): Boolean;
begin
  if ( FCurrent = nil ) then begin
    if FHasItem then
      FCurrent := FList.Item[0];
  end else begin
    FCurrent := FCurrent.NextSibling;
  end;
  Result := ( FCurrent <> nil ) ;
end;

function TDOMNodeListCursor.Clone(): ICursor;
begin
  Result := TDOMNodeListCursor.Create(FList,faNone);
end;

function TDOMNodeListCursor.GetCount() : PtrInt;
begin
  Result := GetNodeListCount(FList);
end;

function TDOMNodeListCursor.GetCurrent(): IDefaultItemType;
begin
  Result := FCurrent;
end;

constructor TDOMNodeListCursor.Create(
        ADataList          : TDOMNodeList;
  const AFreeListOnDestroy : TFreeAction
);
begin
  Assert(Assigned(ADataList));
  FFreeListOnDestroy := AFreeListOnDestroy;
  FList := ADataList;
  FHasItem := ( GetNodeListCount(FList) > 0 );
  Reset();
end;

destructor TDOMNodeListCursor.Destroy();
begin
  FCurrent := nil;
  if ( FFreeListOnDestroy = faFreeOnDestroy ) then
    ReleaseDomNode(FList)
  else
    FList := nil;
  inherited Destroy();
end;

{ TDOMNodeRttiExposer }

function TDOMNodeRttiExposer.GetNodeName: DOMString;
begin
  Result := InnerObject.NodeName;
end;

function TDOMNodeRttiExposer.GetLocalName : DOMString;
begin
  Result := InnerObject.LocalName;
end;

function TDOMNodeRttiExposer.GetNamespaceURI : DOMString;
begin
  Result := InnerObject.NamespaceURI;
end;

function TDOMNodeRttiExposer.GetNodeValue: DOMString;
begin
  Result := InnerObject.NodeValue;
end;

procedure TDOMNodeRttiExposer.SetInnerObject(const AValue: TDOMNode);
begin
  if ( FInnerObject = AValue ) then
    exit;
  FInnerObject := AValue;
end;

constructor TDOMNodeRttiExposer.Create(AInnerObject: TDOMNode);
begin
  Inherited Create();
  SetInnerObject(AInnerObject);
end;

{ TDOMNodeRttiExposerCursor }

procedure TDOMNodeRttiExposerCursor.Reset();
begin
  FBaseCursor.Reset();
end;

function TDOMNodeRttiExposerCursor.MoveNext(): Boolean;
begin
  Result := FBaseCursor.MoveNext();
end;

function TDOMNodeRttiExposerCursor.Clone(): ICursor;
var
  baseClone : ICursor;
begin
  Result := nil;
  baseClone := FBaseCursor.Clone();
  if ( baseClone <> nil ) then
    Result := TDOMNodeRttiExposerCursor.Create(baseClone as IDefaultTypedCursor) ;
end;

function TDOMNodeRttiExposerCursor.GetCount() : PtrInt;
begin
  Result := FBaseCursor.GetCount();
end;

function TDOMNodeRttiExposerCursor.GetCurrent(): TObject;
begin
  FCurrentExposer.InnerObject := FBaseCursor.GetCurrent() as TDOMNode;
  if ( FCurrentExposer.InnerObject = nil ) then
    Result := nil
  else
    Result := FCurrentExposer;
end;

constructor TDOMNodeRttiExposerCursor.Create(ADataList : IDefaultTypedCursor);
begin
  Assert(Assigned(ADataList));
  inherited Create();
  FBaseCursor := ADataList;
  FCurrentExposer := TDOMNodeRttiExposer.Create(nil);
end;

destructor TDOMNodeRttiExposerCursor.Destroy();
begin
  FreeAndNil(FCurrentExposer);;
  inherited Destroy();
end;

{ TDOMNamedNodeMapCursor }

procedure TDOMNamedNodeMapCursor.Reset();
begin
  FCurrent := -1;
end;

function TDOMNamedNodeMapCursor.MoveNext(): Boolean;
begin
  Inc(FCurrent);
  Result := ( FCurrent < GetNodeListCount(FList) );
end;

function TDOMNamedNodeMapCursor.Clone(): ICursor;
begin
  Result := TDOMNamedNodeMapCursor.Create(FList,faNone);
end;

function TDOMNamedNodeMapCursor.GetCount() : PtrInt;
begin
  Result := GetNodeListCount(FList);
end;

function TDOMNamedNodeMapCursor.GetCurrent(): IDefaultItemType;
begin
  if ( FCurrent > -1 ) and ( FCurrent < GetNodeListCount(FList) ) then
    Result := FList.Item[FCurrent]
  else
    Result := nil;
end;

constructor TDOMNamedNodeMapCursor.Create(
        ADataList : TDOMNamedNodeMap;
  const AFreeListOnDestroy : TFreeAction
);
begin
  Assert(Assigned(ADataList));
  FFreeListOnDestroy := AFreeListOnDestroy;
  FList := ADataList;
  Reset();
end;

destructor TDOMNamedNodeMapCursor.Destroy();
begin
  if ( FFreeListOnDestroy = faFreeOnDestroy ) then
    ReleaseDomNode(FList)
  else
    FList := nil;
  inherited Destroy();
end;

end.

