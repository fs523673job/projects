{::APDATA:: David - 11-2016 :: Conversão para Unicode/64bits ::}

{ unit NativeXmlNodes

  NativeXmlNodes.pas provides some functionality for xml nodes and attributes
  in a LINQ-like way. The first LINQ enhancement was directly in NativeXml.pas,
  made by Hans-Dieter Karl. Now this functionality is in unit NativeXmlNodes.pas,
  using auxiliary class NativeXmlEx.
}
unit NativeXmlNodes;

interface

uses
  Classes, Contnrs, NativeXml, NativeXmlCodepages;

type

  TNativeXmlEx = class(TNativeXml)
  public
    constructor CreateEx(AOwner: TComponent; HasDeclaration, HasDocType, HasRoot: boolean; ARootName: String);
    // some more added  methods in a LINQ-like way:
    // attributes
    function AttrText(AName, AValue: String): TsdAttribute;
    function AttrInt(AName: String; AValue: integer): TsdAttribute;
    function AttrInt64(AName: String; AValue: int64): TsdAttribute;
    function AttrFloat(AName: String; AValue: double): TsdAttribute; overload;
    function AttrFloat(AName: String; AValue: double; ASignificantDigits: integer;
      AAllowScientific: boolean): TsdAttribute; overload;
    function AttrDateTime(AName: String; AValue: TDateTime): TsdAttribute;
    function AttrBool(AName: String; AValue: boolean): TsdAttribute;

    // container nodes
    function NodeNew(AName: String): TXmlNode; overload; virtual;
    function NodeNew(AName: String; SubNodes: array of TXmlNode): TXmlNode; overload; virtual;
    function NodeNewEx(AName: String; out AXmlNode: TXmlNode): TXmlNode; overload;
    function NodeNewEx(AName: String; out AXmlNode: TXmlNode; SubNodes: array of TXmlNode): TXmlNode; overload;

    // string nodes
    function NodeNewText(AName, AValue: String): TXmlNode; overload;
    function NodeNewTextEx(AName, AValue: String; out AXmlNode: TXmlNode): TXmlNode; overload;
    function NodeNewText(AName, AValue: String; SubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewTextEx(AName, AValue: String; out AXmlNode: TXmlNode;
      SubNodes: array of TXmlNode): TXmlNode; overload;

    function NodeNewType(AName: String; AElementType: TsdElementType): TXmlNode; overload;
    function NodeNewTypeEx(AName: String; AElementType: TsdElementType;
      out AXmlNode: TXmlNode): TXmlNode; overload;
    function NodeNewType(AName: String; AElementType: TsdElementType;
      SubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewTypeEx(AName: String; AElementType: TsdElementType;
      out AXmlNode: TXmlNode; SubNodes: array of TXmlNode): TXmlNode; overload;

    function NodeNewAttr(AName: String; Attributes: array of TsdAttribute): TXmlNode; overload;
    function NodeNewAttrEx(AName: String; out AXmlNode: TXmlNode;
      Attributes: array of TsdAttribute): TXmlNode; overload;
    function NodeNewAttr(AName: String; Attributes: array of TsdAttribute;
      SubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewAttrEx(AName: String; out AXMLNode: TXmlNode;
      Attributes: array of TsdAttribute; SubNodes: array of TXmlNode): TXmlNode; overload;

    function NodeNewTextType(AName, AValue: String;
      AElementType: TsdElementType): TXmlNode; overload;
    function NodeNewTextTypeEx(AName, AValue: String;
      AElementType: TsdElementType; out AXmlNode: TXmlNode): TXmlNode; overload;
    function NodeNewTextType(AName, AValue: String;
      AElementType: TsdElementType; SubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewTextTypeEx(AName, AValue: String; AElementType: TsdElementType;
      out AXmlNode: TXmlNode; SubNodes: array of TXmlNode): TXmlNode; overload;

    function NodeNewTextAttr(AName, AValue: string; Attributes: array of TsdAttribute): TXmlNode; overload;
    function NodeNewTextAttrEx(AName, AValue: String; out AXmlNode: TXmlNode;
      Attributes: array of TsdAttribute): TXmlNode; overload;
    function NodeNewTextAttr(AName, AValue: String; Attributes: array of TsdAttribute;
      SubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewTextAttrEx(AName, AValue: String; out AXmlNode: TXmlNode;
      Attributes: array of TsdAttribute; SubNodes: array of TXmlNode): TXmlNode; overload;

    function NodeNewTextTypeAttr(AName, AValue: String; AElementType: TsdElementType;
      Attributes: array of TsdAttribute): TXmlNode; overload;
    function NodeNewTextTypeAttr(AName, AValue: String; AElementType: TsdElementType;
      Attributes: array of TsdAttribute; SubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewTextTypeAttrEx(AName, AValue: String; AElementType: TsdElementType;
      out AXmlNode: TXmlNode; Attributes: array of TsdAttribute): TXmlNode; overload;
    function NodeNewTextTypeAttrEx(AName, AValue: String; AElementType: TsdElementType;
      out AXmlNode: TXmlNode; Attributes: array of TsdAttribute;
      SubNodes: array of TXmlNode): TXmlNode; overload;

    // integer nodes
    function NodeNewInt(AName: String; AValue: integer): TXmlNode; overload;
    function NodeNewIntEx(AName: String; AValue: integer; out AXmlNode: TXmlNode): TXmlNode; overload;
    function NodeNewInt(AName: String; AValue: integer; SubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewIntEx(AName: String; AValue: integer; out AXmlNode: TXmlNode;
      SubNodes: array of TXmlNode): TXmlNode; overload;

    function NodeNewIntType(AName: String; AValue: integer;
      AElementType: TsdElementType): TXmlNode; overload;
    function NodeNewIntTypeEx(AName: String; AValue: integer;
      AElementType: TsdElementType; out AXmlNode: TXmlNode): TXmlNode; overload;
    function NodeNewIntType(AName: String; AValue: integer;
      AElementType: TsdElementType; SubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewIntTypeEx(AName: String; AValue: integer; AElementType: TsdElementType;
      out AXmlNode: TXmlNode; SubNodes: array of TXmlNode): TXmlNode; overload;

    function NodeNewIntAttr(AName: String; AValue: integer; Attributes: array of TsdAttribute): TXmlNode; overload;
    function NodeNewIntAttrEx(AName: String; AValue: integer; out AXmlNode: TXmlNode;
      Attributes: array of TsdAttribute): TXmlNode; overload;
    function NodeNewIntAttr(AName: String; AValue: integer; Attributes: array of TsdAttribute;
      SubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewIntAttrEx(AName: String; AValue: integer; out AXmlNode: TXmlNode;
      Attributes: array of TsdAttribute; SubNodes: array of TXmlNode): TXmlNode; overload;

    function NodeNewIntTypeAttr(AName: String; AValue: integer; AElementType: TsdElementType;
      Attributes: array of TsdAttribute): TXmlNode; overload;
    function NodeNewIntTypeAttrEx(AName: String; AValue: integer; AElementType: TsdElementType;
      out AXmlNode: TXmlNode; Attributes: array of TsdAttribute): TXmlNode; overload;
    function NodeNewIntTypeAttr(AName: String; AValue: integer; AElementType: TsdElementType;
      Attributes: array of TsdAttribute; SubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewIntTypeAttrEx(AName: String; AValue: integer; AElementType: TsdElementType;
      out AXmlNode: TXmlNode; Attributes: array of TsdAttribute;
      SubNodes: array of TXmlNode): TXmlNode; overload;
  end;

implementation

// simple constructor without declaration, but with a standard root element
constructor TNativeXmlEx.CreateEx(AOwner: TComponent; HasDeclaration, HasDocType, HasRoot: boolean; ARootName: String);
begin
  inherited Create(AOwner);

  // FRootNodes is an owned list
  FRootNodes := TsdNodeList.Create(True);

  // CreateEx options
//todo  FHasDeclaration := HasDeclaration;
//todo  FHasDocType := HasDocType;
//todo  FHasRoot := HasRoot;
//todo  FRootName := ARootName;

  // this resets defaults
//todo  ResetDefaults;

  // now clear the rootnodes and create optional declaration, doctype and root
//todo  ClearData(FHasDeclaration, FHasDocType, FHasRoot);
end;

function TNativeXmlEx.AttrText(AName, AValue: String): TsdAttribute;
begin
  Result := TsdAttribute.Create(Self);
  Result.Name := AName;
  Result.Value := AValue;
end;

function TNativeXmlEx.AttrInt(AName: String; AValue: integer): TsdAttribute;
begin
  Result := TsdAttribute.Create(Self);
  Result.Name := AName;
  Result.Value := sdIntToString(AValue);
end;

function TNativeXmlEx.AttrInt64(AName: String; AValue: int64): TsdAttribute;
begin
  Result := TsdAttribute.Create(Self);
  Result.Name := AName;
  Result.Value := sdInt64ToString(AValue);
end;


function TNativeXmlEx.AttrFloat(AName: String; AValue: double): TsdAttribute;
begin
  Result := TsdAttribute.Create(Self);
  Result.Name := AName;
  Result.Value := sdFloatToString(AValue, cDefaultFloatSignificantDigits,
    cDefaultFloatAllowScientific);
end;

function TNativeXmlEx.AttrFloat(AName: String; AValue: double; ASignificantDigits: integer;
  AAllowScientific: boolean): TsdAttribute;
begin
  Result := TsdAttribute.Create(Self);
  Result.Name := AName;
  Result.Value := sdFloatToString(AValue, ASignificantDigits, AAllowScientific);
end;

function TNativeXmlEx.AttrDateTime(AName: String; AValue: TDateTime): TsdAttribute;
begin
  Result := TsdAttribute.Create(Self);
  Result.Name := AName;
  Result.Value := sdDateTimeToString(AValue, True, True);
end;

function TNativeXmlEx.AttrBool(AName: String; AValue: boolean): TsdAttribute;
begin
  Result := TsdAttribute.Create(Self);
  Result.Name := AName;
  Result.Value := sdBoolToString(AValue);
end;


function TNativeXmlEx.NodeNew(AName: String): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, '', xeElement, [], []);
end;

function TNativeXmlEx.NodeNewEx(AName: String; out AXmlNode: TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, '', xeElement, AXmlNode, [], []);
end;

function TNativeXmlEx.NodeNew(AName: String; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, '', xeElement, [], SubNodes);
end;

function TNativeXmlEx.NodeNewEx(AName: String; out AXmlNode: TXmlNode;
  SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, '', xeElement, AXmlNode, [], SubNodes);
end;

function TNativeXmlEx.NodeNewType(AName: String; AElementType: TsdElementType): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, '', AElementType, [], []);
end;

function TNativeXmlEx.NodeNewTypeEx(AName: string; AElementType: TsdElementType;
  out AXmlNode: TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, '', AElementType, AXmlNode, [], []);
end;

function TNativeXmlEx.NodeNewType(AName: string; AElementType: TsdElementType;
  SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, '', AElementType, [], SubNodes);
end;

function TNativeXmlEx.NodeNewTypeEx(AName: String; AElementType: TsdElementType;
  out AXmlNode: TXmlNode; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, '', AElementType, AXmlNode, [], SubNodes);
end;

function TNativeXmlEx.NodeNewAttr(AName: String;
  Attributes: array of TsdAttribute): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, '', xeElement, Attributes, []);
end;

function TNativeXmlEx.NodeNewAttrEx(AName: String; out AXmlNode: TXmlNode;
  Attributes: array of TsdAttribute): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, '', xeElement, AXmlNode, Attributes, []);
end;

function TNativeXmlEx.NodeNewAttr(AName: String; Attributes: array of TsdAttribute;
  SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, '', xeElement, Attributes, SubNodes);
end;

function TNativeXmlEx.NodeNewAttrEx(AName: String; out AXmlNode: TXmlNode;
  Attributes: array of TsdAttribute; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, '', xeElement, AXmlNode, Attributes,
    SubNodes);
end;

function TNativeXmlEx.NodeNewText(AName, AValue: String): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, AValue, xeElement, [], []);
end;

function TNativeXmlEx.NodeNewTextEx(AName, AValue: String;
  out AXmlNode: TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, AValue, xeElement, AXmlNode, [], []);
end;

function TNativeXmlEx.NodeNewText(AName, AValue: String; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, AValue, xeElement, [], SubNodes);
end;

function TNativeXmlEx.NodeNewTextEx(AName, AValue: String; out AXmlNode: TXmlNode;
  SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, AValue, xeElement, AXmlNode, [], SubNodes);
end;

function TNativeXmlEx.NodeNewTextType(AName, AValue: String;
  AElementType: TsdElementType): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, AValue, AElementType, [], []);
end;

function TNativeXmlEx.NodeNewTextTypeEx(AName, AValue: String;
  AElementType: TsdElementType; out AXmlNode: TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, AValue, AElementType, AXmlNode, [], []);
end;

function TNativeXmlEx.NodeNewTextType(AName, AValue: String;
  AElementType: TsdElementType; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, AValue, AElementType, [], SubNodes);
end;

function TNativeXmlEx.NodeNewTextTypeEx(AName, AValue: String;
  AElementType: TsdElementType; out AXmlNode: TXmlNode;
  SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, AValue, AElementType, AXmlNode, [],
    SubNodes);
end;

function TNativeXmlEx.NodeNewTextAttr(AName, AValue: String;
  Attributes: array of TsdAttribute): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, AValue, xeElement, Attributes, []);
end;

function TNativeXmlEx.NodeNewTextAttrEx(AName, AValue: String; out AXmlNode: TXmlNode;
  Attributes: array of TsdAttribute): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, AValue, xeElement, AXmlNode, Attributes,
    []);
end;

function TNativeXmlEx.NodeNewTextAttr(AName, AValue: String;
  Attributes: array of TsdAttribute; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, AValue, xeElement, Attributes, SubNodes);
end;

function TNativeXmlEx.NodeNewTextAttrEx(AName, AValue: String; out AXmlNode: TXmlNode;
  Attributes: array of TsdAttribute; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, AValue, xeElement, AXmlNode, Attributes,
    SubNodes);
end;

function TNativeXmlEx.NodeNewTextTypeAttr(AName, AValue: String;
  AElementType: TsdElementType; Attributes: array of TsdAttribute): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, AValue, AElementType, Attributes, []);
end;

function TNativeXmlEx.NodeNewTextTypeAttrEx(AName, AValue: String;
  AElementType: TsdElementType; out AXmlNode: TXmlNode;
  Attributes: array of TsdAttribute): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, AValue, AElementType, AXmlNode,
    Attributes, []);
end;

function TNativeXmlEx.NodeNewTextTypeAttr(AName, AValue: String;
  AElementType: TsdElementType; Attributes: array of TsdAttribute;
  SubNodes: array of TXmlNode): TXmlNode;
var
  NodeClass: TsdNodeClass;
begin
  NodeClass := cNodeClass[AElementType];
  Result := NodeClass.Create(Self);
  Result.Name := AName;
  Result.Value := AValue;

  Result.AttributesAdd(Attributes);
  Result.NodesAdd(SubNodes);
end;

function TNativeXmlEx.NodeNewTextTypeAttrEx(AName, AValue: String;
  AElementType: TsdElementType; out AXmlNode: TXmlNode;
  Attributes: array of TsdAttribute; SubNodes: array of TXmlNode): TXmlNode;
begin
  AXmlNode := NodeNewTextTypeAttr(AName, AValue, AElementType, Attributes,
    SubNodes);
  Result := AXmlNode;
end;

function TNativeXmlEx.NodeNewInt(AName: String; AValue: integer): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, sdIntToString(AValue), xeElement, [], []);
end;

function TNativeXmlEx.NodeNewIntEx(AName: String; AValue: integer;
  out AXmlNode: TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, sdIntToString(AValue), xeElement, AXmlNode,
    [], []);
end;

function TNativeXmlEx.NodeNewInt(AName: String; AValue: integer;
  SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, sdIntToString(AValue), xeElement, [], SubNodes);
end;

function TNativeXmlEx.NodeNewIntEx(AName: String; AValue: integer;
  out AXmlNode: TXmlNode; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, sdIntToString(AValue), xeElement, AXmlNode,
    [], SubNodes);
end;

function TNativeXmlEx.NodeNewIntAttr(AName: String; AValue: integer;
  Attributes: array of TsdAttribute): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, sdIntToString(AValue), xeElement, Attributes,
    []);
end;

function TNativeXmlEx.NodeNewIntAttrEx(AName: String; AValue: integer;
  out AXmlNode: TXmlNode; Attributes: array of TsdAttribute): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, sdIntToString(AValue), xeElement, AXmlNode,
    Attributes, []);
end;

function TNativeXmlEx.NodeNewIntAttr(AName: String; AValue: integer;
  Attributes: array of TsdAttribute; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, sdIntToString(AValue), xeElement, Attributes,
    SubNodes);
end;

function TNativeXmlEx.NodeNewIntAttrEx(AName: String; AValue: integer;
  out AXmlNode: TXmlNode; Attributes: array of TsdAttribute;
  SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, sdIntToString(AValue), xeElement, AXmlNode,
    Attributes, SubNodes);
end;

function TNativeXmlEx.NodeNewIntTypeAttr(AName: String; AValue: integer;
  AElementType: TsdElementType; Attributes: array of TsdAttribute): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, sdIntToString(AValue), AElementType, Attributes,
    []);
end;

function TNativeXmlEx.NodeNewIntTypeAttrEx(AName: String; AValue: integer;
  AElementType: TsdElementType; out AXmlNode: TXmlNode;
  Attributes: array of TsdAttribute): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, sdIntToString(AValue), AElementType, AXmlNode,
    Attributes, []);
end;

function TNativeXmlEx.NodeNewIntType(AName: String; AValue: integer;
  AElementType: TsdElementType): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, sdIntToString(AValue), AElementType, [], []);
end;

function TNativeXmlEx.NodeNewIntTypeEx(AName: String; AValue: integer;
  AElementType: TsdElementType; out AXmlNode: TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, sdIntToString(AValue), AElementType, AXmlNode,
    [], []);
end;

function TNativeXmlEx.NodeNewIntType(AName: String; AValue: integer;
  AElementType: TsdElementType; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, sdIntToString(AValue), AElementType, [], SubNodes);
end;

function TNativeXmlEx.NodeNewIntTypeEx(AName: String; AValue: integer;
  AElementType: TsdElementType; out AXmlNode: TXmlNode;
  SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, sdIntToString(AValue), AElementType, AXmlNode,
    [], SubNodes);
end;

function TNativeXmlEx.NodeNewIntTypeAttr(AName: String; AValue: integer;
  AElementType: TsdElementType; Attributes: array of TsdAttribute;
  SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, sdIntToString(AValue), AElementType, Attributes,
    SubNodes);
end;

function TNativeXmlEx.NodeNewIntTypeAttrEx(AName: String; AValue: integer;
  AElementType: TsdElementType; out AXmlNode: TXmlNode;
  Attributes: array of TsdAttribute; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, sdIntToString(AValue), AElementType, AXmlNode,
    Attributes, SubNodes);
end;

end.
