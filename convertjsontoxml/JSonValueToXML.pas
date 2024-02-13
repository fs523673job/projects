unit JSonValueToXML;

interface

uses
  NativeXML,
  System.Json,
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  System.Generics.Collections

  ;

type
  TJSonUtils = class(TObject)
  public
    class function ConvertJSONValueToJSONObject(const AJSONContent: String): TJSONObject;
    class function JsonToXML(AJSONValue: TJSONValue): String;
    class function FindContentNode(const AXml: String; const APathXml: String): String;
  end;

implementation


class function TJSonUtils.ConvertJSONValueToJSONObject(const AJSONContent: String): TJSONObject;
var
  jsv : TJSONValue;
  jsa : TJSONArray;
begin
  jsv := TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(AJSONContent), 0);
  try
    if not Assigned(jsv) then
      jsv := TJSONObject.ParseJSONValue(AJSONContent, True);

    if (jsv is TJSONObject) then
      Result := jsv as TJSONObject
    else if jsv is TJSONArray then
    begin
      jsa := jsv as TJSONArray;
      Result := TJSONObject.Create;
      Result.AddPair(TJsonPair.Create('dataArray', jsa));
    end
    else if jsv is TJSONString then
    begin
      Result := TJSONObject.Create;
      Result.AddPair('dataString', (jsv as TJSONString).ToString);
    end
    else
      Result := nil;
  except
    Result := nil;
  end;
end;

class function TJSonUtils.FindContentNode(const AXml, APathXml: String): String;
var
  Response : TStringStream;
  NativeXML: TNativeXml;
  XMLListNodes : TList;
  ArrayNodePaths: TArray<String>;
  ArrayNodesName: TArray<String>;
  NodeName: String;
  NodePath: String;
  XmlNode: TXmlNode;
  c: Integer;
begin
  Response := TStringStream.Create;
  try
    NativeXML := TNativeXml.Create(nil);
    try
      XMLListNodes := TList.Create;
      try
        Response.Clear;
        Response.WriteString(AXml);

        NativeXML.DefaultReadEncoding := seUTF8;
        NativeXML.LoadFromStream(Response);

        NodeName := '';
        NodePath := '';

        ArrayNodePaths := APathXml.Split([';']);
        ArrayNodesName := ArrayNodePaths[0].Split(['/']);

        if Length(ArrayNodePaths) > 1 then
          NodeName := ArrayNodePaths[1];

        XmlNode := NativeXML.Root.FindNode(ArrayNodesName[0]);

        if Assigned(XmlNode) then
        begin
          for c := 1 to High(ArrayNodesName) do
          begin
            if Assigned(XmlNode) then
              XmlNode := XmlNode.FindNode(ArrayNodesName[c]);
          end;

          XmlNode.FindNodes(NodePath, XMLListNodes);

          if XMLListNodes.Count - 1 >= 0 then
          begin
            for c := 0 to XMLListNodes.Count - 1 do
            begin
              XmlNode := TXmlNode(XMLListNodes.Items[c]).FindNode(NodeName);
              if Assigned(XmlNode) then
                Result := Result + #13#10 + TXmlNode(XMLListNodes.Items[c]).FullPath + ' ' + XMLNode.ValueUnicode
            end;
          end;
        end
        else
          raise Exception.Create('No nao encontrado');
      finally
        XMLListNodes.Free;
      end;
    finally
      NativeXML.Free;
    end;
  finally
    Response.Free;
  end;
end;

class function TJSonUtils.JsonToXML(AJSONValue: TJSONValue): String;
var
  nativeXML : TNativeXml;

  procedure AddJsonArray(ARootNode: TXmlNode; AJSONArray: TJSONArray); forward;

  procedure AddJsonObject(AElement: TsdElement; ANode: TXmlNode; AJSONObj: TJSONValue);
  var
    XMLNode : TXmlNode;
    c, i, w : Integer;
    item    : TJSONObject;
    itemObj : TJSONObject;
  begin
    if (AJSONObj is TJSONObject) then
    begin
      for c := 0 to TJSONObject(AJSONObj).Count - 1 do
      begin
        if Assigned(AElement) then
          XMLNode := AElement.NodeNew(TJSONObject(AJSONObj).Pairs[c].JsonString.Value)
        else
          XMLNode := ANode.NodeNew(TJSONObject(AJSONObj).Pairs[c].JsonString.Value);

        if (TJSONObject(AJSONObj).Pairs[c].JsonValue is TJSONArray) then
          AddJsonArray(XMLNode, TJSONArray(TJSONObject(AJSONObj).Pairs[c].JsonValue))
        else if (TJSONObject(AJSONObj).Pairs[c].JsonValue is TJSONObject) then
        begin
          if (TJSONObject(TJSONObject(AJSONObj).Pairs[c].JsonValue).Count > 0) then
            AddJsonArray(XMLNode, TJSONArray(TJSONObject(AJSONObj).Pairs[c].JsonValue));
        end
        else
          XMLNode.Value := TJSONObject(AJSONObj).Pairs[c].JsonValue.Value;
      end;
    end
    else if (AJSONObj is TJSONArray) then
    begin
      for c := 0 to TJSONArray(AJSONObj).Count - 1 do
      begin
        if TJSONArray(AJSONObj).Items[c] is TJSONObject then
        begin
          item := TJSONObject(TJSONArray(AJSONObj).Items[c]);

          if Assigned(AElement) then
            XMLNode := AElement.NodeNew('element')
          else
            XMLNode := ANode.NodeNew('element');

          for i := 0 to item.Count - 1 do
          begin
            if (item.Pairs[i].JsonValue is TJSONArray) then
            begin
              XMLNode := XMLNode.NodeNew(item.Pairs[i].JsonString.Value);
              AddJsonArray(XMLNode, TJSONArray(item.Pairs[i].JsonValue));
            end
            else if (item.Pairs[i].JsonValue is TJSONObject) then
            begin
              for w := 0 to TJSONObject(item.Pairs[i].JsonValue).Count - 1 do
              begin
                itemObj := TJSONObject(item.Pairs[i].JsonValue);
                XMLNode.NodeNew(itemObj.Pairs[w].JsonString.Value).Value := itemObj.Pairs[w].JsonValue.Value;
              end;
            end
            else
              XMLNode.NodeNew(item.Pairs[i].JsonString.Value).Value := item.Pairs[i].JsonValue.Value;
          end;
        end;
      end;
    end;
  end;

  procedure AddJsonArray(ARootNode: TXmlNode; AJSONArray: TJSONArray);
  var
    y, x      : Integer;
    LocalNode : TXmlNode;
    auxNode   : TXmlNode;
    itemObj   : TJSONObject;
  begin
    for y := 0 to TJSONArray(AJSONArray).Count - 1 do
    begin
      if AJSONArray.Items[y] is TJSONObject then
      begin
        itemObj := TJSONObject(AJSONArray.Items[y]);

        LocalNode := ARootNode.NodeNew('element');

        for x := 0 to itemObj.Count - 1 do
        begin
          if itemObj.Pairs[x].JsonValue is TJSONArray then
          begin
            auxNode := LocalNode.NodeNew(itemObj.Pairs[x].JsonString.Value);
            AddJsonArray(auxNode, TJSONArray(itemObj.Pairs[x].JsonValue));
          end
          else if (itemObj.Pairs[x].JsonValue is TJSONObject) then
          begin
            auxNode := LocalNode.NodeNew(itemObj.Pairs[x].JsonString.Value);
            AddJsonObject(nil, auxNode, itemObj.Pairs[x].JsonValue);
          end
          else
            LocalNode.NodeNew(itemObj.Pairs[x].JsonString.Value).Value := itemObj.Pairs[x].JsonValue.Value;
        end
      end
      else if AJSONArray.Items[y].ClassType = TJSONPair then
      begin
         auxNode := ARootNode.NodeNew(TJSONPair(AJSONArray.Items[y]).JSONString.Value);
         if (TJSONPair(AJSONArray.Items[y]).JsonValue.Value <> '') then
           auxNode.Value := TJSONPair(AJSONArray.Items[y]).JsonValue.Value
         else
           AddJsonObject(nil, auxNode, TJSONPair(AJSONArray.Items[y]).JsonValue);
      end
      else if AJSONArray.Items[y].ClassType <> TJSONArray then
      begin
        auxNode := ARootNode.NodeNew('element');
        auxNode.Value := TJSONString(AJSONArray.Items[y]).Value;
      end
      else
        AddJsonArray(ARootNode, TJSONArray(AJSONArray.Items[y]));
    end;
  end;

begin
  nativeXML := TNativeXml.CreateName('root');
  try
    AddJsonObject(nativeXML.Root, nil, AJSONValue);
    nativeXML.SaveToString(Result);
  finally
    nativeXML.Free;
  end;
end;


end.
