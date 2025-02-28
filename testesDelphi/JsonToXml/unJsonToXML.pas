unit unJsonToXML;

interface

uses
  System.Classes,
  System.SysUtils,
  StrUtils,
  System.Json,
  RegularExpressions
  ;

type
  TRestUtils = class
  private
  public
    class function JsonToXML(AJSONValue: TJSONValue): String; overload;
    class function ConvertJSONValueToJSONObject(const AJSONContent: String; const AIsUTF8: Boolean = False): TJSONObject;
    class function ConvertJSONObjectToStringStream(const AJSONObj: TJSONObject): TStringStream;
  end;

implementation

uses
  NativeXML;

{ TServerUtils }

class function TRestUtils.ConvertJSONObjectToStringStream(const AJSONObj: TJSONObject): TStringStream;
begin
  Result := TStringStream.Create;
  if Assigned(AJSONObj) then
    Result.WriteString(AJSONObj.ToString);
  Result.Position := 0;
end;

class function TRestUtils.ConvertJSONValueToJSONObject(const AJSONContent: String; const AIsUTF8: Boolean): TJSONObject;
var
  jsv : TJSONValue;
  jsa : TJSONArray;
begin
  if AIsUTF8 then
    jsv := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(AJSONContent), 0)
  else
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

class procedure TRestUtils.XMLToJson(ARequest: TWebRequest; var AJSONObj: TJSONObject);
var
  XML       : TNativeXml;
  c         : Integer;
  jsonArray : TJSONArray;

  procedure AddItemsToJsonArray(ANode: TXmlNode; AJsonArray: TJSONArray);
  var
    i       : Integer;
    jsonObj : TJSONObject;
  begin
    jsonObj := nil;
    for i := 0 to ANode.NodeCount - 1 do
    begin
      if (ANode.Nodes[i].NodeCount - 1 = 0) then
      begin
        if not Assigned(jsonObj) then
          jsonObj := TJSONObject.Create;

        if not AnsiSameStr(ANode.Nodes[i].Name, XML_NODE_ELEMENT) then
          jsonObj.AddPair(ANode.Nodes[i].Name, ANode.Nodes[i].Value);
      end
      else
        AddItemsToJsonArray(ANode.Nodes[i], AJsonArray);
    end;
    AJsonArray.AddElement(jsonObj);
  end;

  function ExistChild(ANode: TXmlNode): Boolean;
  var
    x : Integer;
  begin
    Result := False;

    for x := 0 to ANode.NodeCount - 1 do
    begin
      if (ANode.Nodes[x].NodeCount > 1) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;

begin
  XML := TNativeXml.CreateName('root');
  try
    try
      XML.ReadFromString(ARequest.Content);

      AJSONObj  := TJSONObject.Create;

      if Assigned(XML.Root) then
      begin
        jsonArray := TJSONArray.Create;

        for c := 0 to XML.Root.NodeCount - 1 do
        begin
          if (XML.Root.Nodes[c].NodeCount = 1) then
          begin
            if ExistChild(XML.Root.Nodes[c]) then
              AddItemsToJsonArray(XML.Root.Nodes[c], jsonArray)
            else
              AJSONObj.AddPair(XML.Root.Nodes[c].Name, XML.Root.Nodes[c].Value);
          end
          else
            AddItemsToJsonArray(XML.Root.Nodes[c], jsonArray);
        end;
        AJSONObj.AddPair(PARAMREQUEST_ITEMS, jsonArray);
      end
      else
      begin
        if Assigned(AJSONObj) then
          FreeAndNil(AJSONObj);
      end;
    except
      on e : Exception do
      begin
        if Assigned(AJSONObj) then
          FreeAndNil(AJSONObj)
        else
          AJSONObj := nil;

        raise Exception.Create('bad xml formater');
      end;
    end;
  finally
    XML.Free;
  end;
end;

class function TRestUtils.RequestParameterToJsonArray(ARequest: TWebRequest; var AJSONArray: TJSONArray): Boolean;
begin
  if Assigned(AJSONArray) then
    FreeAndNil(AJSONArray);

  if (Pos('XML', UpperCase(ARequest.ContentType)) > 0) then
  begin
    // TODO :: convert simple XML to json object - not implement yet
  end
  else if (Pos('JSON', UpperCase(ARequest.ContentType)) > 0) then
  begin
    try
      AJSONArray := TJSONObject.ParseJSONValue(ARequest.Content) as TJSONArray;
    except
      AJSONArray := nil;
    end;
  end;

  Result := AJSONArray <> nil;
end;

class procedure TRestUtils.FreeJsonArray(var AJsonArray: TJSONArray);
var
  c         : Integer;
  jsonArray : TJSONArray;
begin
  for c := 0 to AJsonArray.Count - 1 do
  begin
    if (AJsonArray.Items[c] is TJSONObject) then
      (AJsonArray.Items[c] as TJSONObject).Free
    else if (AJsonArray.Items[c] is TJSONArray) then
    begin
      jsonArray := AJsonArray.Items[c] as TJSONArray;
      FreeJsonArray(jsonArray);
    end;
  end;

  AJsonArray := nil;
end;

class function TRestUtils.JsonToXML(AJSONValue: TJSONValue): String;
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
              AddJsonArray(XMLNode, item.Pairs[i].JsonValue as TJSONArray);
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
            AddJsonArray(auxNode, TJSONArray(itemObj.Pairs[x].JsonValue))
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
      end else if (AJSONArray.Items[y] is TJSONString) then
        if String.IsNullOrWhiteSpace(ARootNode.Value) then
          ARootNode.Value := TJSONString(AJSONArray.Items[y]).Value
        else
          ARootNode.Value := ARootNode.Value + ', ' + TJSONString(AJSONArray.Items[y]).Value
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

class function TRestUtils.JsonToXML(ARequest: TWebRequest; var AJSONObj: TJSONObject): String;
begin
  // TODO :: convert simple JSon to xml string - not implement yet
  Result := '';
end;

end.
