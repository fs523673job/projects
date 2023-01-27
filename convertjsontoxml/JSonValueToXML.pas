unit JSonValueToXML;

interface

uses
  NativeXML,
  System.Json,
  System.SysUtils
  ;

type
  TJSonUtils = class(TObject)
  public
    class function ConvertJSONValueToJSONObject(const AJSONContent: String): TJSONObject;
    class function JsonToXML(AJSONValue: TJSONValue): String;
  end;

implementation


class function TJSonUtils.ConvertJSONValueToJSONObject(const AJSONContent: String): TJSONObject;
var
  jsv : TJSONValue;
  jsa : TJSONArray;
begin
  jsv := TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(AJSONContent), 0);
  try
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
            LocalNode := LocalNode.NodeNew(itemObj.Pairs[x].JsonString.Value);
            AddJsonArray(LocalNode, TJSONArray(itemObj.Pairs[x].JsonValue))
          end
          else if (itemObj.Pairs[x].JsonValue is TJSONObject) then
          begin
            auxNode:= LocalNode.NodeNew(itemObj.Pairs[x].JsonString.Value);
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
