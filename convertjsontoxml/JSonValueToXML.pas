unit JSonValueToXML;

interface

uses
  System.Json,
  NativeXML,
  System.SysUtils
  ;

type
  TJSonUtils = class(TObject)
  public
    class function ConvertJSONValueToJSONObject(const AJSONContent: String): TJSONObject;
    class function JsonToXML(AJSONObj: TJSONValue): String;
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

class function TJSonUtils.JsonToXML(AJSONObj: TJSONValue): String;
var
  c, i, w : Integer;
  XMLDoc  : TNativeXml;
  XMLNode : TXmlNode;
  item    : TJSONObject;
  itemObj : TJSONObject;

  procedure AddJsonArray(ARootNode: TXmlNode; AJSONArray: TJSONArray);
  var
    y, x, z   : Integer;
    LocalNode : TXmlNode;
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
            for z := 0 to TJSONObject(itemObj.Pairs[x].JsonValue).Count - 1 do
            begin
              Item := TJSONObject(itemObj.Pairs[x].JsonValue);
              LocalNode.NodeNew(Item.Pairs[z].JsonString.Value).Value := Item.Pairs[z].JsonValue.Value;
            end;
          end
          else
            LocalNode.NodeNew(itemObj.Pairs[x].JsonString.Value).Value := itemObj.Pairs[x].JsonValue.Value;
        end
      end
      else if AJSONArray.Items[y].ClassType = TJSONPair then
         ARootNode.NodeNew(TJSONPair(AJSONArray.Items[y]).JSONString.Value).Value := TJSONPair(AJSONArray.Items[y]).JsonValue.Value
      else
        AddJsonArray(ARootNode, TJSONArray(AJSONArray.Items[y]));
    end;
  end;

begin
  XMLDoc := TNativeXml.CreateName('root');
  try
    if (AJSONObj is TJSONObject) then
    begin
      for c := 0 to TJSONObject(AJSONObj).Count - 1 do
      begin
        if TJSONObject(AJSONObj).Pairs[c].JsonValue is TJSONArray then
        begin
          XMLNode := XMLDoc.Root.NodeNew(TJSONObject(AJSONObj).Pairs[c].JsonString.Value);
          AddJsonArray(XMLNode, TJSONArray(TJSONObject(AJSONObj).Pairs[c].JsonValue));
        end
        else if TJSONObject(AJSONObj).Pairs[c].JsonValue is TJSONObject then
        begin
          XMLNode := XMLDoc.Root.NodeNew(TJSONObject(AJSONObj).Pairs[c].JsonString.Value);
          if (TJSONObject(TJSONObject(AJSONObj).Pairs[c].JsonValue).Count > 0) then
            AddJsonArray(XMLNode, TJSONArray(TJSONObject(AJSONObj).Pairs[c].JsonValue));
        end;
      end;
    end
    else if (AJSONObj is TJSONArray) then
    begin
      for c := 0 to TJSONArray(AJSONObj).Count - 1 do
      begin
        if TJSONArray(AJSONObj).Items[c] is TJSONObject then
        begin
          item := TJSONObject(TJSONArray(AJSONObj).Items[c]);

          XMLNode := XMLDoc.Root.NodeNew('element');

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
    XMLDoc.SaveToString(Result);
  finally
    XMLDoc.Free;
  end;
end;


end.
