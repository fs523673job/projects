unit SimpleJsonToXML;

interface

uses
  SysUtils,
  StrUtils,
  System.Classes,
  System.JSON

  ;

type
  TJsonToXML = class(TObject)
  private
    class function JSONArrayToXmlStr(const AJSONArray: TJSONArray; var AAttr : String; const ATagName : String = ''): String; static;
    class function JSONArrayToXmlStringList(const AJSONArray: TJSONArray; const ALevel: Integer): TStringList; static;
    class function TypeText(const AJsonText: String): String; static;
    class function Tab(nivel: integer): String; static;
    class function ConvertJSONValueToJSONObject(const AJSONContent: String): TJSONObject;
  public
    class function StringToString(const AJSONContent: String): String;
  end;

implementation

{ TJsonToXML }

class function TJsonToXML.StringToString(const AJSONContent: String): String;
var
  dummy: String;
begin
  Result := JSONArrayToXmlStr(TJSONArray(ConvertJSONValueToJSONObject(AJSONContent)), dummy);
end;

class function TJsonToXML.ConvertJSONValueToJSONObject(const AJSONContent: String): TJSONObject;
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

class function TJsonToXML.TypeText(const AJsonText: String): String;
begin
  if AJsonText = 'node' then
    Result := AJsonText
  else if Pos('[{', AJsonText) = 1 then
    Result := 'object'
  else if Pos('[', AJsonText) = 1 then
    Result := 'array'
  else if pos('{', AJsonText) > 0 then
    Result := 'object'
  else
    Result := 'text';
end;

class function TJsonToXML.Tab(nivel: integer): String;
var
  c : Integer;
begin
  Result := EmptyStr;

  for c := 0 to nivel do
    Result := Result + #32#32;
end;


class function TJsonToXML.JSONArrayToXmlStr(const AJSONArray: TJSONArray; var AAttr : String; const ATagName : String = ''): String;
var
  item      : TJSONValue;
  pairItem  : TJSONPair;
  itemArray : TJSONArray;
  tagName   : String;
  tagValue  : String;
  tagOpen   : String;
  tagClose  : String;
  content   : String;
  aux       : String;
  attrib    : String;
  listStr   : TStringList;
  listAux   : TStringList;
  c         : Integer;
begin
  listStr := TStringList.Create();
  listAux := TStringList.Create();
  try
    listStr.Clear;

    content := EmptyStr;
    attrib  := EmptyStr;

    for item in AJSONArray do
    begin
      if (item.ClassType = TJSONPair) then
      begin
        tagName := TJSONPair(item).JsonString.ToString;
        try
          tagValue := TJSONPair(item).JsonValue.ToString;
        except
          tagValue := 'node';
        end;
      end
      else if (item.ClassType = TJSONObject) then
      begin
        for c := 0 to TJSONObject(item).Count - 1 do
        begin
          pairItem := TJSONObject(item).Pairs[c];
          if pairItem.JsonValue.ClassType = TJSONString then
          begin
            tagName  := pairItem.JsonString.Value;
            tagValue := pairItem.JsonValue.Value;

            tagName  := StringReplace(tagName, '"', EmptyStr, [rfReplaceAll]);
            tagOpen  := Format('<%s>', [tagName]);
            tagClose := Format('</%s>', [tagName]);

            listStr.Add(tagOpen);
            listStr.Add(tagValue);
            listStr.Add(tagClose);
          end
          else
          begin
            itemArray := TJSONArray.Create;
            itemArray.AddElement(TJSONObject.ParseJSONValue(pairItem.ToString));
            listStr.Add(JSONArrayToXmlStr(itemArray, attrib, pairItem.JsonString.Value));
          end;
        end;

        Continue;
      end;

      tagName  := StringReplace(tagName, '"', EmptyStr, [rfReplaceAll]);
      tagOpen  := Format('<%s>', [tagName]);
      tagClose := Format('</%s>', [tagName]);

      case AnsiIndexStr(TypeText(tagValue), ['text', 'object', 'array', 'node']) of
        0:
        begin
          if Pos('-', tagName) > 0 then
          begin
            tagName := StringReplace(tagName, '-', EmptyStr, [rfReplaceAll]);
            AAttr   := Format('%s %s=%s', [AAttr, tagName, tagValue]);
          end
          else
          begin
            tagValue := StringReplace(tagValue, '"', EmptyStr, [rfReplaceAll]);

            listStr.Add(tagOpen);
            listStr.Add(tagValue);
            listStr.Add(tagClose);
          end;
        end;
        1:
        begin
          if Pos('[', tagValue) = 1 then
          begin
            tagOpen  := EmptyStr;
            tagClose := EmptyStr;
            aux      := JSONArrayToXmlStr(TJSONArray(TJSONObject.ParseJSONValue(tagValue)), attrib, tagName);
          end
          else
          begin
            aux := JSONArrayToXmlStr(TJSONArray(TJSONObject.ParseJSONValue(tagValue)), attrib);
            if attrib <> EmptyStr then
              tagOpen := Format('<%s %s>', [tagName, attrib]);
          end;

          listStr.Add(tagOpen);
          listStr.Add(aux);
          listStr.Add(tagClose);
          attrib := EmptyStr;
        end;
        2:
        begin
          listAux.Clear;
          listAux.Delimiter := ',';
          listAux.DelimitedText := tagValue;

          for c := 0 to listAux.Count - 1 do
          begin
            aux := listAux.Strings[c];
            aux := StringReplace(aux, '[', EmptyStr, [rfReplaceAll]);
            aux := StringReplace(aux, ']', EmptyStr, [rfReplaceAll]);
            aux := StringReplace(aux, '"', EmptyStr, [rfReplaceAll]);

            if aux <> emptyStr then
            begin
              listStr.Add(tagOpen);
              listStr.Add(aux);
              listStr.Add(tagClose);
            end;
          end;
          listAux.Clear;
        end;
        3:
        begin
          if Assigned(listAux) then
            FreeAndNil(listAux);

          listAux := JSONArrayToXmlStringList(TJSONArray(item), 0);
          listAux.Insert(0, '{');
          listAux.Add('}');

          aux    := EmptyStr;
          attrib := EmptyStr;

          for c := 0 to listAux.Count -1 do
            aux := aux + listAux.Strings[c];

          aux := JSONArrayToXmlStr(TJSONArray(TJSONObject.ParseJSONValue(aux)), attrib);

          if attrib <> EmptyStr then
            tagOpen := Format('<%s %s>', [ATagName, attrib])
          else
            tagOpen := Format('<%s>', [ATagName]);

          tagClose := Format('</%s>', [ATagName]);

          listStr.Add(tagOpen);
          listStr.Add(aux);
          listStr.Add(tagClose);
          attrib := EmptyStr;
        end;
      end;
    end;

    for c := 0 to listStr.Count - 1 do
      content := content + listStr.Strings[c];

    Result := content;
  finally
    FreeAndNil(listStr);
    FreeAndNil(listAux);
  end;
end;

class function TJSONtoXML.JSONArrayToXmlStringList(const AJSONArray: TJSONArray; const ALevel: Integer): TStringList;
var
  c          : Integer;
  item       : TJSONValue;
  listAux    : TStringList;
  listReturn : TStringList;
  tagName    : String;
  tagValue   : String;
  tagOpen    : String;
  tagClose   : String;
  helpStr    : String;
begin
  Result  := TStringList.Create;
  listAux := TStringList.Create;
  try
    for item in AJSONArray do
    begin
      tagName := TJSONPair(item).JsonString.ToString;
      try
        tagValue := TJSONPair(item).JsonValue.ToString;
      except
        tagValue := 'node';
      end;

      case AnsiIndexStr(typeText(tagValue), ['text', 'object', 'array', 'node']) of
        0:
        begin
          tagOpen  := Tab(ALevel) + tagName + ': ';
          tagClose := ',';
        end;
        1:
        begin
          if Pos('[', tagValue) = 1 then
          begin
            tagOpen  := Tab(ALevel) + tagName + ': [';
            tagClose := Tab(ALevel) + '],';
          end
          else
          begin
            tagOpen  := Tab(ALevel) + tagName + ': {';
            tagClose := Tab(ALevel) + '},';
          end;

          if Assigned(listAux) then
            FreeAndNil(listAux);

          listAux := JSONArrayToXmlStringList(TJSONArray(TJSONObject.ParseJSONValue(tagValue)) , ALevel + 1);
        end;
        2:
        begin
          tagOpen  := Tab(ALevel) + tagName + ': [';
          tagClose := Tab(ALevel) + '],';

          listAux.Delimiter     := ',';
          listAux.DelimitedText := tagValue;

          for c := 0 to listAux.Count - 1 do
          begin
            helpStr := listAux.Strings[c];
            helpStr := StringReplace(helpStr, '[', EmptyStr, [rfReplaceAll]);
            helpStr := StringReplace(helpStr, ']', EmptyStr, [rfReplaceAll]);
            helpStr := StringReplace(helpStr, '"', EmptyStr, [rfReplaceAll]);

            if helpStr <> emptyStr then
              listAux.Strings[c] := Tab(ALevel + 1) + '"' + helpStr + '",';
          end;

          listAux.Delete(listAux.Count - 1);
          listAux.Strings[listAux.Count - 1] := StringReplace(listAux.Strings[listAux.Count - 1], ',', EmptyStr, [rfReplaceAll]);
        end;
        3:
        begin
          tagOpen  := Tab(ALevel) + '{';
          tagClose := Tab(ALevel) + '},';

          if Assigned(listAux) then
            FreeAndNil(listAux);

          listAux := JSONArrayToXmlStringList(TJSONArray(item) , ALevel + 1);
        end;

      end;

      if listAux.Count <= 0 then
        Result.Add(tagOpen + tagValue + tagClose)
      else
      begin
        Result.Add(tagOpen);

        for c := 0 to listAux.Count -1 do
          Result.Add(listAux.Strings[c]);

        Result.Add(tagClose);
      end;
    end;
  finally
    if Assigned(listAux) then
      FreeAndNil(listAux);
  end;

  if (Result.Count > 0) then
    Result.Strings[Result.Count - 1] := StringReplace(Result.Strings[Result.Count - 1], ',', EmptyStr, [rfReplaceAll]);
end;


end.
