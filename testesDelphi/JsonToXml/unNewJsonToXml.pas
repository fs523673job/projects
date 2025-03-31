unit unNewJsonToXml;

interface

uses
  System.Classes,
  System.SysUtils,
  StrUtils,
  System.JSON,
  System.Generics.Collections,
  RegularExpressions,
  NativeXML;

type
  TTagsXML = class
  private
    FTagName                  : String;
    FTagParent                : String;
    FFullPath                 : String;
    FTagListOrigem            : TStringList;
    FTagListDestino           : TStringList;
    FListFullPath             : TStringList;
    FCdiTransacao_Retorno     : Integer;
    FNuiTipoEdicao_Retorno    : Integer;
    FCdiModeloIntegracao_Ret  : Integer;
    FDssCampoDestino          : String;
    FDssCampoOrigem           : String;
  public
    constructor Create(const ATagName, ATagParent, AFullPath: String);
    destructor Destroy; override;

    procedure AddTags(const ATagOrigem, ATagDestino, AFullPath: String);

    property TagName                  : String read FTagName;
    property TagParent                : String read FTagParent;
    property FullPath                 : String read FFullPath;
    property TagListOrigem            : TStringList read FTagListOrigem;
    property TagListDestino           : TStringList read FTagListDestino;
    property ListFullPath             : TStringList read FListFullPath;
    property DssCampoDestino          : String read FDssCampoDestino write FDssCampoDestino;
    property DssCampoOrigem           : String read FDssCampoOrigem write FDssCampoOrigem;
    property CdiTransacao_Retorno     : Integer read FCdiTransacao_Retorno write FCdiTransacao_Retorno;
    property NuiTipoEdicao_Retorno    : Integer read FNuiTipoEdicao_Retorno write FNuiTipoEdicao_Retorno;
    property CdiModeloIntegracao_Ret  : Integer read FCdiModeloIntegracao_Ret write FCdiModeloIntegracao_Ret;
  end;

  TNewResUtil = class
  public
    class function JsonToXML(const AJSONValue: TJSONValue): String; overload;
    class function JsonToXML(const AJSONObj: TJSONObject): String; overload;
    class function ConvertJSONValueToJSONObject(const AJSONContent: String; const AIsUTF8: Boolean = False): TJSONObject;
    class function ConvertJSONObjectToStringStream(const AJSONObj: TJSONObject): TStringStream;
    class function ResultContentNodesInMap(const AMap: TArray<String>; const AContentXML: String): TArray<String>;
  end;

implementation

constructor TTagsXML.Create(const ATagName, ATagParent, AFullPath: String);
begin
  FTagParent := ATagParent;
  FTagName   := ATagName;
  FFullPath  := AFullPath;

  FTagListOrigem  := TStringList.Create;
  FTagListDestino := TStringList.Create;
  FListFullPath   := TStringList.Create;
end;

destructor TTagsXML.Destroy;
begin
  FTagListOrigem.Free;
  FTagListDestino.Free;
  FListFullPath.Free;
  inherited;
end;

procedure TTagsXML.AddTags(const ATagOrigem, ATagDestino, AFullPath: String);
begin
  FTagListOrigem.Add(ATagOrigem);
  FTagListDestino.Add(ATagDestino);
  FListFullPath.Add(AFullPath);
end;

class function TNewResUtil.ConvertJSONValueToJSONObject(const AJSONContent: String; const AIsUTF8: Boolean): TJSONObject;
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
      Result.AddPair('dataArray', jsa);
    end
    else if jsv is TJSONString then
    begin
      Result := TJSONObject.Create;
      Result.AddPair('dataString', (jsv as TJSONString).Value);
    end
    else
      Result := nil;
  except
    Result := nil;
  end;
end;

class function TNewResUtil.ConvertJSONObjectToStringStream(const AJSONObj: TJSONObject): TStringStream;
begin
  Result := TStringStream.Create;
  try
    if Assigned(AJSONObj) then
      Result.WriteString(AJSONObj.ToString);
    Result.Position := 0;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure AddJsonArrayToXml(ARootNode: TXmlNode; AJSONArray: TJSONArray);

  procedure AddJsonObjectFields(AParentNode: TXmlNode; AObj: TJSONObject);
  var
    i: Integer;
    SubNode: TXmlNode;
    Pair: TJSONPair;
  begin
    for i := 0 to AObj.Count - 1 do
    begin
      Pair := AObj.Pairs[i];
      SubNode := AParentNode.NodeNew(Pair.JsonString.Value);

      if Pair.JsonValue is TJSONArray then
        AddJsonArrayToXml(SubNode, TJSONArray(Pair.JsonValue))
      else if Pair.JsonValue is TJSONObject then
        AddJsonObjectFields(SubNode, TJSONObject(Pair.JsonValue))
      else
        SubNode.Value := Pair.JsonValue.Value;
    end;
  end;

var
  i: Integer;
  item: TJSONValue;
  nodeItem: TXmlNode;
begin
  for i := 0 to AJSONArray.Count - 1 do
  begin
    item := AJSONArray.Items[i];

    if item is TJSONObject then
    begin
      nodeItem := ARootNode.NodeNew('element');
      AddJsonObjectFields(nodeItem, TJSONObject(item));
    end
    else if item is TJSONArray then
    begin
      nodeItem := ARootNode.NodeNew('element');
      AddJsonArrayToXml(nodeItem, TJSONArray(item));
    end
    else
    begin
      nodeItem := ARootNode.NodeNew('element');
      nodeItem.Value := item.Value;
    end;
  end;
end;

procedure AddJsonObjectToXml(AXmlNode: TXmlNode; AJSONObj: TJSONObject);
var
  i: Integer;
  Pair: TJSONPair;
  SubNode: TXmlNode;
begin
  for i := 0 to AJSONObj.Count - 1 do
  begin
    Pair := AJSONObj.Pairs[i];

    SubNode := AXmlNode.NodeNew(Pair.JsonString.Value);

    if Pair.JsonValue is TJSONArray then
      AddJsonArrayToXml(SubNode, TJSONArray(Pair.JsonValue))
    else if Pair.JsonValue is TJSONObject then
      AddJsonObjectToXml(SubNode, TJSONObject(Pair.JsonValue))
    else
      SubNode.Value := Pair.JsonValue.Value;
  end;
end;

class function TNewResUtil.JsonToXML(const AJSONValue: TJSONValue): String;
var
  nativeXML : TNativeXml;
begin
  nativeXML := TNativeXml.CreateName('root');
  try
    if AJSONValue is TJSONObject then
      AddJsonObjectToXml(nativeXML.Root, TJSONObject(AJSONValue))
    else if AJSONValue is TJSONArray then
      AddJsonArrayToXml(nativeXML.Root, TJSONArray(AJSONValue))
    else
    begin
      nativeXML.Root.NodeNew('value').Value := AJSONValue.Value;
    end;

    nativeXML.SaveToString(Result);
  finally
    nativeXML.Free;
  end;
end;

class function TNewResUtil.JsonToXML(const AJSONObj: TJSONObject): String;
begin
  Result := JsonToXML(TJSONValue(AJSONObj));
end;

function FindNodeBySimplePath(ARoot: TXmlNode; const APath: String): TXmlNode;
var
  Parts: TArray<String>;
  Node: TXmlNode;
  i: Integer;
begin
  Result := nil;
  if not Assigned(ARoot) or (APath = '') then
    Exit;

  Parts := APath.Split(['/']);
  Node := ARoot;

  for i := 0 to High(Parts) do
  begin
    Node := Node.FindNode(Parts[i]);
    if not Assigned(Node) then
      Exit(nil);
  end;
  Result := Node;
end;

class function TNewResUtil.ResultContentNodesInMap(const AMap: TArray<String>; const AContentXML: String): TArray<String>;
var
  XMLResponse : TNativeXml;
  ResponseStream: TStringStream;
  i: Integer;
  PathTags: String;
  FullNodePath: String;
  S: string;
  Node: TXmlNode;
  ValuesFound: TList<String>;
  ValueResult: String;
begin
  SetLength(Result, 0);
  ValuesFound := TList<String>.Create;
  try
    XMLResponse := TNativeXml.Create(nil);
    try
      ResponseStream := TStringStream.Create(AContentXML, TEncoding.UTF8);
      try
        XMLResponse.LoadFromStream(ResponseStream);
      finally
        ResponseStream.Free;
      end;

      for i := Low(AMap) to High(AMap) do
      begin
        PathTags := AMap[i];

        if PathTags.Contains(';') then
        begin
          var SplitMap := PathTags.Split([';']);
          if Length(SplitMap) >= 2 then
          begin
            var PrePath := SplitMap[0];
            var FinalTag := SplitMap[1];

            Node := FindNodeBySimplePath(XMLResponse.Root, PrePath);
            if Assigned(Node) then
            begin
              var ChildNode := Node.FindNode(FinalTag);
              if Assigned(ChildNode) then
                ValueResult := ChildNode.Value
              else
                ValueResult := '';
            end
            else
              ValueResult := '';
          end
          else
            ValueResult := '';
        end
        else
        begin
          Node := FindNodeBySimplePath(XMLResponse.Root, PathTags);
          if Assigned(Node) then
            ValueResult := Node.Value
          else
            ValueResult := '';
        end;

        ValuesFound.Add(ValueResult);
      end;

    finally
      XMLResponse.Free;
    end;

    Result := ValuesFound.ToArray;
  finally
    ValuesFound.Free;
  end;
end;

end.

