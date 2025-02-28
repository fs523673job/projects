unit unJsonToXML;

interface

uses
  System.Classes,
  System.SysUtils,
  StrUtils,
  System.Json,
  System.Generics.Collections,
  RegularExpressions
  ;

type
  TRestUtils = class
  private
  public
    class function JsonToXML(AJSONValue: TJSONValue): String; overload;
    class function JsonToXML(AJSONObj: TJSONObject): String; overload;
    class function ConvertJSONValueToJSONObject(const AJSONContent: String; const AIsUTF8: Boolean = False): TJSONObject;
    class function ConvertJSONObjectToStringStream(const AJSONObj: TJSONObject): TStringStream;
    class function ResultContentNodesInMap(const AMap: TArray<String>; const AContentXML: String): TArray<String>;
  end;

  TTagsXML = class(TObject)
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

class function TRestUtils.JsonToXML(AJSONObj: TJSONObject): String;
begin
  Result := '';
end;


class function TRestUtils.ResultContentNodesInMap(const AMap: TArray<String>; const AContentXML: String): TArray<String>;
var
  ConnId                 : Integer;
  CdiModeloIntegracao    : Integer;
  CdiModeloIntegracaoCmd : Integer;
  IdLastLogsIntegracoes  : Integer;
  Response               : TStringStream;
  IsSOAPResponse         : Boolean;
  XMLResponse            : TNativeXML;
  XMLNode                : TXmlNode;
  XMLListNodes           : TList;
  StrMapXml              : TArray<String>;
  StrTagsNames           : TArray<String>;
  StrTagsChild           : TArray<String>;
  StrTagsDeepChild       : TArray<String>;
  c, i, x                : Integer;
  KeyLogsIntegracoesRets : Integer;
  XMLNodesStructuresRep  : TDictionary<String, TTagsXML>;
  XMLNodesStructures     : TDictionary<String, TTagsXML>;
  TagsStructureRepXML    : TTagsXML;
  TagsStructureXML       : TTagsXML;
  TagName                : String;
  TagParent              : String;
  PathTags               : String;
  Key                    : String;
  Key2                   : String;
  ContentString          : String;
  jsonObject             : TJSONObject;
  ConcatXML              : Boolean;
  FullPathXml            : String;
  ChildPathXml           : String;
begin
  try
    Response := TStringStream.Create;
    try
      KeyLogsIntegracoesRets := 0;
      try
        ConcatXML := False;

        if not IsSOAPResponse then
        begin
          Response.Position := 0;
          ContentString     := Response.DataString;
          jsonObject        := TRestUtils.ConvertJSONValueToJSONObject(ContentString);
          try
            ContentString     := TRestUtils.JsonToXML(jsonObject);

            Response.Clear;
            Response.WriteString(ContentString);
          finally
            if Assigned(jsonObject) then
              FreeAndNil(jsonObject);
          end;
        end;

        XMLResponse  := TNativeXml.Create(nil);
        XMLListNodes := TList.Create;
        try
          Response.Position := 0;
          XMLResponse.DefaultReadEncoding := seUTF8;
          XMLResponse.LoadFromStream(Response);

          XMLNodesStructures    := TDictionary<String, TTagsXML>.Create;
          XMLNodesStructuresRep := TDictionary<String, TTagsXML>.Create;

          while not QueryModelo.Eof do
          begin
            PathTags := QueryModelo.FieldByName('JWR_DssCampoOrigem').AsString;

            //Retro-compatibilidade de mapeamento
            PathTags := StringReplace(PathTags, 'ResponseArray', 'dataArray', [rfReplaceAll]);

            StrMapXml := PathTags.Split([';']);

            if (Length(StrMapXml) < 2) then
            begin
              QueryModelo.Next;
              Continue;
            end;

            if (Pos('/', StrMapXml[0]) > 0) then
            begin
              StrTagsNames     := StrMapXml[0].Split(['/']);
              StrTagsChild     := Copy(StrTagsNames, 1, Length(StrTagsNames) - 1);
              StrTagsDeepChild := Copy(StrMapXml, 1, Length(StrMapXml) - 1);

              XMLNode := FindNodeByNameAndChildren(XMLResponse.Root, StrTagsNames[0], StrTagsChild);
              if Assigned(XMLNode) then
                FullPathXml := Format('%s/%s', [XMLNode.FullPath, String.Join('/', StrTagsChild)])
              else
                FullPathXml := StrMapXml[0];

              if XMLNodesStructuresRep.Count = 0 then
              begin
                TagName   := String.Join('/', StrTagsChild);
                TagParent := StrTagsNames[0];

                TagsStructureRepXML := TTagsXML.Create(TagName, TagParent, FullPathXml);
                TagsStructureRepXML.CdiTransacao_Retorno    := QueryModelo.FieldByName('BBS_CdiTransacao_Retorno').AsInteger;
                TagsStructureRepXML.NuiTipoEdicao_Retorno   := QueryModelo.FieldByName('BBS_NuiTipoEdicao_Retorno').AsInteger;
                TagsStructureRepXML.CdiModeloIntegracao_Ret := QueryModelo.FieldByName('BBS_CdiModeloIntegracao_Ret').AsInteger;

                XMLNodesStructuresRep.Add(FullPathXml, TagsStructureRepXML);
              end
              else
                TagsStructureRepXML := XMLNodesStructuresRep[XMLNodesStructuresRep.Keys.ToArray[0]];

              StrTagsChild := Copy(StrMapXml, 1, Length(StrMapXml) - 1);
              ChildPathXml := String.Join(';', StrTagsChild);

              TagsStructureRepXML.AddTags(ChildPathXml, QueryModelo.FieldByName('JWR_DssCampoDestino').AsString, FullPathXml);
            end
            else
            begin
              TagName   := StrTagsNames[Length(StrTagsNames) - 1];
              TagParent := PathTags;

              Delete(TagParent, Length(TagParent) - Length(TagName), Length(TagName) + 1);

              XMLNodesStructures.TryGetValue(TagName, TagsStructureXML);

              if not Assigned(TagsStructureXML) then
              begin
                TagsStructureXML := TTagsXML.Create(TagName, TagParent, FullPathXml);
                TagsStructureXML.CdiTransacao_Retorno    := QueryModelo.FieldByName('BBS_CdiTransacao_Retorno').AsInteger;
                TagsStructureXML.NuiTipoEdicao_Retorno   := QueryModelo.FieldByName('BBS_NuiTipoEdicao_Retorno').AsInteger;
                TagsStructureXML.CdiModeloIntegracao_Ret := QueryModelo.FieldByName('BBS_CdiModeloIntegracao_Ret').AsInteger;

                XMLNodesStructures.Add(TagName, TagsStructureXML);
              end;

              TagsStructureXML.AddTags(PathTags, QueryModelo.FieldByName('JWR_DssCampoDestino').AsString, FullPathXml);
            end;

            QueryModelo.Next;
          end;

          if (XMLNodesStructuresRep.Count > 0) then
          begin
            for Key in XMLNodesStructuresRep.Keys do
            begin
              XMLNodesStructuresRep.TryGetValue(Key, TagsStructureRepXML);

              if Assigned(TagsStructureRepXML) then
              begin
                XMLNode := XMLResponse.Root.FindNode(TagsStructureRepXML.TagParent);

                if not Assigned(XMLNode) then
                begin
                  if AnsiSameText(XMLResponse.Root.Name, TagsStructureRepXML.TagParent) then
                    XMLNode := XMLResponse.Root
                  else
                    Continue;
                end;

                XMLListNodes.Clear;

                XMLNode.FindNodes(Format('%s/%s', [XMLNode.FullPath, TagsStructureRepXML.TagName]), XMLListNodes);

                if (XMLListNodes.Count - 1 < 0) then
                  XMLNode.FindNodes(Format('/%s/%s', [TagsStructureRepXML.TagParent, TagsStructureRepXML.TagName]), XMLListNodes);

                for c := 0 to XMLListNodes.Count - 1 do
                begin
                  QueryInsertLog.ParamByName('pCdiLogIntegracao').AsInteger        := IdLastLogsIntegracoes;
                  QueryInsertLog.ParamByName('pNuiRegistro').AsInteger             := c + 1;
                  QueryInsertLog.ParamByName('pCdiTransacao_Retorno').AsInteger    := TagsStructureRepXML.CdiTransacao_Retorno;
                  QueryInsertLog.ParamByName('pNuiTipoEdicao_Retorno').AsInteger   := TagsStructureRepXML.NuiTipoEdicao_Retorno;
                  QueryInsertLog.ParamByName('pCdiModeloIntegracao_Ret').AsInteger := TagsStructureRepXML.CdiModeloIntegracao_Ret;
                  QueryInsertLog.ParamByName('pCdiStatusProcArquivo').AsInteger    := 1;
                  QueryInsertLog.ParamByName('pDsbArquivoXML').AsString            := TXmlNode(XMLListNodes.Items[c]).FullPath + #13#10 + #13#10 + TXmlNode(XMLListNodes.Items[c]).WriteToString;

                  KeyLogsIntegracoesRets := NewAutoIncrementInsert(QueryInsertLog, 'LogsIntegracoesRets', 0);

                  if (KeyLogsIntegracoesRets > 0) then
                  begin
                    for i := 0 to TagsStructureRepXML.TagListOrigem.Count - 1 do
                    begin
                      if (Pos(';', TagsStructureRepXML.TagListOrigem[i]) > 0) then
                      begin
                        StrTagsNames := TagsStructureRepXML.TagListOrigem[i].Split([';']);

                        XMLNode := TXmlNode(XMLListNodes.Items[c]).FindNode(StrTagsNames[0]);

                        if Assigned(XMLNode) then
                        begin
                          for x := 1 to Length(StrTagsNames) - 1 do
                          begin
                            XMLNode := XMLNode.FindNode(StrTagsNames[x]);

                            if not Assigned(XMLNode) then
                              Break;
                          end;
                        end;
                      end
                      else
                      begin
                        XMLNode := TXmlNode(XMLListNodes.Items[c]).FindNode(TagsStructureRepXML.TagListOrigem[i]);
                        if Assigned(XMLNode) then
                        begin
                          if not AnsiSameText(XMLNode.FullPath, TXmlNode(XMLListNodes.Items[c]).FullPath + '/' + TagsStructureRepXML.TagListOrigem[i]) then
                          begin
                            XMLNode := TXmlNode(XMLListNodes.Items[c]).FindNode(TXmlNode(XMLListNodes.Items[c]).FullPath + '/' + TagsStructureRepXML.TagListOrigem[i]);
                          end;
                        end
                        else
                          XMLNode := XMLResponse.Root.FindNode(TagsStructureRepXML.ListFullPath[i] + '/' + TagsStructureRepXML.TagListOrigem[i]);
                      end;

                      if Assigned(XMLNode) then
                      begin
                        QueryInsertLogCampos.ParamByName('pCdiLogIntegracaoRet').AsInteger  := KeyLogsIntegracoesRets;
                        QueryInsertLogCampos.ParamByName('pDssCampo').AsString              := TagsStructureRepXML.TagListDestino[i];
                        QueryInsertLogCampos.ParamByName('pDssValor').AsString              := XMLNode.ValueUnicode;

                        NewAutoIncrementInsert(QueryInsertLogCampos, 'LogsIntegracoesRetsCampos', 0);
                      end;
                    end;

                    for Key2 in XMLNodesStructures.Keys do
                    begin
                      XMLNodesStructures.TryGetValue(Key2, TagsStructureXML);

                      if Assigned(TagsStructureXML) then
                      begin
                        XMLNode := XMLResponse.Root.FindNode(TagsStructureXML.TagParent);

                        if Assigned(XMLNode) then
                          XMLNode := XMLNode.FindNode(TagsStructureXML.TagName);

                        if Assigned(XMLNode) then
                        begin
                          QueryInsertLogCampos.ParamByName('pCdiLogIntegracaoRet').AsInteger  := KeyLogsIntegracoesRets;
                          QueryInsertLogCampos.ParamByName('pDssCampo').AsString              := TagsStructureXML.TagListDestino[0];
                          QueryInsertLogCampos.ParamByName('pDssValor').AsString              := XMLNode.ValueUnicode;

                          NewAutoIncrementInsert(QueryInsertLogCampos, 'LogsIntegracoesRetsCampos', 0);
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end
          else if (XMLNodesStructures.Count > 0) then
          begin
            for Key in XMLNodesStructures.Keys do
            begin
              XMLNodesStructures.TryGetValue(Key, TagsStructureXML);

              if Assigned(TagsStructureXML) then
              begin
                XMLNode := XMLResponse.Root.FindNode(TagsStructureXML.TagParent);

                if not Assigned(XMLNode) then
                begin
                  if AnsiSameStr(XMLResponse.Root.Name, TagsStructureXML.TagParent) then
                    XMLNode := XMLResponse.Root.Nodes[0]
                  else
                    Continue;
                end;

                if (KeyLogsIntegracoesRets = 0) then
                begin
                  QueryInsertLog.ParamByName('pCdiLogIntegracao').AsInteger        := IdLastLogsIntegracoes;
                  QueryInsertLog.ParamByName('pNuiRegistro').AsInteger             := 1;
                  QueryInsertLog.ParamByName('pCdiTransacao_Retorno').AsInteger    := TagsStructureXML.CdiTransacao_Retorno;
                  QueryInsertLog.ParamByName('pNuiTipoEdicao_Retorno').AsInteger   := TagsStructureXML.NuiTipoEdicao_Retorno;
                  QueryInsertLog.ParamByName('pCdiModeloIntegracao_Ret').AsInteger := TagsStructureXML.CdiModeloIntegracao_Ret;
                  QueryInsertLog.ParamByName('pCdiStatusProcArquivo').AsInteger    := 1;
                  QueryInsertLog.ParamByName('pDsbArquivoXML').AsString            := XMLNode.FullPath + #13#10 + #13#10 + XMLNode.WriteToString;

                  KeyLogsIntegracoesRets := NewAutoIncrementInsert(QueryInsertLog, 'LogsIntegracoesRets', 0);
                end;

                if (KeyLogsIntegracoesRets > 0) then
                begin
                  XMLNode := XMLResponse.Root.FindNode(TagsStructureXML.TagName);

                  if not Assigned(XMLNode) then
                  begin
                    if AnsiSameStr(XMLResponse.Root.Name, TagsStructureXML.TagName) then
                      XMLNode := XMLResponse.Root.Nodes[0]
                    else
                      Continue;
                  end;

                  QueryInsertLogCampos.ParamByName('pCdiLogIntegracaoRet').AsInteger  := KeyLogsIntegracoesRets;
                  QueryInsertLogCampos.ParamByName('pDssCampo').AsString              := TagsStructureXML.TagListDestino[0];
                  QueryInsertLogCampos.ParamByName('pDssValor').AsString              := XMLNode.ValueUnicode;

                  NewAutoIncrementInsert(QueryInsertLogCampos, 'LogsIntegracoesRetsCampos', 0);

                  if ConcatXML then
                  begin
                    QuerySelectLogCampos.Close;
                    QuerySelectLogCampos.ParamByName('pCdiLogIntegracaoRet').AsInteger := KeyLogsIntegracoesRets;
                    QuerySelectLogCampos.Open;

                    QueryUpdateLog.ParamByName('pDsbArquivoXML').AsString        := QuerySelectLogCampos.FieldByName('JWS_DsbArquivoXML').AsString + #13#10 + #13#10 + XMLNode.FullPath + #13#10 + #13#10 + XMLNode.WriteToString;
                    QueryUpdateLog.ParamByName('pCdiLogIntegracaoRet').AsInteger := KeyLogsIntegracoesRets;
                    QueryUpdateLog.ExecSQL;
                  end;
                end;
                ConcatXML := True;
              end;
            end;
          end;
        finally
          XMLResponse.Free;
          XMLListNodes.Free;
        end;
      finally
        DeleteQuery(QueryModelo, ConnId);
        DeleteQuery(QueryInsertLog, ConnId);
        DeleteQuery(QueryInsertLogCampos, ConnId);
        DeleteQuery(QueryUpdateLog, ConnId);
        DeleteQuery(QuerySelectLogCampos, ConnId);
      end;
    finally
      Response.Free;
    end;
  finally
    ReleaseDBSharedConn(ConnId);
  end;
end;

{ TTagsXML }

procedure TTagsXML.AddTags(const ATagOrigem, ATagDestino, AFullPath: String);
begin
  FTagListOrigem.Add(ATagOrigem);
  FTagListDestino.Add(ATagDestino);
  FListFullPath.Add(AFullPath);
end;

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
end;

end.
