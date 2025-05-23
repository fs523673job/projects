unit NewScheduleXmlToXml;

interface

uses
  Classes, NativeXML, StdCtrls, StrUtils;

type
  TConvertXmlToNewXml = class(TObject)
  private
    FXML      : TNativeXML;
    FListTags : TStringList;
    procedure HierarquiaTagsFilhos(ANode: TXmlNode; const ANoPai: String);
    procedure TagsToCsv(ANode: TXmlNode; var ALine: WideString); overload;
    procedure TagsToCsv(ANode: TXmlNode; var ALine: WideString; const AHeader: TStringList); overload;
    procedure NewTagsToCsv(ANode: TXmlNode; var ALine: WideString; const AHeader: TStringList);
    procedure NewTagsToCsv2(ANode: TXmlNode; var ALine: WideString; const AHeader: TStringList);
    function GetNodeHierarquia(ANode: TXmlNode; const ATags: TStringList; AIndex: Integer): TXmlNode;
    procedure NewTagsToCsv3(ANode: TXmlNode; var ALine: WideString; const AHeader: TStringList; ACSV: TStringList);
    function Adjust(var AHeader: String; out AReturnStr: String; ADelimiter: String): Boolean;
    procedure AddNodeListField(var ANodeListField: String; var ANodeList: TStringList);
    function ClearColumnType(AValue: String): String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure HierarquiaTags(AFileName: String; AMemo: TMemo);
    procedure ConvertXMLToCSV(AFileName: String; AMemo: TMemo); overload;
    procedure ConvertXMLToCSV(AFileName: String; AMemo: TMemo; const AHeader: TStringList); overload;
    procedure RetornaColunas(const AHeader: TStringList; var ALine: String);
    procedure RetornaColunasAlias(const AHeader: TStringList; var ALine: String);
    procedure RetornaTipos(const AHeader: TStringList; var ALine: String);
  end;


implementation

uses
  SysUtils;

{ TConvertXmlToNewXml }

constructor TConvertXmlToNewXml.Create;
begin
  FXML      := TNativeXML.Create;
  FListTags := TStringList.Create;
end;

destructor TConvertXmlToNewXml.Destroy;
begin
  FXML.Free;
  FListTags.Free; 
  inherited;
end;

procedure TConvertXmlToNewXml.HierarquiaTagsFilhos(ANode: TXmlNode; const ANoPai: String);
var
  c : Integer;
  strAux: String;
begin
  if Assigned(ANode) then
  begin
    for c := 0 to ANode.NodeCount - 1 do
    begin
      strAux := Format('%s.%s', [ANoPai, ANode.Nodes[c].Name]);
      if (FListTags.IndexOf(strAux) = -1) then
        FListTags.Add(strAux);

      if (ANode.Nodes[c].NodeCount - 1 >= 0) then
        HierarquiaTagsFilhos(ANode.Nodes[c], strAux);
    end;
  end;
end;

function TConvertXmlToNewXml.GetNodeHierarquia(ANode: TXmlNode; const ATags: TStringList; AIndex: Integer): TXmlNode;
begin
  if (ANode <> nil) and ((AIndex >= 0) and (AIndex <= Pred(ATags.Count))) then
  begin
    Result := ANode.FindNode(ATags[AIndex]);
    if (Result <> nil) and (AIndex <> Pred(ATags.Count)) then
      Result := GetNodeHierarquia(Result, ATags, AIndex + 1);
  end
  else
    Result := nil; 
end;

procedure TConvertXmlToNewXml.NewTagsToCsv(ANode: TXmlNode; var ALine: WideString; const AHeader: TStringList);
var
  c      : Integer;
  strAux : TStringList;
  NodeXml : TXmlNode;
begin
  strAux := TStringList.Create;
  try
    NodeXml := nil;
    for c := 0 to AHeader.Count - 1 do
    begin
      strAux.Clear;
      
      ExtractStrings(['.'], [' '], PChar(AHeader.Strings[c]), strAux);

      NodeXml := GetNodeHierarquia(ANode, strAux, 1);

      if NodeXml <> nil then
        ALine := ALine + IfThen(ALine = '', '', ';') + NodeXml.ValueAsString
      else
        ALine := ALine + IfThen(ALine = '', '', ';') + 'Vazio';

      NodeXml.Delete;
    end;
  finally
    strAux.Free;
  end;
end;

procedure TConvertXmlToNewXml.NewTagsToCsv2(ANode: TXmlNode; var ALine: WideString; const AHeader: TStringList);
var
  c, x          : Integer;
  strAux        : TStringList;
  NodeXml       : TXmlNode;
  NodeList      : Boolean;
  NodeListField : String;
begin
  strAux := TStringList.Create;
  try
    NodeXml := nil;
    for c := 0 to AHeader.Count - 1 do
    begin
      strAux.Clear;

      NodeList := False;

      if (Pos('{', AHeader[c]) <> 0) then
      begin
        NodeList      := True;
        NodeListField := Copy(AHeader[c], Pos('{', AHeader[c]), Length(AHeader[c]));
        NodeListField := Copy(NodeListField, 2, Length(NodeListField) - 2);
        AHeader[c]    := Copy(AHeader[c], 1, Pos('{', AHeader[c]) - 1);
      end;
      
      ExtractStrings(['.'], [' '], PChar(AHeader.Strings[c]), strAux);

      for x := 0 to Pred(strAux.Count) do
      begin
        if (ANode.Name = strAux[x]) then
          Continue;
          
        NodeXml := ANode.FindNode(strAux[x]);

        if (NodeXml = nil) then
          Break;
      end;

      if NodeList and Assigned(NodeXml) then
      begin
        strAux.Clear;
        ExtractStrings([','], [' '], PChar(NodeListField), strAux);
        for x := 0 to NodeXml.NodeCount - 1 do
        begin
          NewTagsToCsv2(NodeXml.Nodes[x], ALine, strAux);
          ALine := ALine + #13#10;
        end;
      end;

      if NodeXml <> nil then
        ALine := ALine + IfThen(ALine = '', '', ';') + NodeXml.ValueAsString
      else
        ALine := ALine + IfThen(ALine = '', '', ';') + 'Vazio'
    end;
  finally
    strAux.Free;
  end;
end;

function TConvertXmlToNewXml.Adjust(var AHeader: String; out AReturnStr: String; ADelimiter: String) : Boolean;
var
  Index: Integer;
begin
  Index  := Pos(ADelimiter, AHeader);
  Result := Index <> 0;
  if (Result) then
  begin
    AReturnStr := Copy(AHeader, Index, Length(AHeader));
    AReturnStr := Copy(AReturnStr, 2, Length(AReturnStr) - 2);
    AHeader    := Copy(AHeader, 1, Index - 1);
  end
  else
    AReturnStr := '';
end;

procedure TConvertXmlToNewXml.AddNodeListField(var ANodeListField: String; var ANodeList: TStringList);
var
  c        : Integer;
  Index    : Integer;
  NodeRept : String;
begin
  ANodeList.Clear;
  Index := Pos('{', ANodeListField);
  if (Index <> 0) then
  begin
    while Index > 0 do
    begin
      if (ANodeListField[Index] <> ';') then
        Dec(Index)
      else
        Break;
    end;

    NodeRept := Copy(ANodeListField, Index + 1, Length(ANodeListField));
    Delete(ANodeListField, Index, Length(ANodeListField));
  end;

  ExtractStrings([';'], [' '], PChar(ANodeListField), ANodeList);
  if NodeRept <> '' then
    ANodeList.Add(NodeRept);
end;

procedure TConvertXmlToNewXml.NewTagsToCsv3(ANode: TXmlNode; var ALine: WideString; const AHeader: TStringList; ACSV: TStringList);
var
  c, x          : Integer;
  strAux        : TStringList;
  ListNode      : TList;
  XmlNode       : TXmlNode;
  strHeader     : String;
  AttribName    : String;
  NodeList      : Boolean;
  NodeListField : String;
  LocalCSV      : TStringList;
  TmpLine       : WideString;
  NodeAttrib    : Boolean;
begin
  strAux := TStringList.Create;
  try
    ListNode := TList.Create;
    try
      XmlNode := nil;
      
      for c := 0 to AHeader.Count - 1 do
      begin
        strAux.Clear;

        strHeader := ClearColumnType(AHeader[c]); //Copy(AHeader[c], 1, Pos('#', AHeader[c]) - 1);

        NodeList   := Adjust(strHeader, NodeListField, '{');
        NodeAttrib := Adjust(strHeader, AttribName, '[');

        ExtractStrings(['.'], [' '], PChar(strHeader), strAux);

        XmlNode := ANode;
        
        for x := 0 to strAux.Count - 1 do
        begin
          if (strAux[x] = XmlNode.Name) and (not (((x - 1) > 0) and (strAux[x - 1] = XmlNode.Name))) then 
            Continue;

          XmlNode := XmlNode.FindNode(strAux[x]);

          if not Assigned(XmlNode) then
            Break;
        end;

        if Assigned(XmlNode) then
        begin
          if (NodeList) then
          begin
            AddNodeListField(NodeListField, strAux);

            LocalCSV := TStringList.Create;
            try
              for x := 0 to XmlNode.NodeCount - 1 do
              begin
                TmpLine := ALine;
                NewTagsToCsv3(XmlNode.Nodes[x], TmpLine, strAux, LocalCSV);
              end;

              for x := 0 to LocalCSV.Count - 1 do
                ACSV.Add(LocalCSV[x]);

              ALine := '';
            finally
              LocalCSV.Free;
            end;
          end
          else
          begin
            if (NodeAttrib) then
              ALine := ALine + IfThen(ALine = '', '', ';') + XmlNode.AttributeByName[AttribName]
            else
              ALine := ALine + IfThen(ALine = '', '', ';') + XmlNode.ValueAsString
          end;
        end
        else
          ALine := ALine + IfThen(ALine = '', '', ';') + ''
      end;

      if (ALine <> '') then
        ACSV.Add(ALine);
    finally
      ListNode.Free;
    end;
  finally
    strAux.Free;
  end;
end;

function TConvertXmlToNewXml.ClearColumnType(AValue: String): String;
var
  Index : Integer;
begin
  Result := AValue;
  Index := Pos('#', AValue);
  if (Index <> 0) then
    Result := Copy(AValue, 1, Index -1);
end;

procedure TConvertXmlToNewXml.RetornaColunas(const AHeader: TStringList; var ALine: String);
var
  c             : Integer;
  strHeader     : String;
  NodeListField : String;
  NodeList      : Boolean;
  strAux        : TStringList;
begin
  strAux := TStringList.Create;
  try
    for c := 0 to AHeader.Count - 1 do
    begin
      strHeader := ClearColumnType(AHeader[c]);
      NodeList  := Adjust(strHeader, NodeListField, '{');

      if NodeList then
      begin
        AddNodeListField(NodeListField, strAux);
        RetornaColunas(strAux, ALine);
      end
      else
        ExtractStrings(['.'], [' '], PChar(strHeader), strAux);

      if not (NodeList) then
        ALine := ALine + IfThen(ALine = '', '', ';') + strAux[strAux.Count - 1];
    end;
  finally
    strAux.Free;
  end;
end;

procedure TConvertXmlToNewXml.RetornaColunasAlias(const AHeader: TStringList; var ALine: String);
var
  c, x      : Integer;
  strHeader : String;
  strAux    : TStringList;
  Index     : Integer;
begin
  strAux := TStringList.Create;
  try
    for c := 0 to AHeader.Count - 1 do
    begin
      strAux.Clear;
      Index := Pos('@', AHeader[c]);
      if (Index <> 0) then
      begin
        strHeader := Copy(AHeader[c], Index + 1, Length(AHeader[c]));

        ExtractStrings([';'], [' '], PChar(strHeader), strAux);

        for x := 0 to strAux.Count - 1 do
          ALine := ALine + IfThen(ALine = '', '', ';') + strAux[x];
      end
      else
        ALine := ALine + IfThen(ALine = '', '', ';') + 'Vazio';
    end;
  finally
    strAux.Free;
  end;
end;

procedure TConvertXmlToNewXml.RetornaTipos(const AHeader: TStringList; var ALine: String);
var
  c, x          : Integer;
  strHeader     : String;
  NodeListField : String;
  NodeList      : Boolean;
  strAux        : TStringList;
begin
  strAux := TStringList.Create;
  try
    for c := 0 to AHeader.Count - 1 do
    begin
      strHeader := Copy(AHeader[c], Pos('#', AHeader[c]) + 1, Length(AHeader[c]));

      strAux.Clear;
      ExtractStrings([';'], [' '], PChar(strHeader), strAux);

      for x := 0 to strAux.Count - 1 do
        ALine := ALine + IfThen(ALine = '', '', ';') + strAux[x];
    end;
  finally
    strAux.Free;
  end;
end;


procedure TConvertXmlToNewXml.TagsToCsv(ANode: TXmlNode; var ALine: WideString; const AHeader: TStringList);
var
  c, x          : Integer;
  strAux        : TStringList;
  xmlNode       : TXmlNode;
  AttributeName : String;
begin
  strAux := TStringList.Create;
  try
    for c := 0 to AHeader.Count - 1 do
    begin
      strAux.Clear;
      
      ExtractStrings(['.'], [' '], PChar(AHeader.Strings[c]), strAux);

      x := 0;
      xmlNode := nil;
      AttributeName := '';

      while (x <= Pred(strAux.Count)) do
      begin
        if (Pos('[', strAux[x]) <> 0) then
          AttributeName := Copy(strAux[x], 2, Length(strAux[x]) - 2)
        else
          xmlNode := ANode.FindNode(strAux[x]);

        Inc(x);
      end;

      if (xmlNode <> nil) then
      begin
        if AttributeName <> '' then
          ALine := ALine + IfThen(ALine = '', '', ';') + xmlNode.AttributeByName[AttributeName]
        else
          ALine := ALine + IfThen(ALine = '', '', ';') + xmlNode.ValueAsString;
      end
      else
        ALine := ALine + IfThen(ALine = '', '', ';') + 'Vazio';
    end;
  finally
    strAux.Free;
  end;
end;

procedure TConvertXmlToNewXml.TagsToCsv(ANode: TXmlNode; var ALine: WideString);
var
  c, x : Integer;
begin
  if Assigned(ANode) then
  begin
    for c := 0 to ANode.NodeCount - 1 do
    begin
      if (ANode.NodeCount - 1 >= 0) then
      begin
        TagsToCsv(ANode.Nodes[c], ALine);

        //Exceção para atributos, uma tag pode ter vários atributos que serão preenchidos com valores dos camps
        for x := 0 to ANode.AttributeCount - 1 do
          ALine := ALine + IfThen(ALine = '', '', ';') + ANode.AttributeValue[x];
      end;
    end;
    ALine := ALine + IfThen(ALine = '', '', ';') + ANode.ValueAsString;
  end;
end;

procedure TConvertXmlToNewXml.HierarquiaTags(AFileName: String; AMemo: TMemo);
var
  c : Integer;
begin
  FXML.LoadFromFile(AFileName);

  FListTags.Clear;

  for c := 0 to FXML.Root.NodeCount - 1 do
  begin
    if (FListTags.IndexOf(FXML.Root.Nodes[c].Name) = -1) then
      FListTags.Add(FXML.Root.Nodes[c].Name);

    HierarquiaTagsFilhos(FXML.Root.Nodes[c], FXML.Root.Nodes[c].Name);
  end;

  AMemo.Lines.Clear;
  AMemo.Lines.Assign(FListTags);
end;

procedure TConvertXmlToNewXml.ConvertXMLToCSV(AFileName: String; AMemo: TMemo); 
var
  c    : Integer;
  Line : WideString;
begin
  FXML.LoadFromFile(AFileName);

  FListTags.Clear;
  Line := '';
  
  for c := 0 to FXML.Root.NodeCount - 1 do
  begin
    if FXML.Root.Nodes[c].NodeCount - 1 >= 0 then
      TagsToCsv(FXML.Root.Nodes[c], Line);

    FListTags.Add(Line);
    Line := '';
  end;

  AMemo.Lines.Clear;
  AMemo.Lines.Assign(FListTags);
end;

procedure TConvertXmlToNewXml.ConvertXMLToCSV(AFileName: String; AMemo: TMemo; const AHeader: TStringList);
var
  c     : Integer;
  Lines : TStringList;
  Line  : WideString;
begin
  FXML.LoadFromFile(AFileName);

  FListTags.Clear;
  Lines := TStringList.Create;
  Line  := '';

  for c := 0 to FXML.Root.NodeCount - 1 do
  begin
    if FXML.Root.Nodes[c].NodeCount - 1 >= 0 then
    begin
      //NewTagsToCsv(FXML.Root.Nodes[c], Line, AHeader);
      //NewTagsToCsv2(FXML.Root.Nodes[c], Line, AHeader);
      NewTagsToCsv3(FXML.Root.Nodes[c], Line, AHeader, Lines)
    end;

    Line := '';
  end;

  AMemo.Lines.Clear;
  AMemo.Lines.Assign(Lines);
end;

end.
