unit unScheduleXmlToXml;

interface

uses
  Classes, BufferDef, NativeXML;

  type
    TScheduleSoftInt = class;

    TScheduleSoftInt = class(TObject)
    strict private
      FXML: TNativeXml;
      FTransactionCount: Integer;
      class function GetParamsNode(const ATransaction: TXmlNode): TXmlNode;
      class function GetFuncionParamsNode(const ATransaction: TXmlNode): TXmlNode;
    private

      function GetTransactionsNode: TXmlNode;
      function AddTransaction(const AWFForm: Integer = 0; const ATransactionID: Integer = 80092;  const AIntegrationMode: Integer = 10002): TXmlNode;
      procedure AddField(const ATransaction: TXmlNode; const AFieldName: string; const ADataType: TRPCFieldType; const AValue: Variant);
      function GetXMLFile: TNativeXml;
      procedure ProcessFromFile(const AFileName: String);
    protected
      procedure Process(const AFileName: String); virtual;
    public
      procedure InternalProcessExecute; 
      function FindClasse: boolean; 
    end;

implementation

uses
  Windows, SysUtils, Math, DateUtils, DB, StrUtils, RxStrUtils;

procedure TScheduleSoftInt.AddField(const ATransaction: TXmlNode; const AFieldName: string; const ADataType: TRPCFieldType; const AValue: Variant);
var
  FParamCount : Integer;
begin
  with GetParamsNode(ATransaction) do
  begin
    FParamCount := NodeCount;
    with NodeNew('Param') do
    begin
      AttributeAdd('Id', FParamCount);
      NodeNew('ClientFieldName').ValueAsString := AFieldName;
      with NodeNew('FieldValue') do
      begin
        case ADataType of
          dtWord,
            dtInteger,
            dtByte: ValueAsInteger := AValue;
          dtLargeint: ValueAsInt64 := AValue;
          dtDateTime,
            dtTime,
            dtDate: ValueAsDateTime := AValue;
          dtExtended,
            dtCurrency: ValueAsFloat := AValue;
          dtBoolean: ValueAsBool := AValue;
        else
          ValueAsString := AValue;
        end;
      end;
    end;
  end;
end;

function TScheduleSoftInt.AddTransaction(const AWFForm, ATransactionID, AIntegrationMode: Integer): TXmlNode;
begin
  Result := GetTransactionsNode.NodeNew('ApTransaction');
  with Result do
  begin
    AttributeAdd('Id', FTransactionCount);
    NodeNew('FunctionName').ValueAsString := 'ExecuteTransaction';
    with NodeNew('FunctionParameters') do
    begin
      NodeNew('TransactionId').ValueAsInteger := ATransactionID;
      NodeNew('LanguageId').ValueAsInteger := 0;
      NodeNew('EditionType').ValueAsInteger := AWFForm;
      NodeNew('Params');
      NodeNew('IntegrationMode').ValueAsInteger := AIntegrationMode;
    end;
    Inc(FTransactionCount);
  end;
end;

function TScheduleSoftInt.FindClasse: boolean;
begin
  Result := True;
end;

class function TScheduleSoftInt.GetFuncionParamsNode(
  const ATransaction: TXmlNode): TXmlNode;
begin
  Result := ATransaction.NodeByName('FunctionParameters');
end;

class function TScheduleSoftInt.GetParamsNode(const ATransaction: TXmlNode): TXmlNode;
begin
  Result := GetFuncionParamsNode(ATransaction).NodeByName('Params');
end;

function TScheduleSoftInt.GetTransactionsNode: TXmlNode;
begin
  Result := GetXMLFile.Root.Nodes[0].Nodes[0];
end;

function TScheduleSoftInt.GetXMLFile: TNativeXml;
begin
  if not Assigned(FXML) then
  begin
    FXML := TNativeXml.Create;
    FXML.Root.Name := 'root';
    FXML.Root.NodeNew('ApIntegrationServerFileExecute').NodeNew('ApTransactions');
    FTransactionCount := 0;
  end;
  Result := FXML;
end;

procedure TScheduleSoftInt.InternalProcessExecute;
var
  FFileList: TStringList;
  I: Integer;
  FFile: string;
  procedure BuildFileList;
  var
    FSearchRec: TSearchRec;
  begin
    if FindFirst('ParamAsString[DirInput]', faAnyFile, FSearchRec) = 0 then
    begin
      repeat
        FFileList.Add(FormatDateTime('yyyymmddhhnnsszzz', FileDateToDateTime(FSearchRec.Time)) +
          ';' +
          IncludeTrailingBackslash(ExtractFileDir(('ParamAsString[DirInput]'))) + ExtractFileName(FSearchRec.Name));
      until FindNext(FSearchRec) <> 0;
      FindClose(FSearchRec);
    end;
  end;
begin
  FFileList := TStringList.Create;
  try
    BuildFileList;
    FFileList.Sorted := True;
    for I := 0 to FFileList.Count - 1 do
    begin
      FFile := Extractword(2, FFileList[I], [';']);
      FXML := nil;
      try
        try
          Process(FFile);
          if Assigned(FXML) then
            FXML.SaveToFile(ChangeFileExt(
              IncludeTrailingBackslash('ParamAsString[DirOutput]') +
              ExtractFileName(FFile),
              '.xml'
            ));
          MoveFile(PChar(FFile), PChar(IncludeTrailingBackslash('ParamAsString[DirOutput]') + ExtractFileName(FFile)));
        except
          on e: Exception do
          begin
            MoveFile(PChar(FFile), PChar(IncludeTrailingBackslash('ParamAsString[DirOutputFail]') + ExtractFileName(FFile)));
          end;
        end;
      finally
        if Assigned(FXML) then
          FreeAndNil(FXML);
      end;
    end;
  finally
    FFileList.Free;
  end;
end;

procedure TScheduleSoftInt.ProcessFromFile(const AFileName: String);
const
  SHEADER: string = '#HEADER';
var
  I: Integer;
  FTransaction: TXmlNode;
  FDateFormat, FTimeFormat: string;
  FInterfaceTransaction: Integer;
  FWorkFlowForm: Integer;
  FIntegrationModel: Integer;
  FFile, FHeaderFile: TStringList;
  FHeaderFileName: string;
  FColumns: TStringList;

  function GetFieldValue(const ALine: string; const AIndex: Integer): string;
  begin
    Result := ExtractDelimited(AIndex, ALine, [';']);
  end;


  function GetDateParam(const ALine: string; const AIndex: Integer): TDateTime;
  begin
    //Result := StrToDateFmtDef(FDateFormat, GetFieldValue(ALine, AIndex), 0);
  end;

  function MakeUTCTime(DateTime: TDateTime): TDateTime;
  var
    TZI: TTimeZoneInformation;
  begin
    case GetTimeZoneInformation(TZI) of
      TIME_ZONE_ID_STANDARD, TIME_ZONE_ID_UNKNOWN: begin
                               Result := DateTime + (TZI.Bias/60/24);
                               if Result < 0 then
                                 Result := 1 + Result;
                             end;
      TIME_ZONE_ID_DAYLIGHT: begin
                               Result := DateTime + (TZI.Bias/60/24) + (TZI.DaylightBias/60/24);
                               if Result < 0 then
                                 Result := 1 + Result;
                             end;
    else raise Exception.Create('Error converting to UTC Time. Time zone could not be determined.');
    end;
  end;

  function GetDateTimeParam(const ALine: string; const AIndex: Integer; const AMask: string = ''): TDateTime;
  var
    FMask: string;
  begin
    FMask := IfThen(AMask <> '', AMask, FDateFormat + ' ' + FTimeFormat);
    //Result := MakeUTCTime(StrToDateTimeFmtDef(FMask, GetFieldValue(ALine, AIndex), 0));
  end;

  function GetTimeParam(const ALine: string; const AIndex: Integer): TDateTime;
  begin
    Result := GetDateTimeParam(ALine, AIndex, FTimeFormat);
  end;

  function GetFieldValueAsType(const ALine: String; const AIndex: Integer; const ADataType: TRPCFieldType): variant;
  begin
    case ADataType of
      dtBoolean:
        begin
          Result := GetFieldValue(ALine, AIndex);
          Result := SameText(Result, 'True') or SameText(Result, '1') or SameText(Result, 'verdadeiro');
        end;
      dtExtended, dtCurrency: Result := StrToFloatDef(GetFieldValue(ALine, AIndex), 0);
      dtWord: Result := Word(StrToIntDef(GetFieldValue(ALine, AIndex), 0));
      dtByte: Result := Byte(StrToIntDef(GetFieldValue(ALine, AIndex), 0));
      dtTime: Result := GetTimeParam(ALine, AIndex);
      dtDate: Result := GetDateParam(ALine, AIndex);
      dtDateTime: Result := GetDateTimeParam(ALine, AIndex);
      dtInteger, dtLargeint: Result := StrToIntDef(GetFieldValue(ALine, AIndex), 0);
      else Result := GetFieldValue(ALine, AIndex);
    end;
  end;

  function StringFieldTypeAsType(const AFieldType: string): TRPCFieldType;
  begin
    if SameText(AFieldType, 'DateTime') then
      Result := dtDateTime
    else if SameText(AFieldType, 'Date') then
      Result := dtDate
    else if SameText(AFieldType, 'Time') then
      Result := dtTime
    else if SameText(AFieldType, 'Byte') then
      Result := dtByte
    else if SameText(AFieldType, 'Word') then
      Result := dtWord
    else if SameText(AFieldType, 'Integer') then
      Result := dtInteger
    else if SameText(AFieldType, 'Extended') or SameText(AFieldType, 'Float') then
      Result := dtExtended
    else if SameText(AFieldType, 'Boolean') then
      Result := dtBoolean
    else Result := dtString;
  end;

  procedure AddColumnInfo(const AColumnName: string; const AFieldType: TRPCFieldType; const AColumnIndex: Integer; const AIgnoreWhenEmpty: boolean);
  begin
    FColumns.Add(Format('%0:s;%1:d;%2:d;%3:d',
                  [
                    AColumnName,
                    Ord(AFieldType),
                    AColumnIndex,
                    Integer(AIgnoreWhenEmpty)
                  ]));
  end;
var
  FFieldType: TRPCFieldType;
  J, FFieldIndex: Integer;
  FFieldValue: string;
  FIgnoreWhenEmpty: boolean;
begin
  FFile := TStringList.Create;
  FColumns := TStringList.Create;
  try
    with FFile do
    begin
      LoadFromFile(AFileName);
      if SameText(Copy(Strings[0],1, Length(SHEADER)), SHEADER) then
      begin
        FHeaderFileName := Copy(Strings[0], Length(SHEADER) + 2, MaxInt);
        if FHeaderFileName <> '' then
        begin
          FHeaderFile := TStringList.Create;
          FHeaderFile.LoadFromFile(FHeaderFileName);
        end else FHeaderFile := FFile;
        try
          Delete(0);
          with FHeaderFile do
          begin
            FDateFormat := GetFieldValue(Strings[0], 1);
            FTimeFormat := GetFieldValue(Strings[0], 2);
            FWorkFlowForm := GetFieldValueAsType(Strings[0], 3, dtInteger);
            FInterfaceTransaction := GetFieldValueAsType(Strings[0], 4, dtInteger);
            FIntegrationModel := GetFieldValueAsType(Strings[0], 5, dtInteger);
            Delete(0);
            while (Count > 0) do
            begin
              try
                AddColumnInfo(
                  GetFieldValue(Strings[0], 1),
                   StringFieldTypeAsType(GetFieldValue(Strings[0], 2)),
                   GetFieldValueAsType(Strings[0], 3, dtInteger),
                   GetFieldValueAsType(Strings[0], 4, dtInteger)
                );
                if SameText(Copy(Strings[0],1, Length(SHEADER)), SHEADER) then
                  break;
              finally
                Delete(0);
              end;
            end;
          end;
        finally
          if FHeaderFileName <> '' then
            FHeaderFile.Free;
        end;
      end else //Process Fix previous file
      begin
        FDateFormat := 'mm/dd/yyyy';
        FTimeFormat := 'hh:nn';
        FWorkFlowForm := 0;
        FInterfaceTransaction := 80092;
        FIntegrationModel := 10002;
        AddColumnInfo('Contractor', dtInteger, 1, false);
        AddColumnInfo('JobStartDate', dtDate, 2, false);
        AddColumnInfo('JobStartTime', dtTime, 3, false);
        AddColumnInfo('JobEndDate', dtDate, 4, false);
        AddColumnInfo('JobEndTime', dtTime, 5, false);
        AddColumnInfo('JobCode', dtString, 6, false);
        AddColumnInfo('ReferenceShiftCode', dtString, 7, false);
      end;
      for I := 0 to Count - 1 do
      begin
        FTransaction := AddTransaction(FWorkFlowForm, FInterfaceTransaction, FIntegrationModel);
        for J := 0 to FColumns.Count - 1 do
        begin
          FFieldType := TRPCFieldType(Integer(GetFieldValueAsType(FColumns[J], 2, dtInteger)));
          FFieldIndex := Integer(GetFieldValueAsType(FColumns[J], 3, dtInteger));
          FFieldValue := GetFieldValue(Strings[I], FFieldIndex);
          FIgnoreWhenEmpty := Boolean(GetFieldValueAsType(FColumns[J], 4, dtInteger));

          if (Trim(FFieldValue) <> '') or (not FIgnoreWhenEmpty) then
          begin
            AddField(FTransaction,
              GetFieldValue(FColumns[J], 1),
              FFieldType,
              GetFieldValueAsType(Strings[I],FFieldIndex, FFieldType));
          end;
        end;
      end;
    end;
  finally
    FFile.Free;
    FColumns.Free;
  end;
end;

procedure TScheduleSoftInt.Process(const AFileName: String);
begin
  ProcessFromFile(AFileName);
end;

end.
