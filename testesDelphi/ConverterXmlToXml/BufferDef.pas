unit BufferDef;
{$IFDEF CLR}
interface

uses System.IO, System.Text, System.XML, Classes, Variants, TypInfo, DB, SysUtils, StrUtils, Math, Clipbrd;

type
  TApBufferFormat = (bfOld, bfNew, bfXML);
  TRPCBuffer = class;
  TRPCFieldType = (dtNotTyped, dtString, dtBlob, dtInteger, dtDateTime,
    dtBoolean, dtExtended, dtPChar, dtObject, dtCurrency, dtVariant, dtWord,
    dtByte, dtTime, dtDate, dtLargeint);

  TRPCField = class(TObject)
  public
    FieldType: TRPCFieldType;
    constructor Create; virtual;
  end;

  TRPCIntegerField = class(TRPCField)
    Value: Integer;
  end;

  TRPCInt64Field = class(TRPCField)
    Value: Int64;
  end;

  TRPCWordField = class(TRPCField)
    Value: Word;
  end;

  TRPCByteField = class(TRPCField)
    Value: Byte;
  end;

  TRPCBooleanField = class(TRPCField)
    Value: Boolean;
  end;

  TRPCDateTimeField = class(TRPCField)
    Value : TDateTime;
  end;

  TRPCStreamField = class(TRPCField)
    Value: TMemoryStream;
  end;

  TRPCFloatField = class(TRPCField)
    Value: Double;
  end;

  TRPCStringField = class(TRPCField)
    Value: String;
  end;

  TRPCCurrencyField = class(TRPCField)
    Value: Currency;
  end;

  TRPCRecord = class(TList)
  private
    FBuffer: TRPCBuffer;
    function GetRPCFields(Index: Integer): TRPCField;
    procedure SetRPCFields(Index: Integer; const Value: TRPCField);

    class function GetFieldAsString(const Field: TRPCField): String;
    class function GetFieldAsInteger(const Field: TRPCField): Integer;
    class function GetFieldAsInt64(const Field: TRPCField): Int64;
    class function GetFieldAsFloat(const Field: TRPCField): Double;
    class function GetFieldAsBoolean(const Field: TRPCField): Boolean;
    class function GetFieldAsByte(const Field: TRPCField): Byte;
    class function GetFieldAsWord(const Field: TRPCField): Word;
  public
    function AddField(const AFieldType: TRPCFieldType; const FieldIndex: Integer = -1): TRPCField;
    procedure DeleteField(Index: Integer);
    procedure AdjustFieldTypeAndSize(var Field: TRPCField; const AFieldType: TRPCFieldType; const FieldIndex, FieldSize: Integer);
    procedure SaveToXML(XML: XmlTextWriter);
    procedure SaveToStream(Stream: Stream; const BufferFormat: TApBufferFormat = bfNew);
    procedure LoadFromStream(Stream: Stream; const BufferFormat: TApBufferFormat = bfNew);
    procedure SaveFieldToStream(Stream: Stream; const Index: Integer; const BufferFormat: TApBufferFormat = bfNew);
    procedure SaveFieldToXML(XML: XmlTextWriter; const Index: Integer);
    procedure LoadFieldFromStream(Stream: Stream; const BufferFormat: TApBufferFormat = bfNew);
    procedure LoadFromXMLDOMNode(const ARecordNode: XmlNode);
    procedure LoadFieldFromXMLNode(const AFieldNode: XmlNode);

    procedure Clear; override;
    destructor Destroy; override;
    
    property RPCFields[Index: Integer]: TRPCField read GetRPCFields write SetRPCFields; default;
  end;

  TRPCBufferSort = class
  private
    FDataList: TList;
    function GetRecordPointer(Index: Integer): Integer;
    function GetCurrentRecordPointer: Integer;
    procedure QuickSort(L, R: Integer);
    function CompareKeys(const Item1, Item2: TRPCRecord; const ACaseSensitive: Boolean): Integer; overload;
    function CompareKeys(const Item: TRPCRecord; const AKeyValues: Array of Variant; const ACaseSensitive: Boolean): Integer; overload;
    function CompareKeys(const Item: TRPCRecord; const AKeyValues: Array of String; const ACaseSensitive: Boolean): Integer; overload;
    function CompareKeys(const Item: TRPCRecord; const AKeyValues: Array of Integer; const ACaseSensitive: Boolean): Integer; overload;
    function GetRPCRecord(const Index: Integer): TRPCRecord;
    procedure SetRPCRecord(const Index: Integer; const Value: TRPCRecord);
    function GetCount: Integer;
  protected
    FCurrentItem: Integer;
    FKeys: Array of Word;
    FBuffer: TRPCBuffer;
    FAscending: Boolean;
    FCaseSensitive: Boolean;
    procedure ClearDataList; virtual;
  public
    constructor Create(const ABuffer: TRPCBuffer; const AKeys: Array of Word; const Ascending, ACaseSensitive: Boolean); reintroduce;
    destructor Destroy; override;

    procedure AddRecord(const ARecord: TRPCRecord); virtual;
    procedure DeleteRecord(const ARecord: TRPCRecord); virtual;
    function FieldIndexIsKey(const AFieldIndex: Integer): Boolean;
    procedure InsertIndexed(const ARecord: TRPCRecord);
    //function CheckNeedReindex(const AFieldIndex: Integer; const ARecord: TRPCRecord; var AList: TList);
    function FindKey(const AKeyValues: Array of Variant; var Index: Integer): Boolean; overload;
    function FindKey(const AKeyValues: Array of String; var Index: Integer): Boolean; overload;
    function FindKey(const AKeyValues: Array of Integer; var Index: Integer): Boolean; overload;
    function FindKey(const ARecord: TRPCRecord; const Exact: Boolean; var Index: Integer): Boolean; overload;

    procedure Sort; reintroduce; virtual;

    property CurrentItem: Integer read FCurrentItem write FCurrentItem;
    property RecordPointer[Index: Integer]: Integer read GetRecordPointer;
    property CurrentRecordPointer: Integer read GetCurrentRecordPointer;
    property RPCRecord[const Index: Integer]: TRPCRecord read GetRPCRecord write SetRPCRecord;
    property Count: Integer read GetCount;
  end;

  TRPCBufferSortList = class(TObject)
  private
    FSortList: TStringList;
    FBuffer: TRPCBuffer;
  protected
    function GetSortItem(AIndex: Integer): TRPCBufferSort;
    function GetCount: Integer;
  public
    constructor Create(const ABuffer: TRPCBuffer);
    destructor Destroy; override;
    function GetSortIndex(const ASortName: String): Integer;
    function GetSortByName(const ASortName: String): TRPCBufferSort;
    function AddSort(const ASortName: String; const AKeys: Array of Word; const Ascending, ACaseSensitive: Boolean): Integer;
    function DeleteSort(ASortIndex: Integer): Boolean;
    function IndexOfSort(const ASort: TRPCBufferSort): SmallInt;
    procedure DeleteRecord(const ARecord: TRPCRecord);
    procedure GenerateNeededUpdates(const AFieldIndex: Integer; const ARecord: TRPCRecord; var AList: TList);
    procedure UpdateNeededUpdates(const ARecord: TRPCRecord; var AList: TList);
    procedure ClearList;
    property Items[AIndex: Integer]: TRPCBufferSort read GetSortItem;
    property Count: Integer read GetCount;
  end;

  TRPCBufferBookMarkStruct = class(TObject)
    RecordPointer: Integer;
    CurrentIndex: Smallint;
  end;

  TRPCBufferBookMark = TRPCBufferBookMarkStruct;

  TFieldNameInfo = class(TObject)
    Index: Integer;
    FieldType: TRPCFieldType;
  end;

  TFieldNames = class(TStringList)
  public
    destructor Destroy; override;
  end;

  TRPCBufferDataList = class(TList)
  public
    procedure Clear; override;
    destructor Destroy; override;
  end;

  ERPCBufferConversionError = class(Exception)
  public
    constructor Create(const Index: Integer; const FieldType: TRPCFieldType); reintroduce;
  end;

  TRPCBuffer = class(TList)
    FDataList: TRPCBufferDataList;
    FRecordPointer: Integer;
    FHitEof: Boolean;
    FHitBof: Boolean;
    FSortIndex: TRPCBufferSortList;
    FCurrentSortIndex: TRPCBufferSort;
    FHasChanged: Boolean;
    FFieldNames: TFieldNames;

    function GetRecords(Index: Integer): TRPCRecord;
    function GetRPCFields(Index: Integer): TRPCField;
    procedure IntSetFieldAsByte(const nIndex: Integer; const Value: Byte);
    procedure IntSetFieldAsWord(const nIndex: Integer; const Value: Word);
    procedure IntSetFieldAsString(const nIndex: Integer; const Value: String);
    procedure IntSetFieldAsInteger(const nIndex: Integer; const Value: Integer);
    procedure IntSetFieldAsInt64(const nIndex: Integer; const Value: Int64);
    procedure IntSetFieldAsFloat(const nIndex: Integer; const Value: Double);
    procedure IntSetFieldAsBoolean(const nIndex: Integer; const Value: Boolean);
    procedure IntSetFieldAsDateTime(const nIndex: Integer; const Value: TDateTime);
    function IntGetFieldAsBoolean(const nIndex: Integer): Boolean;
    function IntGetFieldAsByte(const nIndex: Integer): Byte;
    function IntGetFieldAsFloat(const nIndex: Integer): Double;
    function IntGetFieldAsInteger(const nIndex: Integer): Integer;
    function IntGetFieldAsInt64(const nIndex: Integer): Int64;
    function IntGetFieldAsCardinal(const nIndex: Integer): Cardinal;
    function IntGetFieldAsString(const nIndex: Integer): String;
    function IntGetFieldAsWord(const nIndex: Integer): Word;
    function IntGetFieldAsDateTime(const nIndex: Integer): TDateTime;
    function FindNameIndex(const Index: Integer): Integer;
    procedure FreeFieldNames;
    function IntGetFieldIndexByName(const Name: String; const ARaiseException: Boolean = True): Integer;
    function GetFieldIndexByName(const Name: String): Integer;
    function GetFieldName(const nIndex: Integer): String;
    procedure SetFieldName(const nIndex: Integer; const Value: String);

    function GetFieldByNameAsBoolean(const Name: String): Boolean;
    function GetFieldByNameAsByte(const Name: String): Byte;
    function GetFieldByNameAsDateTime(const Name: String): TDateTime;
    function GetFieldByNameAsFloat(const Name: String): Double;
    function GetFieldByNameAsInteger(const Name: String): Integer;
    function GetFieldByNameAsInt64(const Name: String): Int64;
    function GetFieldByNameAsCardinal(const Name: String): Cardinal;
    function GetFieldByNameAsString(const Name: String): String;
    function GetFieldByNameAsWord(const Name: String): Word;
    procedure SetFieldByNameAsBoolean(const Name: String; const Value: Boolean);
    procedure SetFieldByNameAsByte(const Name: String; const Value: Byte);
    procedure SetFieldByNameAsDateTime(const Name: String; const Value: TDateTime);
    procedure SetFieldByNameAsFloat(const Name: String; const Value: Double);
    procedure SetFieldByNameAsInteger(const Name: String; const Value: Integer);
    procedure SetFieldByNameAsInt64(const Name: String; const Value: Int64);
    procedure SetFieldByNameAsCardinal(const Name: String; const Value: Cardinal);
    procedure SetFieldByNameAsString(const Name, Value: String);
    procedure SetFieldByNameAsWord(const Name: String; const Value: Word);
    function GetFieldByNameFieldType(const Name: String): TRPCFieldType;
    procedure SetFieldByNameFieldType(const Name: String; const Value: TRPCFieldType);
    procedure IntSetFieldAsCardinal(const nIndex: Integer; const Value: Cardinal);
    function getIsSorted: Boolean;
    function GetHasData: Boolean;
    procedure SetHasData(const Value: Boolean);
    function GetCurrentRecord: TRPCRecord;

    procedure SetFieldAsString(var Field: TRPCField; const Value: String; const FieldIndex: Integer);
    procedure SetFieldAsInteger(var Field: TRPCField; Value: Integer; const FieldIndex: Integer);
    procedure SetFieldAsInt64(var Field: TRPCField; Value: Int64; const FieldIndex: Integer);
    procedure SetFieldAsFloat(var Field: TRPCField; Value: Double; const FieldIndex: Integer);
    procedure SetFieldAsBoolean(var Field: TRPCField; Value: Boolean; const FieldIndex: Integer);
    procedure SetFieldAsPChar(var Field: TRPCField; Value: TBytes; const Size: Integer; const FieldIndex: Integer);
    procedure SetFieldFromField(var Field: TRPCField; FieldFrom: TRPCField);
    function SetFieldFromStream(var Field: TRPCField; const AStream: Stream; const Size: Cardinal; const FieldIndex: Integer): Cardinal;
    procedure AdjustFieldTypeAndSize(var Field: TRPCField; const AFieldType: TRPCFieldType;
      FieldSize: Cardinal; const FieldIndex: Integer);

    function AppendRPCRecord: TRPCRecord; virtual;

    function AppendRPCField(const ADataType: TRPCFieldType): TRPCField; virtual;
    procedure RemoveRPCRecord(const Index: Integer); virtual;
    function getCount: Integer;

    property Records[Index: Integer]: TRPCRecord read GetRecords;
    property RPCFields[Index: Integer]: TRPCField read GetRPCFields;
    procedure GetBufferAsStrings(AList: TStrings); // terazawa - 15/07/2014
  protected
    procedure RaiseException(const Where: String);
    function NewGetFields(const nIndex: Integer): String;
    function GetFields(const nIndex: Integer): String;
    function GetFieldType(const nIndex: Integer): TRPCFieldType;
    procedure SetFieldType(const Index: Integer; const FieldType: TRPCFieldType);
    function GetFieldCount: Integer;
    function GetRecordCount: Integer;
    function GetRecNo: Integer;
    function GetEof: Boolean;
    function GetBof: Boolean;
    //== protected write methods
    procedure SetFields(const nIndex: Integer; const newValue: String);
    procedure AppendStreamField(Data: Stream);
    //==
    function GetFieldSize(const nIndex: Integer): Integer;
    procedure ClearDataList; virtual;
    procedure FreeDataObjects; virtual;
  public
    procedure SaveToTextFile(const AFileName: String); virtual;
    procedure SaveToClip; virtual; // terazawa - 15/07/2014
    function AsString: String;

    constructor Create; virtual;
    destructor Destroy; override;

    procedure First;
    procedure Next;
    procedure Prior;
    procedure Last;

    function GetRPCBufferBookMark: TRPCBufferBookMark;
    procedure GotoRPCBufferBookMark(const ABookMark: TRPCBufferBookMark);
    procedure FreeRPCBufferBookMark(var ABookMark: TRPCBufferBookMark);

    function FindFieldIndexByName(const Name: String): Integer;

    //== write methods
    procedure Append;
    procedure Rewrite(const AClearFieldNames: Boolean = False);
    procedure Clear; override;

    procedure WriteRecord(const Args: Array of Variant);
    procedure WriteTypedRecord(const Args: Array of Variant; const Types: Array of TRPCFieldType);
    procedure WriteFields(const NewRec: Boolean; const Args: Array of Variant);
    procedure WriteTFieldToBuffer(const NewRec: Boolean; const Field: TField); virtual;
    procedure WriteTypedFields(const NewRec: Boolean; const Args: Array of Variant; const Types: Array of TRPCFieldType); virtual;
    procedure WriteStreamField(const NewRec: Boolean; Data: Stream; const FromBeggining: Boolean; const ASize: Integer = -1; const FreeStream: Boolean = False); virtual;
    procedure WriteBlockStreamField(const NewRec: Boolean; Data: Stream; const BlockSize: Integer);
    procedure WriteFieldFromBufferField(const NewRec: Boolean; const From: TRPCBuffer; const FieldIndex: Integer);

    procedure WriteStringField(const NewRec: Boolean; const AValue: String);
    procedure WriteBlobField(const NewRec: Boolean; AValue: TBytes);
    procedure WritePCharField(const NewRec: Boolean; AValue: TBytes);
    procedure WriteObjectField(const NewRec: Boolean; AValue: TBytes);
    procedure WriteVariantField(const NewRec: Boolean; const AValue: Variant);
    procedure WriteIntegerField(const NewRec: Boolean; const AValue: Integer);
    procedure WriteInt64Field(const NewRec: Boolean; const AValue: Int64);
    procedure WriteCardinalField(const NewRec: Boolean; const Value: Cardinal);
    procedure WriteDateTimeField(const NewRec: Boolean; const AValue: TDateTime);
    procedure WriteBooleanField(const NewRec: Boolean; const AValue: Boolean);
    procedure WriteExtendedField(const NewRec: Boolean; const AValue: Double);
    procedure WriteCurrencyField(const NewRec: Boolean; const AValue: Currency);
    procedure WriteByteField(const NewRec: Boolean; const AValue: Byte);
    procedure WriteWordField(const NewRec: Boolean; const AValue: Word);
    procedure WriteTimeField(const NewRec: Boolean; const AValue: TDateTime);
    procedure WriteDateField(const NewRec: Boolean; const AValue: TDateTime);

    procedure WriteComponent(Instance: TComponent);
    procedure WriteRPCBuffer(const NewRec: Boolean; Buffer: TRPCBuffer; const BufferFormat: TApBufferFormat = bfNew);

    procedure AppendFields(const Args: Array of Variant);
    procedure AppendTypedFields(const Args: Array of Variant; const Types: Array of TRPCFieldType);
    procedure AppendRecord(const From: TRPCBuffer);

    procedure Sort(const SortName: String; const BuildKey: Array of Word; const IntSort: Boolean = False; const Ascending: Boolean = True; const ACaseSensitive: Boolean = False); virtual;
    function RemoveSort(const ASortName: String): Boolean; overload;
    function RemoveSort(const ASortIndex: Integer): Boolean; overload;

    procedure AssignBuffer(From: TRPCBuffer);
    procedure Delete;

    procedure ReadComponent(Instance: TComponent);
    procedure ReadRPCBuffer(const nIndex: Integer; Buffer: TRPCBuffer; const BufferFormat: TApBufferFormat = bfNew);
    procedure Seek(ARecNo: Integer);
    procedure SetCurrentSortIndex(const SortName: String);
    procedure MirrorSort(const SortName: String; const BuildKey: Array of Word);

    function FindKey(const SortIndex: Integer; const KeyFields: Array of Variant; const AKeySensitive: Boolean): Boolean; overload;
    function FindKey(const SortName: String; const KeyFields: Array of Variant; const AKeySensitive: Boolean): Boolean; overload;

    function FindNearestKey(const SortIndex: Integer; const KeyFields: Array of Variant; const AKeySensitive: Boolean): Boolean; overload;
    function FindNearestKey(const SortName: String; const KeyFields: Array of Variant; const AKeySensitive: Boolean): Boolean; overload;

    function StrFindKey(const SortIndex: Integer; const KeyFields: Array of String; const AKeySensitive: Boolean): Boolean; overload;
    function StrFindKey(const SortName: String; const KeyFields: Array of String; const AKeySensitive: Boolean): Boolean; overload;

    function StrFindNearestKey(const SortIndex: Integer; const KeyFields: Array of String; const AKeySensitive: Boolean): Boolean; overload;
    function StrFindNearestKey(const SortName: String; const KeyFields: Array of String; const AKeySensitive: Boolean): Boolean; overload;

    function IntFindKey(const SortIndex: Integer; const KeyFields: Array of Integer; const AKeySensitive: Boolean; const Inverted: Boolean = False): Boolean; overload;
    function IntFindKey(const SortName: String; const KeyFields: Array of Integer; const AKeySensitive: Boolean): Boolean; overload;

    function IntFindNearestKey(const SortIndex: Integer; const KeyFields: Array of Integer; const AKeySensitive: Boolean): Boolean; overload;
    function IntFindNearestKey(const SortName: String; const KeyFields: Array of Integer; const AKeySensitive: Boolean): Boolean; overload;

    procedure SetFieldNameAndType(const AIndex: Integer; const Value: String; AFieldType: TRPCFieldType);
    procedure AddFieldNameAndType(const Value: String; AFieldType: TRPCFieldType);

    function IsEqualTo(const ABuffer: TRPCBuffer): Boolean;

    function GetStringField(const nIndex: Integer): String;
    function GetCharField(const nIndex: Integer): Char;
    function GetBlobField(const nIndex: Integer): TBytes;
    function GetPCharField(const nIndex: Integer): TBytes;
    function GetVariantField(const nIndex: Integer): TObject;
    function GetIntegerField(const nIndex: Integer): Integer;
    function GetInt64Field(const nIndex: Integer): Int64;
    function GetDateTimeField(const nIndex: Integer): TDateTime;
    function GetBooleanField(const nIndex: Integer): Boolean;
    function GetExtendedField(const nIndex: Integer): Double;
    function GetCurrencyField(const nIndex: Integer): Currency;
    function GetCardinalField(const nIndex: Integer): Cardinal;
    function GetByteField(const nIndex: Integer): Byte;
    function GetWordField(const nIndex: Integer): Word;
    function GetTimeField(const nIndex: Integer): TDateTime;
    function GetDateField(const nIndex: Integer): TDateTime;
    function GetStreamField(const FieldNb: Integer; Data: Stream; const Rewrite: Boolean = True): Integer; overload;
    function GetStreamField(const FieldIndex: Integer): Stream; overload;
    function SetStreamField(const nIndex: Integer; Data: Stream; ARewrite: Boolean = False): Integer;
    function GetBlockedStreamField(const FieldNb: Integer; Data: Stream): Integer;

    function JumpTo(const RecNo: Integer): Boolean;
    function IndexJumpTo(const RecNumber: Integer): Boolean;
    procedure SaveToStream(Stream: Stream; const BufferFormat: TApBufferFormat = bfNew);
    procedure SaveToXMLStream(Stream: Stream);
    procedure LoadFromStream(Stream: Stream; Len: Longint; const BufferFormat: TApBufferFormat = bfNew); overload;
    procedure LoadFromStream(Stream: Stream; const BufferFormat: TApBufferFormat = bfNew); overload;
    procedure LoadFromXMLStream(Stream: Stream);    
    procedure LoadFromDataSet(const Source: TDataSet; const InitialField: Integer = 0; const FinalField: Integer = -1;
      const ALoadFieldNames: Boolean = True);
    procedure LoadFromRecordDataSet(const Source: TDataSet; const InitialField: Integer = 0; const FinalField: Integer = -1);
    procedure LoadFromPChar(const Value: TBytes; const Size: Integer; const BufferFormat: TApBufferFormat = bfNew);
    procedure SaveToFile(const FieldNo: Integer; const FileName: String);
    procedure LoadFromFile(const NewRec: Boolean; const FileName: String);
    procedure LoadFromDelimitedFile(const FileName: String; const Delimiter: String);
    procedure LoadFromList(const NewRec: Boolean; const List: TStrings);
    function GetSortIndex(const ASortName: String): Integer; // walter - 03/07/2012

    property Count: Integer read getCount;
    property Fields[const nIndex: Integer]: String read GetFields write SetFields;
    property NewFields[const nIndex: Integer]: String read NewGetFields write SetFields;

    property FieldSize[const nIndex: Integer]: Integer read GetFieldSize;
    property FieldType[const nIndex: Integer]: TRPCFieldType read GetFieldType write SetFieldType;
    property FieldCount: Integer read GetFieldCount;
    property RecordCount: Integer read GetRecordCount;
    property Recno: Integer read GetRecNo;
    property EOF: Boolean read GetEof;
    property BOF: Boolean read GetBof;
    property HasData: Boolean read GetHasData write SetHasData;
    property HasChanged: Boolean read FHasChanged;
    property IsSorted: Boolean read getIsSorted;

    //New Methods
    property FieldAsInteger[const nIndex: Integer]: Integer read IntGetFieldAsInteger write IntSetFieldAsInteger;
    property FieldAsInt64[const nIndex: Integer]: Int64 read IntGetFieldAsInt64 write IntSetFieldAsInt64;
    property FieldAsCardinal[const nIndex: Integer]: Cardinal read IntGetFieldAsCardinal write IntSetFieldAsCardinal;
    property FieldAsWord[const nIndex: Integer]: Word read IntGetFieldAsWord write IntSetFieldAsWord;
    property FieldAsByte[const nIndex: Integer]: Byte read IntGetFieldAsByte write IntSetFieldAsByte;
    property FieldAsFloat[const nIndex: Integer]: Double read IntGetFieldAsFloat write IntSetFieldAsFloat;
    property FieldAsDateTime[const nIndex: Integer]: TDateTime read IntGetFieldAsDateTime write IntSetFieldAsDateTime;
    property FieldAsString[const nIndex: Integer]: String read IntGetFieldAsString write IntSetFieldAsString;
    property FieldAsBoolean[const nIndex: Integer]: Boolean read IntGetFieldAsBoolean write IntSetFieldAsBoolean;

    property FieldName[const AIndex: Integer]: String read GetFieldName write SetFieldName;
    property FieldByNameAsInteger[const Name: String]: Integer read GetFieldByNameAsInteger write SetFieldByNameAsInteger;
    property FieldByNameAsInt64[const Name: String]: Int64 read GetFieldByNameAsInt64 write SetFieldByNameAsInt64;
    property FieldByNameAsCardinal[const Name: String]: Cardinal read GetFieldByNameAsCardinal write SetFieldByNameAsCardinal;
    property FieldByNameAsWord[const Name: String]: Word read GetFieldByNameAsWord write SetFieldByNameAsWord;
    property FieldByNameAsByte[const Name: String]: Byte read GetFieldByNameAsByte write SetFieldByNameAsByte;
    property FieldByNameAsFloat[const Name: String]: Double read GetFieldByNameAsFloat write SetFieldByNameAsFloat;
    property FieldByNameAsDateTime[const Name: String]: TDateTime read GetFieldByNameAsDateTime write SetFieldByNameAsDateTime;
    property FieldByNameAsString[const Name: String]: String read GetFieldByNameAsString write SetFieldByNameAsString;
    property FieldByNameAsBoolean[const Name: String]: Boolean read GetFieldByNameAsBoolean write SetFieldByNameAsBoolean;
    property FieldByNameFieldtype[const Name: String] : TRPCFieldType read GetFieldByNameFieldType write SetFieldByNameFieldType;
    property FieldIndexByName[const Name: String] : Integer read GetFieldIndexByName;
  end;

const
  XMLFieldTypes: Array[TRPCFieldType] of String = (
    'ap:unknowStr', 'ap:String', 'ap:hexBinary', 'ap:Integer', 'ap:dateTime',
    'ap:Boolean', 'ap:Float', 'ap:pcharStr', 'ap:ObjStr', 'ap:Currency',
    'ap:variantStr', 'ap:unassignedShort', 'ap:unassignedByte', 'ap:time',
    'ap:date', 'ap:largeint');

implementation

{ TRPCRecord }

function TRPCRecord.AddField(const AFieldType: TRPCFieldType;
  const FieldIndex: Integer): TRPCField;
type
  TRPCFieldClass = class of TRPCField;
const
  ARPCClasses: Array[TRPCFieldType] of TRPCFieldClass =
  (TRPCStringField, TRPCStringField, TRPCStreamField, TRPCIntegerField,
   TRPCDateTimeField, TRPCBooleanField, TRPCFloatField, TRPCStringField,
   TRPCStringField, TRPCCurrencyField, TRPCStringField, TRPCWordField,
   TRPCByteField, TRPCDateTimeField, TRPCDateTimeField, TRPCInt64Field);
begin
  Result := ARPCClasses[AFieldType].Create;
  Result.FieldType := AFieldType;
  if FieldIndex >= 0 then
    Insert(FieldIndex, Result)
  else
    Add(Result);
end;

procedure TRPCRecord.AdjustFieldTypeAndSize(var Field: TRPCField;
  const AFieldType: TRPCFieldType; const FieldIndex, FieldSize: Integer);
begin
  if (Field.FieldType <> AFieldType) then
  begin
    DeleteField(FieldIndex);
    Field := AddField(AFieldType, FieldIndex);
  end;
  if (Field.FieldType = dtBlob) and (not Assigned(TRPCStreamField(Field).Value)) then
    TRPCStreamField(Field).Value := TMemoryStream.Create;
end;

procedure TRPCRecord.Clear;
begin
  while Count > 0 do
    DeleteField(Count - 1);
  inherited;
end;

procedure TRPCRecord.DeleteField(Index: Integer);
begin
  if (RPCFields[Index].FieldType = dtBlob) and Assigned(TRPCStreamField(RPCFields[Index]).Value) then
  begin
    TRPCStreamField(RPCFields[Index]).Value.Free;
    TRPCStreamField(RPCFields[Index]).Value := nil;
  end;
  RPCFields[Index].Free;
  Delete(Index);
end;

destructor TRPCRecord.Destroy;
begin
  Clear;
  inherited;
end;

class function TRPCRecord.GetFieldAsBoolean(const Field: TRPCField): Boolean;
begin
  case Field.FieldType of
    dtNotTyped, dtString: Result := GetFieldAsString(Field) = '1';
    dtBlob: raise Exception.Create('Conversion BLOB to Boolean not supported');
    dtInteger: Result := GetFieldAsInteger(Field) = 1;
    dtLargeint: Result := GetFieldAsInt64(Field) = 1;
    dtDateTime,
      dtExtended,
      dtCurrency,
      dtTime,
      dtDate: Result := GetFieldAsFloat(Field) = 1;
    dtBoolean: Result := Boolean(TRPCBooleanField(Field).Value);
    dtByte: Result := GetFieldAsByte(Field) = 1;
    dtWord: Result := GetFieldAsWord(Field) = 1;
  else
    Result := False;
  end;
end;

class function TRPCRecord.GetFieldAsByte(const Field: TRPCField): Byte;
begin
  case Field.FieldType of
    dtNotTyped, dtString: Result := StrToInt(GetFieldAsString(Field));
    dtBlob: raise Exception.Create('Conversion BLOB to Byte not supported');
    dtInteger: Result := GetFieldAsInteger(Field);
    dtLargeint: Result := GetFieldAsInt64(Field);
    dtDateTime,
      dtExtended,
      dtCurrency,
      dtTime,
      dtDate: Result := Trunc(GetFieldAsFloat(Field));
    dtBoolean: Result := IfThen(GetFieldAsBoolean(Field), 1, 0);
    dtByte: Result := TRPCByteField(Field).Value;
    dtWord: Result := GetFieldAsWord(Field);
  else
    Result := 0;
  end;
end;

class function TRPCRecord.GetFieldAsFloat(const Field: TRPCField): Double;
begin
  case Field.FieldType of
    dtNotTyped, dtString: Result := StrToFloat(GetFieldAsString(Field));
    dtBlob: raise Exception.Create('Conversion BLOB to Float not supported');
    dtInteger: Result := GetFieldAsInteger(Field);
    dtLargeint: Result := GetFieldAsInt64(Field);
    dtExtended: Result := TRPCFloatField(Field).Value;
    dtCurrency: Result := TRPCCurrencyField(Field).Value;
    dtDateTime,
    dtTime,
    dtDate: Result := TRPCDateTimeField(Field).Value;
    dtBoolean: Result := IfThen(GetFieldAsBoolean(Field), 1, 0);
    dtByte: Result := GetFieldAsByte(Field);
    dtWord: Result := GetFieldAsWord(Field);
  else
    Result := 0;
  end;
end;

class function TRPCRecord.GetFieldAsInt64(const Field: TRPCField): Int64;
begin
  case Field.FieldType of
    dtNotTyped, dtString: Result := StrToInt64(GetFieldAsString(Field));
    dtBlob: raise Exception.Create('Conversion BLOB to Int64 not supported');
    dtLargeint: Result := TRPCInt64Field(Field).Value;
    dtInteger: Result := TRPCIntegerField(Field).Value;
    dtDateTime,
      dtExtended,
      dtCurrency,
      dtTime,
      dtDate: Result := Trunc(GetFieldAsFloat(Field));
    dtBoolean: Result := IfThen(GetFieldAsBoolean(Field), 1, 0);
    dtByte: Result := GetFieldAsByte(Field);
    dtWord: Result := GetFieldAsWord(Field);
  else
    Result := 0;
  end;
end;

class function TRPCRecord.GetFieldAsInteger(const Field: TRPCField): Integer;
begin
  case Field.FieldType of
    dtNotTyped, dtString: Result := StrToInt(GetFieldAsString(Field));
    dtBlob: raise Exception.Create('Conversion BLOB to Integer not supported');
    dtLargeint: Result := TRPCInt64Field(Field).Value;
    dtInteger: Result := TRPCIntegerField(Field).Value;
    dtDateTime,
      dtExtended,
      dtCurrency,
      dtTime,
      dtDate: Result := Trunc(GetFieldAsFloat(Field));
    dtBoolean: Result := IfThen(GetFieldAsBoolean(Field), 1, 0);
    dtByte: Result := GetFieldAsByte(Field);
    dtWord: Result := GetFieldAsWord(Field);
  else
    Result := 0;
  end;
end;

class function TRPCRecord.GetFieldAsString(const Field: TRPCField): String;
begin
  case Field.FieldType of
    dtBlob: Result := '** BLOB **';
    dtInteger: Result := IntToStr(GetFieldAsInteger(Field));
    dtLargeint: Result := IntToStr(GetFieldAsInt64(Field));
    dtDateTime,
      dtExtended,
      dtCurrency,
      dtTime,
      dtDate: Result := FloatToStr(GetFieldAsFloat(Field));
    dtBoolean: Result := IfThen(GetFieldAsBoolean(Field), '1', '0');
    dtByte: Result := IntToStr(GetFieldAsByte(Field));
    dtWord: Result := IntToStr(GetFieldAsWord(Field));
  else
    Result := TRPCStringField(Field).Value;
  end;
end;

class function TRPCRecord.GetFieldAsWord(const Field: TRPCField): Word;
begin
  case Field.FieldType of
    dtNotTyped, dtString: Result := StrToInt(GetFieldAsString(Field));
    dtBlob: raise Exception.Create('Conversion BLOB to Word not supported');
    dtInteger: Result := GetFieldAsInteger(Field);
    dtLargeint: Result := GetFieldAsInt64(Field);
    dtDateTime,
      dtExtended,
      dtCurrency,
      dtTime,
      dtDate: Result := Trunc(GetFieldAsFloat(Field));
    dtBoolean: Result := IfThen(GetFieldAsBoolean(Field), 1, 0);
    dtByte: Result := GetFieldAsByte(Field);
    dtWord: Result := TRPCWordField(Field).Value;
  else
    Result := 0;
  end;
end;

function TRPCRecord.GetRPCFields(Index: Integer): TRPCField;
begin
  if (Index < 0) or (Index >= Count) then
    raise Exception.CreateFmt('Buffer error: Index %0:d is less than zero or greater then %1:d', [Index, Count - 1])
  else
    Result := TRPCField(Items[Index]);
end;

procedure TRPCRecord.LoadFieldFromStream(Stream: Stream;
  const BufferFormat: TApBufferFormat);  platform;
var
  FField: TRPCField;

  procedure LoadStreamField; platform;
  var
    FSize: Integer;
  begin
    TStream(Stream).read(FSize, SizeOf(FSize));
    if FSize > 0 then
    begin
      TRPCStreamField(FField).Value := TMemoryStream.Create;
      TRPCStreamField(FField).Value.CopyFrom(Stream, FSize);
    end;
  end;

  procedure LoadIntegerField; platform;
  var
    FValue: Integer;
  begin
    TStream(Stream).read(FValue, SizeOf(FValue));
    TRPCIntegerField(FField).Value := FValue;
  end;

  procedure LoadInt64Field; platform;
  var
    FValue: Int64;
  begin
    TStream(Stream).read(FValue, SizeOf(FValue));
    TRPCInt64Field(FField).Value := FValue;
  end;

  procedure LoadDateTimeField; platform;
  var
    FValue: Double;
  begin
    TStream(Stream).read(FValue, SizeOf(FValue));
    TRPCDateTimeField(FField).Value := FValue;
  end;

  procedure LoadFloatField; platform;
  var
    FValue: Double;
  begin
    TStream(Stream).read(FValue, SizeOf(FValue));
    TRPCFloatField(FField).Value := FValue;
  end;

  procedure LoadBooleanField; platform;
  var
    FValue: Boolean;
  begin
    TStream(Stream).read(FValue, SizeOf(FValue));
    TRPCBooleanField(FField).Value := FValue;
  end;

  procedure LoadWordField; platform;
  var
    FValue: Word;
  begin
    TStream(Stream).read(FValue, SizeOf(FValue));
    TRPCWordField(FField).Value := FValue;
  end;

  procedure LoadByteField; platform;
  begin
    TRPCByteField(FField).Value := Stream.ReadByte;
  end;

  procedure LoadStringField; platform;
  var
    FValue: Byte;
  begin
    repeat
      FValue := Stream.ReadByte;
      if FValue <> 0 then
        TRPCStringField(FField).Value := TRPCStringField(FField).Value + Chr(FValue);
    until FValue = 0;
  end;

var
  FDataType: Byte;
begin
  FDataType := Stream.ReadByte;
  FField := AddField(TRPCFieldType(FDataType));
  case TRPCFieldType(FDataType) of
    dtBlob: LoadStreamField;
    dtInteger: LoadIntegerField;
    dtLargeint: LoadInt64Field;
    dtDateTime,
      dtTime,
      dtDate: LoadDateTimeField;
    dtExtended,
      dtCurrency: LoadFloatField;
    dtBoolean: LoadBooleanField;
    dtWord: LoadWordField;
    dtByte: LoadByteField;
  else
    LoadStringField;
  end;
end;

procedure TRPCRecord.LoadFieldFromXMLNode(const AFieldNode: XmlNode); platform;
var
  FField: TRPCField;

  function RecognizeFieldType: TRPCFieldType;
  begin
    for Result := low(XMLFieldTypes) to high(XMLFieldTypes) do
      if SameText(XMLFieldTypes[Result], AFieldNode.Attributes.GetNamedItem('dataType').Value) then
        Break;
  end;

  procedure LoadStreamField; platform;
  var
    FByte: Byte;
    FValue: String;
  begin
    if (AFieldNode.Value <> '') then
    begin
      TRPCStreamField(FField).Value := TMemoryStream.Create;
      with TRPCStreamField(FField).Value do
      begin
        FValue := AFieldNode.Value;
        while Position < (length(FValue) div 2) do
        begin
          FByte := StrToInt('$' + FValue[(Position * 2) + 1] + FValue[(Position * 2) + 2]);
          write(FByte, SizeOf(FByte));
        end;
      end;
    end;
  end;

  procedure LoadIntegerField;
  begin
    TRPCIntegerField(FField).Value := Convert.ToInt32(AFieldNode.Value);
  end;

  procedure LoadInt64Field;
  begin
    TRPCInt64Field(FField).Value := Convert.ToInt64(AFieldNode.Value);
  end;

  procedure LoadDateTimeField;
  begin
    TRPCDateTimeField(FField).Value := Convert.ToDateTime(AFieldNode.Value);
  end;

  procedure LoadFloatField;
  begin
    TRPCFloatField(FField).Value := Convert.ToDouble(AFieldNode.Value);
  end;

  procedure LoadBooleanField;
  begin
    TRPCBooleanField(FField).Value := Convert.ToBoolean(AFieldNode.Value);
  end;

  procedure LoadWordField;
  begin
    TRPCWordField(FField).Value := Convert.ToUInt16(AFieldNode.Value);
  end;

  procedure LoadByteField;
  begin
    TRPCByteField(FField).Value := Convert.ToByte(AFieldNode.Value);
  end;

  procedure LoadStringField;
  begin
    TRPCStringField(FField).Value := AFieldNode.Value;
  end;

begin
  FField := AddField(RecognizeFieldType);
  if FBuffer.FieldName[Count - 1] = '' then
    FBuffer.FieldName[Count - 1] := AFieldNode.Name;
  case FField.FieldType of
    dtBlob: LoadStreamField;
    dtInteger: LoadIntegerField;
    dtLargeint: LoadInt64Field;
    dtDateTime,
      dtTime,
      dtDate: LoadDateTimeField;
    dtExtended,
      dtCurrency: LoadFloatField;
    dtBoolean: LoadBooleanField;
    dtWord: LoadWordField;
    dtByte: LoadByteField;
  else
    LoadStringField;
  end;
end;

procedure TRPCRecord.LoadFromXMLDOMNode(const ARecordNode: XmlNode);
var
  I: Integer;
begin
  for I := 0 to ARecordNode.ChildNodes.Count - 1 do
    LoadFieldFromXMLNode(ARecordNode.ChildNodes[I]);
end;
procedure TRPCRecord.LoadFromStream(Stream: Stream;
  const BufferFormat: TApBufferFormat);  platform;
var
  I, FFieldCount: Word;
begin
  TStream(Stream).read(FFieldCount, SizeOf(FFieldCount));
  for I := 0 to FFieldCount - 1 do
    LoadFieldFromStream(Stream, BufferFormat);
end;

procedure TRPCRecord.SaveFieldToStream(Stream: Stream; const Index: Integer;
  const BufferFormat: TApBufferFormat);  platform;

  procedure SaveStreamField;
  var
    FRPCStreamField: TRPCStreamField;
    FSize: Integer;
  begin
    FRPCStreamField := TRPCStreamField(RPCFields[Index]);
    if Assigned(FRPCStreamField.Value) then
    begin
      FSize := FRPCStreamField.Value.Size;
      TStream(Stream).write(FSize, SizeOf(FSize));
      FRPCStreamField.Value.Position := 0;
      TStream(Stream).CopyFrom(FRPCStreamField.Value, FSize);
    end
    else
    begin
      FSize := 0;
      TStream(Stream).write(FSize, SizeOf(FSize));
    end;
  end;

  procedure SaveIntegerField;
  begin
    with TRPCIntegerField(RPCFields[Index]) do
      TStream(Stream).write(Value, SizeOf(Value));
  end;

  procedure SaveInt64Field;
  begin
    with TRPCInt64Field(RPCFields[Index]) do
      TStream(Stream).write(Value, SizeOf(Value));
  end;

  procedure SaveDateTimeField;
  begin
    with TRPCDateTimeField(RPCFields[Index]) do
      TStream(Stream).write(Value, SizeOf(Value));
  end;

  procedure SaveFloatField;
  begin
    with TRPCFloatField(RPCFields[Index]) do
      TStream(Stream).write(Value, SizeOf(Value));
  end;

  procedure SaveWordField;
  begin
    with TRPCWordField(RPCFields[Index]) do
      TStream(Stream).write(Value, SizeOf(Value));
  end;

  procedure SaveByteField;
  begin
    with TRPCByteField(RPCFields[Index]) do
      TStream(Stream).write(Value, SizeOf(Value));
  end;

  procedure SaveBooleanField;
  begin
    with TRPCBooleanField(RPCFields[Index]) do
      TStream(Stream).write(Value, SizeOf(Value));
  end;

  procedure SaveStringField;
  const
    FNull: Byte = 0;
  begin
    with TRPCStringField(RPCFields[Index]) do
    begin
      if Value <> '' then
        TStream(Stream).write(AnsiEncoding.GetBytes(Value), Length(Value));
      TStream(Stream).write(FNull, SizeOf(FNull));
    end;
  end;

var
  FDataType: Byte;
  FField: TRPCField;
begin
  FField := RPCFields[Index];
  FDataType := Ord(FField.FieldType);
  //Saves DataType
  TStream(Stream).write(FDataType, SizeOf(FDataType));
  case TRPCFieldType(FDataType) of
    dtBlob: SaveStreamField;
    dtInteger: SaveIntegerField;
    dtLargeint: SaveInt64Field;
    dtDateTime,
      dtTime,
      dtDate: SaveDateTimeField;
    dtExtended,
      dtCurrency: SaveFloatField;
    dtBoolean: SaveBooleanField;
    dtWord: SaveWordField;
    dtByte: SaveByteField;
  else
    SaveStringField;
  end;
end;

procedure TRPCRecord.SaveFieldToXML(XML: XmlTextWriter; const Index: Integer);
  function GetFieldName: String;
  begin
    Result := FBuffer.GetFieldName(Index);
    if Result = '' then
      Result := Format('Field_%d', [Index]);
  end;

  function GetFieldDataType: String;
  begin
    Result := XMLFieldTypes[RPCFields[Index].FieldType];
  end;

  procedure SaveStreamField;
  var
    FBytes: TBytes;
  begin
    if Assigned(TRPCStreamField(RPCFields[Index]).Value) and (TRPCStreamField(RPCFields[Index]).Value.Size > 0) then
    begin
      TRPCStreamField(RPCFields[Index]).Value.Position := 0;
      SetLength(FBytes, TRPCStreamField(RPCFields[Index]).Value.Size);
      try
        TRPCStreamField(RPCFields[Index]).Value.read(FBytes, 0, TRPCStreamField(RPCFields[Index]).Value.Size);
        XML.WriteBinHex(FBytes, 0, Length(FBytes));
      finally
        SetLength(FBytes, 0);
      end;
    end;
  end;

  procedure SaveIntegerField;
  begin
    XML.WriteValue(TRPCIntegerField(RPCFields[Index]).Value);
  end;

  procedure SaveInt64Field;
  begin
    XML.WriteValue(TRPCInt64Field(RPCFields[Index]).Value);
  end;

  procedure SaveDateTimeField;
  begin
    XML.WriteValue(TRPCDateTimeField(RPCFields[Index]).Value);
  end;

  procedure SaveFloatField;
  begin
    XML.WriteValue(TRPCFloatField(RPCFields[Index]).Value);
  end;

  procedure SaveWordField;
  begin
    XML.WriteValue(TRPCWordField(RPCFields[Index]).Value);
  end;

  procedure SaveByteField;
  begin
    XML.WriteValue(TRPCByteField(RPCFields[Index]).Value);
  end;

  procedure SaveBooleanField;
  begin
    XML.WriteValue(TRPCBooleanField(RPCFields[Index]).Value);
  end;

  procedure SaveStringField;
  begin
    XML.WriteValue(TRPCStringField(RPCFields[Index]).Value);
  end;
begin
  XML.WriteStartElement(GetFieldName);
  try
    XML.WriteAttributeString('dataType', GetFieldDataType);
    case RPCFields[Index].FieldType of
      dtBlob: SaveStreamField;
      dtInteger: SaveIntegerField;
      dtLargeint: SaveInt64Field;
      dtDateTime,
        dtTime,
        dtDate: SaveDateTimeField;
      dtExtended,
        dtCurrency: SaveFloatField;
      dtBoolean: SaveBooleanField;
      dtWord: SaveWordField;
      dtByte: SaveByteField;
      else
        SaveStringField;
    end;
  finally
    XML.WriteEndElement;
  end;
end;

procedure TRPCRecord.SaveToXML(XML: XmlTextWriter);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if I = 0 then
      XML.WriteStartElement('record');
    SaveFieldToXML(XML, I);
  end;
  if Count > 0 then
    XML.WriteEndElement;
end;
procedure TRPCRecord.SaveToStream(Stream: Stream;
  const BufferFormat: TApBufferFormat);  platform;
  procedure SaveHeader;
  var
    FFieldCount: Word;
  begin
    FFieldCount := Count;
    TStream(Stream).write(FFieldCount, SizeOf(FFieldCount))
  end;
var
  I: Word;
begin
  //record Header
  SaveHeader;
  //Fields
  for I := 0 to Count - 1 do
    SaveFieldToStream(Stream, I, BufferFormat);
end;

procedure TRPCRecord.SetRPCFields(Index: Integer; const Value: TRPCField);
begin
  Items[Index] := Value;
end;

{ TRPCField }

constructor TRPCField.Create;
begin
  inherited Create;
end;

{ TRPCBufferSortList }

function TRPCBufferSortList.AddSort(const ASortName: String; const AKeys: Array of Word; const Ascending, ACaseSensitive: Boolean): Integer;
begin
  Result := FSortList.IndexOf(LowerCase(ASortName));
  if Result <> -1 then
    TRPCBufferSort(FSortList.Objects[Result]).ClearDataList;
  if Result = -1 then
    Result := FSortList.AddObject(LowerCase(ASortName), TRPCBufferSort.Create(FBuffer, AKeys, Ascending, ACaseSensitive));
  TRPCBufferSort(FSortList.Objects[Result]).Sort;
end;

procedure TRPCBufferSortList.ClearList;
var
  x: Integer;
begin
  for x := 0 to FSortList.Count - 1 do
    if Assigned(FSortList.Objects[x]) then
    begin
      TRPCBufferSort(FSortList.Objects[x]).Free;
      FSortList.Objects[x] := nil;
    end;
  FSortList.Clear;
end;

constructor TRPCBufferSortList.Create(const ABuffer: TRPCBuffer);
begin
  inherited Create;
  FSortList := TStringList.Create;
  FBuffer := ABuffer;
end;

procedure TRPCBufferSortList.DeleteRecord(const ARecord: TRPCRecord);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    GetSortItem(I).DeleteRecord(ARecord);
end;

function TRPCBufferSortList.DeleteSort(ASortIndex: Integer): Boolean;
begin
  Result := False;
  if (ASortIndex < 0) or (ASortIndex > FSortList.Count) then
    Exit;
  TRPCBufferSort(FSortList.Objects[ASortIndex]).Free;
  FSortList.Delete(ASortIndex);
end;

destructor TRPCBufferSortList.Destroy;
begin
  ClearList;
  FSortList.Free;
  inherited;
end;

procedure TRPCBufferSortList.GenerateNeededUpdates(const AFieldIndex: Integer; const ARecord: TRPCRecord; var AList: TList);
var
  I: Integer;
  FSortItem: TRPCBufferSort;
begin
  for I := 0 to FSortList.Count - 1 do
  begin
    FSortItem := TRPCBufferSort(FSortList.Objects[I]);
    if FSortItem.FieldIndexIsKey(AFieldIndex) then
    begin
      if not Assigned(AList) then
        AList := TList.Create;
      FSortItem.DeleteRecord(ARecord);
      AList.Add(FSortItem);
    end;
  end;
end;

function TRPCBufferSortList.GetCount: Integer;
begin
  Result := FSortList.Count;
end;

function TRPCBufferSortList.GetSortByName(const ASortName: String): TRPCBufferSort;
var
  x: Integer;
begin
  x := FSortList.IndexOf(LowerCase(ASortName));
  if x <> -1 then
    Result := TRPCBufferSort(FSortList.Objects[x])
  else
    Result := nil;
end;

function TRPCBufferSortList.GetSortIndex(const ASortName: String): Integer;
begin
  Result := FSortList.IndexOf(LowerCase(ASortName));
end;

function TRPCBufferSortList.GetSortItem(AIndex: Integer): TRPCBufferSort;
begin
  if (AIndex + 1) > FSortList.Count then
    Result := nil
  else
    Result := TRPCBufferSort(FSortList.Objects[AIndex]);
end;

function TRPCBufferSortList.IndexOfSort(
  const ASort: TRPCBufferSort): SmallInt;
begin
  Result := FSortList.IndexOfObject(ASort);
end;

procedure TRPCBufferSortList.UpdateNeededUpdates(const ARecord: TRPCRecord; var AList: TList);
var
  I: Integer;
begin
  if Assigned(AList) then
  begin
    try
      for I := 0 to AList.Count - 1 do
        TRPCBufferSort(AList[I]).InsertIndexed(ARecord);
    finally
      FreeAndNil(AList);
    end;
  end;
end;


{ TFieldNames }

destructor TFieldNames.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Assigned(Objects[I]) then
    begin
      TFieldNameInfo(Objects[I]).Free;
      Objects[I] := nil;
    end;
  end;
  inherited;
end;

{ TRPCBufferDataList }

procedure TRPCBufferDataList.Clear;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    if Assigned(Items[i]) then
      TObject(Items[i]).Free;
  inherited;
end;

destructor TRPCBufferDataList.Destroy;
begin
  Clear;
  inherited;
end;

{ TRPCBufferSort }

procedure TRPCBufferSort.AddRecord(const ARecord: TRPCRecord);
begin
  FDataList.Add(ARecord);
end;

procedure TRPCBufferSort.ClearDataList;
begin
  FDataList.Clear;
end;

function TRPCBufferSort.CompareKeys(const Item: TRPCRecord;
  const AKeyValues: Array of String; const ACaseSensitive: Boolean): Integer;
var
  I: Byte;
  FDummy: String;
begin
  Result := 0;
  for I := low(AKeyValues) to high(AKeyValues) do
  begin
    if (FKeys[I] > (Item.Count - 1)) then
    begin
      Result := -1;
      Break;
    end;
    case Item.RPCFields[FKeys[I]].FieldType of
      dtInteger: FDummy := IntToStr(TRPCIntegerField(Item.RPCFields[FKeys[I]]).Value);
      dtLargeint: FDummy := IntToStr(TRPCInt64Field(Item.RPCFields[FKeys[I]]).Value);
      dtDateTime,
        dtDate,
        dtTime: FDummy := FloatToStr(TRPCDateTimeField(Item.RPCFields[FKeys[I]]).Value);
      dtBoolean: FDummy := IfThen(TRPCBooleanField(Item.RPCFields[FKeys[I]]).Value, '1', '0');
      dtExtended,
        dtCurrency: FDummy := FloatToStr(TRPCFloatField(Item.RPCFields[FKeys[I]]).Value);
      dtWord: FDummy := IntToStr(TRPCWordField(Item.RPCFields[FKeys[I]]).Value);
      dtByte: FDummy := IntToStr(TRPCByteField(Item.RPCFields[FKeys[I]]).Value);
      dtBlob: raise Exception.Create('Blob Field cannot be used as Key field');
    else
      FDummy := TRPCStringField(Item.RPCFields[FKeys[I]]).Value;
    end;
    if ACaseSensitive then
      Result := CompareStr(FDummy, AKeyValues[I])
    else
      Result := CompareText(FDummy, AKeyValues[I]);
    if Result <> 0 then
      Break;
  end;
  if not FAscending then
    Result := Result * (-1);
end;

function TRPCBufferSort.CompareKeys(const Item: TRPCRecord;
  const AKeyValues: Array of Integer; const ACaseSensitive: Boolean): Integer;
var
  I: Byte;
  FDummy: Double;
  FDummy64: Int64;
begin
  Result := 0;
  for I := low(AKeyValues) to high(AKeyValues) do
  begin
    if (FKeys[I] > (Item.Count - 1)) then
    begin
      Result := -1;
      Break;
    end;
    case Item.RPCFields[FKeys[I]].FieldType of
      dtInteger: Result := TRPCIntegerField(Item.RPCFields[FKeys[I]]).Value - AKeyValues[I];
      dtLargeint:
        begin
          FDummy64 := TRPCInt64Field(Item.RPCFields[FKeys[I]]).Value - AKeyValues[I];
          if FDummy64 = 0 then
            Result := 0
          else if FDummy64 > 0 then
            Result := 1
          else
            Result := -1;
        end;
      dtDateTime,
        dtDate,
        dtTime:
        begin
          FDummy := (TRPCDateTimeField(Item.RPCFields[FKeys[I]]).Value - AKeyValues[I]);
          if FDummy = 0 then
            Result := 0
          else if FDummy > 0 then
            Result := 1
          else
            Result := -1;
        end;
      dtBoolean: Result := Integer(TRPCBooleanField(Item.RPCFields[FKeys[I]]).Value) - Integer(AKeyValues[I]);
      dtExtended,
        dtCurrency:
        begin
          FDummy := (TRPCFloatField(Item.RPCFields[FKeys[I]]).Value - AKeyValues[I]);
          if FDummy = 0 then
            Result := 0
          else if FDummy > 0 then
            Result := 1
          else
            Result := -1;
        end;
      dtWord: Result := TRPCWordField(Item.RPCFields[FKeys[I]]).Value - AKeyValues[I];
      dtByte: Result := TRPCByteField(Item.RPCFields[FKeys[I]]).Value - AKeyValues[I];
      dtBlob: raise Exception.Create('Blob Field cannot be used as Key field');
    else
      begin
        if ACaseSensitive then
          Result := CompareStr(TRPCStringField(Item.RPCFields[FKeys[I]]).Value, IntToStr(AKeyValues[I]))
        else
          Result := CompareText(TRPCStringField(Item.RPCFields[FKeys[I]]).Value, IntToStr(AKeyValues[I]));
      end;
    end;
    if Result <> 0 then
      Break;
  end;
  if not FAscending then
    Result := Result * (-1);
end;

function TRPCBufferSort.CompareKeys(const Item1, Item2: TRPCRecord;
  const ACaseSensitive: Boolean): Integer;
var
  I: Byte;
  FDummy: Double;
  FStrDummy: String;
begin
  Result := 0;
  for I := low(FKeys) to high(FKeys) do
  begin
    if (FKeys[I] > (Item1.Count - 1)) then
    begin
      if (FKeys[I] > (Item2.Count - 1)) then
      begin
        Result := 0;
        Continue;
      end
      else
      begin
        Result := -1;
        Break;
      end;
    end
    else if (FKeys[I] > (Item2.Count - 1)) then
    begin
      Result := 1;
      Break;
    end;
    case Item1.RPCFields[FKeys[I]].FieldType of
      dtInteger: Result := CompareValue(TRPCIntegerField(Item1.RPCFields[FKeys[I]]).Value, TRPCIntegerField(Item2.RPCFields[FKeys[I]]).Value);
      dtLargeint: Result := CompareValue(TRPCInt64Field(Item1.RPCFields[FKeys[I]]).Value, TRPCInt64Field(Item2.RPCFields[FKeys[I]]).Value);
      dtDateTime,
        dtDate,
        dtTime:
        begin
          FDummy := (TRPCDateTimeField(Item1.RPCFields[FKeys[I]]).Value - TRPCDateTimeField(Item2.RPCFields[FKeys[I]]).Value);
          if FDummy = 0 then
            Result := 0
          else if FDummy > 0 then
            Result := 1
          else
            Result := -1;
        end;
      dtBoolean: Result := Integer(TRPCBooleanField(Item1.RPCFields[FKeys[I]]).Value) - Integer(TRPCBooleanField(Item2.RPCFields[FKeys[I]]).Value);
      dtExtended,
        dtCurrency:
        begin
          FDummy := (TRPCFloatField(Item1.RPCFields[FKeys[I]]).Value - TRPCFloatField(Item2.RPCFields[FKeys[I]]).Value);
          if FDummy = 0 then
            Result := 0
          else if FDummy > 0 then
            Result := 1
          else
            Result := -1;
        end;
      dtWord: Result := TRPCWordField(Item1.RPCFields[FKeys[I]]).Value - TRPCWordField(Item2.RPCFields[FKeys[I]]).Value;
      dtByte: Result := TRPCByteField(Item1.RPCFields[FKeys[I]]).Value - TRPCByteField(Item2.RPCFields[FKeys[I]]).Value;
      dtBlob: raise Exception.Create('Blob Field cannot be used as Key field');
    else
      begin
        FStrDummy := Item2.GetFieldAsString(Item2.RPCFields[FKeys[I]]);
        if ACaseSensitive then
          Result := CompareStr(TRPCStringField(Item1.RPCFields[FKeys[I]]).Value, FStrDummy)
        else
          Result := CompareText(TRPCStringField(Item1.RPCFields[FKeys[I]]).Value, FStrDummy);
      end;
    end;
    if Result <> 0 then
      Break;
  end;
  if not FAscending then
    Result := Result * (-1);
end;

function TRPCBufferSort.CompareKeys(const Item: TRPCRecord;
  const AKeyValues: Array of Variant; const ACaseSensitive: Boolean): Integer;
var
  I: Byte;
  FDummy: Double;
  FDummy64: Int64;
begin
  Result := 0;
  for I := low(AKeyValues) to high(AKeyValues) do
  begin
    if (FKeys[I] > (Item.Count - 1)) then
    begin
      Result := -1;
      Break;
    end;
    case Item.RPCFields[FKeys[I]].FieldType of
      dtInteger: Result := TRPCIntegerField(Item.RPCFields[FKeys[I]]).Value - AKeyValues[I];
      dtLargeint:
        begin
          FDummy64 := TRPCInt64Field(Item.RPCFields[FKeys[I]]).Value - AKeyValues[I];
          if FDummy64 = 0 then
            Result := 0
          else if FDummy64 > 0 then
            Result := 1
          else
            Result := -1;
        end;
      dtDateTime,
        dtDate,
        dtTime:
        begin
          FDummy := (TRPCDateTimeField(Item.RPCFields[FKeys[I]]).Value - AKeyValues[I]);
          if FDummy = 0 then
            Result := 0
          else if FDummy > 0 then
            Result := 1
          else
            Result := -1;
        end;
      dtBoolean: Result := Integer(TRPCBooleanField(Item.RPCFields[FKeys[I]]).Value) - Integer(AKeyValues[I]);
      dtExtended,
        dtCurrency:
        begin
          FDummy := (TRPCFloatField(Item.RPCFields[FKeys[I]]).Value - AKeyValues[I]);
          if FDummy = 0 then
            Result := 0
          else if FDummy > 0 then
            Result := 1
          else
            Result := -1;
        end;
      dtWord: Result := TRPCWordField(Item.RPCFields[FKeys[I]]).Value - AKeyValues[I];
      dtByte: Result := TRPCByteField(Item.RPCFields[FKeys[I]]).Value - AKeyValues[I];
      dtBlob: raise Exception.Create('Blob Field cannot be used as Key field');
    else
      begin
        if ACaseSensitive then
          Result := CompareStr(TRPCStringField(Item.RPCFields[FKeys[I]]).Value, AKeyValues[I])
        else
          Result := CompareText(TRPCStringField(Item.RPCFields[FKeys[I]]).Value, AKeyValues[I]);
      end;
    end;
    if Result <> 0 then
      Break;
  end;
  if not FAscending then
    Result := Result * (-1);
end;

constructor TRPCBufferSort.Create(const ABuffer: TRPCBuffer;
  const AKeys: Array of Word; const Ascending, ACaseSensitive: Boolean);
var
  I: Byte;
begin
  inherited Create;
  FCurrentItem := 0;
  SetLength(FKeys, Length(AKeys));
  for I := 0 to High(AKeys) do
    FKeys[I] := AKeys[I];
  FBuffer := ABuffer;
  FAscending := Ascending;
  FCaseSensitive := ACaseSensitive;
  FDataList := TList.Create;
end;

procedure TRPCBufferSort.DeleteRecord(const ARecord: TRPCRecord);
var
  AIndex: Integer;
begin
  if FindKey(ARecord, True, AIndex) then
    FDataList.Delete(AIndex);
end;

destructor TRPCBufferSort.Destroy;
begin
  if Assigned(FDataList) then
    FreeAndNil(FDataList);
  inherited;
end;

function TRPCBufferSort.FieldIndexIsKey(const AFieldIndex: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := low(FKeys) to high(FKeys) do
  begin
    if FKeys[I] = AFieldIndex then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TRPCBufferSort.FindKey(const ARecord: TRPCRecord; const Exact: Boolean;
  var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareKeys(TRPCRecord(FDataList.Items[I]), ARecord, FCaseSensitive);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        if FAscending then
        begin
          if I > 0 then
          begin
            repeat
              Dec(I);
            until (I = 0) or (CompareKeys(TRPCRecord(FDataList.Items[I]), ARecord, FCaseSensitive) <> 0);
            // Se chegou ao topo, e não é igual o proximo é o item
            if (CompareKeys(TRPCRecord(FDataList.Items[I]), ARecord, FCaseSensitive) <> 0) and (I < (Count - 1)) then
              Inc(I);
          end;
        end
        else
        begin
          if I < (Count - 1) then
          begin
            repeat
              Inc(I);
            until (I = (Count - 1)) or (CompareKeys(TRPCRecord(FDataList.Items[I]), ARecord, FCaseSensitive) <> 0);
            if (CompareKeys(TRPCRecord(FDataList.Items[I]), ARecord, FCaseSensitive) <> 0) and (I > 0) then
              Dec(I);
          end;
        end;
        Result := True;
        L := I;
      end;
    end;
  end;
  if Result and Exact then
  begin
    I := L;
    while (I >= 0) and (I < Count) and (CompareKeys(TRPCRecord(FDataList.Items[I]), ARecord, FCaseSensitive) = 0) and (TRPCRecord(FDataList.Items[I]) <> ARecord) do
    begin
      if FAscending then
        Inc(I)
      else Dec(I);
    end;
    if (I >= 0) and (I < Count) then
      Result := (TRPCRecord(FDataList.Items[I]) = ARecord)
    else Result := False;
    L := I;
  end;
  Index := L;
end;

function TRPCBufferSort.FindKey(const AKeyValues: Array of String;
  var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        if FAscending then
        begin
          if I > 0 then
          begin
            repeat
              Dec(I);
            until (I = 0) or (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0);
            // Se chegou ao topo, e não é igual o proximo é o item
            if (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0) and (I < (Count - 1)) then
              Inc(I);
          end;
        end
        else
        begin
          if I < (Count - 1) then
          begin
            repeat
              Inc(I);
            until (I = (Count - 1)) or (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0);
            if (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0) and (I > 0) then
             Dec(I);
          end;
        end;
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function TRPCBufferSort.FindKey(const AKeyValues: Array of Variant;
  var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive);
    if C < 0 then
      L := I + 1
    else
    if C > 0 then
      H := I - 1
    else begin
      if FAscending then
      begin
        if I > 0 then
        begin
          repeat
            Dec(I);
          until (I = 0) or (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0);
          // Se chegou ao topo, e não é igual o proximo é o item
          if (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0) and (I < (Count - 1)) then
            Inc(I);
        end;
      end
      else
      begin
        if I < (Count - 1) then
        begin
          repeat
            Inc(I);
          until (I = (Count - 1)) or (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0);
          if (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0) and (I > 0) then
            Dec(I);
        end;
      end;
      Result := True;
      L := I;
      Break;
    end;
  end;
  Index := L;
end;

function TRPCBufferSort.FindKey(const AKeyValues: Array of Integer;
  var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        if FAscending then
        begin
          if I > 0 then
          begin
            repeat
              Dec(I);
            until (I = 0) or (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0);
            // Se chegou ao topo, e não é igual o proximo é o item
            if (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0) and (I < (Count - 1)) then
              Inc(I);
          end;
        end
        else
        begin
          if I < (Count - 1) then
          begin
            repeat
              Inc(I);
            until (I = (Count - 1)) or (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0);
            if (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0) and (I > 0) then
             Dec(I);
          end;
        end;
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function TRPCBufferSort.GetCount: Integer;
begin
  Result := FDataList.Count;
end;

function TRPCBufferSort.GetCurrentRecordPointer: Integer;
begin
  Result := RecordPointer[FCurrentItem];
end;

function TRPCBufferSort.GetRecordPointer(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index <= (Count -1)) then
    Result := FBuffer.FDataList.IndexOf(FDataList.Items[Index])
  else Result := -1;
end;

function TRPCBufferSort.GetRPCRecord(const Index: Integer): TRPCRecord;
begin
  Result := TRPCRecord(FDataList.Items[Index]);
end;

procedure TRPCBufferSort.InsertIndexed(const ARecord: TRPCRecord);
var
  Index: Integer;
begin
  FindKey(ARecord, False, Index);
  FDataList.Insert(Index, ARecord);
end;

procedure TRPCBufferSort.QuickSort(L, R: Integer);
var
  I, J: Integer;
  P, T: TRPCRecord;
begin
  repeat
    I := L;
    J := R;
    P := RPCRecord[(L + R) shr 1];
    repeat
      while CompareKeys(RPCRecord[I], P, FCaseSensitive) < 0 do
        Inc(I);
      while CompareKeys(RPCRecord[J], P, FCaseSensitive) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := RPCRecord[I];
        RPCRecord[I] := RPCRecord[J];
        RPCRecord[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TRPCBufferSort.SetRPCRecord(const Index: Integer;
  const Value: TRPCRecord);
begin
  FDataList.Items[Index] := Value;
end;

procedure TRPCBufferSort.Sort;
var
  I: Integer;
begin
  ClearDataList;
  if FBuffer.HasData then
  begin
    for I := 0 to FBuffer.Count - 1 do
      FDataList.Add(FBuffer.Records[I]);
    QuickSort(0, Count - 1);
  end;
end;

{ ERPCBufferConversionError }

constructor ERPCBufferConversionError.Create(const Index: Integer;
  const FieldType: TRPCFieldType);
begin
  inherited Create(Format('Error in RPCBuffer type Conversion. FieldType: %s Index: %d',
    [GetEnumName(TypeInfo(TRPCFieldType), Integer(FieldType)), Index]));
end;

{ TRPCBuffer }

procedure TRPCBuffer.AddFieldNameAndType(const Value: String;
  AFieldType: TRPCFieldType);
begin
  SetFieldName(FFieldNames.Count, Value);
  TFieldNameInfo(FFieldNames.Objects[FFieldNames.IndexOf(Value)]).FieldType := AFieldType;
end;

procedure TRPCBuffer.AdjustFieldTypeAndSize(var Field: TRPCField;
  const AFieldType: TRPCFieldType; FieldSize: Cardinal;
  const FieldIndex: Integer);
begin
  GetCurrentRecord.AdjustFieldTypeAndSize(Field, AFieldType, FieldIndex, FieldSize);
end;

procedure TRPCBuffer.Append;
var
  I, idx: integer;
  FieldInfoType : TRPCFieldType;
begin
  if not Assigned(FFieldNames) or (FFieldNames.Count = 0) then
    raise Exception.Create('Append method need FieldNames definion !!!');
  // Se forem tipados gera os campos já com o Tipo
  for I := 0 to FFieldNames.Count - 1 do
  begin
    idx := FindNameIndex(I);
    FieldInfoType := TFieldNameInfo(FFieldNames.Objects[idx]).FieldType;
    case FieldInfoType of
      // String
      dtString  : WriteTypedFields(I = 0, [''], [FieldInfoType]);
      // Integers
      dtByte,
      dtWord,
      dtInteger,
      dtLargeint: WriteTypedFields(I = 0, [0], [FieldInfoType]);
      // Dates
      dtDateTime,
      dtTime,
      dtDate   : WriteTypedFields(I = 0, [0.0],   [FieldInfoType]);
      // Boolean
      dtBoolean: WriteTypedFields(I = 0, [False], [FieldInfoType]);
      // Float
      dtExtended,
      dtCurrency: WriteTypedFields(I = 0, [0.0], [FieldInfoType]);
    else // dtPChar, dtNotTyped, dtObject, dtVariant dtBlob //???
      WriteTypedFields(I = 0, [''], [dtNotTyped]);
    end;
  end;
end;

procedure TRPCBuffer.AppendFields(const Args: Array of Variant);
begin
  WriteFields(True, Args);
end;

procedure TRPCBuffer.AppendRecord(const From: TRPCBuffer);
var
  I: Integer;
begin
  if From.HasData then
  begin
    for I := 0 to From.FieldCount - 1 do
      WriteFieldFromBufferField(I = 0, From, I);
  end;
end;

function TRPCBuffer.AppendRPCField(const ADataType: TRPCFieldType): TRPCField;
var
  FCurrentRecord: TRPCRecord;
begin
  FCurrentRecord := GetCurrentRecord;
  Result := FCurrentRecord.AddField(ADataType);
end;

function TRPCBuffer.AppendRPCRecord: TRPCRecord;
begin
  Result := TRPCRecord.Create;
  FRecordPointer := FDataList.Add(Result);
  Result.FBuffer := Self;
end;

procedure TRPCBuffer.AppendStreamField(Data: Stream);
begin
  WriteStreamField(True, Data, False);
end;

procedure TRPCBuffer.AppendTypedFields(const Args: Array of Variant;
  const Types: Array of TRPCFieldType);
begin
  WriteTypedFields(True, Args, Types);
end;

procedure TRPCBuffer.AssignBuffer(From: TRPCBuffer);
var
 k:Cardinal;
 I: Integer;
begin
  if Assigned(From) and (From <> Self) then
  begin
    Rewrite;
    if Assigned(From.FFieldNames) then
    begin
      FreeFieldNames;
      for I := 0 to From.FFieldNames.Count - 1 do
        FieldName[TFieldNameInfo(From.FFieldNames.Objects[I]).Index] := From.FieldName[TFieldNameInfo(From.FFieldNames.Objects[I]).Index];
    end;
    if (From.HasData) then
    begin
      k := from.RecNo;
      From.First;
      while not From.EOF do
      begin
        AppendRecord(From);
        From.Next;
      end;
      First;
      from.JumpTo(k);
    end;
  end;
end;

function TRPCBuffer.AsString: String;
var
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  try
    GetBufferAsStrings(StrList);
    Result := StrList.Text;
  finally
    FreeAndNil(StrList);
  end;
end;

procedure TRPCBuffer.Clear;
begin
  if Assigned(FDataList) then
  begin
    SetCurrentSortIndex('');
    while Count > 0 do
      RemoveRPCRecord(Count - 1);
    if Assigned(FSortIndex) then
      FSortIndex.ClearList;
    FHasChanged := True;
    ClearDataList;
  end;  
end;

procedure TRPCBuffer.ClearDataList;
begin
  FDataList.Clear;
end;

constructor TRPCBuffer.Create;
begin
  inherited Create;
  FDataList  := TRPCBufferDataList.Create;
  FSortIndex := TRPCBufferSortList.Create(Self);
  FFieldNames := nil;
  FCurrentSortIndex := nil;
  FRecordPointer := -1;
end;

procedure TRPCBuffer.Delete;
begin
  if HasData then
  begin
    RemoveRPCRecord(FRecordPointer);
    Prior;
  end;
end;

destructor TRPCBuffer.Destroy;
begin
  FreeDataObjects;
  inherited Destroy;
end;

function TRPCBuffer.FindFieldIndexByName(const Name: String): Integer;
begin
  Result := IntGetFieldIndexByName(Name, False);
end;

function TRPCBuffer.FindKey(const SortName: String;
  const KeyFields: Array of Variant; const AKeySensitive: Boolean): Boolean;
begin
  Result := FindKey(FSortIndex.GetSortIndex(SortName), KeyFields, AKeySensitive);
end;

function TRPCBuffer.FindKey(const SortIndex: Integer;
  const KeyFields: Array of Variant; const AKeySensitive: Boolean): Boolean;
var
  I: Integer;
begin
  if (SortIndex >= 0) and Assigned(FSortIndex.GetSortItem(SortIndex)) then
  begin
    FCurrentSortIndex := FSortIndex.Items[SortIndex];
    Result := FCurrentSortIndex.FindKey(KeyFields, I);
    if Result then
    begin
      FCurrentSortIndex.CurrentItem := I;
      FRecordPointer := FCurrentSortIndex.CurrentRecordPointer;
    end;
  end
  else
    Result := False;
end;

function TRPCBuffer.FindNameIndex(const Index: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  if Assigned(FFieldNames) then
  begin
    for I := 0 to FFieldNames.Count - 1 do
    begin
      if TFieldNameInfo(FFieldNames.Objects[I]).Index = Index then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

function TRPCBuffer.FindNearestKey(const SortName: String;
  const KeyFields: Array of Variant; const AKeySensitive: Boolean): Boolean;
begin
  Result := FindNearestKey(FSortIndex.GetSortIndex(SortName), KeyFields, AKeySensitive);
end;

function TRPCBuffer.FindNearestKey(const SortIndex: Integer;
  const KeyFields: Array of Variant; const AKeySensitive: Boolean): Boolean;
var
  I: Integer;
begin
  if (SortIndex >= 0) and Assigned(FSortIndex.GetSortItem(SortIndex)) then
  begin
    FCurrentSortIndex := FSortIndex.Items[SortIndex];
    Result := FCurrentSortIndex.FindKey(KeyFields, I);
    FCurrentSortIndex.CurrentItem := Min(FCurrentSortIndex.Count - 1, I);
    FRecordPointer := FCurrentSortIndex.CurrentRecordPointer;
  end
  else
    Result := False;
end;

procedure TRPCBuffer.First;
begin
  if not Assigned(FCurrentSortIndex) then
  begin
    FRecordPointer := 0;
    FHitBof := True;
    FHitEof := RecordCount = 0;
  end
  else
  begin
    Seek(FCurrentSortIndex.RecordPointer[0]);
    FCurrentSortIndex.CurrentItem := -1;
  end;
end;

procedure TRPCBuffer.FreeDataObjects;
begin
  FreeAndNil(FDataList);
  FreeAndNil(FSortIndex);
  FreeFieldNames;
end;

procedure TRPCBuffer.FreeFieldNames;
begin
  if Assigned(FFieldNames) then
    FreeAndNil(FFieldNames);
end;

procedure TRPCBuffer.FreeRPCBufferBookMark(var ABookMark: TRPCBufferBookMark);
begin
  FreeAndNil(ABookMark);
end;

function TRPCBuffer.GetBlobField(const nIndex: Integer): TBytes;
begin
  try
    if GetFieldType(nIndex) = dtBlob then
      with TRPCStreamField(GetCurrentRecord.GetRPCFields(nIndex)) do
        if Assigned(Value) then
          Result := TMemoryStream(Value).Memory;
  except
    raise ERPCBufferConversionError.Create(nIndex, dtBlob);
  end;
end;

function TRPCBuffer.GetBlockedStreamField(const FieldNb: Integer;
  Data: Stream): Integer;
begin
  Result := GetStreamField(FieldNb, Data, False);
end;

function TRPCBuffer.GetBof: Boolean;
begin
  if not Assigned(FCurrentSortIndex) then
    Result := (FHitBof)
  else
    Result := (FCurrentSortIndex.CurrentItem = -1)
end;

function TRPCBuffer.GetBooleanField(const nIndex: Integer): Boolean;
begin
  try
    Result := TRPCRecord.GetFieldAsBoolean(GetCurrentRecord.GetRPCFields(nIndex));
  except
    raise ERPCBufferConversionError.Create(nIndex, dtBoolean);
  end;
end;

function TRPCBuffer.GetByteField(const nIndex: Integer): Byte;
begin
  try
    Result := GetIntegerField(nIndex);
  except
    raise ERPCBufferConversionError.Create(nIndex, dtByte);
  end;
end;

function TRPCBuffer.GetCardinalField(const nIndex: Integer): Cardinal;
begin
{$R-}
  Result := GetIntegerField(nIndex);
{$R+}
end;

function TRPCBuffer.GetCharField(const nIndex: Integer): Char;
var
  temp: String;
begin
  Result := '/';
  temp := GetStringField(nIndex);
  if Length(temp) > 0 then
    Result := temp[1];
end;

function TRPCBuffer.getCount: Integer;
begin
  Result := FDataList.Count;
end;

function TRPCBuffer.GetCurrencyField(const nIndex: Integer): Currency;
begin
  try
    Result := GetExtendedField(nIndex);
  except
    raise ERPCBufferConversionError.Create(nIndex, dtCurrency);
  end;
end;

function TRPCBuffer.GetCurrentRecord: TRPCRecord;
begin
  if not HasData then
    RaiseException('No data available');
  if FRecordPointer >= 0 then
    Result := Records[FRecordPointer]
  else
    Result := nil;
end;

function TRPCBuffer.GetDateField(const nIndex: Integer): TDateTime;
begin
  try
    Result := Trunc(GetExtendedField(nIndex));
  except
    raise ERPCBufferConversionError.Create(nIndex, dtDate);
  end;
end;

function TRPCBuffer.GetDateTimeField(const nIndex: Integer): TDateTime;
begin
  try
    Result := TRPCRecord.GetFieldAsFloat(GetCurrentRecord.GetRPCFields(nIndex));
  except
    raise ERPCBufferConversionError.Create(nIndex, dtDateTime);
  end;
end;

function TRPCBuffer.GetEof: Boolean;
begin
  if not Assigned(FCurrentSortIndex) then
    Result := (FHitEof)
  else
    Result := (not HasData) or ((FCurrentSortIndex.Count) = FCurrentSortIndex.CurrentItem);
end;

function TRPCBuffer.GetExtendedField(const nIndex: Integer): Double;
begin
  try
    Result := TRPCRecord.GetFieldAsFloat(GetCurrentRecord.GetRPCFields(nIndex));
  except
    raise ERPCBufferConversionError.Create(nIndex, dtExtended);
  end;
end;

function TRPCBuffer.GetFieldByNameAsBoolean(const Name: String): Boolean;
begin
  Result := IntGetFieldAsBoolean(GetFieldIndexByName(Name));
end;

function TRPCBuffer.GetFieldByNameAsByte(const Name: String): Byte;
begin
  Result := IntGetFieldAsByte(GetFieldIndexByName(Name));
end;

function TRPCBuffer.GetFieldByNameAsCardinal(const Name: String): Cardinal;
begin
{$R-}
  Result := GetFieldByNameAsInteger(Name);
{$R+}
end;

function TRPCBuffer.GetFieldByNameAsDateTime(const Name: String): TDateTime;
begin
  Result := IntGetFieldAsDateTime(GetFieldIndexByName(Name));
end;

function TRPCBuffer.GetFieldByNameAsFloat(const Name: String): Double;
begin
  Result := IntGetFieldAsFloat(GetFieldIndexByName(Name));
end;

function TRPCBuffer.GetFieldByNameAsInt64(const Name: String): Int64;
begin
  Result := IntGetFieldAsInt64(GetFieldIndexByName(Name));
end;

function TRPCBuffer.GetFieldByNameAsInteger(const Name: String): Integer;
begin
  Result := IntGetFieldAsInteger(GetFieldIndexByName(Name));
end;

function TRPCBuffer.GetFieldByNameAsString(const Name: String): String;
begin
  Result := IntGetFieldAsString(GetFieldIndexByName(Name));
end;

function TRPCBuffer.GetFieldByNameAsWord(const Name: String): Word;
begin
  Result := IntGetFieldAsWord(GetFieldIndexByName(Name));
end;

function TRPCBuffer.GetFieldByNameFieldType(const Name: String): TRPCFieldType;
begin
  Result := FieldType[GetFieldIndexByName(Name)];
end;

function TRPCBuffer.GetFieldCount: Integer;
begin
  if not HasData then
    Result := 0
  else
    Result := GetCurrentRecord.Count;
end;

function TRPCBuffer.GetFieldIndexByName(const Name: String): Integer;
begin
  Result := IntGetFieldIndexByName(Name, True);
end;

function TRPCBuffer.GetFieldName(const nIndex: Integer): String;
var
  I: Integer;
begin
  if Assigned(FFieldNames) then
  begin
    I := FindNameIndex(nIndex);
    if I >= 0 then
      Result := FFieldNames[I]
    else
      Result := '';
  end
  else
    Result := '';
end;

function TRPCBuffer.GetFields(const nIndex: Integer): String;
begin
  Result := TRPCRecord.GetFieldAsString(GetRPCFields(nIndex));
end;

function TRPCBuffer.GetFieldSize(const nIndex: Integer): Integer;
var
  Field: TRPCField;
begin
  Field := GetRPCFields(nIndex);
  with Field do
  begin
    case FieldType of
      dtBlob: begin
        if Assigned(TRPCStreamField(Field).Value) then
          Result := TRPCStreamField(Field).Value.Size
        else
          Result := 0;
      end;
      dtInteger: Result := SizeOf(Integer);
      dtLargeint: Result := SizeOf(Int64);
      dtDateTime,
        dtExtended,
        dtCurrency,
        dtTime,
        dtDate: Result := SizeOf(Double);
      dtBoolean: Result := SizeOf(Boolean);
      dtByte: Result := SizeOf(Byte);
      dtWord: Result := SizeOf(Word);
    else
      Result := Length(TRPCStringField(Field).Value);
    end;
  end;
end;

function TRPCBuffer.GetFieldType(const nIndex: Integer): TRPCFieldType;
begin
  Result := GetCurrentRecord[nIndex].FieldType;
end;

function TRPCBuffer.GetHasData: Boolean;
begin
  Result := Count > 0;
end;

function TRPCBuffer.GetInt64Field(const nIndex: Integer): Int64;
begin
  try
    Result := TRPCRecord.GetFieldAsInt64(GetCurrentRecord.GetRPCFields(nIndex));
  except
    raise ERPCBufferConversionError.Create(nIndex, dtLargeint);
  end;
end;

function TRPCBuffer.GetIntegerField(const nIndex: Integer): Integer;
begin
  try
    Result := TRPCRecord.GetFieldAsInteger(GetCurrentRecord.GetRPCFields(nIndex));
  except
    raise ERPCBufferConversionError.Create(nIndex, dtInteger);
  end;
end;

function TRPCBuffer.getIsSorted: Boolean;
begin
  Result := FSortIndex.Count > 0;
end;

function TRPCBuffer.GetPCharField(const nIndex: Integer): TBytes;
begin
  try
    Result := GetPCharField(nIndex);
  except
    raise ERPCBufferConversionError.Create(nIndex, dtObject);
  end;
end;

function TRPCBuffer.GetRecNo: Integer;
begin
  if HasData then
    Result := FRecordPointer
  else
    Result := 0;
end;

function TRPCBuffer.GetRecordCount: Integer;
begin
  Result := FDataList.Count;
end;

function TRPCBuffer.GetRecords(Index: Integer): TRPCRecord;
begin
  Result := TRPCRecord(FDataList.Items[Index]);
end;

function TRPCBuffer.GetRPCBufferBookMark: TRPCBufferBookMark;
begin
  Result := TRPCBufferBookMarkStruct.Create;
  Result.CurrentIndex := FSortIndex.IndexOfSort(FCurrentSortIndex);
  if Assigned(FCurrentSortIndex) then
    Result.RecordPointer := FCurrentSortIndex.CurrentItem
  else Result.RecordPointer := FRecordPointer;
end;

function TRPCBuffer.GetRPCFields(Index: Integer): TRPCField;
var
  FCurrentRecord: TRPCRecord;
begin
  if not HasData then
    RaiseException('No data available');
  if (Index < 0) then
    RaiseException('Invalid field Index');

  if FRecordPointer = -1 then
    First;
  FCurrentRecord := GetCurrentRecord;
  if InRange(Index, 0, FCurrentRecord.Count - 1) then
    Result := FCurrentRecord[Index]
  else
    raise Exception.CreateFmt('Field Index %d does not exists', [Index]);
end;

function TRPCBuffer.GetStreamField(const FieldNb: Integer; Data: Stream;
  const Rewrite: Boolean): Integer;
begin
  if GetRPCFields(FieldNb).FieldType = dtBlob then
  begin
    with TRPCStreamField(GetRPCFields(FieldNb)) do
    begin
      if Rewrite then
        TStream(Data).Size := 0;
      if Assigned(Value) then
      begin
        Value.Position := 0;
        Result := TStream(Data).CopyFrom(Value, Value.Size);
      end
      else
        Result := 0;
    end;
  end else raise Exception.CreateFmt('Field %d is not a Blob field. Current type is %s', [FieldNb, GetEnumName(TypeInfo(TRPCFieldType), Ord(GetRPCFields(FieldNb).FieldType))]);
end;

function TRPCBuffer.GetStreamField(const FieldIndex: Integer): Stream;
begin
  Result := TMemoryStream.Create;
  GetStreamField(FieldIndex, Result);
end;

function TRPCBuffer.GetStringField(const nIndex: Integer): String;
begin
  Result := '';
  try
    Result := GetFields(nIndex);
  except
    raise ERPCBufferConversionError.Create(nIndex, dtString);
  end;
end;

function TRPCBuffer.GetTimeField(const nIndex: Integer): TDateTime;
begin
  try
    Result := Frac(GetExtendedField(nIndex));
  except
    raise ERPCBufferConversionError.Create(nIndex, dtTime);
  end;
end;

function TRPCBuffer.GetVariantField(const nIndex: Integer): TObject;
begin
  try
    Result := GetStringField(nIndex);
  except
    raise ERPCBufferConversionError.Create(nIndex, dtVariant);
  end;
end;

function TRPCBuffer.GetWordField(const nIndex: Integer): Word;
begin
  try
    Result := GetIntegerField(nIndex);
  except
    raise ERPCBufferConversionError.Create(nIndex, dtWord);
  end;
end;

procedure TRPCBuffer.GotoRPCBufferBookMark(const ABookMark: TRPCBufferBookMark);
begin
  if ABookMark.CurrentIndex >= 0 then
  begin
    FCurrentSortIndex := FSortIndex.GetSortItem(ABookMark.CurrentIndex);
    FRecordPointer := FCurrentSortIndex.RecordPointer[ABookMark.RecordPointer];
    FCurrentSortIndex.CurrentItem := ABookMark.RecordPointer;
  end else
    JumpTo(ABookMark.RecordPointer)
end;

function TRPCBuffer.IndexJumpTo(const RecNumber: Integer): Boolean;
begin
  Result := JumpTo(RecNumber);
end;

function TRPCBuffer.IntFindKey(const SortName: String;
  const KeyFields: Array of Integer; const AKeySensitive: Boolean): Boolean;
begin
  Result := IntFindKey(FSortIndex.GetSortIndex(SortName), KeyFields, AKeySensitive);
end;

function TRPCBuffer.IntFindKey(const SortIndex: Integer;
  const KeyFields: Array of Integer; const AKeySensitive,
  Inverted: Boolean): Boolean;
var
  I: Integer;
begin
  if (SortIndex >= 0) and Assigned(FSortIndex.GetSortItem(SortIndex)) then
  begin
    FCurrentSortIndex := FSortIndex.Items[SortIndex];
    Result := FCurrentSortIndex.FindKey(KeyFields, I);
    if Result then
    begin
      FCurrentSortIndex.CurrentItem := I;
      FRecordPointer := FCurrentSortIndex.CurrentRecordPointer;
    end;
  end
  else
    Result := False;
end;

function TRPCBuffer.IntFindNearestKey(const SortIndex: Integer;
  const KeyFields: Array of Integer; const AKeySensitive: Boolean): Boolean;
var
  I: Integer;
begin
  if (SortIndex >= 0) and Assigned(FSortIndex.GetSortItem(SortIndex)) then
  begin
    FCurrentSortIndex := FSortIndex.Items[SortIndex];
    Result := FCurrentSortIndex.FindKey(KeyFields, I);
    FCurrentSortIndex.CurrentItem := Min(FCurrentSortIndex.Count, I);
    FRecordPointer := FCurrentSortIndex.CurrentRecordPointer;
  end
  else
    Result := False;
end;

function TRPCBuffer.IntFindNearestKey(const SortName: String;
  const KeyFields: Array of Integer; const AKeySensitive: Boolean): Boolean;
begin
  Result := IntFindNearestKey(FSortIndex.GetSortIndex(SortName), KeyFields, AKeySensitive);
end;

function TRPCBuffer.IntGetFieldAsBoolean(const nIndex: Integer): Boolean;
var
  FField: TRPCField;
begin
  FField := GetRPCFields(nIndex);
  Result := TRPCRecord.GetFieldAsBoolean(FField);
end;

function TRPCBuffer.IntGetFieldAsByte(const nIndex: Integer): Byte;
var
  FField: TRPCField;
begin
  FField := GetRPCFields(nIndex);
  Result := TRPCRecord.GetFieldAsByte(FField);
end;

function TRPCBuffer.IntGetFieldAsCardinal(const nIndex: Integer): Cardinal;
begin
{$R-}
  Result := IntGetFieldAsInteger(nIndex);
{$R+}
end;

function TRPCBuffer.IntGetFieldAsDateTime(const nIndex: Integer): TDateTime;
var
  FField: TRPCField;
begin
  FField := GetRPCFields(nIndex);
  Result := TRPCRecord.GetFieldAsFloat(FField);
end;

function TRPCBuffer.IntGetFieldAsFloat(const nIndex: Integer): Double;
var
  FField: TRPCField;
begin
  FField := GetRPCFields(nIndex);
  Result := TRPCRecord.GetFieldAsFloat(FField);
end;

function TRPCBuffer.IntGetFieldAsInt64(const nIndex: Integer): Int64;
var
  FField: TRPCField;
begin
  FField := GetRPCFields(nIndex);
  Result := TRPCRecord.GetFieldAsInt64(FField);
end;

function TRPCBuffer.IntGetFieldAsInteger(const nIndex: Integer): Integer;
var
  FField: TRPCField;
begin
  FField := GetRPCFields(nIndex);
  Result := TRPCRecord.GetFieldAsInteger(FField);
end;

function TRPCBuffer.IntGetFieldAsString(const nIndex: Integer): String;
var
  FField: TRPCField;
begin
  FField := GetRPCFields(nIndex);
  Result := TRPCRecord.GetFieldAsString(FField);
end;

function TRPCBuffer.IntGetFieldAsWord(const nIndex: Integer): Word;
var
  FField: TRPCField;
begin
  FField := GetRPCFields(nIndex);
  Result := TRPCRecord.GetFieldAsWord(FField);
end;

function TRPCBuffer.IntGetFieldIndexByName(const Name: String; const ARaiseException: Boolean): Integer;
var
  I: Integer;
begin
  Result := -1;
  if Assigned(FFieldNames) then
  begin
    if (FFieldNames.Find(Name, I)) then
      Result := TFieldNameInfo(FFieldNames.Objects[I]).Index;
  end;
  if (Result = -1) and ARaiseException then
    raise Exception.CreateFmt('Field %s not found in field names defined to buffer', [Name]);
end;

procedure TRPCBuffer.IntSetFieldAsBoolean(const nIndex: Integer;
  const Value: Boolean);
var
  FField: TRPCField;
begin
  FField := GetRPCFields(nIndex);
  SetFieldAsBoolean(FField, Value, nIndex);
end;

procedure TRPCBuffer.IntSetFieldAsByte(const nIndex: Integer;
  const Value: Byte);
begin
  IntSetFieldAsInteger(nIndex, Value);
end;

procedure TRPCBuffer.IntSetFieldAsCardinal(const nIndex: Integer;
  const Value: Cardinal);
begin
{$R-}
  IntSetFieldAsInteger(nIndex, Value);
{$R+}
end;

procedure TRPCBuffer.IntSetFieldAsDateTime(const nIndex: Integer;
  const Value: TDateTime);
var
  FField: TRPCField;
begin
  FField := GetRPCFields(nIndex);
  SetFieldAsFloat(FField, Value, nIndex);
end;

procedure TRPCBuffer.IntSetFieldAsFloat(const nIndex: Integer;
  const Value: Double);
var
  FField: TRPCField;
begin
  FField := GetRPCFields(nIndex);
  SetFieldAsFloat(FField, Value, nIndex);
end;

procedure TRPCBuffer.IntSetFieldAsInt64(const nIndex: Integer;
  const Value: Int64);
var
  FField: TRPCField;
begin
  FField := GetRPCFields(nIndex);
  SetFieldAsInt64(FField, Value, nIndex);
end;

procedure TRPCBuffer.IntSetFieldAsInteger(const nIndex, Value: Integer);
var
  FField: TRPCField;
begin
  FField := GetRPCFields(nIndex);
  SetFieldAsInteger(FField, Value, nIndex);
end;

procedure TRPCBuffer.IntSetFieldAsString(const nIndex: Integer;
  const Value: String);
begin
  SetFields(nIndex, Value);
end;

procedure TRPCBuffer.IntSetFieldAsWord(const nIndex: Integer;
  const Value: Word);
begin
  IntSetFieldAsInteger(nIndex, Value);
end;

function TRPCBuffer.IsEqualTo(const ABuffer: TRPCBuffer): Boolean;
var
  I: Integer;
  J: Integer;
  AMyField, AFromField: TRPCField;
begin
  Result := RecordCount = ABuffer.RecordCount;
  I := 0;
  while Result and (I < Count) do
  begin
    Result := Records[I].Count = ABuffer.Records[I].Count;
    J := 0;
    while Result and (J < Records[I].Count) do
    begin
      AMyField := Records[I].GetRPCFields(J);
      AFromField := ABuffer.Records[I].GetRPCFields(J);
      Result := AMyField.FieldType = AFromField.FieldType;
      if Result then
      begin
        case AMyField.FieldType of
          dtBlob:
            begin
              Result := (Assigned(TRPCStreamField(AMyField).Value) and Assigned(TRPCStreamField(AFromField).Value)) or
                (not Assigned(TRPCStreamField(AMyField).Value) and not Assigned(TRPCStreamField(AFromField).Value));
              if Result and Assigned(TRPCStreamField(AMyField).Value) then
              begin

                Result := TRPCStreamField(AMyField).Value.Size = TRPCStreamField(AFromField).Value.Size;
                if Result then
                  Result := TRPCStreamField(AMyField).Equals(TRPCStreamField(AFromField).Value);
              end;
            end;
          dtInteger: Result := TRPCIntegerField(AMyField).Value = TRPCIntegerField(AFromField).Value;
          dtLargeint: Result := TRPCInt64Field(AMyField).Value = TRPCInt64Field(AFromField).Value;
          dtBoolean: Result := TRPCBooleanField(AMyField).Value = TRPCBooleanField(AFromField).Value;
          dtExtended,
            dtCurrency: Result := TRPCFloatField(AMyField).Value = TRPCFloatField(AFromField).Value;
          dtDateTime,
            dtDate,
            dtTime: Result := TRPCDateTimeField(AMyField).Value = TRPCDateTimeField(AFromField).Value;
          dtWord: Result := TRPCWordField(AMyField).Value = TRPCWordField(AFromField).Value;
          dtByte: Result := TRPCByteField(AMyField).Value = TRPCByteField(AFromField).Value;
        else
          Result := TRPCStringField(AMyField).Value = TRPCStringField(AFromField).Value;
        end;
      end;
      Inc(J);
    end;
    Inc(I);
  end;
end;

function TRPCBuffer.JumpTo(const RecNo: Integer): Boolean;
begin
  if HasData and InRange(RecNo, 0, Count - 1) then
  begin
    SetCurrentSortIndex('');
    FRecordPointer := RecNo;
    // walter - 27/07/2006 - igual "Seek"
    FHitEof := False;
    FHitBof := False;
    //
    Result := True;
  end
  else
    Result := False;
end;

procedure TRPCBuffer.Last;
begin
  if HasData then
  begin
    if not Assigned(FCurrentSortIndex) then
    begin
      FRecordPointer := RecordCount - 1;
      FHitEof := True;
      FHitBof := False;
    end
    else
    begin
      Seek(FCurrentSortIndex.RecordPointer[FCurrentSortIndex.Count - 1]);
      FCurrentSortIndex.CurrentItem := FCurrentSortIndex.Count - 1;
    end;
  end;
end;

procedure TRPCBuffer.LoadFromDataSet(const Source: TDataSet; const InitialField,
  FinalField: Integer; const ALoadFieldNames: Boolean);
var
  I: Integer;
  FBookMark: TBookmark;
begin
  if Assigned(Source) and not Source.IsEmpty then
  begin
    FBookMark := Source.GetBookmark;
    try
      if ALoadFieldNames then
      begin
        if Assigned(FFieldNames) then
          FreeFieldNames;
        for I := 0 to Source.FieldCount - 1 do
          FieldName[I] := Source.Fields[I].FieldName;
      end;
      Source.First;
      while not Source.EOF do
      begin
        LoadFromRecordDataSet(Source, InitialField, FinalField);
        Source.Next;
      end;
      First;
    finally
      Source.GotoBookmark(FBookMark);
      Source.FreeBookmark(FBookMark);
    end;
  end;
end;

procedure TRPCBuffer.LoadFromDelimitedFile(const FileName, Delimiter: String);
var
  FileBuffer: TextFile;
  Buff: String;
  LineRead: String;
  I: Integer;
  NewRec: Boolean;
begin
  AssignFile(FileBuffer, FileName);
  try
    Reset(FileBuffer);
    Rewrite;
    First;
    while True do
    begin
      NewRec := True;
      ReadLn(FileBuffer, LineRead);
      if LineRead = '' then
        Break;
      for I := 1 to Length(LineRead) do
      begin
        if LineRead[I] = #10 then
          Continue;
        if (LineRead[I] = Delimiter) or (LineRead[I] = #13) then
        begin
          WriteFields(NewRec, [buff]);
          buff := '';
          NewRec := False;
          Continue;
        end;
        buff := buff + LineRead[I];
      end;
    end;
  finally
    CloseFile(FileBuffer);
  end;
  FHasChanged := True;
end;

procedure TRPCBuffer.LoadFromFile(const NewRec: Boolean;
  const FileName: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    WriteStreamField(NewRec, Stream, True);
  finally
    Stream.Free;
  end;
  FHasChanged := True;
end;

function TRPCBuffer.GetSortIndex(const ASortName: String): Integer;
begin
  Result := FSortIndex.GetSortIndex(ASortName);
end;

procedure TRPCBuffer.LoadFromList(const NewRec: Boolean; const List: TStrings);
var
  I: Integer;
begin
  if List <> nil then
  begin
    for I := 0 to List.Count - 1 do
      WriteFields((NewRec and (I = 0)), [List.Strings[I]]);
  end;
  FHasChanged := True;
end;

procedure TRPCBuffer.LoadFromPChar(const Value: TBytes; const Size: Integer;
  const BufferFormat: TApBufferFormat);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.write(Value, 0, Size);
    Stream.Position := 0;
    LoadFromStream(Stream, Stream.Size, BufferFormat);
  finally
    Stream.Free;
  end;
end;

procedure TRPCBuffer.LoadFromRecordDataSet(const Source: TDataSet;
  const InitialField, FinalField: Integer);
var
  I, FFinalField: Integer;
begin
  if Assigned(Source) and not Source.IsEmpty then
  begin
    if FinalField = -1 then
      FFinalField := Source.FieldCount - 1
    else
      FFinalField := FinalField;
    for I := InitialField to FFinalField do
      WriteFields((I = InitialField), [Source.Fields[I]]);
  end;
end;

procedure TRPCBuffer.LoadFromXMLStream(Stream: Stream);
var
  I: Integer;
  FXMLDoc: XmlDocument;
  FXMLReader: XmlTextReader;
begin
  Stream.Position := 0;
  FXMLReader := XmlTextReader.Create(Stream);
  try
    FXMLDoc := XmlDocument.Create;
    try
      FXMLDoc.Load(FXMLReader);
      if Assigned(FXMLDoc.Item['buffer']) then
        for I := 0 to FXMLDoc.Item['buffer'].ChildNodes.Count - 1 do
          AppendRPCRecord.LoadFromXMLDOMNode(FXMLDoc.Item['buffer'].ChildNodes[I])
      else raise Exception.Create('Invalid XML Structure buffer element not found');
    finally
      FXMLDoc.Free;
    end;
  finally
    FXMLReader.Free;
  end;
end;

procedure TRPCBuffer.LoadFromStream(Stream: Stream;
  const BufferFormat: TApBufferFormat);
begin
  LoadFromStream(Stream, 0, BufferFormat);
end;

procedure TRPCBuffer.LoadFromStream(Stream: Stream; Len: Integer;
  const BufferFormat: TApBufferFormat);  platform;
var
  FHasData: Boolean;
begin
  if HasData then
    Rewrite;
  TStream(Stream).Position := 0;
  TStream(Stream).read(FHasData, SizeOf(FHasData));
  if FHasData then
  begin
    while TStream(Stream).Position < TStream(Stream).Size do
      AppendRPCRecord.LoadFromStream(Stream, BufferFormat);
    First;
  end;
end;

procedure TRPCBuffer.MirrorSort(const SortName: String;
  const BuildKey: Array of Word);
begin
  Sort(SortName, BuildKey);
end;

function TRPCBuffer.NewGetFields(const nIndex: Integer): String;
begin
  Result := GetFields(nIndex);
end;

procedure TRPCBuffer.Next;
begin
  if HasData then
  begin
    if not Assigned(FCurrentSortIndex) then
    begin
      FHitBof := False;
      Inc(FRecordPointer);
      if FRecordPointer >= RecordCount then
      begin
        FRecordPointer := RecordCount - 1;
        FHitEof := True;
      end;
    end
    else
    begin
      if FCurrentSortIndex.CurrentItem < 0 then
      FCurrentSortIndex.CurrentItem := FCurrentSortIndex.CurrentItem + 2 //Anula o BOF
      else FCurrentSortIndex.CurrentItem := FCurrentSortIndex.CurrentItem + 1;
      if GetEof then
        FCurrentSortIndex.CurrentItem := FCurrentSortIndex.Count
      else
        Seek(FCurrentSortIndex.CurrentRecordPointer);
    end;
  end;
end;

procedure TRPCBuffer.Prior;
begin
  if HasData then
  begin
    if not Assigned(FCurrentSortIndex) then
    begin
      FHitEof := False;
      Dec(FRecordPointer);
      if FRecordPointer < 0 then
      begin
        FRecordPointer := 0;
        FHitBof := True;
      end;
    end
    else
    begin
      FCurrentSortIndex.CurrentItem := FCurrentSortIndex.CurrentItem - 1;
      if GetBof then
        FCurrentSortIndex.CurrentItem := -1
      else
        Seek(FCurrentSortIndex.CurrentRecordPointer);
    end;
  end;
end;

procedure TRPCBuffer.RaiseException(const Where: String);
begin
  raise Exception.Create(Where);
end;

procedure TRPCBuffer.ReadComponent(Instance: TComponent);
var
  MemoryStream: TMemoryStream;
  c: TComponent;
begin
  MemoryStream := TMemoryStream.Create;
  try
    GetStreamField(0, MemoryStream);
    MemoryStream.Position := 0;
    //MemoryStream.ReadComponent( Instance ); -> walter 18/11/2004
    c := TComponent.Create(nil);
    try
      c := MemoryStream.ReadComponent(nil);
      Instance.Assign(c);
    finally
      FreeAndNil(c);
    end;
  finally
    MemoryStream.Free;
  end;
end;

procedure TRPCBuffer.ReadRPCBuffer(const nIndex: Integer; Buffer: TRPCBuffer;
  const BufferFormat: TApBufferFormat);
var
  AMemory: TMemoryStream;
begin
  AMemory := TMemoryStream.Create;
  try
    if GetStreamField(nIndex, AMemory) > 0 then
    begin
      AMemory.Position := 0;
      Buffer.LoadFromStream(AMemory, AMemory.Size, BufferFormat);
    end;
  finally
    AMemory.Free;
  end;
end;

procedure TRPCBuffer.RemoveRPCRecord(const Index: Integer);
begin
  if Assigned(FSortIndex) then
    FSortIndex.DeleteRecord(Records[Index]);
  Records[Index].Free;
  FDataList.Delete(Index);
end;

function TRPCBuffer.RemoveSort(const ASortIndex: Integer): Boolean;
begin
  Result := False;
  if ASortIndex > (FSortIndex.Count - 1) then
    Exit;
  if FSortIndex.Items[ASortIndex] = FCurrentSortIndex then
    FCurrentSortIndex := nil;
  Result := FSortIndex.DeleteSort(ASortIndex);
end;

function TRPCBuffer.RemoveSort(const ASortName: String): Boolean;
begin
  Result := RemoveSort(FSortIndex.GetSortIndex(ASortName));
end;

procedure TRPCBuffer.Rewrite(const AClearFieldNames: Boolean);
begin
  Clear;
  if AClearFieldNames then
    FreeFieldNames;
  First;
end;

procedure TRPCBuffer.SaveToFile(const FieldNo: Integer; const FileName: String);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  Self.GetStreamField(FieldNo, Stream);
  try
    Stream.SaveToFile(FileName);
  finally
    Stream.Free;
  end;
end;

procedure TRPCBuffer.SaveToXMLStream(Stream: Stream);
var
  FXML: XmlTextWriter;
  I: Integer;
begin
  FXML := XmlTextWriter.Create(Stream, System.Text.Encoding.default);
  try
    FXML.WriteStartDocument;
    try
      FXML.WriteStartElement('buffer');
      try
        for I := 0 to Count - 1 do
          Records[I].SaveToXML(FXML);
      finally
        FXML.WriteEndElement;
      end;
    finally
      FXML.WriteEndDocument;
    end;
    FXML.Flush;
    FXML.Close;
  finally
    FXML.Free;
  end;
end;

procedure TRPCBuffer.SaveToStream(Stream: Stream;
  const BufferFormat: TApBufferFormat);  platform;
  procedure SaveNewHeader; 
  var
    FHasData: Boolean;
  begin
    FHasData := HasData;
    TStream(Stream).write(FHasData, SizeOf(FHasData));
  end;
var
  I: Integer;  
begin
  if BufferFormat = bfXML then
    SaveToXMLStream(Stream)
  else
  begin
    //Header
    SaveNewHeader;
    //Save Records
    for I := 0 to Count - 1 do
      Records[I].SaveToStream(Stream, BufferFormat);
  end;
end;

procedure TRPCBuffer.GetBufferAsStrings(AList: TStrings);
var
  I, J: Integer;
  S: String;
begin
  J := Recno;
  try
    AList.Clear;
    First;
    while not EOF do
    begin
      S := '';
      for I := 0 to FieldCount - 1 do
      begin
        case FieldType[I] of
          dtString, dtDateTime, dtTime, dtDate, dtPChar:
            S := S + QuotedStr(NewFields[I]);
          dtNotTyped, dtLargeint, dtInteger, dtWord, dtByte, dtExtended, dtCurrency:
            S := S + NewFields[I];
          dtBoolean:
            S := S + BoolToStr(GetBooleanField(I), True);
          dtBlob:
            S := S + '(BLOB)';
          dtObject:
            S := S + '(OBJ)';
          else
            S := S + '(BIN)';
        end;
        S := S + ';';
      end;
      SetLength(S, Length(S) - 1);
      AList.Add(S);
      Next;
    end;
  finally
    JumpTo(J);
  end;
end;

procedure TRPCBuffer.SaveToClip;
var
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  try
    GetBufferAsStrings(StrList);
    Clipboard.AsText := StrList.Text;
  finally
    FreeAndNil(StrList);
  end;
end;

procedure TRPCBuffer.SaveToTextFile(const AFileName: String);
var
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  try
    GetBufferAsStrings(StrList);
    StrList.SaveToFile(AFileName);
  finally
    FreeAndNil(StrList);
  end;
end;

procedure TRPCBuffer.Seek(ARecNo: Integer);
begin
  if InRange(ARecNo, 0, RecordCount) then
  begin
    FRecordPointer := ARecNo;
    FHitEof := False;
    FHitBof := False;
  end
  else if ARecNo >= 0 then
    raise Exception.Create(' Seek raised out of record set');
end;

procedure TRPCBuffer.SetCurrentSortIndex(const SortName: String);
var
  Item: Integer;
begin
  if Trim(SortName) = '' then
    FCurrentSortIndex := nil
  else
  begin
    Item := FSortIndex.GetSortIndex(SortName);
    if Item <> -1 then
      FCurrentSortIndex := FSortIndex.Items[Item]
    else
      raise Exception.Create('The SortIndex Informed in the SetCurrentSortIndex function not found ! ');
  end;
end;

procedure TRPCBuffer.SetFieldAsBoolean(var Field: TRPCField; Value: Boolean;
  const FieldIndex: Integer);  platform;
var
  SValue: String;
  AList: TList;
begin
  if not Assigned(Field) then
    Field := AppendRPCField(dtBoolean);
  AdjustFieldTypeAndSize(Field, Field.FieldType, 1, FieldIndex);
  AList := nil;
  FSortIndex.GenerateNeededUpdates(FieldIndex, GetCurrentRecord, AList);
  try
    case Field.FieldType of
      dtBlob:
        begin
          SValue := IfThen(Value, '1', '0');
          TRPCStreamField(Field).Value.Size := 0;
          TRPCStreamField(Field).Value.write(SValue[1], Length(SValue));
        end;
      dtInteger: TRPCIntegerField(Field).Value := Integer(Value);
      dtLargeint: TRPCInt64Field(Field).Value := Int64(Value);
      dtDateTime,
        dtExtended,
        dtCurrency,
        dtTime,
        dtDate: TRPCFloatField(Field).Value := Integer(Value);
      dtBoolean: TRPCBooleanField(Field).Value := Value;
      dtByte: TRPCByteField(Field).Value := Integer(Value);
      dtWord: TRPCWordField(Field).Value := Integer(Value);
    else
      TRPCStringField(Field).Value := IfThen(Value, '1', '0');
    end;
  finally
    FSortIndex.UpdateNeededUpdates(GetCurrentRecord, AList);
  end;
end;

procedure TRPCBuffer.SetFieldAsFloat(var Field: TRPCField; Value: Double;
  const FieldIndex: Integer);  platform;
var
  SValue: String;
  AList: TList;
begin
  if not Assigned(Field) then
    Field := AppendRPCField(dtExtended);
  AdjustFieldTypeAndSize(Field, Field.FieldType, Length(FloatToStr(Value)), FieldIndex);
  AList := nil;
  FSortIndex.GenerateNeededUpdates(FieldIndex, GetCurrentRecord, AList);
  try
    case Field.FieldType of
      dtBlob:
        begin
          SValue := FloatToStr(Value);
          TRPCStreamField(Field).Value.Size := 0;
          TRPCStreamField(Field).Value.write(SValue[1], Length(SValue));
        end;
      dtInteger: TRPCIntegerField(Field).Value := Trunc(Value);
      dtLargeint: TRPCInt64Field(Field).Value := Trunc(Value);
      dtExtended,
      dtCurrency: TRPCFloatField(Field).Value := Value;
      dtDateTime,
        dtTime,
        dtDate: TRPCDateTimeField(Field).Value := Value;
      dtBoolean: TRPCBooleanField(Field).Value := (1 = Value);
      dtByte: TRPCByteField(Field).Value := Trunc(Value);
      dtWord: TRPCWordField(Field).Value := Trunc(Value);
    else
      TRPCStringField(Field).Value := FloatToStr(Value);
    end;
  finally
    FSortIndex.UpdateNeededUpdates(GetCurrentRecord, AList);
  end;
end;

procedure TRPCBuffer.SetFieldAsInt64(var Field: TRPCField; Value: Int64;
  const FieldIndex: Integer);  platform;
var
  SValue: String;
  AList: TList;
begin
  if not Assigned(Field) then
    Field := AppendRPCField(dtLargeint);
  AdjustFieldTypeAndSize(Field, Field.FieldType, Length(IntToStr(Value)), FieldIndex);
  AList := nil;
  FSortIndex.GenerateNeededUpdates(FieldIndex, GetCurrentRecord, AList);
  try
    case Field.FieldType of
      dtBlob:
        begin
          SValue := IntToStr(Value);
          TRPCStreamField(Field).Value.Size := 0;
          TRPCStreamField(Field).Value.write(SValue[1], Length(SValue));
        end;
      dtInteger: TRPCIntegerField(Field).Value := Value;
      dtLargeint: TRPCInt64Field(Field).Value := Value;
      dtDateTime,
        dtExtended,
        dtCurrency,
        dtTime,
        dtDate: TRPCFloatField(Field).Value := Value;
      dtBoolean: TRPCBooleanField(Field).Value := (1 = Value);
      dtByte: TRPCByteField(Field).Value := Value;
      dtWord: TRPCWordField(Field).Value := Value;
    else
      TRPCStringField(Field).Value := IntToStr(Value);
    end;
  finally
    FSortIndex.UpdateNeededUpdates(GetCurrentRecord, AList);
  end;
end;

procedure TRPCBuffer.SetFieldAsInteger(var Field: TRPCField; Value: Integer;
  const FieldIndex: Integer); platform;
var
  SValue: String;
  AList: TList;
begin
  if not Assigned(Field) then
    Field := AppendRPCField(dtInteger);
  AdjustFieldTypeAndSize(Field, Field.FieldType, Length(IntToStr(Value)), FieldIndex);
  AList := nil;
  FSortIndex.GenerateNeededUpdates(FieldIndex, GetCurrentRecord, AList);
  try
    case Field.FieldType of
      dtBlob:
        begin
          SValue := IntToStr(Value);
          TRPCStreamField(Field).Value.Size := 0;
          TRPCStreamField(Field).Value.write(SValue[1], Length(SValue));
        end;
      dtInteger: TRPCIntegerField(Field).Value := Value;
      dtLargeint: TRPCInt64Field(Field).Value := Value;
      dtDateTime,
        dtExtended,
        dtCurrency,
        dtTime,
        dtDate: TRPCFloatField(Field).Value := Value;
      dtBoolean: TRPCBooleanField(Field).Value := (1 = Value);
      dtByte: TRPCByteField(Field).Value := Value;
      dtWord: TRPCWordField(Field).Value := Value;
    else
      TRPCStringField(Field).Value := IntToStr(Value);
    end;
  finally
    FSortIndex.UpdateNeededUpdates(GetCurrentRecord, AList);
  end;
end;

procedure TRPCBuffer.SetFieldAsPChar(var Field: TRPCField; Value: TBytes;
  const Size, FieldIndex: Integer);
var
  AList: TList;
begin
  if not Assigned(Field) then
    Field := AppendRPCField(dtPChar);
  AdjustFieldTypeAndSize(Field, Field.FieldType, Size, FieldIndex);
  AList := nil;  
  FSortIndex.GenerateNeededUpdates(FieldIndex, GetCurrentRecord, AList);
  try

    case Field.FieldType of
      dtBlob:
        begin
          TRPCStreamField(Field).Value.Size := 0;
          if Size > 0 then
            TRPCStreamField(Field).Value.write(Value, Size);
        end;
      dtInteger: TRPCIntegerField(Field).Value := StrToIntDef(StringOf(Value), 0);
      dtLargeint: TRPCInt64Field(Field).Value := StrToInt64Def(StringOf(Value), 0);
      dtDateTime,
        dtExtended,
        dtCurrency,
        dtTime,
        dtDate: TRPCFloatField(Field).Value := StrToFloatDef(StringOf(Value), 0);
      dtBoolean: TRPCBooleanField(Field).Value := (Value = '1');
      dtByte: TRPCByteField(Field).Value := StrToIntDef(StringOf(Value), 0);
      dtWord: TRPCWordField(Field).Value := StrToIntDef(StringOf(Value), 0);
    else
      TRPCStringfield(Field).Value := StringOf(Value);
    end;
  finally
    FSortIndex.UpdateNeededUpdates(GetCurrentRecord, AList);
  end;
end;

procedure TRPCBuffer.SetFieldAsString(var Field: TRPCField; const Value: String;
  const FieldIndex: Integer); platform;
var
  AList: TList;
begin
  if not Assigned(Field) then
    Field := AppendRPCField(dtString);
  AdjustFieldTypeAndSize(Field, Field.FieldType, Length(Value), FieldIndex);
  AList := nil;
  FSortIndex.GenerateNeededUpdates(FieldIndex, GetCurrentRecord, AList);
  try
    case Field.FieldType of
      dtBlob: if Value <> '' then
        begin
          TRPCStreamField(Field).Value.Size := 0;
          TRPCStreamField(Field).Value.write(Value[1], Length(Value));
        end;
      dtInteger: TRPCIntegerField(Field).Value := StrToIntDef(Value, 0);
      dtLargeint: TRPCInt64Field(Field).Value := StrToInt64Def(Value, 0);
      dtDateTime,
        dtExtended,
        dtCurrency,
        dtTime,
        dtDate: TRPCFloatField(Field).Value := StrToFloatDef(Value, 0);
      dtBoolean: TRPCBooleanField(Field).Value := ('1' = Value);
      dtByte: TRPCByteField(Field).Value := StrToIntDef(Value, 0);
      dtWord: TRPCWordField(Field).Value := StrToIntDef(Value, 0);
    else
      TRPCStringField(Field).Value := Value;
    end;
  finally
    FSortIndex.UpdateNeededUpdates(GetCurrentRecord, AList);
  end;
end;

procedure TRPCBuffer.SetFieldByNameAsBoolean(const Name: String;
  const Value: Boolean);
begin
  IntSetFieldAsBoolean(GetFieldIndexByName(Name), Value);
end;

procedure TRPCBuffer.SetFieldByNameAsByte(const Name: String;
  const Value: Byte);
begin
  IntSetFieldAsByte(GetFieldIndexByName(Name), Value);
end;

procedure TRPCBuffer.SetFieldByNameAsCardinal(const Name: String;
  const Value: Cardinal);
begin
{$R-}
  SetFieldByNameAsInteger(Name, Value);
{$R+}
end;

procedure TRPCBuffer.SetFieldByNameAsDateTime(const Name: String;
  const Value: TDateTime);
begin
  IntSetFieldAsDateTime(GetFieldIndexByName(Name), Value);
end;

procedure TRPCBuffer.SetFieldByNameAsFloat(const Name: String;
  const Value: Double);
begin
  IntSetFieldAsFloat(GetFieldIndexByName(Name), Value);
end;

procedure TRPCBuffer.SetFieldByNameAsInt64(const Name: String;
  const Value: Int64);
begin
  IntSetFieldAsInt64(GetFieldIndexByName(Name), Value);
end;

procedure TRPCBuffer.SetFieldByNameAsInteger(const Name: String;
  const Value: Integer);
begin
  IntSetFieldAsInteger(GetFieldIndexByName(Name), Value);
end;

procedure TRPCBuffer.SetFieldByNameAsString(const Name, Value: String);
begin
  IntSetFieldAsString(GetFieldIndexByName(Name), Value);
end;

procedure TRPCBuffer.SetFieldByNameAsWord(const Name: String;
  const Value: Word);
begin
  IntSetFieldAsWord(GetFieldIndexByName(Name), Value);
end;

procedure TRPCBuffer.SetFieldByNameFieldType(const Name: String;
  const Value: TRPCFieldType);
begin
  FieldType[GetFieldIndexByName(Name)] := Value;
end;

procedure TRPCBuffer.SetFieldFromField(var Field: TRPCField;
  FieldFrom: TRPCField);
  procedure SetBlobField;
  begin
    with TRPCStreamField(Field) do
    begin
      case FieldFrom.FieldType of
        dtBlob:
          begin
            if Assigned(Value) then
            begin
              Value.Free;
              Value := nil;
            end;
            if Assigned(TRPCStreamField(FieldFrom).Value) then
            begin
              Value := TMemoryStream.Create;
              TRPCStreamField(FieldFrom).Value.Position := 0;
              Value.CopyFrom(TRPCStreamField(FieldFrom).Value, TRPCStreamField(FieldFrom).Value.Size);
            end;
          end;
        dtInteger: raise Exception.Create('Buffer Integer Field Cannot be changed into Blob Field');
        dtLargeint: raise Exception.Create('Buffer Int64 Field Cannot be changed into Blob Field');
        dtBoolean: raise Exception.Create('Buffer Boolean Field Cannot be changed into Blob Field');
        dtExtended,
          dtCurrency: raise Exception.Create('Buffer Float Field Cannot be changed into Blob Field');
        dtDate,
          dtDateTime,
          dtTime: raise Exception.Create('Buffer Time Field Cannot be changed into Blob Field');
        dtWord: raise Exception.Create('Buffer Word Field Cannot be changed into Blob Field');
        dtByte: raise Exception.Create('Buffer Byte Field Cannot be changed into Blob Field');
      else
        raise Exception.Create('Buffer String Field Cannot be changed into Blob Field');
      end;
    end;
  end;

  procedure SetIntegerField;
  begin
    with TRPCIntegerField(Field) do
    begin
      case FieldFrom.FieldType of
        dtBlob: raise Exception.Create('Buffer Blob Field Cannot be changed into Integer Field');
        dtInteger: Value := TRPCIntegerField(FieldFrom).Value;
        dtLargeint: Value := TRPCInt64Field(FieldFrom).Value;
        dtBoolean: Value := Integer(TRPCBooleanField(FieldFrom).Value);
        dtExtended,
          dtCurrency: Value := Trunc(TRPCFloatField(FieldFrom).Value);
        dtDate,
          dtDateTime,
          dtTime: Value := Trunc(TRPCDateTimeField(FieldFrom).Value);
        dtWord: Value := TRPCWordField(FieldFrom).Value;
        dtByte: Value := TRPCByteField(FieldFrom).Value;
      else
        Value := StrToIntDef(TRPCStringField(FieldFrom).Value, 0);
      end;
    end;
  end;

  procedure SetInt64Field;
  begin
    with TRPCInt64Field(Field) do
    begin
      case FieldFrom.FieldType of
        dtBlob: raise Exception.Create('Buffer Blob Field Cannot be changed into Int64 Field');
        dtInteger: Value := TRPCIntegerField(FieldFrom).Value;
        dtLargeint: Value := TRPCInt64Field(FieldFrom).Value;
        dtBoolean: Value := Int64(TRPCBooleanField(FieldFrom).Value);
        dtExtended,
          dtCurrency: Value := Trunc(TRPCFloatField(FieldFrom).Value);
        dtDate,
          dtDateTime,
          dtTime: Value := Trunc(TRPCDateTimeField(FieldFrom).Value);
        dtWord: Value := TRPCWordField(FieldFrom).Value;
        dtByte: Value := TRPCByteField(FieldFrom).Value;
      else
        Value := StrToInt64Def(TRPCStringField(FieldFrom).Value, 0);
      end;
    end;
  end;

  procedure SetBooleanField;
  begin
    with TRPCBooleanField(Field) do
    begin
      case FieldFrom.FieldType of
        dtBlob: raise Exception.Create('Buffer Blob Field Cannot be changed into Integer Field');
        dtInteger: Value := Boolean(TRPCIntegerField(FieldFrom).Value);
        dtLargeint: Value := Boolean(TRPCInt64Field(FieldFrom).Value);
        dtBoolean: Value := TRPCBooleanField(FieldFrom).Value;
        dtExtended,
          dtCurrency: Value := Boolean(Trunc(TRPCFloatField(FieldFrom).Value));
        dtDate,
          dtDateTime,
          dtTime: Value := Boolean(Trunc(TRPCDateTimeField(FieldFrom).Value));
        dtWord: Value := Boolean(TRPCWordField(FieldFrom).Value);
        dtByte: Value := Boolean(TRPCByteField(FieldFrom).Value);
      else
        Value := TRPCStringField(FieldFrom).Value = '1';
      end;
    end;
  end;

  procedure SetFloatField;
  begin
    with TRPCFloatField(Field) do
    begin
      case FieldFrom.FieldType of
        dtBlob: raise Exception.Create('Buffer Blob Field Cannot be changed into Integer Field');
        dtInteger: Value := TRPCIntegerField(FieldFrom).Value;
        dtLargeint: Value := TRPCInt64Field(FieldFrom).Value;
        dtBoolean: Value := Integer(TRPCBooleanField(FieldFrom).Value);
        dtExtended,
          dtCurrency: Value := TRPCFloatField(FieldFrom).Value;
        dtDate,
          dtDateTime,
          dtTime: Value := TRPCDateTimeField(FieldFrom).Value;
        dtWord: Value := TRPCWordField(FieldFrom).Value;
        dtByte: Value := TRPCByteField(FieldFrom).Value;
      else
        Value := StrToFloatDef(TRPCStringField(FieldFrom).Value, 0);
      end;
    end;
  end;

  procedure SetDateTimeField;
  begin
    with TRPCDateTimeField(Field) do
    begin
      case FieldFrom.FieldType of
        dtBlob: raise Exception.Create('Buffer Blob Field Cannot be changed into Integer Field');
        dtInteger: Value := TRPCIntegerField(FieldFrom).Value;
        dtLargeint: Value := TRPCInt64Field(FieldFrom).Value;
        dtBoolean: Value := Integer(TRPCBooleanField(FieldFrom).Value);
        dtExtended,
          dtCurrency: Value := TRPCFloatField(FieldFrom).Value;
        dtDate,
          dtDateTime,
          dtTime: Value := TRPCDateTimeField(FieldFrom).Value;
        dtWord: Value := TRPCWordField(FieldFrom).Value;
        dtByte: Value := TRPCByteField(FieldFrom).Value;
      else
        Value := StrToFloatDef(TRPCStringField(FieldFrom).Value, 0);
      end;
    end;
  end;

  procedure SetWordField;
  begin
    with TRPCWordField(Field) do
    begin
      case FieldFrom.FieldType of
        dtBlob: raise Exception.Create('Buffer Blob Field Cannot be changed into Integer Field');
        dtInteger: Value := TRPCIntegerField(FieldFrom).Value;
        dtLargeint: Value := TRPCInt64Field(FieldFrom).Value;
        dtBoolean: Value := Word(TRPCBooleanField(FieldFrom).Value);
        dtExtended,
          dtCurrency: Value := Trunc(TRPCFloatField(FieldFrom).Value);
        dtDate,
          dtDateTime,
          dtTime: Value := Trunc(TRPCDateTimeField(FieldFrom).Value);
        dtWord: Value := TRPCWordField(FieldFrom).Value;
        dtByte: Value := TRPCByteField(FieldFrom).Value;
      else
        Value := StrToIntDef(TRPCStringField(FieldFrom).Value, 0);
      end;
    end;
  end;

  procedure SetByteField;
  begin
    with TRPCByteField(Field) do
    begin
      case FieldFrom.FieldType of
        dtBlob: raise Exception.Create('Buffer Blob Field Cannot be changed into Integer Field');
        dtInteger: Value := TRPCIntegerField(FieldFrom).Value;
        dtLargeint: Value := TRPCInt64Field(FieldFrom).Value;
        dtBoolean: Value := Byte(TRPCBooleanField(FieldFrom).Value);
        dtExtended,
          dtCurrency: Value := Trunc(TRPCFloatField(FieldFrom).Value);
        dtDate,
          dtDateTime,
          dtTime: Value := Trunc(TRPCDateTimeField(FieldFrom).Value);
        dtWord: Value := TRPCWordField(FieldFrom).Value;
        dtByte: Value := TRPCByteField(FieldFrom).Value;
      else
        Value := StrToIntDef(TRPCStringField(FieldFrom).Value, 0);
      end;
    end;
  end;

  procedure SetStringField;
  begin
    with TRPCStringField(Field) do
    begin
      case FieldFrom.FieldType of
        dtBlob: raise Exception.Create('Buffer Blob Field Cannot be changed into Integer Field');
        dtInteger: Value := IntToStr(TRPCIntegerField(FieldFrom).Value);
        dtLargeint: Value := IntToStr(TRPCInt64Field(FieldFrom).Value);
        dtBoolean: Value := IfThen(TRPCBooleanField(FieldFrom).Value, '1', '0');
        dtExtended,
          dtCurrency: Value := FloatToStr(TRPCFloatField(FieldFrom).Value);
        dtDate,
          dtDateTime,
          dtTime: Value := FloatToStr(TRPCDateTimeField(FieldFrom).Value);
        dtWord: Value := IntToStr(TRPCWordField(FieldFrom).Value);
        dtByte: Value := IntToStr(TRPCByteField(FieldFrom).Value);
      else
        Value := TRPCStringField(FieldFrom).Value;
      end;
    end;
  end;

begin
  case Field.FieldType of
    dtBlob: SetBlobField;
    dtInteger: SetIntegerField;
    dtLargeint: SetInt64Field;
    dtBoolean: SetBooleanField;
    dtExtended,
      dtCurrency: SetFloatField;
    dtDate,
      dtDateTime,
      dtTime: SetDateTimeField;
    dtWord: SetWordField;
    dtByte: SetByteField;
  else
    SetStringField;
  end;
end;

function TRPCBuffer.SetFieldFromStream(var Field: TRPCField;
  const AStream: Stream; const Size: Cardinal;
  const FieldIndex: Integer): Cardinal;
begin
  AdjustFieldTypeAndSize(Field, Field.FieldType, Size, FieldIndex);
  Result := TRPCStreamField(Field).Value.CopyFrom(AStream, Size);
end;

procedure TRPCBuffer.SetFieldName(const nIndex: Integer; const Value: String);
var
  I: Integer;
  bFound: Boolean;
  FieldInfo: TFieldNameInfo;
begin
  if not Assigned(FFieldNames) then
  begin
    FFieldNames := TFieldNames.Create;
    FFieldNames.Duplicates := dupError;
    FFieldNames.Sorted := True;
  end;
  try
    bFound := False;
    for I:=0 to FFieldNames.Count-1 do
    begin
      if TFieldNameInfo(FFieldNames.Objects[I]).Index = nIndex then
      begin
        FieldInfo := TFieldNameInfo( FFieldNames.Objects[I] );
        FFieldNames.Delete(I);
        bFound := True;
        Break;
      end;
    end;
    if not bFound then
    begin
      FieldInfo := TFieldNameInfo.Create;
      FieldInfo.Index     := nIndex;
      FieldInfo.FieldType := dtNotTyped;
    end;
    FFieldNames.AddObject(Value, TFieldNameInfo(FieldInfo));
  except
    FreeAndNil(FieldInfo);
    raise;
  end;
end;

procedure TRPCBuffer.SetFieldNameAndType(const AIndex: Integer; const Value: String; AFieldType: TRPCFieldType);
var
  idx: Integer;
begin
  SetFieldName(AIndex, Value);
  FFieldNames.Find(Value, idx);
  TFieldNameInfo(FFieldNames.Objects[idx]).FieldType := AFieldType;
end;

procedure TRPCBuffer.SetFields(const nIndex: Integer; const newValue: String);
var
  Field: TRPCField;
begin
  if Assigned(FCurrentSortIndex) and FCurrentSortIndex.FieldIndexIsKey( nIndex ) then
    SetCurrentSortIndex('');
  Field := GetRPCFields(nIndex);
  SetFieldAsString(Field, newValue, nIndex);
end;

procedure TRPCBuffer.SetFieldType(const Index: Integer;
  const FieldType: TRPCFieldType);
var
  AField: TRPCField;
begin
  if not HasData then
    RaiseException('No data available')
  else
  begin
    AField := GetCurrentRecord.AddField(FieldType, Index);
    try
      SetFieldFromField(AField, GetRPCFields(Index + 1));
      GetCurrentRecord.DeleteField(Index + 1);
    except
      GetCurrentRecord.DeleteField(Index);
      raise;
    end;
  end;
end;

procedure TRPCBuffer.SetHasData(const Value: Boolean);
begin
  Clear;
  FRecordPointer := -1;
end;

function TRPCBuffer.SetStreamField(const nIndex: Integer; Data: Stream;
  ARewrite: Boolean): Integer;
var
  FField: TRPCField;
begin
  FField := RPCFields[nIndex];
  if ARewrite and Assigned(TRPCStreamField(FField).Value) then // walter - 01/03/2009
    TRPCStreamField(FField).Value.Clear;
  if Assigned(Data) then
    Result := SetFieldFromStream(FField, Data, TStream(Data).Size, nIndex)
  else
    Result := 0;
end;

procedure TRPCBuffer.Sort(const SortName: String; const BuildKey: Array of Word;
  const IntSort, Ascending, ACaseSensitive: Boolean);
begin
  FSortIndex.AddSort(SortName, BuildKey, Ascending, ACaseSensitive);
  SetCurrentSortIndex(SortName);
end;

function TRPCBuffer.StrFindKey(const SortIndex: Integer;
  const KeyFields: Array of String; const AKeySensitive: Boolean): Boolean;
var
  I: Integer;
begin
  if (SortIndex >= 0) and Assigned(FSortIndex.GetSortItem(SortIndex)) then
  begin
    FCurrentSortIndex := FSortIndex.Items[SortIndex];
    Result := FCurrentSortIndex.FindKey(KeyFields, I);
    if Result then
    begin
      FCurrentSortIndex.CurrentItem := I;
      FRecordPointer := FCurrentSortIndex.CurrentRecordPointer;
    end;
  end
  else
    Result := False;
end;

function TRPCBuffer.StrFindKey(const SortName: String;
  const KeyFields: Array of String; const AKeySensitive: Boolean): Boolean;
begin
  Result := StrFindKey(FSortIndex.GetSortIndex(SortName), KeyFields, AKeySensitive);
end;

function TRPCBuffer.StrFindNearestKey(const SortName: String;
  const KeyFields: Array of String; const AKeySensitive: Boolean): Boolean;
begin
  Result := StrFindNearestKey(FSortIndex.GetSortIndex(SortName), KeyFields, AKeySensitive);
end;

function TRPCBuffer.StrFindNearestKey(const SortIndex: Integer;
  const KeyFields: Array of String; const AKeySensitive: Boolean): Boolean;
var
  I: Integer;
begin
  if (SortIndex >= 0) and Assigned(FSortIndex.GetSortItem(SortIndex)) then
  begin
    FCurrentSortIndex := FSortIndex.Items[SortIndex];
    Result := FCurrentSortIndex.FindKey(KeyFields, I);
    FCurrentSortIndex.CurrentItem := I;
    FRecordPointer := FCurrentSortIndex.CurrentRecordPointer;
  end
  else
    Result := False;
end;

procedure TRPCBuffer.WriteBlobField(const NewRec: Boolean; AValue: TBytes);
begin
  WriteTypedFields(NewRec, [AValue], [dtBlob]);
end;

procedure TRPCBuffer.WriteBlockStreamField(const NewRec: Boolean; Data: Stream;
  const BlockSize: Integer);
begin
  WriteStreamField(NewRec or not HasData, Data, False, BlockSize);
end;

procedure TRPCBuffer.WriteBooleanField(const NewRec, AValue: Boolean);
begin
  WriteTypedFields(NewRec, [AValue], [dtBoolean]);
end;

procedure TRPCBuffer.WriteByteField(const NewRec: Boolean; const AValue: Byte);
begin
  WriteTypedFields(NewRec, [AValue], [dtByte]);
end;

procedure TRPCBuffer.WriteCardinalField(const NewRec: Boolean;
  const Value: Cardinal);
begin
{$R-}
  WriteTypedFields(NewRec, [Value], [dtInteger]);
{$R+}
end;

procedure TRPCBuffer.WriteComponent(Instance: TComponent);
var
  MemoryStream: TMemoryStream;
begin
  MemoryStream := TMemoryStream.Create;
  try
    MemoryStream.WriteComponent(Instance);
    WriteStreamField(True, MemoryStream, True);
  finally
    MemoryStream.Free;
  end;
  FHasChanged := True;
end;

procedure TRPCBuffer.WriteCurrencyField(const NewRec: Boolean;
  const AValue: Currency);
begin
  WriteTypedFields(NewRec, [AValue], [dtCurrency]);
end;

procedure TRPCBuffer.WriteDateField(const NewRec: Boolean;
  const AValue: TDateTime);
begin
  WriteTypedFields(NewRec, [AValue], [dtDate]);
end;

procedure TRPCBuffer.WriteDateTimeField(const NewRec: Boolean;
  const AValue: TDateTime);
begin
  WriteTypedFields(NewRec, [AValue], [dtDateTime]);
end;

procedure TRPCBuffer.WriteExtendedField(const NewRec: Boolean;
  const AValue: Double);
begin
  WriteTypedFields(NewRec, [AValue], [dtExtended]);
end;

procedure TRPCBuffer.WriteFieldFromBufferField(const NewRec: Boolean;
  const From: TRPCBuffer; const FieldIndex: Integer);
var
  NewField: TRPCField;
  AList: TList;
begin
  if NewRec then
    AppendRPCRecord;
  NewField := AppendRPCField(From.FieldType[FieldIndex]);

  AList := nil;
  FSortIndex.GenerateNeededUpdates(FieldIndex, GetCurrentRecord, AList);
  try
    SetFieldFromField(NewField, From.GetRPCFields(FieldIndex));
  finally
    FSortIndex.UpdateNeededUpdates(GetCurrentRecord, AList);
  end;
end;

procedure TRPCBuffer.WriteFields(const NewRec: Boolean;
  const Args: Array of Variant);
begin
  WriteTypedFields(NewRec, Args, []);
end;

procedure TRPCBuffer.WriteInt64Field(const NewRec: Boolean;
  const AValue: Int64);
begin
  WriteTypedFields(NewRec, [AValue], [dtLargeint]);
end;

procedure TRPCBuffer.WriteIntegerField(const NewRec: Boolean;
  const AValue: Integer);
begin
  WriteTypedFields(NewRec, [AValue], [dtInteger]);
end;

procedure TRPCBuffer.WriteObjectField(const NewRec: Boolean; AValue: TBytes);
begin
  WriteTypedFields(NewRec, [AValue], [dtObject]);
end;

procedure TRPCBuffer.WritePCharField(const NewRec: Boolean; AValue: TBytes);
begin
  WriteTypedFields(NewRec, [AValue], [dtPChar]);
end;

procedure TRPCBuffer.WriteRecord(const Args: Array of Variant);
begin
  AppendFields(Args);
end;

procedure TRPCBuffer.WriteRPCBuffer(const NewRec: Boolean; Buffer: TRPCBuffer;
  const BufferFormat: TApBufferFormat);
var
  AMemory: TMemoryStream;
begin
  AMemory := TMemoryStream.Create;
  try
    Buffer.SaveToStream(AMemory, BufferFormat);
    WriteStreamField(NewRec, AMemory, True);
  finally
    AMemory.Free;
  end;
end;

procedure TRPCBuffer.WriteStreamField(const NewRec: Boolean; Data: Stream;
  const FromBeggining: Boolean; const ASize: Integer;
  const FreeStream: Boolean);
var
  FField: TRPCField;
  FSize: Integer;
begin
  if NewRec or not HasData then
    AppendRPCRecord;
  FSize := 0;
  if Assigned(Data) then
  begin
    if ASize = -1 then
    begin
      if FromBeggining then
      begin
        FSize := TStream(Data).Size;
        TStream(Data).Position := 0;
      end
      else
        FSize := TStream(Data).Size - TStream(Data).Position
    end
    else
    begin
      FSize := ASize;
      if FromBeggining then
        Data.Position := 0;
    end;
  end;
  FField := AppendRPCField(dtBlob);
  if Assigned(Data) then
    SetFieldFromStream(FField, Data, FSize, FieldCount - 1);
  FHasChanged := True;
  if FreeStream then
    Data.Free;
end;

procedure TRPCBuffer.WriteStringField(const NewRec: Boolean;
  const AValue: String);
begin
  WriteTypedFields(NewRec, [AValue], [dtString]);
end;

procedure TRPCBuffer.WriteTFieldToBuffer(const NewRec: Boolean;
  const Field: TField);
var
  ms: TMemoryStream;
begin
  case Field.DataType of
    ftInteger, ftLargeint, ftSmallint,
      ftAutoInc, ftBytes, ftWord: WriteIntegerField(NewRec, Field.AsInteger);
    ftFloat, ftCurrency: WriteExtendedField(NewRec, Field.AsFloat);
    ftString, ftFixedChar, ftWideString: WriteStringField(NewRec, Field.AsString);
    ftBoolean: WriteBooleanField(NewRec, Field.AsBoolean);
    ftDate, ftDateTime, ftTime: WriteDateTimeField(NewRec, Field.AsDateTime);
    ftBlob, ftGraphic, ftOraBlob,
      ftOraClob:
      begin
        ms := TMemoryStream.Create;
        try
          TBlobField(Field).SaveToStream(ms);
          ms.Position := 0;
          WriteBlockStreamField(NewRec, ms, ms.Size);
        finally
          ms.Free;
        end;
      end;
  else
    WriteStringField(NewRec, Field.AsString);
  end;
end;

procedure TRPCBuffer.WriteTimeField(const NewRec: Boolean;
  const AValue: TDateTime);
begin
  WriteTypedFields(NewRec, [AValue], [dtTime]);
end;

procedure TRPCBuffer.WriteTypedFields(const NewRec: Boolean;
  const Args: Array of Variant; const Types: Array of TRPCFieldType);
var
  AField: TRPCField;
  I: Integer;
  FStream: TMemoryStream;
  V: Variant;
  FBytes: TBytes;
begin
  for I := 0 to High(Args) do
  begin
    if (NewRec or not HasData) and (I = 0) then
      AppendRPCRecord;
    if Length(Types) > 0 then
      AField := AppendRPCField(Types[I])
    else
      AField := nil;
    V := Variant(Args[I]);
    case VarType(V) of
      varNull, varEmpty: SetFieldAsString(AField, '', FieldCount - 1);
      varString, varChar: SetFieldAsString(AField, V, FieldCount - 1);
      varBoolean: SetFieldAsBoolean(AField, V, FieldCount - 1);
      varLongWord, varInt64: SetFieldAsInt64(AField, V, FieldCount - 1);
      varShortInt, varWord, varByte, varInteger: SetFieldAsInteger(AField, V, FieldCount - 1);
      varSingle, varDouble: SetFieldAsFloat(AField, V, FieldCount - 1);
      varDateTime, varDate: SetFieldAsFloat(AField, V, FieldCount - 1);
      varObject:
        begin
          if V is TField then
          begin
            if not Assigned(AField) then
              WriteTFieldToBuffer(False, TField(V))
            else
            begin
              if TField(V).IsBlob then
              begin
                 FStream := TMemoryStream.Create;
                 try
                   TBlobField(V).SaveToStream(FStream);
                   FStream.Position := 0;
                   SetLength(FBytes, FStream.Size);
                   FStream.ReadBuffer(FBytes, FStream.Size);
                   SetFieldAsPChar(AField, FBytes, FStream.Size, FieldCount - 1);
                 finally
                   FStream.Free;
                 end;
              end else if TField(V).DataType in [ftDate, ftTime, ftDateTime] then
                SetFieldAsFloat(AField, TField(V).AsDateTime, FieldCount - 1)
              else
                SetFieldAsString(AField, TField(V).AsString, FieldCount - 1);
            end;
          end
          else
            RaiseException('AppendFields: Unsupported object type ' + TObject(V).ClassName);
        end;
    else
      RaiseException('AppendFields: Unsupported data type ' + IntToStr(VarType(V)));
    end;
  end;
  FHasChanged := True;
end;

procedure TRPCBuffer.WriteTypedRecord(const Args: Array of Variant;
  const Types: Array of TRPCFieldType);
begin
  WriteTypedFields(True, Args, Types);
end;

procedure TRPCBuffer.WriteVariantField(const NewRec: Boolean;
  const AValue: Variant);
begin
  WriteTypedFields(NewRec, [AValue], [dtVariant]);
end;

procedure TRPCBuffer.WriteWordField(const NewRec: Boolean; const AValue: Word);
begin
  WriteTypedFields(NewRec, [AValue], [dtWord]);
end;

{$ELSE}

interface

uses
  Windows, Classes, DB, SysUtils, Math, StrUtils, NativeXml, TypInfo, Clipbrd
  {$IFDEF EUREKALOG} , ExceptionLog {$ENDIF};

const
  MAX_KEYS = 5;
  RecordSeparator                   = #20;
  CR                                = #13;
  LF                                = #10;

type
  TApBufferFormat = (bfOld, bfNew, bfXML);
  ERPCSharedWriteError = class( Exception );

type
  TEventBuffer = record
    Event: Longint;
    ResultBuffer: PChar;
    ResultBufferCount: Longint;
  end;

  TRPCFieldType = (dtNotTyped, dtString, dtBlob, dtInteger, dtDateTime,
    dtBoolean, dtExtended, dtPChar, dtObject, dtCurrency, dtVariant, dtWord,
    dtByte, dtTime, dtDate, dtLargeint);

  TRPCField = packed record
    FieldType: TRPCFieldType;
  end;

  TRPCIntegerField = packed record
    FieldType: TRPCFieldType;
    Value: Integer;
  end;

  TRPCInt64Field = packed record
    FieldType: TRPCFieldType;
    Value: Int64;
  end;

  TRPCFloatField = packed record
    FieldType: TRPCFieldType;
    Value: Double;
  end;

  TRPCByteField = packed record
    FieldType: TRPCFieldType;
    Value: Byte;
  end;

  TRPCBooleanField = packed record
    FieldType: TRPCFieldType;
    Value: Boolean;
  end;

  TRPCStringField = packed record
    FieldType: TRPCFieldType;
    Value: string;
  end;

  TRPCStreamField = packed record
    FieldType: TRPCFieldType;
    Value: TMemoryStream;
  end;

  TRPCWordField = packed record
    FieldType: TRPCFieldType;
    Value: Word;
  end;

  TRPCDateTimeField = packed record
    FieldType: TRPCFieldType;
    Value: TDateTime;
  end;

  TPCharStream = class(TCustomMemoryStream)
  public
    constructor Create(APChar: PChar; const ASize: Integer); reintroduce;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  TFieldNameInfo = class
    Index: integer;
    FieldType: TRPCFieldType;
  end;

  TFieldNames = class(TStringList)
  public
    destructor Destroy; override;
  end;

  //Pointers

  PRPCField = ^TRPCField;
  PRPCIntegerField = ^TRPCIntegerField;
  PRPCInt64Field = ^TRPCInt64Field;
  PRPCFloatField = ^TRPCFloatField;
  PRPCByteField = ^TRPCByteField;
  PRPCBooleanField = ^TRPCBooleanField;
  PRPCStringField = ^TRPCStringField;
  PRPCStreamField = ^TRPCStreamField;
  PRPCWordField = ^TRPCWordField;
  PRPCDateTimeField = ^TRPCDateTimeField;
  TRPCBufferBookMarkStruct = record
    RecordPointer: Integer;
    CurrentIndex: Smallint;
  end;

  TRPCBufferBookMark = ^TRPCBufferBookMarkStruct;

  TRPCBuffer = class;
  TRPCRecord = class;
  TStreamClass = class of TStream;

  TRPCBufferDataList = class(TList)
  public
    procedure Clear; override;
    destructor Destroy; override;
  end;

  TRPCBufferSort = class
  private
    FDataList: TList;
    function GetRecordPointer(Index: Integer): Integer;
    function GetCurrentRecordPointer: Integer;
    procedure QuickSort(L, R: Integer);
    function CompareKeys(const Item1, Item2: TRPCRecord; const ACaseSensitive: Boolean): Integer; overload;
    function CompareKeys(const Item: TRPCRecord; const AKeyValues: array of Variant; const ACaseSensitive: Boolean): Integer; overload;
    function CompareKeys(const Item: TRPCRecord; const AKeyValues: array of string; const ACaseSensitive: Boolean): Integer; overload;
    function CompareKeys(const Item: TRPCRecord; const AKeyValues: array of Integer; const ACaseSensitive: Boolean): Integer; overload;
    function GetRPCRecord(const Index: Integer): TRPCRecord;
    procedure SetRPCRecord(const Index: Integer; const Value: TRPCRecord);
    function GetCount: integer;
  protected
    FCurrentItem: Integer;
    FKeys: array of Word;
    FBuffer: TRPCBuffer;
    FAscending: Boolean;
    FCaseSensitive: Boolean;
    procedure ClearDataList; virtual;
    procedure Assign(ASource: TRPCBufferSort);
  public
    constructor Create(const ABuffer: TRPCBuffer; const AKeys: array of word; const Ascending, ACaseSensitive: Boolean); reintroduce;
    destructor Destroy; override;

    procedure AddRecord(const ARecord: TRPCRecord); virtual;
    procedure DeleteRecord(const ARecord: TRPCRecord); virtual;
    function FieldIndexIsKey(const AFieldIndex: Integer): Boolean;
    procedure InsertIndexed(const ARecord: TRPCRecord);
    //function CheckNeedReindex(const AFieldIndex: Integer; const ARecord: TRPCRecord; var AList: TList);

    function FindKey(const AKeyValues: array of Variant; var Index: Integer): Boolean; overload;
    function FindKey(const AKeyValues: array of string; var Index: Integer): Boolean; overload;
    function FindKey(const AKeyValues: array of Integer; var Index: Integer): Boolean; overload;
    function FindKey(const ARecord: TRPCRecord; const Exact: Boolean; var Index: Integer): Boolean; overload;

    procedure Sort; reintroduce; virtual;

    property CurrentItem: Integer read FCurrentItem write FCurrentItem;
    property RecordPointer[Index: Integer]: Integer read GetRecordPointer;
    property CurrentRecordPointer: Integer read GetCurrentRecordPointer;
    property RPCRecord[const Index: Integer]: TRPCRecord read GetRPCRecord write SetRPCRecord;
    property Count: integer read GetCount;
  end;

  TRPCBufferSortList = class(TObject)
  private
    FSortList: TStringList;
    FBuffer: TRPCBuffer;
  protected
    function GetSortItem(AIndex: Integer): TRPCBufferSort;
    function GetCount: Integer;
  public
    constructor Create(const ABuffer: TRPCBuffer);
    destructor Destroy; override;
    function GetSortIndex(const ASortName: string): Integer;
    function GetSortByName(const ASortName: string): TRPCBufferSort;
    function AddSort(const ASortName: string; const AKeys: array of Word; const Ascending, ACaseSensitive: Boolean): Integer;
    function DeleteSort(ASortIndex: Integer): Boolean;
    function IndexOfSort(const ASort: TRPCBufferSort): SmallInt;
    procedure DeleteRecord(const ARecord: TRPCRecord);
    procedure GenerateNeededUpdates(const AFieldIndex: Integer; const ARecord: TRPCRecord; var AList: TList);
    procedure UpdateNeededUpdates(const ARecord: TRPCRecord; var AList: TList);
    procedure ClearList;
    procedure Assign(ARPCBufferSortList: TRPCBufferSortList);
    property Items[AIndex: Integer]: TRPCBufferSort read GetSortItem;
    property Count: Integer read GetCount;
  end;

  TRPCRecord = class(TList)
  private
    FBuffer: TRPCBuffer;
    function GetRPCFields(Index: Integer): PRPCField;
    procedure SetRPCFields(Index: Integer; const Value: PRPCField);

    class function GetFieldAsString(const Field: PRPCField): string;
    class function GetFieldAsInteger(const Field: PRPCField): Integer;
    class function GetFieldAsInt64(const Field: PRPCField): Int64;
    class function GetFieldAsFloat(const Field: PRPCField): Double;
    class function GetFieldAsBoolean(const Field: PRPCField): Boolean;
    class function GetFieldAsByte(const Field: PRPCField): Byte;
    class function GetFieldAsWord(const Field: PRPCField): Word;
  public
    function AddField(const AFieldType: TRPCFieldType; const FieldIndex: Integer = -1): PRPCField;
    procedure DeleteField(Index: Integer);
    procedure AdjustFieldTypeAndSize(var Field: PRPCField; const AFieldType: TRPCFieldType; const FieldIndex, FieldSize: Integer);
    procedure SaveToStream(Stream: TStream; const BufferFormat: TApBufferFormat = bfNew);
    procedure SaveToXML(XML: TNativeXml);
    procedure LoadFromStream(Stream: TStream; const BufferFormat: TApBufferFormat = bfNew);
    procedure LoadFromXMLDOMNode(const ARecordNode: TXMLNode);
    procedure SaveFieldToStream(Stream: TStream; const Index: Integer; const BufferFormat: TApBufferFormat = bfNew);
    procedure SaveFieldToXML(ARecordNode: TXmlNode; const Index: Integer);
    procedure LoadFieldFromStream(Stream: TStream; const BufferFormat: TApBufferFormat = bfNew);
    procedure LoadFieldFromXMLNode(const AFieldNode: TXMLNode);

    procedure Clear; override;
    destructor Destroy; override;
    property RPCFields[Index: Integer]: PRPCField read GetRPCFields write SetRPCFields; default;
  end;

  ERPCBufferConversionError = class(Exception)
  public
    constructor Create(const Index: Integer; const FieldType: TRPCFieldType); reintroduce;
  end;

  {
    É o pacote básico que contém as partes da mensagem transmitida.
    É uma lista com métodos específicos para manipular as partes respeitando os tipos de cada elemento.
  }
  TRPCBuffer = class(TObject)
  private
    FDataList: TRPCBufferDataList;
    function GetRecords(Index: Integer): TRPCRecord;
    function GetRPCFields(Index: Integer): PRPCField;
    procedure IntSetFieldAsByte(const nIndex: Integer; const Value: Byte);
    procedure IntSetFieldAsWord(const nIndex: Integer; const Value: Word);
    procedure IntSetFieldAsString(const nIndex: Integer; const Value: string);
    procedure IntSetFieldAsInteger(const nIndex: Integer; const Value: Integer);
    procedure IntSetFieldAsInt64(const nIndex: Integer; const Value: Int64);
    procedure IntSetFieldAsFloat(const nIndex: Integer; const Value: Double);
    procedure IntSetFieldAsBoolean(const nIndex: Integer; const Value: Boolean);
    procedure IntSetFieldAsDateTime(const nIndex: Integer; const Value: TDateTime);
    function IntGetFieldAsBoolean(const nIndex: Integer): Boolean;
    function IntGetFieldAsByte(const nIndex: Integer): Byte;
    function IntGetFieldAsFloat(const nIndex: Integer): Double;
    function IntGetFieldAsInteger(const nIndex: Integer): Integer;
    function IntGetFieldAsInt64(const nIndex: Integer): Int64;
    function IntGetFieldAsCardinal(const nIndex: Integer): Cardinal;
    function IntGetFieldAsString(const nIndex: Integer): string;
    function IntGetFieldAsWord(const nIndex: Integer): Word;
    function IntGetFieldAsDateTime(const nIndex: Integer): TDateTime;
    function FindNameIndex(const Index: Integer): Integer;
    procedure FreeFieldNames;
    function IntGetFieldIndexByName(const Name: string; const ARaiseException: boolean = true): Integer;
    function GetFieldIndexByName(const Name: string): Integer;
    function GetFieldName(const nIndex: Integer): string;
    procedure SetFieldName(const nIndex: Integer; const Value: string);

    function GetFieldByNameAsBoolean(const Name: string): Boolean;
    function GetFieldByNameAsByte(const Name: string): Byte;
    function GetFieldByNameAsDateTime(const Name: string): TDateTime;
    function GetFieldByNameAsFloat(const Name: string): Double;
    function GetFieldByNameAsInteger(const Name: string): Integer;
    function GetFieldByNameAsInt64(const Name: string): Int64;
    function GetFieldByNameAsCardinal(const Name: string): Cardinal;
    function GetFieldByNameAsString(const Name: string): string;
    function GetFieldByNameAsWord(const Name: string): Word;
    procedure SetFieldByNameAsBoolean(const Name: string; const Value: Boolean);
    procedure SetFieldByNameAsByte(const Name: string; const Value: Byte);
    procedure SetFieldByNameAsDateTime(const Name: string; const Value: TDateTime);
    procedure SetFieldByNameAsFloat(const Name: string; const Value: Double);
    procedure SetFieldByNameAsInteger(const Name: string; const Value: Integer);
    procedure SetFieldByNameAsInt64(const Name: string; const Value: Int64);
    procedure SetFieldByNameAsCardinal(const Name: string; const Value: Cardinal);
    procedure SetFieldByNameAsString(const Name, Value: string);
    procedure SetFieldByNameAsWord(const Name: string; const Value: Word);
    function GetFieldByNameFieldType(const Name: string): TRPCFieldType;
    procedure SetFieldByNameFieldType(const Name: string; const Value: TRPCFieldType);
    procedure IntSetFieldAsCardinal(const nIndex: Integer; const Value: Cardinal);
  private
    FRecordPointer: Integer;
    FHitEof: Boolean;
    FHitBof: Boolean;
    FSortIndex: TRPCBufferSortList;
    FCurrentSortIndex: TRPCBufferSort;
    FHasChanged: Boolean;
    FFieldNames: TFieldNames;
    function getIsSorted: Boolean;
    function GetHasData: Boolean;
    procedure SetHasData(const Value: Boolean);
    function GetCurrentRecord: TRPCRecord;

    procedure SetFieldAsString(var Field: PRPCField; const Value: string; const FieldIndex: Integer);
    procedure SetFieldAsInteger(var Field: PRPCField; Value: Integer; const FieldIndex: Integer);
    procedure SetFieldAsInt64(var Field: PRPCField; Value: Int64; const FieldIndex: Integer);
    procedure SetFieldAsFloat(var Field: PRPCField; Value: Double; const FieldIndex: Integer);
    procedure SetFieldAsBoolean(var Field: PRPCField; Value: Boolean; const FieldIndex: Integer);
    procedure SetFieldAsPChar(var Field: PRPCField; Value: PChar; const Size: Integer; const FieldIndex: Integer);
    procedure SetFieldFromField(var Field: PRPCField; FieldFrom: PRPCField);
    function SetFieldFromStream(var Field: PRPCField; const AStream: TStream; const Size: Cardinal; const FieldIndex: Integer): Cardinal;
    procedure AdjustFieldTypeAndSize(var Field: PRPCField; const AFieldType: TRPCFieldType;
      FieldSize: Cardinal; const FieldIndex: Integer);

    function AppendRPCRecord: TRPCRecord; virtual;

    function AppendRPCField(const ADataType: TRPCFieldType): PRPCField; virtual;
    procedure RemoveRPCRecord(const Index: Integer); virtual;
    function getCount: integer;

    property Records[Index: Integer]: TRPCRecord read GetRecords;
    property RPCFields[Index: Integer]: PRPCField read GetRPCFields;
    procedure GetBufferAsStrings(AList: TStrings); // terazawa - 15/07/2014
  protected
    procedure RaiseException(const Where: string);
    function NewGetFields(const nIndex: Integer): string;
    function GetFields(const nIndex: Integer): string;
    function GetFieldType(const nIndex: Integer): TRPCFieldType;
    procedure SetFieldType(const Index: Integer; const FieldType: TRPCFieldType);
    function GetFieldCount: Integer;
    function GetRecordCount: Integer;
    function GetRecNo: Integer;
    function GetEof: Boolean;
    function GetBof: Boolean;
    //== Protected Write methods
    procedure SetFields(const nIndex: Integer; const newValue: string);
    procedure AppendStreamField(Data: TStream);
    //==
    function GetFieldSize(const nIndex: Integer): Integer;
    procedure ClearDataList; virtual;
    procedure FreeDataObjects; virtual;

  public
    EventBuffer: TEventBuffer;

    function ConstToFormatedString(args: array of const): string;
    procedure SaveToTextFile(const AFileName: string); virtual;
    procedure SaveToClip; virtual; // terazawa - 15/07/2014
    function AsString: string;

    constructor Create; virtual;
    destructor Destroy; override;

    procedure First;
    procedure Next;
    procedure Prior;
    procedure Last;
    function GetRPCBufferBookMark: TRPCBufferBookMark;
    procedure GotoRPCBufferBookMark(const ABookMark: TRPCBufferBookMark);
    procedure FreeRPCBufferBookMark(var ABookMark: TRPCBufferBookMark);

    function FindFieldIndexByName(const Name: String): Integer;

    //== Write methods
    procedure CreateRecordList;

    procedure Append;
    procedure Rewrite(const AClearFieldNames: boolean = false);
    procedure Clear;

    procedure WriteRecord(const Args: array of const);
    procedure WriteTypedRecord(const Args: array of const; const Types: array of TRPCFieldType);
    procedure WriteFields(const NewRec: Boolean; const Args: array of const);
    procedure WriteTFieldToBuffer(const NewRec: Boolean; const Field: TField); virtual;
    procedure WriteTypedFields(const NewRec: Boolean; const Args: array of const; const Types: array of TRPCFieldType); virtual;
    procedure WriteStreamField(const NewRec: Boolean; Data: TStream; const FromBeggining: Boolean; const ASize: Integer = -1; const FreeStream: Boolean = false); virtual;
    procedure WriteBlockStreamField(const NewRec: Boolean; Data: TStream; const BlockSize: Integer);
    procedure WriteFieldFromBufferField(const NewRec: Boolean; const From: TRPCBuffer; const FieldIndex: Integer);

    procedure WriteStringField(const NewRec: Boolean; const AValue: string);
    procedure WriteBlobField(const NewRec: Boolean; AValue: PChar);
    procedure WriteIntegerField(const NewRec: Boolean; const AValue: Integer);
    procedure WriteInt64Field(const NewRec: Boolean; const AValue: Int64);
    procedure WriteCardinalField(const NewRec: boolean; const Value: Cardinal);
    procedure WriteDateTimeField(const NewRec: Boolean; const AValue: TDateTime);
    procedure WriteBooleanField(const NewRec: Boolean; const AValue: Boolean);
    procedure WriteExtendedField(const NewRec: Boolean; const AValue: Extended);
    procedure WritePCharField(const NewRec: Boolean; AValue: PChar);
    procedure WriteObjectField(const NewRec: Boolean; AValue: PChar);
    procedure WriteCurrencyField(const NewRec: Boolean; const AValue: Currency);
    procedure WriteVariantField(const NewRec: Boolean; const AValue: Variant);

    procedure WriteByteField(const NewRec: Boolean; const AValue: Byte);
    procedure WriteWordField(const NewRec: Boolean; const AValue: Word);
    procedure WriteTimeField(const NewRec: Boolean; const AValue: TDateTime);
    procedure WriteDateField(const NewRec: Boolean; const AValue: TDateTime);
    procedure WriteComponent(Instance: TComponent);
    procedure WriteRPCBuffer(const NewRec: Boolean; Buffer: TRPCBuffer; const BufferFormat: TApBufferFormat = bfNew);

    procedure AppendFields(const Args: array of const);
    procedure AppendTypedFields(const Args: array of const; const Types: array of TRPCFieldType);
    procedure AppendRecord(const From: TRPCBuffer);

    procedure Sort(const SortName: string; const BuildKey: array of Word; const IntSort: Boolean = False; const Ascending: Boolean = True; const ACaseSensitive: Boolean = False); virtual;

    function RemoveSort(const ASortName: string): Boolean; overload;
    function RemoveSort(const ASortIndex: Integer): Boolean; overload;

    procedure AssignBuffer(From: TRPCBuffer);
    procedure Delete;
    //=== End Write Methods

    procedure ReadComponent(Instance: TComponent);
    procedure ReadRPCBuffer(const nIndex: Integer; Buffer: TRPCBuffer; const BufferFormat: TApBufferFormat = bfNew);
    procedure Seek(ARecNo: Integer);
    procedure SetCurrentSortIndex(const SortName: string);
    function GetCurrentSortIndexName: String;
    procedure MirrorSort(const SortName: string; const BuildKey: array of Word);

    function FindKey(const SortIndex: Integer; const KeyFields: array of Variant; const AKeySensitive: Boolean): Boolean; overload;
    function FindKey(const SortName: string; const KeyFields: array of Variant; const AKeySensitive: Boolean): Boolean; overload;

    function FindNearestKey(const SortIndex: Integer; const KeyFields: array of Variant; const AKeySensitive: Boolean): Boolean; overload;
    function FindNearestKey(const SortName: string; const KeyFields: array of Variant; const AKeySensitive: Boolean): Boolean; overload;

    function StrFindKey(const SortIndex: Integer; const KeyFields: array of string; const AKeySensitive: Boolean): Boolean; overload;
    function StrFindKey(const SortName: string; const KeyFields: array of string; const AKeySensitive: Boolean): Boolean; overload;

    function StrFindNearestKey(const SortIndex: Integer; const KeyFields: array of string; const AKeySensitive: Boolean): Boolean; overload;
    function StrFindNearestKey(const SortName: string; const KeyFields: array of string; const AKeySensitive: Boolean): Boolean; overload;

    function IntFindKey(const SortIndex: Integer; const KeyFields: array of Integer; const AKeySensitive: Boolean; const Inverted: Boolean = False): Boolean; overload;
    function IntFindKey(const SortName: string; const KeyFields: array of Integer; const AKeySensitive: Boolean): Boolean; overload;

    function IntFindNearestKey(const SortIndex: Integer; const KeyFields: array of Integer; const AKeySensitive: Boolean): Boolean; overload;
    function IntFindNearestKey(const SortName: string; const KeyFields: array of Integer; const AKeySensitive: Boolean): Boolean; overload;

    procedure ImportFromEventBuffer(const ExtEventBuffer: TEventBuffer); overload;
    procedure ImportFromEventBuffer; overload;
    procedure ExportToEventBuffer(const Event: Integer);

    procedure SetFieldNameAndType(const AIndex: Integer; const Value: string; AFieldType: TRPCFieldType);
    procedure AddFieldNameAndType(const Value: string; AFieldType: TRPCFieldType);

    function IsEqualTo(const ABuffer: TRPCBuffer): Boolean;

    function GetStringField(const nIndex: Integer): string;
    function GetCharField(const nIndex: Integer): Char;
    function GetBlobField(const nIndex: Integer): PChar;
    function GetIntegerField(const nIndex: Integer): Integer;
    function GetInt64Field(const nIndex: Integer): Int64;
    function GetDateTimeField(const nIndex: Integer): TDateTime;
    function GetBooleanField(const nIndex: Integer): Boolean;
    function GetExtendedField(const nIndex: Integer): Double;
    function GetPCharField(const nIndex: Integer): PChar;
    function GetObjectField(const nIndex: Integer): PChar;
    function GetCurrencyField(const nIndex: Integer): Currency;
    function GetVariantField(const nIndex: Integer): Variant;
    function GetCardinalField(const nIndex: Integer): Cardinal;

    function GetByteField(const nIndex: Integer): Byte;
    function GetWordField(const nIndex: Integer): Word;
    function GetTimeField(const nIndex: Integer): TDateTime;
    function GetDateField(const nIndex: Integer): TDateTime;
    function GetStreamField(const FieldNb: Integer; Data: TStream; const Rewrite: Boolean = True): Integer; overload;
    function GetStreamField(const FieldIndex: Integer): TStream; overload;
    function SetStreamField(const nIndex: Integer; Data: TStream; ARewrite: boolean = FALSE): Integer;
    function GetBlockedStreamField(const FieldNb: Integer; Data: TStream): Integer;

    function JumpTo(const RecNo: Integer): Boolean;
    function IndexJumpTo(const RecNumber: Integer): Boolean;
    procedure SaveToStream(Stream: TStream; const BufferFormat: TApBufferFormat = bfNew);
    procedure LoadFromXMLStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream; Len: Longint; const BufferFormat: TApBufferFormat = bfNew); overload;
    procedure LoadFromStream(Stream: TStream; const BufferFormat: TApBufferFormat = bfNew); overload;
    procedure SaveToXMLStream(Stream: TStream);
    procedure LoadFromDataSet(const Source: TDataSet; const InitialField: Integer = 0; const FinalField: Integer = -1;
      const ALoadFieldNames: boolean = true);
    procedure LoadFromRecordDataSet(const Source: TDataSet; const InitialField: Integer = 0; const FinalField: Integer = -1);
    procedure LoadFromPChar(const Value: PChar; const Size: Integer; const BufferFormat: TApBufferFormat = bfNew);
    procedure SaveToFile(const FieldNo: Integer; const FileName: string);
    procedure LoadFromFile(const NewRec: Boolean; const FileName: string);
    procedure LoadFromDelimitedFile(const FileName: string; const Delimiter: string);
    procedure LoadFromList(const NewRec: Boolean; const List: TStrings);
    function GetSortIndex(const ASortName: String): Integer; // walter - 03/07/2012
    procedure InvertBuffer;

    property Count: integer read getCount;
    property Fields[const nIndex: Integer]: string read GetFields write SetFields;
    property NewFields[const nIndex: Integer]: string read NewGetFields write SetFields;

    property FieldSize[const nIndex: Integer]: Integer read GetFieldSize;
    property FieldType[const nIndex: Integer]: TRPCFieldType read GetFieldType write SetFieldType;
    property FieldCount: Integer read GetFieldCount;
    property RecordCount: Integer read GetRecordCount;
    property Recno: Integer read GetRecNo;
    property Eof: Boolean read GetEof;
    property Bof: Boolean read GetBof;
    property HasData: Boolean read GetHasData write SetHasData;
    property HasChanged: Boolean read FHasChanged;
    property IsSorted: Boolean read getIsSorted;

    //New Methods
    property FieldAsInteger[const nIndex: Integer]: Integer read IntGetFieldAsInteger write IntSetFieldAsInteger;
    property FieldAsInt64[const nIndex: Integer]: Int64 read IntGetFieldAsInt64 write IntSetFieldAsInt64;
    property FieldAsCardinal[const nIndex: Integer]: Cardinal read IntGetFieldAsCardinal write IntSetFieldAsCardinal;
    property FieldAsWord[const nIndex: Integer]: Word read IntGetFieldAsWord write IntSetFieldAsWord;
    property FieldAsByte[const nIndex: Integer]: Byte read IntGetFieldAsByte write IntSetFieldAsByte;
    property FieldAsFloat[const nIndex: Integer]: Double read IntGetFieldAsFloat write IntSetFieldAsFloat;
    property FieldAsDateTime[const nIndex: Integer]: TDateTime read IntGetFieldAsDateTime write IntSetFieldAsDateTime;
    property FieldAsString[const nIndex: Integer]: string read IntGetFieldAsString write IntSetFieldAsString;
    property FieldAsBoolean[const nIndex: Integer]: Boolean read IntGetFieldAsBoolean write IntSetFieldAsBoolean;

    property FieldName[const AIndex: Integer]: string read GetFieldName write SetFieldName;
    property FieldByNameAsInteger[const Name: string]: Integer read GetFieldByNameAsInteger write SetFieldByNameAsInteger;
    property FieldByNameAsInt64[const Name: string]: Int64 read GetFieldByNameAsInt64 write SetFieldByNameAsInt64;
    property FieldByNameAsCardinal[const Name: string]: Cardinal read GetFieldByNameAsCardinal write SetFieldByNameAsCardinal;
    property FieldByNameAsWord[const Name: string]: Word read GetFieldByNameAsWord write SetFieldByNameAsWord;
    property FieldByNameAsByte[const Name: string]: Byte read GetFieldByNameAsByte write SetFieldByNameAsByte;
    property FieldByNameAsFloat[const Name: string]: Double read GetFieldByNameAsFloat write SetFieldByNameAsFloat;
    property FieldByNameAsDateTime[const Name: string]: TDateTime read GetFieldByNameAsDateTime write SetFieldByNameAsDateTime;
    property FieldByNameAsString[const Name: string]: string read GetFieldByNameAsString write SetFieldByNameAsString;
    property FieldByNameAsBoolean[const Name: string]: Boolean read GetFieldByNameAsBoolean write SetFieldByNameAsBoolean;
    property FieldByNameFieldtype[const Name: string] : TRPCFieldType read GetFieldByNameFieldType write SetFieldByNameFieldType;
    property FieldIndexByName[const Name: String] : Integer read GetFieldIndexByName;
    property CurrentSortIndex: TRPCBufferSort read FCurrentSortIndex;
  end;

  // Compartilha a estrutura de Dados e Indices a partir de outro Buffer
  TSharedBuffer = class(TRPCBuffer)
  private
    procedure RaiseWriteError;
  protected
    procedure FreeDataObjects; override;
  public
    constructor Create(ASourceBuffer: TRPCBuffer); reintroduce;
    procedure WriteTypedFields(const NewRec: Boolean; const Args: array of const; const Types: array of TRPCFieldType); override;
    procedure WriteTFieldToBuffer(const NewRec: Boolean; const Field: TField); override;
    procedure WriteStreamField(const NewRec: Boolean; Data: TStream; const FromBeggining: Boolean; const ASize: Integer = -1; const FreeStream: Boolean = false); override;
    function AppendRPCField(const ADataType: TRPCFieldType): PRPCField; override;
    procedure RemoveRPCRecord(const Index: Integer); override;
    function AppendRPCRecord: TRPCRecord; override;

    procedure Sort(const SortName: string; const BuildKey: array of Word; const IntSort: Boolean = False; const Ascending: Boolean = True; const ACaseSensitive: Boolean = False); override;

    function RemoveSort(const ASortName: string): Boolean; overload;
    function RemoveSort(const ASortIndex: Integer): Boolean; overload;

    procedure AssignBuffer(From: TRPCBuffer);
    procedure Delete;
  end;

  TSharedBufferSort = class(TRPCBufferSort)
  protected
    procedure ClearDataList; override;  
  public
    constructor Create(const ASource: TRPCBufferSort); reintroduce;

    procedure AddRecord(const ARecord: TRPCRecord); override;
    procedure DeleteRecord(const ARecord: TRPCRecord); override;
    procedure Sort; override;
    destructor Destroy; override;
  end;

  TInsertDataBuffer = class(TRPCBuffer)
  strict private
    FZeroDateAsNull: Boolean;
  public
    constructor Create; override;

    property ZeroDateAsNull: Boolean read FZeroDateAsNull write FZeroDateAsNull;
  end;


const
  XMLFieldTypes: array[TRPCFieldType] of String = (
    'ap:unknowStr', 'ap:string', 'ap:hexBinary', 'ap:integer', 'ap:dateTime',
    'ap:boolean', 'ap:float', 'ap:pcharStr', 'ap:objStr', 'ap:Currency',
    'ap:variantStr', 'ap:unassignedShort', 'ap:unassignedByte', 'ap:time',
    'ap:date', 'ap:largeint');

  RPCTextFieldTypes = [dtString, dtInteger, dtDateTime, dtBoolean, dtExtended,
    dtPChar, dtCurrency, dtWord, dtByte, dtTime, dtDate, dtLargeint];

  RPCNumericFieldTypes = [dtInteger, dtExtended, dtCurrency, dtWord, dtByte, dtLargeint];

  RPCTemporalFieldTypes = [dtDateTime, dtTime, dtDate];

  FFieldSep = #18;
  FRecSep = #20;
  FEscSep = #127;
  FNullSep = #0;
  FBlobInit = #84;

function DateTimeToRPCFormat(const DateTime: TDateTime): string;
function DefaultBuildKey(RPCBuffer: TRPCBuffer): string;
function FloatToStrEx(const Value: Extended): string;
function PadFormat(const Value: string; const Len: Word): string;
function StrToFloatDef(const Value: string; const Def: Extended): Extended;
function StrApDataFieldType2RPCFieldType(const AType: string): TRPCFieldType;
function StringPadLeft(const S: AnsiString; Len: Integer; C: AnsiChar): AnsiString;
function StringPadRight(const S: AnsiString; Len: Integer; C: AnsiChar): AnsiString;
procedure StrRealloc(var S: PChar; Size: Cardinal);

implementation

procedure StrRealloc(var S: PChar; Size: Cardinal);
begin
  if (StrBufSize(S) <> Size) then
  begin
    Dec(PChar(S), SizeOf(Cardinal));
    try
      ReallocMem(S, Size + SizeOf(Cardinal));
    finally
      Inc(PChar(S), SizeOf(Cardinal));
    end;
  end;
end;

function StringPadLeft(const S: AnsiString; Len: Integer; C: AnsiChar): AnsiString;
var
  L: Integer;
begin
  L := Length(S);
  if L < Len then
    Result := StringOfChar(C, Len - L) + S
  else
    Result := S;
end;

function StringPadRight(const S: AnsiString; Len: Integer; C: AnsiChar): AnsiString;
var
  L: Integer;
begin
  L := Length(S);
  if L < Len then
    Result := S + StringOfChar(C, Len - L)
  else
    Result := S;
end;

function StrApDataFieldType2RPCFieldType(const AType: string): TRPCFieldType;
var
  ATipo: Integer;
begin
  ATipo := StrToIntDef(AType, 0);
  case ATipo of
    1: Result := BufferDef.dtInteger;
    2: Result := BufferDef.dtInteger;
    3: Result := BufferDef.dtInteger;
    4: Result := BufferDef.dtByte;
    5: Result := BufferDef.dtByte;
    6: Result := BufferDef.dtWord;
    7: Result := BufferDef.dtWord;
    8: Result := BufferDef.dtExtended;
    9: Result := BufferDef.dtString;
    10: Result := BufferDef.dtDateTime;
    11: Result := BufferDef.dtDateTime;
    12: Result := BufferDef.dtBoolean;
    13: Result := BufferDef.dtBlob;
    14: Result := BufferDef.dtBlob;
    15: Result := BufferDef.dtBlob;
    16: Result := BufferDef.dtDateTime;
    17: Result := BufferDef.dtBlob;
    18: Result := BufferDef.dtBlob;
  else
    Result := BufferDef.dtNotTyped;
  end;
end;

function PadFormat(const Value: string; const Len: Word): string;
var
  tmpStr: string;
begin
  tmpStr := StringOfChar(' ', Len) + Value;
  Result := Copy(tmpStr, Length(tmpStr) - Len + 1, Len);
end;

function StrToFloatDef(const Value: string; const Def: Extended): Extended;
begin
  Result := 0;
  if not TextToFloat(PChar(Value), Result, fvExtended) then
    Result := Def;
end;

function FloatToStrEx(const Value: Extended): string;
begin
  Result := FloatToStrF(Value, ffGeneral, 18, 0);
end;

function DefaultBuildKey(RPCBuffer: TRPCBuffer): string;
begin
  Result := RPCBuffer.NewFields[0];
end;

function DateTimeToRPCFormat(const DateTime: TDateTime): string;
var
  TimeStamp: TTimeStamp;
begin
  TimeStamp := DateTimeToTimeStamp(DateTime);
  Result := IntToStr(TimeStamp.Time) + ';' + IntToStr(TimeStamp.Date);
end;

{TRPCBuffer}

constructor TRPCBuffer.Create;
begin
  inherited Create;
  FDataList  := TRPCBufferDataList.Create;
  FSortIndex := TRPCBufferSortList.create(Self);
  FFieldNames := nil;
  FCurrentSortIndex := nil;
  FRecordPointer := -1;
end;

procedure TRPCBuffer.CreateRecordList;
begin
  //
end;

destructor TRPCBuffer.Destroy;
begin
  FreeDataObjects;
  if Assigned(EventBuffer.ResultBuffer) then
  begin
    FreeMem(EventBuffer.ResultBuffer);
    EventBuffer.ResultBuffer := nil;
    EventBuffer.ResultBufferCount := 0;
  end;
  inherited Destroy;
end;

procedure TRPCBuffer.RaiseException(const Where: string);
begin
  raise Exception.Create(Where);
end;

function TRPCBuffer.GetFields(const nIndex: Integer): string;
begin
  Result := TRPCRecord.GetFieldAsString(GetRPCFields(nIndex));
end;

function TRPCBuffer.NewGetFields(const nIndex: Integer): string;
begin
  Result := GetFields(nIndex);
end;

function TRPCBuffer.GetFieldSize(const nIndex: Integer): Integer;
var
  Field: PRPCField;
begin
  Field := GetRPCFields(nIndex);
  with Field^ do
  begin
    case FieldType of
      dtBlob: begin
        if Assigned(PRPCStreamField(Field)^.Value) then
          Result := PRPCStreamField(Field)^.Value.Size
        else
          Result := 0;
      end;
      dtInteger: Result := SizeOf(Integer);
      dtLargeint: Result := SizeOf(Int64);
      dtDateTime,
        dtExtended,
        dtCurrency,
        dtTime,
        dtDate: Result := SizeOf(Double);
      dtBoolean: Result := SizeOf(Boolean);
      dtByte: Result := SizeOf(Byte);
      dtWord: Result := SizeOf(Word);
    else
      Result := Length(PRPCStringField(Field)^.Value);
    end;
  end;
end;

function TRPCBuffer.GetStreamField(const FieldNb: Integer; Data: TStream; const Rewrite: Boolean = True): Integer;
begin
  Result := -1;
  try
    if GetRPCFields(FieldNb)^.FieldType <> dtBlob then
      raise Exception.CreateFmt('Field %d is not a Blob field. Current type is %s', [FieldNb, GetEnumName(TypeInfo(TRPCFieldType), Ord(GetRPCFields(FieldNb)^.FieldType))]);

    with PRPCStreamField(GetRPCFields(FieldNb))^ do
    begin
      if Rewrite then
        Data.Size := 0;
      if Assigned(Value) then
      begin
        Value.Position := 0;
        Result := Data.CopyFrom(Value, Value.Size);
      end
      else
        Result := 0;
    end;
  except
    on E: Exception do
    begin
      {$IFDEF EUREKALOG}
      StandardEurekaNotify(E, ExceptAddr);
      {$ENDIF}
    end;
  end;
end;

function TRPCBuffer.GetBlockedStreamField(const FieldNb: Integer; Data: TStream): Integer;
begin
  Result := GetStreamField(FieldNb, Data, False);
end;

procedure TRPCBuffer.SetFields(const nIndex: Integer; const newValue: string);
var
  Field: PRPCField;
begin
  if Assigned(FCurrentSortIndex) and FCurrentSortIndex.FieldIndexIsKey( nIndex ) then
    SetCurrentSortIndex('');
  Field := GetRPCFields(nIndex);
  SetFieldAsString(Field, newValue, nIndex);
end;

function TRPCBuffer.GetFieldCount: Integer;
begin
  if not HasData then
    Result := 0
  else
    Result := GetCurrentRecord.Count;
end;

function TRPCBuffer.GetRecordCount: Integer;
begin
  Result := FDataList.Count;
end;

function TRPCBuffer.GetBof: Boolean;
begin
  if not Assigned(FCurrentSortIndex) then
    Result := (FHitBof)
  else
    Result := (FCurrentSortIndex.CurrentItem = -1)
end;

function TRPCBuffer.GetEof: Boolean;
begin
  if not Assigned(FCurrentSortIndex) then
    Result := (FHitEof)
  else
    Result := (not HasData) or ((FCurrentSortIndex.Count) = FCurrentSortIndex.CurrentItem);
end;

procedure TRPCBuffer.First;
begin
  if not Assigned(FCurrentSortIndex) then
  begin
    FRecordPointer := 0;
    FHitBof := True;
    FHitEof := RecordCount = 0;
  end
  else
  begin
    Seek(FCurrentSortIndex.RecordPointer[0]);
    FCurrentSortIndex.CurrentItem := -1;
  end;
end;

procedure TRPCBuffer.Next;
begin
  if HasData then
  begin
    if not Assigned(FCurrentSortIndex) then
    begin
      FHitBof := False;
      Inc(FRecordPointer);
      if FRecordPointer >= RecordCount then
      begin
        FRecordPointer := RecordCount - 1;
        FHitEof := True;
      end;
    end
    else
    begin
      if FCurrentSortIndex.CurrentItem < 0 then
      FCurrentSortIndex.CurrentItem := FCurrentSortIndex.CurrentItem + 2 //Anula o BOF
      else FCurrentSortIndex.CurrentItem := FCurrentSortIndex.CurrentItem + 1;
      if GetEof then
        FCurrentSortIndex.CurrentItem := FCurrentSortIndex.Count
      else
        Seek(FCurrentSortIndex.CurrentRecordPointer);
    end;
  end else
  begin
    // caso o buffer não tenha dados, então ele deve retornar verdadeiro no começo (BOF) e no fim (EOF) da lista...
    FHitEof := True;
    FHitBof := True;
  end;
end;

procedure TRPCBuffer.Prior;
begin
  if HasData then
  begin
    if not Assigned(FCurrentSortIndex) then
    begin
      FHitEof := False;
      Dec(FRecordPointer);
      if FRecordPointer < 0 then
      begin
        FRecordPointer := 0;
        FHitBof := True;
      end;
    end
    else
    begin
      FCurrentSortIndex.CurrentItem := FCurrentSortIndex.CurrentItem - 1;
      if GetBof then
        FCurrentSortIndex.CurrentItem := -1
      else
        Seek(FCurrentSortIndex.CurrentRecordPointer);
    end;
  end;
end;

procedure TRPCBuffer.Last;
begin
  if HasData then
  begin
    if not Assigned(FCurrentSortIndex) then
    begin
      FRecordPointer := RecordCount - 1;
      FHitEof := True;
      FHitBof := False;
    end
    else
    begin
      Seek(FCurrentSortIndex.RecordPointer[FCurrentSortIndex.Count - 1]);
      FCurrentSortIndex.CurrentItem := FCurrentSortIndex.Count - 1;
    end;
  end;
end;

procedure TRPCBuffer.Rewrite(const AClearFieldNames: boolean);
begin
  Clear;
  if AClearFieldNames then
    FreeFieldNames;
  First;
end;

procedure TRPCBuffer.WriteRecord(const Args: array of const);
begin
  AppendFields(Args);
end;

procedure TRPCBuffer.WriteFields(const NewRec: Boolean; const Args: array of const);
begin
  WriteTypedFields(NewRec, Args, []);
end;

procedure TRPCBuffer.AssignBuffer(From: TRPCBuffer);
var
 k:cardinal;
 I: Integer;
begin
  if Assigned(From) and (From <> Self) then
  begin
    Rewrite;
    if Assigned(From.FFieldNames) then
    begin
      FreeFieldNames;
      for I := 0 to From.FFieldNames.Count - 1 do
        FieldName[TFieldNameInfo(From.FFieldNames.Objects[I]).Index] := From.FieldName[TFieldNameInfo(From.FFieldNames.Objects[I]).Index];
    end;
    if (From.HasData) then
    begin
      k := from.RecNo;    
      From.First;
      while not From.Eof do
      begin
        AppendRecord(From);
        From.Next;
      end;
      First;
      from.JumpTo(k);      
    end;
  end;
end;

procedure TRPCBuffer.ImportFromEventBuffer(const ExtEventBuffer: TEventBuffer);
begin
  Rewrite;
  EventBuffer.Event := ExtEventBuffer.Event;
  EventBuffer.ResultBufferCount := ExtEventBuffer.ResultBufferCount;
  if ExtEventBuffer.ResultBuffer <> nil then
    LoadFromPChar(ExtEventBuffer.ResultBuffer, ExtEventBuffer.ResultBufferCount);
end;

procedure TRPCBuffer.ImportFromEventBuffer;
begin
  Rewrite;
  if EventBuffer.ResultBuffer <> nil then
    LoadFromPChar(EventBuffer.ResultBuffer, EventBuffer.ResultBufferCount);
end;

procedure TRPCBuffer.ExportToEventBuffer(const Event: Integer);
var
  FMemoryStream: TMemoryStream;
begin
  if Assigned(EventBuffer.ResultBuffer) then
  begin
    FreeMem(EventBuffer.ResultBuffer);
    EventBuffer.ResultBuffer := nil;
  end;

  EventBuffer.Event := Event;
  FMemoryStream := TMemoryStream.Create;
  try
    SaveToStream(FMemoryStream);
    FMemoryStream.Position := 0;
    EventBuffer.ResultBufferCount := FMemoryStream.Size;
    GetMem(EventBuffer.ResultBuffer, FMemoryStream.Size);
    FMemoryStream.Read(EventBuffer.ResultBuffer^, FMemoryStream.Size);
  finally
    FMemoryStream.Free;
  end;
end;

procedure TRPCBuffer.AppendRecord(const From: TRPCBuffer);
var
  I: Integer;
begin
  if From.HasData then
  begin
    for I := 0 to From.FieldCount - 1 do
      WriteFieldFromBufferField(I = 0, From, I);
  end;
end;

procedure TRPCBuffer.AppendFields(const Args: array of const);
begin
  WriteFields(True, Args);
end;

procedure TRPCBuffer.AppendTypedFields(const Args: array of const; const Types: array of TRPCFieldType);
begin
  WriteTypedFields(True, Args, Types);
end;

procedure TRPCBuffer.WriteStreamField(const NewRec: Boolean; Data: TStream; const FromBeggining: Boolean; const ASize: Integer; const FreeStream: Boolean);
var
  FField: PRPCField;
  FSize: Integer;
begin
  if NewRec or not HasData then
    AppendRPCRecord;
  FSize := 0;
  if Assigned(Data) then
  begin
    if ASize = -1 then
    begin
      if FromBeggining then
      begin
        FSize := Data.Size;
        Data.Position := 0;
      end
      else
        FSize := Data.Size - Data.Position
    end
    else
    begin
      FSize := ASize;
      if FromBeggining then
        Data.Position := 0;
    end;
  end;
  FField := AppendRPCField(dtBlob);
  if Assigned(Data) then
    SetFieldFromStream(FField, Data, FSize, FieldCount - 1);
  FHasChanged := True;
  if FreeStream then
    Data.Free;
end;

procedure TRPCBuffer.AppendStreamField(Data: TStream);
begin
  WriteStreamField(true, Data, False);
end;

procedure TRPCBuffer.Delete;
begin
  if HasData then
  begin
    RemoveRPCRecord(FRecordPointer);
    Prior;
  end;
end;

procedure TRPCBuffer.SaveToStream(Stream: TStream; const BufferFormat: TApBufferFormat);
  procedure SaveOldHeader;
  var
    FSep: Char;
  begin
    FSep := FEscSep;
    Stream.Write(FSep, 1);
    FSep := FFieldSep;
    Stream.Write(FSep, 1);
  end;

  procedure SaveNewHeader;
  var
    FHasData: Boolean;
  begin
    FHasData := HasData;
    Stream.Write(FHasData, SizeOf(FHasData));
  end;

  procedure SaveXMLHeader;
  var
    FDummy: String;
  begin
    FDummy := '<?xml version="1.0" encoding="windows-1252"?>' + #13#10 +
      '<buffer>' + #13#10;
    Stream.Write(FDummy[1], Length(FDummy));
  end;

  procedure SaveOldFooter;
  var
    FSep: Char;
  begin
    if Count = 0 then
    begin
      FSep := FRecSep;
      Stream.Write(FSep, 1);
      Stream.Write(FSep, 1);
      Stream.Write(FSep, 1);
      FSep := FNullSep;
      Stream.Write(FSep, 1);
      Stream.Size := Stream.Size - 1;
    end
    else
    begin
      FSep := FRecSep;
      Stream.Write(FSep, 1);
      Stream.Write(FSep, 1);
      FSep := FNullSep;
      Stream.Write(FSep, 1);
      Stream.Size := Stream.Size - 1;
    end;
  end;

  procedure SaveXMLFooter;
  var
    FDummy: String;
  begin
    FDummy := '</buffer>';
    Stream.Write(FDummy[1], Length(FDummy));
  end;
var
  I: Integer;
begin
  if BufferFormat = bfXML then
    SaveToXMLStream(Stream)
  else
  begin
    //Header
    case BufferFormat of
      bfOld: SaveOldHeader;
      bfNew: SaveNewHeader;
      bfXML: SaveXMLHeader;
    end;
    //Save Records
    for I := 0 to Count - 1 do
      Records[I].SaveToStream(Stream, BufferFormat);
    //Footer
    case BufferFormat of
      bfOld: SaveOldFooter;
      bfNew: ; //New don't have Footer
      bfXML: SaveXMLFooter;
    end;
  end;  
end;

procedure TRPCBuffer.LoadFromStream(Stream: TStream; Len: Longint; const BufferFormat: TApBufferFormat=bfNew);
var
  FHasData: Boolean;
  FDummy: string[5];
  FSep: Char;
begin
  if HasData then
    Rewrite;
  Stream.Position := 0;
  if BufferFormat = bfXML then
    LoadFromXMLStream(Stream)
  else if BufferFormat = bfOld then
  begin
    Stream.Read(FDummy[1], 5);
    FHasData := FDummy <> (FEscSep + FFieldSep + FRecSep + FRecSep + FRecSep);
    Stream.Position := 3;
  end
  else
    Stream.Read(FHasData, SizeOf(FHasData));
  if FHasData then
  begin
    while Stream.Position < Stream.Size do
    begin
      AppendRPCRecord.LoadFromStream(Stream, BufferFormat);
      if BufferFormat = bfOld then
      begin
        if Stream.Read(FSep, 1) <> 1 then
          raise Exception.Create('Stream read Error')
        else if FSep = FRecSep then
          Break
        else
          Stream.Position := Stream.Position - 1;
      end;
    end;
    First;
  end;
end;

procedure TRPCBuffer.SaveToFile(const FieldNo: Integer; const FileName: string);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  Self.GetStreamField(FieldNo, Stream);
  try
    Stream.SaveToFile(FileName);
  finally
    Stream.Free;
  end;
end;

procedure TRPCBuffer.LoadFromFile(const NewRec: Boolean; const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    WriteStreamField(NewRec, Stream, True);
  finally
    Stream.Free;
  end;
  FHasChanged := True;
end;

procedure TRPCBuffer.Clear;
begin
  if Assigned(FDataList) then
  begin
    SetCurrentSortIndex('');
    while Count > 0 do
      RemoveRPCRecord(Count - 1);
    if Assigned(FSortIndex) then
      FSortIndex.ClearList;
    FHasChanged := True;
    ClearDataList;
  end;  
end;

procedure TRPCBuffer.ClearDataList;
begin
  FDataList.Clear;
end;

procedure TRPCBuffer.Seek(ARecNo: Integer);
begin
  if InRange(ARecNo, 0, RecordCount) then
  begin
    FRecordPointer := ARecNo;
    FHitEof := False;
    FHitBof := False;
  end
  else If ARecNo >= 0 then
    raise Exception.Create(' Seek raised out of record set');
end;

procedure TRPCBuffer.Sort(const SortName: string; const BuildKey: array of Word; const IntSort: Boolean; const Ascending: Boolean; const ACaseSensitive: Boolean);
begin
  FSortIndex.AddSort(SortName, BuildKey, Ascending, ACaseSensitive);
  SetCurrentSortIndex(SortName);
end;

procedure TRPCBuffer.MirrorSort(const SortName: string; const BuildKey: array of Word);
begin
  Sort(SortName, BuildKey);
end;


function TRPCBuffer.FindKey(const SortIndex: Integer; const KeyFields: array of Variant; const AKeySensitive: Boolean): Boolean;
var
  I: Integer;
begin
  if (SortIndex >= 0) and Assigned(FSortIndex.GetSortItem(SortIndex)) then
  begin
    FCurrentSortIndex := FSortIndex.Items[SortIndex];
    Result := FCurrentSortIndex.FindKey(KeyFields, I);
    if Result then
    begin
      FCurrentSortIndex.CurrentItem := I;
      FRecordPointer := FCurrentSortIndex.CurrentRecordPointer;
    end;
  end
  else
    Result := False;
end;

function TRPCBuffer.ConstToFormatedString(args: array of const): string;
var
  V: TVarRec;
  I: Integer;
begin
  Result := '';
  I := 0;
  while I <= High(Args) do
  begin
    V := args[I];
    case V.VType of
      vtString: Result := Result + string(V.VString) + RecordSeparator;
      vtAnsiString: Result := Result + string(V.VAnsiString) + RecordSeparator;
      vtPChar: Result := Result + StrPas(V.VPChar) + RecordSeparator;
      vtBoolean: Result := Result + IntToStr(Ord(V.VBoolean)) + RecordSeparator;
      vtInteger: Result := Result + IntToStr(V.VInteger) + RecordSeparator;
      vtInt64: Result := Result + IntToStr(V.VInt64^) + RecordSeparator;
      vtExtended: Result := Result + FloatToStrEx(V.VExtended^) + RecordSeparator;
      vtCurrency: Result := Result + FloatToStrEx(V.VCurrency^) + RecordSeparator;
      vtChar: Result := Result + V.VChar + RecordSeparator;
    end;
    Inc(I);
  end;
end;

procedure TRPCBuffer.GetBufferAsStrings(AList: TStrings);
var
  I, J: Integer;
  S: String;
  LastIndex: Integer;
begin
  if IsSorted then
    LastIndex := FSortIndex.IndexOfSort(FCurrentSortIndex)
  else
    LastIndex := -1;

  J := Recno;
  try
    AList.Clear;
    First;
    while not EOF do
    begin
      S := '';
      for I := 0 to FieldCount - 1 do
      begin
        case FieldType[I] of
          dtString, dtDateTime, dtTime, dtDate, dtPChar:
            S := S + QuotedStr(NewFields[I]);
          dtNotTyped, dtLargeint, dtInteger, dtWord, dtByte, dtExtended, dtCurrency:
            S := S + NewFields[I];
          dtBoolean:
            S := S + BoolToStr(GetBooleanField(I), True);
          dtBlob:
            S := S + '(BLOB)';
          dtObject:
            S := S + '(OBJ)';
          else
            S := S + '(BIN)';
        end;
        S := S + ';';
      end;
      SetLength(S, Length(S) - 1);
      AList.Add(S);
      Next;
    end;
  finally
    JumpTo(J);
    if LastIndex <> -1 then
      FCurrentSortIndex := FSortIndex.Items[LastIndex];
  end;
end;

procedure TRPCBuffer.SaveToClip;
var
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  try
    GetBufferAsStrings(StrList);
    Clipboard.AsText := StrList.Text;
  finally
    FreeAndNil(StrList);
  end;
end;

procedure TRPCBuffer.SaveToTextFile(const AFileName: string);
var
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  try
    GetBufferAsStrings(StrList);
    StrList.SaveToFile(AFileName);
  finally
    FreeAndNil(StrList);
  end;
end;

procedure TRPCBuffer.WriteTypedFields(const NewRec: Boolean; const Args: array of const; const Types: array of TRPCFieldType);
var
  AField: PRPCField;
  I: Integer;
  V: TVarRec;
begin
  try
    for I := 0 to High(Args) do
    begin
      if (NewRec or not HasData) and (I = 0) then
        AppendRPCRecord;
      if Length(Types) > 0 then
        AField := AppendRPCField(Types[I])
      else
        AField := nil;
      V := Args[I];
      case V.VType of
        vtString: SetFieldAsString(AField, V.VString^, FieldCount - 1);
        vtAnsiString: SetFieldAsString(AField, AnsiString(V.VAnsiString), FieldCount - 1);
        vtPChar: SetFieldAsPChar(AField, V.VAnsiString, Length(StrPas(V.VPChar)), FieldCount - 1);
        vtBoolean: SetFieldAsBoolean(AField, V.VBoolean, FieldCount - 1);
        vtInt64: SetFieldAsInt64(AField, V.VInt64^, FieldCount - 1);
        vtInteger: SetFieldAsInteger(AField, V.VInteger, FieldCount - 1);
        vtExtended: SetFieldAsFloat(AField, V.VExtended^, FieldCount - 1);
        vtCurrency: SetFieldAsFloat(AField, V.VCurrency^, FieldCount - 1);
        vtChar: SetFieldAsString(AField, V.VChar, FieldCount - 1);
        vtObject:
          begin
            if V.VObject is TField then
            begin
              if not Assigned(AField) then
                WriteTFieldToBuffer(false, TField(V.VObject))
              else
              begin
                if V.VObject is TBlobField then
                  SetFieldAsPChar(AField, PChar(TField(V.VObject).AsString), TBlobField(V.VObject).BlobSize, FieldCount - 1)
                else if TField(V.VObject).DataType in [ftDate, ftTime, ftDateTime] then
                  SetFieldAsFloat(AField, TField(V.VObject).AsDateTime, FieldCount - 1)
                else
                  SetFieldAsString(AField, TField(V.VObject).AsString, FieldCount - 1);
              end;
            end
            else
              RaiseException('AppendFields: Unsupported object type ' + V.VObject.ClassName);
          end;
      else
        RaiseException('AppendFields: Unsupported data type ' + IntToStr(V.VType));
      end;
    end;
  finally
    FHasChanged := true;   // apenas para debug
  end;
  FHasChanged := True;
end;

function TRPCBuffer.GetFieldType(const nIndex: Integer): TRPCFieldType;
begin
  result := GetCurrentRecord[nIndex].FieldType;
end;

function TRPCBuffer.GetBlobField(const nIndex: Integer): PChar;
begin
  Result := '';
  try
    if GetFieldType(nIndex) = dtBlob then
      with PRPCStreamField(GetCurrentRecord.GetRPCFields(nIndex))^ do
        if Assigned(Value) then
          Result := PChar(TMemoryStream(Value).Memory);
  except
    raise ERPCBufferConversionError.Create(nIndex, dtBlob);
  end;
end;

function TRPCBuffer.GetBooleanField(const nIndex: Integer): Boolean;
begin
  try
    Result := TRPCRecord.GetFieldAsBoolean(GetCurrentRecord.GetRPCFields(nIndex));
  except
    raise ERPCBufferConversionError.Create(nIndex, dtBoolean);
  end;
end;

function TRPCBuffer.GetCurrencyField(const nIndex: Integer): Currency;
begin
  try
    Result := GetExtendedField(nIndex);
  except
    raise ERPCBufferConversionError.Create(nIndex, dtCurrency);
  end;
end;

function TRPCBuffer.GetDateTimeField(const nIndex: Integer): TDateTime;
begin
  try
    Result := TRPCRecord.GetFieldAsFloat(GetCurrentRecord.GetRPCFields(nIndex));
  except
    raise ERPCBufferConversionError.Create(nIndex, dtDateTime);
  end;
end;

function TRPCBuffer.GetExtendedField(const nIndex: Integer): Double;
begin
  try
    Result := TRPCRecord.GetFieldAsFloat(GetCurrentRecord.GetRPCFields(nIndex));
  except
    raise ERPCBufferConversionError.Create(nIndex, dtExtended);
  end;
end;

function TRPCBuffer.GetIntegerField(const nIndex: Integer): Integer;
begin
  try
    Result := TRPCRecord.GetFieldAsInteger(GetCurrentRecord.GetRPCFields(nIndex));
  except
    raise ERPCBufferConversionError.Create(nIndex, dtInteger);
  end;
end;

function TRPCBuffer.GetInt64Field(const nIndex: Integer): Int64;
begin
  try
    Result := TRPCRecord.GetFieldAsInt64(GetCurrentRecord.GetRPCFields(nIndex));
  except
    raise ERPCBufferConversionError.Create(nIndex, dtLargeint);
  end;
end;

function TRPCBuffer.GetObjectField(const nIndex: Integer): PChar;
begin
  try
    Result := GetPCharField(nIndex);
  except
    raise ERPCBufferConversionError.Create(nIndex, dtObject);
  end;
end;

function TRPCBuffer.GetPCharField(const nIndex: Integer): PChar;
begin
  try
    Result := PChar(TRPCRecord.GetFieldAsString(GetCurrentRecord.GetRPCFields(nIndex)));
  except
    raise ERPCBufferConversionError.Create(nIndex, dtPChar);
  end;
end;

function TRPCBuffer.GetStringField(const nIndex: Integer): string;
begin
  Result := '';
  try
    Result := GetFields(nIndex);
  except
    raise ERPCBufferConversionError.Create(nIndex, dtString);
  end;
end;

function TRPCBuffer.GetCharField(const nIndex: Integer): Char;
var
  temp: string;
begin
  Result := '/';
  temp := GetStringField(nIndex);
  if Length(temp) > 0 then
    Result := temp[1];
end;

function TRPCBuffer.getCount: integer;
begin
  Result := FDataList.Count;
end;

function TRPCBuffer.GetVariantField(const nIndex: Integer): Variant;
begin
  try
    Result := GetStringField(nIndex);
  except
    raise ERPCBufferConversionError.Create(nIndex, dtVariant);
  end;
end;

procedure TRPCBuffer.WriteTypedRecord(const Args: array of const; const Types: array of TRPCFieldType);
begin
  WriteTypedFields(True, Args, Types);
end;

procedure TRPCBuffer.WriteBlobField(const NewRec: Boolean; AValue: PChar);
begin
  WriteTypedFields(NewRec, [AValue], [dtBlob]);
end;

procedure TRPCBuffer.WriteBooleanField(const NewRec, AValue: Boolean);
begin
  WriteTypedFields(NewRec, [AValue], [dtBoolean]);
end;

procedure TRPCBuffer.WriteCurrencyField(const NewRec: Boolean; const AValue: Currency);
begin
  WriteTypedFields(NewRec, [AValue], [dtCurrency]);
end;

procedure TRPCBuffer.WriteDateTimeField(const NewRec: Boolean; const AValue: TDateTime);
begin
  WriteTypedFields(NewRec, [AValue], [dtDateTime]);
end;

procedure TRPCBuffer.WriteExtendedField(const NewRec: Boolean; const AValue: Extended);
begin
  WriteTypedFields(NewRec, [AValue], [dtExtended]);
end;

procedure TRPCBuffer.WriteIntegerField(const NewRec: Boolean; const AValue: Integer);
begin
  WriteTypedFields(NewRec, [AValue], [dtInteger]);
end;

procedure TRPCBuffer.WriteInt64Field(const NewRec: Boolean; const AValue: Int64);
begin
  WriteTypedFields(NewRec, [AValue], [dtLargeint]);
end;

procedure TRPCBuffer.WriteObjectField(const NewRec: Boolean; AValue: PChar);
begin
  WriteTypedFields(NewRec, [AValue], [dtObject]);
end;

procedure TRPCBuffer.WritePCharField(const NewRec: Boolean; AValue: PChar);
begin
  WriteTypedFields(NewRec, [AValue], [dtPChar]);
end;

procedure TRPCBuffer.WriteStringField(const NewRec: Boolean; const AValue: string);
begin
  WriteTypedFields(NewRec, [AValue], [dtString]);
end;

procedure TRPCBuffer.WriteVariantField(const NewRec: Boolean; const AValue: Variant);
begin
  WriteTypedFields(NewRec, [AValue], [dtVariant]);
end;

function TRPCBuffer.FindKey(const SortName: string; const KeyFields: array of Variant;
  const AKeySensitive: Boolean): Boolean;
begin
  Result := FindKey(FSortIndex.GetSortIndex(SortName), KeyFields, AKeySensitive);
end;

function TRPCBuffer.RemoveSort(const ASortName: string): Boolean;
begin
  Result := RemoveSort(FSortIndex.GetSortIndex(ASortName));
end;

function TRPCBuffer.RemoveSort(const ASortIndex: Integer): Boolean;
begin
  Result := False;
  if ASortIndex > (FSortIndex.Count - 1) then
    exit;
  if FSortIndex.Items[ASortIndex] = FCurrentSortIndex then
    FCurrentSortIndex := nil;
  Result := FSortIndex.DeleteSort(ASortIndex);
end;

function TRPCBuffer.GetSortIndex(const ASortName: String): Integer;
begin
  Result := FSortIndex.GetSortIndex(ASortName);
end;

procedure TRPCBuffer.LoadFromList(const NewRec: Boolean; const List: TStrings);
var
  I: Integer;
begin
  if List <> nil then
  begin
    for I := 0 to List.Count - 1 do
      WriteFields((NewRec and (I = 0)), [List.Strings[I]]);
  end;
  FHasChanged := True;
end;

function TRPCBuffer.GetRecNo: Integer;
begin
  if HasData then
    Result := FRecordPointer
  else
    Result := 0;
end;

procedure TRPCBuffer.WriteByteField(const NewRec: Boolean; const AValue: Byte);
begin
  WriteTypedFields(NewRec, [AValue], [dtByte]);
end;

procedure TRPCBuffer.WriteDateField(const NewRec: Boolean; const AValue: TDateTime);
begin
  WriteTypedFields(NewRec, [AValue], [dtDate]);
end;

procedure TRPCBuffer.WriteTimeField(const NewRec: Boolean; const AValue: TDateTime);
begin
  WriteTypedFields(NewRec, [AValue], [dtTime]);
end;

procedure TRPCBuffer.WriteWordField(const NewRec: Boolean; const AValue: Word);
begin
  WriteTypedFields(NewRec, [AValue], [dtWord]);
end;

function TRPCBuffer.GetByteField(const nIndex: Integer): Byte;
begin
  try
    Result := GetIntegerField(nIndex);
  except
    raise ERPCBufferConversionError.Create(nIndex, dtByte);
  end;
end;

function TRPCBuffer.GetDateField(const nIndex: Integer): TDateTime;
begin
  try
    Result := Trunc(GetExtendedField(nIndex));
  except
    raise ERPCBufferConversionError.Create(nIndex, dtDate);
  end;
end;

function TRPCBuffer.GetTimeField(const nIndex: Integer): TDateTime;
begin
  try
    Result := Frac(GetExtendedField(nIndex));
  except
    raise ERPCBufferConversionError.Create(nIndex, dtTime);
  end;
end;

function TRPCBuffer.GetWordField(const nIndex: Integer): Word;
begin
  try
    Result := GetIntegerField(nIndex);
  except
    raise ERPCBufferConversionError.Create(nIndex, dtWord);
  end;
end;

procedure TRPCBuffer.ReadComponent(Instance: TComponent);
var
  MemoryStream: TMemoryStream;
  c: TComponent;
begin
  MemoryStream := TMemoryStream.Create;
  try
    GetStreamField(0, MemoryStream);
    MemoryStream.Position := 0;
    //MemoryStream.ReadComponent( Instance ); -> walter 18/11/2004
    c := TComponent.Create(nil);
    try
      c := MemoryStream.ReadComponent(nil);
      Instance.Assign(c);
    finally
      FreeAndNil(c);
    end;
  finally
    MemoryStream.Free;
  end;
end;

procedure TRPCBuffer.WriteComponent(Instance: TComponent);
var
  MemoryStream: TMemoryStream;
begin
  MemoryStream := TMemoryStream.Create;
  try
    MemoryStream.WriteComponent(Instance);
    WriteStreamField(True, MemoryStream, True);
  finally
    MemoryStream.Free;
  end;
  FHasChanged := True;
end;

function TRPCBuffer.JumpTo(const RecNo: Integer): Boolean;
begin
  if HasData and InRange(RecNo, 0, Count - 1) then
  begin
    SetCurrentSortIndex('');
    FRecordPointer := RecNo;
    // walter - 27/07/2006 - igual "Seek"
    FHitEof := False;
    FHitBof := False;
    //
    Result := True;
  end
  else
    Result := False;
end;

procedure TRPCBuffer.WriteBlockStreamField(const NewRec: Boolean; Data: TStream; const BlockSize: Integer);
begin
  WriteStreamField(NewRec or not HasData, Data, False, BlockSize);
end;

procedure TRPCBuffer.LoadFromDataSet(const Source: TDataSet;
  const InitialField: Integer = 0; const FinalField: Integer = -1;
  const ALoadFieldNames: boolean = true);
var
  I: Integer;
  FBookMark: TBookmark;
begin
  if Assigned(Source) and not Source.IsEmpty then
  begin
    FBookMark := Source.GetBookmark;
    try
      if ALoadFieldNames then
      begin
        if Assigned(FFieldNames) then
          FreeFieldNames;
        for I := 0 to Source.FieldCount - 1 do
          FieldName[I] := Source.Fields[I].FieldName;
      end;
      Source.First;
      while not Source.Eof do
      begin
        LoadFromRecordDataSet(Source, InitialField, FinalField);
        Source.Next;
      end;
      First;
    finally
      Source.GotoBookmark(FBookMark);
      Source.FreeBookmark(FBookMark);
    end;
  end;
end;

procedure TRPCBuffer.LoadFromRecordDataSet(const Source: TDataSet; const InitialField: Integer; const FinalField: Integer);
var
  I, FFinalField: Integer;
begin
  if Assigned(Source) and not Source.IsEmpty then
  begin
    if FinalField = -1 then
      FFinalField := Source.FieldCount - 1
    else
      FFinalField := FinalField;
    for I := InitialField to FFinalField do
      WriteFields((I = InitialField), [Source.Fields[I]]);
  end;
end;

procedure TRPCBuffer.SetCurrentSortIndex(const SortName: string);
var
  Item: Integer;
begin
  if Trim(SortName) = '' then
    FCurrentSortIndex := nil
  else
  begin
    Item := FSortIndex.GetSortIndex(SortName);
    if Item <> -1 then
      FCurrentSortIndex := FSortIndex.Items[Item]
    else
      raise Exception.Create('The SortIndex Informed in the SetCurrentSortIndex function not found ! ');
  end;
end;

function TRPCBuffer.GetCurrentSortIndexName: String;
begin
  if Assigned(FCurrentSortIndex) then
    Result := FSortIndex.FSortList.Strings[FSortIndex.FSortList.IndexOfObject(FCurrentSortIndex)]
  else
    Result := '';
end;

function TRPCBuffer.StrFindKey(const SortIndex: Integer; const KeyFields: array of string; const AKeySensitive: Boolean): Boolean;
var
  I: Integer;
begin
  if (SortIndex >= 0) and Assigned(FSortIndex.GetSortItem(SortIndex)) then
  begin
    FCurrentSortIndex := FSortIndex.Items[SortIndex];
    Result := FCurrentSortIndex.FindKey(KeyFields, I);
    if Result then
    begin
      FCurrentSortIndex.CurrentItem := I;
      FRecordPointer := FCurrentSortIndex.CurrentRecordPointer;
    end;
  end
  else
    Result := False;
end;

function TRPCBuffer.StrFindKey(const SortName: string;
  const KeyFields: array of string; const AKeySensitive: Boolean): Boolean;
begin
  Result := StrFindKey(FSortIndex.GetSortIndex(SortName), KeyFields, AKeySensitive);
end;

function TRPCBuffer.StrFindNearestKey(const SortIndex: Integer;
  const KeyFields: array of string; const AKeySensitive: Boolean): Boolean;
var
  I: Integer;
begin
  if (SortIndex >= 0) and Assigned(FSortIndex.GetSortItem(SortIndex)) then
  begin
    FCurrentSortIndex := FSortIndex.Items[SortIndex];
    Result := FCurrentSortIndex.FindKey(KeyFields, I);
    FCurrentSortIndex.CurrentItem := I;
    FRecordPointer := FCurrentSortIndex.CurrentRecordPointer;
  end
  else
    Result := False;
end;

function TRPCBuffer.StrFindNearestKey(const SortName: string;
  const KeyFields: array of string; const AKeySensitive: Boolean): Boolean;
begin
  Result := StrFindNearestKey(FSortIndex.GetSortIndex(SortName), KeyFields, AKeySensitive);
end;

function TRPCBuffer.FindNearestKey(const SortIndex: Integer;
  const KeyFields: array of Variant; const AKeySensitive: Boolean): Boolean;
var
  I: Integer;
begin
  if (SortIndex >= 0) and Assigned(FSortIndex.GetSortItem(SortIndex)) then
  begin
    FCurrentSortIndex := FSortIndex.Items[SortIndex];
    Result := FCurrentSortIndex.FindKey(KeyFields, I);
    FCurrentSortIndex.CurrentItem := Min(FCurrentSortIndex.Count - 1, I);
    FRecordPointer := FCurrentSortIndex.CurrentRecordPointer;
  end
  else
    Result := False;
end;

function TRPCBuffer.FindNearestKey(const SortName: string;
  const KeyFields: array of Variant; const AKeySensitive: Boolean): Boolean;
begin
  Result := FindNearestKey(FSortIndex.GetSortIndex(SortName), KeyFields, AKeySensitive);
end;

function TRPCBuffer.IntFindKey(const SortIndex: Integer; const KeyFields: array of Integer; const AKeySensitive: Boolean; const Inverted: Boolean = False): Boolean;
var
  I: Integer;
begin
  if (SortIndex >= 0) and Assigned(FSortIndex.GetSortItem(SortIndex)) then
  begin
    FCurrentSortIndex := FSortIndex.Items[SortIndex];
    Result := FCurrentSortIndex.FindKey(KeyFields, I);
    if Result then
    begin
      FCurrentSortIndex.CurrentItem := I;
      FRecordPointer := FCurrentSortIndex.CurrentRecordPointer;
    end;
  end
  else
    Result := False;
end;

function TRPCBuffer.IntFindKey(const SortName: string;
  const KeyFields: array of Integer; const AKeySensitive: Boolean): Boolean;
begin
  Result := IntFindKey(FSortIndex.GetSortIndex(SortName), KeyFields, AKeySensitive);
end;

function TRPCBuffer.IntFindNearestKey(const SortIndex: Integer;
  const KeyFields: array of Integer; const AKeySensitive: Boolean): Boolean;
var
  I: Integer;
begin
  if (SortIndex >= 0) and Assigned(FSortIndex.GetSortItem(SortIndex)) then
  begin
    FCurrentSortIndex := FSortIndex.Items[SortIndex];
    Result := FCurrentSortIndex.FindKey(KeyFields, I);
    FCurrentSortIndex.CurrentItem := Min(FCurrentSortIndex.Count, I);
    FRecordPointer := FCurrentSortIndex.CurrentRecordPointer;
  end
  else
    Result := False;
end;

function TRPCBuffer.IntFindNearestKey(const SortName: string;
  const KeyFields: array of Integer; const AKeySensitive: Boolean): Boolean;
begin
  Result := IntFindNearestKey(FSortIndex.GetSortIndex(SortName), KeyFields, AKeySensitive);
end;

function TRPCBuffer.IndexJumpTo(const RecNumber: Integer): Boolean;
begin
  Result := JumpTo(RecNumber);
end;

procedure TRPCBuffer.SetFieldType(const Index: Integer; const FieldType: TRPCFieldType);
var
  AField: PRPCField;
begin
  if not HasData then
    RaiseException('No data available')
  else
  begin
    AField := GetCurrentRecord.AddField(FieldType, Index);
    try
      SetFieldFromField(AField, GetRPCFields(Index + 1));
      GetCurrentRecord.DeleteField(Index + 1);
    except
      GetCurrentRecord.DeleteField(Index);
      raise;
    end;
  end;
end;

function TRPCBuffer.getIsSorted: Boolean;
begin
  Result := FSortIndex.Count > 0;
end;

function TRPCBuffer.SetStreamField(const nIndex: Integer; Data: TStream; ARewrite: boolean = FALSE): Integer;
var
  FField: PRPCField;
begin
  FField := RPCFields[nIndex];
  if ARewrite and Assigned(PRPCStreamField(FField)^.Value) then // walter - 01/03/2009
    PRPCStreamField(FField)^.Value.Clear;
  if Assigned(Data) then
    Result := SetFieldFromStream(FField, Data, Data.Size, nIndex)
  else
    Result := 0;
end;

procedure TRPCBuffer.ReadRPCBuffer(const nIndex: Integer; Buffer: TRPCBuffer; const BufferFormat: TApBufferFormat);
var
  AMemory: TMemoryStream;
begin
  AMemory := TMemoryStream.Create;
  try
    if GetStreamField(nIndex, AMemory) > 0 then
    begin
      AMemory.Position := 0;
      Buffer.LoadFromStream(AMemory, AMemory.Size, BufferFormat);
    end;
  finally
    AMemory.Free;
  end;
end;

procedure TRPCBuffer.WriteRPCBuffer(const NewRec: Boolean; Buffer: TRPCBuffer; const BufferFormat: TApBufferFormat);
var
  AMemory: TMemoryStream;
begin
  AMemory := TMemoryStream.Create;
  try
    Buffer.SaveToStream(AMemory, BufferFormat);
    WriteStreamField(NewRec, AMemory, True);
  finally
    AMemory.Free;
  end;
end;

procedure TRPCBuffer.LoadFromDelimitedFile(const FileName, Delimiter: string);
var
  FileBuffer: TextFile;
  Buff: string;
  LineRead: string;
  I: Integer;
  NewRec: Boolean;
begin
  AssignFile(FileBuffer, FileName);
  try
    Reset(FileBuffer);
    Rewrite;
    First;
    while True do
    begin
      NewRec := True;
      ReadLn(FileBuffer, LineRead);
      if LineRead = '' then
        Break;
      for I := 1 to Length(LineRead) do
      begin
        if LineRead[I] = #10 then
          continue;
        if (LineRead[I] = Delimiter) or (LineRead[I] = #13) then
        begin
          WriteFields(NewRec, [buff]);
          buff := '';
          NewRec := False;
          continue;
        end;
        buff := buff + LineRead[I];
      end;
    end;
  finally
    CloseFile(FileBuffer);
  end;
  FHasChanged := True;
end;

procedure TRPCBuffer.LoadFromPChar(const Value: PChar; const Size: Integer; const BufferFormat: TApBufferFormat);
var
  Stream: TPCharStream;
begin
  Stream := TPCharStream.Create(Value, Size);
  try
    Stream.Position := 0;
    LoadFromStream(Stream, Stream.Size, BufferFormat);
  finally
    Stream.Free;
  end;
end;

procedure TRPCBuffer.WriteTFieldToBuffer(const NewRec: Boolean; const Field: TField);
var
  ms: TMemoryStream;
begin
  case Field.DataType of
    ftInteger, ftLargeint, ftSmallint,
      ftAutoInc, ftBytes, ftWord: WriteIntegerField(NewRec, Field.AsInteger);
    ftFloat, ftCurrency: WriteExtendedField(NewRec, Field.AsFloat);
    ftString, ftFixedChar, ftWideString: WriteStringField(NewRec, Field.AsString);
    ftBoolean: WriteBooleanField(NewRec, Field.AsBoolean);
    ftDate, ftDateTime, ftTime: WriteDateTimeField(NewRec, Field.AsDateTime);
    ftBlob, ftGraphic, ftOraBlob,
      ftOraClob:
      begin
        ms := TMemoryStream.Create;
        try
          TBlobField(Field).SaveToStream(ms);
          ms.Position := 0;
          WriteBlockStreamField(NewRec, ms, ms.Size);
        finally
          ms.free;
        end;
      end;
  else
    WriteStringField(NewRec, Field.AsString);
  end;
end;

function TRPCBuffer.GetHasData: Boolean;
begin
  Result := Count > 0;
end;

procedure TRPCBuffer.SetHasData(const Value: Boolean);
begin
  Clear;
  FRecordPointer := -1;
end;

//--- Get and Set Field Methods

procedure TRPCBuffer.SetFieldAsString(var Field: PRPCField; const Value: string; const FieldIndex: Integer);
var
  AList: TList;
begin
  if not Assigned(Field) then
    Field := AppendRPCField(dtString);
  AdjustFieldTypeAndSize(Field, Field^.FieldType, Length(Value), FieldIndex);
  AList := nil;
  FSortIndex.GenerateNeededUpdates(FieldIndex, GetCurrentRecord, AList);
  try
    case Field^.FieldType of
      dtBlob: if Value <> '' then
        begin
          PRPCStreamField(Field)^.Value.Size := 0;
          PRPCStreamField(Field)^.Value.Write(Value[1], Length(Value));
        end;
      dtInteger: PRPCIntegerField(Field)^.Value := StrToIntDef(Value, 0);
      dtLargeint: PRPCInt64Field(Field)^.Value := StrToInt64Def(Value, 0);
      dtDateTime,
        dtExtended,
        dtCurrency,
        dtTime,
        dtDate: PRPCFloatField(Field)^.Value := StrToFloatDef(Value, 0);
      dtBoolean: PRPCBooleanField(Field)^.Value := ('1' = Value);
      dtByte: PRPCByteField(Field)^.Value := StrToIntDef(Value, 0);
      dtWord: PRPCWordField(Field)^.Value := StrToIntDef(Value, 0);
    else
      PRPCStringField(Field)^.Value := Value;
    end;
  finally
    FSortIndex.UpdateNeededUpdates(GetCurrentRecord, AList);
  end;
end;

procedure TRPCBuffer.SetFieldAsInteger(var Field: PRPCField; Value: Integer;
  const FieldIndex: Integer);
var
  SValue: string;
  AList: TList;
begin
  if not Assigned(Field) then
    Field := AppendRPCField(dtInteger);
  AdjustFieldTypeAndSize(Field, Field^.FieldType, Length(IntToStr(Value)), FieldIndex);
  AList := nil;
  FSortIndex.GenerateNeededUpdates(FieldIndex, GetCurrentRecord, AList);
  try
    case Field^.FieldType of
      dtBlob:
        begin
          SValue := IntToStr(Value);
          PRPCStreamField(Field)^.Value.Size := 0;
          PRPCStreamField(Field)^.Value.Write(SValue[1], Length(SValue));
        end;
      dtInteger: PRPCIntegerField(Field)^.Value := Value;
      dtLargeint: PRPCInt64Field(Field)^.Value := Value;
      dtDateTime,
        dtExtended,
        dtCurrency,
        dtTime,
        dtDate: PRPCFloatField(Field)^.Value := Value;
      dtBoolean: PRPCBooleanField(Field)^.Value := (1 = Value);
      dtByte: PRPCByteField(Field)^.Value := Value;
      dtWord: PRPCWordField(Field)^.Value := Value;
    else
      PRPCStringField(Field)^.Value := IntToStr(Value);
    end;
  finally
    FSortIndex.UpdateNeededUpdates(GetCurrentRecord, AList);
  end;
end;

procedure TRPCBuffer.SetFieldAsInt64(var Field: PRPCField; Value: Int64;
  const FieldIndex: Integer);
var
  SValue: string;
  AList: TList;
begin
  if not Assigned(Field) then
    Field := AppendRPCField(dtLargeint);
  AdjustFieldTypeAndSize(Field, Field^.FieldType, Length(IntToStr(Value)), FieldIndex);
  AList := nil;
  FSortIndex.GenerateNeededUpdates(FieldIndex, GetCurrentRecord, AList);
  try
    case Field^.FieldType of
      dtBlob:
        begin
          SValue := IntToStr(Value);
          PRPCStreamField(Field)^.Value.Size := 0;
          PRPCStreamField(Field)^.Value.Write(SValue[1], Length(SValue));
        end;
      dtInteger: PRPCIntegerField(Field)^.Value := Value;
      dtLargeint: PRPCInt64Field(Field)^.Value := Value;
      dtDateTime,
        dtExtended,
        dtCurrency,
        dtTime,
        dtDate: PRPCFloatField(Field)^.Value := Value;
      dtBoolean: PRPCBooleanField(Field)^.Value := (1 = Value);
      dtByte: PRPCByteField(Field)^.Value := Value;
      dtWord: PRPCWordField(Field)^.Value := Value;
    else
      PRPCStringField(Field)^.Value := IntToStr(Value);
    end;
  finally
    FSortIndex.UpdateNeededUpdates(GetCurrentRecord, AList);
  end;
end;

procedure TRPCBuffer.SetFieldAsFloat(var Field: PRPCField; Value: Double;
  const FieldIndex: Integer);
var
  SValue: string;
  AList: TList;
begin
  if not Assigned(Field) then
    Field := AppendRPCField(dtExtended);
  AdjustFieldTypeAndSize(Field, Field^.FieldType, Length(FloatToStr(Value)), FieldIndex);
  AList := nil;
  FSortIndex.GenerateNeededUpdates(FieldIndex, GetCurrentRecord, AList);
  try
    case Field^.FieldType of
      dtBlob:
        begin
          SValue := FloatToStr(Value);
          PRPCStreamField(Field)^.Value.Size := 0;
          PRPCStreamField(Field)^.Value.Write(SValue[1], Length(SValue));
        end;
      dtInteger: PRPCIntegerField(Field)^.Value := Trunc(Value);
      dtLargeint: PRPCInt64Field(Field)^.Value := Trunc(Value);
      dtDateTime,
        dtExtended,
        dtCurrency,
        dtTime,
        dtDate: PRPCFloatField(Field)^.Value := Value;
      dtBoolean: PRPCBooleanField(Field)^.Value := (1 = Value);
      dtByte: PRPCByteField(Field)^.Value := Trunc(Value);
      dtWord: PRPCWordField(Field)^.Value := Trunc(Value);
    else
      PRPCStringField(Field)^.Value := FloatToStr(Value);
    end;
  finally
    FSortIndex.UpdateNeededUpdates(GetCurrentRecord, AList);
  end;
end;

procedure TRPCBuffer.SetFieldAsBoolean(var Field: PRPCField; Value: Boolean;
  const FieldIndex: Integer);
var
  SValue: string;
  AList: TList;  
begin
  if not Assigned(Field) then
    Field := AppendRPCField(dtBoolean);
  AdjustFieldTypeAndSize(Field, Field^.FieldType, 1, FieldIndex);
  AList := nil;  
  FSortIndex.GenerateNeededUpdates(FieldIndex, GetCurrentRecord, AList);
  try
    case Field^.FieldType of
      dtBlob:
        begin
          SValue := IfThen(Value, '1', '0');
          PRPCStreamField(Field)^.Value.Size := 0;
          PRPCStreamField(Field)^.Value.Write(SValue[1], Length(SValue));
        end;
      dtInteger: PRPCIntegerField(Field)^.Value := Integer(Value);
      dtLargeint: PRPCInt64Field(Field)^.Value := Int64(Value);
      dtDateTime,
        dtExtended,
        dtCurrency,
        dtTime,
        dtDate: PRPCFloatField(Field)^.Value := Integer(Value);
      dtBoolean: PRPCBooleanField(Field)^.Value := Value;
      dtByte: PRPCByteField(Field)^.Value := Integer(Value);
      dtWord: PRPCWordField(Field)^.Value := Integer(Value);
    else
      PRPCStringField(Field)^.Value := IfThen(Value, '1', '0');
    end;
  finally
    FSortIndex.UpdateNeededUpdates(GetCurrentRecord, AList);
  end;
end;

procedure TRPCBuffer.SetFieldAsPChar(var Field: PRPCField; Value: PChar;
  const Size: Integer; const FieldIndex: Integer);
var
  AList: TList;
begin
  if not Assigned(Field) then
    Field := AppendRPCField(dtPChar);
  AdjustFieldTypeAndSize(Field, Field^.FieldType, Size, FieldIndex);
  AList := nil;  
  FSortIndex.GenerateNeededUpdates(FieldIndex, GetCurrentRecord, AList);
  try
    case Field^.FieldType of
      dtBlob:
        begin
          PRPCStreamField(Field)^.Value.Size := 0;
          if Size > 0 then
            PRPCStreamField(Field)^.Value.Write(Value^, Size);
        end;
      dtInteger: PRPCIntegerField(Field)^.Value := StrToIntDef(StrPas(Value), 0);
      dtLargeint: PRPCInt64Field(Field)^.Value := StrToInt64Def(StrPas(Value), 0);
      dtDateTime,
        dtExtended,
        dtCurrency,
        dtTime,
        dtDate: PRPCFloatField(Field)^.Value := StrToFloatDef(StrPas(Value), 0);
      dtBoolean: PRPCBooleanField(Field)^.Value := (Value^ = '1');
      dtByte: PRPCByteField(Field)^.Value := StrToIntDef(StrPas(Value), 0);
      dtWord: PRPCWordField(Field)^.Value := StrToIntDef(StrPas(Value), 0);
    else
      PRPCStringfield(Field)^.Value := StrPas(Value);
    end;
  finally
    FSortIndex.UpdateNeededUpdates(GetCurrentRecord, AList);
  end;
end;

function TRPCBuffer.GetCurrentRecord: TRPCRecord;
begin
  if not HasData then
    RaiseException('No data available');
  if FRecordPointer >= 0 then
    Result := Records[FRecordPointer]
  else
    Result := nil;
end;

function TRPCBuffer.GetRecords(Index: Integer): TRPCRecord;
begin
  Result := TRPCRecord(FDataList.Items[Index]);
end;

function TRPCBuffer.GetRPCFields(Index: Integer): PRPCField;
var
  FCurrentRecord: TRPCRecord;
begin
  if not HasData then
    RaiseException('No data available');
  if (Index < 0) then
    RaiseException('Invalid field index');

  if FRecordPointer = -1 then
    First;
  FCurrentRecord := GetCurrentRecord;
  if InRange(Index, 0, FCurrentRecord.Count - 1) then
    Result := FCurrentRecord[Index]
  else
    raise Exception.CreateFmt('Field Index %d does not exists', [Index]);
end;

function TRPCBuffer.AppendRPCRecord: TRPCRecord;
begin
  Result := TRPCRecord.Create;
  FRecordPointer := FDataList.Add(Result);
  Result.FBuffer := Self;
end;

function TRPCBuffer.AppendRPCField(const ADataType: TRPCFieldType): PRPCField;
var
  FCurrentRecord: TRPCRecord;
begin
  FCurrentRecord := GetCurrentRecord;
  Result := FCurrentRecord.AddField(ADataType);
end;

procedure TRPCBuffer.AdjustFieldTypeAndSize(var Field: PRPCField;
  const AFieldType: TRPCFieldType; FieldSize: Cardinal; const FieldIndex: Integer);
begin
  GetCurrentRecord.AdjustFieldTypeAndSize(Field, AFieldType, FieldIndex, FieldSize);
end;

procedure TRPCBuffer.SetFieldFromField(var Field: PRPCField; FieldFrom: PRPCField);

  procedure SetBlobField;
  begin
    with PRPCStreamField(Field)^ do
    begin
      case FieldFrom.FieldType of
        dtBlob:
          begin
            if Assigned(Value) then
            begin
              Value.Free;
              Value := nil;
            end;
            if Assigned(PRPCStreamField(FieldFrom)^.Value) then
            begin
              Value := TMemoryStream.Create;
              PRPCStreamField(FieldFrom)^.Value.Position := 0;
              Value.CopyFrom(PRPCStreamField(FieldFrom)^.Value, PRPCStreamField(FieldFrom)^.Value.Size);
            end;
          end;
        dtInteger: raise Exception.Create('Buffer Integer Field Cannot be changed into Blob Field');
        dtLargeint: raise Exception.Create('Buffer Int64 Field Cannot be changed into Blob Field');
        dtBoolean: raise Exception.Create('Buffer Boolean Field Cannot be changed into Blob Field');
        dtExtended,
          dtCurrency: raise Exception.Create('Buffer Float Field Cannot be changed into Blob Field');
        dtDate,
          dtDateTime,
          dtTime: raise Exception.Create('Buffer Time Field Cannot be changed into Blob Field');
        dtWord: raise Exception.Create('Buffer Word Field Cannot be changed into Blob Field');
        dtByte: raise Exception.Create('Buffer Byte Field Cannot be changed into Blob Field');
      else
        raise Exception.Create('Buffer String Field Cannot be changed into Blob Field');
      end;
    end;
  end;

  procedure SetIntegerField;
  begin
    with PRPCIntegerField(Field)^ do
    begin
      case FieldFrom.FieldType of
        dtBlob: raise Exception.Create('Buffer Blob Field Cannot be changed into Integer Field');
        dtInteger: Value := PRPCIntegerField(FieldFrom)^.Value;
        dtLargeint: Value := PRPCInt64Field(FieldFrom)^.Value;
        dtBoolean: Value := Integer(PRPCBooleanField(FieldFrom)^.Value);
        dtExtended,
          dtCurrency: Value := Trunc(PRPCFloatField(FieldFrom)^.Value);
        dtDate,
          dtDateTime,
          dtTime: Value := Trunc(PRPCDateTimeField(FieldFrom)^.Value);
        dtWord: Value := PRPCWordField(FieldFrom)^.Value;
        dtByte: Value := PRPCByteField(FieldFrom)^.Value;
      else
        Value := StrToIntDef(PRPCStringField(FieldFrom)^.Value, 0);
      end;
    end;
  end;

  procedure SetInt64Field;
  begin
    with PRPCInt64Field(Field)^ do
    begin
      case FieldFrom.FieldType of
        dtBlob: raise Exception.Create('Buffer Blob Field Cannot be changed into Int64 Field');
        dtInteger: Value := PRPCIntegerField(FieldFrom)^.Value;
        dtLargeint: Value := PRPCInt64Field(FieldFrom)^.Value;
        dtBoolean: Value := Int64(PRPCBooleanField(FieldFrom)^.Value);
        dtExtended,
          dtCurrency: Value := Trunc(PRPCFloatField(FieldFrom)^.Value);
        dtDate,
          dtDateTime,
          dtTime: Value := Trunc(PRPCDateTimeField(FieldFrom)^.Value);
        dtWord: Value := PRPCWordField(FieldFrom)^.Value;
        dtByte: Value := PRPCByteField(FieldFrom)^.Value;
      else
        Value := StrToInt64Def(PRPCStringField(FieldFrom)^.Value, 0);
      end;
    end;
  end;

  procedure SetBooleanField;
  begin
    with PRPCBooleanField(Field)^ do
    begin
      case FieldFrom.FieldType of
        dtBlob: raise Exception.Create('Buffer Blob Field Cannot be changed into Integer Field');
        dtInteger: Value := Boolean(PRPCIntegerField(FieldFrom)^.Value);
        dtLargeint: Value := Boolean(PRPCInt64Field(FieldFrom)^.Value);
        dtBoolean: Value := PRPCBooleanField(FieldFrom)^.Value;
        dtExtended,
          dtCurrency: Value := Boolean(Trunc(PRPCFloatField(FieldFrom)^.Value));
        dtDate,
          dtDateTime,
          dtTime: Value := Boolean(Trunc(PRPCDateTimeField(FieldFrom)^.Value));
        dtWord: Value := Boolean(PRPCWordField(FieldFrom)^.Value);
        dtByte: Value := Boolean(PRPCByteField(FieldFrom)^.Value);
      else
        Value := PRPCStringField(FieldFrom)^.Value = '1';
      end;
    end;
  end;

  procedure SetFloatField;
  begin
    with PRPCFloatField(Field)^ do
    begin
      case FieldFrom.FieldType of
        dtBlob: raise Exception.Create('Buffer Blob Field Cannot be changed into Integer Field');
        dtInteger: Value := PRPCIntegerField(FieldFrom)^.Value;
        dtLargeint: Value := PRPCInt64Field(FieldFrom)^.Value;
        dtBoolean: Value := Integer(PRPCBooleanField(FieldFrom)^.Value);
        dtExtended,
          dtCurrency: Value := PRPCFloatField(FieldFrom)^.Value;
        dtDate,
          dtDateTime,
          dtTime: Value := PRPCDateTimeField(FieldFrom)^.Value;
        dtWord: Value := PRPCWordField(FieldFrom)^.Value;
        dtByte: Value := PRPCByteField(FieldFrom)^.Value;
      else
        Value := StrToFloatDef(PRPCStringField(FieldFrom)^.Value, 0);
      end;
    end;
  end;

  procedure SetDateTimeField;
  begin
    with PRPCDateTimeField(Field)^ do
    begin
      case FieldFrom.FieldType of
        dtBlob: raise Exception.Create('Buffer Blob Field Cannot be changed into Integer Field');
        dtInteger: Value := PRPCIntegerField(FieldFrom)^.Value;
        dtLargeint: Value := PRPCInt64Field(FieldFrom)^.Value;
        dtBoolean: Value := Integer(PRPCBooleanField(FieldFrom)^.Value);
        dtExtended,
          dtCurrency: Value := PRPCFloatField(FieldFrom)^.Value;
        dtDate,
          dtDateTime,
          dtTime: Value := PRPCDateTimeField(FieldFrom)^.Value;
        dtWord: Value := PRPCWordField(FieldFrom)^.Value;
        dtByte: Value := PRPCByteField(FieldFrom)^.Value;
      else
        Value := StrToFloatDef(PRPCStringField(FieldFrom)^.Value, 0);
      end;
    end;
  end;

  procedure SetWordField;
  begin
    with PRPCWordField(Field)^ do
    begin
      case FieldFrom.FieldType of
        dtBlob: raise Exception.Create('Buffer Blob Field Cannot be changed into Integer Field');
        dtInteger: Value := PRPCIntegerField(FieldFrom)^.Value;
        dtLargeint: Value := PRPCInt64Field(FieldFrom)^.Value;
        dtBoolean: Value := Word(PRPCBooleanField(FieldFrom)^.Value);
        dtExtended,
          dtCurrency: Value := Trunc(PRPCFloatField(FieldFrom)^.Value);
        dtDate,
          dtDateTime,
          dtTime: Value := Trunc(PRPCDateTimeField(FieldFrom)^.Value);
        dtWord: Value := PRPCWordField(FieldFrom)^.Value;
        dtByte: Value := PRPCByteField(FieldFrom)^.Value;
      else
        Value := StrToIntDef(PRPCStringField(FieldFrom)^.Value, 0);
      end;
    end;
  end;

  procedure SetByteField;
  begin
    with PRPCByteField(Field)^ do
    begin
      case FieldFrom.FieldType of
        dtBlob: raise Exception.Create('Buffer Blob Field Cannot be changed into Integer Field');
        dtInteger: Value := PRPCIntegerField(FieldFrom)^.Value;
        dtLargeint: Value := PRPCInt64Field(FieldFrom)^.Value;
        dtBoolean: Value := Byte(PRPCBooleanField(FieldFrom)^.Value);
        dtExtended,
          dtCurrency: Value := Trunc(PRPCFloatField(FieldFrom)^.Value);
        dtDate,
          dtDateTime,
          dtTime: Value := Trunc(PRPCDateTimeField(FieldFrom)^.Value);
        dtWord: Value := PRPCWordField(FieldFrom)^.Value;
        dtByte: Value := PRPCByteField(FieldFrom)^.Value;
      else
        Value := StrToIntDef(PRPCStringField(FieldFrom)^.Value, 0);
      end;
    end;
  end;

  procedure SetStringField;
  begin
    with PRPCStringField(Field)^ do
    begin
      case FieldFrom.FieldType of
        dtBlob: raise Exception.Create('Buffer Blob Field Cannot be changed into Integer Field');
        dtInteger: Value := IntToStr(PRPCIntegerField(FieldFrom)^.Value);
        dtLargeint: Value := IntToStr(PRPCInt64Field(FieldFrom)^.Value);
        dtBoolean: Value := IfThen(PRPCBooleanField(FieldFrom)^.Value, '1', '0');
        dtExtended,
          dtCurrency: Value := FloatToStr(PRPCFloatField(FieldFrom)^.Value);
        dtDate,
          dtDateTime,
          dtTime: Value := FloatToStr(PRPCDateTimeField(FieldFrom)^.Value);
        dtWord: Value := IntToStr(PRPCWordField(FieldFrom)^.Value);
        dtByte: Value := IntToStr(PRPCByteField(FieldFrom)^.Value);
      else
        Value := PRPCStringField(FieldFrom)^.Value;
      end;
    end;
  end;

begin
  case Field^.FieldType of
    dtBlob: SetBlobField;
    dtInteger: SetIntegerField;
    dtLargeint: SetInt64Field;
    dtBoolean: SetBooleanField;
    dtExtended,
      dtCurrency: SetFloatField;
    dtDate,
      dtDateTime,
      dtTime: SetDateTimeField;
    dtWord: SetWordField;
    dtByte: SetByteField;
  else
    SetStringField;
  end;
end;

function TRPCBuffer.SetFieldFromStream(var Field: PRPCField;
  const AStream: TStream; const Size: Cardinal; const FieldIndex: Integer): Cardinal;
begin
  AdjustFieldTypeAndSize(Field, Field^.FieldType, Size, FieldIndex);
  Result := PRPCStreamField(Field)^.Value.CopyFrom(AStream, Min(Size, AStream.Size - AStream.Position));
end;

procedure TRPCBuffer.RemoveRPCRecord(const Index: Integer);
begin
  if Assigned(FSortIndex) then
    FSortIndex.DeleteRecord(Records[Index]);
  Records[Index].Free;
  FDataList.Delete(Index);
end;

function TRPCBuffer.IsEqualTo(const ABuffer: TRPCBuffer): Boolean;
var
  I: Integer;
  J: Integer;
  AMyField, AFromField: PRPCField;
begin
  Result := RecordCount = ABuffer.RecordCount;
  I := 0;
  while Result and (I < Count) do
  begin
    Result := Records[I].Count = ABuffer.Records[I].Count;
    J := 0;
    while Result and (J < Records[I].Count) do
    begin
      AMyField := Records[I].GetRPCFields(J);
      AFromField := ABuffer.Records[I].GetRPCFields(J);
      Result := AMyField^.FieldType = AFromField^.FieldType;
      if Result then
      begin
        case AMyField^.FieldType of
          dtBlob:
            begin
              Result := (Assigned(PRPCStreamField(AMyField)^.Value) and Assigned(PRPCStreamField(AFromField)^.Value)) or
                (not Assigned(PRPCStreamField(AMyField)^.Value) and not Assigned(PRPCStreamField(AFromField)^.Value));
              if Result and Assigned(PRPCStreamField(AMyField)^.Value) then
              begin
                Result := PRPCStreamField(AMyField)^.Value.Size = PRPCStreamField(AFromField)^.Value.Size;
                if Result then
                begin
                  PRPCStreamField(AMyField)^.Value.Position := 0;
                  PRPCStreamField(AFromField)^.Value.Position := 0;
                  Result := StrIComp(PRPCStreamField(AMyField)^.Value.Memory, PRPCStreamField(AFromField)^.Value.Memory) = 0;
                end;
              end;
            end;
          dtInteger: Result := PRPCIntegerField(AMyField)^.Value = PRPCIntegerField(AFromField)^.Value;
          dtLargeint: Result := PRPCInt64Field(AMyField)^.Value = PRPCInt64Field(AFromField)^.Value;
          dtBoolean: Result := PRPCBooleanField(AMyField)^.Value = PRPCBooleanField(AFromField)^.Value;
          dtExtended,
            dtCurrency: Result := PRPCFloatField(AMyField)^.Value = PRPCFloatField(AFromField)^.Value;
          dtDateTime,
            dtDate,
            dtTime: Result := PRPCDateTimeField(AMyField)^.Value = PRPCDateTimeField(AFromField)^.Value;
          dtWord: Result := PRPCWordField(AMyField)^.Value = PRPCWordField(AFromField)^.Value;
          dtByte: Result := PRPCByteField(AMyField)^.Value = PRPCByteField(AFromField)^.Value;
        else
          Result := PRPCStringField(AMyField)^.Value = PRPCStringField(AFromField)^.Value;
        end;
      end;
      Inc(J);
    end;
    Inc(I);
  end;
end;

procedure TRPCBuffer.IntSetFieldAsByte(const nIndex: Integer; const Value: Byte);
begin
  IntSetFieldAsInteger(nIndex, Value);
end;

procedure TRPCBuffer.IntSetFieldAsWord(const nIndex: Integer; const Value: Word);
begin
  IntSetFieldAsInteger(nIndex, Value);
end;

procedure TRPCBuffer.InvertBuffer;
var
  tmpBuf: TRPCBuffer;
  i, j: Integer;
begin
  tmpBuf := TRPCBuffer.Create;
  try
    First;
    for i := 0 to (Count - 1) do
    begin
      for j := 0 to (FieldCount - 1) do
      begin
        tmpBuf.WriteFieldFromBufferField(Not tmpBuf.JumpTo(j), Self, j);
      end;
      Next;
    end;
    Self.AssignBuffer(tmpBuf);
  finally
    tmpBuf.Free;
  end;
end;

procedure TRPCBuffer.IntSetFieldAsBoolean(const nIndex: Integer; const Value: Boolean);
var
  FField: PRPCField;
begin
  FField := GetRPCFields(nIndex);
  SetFieldAsBoolean(FField, Value, nIndex);
end;

procedure TRPCBuffer.IntSetFieldAsFloat(const nIndex: Integer; const Value: Double);
var
  FField: PRPCField;
begin
  FField := GetRPCFields(nIndex);
  SetFieldAsFloat(FField, Value, nIndex);
end;

procedure TRPCBuffer.IntSetFieldAsInteger(const nIndex: Integer; const Value: Integer);
var
  FField: PRPCField;
begin
  FField := GetRPCFields(nIndex);
  SetFieldAsInteger(FField, Value, nIndex);
end;

procedure TRPCBuffer.IntSetFieldAsInt64(const nIndex: Integer; const Value: Int64);
var
  FField: PRPCField;
begin
  FField := GetRPCFields(nIndex);
  SetFieldAsInt64(FField, Value, nIndex);
end;

procedure TRPCBuffer.IntSetFieldAsString(const nIndex: Integer; const Value: string);
begin
  SetFields(nIndex, Value);
end;

function TRPCBuffer.IntGetFieldAsBoolean(const nIndex: Integer): Boolean;
var
  FField: PRPCField;
begin
  FField := GetRPCFields(nIndex);
  Result := TRPCRecord.GetFieldAsBoolean(FField);
end;

function TRPCBuffer.IntGetFieldAsByte(const nIndex: Integer): Byte;
var
  FField: PRPCField;
begin
  FField := GetRPCFields(nIndex);
  Result := TRPCRecord.GetFieldAsByte(FField);
end;

function TRPCBuffer.IntGetFieldAsFloat(const nIndex: Integer): Double;
var
  FField: PRPCField;
begin
  FField := GetRPCFields(nIndex);
  Result := TRPCRecord.GetFieldAsFloat(FField);
end;

function TRPCBuffer.IntGetFieldAsInteger(const nIndex: Integer): Integer;
var
  FField: PRPCField;
begin
  FField := GetRPCFields(nIndex);
  Result := TRPCRecord.GetFieldAsInteger(FField);
end;

function TRPCBuffer.IntGetFieldAsInt64(const nIndex: Integer): Int64;
var
  FField: PRPCField;
begin
  FField := GetRPCFields(nIndex);
  Result := TRPCRecord.GetFieldAsInt64(FField);
end;

function TRPCBuffer.IntGetFieldAsString(const nIndex: Integer): string;
var
  FField: PRPCField;
begin
  FField := GetRPCFields(nIndex);
  Result := TRPCRecord.GetFieldAsString(FField);
end;

function TRPCBuffer.IntGetFieldAsWord(const nIndex: Integer): Word;
var
  FField: PRPCField;
begin
  FField := GetRPCFields(nIndex);
  Result := TRPCRecord.GetFieldAsWord(FField);
end;

function TRPCBuffer.IntGetFieldAsDateTime(const nIndex: Integer): TDateTime;
var
  FField: PRPCField;
begin
  FField := GetRPCFields(nIndex);
  Result := TRPCRecord.GetFieldAsFloat(FField);
end;

procedure TRPCBuffer.IntSetFieldAsDateTime(const nIndex: Integer; const Value: TDateTime);
var
  FField: PRPCField;
begin
  FField := GetRPCFields(nIndex);
  SetFieldAsFloat(FField, Value, nIndex);
end;

function TRPCBuffer.GetFieldName(const nIndex: Integer): string;
var
  I: Integer;
begin
  if Assigned(FFieldNames) then
  begin
    I := FindNameIndex(nIndex);
    if I >= 0 then
      Result := FFieldNames[I]
    else
      Result := '';
  end
  else
    Result := '';
end;

procedure TRPCBuffer.SetFieldName(const nIndex: Integer; const Value: string);
var
  I: Integer;
  bFound: Boolean;
  FieldInfo: TFieldNameInfo;
begin
  if not Assigned(FFieldNames) then
  begin
    FFieldNames := TFieldNames.Create;
    FFieldNames.Duplicates := dupError;
    FFieldNames.Sorted := True;
  end;
  try
    bFound := false;
    for I:=0 to FFieldNames.Count-1 do
    begin
      if TFieldNameInfo(FFieldNames.Objects[I]).Index = nIndex then
      begin
        FieldInfo := TFieldNameInfo( FFieldNames.Objects[I] );
        FFieldNames.Delete(I);
        bFound := true;
        break;
      end;
    end;
    if not bFound then
    begin
      FieldInfo := TFieldNameInfo.Create;
      FieldInfo.Index     := nIndex;
      FieldInfo.FieldType := dtNotTyped;
    end;
    FFieldNames.AddObject(Value, TFieldNameInfo(FieldInfo));
  except
    FreeAndNil(FieldInfo);
    raise;
  end;
end;

function TRPCBuffer.FindNameIndex(const Index: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  if Assigned(FFieldNames) then
  begin
    for I := 0 to FFieldNames.Count - 1 do
    begin
      if TFieldNameInfo(FFieldNames.Objects[I]).Index = Index then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

procedure TRPCBuffer.FreeDataObjects;
begin
  FreeAndNil(FDataList);
  FreeAndNil(FSortIndex);
  FreeFieldNames;
end;

procedure TRPCBuffer.FreeFieldNames;
begin
  if Assigned(FFieldNames) then
    FreeAndNil(FFieldNames);
end;

function TRPCBuffer.GetFieldIndexByName(const Name: string): Integer;
begin
  Result := IntGetFieldIndexByName(Name, true);
end;

function TRPCBuffer.GetFieldByNameAsBoolean(const Name: string): Boolean;
begin
  Result := IntGetFieldAsBoolean(GetFieldIndexByName(Name));
end;

function TRPCBuffer.GetFieldByNameAsByte(const Name: string): Byte;
begin
  Result := IntGetFieldAsByte(GetFieldIndexByName(Name));
end;

function TRPCBuffer.GetFieldByNameAsDateTime(const Name: string): TDateTime;
begin
  Result := IntGetFieldAsDateTime(GetFieldIndexByName(Name));
end;

function TRPCBuffer.GetFieldByNameAsFloat(const Name: string): Double;
begin
  Result := IntGetFieldAsFloat(GetFieldIndexByName(Name));
end;

function TRPCBuffer.GetFieldByNameAsInteger(const Name: string): Integer;
begin
  Result := IntGetFieldAsInteger(GetFieldIndexByName(Name));
end;

function TRPCBuffer.GetFieldByNameAsInt64(const Name: string): Int64;
begin
  Result := IntGetFieldAsInt64(GetFieldIndexByName(Name));
end;

function TRPCBuffer.GetFieldByNameAsString(const Name: string): string;
begin
  Result := IntGetFieldAsString(GetFieldIndexByName(Name));
end;

function TRPCBuffer.GetFieldByNameAsWord(const Name: string): Word;
begin
  Result := IntGetFieldAsWord(GetFieldIndexByName(Name));
end;

procedure TRPCBuffer.SetFieldByNameAsBoolean(const Name: string; const Value: Boolean);
begin
  IntSetFieldAsBoolean(GetFieldIndexByName(Name), Value);
end;

procedure TRPCBuffer.SetFieldByNameAsByte(const Name: string; const Value: Byte);
begin
  IntSetFieldAsByte(GetFieldIndexByName(Name), Value);
end;

procedure TRPCBuffer.SetFieldByNameAsDateTime(const Name: string; const Value: TDateTime);
begin
  IntSetFieldAsDateTime(GetFieldIndexByName(Name), Value);
end;

procedure TRPCBuffer.SetFieldByNameAsFloat(const Name: string; const Value: Double);
begin
  IntSetFieldAsFloat(GetFieldIndexByName(Name), Value);
end;

procedure TRPCBuffer.SetFieldByNameAsInteger(const Name: string; const Value: Integer);
begin
  IntSetFieldAsInteger(GetFieldIndexByName(Name), Value);
end;

procedure TRPCBuffer.SetFieldByNameAsInt64(const Name: string; const Value: Int64);
begin
  IntSetFieldAsInt64(GetFieldIndexByName(Name), Value);
end;

procedure TRPCBuffer.SetFieldByNameAsString(const Name, Value: string);
begin
  IntSetFieldAsString(GetFieldIndexByName(Name), Value);
end;

procedure TRPCBuffer.SetFieldByNameAsWord(const Name: string; const Value: Word);
begin
  IntSetFieldAsWord(GetFieldIndexByName(Name), Value);
end;
procedure TRPCBuffer.FreeRPCBufferBookMark(
  var ABookMark: TRPCBufferBookMark);
begin
  Dispose(ABookMark);
end;

function TRPCBuffer.GetRPCBufferBookMark: TRPCBufferBookMark;
begin
  New(Result);
  Result^.CurrentIndex := FSortIndex.IndexOfSort(FCurrentSortIndex);
  if Assigned(FCurrentSortIndex) then
    Result^.RecordPointer := FCurrentSortIndex.CurrentItem
  else Result^.RecordPointer := FRecordPointer;  
end;

procedure TRPCBuffer.GotoRPCBufferBookMark(
  const ABookMark: TRPCBufferBookMark);
begin
  if ABookMark^.CurrentIndex >= 0 then
  begin
    FCurrentSortIndex := FSortIndex.GetSortItem(ABookMark^.CurrentIndex);
    FRecordPointer := FCurrentSortIndex.RecordPointer[ABookMark^.RecordPointer];
    FCurrentSortIndex.CurrentItem := ABookMark^.RecordPointer;
  end else
    JumpTo(ABookMark^.RecordPointer)
end;

function TRPCBuffer.GetFieldByNameFieldType(const Name: string): TRPCFieldType;
begin
  Result := FieldType[GetFieldIndexByName(Name)];
end;

procedure TRPCBuffer.SetFieldByNameFieldType(const Name: string;
  const Value: TRPCFieldType);
begin
  FieldType[GetFieldIndexByName(Name)] := Value;
end;

function TRPCBuffer.GetCardinalField(const nIndex: Integer): Cardinal;
begin
{$R-}
  Result := GetIntegerField(nIndex);
{$R+}
end;

function TRPCBuffer.GetFieldByNameAsCardinal(const Name: string): Cardinal;
begin
{$R-}
  Result := GetFieldByNameAsInteger(Name);
{$R+}
end;

function TRPCBuffer.IntGetFieldAsCardinal(const nIndex: Integer): Cardinal;
begin
{$R-}
  Result := IntGetFieldAsInteger(nIndex);
{$R+}
end;

procedure TRPCBuffer.IntSetFieldAsCardinal(const nIndex: Integer;
  const Value: Cardinal);
begin
{$R-}
  IntSetFieldAsInteger(nIndex, Value);
{$R+}
end;

procedure TRPCBuffer.SetFieldByNameAsCardinal(const Name: string;
  const Value: Cardinal);
begin
{$R-}
  SetFieldByNameAsInteger(Name, Value);
{$R+}
end;

procedure TRPCBuffer.LoadFromXMLStream(Stream: TStream);
var
  I: Integer;
  FXMLDoc: TNativeXml;
begin
  FXMLDoc := TNativeXml.CreateName('buffer');
  try
    FXMLDoc.FloatSignificantDigits := 9;
    FXMLDoc.LoadFromStream(Stream);
    if Assigned(FXMLDoc.Root) then
      for I := 0 to FXMLDoc.Root.NodeCount - 1 do
        AppendRPCRecord.LoadFromXMLDOMNode(FXMLDoc.Root.Nodes[I])
    else raise Exception.Create('Invalid XML Structure buffer element not found');
  finally
    FXMLDoc.Free;
  end;
end;

procedure TRPCBuffer.LoadFromStream(Stream: TStream;
  const BufferFormat: TApBufferFormat);
begin
  LoadFromStream(Stream, 0, BufferFormat);
end;

procedure TRPCBuffer.WriteFieldFromBufferField(const NewRec: Boolean;
  const From: TRPCBuffer; const FieldIndex: Integer);
var
  NewField: PRPCField;
  AList: TList;
begin
  if NewRec then
    AppendRPCRecord;
  NewField := AppendRPCField(From.FieldType[FieldIndex]);

  AList := nil;
  FSortIndex.GenerateNeededUpdates(FieldIndex, GetCurrentRecord, AList);
  try
    SetFieldFromField(NewField, From.GetRPCFields(FieldIndex));
  finally
    FSortIndex.UpdateNeededUpdates(GetCurrentRecord, AList);
  end;
end;

procedure TRPCBuffer.SaveToXMLStream(Stream: TStream);
var
  FXML: TNativeXml;
  I: Integer;
begin
  FXML := TNativeXml.CreateName('buffer');
  try
    FXML.FloatSignificantDigits := 9;
    for I := 0 to Count - 1 do
      Records[I].SaveToXML(FXML);
    FXML.SaveToStream(Stream);  
  finally
    FXML.Free;
  end;
end;

function TRPCBuffer.IntGetFieldIndexByName(const Name: string; const ARaiseException: boolean): Integer;
var
  I: Integer;
begin
  Result := -1;
  if Assigned(FFieldNames) then
  begin
    if (FFieldNames.Find(Name, I)) then
      Result := TFieldNameInfo(FFieldNames.Objects[I]).Index;
  end;
  if (Result = -1) and ARaiseException then
    raise Exception.CreateFmt('Field %s not found in field names defined to buffer', [Name]);
end;

function TRPCBuffer.FindFieldIndexByName(const Name: String): Integer;
begin
  Result := IntGetFieldIndexByName(Name, false);
end;

function TRPCBuffer.GetStreamField(const FieldIndex: Integer): TStream;
begin
  Result := TMemoryStream.Create;
  GetStreamField(FieldIndex, Result);
end;

procedure TRPCBuffer.SetFieldNameAndType(const AIndex: Integer; const Value: string; AFieldType: TRPCFieldType);
var
  idx: Integer;
begin
  SetFieldName(AIndex, Value);
  FFieldNames.Find(value, idx);
  TFieldNameInfo(FFieldNames.Objects[idx]).FieldType := AFieldType;
end;

procedure TRPCBuffer.Append;
var
  I, idx: integer;
  FieldInfoType : TRPCFieldType;
begin
  if not Assigned(FFieldNames) or (FFieldNames.Count = 0) then
    raise Exception.Create('Append method need FieldNames definion !!!');
  // Se forem tipados gera os campos já com o Tipo
  for I := 0 to FFieldNames.Count - 1 do
  begin
    idx := FindNameIndex(I);
    FieldInfoType := TFieldNameInfo(FFieldNames.Objects[idx]).FieldType;
    case FieldInfoType of
      // String
      dtString  : WriteTypedFields(I = 0, [''], [FieldInfoType]);
      // Integers
      dtByte,
      dtWord,
      dtInteger,
      dtLargeint: WriteTypedFields(I = 0, [0], [FieldInfoType]);
      // Dates
      dtDateTime,
      dtTime,
      dtDate   : WriteTypedFields(I = 0, [0.0],   [FieldInfoType]);
      // Boolean
      dtBoolean: WriteTypedFields(I = 0, [False], [FieldInfoType]);
      // Float
      dtExtended,
      dtCurrency: WriteTypedFields(I = 0, [0.0], [FieldInfoType]);
    else // dtPChar, dtNotTyped, dtObject, dtVariant dtBlob //???
      WriteTypedFields(I = 0, [''], [dtNotTyped]);
    end;
  end;
end;

procedure TRPCBuffer.AddFieldNameAndType(const Value: string; AFieldType: TRPCFieldType);
begin
  SetFieldName(FFieldNames.Count, Value);
  TFieldNameInfo(FFieldNames.Objects[FFieldNames.Count - 1]).FieldType := AFieldType;
end;

procedure TRPCBuffer.WriteCardinalField(const NewRec: boolean;
  const Value: Cardinal);
begin
{$R-}
  WriteTypedFields(NewRec, [Value], [dtInteger]);
{$R+}
end;

function TRPCBuffer.AsString: string;
var
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  try
    GetBufferAsStrings(StrList);
    Result := StrList.Text;
  finally
    FreeAndNil(StrList);
  end;
end;

{ TRPCBufferSort }

procedure TRPCBufferSort.AddRecord(const ARecord: TRPCRecord);
begin
  FDataList.Add(ARecord);
end;

function TRPCBufferSort.CompareKeys(const Item1, Item2: TRPCRecord; const ACaseSensitive: Boolean): Integer;
var
  I: Byte;
  FDummy: Double;
  FStrDummy: string;
begin
  Result := 0;
  for I := low(FKeys) to high(FKeys) do
  begin
    if (FKeys[I] > (Item1.Count - 1)) then
    begin
      if (FKeys[I] > (Item2.Count - 1)) then
      begin
        Result := 0;
        continue;
      end
      else
      begin
        Result := -1;
        Break;
      end;
    end
    else if (FKeys[I] > (Item2.Count - 1)) then
    begin
      Result := 1;
      Break;
    end;
    case Item1.RPCFields[FKeys[I]]^.FieldType of
      dtInteger: Result := CompareValue(PRPCIntegerField(Item1.RPCFields[FKeys[I]])^.Value, PRPCIntegerField(Item2.RPCFields[FKeys[I]])^.Value);
      dtLargeint: Result := CompareValue(PRPCInt64Field(Item1.RPCFields[FKeys[I]])^.Value, PRPCInt64Field(Item2.RPCFields[FKeys[I]])^.Value);
      dtDateTime,
        dtDate,
        dtTime:
        begin
          FDummy := (PRPCDateTimeField(Item1.RPCFields[FKeys[I]])^.Value - PRPCDateTimeField(Item2.RPCFields[FKeys[I]])^.Value);
          if FDummy = 0 then
            Result := 0
          else if FDummy > 0 then
            Result := 1
          else
            Result := -1;
        end;
      dtBoolean: Result := Integer(PRPCBooleanField(Item1.RPCFields[FKeys[I]])^.Value) - Integer(PRPCBooleanField(Item2.RPCFields[FKeys[I]])^.Value);
      dtExtended,
        dtCurrency:
        begin
          FDummy := (PRPCFloatField(Item1.RPCFields[FKeys[I]])^.Value - PRPCFloatField(Item2.RPCFields[FKeys[I]])^.Value);
          if FDummy = 0 then
            Result := 0
          else if FDummy > 0 then
            Result := 1
          else
            Result := -1;
        end;
      dtWord: Result := PRPCWordField(Item1.RPCFields[FKeys[I]])^.Value - PRPCWordField(Item2.RPCFields[FKeys[I]])^.Value;
      dtByte: Result := PRPCByteField(Item1.RPCFields[FKeys[I]])^.Value - PRPCByteField(Item2.RPCFields[FKeys[I]])^.Value;
      dtBlob: raise Exception.Create('Blob Field cannot be used as Key field');
    else
      begin
        FStrDummy := Item2.GetFieldAsString(Item2.RPCFields[FKeys[I]]);
        if ACaseSensitive then
          Result := CompareStr(PRPCStringField(Item1.RPCFields[FKeys[I]])^.Value, FStrDummy)
        else
          Result := CompareText(PRPCStringField(Item1.RPCFields[FKeys[I]])^.Value, FStrDummy);
      end;
    end;
    if Result <> 0 then
      Break;
  end;
  if not FAscending then
    Result := Result * (-1);
end;

function TRPCBufferSort.CompareKeys(const Item: TRPCRecord;
  const AKeyValues: array of Variant; const ACaseSensitive: Boolean): Integer;
var
  I: byte;
  FDummy: Double;
  FDummy64: Int64;
begin
  Result := 0;
  for I := low(AKeyValues) to high(AKeyValues) do
  begin
    if (FKeys[I] > (Item.Count - 1)) then
    begin
      Result := -1;
      Break;
    end;
    case Item.RPCFields[FKeys[I]]^.FieldType of
      dtInteger: Result := PRPCIntegerField(Item.RPCFields[FKeys[I]])^.Value - AKeyValues[I];
      dtLargeint:
        begin
          FDummy64 := PRPCInt64Field(Item.RPCFields[FKeys[I]])^.Value - AKeyValues[I];
          if FDummy64 = 0 then
            Result := 0
          else if FDummy64 > 0 then
            Result := 1
          else
            Result := -1;
        end;
      dtDateTime,
        dtDate,
        dtTime:
        begin
          FDummy := (PRPCDateTimeField(Item.RPCFields[FKeys[I]])^.Value - AKeyValues[I]);
          if FDummy = 0 then
            Result := 0
          else if FDummy > 0 then
            Result := 1
          else
            Result := -1;
        end;
      dtBoolean: Result := Integer(PRPCBooleanField(Item.RPCFields[FKeys[I]])^.Value) - Integer(AKeyValues[I]);
      dtExtended,
        dtCurrency:
        begin
          FDummy := (PRPCFloatField(Item.RPCFields[FKeys[I]])^.Value - AKeyValues[I]);
          if FDummy = 0 then
            Result := 0
          else if FDummy > 0 then
            Result := 1
          else
            Result := -1;
        end;
      dtWord: Result := PRPCWordField(Item.RPCFields[FKeys[I]])^.Value - AKeyValues[I];
      dtByte: Result := PRPCByteField(Item.RPCFields[FKeys[I]])^.Value - AKeyValues[I];
      dtBlob: raise Exception.Create('Blob Field cannot be used as Key field');
    else
      begin
        if ACaseSensitive then
          Result := CompareStr(PRPCStringField(Item.RPCFields[FKeys[I]])^.Value, AKeyValues[I])
        else
          Result := CompareText(PRPCStringField(Item.RPCFields[FKeys[I]])^.Value, AKeyValues[I]);
      end;
    end;
    if Result <> 0 then
      Break;
  end;
  if not FAscending then
    Result := Result * (-1);
end;

function TRPCBufferSort.CompareKeys(const Item: TRPCRecord;
  const AKeyValues: array of string; const ACaseSensitive: Boolean): Integer;
var
  I: byte;
  FDummy: string;
begin
  Result := 0;
  for I := low(AKeyValues) to high(AKeyValues) do
  begin
    if (FKeys[I] > (Item.Count - 1)) then
    begin
      Result := -1;
      Break;
    end;
    case Item.RPCFields[FKeys[I]]^.FieldType of
      dtInteger: FDummy := IntToStr(PRPCIntegerField(Item.RPCFields[FKeys[I]])^.Value);
      dtLargeint: FDummy := IntToStr(PRPCInt64Field(Item.RPCFields[FKeys[I]])^.Value);
      dtDateTime,
        dtDate,
        dtTime: FDummy := FloatToStr(PRPCDateTimeField(Item.RPCFields[FKeys[I]])^.Value);
      dtBoolean: FDummy := IfThen(PRPCBooleanField(Item.RPCFields[FKeys[I]])^.Value, '1', '0');
      dtExtended,
        dtCurrency: FDummy := FloatToStr(PRPCFloatField(Item.RPCFields[FKeys[I]])^.Value);
      dtWord: FDummy := IntToStr(PRPCWordField(Item.RPCFields[FKeys[I]])^.Value);
      dtByte: FDummy := IntToStr(PRPCByteField(Item.RPCFields[FKeys[I]])^.Value);
      dtBlob: raise Exception.Create('Blob Field cannot be used as Key field');
    else
      FDummy := PRPCStringField(Item.RPCFields[FKeys[I]])^.Value;
    end;
    if ACaseSensitive then
      Result := CompareStr(FDummy, AKeyValues[I])
    else
      Result := CompareText(FDummy, AKeyValues[I]);
    if Result <> 0 then
      Break;
  end;
  if not FAscending then
    Result := Result * (-1);
end;

procedure TRPCBufferSort.ClearDataList;
begin
  FDataList.Clear;
end;

procedure TRPCBufferSort.Assign(ASource: TRPCBufferSort);
var
  i: integer;
begin
  ClearDataList;
  for i := 0 to ASource.Count - 1 do
    AddRecord(ASource.GetRPCRecord(i));
end;


function TRPCBufferSort.CompareKeys(const Item: TRPCRecord;
  const AKeyValues: array of Integer; const ACaseSensitive: Boolean): Integer;
var
  I: Byte;
  FDummy: Double;
  FDummy64: Int64;
begin
  Result := 0;
  for I := low(AKeyValues) to high(AKeyValues) do
  begin
    if (FKeys[I] > (Item.Count - 1)) then
    begin
      Result := -1;
      Break;
    end;
    case Item.RPCFields[FKeys[I]]^.FieldType of
      dtInteger: Result := PRPCIntegerField(Item.RPCFields[FKeys[I]])^.Value - AKeyValues[I];
      dtLargeint:
        begin
          FDummy64 := PRPCInt64Field(Item.RPCFields[FKeys[I]])^.Value - AKeyValues[I];
          if FDummy64 = 0 then
            Result := 0
          else if FDummy64 > 0 then
            Result := 1
          else
            Result := -1;
        end;
      dtDateTime,
        dtDate,
        dtTime:
        begin
          FDummy := (PRPCDateTimeField(Item.RPCFields[FKeys[I]])^.Value - AKeyValues[I]);
          if FDummy = 0 then
            Result := 0
          else if FDummy > 0 then
            Result := 1
          else
            Result := -1;
        end;
      dtBoolean: Result := Integer(PRPCBooleanField(Item.RPCFields[FKeys[I]])^.Value) - Integer(AKeyValues[I]);
      dtExtended,
        dtCurrency:
        begin
          FDummy := (PRPCFloatField(Item.RPCFields[FKeys[I]])^.Value - AKeyValues[I]);
          if FDummy = 0 then
            Result := 0
          else if FDummy > 0 then
            Result := 1
          else
            Result := -1;
        end;
      dtWord: Result := PRPCWordField(Item.RPCFields[FKeys[I]])^.Value - AKeyValues[I];
      dtByte: Result := PRPCByteField(Item.RPCFields[FKeys[I]])^.Value - AKeyValues[I];
      dtBlob: raise Exception.Create('Blob Field cannot be used as Key field');
    else
      begin
        if ACaseSensitive then
          Result := CompareStr(PRPCStringField(Item.RPCFields[FKeys[I]])^.Value, IntToStr(AKeyValues[I]))
        else
          Result := CompareText(PRPCStringField(Item.RPCFields[FKeys[I]])^.Value, IntToStr(AKeyValues[I]));
      end;
    end;
    if Result <> 0 then
      Break;
  end;
  if not FAscending then
    Result := Result * (-1);
end;

constructor TRPCBufferSort.Create(const ABuffer: TRPCBuffer; const AKeys: array of word; const Ascending, ACaseSensitive: Boolean);
var
  I: byte;
begin
  inherited Create;
  FCurrentItem := 0;
  SetLength(FKeys, Length(AKeys));
  for I := 0 to High(AKeys) do
    FKeys[I] := AKeys[I];
  FBuffer := ABuffer;
  FAscending := Ascending;
  FCaseSensitive := ACaseSensitive;
  FDataList := TList.Create;
end;

function TRPCBufferSort.FindKey(const AKeyValues: array of Variant; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive);
    if C < 0 then
      L := I + 1
    else
    if C > 0 then
      H := I - 1
    else begin
      if FAscending then
      begin
        if I > 0 then
        begin
          repeat
            Dec(I);
          until (I = 0) or (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0);
          // Se chegou ao topo, e não é igual o proximo é o item
          if (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0) and (I < (Count - 1)) then
            Inc(I);
        end;
      end
      else
      begin
        if I < (Count - 1) then
        begin
          repeat
            Inc(I);
          until (I = (Count - 1)) or (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0);
          if (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0) and (I > 0) then
            Dec(I);
        end;
      end;
      Result := True;
      L := I;
      break;
    end;
  end;
  Index := L;
end;

function TRPCBufferSort.FindKey(const AKeyValues: array of string; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        if FAscending then
        begin
          if I > 0 then
          begin
            repeat
              Dec(I);
            until (I = 0) or (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0);
            // Se chegou ao topo, e não é igual o proximo é o item
            if (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0) and (I < (Count - 1)) then
              Inc(I);
          end;
        end
        else
        begin
          if I < (Count - 1) then
          begin
            repeat
              Inc(I);
            until (I = (Count - 1)) or (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0);
            if (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0) and (I > 0) then
             Dec(I);
          end;
        end;
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

procedure TRPCBufferSort.DeleteRecord(const ARecord: TRPCRecord);
var
  AIndex: Integer;
begin
  if FindKey(ARecord, True, AIndex) then
    FDataList.Delete(AIndex);
end;

destructor TRPCBufferSort.Destroy;
begin
  if Assigned(FDataList) then
    FreeAndNil(FDataList);
  inherited;
end;

function TRPCBufferSort.FindKey(const AKeyValues: array of Integer; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        if FAscending then
        begin
          if I > 0 then
          begin
            repeat
              Dec(I);
            until (I = 0) or (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0);
            // Se chegou ao topo, e não é igual o proximo é o item
            if (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0) and (I < (Count - 1)) then
              Inc(I);
          end;
        end
        else
        begin
          if I < (Count - 1) then
          begin
            repeat
              Inc(I);
            until (I = (Count - 1)) or (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0);
            if (CompareKeys(TRPCRecord(FDataList.Items[I]), AKeyValues, FCaseSensitive) <> 0) and (I > 0) then
             Dec(I);
          end;
        end;
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function TRPCBufferSort.GetCount: integer;
begin
  Result := FDataList.Count;
end;

function TRPCBufferSort.GetCurrentRecordPointer: Integer;
begin
  Result := RecordPointer[FCurrentItem];
end;

function TRPCBufferSort.GetRecordPointer(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index <= (Count -1)) then
    Result := FBuffer.FDataList.IndexOf(FDataList.Items[Index])
  else Result := -1;
end;

function TRPCBufferSort.GetRPCRecord(const Index: Integer): TRPCRecord;
begin
  Result := TRPCRecord(FDataList.Items[Index]);
end;

procedure TRPCBufferSort.QuickSort(L, R: Integer);
var
  I, J: Integer;
  P, T: TRPCRecord;
begin
  repeat
    I := L;
    J := R;
    P := RPCRecord[(L + R) shr 1];
    repeat
      while CompareKeys(RPCRecord[I], P, FCaseSensitive) < 0 do
        Inc(I);
      while CompareKeys(RPCRecord[J], P, FCaseSensitive) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := RPCRecord[I];
        RPCRecord[I] := RPCRecord[J];
        RPCRecord[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TRPCBufferSort.SetRPCRecord(const Index: Integer;
  const Value: TRPCRecord);
begin
  FDataList.Items[Index] := Value;
end;

procedure TRPCBufferSort.Sort;
var
  I: Integer;
begin
  ClearDataList;
  if FBuffer.HasData then
  begin
    for I := 0 to FBuffer.Count - 1 do
      FDataList.Add(FBuffer.Records[I]);
    QuickSort(0, Count - 1);
  end;
end;

function TRPCBufferSort.FindKey(const ARecord: TRPCRecord; const Exact: Boolean; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareKeys(TRPCRecord(FDataList.Items[I]), ARecord, FCaseSensitive);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        if FAscending then
        begin
          if I > 0 then
          begin
            repeat
              Dec(I);
            until (I = 0) or (CompareKeys(TRPCRecord(FDataList.Items[I]), ARecord, FCaseSensitive) <> 0);
            // Se chegou ao topo, e não é igual o proximo é o item
            if (CompareKeys(TRPCRecord(FDataList.Items[I]), ARecord, FCaseSensitive) <> 0) and (I < (Count - 1)) then
              Inc(I);
          end;
        end
        else
        begin
          if I < (Count - 1) then
          begin
            repeat
              Inc(I);
            until (I = (Count - 1)) or (CompareKeys(TRPCRecord(FDataList.Items[I]), ARecord, FCaseSensitive) <> 0);
            if (CompareKeys(TRPCRecord(FDataList.Items[I]), ARecord, FCaseSensitive) <> 0) and (I > 0) then
              Dec(I);
          end;
        end;
        Result := True;
        L := I;
      end;
    end;
  end;
  if Result and Exact then
  begin
    I := L;
    while (I >= 0) and (I < Count) and (CompareKeys(TRPCRecord(FDataList.Items[I]), ARecord, FCaseSensitive) = 0) and (TRPCRecord(FDataList.Items[I]) <> ARecord) do
    begin
      if FAscending then
        Inc(I)
      else Dec(I);
    end;
    if (I >= 0) and (I < Count) then
      Result := (TRPCRecord(FDataList.Items[I]) = ARecord)
    else Result := False;
    L := I;
  end;
  Index := L;
end;

function TRPCBufferSort.FieldIndexIsKey(const AFieldIndex: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := low(FKeys) to high(FKeys) do
  begin
    if FKeys[I] = AFieldIndex then
    begin
      Result := True;
      break;
    end;
  end;
end;

procedure TRPCBufferSort.InsertIndexed(const ARecord: TRPCRecord);
var
  Index: Integer;
begin
  FindKey(ARecord, false, Index);
  FDataList.Insert(Index, ARecord);
end;

{ TRPCBufferSortList }

function TRPCBufferSortList.AddSort(const ASortName: string; const AKeys: array of Word; const Ascending, ACaseSensitive: Boolean): Integer;
begin
  Result := FSortList.IndexOf(AnsiLowerCase(ASortName));
  if Result <> -1 then
    TRPCBufferSort(FSortList.Objects[Result]).ClearDataList;
  if Result = -1 then
    Result := FSortList.AddObject(AnsiLowerCase(ASortName), TRPCBufferSort.Create(FBuffer, AKeys, Ascending, ACaseSensitive));
  TRPCBufferSort(FSortList.Objects[Result]).Sort;
end;

procedure TRPCBufferSortList.Assign(ARPCBufferSortList: TRPCBufferSortList);
var
  i: integer;
  Src, Dst: TRPCBufferSort;
begin
  FSortList.Clear;
  for i := 0 to ARPCBufferSortList.Count - 1 do
  begin
    Src := TRPCBufferSort(ARPCBufferSortList.Items[i]);
    Dst := TRPCBufferSort.Create(FBuffer, Src.FKeys, Src.FAscending, Src.FCaseSensitive);
    Dst.Assign(Src);
    FSortList.AddObject(ARPCBufferSortList.FSortList[i], Dst);
  end;
end;

procedure TRPCBufferSortList.ClearList;
var
  x: Integer;
begin
  for x := 0 to FSortList.Count - 1 do
    if Assigned(FSortList.Objects[x]) then
      TRPCBufferSort(FSortList.Objects[x]).Free;
  FSortList.Clear;
end;

constructor TRPCBufferSortList.Create(const ABuffer: TRPCBuffer);
begin
  FSortList := TStringList.Create;
  FBuffer := ABuffer;
end;

procedure TRPCBufferSortList.DeleteRecord(const ARecord: TRPCRecord);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    GetSortItem(I).DeleteRecord(ARecord);
end;

function TRPCBufferSortList.DeleteSort(ASortIndex: Integer): Boolean;
begin
  Result := False;
  if (ASortIndex < 0) or (ASortIndex > FSortList.Count) then
    Exit;
  TRPCBufferSort(FSortList.Objects[ASortIndex]).Free;
  FSortList.Delete(ASortIndex);
end;

destructor TRPCBufferSortList.Destroy;
begin
  ClearList;
  FSortList.Free;
  inherited;
end;

procedure TRPCBufferSortList.GenerateNeededUpdates(const AFieldIndex: Integer; const ARecord: TRPCRecord; var AList: TList);
var
  I: Integer;
  FSortItem: TRPCBufferSort;
begin
  for I := 0 to FSortList.Count - 1 do
  begin
    FSortItem := TRPCBufferSort(FSortList.Objects[I]);
    if FSortItem.FieldIndexIsKey(AFieldIndex) then
    begin
      if not Assigned(AList) then
        AList := TList.Create;
      FSortItem.DeleteRecord(ARecord);
      AList.Add(FSortItem);
    end;
  end;
end;

function TRPCBufferSortList.GetCount: Integer;
begin
  Result := FSortList.Count;
end;

function TRPCBufferSortList.GetSortByName(const ASortName: string): TRPCBufferSort;
var
  x: Integer;
begin
  x := FSortList.IndexOf(AnsiLowerCase(ASortName));
  if x <> -1 then
    Result := TRPCBufferSort(FSortList.Objects[x])
  else
    Result := nil;
end;

function TRPCBufferSortList.GetSortIndex(const ASortName: string): Integer;
begin
  Result := FSortList.IndexOf(AnsiLowerCase(ASortName));
end;

function TRPCBufferSortList.GetSortItem(AIndex: Integer): TRPCBufferSort;
begin
  if (AIndex + 1) > FSortList.Count then
    Result := nil
  else
    Result := TRPCBufferSort(FSortList.Objects[AIndex]);
end;

function TRPCBufferSortList.IndexOfSort(
  const ASort: TRPCBufferSort): SmallInt;
begin
  Result := FSortList.IndexOfObject(ASort);
end;

procedure TRPCBufferSortList.UpdateNeededUpdates(const ARecord: TRPCRecord; var AList: TList);
var
  I: Integer;
begin
  if Assigned(AList) then
  begin
    try
      for I := 0 to AList.Count - 1 do
        TRPCBufferSort(AList[I]).InsertIndexed(ARecord);
    finally
      FreeAndNil(AList);
    end;
  end;
end;

{ TRPCRecord }

function TRPCRecord.AddField(const AFieldType: TRPCFieldType; const FieldIndex: Integer): PRPCField;
var
  FRPCIntegerField: PRPCIntegerField;
  FRPCInt64Field: PRPCInt64Field;
  FRPCFloatField: PRPCFloatField;
  FRPCByteField: PRPCByteField;
  FRPCBooleanField: PRPCBooleanField;
  FRPCStringField: PRPCStringField;
  FRPCStreamField: PRPCStreamField;
  FRPCWordField: PRPCWordField;
  FRPCDateTimeField: PRPCDateTimeField;
begin
  case AFieldType of
    dtBlob:
      begin
        New(FRPCStreamField);
        Result := PRPCField(FRPCStreamField);
      end;
    dtInteger:
      begin
        New(FRPCIntegerField);
        Result := PRPCField(FRPCIntegerField);
      end;
    dtLargeint:
      begin
        New(FRPCInt64Field);
        Result := PRPCField(FRPCInt64Field);
      end;
    dtDateTime,
      dtTime,
      dtDate:
      begin
        New(FRPCDateTimeField);
        Result := PRPCField(FRPCDateTimeField);
      end;
    dtExtended,
      dtCurrency:
      begin
        New(FRPCFloatField);
        Result := PRPCField(FRPCFloatField);
      end;
    dtBoolean:
      begin
        New(FRPCBooleanField);
        Result := PRPCField(FRPCBooleanField);
      end;
    dtWord:
      begin
        New(FRPCWordField);
        Result := PRPCField(FRPCWordField);
      end;
    dtByte:
      begin
        New(FRPCByteField);
        Result := PRPCField(FRPCByteField);
      end;
  else
    begin
      New(FRPCStringField);
      Result := PRPCField(FRPCStringField);
    end;
  end;
  Result.FieldType := AFieldType;
  if FieldIndex >= 0 then
    Insert(FieldIndex, Result)
  else
    Add(Result);
  if Result.FieldType = dtBlob then
    PRPCStreamField(Result)^.Value := nil;
end;

procedure TRPCRecord.AdjustFieldTypeAndSize(var Field: PRPCField;
  const AFieldType: TRPCFieldType; const FieldIndex, FieldSize: Integer);
begin
  if (Field^.FieldType <> AFieldType) then
  begin
    DeleteField(FieldIndex);
    Field := AddField(AFieldType, FieldIndex);
  end;
  if (Field^.FieldType = dtBlob) and (not Assigned(PRPCStreamField(Field)^.Value)) then
    PRPCStreamField(Field)^.Value := TMemoryStream.Create;
end;

procedure TRPCRecord.Clear;
begin
  while Count > 0 do
    DeleteField(Count - 1);
  inherited;
end;

procedure TRPCRecord.DeleteField(Index: Integer);
var
  FRPCIntegerField: PRPCIntegerField;
  FRPCInt64Field: PRPCInt64Field;
  FRPCFloatField: PRPCFloatField;
  FRPCByteField: PRPCByteField;
  FRPCBooleanField: PRPCBooleanField;
  FRPCStringField: PRPCStringField;
  FRPCStreamField: PRPCStreamField;
  FRPCWordField: PRPCWordField;
  FRPCDateTimeField: PRPCDateTimeField;
begin
  case RPCFields[Index]^.FieldType of
    dtBlob:
      begin
        FRPCStreamField := PRPCStreamField(RPCFields[Index]);
        if Assigned(FRPCStreamField.Value) then
          FRPCStreamField.Value.Free;
        Dispose(FRPCStreamField);
      end;
    dtInteger:
      begin
        FRPCIntegerField := PRPCIntegerField(RPCFields[Index]);
        Dispose(FRPCIntegerField);
      end;
    dtLargeint:
      begin
        FRPCInt64Field := PRPCInt64Field(RPCFields[Index]);
        Dispose(FRPCInt64Field);
      end;
    dtDateTime,
      dtTime,
      dtDate:
      begin
        FRPCDateTimeField := PRPCDateTimeField(RPCFields[Index]);
        Dispose(FRPCDateTimeField);
      end;
    dtExtended,
      dtCurrency:
      begin
        FRPCFloatField := PRPCFloatField(RPCFields[Index]);
        Dispose(FRPCFloatField);
      end;
    dtBoolean:
      begin
        FRPCBooleanField := PRPCBooleanField(RPCFields[Index]);
        Dispose(FRPCBooleanField);
      end;
    dtWord:
      begin
        FRPCWordField := PRPCWordField(RPCFields[Index]);
        Dispose(FRPCWordField);
      end;
    dtByte:
      begin
        FRPCByteField := PRPCByteField(RPCFields[Index]);
        Dispose(FRPCByteField);
      end;
  else
    begin
      FRPCStringField := PRPCStringField(RPCFields[Index]);
      Dispose(FRPCStringField);
    end;
  end;
  Delete(Index);
end;

destructor TRPCRecord.Destroy;
begin
  Clear;
  inherited;
end;

class function TRPCRecord.GetFieldAsBoolean(const Field: PRPCField): Boolean;
begin
  case Field^.FieldType of
    dtNotTyped, dtString: Result := GetFieldAsString(Field) = '1';
    dtBlob: raise Exception.Create('Conversion BLOB to Boolean not supported');
    dtInteger: Result := GetFieldAsInteger(Field) = 1;
    dtLargeint: Result := GetFieldAsInt64(Field) = 1;
    dtDateTime,
      dtExtended,
      dtCurrency,
      dtTime,
      dtDate: Result := GetFieldAsFloat(Field) = 1;
    dtBoolean: Result := PRPCBooleanField(Field)^.Value;
    dtByte: Result := GetFieldAsByte(Field) = 1;
    dtWord: Result := GetFieldAsWord(Field) = 1;
  else
    Result := False;
  end;
end;

class function TRPCRecord.GetFieldAsByte(const Field: PRPCField): Byte;
begin
  case Field^.FieldType of
    dtNotTyped, dtString: Result := StrToInt(GetFieldAsString(Field));
    dtBlob: raise Exception.Create('Conversion BLOB to Byte not supported');
    dtInteger: Result := GetFieldAsInteger(Field);
    dtLargeint: Result := GetFieldAsInt64(Field);
    dtDateTime,
      dtExtended,
      dtCurrency,
      dtTime,
      dtDate: Result := Trunc(GetFieldAsFloat(Field));
    dtBoolean: Result := IfThen(GetFieldAsBoolean(Field), 1, 0);
    dtByte: Result := PRPCByteField(Field)^.Value;
    dtWord: Result := GetFieldAsWord(Field);
  else
    Result := 0;
  end;
end;

class function TRPCRecord.GetFieldAsFloat(const Field: PRPCField): Double;
begin
  case Field^.FieldType of
    dtNotTyped, dtString: Result := StrToFloat(GetFieldAsString(Field));
    dtBlob: raise Exception.Create('Conversion BLOB to Float not supported');
    dtInteger: Result := GetFieldAsInteger(Field);
    dtLargeint: Result := GetFieldAsInt64(Field);
    dtExtended,
      dtCurrency: Result := PRPCFloatField(Field)^.Value;
    dtDateTime,
      dtTime,
      dtDate: Result := PRPCDateTimeField(Field)^.Value;
    dtBoolean: Result := IfThen(GetFieldAsBoolean(Field), 1, 0);
    dtByte: Result := GetFieldAsByte(Field);
    dtWord: Result := GetFieldAsWord(Field);
  else
    Result := 0;
  end;
end;

class function TRPCRecord.GetFieldAsInteger(const Field: PRPCField): Integer;
begin
  case Field^.FieldType of
    dtNotTyped, dtString: Result := StrToInt(GetFieldAsString(Field));
    dtBlob: raise Exception.Create('Conversion BLOB to Integer not supported');
    dtInteger: Result := PRPCIntegerField(Field).Value;
    dtLargeint: Result := PRPCInt64Field(Field).Value;
    dtDateTime,
      dtExtended,
      dtCurrency,
      dtTime,
      dtDate: Result := Trunc(GetFieldAsFloat(Field));
    dtBoolean: Result := IfThen(GetFieldAsBoolean(Field), 1, 0);
    dtByte: Result := GetFieldAsByte(Field);
    dtWord: Result := GetFieldAsWord(Field);
  else
    Result := 0;
  end;
end;

class function TRPCRecord.GetFieldAsInt64(const Field: PRPCField): Int64;
begin
  case Field^.FieldType of
    dtNotTyped, dtString: Result := StrToInt64(GetFieldAsString(Field));
    dtBlob: raise Exception.Create('Conversion BLOB to Int64 not supported');
    dtInteger: Result := PRPCIntegerField(Field).Value;
    dtLargeint: Result := PRPCInt64Field(Field).Value;
    dtDateTime,
      dtExtended,
      dtCurrency,
      dtTime,
      dtDate: Result := Trunc(GetFieldAsFloat(Field));
    dtBoolean: Result := IfThen(GetFieldAsBoolean(Field), 1, 0);
    dtByte: Result := GetFieldAsByte(Field);
    dtWord: Result := GetFieldAsWord(Field);
  else
    Result := 0;
  end;
end;

class function TRPCRecord.GetFieldAsString(const Field: PRPCField): string;
begin
  case Field^.FieldType of
    dtBlob: Result := '** BLOB **';
    dtInteger: Result := IntToStr(GetFieldAsInteger(Field));
    dtLargeint: Result := IntToStr(GetFieldAsInt64(Field));
    dtDateTime,
      dtExtended,
      dtCurrency,
      dtTime,
      dtDate: Result := FloatToStr(GetFieldAsFloat(Field));
    dtBoolean: Result := IfThen(GetFieldAsBoolean(Field), '1', '0');
    dtByte: Result := InttoStr(GetFieldAsByte(Field));
    dtWord: Result := InttoStr(GetFieldAsWord(Field));
  else
    Result := PRPCStringField(Field)^.Value;
  end;
end;

class function TRPCRecord.GetFieldAsWord(const Field: PRPCField): Word;
begin
  case Field^.FieldType of
    dtNotTyped, dtString: Result := StrToInt(GetFieldAsString(Field));
    dtBlob: raise Exception.Create('Conversion BLOB to Word not supported');
    dtInteger: Result := GetFieldAsInteger(Field);
    dtLargeint: Result := GetFieldAsInt64(Field);
    dtDateTime,
      dtExtended,
      dtCurrency,
      dtTime,
      dtDate: Result := Trunc(GetFieldAsFloat(Field));
    dtBoolean: Result := IfThen(GetFieldAsBoolean(Field), 1, 0);
    dtByte: Result := GetFieldAsByte(Field);
    dtWord: Result := PRPCWordField(Field)^.Value;
  else
    Result := 0;
  end;
end;

function TRPCRecord.GetRPCFields(Index: Integer): PRPCField;
begin
  if (Index < 0) or (Index >= Count) then
    raise Exception.CreateFmt('Buffer error: Index %0:d is less than zero or greater then %1:d', [Index, Count - 1])
  else
    Result := PRPCField(Items[Index]);
end;

procedure TRPCRecord.LoadFieldFromStream(Stream: TStream; const BufferFormat: TApBufferFormat);
var
  FDataType: TRPCFieldType;
  FField: PRPCField;

  procedure LoadStreamField;
  var
    FSize: Integer;
  begin
    Stream.Read(FSize, SizeOf(FSize));
    if FSize > 0 then
    begin
      PRPCStreamField(FField)^.Value := TMemoryStream.Create;
      PRPCStreamField(FField)^.Value.CopyFrom(Stream, FSize);
    end;
  end;

  procedure LoadIntegerField;
  var
    FValue: Integer;
  begin
    Stream.Read(FValue, SizeOf(FValue));
    PRPCIntegerField(FField)^.Value := FValue;
  end;

  procedure LoadInt64Field;
  var
    FValue: Int64;
  begin
    Stream.Read(FValue, SizeOf(FValue));
    PRPCInt64Field(FField)^.Value := FValue;
  end;

  procedure LoadDateTimeField;
  var
    FValue: TDateTime;
  begin
    Stream.Read(FValue, SizeOf(FValue));
    PRPCDateTimeField(FField)^.Value := FValue;
  end;

  procedure LoadFloatField;
  var
    FValue: Double;
  begin
    Stream.Read(FValue, SizeOf(FValue));
    PRPCFloatField(FField)^.Value := FValue;
  end;

  procedure LoadBooleanField;
  var
    FValue: Boolean;
  begin
    Stream.Read(FValue, SizeOf(FValue));
    PRPCBooleanField(FField)^.Value := FValue;
  end;

  procedure LoadWordField;
  var
    FValue: Word;
  begin
    Stream.Read(FValue, SizeOf(FValue));
    PRPCWordField(FField)^.Value := FValue;
  end;

  procedure LoadByteField;
  var
    FValue: Byte;
  begin
    Stream.Read(FValue, SizeOf(FValue));
    PRPCByteField(FField)^.Value := FValue;
  end;

  procedure LoadStringField;
  var
    FValue: Char;
  begin
    repeat
      if Stream.Read(FValue, SizeOf(FValue)) <> SizeOf(FValue) then
        raise Exception.CreateFmt('Server Buffer Error: String Field Corrupted. Original type %s', [GetEnumName(TypeInfo(TRPCFieldType), Ord(FField.FieldType))])
      else if FValue <> #0 then
        PRPCStringField(FField)^.Value := PRPCStringField(FField)^.Value + FValue;
    until FValue = #0;
  end;

var
  FFieldValue: PChar;
  FLastChar, FPriorChar: Char;
  FBeginOff, FEndOff: Integer;
begin
  if BufferFormat = bfOld then
  begin
    FBeginOff := Stream.Position;
    FEndOff := FBeginOff;
    FLastChar := #0;
    FDataType := dtString;
    repeat
      FPriorChar := FLastChar;
      if Stream.Read(FLastChar, 1) <> 1 then
        raise Exception.Create('Stream read Error')
      else
      begin
        if (FLastChar = FBlobInit) and (FPriorChar = FEscSep) then
        begin
          FDataType := dtBlob;
          //Jumps DataType
          Stream.Position := Stream.Position + 1;
          Break;
        end
        else if FLastChar = FFieldSep then
        begin
          if FDataType <> dtBlob then
          begin
            FEndOff := Stream.Position - 2;
            FDataType := TRPCFieldType(StrToInt('0x' + FPriorChar));
          end
          else
            FEndOff := Stream.Position - 1;
        end;
      end;
    until FLastChar = FFieldSep;
    if FDataType <> dtBlob then
    begin
      Stream.Position := FBeginOff;
      FFieldValue := AllocMem(FEndOff - FBeginOff);
      try
        Stream.Read(FFieldValue^, FEndOff - FBeginOff);
        Stream.Position := Stream.Position + 2;
        FField := AddField(FDataType);
        case FDataType of
          dtInteger: PRPCIntegerField(FField)^.Value := StrToInt(StrPas(FFieldValue));
          dtLargeint: PRPCInt64Field(FField)^.Value := StrToInt64(StrPas(FFieldValue));
          dtDateTime,
            dtTime,
            dtDate: PRPCDateTimeField(FField)^.Value := StrToFloat(StrPas(FFieldValue));
          dtExtended,
            dtCurrency: PRPCFloatField(FField)^.Value := StrToFloat(StrPas(FFieldValue));
          dtBoolean: PRPCBooleanField(FField)^.Value := (FFieldValue^ = '1');
          dtWord: PRPCWordField(FField)^.Value := StrToInt(StrPas(FFieldValue));
          dtByte: PRPCByteField(FField)^.Value := StrToInt(StrPas(FFieldValue));
        else
          PRPCStringField(FField)^.Value := StrPas(FFieldValue);
        end;
      finally
        FreeMem(FFieldValue);
      end;
    end
    else //Treats Blob Field
    begin
      FField := AddField(dtBlob);
      FPriorChar := #0;
      while true do
      begin
        if Stream.Read(FLastChar, 1) <> 1 then
          raise Exception.Create('Stream Read Error')
        else
        begin
          if (FLastChar = FEscSep) and (FPriorChar = #0) then
            FPriorChar := FEscSep
          else
          begin
            if FPriorChar = FEscSep then
            begin
              case FLastChar of
                'F': FLastChar := FFieldSep;
                'R': FLastChar := FRecSep;
                'N': FLastChar := FNullSep;
                'C': FLastChar := CR;
                'L': FLastChar := LF;
              end;
              FPriorChar := #0;
            end
            else if FLastChar = FFieldSep then
              Break;
            if not Assigned(PRPCStreamField(FField)^.Value) then
              PRPCStreamField(FField)^.Value := TMemoryStream.Create;
            PRPCStreamField(FField)^.Value.Write(FLastChar, 1);
          end;
        end;
      end;
    end;
  end
  else
  begin
    Stream.Read(FDataType, SizeOf(FDataType));
    FField := AddField(FDataType);
    case FDataType of
      dtBlob: LoadStreamField;
      dtInteger: LoadIntegerField;
      dtLargeint: LoadInt64Field;
      dtDateTime,
        dtTime,
        dtDate: LoadDateTimeField;
      dtExtended,
        dtCurrency: LoadFloatField;
      dtBoolean: LoadBooleanField;
      dtWord: LoadWordField;
      dtByte: LoadByteField;
    else
      LoadStringField;
    end;
  end;
end;

procedure TRPCRecord.LoadFieldFromXMLNode(const AFieldNode: TXMLNode);
var
  FField: PRPCField;

  function RecognizeFieldType: TRPCFieldType;
  begin
    for Result := low(XMLFieldTypes) to high(XMLFieldTypes) do
      if SameText(XMLFieldTypes[Result], AFieldNode.AttributeByName['dataType']) then
        break;
  end;

  procedure LoadStreamField;
  var
    FByte: Byte;
    FValue: String;
  begin
    if (AFieldNode.ValueAsWidestring <> '') then
    begin
      PRPCStreamField(FField)^.Value := TMemoryStream.Create;
      with PRPCStreamField(FField)^.Value do
      begin
        FValue := AFieldNode.ValueAsWidestring;
        while Position < (length(FValue) div 2) do
        begin
          FByte := StrToInt('$' + FValue[(Position * 2) + 1] + FValue[(Position * 2) + 2]);
          Write(FByte, SizeOf(FByte));
        end;
      end;
    end;
  end;

  procedure LoadIntegerField;
  begin
    PRPCIntegerField(FField)^.Value := AFieldNode.ValueAsIntegerDef(0);
    {if Assigned(AFieldNode.firstChild) then
      PRPCIntegerField(FField)^.Value := StrToIntDef(AfieldNode.firstChild.nodeValue, 0)
    else PRPCIntegerField(FField)^.Value := 0;}
  end;

  procedure LoadInt64Field;
  begin
    PRPCInt64Field(FField)^.Value := AFieldNode.ValueAsInt64Def(0);
    {if Assigned(AFieldNode.FirstChild) then
      PRPCInt64Field(FField)^.Value := StrToInt64Def(AFieldNode.FirstChild.NodeValue, 0)
    else PRPCInt64Field(FField)^.Value := 0;}
  end;

  procedure LoadDateTimeField;
  begin
    PRPCDateTimeField(FField)^.Value := AFieldNode.ValueAsDateTimeDef(0); 
    {if Assigned(AFieldNode.firstChild) then
      PRPCDateTimeField(FField)^.Value := StrtoDateTime(AfieldNode.firstChild.nodeValue)
    else PRPCDateTimeField(FField)^.Value := 0;}
  end;

  procedure LoadFloatField;
  begin
    PRPCFloatField(FField)^.Value := AFieldNode.ValueAsFloatDef(0);
    {if Assigned(AFieldNode.firstChild) then
      PRPCFloatField(FField)^.Value := StrToFloatDef(AfieldNode.firstChild.nodeValue, 0)
    else PRPCFloatField(FField)^.Value := 0;}
  end;

  procedure LoadBooleanField;
  begin
    PRPCBooleanField(FField)^.Value := AFieldNode.ValueAsBoolDef(false);
    {if Assigned(AFieldNode.firstChild) then
      PRPCBooleanField(FField)^.Value := SameText(AfieldNode.firstChild.nodeValue, 'true')
    else PRPCBooleanField(FField)^.Value := false;}
  end;

  procedure LoadWordField;
  begin
    PRPCWordField(FField)^.Value := AFieldNode.ValueAsIntegerDef(0);
    {if Assigned(AFieldNode.firstChild) then
      PRPCWordField(FField)^.Value := StrToIntDef(AfieldNode.firstChild.nodeValue, 0)
    else PRPCWordField(FField)^.Value := 0;}
  end;

  procedure LoadByteField;
  begin
    PRPCByteField(FField)^.Value := AFieldNode.ValueAsIntegerDef(0);
    {if Assigned(AFieldNode.firstChild) then
      PRPCByteField(FField)^.Value := StrToIntDef(AfieldNode.firstChild.nodeValue, 0)
    else PRPCByteField(FField)^.Value := 0;}
  end;

  procedure LoadStringField;
  begin
    PRPCStringField(FField)^.Value := AfieldNode.ValueAsString;  
    {if Assigned(AFieldNode.firstChild) then
      PRPCStringField(FField)^.Value := AfieldNode.firstChild.nodeValue
    else PRPCStringField(FField)^.Value := '';}
  end;

begin
  FField := AddField(RecognizeFieldType);
  if FBuffer.FieldName[Count - 1] = '' then
    FBuffer.FieldName[Count - 1] := AFieldNode.Name;
  case FField.FieldType of
    dtBlob: LoadStreamField;
    dtInteger: LoadIntegerField;
    dtLargeint: LoadInt64Field;
    dtDateTime,
      dtTime,
      dtDate: LoadDateTimeField;
    dtExtended,
      dtCurrency: LoadFloatField;
    dtBoolean: LoadBooleanField;
    dtWord: LoadWordField;
    dtByte: LoadByteField;
  else
    LoadStringField;
  end;
end;

procedure TRPCRecord.LoadFromStream(Stream: TStream; const BufferFormat: TApBufferFormat=bfNew);
var
  I, FFieldCount: Word;
  FSep: Char;
begin
  //Read Fieldcount For This Record
  if BufferFormat = bfOld then
  begin
    repeat
      LoadFieldFromStream(Stream, BufferFormat);
      if Stream.Read(FSep, 1) <> 1 then
        Break;
      if FSep <> FRecSep then
        Stream.Position := Stream.Position - 1;
    until FSep = FRecSep;
  end
  else
  begin
    FFieldCount := 0;
    Stream.Read(FFieldCount, SizeOf(FFieldCount));
    for I := 0 to FFieldCount - 1 do
      LoadFieldFromStream(Stream, BufferFormat);
  end;
end;

procedure TRPCRecord.LoadFromXMLDOMNode(const ARecordNode: TXMLNode);
var
  I: Integer;
begin
  for I := 0 to ARecordNode.NodeCount - 1 do
    LoadFieldFromXMLNode(ARecordNode.Nodes[I]);
end;

procedure TRPCRecord.SaveFieldToStream(Stream: TStream; const Index: Integer;
  const BufferFormat: TApBufferFormat);
var
  FDummy: String;

  function GetFieldName: string;
  begin
    Result := FBuffer.GetFieldName(Index);
    if Result = '' then
      Result := Format('Field_%d', [Index]);
  end;

  function GetFieldDataType: string;
  begin
    Result := XMLFieldTypes[RPCFields[Index].FieldType];
  end;

  procedure SaveStreamField;
  var
    FRPCStreamField: PRPCStreamField;
    FSize: Integer;
    FDummy: String;
    FDummyByte: Byte;
  begin
    if BufferFormat = bfNew then
    begin
      FRPCStreamField := PRPCStreamField(RPCFields[Index]);
      if Assigned(FRPCStreamField.Value) then
      begin
        FSize := FRPCStreamField.Value.Size;
        Stream.Write(FSize, SizeOf(FSize));
        FRPCStreamField.Value.Position := 0;
        Stream.CopyFrom(FRPCStreamField.Value, FSize);
      end
      else
      begin
        FSize := 0;
        Stream.Write(FSize, SizeOf(FSize));
      end;
    end else //XML
    begin
      FRPCStreamField := PRPCStreamField(RPCFields[Index]);
      FDummy := '';
      if Assigned(FRPCStreamField.Value) and (FRPCStreamField.Value.Size > 0) then
      begin
        FRPCStreamField.Value.Position := 0;
        while FRPCStreamField.Value.Position < FRPCStreamField.Value.Size do
        begin
          FRPCStreamField.Value.Read(FDummyByte, SizeOf(FDummyByte));
          FDummy := FDummy + IntToHex(FDummyByte, 2);
        end;
      end;
      FDummy := Format('<%0:s dataType="%2:s">%1:s</%0:s>', [GetFieldName,
        FDummy, GetFieldDataType]);
      Stream.Write(FDummy[1], Length(FDummy));
    end;
  end;

  procedure SaveIntegerField;
  begin
    with PRPCIntegerField(RPCFields[Index])^ do
    begin
      if BufferFormat = bfNew then
        Stream.Write(Value, SizeOf(Value))
      else begin//XML
        FDummy := Format('<%0:s dataType="%2:s">%1:d</%0:s>' + #13#10,
          [GetFieldName, Value, GetFieldDataType]);
        Stream.Write(FDummy[1], Length(FDummy));
      end;
    end;
  end;

  procedure SaveInt64Field;
  begin
    with PRPCInt64Field(RPCFields[Index])^ do
    begin
      if BufferFormat = bfNew then
        Stream.Write(Value, SizeOf(Value))
      else begin//XML
        FDummy := Format('<%0:s dataType="%2:s">%1:d</%0:s>' + #13#10,
          [GetFieldName, Value, GetFieldDataType]);
        Stream.Write(FDummy[1], Length(FDummy));
      end;
    end;
  end;

  procedure SaveDateTimeField;
  begin
    with PRPCDateTimeField(RPCFields[Index])^ do
    begin
      if BufferFormat = bfNew then
        Stream.Write(Value, SizeOf(Value))
      else begin//XML
        if FieldType = dtDateTime then
          FDummy := Format('<%0:s dataType="%2:s">%1:s</%0:s>' + #13#10,
            [GetFieldName, DateTimeToStr(Value), GetFieldDataType])
        else if FieldType = dtDate then
          FDummy := Format('<%0:s dataType="%2:s">%1:s</%0:s>' + #13#10,
            [GetFieldName, DateToStr(Value), GetFieldDataType])
        else
          FDummy := Format('<%0:s dataType="%2:s">%1:s</%0:s>' + #13#10,
            [GetFieldName, TimeToStr(Value), GetFieldDataType]);
        Stream.Write(FDummy[1], Length(FDummy));
      end;
    end;
  end;

  procedure SaveFloatField;
  begin
    with PRPCFloatField(RPCFields[Index])^ do
    begin
      if BufferFormat = bfNew then
        Stream.Write(Value, SizeOf(Value))
      else begin//XML
        FDummy := Format('<%0:s dataType="%2:s">%1:f</%0:s>' + #13#10,
          [GetFieldName, Value, GetFieldDataType]);
        Stream.Write(FDummy[1], Length(FDummy));
      end;
    end;
  end;

  procedure SaveWordField;
  begin
    with PRPCWordField(RPCFields[Index])^ do
    begin
      if BufferFormat = bfNew then
        Stream.Write(Value, SizeOf(Value))
      else begin//XML
        FDummy := Format('<%0:s dataType="%2:s">%1:d</%0:s>' + #13#10,
          [GetFieldName, Value, GetFieldDataType]);
        Stream.Write(FDummy[1], Length(FDummy));
      end;
    end;
  end;

  procedure SaveByteField;
  begin
    with PRPCByteField(RPCFields[Index])^ do
    begin
      if BufferFormat = bfNew then
        Stream.Write(Value, SizeOf(Value))
      else begin//XML
        FDummy := Format('<%0:s dataType="%2:s">%1:d</%0:s>' + #13#10,
          [GetFieldName, Value, GetFieldDataType]);
        Stream.Write(FDummy[1], Length(FDummy));
      end;
    end;
  end;

  procedure SaveBooleanField;
  begin
    with PRPCBooleanField(RPCFields[Index])^ do
    begin
      if BufferFormat = bfNew then
        Stream.Write(Value, SizeOf(Value))
      else begin//XML
        FDummy := Format('<%0:s dataType="%2:s">%1:s</%0:s>' + #13#10,
          [GetFieldName, IfThen(Value, 'true', 'false'), GetFieldDataType]);
        Stream.Write(FDummy[1], Length(FDummy));
      end;
    end;
  end;

  procedure SaveStringField;
  const
    FNull: Char = #0;
  begin
    with PRPCStringField(RPCFields[Index])^ do
    begin
      if BufferFormat = bfNew then
      begin
        if Value <> '' then
          Stream.Write(Value[1], Length(Value) + 1)
        else
          Stream.Write(FNull, SizeOf(FNull));
      end else //XML
      begin
        FDummy := StringReplace(StringReplace(StringReplace(StringReplace(
          StringReplace(Value, '&','&amp;', [rfReplaceAll]), '<', '&lt;',
          [rfReplaceAll]), '''', '&apos;', [rfReplaceAll]), '"', '&quot;',
          [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]);
        FDummy := Format('<%0:s dataType="%2:s">%1:s</%0:s>' + #13#10,
          [GetFieldName, FDummy, GetFieldDataType]);
        Stream.Write(FDummy[1], Length(FDummy));
      end;
    end;
  end;

  function GetReplaceChar(const AChar: Char): Char;
  begin
    case AChar of
      FFieldSep: Result := 'F';
      FRecSep: Result := 'R';
      FEscSep: Result := FEscSep;
      FNullSep: Result := 'N';
      CR: Result := 'C';
      LF: Result := 'L';
    else
      Result := #0;
    end;
  end;

var
  FDataType: TRPCFieldType;
  FField: PRPCField;
  I, FSize: Integer;
  FChar: Char;
begin
  FField := RPCFields[Index];
  FDataType := FField^.FieldType;
  if BufferFormat = bfOld then
  begin
    if FDataType = dtBlob then
    begin
      FDummy := #127#84 + Chr(Ord('0') + Ord(dtBlob));
      Stream.Write(FDummy[1], 3);
      FDummy := '';
      with PRPCStreamField(FField)^ do
      begin
        if Assigned(Value) and (Value.Size > 0) then
        begin
          FSize := Value.Size;
          Value.Position := 0;
          I := 1;
          SetLength(FDummy, FSize);
          repeat
            if Value.Read(FChar, 1) <> 1 then
              raise Exception.Create('Stream Read Error')
            else
            begin
              case FChar of
                FFieldSep,
                  FRecSep,
                  FEscSep,
                  FNullSep,
                  CR, LF:
                  begin
                    FSize := FSize + 1;
                    SetLength(FDummy, FSize);
                    FDummy[I] := FEscSep;
                    Inc(I);
                    FDummy[I] := GetReplaceChar(FChar);
                  end;
              else
                FDummy[I] := FChar;
              end;
              Inc(I);
            end;
          until Value.Position = Value.Size;
        end
        else
          FSize := 0;
      end;
    end
    else
    begin
      FDummy := GetFieldAsString(FField);
      FSize := Length(FDummy);
    end;
    if FDummy <> '' then
      Stream.Write(FDummy[1], FSize);
    FDummy := IfThen(FDataType = dtBlob, '', IntToHex(Ord(FDataType), 1)) + FFieldSep;
    Stream.Write(FDummy[1], Length(FDummy));
  end else
  begin
    //Saves DataType
    if BufferFormat = bfNew then
      Stream.Write(FDataType, SizeOf(FDataType));
    case FDataType of
      dtBlob: SaveStreamField;
      dtInteger: SaveIntegerField;
      dtLargeint: SaveInt64Field;
      dtDateTime,
        dtTime,
        dtDate: SaveDateTimeField;
      dtExtended,
        dtCurrency: SaveFloatField;
      dtBoolean: SaveBooleanField;
      dtWord: SaveWordField;
      dtByte: SaveByteField;
    else
      SaveStringField;
    end;
  end;
end;

procedure TRPCRecord.SaveFieldToXML(ARecordNode: TXmlNode; const Index: Integer);

  function GetFieldName: string;
  begin
    Result := FBuffer.GetFieldName(Index);
    if Result = '' then
      Result := Format('Field_%d', [Index]);
  end;

  function GetFieldDataType: string;
  begin
    Result := XMLFieldTypes[RPCFields[Index].FieldType];
  end;

  procedure SaveStreamField(const Field: TXmlNode);
  var
    FRPCStreamField: PRPCStreamField;
    FDummy: String;
    FDummyByte: Byte;
  begin
    FRPCStreamField := PRPCStreamField(RPCFields[Index]);
    FDummy := '';
    if Assigned(FRPCStreamField.Value) and (FRPCStreamField.Value.Size > 0) then
    begin
      FRPCStreamField.Value.Position := 0;
      while FRPCStreamField.Value.Position < FRPCStreamField.Value.Size do
      begin
        FRPCStreamField.Value.Read(FDummyByte, SizeOf(FDummyByte));
        FDummy := FDummy + IntToHex(FDummyByte, 2);
      end;
    end;
    Field.ValueAsWidestring := FDummy;
  end;

  procedure SaveIntegerField(const Field: TXmlNode);
  begin
    Field.ValueAsInteger := PRPCIntegerField(RPCFields[Index])^.Value;
  end;

  procedure SaveInt64Field(const Field: TXmlNode);
  begin
    Field.ValueAsInt64 := PRPCInt64Field(RPCFields[Index])^.Value;
  end;

  procedure SaveDateTimeField(const Field: TXmlNode);
  begin
    Field.ValueAsDateTime := PRPCDateTimeField(RPCFields[Index])^.Value;
  end;

  procedure SaveFloatField(const Field: TXmlNode);
  begin
    Field.ValueAsFloat := PRPCFloatField(RPCFields[Index])^.Value;
  end;
  
  procedure SaveWordField(const Field: TXmlNode);
  begin
    Field.ValueAsInteger := PRPCWordField(RPCFields[Index])^.Value;
  end;

  procedure SaveByteField(const Field: TXmlNode);
  begin
    Field.ValueAsInteger := PRPCByteField(RPCFields[Index])^.Value;
  end;
  
  procedure SaveBooleanField(const Field: TXmlNode);
  begin
    Field.ValueAsBool := PRPCBooleanField(RPCFields[Index])^.Value;
  end;

  procedure SaveStringField(const Field: TXmlNode);
  begin
    Field.ValueAsString := PRPCStringField(RPCFields[Index])^.Value;
  end;

var
  FField: TXmlNode;
begin
  FField := ARecordNode.NodeNew(GetFieldName);
  FField.AttributeAdd('dataType', GetFieldDataType);
  case RPCFields[Index]^.FieldType of
    dtBlob: SaveStreamField(FField);
    dtInteger: SaveIntegerField(FField);
    dtLargeint: SaveInt64Field(FField);
    dtDateTime,
      dtTime,
      dtDate: SaveDateTimeField(FField);
    dtExtended,
      dtCurrency: SaveFloatField(FField);
    dtBoolean: SaveBooleanField(FField);
    dtWord: SaveWordField(FField);
    dtByte: SaveByteField(FField);
  else
    SaveStringField(FField);
  end;
end;

procedure TRPCRecord.SaveToStream(Stream: TStream; const BufferFormat: TApBufferFormat=bfNew);

  procedure SaveOldHeader;
  var
    FDummy: string[1];
  begin
    FDummy := FRecSep;
    Stream.Write(FDummy[1], 1);
  end;

  procedure SaveNewHeader;
  var
    FFieldCount: Word;
  begin
    FFieldCount := Count;
    Stream.Write(FFieldCount, SizeOf(FFieldCount))
  end;

  procedure SaveXMLHeader;
  var
    FDummy: String;
  begin
    FDummy := '<record>' + #13#10;
    Stream.Write(FDummy[1], Length(FDummy));
  end;

  procedure SaveXMLFooter;
  var
    FDummy: String;
  begin
    FDummy := '</record>' + #13#10;
    Stream.Write(FDummy[1], Length(FDummy));
  end;

var
  I: Word;
begin
  //Record Header
  case BufferFormat of
    bfOld: SaveOldHeader;
    bfNew: SaveNewHeader;
    bfXML: SaveXMLHeader;
  end;
  //Fields
  for I := 0 to Count - 1 do
    SaveFieldToStream(Stream, I, BufferFormat);
  //Record Footer  
  if BufferFormat = bfXML then
    SaveXMLFooter;
end;

procedure TRPCRecord.SaveToXML(XML: TNativeXml);
var
  FRecordNode: TXmlNode;
  I: Integer;
begin
  FRecordNode := nil;
  for I := 0 to Count - 1 do
  begin
    if not Assigned(FRecordNode) then
      FRecordNode := XML.Root.NodeNew('record');
    SaveFieldToXML(FRecordNode, I);
  end;
end;

procedure TRPCRecord.SetRPCFields(Index: Integer; const Value: PRPCField);
begin
  Items[Index] := Value;
end;

{ ERPCBufferConversionError }

constructor ERPCBufferConversionError.Create(const Index: Integer; const FieldType: TRPCFieldType);
begin
  inherited CreateFmt('Error in RPCBuffer type Conversion. FieldType: %s Index: %d',
    [GetEnumName(TypeInfo(TRPCFieldType), Integer(FieldType)), Index]);
end;

{ TPCharStream }

constructor TPCharStream.Create(APChar: PChar; const ASize: Integer);
begin
  inherited Create;
  SetPointer(APChar, ASize);
end;

function TPCharStream.Write(const Buffer; Count: Integer): Longint;
begin
  raise Exception.Create('PCharStream is readonly Stream, write method doesn''t supported');
end;

{ TFieldNames }

destructor TFieldNames.Destroy;
var
  I: integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Assigned(Objects[I]) then
    begin
      TFieldNameInfo(Objects[I]).Free;
      Objects[I] := nil;
    end;
  end;
  inherited;
end;

{ TSharedBuffer }

function TSharedBuffer.AppendRPCField(const ADataType: TRPCFieldType): PRPCField;
begin
  Result := nil;
  RaiseWriteError;
end;

function TSharedBuffer.AppendRPCRecord: TRPCRecord;
begin
  Result := nil;
  RaiseWriteError;
end;

procedure TSharedBuffer.AssignBuffer(From: TRPCBuffer);
begin
  RaiseWriteError;
end;

constructor TSharedBuffer.Create(ASourceBuffer: TRPCBuffer);
  procedure AssignIndexes;
  var
    i: integer;
    Src: TRPCBufferSort;
    Dst: TSharedBufferSort;
  begin
    for i := 0 to ASourceBuffer.FSortIndex.Count - 1 do
    begin
      Src := TRPCBufferSort(ASourceBuffer.FSortIndex.Items[i]);
      Dst := TSharedBufferSort.Create(Src);
      FSortIndex.FSortList.AddObject(ASourceBuffer.FSortIndex.FSortList[i], Dst);
    end;
  end;
begin
  inherited Create;
  // Libera a lista de dados atual e aponta para a do Buffer compartilhado
  FreeAndNil(FDataList);
  FDataList := ASourceBuffer.FDataList;
  // Indices
  AssignIndexes;
  // Lista de nomes
  FreeFieldNames;
  FFieldNames := ASourceBuffer.FFieldNames;
end;


procedure TSharedBuffer.Delete;
begin
  RaiseWriteError;
end;

procedure TSharedBuffer.FreeDataObjects;
begin
  // Data structs has been shared, nothing will be free now
  if Assigned(FSortIndex) then FreeAndNil(FSortIndex);  
end;

procedure TSharedBuffer.RaiseWriteError;
begin
  raise ERPCSharedWriteError.Create('You do not write to shared buffers');
end;

procedure TSharedBuffer.RemoveRPCRecord(const Index: Integer);
begin
  RaiseWriteError;
end;

function TSharedBuffer.RemoveSort(const ASortIndex: Integer): Boolean;
begin
  Result := False;
  RaiseWriteError;
end;

function TSharedBuffer.RemoveSort(const ASortName: string): Boolean;
begin
  Result := False;
  RaiseWriteError;
end;

procedure TSharedBuffer.Sort(const SortName: string;
  const BuildKey: array of Word; const IntSort, Ascending,
  ACaseSensitive: Boolean);
begin
  RaiseWriteError;
end;

procedure TSharedBuffer.WriteStreamField(const NewRec: Boolean; Data: TStream;
  const FromBeggining: Boolean; const ASize: Integer;
  const FreeStream: Boolean);
begin
  RaiseWriteError;
end;

procedure TSharedBuffer.WriteTFieldToBuffer(const NewRec: Boolean;
  const Field: TField);
begin
  RaiseWriteError;
end;

procedure TSharedBuffer.WriteTypedFields(const NewRec: Boolean; const Args: array of const; const Types: array of TRPCFieldType);
begin
  RaiseWriteError;
end;

{ TRPCBufferDataList }

procedure TRPCBufferDataList.Clear;
var
  i: integer;
begin
  for i:=0 to Count-1 do
    if Assigned(Items[i]) then
      TObject(Items[i]).Free;
  inherited;
end;

destructor TRPCBufferDataList.Destroy;
begin
  Clear;
  inherited;
end;

{ TInsertData }

constructor TInsertDataBuffer.Create;
begin
  inherited;
  ZeroDateAsNull := True;
end;

{ TSharedBufferSort }

procedure TSharedBufferSort.AddRecord(const ARecord: TRPCRecord);
begin
  //
end;

procedure TSharedBufferSort.ClearDataList;
begin
  //
end;

constructor TSharedBufferSort.Create(const ASource: TRPCBufferSort);
begin
  inherited Create(ASource.FBuffer, ASource.FKeys, ASource.FAscending, ASource.FCaseSensitive); 
  if Assigned(FDataList) then
    FreeAndNil(FDataList);
  FDataList := ASource.FDataList;
end;

procedure TSharedBufferSort.DeleteRecord(const ARecord: TRPCRecord);
begin
 //
end;

destructor TSharedBufferSort.Destroy;
begin
  FDataList := nil; //Nao pode destruir
  inherited;
end;

procedure TSharedBufferSort.Sort;
begin
  //
end;

{$ENDIF}

end.
