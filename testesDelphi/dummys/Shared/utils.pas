unit utils;

interface

uses
  System.SysUtils,
  System.Win.ComObj,
  System.Generics.Collections,
  System.Character,
  System.Math,
  System.StrUtils,

  WinApi.Windows,
  JwaAdsTLB,
  JwaAdsHlp,
  JwaAdsErr,
  WinApi.ShellApi,
  WinApi.ActiveX
  ;

type
  TWordParsedEntry = class;
  TWordParsedType = (wptWord, wptSeparator);
  TCharSet = Array of Char;
  TWordParsedArray = TArray<TWordParsedEntry>;

  TWordParsedEntry = class
  public
    EntryStr: String;
    EntryDelim: Char;
    EntryType: TWordParsedType;
    constructor Create(const AEntryStr: String; AEntryType: TWordParsedType);
    function Clone: TWordParsedEntry;
  end;

  TWordParsedList = class(TObjectList<TWordParsedEntry>)
  private
    FDelims: TCharSet;
    FIgnoreStrInQuotes: Boolean;
    FQuoteChar: Char;
    FRelaxedDelim: Boolean;
    FStrLen: Integer;
    function GetCloneArray: TWordParsedArray;
  public
    constructor Create(const AStr: String; const AWordDelims: TCharSet; const AIgnoreStrInQuotes: Boolean;
                       const AQuoteChar: Char; const ARelaxedDelim: Boolean);
    procedure InsertEntry(const Index: Integer; const EntryStr: String; const EntryType: TWordParsedType);
    procedure ReplaceWord(const AIndex: Integer; const ANewValue: String);
    function EntryIndex(const AEntryStr: String; const AEntryType: TWordParsedType; const ACaseSensitive: Boolean; const AStartEntry: Integer = 0): Integer;
    function GetRangeStr(const AStart: Integer; const ALen: Integer = MaxInt): String;
    function WordCount: Integer;
    function WordPosition(const AIndex: Integer): Integer;
    function ExtractWord(const AIndex: Integer): String;
    function WordIndex(const AWord: String; ACaseSensitive: Boolean): Integer;
    function PosWord(const AWord: String; const ACaseSensitive: Boolean): Integer;
    function IsWordPresent(const AWord: String; const ACaseSensitive: Boolean): Boolean;
    function WordOccurs(const AWord: String; const ACaseSensitive: Boolean): Integer;
    function WordReplace(const ASearchWord, AReplaceWord: String; const ACaseSensitive: Boolean): String;
    function PersistentWordReplace(const ASearchWord, AReplaceWord: String; const ACaseSensitive: Boolean): Boolean;
    function GetStringArray: TArray<String>;
    function ToString: String; override;
  end;

procedure NTLMSplitUserName(const LogonString: String; var AUserName, ADomain: String);
function ParseWords(const S: String; const AWordDelims: TCharSet; const AIgnoreStrInQuotes: Boolean = False; const AQuoteChar: Char = #39; const ARelaxedDelim: Boolean = False): TWordParsedList; inline;
function WordCount(const S: String; const AWordDelims: TCharSet; const AIgnoreStrInQuotes: Boolean = False; const AQuoteChar: Char = #39; const ARelaxedDelim: Boolean = False): Integer; inline;
function WordPosition(const N: Integer; const S: String; const AWordDelims: TCharSet; const AIgnoreStrInQuotes: Boolean = False; const AQuoteChar: Char = #39; const ARelaxedDelim: Boolean = False): Integer; inline;
function ExtractWord(const N: Integer; const S: String; const AWordDelims: TCharSet; const AIgnoreStrInQuotes: Boolean = False; const AQuoteChar: Char = #39; const ARelaxedDelim: Boolean = False): String; inline;
function FirstWord(const S: String; const AWordDelims: TCharSet; const AIgnoreStrInQuotes: Boolean = False; const AQuoteChar: Char = #39; const ARelaxedDelim: Boolean = False): String; inline;
function LastWord(const S: String; const AWordDelims: TCharSet; const AIgnoreStrInQuotes: Boolean = False; const AQuoteChar: Char = #39; const ARelaxedDelim: Boolean = False): String; inline;
function WordIndex(const W, S: String; const AWordDelims: TCharSet; const ACaseSensitive: Boolean = False; const AIgnoreStrInQuotes: Boolean = False; const AQuoteChar: Char = #39; const ARelaxedDelim: Boolean = False): Integer; inline;
function PosWord(const AWord, AText: String; const AWordDelims: TCharSet; const ACaseSensitive: Boolean = True; const AIgnoreStrInQuotes: Boolean = False; const AQuoteChar: Char = #39; const ARelaxedDelim: Boolean = False): Integer; inline;
function IsTextPresent(const AText, AStr: String; const AWordDelims: TCharSet; const ACaseSensitive: Boolean = False; const AIgnoreStrInQuotes: Boolean = False; const AQuoteChar: Char = #39; const ARelaxedDelim: Boolean = True): Boolean; inline;
function WordOccurs(const AWord, AStr: String; const AWordDelims: TCharSet; const ACaseSensitive: Boolean; const AIgnoreStrInQuotes: Boolean = False; const AQuoteChar: Char = #39; const ARelaxedDelim: Boolean = False): Integer; inline;
function WordReplace(var Text: String; const ASearchWord, AReplaceWord: String; const ACaseSensitive: Boolean;
                     const ADelimiters: TCharSet; const AIgnoreStrInQuotes: Boolean = False;
                     const AQuoteChar: Char = #39; const ARelaxedDelim: Boolean = False): Boolean; inline; overload;
function WordReplace(var Text: String; const ASearchWord, AReplaceWord: String; const ACaseSensitive: Boolean = True;
                     const AIgnoreStrInQuotes: Boolean = False; const AQuoteChar: Char = #39;
                     const ARelaxedDelim: Boolean = False): Boolean; inline; overload;
function WordSearch(const ASearchWord, Text: String; const ACaseSensitive: Boolean; const ADelimiters: TCharSet;
                    const AIgnoreStrInQuotes: Boolean = False; const AQuoteChar: Char = #39;
                    const ARelaxedDelim: Boolean = False): Integer; inline; overload;
function WordSearch(const ASearchWord, Text: String; const ACaseSensitive: Boolean = True;
                    const AIgnoreStrInQuotes: Boolean = False; const AQuoteChar: Char = #39;
                    const ARelaxedDelim: Boolean = False): Integer; inline; overload;
function StrOccurs(const SubStr, AStr: String; const ACaseSensitive: Boolean = False): Integer;


implementation

{ TWordParserEntry }

constructor TWordParsedEntry.Create(const AEntryStr: String; AEntryType: TWordParsedType);
begin
  inherited Create;

  EntryType := AEntryType;
  case EntryType of
    wptWord:
      EntryStr := AEntryStr;

    wptSeparator:
      {$IFNDEF CLR}
      EntryDelim := PChar(AEntryStr)^;
      {$ELSE}
      if AEntryStr.Length > 0 then
        EntryDelim := AEntryStr[1]
      else
        EntryDelim := #0;
      {$ENDIF}
  end;
end;

function TWordParsedEntry.Clone: TWordParsedEntry;
begin
  if EntryType = wptWord then
    Result := TWordParsedEntry.Create(EntryStr, EntryType)
  else
    Result := TWordParsedEntry.Create(EntryDelim, EntryType);
end;

{ TWordParsedList }

function IsSep(const C: Char; const AWordDelims: TCharSet; const ARelaxedDelim: Boolean = False): Boolean; inline;
begin
  if Length(AWordDelims) = 0 then
    Result := C.IsControl or C.IsWhiteSpace or (C = #0) or C.IsInArray(AWordDelims)
  else
    Result := (ARelaxedDelim and (C.IsControl or C.IsWhiteSpace or (C = #0))) or C.IsInArray(AWordDelims);
end;

function IsSepNonNull(const C: Char; const AWordDelims: TCharSet; const ARelaxedDelim: Boolean = False): Boolean; inline;
begin
  if Length(AWordDelims) = 0 then
    Result := (C <> #0) and (C.IsControl or C.IsWhiteSpace or C.IsInArray(AWordDelims))
  else
    Result := (C <> #0) and ((ARelaxedDelim and (C.IsControl or C.IsWhiteSpace)) or C.IsInArray(AWordDelims));
end;

function IsEqualWords(const AStr1, AStr2: String; ACaseSensitive: Boolean): Boolean; inline;
begin
  Result := False;
  if AStr1.Length = AStr2.Length then
  begin
    if ACaseSensitive then
      Result := AnsiSameStr(AStr1, AStr2)
    else
      Result := AnsiSameText(AStr1, AStr2);
  end;
end;

constructor TWordParsedList.Create(const AStr: String; const AWordDelims: TCharSet; const AIgnoreStrInQuotes: Boolean;
                                   const AQuoteChar: Char; const ARelaxedDelim: Boolean);
var
  WordLen, WordStart: Integer;
  InQuote: Boolean;
  SLen, I: Integer;
begin
  FDelims := AWordDelims;
  FIgnoreStrInQuotes := AIgnoreStrInQuotes;
  FQuoteChar := AQuoteChar;
  FRelaxedDelim := ARelaxedDelim;
  FStrLen := AStr.Length;

  inherited Create {$IFNDEF CLR} (True) {$ENDIF};

  SLen := Length(AStr);

  if (SLen = 0) then
    Exit;

  I := 1;
  WordLen := 0;
  WordStart := 0;
  InQuote := False;

  while I <= SLen do
  begin
    if (AStr[I] = AQuoteChar) and not AIgnoreStrInQuotes then
    begin
      InQuote := not InQuote;

      if WordLen = 0 then
        WordStart := I;

      Inc(WordLen);
    end
    else if IsSep(AStr[I], AWordDelims, ARelaxedDelim) and not InQuote then
    begin
      if (WordLen > 0) and (WordStart > 0) then
        Add(TWordParsedEntry.Create(Copy(AStr, WordStart, WordLen), wptWord));

      Add(TWordParsedEntry.Create(AStr[I], wptSeparator));

      WordLen := 0;
      WordStart := 0;
    end
    else
    begin
      if WordLen = 0 then
        WordStart := I;
      Inc(WordLen);
    end;

    Inc(I);
  end;

  if (WordLen > 0) and (WordStart > 0) then
    Add(TWordParsedEntry.Create(Copy(AStr, WordStart, WordLen), wptWord));
end;

function TWordParsedList.GetRangeStr(const AStart: Integer; const ALen: Integer = MaxInt): String;
var
  SB: TStringBuilder;
  Entry: TWordParsedEntry;
  i: Integer;
begin
  Result := '';

  if (AStart >= Count) or (AStart < 0) then
    Exit;

  SB := TStringBuilder.Create(FStrLen);
  try
    for i := AStart to Min((AStart + ALen), Count) - 1 do
    begin
      Entry := Self[i];
      case Entry.EntryType of
        wptWord: SB.Append(Entry.EntryStr);
        wptSeparator: SB.Append(Entry.EntryDelim);
      end;
    end;

    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TWordParsedList.WordCount: Integer;
var
  Entry: TWordParsedEntry;
begin
  Result := 0;
  for Entry in Self do
    if (Entry.EntryType = wptWord) then
      Inc(Result);
end;

function TWordParsedList.WordPosition(const AIndex: Integer): Integer;
var
  Entry: TWordParsedEntry;
  wc: Integer;
  CurrPos: Integer;
begin
  Result := 0;
  wc := 0;
  CurrPos := 1;

  for Entry in Self do
  begin
    if (Entry.EntryType = wptWord) then
    begin
      Inc(wc);

      if (wc = AIndex) then
      begin
        Result := CurrPos;
        Exit;
      end;

      Inc(CurrPos, (Entry.EntryStr.Length));
    end
    else
      Inc(CurrPos, 1);
  end;
end;

function TWordParsedList.ExtractWord(const AIndex: Integer): String;
var
  Entry: TWordParsedEntry;
  wc: Integer;
begin
  Result := '';
  wc := 0;

  for Entry in Self do
    if (Entry.EntryType = wptWord) then
    begin
      Inc(wc);

      if (wc = AIndex) then
      begin
        Result := Entry.EntryStr;
        Exit;
      end;
    end;
end;

function TWordParsedList.WordIndex(const AWord: String; ACaseSensitive: Boolean): Integer;
var
  Entry: TWordParsedEntry;
  wc: Integer;
begin
  Result := 0;
  wc := 0;

  for Entry in Self do
    if (Entry.EntryType = wptWord) then
    begin
      Inc(wc);

      if IsEqualWords(Entry.EntryStr, AWord, ACaseSensitive) then
      begin
        Result := wc;
        Exit;
      end;
    end;
end;

function TWordParsedList.PosWord(const AWord: String; const ACaseSensitive: Boolean): Integer;
var
  Entry: TWordParsedEntry;
  CurrPos: Integer;
begin
  Result := 0;
  CurrPos := 1;

  for Entry in Self do
    if (Entry.EntryType = wptWord) then
    begin
      if IsEqualWords(Entry.EntryStr, AWord, ACaseSensitive) then
      begin
        Result := CurrPos;
        Exit;
      end;

      Inc(CurrPos, (Entry.EntryStr.Length));
    end
    else
      Inc(CurrPos, 1);
end;

{$IFNDEF CLR}
procedure TWordParsedList.InsertEntry(const Index: Integer;
  const EntryStr: String; const EntryType: TWordParsedType);
begin
  Insert(Index, TWordParsedEntry.Create(EntryStr, EntryType));
end;

procedure TWordParsedList.ReplaceWord(const AIndex: Integer;
  const ANewValue: String);
var
  Entry: TWordParsedEntry;
  wc: Integer;
begin
  wc := 0;

  for Entry in Self do
    if (Entry.EntryType = wptWord) then
    begin
      Inc(wc);

      if (wc = AIndex) then
      begin
        Entry.EntryStr := ANewValue;
        Exit;
      end;
    end;
end;

function TWordParsedList.EntryIndex(const AEntryStr: String;
  const AEntryType: TWordParsedType; const ACaseSensitive: Boolean; const AStartEntry: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := AStartEntry to Count - 1 do
  begin
    if (Items[I].EntryType = AEntryType) and IsEqualWords(Items[I].EntryStr, AEntryStr, ACaseSensitive) then
    begin
      Result := I;
      break;
    end;
  end;
end;
{$ENDIF}

function TWordParsedList.IsWordPresent(const AWord: String; const ACaseSensitive: Boolean): Boolean;
var
  Entry: TWordParsedEntry;
begin
  Result := False;

  for Entry in Self do
    if (Entry.EntryType = wptWord) and IsEqualWords(Entry.EntryStr, AWord, ACaseSensitive) then
    begin
      Result := True;
      Exit;
    end;
end;

function TWordParsedList.WordOccurs(const AWord: String; const ACaseSensitive: Boolean): Integer;
var
  Entry: TWordParsedEntry;
begin
  Result := 0;

  for Entry in Self do
    if (Entry.EntryType = wptWord) then
      if IsEqualWords(Entry.EntryStr, AWord, ACaseSensitive) then
        Inc(Result);
end;

function TWordParsedList.WordReplace(const ASearchWord, AReplaceWord: String; const ACaseSensitive: Boolean): String;
var
  ParsedWord: TWordParsedList;
  i: Integer;
  SB: TStringBuilder;
  Entry: TWordParsedEntry;
begin
  Result := '';

  if (ASearchWord = '') or (AReplaceWord = '') or (Count = 0) then
    Exit;

  ParsedWord := TWordParsedList.Create(ASearchWord, FDelims, FIgnoreStrInQuotes, FQuoteChar, FRelaxedDelim);
  try
    if (ParsedWord.WordCount = 0) then
      Exit;

    SB := TStringBuilder.Create(FStrLen);
    try
      i := 0;
      while i < Count do
      begin
        Entry := Self[i];
        if IsEqualWords(GetRangeStr(i, ParsedWord.Count), ASearchWord, ACaseSensitive) then
        begin
          SB.Append(AReplaceWord);
          Inc(i, ParsedWord.Count);
        end
        else
        begin
          case Entry.EntryType of
            wptWord: SB.Append(Entry.EntryStr);
            wptSeparator: SB.Append(Entry.EntryDelim);
          end;
          Inc(i);
        end;
      end;

      Result := SB.ToString;
    finally
      SB.Free;
    end;
  finally
    ParsedWord.Free;
  end;
end;


function TWordParsedList.GetCloneArray: TWordParsedArray;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := Self[i].Clone;
end;

function TWordParsedList.PersistentWordReplace(const ASearchWord, AReplaceWord: String; const ACaseSensitive: Boolean): Boolean;
var
  ParsedSearch, ParsedReplace: TWordParsedList;
  i, j, k, wc: Integer;
  Entry: TWordParsedEntry;
  Match: Boolean;
  Clones: TWordParsedArray;
begin
  Result := False;

  if (ASearchWord = '') or (AReplaceWord = '') or (Count = 0) then
    Exit;

  ParsedSearch := TWordParsedList.Create(ASearchWord, FDelims, FIgnoreStrInQuotes, FQuoteChar, FRelaxedDelim);
  ParsedReplace := TWordParsedList.Create(AReplaceWord, FDelims, FIgnoreStrInQuotes, FQuoteChar, FRelaxedDelim);
  try
    wc := ParsedSearch.WordCount;
    if (wc = 0) then
      Exit;

    i := 0;
    while i < Count do
    begin
      Entry := Self[i];

      if (Entry.EntryType = wptWord) and (IsEqualWords(Entry.EntryStr, ParsedSearch.ExtractWord(1), ACaseSensitive)) then
      begin
        Match := True;
        j := 2;
        k := 1;
        while Match and (j <= wc) and ((i + k) < Count) do
        begin
          Entry := Self[i + k];
          if (Entry.EntryType <> wptWord) then
          begin
            if (i + k) = Count - 1 then
              Match := False
            else
              Inc(k);
          end
          else
          begin
            if IsEqualWords(Entry.EntryStr, ParsedSearch.ExtractWord(j), ACaseSensitive) then
            begin
              Inc(k);
              if j = wc then
                Break
              else
              begin
                Inc(j);

                if (i + k) = Count then
                  Match := False;
              end;
            end
            else
              Match := False;
          end;
        end;

        if Match then
        begin
          Result := True;
          {$IFNDEF CLR}
          DeleteRange(i, k);
          {$ELSE}
          RemoveRange(i, k);
          {$ENDIF}

          Clones := ParsedReplace.GetCloneArray;
          if Length(Clones) > 0 then
          begin
            InsertRange(i, Clones);
            Inc(i, Length(Clones));
          end;
        end;
      end;

      Inc(i);
    end;

  finally
    ParsedSearch.Free;
    ParsedReplace.Free;
  end;
end;

{$IFNDEF CLR}
function TWordParsedList.GetStringArray: TArray<String>;
var
  Entry: TWordParsedEntry;
  i: Integer;
begin
  SetLength(Result, WordCount);
  i := 0;

  for Entry in Self do
    if (Entry.EntryType = wptWord) then
    begin
      Result[i] := Entry.EntryStr;
      Inc(i);
    end;
end;

function TWordParsedList.ToString: String;
begin
  Result := GetRangeStr(0);
end;
{$ENDIF}

function ParseWords(const S: String; const AWordDelims: TCharSet; const AIgnoreStrInQuotes: Boolean = False;
                    const AQuoteChar: Char = #39; const ARelaxedDelim: Boolean = False): TWordParsedList;
begin
  Result := TWordParsedList.Create(S, AWordDelims, AIgnoreStrInQuotes, AQuoteChar, ARelaxedDelim);
end;

function WordCount(const S: String; const AWordDelims: TCharSet; const AIgnoreStrInQuotes: Boolean = False;
                   const AQuoteChar: Char = #39; const ARelaxedDelim: Boolean = False): Integer;
var
  Parsed: TWordParsedList;
begin
  Parsed := ParseWords(S, AWordDelims, AIgnoreStrInQuotes, AQuoteChar, ARelaxedDelim);
  try
    Result := Parsed.WordCount;
  finally
    Parsed.Free;
  end;
end;

function WordPosition(const N: Integer; const S: String; const AWordDelims: TCharSet;
                      const AIgnoreStrInQuotes: Boolean = False; const AQuoteChar: Char = #39;
                      const ARelaxedDelim: Boolean = False): Integer;
var
  Parsed: TWordParsedList;
begin
  Parsed := ParseWords(S, AWordDelims, AIgnoreStrInQuotes, AQuoteChar, ARelaxedDelim);
  try
    Result := Parsed.WordPosition(N);
  finally
    Parsed.Free;
  end;
end;

function ExtractWord(const N: Integer; const S: String; const AWordDelims: TCharSet; const AIgnoreStrInQuotes: Boolean = False;
                     const AQuoteChar: Char = #39; const ARelaxedDelim: Boolean = False): String;
var
  Parsed: TWordParsedList;
begin
  Parsed := ParseWords(S, AWordDelims, AIgnoreStrInQuotes, AQuoteChar, ARelaxedDelim);
  try
    Result := Parsed.ExtractWord(N);
  finally
    Parsed.Free;
  end;
end;

function FirstWord(const S: String; const AWordDelims: TCharSet; const AIgnoreStrInQuotes: Boolean = False; const AQuoteChar: Char = #39; const ARelaxedDelim: Boolean = False): String; inline;
var
  Parsed: TWordParsedList;
begin
  Parsed := ParseWords(S, AWordDelims, AIgnoreStrInQuotes, AQuoteChar, ARelaxedDelim);
  try
    Result := Parsed.ExtractWord(1);
  finally
    Parsed.Free;
  end;
end;

function LastWord(const S: String; const AWordDelims: TCharSet; const AIgnoreStrInQuotes: Boolean = False; const AQuoteChar: Char = #39; const ARelaxedDelim: Boolean = False): String; inline;
var
  Parsed: TWordParsedList;
begin
  Parsed := ParseWords(S, AWordDelims, AIgnoreStrInQuotes, AQuoteChar, ARelaxedDelim);
  try
    Result := Parsed.ExtractWord(Parsed.WordCount);
  finally
    Parsed.Free;
  end;
end;

function WordIndex(const W, S: String; const AWordDelims: TCharSet; const ACaseSensitive: Boolean = False;
                   const AIgnoreStrInQuotes: Boolean = False; const AQuoteChar: Char = #39;
                   const ARelaxedDelim: Boolean = False): Integer;
var
  Parsed: TWordParsedList;
begin
  Parsed := ParseWords(S, AWordDelims, AIgnoreStrInQuotes, AQuoteChar, ARelaxedDelim);
  try
    Result := Parsed.WordIndex(W, ACaseSensitive);
  finally
    Parsed.Free;
  end;
end;

function PosWord(const AWord, AText: String; const AWordDelims: TCharSet; const ACaseSensitive: Boolean = True;
                 const AIgnoreStrInQuotes: Boolean = False; const AQuoteChar: Char = #39;
                 const ARelaxedDelim: Boolean = False): Integer;
var
  Parsed: TWordParsedList;
begin
  Parsed := ParseWords(AText, AWordDelims, AIgnoreStrInQuotes, AQuoteChar, ARelaxedDelim);
  try
    Result := Parsed.PosWord(AWord, ACaseSensitive);
  finally
    Parsed.Free;
  end;
end;

function IsTextPresent(const AText, AStr: String; const AWordDelims: TCharSet; const ACaseSensitive: Boolean = False;
                       const AIgnoreStrInQuotes: Boolean = False; const AQuoteChar: Char = #39;
                       const ARelaxedDelim: Boolean = True): Boolean;
var
  Parsed: TWordParsedList;
begin
  Parsed := ParseWords(AStr, AWordDelims, AIgnoreStrInQuotes, AQuoteChar, ARelaxedDelim);
  try
    Result := Parsed.IsWordPresent(AText, ACaseSensitive);
  finally
    Parsed.Free;
  end;
end;

function WordOccurs(const AWord, AStr: String; const AWordDelims: TCharSet; const ACaseSensitive: Boolean;
                    const AIgnoreStrInQuotes: Boolean = False; const AQuoteChar: Char = #39;
                    const ARelaxedDelim: Boolean = False): Integer;
var
  Parsed: TWordParsedList;
begin
  Parsed := ParseWords(AStr, AWordDelims, AIgnoreStrInQuotes, AQuoteChar, ARelaxedDelim);
  try
    Result := Parsed.WordOccurs(AWord, ACaseSensitive);
  finally
    Parsed.Free;
  end;
end;

function WordReplace(var Text: String; const ASearchWord, AReplaceWord: String; const ACaseSensitive: Boolean;
                     const ADelimiters: TCharSet; const AIgnoreStrInQuotes: Boolean = False;
                     const AQuoteChar: Char = #39; const ARelaxedDelim: Boolean = False): Boolean;
var
  Parsed: TWordParsedList;
begin
  Parsed := ParseWords(Text, ADelimiters, AIgnoreStrInQuotes, AQuoteChar, ARelaxedDelim);
  try
    Result := Parsed.PersistentWordReplace(ASearchWord, AReplaceWord, ACaseSensitive);

    if Result then
      Text := Parsed.ToString;
  finally
    Parsed.Free;
  end;
end;

function WordReplace(var Text: String; const ASearchWord, AReplaceWord: String; const ACaseSensitive: Boolean = True;
                     const AIgnoreStrInQuotes: Boolean = False; const AQuoteChar: Char = #39;
                     const ARelaxedDelim: Boolean = False): Boolean;
{$IFNDEF CLR}
begin
  Result := WordReplace(Text, ASearchWord, AReplaceWord, ACaseSensitive, [], AIgnoreStrInQuotes, AQuoteChar, ARelaxedDelim);
{$ELSE}
var
  EmptyArr: TCharSet;
begin
  SetLength(EmptyArr, 0);
  Result := WordReplace(Text, ASearchWord, AReplaceWord, ACaseSensitive, EmptyArr, AIgnoreStrInQuotes, AQuoteChar, ARelaxedDelim);
{$ENDIF}
end;

function WordSearch(const ASearchWord, Text: String; const ACaseSensitive: Boolean; const ADelimiters: TCharSet;
                    const AIgnoreStrInQuotes: Boolean = False; const AQuoteChar: Char = #39;
                    const ARelaxedDelim: Boolean = False): Integer;
begin
  Result := PosWord(ASearchWord, Text, ADelimiters, ACaseSensitive, AIgnoreStrInQuotes, AQuoteChar, ARelaxedDelim);
end;

function WordSearch(const ASearchWord, Text: String; const ACaseSensitive: Boolean = True;
                    const AIgnoreStrInQuotes: Boolean = False; const AQuoteChar: Char = #39;
                    const ARelaxedDelim: Boolean = False): Integer; inline; overload;
{$IFNDEF CLR}
begin
  Result := WordSearch(ASearchWord, Text, ACaseSensitive, [], AIgnoreStrInQuotes, AQuoteChar, ARelaxedDelim);
{$ELSE}
var
  EmptyArr: TCharSet;
begin
  SetLength(EmptyArr, 0);
  Result := WordSearch(ASearchWord, Text, ACaseSensitive, EmptyArr, AIgnoreStrInQuotes, AQuoteChar, ARelaxedDelim);
{$ENDIF}
end;

function StrOccurs(const SubStr, AStr: String; const ACaseSensitive: Boolean = False): Integer;
var
  {$IFNDEF CLR}
  P: PChar;
  {$ELSE}
  I: Integer;
  {$ENDIF}
  StrSearchOptions: TStringSearchOptions;
begin
  Result := 0;
  StrSearchOptions := [soDown];

  if ACaseSensitive then
    Include(StrSearchOptions, soMatchCase);

  {$IFNDEF CLR}
  P := PChar(AStr);
  while Assigned(P) do
  begin
    P := SearchBuf(P, StrLen(P), 0, 0, SubStr, StrSearchOptions);

    if Assigned(P) then
    begin
      Inc(P);
      Inc(Result);
    end;
  end;
  {$ELSE}
  I := 0;
  repeat
    I := SearchBuf(AStr, I, AStr.Length - I, SubStr, StrSearchOptions);
    if (I > 0) then
      Inc(Result);
  until I = 0;
  {$ENDIF}
end;

procedure NTLMSplitUserName(const LogonString: String; var AUserName, ADomain: String);

  function AcquireDomainFromUPN(const AUserPrincipalName: String; var AUserName, ADomain: String): boolean;
  var
    NT: IADsNameTranslate;
    NewLogonString: String;
  begin
    Result := False;
    try
      NT := CreateOleObject('NameTranslate') as IADsNameTranslate;
      try
        NT.Init(ADS_NAME_INITTYPE_GC, '');
      except
        on e: exception do
          raise Exception.Create('Error Translating UPN To NT4 format during Init: ' + e.Message);
      end;
      try
        NT.Set_(ADS_NAME_TYPE_USER_PRINCIPAL_NAME, AUserPrincipalName);
      except
        on e: exception do
        begin
          raise Exception.Create('Error Translating UPN To NT4 format during Set: ' + e.Message);
        end;
      end;
      try
        NewLogonString := NT.Get(ADS_NAME_TYPE_NT4);
      except
        on e: exception do
        begin
          raise Exception.Create('Error Translating UPN To NT4 format during Get: ' + e.Message);
        end;
      end;
      Result := (Pos('\', NewLogonString) > 0);
    except
    end;
    if Result then
    begin
      NTLMSplitUserName(NewLogonString, AUserName, ADomain);
      AUserName := LogonString;
    end;
  end;
begin
  if (Pos('\', LogonString) > 0) then
  begin

    if Pos('\', LogonString) = 1 then
    begin
      AUserName := ExtractWord(1, LogonString, ['\']);
      ADomain := '';
    end
    else
    begin
      ADomain := ExtractWord(1, LogonString, ['\']);
      AUserName := ExtractWord(2, LogonString, ['\']);
    end;

  end else if (Pos('@', LogonString) > 0) then
  begin
    if not AcquireDomainFromUPN(LogonString, AUserName, ADomain) then //If can't translate so keep doing as always did.
    begin
      ADomain := ExtractWord(2, LogonString, ['@']);
      AUserName := LogonString;
    end;
  end else
  begin
    AUserName := LogonString;
    ADomain := '';
  end;
end;

end.
