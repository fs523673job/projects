unit unSQLAnalyser;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.StrUtils,
  System.Generics.Collections,
  System.RegularExpressions
  ;

type
  TValidationOption = (voNone, voValidParenthesizeOrClause, voValidSecurityTag);
  TValidationOptions = set of TValidationOption;

  TSQLAnalyzer = class
  private
    FValidationOptions: TValidationOptions;

    const
      TAG_SECURITY_START = '/*AUTOEMPLOYEEFILTER=';
      TAG_SECURITY_END = '*/';

    function TableExists(const TableName: string; const Tables: TArray<string>): Boolean;
    function ExtractAllSecurityTags(const ASQL: String): TArray<string>;
    function ExtractTableNamesFromScript(const ASQL: string): TArray<string>;
    function ExtractTablesInSecurityTag(const ASecurityTags: TArray<string>): TArray<string>;
    function AnaliseSQLClausesOR(const ASQL: String): Boolean;
    function HasUnparenthesizedOrClauses(const ASQL: String): Boolean;
    function HasFailSecurityTag(const ASQL: String): Boolean;
    function LinearizeSQL(const ASQL: String): String;
  public
    function HasSQLErrors(const ASQL: String; out AMsg: String): Boolean;

    property ValidationOption: TValidationOptions read FValidationOptions write FValidationOptions;
  end;

implementation

{ TSQLAnalyzer }

function TSQLAnalyzer.ExtractAllSecurityTags(const ASQL: string): TArray<string>;
var
  TagStartPos, TagEndPos, LastEndPos: Integer;
  TagList: TList<string>;
begin
  TagList := TList<string>.Create;
  try
    LastEndPos := 1;

    repeat
      TagStartPos := PosEx(TAG_SECURITY_START, ASQL, LastEndPos);
      if TagStartPos = 0 then
        Break;

      TagEndPos := PosEx(TAG_SECURITY_END, ASQL, TagStartPos);
      if TagEndPos = 0 then
        Break;

      TagList.Add(Copy(ASQL, TagStartPos, TagEndPos - TagStartPos + 2));

      LastEndPos := TagEndPos + 2;
    until False;

    Result := TagList.ToArray;
  finally
    TagList.Free;
  end;
end;

function TSQLAnalyzer.AnaliseSQLClausesOR(const ASQL: string): Boolean;
type
  TExcerptStructure = record
    IndexBegin : Integer;
    IndexEnd   : Integer;
    SQLSnipped : String;
  end;

var
  SQLSanitized  : String;
  AnalysSnipped : TStringList;
  Snipped       : String;

  procedure AddExistSecurityTagInSnippedSQL(const ASQLSnipped: String; const AAnalysSnipped: TStringList);
  begin
    if (Pos(TAG_SECURITY_START, ASQLSnipped.ToUpper) > 0) then
      AAnalysSnipped.Add(Copy(ASQLSnipped, 2, Length(ASQLSnipped) - 2));
  end;

  procedure RemoveSnippetInSQL(var SQLText: String; const AAnalysSnipped: TStringList; AIgnoratedChar: TSysCharSet; AKeyWordBegin, AKeyWordEnd: String);
  var
    SizeSQL, PosSQL  : Integer;
    ExcerptStructure : TExcerptStructure;
    StackSnippetPos  : TStack<TExcerptStructure>;
    KeyWord          : String;
    InQuoted         : Boolean;
    InComment        : Boolean;
  begin
    SQLText := SQLText.Trim;
    SizeSQL := Length(SQLText);
    StackSnippetPos := TStack<TExcerptStructure>.Create;
    try
      PosSQL    := 1;
      InQuoted  := False;
      InComment := False;
      while PosSQL <= SizeSQL  do
      begin
        if (SQLText[PosSQL] = '/') and ((PosSQL + 1) <= SizeSQL) and (SQLText[PosSQL + 1] = '*') then
        begin
          InComment := not InComment;
          Inc(PosSQL);
          KeyWord := '/*';
        end
        else if (SQLText[PosSQL] = '*') and ((PosSQL + 1) <= SizeSQL) and (SQLText[PosSQL + 1] = '/') then
        begin
          InComment := not InComment;
          Inc(PosSQL);
          KeyWord := '*/';
        end
        else if (SQLText[PosSQL] = '''') or (SQLText[PosSQL] = '"') then
        begin
          InQuoted := not InQuoted;
          Inc(PosSQL);
          KeyWord := EmptyStr;
          Continue
        end
        else if CharInSet(SQLText[PosSQL], AIgnoratedChar) then
        begin
          Inc(PosSQL);
          KeyWord := EmptyStr;
          Continue;
        end
        else if SQLText[PosSQL] = ' ' then
        begin
          Inc(PosSQL);
          KeyWord := EmptyStr;
          Continue;
        end
        else if CharInSet(SQLText[PosSQL], ['(', ')']) then
          KeyWord := SQLText[PosSQL]
        else
          KeyWord := KeyWord + SQLText[PosSQL];

        if (IndexStr(KeyWord.ToUpper, [AKeyWordBegin, AKeyWordEnd]) >= 0) and (not InQuoted) then
        begin
          if (KeyWord.ToUpper = AKeyWordBegin) then
          begin
            ExcerptStructure.IndexBegin := PosSQL - Length(KeyWord) + 1;
            StackSnippetPos.Push(ExcerptStructure);
            KeyWord := EmptyStr;
          end
          else if (KeyWord.ToUpper = AKeyWordEnd) and (StackSnippetPos.Count > 0) and (not InQuoted) then
          begin
            ExcerptStructure := StackSnippetPos.Pop();
            ExcerptStructure.IndexEnd := PosSQL;
            ExcerptStructure.SQLSnipped := Copy(SQLText, ExcerptStructure.IndexBegin, (ExcerptStructure.IndexEnd + 1) - ExcerptStructure.IndexBegin);

            if Assigned(AAnalysSnipped) then
              AddExistSecurityTagInSnippedSQL(ExcerptStructure.SQLSnipped, AAnalysSnipped);

            SQLText := StringReplace(SQLText, ExcerptStructure.SQLSnipped, '', [rfReplaceAll]);

            PosSQL  := 0;
            SizeSQL := Length(SQLText);
            KeyWord := EmptyStr;

            StackSnippetPos.Clear();
          end;
        end;

        Inc(PosSQL);
      end;

      if StackSnippetPos.Count > 0 then
      begin
        ExcerptStructure := StackSnippetPos.Pop();
        ExcerptStructure.IndexEnd := PosSQL - Length(KeyWord) + 1;
        ExcerptStructure.SQLSnipped := Copy(SQLText, ExcerptStructure.IndexBegin, (ExcerptStructure.IndexEnd + 1) - ExcerptStructure.IndexBegin);

        if Assigned(AAnalysSnipped) then
          AddExistSecurityTagInSnippedSQL(ExcerptStructure.SQLSnipped, AAnalysSnipped);

        SQLText := StringReplace(SQLText, ExcerptStructure.SQLSnipped, '', [rfReplaceAll]);

        StackSnippetPos.Clear();
      end;
    finally
       StackSnippetPos.Free;
    end;
  end;

  function ContainsValidOR(const Condition: String): Boolean;
  begin
    Result := Pos(' OR ', Condition.ToUpper) > 0;
    Result := Result or (Pos(')OR(', Condition.ToUpper) > 0);
    Result := Result or (Pos(')OR', Condition.ToUpper) > 0);
    Result := Result or (Pos('OR(', Condition.ToUpper) > 0);
    Result := Result or (Pos(')OR ', Condition.ToUpper) > 0);
    Result := Result or (Pos(' OR)', Condition.ToUpper) > 0);
  end;

  function ExtractInternalOperators(const Expr: string): String;
  var
    c, LevelParentheses: Integer;
    TempResult: string;
  begin
    TempResult := '';
    LevelParentheses := 0;

    for c := 1 to Length(Expr) do
    begin
      if Expr[c] = '(' then
        Inc(LevelParentheses)
      else if Expr[c] = ')' then
        Dec(LevelParentheses)
      else if LevelParentheses = 0 then
        TempResult := TempResult + Expr[c];
    end;

    if ContainsValidOR(TempResult) then
      Result := 'OR'
    else if LevelParentheses > 0 then
      Result := 'PARENTHESES'
    else
      Result := '';
  end;

begin
  SQLSanitized  := LinearizeSQL(ASQL);
  AnalysSnipped :=  TStringList.Create;
  try
    RemoveSnippetInSQL(SQLSanitized, AnalysSnipped, ['<', '>', '/', '-', '+', '*', '=', '|', ',', '(', ')'], '/*', '*/');
    RemoveSnippetInSQL(SQLSanitized, AnalysSnipped, ['<', '>', '/', '-', '+', '*', '=', '|', ','], '(', ')');
    RemoveSnippetInSQL(SQLSanitized, AnalysSnipped, ['<', '>', '/', '-', '+', '*', '=', '|', ',', '(', ')'], 'CASE', 'END');
    RemoveSnippetInSQL(SQLSanitized, AnalysSnipped, ['<', '>', '/', '-', '+', '*', '=', '|', ',', '(', ')'], 'HAVING', 'UNION');
    RemoveSnippetInSQL(SQLSanitized, AnalysSnipped, ['<', '>', '/', '-', '+', '*', '=', '|', ',', '(', ')'], 'HAVING', 'ORDER');

    Result := not ExtractInternalOperators(SQLSanitized).IsEmpty;

    if not Result then
    begin
      for Snipped in AnalysSnipped do
      begin
        SQLSanitized := LinearizeSQL(Snipped);

        RemoveSnippetInSQL(SQLSanitized, nil, ['<', '>', '/', '-', '+', '*', '=', '|', ','], '(', ')');
        RemoveSnippetInSQL(SQLSanitized, nil, ['<', '>', '/', '-', '+', '*', '=', '|', ',', '(', ')'], 'CASE', 'END');
        RemoveSnippetInSQL(SQLSanitized, nil, ['<', '>', '/', '-', '+', '*', '=', '|', ',', '(', ')'], 'HAVING', 'UNION');
        RemoveSnippetInSQL(SQLSanitized, nil, ['<', '>', '/', '-', '+', '*', '=', '|', ',', '(', ')'], 'HAVING', 'ORDER');

        Result := (not ExtractInternalOperators(SQLSanitized).IsEmpty);

        if not Result then
          Break;
      end;
    end;
  finally
    AnalysSnipped.Free;
  end;
end;

function TSQLAnalyzer.HasUnparenthesizedOrClauses(const ASQL: String): Boolean;
var
  SQL: String;
  SecurityTags: TArray<string>;
begin
  Result := False;
  SQL := LinearizeSQL(ASQL);
  SecurityTags := ExtractAllSecurityTags(SQL);

  if High(SecurityTags) >= 0 then
    Result := AnaliseSQLClausesOR(SQL);
end;

function TSQLAnalyzer.LinearizeSQL(const ASQL: String): String;
var
  i: Integer;
  prevCharIsSpace: Boolean;
  auxSQL: String;
begin
  auxSQL := StringReplace(ASQL.ToUpper,   #$D#$A, ' ', [rfReplaceAll]);
  auxSQL := StringReplace(auxSQL.ToUpper, #9, ' ', [rfReplaceAll]);
  for i := 10 downto 2 do
    auxSQL := StringReplace(auxSQL.ToUpper, StringOfChar(' ', i), ' ', [rfReplaceAll]);

  prevCharIsSpace := False;

  Result := '';

  for i := 1 to Length(auxSQL) do
  begin
    if auxSQL[i] <> ' ' then
    begin
      Result := Result + auxSQL[i];
      prevCharIsSpace := False;
    end
    else
    begin
      if not prevCharIsSpace then
      begin
        Result := Result + auxSQL[i];
        prevCharIsSpace := True;
      end;
    end;
  end;
end;

function TSQLAnalyzer.TableExists(const TableName: string; const Tables: TArray<string>): Boolean;
var
  Table: string;
begin
  for Table in Tables do
  begin
    if SameText(Table, TableName) then
      Exit(True);
  end;
  Exit(False);
end;

function TSQLAnalyzer.HasFailSecurityTag(const ASQL: String): Boolean;
var
  SQL: String;
  Table, TableSec: String;
  TablesInScript: TArray<string>;
  TablesInTagSec: TArray<string>;
  SecurityTag: TArray<string>;
begin
  SQL := LinearizeSQL(ASQL);
  SecurityTag := ExtractAllSecurityTags(SQL);
  TablesInScript := ExtractTableNamesFromScript(SQL);
  TablesInTagSec := ExtractTablesInSecurityTag(SecurityTag);

  Result := True;

  for Table in TablesInScript do
  begin
    for TableSec in TablesInTagSec do
    begin
      if AnsiSameText(Table, TableSec) then
      begin
        Result := False;
        Break;
      end;
    end;

    if not Result then
      Break;
  end;
end;

function TSQLAnalyzer.HasSQLErrors(const ASQL: String; out AMsg: String): Boolean;
begin
  Result := False;

  if (voValidParenthesizeOrClause in FValidationOptions)  then
  begin
    Result := HasUnparenthesizedOrClauses(ASQL);
    if Result  then
    begin
      AMsg := 'A consulta possui a cláusula OR sem estar entre parênteses. Isso pode ocasionar problemas com os resultados e performance da consulta#11568';
      Exit;
    end;
  end;

  if voValidSecurityTag in FValidationOptions then
  begin
    Result := HasFailSecurityTag(ASQL);
    if Result then
    begin
      AMsg := 'Relatório ou consulta não autorizado para geração neste perfil#11579';
      Exit;
    end;
  end;
end;

function TSQLAnalyzer.ExtractTableNamesFromScript(const ASQL: string): TArray<string>;
var
  Regex: TRegEx;
  Match: TMatch;
  SQL: string;
begin
  SetLength(Result, 0);

  SQL := LowerCase(ASQL);

  Regex := TRegEx.Create('\b(?:from|join|inner join|left join|right join|,)\b\s*(\w*]*)', [roIgnoreCase]);

  Match := Regex.Match(Regex.Replace(SQL, '\s*,\s*',','));

  while Match.Success do
  begin
    Result := Result + [Match.Groups[1].Value];
    Match := Match.NextMatch;
  end;
end;


function TSQLAnalyzer.ExtractTablesInSecurityTag(const ASecurityTags: TArray<string>): TArray<string>;
var
  Tag: string;
  TablesInTag: TArray<string>;
  Table: string;
  TagContent: string;
begin
  SetLength(Result, 0);

  for Tag in ASecurityTags do
  begin
    if Pos(TAG_SECURITY_START, Tag) = 1 then
    begin
      TagContent := Copy(Tag, Length(TAG_SECURITY_START) + 1, Length(Tag) - Length(TAG_SECURITY_START) - 2);

      TablesInTag := SplitString(TagContent, ',');

      for Table in TablesInTag do
      begin
        if not TableExists(Table, Result) then
          Result := Result + [Table];
      end;
    end;
  end;
end;


end.
