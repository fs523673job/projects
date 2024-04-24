unit unTestSQL;

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.RegularExpressions
  ;


function LinearizeSQL(const ASQL: String): String;

function AnaliseSQLClausesOR_StringList(var SQLText: string): TStringList;
function AnaliseSQLClausesOR(const ASQL: string): Boolean;


implementation

uses
  System.Generics.Collections;

function LinearizeSQL(const ASQL: String): String;
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

(***************************************************************************************************
  Analise SQL Clause OR String List
***************************************************************************************************)

function AnaliseSQLClausesOR_StringList(var SQLText: string): TStringList;

const
  SECURITY_TAG_BEGIN = '/*AUTOEMPLOYEEFILTER';

type
  TExcerptStructure = record
    IndexBegin : Integer;
    IndexEnd   : Integer;
    SQLSnipped : String;
  end;

var
  AnalysSnipped : TStringList;
  Snipped       : String;
  SQLSanitized  : String;

  function ExistSecurityTagInSnippedSQL(const ASQLSnipped: String; const AAnalysSnipped: TStringList): Boolean;
  begin
    Result := (Pos(SECURITY_TAG_BEGIN, ASQLSnipped.ToUpper) > 0);
    if Result  then
      AAnalysSnipped.Add(ASQLSnipped);
  end;

  procedure RemoveSnippetInSQL(var SQLText: String; const OutSnipped: TStringList; const AAnalysSnipped: TStringList; AIgnoratedChar: TSysCharSet; AKeyWordBegin, AKeyWordEnd: String);
  var
    SizeSQL, PosSQL  : Integer;
    ExcerptStructure : TExcerptStructure;
    StackSnippetPos  : TStack<TExcerptStructure>;
    KeyWord          : String;
    InQuoted         : Boolean;
    InComment        : Boolean;
  begin
    SizeSQL := Length(SQLText);
    StackSnippetPos := TStack<TExcerptStructure>.Create;
    try
      PosSQL    := 1;
      InQuoted  := False;
      InComment := False;
      while PosSQL <= SizeSQL  do
      begin
        if (SQLText[PosSQL] = '/') and (SQLText[PosSQL + 1] = '*') then
        begin
          InComment := not InComment;
          Inc(PosSQL, 2);
          KeyWord := EmptyStr;
          Continue;
        end
        else if (SQLText[PosSQL] = '*') and (SQLText[PosSQL + 1] = '/') then
        begin
          InComment := not InComment;
          Inc(PosSQL, 2);
          KeyWord := EmptyStr;
          Continue;
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
            ExcerptStructure.IndexEnd := PosSQL - Length(KeyWord) + 1;
            ExcerptStructure.SQLSnipped := Copy(SQLText, ExcerptStructure.IndexBegin, (ExcerptStructure.IndexEnd + 1) - ExcerptStructure.IndexBegin);

            if not ExistSecurityTagInSnippedSQL(ExcerptStructure.SQLSnipped, AAnalysSnipped) then
              OutSnipped.Add(ExcerptStructure.SQLSnipped);

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

        if not ExistSecurityTagInSnippedSQL(ExcerptStructure.SQLSnipped, AAnalysSnipped) then
          OutSnipped.Add(ExcerptStructure.SQLSnipped);

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
  SQLSanitized := LinearizeSQL(SQLText);

  Result        := TStringList.Create;
  AnalysSnipped := TStringList.Create;

  RemoveSnippetInSQL(SQLSanitized, Result, AnalysSnipped, ['<', '>', '/', '-', '+', '*', '=', '|', ','], '(', ')');
  RemoveSnippetInSQL(SQLSanitized, Result, AnalysSnipped, ['<', '>', '/', '-', '+', '*', '=', '|', ',', '(', ')'], 'CASE', 'END');
  RemoveSnippetInSQL(SQLSanitized, Result, AnalysSnipped, ['<', '>', '/', '-', '+', '*', '=', '|', ',', '(', ')'], 'HAVING', 'UNION');
  RemoveSnippetInSQL(SQLSanitized, Result, AnalysSnipped, ['<', '>', '/', '-', '+', '*', '=', '|', ',', '(', ')'], 'HAVING', 'ORDER');

  for Snipped in AnalysSnipped do
    Result.Add(Snipped);

  SQLText := SQLSanitized;
end;

(****************************************************************************************************
  ANALISE SQL CLAUSE OR
*****************************************************************************************************)

function AnaliseSQLClausesOR(const ASQL: string): Boolean;

const
  SECURITY_TAG_BEGIN = '/*AUTOEMPLOYEEFILTER';

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
    if (Pos(SECURITY_TAG_BEGIN, ASQLSnipped.ToUpper) > 0) then
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
    SizeSQL := Length(SQLText);
    StackSnippetPos := TStack<TExcerptStructure>.Create;
    try
      PosSQL    := 1;
      InQuoted  := False;
      InComment := False;
      while PosSQL <= SizeSQL  do
      begin
        if (SQLText[PosSQL] = '/') and (SQLText[PosSQL + 1] = '*') then
        begin
          InComment := not InComment;
          Inc(PosSQL);
          KeyWord := '/*';
        end
        else if (SQLText[PosSQL] = '*') and (SQLText[PosSQL + 1] = '/') then
        begin
          InComment := not InComment;
          Inc(PosSQL);
          KeyWord := '*/';
          KeyWord := EmptyStr;
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
            ExcerptStructure.IndexEnd := PosSQL - Length(KeyWord) + 1;
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

end.
