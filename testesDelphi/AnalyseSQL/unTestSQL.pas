unit unTestSQL;

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.RegularExpressions
  ;


function LinearizeSQL(const ASQL: String): String;

function AnaliseSQLClausesOR_StringList(var SQLText: string): TStringList; overload;
function AnaliseSQLClausesOR(const ASQL: string): Boolean; overload;


implementation

uses
  System.Generics.Collections;

function LinearizeSQL(const ASQL: String): String;
var
  i: Integer;
  prevCharIsSpace: Boolean;
  auxSQL: String;
begin
  auxSQL := StringReplace(ASQL.ToUpper, #$D#$A, ' ', [rfReplaceAll]);
  auxSQL := StringReplace(ASQL.ToUpper, #9, ' ', [rfReplaceAll]);

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

function AnaliseSQLClausesOR_StringList(var SQLText: string): TStringList;
type
  TParenthesesSQL = record
    IndexBegin : Integer;
    IndexEnd   : Integer;
    SQLSnipped : String;
  end;

var
  Conditions   : TStringList;
  Condition    : String;
  SQLSanitized : String;

  function RemoveSnippetInParentheses(var SQLText: string): TStringList;
  var
    SizeSQL, PosSQL     : Integer;
    ParenthesesSQL      : TParenthesesSQL;
    StackParenthesesPos : TStack<TParenthesesSQL>;
  begin
    SizeSQL := Length(SQLText);
    Result := TStringList.Create;
    StackParenthesesPos := TStack<TParenthesesSQL>.Create;
    try
      PosSQL := 1;
      while PosSQL <= SizeSQL  do
      begin
        if CharInSet(SQLText[PosSQL], ['(', ')']) then
        begin
          if SQLText[PosSQL] = '(' then
          begin
            ParenthesesSQL.IndexBegin := PosSQL;
            StackParenthesesPos.Push(ParenthesesSQL);
          end
          else if (SQLText[PosSQL] = ')') and (StackParenthesesPos.Count > 0) then
          begin
            ParenthesesSQL := StackParenthesesPos.Pop();
            ParenthesesSQL.IndexEnd := PosSQL;
            ParenthesesSQL.SQLSnipped := Copy(SQLText, ParenthesesSQL.IndexBegin, (ParenthesesSQL.IndexEnd + 1) - ParenthesesSQL.IndexBegin);

            Result.Add(ParenthesesSQL.SQLSnipped);

            SQLText := StringReplace(SQLText, ParenthesesSQL.SQLSnipped, '', [rfReplaceAll]);

            PosSQL  := 0;
            SizeSQL := Length(SQLText);

            StackParenthesesPos.Clear();
          end;
        end;

        Inc(PosSQL);
      end;
    finally
       StackParenthesesPos.Free;
    end;
  end;

  function RemoveSnippetInCaseWhen(var SQLText: string): TStringList;
  begin
    Result := TStringList.Create;
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
  Result := RemoveSnippetInParentheses(SQLSanitized);
  SQLText := SQLSanitized;
end;

function AnaliseSQLClausesOR(const ASQL: string): Boolean;

type
  TParenthesesSQL = record
    IndexBegin : Integer;
    IndexEnd   : Integer;
    SQLSnipped : String;
  end;

var
  Conditions   : TStringList;
  Condition    : String;
  SQLSanitized : String;

  function RemoveSnippetInParentheses(var SQLText: string): TStringList;
  var
    SizeSQL, PosSQL     : Integer;
    ParenthesesSQL      : TParenthesesSQL;
    StackParenthesesPos : TStack<TParenthesesSQL>;
  begin
    SizeSQL := Length(SQLText);
    Result := TStringList.Create;
    StackParenthesesPos := TStack<TParenthesesSQL>.Create;
    try
      PosSQL := 1;
      while PosSQL <= SizeSQL  do
      begin
        if CharInSet(SQLText[PosSQL], ['(', ')']) then
        begin
          if SQLText[PosSQL] = '(' then
          begin
            ParenthesesSQL.IndexBegin := PosSQL;
            StackParenthesesPos.Push(ParenthesesSQL);
          end
          else if (SQLText[PosSQL] = ')') and (StackParenthesesPos.Count > 0) then
          begin
            ParenthesesSQL := StackParenthesesPos.Pop();
            ParenthesesSQL.IndexEnd := PosSQL;
            ParenthesesSQL.SQLSnipped := Copy(SQLText, ParenthesesSQL.IndexBegin, (ParenthesesSQL.IndexEnd + 1) - ParenthesesSQL.IndexBegin);

            Result.Add(ParenthesesSQL.SQLSnipped);

            SQLText := StringReplace(SQLText, ParenthesesSQL.SQLSnipped, '', [rfReplaceAll]);

            PosSQL  := 0;
            SizeSQL := Length(SQLText);

            StackParenthesesPos.Clear();
          end;
        end;

        Inc(PosSQL);
      end;
    finally
       StackParenthesesPos.Free;
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
  SQLSanitized := LinearizeSQL(ASQL);
  Conditions := RemoveSnippetInParentheses(SQLSanitized);
  try
    Result := not ExtractInternalOperators(SQLSanitized).IsEmpty;
  finally
    Conditions.Free;
  end;
end;

end.
