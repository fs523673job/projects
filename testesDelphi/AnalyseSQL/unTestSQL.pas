unit unTestSQL;

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.RegularExpressions
  ;


function LinearizeSQL(const ASQL: String): String;
function RemoveAutoEmployeeFilters(var SQLText: string): TStringList;
function AnaliseSQLClausesOR(const ASQL: string): Boolean;
function NewAnaliseSQLClausesOR(const ASQL: string): Boolean;


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

function RemoveAutoEmployeeFilters(var SQLText: string): TStringList;
var
  RemovedTrecho: string;

  function RemoveUmTrechoAutoEmployeeFilter(var SQLText: string): string;
  var
    StartPos, EndPos, WherePos, CommentStart: Integer;
  begin
    Result := '';
    CommentStart := Pos('/*AUTOEMPLOYEEFILTER=', SQLText);

    if CommentStart = 0 then
      Exit;

    EndPos := PosEx('*/', SQLText, CommentStart) + 2;

    if EndPos = 2 then
      Exit;

    WherePos := CommentStart;
    repeat
      Dec(WherePos);
      if Copy(SQLText, WherePos, 5).ToUpper = 'WHERE' then Break;
    until WherePos <= 1;
    if WherePos < 1 then
      Exit;

    Result := Copy(SQLText, WherePos, EndPos - WherePos);

    Delete(SQLText, WherePos, EndPos - WherePos);
  end;

  function ContemOrValido(const Condicao: string): Boolean;
  begin
    Result := Pos(' OR ', Condicao.ToUpper) > 0;
    Result := Result or (Pos(')OR(', Condicao.ToUpper) > 0);
    Result := Result or (Pos(')OR', Condicao.ToUpper) > 0);
    Result := Result or (Pos('OR(', Condicao.ToUpper) > 0);
    Result := Result or (Pos(')OR ', Condicao.ToUpper) > 0);
    Result := Result or (Pos(' OR)', Condicao.ToUpper) > 0);
  end;


  function ExtrairOperadoresExternos(const Expr: string): string;
  var
    i, ParentesesNivel: Integer;
    TempResult: string;
    ForaParenteses: string;
  begin
    TempResult := ''; // Inicializa o resultado tempor�rio
    ParentesesNivel := 0; // Inicializa o contador de n�vel de par�nteses
    ForaParenteses := '';

    // Percorre cada caracter da string
    for i := 1 to Length(Expr) do
    begin
      if Expr[i] = '(' then
        Inc(ParentesesNivel)
      else if Expr[i] = ')' then
        Dec(ParentesesNivel)
      else if ParentesesNivel = 0 then
        ForaParenteses := ForaParenteses + Expr[i];
    end;

    // Aplica a l�gica de ContemOrValido para o texto fora dos par�nteses
    if ContemOrValido(ForaParenteses) then
      Result := 'OR'
    else
      Result := ''; // N�o encontrou "OR" v�lido fora dos par�nteses
  end;

begin
  Result := TStringList.Create;
  repeat
    RemovedTrecho := RemoveUmTrechoAutoEmployeeFilter(SQLText);
    if not RemovedTrecho.IsEmpty then
      Result.Add(ExtrairOperadoresExternos(RemovedTrecho));
  until RemovedTrecho.IsEmpty;
end;

function AnaliseSQLClausesOR(const ASQL: string): Boolean;

  function ContainsValidOR(const Condition: String): Boolean;
  begin
    Result := Pos(' OR ', Condition.ToUpper) > 0;
    Result := Result or (Pos(')OR(', Condition.ToUpper) > 0);
    Result := Result or (Pos(')OR', Condition.ToUpper) > 0);
    Result := Result or (Pos('OR(', Condition.ToUpper) > 0);
    Result := Result or (Pos(')OR ', Condition.ToUpper) > 0);
    Result := Result or (Pos(' OR)', Condition.ToUpper) > 0);
  end;

  function RemoveSnippetAutoEmployeeFilter(var SQLText: string): string;
  var
    StartPos, EndPos, WherePos, CommentStart: Integer;
  begin
    CommentStart := Pos('/*AUTOEMPLOYEEFILTER=', SQLText);

    if CommentStart = 0 then
      Exit('');

    EndPos := PosEx('*/', SQLText, CommentStart) + 2;

    if EndPos = 2 then
      Exit('');

    WherePos := CommentStart;
    repeat
      Dec(WherePos);
      if Copy(SQLText, WherePos, 5).ToUpper = 'WHERE' then
        Break;
    until WherePos <= 1;

    if WherePos < 1 then
      Exit('');

    Result := Copy(SQLText, WherePos, EndPos - WherePos);

    Delete(SQLText, WherePos, EndPos - WherePos);
  end;

  function ExtractWhereConditionsWithOR(const AASQL: string): TStringList;
  var
    SnippetSQL: string;
    AlteredSQL: String;
  begin
    AlteredSQL := AASQL;

    Result := TStringList.Create;
    repeat
      SnippetSQL := RemoveSnippetAutoEmployeeFilter(AlteredSQL);
      if not SnippetSQL.IsEmpty then
        Result.Add(SnippetSQL);
    until SnippetSQL.IsEmpty;
  end;

  function ExtractInternalOperators(const Expr: string): string;
  var
    c, LevelPrentheses: Integer;
    TempResult: string;
  begin
    TempResult := '';
    LevelPrentheses := 0;

    for c := 1 to Length(Expr) do
    begin
      if Expr[c] = '(' then
        Inc(LevelPrentheses)
      else if Expr[c] = ')' then
        Dec(LevelPrentheses)
      else if LevelPrentheses = 0 then
        TempResult := TempResult + Expr[c];
    end;

    if ContainsValidOR(TempResult) then
      Result := 'OR'
    else
      Result := ''; // N�o encontrou "OR" v�lido fora dos par�nteses
  end;

var
  Conditions   : TStringList;
  Condition    : String;
  SQLSanitized : String;
begin
  Result := False;
  SQLSanitized := LinearizeSQL(ASQL);
  Conditions := ExtractWhereConditionsWithOR(ASQL);
  try
    for Condition in Conditions do
    begin
      Result := not ExtractInternalOperators(Condition).IsEmpty;
      if Result then
        Break;
    end;
  finally
    Conditions.Free;
  end;
end;


function NewAnaliseSQLClausesOR(const ASQL: string): Boolean;

type
  TParenthesesSQL = record
    IndexBegin : Integer;
    IndexEnd   : Integer;
    Caracter   : String;
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
    LastParentheses     : String;
  begin
    SizeSQL := Length(SQLText);
    StackParenthesesPos := TStack<TParenthesesSQL>.Create;
    try
      PosSQL := 1;
      while PosSQL <= SizeSQL  do
      begin
        if CharInSet(SQLText[PosSQL], ['(', ')']) then
        begin
          if SQLText[PosSQL] = '(' then
            ParenthesesSQL.IndexBegin := PosSQL
          else if SQLText[PosSQL] = ')' then
          begin
            if LastParentheses = '(' then
            begin
              ParenthesesSQL.IndexEnd   := PosSQL;
              ParenthesesSQL.SQLSnipped := Copy(SQLText, StackParenthesesPos.Peek().IndexBegin, (ParenthesesSQL.IndexEnd + 1) - StackParenthesesPos.Peek().IndexBegin);
            end;
          end;

          LastParentheses := SQLText[PosSQL];
          ParenthesesSQL.Caracter   := SQLText[PosSQL];
          StackParenthesesPos.Push(ParenthesesSQL);
        end;

        Inc(PosSQL);
      end;

      Result := TStringList.Create;

      while StackParenthesesPos.Count > 0 do
      begin
        Result.Add(StackParenthesesPos.Peek().SQLSnipped);
        SQLText := StringReplace(SQLText, StackParenthesesPos.Peek().SQLSnipped, '', [rfReplaceAll]);
        StackParenthesesPos.Pop();
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
    c, LevelPrentheses: Integer;
    TempResult: string;
  begin
    TempResult := '';
    LevelPrentheses := 0;

    for c := 1 to Length(Expr) do
    begin
      if Expr[c] = '(' then
        Inc(LevelPrentheses)
      else if Expr[c] = ')' then
        Dec(LevelPrentheses)
      else if LevelPrentheses = 0 then
        TempResult := TempResult + Expr[c];
    end;

    if ContainsValidOR(TempResult) then
      Result := 'OR'
    else
      Result := ''; // N�o encontrou "OR" v�lido fora dos par�nteses
  end;

begin
  SQLSanitized := LinearizeSQL(ASQL);
  Conditions := RemoveSnippetInParentheses(SQLSanitized);
  try
    Result := ExtractInternalOperators(SQLSanitized).IsEmpty;
    for Condition in Conditions do
    begin
      Result := not Condition.IsEmpty;
      if Result then
        Break;
    end;
  finally
    Conditions.Free;
  end;

end;

end.
