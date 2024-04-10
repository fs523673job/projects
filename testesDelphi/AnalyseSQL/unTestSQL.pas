unit unTestSQL;

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.RegularExpressions
  ;


function LinearizeSQL(const ASQL: String): String;
function AnaliseSQLClausesOR(const ASQL: String): Boolean;
function RegxAnaliseSQLClausesOR(const ASQL: String): Boolean;
function NewAnaliseSQLClausesOR(const ASQL: String): String;
function ExtractSQLConditions(SQL: string): TStringList;
function ExtractSQLConditions_Where(const SQL: string): TStringList;
function ExtrairCondicoesWhereComOr(SQL: string): TStringList;

implementation

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

function AnaliseSQLClausesOR(const ASQL: string): Boolean;
var
  ParenthesisCount, i, j: Integer;
  InSingleQuote, InDoubleQuote, InSubselect, InJoinCommand: Boolean;
begin
  Result := False;
  ParenthesisCount := 0;
  InSingleQuote := False;
  InDoubleQuote := False;
  InSubselect := False;
  i := 1;

  while (i < Length(ASQL)) do
  begin
    if (ASQL[i] = '''') then
    begin
      InSingleQuote := not InSingleQuote;
    end
    else if (ASQL[i] = '"') then
    begin
      InDoubleQuote := not InDoubleQuote;
    end
    else if (ASQL[i] = '(') then
    begin
      if not InSingleQuote and not InDoubleQuote then
      begin
        Inc(ParenthesisCount);
        if InSubselect then
          InSubselect := False;
      end;
    end
    else if (ASQL[i] = ')') then
    begin
      if not InSingleQuote and not InDoubleQuote then
      begin
        Dec(ParenthesisCount);
        if ParenthesisCount < 0 then
          Exit;
      end;
    end
    else if (ASQL.ToUpper[i] = 'S') then
    begin
      if not InSingleQuote and not InDoubleQuote and not InSubselect then
      begin
        if Copy(ASQL.ToUpper, i, 7) = 'SELECT ' then
        begin
          InSubselect := True;
        end;
      end;
    end
    else if (ASQL.ToUpper[i] in ['L', 'I', 'R']) then
    begin
      if not InSingleQuote and not InDoubleQuote and not InSubselect then
      begin
        //LEFT, INNER, RIGTH
        if (Copy(ASQL.ToUpper, i, 4) = 'LEFT') or
           (Copy(ASQL.ToUpper, i, 5) = 'RIGHT') or
           (Copy(ASQL.ToUpper, i, 5) = 'INNER') then
        begin
          InJoinCommand := True;
        end;
      end;
    end
    else if (ASQL.ToUpper[i] = 'W') then
    begin
      if (Copy(ASQL.ToUpper, i, 5) = 'WHERE') then
        InJoinCommand := False;
    end
    else if (ASQL.ToUpper[i] = 'O') then
    begin
      if (Copy(ASQL.ToUpper, i, 3) = 'OR ') and (not CharInSet(ASQL[i - 1], ['A'..'Z', 'a'..'z'])) and (not InJoinCommand) then
      begin
        j := i - 1;
        while (j > 0) and CharInSet(ASQL[j], [' ', #9, #10, #13]) do
          Dec(j);

        if (j > 0) and (ASQL[j] <> ')') then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
    Inc(i);
  end;
end;

function RegxAnaliseSQLClausesOR(const ASQL: String): Boolean;
const
  RE0 = '\bWHERE\b(?:[^()]*?\bOR\b[^();]*?)+';
  RE1 = '\bWHERE\b(?![^\(]*\)).*?\bOR\b.*?(?![^\(]*\))';
var
  Regex: TRegEx;
  Match: TMatch;
begin
  Regex := TRegEx.Create(RE1);
  Match := Regex.Match(ASQL);
  Result := Match.Success;
end;

function NewAnaliseSQLClausesOR(const ASQL: String): String;
const
  TAG_SECURITY_START = '/*AUTOEMPLOYEEFILTER=';
  TAG_SECURITY_END = '*/';
var
  TagStartPos: Integer;
  IndexSQL   : Integer;
begin
  Result := 'Não Encontrado';

  TagStartPos := Pos(TAG_SECURITY_START, ASQL);
  IndexSQL    := TagStartPos;

  while (IndexSQL > 0) do
  begin
    if (Copy(ASQL.ToUpper, IndexSQL, 5) = 'WHERE') then
    begin
      Result := Copy(ASQL.ToUpper, IndexSQL, TagStartPos);
      Break;
    end;

    Dec(IndexSQL);
  end;
end;

function ExtractSQLConditions(SQL: string): TStringList;
var
  InQuotes, InWhere, InSubQuery: Boolean;
  CommentLevel: Integer;
  Condition, Token: string;
  I: Integer;
begin
  Result := TStringList.Create;
  InQuotes := False;
  InWhere := False;
  InSubQuery := False;
  CommentLevel := 0;
  Condition := '';
  Token := '';
  I := 1;

  while I <= Length(SQL) do
  begin
    // Ignorar conteúdo dentro de comentários
    if (I < Length(SQL)) and (SQL[I] + SQL[I+1] = '/*') then
    begin
      Inc(CommentLevel);
      Inc(I, 2);
      Continue;
    end
    else if (CommentLevel > 0) and (I < Length(SQL)) and (SQL[I] + SQL[I+1] = '*/') then
    begin
      Dec(CommentLevel);
      Inc(I, 2);
      Continue;
    end;

    if CommentLevel > 0 then
    begin
      Inc(I);
      Continue;
    end;

    // Trata strings literais
    if SQL[I] = '''' then
    begin
      InQuotes := not InQuotes;
      Inc(I);
      Continue;
    end;

    if not InQuotes then
    begin
      // Identifica e ignora subqueries
      if SQL[I] = '(' then
        InSubQuery := True
      else if SQL[I] = ')' then
        InSubQuery := False;

      if not InSubQuery then
      begin
        // Processa a cláusula WHERE
        if InWhere then
        begin
          if SQL[I] in [' ', '(', ')', ';', ','] then
          begin
            if Token <> '' then
            begin
              Condition := Condition + Token + ' ';
              Token := '';
            end;

            if SQL[I] = ';' then
              Break;  // Final do comando SQL
          end
          else
            Token := Token + SQL[I];
        end
        else if UpperCase(Copy(SQL, I, 6)) = 'WHERE ' then
        begin
          InWhere := True;
          Inc(I, 5);  // Avança o índice para o final do 'WHERE '
        end;
      end;
    end;
    Inc(I);
  end;

  if Condition <> '' then
    Result.Add(Condition);  // Adiciona a última condição encontrada
end;

function ExtractSQLConditions_Where(const SQL: string): TStringList;
var
  I: Integer;
  InQuotes, InWhere: Boolean;
  Token: string;
  CommentDepth, ParenthesisLevel: Integer;
begin
  Result := TStringList.Create;
  InQuotes := False;
  InWhere := False;
  Token := '';
  CommentDepth := 0;
  ParenthesisLevel := 0;

  I := 1;
  while I <= Length(SQL) do
  begin
    if CommentDepth = 0 then
    begin
      if (I <= Length(SQL) - 1) and (SQL[I] = '/') and (SQL[I + 1] = '*') then
      begin
        Inc(CommentDepth);
        Inc(I);
      end
      else if (I <= Length(SQL) - 1) and (SQL[I] = '*') and (SQL[I + 1] = '/') then
      begin
        Dec(CommentDepth);
        Inc(I);
      end
      else if not InQuotes and (SQL[I] = '(') then
      begin
        Inc(ParenthesisLevel);
      end
      else if not InQuotes and (SQL[I] = ')') then
      begin
        Dec(ParenthesisLevel);
      end;
    end
    else if (SQL[I] = '*') and (I < Length(SQL)) and (SQL[I + 1] = '/') then
    begin
      Dec(CommentDepth);
      Inc(I);
    end;

    if (CommentDepth = 0) and not InQuotes then
    begin
      if SQL[I] = '''' then
      begin
        InQuotes := not InQuotes;
      end
      else if UpperCase(Copy(SQL, I, 5)) = 'WHERE' then
      begin
        InWhere := True;
        Inc(I, 4);  // Move past the 'WHERE' keyword
      end
      else if InWhere and (ParenthesisLevel = 0) and
              ((UpperCase(Copy(SQL, I, 3)) = 'AND') or (UpperCase(Copy(SQL, I, 2)) = 'OR')) then
      begin
        if Token.Trim <> '' then
        begin
          Result.Add(Token.Trim);
          Token := '';
        end;
        if UpperCase(Copy(SQL, I, 3)) = 'AND' then
          Inc(I, 2)
        else if UpperCase(Copy(SQL, I, 2)) = 'OR' then
          Inc(I, 1);
      end;
    end;

    if InWhere and (CommentDepth = 0) then
      Token := Token + SQL[I];

    Inc(I);
  end;

  if Token.Trim <> '' then
    Result.Add(Token.Trim);
end;

function UltimaPosicaoSubString(const SubStr, InStr: string): Integer;
var
  LastPos, TempPos: Integer;
begin
  Result := 0;
  LastPos := Pos(SubStr, InStr);
  while LastPos > 0 do
  begin
    TempPos := PosEx(SubStr, InStr, LastPos + 1);
    if TempPos = 0 then
      Break
    else
      LastPos := TempPos;
  end;
  if LastPos > 0 then
    Result := LastPos;
end;

function ContemOrValido(const Condicao: string): Boolean;
begin
  // Verifica se a condição contém "or" com várias possíveis formatações
  Result := Pos(' or ', LowerCase(Condicao)) > 0;
  Result := Result or (Pos(')or(', LowerCase(Condicao)) > 0);
  Result := Result or (Pos(')or', LowerCase(Condicao)) > 0);
  Result := Result or (Pos('or(', LowerCase(Condicao)) > 0);
  Result := Result or (Pos(' or', LowerCase(Condicao)) > 0);
  Result := Result or (Pos('or ', LowerCase(Condicao)) > 0);
  Result := Result or (Pos(')or ', LowerCase(Condicao)) > 0);
  Result := Result or (Pos(' or)', LowerCase(Condicao)) > 0);
end;

function ExtrairCondicoesWhereComOr(SQL: string): TStringList;
var
  PosicaoInicio, PosicaoFim, PosicaoComentario: Integer;
  SQLLower, TempResult, SubStr: string;
begin
  Result := TStringList.Create; // Cria a lista para armazenar as condições
  SQLLower := LowerCase(SQL); // Converte o SQL para minúsculas para facilitar a busca

  // Loop para encontrar todas as ocorrências do comentário
  PosicaoComentario := Pos('/*autoemployeefilter', SQLLower);
  while PosicaoComentario > 0 do
  begin
    // Encontra a última posição do WHERE antes do comentário
    PosicaoInicio := UltimaPosicaoSubString('where', Copy(SQLLower, 1, PosicaoComentario));
    if PosicaoInicio > 0 then
    begin
      Inc(PosicaoInicio, Length('where'));
      PosicaoFim := PosEx('group by', SQLLower, PosicaoInicio);
      if PosicaoFim = 0 then PosicaoFim := PosEx('order by', SQLLower, PosicaoInicio);
      if PosicaoFim = 0 then PosicaoFim := PosicaoComentario;

      if PosicaoFim > PosicaoInicio then
        TempResult := Trim(Copy(SQL, PosicaoInicio, PosicaoFim - PosicaoInicio))
      else
        TempResult := Trim(Copy(SQL, PosicaoInicio, PosicaoComentario - PosicaoInicio));

      if ContemOrValido(TempResult) then
        Result.Add(TempResult);
    end;

    // Prepara para buscar a próxima ocorrência do comentário, se houver
    SubStr := Copy(SQLLower, PosicaoComentario + Length('/*autoemployeefilter'), MaxInt);
    PosicaoComentario := PosEx('/*autoemployeefilter', SQLLower, PosicaoComentario + Length('/*autoemployeefilter'));
  end;
end;
end.
