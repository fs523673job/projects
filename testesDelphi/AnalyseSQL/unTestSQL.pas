unit unTestSQL;

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.RegularExpressions
  ;


function LinearizeSQL(const ASQL: String): String;
function ExtrairCondicoesWhereComOr(SQL: string): TStringList;
function ExtrairOperadoresExternos(const Expr: string): string;
function RemoveUmTrechoAutoEmployeeFilter(var SQLText: string): string;
function RemoveAutoEmployeeFilters(var SQLText: string): TStringList;
function AnaliseSQLClausesOR(const ASQL: string): Boolean;


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
  Result := Pos(' OR ', Condicao.ToUpper) > 0;
  Result := Result or (Pos(')OR(', Condicao.ToUpper) > 0);
  Result := Result or (Pos(')OR', Condicao.ToUpper) > 0);
  Result := Result or (Pos('OR(', Condicao.ToUpper) > 0);
  Result := Result or (Pos(' OR', Condicao.ToUpper) > 0);
  Result := Result or (Pos('OR ', Condicao.ToUpper) > 0);
  Result := Result or (Pos(')OR ', Condicao.ToUpper) > 0);
  Result := Result or (Pos(' OR)', Condicao.ToUpper) > 0);
end;

function ExtrairCondicoesWhereComOr(SQL: string): TStringList;
var
  PosicaoInicio, PosicaoFim, PosicaoComentario, PosicaoProximoComentario, PosicaoFimComentario: Integer;
  SQLLower, TempResult, SubStr, CondicaoSemComentario: string;
begin
  Result := TStringList.Create; // Cria a lista para armazenar as condições
  SQLLower := LowerCase(SQL); // Converte o SQL para minúsculas para facilitar a busca
  // Loop para encontrar todas as ocorrências do comentário
  PosicaoComentario := Pos('/*AUTOEMPLOYEEFILTER', SQLLower);
  while PosicaoComentario > 0 do
  begin
    // Encontra a posição do próximo comentário para limitar a busca do WHERE ao bloco atual
    PosicaoProximoComentario := PosEx('/*AUTOEMPLOYEEFILTER', SQLLower, PosicaoComentario + Length('/*AUTOEMPLOYEEFILTER'));
    if PosicaoProximoComentario = 0 then
      PosicaoProximoComentario := MaxInt;
    // Encontra a última posição do WHERE antes do comentário, limitando a busca ao bloco atual
    PosicaoInicio := UltimaPosicaoSubString('WHERE', Copy(SQLLower, 1, PosicaoComentario));
    if (PosicaoInicio > 0) and (PosicaoInicio < PosicaoProximoComentario) then
    begin
      Inc(PosicaoInicio, Length('WHERE'));
      PosicaoFim := PosEx('GROUP by', SQLLower, PosicaoInicio);
      if (PosicaoFim = 0) or (PosicaoFim > PosicaoProximoComentario) then
        PosicaoFim := PosEx('ORDER by', SQLLower, PosicaoInicio);
      if (PosicaoFim = 0) or (PosicaoFim > PosicaoProximoComentario) then
        PosicaoFim := PosicaoProximoComentario;
      if PosicaoFim > PosicaoInicio then
        TempResult := Trim(Copy(SQL, PosicaoInicio, PosicaoFim - PosicaoInicio))
      else
        TempResult := Trim(Copy(SQL, PosicaoInicio, PosicaoComentario - PosicaoInicio));
      // Remove o comentário /*AutoEmployeeFilter*/ e qualquer texto após ele
      PosicaoFimComentario := Pos('/*AUTOEMPLOYEEFILTER', LowerCase(TempResult));
      if PosicaoFimComentario > 0 then
        CondicaoSemComentario := Copy(TempResult, 1, PosicaoFimComentario - 1)
      else
        CondicaoSemComentario := TempResult;
      if ContemOrValido(CondicaoSemComentario) then
      begin
        Result.Add(Trim(CondicaoSemComentario));
        //Teste
        Delete(SQLLower, PosicaoInicio, PosicaoFim - PosicaoInicio);
      end;
    end;
    // Prepara para buscar a próxima ocorrência do comentário, se houver
    if PosicaoProximoComentario <> MaxInt then
      //PosicaoComentario := PosEx('/*autoemployeefilter', SQLLower, PosicaoProximoComentario)
      PosicaoComentario := Pos('/*AUTOEMPLOYEEFILTER', SQLLower)
    else
      Break; // Sai do loop se não houver mais comentários
  end;
end;

function ExtrairOperadoresExternos(const Expr: string): string;
var
  i, ParentesesNivel: Integer;
  TempResult: string;
  ForaParenteses: string;
begin
  TempResult := ''; // Inicializa o resultado temporário
  ParentesesNivel := 0; // Inicializa o contador de nível de parênteses
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

  // Aplica a lógica de ContemOrValido para o texto fora dos parênteses
  if ContemOrValido(ForaParenteses) then
    Result := 'OR'
  else
    Result := ''; // Não encontrou "OR" válido fora dos parênteses
end;

//function ExtrairOperadoresExternos(const Expr: string): string;
//var
//  i, ParentesesNivel: Integer;
//  TempResult: string;
//begin
//  TempResult := ''; // Inicializa o resultado temporário
//  ParentesesNivel := 0; // Inicializa o contador de nível de parênteses
//  // Percorre cada caracter da string
//  for i := 1 to Length(Expr) do
//  begin
//    // Verifica se o caracter atual é um parêntese de abertura
//    if Expr[i] = '(' then
//      Inc(ParentesesNivel) // Incrementa o nível de parênteses
//    else if Expr[i] = ')' then
//      Dec(ParentesesNivel) // Decrementa o nível de parênteses
//    else if ParentesesNivel = 0 then
//      // Se não estiver dentro de parênteses, adiciona o caracter ao resultado temporário
//      TempResult := TempResult + Expr[i];
//  end;
//  // Remove espaços em branco extras do resultado temporário
//  Result := Trim(TempResult);
//  // Verifica se o resultado contém apenas operadores lógicos externos
//  // Neste exemplo, simplificamos verificando apenas a presença do operador "OR" fora dos parênteses
//  // Esta verificação pode ser expandida para outros operadores ou lógica conforme necessário
//  if (Pos('OR', UpperCase(Result)) > 0) and (Pos('AND', UpperCase(Result)) = 0) then
//    Result := 'OR'
//  else if (Pos('AND', UpperCase(Result)) > 0) and (Pos('OR', UpperCase(Result)) = 0) then
//    Result := 'AND'
//  else
//    // Se a string resultante não corresponder a um único operador lógico externo, limpa o resultado
//    Result := '';
//end;

function RemoveUmTrechoAutoEmployeeFilter(var SQLText: string): string;
var
  StartPos, EndPos, WherePos, CommentStart: Integer;
begin
  Result := '';
  // Encontra o fim do comentário
  CommentStart := Pos('/*AUTOEMPLOYEEFILTER=', SQLText);
  if CommentStart = 0 then Exit;
  EndPos := PosEx('*/', SQLText, CommentStart) + 2; // Inclui o fim do comentário
  if EndPos = 2 then Exit; // Não encontrou o fim do comentário
  // Busca para trás a partir do início do comentário para encontrar o WHERE mais próximo
  WherePos := CommentStart;
  repeat
    Dec(WherePos);
    if Copy(SQLText, WherePos, 5).ToUpper = 'WHERE' then Break;
    if Copy(SQLText, WherePos, 6).ToUpper = 'SELECT' then Break;
  until WherePos <= 1;
  if WherePos < 1 then Exit; // Não encontrou WHERE antes do comentário
  // Extrai o trecho
  Result := Copy(SQLText, WherePos, EndPos - WherePos);
  // Remove o trecho do SQL original
  Delete(SQLText, WherePos, EndPos - WherePos);
end;

function RemoveAutoEmployeeFilters(var SQLText: string): TStringList;
var
  RemovedTrecho: string;
begin
  Result := TStringList.Create;
  repeat
    RemovedTrecho := RemoveUmTrechoAutoEmployeeFilter(SQLText);
    if not RemovedTrecho.IsEmpty then
      Result.Add(ExtrairOperadoresExternos(RemovedTrecho));
  until RemovedTrecho.IsEmpty;
end;

function AnaliseSQLClausesOR(const ASQL: string): Boolean;

  function LastPosSubString(const SubStr, InStr: string): Integer;
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

  function ContainsValidOR(const Condition: String): Boolean;
  begin
    Result := Pos(' OR ', Condition.ToUpper) > 0;
    Result := Result or (Pos(')OR(', Condition.ToUpper) > 0);
    Result := Result or (Pos(')OR', Condition.ToUpper) > 0);
    Result := Result or (Pos('OR(', Condition.ToUpper) > 0);
    Result := Result or (Pos(' OR', Condition.ToUpper) > 0);
    Result := Result or (Pos('OR ', Condition.ToUpper) > 0);
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
      if Copy(SQLText, WherePos, 6).ToUpper = 'SELECT' then
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
      Result := ''; // Não encontrou "OR" válido fora dos parênteses
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




end.
