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
  PosicaoInicio, PosicaoFim, PosicaoComentario, PosicaoProximoComentario, PosicaoFimComentario: Integer;
  SQLLower, TempResult, SubStr, CondicaoSemComentario: string;
begin
  Result := TStringList.Create; // Cria a lista para armazenar as condições
  SQLLower := LowerCase(SQL); // Converte o SQL para minúsculas para facilitar a busca

  // Loop para encontrar todas as ocorrências do comentário
  PosicaoComentario := Pos('/*autoemployeefilter', SQLLower);
  while PosicaoComentario > 0 do
  begin
    // Encontra a posição do próximo comentário para limitar a busca do WHERE ao bloco atual
    PosicaoProximoComentario := PosEx('/*autoemployeefilter', SQLLower, PosicaoComentario + Length('/*autoemployeefilter'));
    if PosicaoProximoComentario = 0 then
      PosicaoProximoComentario := MaxInt;

    // Encontra a última posição do WHERE antes do comentário, limitando a busca ao bloco atual
    PosicaoInicio := UltimaPosicaoSubString('where', Copy(SQLLower, 1, PosicaoComentario));
    if (PosicaoInicio > 0) and (PosicaoInicio < PosicaoProximoComentario) then
    begin
      Inc(PosicaoInicio, Length('where'));
      PosicaoFim := PosEx('group by', SQLLower, PosicaoInicio);
      if (PosicaoFim = 0) or (PosicaoFim > PosicaoProximoComentario) then
        PosicaoFim := PosEx('order by', SQLLower, PosicaoInicio);
      if (PosicaoFim = 0) or (PosicaoFim > PosicaoProximoComentario) then
        PosicaoFim := PosicaoProximoComentario;

      if PosicaoFim > PosicaoInicio then
        TempResult := Trim(Copy(SQL, PosicaoInicio, PosicaoFim - PosicaoInicio))
      else
        TempResult := Trim(Copy(SQL, PosicaoInicio, PosicaoComentario - PosicaoInicio));

      // Remove o comentário /*AutoEmployeeFilter*/ e qualquer texto após ele
      PosicaoFimComentario := Pos('/*autoemployeefilter', LowerCase(TempResult));
      if PosicaoFimComentario > 0 then
        CondicaoSemComentario := Copy(TempResult, 1, PosicaoFimComentario - 1)
      else
        CondicaoSemComentario := TempResult;

      if ContemOrValido(CondicaoSemComentario) then
        Result.Add(Trim(CondicaoSemComentario));
    end;

    // Prepara para buscar a próxima ocorrência do comentário, se houver
    if PosicaoProximoComentario <> MaxInt then
      PosicaoComentario := PosEx('/*autoemployeefilter', SQLLower, PosicaoProximoComentario)
    else
      Break; // Sai do loop se não houver mais comentários
  end;
end;

function ExtrairOperadoresExternos(const Expr: string): string;
var
  i, ParentesesNivel: Integer;
  TempResult: string;
begin
  TempResult := ''; // Inicializa o resultado temporário
  ParentesesNivel := 0; // Inicializa o contador de nível de parênteses

  // Percorre cada caracter da string
  for i := 1 to Length(Expr) do
  begin
    // Verifica se o caracter atual é um parêntese de abertura
    if Expr[i] = '(' then
      Inc(ParentesesNivel) // Incrementa o nível de parênteses
    else if Expr[i] = ')' then
      Dec(ParentesesNivel) // Decrementa o nível de parênteses
    else if ParentesesNivel = 0 then
      // Se não estiver dentro de parênteses, adiciona o caracter ao resultado temporário
      TempResult := TempResult + Expr[i];
  end;

  // Remove espaços em branco extras do resultado temporário
  Result := Trim(TempResult);

  // Verifica se o resultado contém apenas operadores lógicos externos
  // Neste exemplo, simplificamos verificando apenas a presença do operador "OR" fora dos parênteses
  // Esta verificação pode ser expandida para outros operadores ou lógica conforme necessário
  if (Pos('OR', UpperCase(Result)) > 0) and (Pos('AND', UpperCase(Result)) = 0) then
    Result := 'OR'
  else if (Pos('AND', UpperCase(Result)) > 0) and (Pos('OR', UpperCase(Result)) = 0) then
    Result := 'AND'
  else
    // Se a string resultante não corresponder a um único operador lógico externo, limpa o resultado
    Result := '';
end;


end.
