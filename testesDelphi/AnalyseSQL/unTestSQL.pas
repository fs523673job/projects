unit unTestSQL;

interface

uses
  System.SysUtils,
  System.Classes,
  System.RegularExpressions
  ;


function LinearizeSQL(const ASQL: String): String;
function AnaliseSQLClausesOR(const ASQL: String): Boolean;
function RegxAnaliseSQLClausesOR(const ASQL: String): Boolean;
function NewAnaliseSQLClausesOR(const ASQL: String): Boolean;

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

function NewAnaliseSQLClausesOR(const ASQL: String): Boolean;
const
  TAG_SECURITY_START = '/*AutoEmployeeFilter=';
  TAG_SECURITY_END = '*/';
var
  TagStartPos: Integer;
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

end.
