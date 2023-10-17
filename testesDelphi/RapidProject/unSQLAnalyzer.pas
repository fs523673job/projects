unit unSQLAnalyzer;

interface

uses
  System.Generics.Collections,
  System.StrUtils,
  System.SysUtils
  ;

type
  TSQLAnalyzer = class
  private
    FSecurityTags: TArray<string>;
    FSubSelects: TArray<string>;
    function GetSubselectsInSelect(const ASQL: string): TArray<string>;
    function HasUnbalancedParentheses(const SQL: string): Boolean;
    function VerifySQLCommand(const ASQL: String): Boolean;
    function ValidateORClauses(const SQL: string): Boolean;
    function CheckOrsWithParentheses(const SQL: string): Boolean;
    const
      TAG_SECURITY_START = '/*AutoEmployeeFilter=';
      TAG_SECURITY_END = '*/';

    function CountChar(const C: Char; const S: string): Integer;
    function CountOccurrences(const SubStr, Str: string; UntilPos: Integer): Integer;
    procedure ExtractAllSecurityTags(const ASQL: string);
    procedure ExtractSubselectsFromSelect(const ASQL: String);
    function RemoveSubselectsFromSelect(const ASQL: string): string;
    function FindLastWhereBeforeTag(ASQL: string; StartPos: Integer): String;
    function FindLookForClausesOr(ASQL: String): Boolean;
  public
    function FindTagSecurityErrors(const ASQL: String): Boolean;
  end;

implementation

function TSQLAnalyzer.CountOccurrences(const SubStr, Str: string; UntilPos: Integer): Integer;
var
  Offset: Integer;
begin
  Result := 0;
  Offset := Pos(SubStr, Str);
  while (Offset <> 0) and (Offset < UntilPos) do
  begin
    Inc(Result);
    Offset := PosEx(SubStr, Str, Offset + Length(SubStr));
  end;
end;

procedure TSQLAnalyzer.ExtractSubselectsFromSelect(const ASQL: string);
var
  Subselects: TList<string>;
  OpenParenthesisCount, CloseParenthesisCount, StartPos, CurrentPos: Integer;
  InSingleQuote, InDoubleQuote: Boolean;
begin
  Subselects := TList<string>.Create;
  try
    OpenParenthesisCount := 0;
    CloseParenthesisCount := 0;
    InSingleQuote := False;
    InDoubleQuote := False;
    StartPos := 1;
    CurrentPos := 1;

    while CurrentPos <= Length(ASQL) do
    begin
      case ASQL[CurrentPos] of
        '''': InSingleQuote := not InSingleQuote;
        '"': InDoubleQuote := not InDoubleQuote;
        '(':
          begin
            if not InSingleQuote and not InDoubleQuote then
            begin
              Inc(OpenParenthesisCount);
            end;
          end;
        ')':
          begin
            if not InSingleQuote and not InDoubleQuote then
            begin
              Inc(CloseParenthesisCount);
              if OpenParenthesisCount = CloseParenthesisCount then
              begin
                Subselects.Add(Trim(Copy(ASQL, StartPos, CurrentPos - StartPos + 1)));
                StartPos := CurrentPos + 1;
                OpenParenthesisCount := 0;
                CloseParenthesisCount := 0;
              end;
            end;
          end;
        'S', 's':
          begin
            if not InSingleQuote and not InDoubleQuote and (StartPos = CurrentPos) then
            begin
              if UpperCase(Copy(ASQL, CurrentPos, 7)) = 'SELECT ' then
              begin
                Inc(OpenParenthesisCount);
              end;
            end;
          end;
      end;
      Inc(CurrentPos);
    end;

    FSubSelects := Subselects.ToArray;
  finally
    Subselects.Free;
  end;
end;

function TSQLAnalyzer.GetSubselectsInSelect(const ASQL: string): TArray<string>;
var
  Subselects: TList<string>;
  OpenParenthesisCount, CloseParenthesisCount, StartPos, i: Integer;
  InSingleQuote, InDoubleQuote: Boolean;
begin
  Subselects := TList<string>.Create;
  try
    OpenParenthesisCount := 0;
    CloseParenthesisCount := 0;
    InSingleQuote := False;
    InDoubleQuote := False;
    StartPos := 1;

    for i := 1 to Length(ASQL) do
    begin
      case ASQL[i] of
        '''': InSingleQuote := not InSingleQuote;
        '"': InDoubleQuote := not InDoubleQuote;
        '(': if not InSingleQuote and not InDoubleQuote then Inc(OpenParenthesisCount);
        ')':
          begin
            if not InSingleQuote and not InDoubleQuote then
            begin
              Inc(CloseParenthesisCount);
              if OpenParenthesisCount = CloseParenthesisCount then
              begin
                Subselects.Add(Copy(ASQL, StartPos, i - StartPos + 1));
                OpenParenthesisCount := 0;
                CloseParenthesisCount := 0;
                StartPos := i + 1;
              end;
            end;
          end;
      end;
    end;

    Result := Subselects.ToArray;
  finally
    Subselects.Free;
  end;
end;

function TSQLAnalyzer.RemoveSubselectsFromSelect(const ASQL: string): string;
var
  Subselect: string;
  Subselects: TArray<string>;
begin
  Subselects := GetSubselectsInSelect(ASQL);
  Result := ASQL;

  for Subselect in Subselects do
  begin
    Result := Result.Replace(Subselect, '');
  end;

  Result := Result.Trim;
end;

procedure TSQLAnalyzer.ExtractAllSecurityTags(const ASQL: string);
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

    FSecurityTags := TagList.ToArray;
  finally
    TagList.Free;
  end;
end;

function TSQLAnalyzer.CountChar(const C: Char; const S: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    if S[i] = C then
      Inc(Result);
end;

function TSQLAnalyzer.FindLastWhereBeforeTag(ASQL: string; StartPos: Integer): String;
var
  SearchPos: Integer;
  SQLWhere: String;
begin
  Result := '';
  SearchPos := StartPos;

  while SearchPos > 0 do
  begin
    SearchPos := LastDelimiter('WHERE', Copy(ASQL, 1, SearchPos - 1));
    if SearchPos = 0 then
      Exit;

    if (CountChar('''', Copy(ASQL, 1, SearchPos)) mod 2 = 0) and
       (CountChar('"', Copy(ASQL, 1, SearchPos)) mod 2 = 0) then
    begin
      SQLWhere := Trim(Copy(ASQL, SearchPos - 4, SearchPos - 1));
      ASQL := StringReplace(ASQL, SQLWhere, '', [rfReplaceAll]);
      Exit(SQLWhere);
    end;

    Dec(SearchPos);
  end;
end;

function TSQLAnalyzer.FindLookForClausesOr(ASQL: String): Boolean;
var
  PieceOfSQL: String;
  OrPos, ParenthesisCount, i: Integer;
  InSingleQuote, InDoubleQuote: Boolean;
begin
  Result := False;
  PieceOfSQL := ASQL;
  OrPos := Pos('OR', PieceOfSQL.ToUpper);

  while OrPos > 0 do
  begin
    InSingleQuote := False;
    InDoubleQuote := False;
    ParenthesisCount := 0;

    for i := 1 to OrPos do
    begin
      case PieceOfSQL[i] of
        '''': InSingleQuote := not InSingleQuote;
        '"': InDoubleQuote := not InDoubleQuote;
        '(': if not InSingleQuote and not InDoubleQuote then Inc(ParenthesisCount);
        ')': if not InSingleQuote and not InDoubleQuote then Dec(ParenthesisCount);
      end;
    end;

    // Se encontrarmos um OR que não está dentro de aspas e não está dentro de parênteses, temos um erro
    if (ParenthesisCount = 0) and not InSingleQuote and not InDoubleQuote then
      Exit(True);

    OrPos := PosEx('OR', PieceOfSQL.ToUpper, OrPos + 2);
  end;
end;

function TSQLAnalyzer.HasUnbalancedParentheses(const SQL: string): Boolean;
var
  ParenthesisCount, i: Integer;
  InSingleQuote, InDoubleQuote: Boolean;
begin
  ParenthesisCount := 0;
  InSingleQuote := False;
  InDoubleQuote := False;

  for i := 1 to Length(SQL) do
  begin
    case SQL[i] of
      '''': InSingleQuote := not InSingleQuote;
      '"': InDoubleQuote := not InDoubleQuote;
      '(':
      begin
        if not InSingleQuote and not InDoubleQuote then
          Inc(ParenthesisCount);
      end;
      ')':
      begin
        if not InSingleQuote and not InDoubleQuote then
          Dec(ParenthesisCount);
      end;
      'O', 'o':
      begin
        if not InSingleQuote and not InDoubleQuote then
        begin
          if SameText(Copy(SQL, i, 2), 'OR') and (ParenthesisCount = 0) then
            Exit(True);
        end;
      end;
    end;
  end;

  Result := False;
end;

function TSQLAnalyzer.ValidateORClauses(const SQL: string): Boolean;
var
  InSubselect: Boolean;
  ParenthesisCount, i: Integer;
  InSingleQuote, InDoubleQuote: Boolean;
begin
  InSubselect := False;
  ParenthesisCount := 0;
  InSingleQuote := False;
  InDoubleQuote := False;

  for i := 1 to Length(SQL) do
  begin
    case SQL[i] of
      '''': InSingleQuote := not InSingleQuote;
      '"': InDoubleQuote := not InDoubleQuote;
      '(':
      begin
        if not InSingleQuote and not InDoubleQuote then
        begin
          Inc(ParenthesisCount);

          if InSubselect then
            InSubselect := False;
        end;
      end;
      ')':
      begin
        if not InSingleQuote and not InDoubleQuote then
          Dec(ParenthesisCount);
      end;
      'S', 's':
      begin
        if not InSingleQuote and not InDoubleQuote then
        begin
          if (i + 6 <= Length(SQL)) and SameText(Copy(SQL, i, 7), 'SELECT ') then
            InSubselect := True;
        end;
      end;
      'O', 'o':
      begin
        if not InSingleQuote and not InDoubleQuote then
        begin
          if (i + 1 <= Length(SQL)) and SameText(Copy(SQL, i, 2), 'OR') then
          begin
            if ParenthesisCount = 0 then
              Exit(True);
          end;
        end;
      end;
    end;
  end;

  Result := False;
end;

//function TSQLAnalyzer.FindTagSecurityErrors(const ASQL: string): Boolean;
//begin
//  Result := HasUnbalancedParentheses(ASQL) or ValidateORClauses(ASQL);
//end;

function TSQLAnalyzer.CheckOrsWithParentheses(const SQL: string): Boolean;
var
  ParenthesisLevel, i: Integer;
  InSingleQuote, InDoubleQuote: Boolean;
  InSubselect: Boolean;
begin
  Result := True;
  ParenthesisLevel := 0;
  InSingleQuote := False;
  InDoubleQuote := False;
  InSubselect := False;

  for i := 1 to Length(SQL) do
  begin
    case SQL[i] of
      '''': InSingleQuote := not InSingleQuote;
      '"': InDoubleQuote := not InDoubleQuote;
      '(':
        if not InSingleQuote and not InDoubleQuote then
        begin
          Inc(ParenthesisLevel);
          if InSubselect then
            InSubselect := False;
        end;
      ')':
        if not InSingleQuote and not InDoubleQuote then
        begin
          Dec(ParenthesisLevel);
          if ParenthesisLevel = 0 then
            InSubselect := False;
        end;
      'S', 's':
        if not InSingleQuote and not InDoubleQuote and (i + 6 <= Length(SQL)) then
        begin
          if UpperCase(Copy(SQL, i, 7)) = 'SELECT ' then
            InSubselect := True;
        end;
      'O', 'o':
        if not InSingleQuote and not InDoubleQuote and (i + 1 <= Length(SQL)) then
        begin
          if (UpperCase(Copy(SQL, i, 2)) = 'OR') and (ParenthesisLevel = 0) and not InSubselect then
          begin
            Result := False;
            Exit;
          end;
        end;
    end;
  end;

  Result := True;
end;


function HasUnparenthesizedOrClauses(const ASQL: string): Boolean;
var
  ParenthesisCount, i, j: Integer;
  InSingleQuote, InDoubleQuote, InSubselect: Boolean;
begin
  Result := False;
  ParenthesisCount := 0;
  InSingleQuote := False;
  InDoubleQuote := False;
  InSubselect := False;

  for i := 1 to Length(ASQL) do
  begin
    case ASQL[i] of
      '''': InSingleQuote := not InSingleQuote;
      '"': InDoubleQuote := not InDoubleQuote;
      '(':
        begin
          if not InSingleQuote and not InDoubleQuote then
          begin
            Inc(ParenthesisCount);
            if InSubselect then
              InSubselect := False; // Reset InSubselect when entering new level of parentheses
          end;
        end;
      ')':
        begin
          if not InSingleQuote and not InDoubleQuote then
          begin
            Dec(ParenthesisCount);
            if ParenthesisCount < 0 then
              Exit; // Found an extra closing parenthesis
          end;
        end;
      'S', 's':
        begin
          if not InSingleQuote and not InDoubleQuote and not InSubselect then
          begin
            if UpperCase(Copy(ASQL, i, 7)) = 'SELECT ' then
            begin
              InSubselect := True;
            end;
          end;
        end;
      'O', 'o':
        begin
          if not InSingleQuote and not InDoubleQuote and (ParenthesisCount = 0) then
          begin
            if UpperCase(Copy(ASQL, i, 3)) = 'OR ' then
            begin
              // Check if the "OR" is not encapsulated by parentheses
              j := i - 1;
              while (j > 0) and (ASQL[j] in [' ', #9, #10, #13]) do
                Dec(j);

              if (j > 0) and (ASQL[j] <> ')') then
              begin
                Result := True;
                Exit;
              end;
            end;
          end;
        end;
    end;
  end;
end;


function TSQLAnalyzer.FindTagSecurityErrors(const ASQL: string): Boolean;
begin
//  ExtractAllSecurityTags(ASQL);
//
//  if High(FSecurityTags) > 0 then
//    Result :=  HasUnbalancedParentheses(ASQL)


  //FSecurityTags := GetSubselectsInSelect(ASQL);
  //Result := HasUnbalancedParentheses(ASQL) or ValidateORClauses(ASQL);
  //Result := not CheckOrsWithParentheses(ASQL);

  Result := HasUnparenthesizedOrClauses(ASQL);
end;

function TSQLAnalyzer.VerifySQLCommand(const ASQL: String): Boolean;
var
  PosTagSecurity: Integer;
  PosWhere: Integer;
  c: Integer;
  NewSQL: String;
begin
  ExtractAllSecurityTags(ASQL);
  NewSQL := RemoveSubselectsFromSelect(ASQL);
//  FSubSelects := GetSubselectsInSelect(ASQL);

  Result := FindTagSecurityErrors(ASQL);

  Result := False;

  for c := 0 to High(FSecurityTags) do
  begin
    PosTagSecurity := Pos(FSecurityTags[c], ASQL);
    NewSQL := FindLastWhereBeforeTag(ASQL, PosTagSecurity);
    Result := FindLookForclausesOr(NewSQL);
  end;
end;

end.
