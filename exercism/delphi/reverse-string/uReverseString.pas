unit uReverseString;

interface

function reverse(const AValue: String): String;

implementation

function reverse(const AValue: String): String;
begin
  Result := '';

  for var c := Length(AValue) downto 1 do
    Result := Result + AValue[c];
end;

end.

