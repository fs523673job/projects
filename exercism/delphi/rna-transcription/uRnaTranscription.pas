unit uRnaTranscription;

interface

type
  complement = class
    class function OfDna(const AValue: String): String;
  end;

implementation

{ complement }

class function complement.OfDna(const AValue: String): String;

type
  RDnaToRna = record
    DNA: String;
    RNA: String;
  end;

const
  CDNATORNA: array[1..4] of RDnaToRna = (
    (DNA: 'G'; RNA: 'C'),
    (DNA: 'C'; RNA: 'G'),
    (DNA: 'T'; RNA: 'A'),
    (DNA: 'A'; RNA: 'U')
  );

begin
  Result := '';
  for var c := 1 to Length(AValue) do
    for var x := Low(CDNATORNA) to High(CDNATORNA) do
      if (CDNATORNA[x].DNA = AValue[c]) then
      begin
        Result := Result + CDNATORNA[x].RNA;
        break;
      end;
end;

end.
