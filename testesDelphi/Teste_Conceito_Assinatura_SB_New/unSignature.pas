unit unSignature;

interface

uses
  Vcl.Graphics;

type
  ISignature = interface
    ['{8DF1B3F8-2745-4CB2-BFD8-F1154E8ACD3B}']
    function GetFinalSignatureImage: TBitmap;
  end;

implementation

end.
