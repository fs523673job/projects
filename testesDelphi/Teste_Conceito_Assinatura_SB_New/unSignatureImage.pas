unit unSignatureImage;

interface

uses
  Vcl.Imaging.jpeg, Vcl.Graphics, unSignatureDTO;

type
  ISignatureImage = interface
    ['{81D5F087-0FC7-4924-83B6-621A2B3C5AE3}']
    function TreatSignatureImage(const ASignatureDTO: TSignatureDTO): TBitmap;
    procedure AdjustImageSize(const AImage, ANewImage: TJPEGImage; const AFieldHeight, AFieldWidth: Integer; const AHeighPercent, AWidthPercent: Double);
  end;

implementation

end.
