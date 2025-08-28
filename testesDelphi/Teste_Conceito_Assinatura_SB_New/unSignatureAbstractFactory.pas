unit unSignatureAbstractFactory;

interface

uses
  unSignatureImage, unSignatureDTO;

type
  TSignatureAbstractFactory = class
  public
    class function GetSignatureAndStamp(const ASignatureDTO: TSignatureDTO): ISignatureImage;
  end;

implementation

uses
  unVerticalSignatureImageImp, unHorizontalSignatureImageImp;

{ TSignatureAbstractFactory }

class function TSignatureAbstractFactory.GetSignatureAndStamp(const ASignatureDTO: TSignatureDTO): ISignatureImage;
begin
  if ASignatureDTO.ImageWidth > ASignatureDTO.ImageHeight then
    Result := THorizontalSignatureImage.Create
  else
    Result := TVerticalSignatureImage.Create;
end;

end.
