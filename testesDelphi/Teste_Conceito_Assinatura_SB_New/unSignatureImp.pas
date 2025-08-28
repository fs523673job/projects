unit unSignatureImp;

interface

uses
  unSignature, unSignatureImage, unSignatureDTO, Vcl.Graphics;

type
  TSignature = class(TInterfacedObject, ISignature)
  private
    FSignatureImage: ISignatureImage;
    FSignatureDTO: TSignatureDTO;
  public
    constructor Create(const ASignatureDTO: TSignatureDTO);
    function GetFinalSignatureImage: TBitmap;
  end;

implementation

uses
  unSignatureAbstractFactory;

{ TSignature }

constructor TSignature.Create(const ASignatureDTO: TSignatureDTO);
begin
  FSignatureDTO := ASignatureDTO;
  FSignatureImage := TSignatureAbstractFactory.GetSignatureAndStamp(FSignatureDTO);
end;

function TSignature.GetFinalSignatureImage: TBitmap;
begin
  Result := FSignatureImage.TreatSignatureImage(FSignatureDTO);
end;

end.
