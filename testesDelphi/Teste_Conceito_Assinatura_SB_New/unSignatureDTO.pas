unit unSignatureDTO;

interface

uses
  Vcl.Imaging.jpeg;

type
  TSignatureDTO = class
  private
  public
    ImageHeight: Integer;
    ImageWidth: Integer;
    FieldHeight: Integer;
    FieldWidth: Integer;
    SignatureImage: TJPEGImage;
    SignatureDate: TDateTime;
    User: String;
    Ip: String;
    Aplicacao: String;
    Latitude: Double;
    Longitude: Double;
    StampModelID: Integer;
    Name: String;
    CPF: String;
    constructor Create (const AImageHeight, AImageWidth, AFieldHeight, AFieldWidth: Integer; const ASignatureImage: TJPEGImage; const ASignatureDate: TDateTime; const AUser, AIp, AAplicacao: String; const ALatitude, ALongitude: Double; const AStampModelID: Integer; const AName, ACPF: String);
  end;

implementation

{ TSignatureDTO }

constructor TSignatureDTO.Create (const AImageHeight, AImageWidth,
  AFieldHeight, AFieldWidth: Integer; const ASignatureImage: TJPEGImage;
  const ASignatureDate: TDateTime; const AUser, AIp, AAplicacao: String;
  const ALatitude, ALongitude: Double; const AStampModelID: Integer;
  const AName, ACPF: String);
begin
  ImageHeight := AImageHeight;
  ImageWidth := AImageWidth;
  FieldHeight := AFieldHeight;
  FieldWidth := AFieldWidth;
  SignatureImage := ASignatureImage;
  SignatureDate := ASignatureDate;
  User := AUser;
  Ip := AIp;
  Aplicacao := AAplicacao;
  Latitude := ALatitude;
  Longitude := ALongitude;
  StampModelID := AStampModelID;
  Name := AName;
  CPF := ACPF;
end;

end.
