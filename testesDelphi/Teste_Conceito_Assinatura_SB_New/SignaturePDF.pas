unit SignaturePDF;

{$ifdef UNICODE}
  {$define SB_UNICODE_VCL}
{$endif}

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.Types,
  Graphics,
  Generics.Collections,

  Vcl.Imaging.pngimage,
  cxGraphics,
  Math,

  JPeg,

  //BlackBox
  SBPDF,
  SBPDFSecurity,
  SBUtils,
  SBTypes,
  SBCustomCertStorage,
  SBX509,
  SBWinCertStorage,
  SBHTTPTSPClient,
  SBHTTPSClient,
  unSignature,
  unSignatureImp,
  unSignatureDTO;

type
  TTSPAuthenticationType = (atNone = 0,
                            atUserNamePassword = 1,
                            atCertificate = 2);

  TTSPHashAlgorithm = (haSHA1 = 0,
                       haSHA256 = 1,
                       haSHA512 = 2);

  TTimestampingProtocol = class
  strict private
    FAuthenticationType: TTSPAuthenticationType;
    FUserName: string;
    FPassword: string;
    FCertificateId: string;
    FHashAlgorithm: TTSPHashAlgorithm;
    FServerURL: string;

  public
    property AuthenticationType: TTSPAuthenticationType read FAuthenticationType write FAuthenticationType;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property CertificateId: string read FCertificateId write FCertificateId;
    property HashAlgorithm: TTSPHashAlgorithm read FHashAlgorithm write FHashAlgorithm;
    property ServerURL: string read FServerURL write FServerURL;

    constructor Create;

    procedure Setup(const AServerURL: string = '';
      const AAuthenticationType: TTSPAuthenticationType = atNone;
      const AUserName: string = ''; const APassword: string = '';
      const ACertificateId: string = '';
      const AHashAlgorithm: TTSPHashAlgorithm = haSHA1);

    function GetSBBHashAlgorithm: Integer; overload;

    class function GetSBBHashAlgorithm(const AHashAlgorithm: TTSPHashAlgorithm): Integer; overload;
  end;

  THTTPTSPCliente = class(TElHTTPSClient)
  private
    procedure HandleCertificateValidate(ASender : TObject; AX509Certificate : TElX509Certificate; var AValidate: Boolean);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  end;

  TPDFSignatureField = class;
  TPDFCertificate    = class;

  TPDFSignature = class
  private
    FPDFStream     : TMemoryStream;
    FDocumentPDF   : TElPDFDocument;
    FListSignField : TObjectList<TPDFSignatureField>;
    FCertificateId : string;
    FCertificate   : TPDFCertificate;
    FUsuarioLogado : string;
    FExibiCarimbo  : boolean;
    FLatitude      : double;
    FLongitude     : double;
    FIp            : string;
    FAplicacao     : string;
    // Timestamp
    FUseTimestamping: Boolean;
    FTimestampingProtocol: TTimestampingProtocol;
    FTSPClient: TElHTTPTSPClient;
    FTSPHTTPSClient: THTTPTSPCliente;

  private
    procedure InternalOpen;
    procedure PopulateSignatureField;
    function GetSignField(AIndex: Integer): TPDFSignatureField;
    procedure AddImageToSignature(ASignature: TElPDFSignature; ASignatureField : TPDFSignatureField);
    procedure TreatSignatureImageAndAddStamp(const AImage, AOutImage: TJPEGImage; const ASignatureDate: TDateTime; const AUser: string; const AWidthFiel, AHeightField: Integer; const ASignature: TElPDFSignature);
    procedure SetWidgetProps(const ASignature: TElPDFSignature; const ASignatureDTO: TSignatureDTO);

    function GetBytesJPG(AJPGImage: TJPEGImage): ByteArray;

    function ConvertAndResizeToJpeg(const ASource, ADest: TStream; const AMaxWidth, AMaxHeight: Integer; const AOnlyReduce: Boolean = False; const AQuality: Cardinal = 90): Double;
    procedure AdjustImageSize(const AImage, ANewImage: TJPEGImage; const AWidth, AHeight: Integer);

  public
    StampModelID: Integer;
    EmployeeCPF: String;

    constructor Create(const ACertificateId: string; const AIgnoreValidateCertificate: Boolean = False);
    destructor Destroy; override;

    procedure LoadPDFFromStream(APDFStream: TStream);
    procedure LoadPDFFromFile(const AFileName: String);

    procedure SavePDFToFile(const AFileName: String);
    procedure SavePDFToStream(APDFStream: TStream);
    procedure SaveToStream(ADestStream: TMemoryStream);

    function CountSignFields: Integer;

    function SignField(ASignatureField: TPDFSignatureField): Boolean; overload;
    function SignField(AFieldIndex: Integer): Boolean; overload;

    procedure RemoveEmptySignatureField(const Index: Integer);

    function SignFieldRectF(const AIndex: Integer; const AConvertToMM: Boolean): TRectF;
    function SignFieldRect(const AIndex: Integer; const AConvertToMM: Boolean = True): TRect;

    procedure SetupTimestampingProtocol(const AUseTimestamping: Boolean = False;
      const ATSPURL: string = '';
      const ATSPAuthenticationType: TTSPAuthenticationType = atNone;
      const ATSPUserName: string = ''; const ATSPPassword: string = '';
      const ATSPCertificate: string = '';
      const ATSPHashAlgorithm: TTSPHashAlgorithm = haSHA1);

    property Field[Index:Integer]: TPDFSignatureField read GetSignField;
    property Certificate : TPDFCertificate read FCertificate;
    property UsuarioLogado: string read FUsuarioLogado write FUsuarioLogado;
    property ExibiCarimbo: boolean read FExibiCarimbo write FExibiCarimbo;
    property Latitude: double read FLatitude write FLatitude;
    property Longitude: double read FLongitude write FLongitude;
    property Ip: string read FIp write FIp;
    property Aplicacao: string read FAplicacao write FAplicacao;
  end;

  TPDFSignatureField = class
  strict private
    FJPGImage              : TJPEGImage;
    FSignInfo              : TElPDFSignatureInfo;
    FCertificate           : TPDFCertificate;
    FSignIndex             : Integer;
    FSigned                : Boolean;
    FAuthorName            : String;
    FReason                : String;
    FInvisible             : Boolean;
    FSignatureOrder        : Integer;
    FSignatureIdentifierID : Integer;
    FSignatureTitleType    : Integer;

    procedure ConvertPngToJPG(AStream: TMemoryStream);
  public
    constructor Create(const ASignInfo: TElPDFSignatureInfo; const ACertificateId: string; const AIgnoreValidateCertificate: Boolean = False);
    destructor Destroy; override;

    procedure LoadJPEGFromFile(const AFileName: String);
    procedure LoadJPEGFromStream(AStream: TMemoryStream);

    function GetJPGImage: TJPEGImage;

    property JPGImage    : TJPEGImage read FJPGImage;
    property SignInfo    : TElPDFSignatureInfo read FSignInfo;
    property Certificate : TPDFCertificate read FCertificate write FCertificate;
    property SignIndex   : Integer read FSignIndex;
    property Signed      : Boolean read FSigned write FSigned;
    property AuthorName  : String read FAuthorName write FAuthorName;
    property Reason      : String read FReason write FReason;
    property Invisible   : Boolean read FInvisible write FInvisible;

    property SignatureOrder: Integer read FSignatureOrder;
    property SignatureTitleType: Integer read FSignatureTitleType write FSignatureTitleType;
    property SignatureIdentifierID: Integer read FSignatureIdentifierID write FSignatureIdentifierID;
  end;

  TPDFSignatureFields = class(TObjectList<TPDFSignatureField>) //Does not owns objects
  public
    constructor Create; reintroduce;
  end;

  TPDFCertificate = class
  private
    FCertificateId             : String;
    FFileName                  : String;
    FPassword                  : String;
    FSystemStores              : TStringList;
    FCertStoreIndex            : Integer;
    FMemoryCertStorage         : TElMemoryCertStorage;
    FIgnoreValidateCertificate : Boolean;

    procedure ValidateCertificate(const ACertificate: TElX509Certificate);
  public
    constructor Create(const ADigitalCertificateId: string);
    destructor Destroy; override;

    function GetMemCertStorage: TElMemoryCertStorage;
    function GetCertificateFromRegistry(const ACertificateStorage : TElWinCertStorage): TElX509Certificate;
    procedure GetInfoFromCertificateUniqueId(AUniqueId: string;
      var ASerialNumber, AStorageName, AIssuerName: string);
    procedure GetCertificateInfo(const ACert: TElX509Certificate; var AUniqueId, ASerialNumber: string);

    property CertificateId             : String read FCertificateId write FCertificateId;
    property FileName                  : String read FFileName     write FFileName;
    property Password                  : String read FPassword     write FPassword;
    property SystemStores              : TStringList read FSystemStores write FSystemStores;
    property CertStoreIndex            : Integer read FCertStoreIndex write FCertStoreIndex;
    property IgnoreValidateCertificate : Boolean read FIgnoreValidateCertificate write FIgnoreValidateCertificate;
  end;

implementation

uses
  System.TypInfo,
  SBConstants,
  SBCertValidator
  ;

{ TPDFSignature }

procedure TPDFSignature.TreatSignatureImageAndAddStamp(const AImage, AOutImage: TJPEGImage; const ASignatureDate: TDateTime; const AUser: string; const AWidthFiel, AHeightField: Integer; const ASignature: TElPDFSignature);
var
  FinalImage: TBitmap;
  SignatureImageAndStamp: ISignature;
  SignatureDTO: TSignatureDTO;
begin
  SignatureDTO := TSignatureDTO.Create(AImage.Height, AImage.Width, AHeightField, AWidthFiel, AImage, ASignatureDate, AUser, FIp, FAplicacao, FLatitude, FLongitude, StampModelID, ASignature.AuthorName, EmployeeCPF);
  try
    SignatureImageAndStamp := TSignature.Create(SignatureDTO);
    FinalImage := SignatureImageAndStamp.GetFinalSignatureImage;
    try
      AOutImage.Assign(FinalImage);
      SetWidgetProps(ASignature, SignatureDTO);
    finally
      FinalImage.Free;
    end;
  finally
    SignatureDTO.Free;
  end;
end;

procedure TPDFSignature.AddImageToSignature(ASignature: TElPDFSignature; ASignatureField : TPDFSignatureField);
var
  NewImage: TJPEGImage;
  NewSignature: TJPEGImage;
  HoraAssinatura: TDateTime;
  HoraReal: TDateTime;
begin
  HoraReal := Now;
  HoraAssinatura := LocalTimeToUTCTime(HoraReal); //Trocar pelo carimbo do tempo;
  NewImage := TJPEGImage.Create;
  try
    // Make image the same size of SignatureField
	  try
      if FExibiCarimbo then
      begin
        NewSignature := TJPEGImage.Create;
        try
          TreatSignatureImageAndAddStamp(ASignatureField.JPGImage, NewSignature, HoraReal, FUsuarioLogado, ASignatureField.SignInfo.Width, ASignatureField.SignInfo.Height, ASignature);
          NewImage.Assign(NewSignature);
        finally
          NewSignature.Free;
        end;
      end
      else
      begin
        AdjustImageSize(ASignatureField.JPGImage, NewImage, ASignatureField.SignInfo.Width, ASignatureField.SignInfo.Height);
      end;
    except
      on e : Exception do
        raise Exception.Create('image load not valid.' + #13#10+ 'Error: ' + e.Message);
    end;

    ASignature.Invisible := False;
    ASignature.WidgetProps.AutoText              := False;
    ASignature.WidgetProps.AutoSize              := False;
    ASignature.WidgetProps.AutoPos               := False;
    ASignature.WidgetProps.NoView                := False;
    ASignature.WidgetProps.ToggleNoView          := False;
    ASignature.WidgetProps.AutoAdjustEncoding    := True;
    ASignature.WidgetProps.ShowOnAllPages        := True;
    ASignature.WidgetProps.AutoFontSize          := False;
    ASignature.WidgetProps.SigningTime           := HoraAssinatura;
    ASignature.WidgetProps.BackgroundStyle       := pbsCustom;
    ASignature.WidgetProps.AutoStretchBackground := True;
    ASignature.WidgetProps.HideDefaultText       := True;
    ASignature.WidgetProps.IgnoreExistingAppearance := True;

    ASignature.WidgetProps.Background.Width      := ASignature.WidgetProps.Width;
    ASignature.WidgetProps.Background.Height     := ASignature.WidgetProps.Height;
    ASignature.WidgetProps.Background.ImageType  := pitJPEG;
    ASignature.WidgetProps.Background.Data       := GetBytesJPG(NewImage);

  finally
    NewImage.Free;
  end;
end;


procedure TPDFSignature.AdjustImageSize(const AImage, ANewImage: TJPEGImage;
  const AWidth, AHeight: Integer);
var
  NewJPG: TMemoryStream;
  OldJPG: TMemoryStream;
  Bitmap: TBitmap;
  NewWidth: Integer;
  NewHeight: Integer;
  AdjustWidth: Extended;
  AdjustHeight: Extended;
  AdjustFactor: Extended;
  HPos: Integer;
  VPos: Integer;
begin
  // Nao precisa redimensionar
  if (AImage.Width = AWidth) and (AImage.Height = AHeight) then
  begin
    ANewImage.Assign(AImage);
    Exit;
  end;

  NewJPG := TMemoryStream.Create;
  OldJPG := TMemoryStream.Create;
  Bitmap := TBitmap.Create;
  try
    // Ajusta proporcionalmente o tamanho da imagem original para caber na
    // imagem destino verificando se a diferença de altura ou de largura deve
    // determinar o ajuste
    AdjustWidth := AImage.Width / AWidth;
    AdjustHeight := AImage.Height / AHeight;
    AdjustFactor := Max(AdjustWidth, AdjustHeight);
    NewWidth := Trunc((AImage.Width / AdjustFactor) + 0.99999);
    NewHeight := Trunc((AImage.Height / AdjustFactor) + 0.99999);

    AImage.SaveToStream(OldJPG);
    OldJPG.Position := 0;

    ConvertAndResizeToJpeg(OldJPG, NewJPG, NewWidth, NewHeight);

    NewJPG.Position := 0;
    ANewImage.LoadFromStream(NewJPG);


    // Calcula a posicao onde a imagem devera ser posicionada na nova imagem
    // Horizontalmente: Centralizar a imagem
    // Verticalmente: Colar na parte inferior da nova imagem por se tratar de
    //                uma assinatura
    HPos := Trunc(AWidth div 2) - Trunc((NewWidth div 2) + 0.99999);
    HPos := Math.Max(0, HPos);
    VPos := AHeight - NewHeight + 1;

    // Cria uma nova imagem toda branca com o tamanho do campo e encaixa a
    // imagem original na nova imagem
    Bitmap.Canvas.Brush.Color := clWhite;
    Bitmap.Canvas.Brush.Style := bsSolid;
    Bitmap.SetSize(AWidth, AHeight);
    Bitmap.Canvas.CopyRect(TRect.Create(HPos, VPos, HPos + NewWidth - 1, VPos + NewHeight - 1), ANewImage.Canvas, TRect.Create(0, 0, NewWidth - 1, NewHeight - 1));

    ANewImage.Assign(Bitmap);

  finally
    NewJPG.Free;
    OldJPG.Free;
    Bitmap.Free;
  end;
end;

function TPDFSignature.ConvertAndResizeToJpeg(const ASource, ADest: TStream; const AMaxWidth, AMaxHeight: Integer; const AOnlyReduce: Boolean = False; const AQuality: Cardinal = 90): Double;
var
  Pict: TPicture;
  FactorW, FactorH: Double;
  NewW, NewH: Integer;
  newBmp: TBitmap;
  oldBmp: TBitmap;
  jpg: TJPEGImage;
begin
  Pict := TPicture.Create;
  try
    Pict.LoadFromStream(ASource);
    FactorW := IfThen(AMaxWidth > 0, Pict.Width / AMaxWidth, 1.0);
    FactorH := IfThen(AMaxHeight > 0, Pict.Height / AMaxHeight, 1.0);
    Result := IfThen(FactorW > FactorH, FactorW, FactorH);
    if AOnlyReduce and (Result < 1.0) then
      Result := 1.0;
    NewW := Round(Pict.Width / Result);
    NewH := Round(Pict.Height / Result);
    newBmp := Graphics.TBitmap.Create;
    oldBmp := Graphics.TBitMap.Create;
    try
      oldBmp.PixelFormat := pf32bit;
      oldBmp.Transparent := True;
      oldBmp.Assign(Pict.Graphic);
      newBmp.PixelFormat := pf32bit;
      newBmp.Transparent := True;
      newBmp.SetSize(NewW, NewH);
      cxSmoothResizeBitmap(oldBmp, newBmp, True);
      jpg := TJPEGImage.Create;
      try
        jpg.CompressionQuality := AQuality;
        jpg.Scale := jsFullSize;
        jpg.Performance := jpBestQuality;
        jpg.PixelFormat := jf24Bit;
        jpg.ProgressiveEncoding := False;
        jpg.Smoothing := True;
        jpg.Assign(newBmp);
        jpg.Compress;
        ADest.Size := 0;
        jpg.SaveToStream(ADest);
      finally
        jpg.Free;
      end;
    finally
      newBmp.Free;
      oldBmp.Free;
    end;
  finally
    Pict.Free;
  end;
end;

function TPDFSignature.GetBytesJPG(AJPGImage: TJPEGImage) : ByteArray;
var
  Stream: TStream;
begin
  Stream := TMemoryStream.Create;
  try
    AJPGImage.SaveToStream(Stream);
    Stream.Position := 0;

    if (Stream.Size > 0) then
    begin
      SetLength(Result, Stream.Size);
      Stream.Read(Result[0], Stream.Size);
    end
    else
      Result := nil;
  finally
    Stream.Free;
  end;
end;


function TPDFSignature.CountSignFields: Integer;
begin
  Result := FListSignField.Count;
end;

constructor TPDFSignature.Create;
begin
  FCertificateId := ACertificateId;
  FPDFStream     := TMemoryStream.Create;
  FDocumentPDF   := TElPDFDocument.Create(nil);

  FTimestampingProtocol := TTimestampingProtocol.Create;

  FTSPHTTPSClient := THTTPTSPCliente.Create(nil);
  FTSPClient := TElHTTPTSPClient.Create(nil);
  FTSPClient.HTTPClient := FTSPHTTPSClient;

  // Reseta o uso do servidor de carimbo de tempo (timestamp) para que ele nao
  // seja utilizado por default
  SetupTimestampingProtocol;

  // Faz com que a instancia de TElPDFDocument destrua os handlers que ela mesma criar.
  // Sem esta configuracao, ha vazamento de memoria
  FDocumentPDF.OwnActivatedSecurityHandlers := true;

  FListSignField := TObjectList<TPDFSignatureField>.Create(True);
  FCertificate   := TPDFCertificate.Create(FCertificateId);
  FCertificate.IgnoreValidateCertificate := AIgnoreValidateCertificate;
end;

destructor TPDFSignature.Destroy;
begin
  if Assigned(FPDFStream) then FPDFStream.Free;
  if Assigned(FDocumentPDF) then FDocumentPDF.Free;
  if Assigned(FListSignField) then FListSignField.Free;
  if Assigned(FCertificate) then FCertificate.Free;
  if Assigned(FTimestampingProtocol) then FreeAndNil(FTimestampingProtocol);
  if Assigned(FTSPHTTPSClient) then FreeAndNil(FTSPHTTPSClient);
  if Assigned(FTSPClient) then FreeAndNil(FTSPClient);
end;

function TPDFSignature.GetSignField(AIndex: Integer): TPDFSignatureField;
begin
  if (AIndex >= 0) and (AIndex <= FListSignField.Count - 1) then
    Result := FListSignField[AIndex]
  else
    raise Exception.CreateFmt('Index %d do not exist in list signature fields', [AIndex]);
end;

procedure TPDFSignature.InternalOpen;
begin
  FPDFStream.Position := 0;
  if (FPDFStream.Size > 0) then
  begin
    if (FDocumentPDF.Opened) then
      FDocumentPDF.Close(False);

    FDocumentPDF.Open(FPDFStream);

    if (FDocumentPDF.Encrypted) then
      raise Exception.Create('Cannot sign the encrypted pdf file')
    else
      PopulateSignatureField;
  end
  else
    raise Exception.Create('Not loaded stream from pdf file');
end;

procedure TPDFSignature.LoadPDFFromFile(const AFileName: String);
begin
  if Assigned(FListSignField) then
  begin
    FreeAndNil(FListSignField);
    FListSignField := TObjectList<TPDFSignatureField>.Create(True);
  end;

  FPDFStream.LoadFromFile(AFileName);
  InternalOpen;
end;

procedure TPDFSignature.LoadPDFFromStream(APDFStream: TStream);
begin
  if Assigned(FListSignField) then
  begin
    FreeAndNil(FListSignField);
    FListSignField := TObjectList<TPDFSignatureField>.Create(True);
  end;

  FPDFStream.LoadFromStream(APDFStream);
  InternalOpen;
end;

procedure TPDFSignature.PopulateSignatureField;
var
  SignField : TPDFSignatureField;
  c         : Integer;
begin
  if (FDocumentPDF.EmptySignatureFieldCount > 0) then
  begin
    for c := 0 to FDocumentPDF.EmptySignatureFieldCount - 1 do
    begin
      SignField := TPDFSignatureField.Create(FDocumentPDF.EmptySignatureFields[c], FCertificateId, FCertificate.IgnoreValidateCertificate);
      FListSignField.Add(SignField);
    end;
  end;
end;

procedure TPDFSignature.RemoveEmptySignatureField(const Index: Integer);
begin
  FDocumentPDF.RemoveEmptySignatureField(Index);
end;

procedure TPDFSignature.SavePDFToFile(const AFileName: String);
begin
  FPDFStream.Position := 0;
  FPDFStream.SaveToFile(AFileName);
end;

procedure TPDFSignature.SavePDFToStream(APDFStream: TStream);
begin
  if (FDocumentPDF.Opened) then
    FDocumentPDF.Close(True);

  FPDFStream.Position := 0;
  FPDFStream.SaveToStream(APDFStream);
end;

procedure TPDFSignature.SaveToStream(ADestStream: TMemoryStream);
begin
  if (FDocumentPDF.Opened) then
    FDocumentPDF.Close(True);

  FPDFStream.Position := 0;
  ADestStream.Clear;
  ADestStream.LoadFromStream(FPDFStream);
  ADestStream.Position := 0;
end;

procedure TPDFSignature.SetupTimestampingProtocol(const AUseTimestamping: Boolean; const ATSPURL: string;
  const ATSPAuthenticationType: TTSPAuthenticationType;
  const ATSPUserName, ATSPPassword, ATSPCertificate: string;
  const ATSPHashAlgorithm: TTSPHashAlgorithm);
begin
  FUseTimestamping := AUseTimestamping;

  // Configura o timestamp protocol client
  FTimestampingProtocol.Setup(ATSPURL, ATSPAuthenticationType, ATSPUserName, ATSPPassword, ATSPCertificate, ATSPHashAlgorithm);

  // Verifica o tipo de autenticacao do servidor de carimbo de tempo
  case FTimestampingProtocol.AuthenticationType of
    atUserNamePassword:
    begin
      FTSPHTTPSClient.RequestParameters.Username := FTimestampingProtocol.UserName;
      FTSPHTTPSClient.RequestParameters.Password := FTimestampingProtocol.Password;
    end;
    atCertificate:
      raise Exception.Create('Certificate authentication is not implemented for timestamp servers');
  end;

  FTSPClient.URL := FTimestampingProtocol.ServerURL;
  FTSPClient.HashAlgorithm := FTimestampingProtocol.GetSBBHashAlgorithm;
end;

procedure TPDFSignature.SetWidgetProps(const ASignature: TElPDFSignature; const ASignatureDTO: TSignatureDTO);
  function GetApplicationName(const AAplicacao: String): String;
  begin
    if AAplicacao = 'ApWebDispatcher' then
      Result := 'Portal Web'
    else
      Result := 'App Mobile'
  end;
begin
  ASignature.WidgetProps.CustomText.Clear;
  case ASignatureDTO.StampModelID of
    0, 1:
      ASignature.WidgetProps.CustomText.Add('Usuario : ' + Format('%-16s', [ASignatureDTO.User]), 3, 25, 5.75);
    2:
    begin
      ASignature.WidgetProps.CustomText.Add('Nome : ' + Format('%-30s', [ASignatureDTO.Name]), 3, 30, 5.75);
      ASignature.WidgetProps.CustomText.Add('CPF: ' + Format('%-14s', [ASignatureDTO.CPF]), 3, 25, 5.75);
    end;
  end;

  ASignature.WidgetProps.CustomText.Add('Em : ' + Format('%-14s', [FormatDateTime('dd/mm/yy hh:nn', ASignatureDTO.SignatureDate)]), 3, 20, 5.75);
  ASignature.WidgetProps.CustomText.Add('Ip : ' + Format('%-25s', [ASignatureDTO.Ip]), 3, 15, 5.75);
  ASignature.WidgetProps.CustomText.Add('Client: ' + Format('%-10s', [GetApplicationName(ASignatureDTO.Aplicacao)]), 3, 10, 5.75);
  ASignature.WidgetProps.CustomText.Add('Local : ' + FloatToStr(ASignatureDTO.Latitude) + ', ' + FloatToStr(ASignatureDTO.Longitude), 3, 5, 5.75);
end;

function TPDFSignature.SignField(AFieldIndex: Integer): Boolean;
begin
  Result := SignField(GetSignField(AFieldIndex));
end;

function TPDFSignature.SignFieldRect;
begin
  if (AIndex >= 0) and (AIndex < FDocumentPDF.EmptySignatureFieldCount) then
  begin
    Exit(Bounds(
                FDocumentPDF.EmptySignatureFields[AIndex].OffsetX,
                FDocumentPDF.EmptySignatureFields[AIndex].OffsetY,
                FDocumentPDF.EmptySignatureFields[AIndex].Width,
                FDocumentPDF.EmptySignatureFields[AIndex].Height
               )
          );
  end
  else
    Exit(Bounds(0, 0, 0, 0));
end;

function TPDFSignature.SignFieldRectF(const AIndex: Integer; const AConvertToMM: Boolean): TRectF;
const
  PT_TO_MM = 25.4 / 72.0;
var
  L, B, W, H, K: Double;
begin
  if (AIndex < 0) or (AIndex >= FDocumentPDF.EmptySignatureFieldCount) then
    Exit(TRectF.Empty);

  L := FDocumentPDF.EmptySignatureFields[AIndex].OffsetX;
  B := FDocumentPDF.EmptySignatureFields[AIndex].OffsetY;
  W := FDocumentPDF.EmptySignatureFields[AIndex].Width;
  H := FDocumentPDF.EmptySignatureFields[AIndex].Height;

  if AConvertToMM then
  begin
    K := PT_TO_MM;
    L := L * K; B := B * K; W := W * K; H := H * K;
  end;

  Result := TRectF.Create(L, B, L + W, B + H);
end;

function TPDFSignature.SignField(ASignatureField: TPDFSignatureField): Boolean;
var
  EmptyFieldIndex,
  Index            : Integer;
  Signature        : TElPDFSignature;
  PublicKeyHandler : TElPDFPublicKeySecurityHandler;
begin
  Result := False;

  if (FDocumentPDF.Opened) and (ASignatureField <> nil) then
  begin
    Index := -1;
    for EmptyFieldIndex := 0 to FDocumentPDF.EmptySignatureFieldCount - 1 do
    begin
      if (FDocumentPDF.EmptySignatureFields[EmptyFieldIndex].FieldName = ASignatureField.SignInfo.FieldName) and (FDocumentPDF.EmptySignatureFields[EmptyFieldIndex].MappingName = ASignatureField.SignInfo.MappingName) then
      begin
        Index := EmptyFieldIndex;
        Break;
      end;
    end;

    if (Index > -1) then
    begin
      Index := FDocumentPDF.AddSignature(Index);

      PublicKeyHandler := TElPDFPublicKeySecurityHandler.Create(nil);
      try
        Signature := FDocumentPDF.Signatures[Index];

        Signature.WidgetProps.Width := FDocumentPDF.EmptySignatureFields[EmptyFieldIndex].Width;
        Signature.WidgetProps.Height := FDocumentPDF.EmptySignatureFields[EmptyFieldIndex].Height;
        Signature.WidgetProps.OffsetX := FDocumentPDF.EmptySignatureFields[EmptyFieldIndex].OffsetX;
        Signature.WidgetProps.OffsetY := FDocumentPDF.EmptySignatureFields[EmptyFieldIndex].OffsetY;

        Signature.Handler       := PublicKeyHandler;
        Signature.AuthorName    := ASignatureField.AuthorName;
        Signature.SigningTime   := LocalTimeToUTCTime(Now);
        Signature.Reason        := ASignatureField.Reason;
        Signature.SignatureType := stDocument;
        Signature.Invisible     := ASignatureField.Invisible;

        if not ASignatureField.GetJPGImage.Empty then
          // Do the sign here
          AddImageToSignature(Signature, ASignatureField);

        PublicKeyHandler.SignatureType := pstPKCS7SHA1;
        PublicKeyHandler.CertStorage   := ASignatureField.Certificate.GetMemCertStorage;
        PublicKeyHandler.CustomName    := 'Adobe.PPKMS';

        // Verifica se deve utilizar servidor de carimbo de tempo (timestamp)
        if (FUseTimestamping) then
          PublicKeyHandler.TSPClient := FTSPClient
        else
          PublicKeyHandler.TSPClient := nil;

        FDocumentPDF.Close(True);
        ASignatureField.Signed := True;
      finally
        PublicKeyHandler.Free;
      end;

      FPDFStream.Position := 0;
      FDocumentPDF.Open(FPDFStream);

      Result := True;
    end;
  end;
end;

{ TPDFSignatureField }

procedure TPDFSignatureField.ConvertPngToJPG(AStream: TMemoryStream);
var
  Png : TPngImage;
  Bmp : TBitmap;
  Jpg : TJPEGImage;
begin
  AStream.Position := 0;
  Png := TPngImage.Create;
  Bmp := TBitmap.Create;
  Jpg := TJPEGImage.Create;
  try
    Png.LoadFromStream(AStream);
    Bmp.Assign(Png);
    Jpg.Assign(Bmp);
    AStream.Clear;
    Jpg.SaveToStream(AStream);
    AStream.Position := 0;
  finally
    Png.Free;
    Bmp.Free;
    Jpg.Free;
  end;
end;

constructor TPDFSignatureField.Create;
begin
  FJPGImage    := TJPEGImage.Create;
  FCertificate := TPDFCertificate.Create(ACertificateId);
  FCertificate.IgnoreValidateCertificate := AIgnoreValidateCertificate;
  FSigned      := False;
  FAuthorName  := '';
  FReason      := '';
  FInvisible   := False;
  FSignInfo := ASignInfo;

  if ASignInfo.MappingName <> '' then
  begin
  end
  else
  begin
    FSignatureTitleType := 0;
  end;
end;

destructor TPDFSignatureField.Destroy;
begin
  FSignInfo := nil;
  if Assigned(FJPGImage) then FJPGImage.Free;
  if Assigned(FCertificate) then FCertificate.Free;
  inherited;
end;

function TPDFSignatureField.GetJPGImage: TJPEGImage;
begin
  Result := FJPGImage;
end;

procedure TPDFSignatureField.LoadJPEGFromFile(const AFileName: String);
begin
  FJPGImage.LoadFromFile(AFileName);
end;

procedure TPDFSignatureField.LoadJPEGFromStream(AStream: TMemoryStream);
begin
  AStream.Position := 0;
  try
    FJPGImage.LoadFromStream(AStream);
  except
    on e : Exception do
    begin
      ConvertPngToJPG(AStream);
      FJPGImage.LoadFromStream(AStream);
    end;
  end;
end;

{ TPDFCertificate }

constructor TPDFCertificate.Create(const ADigitalCertificateId: string);
begin
  FCertificateId            := ADigitalCertificateId;
  FMemoryCertStorage        := TElMemoryCertStorage.Create(nil);
  FSystemStores             := TStringList.Create;
  IgnoreValidateCertificate := False;
end;

destructor TPDFCertificate.Destroy;
begin
  if Assigned(FMemoryCertStorage) then FMemoryCertStorage.Free;
  if Assigned(FSystemStores) then FSystemStores.Free;
end;

function TPDFCertificate.GetCertificateFromRegistry(
  const ACertificateStorage : TElWinCertStorage): TElX509Certificate;
var
  CertificateSerialNumber: string;
  CertificateIssuerName: string;
  CertificateStorageName: string;
  CertifId: string;
  CertifSN: string;
  i: Integer;
begin
  Result := nil;
  try
    // Procura e carrega o certificado configurado
    GetInfoFromCertificateUniqueId(FCertificateId, CertificateSerialNumber, CertificateStorageName, CertificateIssuerName);

    ACertificateStorage.SystemStores.Text := CertificateStorageName;
    for i := 0 to ACertificateStorage.Count - 1 do
    begin
      Result := ACertificateStorage.Certificates[i];
      GetCertificateInfo(Result, CertifId, CertifSN);
      if (SameText(CertifId, FCertificateId)) and
         (SameText(CertifSN, CertificateSerialNumber)) then
      begin
        Break;
      end;
      Result := nil;
    end;
  except
    on E: Exception do
    begin
      Result := nil;
    end;
  end;
end;

procedure TPDFCertificate.GetCertificateInfo(const ACert: TElX509Certificate;
  var AUniqueId, ASerialNumber: string);
var
  buf: ByteArray;
begin
  // Adiciona o issuer
  AUniqueId := ACert.IssuerName.CommonName;
  if (AUniqueId = '') then
    AUniqueId := ACert.IssuerName.Organization;
  // Adiciona o nome do Storage (no inicio)
  AUniqueId := ACert.StorageName + '-' + AUniqueId;
  // Adiciona o serial number (ele pode repetir entre issuers diferentes)
  buf := ACert.SerialNumber;
  ASerialNumber := BinaryToString(@Buf[0], Length(Buf));
  AUniqueId := AUniqueId + '-' + ASerialNumber;
end;

procedure TPDFCertificate.GetInfoFromCertificateUniqueId(AUniqueId: string;
  var ASerialNumber, AStorageName, AIssuerName: string);
var
  posStr: Integer;
begin
  AStorageName  := '';
  AIssuerName   := '';
  ASerialNumber := '';
  // Pega o nome do Storage (no inicio)
  posStr := Pos('-', AUniqueId);
  if (posStr > 0) then
  begin
    AStorageName := Copy(AUniqueId, 1, posStr - 1);
    AUniqueId    := Copy(AUniqueId, posStr + 1, Length(AUniqueId));
  end;
  // Pega o serial number
  posStr := LastDelimiter('-', AUniqueId);
  if (posStr > 0) then
  begin
    ASerialNumber := Copy(AUniqueId, posStr + 1, Length(AUniqueId));
    AUniqueId   := Copy(AUniqueId, 1, posStr - 1);
  end;
  // Pega o issuer
  AIssuerName := AUniqueId;
end;

function TPDFCertificate.GetMemCertStorage: TElMemoryCertStorage;
var
  Cert    : TElX509Certificate;
  WinCert : TElWinCertStorage;
begin
  // Se o custom certificate storage ainda esta vazio, tenta encontrar e
  // adicionar o certificado configurado ao storage. Caso contrario, continua
  // usando o storage ja configurado
  if (FMemoryCertStorage.Count = 0) then
  begin
    FMemoryCertStorage.Clear;

    WinCert := TElWinCertStorage.Create(nil);
    try
      Cert := GetCertificateFromRegistry(WinCert);
      if (Cert <> nil) then
      begin
        ValidateCertificate(Cert);
        FMemoryCertStorage.Add(Cert, True);
      end
      else
      begin
        if (FFileName <> '') and (FPassword <> '') then
        begin
          Cert := TElX509Certificate.Create(nil);
          try
            if (Cert.LoadFromFileAuto(FFileName, FPassword) = 0) then
            begin
              ValidateCertificate(Cert);
              FMemoryCertStorage.Add(Cert, True);
            end
            else
              raise Exception.Create('fail load certificate');
          finally
            Cert.Free;
          end;
        end
        else if (FCertStoreIndex > -1) then
        begin
          if (FSystemStores.Count = 0) then
            FSystemStores.Add('My');

          WinCert.SystemStores.Assign(FSystemStores);
          if (WinCert.Count > 0) then
          begin
            if (WinCert.Certificates[FCertStoreIndex] <> nil) then
            begin
              if not IgnoreValidateCertificate then
                ValidateCertificate(WinCert.Certificates[FCertStoreIndex]);
              FMemoryCertStorage.Add(WinCert.Certificates[FCertStoreIndex], True);
            end
            else
              raise Exception.Create('invalid index certificate');
          end;
        end;
      end;
    finally
      WinCert.Free;
    end;
  end;

  Result := FMemoryCertStorage;
end;

procedure TPDFCertificate.ValidateCertificate(
  const ACertificate: TElX509Certificate);
begin
  if (Now < ACertificate.ValidFrom) or (Now > ACertificate.ValidTo) then
    raise Exception.CreateFmt('The certificate is expired (certificate validity: from %s to %s)', [FormatDateTime('dd/MM/yyyy hh:mm:ss', ACertificate.ValidFrom), FormatDateTime('dd/MM/yyyy hh:mm:ss', ACertificate.ValidTo)]);
end;

{ TTimestampingProtocol }

constructor TTimestampingProtocol.Create;
begin
  inherited;
  Setup;
end;

class function TTimestampingProtocol.GetSBBHashAlgorithm(
  const AHashAlgorithm: TTSPHashAlgorithm): Integer;
begin
  case AHashAlgorithm of
    haSHA1:   Result := SB_ALGORITHM_DGST_SHA1;
    haSHA256: Result := SB_ALGORITHM_DGST_SHA256;
    haSHA512: Result := SB_ALGORITHM_DGST_SHA512;
  else
    raise Exception.CreateFmt('Hash algorithm %s not suported', [GetEnumName(System.TypeInfo(TTSPHashAlgorithm), Ord(AHashAlgorithm))]);
  end;
end;

function TTimestampingProtocol.GetSBBHashAlgorithm: Integer;
begin
  Result := TTimestampingProtocol.GetSBBHashAlgorithm(FHashAlgorithm);
end;

procedure TTimestampingProtocol.Setup(const AServerURL: string = '';
  const AAuthenticationType: TTSPAuthenticationType = atNone;
  const AUserName: string = ''; const APassword: string = '';
  const ACertificateId: string = '';
  const AHashAlgorithm: TTSPHashAlgorithm = haSHA1);
begin
  FServerURL := AServerURL;
  FAuthenticationType := AAuthenticationType;
  FUserName := AUserName;
  FPassword := APassword;
  FCertificateId := ACertificateId;
  FHashAlgorithm := AHashAlgorithm;
end;

{ THTTPTSPCliente }

constructor THTTPTSPCliente.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnCertificateValidate := HandleCertificateValidate;
  CertStorage := TElWinCertStorage.Create(nil);
end;

destructor THTTPTSPCliente.Destroy;
begin
  if Assigned(CertStorage) then CertStorage.Free;
  inherited;
end;

procedure THTTPTSPCliente.HandleCertificateValidate(ASender: TObject;
  AX509Certificate: TElX509Certificate; var AValidate: Boolean);
var
  Validity: TSBCertificateValidity;
  Reason: TSBCertificateValidityReason;
begin
  InternalValidate(Validity, Reason);
  AValidate := (Validity in [cvOk, cvSelfSigned]);
end;

{ TPDFSignatureFields }

constructor TPDFSignatureFields.Create;
begin
  inherited Create(False);
end;

end.
