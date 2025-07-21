unit SignaturePDF;

{$ifdef UNICODE}
  {$define SB_UNICODE_VCL}
{$endif}

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  Generics.Collections,
  Graphics,
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

  //SYNPDF
  SynCommons,
  SynPDF
  ;

type
  TPDFSignatureField = class;
  TPDFCertificate    = class;

  TPDFSignature = class
  private
    FPDFStream      : TMemoryStream;
    FDocumentPDF    : TElPDFDocument;
    FListSignField  : TObjectList<TPDFSignatureField>;
    FCertificate    : TPDFCertificate;
  private
    procedure InternalOpen;
    procedure PopulateSignatureField;
    function GetSignField(AIndex: Integer): TPDFSignatureField;
    procedure AddImageToSignature(ASignature: TElPDFSignature; ASignatureField : TPDFSignatureField);
    function GetBytesJPG(AJPGImage: TJPEGImage): ByteArray;

    function ConvertAndResizeToJpeg(const ASource, ADest: TStream; const AMaxWidth, AMaxHeight: Integer; const AOnlyReduce: Boolean = False; const AQuality: Cardinal = 90): Double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadPDFFromStream(APDFStream: TStream);
    procedure LoadPDFFromFile(const AFileName: String);

    procedure SavePDFToFile(const AFileName: String);
    procedure SavePDFToStream(APDFStream: TStream);

    function CountSignFields: Integer;

    function SignField(ASignatureField: TPDFSignatureField): Boolean; overload;
    function SignField(AFieldIndex: Integer): Boolean; overload;

    function RubricateField(ASignatureField: TPDFSignatureField): Boolean; overload;
    function RubricateField(AFieldIndex: Integer): Boolean; overload;

    function RemoveSignature(Index: integer): TMemoryStream;
    function RemoveEmptySignatureField(Index: Integer): TMemoryStream;

    procedure PreparePDF(const FileName: String = ''; const AImagePath: String = '');

    function AddSignField(const ASignName: String): Boolean;

    property Field[Index:Integer]: TPDFSignatureField read GetSignField;
    property Certificate : TPDFCertificate read FCertificate;
  end;

  TPDFSignatureField = class
  private
    FJPGImage    : TJPEGImage;
    FTitle       : String;
    FSignInfo    : TElPDFSignatureInfo;
    FCertificate : TPDFCertificate;
    FSignIndex   : Integer;
    FSigned      : Boolean;
    FAuthorName  : String;
    FReason      : String;
    FInvisible   : Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadJPEGFromFile(const AFileName: String);
    procedure LoadJPEGFromStream(AStream: TMemoryStream);

    function GetJPGImage: TJPEGImage;

    property Title       : String read FTitle;
    property SignInfo    : TElPDFSignatureInfo read FSignInfo;
    property Certificate : TPDFCertificate read FCertificate write FCertificate;
    property SignIndex   : Integer read FSignIndex;
    property Signed      : Boolean read FSigned;
    property AuthorName  : String read FAuthorName write FAuthorName;
    property Reason      : String read FReason write FReason;
    property Invisible   : Boolean read FInvisible write FInvisible;
  end;

  TPDFCertificate = class
  private
    FFileName          : String;
    FPassword          : String;
    FSystemStores      : TStringList;
    FCertStoreIndex    : Integer;
    FMemoryCertStorage : TElMemoryCertStorage;
  public
    constructor Create;
    destructor Destroy; override;

    function GetMemCertStorage: TElMemoryCertStorage;

    property FileName       : String read FFileName     write FFileName;
    property Password       : String read FPassword     write FPassword;
    property SystemStores   : TStringList read FSystemStores write FSystemStores;
    property CertStoreIndex : Integer read FCertStoreIndex write FCertStoreIndex;
  end;

implementation

{ TPDFSignature }

procedure TPDFSignature.AddImageToSignature(ASignature: TElPDFSignature; ASignatureField : TPDFSignatureField);
var
  PDFImage: TElPDFImage;
  NewJPG  : TMemoryStream;
  OldJPG  : TMemoryStream;
begin
  //https://sbb.eldos.com/forum/read.php?FID=7&TID=6532&MID=36599#message36599

  NewJPG := TMemoryStream.Create;
  OldJPG := TMemoryStream.Create;

  //ASignatureField.FJPGImage.SaveToStream(OldJPG);

  OldJPG.Position := 0;

  var width  := ASignatureField.SignInfo.Width;
  var height := ASignatureField.SignInfo.Height;

  if width > 1000 then
    width := 40;

  if height > 1000 then
    height := 40;

  //ConvertAndResizeToJpeg(OldJPG, NewJPG, width, height);

  //ASignatureField.FJPGImage.LoadFromStream(NewJPG);

  //ADICIONANDO UM IMAGEM CUSTOMIZADA NA ASSINATURA
  //ASignature.Invisible := False;
  //ASignature.WidgetProps.AutoSize := False;
  //ASignature.WidgetProps.AutoPos  := False;

  //ASignature.WidgetProps.OffsetX := ASignatureField.SignInfo.OffsetX;
  //ASignature.WidgetProps.OffsetY := ASignatureField.SignInfo.OffsetX;
  //ASignature.WidgetProps.Width   := ASignatureField.SignInfo.Width;
  //ASignature.WidgetProps.Height  := ASignatureField.SignInfo.Height;

//  PDFImage := TElPDFImage.Create;
//  PDFImage.ImageType := pitJPEG;
//  PDFImage.Width     := ASignatureField.SignInfo.Width;
//  PDFImage.Height    := ASignatureField.SignInfo.Height;
//  PDFImage.Data      := GetBytesJPG(ASignatureField.GetJPGImage);

  //ASignature.WidgetProps.BackgroundStyle := pbsNoBackground;
  //ASignature.WidgetProps.AddImage(PDFImage, 0, 0, 66 * PDFImage.Width / PDFImage.Height * ASignature.WidgetProps.Height / ASignature.WidgetProps.Width, 66);
//  ASignature.WidgetProps.AddImage(PDFImage);//, 0, 0, 66 * PDFImage.Width / PDFImage.Height * ASignature.WidgetProps.Height / ASignature.WidgetProps.Width, 66);

  (*
  ASignature.WidgetProps.NoView        := False;
  ASignature.WidgetProps.ToggleNoView  := False;
  ASignature.WidgetProps.ShowTimestamp := True;
  ASignature.WidgetProps.SigningTime   := Now;

  ASignature.WidgetProps.AutoAdjustEncoding := True;
  ASignature.WidgetProps.ShowOnAllPages     := True;
  ASignature.WidgetProps.AutoFontSize       := True;

  ASignature.WidgetProps.BackgroundStyle      := pbsCustom;
  ASignature.WidgetProps.Background.Width     := ASignatureField.SignInfo.Width;
  ASignature.WidgetProps.Background.Height    := ASignatureField.SignInfo.Height;
  ASignature.WidgetProps.Background.Data      := GetBytesJPG(ASignatureField.GetJPGImage);
  ASignature.WidgetProps.Background.ImageType := pitJPEG;
  *)

    ASignature.Invisible := False;
    ASignature.WidgetProps.AutoText              := False;
    ASignature.WidgetProps.AutoSize              := False;
    ASignature.WidgetProps.AutoPos               := False;
    ASignature.WidgetProps.NoView                := False;
    ASignature.WidgetProps.ToggleNoView          := False;
    ASignature.WidgetProps.AutoAdjustEncoding    := True;
    ASignature.WidgetProps.ShowOnAllPages        := True;
    ASignature.WidgetProps.AutoFontSize          := True;
    //ASignature.WidgetProps.ShowTimestamp         := True;
    ASignature.WidgetProps.SigningTime           := LocalTimeToUTCTime(Now);
    ASignature.WidgetProps.BackgroundStyle       := pbsCustom;
    ASignature.WidgetProps.AutoStretchBackground := True;
    ASignature.WidgetProps.HideDefaultText       := True;
    ASignature.WidgetProps.IgnoreExistingAppearance := True;

    ASignature.WidgetProps.Background.Width      := ASignature.WidgetProps.Width;
    ASignature.WidgetProps.Background.Height     := ASignature.WidgetProps.Height;
    ASignature.WidgetProps.Background.ImageType  := pitJPEG;
    ASignature.WidgetProps.Background.Data       := GetBytesJPG(ASignatureField.GetJPGImage);
  //ADICIONANDO UM IMAGEM CUSTOMIZADA NA ASSINATURA
end;

function TPDFSignature.AddSignField(const ASignName: String): Boolean;
begin
// Não implementado
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
  FPDFStream     := TMemoryStream.Create;
  FDocumentPDF   := TElPDFDocument.Create(nil);
  FListSignField := TObjectList<TPDFSignatureField>.Create(True);
  FCertificate   := TPDFCertificate.Create;
end;

destructor TPDFSignature.Destroy;
begin
  if Assigned(FPDFStream) then FPDFStream.Free;
  if Assigned(FDocumentPDF) then FDocumentPDF.Free;
  if Assigned(FListSignField) then FListSignField.Free;
  if Assigned(FCertificate) then FCertificate.Free;
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
      SignField := TPDFSignatureField.Create;
      SignField.FTitle    := FDocumentPDF.EmptySignatureFields[c].FieldName;
      SignField.FSignInfo := FDocumentPDF.EmptySignatureFields[c];

      FListSignField.Add(SignField);
    end;
  end;
end;

procedure TPDFSignature.PreparePDF(const FileName: String = ''; const AImagePath: String = '');
var
 lPdf      : TPdfDocument;
 lPage     : TPdfPage;
begin
  lPdf := TPdfDocument.Create;
  try
    lPdf.Info.Author        := 'Autor do Relatório';
    lPdf.Info.CreationDate  := Now;
    lPdf.Info.Creator       := 'Relatório Financeiro - Agosto 2024';
    lPdf.DefaultPaperSize   := psA4;

    lPage := lPDF.AddPage;

    lPdf.Canvas.SetFont('Arial',12.0,[]);
    lPdf.Canvas.SetLeading(lPDF.Canvas.Page.FontSize);
    lPdf.Canvas.SetLineWidth(0.1);

    lPdf.Canvas.TextOut(50, 100, 'Assinatura 1: _____________________________');
    lPdf.Canvas.TextOut(50, 80, 'CPF: ');
    lPdf.Canvas.TextOut(50, 60, 'Geolocalização: ');

    lPdf.Canvas.TextOut(50, 40, 'Assinatura 2: _____________________________');
    lPdf.Canvas.TextOut(50, 20, 'CPF: ');
    lPdf.Canvas.TextOut(50, 0, 'Geolocalização: ');

    if (not AImagePath.IsEmpty) and TFile.Exists(AImagePath) then
    begin
      var imageStream : TBitmap := TBitmap.Create;
      try
        imageStream.LoadFromFile(AImagePath);
        var lPdfImage: TPdfImage := TPdfImage.Create(lPdf, imageStream, True);
        var index := lPdf.RegisterXObject(lPdfImage, 'imagePDF');
        lPdf.Canvas.DrawXObject(200, 200, 200, 200, 'imagePDF');
      finally
        imageStream.FreeImage;
      end;
    end;

    if FileName.IsEmpty then
      lPdf.SaveToFile(ExtractFilePath(ParamStr(0)) +'\NewPDFTest.PDF')
    else
      lPdf.SaveToFile(FileName);
  finally
    lPdf.Free;
  end;
end;

function TPDFSignature.RemoveEmptySignatureField(Index: Integer): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  try
    FDocumentPDF.RemoveEmptySignatureField(Index);
    //FDocumentPDF.RemoveEmptySignatureField(Index, sroKeepField);
  finally
    SavePDFToStream(Result);
  end;
end;

function TPDFSignature.RemoveSignature(Index: integer): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  try
    FDocumentPDF.RemoveSignature(Index);
  finally
    SavePDFToStream(Result);
  end;
end;

function TPDFSignature.RubricateField(AFieldIndex: Integer): Boolean;
begin
  Result := RubricateField(GetSignField(AFieldIndex));
end;

function TPDFSignature.RubricateField(ASignatureField: TPDFSignatureField): Boolean;
var
  c, Index         : Integer;
  Signature        : TElPDFSignature;
  PublicKeyHandler : TElPDFPublicKeySecurityHandler;
  PageIndex        : Integer;
begin
  Result := False;

  if (FDocumentPDF.Opened) and (ASignatureField <> nil) then
  begin
    Index := -1;
    for c := 0 to FDocumentPDF.EmptySignatureFieldCount - 1 do
    begin
      if (FDocumentPDF.EmptySignatureFields[c].FieldName = ASignatureField.Title) then
      begin
        PageIndex := FDocumentPDF.EmptySignatureFields[c].Page;
        Index := c;
        Break;
      end;
    end;

    if (Index > -1) then
    begin
      ASignatureField.AuthorName := 'Rubricado - ' + DateTimeToStr(Now) ;
      ASignatureField.Reason     := 'Rubrica Teste';

      if not ASignatureField.GetJPGImage.Empty then
        AddImageToSignature(FDocumentPDF.Signatures[Index], ASignatureField);

      FDocumentPDF.Close(True);

      FPDFStream.Position := 0;
      FDocumentPDF.Open(FPDFStream);

      Result := True;
    end;
  end;
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

function TPDFSignature.SignField(AFieldIndex: Integer): Boolean;
begin
  Result := SignField(GetSignField(AFieldIndex));
end;

function TPDFSignature.SignField(ASignatureField: TPDFSignatureField): Boolean;
var
  c, Index         : Integer;
  Signature        : TElPDFSignature;
  PublicKeyHandler : TElPDFPublicKeySecurityHandler;
begin
  Result := False;

  if (FDocumentPDF.Opened) and (ASignatureField <> nil) then
  begin
    Index := -1;
    for c := 0 to FDocumentPDF.EmptySignatureFieldCount - 1 do
    begin
      if (FDocumentPDF.EmptySignatureFields[c].FieldName = ASignatureField.Title) then
      begin
        Index := c;
        Break;
      end;
    end;

    if (Index > -1) then
    begin
      Index := FDocumentPDF.AddSignature(Index);

      PublicKeyHandler := TElPDFPublicKeySecurityHandler.Create(nil);
      try
        Signature := FDocumentPDF.Signatures[Index];
        Signature.Handler       := PublicKeyHandler;
        Signature.AuthorName    := ASignatureField.AuthorName;
        Signature.SigningTime   := LocalTimeToUTCTime(Now);
        Signature.Reason        := ASignatureField.Reason;
        Signature.SignatureType := stDocument;
        Signature.Invisible     := ASignatureField.Invisible;

        //ASignatureField.SignInfo.Invisible := ASignatureField.Invisible;
        //ASignatureField.SignInfo.Invisible := False;

        if not ASignatureField.GetJPGImage.Empty then
          AddImageToSignature(Signature, ASignatureField);

        PublicKeyHandler.SignatureType := pstPKCS7SHA1;
        PublicKeyHandler.CertStorage   := ASignatureField.Certificate.GetMemCertStorage;
        PublicKeyHandler.CustomName    := 'Adobe.PPKMS';

        FDocumentPDF.Close(True);
        ASignatureField.FSigned := True;
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

constructor TPDFSignatureField.Create;
begin
  FJPGImage    := TJPEGImage.Create;
  FCertificate := TPDFCertificate.Create;
  FSigned      := False;
  FAuthorName  := '';
  FReason      := '';
  FInvisible   := False;
end;

destructor TPDFSignatureField.Destroy;
begin
  FSignInfo := nil;
  if Assigned(FJPGImage) then FJPGImage.Free;
  if Assigned(FCertificate) then FCertificate.Free;
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
  FJPGImage.LoadFromStream(AStream);
end;

{ TPDFCertificate }

constructor TPDFCertificate.Create;
begin
  FMemoryCertStorage := TElMemoryCertStorage.Create(nil);
  FSystemStores      := TStringList.Create;
end;

destructor TPDFCertificate.Destroy;
begin
  if Assigned(FMemoryCertStorage) then FMemoryCertStorage.Free;
  if Assigned(FSystemStores) then FSystemStores.Free;
end;

function TPDFCertificate.GetMemCertStorage: TElMemoryCertStorage;
var
  Cert    : TElX509Certificate;
  WinCert : TElWinCertStorage;
begin
  FMemoryCertStorage.Clear;

  if (FFileName <> '') and (FPassword <> '') then
  begin
    Cert := TElX509Certificate.Create(nil);
    try
      if (Cert.LoadFromFileAuto(FFileName, FPassword) = 0) then
        FMemoryCertStorage.Add(Cert, True)
      else
        raise Exception.Create('fail load certificate');
    finally
      Cert.Free;
    end;
  end
  else if (FCertStoreIndex > -1) then
  begin
    WinCert := TElWinCertStorage.Create(nil);
    try
      WinCert.SystemStores.Assign(FSystemStores);
      if (WinCert.Count > 0) then
      begin
        if (WinCert.Certificates[FCertStoreIndex] <> nil) then
          FMemoryCertStorage.Add(WinCert.Certificates[FCertStoreIndex], True)
        else
          raise Exception.Create('invalid index certificate');
      end;
    finally
      WinCert.Free;
    end;
  end;

  Result := FMemoryCertStorage;
end;

end.
