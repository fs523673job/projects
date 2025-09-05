unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.IOUtils,
  System.Math,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ComCtrls,
  Vcl.Imaging.jpeg,
  Vcl.Imaging.pngimage,

  SBWinCertStorage,
  SBX509,

  SignaturePDF,

  litePDF
  ;

type
  TfrmMain = class(TForm)
    OpenDialog: TOpenDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    GroupBox4: TGroupBox;
    Label2: TLabel;
    edtPDF: TEdit;
    btnLoadPDF: TBitBtn;
    cmbCamposAssinaturas: TComboBox;
    ckbTodos: TCheckBox;
    btnRemoveSignature: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edtCertFile: TEdit;
    btnLoadCert: TBitBtn;
    edtPassCert: TEdit;
    GroupBox2: TGroupBox;
    cmbCertificate: TComboBox;
    GroupBox3: TGroupBox;
    edtJPEGPath: TEdit;
    btnLoadJpegPath: TBitBtn;
    btnSignDocument: TBitBtn;
    edtCriarNovoPDF: TEdit;
    btnCriarNovoPDF: TButton;
    GroupBox5: TGroupBox;
    Button1: TButton;
    btnRubricar: TButton;
    ckRubricas: TCheckBox;
    procedure btnLoadPDFClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoadCertClick(Sender: TObject);
    procedure btnLoadJpegPathClick(Sender: TObject);
    procedure btnSignDocumentClick(Sender: TObject);
    procedure btnRemoveSignatureClick(Sender: TObject);
    procedure btnCriarNovoPDFClick(Sender: TObject);
    procedure btnRubricarClick(Sender: TObject);
    procedure ckRubricasClick(Sender: TObject);
  private
    PDFSign: TPDFSignature;
  private
    procedure PopulateWindowsCertificate;
    procedure PopulateFieldsSign;
    procedure PopulateFieldsSignRubricados;

    procedure InitializeFields;
  public
  end;

var
  frmMain: TfrmMain;

procedure JpegToPng(const JpegFile, PngFile: string);
procedure BmpToPng(const BmpFile, PngFile: string);
procedure Pdf_AddImageAt(const InFile, OutFile, ImageFile: string;  PageIndex: Integer;  Xmm, Ymm, Wmm, Hmm: Double; FromBottom: Boolean = False);

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  InitializeFields;
  PopulateWindowsCertificate;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(PDFSign) then
    PDFSign.Free;
end;

procedure TfrmMain.InitializeFields;
begin
  PDFSign := TPDFSignature.Create(EmptyStr, True);

  cmbCertificate.Items.Clear;
  cmbCamposAssinaturas.Items.Clear;

  edtCriarNovoPDF.Text := ExtractFilePath(ParamStr(0)) + '\NewPDFTest.PDF';
end;

procedure TfrmMain.PopulateFieldsSign;
var
  c : Integer;
begin
  cmbCamposAssinaturas.Clear;
  cmbCamposAssinaturas.Items.Clear;
  for c := 0 to PDFSign.CountSignFields - 1 do
  begin
    cmbCamposAssinaturas.Items.Add('Campo: ' + PDFSign.Field[c].SignInfo.FieldName + ', Page: ' + IntToStr(PDFSign.Field[c].SignInfo.Page));
  end;
end;

procedure TfrmMain.PopulateFieldsSignRubricados;
var
  c : Integer;
begin
  cmbCamposAssinaturas.Clear;
  cmbCamposAssinaturas.Items.Clear;
  for c := 0 to PDFSign.CountSignFields - 1 do
  begin
    if PDFSign.Field[c].AuthorName.IsEmpty then
      cmbCamposAssinaturas.Items.Add('Campo: ' + PDFSign.Field[c].SignInfo.FieldName + ', Page: ' + IntToStr(PDFSign.Field[c].SignInfo.Page));
  end;
end;

procedure TfrmMain.PopulateWindowsCertificate;
var
  WinCert : TElWinCertStorage;
  Cert    : TElX509Certificate;
  c       : Integer;
begin
  WinCert := TElWinCertStorage.Create(nil);
  try
    WinCert.AccessType  := atCurrentUser;
    WinCert.StorageType := stSystem;
    WinCert.SystemStores.Add('My');
    if (WinCert.Count > 0) then
    begin
      cmbCertificate.Clear;
      for c := 0 to WinCert.Count - 1 do
      begin
        Cert := WinCert.Certificates[c];
        cmbCertificate.Items.Add('CertID: ' + IntToStr(c) +  ', Subject: ' + Cert.SubjectName.CommonName + ', Issuer: ' + Cert.IssuerName.CommonName);
      end;
    end;
  finally
    WinCert.Free;
  end;
end;

procedure TfrmMain.btnCriarNovoPDFClick(Sender: TObject);
begin
//
end;

procedure TfrmMain.btnLoadCertClick(Sender: TObject);
begin
  OpenDialog.FileName := '';
  OpenDialog.Filter   := 'PKCS#12 certificates (*.pfx)|*.pfx|All files (*.*)|*.*';
  if (OpenDialog.Execute) then
  begin
    if (OpenDialog.FileName <> '') then
      edtCertFile.Text := OpenDialog.FileName;
  end;
end;

procedure TfrmMain.btnLoadJpegPathClick(Sender: TObject);
begin
  OpenDialog.FileName := '';
  OpenDialog.Filter   := 'JPEG File (*.jpg)|*.jpg|All files (*.*)|*.*';
  if (OpenDialog.Execute) then
  begin
    if (OpenDialog.FileName <> '') then
      edtJPEGPath.Text := OpenDialog.FileName;
  end;
end;

procedure TfrmMain.btnLoadPDFClick(Sender: TObject);
begin
  OpenDialog.FileName := '';
  OpenDialog.Filter   := 'PDF document (*.pdf)|*.pdf|All files (*.*)|*.*';
  if (OpenDialog.Execute) then
  begin
    if (OpenDialog.FileName <> '') then
    begin
      edtPDF.Text := OpenDialog.FileName;

      PDFSign.LoadPDFFromFile(edtPDF.Text);
      PopulateFieldsSign;
    end;
  end;
end;

procedure TfrmMain.btnSignDocumentClick(Sender: TObject);
var
  c, Index : Integer;
  FileName : String;
begin
  if ckbTodos.Checked then
  begin
    for c := 0 to PDFSign.CountSignFields - 1 do
    begin
      PDFSign.Field[c].Certificate.SystemStores.Add('My');
      PDFSign.Field[c].Certificate.CertStoreIndex := cmbCertificate.ItemIndex;;
      if not String(edtJPEGPath.Text).IsEmpty then
      begin
        if TFile.Exists(edtJPEGPath.Text) then
          PDFSign.Field[c].LoadJPEGFromFile(edtJPEGPath.Text);
      end;

      if not PDFSign.Field[c].Signed then
      begin
        if not PDFSign.SignField(PDFSign.Field[c]) then
        begin
          ShowMessage(Format('Erro ao assinar o campo %d', [0]));
          Break;
        end;
      end;
    end;
  end
  else if (not ckbTodos.Checked and (not ckRubricas.Checked)) then
  begin
    Index := cmbCamposAssinaturas.ItemIndex;
    if (Index > -1) then
    begin
      PDFSign.Field[Index].Certificate.SystemStores.Add('My');
      PDFSign.Field[Index].Certificate.CertStoreIndex := cmbCertificate.ItemIndex;
      if not String(edtJPEGPath.Text).IsEmpty then
      begin
        if TFile.Exists(edtJPEGPath.Text) then
          PDFSign.Field[Index].LoadJPEGFromFile(edtJPEGPath.Text);
      end;

      if PDFSign.SignField(PDFSign.Field[Index]) then
      begin
        FileName := ExtractFilePath(edtPDF.Text) + ExtractFileName(edtPDF.Text) + '_Assinado.PDF';
        PDFSign.SavePDFToFile(FileName);

        ShowMessage('Assinado');
      end;
    end;
  end
  else if (ckRubricas.Checked and (not ckbTodos.Checked)) then
  begin
    Index :=  PDFSign.CountSignFields - 1;
    PDFSign.Field[Index].Certificate.SystemStores.Add('My');
    PDFSign.Field[Index].Certificate.CertStoreIndex := 0;
    if not String(edtJPEGPath.Text).IsEmpty then
    begin
      if TFile.Exists(edtJPEGPath.Text) then
        PDFSign.Field[Index].LoadJPEGFromFile(edtJPEGPath.Text);
    end;

    if not PDFSign.SignField(PDFSign.Field[Index]) then
      ShowMessage(Format('Erro ao assinar o campo %d', [Index]));
  end;
end;

procedure TfrmMain.ckRubricasClick(Sender: TObject);
begin
  if ckRubricas.Checked then
    PopulateFieldsSignRubricados
  else
    PopulateFieldsSign;
end;

procedure TfrmMain.btnRemoveSignatureClick(Sender: TObject);
var
  FileName: String;
begin
  FileName := ExtractFilePath(edtPDF.Text) + ExtractFileName(edtPDF.Text) + '_RemoveCampoAssinatura.PDF';
  PDFSign.RemoveEmptySignatureField(cmbCamposAssinaturas.ItemIndex);
end;

const
  PT_TO_MM = 25.4 / 72.0;

procedure TfrmMain.btnRubricarClick(Sender: TObject);
begin
  var FileName := ExtractFilePath(edtPDF.Text) + ExtractFileName(edtPDF.Text) + '_RubricarCampoAssinatura.PDF';
  var LRect := PDFSign.SignFieldRect(cmbCamposAssinaturas.ItemIndex);

  var fileAssinatura := edtJPEGPath.Text;
  if TFile.Exists(fileAssinatura) then
  begin
    var filePng := TPath.Combine(TPath.GetDirectoryName(fileAssinatura),  TPath.GetFileNameWithoutExtension(fileAssinatura) + '_conpng.png');
    JpegToPng(fileAssinatura, filePng);

    Pdf_AddImageAt(
                   edtPDF.Text,
                   FileName,
                   filePng,
                   PDFSign.Field[cmbCamposAssinaturas.ItemIndex].SignInfo.Page,
                   LRect.Left,
                   LRect.Top,
                   LRect.Width,
                   LRect.Height,
                   True
                 );
  end;
end;

(* ========================================== LITEPDF =============================================================*)

procedure Pdf_AddImageAt(const InFile, OutFile, ImageFile: string;  PageIndex: Integer;  Xmm, Ymm, Wmm, Hmm: Double; FromBottom: Boolean = False);
const
  // Fator de “pixels por mm” para o HDC interno (igual aos exemplos do litePDF):
  // use 10 para boa precisão (210 mm -> 2100 px em A4).
  PX_PER_MM = 10.0;
var
  pdf: TLitePDF;
  hDC: THandle;
  Canvas: TCanvas;
  Pic: TPicture;
  PageW_u, PageH_u: Cardinal; // tamanho da página na unidade atual
  PageW_mm, PageH_mm: Double;
  PageW_px, PageH_px: Integer;
  Xpx, Ypx, Wpx, Hpx: Integer;
  Dest: TRect;
begin
  pdf := TLitePDF.Create;
  try
    // Abra o PDF. Os 2 últimos parâmetros controlam:
    //   overwrite (gravar sobre o mesmo arquivo) e incremental update.
    // Como vamos salvar em OutFile, não precisamos "overwrite".
    // Preferimos incremental update (True) para manter o mínimo de alterações.
    pdf.LoadFromFile(InFile, '', False, True);

    // Trabalhar em milímetros para obter o tamanho da página;
    // depois convertendo para "pixels" do HDC (escala PX_PER_MM).
    pdf.SetUnit(LitePDFUnit_mm);
    pdf.GetPageSize(PageIndex, PageW_u, PageH_u); // u = mm, pois SetUnit(mm)
    PageW_mm := PageW_u;
    PageH_mm := PageH_u;

    PageW_px := Round(PageW_mm * PX_PER_MM);
    PageH_px := Round(PageH_mm * PX_PER_MM);

    // Converter a posição/tamanho desejados para pixels do HDC:
    Wpx := Max(1, Round(Wmm * PX_PER_MM));
    Hpx := Max(1, Round(Hmm * PX_PER_MM));
    Xpx := Round(Xmm * PX_PER_MM);

    if FromBottom then
      // Origem no canto inferior: inverter o Y
      Ypx := PageH_px - Round((Ymm + Hmm) * PX_PER_MM)
    else
      // Origem no topo (convencional do GDI)
      Ypx := Round(Ymm * PX_PER_MM);

    // Entrar em modo de atualização da página existente (pega um HDC “virtual”)
    hDC := pdf.UpdatePage(PageIndex, PageW_px, PageH_px, LongWord(LitePDFDrawFlag_None));
    if hDC = 0 then
      raise Exception.Create('Falha ao iniciar UpdatePage.');

    Canvas := TCanvas.Create;
    try
      Canvas.Handle := hDC;

      // Carregar a imagem (PNG/JPG/BMP…)
      Pic := TPicture.Create;
      try
        Pic.LoadFromFile(ImageFile);

        // Retângulo de destino em pixels
        Dest := Rect(Xpx, Ypx, Xpx + Wpx, Ypx + Hpx);

        // Desenhar com redimensionamento
        Canvas.StretchDraw(Dest, Pic.Graphic);
      finally
        Pic.Free;
      end;
    finally
      Canvas.Free;
    end;

    // Finalizar desenho nesta página
    pdf.FinishPage(hDC);

    // Salvar (como incremental update) em OUTFILE
    pdf.SaveToFile(OutFile);

    // Fechar o documento
    pdf.Close;
  finally
    pdf.Free;
  end;
end;

(*=============================== UTILITARIOS =======================================================================*)

procedure JpegToPng(const JpegFile, PngFile: string);
var
  Jpeg: TJPEGImage;
  Bitmap: TBitmap;
  Png: TPngImage;
begin
  Jpeg := TJPEGImage.Create;
  Bitmap := TBitmap.Create;
  Png := TPngImage.Create;
  try
    // Carregar JPG
    Jpeg.LoadFromFile(JpegFile);

    // Converter JPG -> Bitmap
    Bitmap.Assign(Jpeg);

    // Converter Bitmap -> PNG
    Png.Assign(Bitmap);
    Png.SaveToFile(PngFile);
  finally
    Png.Free;
    Bitmap.Free;
    Jpeg.Free;
  end;
end;

procedure BmpToPng(const BmpFile, PngFile: string);
var
  Bitmap: TBitmap;
  Png: TPngImage;
begin
  Bitmap := TBitmap.Create;
  Png := TPngImage.Create;
  try
    Bitmap.LoadFromFile(BmpFile);

    Png.Assign(Bitmap);
    Png.SaveToFile(PngFile);
  finally
    Png.Free;
    Bitmap.Free;
  end;
end;

end.
