unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.IOUtils,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Buttons,

  SBWinCertStorage,
  SBX509,

  SignaturePDF,
  Vcl.ComCtrls
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

  FileName := ExtractFilePath(edtPDF.Text) + ExtractFileName(edtPDF.Text) + '_Assinado.PDF';
  PDFSign.SavePDFToFile(FileName);

  ShowMessage('Assinado');
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

procedure TfrmMain.btnRubricarClick(Sender: TObject);
begin
//
end;

end.
