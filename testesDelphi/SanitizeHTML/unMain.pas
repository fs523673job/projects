unit unMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,

  SynEditHighlighter,
  SynHighlighterHtml,
  SynEdit,

  unSanitizeHTML,
  unConstTeste
  ;

type
  TfrmMain = class(TForm)
    seInput: TSynEdit;
    seOutput: TSynEdit;
    SynHTMLSyn1: TSynHTMLSyn;
    btnSanitizeHTML: TButton;
    btClearAll: TButton;
    cbAnalisysXSS: TCheckBox;
    cbAnalysisXSSTags: TCheckBox;
    btnProvavelHTML: TButton;
    cmbExamples: TComboBox;
    cbAnalysisXSSAllContent: TCheckBox;
    cbAnalysisXSSTexto: TCheckBox;
    cbAnalysisXSSTextoBL: TCheckBox;
    procedure btnSanitizeHTMLClick(Sender: TObject);
    procedure btClearAllClick(Sender: TObject);
    procedure btnProvavelHTMLClick(Sender: TObject);
    procedure cmbExamplesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure AddItensCmb;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  AddItensCmb;
end;

procedure TfrmMain.btClearAllClick(Sender: TObject);
begin
  seInput.Lines.Clear;
  seOutput.Lines.Clear;
end;

procedure TfrmMain.btnProvavelHTMLClick(Sender: TObject);
begin
  seOutput.Lines.Clear;
  if TPreventXSS.IsProbablyHTML(seInput.Lines.Text) then
    seOutput.Text := 'Prov�vel HTML'
  else
    seOutput.Text := 'Prov�vel N�o � HTML';
end;

procedure TfrmMain.btnSanitizeHTMLClick(Sender: TObject);
begin
  seOutput.Lines.Clear;
  if cbAnalisysXSS.Checked and cbAnalysisXSSTags.Checked then
  begin
    seOutput.Text := TPreventXSS.SanitizeTag(seInput.Lines.Text);
    seOutput.Text := TPreventXSS.SanitizeHTML(seOutput.Text);
  end
  else if cbAnalisysXSS.Checked then
    seOutput.Text := TPreventXSS.SanitizeHTML(seInput.Lines.Text)
  else if cbAnalysisXSSTags.Checked then
    seOutput.Text := TPreventXSS.SanitizeTag(seInput.Lines.Text)
  else if cbAnalysisXSSAllContent.Checked then
    seOutput.Text := TPreventXSS.SanitizeForceAll(seInput.Lines.Text)
  else if cbAnalysisXSSTexto.Checked then
    seOutput.Text := TPreventXSS.SanitizeTexto(seInput.Lines.Text)
  else if cbAnalysisXSSTextoBL.Checked then
    seOutput.Text := TPreventXSS.SanitizeBlackList(seInput.Lines.Text)
  else
    seOutput.Text := TPreventXSS.SanitizeHTML(seInput.Lines.Text);
end;

procedure TfrmMain.cmbExamplesChange(Sender: TObject);
begin
  btClearAllClick(Self);
  case cmbExamples.ItemIndex of
    0 : seInput.Lines.Text := GENERAL_TESTE;
    1 : seInput.Lines.Text := GENERAL_TEST_CONTENT_01;
    2 : seInput.Lines.Text := GENERAL_TEST_CONTENT_02;
    3 : seInput.Lines.Text := GENERAL_TEST_CONTENT_03;
    4 : seInput.Lines.Text := GENERAL_TEST_CONTENT_04;
    5 : seInput.Lines.Text := GENERAL_TEST_CONTENT_05;
    6 : seInput.Lines.Text := GENERAL_TEST_CONTENT_06;
  end;
end;

procedure TfrmMain.AddItensCmb;
begin
  cmbExamples.Items.Clear;
  cmbExamples.Items.Add('01 - Exemplo Geral');
  cmbExamples.Items.Add('02 - Conteudo 01');
  cmbExamples.Items.Add('03 - Conteudo 02');
  cmbExamples.Items.Add('04 - Conteudo 03');
  cmbExamples.Items.Add('05 - Conteudo 04');
  cmbExamples.Items.Add('06 - Conteudo 05');
  cmbExamples.Items.Add('07 - Conteudo 06');
end;

end.
