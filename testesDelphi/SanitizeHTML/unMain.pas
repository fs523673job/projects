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

  unSanitizeHTML
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
    procedure btnSanitizeHTMLClick(Sender: TObject);
    procedure btClearAllClick(Sender: TObject);
    procedure btnProvavelHTMLClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btClearAllClick(Sender: TObject);
begin
  seInput.Lines.Clear;
  seOutput.Lines.Clear;
end;

procedure TfrmMain.btnProvavelHTMLClick(Sender: TObject);
begin
  seOutput.Lines.Clear;
  if TPreventXSS.IsProbablyHTML(seInput.Lines.Text) then
    seOutput.Text := 'Provável HTML'
  else
    seOutput.Text := 'Provável Não é HTML';
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
  else
    seOutput.Text := TPreventXSS.SanitizeHTML(seInput.Lines.Text);
end;

end.
