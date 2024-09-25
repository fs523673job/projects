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
    procedure btnSanitizeHTMLClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnSanitizeHTMLClick(Sender: TObject);
begin
  seOutput.Lines.Clear;
  seOutput.Text := TPreventXSS.SanitizeHTML(seInput.Lines.Text);
end;

end.
