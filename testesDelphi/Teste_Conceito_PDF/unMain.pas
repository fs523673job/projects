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
  Vcl.Buttons,

  //SYNPDF
  SynPDF,

  //POWERPDF
  PdfDoc,
  PReport, Vcl.ExtCtrls
  ;

type
  TForm1 = class(TForm)
    edtPDFFile: TEdit;
    Label1: TLabel;
    btnLoadPDF: TBitBtn;
    PReport1: TPReport;
    PRPage1: TPRPage;
    procedure btnLoadPDFClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnLoadPDFClick(Sender: TObject);
begin
  PReport1.FileName := edtPDFFile.Text;
  ShowMessage(PReport1.PageNumber.ToString);
end;

end.
