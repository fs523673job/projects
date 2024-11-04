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
  SynEditHighlighter,
  SynHighlighterSQL,
  SynEdit,
  Vcl.StdCtrls,

  unSQLAnalyser
  ;

type
  TForm1 = class(TForm)
    seSQLText: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    btnExecutar: TButton;
    procedure btnExecutarClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnExecutarClick(Sender: TObject);
var
  SQLAnalyzer: TSQLAnalyzer;
  ResultMessage: String;
begin
  SQLAnalyzer := TSQLAnalyzer.Create;
  try
    SQLAnalyzer.ValidationOption := [voValidParenthesizeOrClause, voValidSecurityTag];
    if SQLAnalyzer.HasSQLErrors(seSQLText.Text, ResultMessage) then
      ShowMessage(ResultMessage);
  finally
    SQLAnalyzer.Free;
  end;

end;

end.
