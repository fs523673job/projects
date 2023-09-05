unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SynEditHighlighter,
  SynHighlighterSQL, SynEdit, Vcl.StdCtrls,

  unSQLAnalyzer;

type
  TForm1 = class(TForm)
    SynEdit1: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  SQLAnalyzer: TSQLAnalyzer;
begin
  SQLAnalyzer := TSQLAnalyzer.Create;
  try
    if (SQLAnalyzer.FindTagSecurityErrors(SynEdit1.Text)) then
      ShowMessage('Comando com erro');
  finally
    SQLAnalyzer.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  SynEdit1.Clear;
end;

end.
