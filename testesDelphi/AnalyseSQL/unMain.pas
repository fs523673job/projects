unit unMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SynEditHighlighter, SynHighlighterSQL, SynEdit, Vcl.Buttons;

type
  TForm1 = class(TForm)
    SynEdit1: TSynEdit;
    SpeedButton1: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  unTestSQL
  ;

{$R *.dfm}

procedure TForm1.SpeedButton1Click(Sender: TObject);
var
  SQL: String;
begin
  SQL := LinearizeSQL(SynEdit1.Text);
  if AnaliseSQLClausesOR(SQL) then
    ShowMessage('AnaliseSQLClausesOR - Comando com OR inválido');

  if RegxAnaliseSQLClausesOR(SQL) then
    ShowMessage('RegEx - AnaliseSQLClausesOR - Comando com OR inválido');
end;

end.
