program Project3;

uses
  Vcl.Forms,
  Unit3 in 'Unit3.pas' {Form1},
  unSQLAnalyzer in 'unSQLAnalyzer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
