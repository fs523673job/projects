program DummySQLAnalyser;

uses
  Vcl.Forms,
  unMain in 'unMain.pas' {Form1},
  unSQLAnalyser in 'unSQLAnalyser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
