program AnalyseSQL;

uses
  Vcl.Forms,
  unMain in 'unMain.pas' {Form1},
  unTestSQL in 'unTestSQL.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
