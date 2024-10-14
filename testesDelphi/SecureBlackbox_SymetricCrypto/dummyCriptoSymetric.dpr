program dummyCriptoSymetric;

uses
  Vcl.Forms,
  unMain in 'unMain.pas' {Form1},
  unImplementacao in 'unImplementacao.pas',
  unSymetricCripto in 'unSymetricCripto.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
