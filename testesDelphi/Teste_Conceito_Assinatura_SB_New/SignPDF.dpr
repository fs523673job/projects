program SignPDF;

uses
  Vcl.Forms,
  unFunctions in 'unFunctions.pas',
  unSignature in 'unSignature.pas',
  Main in 'Main.pas' {frmMain},
  litePDF in 'litePDF\litePDF.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
