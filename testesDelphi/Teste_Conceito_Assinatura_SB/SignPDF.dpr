program SignPDF;

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  SignaturePDF in 'SignaturePDF.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
