program SanitizeHTML;

uses
  Vcl.Forms,
  unMain in 'unMain.pas' {frmMain},
  unSanitizeHTML in 'unSanitizeHTML.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.