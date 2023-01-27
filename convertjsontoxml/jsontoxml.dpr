program jsontoxml;

uses
  Vcl.Forms,
  unMain in 'unMain.pas' {frmMain},
  JSonValueToXML in 'JSonValueToXML.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
