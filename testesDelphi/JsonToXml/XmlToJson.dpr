program XmlToJson;

uses
  Vcl.Forms,
  unMain in 'unMain.pas' {frmMain},
  unJsonToXML in 'unJsonToXML.pas',
  unNewJsonToXml in 'unNewJsonToXml.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
