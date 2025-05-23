program ConvertXmlToXml;

uses
  Forms,
  unMain in 'unMain.pas' {frmMain},
  unScheduleXmlToXml in 'unScheduleXmlToXml.pas',
  NewScheduleXmlToXml in 'NewScheduleXmlToXml.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
