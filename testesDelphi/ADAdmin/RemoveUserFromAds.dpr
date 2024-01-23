program RemoveUserFromAds;

uses
  GnuGetText in 'units\GnuGetText.pas',
  LangUtils in 'units\LangUtils.pas',
  Forms,
  Graphics,
  RemUserMain in 'RemUserMain.pas' {Form1},
  CsvImportDlg in 'CsvImportDlg.pas' {CsvImportDialog},
  StatWind in 'units\StatWind.pas' {StatusWindow},
  ShellDirDlg in 'units\ShellDirDlg.pas' {ShellDirDialog};

{$R *.res}

begin
  TP_GlobalIgnoreClass(TFont);
  // Subdirectory in AppData for user configuration files and supported languages
  InitTranslation('','',['delphi10','units']);

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Remove user from Active Directory';
  Application.CreateForm(TfrmDelUser, frmDelUser);
  Application.CreateForm(TCsvImportDialog, CsvImportDialog);
  Application.CreateForm(TStatusWindow, StatusWindow);
  Application.CreateForm(TShellDirDialog, ShellDirDialog);
  Application.Run;
end.
