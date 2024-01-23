program DomUser;

uses
  GnuGetText in 'units\GnuGetText.pas',
  LangUtils in 'units\LangUtils.pas',
  Forms,
  Graphics,
  DUMain in 'DUMain.pas' {frmDomUser},
  NewPwdDlg in 'units\NewPwdDlg.pas' {NewPwdDialog},
  DuPrefs in 'DuPrefs.pas' {PrefDialog};

{$R *.res}

begin
  TP_GlobalIgnoreClass(TFont);
  // Subdirectory in AppData for user configuration files and supported languages
  InitTranslation('','',['delphi10','units']);

  Application.Initialize;
  Application.CreateForm(TfrmDomUser, frmDomUser);
  Application.CreateForm(TNewPwdDialog, NewPwdDialog);
  Application.CreateForm(TPrefDialog, PrefDialog);
  Application.Run;
end.
