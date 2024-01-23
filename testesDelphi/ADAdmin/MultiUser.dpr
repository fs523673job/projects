program MultiUser;

uses
  GnuGetText in 'units\GnuGetText.pas',
  LangUtils in 'units\LangUtils.pas',  Forms,
  Graphics,
  MultiMain in 'MultiMain.pas' {frmMultiUser},
  EtikettDlg in 'units\EtikettDlg.pas' {EtikettenDialog};

{$R *.res}

begin
  TP_GlobalIgnoreClass(TFont);
  // Subdirectory in AppData for user configuration files and supported languages
  InitTranslation('','',['delphi10','units']);

  Application.Initialize;
  Application.CreateForm(TfrmMultiUser, frmMultiUser);
  Application.CreateForm(TEtikettenDialog, EtikettenDialog);
  Application.Run;
end.
