unit unMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Diagnostics,
  System.TimeSpan,
  System.Rtti,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Buttons,
  Vcl.StdCtrls,
  Vcl.Mask,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,

  SynEdit,

  unImplementacao,
  unSymetricCripto,
  unSymetricCriptorefactor

  ;

type
  TForm1 = class(TForm)
    leFilePath: TLabeledEdit;
    spFileName: TSpeedButton;
    OpenDialog: TOpenDialog;
    ckAlterExtension: TCheckBox;
    seContentFile: TSynEdit;
    Label1: TLabel;
    ckLoadFile: TCheckBox;
    ckbAddHeaderSecuritySymetric: TCheckBox;
    pgControl: TPageControl;
    tabApenasMetodos: TTabSheet;
    btnEncrypt: TButton;
    btnDescriptografar: TButton;
    btStress: TButton;
    edtCount: TEdit;
    tbClasseSymetricCript: TTabSheet;
    btnEncryptSC: TButton;
    btDescriptografarSC: TButton;
    btStressSC: TButton;
    Edit1: TEdit;
    tbClasseReSymetricCript: TTabSheet;
    btEncrypt3: TButton;
    btDecript3: TButton;
    btStress3: TButton;
    Edit2: TEdit;
    cmbTypeCriptografic: TComboBox;
    btTimeExecute: TEdit;
    procedure spFileNameClick(Sender: TObject);
    procedure btnEncryptClick(Sender: TObject);
    procedure btnDescriptografarClick(Sender: TObject);
    procedure btStressClick(Sender: TObject);
    procedure btnEncryptSCClick(Sender: TObject);
    procedure btDescriptografarSCClick(Sender: TObject);
    procedure btEncrypt3Click(Sender: TObject);
    procedure btDecript3Click(Sender: TObject);
  private
    { Private declarations }
    procedure EnableDisable(const AFlag: Boolean);
  public
    { Public declarations }
  end;

//https://cdn.nsoftware.com/help/legacy/sbb/ref_howto_pki_basic_encryptsymmetric.html

type
  TFunc = reference to function(var AFilePath: String; const ACreateNewFile: Boolean = False): Boolean;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure MeansureExecutionTime(AMethod: TFunc; var AFilePath: String; const ACreateNewFile: Boolean; out AResult: Boolean; out AElapsedTime: TTimeSpan);
var
  Stopwatch: TStopwatch;
begin
  Stopwatch := TStopwatch.StartNew;
  try
    AResult := AMethod(AFilePath, ACreateNewFile);
  finally
    Stopwatch.Stop;
  end;

  AElapsedTime := Stopwatch.Elapsed;
end;

procedure TForm1.EnableDisable(const AFlag: Boolean);
begin
  leFilePath.Enabled := AFlag;
  ckAlterExtension.Enabled := AFlag;
  ckLoadFile.Enabled := AFlag;
  btnEncrypt.Enabled := AFlag;
  btnDescriptografar.Enabled := AFlag;
  btStress.Enabled := AFlag;
  edtCount.Enabled := AFlag;
end;

procedure TForm1.spFileNameClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    if (OpenDialog.FileName <> '') then
      leFilePath.Text := OpenDialog.FileName
    else
      leFilePath.Clear;
  end;
end;

procedure TForm1.btnEncryptClick(Sender: TObject);
var
  FilePath: String;
  TimeExec: TTimeSpan;
  ExecOk: Boolean;
begin
  FilePath := leFilePath.Text;

  if FileExists(FilePath) then
  begin
    EnableDisable(False);
    try
      if ckLoadFile.Checked then
      begin
        seContentFile.Lines.Clear;
        seContentFile.Lines.LoadFromFile(FilePath);
      end;

      if ckbAddHeaderSecuritySymetric.Checked then
        MeansureExecutionTime(EncryptSymetricFile_HeaderSecurity, FilePath, ckAlterExtension.Checked, ExecOk, TimeExec)
      else
        MeansureExecutionTime(EncryptSymetricFile, FilePath, ckAlterExtension.Checked, ExecOk, TimeExec);

      if (ExecOk) then
      begin
        ShowMessage(Format('Arquivo Criptografado - [%f Seconds][%s]', [TimeExec.TotalSeconds, TimeExec.ToString]));

        if ckLoadFile.Checked then
        begin
          seContentFile.Lines.Clear;
          seContentFile.Lines.LoadFromFile(FilePath);
        end;

        leFilePath.Text := FilePath;
      end
      else
        ShowMessage('Falha ao criptografar o arquivo');
    finally
      EnableDisable(True)
    end;
  end
  else
    ShowMessage('Arquivo selecionado não encontrado');
end;

procedure TForm1.btnEncryptSCClick(Sender: TObject);
var
  SymetricCript: TSymetricCript;
  NewNameFile: String;
begin
  SymetricCript := TSymetricCript.Create('CriptoSymetric', False);
  try
    NewNameFile := leFilePath.Text;
    SymetricCript.EncryptFile(NewNameFile, False);
  finally
    SymetricCript.Free;
  end;
end;

procedure TForm1.btDecript3Click(Sender: TObject);
var
  SymetricCript: ISymmetricCrypt;
  NewNameFile: String;
  Stopwatch: TStopwatch;
begin
  Stopwatch := TStopwatch.StartNew;
  try
    try
      case cmbTypeCriptografic.ItemIndex of
        0 : SymetricCript := TSymmetricCryptFactory.NewSymmetricCrypt(tsclegacy, 'CriptoSymetric', False);
        1 : SymetricCript := TSymmetricCryptFactory.NewSymmetricCrypt(tscKekDek, 'CriptoSymetric', False);
        else
          SymetricCript := TSymmetricCryptFactory.NewSymmetricCrypt(tsclegacy, 'CriptoSymetric', False);
      end;
      NewNameFile := leFilePath.Text;
      if not SymetricCript.DecryptFile(NewNameFile, False) then
        ShowMessage('Falha na descriptografia');
    finally
      SymetricCript := nil;
    end;
  finally
    Stopwatch.Stop;
  end;
  btTimeExecute.Text := Format('Time to execute: %d', [Stopwatch.Elapsed.Seconds]);
end;

procedure TForm1.btDescriptografarSCClick(Sender: TObject);
var
  SymetricCript: TSymetricCript;
  NewNameFile: String;
begin
  SymetricCript := TSymetricCript.Create('CriptoSymetric', False);
  try
    NewNameFile := leFilePath.Text;
    SymetricCript.DecryptFile(NewNameFile, False);
  finally
    SymetricCript.Free;
  end;

end;

procedure TForm1.btEncrypt3Click(Sender: TObject);
var
  SymetricCript: ISymmetricCrypt;
  NewNameFile: String;
  Stopwatch: TStopwatch;
begin
  Stopwatch := TStopwatch.StartNew;
  try
    try
      case cmbTypeCriptografic.ItemIndex of
        0 : SymetricCript := TSymmetricCryptFactory.NewSymmetricCrypt(tsclegacy, 'CriptoSymetric', False);
        1 : SymetricCript := TSymmetricCryptFactory.NewSymmetricCrypt(tscKekDek, 'CriptoSymetric', False);
        else
          SymetricCript := TSymmetricCryptFactory.NewSymmetricCrypt(tsclegacy, 'CriptoSymetric', False);
      end;
      NewNameFile := leFilePath.Text;
      if not SymetricCript.EncryptFile(NewNameFile, False) then
        ShowMessage('Falha na criptografia');
    finally
      SymetricCript := nil;
    end;
  finally
    Stopwatch.Stop;
  end;
  btTimeExecute.Text := Format('Time to execute: %d', [Stopwatch.Elapsed.Seconds]);
end;

procedure TForm1.btnDescriptografarClick(Sender: TObject);
var
  FilePath: String;
  TimeExec: TTimeSpan;
  ExecOk: Boolean;
begin
  FilePath := leFilePath.Text;

  if FileExists(FilePath) then
  begin
    EnableDisable(False);
    try
      if ckLoadFile.Checked then
      begin
        seContentFile.Lines.Clear;
        seContentFile.Lines.LoadFromFile(FilePath);
      end;

      if ckbAddHeaderSecuritySymetric.Checked then
        MeansureExecutionTime(DecryptSymetricFile_HeaderSecurity, FilePath, ckAlterExtension.Checked, ExecOk, TimeExec)
      else
        MeansureExecutionTime(DecryptSymetricFile, FilePath, ckAlterExtension.Checked, ExecOk, TimeExec);

      if ExecOk then
      begin
        ShowMessage(Format('Arquivo Descriptografado - [%f Seconds][%s]', [TimeExec.TotalSeconds, TimeExec.ToString]));

        if ckLoadFile.Checked then
        begin
          seContentFile.Lines.Clear;
          seContentFile.Lines.LoadFromFile(FilePath);
        end;

        leFilePath.Text := FilePath;
      end
      else
        ShowMessage('Falha ao descriptografar o arquivo');
    finally
      EnableDisable(True);
    end;
  end
  else
    ShowMessage('Arquivo selecionado não encontrado');
end;

procedure TForm1.btStressClick(Sender: TObject);
var
  Stopwatch: TStopwatch;
  count: Integer;
  c : Integer;
  FilePath: String;
  TimeExec: TTimeSpan;
  ExecOk: Boolean;
begin
  FilePath := leFilePath.Text;

  if FileExists(FilePath) then
  begin
    EnableDisable(False);
    try
      Stopwatch := TStopwatch.StartNew;
      try
        count := StrToIntDef(edtCount.Text, 1);

        seContentFile.Lines.Clear;

        for c := 0 to count do
        begin
          MeansureExecutionTime(EncryptSymetricFile, FilePath, ckAlterExtension.Checked, ExecOk, TimeExec);
          if (ExecOk) then
            seContentFile.Lines.Insert(0, Format('%d - Encrypt [Seconds %f][%s]', [c, TimeExec.TotalSeconds, TimeExec.ToString]))
          else
            seContentFile.Lines.Insert(0, Format('%d - Encrypt Fail', [c]));

          Application.ProcessMessages;

          MeansureExecutionTime(DecryptSymetricFile, FilePath, ckAlterExtension.Checked, ExecOk, TimeExec);
          if (ExecOk) then
            seContentFile.Lines.Insert(0, Format('%d - Decript [Seconds %f][%s]', [c, TimeExec.TotalSeconds, TimeExec.ToString]))
          else
            seContentFile.Lines.Insert(0, Format('%d - Decript Fail', [c]));

          Application.ProcessMessages;
        end;

      finally
        Stopwatch.Stop;
      end;

      seContentFile.Lines.Insert(0, Format('Final Time [%f seconds][%f minuts][%s]', [Stopwatch.Elapsed.TotalSeconds, Stopwatch.Elapsed.TotalMinutes, Stopwatch.Elapsed.ToString]));
    finally
      EnableDisable(True);
    end;
  end
  else
    ShowMessage('Arquivo selecionado não encontrado');
end;

end.
