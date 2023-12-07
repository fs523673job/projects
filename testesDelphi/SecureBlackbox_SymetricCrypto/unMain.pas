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

  SynEdit,

  unImplementacao

  ;

type
  TForm1 = class(TForm)
    leFilePath: TLabeledEdit;
    spFileName: TSpeedButton;
    OpenDialog: TOpenDialog;
    btnEncrypt: TButton;
    ckAlterExtension: TCheckBox;
    seContentFile: TSynEdit;
    Label1: TLabel;
    btnDescriptografar: TButton;
    ckLoadFile: TCheckBox;
    btStress: TButton;
    edtCount: TEdit;
    procedure spFileNameClick(Sender: TObject);
    procedure btnEncryptClick(Sender: TObject);
    procedure btnDescriptografarClick(Sender: TObject);
    procedure btStressClick(Sender: TObject);
  private
    { Private declarations }
    procedure EnableDisable(const AFlag: Boolean);
  public
    { Public declarations }
  end;

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

      MeansureExecutionTime(EncryptSymetricFile, FilePath, ckAlterExtension.Checked, ExecOk, TimeExec);

      if (ExecOk) then
      begin
        ShowMessage('Arquivo Criptografado - ' + IntToStr(TimeExec.Seconds));

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

      MeansureExecutionTime(DecryptSymetricFile, FilePath, ckAlterExtension.Checked, ExecOk, TimeExec);

      if ExecOk then
      begin
        ShowMessage('Arquivo Descriptografado - ' + IntToStr(TimeExec.Seconds));

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
            seContentFile.Lines.Add(Format('%d - Encrypt Seconds %f', [c, TimeExec.TotalSeconds]))
          else
            seContentFile.Lines.Add(Format('%d - Encrypt Fail', [c]));

          Application.ProcessMessages;

          MeansureExecutionTime(DecryptSymetricFile, FilePath, ckAlterExtension.Checked, ExecOk, TimeExec);
          if (ExecOk) then
            seContentFile.Lines.Add(Format('%d - Decript Seconds %f', [c, TimeExec.TotalSeconds]))
          else
            seContentFile.Lines.Add(Format('%d - Decript Fail', [c]));

          Application.ProcessMessages;
        end;

      finally
        Stopwatch.Stop;
      end;

      seContentFile.Lines.Add(Format('Final Time %f seconds', [Stopwatch.Elapsed.TotalSeconds]));
    finally
      EnableDisable(True);
    end;
  end
  else
    ShowMessage('Arquivo selecionado não encontrado');
end;

end.
