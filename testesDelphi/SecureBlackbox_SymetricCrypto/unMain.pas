unit unMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
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
    Button1: TButton;
    procedure spFileNameClick(Sender: TObject);
    procedure btnEncryptClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

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
begin
  FilePath := leFilePath.Text;

  if FileExists(FilePath) then
  begin
    seContentFile.Lines.Clear;
    seContentFile.Lines.LoadFromFile(FilePath);

    if EncryptSymetricFile(FilePath, ckAlterExtension.Checked) then
      ShowMessage('Arquivo Criptografado')
    else
      ShowMessage('Falha ao criptografar o arquivo');
  end
  else
    ShowMessage('Arquivo selecionado não encontrado');

  seContentFile.Lines.Clear;
  seContentFile.Lines.LoadFromFile(FilePath);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  FilePath: String;
begin
  FilePath := leFilePath.Text;

  if FileExists(FilePath) then
  begin
    seContentFile.Lines.Clear;
    seContentFile.Lines.LoadFromFile(FilePath);

    if DecryptSymetricFile(FilePath, ckAlterExtension.Checked) then
      ShowMessage('Arquivo Descriptografado')
    else
      ShowMessage('Falha ao descriptografar o arquivo');
  end
  else
    ShowMessage('Arquivo selecionado não encontrado');

  seContentFile.Lines.Clear;
  seContentFile.Lines.LoadFromFile(FilePath);
end;


end.
