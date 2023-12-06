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

  unImplementacao

  ;

type
  TForm1 = class(TForm)
    leFilePath: TLabeledEdit;
    spFileName: TSpeedButton;
    OpenDialog: TOpenDialog;
    btnExecute: TButton;
    procedure spFileNameClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
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

procedure TForm1.btnExecuteClick(Sender: TObject);
begin
 if FileExists(leFilePath.Text) then
 begin
   if EncryptSymetricFile(leFilePath.Text) then
     ShowMessage('Arquivo Criptografado')
   else
     ShowMessage('Falha ao criptografar o arquivo');
 end
 else
   ShowMessage('Arquivo selecionado não encontrado');
end;


end.
