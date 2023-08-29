unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons;

type
  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
    procedure CustomFormPaint(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  Self.OnPaint := CustomFormPaint;
  Self.Invalidate;
end;

procedure TForm1.CustomFormPaint(Sender: TObject);
const
  ESPESSURA = 10;
var
  Form: TForm;
begin
  Form := Sender as TForm;
  Form.Canvas.Pen.Color := clRed;
  Form.Canvas.Pen.Width := ESPESSURA;
  Form.Canvas.Brush.Style := bsClear;
  Form.Canvas.Rectangle(ESPESSURA div 2, ESPESSURA div 2, Form.ClientWidth - ESPESSURA div 2, Form.ClientHeight - ESPESSURA div 2);
end;

end.
