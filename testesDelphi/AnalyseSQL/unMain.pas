unit unMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SynEditHighlighter, SynHighlighterSQL, SynEdit, Vcl.Buttons, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    SynEdit1: TSynEdit;
    SynEdit2: TSynEdit;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  unTestSQL
  ;

{$R *.dfm}

procedure TForm1.SpeedButton1Click(Sender: TObject);
var
  SQL: String;
  Conditions: TStringList;
  Condition: String;
begin
  SynEdit2.Lines.Clear;

  SQL := LinearizeSQL(SynEdit1.Text);
  Conditions := ExtrairCondicoesWhereComOr(SQL);
  try
    for Condition in Conditions do
    begin
      SynEdit2.Lines.Add(Condition);
      SynEdit2.Lines.Add(StringOfChar('*', 100));
    end;
  finally
    Conditions.Free;
  end;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  SynEdit1.Lines.Clear;
  SynEdit2.Lines.Clear;
end;

end.
