unit untMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, SynEditHighlighter,
  SynHighlighterJSON, SynEdit, SynMemo, System.Json;

const
  JSONSIMPES = '{"date": "20221213", "nameUser": "User One", "dataInt": 123}';
  JSONARRAY1 = '[{"date": "20221213", "nameUser": "User One", "dataInt": 123}, {"date": "20221213", "nameUser": "User Two", "dataInt": 124}]';

type
  TfrmMain = class(TForm)
    cmbexemplosjson: TComboBox;
    lblexemplosjson: TLabel;
    smexemposjson: TSynMemo;
    SynJSONSyn: TSynJSONSyn;
    btnExecute: TButton;
    smjsonresult: TSynMemo;
    procedure cmbexemplosjsonChange(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function ConvertJSONValueToJSONObject(const AJSONContent: String): TJSONObject;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

function TfrmMain.ConvertJSONValueToJSONObject(const AJSONContent: String): TJSONObject;
var
  c, i     : Integer;
  item     : TJSONValue;
  jsonValue: TJSONValue;
  jsonArray: TJSONArray;
begin
  Result := nil;
  try
    jsonValue := TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(AJSONContent), 0);
    if (jsonValue is TJSONObject) then
      Result := (jsonValue as TJSONObject)
    else if (jsonValue is TJSONArray) then
    begin
      jsonArray := (jsonValue as TJSONArray);
      Result    := TJSONObject.Create;
      Result.AddPair(TJSONPair.Create('Array', jsonArray));
    end;
  except
    Result := nil;
  end;
end;

procedure TfrmMain.btnExecuteClick(Sender: TObject);
var
  jsonObject : TJSONObject;
begin
  smjsonresult.Clear;
  jsonObject := ConvertJSONValueToJSONObject(smexemposjson.Text);
  if Assigned(jsonObject) then
  begin
    smjsonresult.Lines.Clear;
    smjsonresult.Lines.Add(jsonObject.ToJSON);
  end
  else
    MessageBox(Self.Handle, PChar('Erro ao converter'), PChar('ATENÇÃO'), MB_ICONERROR + MB_OK);
end;

procedure TfrmMain.cmbexemplosjsonChange(Sender: TObject);
begin
  smexemposjson.Lines.Clear;
  case cmbexemplosjson.ItemIndex of
    0 : smexemposjson.Lines.Add(JSONSIMPES);
    1 : smexemposjson.Lines.Add(JSONARRAY1);
  end;
end;

end.
