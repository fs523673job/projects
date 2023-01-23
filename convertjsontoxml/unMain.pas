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
  SynEdit,
  Vcl.StdCtrls,
  SynHighlighterJSON,
  SynEditHighlighter,
  SynHighlighterXML,
  U_FormatConverter,
  SimpleJsonToXML,
  JsonValueToXML
  ;

type
  TfrmMain = class(TForm)
    seXML: TSynEdit;
    btnConvert: TButton;
    SynXMLSyn1: TSynXMLSyn;
    SynJSONSyn1: TSynJSONSyn;
    btConvert2: TButton;
    seJSON: TSynEdit;
    btnConvert3: TButton;
    procedure btnConvertClick(Sender: TObject);
    procedure btConvert2Click(Sender: TObject);
    procedure btnConvert3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnConvertClick(Sender: TObject);
var
  FC : TFormatConverter;
begin
  FC := TFormatConverter.Create(nil);
  try
    seXML.Lines.Clear;
    seXML.Lines.Add(FC.JSONtoXML.stringToString(seJSON.Text));
  finally
    FC.Free;
  end;
end;

procedure TfrmMain.btConvert2Click(Sender: TObject);
begin
  seXML.Lines.Clear;
  seXML.Lines.Add(TJsonToXML.StringToString(seJSON.Text));
end;

procedure TfrmMain.btnConvert3Click(Sender: TObject);
begin
  seXML.Lines.Clear;
  seXML.Lines.Add(TJSonUtils.JsonToXML(TJSonUtils.ConvertJSONValueToJSONObject(seJSON.Text)));
end;


end.
