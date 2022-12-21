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

  U_FormatConverter, SynHighlighterJSON, SynEditHighlighter,
  SynHighlighterXML
  ;

type
  TfrmMain = class(TForm)
    seJSON: TSynEdit;
    seXML: TSynEdit;
    btnConvert: TButton;
    SynXMLSyn1: TSynXMLSyn;
    SynJSONSyn1: TSynJSONSyn;
    procedure btnConvertClick(Sender: TObject);
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

end.
