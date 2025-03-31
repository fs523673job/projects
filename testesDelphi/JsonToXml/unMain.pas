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
  Vcl.StdCtrls,
  Vcl.ComCtrls,

  SynEdit,
  SynEditHighlighter,
  SynEditCodeFolding,
  SynHighlighterJSON,
  SynHighlighterXML,

  unJsonToXML,
  unNewJsonToXml
  ;

type
  TfrmMain = class(TForm)
    SynJSONSyn1: TSynJSONSyn;
    SynXMLSyn1: TSynXMLSyn;
    btConvertJsonToXml: TButton;
    cmModel: TComboBox;
    pg: TPageControl;
    tbJson: TTabSheet;
    seJson: TSynEdit;
    tbXML: TTabSheet;
    seXML: TSynEdit;
    procedure btConvertJsonToXmlClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btConvertJsonToXmlClick(Sender: TObject);
begin
  seXML.Lines.Clear;
  case cmModel.ItemIndex of
    0:
      begin
        try
          seXML.Text := TRestUtils.ConvertJSONValueToJSONObject(seJson.Text).ToJSON;
        except
          on e: Exception do
            ShowMessage('Error na conversão do json' + sLineBreak + e.Message);
        end;
      end;
    1:
      begin
      end;
  end;
end;

end.
