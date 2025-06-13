unit unMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Types,
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
    tabPath: TTabSheet;
    seXMLPath: TSynEdit;
    tabXMLMap: TTabSheet;
    seXMLMap: TSynEdit;
    btnClearAll: TButton;
    procedure btConvertJsonToXmlClick(Sender: TObject);
    procedure btnClearAllClick(Sender: TObject);
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
          var xmlResult := TRestUtils.BeautifierXML(TRestUtils.JsonToXML(TRestUtils.ConvertJSONValueToJSONObject(seJson.Text)));
          seXML.Text := xmlResult;
          seXMLPath.Text := TRestUtils.XMLPath(xmlResult);
          seXMLMap.Lines.AddStrings(TRestUtils.ResultContentNodesInMap(seXMLPath.Lines.ToStringArray, seJson.Text));
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

procedure TfrmMain.btnClearAllClick(Sender: TObject);
begin
  seJson.Lines.Clear;
  seXML.Lines.Clear;
  seXMLPath.Clear;
  seXMLMap.Clear;

  pg.TabIndex := 0;
end;

end.
