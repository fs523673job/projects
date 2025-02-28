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

  SynEditHighlighter,
  SynEditCodeFolding,
  SynHighlighterJSON,
  SynEdit,
  SynHighlighterXML;

type
  TfrmMain = class(TForm)
    seJson: TSynEdit;
    synXML: TSynEdit;
    SynJSONSyn1: TSynJSONSyn;
    SynXMLSyn1: TSynXMLSyn;
    btConvertJsonToXml: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

end.
