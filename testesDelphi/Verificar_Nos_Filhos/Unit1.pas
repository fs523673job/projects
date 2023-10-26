unit Unit1;

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
  SynEditHighlighter,
  SynHighlighterXML,
  SynEdit,
  Vcl.StdCtrls,

  NativeXML
  ;

type
  TForm1 = class(TForm)
    seXML: TSynEdit;
    SynXMLSyn1: TSynXMLSyn;
    edtFileName: TEdit;
    Label1: TLabel;
    btCarregarArquivo: TButton;
    OpenDialog1: TOpenDialog;
    btTest: TButton;
    procedure btCarregarArquivoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btTestClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  seXML.Lines.Clear;
  seXML.Lines.LoadFromFile(edtFileName.Text);
end;

procedure TForm1.btCarregarArquivoClick(Sender: TObject);
begin
  if (OpenDialog1.Execute) then
  begin
    if (OpenDialog1.FileName <> '') then
    begin
      edtFileName.Text := OpenDialog1.FileName;
      seXML.Lines.Clear;
      seXML.Lines.LoadFromFile(OpenDialog1.FileName);
    end;
  end;
end;

procedure TForm1.btTestClick(Sender: TObject);
var
  NativeXML    : TNativeXML;
  XMLNode      : TXmlNode;
  XMLListNodes : TList;
  c : Integer;
begin
  NativeXML := TNativeXML.Create(nil);
  XMLListNodes := TList.Create;
  try
    NativeXML.LoadFromFile(edtFileName.Text);

    XMLNode := NativeXML.Root.FindNode('root');

    if not Assigned(XMLNode) then
    begin
      if AnsiSameText(NativeXML.Root.Name, 'root') then
        XMLNode := NativeXML.Root
      else
        Exit;
    end;

    XMLListNodes.Clear;

    XMLNode.FindNodes('/root/element/dataArray/element/data/element', XMLListNodes);

    for c := 0 to XMLListNodes.Count - 1 do
    begin
    end;
  finally
    NativeXML.Free;
    XMLListNodes.Free;
  end;
end;

end.
