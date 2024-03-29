﻿unit unMain;

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
  JsonValueToXML,
  SynEditCodeFolding,
  ActiveX,
  XMLIntf,
  XMLDoc
  ;

type
  TfrmMain = class(TForm)
    SynJSONSyn1: TSynJSONSyn;
    seJSON: TSynEdit;
    btnConvertJasonXML: TButton;
    seXML: TMemo;
    edtFindNode: TEdit;
    btnFindNodes: TButton;
    procedure btnConvertJasonXMLClick(Sender: TObject);
    procedure btnFindNodesClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnConvertJasonXMLClick(Sender: TObject);

  function BeautifierXML(const XmlContent: String): String;
  var
    oXml : IXMLDocument;
  begin
    try
      CoInitialize(nil);
      try
        oXml := TXMLDocument.Create(nil);
        oXml.LoadFromXML(XmlContent);
        oXml.XML.Text:= xmlDoc.FormatXMLData(oXml.XML.Text);
        oXml.Active := True;
        oXml.SaveToXML(Result);
      finally
        CoUninitialize;
      end;
    except
      Result := XmlContent;
    end;
  end;

begin
  seXML.Lines.Clear;
  seXML.Lines.Add(BeautifierXML(TJSonUtils.JsonToXML(TJSonUtils.ConvertJSONValueToJSONObject(seJSON.Text))));
end;

procedure TfrmMain.btnFindNodesClick(Sender: TObject);
begin
  ShowMessage(TJsonUtils.FindContentNode(seXML.Text, edtFindNode.Text));
end;

end.
