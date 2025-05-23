unit wmMain;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.IOUtils,
  Web.HTTPApp;

type
  TWebModule1 = class(TWebModule)
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  Boundary, SoapEnvelope, HeaderPart, BinaryHeader, Footer: string;
  FileName, FilePath: string;
  FileBytes: TBytes;
  MemStream: TMemoryStream;
begin
  // Lê parâmetro da URL, ex: http://localhost:8080/?file=teste.pdf
  FileName := Request.QueryFields.Values['file'];
  if FileName = '' then
  begin
    Response.Content := 'Missing "file" parameter';
    Response.StatusCode := 400;
    Handled := True;
    Exit;
  end;

  FilePath := TPath.Combine(TPath.GetDocumentsPath, FileName);
  if not TFile.Exists(FilePath) then
  begin
    Response.Content := 'File not found';
    Response.StatusCode := 404;
    Handled := True;
    Exit;
  end;

  FileBytes := TFile.ReadAllBytes(FilePath);

  Boundary := 'MIME_boundary_12345';

  SoapEnvelope :=
    '<?xml version="1.0" encoding="UTF-8"?>' + sLineBreak +
    '<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope">' + sLineBreak +
    '  <soap:Body>' + sLineBreak +
    '    <ns:getFileResponse xmlns:ns="http://example.org/">' + sLineBreak +
    '      <ns:file>' + sLineBreak +
    '        <xop:Include href="cid:filecontent@example.org" xmlns:xop="http://www.w3.org/2004/08/xop/include"/>' + sLineBreak +
    '      </ns:file>' + sLineBreak +
    '    </ns:getFileResponse>' + sLineBreak +
    '  </soap:Body>' + sLineBreak +
    '</soap:Envelope>';

  HeaderPart :=
    '--' + Boundary + sLineBreak +
    'Content-Type: application/xop+xml; charset=UTF-8; type="application/soap+xml"' + sLineBreak +
    'Content-Transfer-Encoding: 8bit' + sLineBreak +
    'Content-ID: <rootpart@example.org>' + sLineBreak + sLineBreak +
    SoapEnvelope + sLineBreak;

  BinaryHeader :=
    '--' + Boundary + sLineBreak +
    'Content-Type: application/octet-stream' + sLineBreak +
    'Content-Transfer-Encoding: binary' + sLineBreak +
    'Content-ID: <filecontent@example.org>' + sLineBreak + sLineBreak;

  Footer :=
    sLineBreak + '--' + Boundary + '--' + sLineBreak;

  MemStream := TMemoryStream.Create;
  try
    MemStream.Write(Pointer(HeaderPart)^, Length(HeaderPart));
    MemStream.Write(Pointer(BinaryHeader)^, Length(BinaryHeader));
    MemStream.Write(Pointer(FileBytes)^, Length(FileBytes));
    MemStream.Write(Pointer(Footer)^, Length(Footer));
    MemStream.Position := 0;

    Response.ContentStream := MemStream;
    Response.ContentType :=
      'multipart/related; type="application/xop+xml"; boundary="' + Boundary + '"';
    Response.StatusCode := 200;
    Response.SendResponse;
    Handled := True;
  except
    MemStream.Free;
    raise;
  end;
end;

end.
