unit Service;

interface

uses
  InvokeRegistry,
  Types,
  XSBuiltIns,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.IOUtils,
  System.NetEncoding,
  System.Zip
  ;

type
  IServices = interface(IInvokable)
    ['{0EC47E7C-B8CE-424F-95AB-F3F93D3661CC}']
    function getFile(const AFileName: String): WideString; stdcall;
    function listFiles: TArray<String>; stdcall;
    function getFileAsMultipart(const AFileName: String): WideString; stdcall;
    function downloadFiles(const FileNames: TArray<string>; const Zip: Boolean): WideString; stdcall;
  end;

  TServices = class(TInvokableClass, IServices)
  public
    function getFile(const AFileName: String): WideString; stdcall;
    function listFiles: TArray<String>; stdcall;
    function getFileAsMultipart(const AFileName: String): WideString; stdcall;
    function downloadFiles(const FileNames: TArray<string>; const Zip: Boolean): WideString; stdcall;
  end;

implementation

{ TServices }

function TServices.downloadFiles(const FileNames: TArray<string>; const Zip: Boolean): WideString;
var
  TempStream: TMemoryStream;
  FileStream: TFileStream;
  FilePath, FileName: string;
  i: Integer;
  FileBytes: TBytes;
  ZipFile: TZipFile;
  NeedZip: Boolean;
begin
  if Length(FileNames) = 0 then
    raise Exception.Create('No file specified.');

  NeedZip := Zip or (Length(FileNames) > 1);

  if NeedZip then
  begin
    // Cria ZIP em memória
    TempStream := TMemoryStream.Create;
    ZipFile := TZipFile.Create;
    try
      ZipFile.Open(TempStream, zmWrite);

      for FileName in FileNames do
      begin
        FilePath := TPath.Combine(TPath.GetDocumentsPath, FileName);
        if not TFile.Exists(FilePath) then
          raise Exception.Create('File not found: ' + FileName);

        ZipFile.Add(FilePath, FileName); // Adiciona ao ZIP com nome relativo
      end;

      ZipFile.Close;
      TempStream.Position := 0;

      SetLength(FileBytes, TempStream.Size);
      TempStream.ReadBuffer(FileBytes, TempStream.Size);

      Result := TNetEncoding.Base64.EncodeBytesToString(FileBytes);
    finally
      ZipFile.Free;
      TempStream.Free;
    end;
  end
  else
  begin
    // Retorna arquivo único sem zip
    FilePath := TPath.Combine(TPath.GetDocumentsPath, FileNames[0]);

    if not TFile.Exists(FilePath) then
      raise Exception.Create('File not found: ' + FileNames[0]);

    FileBytes := TFile.ReadAllBytes(FilePath);
    Result := TNetEncoding.Base64.EncodeBytesToString(FileBytes);
  end;
end;

function TServices.getFile(const AFileName: String): WideString;
var
  FilePath: string;
  FileBytes: TBytes;
begin
  FilePath := TPath.Combine(TPath.GetDocumentsPath, AFileName);
  if not TFile.Exists(FilePath) then
    raise Exception.Create('File not found: ' + AFileName);

  FileBytes := TFile.ReadAllBytes(FilePath);

  // Simples Base64 encoding como retorno no SOAP
  Result := TNetEncoding.Base64.EncodeBytesToString(FileBytes);
end;

function TServices.getFileAsMultipart(const AFileName: String): WideString;
var
  FilePath: string;
  FileBytes: TBytes;
  Boundary, SoapEnvelope, HeaderPart, BinaryHeader, Footer: string;
  BinaryDataBase64: string;
begin
  FilePath := TPath.Combine(TPath.GetDocumentsPath, AFileName);

  if not TFile.Exists(FilePath) then
    raise Exception.Create('File not found: ' + AFileName);

  FileBytes := TFile.ReadAllBytes(FilePath);

  // SOAP no Delphi não permite retorno direto binário, então codificamos em Base64
  BinaryDataBase64 := TNetEncoding.Base64.EncodeBytesToString(FileBytes);

  Boundary := 'MIME_boundary_12345';

  SoapEnvelope :=
    '<?xml version="1.0" encoding="UTF-8"?>' + sLineBreak +
    '<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope">' + sLineBreak +
    '  <soap:Body>' + sLineBreak +
    '    <ns:getFileAsMultipartResponse xmlns:ns="http://example.org/">' + sLineBreak +
    '      <ns:file>' + sLineBreak +
    '        <xop:Include href="cid:filecontent@example.org" xmlns:xop="http://www.w3.org/2004/08/xop/include"/>' + sLineBreak +
    '      </ns:file>' + sLineBreak +
    '    </ns:getFileAsMultipartResponse>' + sLineBreak +
    '  </soap:Body>' + sLineBreak +
    '</soap:Envelope>' + sLineBreak;

  HeaderPart :=
    '--' + Boundary + sLineBreak +
    'Content-Type: application/xop+xml; charset=UTF-8; type="application/soap+xml"' + sLineBreak +
    'Content-Transfer-Encoding: 8bit' + sLineBreak +
    'Content-ID: <rootpart@example.org>' + sLineBreak + sLineBreak +
    SoapEnvelope + sLineBreak;

  BinaryHeader :=
    '--' + Boundary + sLineBreak +
    'Content-Type: application/octet-stream' + sLineBreak +
    'Content-Transfer-Encoding: base64' + sLineBreak + // Note que é base64, não binary real
    'Content-ID: <filecontent@example.org>' + sLineBreak + sLineBreak +
    BinaryDataBase64 + sLineBreak;

  Footer :=
    '--' + Boundary + '--' + sLineBreak;

  Result := HeaderPart + BinaryHeader + Footer;
end;

function TServices.listFiles: TArray<String>;
var
  Files: TStringDynArray;
  FileList: TList<String>;
  FileName: string;
begin
  Files := TDirectory.GetFiles(TPath.GetDocumentsPath);
  FileList := TList<String>.Create;
  try
    for FileName in Files do
      FileList.Add(TPath.GetFileName(FileName));

    Result := FileList.ToArray;
  finally
    FileList.Free;
  end;
end;

end.
