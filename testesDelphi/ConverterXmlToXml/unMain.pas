unit unMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,

  unScheduleXmlToXml,
  NewScheduleXmlToXml
  ;

type
  TfrmMain = class(TForm)
    btnHierarquiaTags: TBitBtn;
    Memo: TMemo;
    cmbFiles: TComboBox;
    btnConvertXmlToCsv: TBitBtn;
    btnConvertXmlToCsvTags: TBitBtn;
    btnClear: TBitBtn;
    MemoHeader: TMemo;
    btnColunaCSV: TBitBtn;
    btnTiposCSV: TBitBtn;
    btnAliasCSV: TBitBtn;
    btnCarregarXML: TBitBtn;
    OpenDialog: TOpenDialog;
    procedure btnHierarquiaTagsClick(Sender: TObject);
    procedure btnConvertXmlToCsvClick(Sender: TObject);
    procedure btnConvertXmlToCsvTagsClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnColunaCSVClick(Sender: TObject);
    procedure btnTiposCSVClick(Sender: TObject);
    procedure btnAliasCSVClick(Sender: TObject);
    procedure btnCarregarXMLClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnHierarquiaTagsClick(Sender: TObject);
var
  NewXML : TConvertXmlToNewXml;
begin
  NewXML := TConvertXmlToNewXml.Create;
  try
    NewXML.HierarquiaTags(cmbFiles.Text, Memo);
  finally
    NewXML.Free;
  end;
end;

procedure TfrmMain.btnCarregarXMLClick(Sender: TObject);
begin
  if (OpenDialog.Execute) then
  begin
    if (OpenDialog.FileName <> '') then
    begin
      cmbFiles.Items.Clear;
      cmbFiles.Text := OpenDialog.FileName;
    end;
  end;
end;

procedure TfrmMain.btnClearClick(Sender: TObject);
begin
  Memo.Lines.Clear;
end;

procedure TfrmMain.btnColunaCSVClick(Sender: TObject);
var
  NewXML  : TConvertXmlToNewXml;
  Line    : String;
  strCSV  : TStringList;
begin
  Memo.Lines.Clear;

  strCSV := TStringList.Create;
  try
    NewXML := TConvertXmlToNewXml.Create;
    try
      strCSV.Assign(MemoHeader.Lines);
      NewXML.RetornaColunas(strCSV, Line);
      Memo.Lines.Add(Line);
    finally
      NewXML.Free;
    end;
  finally
    strCSV.Free;
  end;
end;

procedure TfrmMain.btnConvertXmlToCsvClick(Sender: TObject);
var
  NewXML : TConvertXmlToNewXml;
begin
  NewXML := TConvertXmlToNewXml.Create;
  try
    NewXML.ConvertXMLToCSV(cmbFiles.Text, Memo);
  finally
    NewXML.Free;
  end;
end;

procedure TfrmMain.btnConvertXmlToCsvTagsClick(Sender: TObject);
var
  NewXML  : TConvertXmlToNewXml;
  strTags : TStringList;
begin
  NewXML := TConvertXmlToNewXml.Create;
  try
    strTags := TStringList.Create;
    try
      strTags.Assign(MemoHeader.Lines);
      NewXML.ConvertXMLToCSV(cmbFiles.Text, Memo, strTags);
    finally
      strTags.Free;
    end;
  finally
    NewXML.Free;
  end;
end;

procedure TfrmMain.btnTiposCSVClick(Sender: TObject);
var
  NewXML  : TConvertXmlToNewXml;
  Line    : String;
  strCSV  : TStringList;
begin
  Memo.Lines.Clear;

  strCSV := TStringList.Create;
  try
    NewXML := TConvertXmlToNewXml.Create;
    try
      strCSV.Assign(MemoHeader.Lines);
      NewXML.RetornaTipos(strCSV, Line);
      Memo.Lines.Add(Line);
    finally
      NewXML.Free;
    end;
  finally
    strCSV.Free;
  end;
end;

procedure TfrmMain.btnAliasCSVClick(Sender: TObject);
var
  NewXML  : TConvertXmlToNewXml;
  Line    : String;
  strCSV  : TStringList;
begin
  Memo.Lines.Clear;

  strCSV := TStringList.Create;
  try
    NewXML := TConvertXmlToNewXml.Create;
    try
      strCSV.Assign(MemoHeader.Lines);
      NewXML.RetornaColunasAlias(strCSV, Line);
      Memo.Lines.Add(Line);
    finally
      NewXML.Free;
    end;
  finally
    strCSV.Free;
  end;
end;



end.
