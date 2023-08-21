unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TSQLAnalyzer = class
  private
    const
      TAG_SECURITY = '/*tag seguranca*/';
  public
    function ModifySQL(const ASQL: string): string;
  end;


var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TSQLAnalyzer }

function TSQLAnalyzer.ModifySQL(const ASQL: string): string;
var
  TagPos, WherePos, ParenthesisCount, i: Integer;
  BeforeTag, AfterTag: string;
  InSingleQuote, InDoubleQuote: Boolean;
begin
  Result := ASQL;

  TagPos := Pos(TAG_SECURITY, ASQL);
  if TagPos = 0 then
    Exit;

  BeforeTag := Trim(Copy(ASQL, 1, TagPos - 1));
  AfterTag := Copy(ASQL, TagPos, MaxInt);

  WherePos := LastDelimiter('WHERE', BeforeTag);
  if WherePos = 0 then
   Exit;

  InSingleQuote := False;
  InDoubleQuote := False;
  ParenthesisCount := 0;

  for i := WherePos to Length(BeforeTag) do
  begin
    case BeforeTag[i] of
      '''': InSingleQuote := not InSingleQuote;
      '"': InDoubleQuote := not InDoubleQuote;
      '(': if not InSingleQuote and not InDoubleQuote then Inc(ParenthesisCount);
      ')': if not InSingleQuote and not InDoubleQuote then Dec(ParenthesisCount);
    end;
  end;

  if ParenthesisCount = 0 then
  begin
    BeforeTag := Copy(BeforeTag, 1, WherePos + 5) + ' (' +
                 Copy(BeforeTag, WherePos + 6, MaxInt) + ')';
  end;

  Result := BeforeTag + ' ' + AfterTag;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  SQL: String;
  SQLAnalyzer: TSQLAnalyzer;
begin
  SQLAnalyzer := TSQLAnalyzer.Create;
  try
    SQL := Memo1.Lines.Text;
    SQL := SQLAnalyzer.ModifySQL(SQL);
    Memo1.Lines.Clear;
    Memo1.Lines.Add(SQL);
  finally
    SQLAnalyzer.Free;
  end;

end;

end.
