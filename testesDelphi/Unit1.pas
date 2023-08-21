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
  InSingleQuote, InDoubleQuote, AlreadyHasParenthesis: Boolean;
begin
  Result := ASQL;

  TagPos := Pos(TAG_SECURITY, ASQL);
  if TagPos = 0 then Exit; // Tag not found

  BeforeTag := Trim(Copy(ASQL, 1, TagPos - 1));
  AfterTag := Copy(ASQL, TagPos, MaxInt);

  WherePos := LastDelimiter('WHERE', BeforeTag);
  if WherePos = 0 then Exit; // WHERE not found

  InSingleQuote := False;
  InDoubleQuote := False;
  ParenthesisCount := 0;
  AlreadyHasParenthesis := False;

  for i := WherePos to Length(BeforeTag) do
  begin
    case BeforeTag[i] of
      '''': InSingleQuote := not InSingleQuote;
      '"': InDoubleQuote := not InDoubleQuote;
      '(':
      begin
        if not InSingleQuote and not InDoubleQuote then
        begin
          Inc(ParenthesisCount);
          if (i > WherePos) and (i < WherePos + 10) then AlreadyHasParenthesis := True;
        end;
      end;
      ')': if not InSingleQuote and not InDoubleQuote then Dec(ParenthesisCount);
    end;
  end;

  // Se não houver parênteses desequilibrados após o WHERE e não houver parênteses logo após o WHERE, adicione os parênteses externos
  if (ParenthesisCount = 0) and (not AlreadyHasParenthesis) then
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
