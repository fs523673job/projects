unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.StrUtils;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button4: TButton;
    ComboBox1: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TSQLAnalyzer = class
  private
    const
      TAG_SECURITY_START = '/*AutoEmployeeFilter=';
      TAG_SECURITY_END = '*/';
    function CountOccurrences(const SubStr, Str: string; UntilPos: Integer): Integer;
  public
    function ModifySQL(const ASQL: string): string;
  end;

const
  COMMAND_01 =
      'Select CON_CdiContratado, CON_CdiSituacao ' + #13#10 +
      'From contratados contratados ' + #13#10 +
      '    WHERE CON_CdiSituacao = 1 or CON_CdiSituacao = 9 ' + #13#10 +
      '/*AutoEmployeeFilter=Contratados*/' + #13#10 +
      'order by 1';

  COMMAND_02 =
      'Select Campo1, Campo2 ' + #13#10 +
      '  From Tabela ' + #13#10 +
      '  Where  Condicao = Conteudo1 ' + #13#10 +
      '           or Condicao = Conteudo2 ' + #13#10 +
      '        and (select field1, field2 from tabela2 where field3 = field4) ' + #13#10 +
      ' /*AutoEmployeeFilter=ContratatadosExtras*/ ' + #13#10 +
      'Order by 1';

  COMMAND_03 = 'SELECT field1 , field2 FROM Tabela1 WHERE Field1 = Field2 or Field2 = Field4 /*AutoEmployeeFilter=ContratadosExtras*/ GROUP BY ' +
               'field1 , field2 HAVING field1 > field3 UNION ALL ( SELECT field3 , field4 FROM Tabela2 WHERE 1 = 2 GROUP BY field3 , field4 HAVING field3 > field5 ) ORDER BY 1';

  COMMAND_04 = 'SELECT field1 , field2 FROM Tabela1 WHERE Field1 = Field2 or Field2 = Field4 GROUP BY field1 , field2 HAVING field1 > field3 UNION ' +
               ' ALL ( SELECT field3 , field4 FROM Tabela2 WHERE 1 = 2 GROUP BY field3 , field4 HAVING field3 > field5 )  /*AutoEmployeeFilter=ContratadosExtras*/ ORDER BY 1';


var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TSQLAnalyzer }

function TSQLAnalyzer.CountOccurrences(const SubStr, Str: string; UntilPos: Integer): Integer;
var
  Offset: Integer;
begin
  Result := 0;
  Offset := Pos(SubStr, Str);
  while (Offset <> 0) and (Offset < UntilPos) do
  begin
    Inc(Result);
    Offset := PosEx(SubStr, Str, Offset + Length(SubStr));
  end;
end;

function TSQLAnalyzer.ModifySQL(const ASQL: string): string;
var
  TagStartPos, TagEndPos, WherePos, StartConditionPos, EndConditionPos, ParenthesisCount, i: Integer;
  BeforeTag, InsideTag, AfterTag, Condition: string;
  InSingleQuote, InDoubleQuote, AlreadyHasParenthesis, SingleWhere: Boolean;
begin
  Result := ASQL;

  TagStartPos := Pos(TAG_SECURITY_START, ASQL);
  if TagStartPos = 0 then
    Exit;

  TagEndPos := Pos(TAG_SECURITY_END, ASQL);
  if TagEndPos = 0 then
    Exit;

  BeforeTag := TrimRight(Copy(ASQL, 1, TagStartPos - 1));
  InsideTag := Copy(ASQL, TagStartPos, TagEndPos - TagStartPos + 2);
  AfterTag := TrimLeft(Copy(ASQL, TagEndPos + 2, MaxInt));

  WherePos := LastDelimiter('WHERE', BeforeTag);

  SingleWhere := (CountOccurrences('WHERE', ASQL, TagStartPos) = 1);

  StartConditionPos := WherePos + 6;
  if SingleWhere then
    StartConditionPos := WherePos + 1;

  while (StartConditionPos <= Length(BeforeTag)) and (BeforeTag[StartConditionPos] in [' ', #9, #10, #13]) do
    Inc(StartConditionPos);

  EndConditionPos := TagStartPos - 1;
  while (EndConditionPos > StartConditionPos) and (EndConditionPos <= Length(BeforeTag)) and (BeforeTag[EndConditionPos] in [' ', #9, #10, #13]) do
    Dec(EndConditionPos);

  Condition := Copy(BeforeTag, StartConditionPos, EndConditionPos - StartConditionPos + 1);

  InSingleQuote := False;
  InDoubleQuote := False;
  ParenthesisCount := 0;
  AlreadyHasParenthesis := False;

  for i := 1 to Length(Condition) do
  begin
    case Condition[i] of
      '''': InSingleQuote := not InSingleQuote;
      '"': InDoubleQuote := not InDoubleQuote;
      '(':
      begin
        if not InSingleQuote and not InDoubleQuote then
        begin
          Inc(ParenthesisCount);
          if i < 10 then AlreadyHasParenthesis := True;
        end;
      end;
      ')': if not InSingleQuote and not InDoubleQuote then Dec(ParenthesisCount);
    end;
  end;

  if (ParenthesisCount = 0) and (not AlreadyHasParenthesis) then
  begin
    Condition := '(' + Condition + ')';
  end;

  BeforeTag := Copy(BeforeTag, 1, StartConditionPos - 1) + Condition;

  if not (BeforeTag.EndsWith(sLineBreak) or InsideTag.StartsWith(sLineBreak)) then
    BeforeTag := BeforeTag + sLineBreak;

  if not (InsideTag.EndsWith(sLineBreak) or AfterTag.StartsWith(sLineBreak)) then
    InsideTag := InsideTag + sLineBreak;

  Result := BeforeTag + InsideTag + AfterTag;
end;

{ Form1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  ComboBox1.Items.Clear;
  ComboBox1.Items.Add(COMMAND_01);
  ComboBox1.Items.Add(COMMAND_02);
  ComboBox1.Items.Add(COMMAND_03);
  ComboBox1.Items.Add(COMMAND_04);
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

procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add(COMMAND_01);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add(COMMAND_02);
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  Memo1.Lines.Clear;
  case ComboBox1.ItemIndex of
    0: Memo1.Lines.Add(COMMAND_01);
    1: Memo1.Lines.Add(COMMAND_02);
    2: Memo1.Lines.Add(COMMAND_03);
    3: Memo1.Lines.Add(COMMAND_04);
  end;
end;

end.
