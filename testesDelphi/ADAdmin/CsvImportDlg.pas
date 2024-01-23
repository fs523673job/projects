(* Delphi XE
   Importieren von Text (z.B. csv-Format)
   Auswahl einer Spalte mot Mailadresse

   © Dr. J. Rathlev, D-24222 Schwentinental (info(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   letzte Änderung: Juni 2010
   *)

unit CsvImportDlg;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.ComCtrls;

const
  lmax = 9;

type
  TCsvImportDialog = class(TForm)
    Panel1: TPanel;
    Label3: TLabel;
    mePreview: TMemo;
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    edFilename: TEdit;
    gbDelim: TGroupBox;
    rbSpace: TRadioButton;
    rbTab: TRadioButton;
    rbSemi: TRadioButton;
    rbComma: TRadioButton;
    rbOtherDel: TRadioButton;
    edDelim: TEdit;
    rbSlash: TRadioButton;
    gbQuote: TGroupBox;
    rbNoQuote: TRadioButton;
    rbQuote: TRadioButton;
    rbOtherQuote: TRadioButton;
    edQuote: TEdit;
    gbLine: TGroupBox;
    Label4: TLabel;
    rgDecimal: TRadioGroup;
    GroupBox1: TGroupBox;
    edCol1: TLabeledEdit;
    edLine: TEdit;
    udLine: TUpDown;
    edColumn: TEdit;
    udColumn: TUpDown;
    procedure FormCreate(Sender: TObject);
    procedure rbTabClick(Sender: TObject);
    procedure rbSemiClick(Sender: TObject);
    procedure rbSlashClick(Sender: TObject);
    procedure rbSpaceClick(Sender: TObject);
    procedure rbCommaClick(Sender: TObject);
    procedure rbOtherDelClick(Sender: TObject);
    procedure rbNoQuoteClick(Sender: TObject);
    procedure rbQuoteClick(Sender: TObject);
    procedure rbOtherQuoteClick(Sender: TObject);
    procedure rgDecimalClick(Sender: TObject);
    procedure edLineChange(Sender: TObject);
    procedure edColumnChange(Sender: TObject);
  private
    { Private declarations }
    NDel,NQuote,DecSep : char;
    procedure UpdatePreview;
  public
    { Public declarations }
    function Execute (AFilename : string;
                      var Delim,Quote,Dec : char;
                      var FLine,FCol : integer) : boolean;
  end;

var
  CsvImportDialog: TCsvImportDialog;

implementation

{$R *.dfm}

uses StringUtils, FileUtils, GnuGetText;

procedure TCsvImportDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent (self);
  end;

procedure TCsvImportDialog.UpdatePreview;
var
  n,k : integer;
  s,t : string;
begin
//  if not Visible then Exit;
  n:=udLine.Position;
  with mePreView.Lines do if n>Count then n:=Count;
  s:=mePreview.Lines[n-1];
  edCol1.Text:=''; 
  k:=0;
  repeat
    if NQuote=#0 then t:=ReadNxtStr(s,NDel)
    else t:=ReadNxtQuotedStr(s,NDel,NQuote);
    inc(k);
    if (length(t)>0) and (udColumn.Position=k) then edCol1.Text:=t;
    until length(s)=0;
  end;

procedure TCsvImportDialog.rbTabClick(Sender: TObject);
begin
  NDel:=Tab;
  edDelim.Hide;
  UpdatePreview;
  end;

procedure TCsvImportDialog.rbSpaceClick(Sender: TObject);
begin
  NDel:=Space;
  edDelim.Hide;
  UpdatePreview;
  end;

procedure TCsvImportDialog.rbSemiClick(Sender: TObject);
begin
  NDel:=Semicolon;
  edDelim.Hide;
  UpdatePreview;
  end;

procedure TCsvImportDialog.rbCommaClick(Sender: TObject);
begin
  NDel:=Comma;
  edDelim.Hide;
  UpdatePreview;
  end;

procedure TCsvImportDialog.rbSlashClick(Sender: TObject);
begin
  NDel:='/';
  edDelim.Hide;
  UpdatePreview;
  end;

procedure TCsvImportDialog.rbOtherDelClick(Sender: TObject);
begin
  with edDelim do begin
    if length(Text)>0 then NDel:=Text[1] else NDel:=',';
    Text:=NDel;
    Show;
    end;
  UpdatePreview;
  end;

procedure TCsvImportDialog.rbNoQuoteClick(Sender: TObject);
begin
  NQuote:=#0;
  edQuote.Hide;
  UpdatePreview;
  end;

procedure TCsvImportDialog.rbQuoteClick(Sender: TObject);
begin
  NQuote:='"';
  edQuote.Hide;
  UpdatePreview;
  end;

procedure TCsvImportDialog.rbOtherQuoteClick(Sender: TObject);
begin
  with edQuote do begin
    if length(Text)>0 then NQuote:=Text[1] else NQuote:='''';
    Text:=NQuote;
    Show;
    end;
  UpdatePreview;
  end;

procedure TCsvImportDialog.rgDecimalClick(Sender: TObject);
begin
  if rgDecimal.ItemIndex=0 then DecSep:=Period else DecSep:=Comma;
  UpdatePreview;
  end;

procedure TCsvImportDialog.edColumnChange(Sender: TObject);
begin
  UpdatePreview;
  end;

procedure TCsvImportDialog.edLineChange(Sender: TObject);
begin
  UpdatePreview;
  end;

function TCsvImportDialog.Execute (AFilename : string;
                                var Delim,Quote,Dec : char;
                                var FLine,FCol : integer) : boolean;
var
  fi      : TextFile;
  s       : string;
  i       : integer;
begin
  edFilename.Text:=StripPath(AFilename,35);
  NDel:=Delim; NQuote:=Quote;
  edDelim.Text:=''; edQuote.Text:='';
  gbLine.Visible:=FLine>0;
  udLine.Position:=FLine;
  udColumn.Position:=FCol;
  AssignFile(fi,AFilename); reset(fi);
  with mePreview do begin
    Clear;
    i:=1;
    repeat
      readln(fi,s);
      Lines.Add(s);
      inc(i);
      until (i>lmax) or Eof(fi);
    CloseFile(fi);
    end;
  edDelim.Hide;
  case NDel of
  Space     : rbSpace.Checked:=true;
  Tab       : rbTab.Checked:=true;
  Semicolon : rbSemi.Checked:=true;
  Comma     : rbComma.Checked:=true;
  '/'       : rbSlash.Checked:=true;
  else with edDelim do begin
      Text:=NDel; Show;
      rbOtherDel.Checked:=true;
      end;
    end;
  DecSep:=Dec;
  with rgDecimal do begin
    Visible:=Dec<>#0;
    case Dec of
      Period: ItemIndex:=0;
      Comma : ItemIndex:=1;
      end;
    end;
  edQuote.Hide;
  case NQuote of
  '"' : rbQuote.Checked:=true;
  #0  : rbNoQuote.Checked:=true;
  else with edQuote do begin
      rbOtherQuote.Checked:=true;
      Text:=NQuote; Show;
      end;
    end;
  UpdatePreview;
  if ShowModal=mrOK then begin
    Delim:=NDel;
    Quote:=NQuote;
    if rgDecimal.Visible then Dec:=DecSep;
    if gbLine.Visible then FLine:=udLine.Position;
    FCol:=udColumn.Position;
    Result:=true;
    end
  else Result:=false;
  end;

end.
