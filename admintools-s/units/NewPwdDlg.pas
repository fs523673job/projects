(* Delphi dialog
   Edit password with confirmation

   © Dr. J. Rathlev, D-24222 Schwentinental (info(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.
    
   Vers. 1 - June 2005
   *)
   
unit NewPwdDlg;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls;

const
  defMinPwdLength = 6;

type
  TNewPwdDialog = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    edtRepPwd: TEdit;
    Label1: TLabel;
    edtPwd: TEdit;
    btnShow: TSpeedButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnShowClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FConf   : boolean;
    FMinLen : integer;
  public
    { Public declarations }
    function Execute (const Title : string; var Pwd : String;
                      MinLen   : integer; Confirm,Show : boolean) : boolean; overload;
    function Execute (APos : TPoint; const Title : string; var Pwd : String;
                      MinLen   : integer; Confirm,Show : boolean) : boolean; overload;
    function Execute (APos : TPoint; const Title : string; var Pwd : AnsiString;
                      MinLen   : integer; Confirm,Show : boolean) : boolean; overload;
  end;

// Ansi password (no unicode)
function InputNewPassword (const Title : string; var Pwd : AnsiString;
                           MinLen   : integer; Confirm,Show : boolean) : boolean;

function GetPassword (const Title : string; var Pwd : String;
                      MinLen   : integer) : boolean;

var
  NewPwdDialog: TNewPwdDialog;

implementation

{$R *.DFM}

uses GnuGetText, WinUtils;

{ ------------------------------------------------------------------- }
procedure TNewPwdDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent (self,'dialogs');
  end;

procedure TNewPwdDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if (ModalResult=mrOK) and FConf then
    CanClose:=(edtPwd.Text=edtRepPwd.Text)
               and ((length(edtPwd.Text)>=FMinLen) or (length(edtPwd.Text)=0));
  if not CanClose then begin
    if length(edtPwd.Text)<FMinLen then begin
      ErrorDialog(Caption,Format(dgettext('dialogs','Minimum length of password is %u characters'),[FMinLen]),CenterPos);
      edtPwd.SetFocus;
      end
    else begin
      ErrorDialog(Caption,dgettext('dialogs','Passwords do not match!'),CenterPos);
      edtRepPwd.SetFocus;
      end;
    end;
  end;

procedure TNewPwdDialog.btnShowClick(Sender: TObject);
var
  c : char;
begin
  if btnShow.Down then c:=#0 else c:='*';
  edtPwd.PasswordChar:=c;
  edtRepPwd.PasswordChar:=c;
  end;

{ ------------------------------------------------------------------- }
(* Passworteingabe, Ergebnis: "true" bei "ok" *)
function TNewPwdDialog.Execute (const Title  : string;
                                var Pwd      : String;
                                MinLen       : integer;
                                Confirm,Show : boolean) : boolean;
begin
  Result:=Execute(CenterPos,Title,Pwd,MinLen,Confirm,Show);
  end;

function TNewPwdDialog.Execute (APos : TPoint; const Title : string; var Pwd : String;
                                MinLen : integer; Confirm,Show : boolean) : boolean;
begin
  with APos do begin
    if (Y < 0) or (X < 0) then Position:=poScreenCenter
    else begin
      if X<0 then X:=Left;
      if Y<0 then Y:=Top;
      CheckScreenBounds(Screen,x,y,Width,Height);
      Left:=x; Top:=y;
      end;
    end;
  Caption:=Title;
  edtPwd.Text:=Pwd;
  edtRepPwd.Text:=Pwd;
  FConf:=Confirm;
  FMinLen:=MinLen;
  btnShow.Visible:=Show;
  if FConf then edtRepPwd.Show
  else edtRepPwd.Hide;
  ActiveControl:=edtPwd;
  if ShowModal=mrOK then begin
    Pwd:=edtPwd.Text;
    Result:=true;
    end
  else Result:=false;
  end;

function TNewPwdDialog.Execute (APos : TPoint; const Title : string; var Pwd : AnsiString;
                      MinLen   : integer; Confirm,Show : boolean) : boolean;
var
  s : string;
begin
  Result:=Execute (APos,Title,s,MinLen,Confirm,Show );
  if Result then Pwd:=s;
  end;

{ ------------------------------------------------------------------- }
function InputNewPassword (const Title : string; var Pwd : AnsiString;
                           MinLen   : integer; Confirm,Show : boolean) : boolean;
var
  s : string;
begin
  if not assigned(NewPwdDialog) then NewPwdDialog:=TNewPwdDialog.create(Application);
  Result:=NewPwdDialog.execute(Title,s,MinLen,Confirm,Show);
  if Result then Pwd:=s;
  FreeAndNil(NewPwdDialog);
  end;

function GetPassword (const Title : string; var Pwd : String;
                      MinLen   : integer) : boolean;
begin
  if not assigned(NewPwdDialog) then NewPwdDialog:=TNewPwdDialog.create(Application);
  Result:=NewPwdDialog.Execute(Title,Pwd,MinLen,false,false);
  FreeAndNil(NewPwdDialog);
  end;


end.
