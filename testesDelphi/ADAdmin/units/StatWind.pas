(* Delphi-Unit
   Hinweisfenster für Status mit Fortschrittsanzeige (opt.)
   ========================================================
   
   © Dr. J. Rathlev, D-24222 Schwentinental (info(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.
    
   Vers. 1 - Okt. 2002 
   *)

unit StatWind;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TStatusWindow = class(TForm)
    StatusLabel: TLabel;
    btnCancel: TBitBtn;
    ProgLabel: TLabel;
    pbStatus: TProgressBar;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
    procedure SetStatus (Value: string);
    procedure SetInfo (Value: string);
    procedure SetProgress (Value : integer);
  public
    { Public declarations }
    Stopped : boolean;
    procedure SetState (AState : TProgressBarState);
    procedure ShowStatus (Pos        : TPoint;
                          Titel,Stat : string;
                          AllowBreak : boolean;
                          AInterval  : integer);
    property Status : string write SetStatus;
    property Info : string write SetInfo;
    property Progress : integer write SetProgress;
  end;

function CreateStatusWindow (Pos : TPoint; Titel,Stat : string;
                      AllowBreak : boolean; AInterval  : integer) : TStatusWindow;

procedure ReleaseStatusWindow (StatusWindow : TStatusWindow);

var
  StatusWindow: TStatusWindow;

implementation

{$R *.dfm}

uses FileCtrl, GnuGetText;

{ ---------------------------------------------------------------- }
procedure TStatusWindow.FormCreate(Sender: TObject);
begin
  TranslateComponent (self,'dialogs');
  end;

procedure TStatusWindow.FormResize(Sender: TObject);
var
  w : integer;
begin
  w:=ClientWidth;
  with btnCancel do Left:=(w-Width) div 2;
  end;

procedure TStatusWindow.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  end;

{ ---------------------------------------------------------------- }
procedure TStatusWindow.btnCancelClick(Sender: TObject);
begin
  Stopped:=true;
  end;

procedure TStatusWindow.SetState (AState : TProgressBarState);
begin
  pbStatus.State:=AState;
  end;

procedure TStatusWindow.SetStatus (Value : string);
begin
  with StatusLabel do Caption:=MinimizeName(Value,Canvas,pbStatus.Width);
  end;

procedure TStatusWindow.SetInfo (Value : string);
begin
  ProgLabel.Caption:=Value;
  end;

procedure TStatusWindow.SetProgress (Value : integer);
begin
  with pbStatus do begin
    if Value<Min then Value:=Min;
    if Value>Max then Value:=Max;
    if Value=Max then begin
      Max:=101; Position:=101; Max:=100;
      end
    else Position:=Value;
    end;
  end;

procedure TStatusWindow.TimerTimer(Sender: TObject);
begin
  Application.ProcessMessages;
  end;

{ ---------------------------------------------------------------- }
(* String in Hinweisfenster bei X,Y anzeigen *)
procedure TStatusWindow.ShowStatus (Pos        : TPoint;
                                    Titel,Stat : string;
                                    AllowBreak : boolean;
                                    AInterval  : integer);
(* Titel  : Fensterüberschrift
   Stat   : anzuzeigender String
   AIncrement > 0 : Fortschrittsbalken im Marquee-Stil
              < 0 : normaler Fortschrittsbalken
              = 0 : kein Balken *)
var
  Neu  : boolean;
begin
  Neu:=not Visible;
  Stopped:=false;
  if AllowBreak then begin
    ClientHeight:=btnCancel.Top+btnCancel.Height+5;
    btnCancel.Show;
    end
  else begin
    ClientHeight:=btnCancel.Top;
    btnCancel.Hide;
    end;
  if Neu or (Titel<>'') then Caption:=Titel;
  ProgLabel.Caption:='';
  StatusLabel.Caption:=Stat;
  if Neu then begin
    with pbStatus do begin
      if AInterval>0 then begin
        Style:=pbstMarquee;
        Visible:=true; MarqueeInterval:=AInterval;
        end
      else if AInterval<0 then begin
        Style:=pbstNormal;
        Position:=0;
        end
      else Visible:=false;
      end;
    if Pos.X<0 then Position:=poScreenCenter
    else begin
      Position:=poDesigned;
      with Pos do begin
        if X+Width>Screen.Width then X:=Screen.Width-Width-20;
        if Y+Height>Screen.Height then Y:=Screen.Height-Height-20;
        end;
      Left:=Pos.X; Top:=Pos.Y;
      end;
    btnCancel.Left:=(ClientWidth-btnCancel.Width) div 2;
    end;
  if Neu then Show else Update;
  end;

function CreateStatusWindow (Pos : TPoint; Titel,Stat : string;
                      AllowBreak : boolean; AInterval  : integer) : TStatusWindow;
begin
  Result:=TStatusWindow.Create(Application);
  with Result do begin
    FormStyle:=fsStayOnTop;
    ShowStatus(Pos,Titel,Stat,AllowBreak,AInterval);
    end;
  Application.ProcessMessages;
  end;

procedure ReleaseStatusWindow (StatusWindow : TStatusWindow);
begin
  with StatusWindow do begin
    Close; Release;
    end;
  StatusWindow:=nil;
  end;


end.
