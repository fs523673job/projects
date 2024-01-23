(* Einstelldialog für Etikettenlayout
   ==================================
   © Dr. J. Rathlev, D-24222 Schwentinental (info(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1
   *)

unit EtikettDlg;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, WinUtils,
  NumberEd;

const
  Rand = 5;

type
  TPageLayout = record
    PrSize,                     // Papiergröße in mm einschl. Rand
    MinMarg,                    // Mindestrand in mm
    UsableSize,                 // nutzbare Größe in mm
    Margin,                     // Rand links/oben zu den Etiketten in mm
    Size,                       // Größe der Etiketten in mm
    Distance,                   // Abstand zwischen den Etiketten in mm
    Count            : TPoint;  // Anzahl der Etiketten hor. und dvert.
    ShowFrame        : boolean; // mit/ohne Linien
    end;

  TEtikettenDialog = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Panel1: TPanel;
    Image1: TImage;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Panel2: TPanel;
    PaintBox: TPaintBox;
    cbFrame: TCheckBox;
    reHR: TNumberEdit;
    udHR: TNumUpDown;
    udHB: TNumUpDown;
    reHB: TNumberEdit;
    NumUpDown2: TNumUpDown;
    RangeEdit2: TNumberEdit;
    udHA: TNumUpDown;
    reHA: TNumberEdit;
    udHC: TNumUpDown;
    reHC: TNumberEdit;
    udVR: TNumUpDown;
    reVR: TNumberEdit;
    udVH: TNumUpDown;
    reVH: TNumberEdit;
    udVA: TNumUpDown;
    reVA: TNumberEdit;
    udVC: TNumUpDown;
    reVC: TNumberEdit;
    procedure PaintBoxPaint(Sender: TObject);
    procedure reChange(Sender: TObject);
    procedure reHAChange(Sender: TObject);
    procedure reVAChange(Sender: TObject);
    procedure reVHChange(Sender: TObject);
    procedure reHBChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    PWidth : integer;
    FPage  : TPageLayout;
    function SetScreenScale : integer;
  public
    { Public declarations }
    function Execute (Title      : string;
                      var Page   : TPageLayout) : boolean;
  end;

var
  EtikettenDialog : TEtikettenDialog;

implementation

{$R *.DFM}

{ ------------------------------------------------------------------- }
procedure TEtikettenDialog.FormCreate(Sender: TObject);
begin
  // ***
  end;

function TEtikettenDialog.SetScreenScale : integer;
begin
  with PaintBox,Canvas do begin
    Result:=SetMapMode (Handle,MM_ISOTROPIC);
    SetWindowOrgEx (Handle,0,0,nil);
    SetWindowExtEx (Handle,PWidth,PWidth,nil);
    SetViewPortOrgEx (Handle,0,0,nil);
    SetViewportExtEX (handle,Width,Height,nil);
    end;
  end;

procedure TEtikettenDialog.PaintBoxPaint(Sender: TObject);
var
  i,j,
  x0,y0,
  x1,y1 : integer;
begin
  SetScreenScale;
  with PaintBox.Canvas do begin
    Brush.Color:=clWhite;
    FillRect(Rect(0,0,PWidth,PWidth));
    with Pen do begin
      Color:=clBlack; Width:=1;
      end;
    with FPage.PrSize do begin
      x0:=(PWidth-x) div 2;
      y0:=(PWidth-y) div 2;
      Rectangle(x0,y0,x0+x,y0+y);
      for i:=0 to reHC.Value-1 do
        for j:=0 to reVC.Value-1 do begin
          x1:=x0+reHR.Value+i*reHA.Value;
          y1:=y0+reVR.Value+j*reVA.Value;
          Rectangle(x1,y1,x1+reHB.Value,y1+reVH.Value);
          end;
      end;
    end;
  end;

procedure TEtikettenDialog.reChange(Sender: TObject);
begin
  PaintBox.Invalidate;
  end;

procedure TEtikettenDialog.reHAChange(Sender: TObject);
begin
  if reHB.Value>reHA.Value then reHB.Value:=reHA.Value;
  PaintBox.Invalidate;
  end;

procedure TEtikettenDialog.reHBChange(Sender: TObject);
begin
  if reHB.Value>reHA.Value then reHA.Value:=reHB.Value;
  PaintBox.Invalidate;
  end;

procedure TEtikettenDialog.reVAChange(Sender: TObject);
begin
  if reVH.Value>reVA.Value then reVH.Value:=reVA.Value;
  PaintBox.Invalidate;
  end;

procedure TEtikettenDialog.reVHChange(Sender: TObject);
begin
  if reVH.Value>reVA.Value then reVA.Value:=reVH.Value;
  PaintBox.Invalidate;
  end;

{ ------------------------------------------------------------------- }
function TEtikettenDialog.Execute (Title      : string;
                                   var Page   : TPageLayout) : boolean;
begin
  if length(Title)>0 then Caption:=Title;
  FPage:=Page;
  with Page do begin
    with PrSize do begin
      if x>y then PWidth:=x else PWidth:=y;
      end;
    PWidth:=PWidth+2*Rand;
    reHR.Value:=Margin.x;
    reVR.Value:=Margin.y;
    reHB.Value:=Size.x;
    reVH.Value:=Size.y;
    reHA.Value:=Distance.x;
    reVA.Value:=Distance.y;
    reHC.Value:=Count.x;
    reVC.Value:=Count.y;
    cbFrame.Checked:=ShowFrame;
    end;
  if ShowModal=mrOK then begin
    with Page do begin
      Margin.x:=reHR.Value;
      Margin.y:=reVR.Value;
      Size.x:=reHB.Value;
      Size.y:=reVH.Value;
      Distance.x:=reHA.Value;
      Distance.y:=reVA.Value;
      Count.x:=reHC.Value;
      Count.y:=reVC.Value;
      ShowFrame:=cbFrame.Checked;
      end;
    Result:=true;
    end
  else Result:=false;
  end;

end.
