(* Delphi Unit
   Show text and buttons to select several options

   � Dr. J. Rathlev, D-24222 Schwentinental (info(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Jun. 2009
   Last changes: Nov. 2013
   *)

unit SelectDlg;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms, 
  Vcl.Controls, Vcl.StdCtrls, Vcl.Dialogs, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.ImgList, Vcl.Imaging.pngimage;

type
  TSelectDialog = class(TForm)
    lbCaption: TLabel;
    imgIcon: TImage;
    ImageList: TImageList;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    Buttons : array  of TButton;
    Selection : integer;
    procedure ButtonClick(Sender: TObject);
  public
    { Public declarations }
    procedure ScaleWindow(M,D : Integer);
    function Execute (Pos : TPoint;
                      const ATitle,ACaption : string;
                      DlgType: TMsgDlgType;
                      ACaptionFormat : TFontStyles;
                      const AButtons : array of string;
                      ButtonWidth    : integer = 0;
                      PreSelection   : integer = -1;
                      const DefaultButton : string = '') : integer;
  end;

var
  SelectDialog: TSelectDialog;

function SelectOption (Pos : TPoint;
                       const ATitle,ACaption : string;
                       DlgType: TMsgDlgType;
                       ACaptionFormat : TFontStyles;
                       const AButtons : array of string;
                       ButtonWidth    : integer = 0;
                       PreSelection   : integer = -1;
                       const DefaultButton : string = '') : integer; overload;

function SelectOption (const ATitle,ACaption : string;
                       DlgType: TMsgDlgType;
                       ACaptionFormat : TFontStyles;
                       const AButtons : array of string;
                       ButtonWidth    : integer = 0;
                       PreSelection   : integer = -1;
                       const DefaultButton : string = '') : integer; overload;

procedure ScaleSelectOption (M,D : Integer);

implementation

{$R *.dfm}

uses GnuGetText, WinUtils;

var
  Mult,Divi : integer;

procedure TSelectDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent (self,'dialogs');
  end;

procedure TSelectDialog.ScaleWindow(M, D: Integer);
begin
  ChangeScale(M, D);
  end;

procedure TSelectDialog.ButtonClick(Sender: TObject);
begin
  Selection:=(Sender as TButton).Tag;
  end;

//   Result = -1 : default button
//             0,1,2,.. : Index of AButtons
function TSelectDialog.Execute (Pos : TPoint;
                                const ATitle,ACaption : string;
                                DlgType: TMsgDlgType;
                                ACaptionFormat : TFontStyles;
                                const AButtons : array of string;
                                ButtonWidth    : integer = 0;
                                PreSelection   : integer = -1;
                                const DefaultButton : string = '') : integer;
var
  w,h,dh,i,j,n,nr,nl,k,l,bw : integer;
begin
  with Pos do begin
    if (Y < 0) and (X < 0) then Position:=poScreenCenter
    else begin
      CheckScreenBounds(Screen,x,y,Width,Height);
      Left:=x; Top:=y;
      end;
    end;
  Caption:=ATitle;
  if ButtonWidth=0 then bw:=105 else bw:=ButtonWidth;
  ClientWidth:=3*bw+70;
  w:=ClientWidth; l:=50;
  with lbCaption do begin
    Font.Style:=ACaptionFormat;
    Left:=l; Width:=w-l-10;
    Caption:=ACaption;
    h:=Top+Height+20;
    dh:=MulDiv(abs(Font.Height),24,10);
    end;
  with imgIcon do if DlgType=mtCustom then Hide
  else begin
    Picture:=nil;
    ImageList.GetBitmap(integer(DlgType),Picture.Bitmap);
    Show;
    end;
  n:=length(AButtons)+1;
  nr:=(n-1) div 3;       // Anzahl Zeilen
  nl:=n-3*nr;            // Spalten in letzter Zeile
  SetLength(Buttons,n);
  for i:=0 to nr-1 do begin
    for j:=0 to 2 do begin
      k:=3*i+j;
      Buttons[k]:=TButton.Create(self);
      with Buttons[k] do begin
        Parent:=self;
        Height:=dh; Width:=bw;
        Top:=h; Left:=l+j*(Width+5);
        Caption:=AButtons[k];
        Tag:=k;
        TabStop:=true;
        TabOrder:=k;
        ModalResult:=mrOK;
        OnClick:=ButtonClick;
        end;
      end;
    inc(h,MulDiv(dh,30,26));
    end;
  for j:=0 to nl-1 do begin
    k:=3*nr+j;
    Buttons[k]:=TButton.Create(self);
    with Buttons[k] do begin
      Parent:=self;
      Height:=dh; Width:=bw;
      Top:=h; Left:=l+(3-nl+j)*(Width+5);
      if k=n-1 then begin
        if length(DefaultButton)=0 then Caption:=dgettext('dialogs','Cancel')
        else Caption:=DefaultButton;
        Cancel:=true;
        Tag:=-1;
        end
      else begin
        Caption:=AButtons[k];
        Tag:=k;
        end;
      TabStop:=true;
      TabOrder:=k;
      ModalResult:=mrOK;
      OnClick:=ButtonClick;
      end;
    end;
  ClientHeight:=h+MulDiv(dh,30,26);
  Selection:=-1;
  if PreSelection>length(AButtons) then PreSelection:=-1;
  if PreSelection<0 then ActiveControl:=Buttons[n-1]
  else ActiveControl:=Buttons[PreSelection];
  ShowModal;
  Result:=Selection;
  for i:=ControlCount-1 downto 0 do if (Controls[i] is TButton) then Controls[i].Free;
  end;

function SelectOption (Pos : TPoint;
                       const ATitle,ACaption : string;
                       DlgType: TMsgDlgType;
                       ACaptionFormat : TFontStyles;
                       const AButtons : array of string;
                       ButtonWidth    : integer = 0;
                       PreSelection   : integer = -1;
                       const DefaultButton : string = '') : integer;
begin
  if not assigned(SelectDialog)then SelectDialog:=TSelectDialog.Create(Application);
//  SelectDialog.ScaleWindow(Mult,Divi);
  Result:=SelectDialog.Execute(Pos,ATitle,ACaption,DlgType,ACaptionFormat,AButtons,
    ButtonWidth,PreSelection,DefaultButton);
  FreeAndNil(SelectDialog);
  end;


function SelectOption (const ATitle,ACaption : string;
                       DlgType: TMsgDlgType;
                       ACaptionFormat : TFontStyles;
                       const AButtons : array of string;
                       ButtonWidth    : integer = 0;
                       PreSelection   : integer = -1;
                       const DefaultButton : string = '') : integer;
begin
  if not assigned(SelectDialog)then SelectDialog:=TSelectDialog.Create(Application);
//  SelectDialog.ScaleWindow(Mult,Divi);
  Result:=SelectDialog.Execute(CenterPos,ATitle,ACaption,DlgType,ACaptionFormat,AButtons,
    ButtonWidth,PreSelection,DefaultButton);
  FreeAndNil(SelectDialog);
  end;

procedure ScaleSelectOption (M,D : Integer);
begin
  Mult:=M; Divi:=D;
  end;

begin
  Mult:=1; Divi:=1;
end.
