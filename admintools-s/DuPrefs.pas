(* Delphi dialog
   Voreinstellungen für einen Domänenbenutzer

   © Dr. J. Rathlev, D-24222 Schwentinental (info(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.
    
   Vers. 1 - June 2005
   *)

unit DuPrefs;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, System.IniFiles, HListBox;

type
  TPrefDialog = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Label1: TLabel;
    hcbDesc: THistoryCombo;
    Label2: TLabel;
    Label3: TLabel;
    edtPwd: TEdit;
    Label4: TLabel;
    hcbRoom: THistoryCombo;
    cbNoExpire: TCheckBox;
    gbxDesc: TGroupBox;
    gbxPassword: TGroupBox;
    gbxProfile: TGroupBox;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    cbHomeDrive: TComboBox;
    cbProfile: TComboBox;
    cbHomeDir: TComboBox;
    Label5: TLabel;
    hcbPhone: THistoryCombo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure LoadFromIni(AIniName : string);
    function Execute (ProfileList,HomeList : TStrings;
                      var ADesc,ARoom,APhone,APwd,AProfile,AHome : string;
                      var ADrive : integer;
                      var ANoExpire : boolean) : boolean;
  end;

var
  PrefDialog: TPrefDialog;

implementation

{$R *.DFM}

uses GnuGetText;

const
  PrefSekt = 'Preferences';
  DescSekt = 'Descriptions';
  RoomSekt = 'Rooms';
  PhoneSekt = 'PhoneNumbers';

{ ------------------------------------------------------------------- }
procedure TPrefDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent(self);
  end;

{ ------------------------------------------------------------------- }
procedure TPrefDialog.LoadFromIni(AIniName : string);
begin
  hcbDesc.LoadFromIni(AIniName,DescSekt);
  hcbRoom.LoadFromIni(AIniName,RoomSekt);
  hcbPhone.LoadFromIni(AIniName,PhoneSekt);
  end;

{ ------------------------------------------------------------------- }
function TPrefDialog.Execute (ProfileList,HomeList : TStrings;
                              var ADesc,ARoom,APhone,APwd,AProfile,AHome : string;
                              var ADrive : integer;
                              var ANoExpire : boolean) : boolean;
begin
  hcbDesc.Text:=ADesc;
  hcbRoom.Text:=ARoom;
  hcbPhone.Text:=APhone;
  edtPwd.Text:=APwd;
  cbNoExpire.Checked:=ANoExpire;
  with cbProfile do begin
    Items.Assign(ProfileList);
    Text:=AProfile;
    end;
  with cbHomeDir do begin
    Items.Assign(HomeList);
    Text:=AHome;
    end;
  cbHomeDrive.ItemIndex:=ADrive;
  if ShowModal=mrOK then begin
    ADesc:=hcbDesc.Text;
    ARoom:=hcbRoom.Text;
    APhone:=hcbPhone.Text;
    APwd:=edtPwd.Text;
    ANoExpire:=cbNoExpire.Checked;
    AProfile:=cbProfile.Text;
    AHome:=cbHomeDir.Text;
    ADrive:=cbHomeDrive.ItemIndex;
    Result:=true;
    end
  else Result:=false;
  end;

end.
