(* Erzeugen von mehreren ADS-Benutzern z.B. für Kurse
   ==================================================
   Vorgabe: Basis-Benutzername (z.B. APrakt) und Anzahl der Benutzer
    ==>   : entsprechend der Anzahl werden Benutzer der Form NameNN (z.B.
            Prakt01, Prakt02, etc.) angelegt
            Jeder Benutzer erhält ein automatisch generiertes Passwort.
            Sind die Benutzer bereits vorhanden, werden nur neue Passwörter
            generiert.
            Namen und Passwörter können zur Weitergabe an die benutzer auf
            Karten ausgedruckt werden

   Achtung: Bei der Compilierung muss ev. die Optimierung abgeschaltet werden, da
   ======== sonst Schutzverletzungsfehler auftreten (warum???)

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU General Public License Version 2 or later (the "GPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1.1 - Jun. 2005
   Vers. 2.0   - Apr. 2008  - Mehrsprachigkeit mit GnugetText
   Vers. 3.0   - Jun. 2010  - Unicode (Delphi 2009)
   Vers. 4.0   - Jul. 2010  - für Jedi-Bibl.
   Vers. 4.5   - Jan. 2016  - Anpassung an Delphi 10
   *)

unit MultiMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  System.IniFiles, System.StrUtils, System.Win.ComObj, Vcl.Printers, Winapi.ActiveX,
  Vcl.ComCtrls, JwaAdsHlp, JwaAdsTLB, JwaAdsErr, JwaActiveX,
  NumberUtils, WinUtils, StringUtils, FileUtils, WinApiUtils, WinShell, HListBox,
  EtikettDlg, ExtCtrls, XPManUac;

const
  Vers = ' (Vers. 4.5)';
  CopRgt = '© 2008-2016 Dr. J. Rathlev, D-24222 Schwentinental';
  EmailAdr = 'kontakt(a)rathlev-home.de';

  IniExt = 'ini';

  clLightRed = $BFBFFF;
  clLightGreen = $BFFFBF;

  UserCannotChangePassword = true;

  phUsername = '%username%';
  PwdLength = 6;
  MaxUser = 25;

  DefFontName = 'Arial';
  DefFontSize = 12;
  HdSize = 1.25;     // Skalierungsfaktor für Schriftgöße der Kopfzeile
  HdLine = 1.5;      // Zeilenhöhe der Kopfzeile
  DefLineDist = 11;  // normale Zeilenhöhe * 10
  DefTopPos = 0;     // Abstand obere Papierkante - Nullpunkt in mm
  PtToCm = 0.0353;   // 1 Pt in cm
  // Standardlayout (Angaben in mm)
  DefPage : TPageLayout = (Margin   : (x : 10; y : 10);
                           Size     : (x : 80; y : 50);
                           Distance : (x : 80; y : 50);
                           Count    : (x : 2; y : 5));
type
  TCourseObject = class (Tobject)
    Descriptor,
    UserName,
    HomeDir,
    ProfileDir,
    Organisation,
    Term,
    OrgUnit     : string;
    HomeDrv,
    OUIndex,
    UserCount   : integer;
    Expire      : TDateTime;
    Groups      : TStringList;
    Pwd         : array [0..MaxUser-1] of string[PwdLength];
    constructor Create (ADesc,AName : string ; ACount : integer);
    destructor Destroy; override;
    end;

  TADsObject = class (TObject)
    FName,FDesc,FPath : string;
    constructor Create (AName,ADesc,Apath : string);
    end;

  TfrmMultiUser = class(TForm)
    Label6: TLabel;
    btnConnect: TBitBtn;
    btnMakeUser: TBitBtn;
    btnExit: TSpeedButton;
    btnInfo: TSpeedButton;
    gbxUser: TGroupBox;
    edtDNSName: TEdit;
    edtBaseName: TEdit;
    Label1: TLabel;
    Label5: TLabel;
    btnNew: TBitBtn;
    gbxOU: TGroupBox;
    rbtGlobal: TRadioButton;
    rbtOU: TRadioButton;
    cbxOU: TComboBox;
    gbxAg: TGroupBox;
    lbxAg: TListBox;
    hcbDomain: THistoryCombo;
    Label9: TLabel;
    Label2: TLabel;
    lvwUser: TListView;
    edtDescription: TComboBox;
    btnGenPwd: TBitBtn;
    btnLayout: TSpeedButton;
    btnFont: TSpeedButton;
    stbStatus: TStatusBar;
    FontDialog: TFontDialog;
    PrintDialog: TPrintDialog;
    btnPrint: TBitBtn;
    speCount: TEdit;
    udCount: TUpDown;
    Label3: TLabel;
    hcbInstitut: THistoryCombo;
    Panel1: TPanel;
    rbSummer: TRadioButton;
    rbWinter: TRadioButton;
    edtYear: TEdit;
    udYear: TUpDown;
    gbxProfile: TGroupBox;
    Label4: TLabel;
    hcbHomeDir: THistoryCombo;
    Label7: TLabel;
    hcbProfile: THistoryCombo;
    Panel2: TPanel;
    rbNoExpire: TRadioButton;
    rbExpire: TRadioButton;
    dtpExpire: TDateTimePicker;
    btnDelCourse: TBitBtn;
    cbHomeDrive: TComboBox;
    Label8: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure rbtGlobalClick(Sender: TObject);
    procedure rbtOUClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnMakeUserClick(Sender: TObject);
    procedure edtDescriptionSelect(Sender: TObject);
    procedure btnGenPwdClick(Sender: TObject);
    procedure edtDescriptionDropDown(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure btnLayoutClick(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure udCountClick(Sender: TObject; Button: TUDBtnType);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure hcbHomeDirExit(Sender: TObject);
    procedure hcbProfileExit(Sender: TObject);
    procedure edtYearChange(Sender: TObject);
    procedure rbNoExpireClick(Sender: TObject);
    procedure rbExpireClick(Sender: TObject);
    procedure btnDelCourseClick(Sender: TObject);
    procedure cbHomeDriveCloseUp(Sender: TObject);
  private
    { Private-Deklarationen }
    ProgVersName,
    ProgVersDate,
    AppPath,UserPath,
    IniName,ProgPath,
    DNSName,FDomain,
    PrtName,Context,
    LastCrs,ActTerm,
    TrSelf,TrEvrOne   : string;
    InitOK            : boolean;
    TextFont          : TFont;
    LineDist          : double;
    PageLayout        : TPageLayout;
    function InitDomainData : boolean;
    function SaveData (Desc : string; var co : TCourseObject) : boolean;
    procedure ShowData;
    function MakePwd : string;
    function CheckHomeDir (ADir : string) : boolean;
    function ReplaceUsername (ps,un : string) : string;
    procedure MakeUser (const co : TCourseObject);
    function SearchUser (CommonName : string) : string;
    procedure ShowExceptionError (const Action : string; E : EOleException);
    procedure GetPaperSize;
    procedure PrintInfo (co : TCourseObject);
  public
    { Public-Deklarationen }
  end;

var
  frmMultiUser: TfrmMultiUser;

implementation

uses System.DateUtils, GnuGetText, LangUtils, InitProg, TsUserEx;

{$R *.dfm}

{ ---------------------------------------------------------------- }
type
  TCrReg = array [1..8] of byte;
  TByteArray = array of byte;

function ShiftReg (var CodeReg : TCrReg;
                   b,pb        : byte) : byte;
begin
  Result:=b xor pb xor CodeReg[3] xor CodeReg[8];
  move (CodeReg[1],CodeReg[2],7);
  end;

procedure SetByte (ba : TByteArray; ndx : integer; b : byte);
begin
  if ndx and 1 = 0 then begin // gerade
    ba[2*ndx]:=b and $F + random(16)*$10;
    ba[2*ndx+1]:=(b and $F0) div $10 + random(16)*$10;
    end
  else begin // ungerade
    ba[2*ndx]:=(b and $F0) + random(16);
    ba[2*ndx+1]:=(b and $F)*$10 + random(16);
    end;
  end;

function GetByte (ba : TByteArray; ndx : integer) : byte;
begin
  if ndx and 1 = 0 then begin // gerade
    Result:=ba[2*ndx] and $F + (ba[2*ndx+1] and $F)*$10;
    end
  else begin // ungerade
    Result:=ba[2*ndx] and $F0 + (ba[2*ndx+1] and $F0) div $10;
    end;
  end;

{ ---------------------------------------------------------------- }
// Routines to obscure plain text,
// e.g. to prevent simple reading of program internal passwords
const
  Key : byte = $93;

function EncodeString (s : ShortString) : string;
var
  ba  : TByteArray;
  sn  : byte;
  n,i : integer;
begin
  sn:=length(s);
  randomize;
  n:=2*(sn+1)+5+random(5);
  SetLength (ba,n);
  SetByte (ba,0,sn);
  for i:=1 to sn do SetByte (ba,i,ord(s[i]));
  for i:=2*(sn+1) to n-1 do ba[i]:=random(256);
  for i:=0 to n-1 do ba[i]:=ba[i] xor Key;
  Result:='';
  for i:=0 to n-1 do Result:=Result+IntToHex(ba[i],2);
  end;

function DecodeString (s : string) : ShortString;
var
  ba  : TByteArray;
  sn  : byte;
  n,i : integer;
begin
  if length(s)>0 then begin
    n:=length(s) div 2;
    SetLength (ba,n);
    for i:=0 to n-1 do ba[i]:=StrToInt('$'+s[2*i+1]+s[2*i+2]);
    for i:=0 to n-1 do ba[i]:=ba[i] xor Key;
    sn:=GetByte(ba,0);
    Result:='';
    for i:=1 to sn do Result:=Result+chr(GetByte(ba,i));
    end
  else Result:='';
  end;

{ ---------------------------------------------------------------- }
constructor TADsObject.Create (AName,ADesc,Apath : string);
begin
  inherited Create;
  FName:=AName; FDesc:=ADesc; FPath:=APath;
  end;

{ ---------------------------------------------------------------- }
constructor TCourseObject.Create (ADesc,AName : string ; ACount : integer);
var
  i : integer;
begin
  inherited Create;
  Descriptor:=ADesc; UserName:=AName; UserCount:=ACount;
  HomeDir:=''; HomeDrv:=0; ProfileDir:=''; Organisation:=''; Term:=''; OrgUnit:='';
  Expire:=Date+1;
  OUIndex:=-1;
  for i:=0 to MaxUser-1 do Pwd[i]:='';
  Groups:=TStringList.Create;
  end;

destructor TCourseObject.Destroy;
begin
  Groups.Free;
  inherited Destroy;
  end;

{ ---------------------------------------------------------------- }
function ByteToWideChar(AByte : byte) : WideChar;
type
  TWideChar = record
    case integer of
    1 : (Lo,Hi : byte);
    2 : (wc : WideChar);
    end;
var
  buf : TWideChar;
begin
  with buf do begin
    if AByte=0 then Lo:=0 else Lo:=AByte+64;
    Hi:=0;
    Result:=wc;
    end;
  end;

{ ---------------------------------------------------------------- }
const
  (* INI-Sektionen *)
  CfgSekt  = 'Config';
  DomSekt  = 'Domains';
  OrgSekt  = 'Organisations';
  KursSekt = 'Course';
  HomeSekt = 'Directories';
  ProfSekt = 'Profiles';
  PrtSekt = 'Printer';
  LOSekt = 'Layout';

  (* INI-Variablen *)
  iniDomain  = 'DomainName';
  iniCount   = 'Count';
  iniDesc    = 'Name';
  iniUser    = 'User';
  iniOrg     = 'Organisation';
  iniSem     = 'Term';
  iniExpire  = 'ExpireDate';
  iniOU      = 'OU';
  iniGCount  = 'GroupCount';
  iniGrp     = 'Group';
  iniPwd     = 'Password';
  iniProf    = 'Profile';
  iniDrive   = 'Drive';
  iniHome    = 'Home';
  iniName        = 'Name';
  iniFont        = 'Font';
  iniFontSize    = 'FontSize';
  iniPrinter     = 'Printer';
  iniLineDist    = 'LineDistance';
  iniLeftMargin  = 'LeftMargin';
  iniTopMargin   = 'TopMargin';
  iniWidth       = 'Width';
  iniHeight      = 'Height';
  iniHorDist     = 'HorDistance';
  iniVertDist    = 'VertDistance';
  iniHorCount    = 'HorCount';
  iniVertCount   = 'VertCount';
  iniFrames      = 'Frames';

procedure TfrmMultiUser.FormCreate(Sender: TObject);
var
  IniFile  : TIniFile;
  cs,s,t   : string;
  i,j,
  n,nu,ng  : integer;
  co       : TCourseObject;
begin
  TranslateComponent(self);
  InitOK:=succeeded(CoInitialize(nil));
  Application.Title:=_('Create ADS users for study course'); // 'ADS-Benutzer für Kurse anlegen';
  InitPaths(AppPath,UserPath,ProgPath);
  InitVersion(_('Create ADS users for study course'),Vers,CopRgt,2,2,ProgVersName,ProgVersDate);
  Caption:=_('Create ADS users for study course')+' - '+VersInfo.Comments;
  IniName:=Erweiter(AppPath,PrgName,IniExt);
  if FileExists(IniName) then s:=IniName
  else s:=Erweiter(UserProfile,PrgName,IniExt); // von Vorversion
  FDomain := '';
  n:=MonthOf(ToDay);
  if (n>2) and (n<9) then begin
    ActTerm:='S';
    rbSummer.Checked:=true;
    end
  else begin
    ActTerm:='W';
    rbWinter.Checked:=true;
    end;
  ActTerm:=ActTerm+FormatDateTime('yyyy',ToDay);
  TextFont:=TFont.Create;
  IniFile:=TIniFile.Create(s);
  with IniFile do begin
    FDomain:=ReadString(CfgSekt,iniDomain,'');
    n:=ReadInteger(CfgSekt,iniCount,0);
    if n>0 then for i:=0 to n-1 do begin
      cs:=KursSekt+ZStrInt(i,2);
      s:=ReadString(cs,iniDesc,'');
      t:=ReadString(cs,iniUser,'');
      nu:=ReadInteger(cs,iniCount,0);
      if (length(s)>0) and (length(t)>0) and (nu>0) then begin
        co:=TCourseObject.Create(s,t,nu);
        with co do begin
          HomeDir:=ReadString(cs,iniHome,'');
          HomeDrv:=ReadInteger(cs,iniDrive,0);
          ProfileDir:=ReadString(cs,iniProf,'');
          Organisation:=ReadString(cs,iniOrg,'');
          Term:=ReadString(cs,iniSem,ActTerm);
          Expire:=ReadInteger(cs,iniExpire,round(Today+180));
          OrgUnit:=ReadString(cs,iniOU,'');
          ng:=ReadInteger(cs,iniGCount,0);
          for j:=0 to ng-1 do Groups.Add(ReadString(cs,iniGrp+ZStrInt(j,2),''));
          for j:=0 to nu-1 do Pwd[j]:=DecodeString(ReadString(cs,iniPwd+ZStrInt(j,2),''));
          end;
        edtDescription.Items.AddObject(s,co);
        end;
      end;
    PrtName:=ReadString (PrtSekt,iniDesc,'');
    TextFont.Name:=ReadString(PrtSekt,iniFont,DefFontName);
    TextFont.Size:=ReadInteger(PrtSekt,iniFontSize,DefFontSize);
    j:=ReadInteger (PrtSekt,iniLineDist,0);
    if j<DefLineDist then j:=DefLineDist;
    LineDist:=j/10;
    with PageLayout do begin
      with Margin do begin
        x:=ReadInteger (LOSekt,iniLeftMargin,DefPage.Margin.x);
        y:=ReadInteger (LOSekt,iniTopMargin,DefPage.Margin.y);
        end;
      with Size do begin
        x:=ReadInteger (LOSekt,iniWidth,DefPage.Size.x);
        y:=ReadInteger (LOSekt,iniHeight,DefPage.Size.y);
        end;
      with Distance do begin
        x:=ReadInteger (LOSekt,iniHorDist,DefPage.Distance.x);
        y:=ReadInteger (LOSekt,iniVertDist,DefPage.Distance.y);
        end;
      with Count do begin
        x:=ReadInteger (LOSekt,iniHorCount,DefPage.Count.x);
        y:=ReadInteger (LOSekt,iniVertCount,DefPage.Count.y);
        end;
      if (Size.x<50) or (Size.y<20) then PageLayout:=DefPage;
      ShowFrame:=ReadBool(LOSekt,iniFrames,false);
      end;
    Free;
    end;
  hcbDomain.LoadFromIni(IniName,DomSekt);
  hcbInstitut.LoadFromIni(IniName,OrgSekt);
  hcbHomeDir.LoadFromIni(IniName,HomeSekt);
  hcbProfile.LoadFromIni(IniName,ProfSekt);
  udCount.Max:=MaxUser;
  LastCrs:='';
  end;

procedure TfrmMultiUser.FormDestroy(Sender: TObject);
var
  IniFile  : TIniFile;
  cs       : string;
  i,j      : integer;
begin
  IniFile:=TIniFile.Create(IniName);
  with IniFile do begin
    WriteString(CfgSekt,iniDomain,FDomain);
    with edtDescription.Items do begin
      WriteInteger(CfgSekt,iniCount,Count);
      for i:=0 to Count-1 do with (Objects[i] as TCourseObject) do begin
        cs:=KursSekt+ZStrInt(i,2);
        WriteString(cs,iniDesc,Descriptor);
        WriteString(cs,iniUser,Username);
        WriteInteger(cs,iniCount,UserCount);
        WriteString(cs,iniHome,HomeDir);
        WriteInteger(cs,iniDrive,HomeDrv);
        WriteString(cs,iniProf,ProfileDir);
        WriteString(cs,iniOrg,Organisation);
        WriteString(cs,iniSem,Term);
        WriteInteger(cs,iniExpire,round(Expire));
        WriteString(cs,iniOU,OrgUnit);
        with Groups do begin
          WriteInteger(cs,iniGCount,Count);
          for j:=0 to Count-1 do WriteString(cs,iniGrp+ZStrInt(j,2),Strings[j]);
          end;
        for j:=0 to UserCount-1 do WriteString(cs,iniPwd+ZStrInt(j,2),EncodeString(Pwd[j]));
        end;
      end;
    WriteString (PrtSekt,iniDesc,PrtName);
    WriteString (PrtSekt,IniFont,TextFont.Name);
    WriteInteger(PrtSekt,IniFontSize,TextFont.Size);
    WriteInteger (PrtSekt,iniLineDist,round(10*LineDist));
    with PageLayout do begin
      with Margin do begin
        WriteInteger (LOSekt,iniLeftMargin,x);
        WriteInteger (LOSekt,iniTopMargin,y);
        end;
      with Size do begin
        WriteInteger (LOSekt,iniWidth,x);
        WriteInteger (LOSekt,iniHeight,y);
        end;
      with Distance do begin
        WriteInteger (LOSekt,iniHorDist,x);
        WriteInteger (LOSekt,iniVertDist,y);
        end;
      with Count do begin
        WriteInteger (LOSekt,iniHorCount,x);
        WriteInteger (LOSekt,iniVertCount,y);
        end;
      WriteBool(LOSekt,iniFrames,ShowFrame);
      end;
    Free;
    end;
  TextFont.Free;
  if InitOK then CoUninitialize;
  end;

procedure TfrmMultiUser.FormShow(Sender: TObject);
begin
  if InitOK then begin
    with Printer do begin
      PrinterIndex:=Printers.IndexOf(PrtName);
      PrtName:=Printers[PrinterIndex];
      end;
    GetPaperSize;
    with TextFont do stbStatus.Panels[1].Text:=' '+Name+' '+IntToStr(Size)+' pt';
    with Printer do stbStatus.Panels[0].Text:=_(' Printer: ')+PrtName;
    if length(FDomain)>0 then InitDomainData;   // muss als letztes aufgerufen werden ???
    end
  else ErrorDialog('',_('COM Initializing failed!')); //'COM-Initialisierung fehlgeschlagen!'
  end;

{ ------------------------------------------------------------------- }
procedure TfrmMultiUser.GetPaperSize;
var
  v : integer;
begin
  with Printer,PageLayout do begin
    PrSize.X:=GetPaperWidth(Printer);
    PrSize.Y:=GetPaperHeight(Printer);
    MinMarg.X:=GetLeftOffset(Printer);
    MinMarg.Y:=GetTopOffset(Printer);
    UsableSize.X:=GetMaxWidth(Printer);
    UsableSize.Y:=GetMaxHeight(Printer);
    if Orientation=poPortrait then begin
      with PrSize do if X>Y then begin
        v:=X; X:=Y; Y:=v;
        end;
      end
    else begin
      with PrSize do if X<Y then begin
        v:=X; X:=Y; Y:=v;
        end;
      end
    end;
  end;

{ ---------------------------------------------------------------- }
const
  ColCount = 3;
  ColNames : array[0..ColCount-1] of WideString = ('Name','Description','ADsPath');

function TfrmMultiUser.InitDomainData : boolean;
const
  szLen = 256;

var
  root         : IADs;
  DSearch      : IDirectorySearch;
  bind         : widestring;
  opt          : ads_searchpref_info;
  hr           : HResult;
  ptrResult    : THandle;
  col          : ads_search_column;
  dwErr        : DWord;
  szErr,szName : array[0..szLen-1] of WideChar;
  sd,sn,sp     : string;

  function ToPrincipal(cs : string) : string;
  var
    i : integer;
    t : string;
  begin
    t:='';
    while length(cs)>0 do begin
      i:=pos('DC=',cs);
      if i>0 then begin
        delete(cs,1,3);
        if length(t)=0 then t:=ReadNxtStr(cs,',')
        else t:=t+'.'+ReadNxtStr(cs,',');
        end
      else cs:='';
      end;
    Result:=t;
    end;

begin
  Result:=false;
  hcbDomain.Color:=clLightRed;
  Application.Processmessages;
  Screen.Cursor:=crHourglass;
  bind:='LDAP://'+FDomain;
  try
    ADsGetObject(PWideChar(bind),IID_IADs,pointer(root));
    Context:=root.Get('distinguishedName');
    DNSName:=ToPrincipal(Context);
    edtDNSName.Text:='@'+DNSName;
    Caption:=FDomain+' - Add Course Users to Domain';
    edtBaseName.Text:='';
    ADsGetObject(PWideChar(bind),IID_IDirectorySearch,pointer(DSearch));
    btnMakeUser.Enabled:=true;
    with hcbDomain do begin
      Color:=clLightGreen;
      Text:=FDomain;
      end;
    with opt do begin
      dwSearchPref:=ADS_SEARCHPREF_SEARCH_SCOPE;
      vValue.dwType:=ADSTYPE_INTEGER;
      vValue.Integer:=ADS_SCOPE_SUBTREE;
      end;
    with DSearch do begin
      if Failed(SetSearchPreference(@opt,1)) then begin
        ADsGetLastError(dwErr,@szErr,szLen,@szName[0],szLen);
        ShowMessage(WideCharToString(szErr)+sLineBreak+WideCharToString(szName));
        btnMakeUser.Enabled:=false;
        exit;
        end;
      // Organisationseinheiten suchen
      cbxOU.Clear;
      ExecuteSearch('(objectClass=organizationalUnit)',@ColNames,ColCount,ptrResult);
      hr:=GetNextRow(ptrResult);
      while (hr<>S_ADS_NOMORE_ROWS) do begin
        sd:=''; sd:=''; sp:='';
        if Succeeded(GetColumn(ptrResult,PWideChar(ColNames[1]),col)) then begin //Description
          with col do if pADsValues<>nil then sd:=pAdsvalues^.CaseExactString;
          FreeColumn(@col);
          if (length(sd)>0) and (sd[1]='-') then sd:='';
          end;
        if length(sd)>0 then begin
          if Succeeded(GetColumn(ptrResult,PWideChar(ColNames[0]),col)) then begin //Name
            with col do if pADsValues<>nil then sn:=pAdsvalues^.CaseIgnoreString;
            FreeColumn(@col);
            end;
          if Succeeded(GetColumn(ptrResult,PWideChar(ColNames[2]),col)) then begin //ADSPath
            with col do if pADsValues<>nil then sp:=pAdsvalues^.CaseIgnoreString;
            FreeColumn(@col);
            end;
          if length(sp)>0 then cbxOU.Items.AddObject(sd,TADsObject.Create(sn,sd,sp));
          end;
        hr:=GetNextRow(ptrResult);
        end;
      // Gruppen suchen
      lbxAg.Clear;
      ExecuteSearch('(&(objectClass=group)(!CN=Builtin))',@ColNames,ColCount-1,ptrResult);
      hr:=GetNextRow(ptrResult);
      while (hr<>S_ADS_NOMORE_ROWS) do begin
        sd:=''; sd:=''; sp:='';
        if Succeeded(GetColumn(ptrResult,PWideChar(ColNames[1]),col)) then begin
          with col do if pADsValues<>nil then sd:=pAdsvalues^.CaseExactString;
          FreeColumn(@col);
          end;
        if length(sd)>0 then begin
          if Succeeded(GetColumn(ptrResult,PWideChar(ColNames[0]),col)) then begin
            with col do if pADsValues<>nil then sn:=pAdsvalues^.CaseIgnoreString;
            FreeColumn(@col);
            end;
          if Succeeded(GetColumn(ptrResult,PWideChar(ColNames[2]),col)) then begin
            with col do if pADsValues<>nil then sp:=pAdsvalues^.CaseIgnoreString;
            FreeColumn(@col);
            end;
          if length(sp)>0 then lbxAg.Items.AddObject(sd,TADsObject.Create(sn,sd,sp));
          end;
        hr:=GetNextRow(ptrResult);
        end;
      // Retrieve Sids for groups "Everyone" and "NT Authority\SELF"
//      TrSelf:=GetAccountName('S-1-5');
      TrSelf:=GetAccountName('S-1-5-10');  // SELF
      TrEvrOne:=GetAccountName('S-1-1-0'); // EVERYONE
      end;
    DSearch:=nil;
    Result:=true;
  except
    ErrorDialog('',_('Error connecting to ')+FDomain); //'Fehler bei der Anmeldung an '
    Caption:=_('No connection to domain'); //'Keine Verbindung zur Domäne';
    btnMakeUser.Enabled:=false;
    with hcbDomain do begin
      Color:=clWindow;
      Text:=FDomain;
      SetFocus;
      end;
    end;
  root:=nil;
  Screen.Cursor:=crDefault;
  end;

{ ---------------------------------------------------------------- }
procedure TfrmMultiUser.btnConnectClick(Sender: TObject);
begin
  with hcbDomain do begin
    FDomain:=Text;
    AddItem(Text);
    end;
  if length(FDomain)>0 then InitDomainData;
  end;

procedure TfrmMultiUser.btnExitClick(Sender: TObject);
begin
  Close;
  end;

procedure TfrmMultiUser.btnInfoClick(Sender: TObject);
begin
  InfoDialog('',Application.Title+Vers+' - '+ProgVersDate+sLineBreak+
             CopRgt+sLineBreak+EmailAdr);
  end;

procedure TfrmMultiUser.btnNewClick(Sender: TObject);
var
  n : integer;
begin
  edtBaseName.Text:='';
  edtDescription.Text:='';
  udCount.Position:=1;
  n:=MonthOf(ToDay);
  rbSummer.Checked:=(n>2) and (n<9);
  udYear.Position:=YearOf(ToDay);
  ActTerm:=ActTerm+FormatDateTime('yyyy',ToDay);
  hcbHomeDir.Text:='';
  cbHomeDrive.ItemIndex:=0;
  hcbProfile.Text:='';
  hcbInstitut.Text:='';
  rbtGlobal.Checked:=true;
  cbxOU.ItemIndex:=-1;
  lbxAg.DeleteSelected;
  lvwUser.Clear;
  end;

{ ---------------------------------------------------------------- }
// Domänen-Benutzer suchen, Rückgabewert: ADSPath
function TfrmMultiUser.SearchUser (CommonName : string) : string;
var
  DSearch      : IDirectorySearch;
  opt          : ads_searchpref_info;
  ptrResult    : THandle;
  col          : ads_search_column;
begin
  Result:='';
  if Failed(ADsGetObject(PWideChar('LDAP://'+FDomain),IID_IDirectorySearch,pointer(DSearch))) then exit;
  with opt do begin
    dwSearchPref:=ADS_SEARCHPREF_SEARCH_SCOPE;
    vValue.dwType:=ADSTYPE_INTEGER;
    vValue.Integer:=ADS_SCOPE_SUBTREE;
    end;
  with DSearch do begin
    if Succeeded(SetSearchPreference(@opt,1)) then begin
      ExecuteSearch(PWideChar('(&(objectClass=user)(sAMAccountName='+CommonName+'))'),@ColNames,ColCount,ptrResult);
      if GetNextRow(ptrResult)<>S_ADS_NOMORE_ROWS then begin
        if Succeeded(GetColumn(ptrResult,PWideChar(ColNames[2]),col)) then begin
          with col do if pADsValues<>nil then Result:=pAdsvalues^.CaseIgnoreString;
          FreeColumn(@col);
          end;
        end
      end
    end;
  end;

procedure  TfrmMultiUser.ShowExceptionError (const Action : string; E : EOleException);
begin
  ErrorDialog('ADS-Error ('+Action+'): '+sLineBreak+E.Message+' ('+IntToHex(E.ErrorCode,8)+')');
  end;

{ ---------------------------------------------------------------- }
// automatische Generierung eines Passwortes der Form
// xaxann  mit x = Konsonant, a = Vokal, n = Ziffer
const
  Vokale : set of char = ['a','e','i','o','u'];
  NChars = 6;

function TfrmMultiUser.MakePwd : string;

  function RandomChar (NoVocal : boolean) : char;
  var
    c : char;
  begin
    repeat c:=chr(random(26)+$61) until (c in Vokale) xor NoVocal;
    result:=c;
    end;

  function RandomDigit : char;
  begin
    result:=chr(random(10)+$30);
    end;

begin
  randomize;
  Result:=RandomChar(true)+RandomChar(false)+RandomChar(true)+RandomChar(false)+
     RandomDigit+RandomDigit;
  end;

{ ---------------------------------------------------------------- }
// Einstellungen sichern
function TfrmMultiUser.SaveData (Desc : string; var co : TCourseObject) : boolean;
var
  i,n : integer;
  s   : string;
begin
  Result:=true; co:=nil;
  if length(Desc)>0 then with edtDescription,Items do begin
    n:=IndexOf(Desc);
    if n>=0 then begin
      co:=Objects[n] as TCourseObject;
      with co do begin
        UserName:=edtBaseName.Text;
        if UserCount>udCount.Position then begin
          for i:=udCount.Position to UserCount-1 do Pwd[i]:='';
          end;
        if UserCount<udCount.Position then begin
          for i:=UserCount to udCount.Position-1 do Pwd[i]:=MakePwd;
          end;
        UserCount:=udCount.Position;
//        ItemIndex:=n;
        end;
      end
    else begin
      if ConfirmDialog('',Format(_('Create new course: %s'),[Desc])) then begin //'Neuen Kurs: %s anlegen?'
        co:=TCourseObject.Create(Desc,edtBaseName.Text,udCount.Position);
        ItemIndex:=AddObject(Desc,co);
        for i:=0 to udCount.Position-1 do co.Pwd[i]:=MakePwd;
        end
      else Result:=false;
      end;
    if Result then with co do begin
      Organisation:=hcbInstitut.Text;
      HomeDir:=hcbHomeDir.Text;
      HomeDrv:=cbHomeDrive.ItemIndex;
      ProfileDir:=hcbProfile.Text;
      if rbSummer.Checked then s:='S' else s:='W';
      Term:=s+edtYear.Text;
      Expire:=dtpExpire.Date;
      if rbtGlobal.Checked then begin
        OrgUnit:=''; OUIndex:=-1;
        end
      else with cbxOU do begin
        OrgUnit:=Text; OUIndex:=ItemIndex;
        end;
      Groups.Clear;
      with lbxAg do for i:=0 to Count-1 do if Selected[i] then begin
        if Groups.Count<20 then Groups.Add(Items[i]);
        end;
      end;
    end
  else Result:=false;
  end;

// gespeicherte Einstellungen anzeigen
procedure TfrmMultiUser.ShowData;
var
  n,i : integer;
  li  : TListItem;
begin
  with edtDescription do if Items.Count>0 then
   with (Items.Objects[ItemIndex] as TCourseObject) do begin
    edtBaseName.Text:=UserName;
    udCount.Position:=UserCount;
    hcbInstitut.Text:=Organisation;
    hcbHomeDir.Text:=HomeDir;
    cbHomeDrive.ItemIndex:=HomeDrv;
    hcbProfile.Text:=ProfileDir;
    edtYear.Text:=copy(Term,2,4);
    rbSummer.Checked:=Term[1]='S';
    dtpExpire.Date:=Expire;
    with cbxOU do begin
      ItemIndex:=cbxOU.Items.IndexOf(OrgUnit);
      if ItemIndex<0 then rbtGlobal.Checked:=true
      else rbtOU.Checked:=true;
      end;
    lbxAg.ClearSelection;
    with Groups do for i:=0 to Count-1 do begin
      n:=lbxAg.Items.IndexOf(Strings[i]);
      if n>=0 then lbxAg.Selected[n]:=true;
      end;
    with lvwUser do begin
      Clear;
      for i:=0 to UserCount-1 do begin
        li:=Items.Add;
        li.Caption:=IntToStr(i+1);
        with li do begin
          SubItems.Add(UserName+ZStrint(i+1,2));
          SubItems.Add(Pwd[i]);
          end;
        end;
      end;
    end;
  end;

{ ---------------------------------------------------------------- }
procedure TfrmMultiUser.btnGenPwdClick(Sender: TObject);
var
  i   : integer;
  co  : TCourseObject;
begin
  if SaveData (edtDescription.Text,co) then with co do begin
    for i:=0 to udCount.Position-1 do co.Pwd[i]:=MakePwd;
    ShowData;
    end;
  end;

procedure TfrmMultiUser.btnMakeUserClick(Sender: TObject);
var
  co    : TCourseObject;
  s     : string;
begin
  if SaveData(edtDescription.Text,co) then begin
    if co.OUIndex<0 then s:=_('as domain user') //'als Domänenbenutzer'
    else s:=_('to organizational unit: ') //'in Organisationseinheit: '
       +(cbxOU.Items.Objects[co.OUIndex] as TAdsObject).FDesc;;
    with co do begin
      s:=_('Course: ')+edtDescription.Text+sLineBreak+
         Format(_('Add user names: %s01..%u'+sLineBreak+'%s?'),[UserName,UserCount,s]);
//       Format(_('Benutzernamen: : %s01..%u'+sLineBreak+'%s eintragen?'),[UserName,UserCount,s]);
      if ConfirmDialog('',s) then MakeUser(co);
      end;
    end;
  end;

{ ------------------------------------------------------------------- }
(* Platzhalter im String ersetzen *)
function TfrmMultiUser.ReplaceUsername (ps,un : string) : string;
begin
  if AnsiContainsText(ps,phUserName) then Result:=AnsiReplaceText(ps,phUserName,un)
  else Result:=ps;
  end;

{ ---------------------------------------------------------------- }
procedure TfrmMultiUser.MakeUser (const co : TCourseObject);
var
  ADsCont  : IADsContainer;
  User     : IDispatch;
  TSUser   : IADsTSUserEx;          // Terminal services usesr
  Grp      : IAdsGroup;
  varArr   : OleVariant;
  lNumEl   : ULong;
  SecDesc  : IADsSecurityDescriptor;
  ACL      : IADsAccessControlList;
  ACE      : IADsAccessControlEntry;
  IU       : IUnknown;
  ENum     : IEnumVARIANT;
  bind     : widestring;
  ap,un,fn,
  s,sh,sp,sd : string;
  OldType,
  i,j,nn,nc  : integer;
  chg        : boolean;

const
  SpecChars : set of char = [',','/',';'];

  function ReplaceSpecChar(s : string) : string;
  var
    t : string;
    i : integer;
  begin
    t:='';
    for i:=1 to length(s) do if (s[i] in SpecChars) then t:=t+'\'+s[i] else t:=t+s[i];
    Result:=t;
    end;

begin
  with co do if (length(UserName)>0) then begin
    if co.OUIndex<0 then bind:='LDAP://'+FDomain+'/CN=Users,'+Context
    else bind:=(cbxOU.Items.Objects[OUIndex] as TAdsObject).FPath;
    nn:=0; nc:=0;
    for i:=0 to UserCount-1 do begin
      un:=UserName+ZStrInt(i+1,2);
      fn:=Descriptor+', '+_('User ')+IntToStr(i+1);
    // prüfen, ob der Benutzer bereits eingetragen ist
      ap:=SearchUser (un);
      if length(ap)>0 then begin
        if Failed(ADsGetObject(PWideChar(ap),IID_IADsUser,pointer(user))) then Break;
        with User as IADsUser  do begin
          GetInfo;
    // Benutzer bereits vorhanden, nur Passwort ändern
          if length(Pwd[i])>0 then begin
            try
              SetPassword (Pwd[i]);
            except
              on E:EOleException do begin
                ShowExceptionError(_('Setting password'),E);
                Break;
                end;
              end;
            if rbNoExpire.Checked then
              Put('userAccountControl',ADS_UF_DONT_EXPIRE_PASSWD or ADS_UF_PASSWD_CANT_CHANGE)
            else begin
              AccountExpirationDate:=dtpExpire.Date+1;
              Put('userAccountControl',ADS_UF_PASSWD_CANT_CHANGE);
              end;
            end;
          AccountDisabled:=false;
          try
            SetInfo;       // Speichern
          except
            on E:EOleException do begin
              ShowExceptionError(_('Setting password credentials'),E);
              Break;
              end;
            end;
          end;
        inc(nc);
        end
      else begin // noch nicht vorhanden
        if Failed(ADsGetObject(PWideChar(bind),IADsContainer,pointer(ADsCont))) then Break;
        User:=ADsCont.Create('user','CN='+ReplaceSpecChar(fn));
        if assigned(user) then with User as IADsUser do begin
      // set Mandatory attributes
          Put('sAMAccountName',un);
          sh:=ReplaceUsername(HomeDir,un);
          sp:=ReplaceUsername(ProfileDir,un);
          with cbHomeDrive do if ItemIndex>0 then sd:=Items[ItemIndex] else sd:='';
      // set Optional attributes
          FullName:=fn;
          LastName:=un;
          Description:=Descriptor;
          if length(Organisation)>0 then Department:=Organisation;
          if length(HomeDir)>0 then HomeDirectory:=sh;
          if length(sd)>0 then Put('homeDrive',sd);
          if length(ProfileDir)>0 then Profile:=sp;
          Put('userPrincipalName',un+'@'+DNSName);
          try
            SetInfo;       // Speichern
          except
            on E:EOleException do begin
              ShowExceptionError(_('Setting user name'),E);
              Break;
              end;
            end;

          // Terminal services - requires at least Windows Server 2008
          // https://msdn.microsoft.com/en-us/library/aa380823(v=vs.85).aspx
          if ((length(HomeDir)>0) or (length(ProfileDir)>0)) and
               FileExists(IncludeTrailingPathDelimiter(SystemDirectory)+'tsuserex.dll') then begin
            try
              GetInfo;
              if succeeded(QueryInterface(IID_IADsTSUserEx, TSUser)) then begin
                TSUser.TerminalServicesProfilePath:=sp;
                if length(sd)=0 then sd:='U:';
                TSUser.TerminalServicesHomeDrive:=s;
                TSUser.TerminalServicesHomeDirectory:=sh;
                SetInfo;
                end;
            except
              end;
            end;

      // additional attributes
          if length(Pwd[i])>0 then begin
            try
              SetPassword (Pwd[i]);
            except
              on E:EOleException do begin
                ShowExceptionError(_('Setting password'),E);
                Break;
                end;
              end;
            if rbNoExpire.Checked then
              Put('userAccountControl',ADS_UF_DONT_EXPIRE_PASSWD or ADS_UF_PASSWD_CANT_CHANGE)
            else begin
              AccountExpirationDate:=dtpExpire.Date+1;
              Put('userAccountControl',ADS_UF_PASSWD_CANT_CHANGE);
              end;
            end;
          AccountDisabled:=false;
          try
            SetInfo;       // Speichern
          except
            on E:EOleException do begin
              ShowExceptionError(_('Setting password credentials'),E);
              Break;
              end;
            end;
          end;
      // Arbeitsgruppen zuordnen
        try
          with lbxAG do if SelCount>0 then for j:=0 to Items.Count-1 do if Selected[j] then begin
            if succeeded(ADsGetObject(PWideChar((Items.Objects[j] as TADsObject).FPath),IID_IADsGroup,pointer(Grp))) then
              Grp.Add((user as IAdsUser).AdsPath);
            end;
        except
          on E:EOleException do begin
            ShowExceptionError(_('Setting group'),E);
            Break;
            end;
          end;
        inc(nn);
        end;
      // Set "User Cannot Change Password"
      // see sample in "Windows Platform SDK"
      IU:=(User as IADsUser).Get('ntSecurityDescriptor');
      IU.QueryInterface(IID_IADsSecurityDescriptor,SecDesc);
      ACL:=IADsAccessControlList(SecDesc.DiscretionaryAcl);
      IU:=ACL._NewEnum; chg:=false;
      if succeeded(IU.QueryInterface(IID_IEnumVARIANT,Enum)) then begin
        while (Succeeded(ADsEnumerateNext(Enum,1,VarArr,lNumEl)))and (lNumEl>0) do begin
          if Succeeded(IDispatch(varArr).QueryInterface(IID_IADsAccessControlEntry,ACE)) then with ACE do begin
            OldType:=AceType;
            if UpperCase(ObjectType)=UpperCase(CHANGE_PASSWORD_GUID) then begin
            // nachfolgend sind die länderspez. Namen zu benutzen:
            // z.B. "Jeder" und "NT-AUTORITÄT\SELBST"
            // Die Ermittlung dieser Namen erfolgt über die Funktion
            // GetAccountName in WinApi (s.o.)
              if Trustee=TrEvrOne then begin  // 'Everyone'
                // Modify the ace type of the entry.
                if UserCannotChangePassword then AceType:=ADS_ACETYPE_ACCESS_DENIED_OBJECT
                else AceType:=ADS_ACETYPE_ACCESS_ALLOWED_OBJECT;
                end;
            // TrSelf (s.o.) enthält nicht den Teil "NT AUTHORITY\" (bzw. "NT-AUTORITÄT\")
            // Ich habe keine Infos gefunden, wie das anders zu machen geht
            // Da die Strings so nicht mit Trustee verglichen werden können,
            // wird der Teil vor "\" entfernt
              j:=AnsiPos('\',Trustee);
              if j>0 then begin
                s:=AnsiRightStr(Trustee,length(Trustee)-j);
                if s=TrSelf then begin // 'NT AUTHORITY\SELF'
                  // Modify the ace type of the entry.
                  if UserCannotChangePassword then AceType:=ADS_ACETYPE_ACCESS_DENIED_OBJECT
                  else AceType:=ADS_ACETYPE_ACCESS_ALLOWED_OBJECT;
                  end;
                end;
              end;
            chg:=OldType<>AceType;
            end;
          end;
        if chg then begin
          try
            // Update the ntSecurityDescriptor property.
            (User as IADsUser).Put ('ntSecurityDescriptor',SecDesc);
            //Commit the changes to the server.
            (User as IADsUser).SetInfo;
          except
            on E:EOleException do begin
              ShowExceptionError(_('Setting security descriptor'),E);
              Break;
              end;
            end;
          end;
        end;
      Grp:=nil;
      user:=nil;
      ADsCont:=nil;
      end;
    if nn=0 then begin
      if nc=0 then ap:=_('Unable to make changes!') //'Es konnten keine Änderungen durchgeführt werden!'
      else ap:=Format(_('New passwords were set for %u user accounts!'),[nc]);
//      'Für %u Benutzerkonten wurden neue Passwörter festgelegt!';
      end
    else begin
      ap:=Format(_('%u new user accounts were created'),[nn]);
//      'Es wurden %u neue Benutzerkonten angelegt';
      if nc=0 then ap:=ap+'!'
      else ap:=ap+sLineBreak+
        Format(_('and new passwords were set for %u user accounts!'),[nc]);
//      'und für %u Benutzerkonten wurden neue Passwörter festgelegt!';
      end;
    InfoDialog('',ap);
    end
  else InfoDialog('',_('Please specify a user name!')); //'Geben Sie bitte einen Anmeldenamen an!'
  end;

procedure TfrmMultiUser.rbtGlobalClick(Sender: TObject);
begin
  cbxOU.Enabled:=false;
  end;

procedure TfrmMultiUser.rbtOUClick(Sender: TObject);
begin
  cbxOU.Enabled:=true;
  end;

procedure TfrmMultiUser.edtDescriptionSelect(Sender: TObject);
var
  co : TCourseObject;
begin
  SaveData(LastCrs,co);
  ShowData;
  end;

procedure TfrmMultiUser.edtDescriptionDropDown(Sender: TObject);
begin
  LastCrs:=edtDescription.Text;
  end;

{ ------------------------------------------------------------------- }
(* Liste drucken *)
procedure TfrmMultiUser.PrintInfo (co : TCourseObject);
var
  i,j,k,h,lh,
  x,x0,y,y0,
  dx,dy,
  yn,xn,yc,xc : integer;
  f        : double;
  s        : string;

  procedure DrawRect(Canvas : TCanvas; Left,Width,Top,Height : integer);
  begin
    with Canvas do begin
      MoveTo(10*Left,-10*Top);
      LineTo(10*(Left+Width),-10*Top);
      LineTo(10*(Left+Width),-10*(Top+Height));
      LineTo(10*Left,-10*(Top+Height));
      LineTo(10*Left,-10*Top);
      end;
    end;

begin
  with Printer do begin
    with Canvas do begin
      TextOut (0,0,'');
      SetMapMode (Handle,MM_LOMETRIC);   // 1/10 mm
      with PageLayout do begin
        x0:=(Margin.x-MinMarg.x+Size.x div 2)*10;
        y0:=(Margin.y-MinMarg.y)*10;
        dx:=Distance.x*10; dy:=Distance.y*10;
        h:=size.y*10;
        yn:=Count.y; xn:=Count.x;
        if ShowFrame then begin
          for k:=0 to Count.Y-1 do for j:=0 to Count.X-1 do
            DrawRect(Canvas,Margin.x-MinMarg.x+j*Distance.X,Size.X,
                  Margin.y-MinMarg.y+k*Distance.Y,Size.Y);
          end;
        end;
      Font:=TextFont;
      with Font do begin
        f:=PtToCm*100*Size;
        Height:=round(f);
        lh:=round(LineDist*f);    // Zeilenhöhe
        end;
      y0:=y0+(h-6*lh) div 2;
      x:=x0; y:=y0;
      yc:=-1; xc:=0;
      SetTextAlign (Canvas.Handle,TA_CENTER);
      with co do begin
        for i:=0 to UserCount-1 do begin
          if yc=yn then begin // neue Seite
            Newpage;
            y:=y0; yc:=0; x:=x0;  xc:=0;
            TextOut (0,0,'');
            SetMapMode (Handle,MM_LOMETRIC);
            with PageLayout do if ShowFrame then begin
              for k:=0 to Count.Y-1 do for j:=0 to Count.X-1 do
                DrawRect(Canvas,Margin.x-MinMarg.x+j*Distance.X,Size.X,
                      Margin.y-MinMarg.y+k*Distance.Y,Size.Y);
              end;
            end;
          if yc<0 then yc:=0;
          TextOut (x,-y,Organisation);
          if Term[1]='S' then s:=Format(_('Summer term %s'),[copy(term,2,4)]) //'Sommersemester %s'
          else begin
            s:=Format(_('Winter term %s'),[copy(term,2,4)]); //'Wintersemster ';
            try
              k:=StrToInt(copy(term,2,4));
              s:=s+'/'+IntToStr(k+1);
            except
              end;
            end;
          TextOut (x,-y-lh,s);
          TextOut (x,-y-2*lh,Descriptor);
          TextOut (x,-y-4*lh,_('User name: ')+UserName+ZStrint(i+1,2)); //'Benutzername: '
          TextOut (x,-y-5*lh,_('Password: ')+Pwd[i]);   //'Passwort: '
          inc(xc);
          if xc=xn then begin  // neue Zeile
            x:=x0;  xc:=0; inc(y,dy); inc(yc);
            end
          else begin  // neue Spalte
            inc(x,dx);
            end;
          end;
        end;
      end;
    end;
  end;

procedure TfrmMultiUser.btnFontClick(Sender: TObject);
begin
  with FontDialog do begin
    Font:=TextFont;
    if Execute then begin
      TextFont:=Font;
      with TextFont do stbStatus.Panels[1].Text:=' '+Name+' '+IntToStr(Size)+' pt';
      end;
    end;
  end;

procedure TfrmMultiUser.btnLayoutClick(Sender: TObject);
begin
  // Einstellen der Etikettenabmessungen
  EtikettenDialog.Execute ('',PageLayout);
  end;

procedure TfrmMultiUser.btnPrintClick(Sender: TObject);
var
  co  : TCourseObject;
begin
  if not Printer.Printing and SaveData (edtDescription.Text,co) then begin
    ShowData;
    if PrintDialog.Execute then begin
      GetPaperSize;
      with Printer do begin
        PrtName:=Printers[PrinterIndex];
        stbStatus.Panels[0].Text:=_(' Printer: ')+PrtName;
        Title:=co.Descriptor;
        Orientation:=poPortrait;
        BeginDoc;
        PrintInfo(co);
        EndDoc;
        end;
      end;
    end;
  end;

procedure TfrmMultiUser.udCountClick(Sender: TObject; Button: TUDBtnType);
var
  co  : TCourseObject;
begin
  if SaveData(edtDescription.Text,co) then Showdata;
  end;

procedure TfrmMultiUser.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  co  : TCourseObject;
begin
  SaveData(edtDescription.Text,co)
  end;

function TfrmMultiUser.CheckHomeDir (ADir : string) : boolean;
begin
  if length(ADir)>0 then  begin
    if cbHomeDrive.ItemIndex=0 then begin   // local path
      Result:=(Length(ADir)>=2) and (ADir[2]=DriveDelim);
      if not Result then begin
        ErrorDialog('',_('Specify a full local path!')); //'Es muss ein vollständiger lokaler Pfad angegeben werden!'
        hcbHomeDir.SetFocus;
        end;
      end
    else begin                              // UNC path
      Result:=(Length(ADir)>=2) and (ADir[1]=PathDelim) and (ADir[2]=PathDelim);
      if not Result then begin
        ErrorDialog('',_('Specify a full UNC path!')); //'Es muss ein vollständiger UNC-Pfad angegeben werden!'
        hcbHomeDir.SetFocus;
        end;
      end;
    end
  else Result:=true;
  end;

procedure TfrmMultiUser.hcbHomeDirExit(Sender: TObject);
begin
  with hcbHomeDir do if CheckHomeDir(Text) and (Length(Text)>0)
    and not AnsiContainsText(Text,phUsername)then Text:=Text+'\'+phUsername;
  end;

procedure TfrmMultiUser.cbHomeDriveCloseUp(Sender: TObject);
begin
  CheckHomeDir(hcbHomeDir.Text);
  end;

procedure TfrmMultiUser.hcbProfileExit(Sender: TObject);
begin
  with hcbProfile do
    if (Length(Text)>0) and not AnsiContainsText(Text,phUsername) then Text:=Text+'\'+phUsername;
  end;

procedure TfrmMultiUser.edtYearChange(Sender: TObject);
begin
  if rbSummer.Checked then dtpExpire.Date:=EncodeDate(udYear.Position,7,15)
  else dtpExpire.Date:=EncodeDate(udYear.Position+1,2,15);
  end;

procedure TfrmMultiUser.rbNoExpireClick(Sender: TObject);
begin
  dtpExpire.Enabled:=false;
  end;

procedure TfrmMultiUser.rbExpireClick(Sender: TObject);
begin
  dtpExpire.Enabled:=true;
  end;

procedure TfrmMultiUser.btnDelCourseClick(Sender: TObject);
var
  n : integer;
begin
  if ConfirmDialog ('',Format(_('Delete course: %s?'),[edtDescription.Text])) then //'Kurs: %s löschen?'
   with edtDescription do begin
    (Items.Objects[ItemIndex] as TCourseObject).Free;
    n:=ItemIndex;
    Items.Delete(ItemIndex);
    if n<Items.Count then ItemIndex:=n else ItemIndex:=Items.Count-1;
    ShowData;
    end;
  end;

end.
