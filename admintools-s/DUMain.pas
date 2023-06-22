(* Erzeugen von ADS-Benutzern

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU General Public License Version 2 or later (the "GPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1.2.3 - Juni 2005
   Vers. 2.0   - Apr. 2008  - Mehrsprachigkeit mit GnuGetText
   Vers. 3.0   - Jun. 2010  - Unicode (Delphi 2009)
   Vers. 4.0   - Jul. 2010  - für Jedi-Bibl.
   Vers. 4.5   - Jan. 2016  - Anpassung an Delphi 10
   *)

unit DUMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.ComCtrls, System.IniFiles, System.StrUtils, System.Win.ComObj,
  JwaAdsHlp, JwaAdsTLB, JwaAdsErr, WinUtils,
  StringUtils, FileUtils, WinApiUtils, WinShell, HListBox, XPManUac;

const
  Vers = ' (Vers. 4.5)';
  CopRgt = '© 2008-2016 Dr. J. Rathlev, D-24222 Schwentinental';
  EmailAdr = 'kontakt(a)rathlev-home.de';

  IniExt = 'ini';
  phUsername = '%username%';

  MinLen = 6;
  
  clLightRed = $BFBFFF;
  clLightGreen = $BFFFBF;

  ColCount = 3;
  ColNames : array[0..ColCount-1] of WideString = ('Name','Description','ADsPath');

type
  TADsObject = class (TObject)
    FName,FDesc,FPath : string;
    constructor Create (AName,ADesc,APath : string);
    end;

  TfrmDomUser = class(TForm)
    Label6: TLabel;
    btnConnect: TBitBtn;
    btnMakeUser: TBitBtn;
    btnExit: TSpeedButton;
    btnInfo: TSpeedButton;
    gbxUser: TGroupBox;
    edtDNSName: TEdit;
    edtKontoName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtFirstName: TEdit;
    Label3: TLabel;
    edtLastName: TEdit;
    edtFullName: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    edtDescription: TEdit;
    Label7: TLabel;
    edtRoom: TEdit;
    Label8: TLabel;
    edtPhone: TEdit;
    chbDisabled: TCheckBox;
    btnNew: TBitBtn;
    btnPref: TSpeedButton;
    gbxOU: TGroupBox;
    rbtGlobal: TRadioButton;
    rbtOU: TRadioButton;
    cbxOU: TComboBox;
    gbxAg: TGroupBox;
    lbxAg: TListBox;
    hcbDomain: THistoryCombo;
    btnPwd: TBitBtn;
    gbxProfile: TGroupBox;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    hcbHomeDir: THistoryCombo;
    hcbProfile: THistoryCombo;
    cbHomeDrive: TComboBox;
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
    procedure edtNameChange(Sender: TObject);
    procedure cbxOUChange(Sender: TObject);
    procedure chbDisabledClick(Sender: TObject);
    procedure btnPwdClick(Sender: TObject);
    procedure btnPrefClick(Sender: TObject);
    procedure hcbProfileExit(Sender: TObject);
    procedure hcbHomeDirExit(Sender: TObject);
  private
    { Private-Deklarationen }
    ProgVersName,
    ProgVersDate,
    AppPath,UserPath,
    IniName,ProgPath,
    DNSName,FDomain,
    DefPwd,Context,
    Desc,
    Room,Phone,
    Profile,HomeDir  : string;
    Pwd              : AnsiString;
    HomeDrv          : integer;
    NoExpire,InitOK  : boolean;
    function InitDomainData : boolean;
    function CheckHomeDir : boolean;
    function ReplacePlaceholder(ps : string) : string;
    function ReplaceUsername (ps : string) : string;
    function GetRoomBase (s : string) : string;
    function SearchUser (CommonName : string) : string;
    procedure ShowExceptionError (E : EOleException);
  public
    { Public-Deklarationen }
  end;

var
  frmDomUser   : TfrmDomUser;

implementation

uses Winapi.ActiveX, System.DateUtils, GnuGetText, LangUtils, InitProg, DuPrefs,
  NewPwdDlg, TsUserEx;

{$R *.dfm}

{ ---------------------------------------------------------------- }
constructor TADsObject.Create (AName,ADesc,Apath : string);
begin
  inherited Create;
  FName:=AName; FDesc:=ADesc; FPath:=APath;
  end;

{ ---------------------------------------------------------------- }
const
  (* INI-Sektionen *)
  CfgSekt  = 'Config';
  DomSekt  = 'Domains';
  HomeSekt = 'Directories';
  ProfSekt = 'Profiles';

  (* INI-Variablen *)
  iniDomain  = 'DomainName';
  iniDesc    = 'Description';
  iniRoom    = 'Room';
  iniPhone   = 'Phone';
  iniPwd     = 'Password';
  iniNoExp   = 'NoExpire';
  iniProfile = 'ProfilePath';
  iniHomeDir = 'HomeDirectory';
  iniHomeDrv = 'HomeDrive';

procedure TfrmDomUser.FormCreate(Sender: TObject);
var
  IniFile  : TIniFile;
  s        : string;
begin
  TranslateComponent(self);
  InitOK:=succeeded(CoInitialize(nil));
  Application.Title:=_('Create Domain User');
  InitPaths(AppPath,UserPath,ProgPath);
  InitVersion(_('Create Domain User'),Vers,CopRgt,2,2,ProgVersName,ProgVersDate);
  Caption:=_('Create Domain User')+' - '+VersInfo.Comments;
  IniName:=Erweiter(AppPath,PrgName,IniExt);
  if FileExists(IniName) then s:=IniName
  else s:=Erweiter(UserProfile,PrgName,IniExt); // von Vorversion
  FDomain := '';
  IniFile:=TIniFile.Create(s);
  with IniFile do begin
    FDomain:=ReadString(CfgSekt,iniDomain,'');
    Desc:=ReadString(CfgSekt,iniDesc,_('Domain user')); //'Domänen-Benutzer'
    Room:=ReadString(CfgSekt,iniRoom,'');
    Phone:=ReadString(CfgSekt,iniPhone,'');
    DefPwd:=ReadString(CfgSekt,iniPwd,'_#_');
    NoExpire:=ReadBool(CfgSekt,iniNoExp,false);
    Profile:=ReadString(CfgSekt,iniProfile,'');
    HomeDir:=ReadString(CfgSekt,iniHomeDir,'');
    HomeDrv:=ReadInteger(CfgSekt,iniHomeDrv,0);
    Free;
    end;
  hcbDomain.LoadFromIni(IniName,DomSekt);
  hcbHomeDir.LoadFromIni(IniName,HomeSekt);
  hcbProfile.LoadFromIni(IniName,ProfSekt);
  Pwd:=''; btnPwd.Font.Color:=clRed;
  end;

function TfrmDomUser.GetRoomBase (s : string) : string;
var
  n        : integer;
begin
    n:=Pos('/',edtRoom.Text);
    if n>0 then Result:=AnsiLeftStr(edtRoom.Text,n)
    else Result:=s;
    end;

procedure TfrmDomUser.FormDestroy(Sender: TObject);
var
  IniFile  : TIniFile;
begin
  IniFile:=TIniFile.Create(IniName);
  with IniFile do begin
    WriteString(CfgSekt,iniDomain,FDomain);
    WriteString(CfgSekt,IniDesc,Desc);
    WriteString(CfgSekt,IniRoom,GetRoomBase(edtRoom.Text));
    WriteString(CfgSekt,iniPhone,Phone);
    WriteString(CfgSekt,IniPwd,DefPwd);
    WriteBool(CfgSekt,iniNoExp,NoExpire);
    WriteString(CfgSekt,iniProfile,Profile);
    WriteString(CfgSekt,iniHomeDir,HomeDir);
    WriteInteger(CfgSekt,iniHomeDrv,HomeDrv);
    Free;
    end;
  if InitOK then CoUninitialize;
  end;

procedure TfrmDomUser.FormShow(Sender: TObject);
begin
  if InitOK then begin
    PrefDialog.LoadFromIni(Ininame);
    if length(FDomain)>0 then InitDomainData;
    edtDescription.Text:=Desc+' ('+FormatDateTime('yyyy-mm-dd',ToDay)+')';
    edtRoom.Text:=Room;
    edtPhone.Text:=Phone;
    hcbProfile.Text:=Profile;
    hcbHomeDir.Text:=HomeDir;
    cbHomeDrive.ItemIndex:=HomeDrv;
    end
  else ErrorDialog('',_('COM Initializing failed!')); //'COM-Initialisierung fehlgeschlagen!'
  end;

{ ---------------------------------------------------------------- }
function TfrmDomUser.InitDomainData : boolean;
const
  szLen = 256;

var
  bind         : widestring;
  opt          : ads_searchpref_info;
  hr           : HResult;
  ptrResult    : THandle;
  col          : ads_search_column;
  dwErr        : DWord;
  szErr,szName : array[0..szLen-1] of WideChar;
  sd,sn,sp     : string;
  root         : IADs;
  DSearch      : IDirectorySearch;

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
    ADsGetObject(PWideChar(Bind),IID_IADs,pointer(root));
    Context:=root.Get('distinguishedName');
    DNSName:=ToPrincipal(Context);
    edtDNSName.Text:='@'+DNSName;
    Caption:=FDomain+_(' - Add New User to Domain'); //' - Neuen Domänen-Benutzer eintragen';
    ADsGetObject(PWideChar(bind),IID_IDirectorySearch,pointer(DSearch));
    btnMakeUser.Enabled:=true;
    with hcbDomain do begin
      Color:=clLightGreen;
      Text:=FDomain;
      end;
    edtKontoName.Text:='';
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
        if Succeeded(GetColumn(ptrResult,PWideChar(ColNames[1]),col)) then begin
          with col do if pADsValues<>nil then sd:=pAdsvalues^.CaseExactString;
          FreeColumn(@col);
          if (length(sd)>0) and (sd[1]='-') then sd:='';
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
procedure TfrmDomUser.btnConnectClick(Sender: TObject);
begin
  with hcbDomain do begin
    FDomain:=Text;
    AddItem(Text);
    end;
  if length(FDomain)>0 then InitDomainData;
  end;

procedure TfrmDomUser.btnExitClick(Sender: TObject);
begin
  Close;
  end;

procedure TfrmDomUser.btnInfoClick(Sender: TObject);
begin
  InfoDialog('',Application.Title+Vers+' - '+ProgVersDate+sLineBreak+
             CopRgt+sLineBreak+EmailAdr);
  end;

procedure TfrmDomUser.btnNewClick(Sender: TObject);
begin
  edtFirstName.Text:='';
  edtLastName.Text:='';
  edtFullName.Text:='';
  edtDescription.Text:=Desc+' ('+FormatDateTime('yyyy-mm-dd',ToDay)+')';
  edtRoom.Text:=Room;
  edtPhone.Text:=Phone;
  chbDisabled.Checked:=true;
  hcbProfile.Text:=Profile;
  hcbHomeDir.Text:=HomeDir;
  cbHomeDrive.ItemIndex:=HomeDrv;
  Pwd:=''; btnPwd.Font.Color:=clRed;
  rbtGlobal.Checked:=true;
  cbxOU.ItemIndex:=-1;
  lbxAg.DeleteSelected;
  with edtKontoname do begin
    Text:='';
    SetFocus;
    end;
  btnMakeUser.Enabled:=true;
  end;

// Domänen-Benutzer suchen, Rückgabewert: ADSPath
function TfrmDomUser.SearchUser (CommonName : string) : string;
var
  DSearch      : IDirectorySearch;
  opt          : ads_searchpref_info;
  ptrResult    : THandle;
  col          : ads_search_column;
begin
  Result:='';
  ADsGetObject(PWideChar('LDAP://'+FDomain),IID_IDirectorySearch,pointer(DSearch));
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

{ ------------------------------------------------------------------- }
(* Platzhalter im String ersetzen *)
function TfrmDomUser.ReplacePlaceholder (ps : string) : string;
begin
  if AnsiContainsText(ps,phUserName) then
    Result:=AnsiReplaceText(ps,phUserName,edtKontoName.Text)
  else Result:=ps;
  end;

function TfrmDomUser.ReplaceUsername (ps : string) : string;
begin
  if AnsiContainsText(ps,edtKontoName.Text) then
    Result:=AnsiReplaceText(ps,edtKontoName.Text,phUserName)
  else Result:=ps;
  end;

procedure TfrmDomUser.ShowExceptionError (E : EOleException);
begin
  ErrorDialog('','ADS-Error: '+IntToHex(E.ErrorCode,8)+sLineBreak+E.Message);
  end;

{ ------------------------------------------------------------------- }
procedure TfrmDomUser.btnMakeUserClick(Sender: TObject);
var
  ADsCont  : IADsContainer;
  ADsOU    : IADsOU;
  User     : IDispatch;
  Grp      : IAdsGroup;
  TSUser   : IADsTSUserEx;          // Terminal services usesr
  bind     : widestring;
  ap,s,sh,
  sp,sd    : string;
  i,n      : integer;

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

   function InsUser (Container,User : string) : widestring;
   var
     n : integer;
   begin
     n:=Pos('//',Container);
     n:=PosEx('/',Container,n+2);
     Insert (User+',',Container,n+1);
     Result:=Container;
     end;

begin
  if CheckHomeDir and (length(edtKontoName.Text)>0) then begin
    if rbtGlobal.Checked then bind:='LDAP://'+FDomain+'/CN=Users,'+Context
    else with cbxOU do bind:=(Items.Objects[ItemIndex] as TAdsObject).FPath;
  // prüfen, ob der Benutzer bereits eingetragen ist
    ap:=SearchUser (edtKontoName.text);
    if length(ap)>0 then begin
      ADsGetObject(PWideChar(ap),IID_IADsUser,pointer(user));
      with User as IADsUser  do begin
        GetInfo;
        try
          edtFullName.Text:=FullName;
        except
          edtFullName.Text:='';
          end;
        ErrorDialog('',Format(_('User %s (%s%s)'+sLineBreak+'is already registered!'),
                     [edtFullname.Text,edtKontoName.Text,edtDNSName.Text]));
//        'Der Benutzer %s (%s@%s)'+sLineBreak+'ist bereits eingetragen!'
        try
          edtFirstName.Text:=FirstName;
        except
          edtFirstName.Text:='';
          end;
        try
          edtLastName.Text:=LastName;
        except
          edtLastName.Text:='';
          end;
        try
          edtDescription.Text:=Description;
        except
          edtDescription.Text:='';
          end;
        try
          edtKontoName.Text:=Get('sAMAccountName');
        except
          edtKontoName.Text:='';
          end;
        try
          edtRoom.Text:=OfficeLocations;
        except
          edtRoom.Text:='';
          end;
        try
          edtPhone.Text:=TelephoneNumber;
        except
          edtPhone.Text:='';
          end;
        try
          chbDisabled.Checked:=AccountDisabled;
        except
          chbDisabled.State:=cbGrayed;
          end;
        try
          s:=Profile;
          with hcbProfile do begin
            Text:=s;
            AddItem(ReplaceUsername(s));
            end;
        except
          end;
        try
          s:=HomeDirectory;
          with hcbHomeDir do begin
            Text:=s;
            AddItem(ReplaceUsername(s));
            end;
        except
          end;
        try
          s:=Get('homeDrive');
          with cbHomeDrive do begin
            n:=Items.IndexOf(s);
            if n<0 then n:=0;
            ItemIndex:=n;
            end;
        except
          end;
        try
          ADsGetObject(PWideChar(Parent),IID_IADsOU,pointer(ADsOU));
          rbtOU.Checked:=true;
          with cbxOU do ItemIndex:=Items.IndexOf(ADsOU.Description);
        except
          rbtGlobal.Checked:=true;
          cbxOU.ItemIndex:=-1;
          end;
        ADsOU:=nil;
        end;
      btnMakeUser.Enabled:=false;
      user:=nil;
      end
    else if ConfirmDialog ('',Format(_('Add new user: %s%s'+sLineBreak+'to: %s'),
                          [edtKontoName.Text,edtDNSName.Text,bind])) then begin // noch nicht vorhanden
//    'Neuen Benutzer: %s%s'+sLineBreak+'unter: %s anlegen?')
      ADsGetObject(PWideChar(bind),IID_IADsContainer,pointer(ADsCont));
      User:=ADsCont.Create('user','CN='+ReplaceSpecChar(edtFullname.Text));
      with User as IADsUser do begin
    // set Mandatory attributes
        Put('sAMAccountName',edtKontoName.Text);
        sh:=ReplacePlaceholder(hcbHomeDir.Text);
        sp:=ReplacePlaceholder(hcbProfile.Text);
        with cbHomeDrive do if ItemIndex>0 then sd:=Items[ItemIndex] else sd:='';
    // set Optional attributes
        FullName:=edtFullName.Text;
        if length(edtFirstName.Text)>0 then FirstName:=edtFirstName.Text;
        LastName:=edtLastName.Text;
        Description:=edtDescription.Text;
        if length(sh)>0 then HomeDirectory:=sh;
        if length(sd)>0 then Put('homeDrive',sd);
        if length(sp)>0 then Profile:=sp;
        Put('userPrincipalName',edtKontoname.Text+'@'+DNSName);
        try
          SetInfo;       // Speichern
        except
          on E:EOleException do ShowExceptionError(E);
          end;

        // Terminal services - requires at least Windows Server 2008
        // https://msdn.microsoft.com/en-us/library/aa380823(v=vs.85).aspx
        if ((length(sh)>0) or (length(sp)>0)) and
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
        if length(Pwd)=0 then begin
          if not chbDisabled.Checked then begin  // Passwort
            Put('pwdLastSet',0);   // ändern bei nächster Anmeldung
            s:=StringReplace(DefPwd,'#',edtKontoname.Text,[]);
            try
              SetPassword (s);
            except
              on E:EOleException do ShowExceptionError(E);
              end;
            end;
          Put('userAccountControl',0);
          end
        else begin
          try
            SetPassword (Pwd);
          except
            on E:EOleException do ShowExceptionError(E);
            end;
          if NoExpire then
            Put('userAccountControl',ADS_UF_DONT_EXPIRE_PASSWD)
          else Put('userAccountControl',0);
          end;
        AccountDisabled:=chbDisabled.Checked;
        if length(edtRoom.Text)>0 then OfficeLocations:=edtRoom.Text;
        if length(edtPhone.Text)>0 then TelephoneNumber:=edtPhone.Text;
        EmailAddress:=edtKontoname.Text+'@'+DNSName;
        try
          SetInfo;       // Speichern
        except
          on E:EOleException do ShowExceptionError(E);
          end;
        end;
    // Arbeitsgruppen zuordnen
      with lbxAG do if SelCount>0 then for i:=0 to Items.Count-1 do if Selected[i] then begin
        if succeeded(ADsGetObject(PWideChar((Items.Objects[i] as TADsObject).FPath),IID_IADsGroup,pointer(Grp))) then
          Grp.Add((user as IAdsUser).AdsPath);
        end;
      Grp:=nil;
      Room:=GetRoomBase(edtRoom.Text);
      InfoDialog('',Format(_('A new user was added to %s:'+sLineBreak+'%s (%s%s)'),
                        [FDomain,edtFullname.Text,edtKontoname.Text,edtDNSName.Text]));
//      'In %s wurde ein neuer Benutzer angelegt:'+sLineBreak+'%s(%s%s)'
      user:=nil;
      end;
    ADsCont:=nil;
    Pwd:=''; btnPwd.Font.Color:=clRed;
    end
  else ErrorDialog('',_('Please specify an account name!')); //'Geben Sie bitte einen Anmeldenamen an!'
  end;

procedure TfrmDomUser.edtNameChange(Sender: TObject);
begin
  if length(edtFirstName.Text)>0 then
    edtFullname.Text:=edtLastName.Text+', '+edtFirstName.Text
  else edtFullname.Text:=edtLastName.Text;
  end;

procedure TfrmDomUser.cbxOUChange(Sender: TObject);
begin
  with cbxOU do if ItemIndex>=0 then with (Items.Objects[ItemIndex] as TADsObject) do
    edtDescription.Text:=Desc+' ('+FormatDateTime('yyyy-mm-dd',ToDay)+')'+' - '+FName;
  end;

procedure TfrmDomUser.rbtGlobalClick(Sender: TObject);
begin
  cbxOU.Enabled:=false;
  edtDescription.Text:=Desc+' ('+FormatDateTime('yyyy-mm-dd',ToDay)+')';
  end;

procedure TfrmDomUser.rbtOUClick(Sender: TObject);
begin
  cbxOU.Enabled:=true;
  end;

procedure TfrmDomUser.chbDisabledClick(Sender: TObject);
begin
  if length(edtKontoName.Text)=0 then chbDisabled.Checked:=true;
  end;

procedure TfrmDomUser.btnPwdClick(Sender: TObject);
begin
  if NewPwdDialog.Execute (CenterPos,_('Specify a password'),Pwd,MinLen,true,true) then  //'Passwort eingeben'
    btnPwd.Font.Color:=clGreen;
  end;

procedure TfrmDomUser.btnPrefClick(Sender: TObject);
begin
  if PrefDialog.Execute (hcbProfile.Items,hcbHomeDir.Items,
                         Desc,Room,Phone,DefPwd,Profile,HomeDir,HomeDrv,NoExpire) then begin
    if length(edtDescription.Text)>0 then cbxOUChange(Sender)
    else edtDescription.Text:=Desc+' ('+FormatDateTime('yyyy-mm-dd',ToDay)+')';
    end;
  end;

function TfrmDomUser.CheckHomeDir : boolean;
var
  ADir : string;
begin
  ADir:=hcbHomeDir.Text;
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

procedure TfrmDomUser.hcbProfileExit(Sender: TObject);
begin
  with hcbProfile do
    if (Length(Text)>0) and not AnsiContainsText(Text,phUsername) then Text:=Text+'\'+phUsername;
  end;

procedure TfrmDomUser.hcbHomeDirExit(Sender: TObject);
begin
  with hcbHomeDir do if (Length(Text)>0)
    and not AnsiContainsText(Text,phUsername)then Text:=Text+'\'+phUsername;
  end;

end.
