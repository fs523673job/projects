(* Löschen von ADS-Benutzern und dem persönlichen Verzeichnis

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU General Public License Version 2 or later (the "GPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1.0   - Dez. 2007
   Vers. 2.0   - Apr. 2008  - Mehrsprachigkeit mit GnugetText
   Vers. 3.0   - Jun. 2010  - Unicode (Delphi 2009)
   Vers. 4.0   - Jul. 2010  - für Jedi-Bibl.
   Vers. 4.5   - Jan. 2016  - Anpassung an Delphi 10
   *)

unit RemUserMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ImgList, System.ImageList,
  JwaAdsHlp, JwaAdsTLB, JwaAdsErr, HListBox, Spin, Indicators,XPManUAC;

const
  Vers = ' (Vers. 4.5)';
  CopRgt = '© 2008-2016 Dr. J. Rathlev, D-24222 Schwentinental';
  EmailAdr = 'kontakt(a)rathlev-home.de';

  IniExt = 'ini';
  DefColSep = ',';
  DefQuote = '"';

  clLightRed = $BFBFFF;
  clLightGreen = $BFFFBF;

//  OuColNames : array[0..OuColCount-1] of WideString =
  OuCols = 'Name,Description,ADsPath';

//  UserColNames : array[0..UserColCount-1] of WideString =  ('Name','sAMAccountName','ADsPath','userAccountControl','mail');
  UserCols = 'Name,sAMAccountName,ADsPath,userAccountControl,mail';

type
  TPWideArray = array of PWideChar;

  TADsObject = class (TObject)
    FName,FAccount,FPath,FMail : string;
    FStatus : integer;
    constructor Create (AName,AAccount,APath,AMail: string; AStatus : integer);
    end;

  TfrmDelUser = class(TForm)
    Label6: TLabel;
    btnConnect: TBitBtn;
    btnInfo: TSpeedButton;
    gbUserProperties: TGroupBox;
    Label2: TLabel;
    edtKontoName: TEdit;
    edtDNSName: TEdit;
    Label3: TLabel;
    edtFirstName: TEdit;
    edtLastName: TEdit;
    Label5: TLabel;
    edtDescription: TEdit;
    Label7: TLabel;
    edtRoom: TEdit;
    Label8: TLabel;
    edtPhone: TEdit;
    Label4: TLabel;
    edtFullName: TEdit;
    btnExit: TSpeedButton;
    btnDelete: TBitBtn;
    cbUserData: TCheckBox;
    cbBaseDir: TComboBox;
    Label9: TLabel;
    sbDir: TSpeedButton;
    Label10: TLabel;
    gbRemUser: TGroupBox;
    lbUserDirs: TListBox;
    btnSearch: TBitBtn;
    Label13: TLabel;
    Label12: TLabel;
    lvUser: TListView;
    ImageList: TImageList;
    btToggleAccount: TBitBtn;
    lmDisabled: TLamp;
    laDisabled: TLabel;
    btObsoleteUsers: TBitBtn;
    OpenDialog: TOpenDialog;
    pnTop: TPanel;
    pnCenter: TPanel;
    pnBottom: TPanel;
    cbxOU: TComboBox;
    leMailList: TLabeledEdit;
    Label1: TLabel;
    cbDomain: TComboBox;
    btMlFile: TBitBtn;
    edLevel: TEdit;
    udLevel: TUpDown;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure sbDirClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure lvUserData(Sender: TObject; Item: TListItem);
    procedure lvUserClick(Sender: TObject);
    procedure btToggleAccountClick(Sender: TObject);
    procedure btObsoleteUsersClick(Sender: TObject);
    procedure cbxOUCloseUp(Sender: TObject);
    procedure btMlFileClick(Sender: TObject);

  private
    { Private-Deklarationen }
    ProgVersName,
    ProgVersDate,
    AppPath,UserPath,
    IniName,ProgPath,
    Context,CsvFile,
    DNSName,FDomain,
    OUPath,LastOU    : string;
    AccDisabled,
    InitOK           : boolean;
    DirLevel         : integer;
    UserList         : TStringList;
    User             : IDispatch;
    DSearch          : IDirectorySearch;
    QuoteChar,ColSep,DecSep   : char;
    FirstLine,MailCol         : integer;
    function InitDomainData (const ABind : widestring) : boolean;
    function LoadUserList (const OU : string) : boolean;
    function GetOU(const OuPath : string) : string;
    function SelectCsvFile : boolean;
    procedure SearchDirs (Base,SubDir,UserName : string);
    procedure ShowFirstUser;
    procedure ShowUserData (AIndex : integer);
    procedure RemoveUser (const ABind : string);
    procedure DeleteDirectories (const Base,Dir    : string;
                                 var DCount,FCount : integer);
  public
    { Public-Deklarationen }
  end;

var
  frmDelUser: TfrmDelUser;

implementation

{$R *.dfm}

uses System.IniFiles, Winapi.ActiveX, System.Win.ComObj, StringUtils, WinUtils,
  WinApiUtils, WinShell, ShellDirDlg, WinNet, FileUtils, StatWind, GnuGetText,
  LangUtils, CsvImportDlg, InitProg;

procedure StringToPwArray (SListe : string; var PwArr : TPWideArray; var Count : integer);
var
  s : string;
begin
  Count:=0;
  while length(SListe)>0 do begin
    s:=ReadNxtStr(SListe,Comma);
    if length(s)>0 then begin
      SetLength(PwArr,Count+1);
//      PwArr[Count]:=PWideChar(s);
      GetMem(PwArr[Count],256);
      StringToWideChar(s,PwArr[Count],256);
      inc(Count)
      end;
    end;
  end;

{ ---------------------------------------------------------------- }
constructor TADsObject.Create (AName,AAccount,APath,AMail : string; AStatus : integer);
begin
  inherited Create;
  FName:=AName; FAccount:=AAccount; FPath:=APath; FMail:=AMail;
  FStatus:=AStatus;
  end;

{ ---------------------------------------------------------------- }
const
  (* INI-Sektionen *)
  CfgSekt  = 'Config';
  DomSekt  = 'Domains';
  HomeSekt = 'Directories';
  CsvSekt = 'CsvImport';

  (* INI-Variablen *)
  iniDomain  = 'DomainName';
  iniBaseDir = 'BaseDir';
  iniDelData = 'DeleteData';
  iniLevels  = 'Levels';
  iniCsvName = 'CsvName';
  iniDecSep = 'DecSeparator';
  iniColSep = 'ColSeparator';
  iniQuote = 'QuoteCharacter';
  iniFirst = 'FirstLine';
  iniMail = 'MailColumn';
  iniOU = 'OrganizationalUnit';

procedure TfrmDelUser.FormCreate(Sender: TObject);
var
  IniFile  : TIniFile;
  s        : string;
begin
  TranslateComponent(self);
  InitOK:=succeeded(CoInitialize(nil));
  Application.Title:=_('Delete Domain User');
  InitPaths(AppPath,UserPath,ProgPath);
  InitVersion(_('Delete Domain User'),Vers,CopRgt,2,2,ProgVersName,ProgVersDate);
  Caption:=_('Delete Domain User')+' - '+VersInfo.Comments;
  IniName:=Erweiter(AppPath,PrgName,IniExt);
  if FileExists(IniName) then s:=IniName
  else s:=Erweiter(UserProfile,PrgName,IniExt); // von Vorversion
  FDomain:= '';
  IniFile:=TIniFile.Create(s);
  with IniFile do begin
    FDomain:=ReadString(CfgSekt,iniDomain,'');
    LastOU:=ReadString(CfgSekt,iniOU,'');
    udLevel.Position:=ReadInteger(CfgSekt,iniLevels,3);
    LoadHistory(IniFile,DomSekt,'Domain',cbDomain.Items);
    LoadHistory(IniFile,HomeSekt,'Dir',cbBaseDir.Items);
    cbBaseDir.Text:=ReadString(CfgSekt,iniBaseDir,'');
//    cbUserData.Checked:=ReadBool(CfgSekt,iniDeldata,false);
    CsvFile:=ReadString(CsvSekt,iniCsvName,'');
    leMailList.Text:=CsvFile;
    ColSep:=chr(ReadInteger(CsvSekt,IniColSep,ord(DefColSep)));
    QuoteChar:=chr(ReadInteger(CsvSekt,IniQuote,ord(DefQuote)));
    DecSep:=chr(ReadInteger(CsvSekt,IniDecSep,ord(FormatSettings.DecimalSeparator)));
    FirstLine:=ReadInteger(CsvSekt,IniFirst,1);
    MailCol:=ReadInteger(CsvSekt,IniMail,1);
    Free;
    end;
  UserList:=TStringList.Create;
  UserList.Sorted:=true;
  end;

procedure TfrmDelUser.FormDestroy(Sender: TObject);
var
  IniFile  : TIniFile;
begin
  IniFile:=TIniFile.Create(IniName);
  with IniFile do begin
    WriteString(CfgSekt,iniDomain,FDomain);
    WriteString(CfgSekt,iniOU,LastOU);
    WriteString(CfgSekt,iniBaseDir,cbBaseDir.Text);
    WriteInteger(CfgSekt,iniLevels,udLevel.Position);
//    WriteBool(CfgSekt,iniDeldata,cbUserData.Checked);
    WriteString(CsvSekt,iniCsvName,CsvFile);
    WriteInteger(CsvSekt,IniColSep,ord(ColSep));
    WriteInteger(CsvSekt,IniQuote,ord(QuoteChar));
    WriteInteger(CsvSekt,IniDecSep,ord(DecSep));
    WriteInteger(CsvSekt,IniFirst,FirstLine);
    WriteInteger(CsvSekt,IniMail,MailCol);
    SaveHistory(IniFile,DomSekt,'Domain',true,cbDomain.Items);
    SaveHistory(IniFile,HomeSekt,'Dir',true,cbBaseDir.Items);
    Free;
    end;
  DSearch:=nil;
  UserList.Free;
  if InitOK then CoUninitialize;
  end;

procedure TfrmDelUser.FormShow(Sender: TObject);
var
  ok : boolean;
begin
  if InitOK then begin
    with cbDomain do begin
      Color:=clLightRed;;
      Text:=FDomain;
      end;
    if (length(FDomain)=0) then begin
      repeat
        ok:=InputQuery(_('Connect to'),_('Domain:'),FDomain);
        until not ok or (length(FDomain)>0);
      end
    else ok:=true;
    if ok and InitDomainData('LDAP://'+FDomain) then ShowFirstUser
    else Close;
    end
  else ErrorDialog('',_('COM Initializing failed!')); //'COM-Initialisierung fehlgeschlagen!'
  end;

procedure TfrmDelUser.btnConnectClick(Sender: TObject);
begin
  with cbDomain do begin
    FDomain:=Text; AddToHistory(Items,Text);
    end;
  if (length(FDomain)>0) and InitDomainData('LDAP://'+FDomain) then ShowFirstUser;
  end;

procedure TfrmDelUser.ShowFirstUser;
begin
  btnConnect.Enabled:=true;
  with lvUser do if Items.Count>0 then begin
    ItemIndex:=0;
    ShowUserData(0);
    end;
  end;

{ ---------------------------------------------------------------- }
function TfrmDelUser.InitDomainData (const ABind : widestring) : boolean;
const
  szLen = 256;

var
//  bind         : widestring;
  opt          : ads_searchpref_info;
  hr           : HResult;
  ptrResult    : THandle;
  colnames     : TPWideArray;
  col          : ads_search_column;
  dwErr        : DWord;
  szErr,szName : array[0..szLen-1] of WideChar;
  sd,sn,sp     : string;
  root         : IAds;
  ColCount,i   : integer;

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
  cbDomain.Color:=clLightRed;
  Application.Processmessages;
  Screen.Cursor:=crHourglass;
  try
    ADsGetObject(PWideChar(ABind),IID_IADs,pointer(root));
    Context:=root.Get('distinguishedName');
    DNSName:=ToPrincipal(Context);
    edtDNSName.Text:='@'+DNSName;
    Caption:=FDomain+' - '+_('Delete Domain User'); //Domänen-Benutzer löschen'
    DSearch:=nil;
    ADsGetObject(PWideChar(ABind),IID_IDirectorySearch,pointer(DSearch));
    btnDelete.Enabled:=true;
    with cbDomain do begin
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
        ADsGetLastError(dwErr,@szErr[0],szLen,@szName[0],szLen);
        ShowMessage(WideCharToString(szErr)+sLineBreak+WideCharToString(szName));
        btnDelete.Enabled:=false;
        exit;
        end;
      // Organisationseinheiten suchen
      StringToPwArray(OuCols,colnames,ColCount);
      with cbxOU do begin
        Clear;
        Items.AddObject(_(' Default container'),TADsObject.Create('Default','','','',0));
        ItemIndex:=0;
        end;
      ExecuteSearch('(objectClass=organizationalUnit)',@ColNames[0],ColCount,ptrResult);
      hr:=GetNextRow(ptrResult);
      while (hr<>S_ADS_NOMORE_ROWS) do begin
        sd:=''; sd:=''; sp:='';
        if Succeeded(GetColumn(ptrResult,ColNames[1],col)) then begin
          with col do if pADsValues<>nil then sd:=pAdsvalues^.CaseExactString;
          FreeColumn(@col);
          if (length(sd)>0) and (sd[1]='-') then sd:='';
          end;
        if length(sd)>0 then begin
          if Succeeded(GetColumn(ptrResult,ColNames[0],col)) then begin
            with col do if pADsValues<>nil then sn:=pAdsvalues^.CaseIgnoreString;
            FreeColumn(@col);
            end;
          if Succeeded(GetColumn(ptrResult,ColNames[2],col)) then begin
            with col do if pADsValues<>nil then sp:=pAdsvalues^.CaseIgnoreString;;
            FreeColumn(@col);
            end;
          if length(sp)>0 then cbxOU.Items.AddObject(sd,TADsObject.Create(sn,sd,sp,'',0));
          end;
        hr:=GetNextRow(ptrResult);
        end;
      end;
    with cbxOU do begin
      for i:=0 to Items.Count-1 do
        if AnsiSameText((Items.Objects[i] as TADsObject).FPath,LastOU) then Break;
      ItemIndex:=i;
      end;
    Result:=LoadUserList (GetOU(LastOu));
  except
    ErrorDialog('',_('Error connecting to ')+FDomain); //'Fehler bei der Anmeldung an '
    Caption:=_('No connection to domain'); //'Keine Verbindung zur Domäne';
    btnDelete.Enabled:=false;
    with cbDomain do begin
      Color:=clWindow;
      Text:=FDomain;
      SetFocus;
      end;
    end;
  root:=nil; OuPath:='';
  Screen.Cursor:=crDefault;
  end;

function TfrmDelUser.LoadUserList (const OU : string) : boolean;
var
  hr           : HResult;
  ptrResult    : THandle;
  colnames     : TPWideArray;
  col          : ads_search_column;
  sd,sn,sp,sm  : string;
  ColCount,ns  : integer;
  ok           : boolean;
begin
  try
    with DSearch do begin
      // Benutzer suchen
      UserList.Clear;
      StringToPwArray(UserCols,ColNames,ColCount);
      ExecuteSearch('(&(objectCategory=person)(objectClass=user))',@ColNames[0],ColCount,ptrResult);
      hr:=GetNextRow(ptrResult);
      while (hr<>S_ADS_NOMORE_ROWS) do begin
        sd:=''; sd:=''; sp:=''; sn:=''; sm:=''; ns:=4;
        if Succeeded(GetColumn(ptrResult,ColNames[1],col)) then begin
          with col do if pADsValues<>nil then sd:=pAdsvalues^.CaseExactString;
          FreeColumn(@col);
          if (length(sd)>0) and (sd[1]='-') then sd:='';
          end;
        if length(sd)>0 then begin
          if Succeeded(GetColumn(ptrResult,ColNames[0],col)) then begin
            with col do if pADsValues<>nil then sn:=pAdsvalues^.CaseIgnoreString;
            FreeColumn(@col);
            end;
          if Succeeded(GetColumn(ptrResult,ColNames[2],col)) then begin
            with col do if pADsValues<>nil then sp:=pAdsvalues^.CaseIgnoreString;
            FreeColumn(@col);
            end;
          if Succeeded(GetColumn(ptrResult,ColNames[3],col)) then begin
            with col do if pADsValues<>nil then begin
              if (pAdsvalues^.Integer and ADS_UF_ACCOUNTDISABLE)<>0 then ns:=1
              else ns:=0;
              end;
            FreeColumn(@col);
            end;
          if Succeeded(GetColumn(ptrResult,ColNames[4],col)) then begin
            with col do if pADsValues<>nil then sm:=pAdsvalues^.CaseIgnoreString;
            FreeColumn(@col);
            end;
          if length(OU)=0 then ok:=TextPos('ou',sp)=0
          else ok:=TextPos(OU,sp)>0;
          if (length(sn)>0) and ok then begin
            UserList.AddObject(sn,TADsObject.Create(sn,sd,sp,sm,ns));
            end;
          end;
        hr:=GetNextRow(ptrResult);
        end;
      lvUser.Items.Count:=UserList.Count;
      Result:=true;
      end;
  except
    Result:=false;
    end;
  end;

function TfrmDelUser.GetOU(const OuPath : string) : string;
var
  n : integer;
begin
  Result:=OuPath;
  if length(Result)>0 then begin
    n:=TextPos(FDomain,Result);
    n:=TextPosEx('OU=',Result,n);
    Delete(Result,1,n-1);
    end;
  end;

procedure TfrmDelUser.cbxOUCloseUp(Sender: TObject);
begin
  with cbxOU do if ItemIndex>=0 then with (Items.Objects[ItemIndex] as TADsObject) do begin
    LastOu:=FPath;
    LoadUserList(GetOu(FPath));
    end;
  with lvUser do if Items.Count>0 then begin
    Invalidate;
    ItemIndex:=0;
    ShowUserData(0);
    end;
  end;

procedure TfrmDelUser.btnInfoClick(Sender: TObject);
begin
  InfoDialog('',Application.Title+Vers+' - '+ProgVersDate+sLineBreak+
             CopRgt+sLineBreak+EmailAdr);
  end;

procedure TfrmDelUser.btnExitClick(Sender: TObject);
begin
  Close;
  end;

procedure TfrmDelUser.ShowUserData (AIndex : integer);
var
  ap    : string;
  ADsOU : IADsOU;
begin
  if (AIndex>=0) and (AIndex<UserList.Count) then begin
    cbUserData.Checked:=false;
    lbUserDirs.Clear;
    ap:=(UserList.Objects[AIndex] as TADsObject).FPath;
    if Failed(ADsGetObject(PWideChar(ap),IID_IADsUser,pointer(user))) then Exit;
    with User as IADsUser  do begin
      GetInfo;
      try
        edtFullName.Text:=FullName;
      except
        edtFullName.Text:='';
        end;
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
        AccDisabled:=AccountDisabled;
        with btToggleAccount do begin
          Enabled:=true;
          Glyph:=nil;
          if AccDisabled then begin
            Caption:=_('Enable account');
            ImageList.GetBitmap(0,Glyph);
            end
          else begin
            Caption:=_('Disable account');
            ImageList.GetBitmap(1,Glyph);
            end;
          end;
      except
        AccDisabled:=false;
        btToggleAccount.Enabled:=false;
        end;
      lmDisabled.Visible:=AccDisabled;
      laDisabled.Visible:=AccDisabled;
      try
        ADsGetObject(PWideChar(Parent),IID_IADsOU,pointer(ADsOU));
        OUPath:=ADsOU.AdsPath;
      except
        OuPath:='';
        end;
      ADsOU:=nil;
      end;
    end;
  end;

procedure TfrmDelUser.btToggleAccountClick(Sender: TObject);
begin
  with User as IADsUser do begin
    AccountDisabled:=not AccDisabled;
    try
      SetInfo;       // Speichern
      with (UserList.Objects[lvUser.ItemIndex] as TADsObject) do begin
        if AccountDisabled then FStatus:=FStatus or 1 else FStatus:=FStatus and $FE;
        end;
      with lvUser do begin
        Invalidate;
        ShowUserData(ItemIndex);
        end;
    except
      on E:EOleException do
        ErrorDialog('','ADS-Error: '+IntToHex(E.ErrorCode,8)+sLineBreak+E.Message);
      end;
    end;
  end;

procedure TfrmDelUser.lvUserClick(Sender: TObject);
begin
  ShowUserData(lvUser.ItemIndex);
  end;

procedure TfrmDelUser.lvUserData(Sender: TObject; Item: TListItem);
begin
  with Item do begin
    Caption:=UserList[Index];
    ImageIndex:=(UserList.Objects[Index] as TADsObject).FStatus;
    end;
  end;

procedure TfrmDelUser.sbDirClick(Sender: TObject);
var
  s : string;
begin
  s:=cbBaseDir.Text;
  if ShellDirDialog.Execute (_('Root Directory for Users'),true,true,'',s) then begin //'Basisverzeichnis für Benutzer'
    with cbBaseDir do begin
      Text:=s; AddToHistory(Items,s);
      end;
    end;
  end;

{ ------------------------------------------------------------------- }
procedure TfrmDelUser.SearchDirs (Base,SubDir,UserName : string);
var
  DirInfo    : TSearchRec;
  Findresult : integer;
  sd         : string;
begin
  inc(DirLevel);
  if (length(SubDir)=0) then sd:=Base
  else if (SubDir[1]='\') then  sd:=Base+SubDir
  else sd:=IncludeTrailingPathDelimiter(Base)+SubDir;
  FindResult:=FindFirst(IncludeTrailingPathDelimiter(sd)+'*.*',faDirectory+faReadOnly+faHidden+faSysfile,DirInfo);
  while (FindResult=0) do with DirInfo do begin
    if NotSpecialDir(Name) then begin
      StatusWindow.Status:=IncludeTrailingPathDelimiter(SubDir)+DirInfo.Name;
      Application.ProcessMessages;
      if SameFileName(DirInfo.Name,UserName) then
        lbUserDirs.Items.Add(IncludeTrailingPathDelimiter(sd)+DirInfo.Name)
      else if DirLevel<udLevel.Position then
        SearchDirs(Base,IncludeTrailingPathDelimiter(SubDir)+DirInfo.Name,UserName);
      end;
    if StatusWindow.Stopped then begin
      FindClose(DirInfo);
      Exit;
      end;
    FindResult:=FindNext(DirInfo);
    end;
  FindClose(DirInfo);
  dec(DirLevel);
  end;

{ ------------------------------------------------------------------- }
(* Lösche ein Verzeichnis einschließlich aller Unterverzeichnisse und Dateien *)
procedure TfrmDelUser.DeleteDirectories (const Base,Dir    : string;
                                         var DCount,FCount : integer);
var
  DirInfo    : TSearchRec;
  fc,dc,
  Findresult : integer;
  s,sd       : string;
begin
  if length(Dir)>0 then sd:=IncludeTrailingPathDelimiter(Base)+Dir else sd:=Base;
  if DirectoryExists(sd) then begin
    StatusWindow.Status:=sd;
    FindResult:=FindFirst (Erweiter(sd,'*','*'),faAnyFile,DirInfo);
    while (FindResult=0) and not StatusWindow.Stopped do with DirInfo do begin
      if NotSpecialDir(Name) and ((Attr and faDirectory)<>0) then
        DeleteDirectories(Base,Erweiter(Dir,DirInfo.Name,''),DCount,FCount);
      FindResult:=FindNext (DirInfo);
      end;
    FindClose(DirInfo);
    if not StatusWindow.Stopped then begin
      fc:=0; dc:=0;
      FindResult:=FindFirst (Erweiter(sd,'*','*'),faArchive+faReadOnly+faHidden+faSysfile,DirInfo);
      while FindResult=0 do with DirInfo do begin
        if NotSpecialDir(Name) then begin
          inc(fc);
          (* Dateien löschen *)
          s:=SetDirName(sd)+Name;
          StatusWindow.Status:=s;
          Application.ProcessMessages;
          // immer löschen
          FileSetAttr(s,faArchive);
          if DeleteFile(s) then begin
            inc(FCount); inc(dc);
            end;
          if StatusWindow.Stopped then begin
            FindClose(DirInfo);
            Exit;
            end;
          end;
        FindResult:=FindNext (DirInfo);
        end;
      FindClose(DirInfo);
      if (fc=dc) then begin   // Verzeichnis leer ==> löschen
        FileSetAttr(sd,0);    // Attribute zum Löschen entfernen
        if RemoveDir(sd) then inc(DCount);
        end;
      end;
    end;
  end;

{ ------------------------------------------------------------------- }
procedure TfrmDelUser.btnSearchClick(Sender: TObject);
begin
  if DirectoryExists(cbBaseDir.Text) then begin
    DirLevel:=0;
    StatusWindow.ShowStatus(BottomLeftPos(btnSearch,Point(0,10)),_('Searching for user data'),'',true,1); //'Benutzerdaten werden gesucht'
    SearchDirs(cbBaseDir.Text,'',edtKontoName.Text);
    StatusWindow.Close;
    if lbUserDirs.Items.Count=0 then
      ErrorDialog('',_('No user data found!')) //'Keine Benutzerdaten gefunden!'
    else cbUserData.Checked:=true;
    end
  else ErrorDialog('',_('Directory not found!')+sLineBreak+cbBaseDir.Text); //'Verzeichnis nicht gefunden:'
  end;

procedure TfrmDelUser.btMlFileClick(Sender: TObject);
begin
  SelectCsvFile;
  end;

function TfrmDelUser.SelectCsvFile : boolean;
begin
  with OpenDialog do begin
    if length(CsvFile)>0 then InitialDir:=ExtractFilePath(CsvFile)
    else InitialDir:=UserPath;
    Filename:=ExtractFilename(CsvFile);
    Title:='Select CSV file';
    if Execute and CsvImportDialog.Execute(Filename,ColSep,QuoteChar,DecSep,FirstLine,MailCol) then begin
      CsvFile:=Filename;
      leMailList.Text:=CsvFile;
      Result:=true;
      end
    else Result:=false;
    end;
  end;

procedure TfrmDelUser.btObsoleteUsersClick(Sender: TObject);
var
  fi      : TextFile;
  s,sm    : string;
  i,n     : integer;
  ml      : TStringList;
begin
  if CheckPath(CsvFile)=ptNotAvailable then // ev. nicht verbundener Netzwerkpfad?
    ReconnectPath(ExtractFilePath(CsvFile));
  if FileExists(CsvFile) or SelectCsvFile then begin
    ml:=TStringList.Create;  // alphabet. Liste für Mailadressen
    ml.Sorted:=true;
    with UserList do for i:=0 to Count-1 do with (Objects[i] as TAdsObject) do
      if length(Trim(FMail))>0 then ml.AddObject(FMail,pointer(i+1));
    AssignFile(fi,CsvFile); reset(fi);
    while not Eof(fi) do begin
      readln(fi,s);
      for i:=1 to MailCol do begin
        if QuoteChar=#0 then sm:=ReadNxtStr(s,ColSep)
        else sm:=ReadNxtQuotedStr(s,ColSep,QuoteChar);
        end;
      n:=ml.IndexOf(sm);
      if n>=0 then ml.Objects[n]:=nil;
      end;
    CloseFile(fi);
    with ml do for i:=0 to Count-1 do if integer(Objects[i])>0 then begin  //Mailadresse nicht in Csv
      with (UserList.Objects[integer(Objects[i])-1] as TADsObject) do FStatus:=FStatus or 2;
      end;
    ml.Free;
    lvUser.Invalidate;
    end;
  end;

{ ------------------------------------------------------------------- }
procedure TfrmDelUser.btnDeleteClick(Sender: TObject);
var
  s,bind    : string;
begin
  with lbUserDirs do if cbUserData.Checked and (SelCount=0) then SelectAll;
  if cbUserData.Checked then
    s:=Format(_('Remove user %s from ADS'+sLineBreak+
                'and delete selected user data in'+sLineBreak+'%s?'),
                [edtFullName.Text,cbBaseDir.Text])
//  'Benutzer %s aus ADS entfernen'+sLineBreak+'und ausgewählte Benutzerdaten auf'+sLineBreak+'%s löschen?'
  else s:=Format(_('Remove user %s from ADS'),[edtFullName.Text]); //'Benutzer %s aus ADS entfernen?';
  if ConfirmDialog(Caption,s,BottomLeftPos(btnSearch,Point(0,-150))) then begin
    if length(OUPath)>0 then bind:=OuPath
    else bind:='LDAP://'+FDomain+'/CN=Users,'+Context;
    RemoveUser(bind);
    end;
  end;

procedure TfrmDelUser.RemoveUser (const ABind : string);
var
  s          : string;
  dc,fc,i,n  : integer;
  ADsCont    : IADsContainer;
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
  ADsGetObject(PWideChar(ABind),IID_IADsContainer,pointer(ADsCont));
  try
    ADsCont.Delete('user','CN='+ReplaceSpecChar(edtFullName.Text));
    s:=Format(_('User: %s was removed from ADS!'),[edtFullName.Text]); //'Der Benutzer: %s wurde aus dem ADS entfernt!';
    if cbUserData.Checked then begin
      StatusWindow.ShowStatus(BottomLeftPos(btnSearch,Point(0,-150)),_('Deleting user data'),'',true,5);
      dc:=0; fc:=0;
      s:=s+sLineBreak+_('Following user data were deleted:'); //'Folgende Benutzerdaten wurden gelöscht:'
      with lbUserDirs do for i:=0 to Items.Count-1 do if Selected[i] then begin
        StatusWindow.Status:=Items[i];
        Application.ProcessMessages;
        DeleteDirectories(Items[i],'',dc,fc);
        s:=s+sLineBreak+_('  Directory: ')+Items[i]; //'  Verzeichnis: '
        end;
      StatusWindow.Close;
      dec(dc);
      if dc>0 then begin
        s:=s+sLineBreak+IntToStr(dc);
        if dc=1 then s:=s+_(' Subdirectory') //' Unterverzeichnis'
        else s:=s+_(' Subdirectories') //' Unterverzeichnisse';
        end
      else s:=s+sLineBreak+_('No subdirectories found'); //'keine Unterverzeichnisse gefunden'
      if fc>0 then begin
        s:=s+sLineBreak+IntToStr(fc);
        if fc=1 then s:=s+_(' file') //' Datei'
        else s:=s+_(' files'); //' Dateien';
        end;
      end ;
    InfoDialog (Caption,s);
    n:=lvUser.ItemIndex;
    with UserList do begin
      Delete(n);
      if n>=Count then n:=Count-1;
      end;
    with lvUser do begin
      Items.Count:=UserList.Count;
      ItemIndex:=n;
      ShowUserData(n);
      Invalidate;
      end;
  except
     ErrorDialog('',Format(_('User: %s '+sLineBreak+'could not be removed from ADS!'),
                           [edtFullName.Text]));
//       'Der Benutzer: %s'+sLineBreak+'konnte nicht aus dem ADS entfernt werden!';
     end;
  end;

end.
