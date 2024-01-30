(* Delphi Unit
   verschiedene Unterroutinen für Windows Netzwerkverbindungen
   ===========================================================
   
   © Dr. J. Rathlev, D-24222 Schwentinental (info(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1 - Sep. 2002
     letzte Änderung: Jun. 2015
   *)

unit WinNet;

interface

uses
  WinApi.Windows, System.Classes, System.SysUtils, Vcl.Controls;

type
  TUserAccount = record
    Username,Password : string;
    end;

  TDriveType = (dtUnknown,dtNoRoot,dtRemovable,dtFixed,dtRemote,dtCdRom,dtRamDisk);
  TDriveTypes = set of TDriveType;
  TPathType =  (ptNotAvailable,ptAvailable,ptRelative,ptRemovable,ptRemote);

  TDriveProperties = class(TObject)
    Number    : integer;
    DriveType : TDriveType;
    DriveName,
    VolName   : string
    end;

const
  DriveTypeList : array[TDriveType] of string =
                ('unknown','no root','removable','fixed','remote','CD-ROM','Ram-Disk');

// Typ eines Laufwerkes ermitteln
function DriveType (const Path : string) : TDriveType;

// Removable oder Fixed
function IsLocalDrive (const Path : string) : boolean;

// Typ eines Pfades ermitteln (siehe TPathtype)
function CheckPath (const Path : string) : TPathType;

// Prüfe Laufwerk auf Verfügbarkeit
function CheckForDriveAvailable (const Path : string; var VolumeID : string) : boolean;

// Liste aller Laufwerke der Typen "UseTypes" aufbauen
procedure BuildDriveList(DriveList : TStrings; UseTypes : TDriveTypes);

// Zu einem Datenträgernamen gehörendes Laufwerk ermitteln
function GetDriveForVolume (const Vol : string; FirstDrive : integer) : string;

// Name des angemeldeten Benutzers ermitteln
function GetUserName : string;

// Prüfen, ob ein in einem Pfad enthaltenes Laufwerk (lokal oder Netz) verfügbar ist.
// StdUser: Name des angemeldeten Benutzers oder leer
function NetPathAvailable (const Path,StdUser : string; const AltUser : TUserAccount; ReadOnly,Prompt : boolean) : integer; overload;
function NetPathAvailable (const Path,StdUser : string; ReadOnly,Prompt : boolean) : integer; overload;
function PathIsAvailable (const Path : string) : boolean;
function CheckForWritablePath (const Path : string) : boolean;

function ReconnectPathEx (const Path : string) : cardinal;
function ReconnectPath (const Path : string) : boolean;

function CheckForDirectory (const Path : string) : boolean;
function CheckForFile (const Filename : string) : boolean;

// Account eines alternativen Benutzers abfragen
function ReadAltUserAccount (NetResource : string) : boolean;

// Alternativen Benutzernamen setzen
function MakeAltUserAccount (User,Pwd : string) : TUserAccount;
procedure SetAltUserAccount (User,Pwd : string);

// Alternativen Benutzernamen löschen
function ReconnectDefaultUser (const Path : string) : integer;
procedure ResetAltUserAccount;

function GetVolumeNameForVolumeMountPoint(lpszVolumeMountPoint: PChar;
  lpszVolumeName : PChar; var cchBufferLength : DWord): BOOL; stdcall;
{$EXTERNALSYM GetVolumeNameForVolumeMountPoint}

implementation

uses LogonDlg, UnitConsts;

var
  AltUserAccount : TUserAccount;
  RemoteName     : string;
  AltConnect     : boolean;

function GetVolumeNameForVolumeMountPoint; external 'kernel32.dll' name 'GetVolumeNameForVolumeMountPointA';

// Typ eines Laufwerkes ermitteln
function DriveType (const Path : string) : TDriveType;
begin
  case GetDriveType(pchar(ExtractFileDrive(Path))) of
  DRIVE_NO_ROOT_DIR : Result:=dtNoRoot;
  DRIVE_REMOVABLE   : Result:=dtRemovable;
  DRIVE_FIXED       : Result:=dtFixed;
  DRIVE_REMOTE      : Result:=dtRemote;
  DRIVE_CDROM       : Result:=dtCdRom;
  DRIVE_RAMDISK     : Result:=dtRamDisk;
  else Result:=dtUnknown;
    end;
  end;

function IsLocalDrive (const Path : string) : boolean;
var
  dt : TDriveType;
begin
  dt:=DriveType(Path);
  Result:=(dt=dtRemovable) or (dt=dtFixed);
  end;

// Typ eines Pfades ermitteln (siehe TPathType)
function CheckPath (const Path : string) : TPathType;
var
  dr : string;
  dt : TDriveType;
begin
  dr:=ExtractFileDrive(IncludeTrailingPathDelimiter(Path));
  if length(dr)>0 then begin
    if (copy(dr,1,2)='\\') then Result:=ptRemote    // Netzwerkumgebung
    else begin                                      // Pfad mit Laufwerksangabe
      dt:=DriveType(dr);
      case dt of
      dtRemote : Result:=ptRemote;          // Netzlaufwerk
      dtUnknown,
      dtNoRoot : Result:=ptNotAvailable;    // nicht verfügbar
      dtCdRom,
      dtRemovable : Result:=ptRemovable;    // Laufwerk mit Wechselmedium
      else Result:=ptAvailable;
        end;
      end;
    end
  else Result:=ptRelative;
  end;

// Prüfe Laufwerk auf Verfügbarkeit
function CheckForDriveAvailable (const Path : string; var VolumeID : string) : boolean;
var
  v : pchar;
  d : string;
  n,cl,sf : dword;
begin
  d:=IncludeTrailingPathDelimiter(ExtractFileDrive(Path));
  n:=50; v:=StrAlloc(n);
  Result:=GetVolumeInformation(pchar(d),v,n,nil,cl,sf,nil,0);
  if Result then VolumeID:=v else VolumeID:=rsNotAvail;
  StrDispose(v);
  end;

// Liste aller Laufwerke der Typen "UseTypes" aufbauen
procedure BuildDriveList (DriveList : TStrings; UseTypes : TDriveTypes);
var
  i         : integer;
  DriveBits : set of 0..25;
  dp        : TDriveProperties;
begin
  DriveList.Clear;
  Integer(DriveBits):=GetLogicalDrives;
  for i:=0 to 25 do begin
    if not (i in DriveBits) then Continue;
    dp:=TDriveProperties.Create;
    with dp do begin
      Number:=i;
      DriveName:=Char(i+Ord('A'))+':\';
      DriveType:=TDriveType(GetDriveType(PChar(DriveName)));
      CheckForDriveAvailable(DriveName,VolName);
      end;
    if dp.DriveType in UseTypes then DriveList.AddObject(dp.VolName,dp)
    else dp.Free;;
    end;
  end;

// Zu einem Datenträgernamen gehörendes Laufwerk ermitteln
function GetDriveForVolume (const Vol : string; FirstDrive : integer) : string;
var
  i         : integer;
  DriveBits : set of 0..25;
  sd,sv     : string;
begin
  Result:='';
  Integer(DriveBits):=GetLogicalDrives;
  for i:=FirstDrive to 25 do begin
    if not (i in DriveBits) then Continue;
    sd:=Char(i+Ord('A'))+':\';
    if CheckForDriveAvailable(sd,sv) and AnsiSameText(sv,Vol) then begin
      Result:=sd; Exit;
      end;
    end;
  end;

const
  NwTestName = 'test.tmp';

// Name des angemeldeten Benutzers ermitteln
function GetUserName : string;
var
  ul    : dword;
  un    : pchar;
begin
  ul:=1024; GetMem (un,ul);
  if WNetGetUser(nil,un,ul)=NO_ERROR then Result:=un else Result:='';
  FreeMem (un);
  end;

function MakeNetRes (const Path : string; var NetRes : TNetResource) : integer;
var
  nl : dword;
begin
  with NetRes do begin
    dwScope:=0; dwDisplayType:=0;
    lpProvider:=nil; lpLocalName:=nil; lpComment:=nil;
    dwUsage:=RESOURCEUSAGE_CONNECTABLE;
    dwType:=RESOURCETYPE_DISK;
    if (copy(Path,1,2)='\\') then begin  // Netzwerkumgebung
      lpRemoteName:=StrNew(pchar(ExcludeTrailingPathDelimiter(Path)));
      Result:=ERROR_NOT_CONNECTED;
      end
    else begin
      nl:=1024;
      lpLocalName:=StrNew(pchar(ExtractFileDrive(Path)));
      lpRemoteName:=StrAlloc(nl);
      Result:=WNetGetConnection(lpLocalName,lpRemoteName,nl);   // Netzwerkname des Pfads
      end;
    end;
  end;

procedure ReleaseNetRes (NetRes : TNetResource);
begin
  with NetRes do begin
    StrDispose(lpLocalName); StrDispose(lpRemoteName);
    end;
  end;

// Prüfen, ob ein in einem Pfad enthaltenes Laufwerk (lokal oder Netz) verfügbar ist.
// Bei Netzlaufwerken wird geprüft, ob die Verbindung zum Schreiben verfügbar ist,
// wenn nicht, wird versucht, sie herzustellen. Dazu wird zuerst versucht,
// dies mit dem angemeldeten Benutzer zu machen, dann mit einem eingetragenen
// alternativen Benutzer ("AltUser")
// Prompt = true:  Wenn das nicht geht, wird ein Benutzername und ein Passwort abgefragt.
function NetPathAvailable (const Path,StdUser : string; const AltUser : TUserAccount; ReadOnly,Prompt : boolean) : integer;
var
  ec,nl    : dword;
  NetRes   : TNetResource;
  FindData : TWin32FindData;
begin
  AltConnect:=false;
  // prüfe, ob Verbindung vorhanden
  if FindFirstFile(PChar(IncludeTrailingPathDelimiter(ExtractFileDrive(Path))+'*.*'),FindData)=INVALID_HANDLE_VALUE then begin
    ec:=MakeNetRes(Path,NetRes);
    if ec<>NO_ERROR then begin
    // nein - als angemeldeter Benutzer versuchen
      if (length(StdUser)>0) then begin
        nl:=0;
        repeat
          if nl>0 then Sleep(1000); // wait 1 s
          ec:=WNetAddConnection2(NetRes,nil,pchar(StdUser),0);
          if (ec=ERROR_ALREADY_ASSIGNED) or (ec=ERROR_DEVICE_ALREADY_REMEMBERED)
            or (ec=ERROR_SESSION_CREDENTIAL_CONFLICT) then ec:=NO_ERROR;
          inc(nl);
          until (ec<>ERROR_NETNAME_DELETED) or (nl=3);  // try 3 times
        end
      else ec:=ERROR_LOGON_FAILURE;
      end;
    // prüfe, ob geschrieben werden kann, fallse nicht "ReadOnly"
    if not ReadOnly and (ec=NO_ERROR) and not CheckForWritablePath(NetRes.lpRemoteName) then
        ec:=ERROR_SESSION_CREDENTIAL_CONFLICT;
    if ec<>NO_ERROR then begin
      // Mit alternativem Benutzer versuchen
      with AltUser do if (length(Username)>0) then begin
        ec:=WNetCancelConnection2(NetRes.lpRemoteName,0,true);
        if (ec=NO_ERROR) or (ec=ERROR_NOT_CONNECTED) then begin
              ec:=WNetAddConnection2(NetRes,pchar(Password),pchar(Username),0);
          if ec=NO_ERROR then begin
            AltConnect:=true;
            RemoteName:=NetRes.lpRemoteName;
            end;
          end;
        end;
      // falls nicht möglich und Dialog erlaubt, interaktiv versuchen
      if (ec<>NO_ERROR) and Prompt then begin
        repeat
          ec:=WNetCancelConnection2(NetRes.lpRemoteName,0,false);
          if (ec=NO_ERROR) or (ec=ERROR_NOT_CONNECTED) then begin
            ec:=WNetAddConnection2(NetRes,nil,nil,CONNECT_INTERACTIVE or CONNECT_PROMPT);
            end
          else ec:=ERROR_CANCELLED;
          until (ec=NO_ERROR) or (ec=ERROR_CANCELLED);
        if ec=NO_ERROR then begin
          RemoteName:=NetRes.lpRemoteName;
          end;
        end;
      // prüfe, ob geschrieben werden kann, fallse nicht "ReadOnly"
      if not ReadOnly and (ec=NO_ERROR) and not CheckForWritablePath(NetRes.lpRemoteName) then
          ec:=ERROR_SESSION_CREDENTIAL_CONFLICT;
      end;
    ReleaseNetRes(NetRes);
    end
  else begin
    if ReadOnly or CheckForWritablePath(Path) then ec:=NO_ERROR else ec:=ERROR_ACCESS_DENIED;
    end;
  Result:=ec;
  end;

function NetPathAvailable (const Path,StdUser : string; ReadOnly,Prompt : boolean) : integer;
begin
  Result:=NetPathAvailable(Path,StdUser,AltUserAccount,ReadOnly,Prompt);
  end;

function PathIsAvailable (const Path : string) : boolean;
var
  n,m : int64;
begin
  Result:=GetDiskFreeSpaceEx(pchar(IncludeTrailingPathDelimiter(Path)),n,m,nil);
  end;

// Prüfen, ob in einen Pfad geschrieben werden kann
function CheckForWritablePath (const Path : string) : boolean;
var
  fsT      : TextFile;
  s        : string;
  nd       : boolean;
begin
  Result:=DirectoryExists(Path);
  nd:=not Result;
  if nd then Result:=ForceDirectories(Path);  // versuche Pfad zu erstellen
  if Result then begin
    s:=IncludeTrailingPathDelimiter(Path)+NwTestName;
    AssignFile (fsT,s);
    {$I-} Rewrite(fsT); {$I+}
    Result:=IoResult=0;
    if Result then begin
      CloseFile(fsT);
      DeleteFile(s);
      end;
    if nd then RemoveDir(Path);
    end;
  end;

function ReconnectPathEx (const Path : string) : cardinal;
var
  nl       : dword;
  NetRes   : TNetResource;
  nn       : pchar;
begin
  nl:=1024; nn:=StrAlloc(nl);
  with NetRes do begin
    dwScope:=0; dwDisplayType:=0;
    lpProvider:=''; lpLocalName:='';
    dwUsage:=RESOURCEUSAGE_CONNECTABLE;
    dwType:=RESOURCETYPE_DISK;
    if (copy(Path,1,2)='\\') then begin  // Netzwerkumgebung
      lpRemoteName:=pchar(ExcludeTrailingPathDelimiter(Path));
      end
    else begin
      lpLocalName:=pchar(ExtractFileDrive(Path));
      WNetGetConnection(lpLocalName,nn,nl);   // Netzwerkname des Pfads
      lpRemoteName:=nn;
      end;
    end;
  // als angemeldeter Benutzer versuchen
  Result:=WNetAddConnection2(NetRes,nil,nil,0);
  end;

function ReconnectPath (const Path : string) : boolean;
begin
  Result:=ReconnectPathEx(Path)=NO_ERROR;
  end;

function CheckForDirectory (const Path : string) : boolean;
var
  pt   : TPathType;
begin
  // prüfen, ob lokales Ziel oder im Netz
  pt:=CheckPath(Path);
  if pt=ptNotAvailable then begin // nicht verbundener Netzwerkpfad?
    if ReconnectPath(Path) then begin
      pt:=CheckPath(Path);
      Result:=pt<>ptNotAvailable;
      end
    else Result:=false;
    end
  else Result:=true;
  end;

function CheckForFile (const Filename : string) : boolean;
begin
  Result:=CheckForDirectory(ExtractFilePath(Filename));
  if Result then Result:=FileExists(Filename);
  end;

// Account eines alternativen Benutzers abfragen
function ReadAltUserAccount (NetResource : string) : boolean;
var
  User,Pwd : string;
begin
  User:=''; Pwd:='';
  if InputUserAccount(rsConnectTo,NetResource,false,User,Pwd)=mrOK then
       with AltUserAccount do begin
    Username:=User; Password:=Pwd;
    Result:=true;
    end
  else Result:=false;
  end;

// Alternativen Benutzernamen setzen
function MakeAltUserAccount (User,Pwd : string) : TUserAccount;
begin
  with Result do begin
    Username:=User; Password:=Pwd;
    end
  end;

procedure SetAltUserAccount (User,Pwd : string);
begin
  AltUserAccount:=MakeAltUserAccount(User,Pwd);
  end;

// wieder mit Standardbenutzer verbinden
function ReconnectDefaultUser (const Path : string) : integer;
var
  NetRes   : TNetResource;
begin
  if (length(RemoteName)>0) then begin
    WNetCancelConnection2(pchar(RemoteName),0,false);
    Result:=MakeNetRes(Path,NetRes);
    if Result<>NO_ERROR then begin
      Result:=WNetAddConnection2(NetRes,nil,nil,0);
      if (Result=ERROR_ALREADY_ASSIGNED) or (Result=ERROR_DEVICE_ALREADY_REMEMBERED)
        or (Result=ERROR_SESSION_CREDENTIAL_CONFLICT) then Result:=NO_ERROR;
      end;
    ReleaseNetRes(NetRes);
    end
  else Result:=NO_ERROR;
  end;
  
// Alternativen Benutzernamen löschen
procedure ResetAltUserAccount;
begin
  if AltConnect and (length(RemoteName)>0) then
    WNetCancelConnection2(pchar(RemoteName),0,false);
  with AltUserAccount do begin
    Username:=''; Password:='';
    end;
  AltConnect:=false; RemoteName:='';
  end;

initialization
  with AltUserAccount do begin
    Username:=''; Password:='';
    end;
  AltConnect:=false; RemoteName:='';
finalization
  ResetAltUserAccount;
  end.

