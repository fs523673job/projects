(* Delphi Unit
   Definition aus Window API, die in Unit "WinApi.Windows" fehlen
   verschiedene Unterroutinen f�r Windows API
   ==========================================

   � Dr. J. Rathlev, D-24222 Schwentinental (info(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1 - Sep. 2002
   Vers. 2 - Jan. 2009 : dyn. Einbindung von CreateProcessWithLogonW
                         und GetxxxDefaultUILanguage
   Vers. 3 - Mar. 2010 : several constants from WinNT.h, WinBase.h, WinIoCtl.h
                         see: JclWin32.pas
   Vers. 4 - Jun. 2011 : SetThreadExecutionState, SetSuspendState
   Vers. 5 - Nov. 2011 : GetUserSessionData
   Vers. 6 - July 2013 : more environment folders
   Vers. 7 - Dec. 2015 : Modifications for Delphi 10 (all functions and constants
                         removed that are now handled in Winapi.Windows)
   *)

unit WinApiUtils;

interface

uses Winapi.Windows, System.Classes, System.SysUtils, System.Win.Registry;

const
  powrprof  = 'powrprof.dll';
  secur32 = 'Secur32.dll';
  wtsapi32 = 'Wtsapi32.dll';

// Reason flags       (not used on Windows 2000, Windows NT and Windows Me/98/95)
// Flags that end up in the event log code
  SHTDN_REASON_FLAG_USER_DEFINED = $40000000;
  SHTDN_REASON_FLAG_PLANNED = $80000000;
// Microsoft major reasons
  SHTDN_REASON_MAJOR_OTHER = 0;
// Microsoft minor reasons
  SHTDN_REASON_MINOR_OTHER = 0;

  EWX_HYBRID_SHUTDOWN = $00400000;  // Windows 8 and newer

  DOMAIN_ALIAS_RID_ADMINS = $00000220;
  DOMAIN_ALIAS_RID_POWER_USERS = $00000223;

  LOGON_WITH_PROFILE = $00000001;
  LOGON_NETCREDENTIALS_ONLY = $00000002;
  LOGON_ZERO_PASSWORD_BUFFER = DWORD($80000000);

  // constants missing in unit Windows
  IO_REPARSE_TAG_MOUNT_POINT         = $A0000003;
  {$EXTERNALSYM IO_REPARSE_TAG_MOUNT_POINT}
  IO_REPARSE_TAG_HSM                 = $C0000004;
  {$EXTERNALSYM IO_REPARSE_TAG_HSM}
  IO_REPARSE_TAG_SIS                 = $80000007;
  {$EXTERNALSYM IO_REPARSE_TAG_SIS}
  IO_REPARSE_TAG_DFS                 = $8000000A;
  {$EXTERNALSYM IO_REPARSE_TAG_DFS}
  IO_REPARSE_TAG_FILTER_MANAGER      = $8000000B;
  {$EXTERNALSYM IO_REPARSE_TAG_FILTER_MANAGER}
  IO_REPARSE_TAG_SYMLINK             = $A000000C;
  {$EXTERNALSYM IO_REPARSE_TAG_SYMLINK}

  MAXIMUM_REPARSE_DATA_BUFFER_SIZE  = 16 * 1024;
  REPARSE_DATA_BUFFER_HEADER_SIZE  = 8;

  WTS_CURRENT_SERVER_HANDLE = 0;

////////////////////////////////////////////////////////////////////////
//                                                                    //
//               NT Defined Privileges                                //
//                                                                    //
////////////////////////////////////////////////////////////////////////

  {$EXTERNALSYM SE_CREATE_TOKEN_NAME}
  SE_CREATE_TOKEN_NAME                = 'SeCreateTokenPrivilege';
  {$EXTERNALSYM SE_ASSIGNPRIMARYTOKEN_NAME}
  SE_ASSIGNPRIMARYTOKEN_NAME          = 'SeAssignPrimaryTokenPrivilege';
  {$EXTERNALSYM SE_LOCK_MEMORY_NAME}
  SE_LOCK_MEMORY_NAME                 = 'SeLockMemoryPrivilege';
  {$EXTERNALSYM SE_INCREASE_QUOTA_NAME}
  SE_INCREASE_QUOTA_NAME              = 'SeIncreaseQuotaPrivilege';
  {$EXTERNALSYM SE_UNSOLICITED_INPUT_NAME}
  SE_UNSOLICITED_INPUT_NAME           = 'SeUnsolicitedInputPrivilege';
  {$EXTERNALSYM SE_MACHINE_ACCOUNT_NAME}
  SE_MACHINE_ACCOUNT_NAME             = 'SeMachineAccountPrivilege';
  {$EXTERNALSYM SE_TCB_NAME}
  SE_TCB_NAME                         = 'SeTcbPrivilege';
  {$EXTERNALSYM SE_SECURITY_NAME}
  SE_SECURITY_NAME                    = 'SeSecurityPrivilege';
  {$EXTERNALSYM SE_TAKE_OWNERSHIP_NAME}
  SE_TAKE_OWNERSHIP_NAME              = 'SeTakeOwnershipPrivilege';
  {$EXTERNALSYM SE_LOAD_DRIVER_NAME}
  SE_LOAD_DRIVER_NAME                 = 'SeLoadDriverPrivilege';
  {$EXTERNALSYM SE_SYSTEM_PROFILE_NAME}
  SE_SYSTEM_PROFILE_NAME              = 'SeSystemProfilePrivilege';
  {$EXTERNALSYM SE_SYSTEMTIME_NAME}
  SE_SYSTEMTIME_NAME                  = 'SeSystemtimePrivilege';
  {$EXTERNALSYM SE_PROF_SINGLE_PROCESS_NAME}
  SE_PROF_SINGLE_PROCESS_NAME         = 'SeProfileSingleProcessPrivilege';
  {$EXTERNALSYM SE_INC_BASE_PRIORITY_NAME}
  SE_INC_BASE_PRIORITY_NAME           = 'SeIncreaseBasePriorityPrivilege';
  {$EXTERNALSYM SE_CREATE_PAGEFILE_NAME}
  SE_CREATE_PAGEFILE_NAME             = 'SeCreatePagefilePrivilege';
  {$EXTERNALSYM SE_CREATE_PERMANENT_NAME}
  SE_CREATE_PERMANENT_NAME            = 'SeCreatePermanentPrivilege';
  {$EXTERNALSYM SE_BACKUP_NAME}
  SE_BACKUP_NAME                      = 'SeBackupPrivilege';
  {$EXTERNALSYM SE_RESTORE_NAME}
  SE_RESTORE_NAME                     = 'SeRestorePrivilege';
  {$EXTERNALSYM SE_SHUTDOWN_NAME}
  SE_SHUTDOWN_NAME                    = 'SeShutdownPrivilege';
  {$EXTERNALSYM SE_DEBUG_NAME}
  SE_DEBUG_NAME                       = 'SeDebugPrivilege';
  {$EXTERNALSYM SE_AUDIT_NAME}
  SE_AUDIT_NAME                       = 'SeAuditPrivilege';
  {$EXTERNALSYM SE_SYSTEM_ENVIRONMENT_NAME}
  SE_SYSTEM_ENVIRONMENT_NAME          = 'SeSystemEnvironmentPrivilege';
  {$EXTERNALSYM SE_CHANGE_NOTIFY_NAME}
  SE_CHANGE_NOTIFY_NAME               = 'SeChangeNotifyPrivilege';
  {$EXTERNALSYM SE_REMOTE_SHUTDOWN_NAME}
  SE_REMOTE_SHUTDOWN_NAME             = 'SeRemoteShutdownPrivilege';
  {$EXTERNALSYM SE_UNDOCK_NAME}
  SE_UNDOCK_NAME                      = 'SeUndockPrivilege';
  {$EXTERNALSYM SE_SYNC_AGENT_NAME}
  SE_SYNC_AGENT_NAME                  = 'SeSyncAgentPrivilege';
  {$EXTERNALSYM SE_ENABLE_DELEGATION_NAME}
  SE_ENABLE_DELEGATION_NAME           = 'SeEnableDelegationPrivilege';
  {$EXTERNALSYM SE_MANAGE_VOLUME_NAME}
  SE_MANAGE_VOLUME_NAME               = 'SeManageVolumePrivilege';
  {$EXTERNALSYM SE_IMPERSONATE_NAME}
  SE_IMPERSONATE_NAME                 = 'SeImpersonatePrivilege';
  {$EXTERNALSYM SE_CREATE_GLOBAL_NAME}
  SE_CREATE_GLOBAL_NAME               = 'SeCreateGlobalPrivilege';
  {$EXTERNALSYM SE_TRUSTED_CREDMAN_ACCESS_NAME}
  SE_TRUSTED_CREDMAN_ACCESS_NAME      = 'SeTrustedCredManAccessPrivilege';
  {$EXTERNALSYM SE_RELABEL_NAME}
  SE_RELABEL_NAME                     = 'SeRelabelPrivilege';
  {$EXTERNALSYM SE_INC_WORKING_SET_NAME}
  SE_INC_WORKING_SET_NAME             = 'SeIncreaseWorkingSetPrivilege';
  {$EXTERNALSYM SE_TIME_ZONE_NAME}
  SE_TIME_ZONE_NAME                   = 'SeTimeZonePrivilege';
  {$EXTERNALSYM SE_CREATE_SYMBOLIC_LINK_NAME}
  SE_CREATE_SYMBOLIC_LINK_NAME        = 'SeCreateSymbolicLinkPrivilege';

  // constants from rpcdce.h
  RPC_C_AUTHN_LEVEL_DEFAULT         = 0;
  RPC_C_AUTHN_LEVEL_NONE            = 1;
  RPC_C_AUTHN_LEVEL_CONNECT         = 2;
  RPC_C_AUTHN_LEVEL_CALL            = 3;
  RPC_C_AUTHN_LEVEL_PKT             = 4;
  RPC_C_AUTHN_LEVEL_PKT_INTEGRITY   = 5;
  RPC_C_AUTHN_LEVEL_PKT_PRIVACY     = 6;

  RPC_C_IMP_LEVEL_DEFAULT           = 0;
  RPC_C_IMP_LEVEL_ANONYMOUS         = 1;
  RPC_C_IMP_LEVEL_IDENTIFY          = 2;
  RPC_C_IMP_LEVEL_IMPERSONATE       = 3;
  RPC_C_IMP_LEVEL_DELEGATE          = 4;

  // constants from objidl.h -  EOLE_AUTHENTICATION_CAPABILITIES
  EOAC_NONE                    = $0;
  EOAC_MUTUAL_AUTH             = $1;
  EOAC_SECURE_REFS             = $2;
  EOAC_ACCESS_CONTROL          = $4;
  EOAC_APPID                   = $8;
  EOAC_DYNAMIC                 = $10;
  EOAC_STATIC_CLOAKING         = $20;
  EOAC_DYNAMIC_CLOAKING        = $40;
  EOAC_ANY_AUTHORITY           = $80;
  EOAC_MAKE_FULLSIC            = $100;
  EOAC_REQUIRE_FULLSIC         = $200;
  EOAC_AUTO_IMPERSONATE        = $400;
  EOAC_DEFAULT                 = $800;
  EOAC_DISABLE_AAA             = $1000;
  EOAC_NO_CUSTOM_MARSHAL       = $2000;

type
  TLongWord = record
    case integer of
    0 : (LongWord : cardinal);
    1 : (Lo,Hi : word);
    2 : (LoL,LoH,HiL,HiH : byte);
    3 : (Bytes : array [0..3] of Byte);
    end;

  TLuidArray = array of TLuid;
  USHORT = word;

  _LSA_UNICODE_STRING = record
    Length: USHORT;
    MaximumLength: USHORT;
    Buffer: LPWSTR;
  end;
  LSA_UNICODE_STRING = _LSA_UNICODE_STRING;

  _SECURITY_LOGON_TYPE = (
    seltFiller0, seltFiller1,
    Interactive,
    Network,
    Batch,
    Service,
    Proxy,
    Unlock,
    NetworkCleartext,
    NewCredentials,
    RemoteInteractive,
    CachedInteractive,
    CachedRemoteInteractive);
  SECURITY_LOGON_TYPE = _SECURITY_LOGON_TYPE;
  TSecurityLogonType = SECURITY_LOGON_TYPE;

  _SECURITY_LOGON_SESSION_DATA = record
    Size: ULONG;
    LogonId: LUID;
    UserName: LSA_UNICODE_STRING;
    LogonDomain: LSA_UNICODE_STRING;
    AuthenticationPackage: LSA_UNICODE_STRING;
    LogonType: SECURITY_LOGON_TYPE;
    Session: ULONG;
    Sid: PSID;
    LogonTime: LARGE_INTEGER;
    LogonServer: LSA_UNICODE_STRING;
    DnsDomainName: LSA_UNICODE_STRING;
    Upn: LSA_UNICODE_STRING;
  end;
  SECURITY_LOGON_SESSION_DATA = _SECURITY_LOGON_SESSION_DATA;
  TSecurityLogonSessionData = _SECURITY_LOGON_SESSION_DATA;
  PSecurityLogonSessionData = ^TSecurityLogonSessionData;

  _WTS_INFO_CLASS = (
    WTSInitialProgram,
    WTSApplicationName,
    WTSWorkingDirectory,
    WTSOEMId,
    WTSSessionId,
    WTSUserName,
    WTSWinStationName,
    WTSDomainName,
    WTSConnectState,
    WTSClientBuildNumber,
    WTSClientName,
    WTSClientDirectory,
    WTSClientProductId,
    WTSClientHardwareId,
    WTSClientAddress,
    WTSClientDisplay,
    WTSClientProtocolType);
  WTS_INFO_CLASS = _WTS_INFO_CLASS;
  TWtsInfoClass = _WTS_INFO_CLASS;

  _WTS_CONNECTSTATE_CLASS = (
    WTSActive, // User logged on to WinStation
    WTSConnected, // WinStation connected to client
    WTSConnectQuery, // In the process of connecting to client
    WTSShadow, // Shadowing another WinStation
    WTSDisconnected, // WinStation logged on without client
    WTSIdle, // Waiting for client to connect
    WTSListen, // WinStation is listening for connection
    WTSReset, // WinStation is being reset
    WTSDown, // WinStation is down due to error
    WTSInit); // WinStation in initialization
  WTS_CONNECTSTATE_CLASS = _WTS_CONNECTSTATE_CLASS;
  TWtsConnectionState = _WTS_CONNECTSTATE_CLASS;

  {$EXTERNALSYM _REPARSE_DATA_BUFFER}
  _REPARSE_DATA_BUFFER = record
    ReparseTag: DWORD;
    ReparseDataLength: Word;
    Reserved: Word;
    case Integer of
      0: ( // SymbolicLinkReparseBuffer and MountPointReparseBuffer
        SubstituteNameOffset: Word;
        SubstituteNameLength: Word;
        PrintNameOffset: Word;
        PrintNameLength: Word;
        PathBuffer: array [0..MAXIMUM_REPARSE_DATA_BUFFER_SIZE] of WCHAR);
      1: ( // GenericReparseBuffer
        DataBuffer: array [0..MAXIMUM_REPARSE_DATA_BUFFER_SIZE+4*sizeof(word)] of Byte);
  end;
  {$EXTERNALSYM REPARSE_DATA_BUFFER}
  REPARSE_DATA_BUFFER = _REPARSE_DATA_BUFFER;
  {$EXTERNALSYM PREPARSE_DATA_BUFFER}
  PREPARSE_DATA_BUFFER = ^_REPARSE_DATA_BUFFER;
  TReparseDataBuffer = _REPARSE_DATA_BUFFER;
  PReparseDataBuffer = PREPARSE_DATA_BUFFER;

  TSetSuspendState = function (Hibernate, ForceCritical, DisableWakeEvent: BOOL) : BOOL; stdcall;

  TProcessIdToSessionId = function (ProcessId : DWORD; var SessionId : DWORD) : BOOL; stdcall;

  TCreateProcessWithLogonW = function(lpUsername: PWideChar;
    lpDomain: PWideChar; lpPassword: PWideChar; dwLogonFlags: DWORD;
    lpApplicationName: PWideChar; lpCommandLine: PWideChar;
    dwCreationFlags: DWORD; lpEnvironment: Pointer;
    lpCurrentDirectory: PWideChar; const lpStartupInfo: TStartupInfoW;
    var lpProcessInformation: TProcessInformation): BOOL; stdcall;

  TWTSQuerySessionInformation = function (hServer: THandle; SessionId: DWORD;
    WTSInfoClass: WTS_INFO_CLASS; var pBuffer: Pointer;
    var pBytesReturned: DWORD): BOOL; stdcall;

  TWTSGetActiveConsoleSessionId = function : DWORD;

  TLsaEnumerateLogonSessions = function (var Count: ULONG; var List: PLUID): LongInt; stdcall;

  TLsaGetLogonSessionData = function (LogonId: PLUID;
    var ppLogonSessionData: PSecurityLogonSessionData): LongInt; stdcall;

  TLsaFreeReturnBuffer = function (Buffer: pointer): Integer; stdcall;

  TFileVersionInfo = record
    Company,Description,Version,InternalName,Copyright,Comments : string;
    end;

  TFileSystemFlag =
   (
    fsCaseSensitive,            // The file system supports case-sensitive file names.
    fsCasePreservedNames,       // The file system preserves the case of file names when it places a name on disk.
    fsSupportsUnicodeOnDisk,    // The file system supports Unicode in file names as they appear on disk.
    fsPersistentACLs,           // The file system preserves and enforces ACLs. For example, NTFS preserves and enforces ACLs, and FAT does not.
    fsSupportsFileCompression,  // The file system supports file-based compression.
    fsSupportsVolumeQuotas,     // The file system supports disk quotas.
    fsSupportsSparseFiles,      // The file system supports sparse files.
    fsSupportsReparsePoints,    // The file system supports reparse points.
    fsSupportsRemoteStorage,    // ?
    fsVolumeIsCompressed,       // The specified volume is a compressed volume; for example, a DoubleSpace volume.
    fsSupportsObjectIds,        // The file system supports object identifiers.
    fsSupportsEncryption,       // The file system supports the Encrypted File System (EFS).
    fsSupportsNamedStreams,     // The file system supports named streams.
    fsVolumeIsReadOnly          // The specified volume is read-only.
                                //   Windows 2000/NT and Windows Me/98/95:  This value is not supported.
   );

  TFileSystemFlags = set of TFileSystemFlag;

  TSessionData = record
    UserLuid  : TLUID;
    UserName,
    Domain    : string;
    LogonType : TSecurityLogonType;
    LogonTime : TDateTime;
    end;
  TSessionList = array of TSessionData;


function GetTickCount64: ULONGLONG; stdcall;
{$EXTERNALSYM GetTickCount64}

{ ---------------------------------------------------------------- }
{$EXTERNALSYM SetSuspendState}
function SetSuspendState(Hibernate, ForceCritical, DisableWakeEvent: Boolean): Boolean;

{$EXTERNALSYM GetFileSizeEx}
function GetFileSizeEx(hFile: THandle; lpFileSize : Large_Integer): BOOL; stdcall;

{$EXTERNALSYM ConvertStringSidToSid}
function ConvertStringSidToSid (lpStringSid: PChar; var Sid: PSID): BOOL; stdcall;

{$EXTERNALSYM ConvertSidToStringSid}
function ConvertSidToStringSid (Sid: PSID; var lpStringSid: LPSTR): BOOL; stdcall;

{ ---------------------------------------------------------------- }
{$EXTERNALSYM InitiateSystemShutdownEx}
function InitiateSystemShutdownEx(lpMachineName, lpMessage: LPWSTR;
  dwTimeout: DWORD; bForceAppsClosed, bRebootAfterShutdown : BOOL; dwReason : DWORD): BOOL; stdcall;

{$EXTERNALSYM InitiateSystemShutdownExA}
function InitiateSystemShutdownExA(lpMachineName, lpMessage: LPSTR;
  dwTimeout: DWORD; bForceAppsClosed, bRebootAfterShutdown : BOOL; dwReason : DWORD): BOOL; stdcall;

{$EXTERNALSYM InitiateSystemShutdownExW}
function InitiateSystemShutdownExW(lpMachineName, lpMessage: LPWSTR;
  dwTimeout: DWORD; bForceAppsClosed, bRebootAfterShutdown : BOOL; dwReason : DWORD): BOOL; stdcall;

{ ---------------------------------------------------------------- }
{$EXTERNALSYM LsaNtStatusToWinError}
function LsaNtStatusToWinError(Status: cardinal): ULONG; stdcall;

{$EXTERNALSYM GetVolumeNameForVolumeMountPoint}
function GetVolumeNameForVolumeMountPoint (lpszVolumeMountPoint : PWideChar;
  lpszVolumeName : PWideChar; cchBufferLength : DWORD) : BOOL; stdcall;

{$EXTERNALSYM GetVolumePathName}
function GetVolumePathName(lpszFileName: PWideChar; lpszVolumePathName: PWideChar;
         cchBufferLength: DWORD): BOOL; stdcall;

{$EXTERNALSYM GetVolumePathNamesForVolumeName}
function GetVolumePathNamesForVolumeName(lpszVolumeName: PWideChar; lpszVolumePathNames: PWideChar;
         cchBufferLength: DWORD; var lpcchReturnLength: DWORD): BOOL; stdcall;

{ ---------------------------------------------------------------- }
function CreateProcessWithLogonW(lpUsername: PWideChar;
  lpDomain: PWideChar; lpPassword: PWideChar; dwLogonFlags: DWORD;
  lpApplicationName: PWideChar; lpCommandLine: PWideChar;
  dwCreationFlags: DWORD; lpEnvironment: Pointer;
  lpCurrentDirectory: PWideChar; const lpStartupInfo: TStartupInfoW;
  var lpProcessInformation: TProcessInformation): BOOL; stdcall;

{ ---------------------------------------------------------------- }
// Ermitteln des sprachabh�ngigen Namens eines Kontos
function GetAccountName(sSID : string) : string;

{ ---------------------------------------------------------------- }
(* Windows-System-Info (Plattform, Version, Build) *)
function IsWinNT : boolean;
function IsWin2000 : boolean;
function IsVista : boolean;
function IsWindows64 : boolean;
function Is64BitApp : boolean;

function GetVolumeName(const Drive: string): string;
function GetVolumeSerialNumber(const Drive: string): string;
function GetVolumeFileSystem(const Drive: string): string;
function GetVolumeComponentLength(const Drive: string): string;
function IsNtfs (const Drive : string) : boolean;
function IsExFat (const Drive : string) : boolean;
function IsLtfs (const Drive : string) : boolean;
function IsExtFs (const Drive : string) : boolean; // file systems capable to keep files > 4GB

function GetVolumeFileSystemFlags(const Volume: string): TFileSystemFlags;
function GetVolumeUniqueName(const Drive : string) : string;

{ ---------------------------------------------------------------- }
(* Disk size and free space *)

function GetDiskFree (const Path : string) : int64;
function GetDiskTotal (const Path : string) : int64;

{ ---------------------------------------------------------------- }
(* Windows-Verzeichnisse *)
function WindowsDirectory : string;
function SystemDirectory : string;
function TempDirectory : string;
function UserName : string;
function UserProfile : string;
function AllUsersProfile : string;
function PublicFolder : string;
function ComputerName : string;

{ ---------------------------------------------------------------- }
// nachfolgende Funktionen sind nur ab Vista verf�gbar
// dort erforderlich, um bei WM_QUERYENDSESSION das Abmelden/Herunterfahren zu unterbrechen
function SetShutDownReason (fHandle: hWnd; Reason : string) : boolean;
function ClearShutDownReason (fHandle: hWnd) : boolean;
function QueryShutDownReason (fHandle: hWnd; var Reason : string) : boolean;

{ ---------------------------------------------------------------- }
(*  Get Version Info from File *)
function GetFileVersion (Filename : string; var FileVersionInfo : TFileVersionInfo) : boolean;
function GetFileVersionString (Filename : string; var Version : string) : boolean;
function GetFileVersionName (Filename,DefName,DefVers : string): string;

{ ---------------------------------------------------------------- }
(* ermittle Zeitzonen-Info f�r aktuelle Zone *)
procedure GetTimeZoneInfo (var Zone,DlBias : integer);

{ ---------------------------------------------------------------- }
// Erzeuge einen Eintrag im Event-Log
function ReportToEventLog(Source : string; EventType,CatID,MsgID : cardinal;
                          const Parameters : array of string) : integer;

// Pr�fe, ob f�r AppName ein Eintrag in der Registry unter Eventlog vorhanden ist
function EventLogInstalled(AppName : string) : boolean;

{ ---------------------------------------------------------------- }
// Pr�fe, ob der angemeldete Benutzer mind. zu einer best. Gruppe geh�rt 
function IsAdminLoggedOn : boolean;
function IsPowerUserLoggedOn : boolean;

{ ---------------------------------------------------------------- }
// pr�fe, ob eine Exe-Datei gerade l�uft
function IsExeRunning(const AExeName: string): boolean;

// Liste aller laufenden und sichtbaren Programme
function GetProgramList(const List: TStrings): Boolean;

// Handle von dem in der Z-Reihenfolge vorangehenden sichtbaren Programm
function GetPreviousProgram : HWND;

{ ---------------------------------------------------------------- }
// Get all LUIDs from running processes
function GetLUIDsFromProcesses(ExcludeProcess : dword; var SessionLuidList : TLuidArray) : integer;

{ ---------------------------------------------------------------- }
// Daten des angemeldeten Benutzers ermitteln
function GetUserSessionData (var SessionData : TSessionData) : boolean;
function GetUserLogonTime : TDateTime;
function GetUserSidString : string;

// Enumerate Logon Sessions (exclude all stale logon sessions)
function GetInteractiveUserSessions (var UserSessions : TSessionList) : boolean;
function GetInteractiveUserSessionCount : integer;

{ ---------------------------------------------------------------- }
// Modify privilege of calling process
function ModifyPrivilege (const PrivilegeName : string; Enable : boolean) : boolean;

{ ---------------------------------------------------------------- }
// WinHandle = Window Handle of process
 procedure KillProcessByWinHandle(WinHandle : Hwnd);

implementation

uses System.StrUtils, System.DateUtils, WinApi.TlHelp32, WinApi.PsAPI;

const
  InfoNum = 12;
  InfoStr: array[1..InfoNum] of string = ('CompanyName','FileDescription','FileVersion',
          'InternalName','LegalCopyright','Comments','LegalTradeMarks','OriginalFileName',
          'ProductName','ProductVersion','PrivateBuild','SpecialBuild');

var
  DllHandle : THandle;
  FCreateProcessWithLogonW : TCreateProcessWithLogonW; // erst ab Win 2000
  FSetSuspendState : TSetSuspendState;                 // erst ab Win 2000

{ ---------------------------------------------------------------- }
function GetFileSizeEx; external kernel32 name 'GetFileSizeEx';
function GetTickCount64; external kernel32 name 'GetTickCount64';
function GetVolumeNameForVolumeMountPoint; external kernel32 name 'GetVolumeNameForVolumeMountPointW';
function GetVolumePathName; external kernel32 name 'GetVolumePathNameW';
function GetVolumePathNamesForVolumeName; external kernel32 name 'GetVolumePathNamesForVolumeNameW';

function ConvertStringSidToSid; external advapi32 name 'ConvertStringSidToSidA';
function ConvertSidToStringSid; external advapi32 name 'ConvertSidToStringSidA';

function InitiateSystemShutdownEx; external advapi32 name 'InitiateSystemShutdownExW';
function InitiateSystemShutdownExA; external advapi32 name 'InitiateSystemShutdownExA';
function InitiateSystemShutdownExW; external advapi32 name 'InitiateSystemShutdownExW';

function LsaNtStatusToWinError(Status: cardinal): ULONG; stdcall;
  external advapi32 name 'LsaNtStatusToWinError';

{ ---------------------------------------------------------------- }
function SetSuspendState(Hibernate, ForceCritical, DisableWakeEvent: Boolean): Boolean;
begin
  if assigned(@FSetSuspendState) then Result:=FSetSuspendState(Hibernate,ForceCritical,DisableWakeEvent)
  else Result:=false;
  end;

function CreateProcessWithLogonW;
begin
  if assigned(@FCreateProcessWithLogonW) then begin
    try
      Result:=FCreateProcessWithLogonW(lpUsername,lpDomain,lpPassword,
                 dwLogonFlags,lpApplicationName,lpCommandLine,
                 dwCreationFlags,lpEnvironment,lpCurrentDirectory,
                 lpStartupInfo,lpProcessInformation)
    except
      Result:=false;
      end;
    end
  else Result:=false;
  end;

{ ---------------------------------------------------------------- }
type
  TSDBlockReasonCreate = function(fHandle: hWnd; pwszReason: LPCWSTR): BOOL; stdcall;
  TSDBlockReasonDestroy = function(fHandle: hWnd): BOOL; stdcall;
  TSDBlockReasonQuery= function(fHandle: hWnd; pwszBuff: LPCWSTR; var pcchBuff : DWORD): BOOL; stdcall;

// nachfolgende Funktionen sind nur ab Vista verf�gbar
// dort erforderlich, um bei WM_QUERYENDSESSION das Abmelden/Herunterfahren zu unterbrechen
function SetShutDownReason (fHandle: hWnd; Reason : string) : boolean;
var
  dh  : THandle;
  sdc : TSDBlockReasonCreate;
  ws  : widestring;
begin
  Result:=false;
  dh:=LoadLibrary(user32);
  if dh<>0 then begin
    @sdc:=GetProcAddress(dh,'ShutdownBlockReasonCreate');
    if @sdc<>nil then begin
      ws:=Reason;
      Result:=sdc(fHandle,PWideChar(ws));
      end;
    FreeLibrary(dh);
    end;
  end;

function ClearShutDownReason (fHandle: hWnd) : boolean;
var
  dh  : THandle;
  sdd : TSDBlockReasonDestroy;
begin
  Result:=false;
  dh:=LoadLibrary(user32);
  if dh<>0 then begin
    @sdd:=GetProcAddress(dh,'ShutdownBlockReasonDestroy');
    if @sdd<>nil then Result:=sdd(fHandle);
    FreeLibrary(dh);
    end;
  end;

function QueryShutDownReason (fHandle: hWnd; var Reason : string) : boolean;
var
  dh  : THandle;
  sdq : TSDBlockReasonQuery;
  nBuf : DWORD;
  sBuf : PWideChar;
begin
  Result:=false;
  dh:=LoadLibrary(user32);
  if dh<>0 then begin
    @sdq:=GetProcAddress(dh,'ShutdownBlockReasonQuery');
    if @sdq<>nil then begin
      Result:=sdq(fHandle,nil,nBuf);
      if Result then begin
        sBuf:=StrAlloc(nBuf+1);
        Result:=sdq(fHandle,sBuf,nBuf);
        Reason:=sBuf;
        StrDispose(sBuf);
        end;
      end;
    FreeLibrary(dh);
    end;
  end;

{ ---------------------------------------------------------------- }
// Ermitteln des sprach�bh�ngigen Namens eines Kontos
// sSID : String-Ausdruck f�r entsprechende SID
// siehe dazu WinNT.h und http://www.windowsitpro.com/Article/ArticleID/14781/14781.html
function GetAccountName(sSID : string) : string;
var
  sid        : psid;
  pn,pd      : PChar;
  dn,dd      : dword;
  pu         : SID_NAME_USE;
  ok         : boolean;
begin
  Result:='';
  if ConvertStringSidToSid (pchar(sSID),sid) then begin
    dn:=0; dd:=0;
    ok:=LookupAccountSid(nil,sid,nil,dn,nil,dd,pu);
    if (not ok) and (GetLastError=ERROR_INSUFFICIENT_BUFFER) then begin
      pn:=StrAlloc(dn); pd:=StrAlloc(dd);
      if LookupAccountSid(nil,sid,pn,dn,pd,dd,pu) then Result:=pn
      else RaiseLastOSError;
      StrDispose(pn); StrDispose(pd);
      end
    else RaiseLastOSError;
    LocalFree(HLOCAL(sid));
    end;
  end;

{ ---------------------------------------------------------------- }
{ ------------------------------------------------------------------- }
// following part from JclSysInfo (Project JEDI Code Library)
type
  TVolumeInfoKind = (vikName, vikSerial, vikFileSystem, vikComponentLength);

function GetVolumeInfoHelper(const Drive: string; InfoKind: TVolumeInfoKind): string;
var
  VolumeSerialNumber: DWORD;
  MaximumComponentLength: DWORD;
  Flags: DWORD;
  Name: array [0..MAX_PATH] of Char;
  FileSystem: array [0..15] of Char;
  ErrorMode: Cardinal;
  DriveStr: string;
begin
  { TODO : Change to RootPath }
  { TODO : Perform better checking of Drive param or document that no checking
    is performed. RM Suggested:
    DriveStr := Drive;
    if (Length(Drive) < 2) or (Drive[2] <> ':') then
      DriveStr := GetCurrentFolder;
    DriveStr  := DriveStr[1] + ':\'; }
  Result:='';
  if length(Drive)=0 then Exit;
  if pos(':',Drive)=0 then DriveStr:=Drive + ':'
  else DriveStr:=Drive;
  DriveStr:=IncludeTrailingPathDelimiter(DriveStr);
  ErrorMode:=SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    if GetVolumeInformation(PChar(DriveStr), Name, SizeOf(Name), @VolumeSerialNumber,
      MaximumComponentLength, Flags, FileSystem, SizeOf(FileSystem)) then
    case InfoKind of
      vikName:
        Result:=StrPas(Name);
      vikSerial:
        begin
          Result:=IntToHex(HiWord(VolumeSerialNumber), 4) + '-' +
          IntToHex(LoWord(VolumeSerialNumber), 4);
        end;
      vikFileSystem:
        Result:=StrPas(FileSystem);
      vikComponentLength:
        Result:=IntToStr(MaximumComponentLength);
    end;
  finally
    SetErrorMode(ErrorMode);
  end;
end;

function GetVolumeName(const Drive: string): string;
begin
  Result:=GetVolumeInfoHelper(Drive, vikName);
end;

function GetVolumeSerialNumber(const Drive: string): string;
begin
  Result:=GetVolumeInfoHelper(Drive, vikSerial);
end;

function GetVolumeFileSystem(const Drive: string): string;
begin
  Result:=GetVolumeInfoHelper(Drive, vikFileSystem);
end;

function GetVolumeComponentLength(const Drive: string): string;
begin
  Result:=GetVolumeInfoHelper(Drive, vikComponentLength);
end;

function IsNtfs (const Drive : string) : boolean;
begin
  Result:=AnsiSameText('NTFS',GetVolumeFileSystem(Drive));
  end;

function IsExFat (const Drive : string) : boolean;
begin
  Result:=AnsiSameText('exFAT',GetVolumeFileSystem(Drive));
  end;

function IsLtfs (const Drive : string) : boolean;
begin
  Result:=AnsiSameText('LTFS',GetVolumeFileSystem(Drive));
  end;

// file systems capable to keep files > 4GB
function IsExtFs (const Drive : string) : boolean;
var
  s : string;
begin
  s:=GetVolumeFileSystem(Drive);
  Result:=AnsiSameText('NTFS',s) or AnsiSameText('exFAT',s) or AnsiSameText('LTFS',s);
  end;

{ TODO -cHelp : Donator (incl. TFileSystemFlag[s]): Robert Rossmair }

function GetVolumeFileSystemFlags(const Volume: string): TFileSystemFlags;
const
  FileSystemFlags: array [TFileSystemFlag] of DWORD =
    ( FILE_CASE_SENSITIVE_SEARCH,   // fsCaseSensitive
      FILE_CASE_PRESERVED_NAMES,    // fsCasePreservedNames
      FILE_UNICODE_ON_DISK,         // fsSupportsUnicodeOnDisk
      FILE_PERSISTENT_ACLS,         // fsPersistentACLs
      FILE_FILE_COMPRESSION,        // fsSupportsFileCompression
      FILE_VOLUME_QUOTAS,           // fsSupportsVolumeQuotas
      FILE_SUPPORTS_SPARSE_FILES,   // fsSupportsSparseFiles
      FILE_SUPPORTS_REPARSE_POINTS, // fsSupportsReparsePoints
      FILE_SUPPORTS_REMOTE_STORAGE, // fsSupportsRemoteStorage
      FILE_VOLUME_IS_COMPRESSED,    // fsVolumeIsCompressed
      FILE_SUPPORTS_OBJECT_IDS,     // fsSupportsObjectIds
      FILE_SUPPORTS_ENCRYPTION,     // fsSupportsEncryption
      FILE_NAMED_STREAMS,           // fsSupportsNamedStreams
      FILE_READ_ONLY_VOLUME         // fsVolumeIsReadOnly
    );
var
  MaximumComponentLength, Flags: Cardinal;
  Flag: TFileSystemFlag;
begin
  if not GetVolumeInformation(PChar(IncludeTrailingPathDelimiter(Volume)), nil, 0, nil,
    MaximumComponentLength, Flags, nil, 0) then
    RaiseLastOSError;
  Result:=[];
  for Flag:=Low(TFileSystemFlag) to High(TFileSystemFlag) do
    if (Flags and FileSystemFlags[Flag]) <> 0 then
      Include(Result, Flag);
end;

function GetVolumeUniqueName(const Drive : string) : string;
var
  uname : array [0..MAX_PATH] of Char;
begin
  if (length(Drive)>0) and GetVolumeNameForVolumeMountPoint(PChar(Drive),uname,MAX_PATH+1) then
    Result:=uname
  else Result:='';
  end;

{ ---------------------------------------------------------------- }
(* Disk size and free space *)

function GetDiskFree (const Path : string) : int64 ;
var
  n : Int64;
begin
  GetDiskFreeSpaceEx(pchar(IncludeTrailingPathDelimiter(Path)),Result,n,nil);
  end;

function GetDiskTotal (const Path : string) : int64;
var
  n : Int64;
begin
  GetDiskFreeSpaceEx(pchar(IncludeTrailingPathDelimiter(Path)),n,Result,nil);
  end;

{ ---------------------------------------------------------------- }
(* Windows-Verzeichnisse *)
function WindowsDirectory : string;
var
  p : pchar;
begin
  p:=StrAlloc(MAX_PATH+1);
  GetWindowsDirectory (p,MAX_PATH+1);
  Result:=p;
  Strdispose(p);
  end;

function SystemDirectory : string;
var
  p : pchar;
begin
  p:=StrAlloc(MAX_PATH+1);
  GetSystemDirectory (p,MAX_PATH+1);
  Result:=p;
  Strdispose(p);
  end;

function TempDirectory : string;
var
  p : pchar;
begin
  p:=StrAlloc(MAX_PATH+1);
  GetTempPath(MAX_PATH+1,p);
  Result:=p;
  Strdispose(p);
  end;

function UserName : string;
var
  p : pchar;
  size : dword;
begin
  size:=1024;
  p:=StrAlloc(size);
  GetUserName (p,size);
  Result:=p;
  Strdispose(p);
  end;

function ComputerName : string;
var
  p : pchar;
  size : dword;
begin
  size:=1024;
  p:=StrAlloc(size);
  GetComputerName (p,size);
  Result:=p;
  Strdispose(p);
  end;

function UserProfile : string;
var
  reg  : TRegistry;
  p    : pchar;
  size : dword;
begin
  Result:=WindowsDirectory;
  if IsWinNT then begin
    size:=1024;
    p:=StrAlloc(size);
    ExpandEnvironmentStrings('%USERPROFILE%',p,size);
    if p[0]='%' then Result:=WindowsDirectory else Result:=p;
    StrDispose(p);
    end
  else begin
    Reg:=TRegistry.Create;
    try
      Reg.RootKey:=HKEY_CURRENT_USER;
      if Reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\ProfileReconciliation', True)
        then Result:=Reg.ReadString('ProfileDirectory')
      else Result:=WindowsDirectory;
    finally
      Reg.CloseKey;
      Reg.Free;
      end;
    end;
  end;

function AllUsersProfile : string;
var
  p    : pchar;
  size : dword;
begin
size:=1024;
  p:=StrAlloc(size);
  if ExpandEnvironmentStrings('%ALLUSERSPROFILE%',p,size)=0 then Result:=WindowsDirectory
  else Result:=p;
  StrDispose(p);
  end;

function PublicFolder : string;
var
  p    : pchar;
  size : dword;
begin
  if IsWinNT then begin
    size:=1024;
    p:=StrAlloc(size);
    if ExpandEnvironmentStrings('%PUBLIC%',p,size)=0 then Result:=''
    else Result:=p;
    StrDispose(p);
    end
  else Result:='';
  end;

{ ---------------------------------------------------------------- }
(* Windows-System-Info (Plattform, Version, Build) *)
function IsWinNT : boolean;
begin
  Result:=(Win32Platform=VER_PLATFORM_WIN32_NT) and (Win32MajorVersion>=4);
  end;

function IsWin2000 : boolean;
begin
  Result:=(Win32Platform=VER_PLATFORM_WIN32_NT) and (Win32MajorVersion>=5);
  end;

function IsVista : boolean;
begin
  Result:=(Win32Platform=VER_PLATFORM_WIN32_NT) and (Win32MajorVersion>=6);
  end;

function IsWindows64 : boolean;
var
  KernelModule: HMODULE;
  GetNativeSystemInfoFunc: procedure(var lpSystemInfo: WinApi.Windows.TSystemInfo); stdcall;
  SysInfo: WinApi.Windows.TSystemInfo;
begin
  Result:=False;
  KernelModule:=GetModuleHandle(kernel32);
  GetNativeSystemInfoFunc:=GetProcAddress(KernelModule, 'GetNativeSystemInfo');
  if Assigned(GetNativeSystemInfoFunc) then begin
    GetNativeSystemInfoFunc(SysInfo);
    Result:=SysInfo.wProcessorArchitecture<>0;
    end;
  end;

function Is64BitApp : boolean;
var
  KernelModule: HMODULE;
  GetNativeSystemInfoFunc: procedure(var lpSystemInfo: WinApi.Windows.TSystemInfo); stdcall;
  IsWow64ProcessFunc: function(hProcess: THandle; var Wow64Process: BOOL): BOOL; stdcall;
  Wow64Process: BOOL;
  SysInfo: WinApi.Windows.TSystemInfo;
begin
  Result:=False;
  KernelModule:=GetModuleHandle(kernel32);
  GetNativeSystemInfoFunc:=GetProcAddress(KernelModule, 'GetNativeSystemInfo');
  if Assigned(GetNativeSystemInfoFunc) then begin
    GetNativeSystemInfoFunc(SysInfo);
    if SysInfo.wProcessorArchitecture<>0 then begin
      IsWow64ProcessFunc:=GetProcAddress(KernelModule, 'IsWow64Process');
      if Assigned(IsWow64ProcessFunc) and
        IsWow64ProcessFunc(GetCurrentProcess,Wow64Process) then Result:=not Wow64Process;
      end;
    end;
  end;

{ ---------------------------------------------------------------- }
(*  Get Version Info from File *)
function GetFileVersion (Filename : string; var FileVersionInfo : TFileVersionInfo) : boolean;
var
  t       : string;
  n,Len,i : DWORD;
  Buf     : PByte;
  Value   : PChar;
  vvar    : PWordArray;
begin
  Result:=false;
  if IsWin2000 then begin   // use for Windows 2000 and newer
    FillChar (FileVersionInfo,sizeof(TFileVersionInfo),0);
    n:=GetFileVersionInfoSize(PChar(Filename),n);
    if n > 0 then begin
      Buf:=AllocMem(n);
      GetFileVersionInfo(PChar(Filename),0,n,Buf);     // get buffer
      // get translation code
      VerQueryValue(Buf,PChar('\VarFileInfo\Translation'),pointer(vvar),Len);
      t:=IntToHex(vvar^[0],4)+IntToHex(vvar^[1],4);
      with FileVersionInfo do for i:=1 to InfoNum do begin
        if VerQueryValue(Buf, PChar('\StringFileInfo\'+t+'\'+InfoStr[i]), Pointer(Value), Len) then begin
          case i of
          1 : Company:=value;
          2 : Description:=value;
          3 : Version:=value;
          4 : InternalName:=value;
          5 : Copyright:=value;
          6 : Comments:=value;
            end;
          end;
        end;
      FreeMem(Buf,n);
      Result:=true;
      end;
    end;
  end;

{ ---------------------------------------------------------------- }
(*  Get Version Infostring from File *)
function GetFileVersionString (Filename : string; var Version : string) : boolean;
var
  t       : string;
  n,Len,i : DWORD;
  Buf     : PByte;
  Value   : PChar;
  vvar    : PWordArray;
begin
  Result:=false;  Version:='';
  if IsWin2000 then begin   // use for Windows 2000 and newer
    n:=GetFileVersionInfoSize(PChar(Filename),n);
    if n>0 then begin
      Buf:=AllocMem(n);
      GetFileVersionInfo(PChar(Filename),0,n,Buf);     // get buffer
      // get translation code
      VerQueryValue(Buf,PChar('\VarFileInfo\Translation'),pointer(vvar),Len);
      t:=IntToHex(vvar^[0],4)+IntToHex(vvar^[1],4);
      for i:=1 to InfoNum do begin
        if VerQueryValue(Buf, PChar('\StringFileInfo\'+t+'\'+InfoStr[i]), Pointer(Value), Len) then begin
          if i=3 then Version:=value;
          end;
        end;
      FreeMem(Buf,n);
      Result:=true;
      end;
    end;
  end;

function GetFileVersionName (Filename,DefName,DefVers : string) : string;
var
  VersInfo : TFileVersionInfo;
begin
// Versions-Info
  with VersInfo do if GetFileVersion (Filename,VersInfo) then begin
    // Versionsnr. ohne "Compilierung"
    Version:=ChangeFileExt(Version,'');
    Comments:=' (Vers. '+Version+')';
    end
  else begin
    InternalName:=DefName; Version:=DefVers;
    Comments:=' (Vers. '+DefVers+')';
    end;
  with VersInfo do Result:=InternalName+Comments;
  end;

{ ---------------------------------------------------------------- }
(* ermittle Zeitzonen-Info f�r aktuelle Zone *)
procedure GetTimeZoneInfo (var Zone,DlBias : integer);
var
  TzInfo : TTimeZoneInformation;
  n      : DWord;
begin
  n:=GetTimeZoneInformation(TzInfo);
  Zone:=-TzInfo.Bias div 60;
  if n=TIME_ZONE_ID_DAYLIGHT then DlBias:=TzInfo.DaylightBias
  else DlBias:=0;
  end;

{ ---------------------------------------------------------------- }
// Erzeuge einen Eintrag im Event-Log
// Result = 0:  ok
//        > 0:  System-Fehlercode
function ReportToEventLog(Source : string; EventType,CatID,MsgID : cardinal;
                          const Parameters : array of string) : integer;
var
  hEventLog : THandle;
  pmsgArray : array of PWideChar;
begin
  Result:=NO_ERROR;
  hEventLog:=RegisterEventSource(nil,pchar(Source));
  if hEventLog<>0 then begin
    SetLength(pmsgArray,length(Parameters));
    if not ReportEvent(hEventLog,EventType,CatID,MsgID,nil,
      length(Parameters),0,@Parameters,nil) then Result:=GetLastError;
    DeregisterEventSource(hEventLog);
    end;
  end;

// Pr�fe, ob f�r AppName ein Eintrag in der Registry unter Eventlog vorhanden ist
function EventLogInstalled(AppName : string) : boolean;
begin
  with TRegistry.Create do begin
    Access:=KEY_READ;
    RootKey:=HKEY_LOCAL_MACHINE;
    try
      Result:=OpenKey('SYSTEM\CurrentControlSet\Services\Eventlog\Application\'+AppName,false);
    finally
      Free;
      end;
    end;
  end;

{ ---------------------------------------------------------------- }
// from Inno-Setup file CmnFunc2.pas
function IsMemberOfGroup(const DomainAliasRid : DWORD): Boolean;
{ Returns True if the logged-on user is a member of the specified local
  group. Always returns True on Windows 9x/Me. }
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority =
    (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  SE_GROUP_ENABLED           = $00000004;
  SE_GROUP_USE_FOR_DENY_ONLY = $00000010;
var
  Sid: PSID;
  CheckTokenMembership: function(TokenHandle: THandle; SidToCheck: PSID;
    var IsMember: BOOL): BOOL; stdcall;
  IsMember: BOOL;
  Token: THandle;
  GroupInfoSize: DWORD;
  GroupInfo: PTokenGroups;
  I: Integer;
begin
  if Win32Platform <> VER_PLATFORM_WIN32_NT then begin
    Result:=True;
    Exit;
  end;

  Result:=False;

  if not AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
     SECURITY_BUILTIN_DOMAIN_RID, DomainAliasRid,
     0, 0, 0, 0, 0, 0, Sid) then Exit;
  try
    { Use CheckTokenMembership if available. MSDN states:
      "The CheckTokenMembership function should be used with Windows 2000 and
      later to determine whether a specified SID is present and enabled in an
      access token. This function eliminates potential misinterpretations of
      the active group membership if changes to access tokens are made in
      future releases." }
    CheckTokenMembership:=nil;
    if  Win32MajorVersion>=5 then
      CheckTokenMembership:=GetProcAddress(GetModuleHandle(advapi32),'CheckTokenMembership');
    if Assigned(CheckTokenMembership) then begin
      if CheckTokenMembership(0, Sid, IsMember) then Result:=IsMember;
      end
    else begin
      GroupInfo:=nil;
      if not OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, Token) then begin
        if GetLastError<>ERROR_NO_TOKEN then Exit;
        if not OpenProcessToken(GetCurrentProcess, TOKEN_QUERY,Token) then Exit;
        end;
      try
        GroupInfoSize:=0;
        if not GetTokenInformation(Token, TokenGroups, nil, 0, GroupInfoSize) and
           (GetLastError<>ERROR_INSUFFICIENT_BUFFER) then Exit;
        GetMem(GroupInfo, GroupInfoSize);
        if not GetTokenInformation(Token, TokenGroups, GroupInfo,
           GroupInfoSize, GroupInfoSize) then Exit;
        for I:=0 to GroupInfo.GroupCount-1 do begin
          if EqualSid(Sid, GroupInfo.Groups[I].Sid) and
             (GroupInfo.Groups[I].Attributes and (SE_GROUP_ENABLED or
              SE_GROUP_USE_FOR_DENY_ONLY) = SE_GROUP_ENABLED) then begin
            Result:=True;
            Break;
            end;
          end;
      finally
        FreeMem(GroupInfo);
        CloseHandle(Token);
      end;
    end;
  finally
    FreeSid(Sid);
  end;
end;

// User belongs to group Administrators
function IsAdminLoggedOn : Boolean;
{ Returns True if the logged-on user is a member of the Administrators local
  group. Always returns True on Windows 9x/Me. }
begin
  Result:=IsMemberOfGroup(DOMAIN_ALIAS_RID_ADMINS);
  end;

// User belongs to at least to group Power Users
function IsPowerUserLoggedOn : Boolean;
begin
  Result:=IsMemberOfGroup(DOMAIN_ALIAS_RID_POWER_USERS) or
     IsMemberOfGroup(DOMAIN_ALIAS_RID_ADMINS);
  end;

{ ---------------------------------------------------------------- }
// pr�fe, ob eine Exe-Datei gerade l�uft
function IsExeRunning(const AExeName: string) : boolean;
var
  h: THandle;
  p: TProcessEntry32;
begin
  Result:=False;
  p.dwSize:=SizeOf(p);
  h:=CreateToolHelp32Snapshot(TH32CS_SnapProcess, 0);
  try
    Process32First(h, p);
    repeat
      Result:=AnsiSameText(AExeName,p.szExeFile);
    until Result or (not Process32Next(h, p));
  finally
    CloseHandle(h);
    end;
  end;

{ ---------------------------------------------------------------- }
function IsMainAppWindow(Wnd: THandle) : Boolean;
var
  ParentWnd: THandle;
  ExStyle: DWORD;
begin
  if IsWindowVisible(Wnd) then begin
    ParentWnd:=GetWindowLong(Wnd, GWL_HWNDPARENT);
    ExStyle:=GetWindowLong(Wnd, GWL_EXSTYLE);
    Result:=((ParentWnd = 0) or (ParentWnd = GetDesktopWindow)) and
      ((ExStyle and WS_EX_TOOLWINDOW = 0) or (ExStyle and WS_EX_APPWINDOW <> 0));
    end
  else Result:=False;
  end;

{ ---------------------------------------------------------------- }
// get list of all running visible processes
function GetProgramList(const List: TStrings) : Boolean;

  function EnumWindowsProc(Wnd: THandle; List: TStrings): Boolean; stdcall;
  var
    Caption: array [0..1024] of Char;
  begin
    if IsMainAppWindow(Wnd) and (GetWindowText(Wnd, Caption, SizeOf(Caption)) > 0) then
      List.AddObject(Caption,Pointer(Wnd));
    Result:=True;
  end;

begin
List.BeginUpdate;
  try
    Result:=EnumWindows(@EnumWindowsProc, Integer(List));
  finally
    List.EndUpdate;
  end;
end;

// get handle to previous visible process (like Ctrl + Alt)
function GetPreviousProgram : HWND;
var
  sl : TStringList;
begin
  sl:=TStringList.Create;
  Result:=0;
  if GetProgramList(sl) then with sl do if Count>1 then Result:=HWND(Objects[1]);
  sl.Free;
  end;

{ ------------------------------------------------------------------- }
// convert Filetime to Delphi time (TDateTime)
function FileTimeToDateTime (ft : TFileTime) : TDateTime;
var
  st : TSystemTime;
begin
  if not (FileTimeToSystemTime(ft,st) and TrySystemTimeToDateTime(st,Result)) then
    Result:=EncodeDate(1899,12,30);
  end;

{ ---------------------------------------------------------------- }
// Daten des angemeldeten Benutzers ermitteln
function GetUserSessionData (var SessionData : TSessionData) : boolean;
var
  Count: cardinal;
  SessionList,Luid: PLUID;
  PSesDat: PSecurityLogonSessionData;
  sid : cardinal;
  SizeNeeded, SizeNeeded2: DWORD;
  OwnerName, DomainName: PChar;
  OwnerType: SID_NAME_USE;
  pBuffer: Pointer;
  pBytesreturned: DWord;
  LocalFileTime: TFileTime;
  Secur32Handle,Wtsapi32Handle : THandle;
  FWTSQuerySessionInformation : TWTSQuerySessionInformation; // ab Win XP
  FWTSGetActiveConsoleSessionId : TWTSGetActiveConsoleSessionId; // ab Win XP
  FLsaEnumerateLogonSessions : TLsaEnumerateLogonSessions; // ab Win XP
  FLsaGetLogonSessionData : TLsaGetLogonSessionData;      // ab Win 2000
  FLsaFreeReturnBuffer : TLsaFreeReturnBuffer;            // ab Win 2000
begin
  result:=false;
  Secur32Handle:=LoadLibrary(secur32);
  if Secur32Handle=0 then Exit;
  FLsaEnumerateLogonSessions:=GetProcAddress(Secur32Handle,'LsaEnumerateLogonSessions');
  if not assigned(FLsaEnumerateLogonSessions) then Exit;
  FLsaGetLogonSessionData:=GetProcAddress(Secur32Handle,'LsaGetLogonSessionData');
  if not assigned(FLsaGetLogonSessionData) then Exit;
  FLsaFreeReturnBuffer:=GetProcAddress(Secur32Handle,'LsaFreeReturnBuffer');
  if not assigned(FLsaFreeReturnBuffer) then Exit;
  Wtsapi32Handle:=LoadLibrary(wtsapi32);
  if Wtsapi32Handle=0 then Exit;
  FWTSQuerySessionInformation:=GetProcAddress(Wtsapi32Handle,'WTSQuerySessionInformationW');
  if not assigned(FWTSQuerySessionInformation) then Exit;
  DllHandle:=GetModuleHandle(kernel32);
  if DllHandle=0 then Exit;
  FWTSGetActiveConsoleSessionId:=GetProcAddress(DllHandle,'WTSGetActiveConsoleSessionId');
  if not assigned(FWTSGetActiveConsoleSessionId) then Exit;
  //Auflisten der LogOnSessions
  try
    if (LsaNtStatusToWinError(FLsaEnumerateLogonSessions(Count,SessionList))=0) then begin
      Luid:=SessionList;
      if Count > 0 then repeat
        // Pr�fe auf m�gliche Fehler (z.B. Access denied)
        if LsaNtStatusToWinError(FLsaGetLogonSessionData(Luid,PSesDat))=0 then begin
          // Pr�fe, ob es sich um eine Konsolen- oder Remote-Anmeldung handelt
          if (PSesDat^.LogonType=Interactive) or (PSesDat^.LogonType=RemoteInteractive) then begin
            SizeNeeded:=MAX_PATH;
            SizeNeeded2:= MAX_PATH;
            GetMem(OwnerName, MAX_PATH);
            GetMem(DomainName, MAX_PATH);
            try
              if LookupAccountSID(nil, PSesDat^.SID, OwnerName,SizeNeeded,DomainName,SizeNeeded2,
                              OwnerType) then begin
                // Pr�fen ob es sich um einen Benutzer handelt und ob es die
                // SessionId des aufrufenden Prozesses ist
                if (OwnerType=1) and ProcessIdToSessionId(GetCurrentProcessId,sid)
                    and(PSesDat^.Session=sid) then begin
                  if FWTSQuerySessionInformation(WTS_CURRENT_SERVER_HANDLE,
                      PSesDat^.Session, WTSConnectState,pBuffer,pBytesreturned) then begin
                    if WTS_CONNECTSTATE_CLASS(pBuffer^) = WTSActive then with SessionData do begin
                      UserLuid:=Luid^;
                      UserName:=PSesDat^.UserName.Buffer;
                      DOmain:=PSesDat^.LogonDomain.Buffer;
                      LogonType:=PSesDat^.LogonType;
                      LogonTime:=Now;
                      if FileTimeToLocalFileTime(TFileTime(PSesDat^.LogonTime),LocalFileTime) then
                        LogonTime:=FileTimeToDateTime(LocalFileTime);
                      result:=true;
                      end;
                    end;
                  FLSAFreeReturnBuffer(pBuffer);
                  end;
                end;
            finally
              FreeMem(OwnerName);
              FreeMem(DomainName);
              end;
            end;
          end;
        inc(Luid);
        dec(Count);
        try
          FLSAFreeReturnBuffer(PSesDat);
        except
          end;
        until (Count=0) or result;
      end;
  finally
    FLSAFreeReturnBuffer(SessionList);
    FreeLibrary(Wtsapi32Handle); FreeLibrary(Secur32Handle);
    end;
  end;

function GetUserLogonTime : TDateTime;
var
  sd : TSessionData;
begin
  if GetUserSessionData(sd) then Result:=sd.LogonTime
  else Result:=Now;
  end;

function GetUserSidString : string;
const
  SECURITY_NT_AUTHORITY = 5;
type
  TSID = packed record
    Revision: Byte;
    SubAuthorityCount: Byte;
    IdentifierAuthority: TSIDIdentifierAuthority;
    SubAuthority: array [Byte] of DWORD; // [0..SubAuthorityCount - 1]
    end;
  PSID = ^TSID;

  TTokenUser = record
    User: TSIDAndAttributes;
    end;
  PTokenUser = ^TTokenUser;
var
  Token: THandle;
  User: PTokenUser;
  Size: DWORD;
  Loop: Byte;
begin
  Result:='';
  if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, Token) then begin
    Size:=4096;
    GetMem(User, Size);
    try
      FillChar(User^, Size, 0);
      if GetTokenInformation(Token, TokenUser, User, Size, Size) then
          with PSID(User^.User.Sid)^ do begin
        Result:='S-' + IntToStr(Revision) + '-' +
        IntToStr(IdentifierAuthority.Value[SECURITY_NT_AUTHORITY]);
        for Loop:=0 to SubAuthorityCount - 1 do
        Result:=Result+'-'+IntToStr(SubAuthority[Loop]);
        end;
    finally
      FreeMem(User);
      CloseHandle(Token)
      end;
    end;
  end;

{ ---------------------------------------------------------------- }
// Compare locally Unique Identifier
function SameLuid(Luid1,Luid2 : TLuid): boolean;
begin
  Result:=(Luid1.LowPart=Luid2.LowPart) and (Luid1.HighPart=Luid2.HighPart);
  end;

// Check if there processes belonging to LogonId
function HasProcess(LogonId : TLuid; ProcSessions : TLuidArray) : boolean;
var
  i : integer;
begin
  Result:=true;
  for i:=Low(ProcSessions) to High(ProcSessions) do if SameLuid(LogonID,ProcSessions[i]) then Exit;
  Result:=false;
  end;

// Get all LUIDs from running processes
function GetLUIDsFromProcesses(ExcludeProcess : dword; var SessionLuidList : TLuidArray) : integer;
var
  hToken : THandle;
  dwSize,k : dword;
  n,i,j  : integer;
  lpdwPIDs,
  luid     : LPDWORD;
  hProcess : THandle;
  ts : TTokenStatistics;
  fnd : boolean;
begin
  Result:=NO_ERROR;
  k:=256*sizeof(dword);
  lpdwPIDs:=nil;
  repeat
    if lpdwPIDs<>nil then begin
      FreeMem(lpdwPIDs);
      k:=2*k;
      end;
    GetMem(lpdwPIDs,k);
    if not EnumProcesses(lpdwPIDs,k,dwSize) then begin
      Result:=GetLastError; Exit;
      end;
    until k<>dwSize;
  luid:=lpdwPIDs;
  dwSize:=dwSize div sizeof(dword);
  n:=0;
  setlength(SessionLuidList,dwSize);
  for i:=0 to dwSize-1 do if (ExcludeProcess>0) and (ExcludeProcess<>luid^) then begin
    with SessionLuidList[i] do begin
      LowPart:=0; HighPart:=0;
      end;
    hProcess:=OpenProcess(PROCESS_QUERY_INFORMATION,false,luid^);
    if hProcess<>0 then begin
      if OpenProcessToken(hProcess,TOKEN_QUERY,hToken) then begin
        if GetTokenInformation(hToken,TokenStatistics,@ts,sizeof(ts),k) then begin
          fnd:=false;
          for j:=0 to n-1 do if SameLuid(SessionLuidList[j],ts.AuthenticationId) then begin
            fnd:=true; break;
            end;
          if not fnd then begin
            SessionLuidList[n]:=ts.AuthenticationId;
            inc(n);
            end;
          end;
        CloseHandle(hToken);
        end;
      CloseHandle(hProcess);
      end;
    inc(luid);
    end;
  if lpdwPIDs<>nil then FreeMem(lpdwPIDs);
  setlength(SessionLuidList,n);
  end;

// Enumerate Logon Sessions
// exclude all stale logon sessions
// see: http://www.codeproject.com/Articles/7483/Enumerating-Logon-Sessions
// and http://en.verysource.com/code/4448928_1/logonsessiondata.cpp.html
function GetInteractiveUserSessions (var UserSessions : TSessionList) : boolean;
var
  Count : cardinal;
  i : integer;
  LocalFileTime : TFileTime;
  SessionList,Luid: PLUID;
  ProcSessions : TLuidArray;
  PSesDat: PSecurityLogonSessionData;
  Secur32Handle : THandle;
  FLsaEnumerateLogonSessions : TLsaEnumerateLogonSessions; // ab Win XP
  FLsaGetLogonSessionData : TLsaGetLogonSessionData;      // ab Win 2000
  FLsaFreeReturnBuffer : TLsaFreeReturnBuffer;            // ab Win 2000
begin
  Result:=false;
  Secur32Handle:=LoadLibrary(secur32);
  if Secur32Handle=0 then Exit;
  FLsaEnumerateLogonSessions:=GetProcAddress(Secur32Handle,'LsaEnumerateLogonSessions');
  if not assigned(FLsaEnumerateLogonSessions) then Exit;
  FLsaGetLogonSessionData:=GetProcAddress(Secur32Handle,'LsaGetLogonSessionData');
  if not assigned(FLsaGetLogonSessionData) then Exit;
  FLsaFreeReturnBuffer:=GetProcAddress(Secur32Handle,'LsaFreeReturnBuffer');
  if not assigned(FLsaFreeReturnBuffer) then Exit;
  // Get LUIDs from running processes
  if GetLUIDsFromProcesses(GetCurrentProcessId,ProcSessions)<>NO_ERROR then Exit;
  // Enumerate LogOnSessions
  try
    if (LsaNtStatusToWinError(FLsaEnumerateLogonSessions(Count,SessionList))=0) then begin
      Luid:=SessionList;
      for i:=0 to Count-1 do begin
        // Pr�fe auf m�gliche Fehler (z.B. Access denied)
        if LsaNtStatusToWinError(FLsaGetLogonSessionData(Luid,PSesDat))=0 then begin
          // Pr�fe, ob es sich um eine Konsolen- oder Remote-Anmeldung handelt
          if ((PSesDat^.LogonType=Interactive) or (PSesDat^.LogonType=RemoteInteractive))
              and HasProcess(PSesDat^.LogonId,ProcSessions) then begin
            setlength(UserSessions,length(UserSessions)+1);
            with UserSessions[High(UserSessions)] do begin
              UserLuid:=Luid^;
              UserName:=PSesDat^.UserName.Buffer;
              DOmain:=PSesDat^.LogonDomain.Buffer;
              LogonType:=PSesDat^.LogonType;
              LogonTime:=Now;
              if FileTimeToLocalFileTime(TFileTime(PSesDat^.LogonTime),LocalFileTime) then
                LogonTime:=FileTimeToDateTime(LocalFileTime);
              Result:=true;
              end;
            end;
          end;
        inc(Luid);
        try FLSAFreeReturnBuffer(PSesDat); except end;
        end;
      end;
  finally
    FLSAFreeReturnBuffer(SessionList);
    FreeLibrary(Secur32Handle);
    end;
  end;

function GetInteractiveUserSessionCount : integer;
var
  UserSessions : TSessionList;
begin
  UserSessions:=nil;
  if GetInteractiveUserSessions (UserSessions) then Result:=length(UserSessions)
  else Result:=-1;
  UserSessions:=nil;
  end;

{ ---------------------------------------------------------------- }
// Modify privilege of calling process
function ModifyPrivilege (const PrivilegeName : string; Enable : boolean) : boolean;
var
  hToken : THandle;
  tkp    : TTokenPrivileges;
  n      : dword;
begin
  Result:=OpenProcessToken(GetCurrentProcess,TOKEN_ADJUST_PRIVILEGES,hToken);
  if Result then begin
  // Get the LUID for the backup privilege.
    Result:=LookupPrivilegeValue(nil,PChar(PrivilegeName),tkp.Privileges[0].Luid);
    if Result then begin
      tkp.PrivilegeCount:=1;  // one privilege to set
      with tkp.Privileges[0] do if Enable then Attributes:=SE_PRIVILEGE_ENABLED
      else Attributes:=0;
    // Get the backup privilege for this process.
      Result:=AdjustTokenPrivileges(hToken,FALSE,tkp,0,nil,n);
      end;
    CloseHandle(hToken);
    end
  end;

{ ---------------------------------------------------------------- }
// wh = Window Handle, nicht Process Handle (wie erforderlich)!
 procedure KillProcessByWinHandle(WinHandle : Hwnd);
 var
   pid: Cardinal;
   ProcessHandle: THandle;
 begin
   GetWindowThreadProcessId(WinHandle,@pid);
   ProcessHandle := OpenProcess(PROCESS_TERMINATE,FALSE,pid);
   TerminateProcess(ProcessHandle,4);
   end;

{ ---------------------------------------------------------------- }
initialization
  DllHandle:=GetModuleHandle(advapi32);
  if DllHandle<>0 then
    @FCreateProcessWithLogonW:=GetProcAddress(DllHandle,'CreateProcessWithLogonW')
  else FCreateProcessWithLogonW:=nil;

  DllHandle:=LoadLibrary(powrprof);
  if DllHandle<>0 then begin
    @FSetSuspendState:=GetProcAddress(DllHandle,'SetSuspendState');
    end
  else begin
    FSetSuspendState:=nil;
    end;
  end.


