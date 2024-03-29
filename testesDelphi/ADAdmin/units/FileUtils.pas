(* Delphi Unit (Unicode)
   procedures and functions for file and directory processing
   ==========================================================

   � Dr. J. Rathlev, D-24222 Schwentinental (info(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1 -   Feb. 2010
         2.0 - Nov. 2011: functions for optional debug logging (Compiler switch: trace)
                          new function "CheckForParsePoint"
         2.1 - May  2012: changed "OpenDebugLog"
         2.2 - Jun. 2012: new function "FileTimeToFileAge"
                          Check for identical filenames in CopyFileTS
         2.3 - Jul. 2012: Timestamp routines fixed for unvalid DateTime
         2.4 - Oct. 2012: added overload functions "SetDirectoryAge" and
                          "SetFileAge" with FileTime argument
         2.5 - Dec. 2012: "FollowLink" added to "GetFileTimestamps" and "GetDirTimestamps", 
                          new functions "FileDate", "FileTimeStamp" and "DirectoryTimeStamp"
         2.6 - Jan. 2013: LastAccesTime added to SetFileTimestamps
         2.7 - Apr. 2013: "DeleteMatchingFiles" returns number of deleted files,
                          new function "DeleteOlderFiles"
         2.8 - Jun. 2013: new function "LocalFileTimeToDateTime",
                          no function with "SetDirectoryAge" on root paths
         3.0 - Jul. 2013: new function "CopyFileAcl"
                          added all file and directory related functions from JrUtils
         3.1 - Apr. 2015: Updated: Reparse points and junctions
         4.0 - Jan. 2016: reorganized code, e.g. use of GetFileAttributesEx
                          handling of extended-length paths in unit XlFileUtils
                          new file for resource strings: FileConsts

   *)

unit FileUtils;

interface

uses WinApi.Windows, System.Classes, System.SysUtils, System.IniFiles;

const
  Punkt = '.';
  DPunkt = ':';
  BSlash = '\';
  IllegalFilenameChars = [#0..#31,'*','?','<','>',':','"','/','\','|'];

  FILE_WRITE_ATTRIBUTES = $0100;
  defBlockSize = 256*1024;

  faAllFiles  = faArchive+faReadOnly+faHidden+faSysfile;
  faNoArchive = faReadOnly+faHidden+faSysfile;

  UcBom = $FEFF;   //UniCode-Signatur (Low Endian)
  Utf8BomS : RawByteString = #$EF#$BB#$BF;
  Utf8Bom : array[0..2] of byte = ($EF,$BB,$BF);

  cpUtf8 = 65001;
  cpUtf16LE = 1200;
  cpUtf16BE = 1201;

type
{ ------------------------------------------------------------------- }
  TUnicodeIniFile = class(TIniFile)
    FFileName : string;
    constructor CreateForRead (const FileName: string);
    constructor CreateForWrite (const FileName: string);
    function ReadString(const Section, Ident, Default: string): string; override;
  end;

  TInt64 = record
    case integer of
    0: (AsInt64 : int64);
    1: (Lo, Hi  : Cardinal);
    2: (Cardinals: array [0..1] of Cardinal);
    3: (Words: array [0..3] of Word);
    4: (Bytes: array [0..7] of Byte);
    5 :(FileTime : TFileTime);
    end;

  TFileTimestamps = record
    Valid          : boolean;
    CreationTime,
    LastAccessTime,
    LastWriteTime  : TFileTime;
    end;

  TFileInfo = record
    Name     : string;
    DateTime : TDateTime;
    FileTime : TFileTime;
    Size     : int64;
    Attr,Res : cardinal;
    end;

  TReparseType = (rtNone,rtJunction,rtSymbolic);

{ ---------------------------------------------------------------- }
  ECopyError = class(EInOutError);

{ ---------------------------------------------------------------- }
// Write opt. Debug Log
{$IFDEF Trace}
procedure OpenDebugLog (Filename : string);
procedure WriteDebugLog (const DebugText: string);
procedure CloseDebugLog;
{$ENDIF}

{ ---------------------------------------------------------------- }
function IsAnsiStr (ws : string) : boolean;
function WideUpCase (c : WideChar) : WideChar;

{ ------------------------------------------------------------------- }
function WriteUtf8Bom (const Filename : string) : integer;
function CheckUtf8Bom (const Filename : string) : boolean;

{ ---------------------------------------------------------------- }
// convert Filetime to Delphi time (TDateTime)
function FileTimeToDateTime (ft : TFileTime; var dt : TDateTime) : boolean;
function GetFileTimeToDateTime (ft : TFileTime) : TDateTime;
function FileTimeToLocalDateTime (ft : TFileTime; var dt : TDateTime) : boolean;

// convert Delphi time (TDateTime) to Filetime
function DateTimeToFileTime (dt : TDateTime) : TFileTime;
function LocalDateTimeToFileTime (dt : TDateTime) : TFileTime;

// FileDateToDateTime with error check
function DateTimeFromFileDate(FileDate : integer) : TDateTime;

// convert Delphi time to Unix time and reverse
function DateTimeToUnixTime (dt : TDateTime) : cardinal;
function UnixTimeToDateTime (ut : cardinal) : TDateTime;

// convert Fileage to Unix time
function FileAgeToUnixTime (Age : cardinal) : cardinal;
function FileTimeToFileAge (ft : TFileTime) : cardinal;

// convert Filetime to Unix time and reverse
function FileTimeToUnixTime (ft : TFileTime) : cardinal;
function UnixTimeToFileTime (ut : cardinal) : TFileTime;

// convert Filetime to seconds
function FileTimeToSeconds (ft : TFileTime) : cardinal;

{ ---------------------------------------------------------------- }
// get file size, last write time and attributes
function GetFileInfos(const FileName : string; var FSize : int64;
                      var FTime : TFileTime; var FAttr : cardinal) : boolean; overload;

function GetFileInfos(const FileName : string; var FSize : int64;
                      var FTime : TFileTime; var FAttr,FReserved : cardinal) : boolean; overload;

function GetFileInfos(const FileName : string; var FileInfo : TFileInfo) : boolean; overload;

// get time (UTC) of last file write
function GetFileLastWriteDateTime(const FileName: string): TDateTime;

// get time (UTC) of last file write
function GetFileLastWriteTime(const FileName: string) : TFileTime;

// set time (UTC) of last file write
function SetFileLastWriteDateTime(const FileName: string; FileTime : TDateTime) : integer;

// set time (UTC) of last file write
function SetFileLastWriteTime(const FileName: string; FileTime : TFileTime;
                              CheckTime : boolean = false) : integer;

// get file timestamps (UTC)
function GetFileTimestamps(const FileName: string; FollowLink: Boolean = True): TFileTimestamps;

// get directory timestamps (UTC)
function GetDirTimestamps(const DirName: string; FollowLink: Boolean = True): TFileTimestamps;

// get time (UTC) of last directory change
function GetDirLastChangeTime(const DirName: string): TFileTime;

// get file or directory timestamps (UTC) from FindData
function GetTimestampsFromFindData(const FindData : TWin32FindData) : TFileTimestamps;

// set file timestamps (UTC)
function SetFileTimestamps (const FileName: string; Timestamps : TFileTimestamps;
                            CheckTime,SetCreationTime : boolean; FollowLink : Boolean = True) : integer;

// Get time stamp of file or directory (see FileAge)
function FileGetDateTime(const FileName: string; FollowLink : Boolean = True): TDateTime;
function DirectoryGetDateTime(const FileName: string; FollowLink : Boolean = True): TDateTime;

// get DOS file age from file data
function GetFileAge (const FileData: TWin32FindData) : integer; overload;

function DirectoryAge(const DirName: string; FollowLink : Boolean = True): Integer; overload;
function DirectoryAge(const DirName: string; out FileDateTime: TDateTime; FollowLink : Boolean = True): Boolean; overload;

// Set time stamp of file or directory (see FileSetDate)
function SetFileAge(const FileName: string; Age: Integer; FollowLink : Boolean = True): Integer; overload;
function SetDirectoryAge(const DirName: string; Age: Integer; FollowLink : Boolean = True): Integer; overload;

function SetFileAge(const FileName: string; Age: FileTime; FollowLink : Boolean = True): Integer; overload;
function SetDirectoryAge(const DirName: string; Age: FileTime; FollowLink : Boolean = True): Integer; overload;

{ ---------------------------------------------------------------- }
// convert file attribute to string
function FileAttrToString(Attr : word) : string;

// Clear = true  : clear given attributes
//       = false : set given attributes
function FileChangeAttr (const FileName: string; Attr: Integer; Clear : boolean;
                         FollowLink: Boolean = True) : Integer;

// read file infos (WIN32_FIND_DATA)
function GetFileData(FileName : string; var FileData : TWin32FindData) : boolean;

function GetFileAttrData(const FileName : string; var FileData : TWin32FileAttributeData;
                     FollowLink: Boolean = True) : integer;

{ ---------------------------------------------------------------- }
// get file size as int64
function GetFileSize (const FileData: TWin32FindData) : int64; overload;
function GetFileSize (const FileData: TWin32FileAttributeData) : int64; overload;
function LongFileSize (const FileName : string) : int64;

// Convert SearchRec to FileInfo
function SearchRecToFileInfo (SearchRec : TSearchRec) : TFileInfo;

// set timestamp and attrtibutes from SearcRec
function SetTimestampAndAttr(const FileName : string; SearchRec : TSearchRec) : integer;

{ ---------------------------------------------------------------- }
// Delete file even if readonly
function EraseFile(const FileName: string): Boolean;

// Delete all matching files in directory
function DeleteMatchingFiles(const APath,AMask : string) : integer;

// Delete all files in directory older than date
function DeleteOlderFiles(const APath,AMask : string; ADate : TDateTime) : integer;

{ ---------------------------------------------------------------- }
// Pr�fe, ob Textdatei vorhanden
function ExistsFile (var f : TextFile) : boolean;

// Pr�fen, ob in einem Verzeichnis eine Datei existiert, die der Maske entspricht
function FileMatchesMask (const Dir,Mask : string) : string;

{ ---------------------------------------------------------------- }
// Copy file with timestamp and attributes *)
procedure CopyFileTS (const SrcFilename,DestFilename : string;
                      AAttr : integer = -1; BlockSize : integer = defBlockSize);

{ ---------------------------------------------------------------- }
// Copy files from one directory to another
function CopyFiles (const FromDir,ToDir,AMask : string; OverWrite : boolean) : boolean;

{ ---------------------------------------------------------------- }
// Copy file permissions (ACL)
function CopyFileAcl (const srcfilename,destfilename : string) : cardinal;

{ ---------------------------------------------------------------- }
// Bei nicht leeren Pfaden Backslash erg�nzen
function SetDirName (const Dir : string) : string;

// Verzeichnisse vergleichen
function SameDirName (const S1, S2: string): Boolean;

{ ---------------------------------------------------------------- }
// Ausmaskieren der Erweiterung
function GetExt (const Name : string) : string;

// Dateinamenerweiterung entfernen
function DelExt (Name : string) : string;

// �ndern einer Dateinamenerweiterung
function NewExt (Name,Ext  : string) : string;

// Hinzuf�gen einer Dateinamenerweiterung
function AddExt (const Name,Ext  : string; Always : boolean = false) : string;

// Hinzuf�gen eines Pfades zum Dateinamen
function AddPath (Path,Name : string) : string;

// Dateiname mit Laufwerk, Verzeichnis und Extension (falls nicht vorhanden) erweitern
function Erweiter (Pfad,Name,Ext : string) : string;

// Suffix zu Dateinamen hinzuf�gen
function AddNameSuffix (FName,Suffix,Ext : string) : string;

// Suffix zu Dateinamen mit Extension hinzuf�gen
function InsertNameSuffix (FName,Suffix : string) : string;

(* Suffix (L�nge: CharCount) von Dateinamen mit Extension entfernen *)
function RemoveNameSuffix (FName : string; CharCount : integer) : string;

// FName startet mit Prefix
function HasPrefix(const Pref,FName : string) : boolean;

// Pfad aus Dateinamen teilweise entfernen, wenn der Name zu lang wird
function StripPath (Name : String;
                    Len  : integer) : String;

// Extrahiere den �bergeordneten Pfad
function ExtractParentPath (Dir : string) : string;

// Extrahiere das letzte Unterverzeichnis
function ExtractLastDir (const Path : string) : string;

// pr�fe auf untergeordneten Pfad
function IsSubPath (const Path1,Path2 : string) : boolean;

// pr�fe auf Root-Pfad (z.B. "C:\")
function IsRootPath (const Path : string) : boolean;

// enth�lt vollst�ndigen Pfad
function ContainsFullPath (const Name : string) : boolean;

// abs. Pfad in rel. Pfad umwandel und umgekehrt
function MakeRelativePath (const BaseName,DestName: string) : string;
function MakeAbsolutePath (const BaseName,DestName: string) : string;
function ExpandPath (const DestName: string) : string;

(* pr�fe, ob alle Zeichen in APath zul�ssig sind *)
function CheckPathChars (APath : string) : boolean;

// pr�fe, ob alle Zeichen in Filename zul�ssig sind
function CheckFilename (const Filename : string) : boolean;

// Ersetze ung�ltige Zeichen in Filename durch AChar
function ReplaceIllegalChars (const Filename : string; AChar : char) : string;

// identischen Pfadanteil am Anfang extrahieren
function ExtractSamePath(const Path1,Path2 : string) : string;

// Laufwerkesbuchstaben ersetzen
function ReplaceDrive(const Path,Drive : string) : string;

{ ---------------------------------------------------------------- }
// kein spezielles Verzeichnis (selbst und eins zur�ck)
function NotSpecialDir (const Name : string; StartChars : array of char) : boolean; overload;
function NotSpecialDir (const Name : string) : boolean; overload;

{ ---------------------------------------------------------------- }
// get the type of the reparse point (junction or symbolic)
function GetReparsePointType(const FileName : string) : TReparseType;

// Check if reparse point and return linked path if not recursive
function CheckForReparsePoint (const Path : string; Attr : integer;
                               var LinkPath : string; var RpType : TReparseType) : boolean;

// Check if reparse point is recursive
function IsRecursiveReparsePoint (const Path : string; Attr : integer; AllowJunctions : boolean) : boolean; overload;
function IsRecursiveReparsePoint (const Path : string; AllowJunctions : boolean) : boolean; overload;

// Pafdname eines reparse points
function GetJunction (const Path : string) : string;
function CreateJunction (const Source,Destination: string) : integer;

{ ---------------------------------------------------------------- }
// Check if file is read-only
function IsFileReadOnly (const fName : string) : boolean;

// Check if file is in use
function IsFileInUse (const fName : string) : boolean;

// Check if directory is empty
function IsEmptyDir (const Directory : string) : boolean;

// Delete empty directories
procedure DeleteEmptyDirectories (const Directory : string);

{ ---------------------------------------------------------------- }
// Dateien in einem Verzeichnis z�hlen und Gesamtgr��e bestimmen
procedure CountFiles (const Base,Dir : string; IncludeSubDir : boolean;
                      var FileCount : integer; var FileSize : int64);
function DirFiles (const Directory : string; IncludeSubDir : boolean) : integer;
function DirSize (const Directory : string; IncludeSubDir : boolean) : int64;

{ ---------------------------------------------------------------- }
(* L�sche ein Verzeichnis einschlie�lich aller Unterverzeichnisse und Dateien *)
procedure DeleteDirectory (const Base,Dir           : string;
                           DeleteRoot               : boolean;
                           var DCount,FCount,ECount : cardinal); overload;
function DeleteDirectory (const Directory : string;
                          DeleteRoot      : boolean) : boolean; overload;

{ ---------------------------------------------------------------- }
// pr�fen, ob auf ein Verzeichnis zugegriffen werden  kann
function CanAccess (const Directory : string; var ErrorCode : integer) : boolean; overload;
function CanAccess (const Directory : string) : boolean; overload;

implementation

uses System.StrUtils, System.Masks, Winapi.PsAPI, WinApi.AccCtrl, WinApi.AclApi,
  FileConsts, WinApiUtils;

{ ---------------------------------------------------------------- }
// Ini files using Unicode
constructor TUnicodeIniFile.CreateForRead (const FileName: string);
begin
  FFileName:=FileName;
  inherited Create(FileName);
  end;

// force Unicode to ini file
constructor TUnicodeIniFile.CreateForWrite (const FileName: string);
const
  UcSection = '[Unicode]';
var
  fs  : TFileStream;
  uid : word;
  sl  : TStringList;
begin
  FFileName:=FileName;
  try
    if FileExists(FileName) then begin
      fs:=TFileStream.Create(FileName,fmOpenReadWrite);
      fs.Read(uid,2);
      if uid<>UcBom then begin  // in Unicode konvertieren
        fs.Position:=0;
        sl:=TStringList.Create;
        sl.LoadFromStream(fs);
        sl.Insert(0,UcSection);
        fs.Size:=0;
        sl.SaveToStream(fs,TEncoding.Unicode);
        sl.Free;
        end
      end
    else begin
      fs:=TFileStream.Create(FileName,fmCreate);
      sl:=TStringList.Create;
      sl.Add(UcSection);
      sl.SaveToStream(fs,TEncoding.Unicode);
      sl.Free;
      end;
  finally
    fs.Free;
    end;
  inherited Create(FileName);
  end;

// replace original function to increase the string length to > 2047
function TUnicodeIniFile.ReadString(const Section, Ident, Default: string): string;
var
  Buffer: array[0..65535] of Char;
begin
  SetString(Result, Buffer, GetPrivateProfileString(PChar(Section),
    PChar(Ident), PChar(Default), Buffer, Length(Buffer), PChar(FFileName)));
  end;

{ ------------------------------------------------------------------- }
// create new file and write BOM
function WriteUtf8Bom (const Filename : string) : integer;
var
  fp    : file;
  n     : integer;
begin
  assignfile(fp,Filename);
  {$I-} rewrite(fp,1) {$I+};
  Result:=IOResult;
  if Result=0 then begin
    BlockWrite(fp,Utf8Bom,3,n);
    CloseFile(fp);
    end;
  end;

// check if file has leading BOM
function CheckUtf8Bom (const Filename : string) : boolean;
var
  fp    : file;
  n     : integer;
  Bom   : array[0..2] of byte;
begin
  assignfile(fp,Filename);
  {$I-} reset(fp,1) {$I+};
  Result:=IOResult<>0;
  if not Result then begin
    BlockRead(fp,Bom,3,n);
    Result:=(n=3) and CompareMem(@Bom[0],@Utf8Bom[0],3);
    CloseFile(fp);
    end;
  end;

{ ------------------------------------------------------------------- }
function IsAnsiStr (ws : string) : boolean;
var
  i : integer;
begin
  Result:=false;
  for i:=0 to length(ws) do if WordRec(ws[i]).Hi<>0 then exit;
  Result:=true;
  end;

{ ------------------------------------------------------------------- }
function WideUpCase (c : WideChar) : WideChar;
var
  s : string;
begin
  s:=c;
  CharUpperBuff(PChar(s),1);
  Result:=s[1];
  end;

{ ---------------------------------------------------------------- }
// following routine comes from SysUtils
type
  OBJECT_INFORMATION_CLASS = (ObjectBasicInformation, ObjectNameInformation,
    ObjectTypeInformation, ObjectAllTypesInformation, ObjectHandleInformation);

  UNICODE_STRING = packed record
    Length: Word;
    MaximumLength: Word;
    Buffer:PWideChar;
  end;

  OBJECT_NAME_INFORMATION = record
    TypeName: UNICODE_STRING;
    Reserved: array[0..21] of ULONG; // reserved for internal use
  end;

  TNtQueryObject = function (ObjectHandle: THandle;
    ObjectInformationClass: OBJECT_INFORMATION_CLASS; ObjectInformation: Pointer;
    Length: ULONG; ResultLength: PDWORD): THandle; stdcall;

var
  NTQueryObject: TNtQueryObject;

function GetFileNameFromSymLink(const FileName: string; var TargetName: string): Boolean;

  // Use this function to get the final target file name of a symbolic link.
  // It is the same as GetFinalPathNameByHandle but GetFinalPathNameByHandle
  // has some problems.
  function InternalGetFileNameFromHandle(Handle: THandle; var FileName: string; Flags: DWORD): Boolean;

    function ExpandVolumeName(const AFileName: string): string;
    var
      Drives, Temp: array[0..MAX_PATH + 1] of Char;
      P: PChar;
      Len: Integer;
      VolumeName: string;
    begin
      Len := GetLogicalDriveStrings(MAX_PATH, Drives);

      if Len > 0 then
      begin
        P := @Drives[0];

        repeat
          (P + 2)^ := #0;
          Len := Integer(QueryDosDevice(P, Temp, MAX_PATH));

          if Len > 0 then
          begin
            VolumeName := Temp;
            if Pos(VolumeName, AFileName) > 0 then
            begin
              Len := Length(VolumeName);
              Result := P + Copy(AFileName, Len + 1, Length(AFileName) - Len);
              Break;
            end;
          end;

          while P^ <> #0 do
            Inc(P);
          Inc(P, 2);

        until P = '';
      end;
    end;

    function GetObjectInfoName(Handle: THandle): string;
    const
      STATUS_SUCCESS = $00000000;
    var
      Info: ^OBJECT_NAME_INFORMATION;
      Status: THandle;
      Size: DWORD;
    begin
      Result := '';
      if not Assigned(NTQueryObject) then
        NTQueryObject := GetProcAddress(GetModuleHandle('NTDLL.DLL'), 'NtQueryObject'); // Do not localize

      if not Assigned(NTQUeryObject) then
        Exit;

      NtQueryObject(Handle, ObjectNameInformation, nil, 0, @Size);
      GetMem(Info, size);
      try
        Status := NTQueryObject(Handle, ObjectNameInformation, Info, Size, @Size);

        if Status = STATUS_SUCCESS then
          Result := Info.TypeName.Buffer;
      finally
        FreeMem(Info);
      end;
    end;

  var
    FileMapHandle: THandle;
    FileSizeHigh, FileSizeLow: DWORD;
    Memory: Pointer;
    Buffer: array[0..MAX_PATH + 1] of Char;
  begin
    Result := False;
    FileName := '';
    FileSizeHigh := 0;
    FileSizeLow := WinApi.Windows.GetFileSize(Handle, @FileSizeHigh);

    // GetFinalPathNameByHandle is broken so use other techniques to retrieve
    // the file's name.
    if (FileSizeLow = 0) and (FileSizeHigh = 0) and CheckWin32Version(6, 0) then
    begin
      if GetFinalPathNameByHandle(Handle, Buffer, MAX_PATH, VOLUME_NAME_NT) > 0 then
      begin
        FileName := ExpandVolumeName(Buffer);
        Exit(True);
      end;
    end;

    if (FileSizeLow <> 0) or (FileSizeHigh <> 0) then
    begin
      FileMapHandle := CreateFileMapping(Handle, nil, PAGE_READONLY, 0, 1, nil);

      if FileMapHandle <> ERROR_FILE_INVALID then
      begin
        try
          Memory := MapViewOfFile(FileMapHandle, FILE_MAP_READ, 0, 0, 1);

          if (Memory <> nil) then
          begin
            try
              if (GetMappedFileName(GetCurrentProcess(), Memory, Buffer, MAX_PATH) > 0) then
              begin
                FileName := ExpandVolumeName(Buffer);
                Result := True;
              end;
            finally
              UnmapViewOfFile(Memory);
            end;
          end;
        finally
          CloseHandle(FileMapHandle);
        end;
      end;
    end;

    if not Result then
    begin
      // If the file is zero size or is a directory then CreateFileMapping will
      // fail. Try using ZwQuerySystemInformation and ZwQueryObject.
      FileName := GetObjectInfoName(Handle);
      if FileName <> '' then
      begin
        FileName := ExpandVolumeName(FileName);
        Result := True;
      end;
    end;
  end;

var
  Handle: THandle;
  Flags: DWORD;
  Code: Cardinal;
begin
  Result := False;

  if CheckWin32Version(6, 0) then
  begin
    Code := GetFileAttributes(PChar(FileName));
    if (Code <> INVALID_FILE_ATTRIBUTES) and ((faSymLink and Code) <> 0) then
    begin
      if faDirectory and Code <> 0 then
        Flags := FILE_FLAG_BACKUP_SEMANTICS
      else
        Flags := 0;

      Handle := CreateFile(PChar(FileName), GENERIC_EXECUTE, FILE_SHARE_READ, nil,
        OPEN_EXISTING, Flags, 0);

      if Handle <> INVALID_HANDLE_VALUE then begin
        try
          Result := InternalGetFileNameFromHandle(Handle, TargetName, Flags);
        finally
          CloseHandle(Handle);
          end;
        end
      else Flags:=GetLastError;
    end;
  end;
end;

function GetLinkPath (const FileName: string) : string;
begin
  if not GetFileNameFromSymLink(FileName,Result) then Result:='';
  end;

{ ------------------------------------------------------------------- }
// read file infos
function GetFileData(FileName : string; var FileData : TWin32FindData) : boolean;
var
  Handle: THandle;
begin
  Handle:=FindFirstFile(PChar(FileName),FileData);
  Result:=Handle<>INVALID_HANDLE_VALUE;
  if Result then begin
    WinApi.Windows.FindClose(Handle);
    Result:=(FileData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0;
    end;
  end;

function GetFileAttrData(const FileName : string; var FileData : TWin32FileAttributeData;
                     FollowLink: Boolean = True) : integer;
var
  TargetName  : string;
  FindData : TWin32FindData;
  ok : boolean;
begin
  ok:=GetFileAttributesEx(PChar(FileName),GetFileExInfoStandard,@FileData);
  if ok then begin
    if (FileData.dwFileAttributes and faSymLink <> 0) and FollowLink then begin
      if GetFileNameFromSymLink(FileName, TargetName) then begin
        if IsRelativePath(TargetName) then
          TargetName:=IncludeTrailingPathDelimiter(ExtractFilePath(FileName))+TargetName;
        ok:=GetFileAttributesEx(PChar(TargetName),GetFileExInfoStandard,@FileData);
        end;
      end;
    Result:=ERROR_SUCCESS;
    end;
  if not ok then begin
    Result:=GetLastError;
    case Result of
      ERROR_SHARING_VIOLATION,
      ERROR_LOCK_VIOLATION:
       if not GetFileData(FileName,FindData) then Result:=ERROR_SHARING_VIOLATION
       else begin
        Move(FindData,FileData,SizeOf(TWin32FileAttributeData));
        Result:=ERROR_SUCCESS;
        end;
      end;
    end;
  end;

{ ---------------------------------------------------------------- }
// get time (UTC) of last file write
function GetFileLastWriteTime(const FileName: string): TFileTime;
var
  FileData : TWin32FileAttributeData;
begin
  if GetFileAttrData(FileName,FileData)=ERROR_SUCCESS then Result:=FileData.ftLastWriteTime
  else with Result do begin
    dwLowDateTime:=0; dwHighDateTime:=0;
    end;
  end;

// set time (UTC) of last file write
// CheckTime = true: Change FileTime to actual time if out of range
function SetFileLastWriteTime(const FileName: string; FileTime : TFileTime;
                              CheckTime : boolean) : integer;
var
  Handle   : THandle;
  dt       : TDateTime;
begin
  if CheckTime then begin
    if not FileTimeToDateTime(FileTime,dt) or (dt>Now+1) then FileTime:=DateTimeToFileTime(Now);
    end;
  Handle:=FileOpen(FileName,fmOpenWrite);
  if Handle=THandle(-1) then Result:=GetLastError
  else begin
    if SetFileTime(Handle,nil,nil,@FileTime) then Result:=0
    else Result:=GetLastError;
    FileClose(Handle);
    end;
  end;

{ ---------------------------------------------------------------- }
// get time (UTC) of last file write
function FileGetDateTime(const FileName: string; FollowLink : Boolean = True): TDateTime;
begin
  if not FileAge(FileName,Result,FollowLink) then Result:=0;
  end;

{ ---------------------------------------------------------------- }
// get time (UTC) of last file write
function GetFileLastWriteDateTime(const FileName: string): TDateTime;
var
  FileData : TWin32FileAttributeData;
begin
  if (GetFileAttrData(FileName,FileData)<>ERROR_SUCCESS)
    or not FileTimeToDateTime(FileData.ftLastWriteTime,Result) then Result:=0;
  end;

// set time (UTC) of last file write
function SetFileLastWriteDateTime(const FileName: string; FileTime : TDateTime) : integer;
var
  f   : THandle;
  ft  : TFileTime;
begin
  f:=CreateFile(PChar(FileName),FILE_WRITE_ATTRIBUTES,0,nil, OPEN_EXISTING,FILE_FLAG_BACKUP_SEMANTICS,0);
  if f=THandle(-1) then Result:=GetLastError
  else begin
    ft:=DateTimeToFileTime(FileTime);
    if SetFileTime(f,nil,nil,@ft) then Result:=NO_ERROR else Result:=GetLastError;
    FileClose(f);
    end;
  end;

{ ---------------------------------------------------------------- }
// get file/directory size, last write time, attributes and reparse point tags
function GetFileInfos(const FileName : string; var FileInfo : TFileInfo) : boolean; overload;
var
  FindData : TSearchRec;
  FindResult : integer;
begin
  Result:=false;
  with FileInfo do begin
    Name:='';
    DateTime:=Now;
    FileTime:=DateTimeToFileTime(DateTime);
    Size:=0;
    Attr:=INVALID_FILE_ATTRIBUTES;
    Res:=0;
    end;
  FindResult:=FindFirst(FileName,faAnyFile,FindData);
  if FindResult=0 then begin
    FileInfo:=SearchRecToFileInfo(FindData);
    Result:=true;
    end;
  end;

function GetFileInfos(const FileName : string; var FSize : int64;
                      var FTime : TFileTime; var FAttr,FReserved : cardinal) : boolean;
var
  fi : TFileInfo;
begin
  Result:=GetFileInfos(FileName,fi);
  with fi do begin
    FSize:=Size; FTime:=FileTime;
    FAttr:=Attr; FReserved:=Res;
    end;
  end;

function GetFileInfos(const FileName : string; var FSize : int64;
                      var FTime : TFileTime; var FAttr : cardinal) : boolean;
var
  n : cardinal;
begin
  Result:=GetFileInfos(FileName,FSize,FTime,FAttr,n);
  end;

{ ---------------------------------------------------------------- }
// set timestamp and attrtibutes from SearcRec
// Result = 0 : ok
//        > 0 : sytem error code - setting timestamp failed
//        < 0 : -sytem error code - setting attribute failed
function SetTimestampAndAttr(const FileName : string; SearchRec : TSearchRec) : integer;
begin
  with SearchRec do begin
    Result:=SetFileLastWriteDateTime(Filename,TimeStamp);
    if Result=NO_ERROR then Result:=-FileChangeAttr(FileName,Attr,false);
    end;
  end;

{ ---------------------------------------------------------------- }
// get file or directory timestamps (UTC) from FindData
function GetTimestampsFromFindData(const FindData : TWin32FindData) : TFileTimestamps;
begin
  with Result do begin
    CreationTime:=FindData.ftCreationTime;
    LastAccessTime:=FindData.ftLastAccessTime;
    LastWriteTime:=FindData.ftLastWriteTime;
    Valid:=true;
    end;
  end;

// get file or directory timestamps (UTC)
function GetTimestamps(const AName : string; ADir : boolean; FollowLink : Boolean = True) : TFileTimestamps;
var
  FileData   : TWin32FileAttributeData;
  TargetName : string;
  ok         : boolean;
begin
  ok:=GetFileAttrData(AName,FileData,FollowLink)=ERROR_SUCCESS;
  if ok and ((FileData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY =0) xor ADir) then begin
    with Result do begin
      CreationTime:=FileData.ftCreationTime;
      LastAccessTime:=FileData.ftLastAccessTime;
      LastWriteTime:=FileData.ftLastWriteTime;
      Valid:=true;
      Exit;
      end;
    end
  else begin
    FillChar(Result,sizeof(TFileTimestamps),0); // error
    Result.Valid:=false;
    end;
  end;

// get file timestamps (UTC)
function GetFileTimestamps(const FileName : string; FollowLink : Boolean = True): TFileTimestamps;
begin
  Result:=GetTimestamps(FileName,false,FollowLink);
  end;

// get directory timestamps (UTC)
function GetDirTimestamps(const DirName : string; FollowLink : Boolean = True): TFileTimestamps;
begin
  Result:=GetTimestamps(DirName,true,FollowLink);
  end;

// get time (UTC) of last directory change
function GetDirLastChangeTime(const DirName: string): TFileTime;
begin
  Result:=GetTimestamps(DirName,true).LastWriteTime;
  end;

// set file or directory timestamps (UTC)
// CheckTime = true: Change FileTime to actual time if out of range
// SetCreationTime = true: Copy timestamp ftCreationTime
function SetFileTimestamps (const FileName: string; Timestamps : TFileTimestamps;
                            CheckTime,SetCreationTime : boolean; FollowLink : Boolean = True) : integer;
var
  Handle   : THandle;
  tm       : TFiletime;
  dt       : TDateTime;
  fn,tn    : string;
  ok       : boolean;
begin
  tm:=DateTimeToFileTime(Now);
  with Timestamps do if Valid then begin
    if CheckTime then begin
      if not FileTimeToDateTime(CreationTime,dt) or (dt>Now+1) then CreationTime:=tm;
      if not FileTimeToDateTime(LastAccessTime,dt) or (dt>Now+1) then LastAccessTime:=tm;
      if not FileTimeToDateTime(LastWriteTime,dt) or (dt>Now+1) then LastWriteTime:=tm;
      end;
    end
  else begin
    CreationTime:=tm;
    LastAccessTime:=tm;
    LastWriteTime:=tm;
    end;

  fn:=FileName;
  if FollowLink then begin
    if ((faSymLink and GetFileAttributes(PChar(fn))) <> 0) and
        GetFileNameFromSymLink(fn,tn) then begin
      if IsRelativePath(tn) then
        fn:=IncludeTrailingPathDelimiter(ExtractFilePath(fn)) + tn
        else fn:=tn;
      end;
    end;

  Handle:=CreateFile(PChar(fn),FILE_WRITE_ATTRIBUTES,0,nil,OPEN_EXISTING,FILE_FLAG_BACKUP_SEMANTICS,0);
  if Handle=THandle(-1) then Result:=GetLastError
  else with Timestamps do begin
    if SetCreationTime then ok:=SetFileTime(Handle,@CreationTime,@LastAccessTime,@LastWriteTime)
    else ok:=SetFileTime(Handle,nil,nil,@LastWriteTime);
    if ok then Result:=NO_ERROR else Result:=GetLastError;
    FileClose(Handle);
    end;
  end;

{ ------------------------------------------------------------------- }
// convert Filetime to Delphi time (TDateTime)
function FileTimeToDateTime (ft : TFileTime; var dt : TDateTime) : boolean;
var
  st : TSystemTime;
begin
  Result:=false;
  if not (FileTimeToSystemTime(ft,st) and TrySystemTimeToDateTime(st,dt)) then dt:=Now
  else Result:=true;
  end;

function GetFileTimeToDateTime (ft : TFileTime) : TDateTime;
begin
  FileTimeToDateTime(ft,Result);
  end;

function FileTimeToLocalDateTime (ft : TFileTime; var dt : TDateTime) : boolean;
var
  ftl : TFileTime;
begin
  Result:=FileTimeToLocalFileTime(ft,ftl);
  Result:=Result and FileTimeToDateTime(ftl,dt);
  end;

// convert Delphi time (TDateTime) to Filetime
function DateTimeToFileTime (dt : TDateTime) : TFileTime;
var
  st : TSystemTime;
begin
  with st do begin
    DecodeDate(dt,wYear,wMonth,wDay);
    DecodeTime(dt,wHour,wMinute,wSecond,wMilliseconds);
    end;
  SystemTimeToFileTime(st,Result);
  end;

function LocalDateTimeToFileTime (dt : TDateTime) : TFileTime;
var
  ft : TFileTime;
begin
  ft:=DateTimeToFileTime(dt);
  LocalFileTimeToFileTime(ft,Result);
  end;

function DateTimeFromFileDate(FileDate : integer) : TDateTime;
begin
  try
    Result:=FileDateToDateTime(FileDate);
  except
    on EConvertError do Result:=Now;
    end;
  end;

// get DOS file age from file data
function GetFileAge (const FileData: TWin32FindData) : integer;
var
  LocalFileTime: TFileTime;
begin
  Result:=-1;
  if FileTimeToLocalFileTime(FileData.ftLastWriteTime,LocalFileTime) then begin
    if not FileTimeToDosDateTime(LocalFileTime,LongRec(Result).Hi,LongRec(Result).Lo) then
      Result:=-1;
    end;
  end;

{ ------------------------------------------------------------------- }
// convert Delphi time to Unix time (= seconds sinvce 00:00:00 UTC, 1.1.1970) and reverse
function DateTimeToUnixTime (dt : TDateTime) : cardinal;
begin
  Result:=round(SecsPerDay*(dt-25569));
  end;

function UnixTimeToDateTime (ut : cardinal) : TDateTime;
begin
  Result:=ut/SecsPerDay+25569;
  end;

// convert Fileage to Unix time and reverse
function FileAgeToUnixTime (Age : cardinal) : cardinal;
var
  lft,ft : TFileTime;
begin
  if DosDateTimeToFileTime(LongRec(Age).Hi,LongRec(Age).Lo,lft)
    and LocalFileTimeToFileTime(lft,ft) then
      Result:=FileTimeToUnixTime(ft)
  else Result:=0;
  end;

function FileTimeToFileAge (ft : TFileTime) : cardinal;
begin
  if not FileTimeToDosDateTime(ft,LongRec(Result).Hi,LongRec(Result).Lo) then Result:=0;
  end;

// convert Filetime to Unix time and reverse
function FileTimeToUnixTime (ft : TFileTime) : cardinal;
var
  dt : TDateTime;
begin
  FileTimeToDateTime(ft,dt);
  Result:=DateTimeToUnixTime (dt);
  end;

function UnixTimeToFileTime (ut : cardinal) : TFileTime;
begin
  Result:=DateTimeToFileTime(UnixTimeTodateTime(ut));
  end;

// convert Filetime to seconds
function FileTimeToSeconds (ft : TFileTime) : cardinal;
begin
  Result:=TInt64(ft).AsInt64 div 10000000;
  end;

{ ------------------------------------------------------------------- }
// get DOS time stamp of directory (similar to FileAge)
function DirectoryAge(const DirName: string; FollowLink : Boolean = True): Integer;
var
  dt : TDateTime;
begin
  if DirectoryAge(DirName,dt,FollowLink) then Result:=DateTimeToFileDate(dt)
  else Result:=-1;
  end;

function DirectoryAge(const DirName: string; out FileDateTime : TDateTime; FollowLink : Boolean = True): Boolean;
var
  FileData      : TWin32FileAttributeData;
  ok            : boolean;
begin
  Result:=False;
  ok:=GetFileAttrData(DirName,FileData)=ERROR_SUCCESS;
  if ok and (FileData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <>0) then
    Result:=FileTimeToLocalDateTime(FileData.ftLastWriteTime,FileDateTime);
  end;

function DirectoryGetDateTime(const FileName: string; FollowLink : Boolean = True): TDateTime;
begin
  if not DirectoryAge(FileName,Result,FollowLink) then Result:=0;
  end;

// Set DOS time stamp of directory
function SetDirectoryAge(const DirName: string; Age: Integer; FollowLink : Boolean = True): Integer;
begin
  Result:=SetFileAge(DirName,Age,FollowLink);
  end;

function SetFileAge(const FileName: string; Age: Integer; FollowLink : Boolean = True): Integer;
var
  LocalFileTime : TFileTime;
begin
  if DosDateTimeToFileTime(LongRec(Age).Hi,LongRec(Age).Lo,LocalFileTime) then
    Result:=SetFileAge(FileName,LocalFileTime,FollowLink)
  else Result:=GetLastError;
  end;

// Set timestamp stamp of directory
function SetDirectoryAge(const DirName: string; Age: FileTime; FollowLink : Boolean = True): Integer;
begin
  if IsRootPath(DirName) then Result:=NO_ERROR
  else Result:=SetFileAge(DirName,Age,FollowLink);
  end;

function SetFileAge(const FileName: string; Age: FileTime; FollowLink : Boolean = True) : Integer;
var
  f             : THandle;
  fn,TargetName : string;
  FileTime      : TFileTime;
begin
  fn:=FileName;
  if FollowLink then begin
    if ((faSymLink and GetFileAttributes(PChar(fn)))<>0) and
        GetFileNameFromSymLink(fn,TargetName) then begin
      if IsRelativePath(TargetName) then
        fn:=IncludeTrailingPathDelimiter(ExtractFilePath(fn)+TargetName)
        else fn:=TargetName;
      end;
    end;
  f:=CreateFile(PChar(fn),FILE_WRITE_ATTRIBUTES,0,nil,OPEN_EXISTING,FILE_FLAG_BACKUP_SEMANTICS,0);
  if f=THandle(-1) then Result:=GetLastError
  else begin
    if LocalFileTimeToFileTime(Age,FileTime) and SetFileTime(f, nil, nil, @FileTime)
      then Result:=NO_ERROR else Result:=GetLastError;
    FileClose(f);
    end;
  end;

{ ------------------------------------------------------------------- }
// Delete all matching files in directory
function DeleteMatchingFiles(const APath,AMask : string) : integer;
var
  DirInfo    : TSearchRec;
  Findresult : integer;
begin
  Result:=0;
  FindResult:=FindFirst(SetDirName(APath)+AMask,faAllFiles,DirInfo);
  while (FindResult=0) do begin
    if EraseFile(SetDirName(APath)+DirInfo.Name) then inc(Result);
    FindResult:=FindNext(DirInfo)
    end;
  FindClose(DirInfo);
  end;

// Delete all files in directory older than date
function DeleteOlderFiles(const APath,AMask : string; ADate : TDateTime) : integer;
var
  DirInfo    : TSearchRec;
  Findresult : integer;
begin
  Result:=0;
  FindResult:=FindFirst(SetDirName(APath)+AMask,faAllFiles,DirInfo);
  while (FindResult=0) do begin
    if DirInfo.TimeStamp<ADate then begin
      if EraseFile(SetDirName(APath)+DirInfo.Name) then inc(Result);
      end;
    FindResult:=FindNext(DirInfo)
    end;
  FindClose(DirInfo);
  end;

//-----------------------------------------------------------------------------
// Pr�fen, ob in einem Verzeichnis eine Datei existiert, die der Maske entspricht
function FileMatchesMask (const Dir,Mask : string) : string;
var
  FInfo      : TSearchRec;
begin
  Result:='';
  if FindFirst (SetDirName(Dir)+Mask,faArchive,FInfo)=0 then
    Result:=SetDirName(Dir)+FInfo.Name;
  System.Sysutils.FindClose (FInfo);
  end;

{ ---------------------------------------------------------------- }
(* Diese Funktion pr�ft, ob eine Text-Datei vorhanden ist und �ffnet sie *)
function ExistsFile (var f : TextFile) : boolean;
begin
  (*$i-*) reset (f) (*$i+*);
  Result:=ioresult=0;
  end;

{ ------------------------------------------------------------------- }
// Bei nicht leeren Pfaden Backslash erg�nzen
function SetDirName (const Dir : string) : string;
begin
  if length(Dir)>0 then Result:=IncludeTrailingPathDelimiter(Dir)
  else Result:='';
  end;

// Verzeichnisse vergleichen
function SameDirName (const S1,S2: string): Boolean;
begin
  Result:=SameFileName(SetDirName(S1),SetDirName(S2));
  end;

{ --------------------------------------------------------------- }
(* Ausmaskieren der Erweiterung (ohne Punkt) *)
function GetExt (const Name : string) : string;
var
  i,j : integer;
begin
  Result:='';
  j:=length(Name);
  if j>0 then begin
    i:=j;
    while (i>0) and (Name[i]<>Punkt) and (not IsPathDelimiter(Name,i)) do dec(i);
    if (i>0) and (Name[i]=Punkt) then Result:=copy(name,i+1,j-i)
    end;
  end;

{ --------------------------------------------------------------- }
(* Dateinamenerweiterung entfernen (sucht letzten Punkt) *)
function DelExt (Name : string) : string;
var
  i,j : integer;
begin
  j:=length(Name);
  if j>0 then begin
    i:=j;
    while (i>0) and (Name[i]<>Punkt) and (not IsPathDelimiter(Name,i)) do dec(i);
    if (i>0) and (Name[i]=Punkt) then delete (Name,i,j-i+1);
    end;
  Result:=Name;
  end;

{ --------------------------------------------------------------- }
(* �ndern einer Dateinamenerweiterung (sucht letzten Punkt - JR 9.8.05) *)
function NewExt (Name,Ext  : string) : string;
begin
  Name:=DelExt(Name);
  if (length(Ext)>0) and (Ext[1]=Punkt) then delete(Ext,1,1);
  if length(Ext)>0 then begin
    if (Ext[1]=Punkt) then Result:=Name+Ext else Result:=Name+Punkt+Ext
    end
  else Result:=Name;
  end;

{ --------------------------------------------------------------- }
// Hinzuf�gen einer Dateinamenerweiterung
// Always = true - immer, = false - nur wenn keine vorhanden ist
function AddExt (const Name,Ext  : string; Always : boolean) : string;
var
  i : integer;
begin
  Result:=Name;
  i:=length(Name);
  if (i=0) or (length(Ext)=0) then Exit
  else begin
    if not Always then
      while (i>0) and (Name[i]<>Punkt) and (not IsPathDelimiter(Name,i)) do dec(i)
    else i:=0;
    if (i>0) and (Name[i]=Punkt) then Exit
    else if (Ext[1]=Punkt) then Result:=Name+Ext else Result:=Name+Punkt+Ext;
    end;
  end;

{ --------------------------------------------------------------- }
// Hinzuf�gen eines Pfades zum Dateinamen
function AddPath (Path,Name : string) : string;
begin
  if length(Name)>0 then begin
    if length(Path)>0 then Result:=SetDirName(Path)+Name else Result:=Name;
    end
  else Result:=Path;
  end;

{ --------------------------------------------------------------- }
(* Dateiname mit Laufwerk, Verzeichnis und
   Extension (falls nicht vorhanden) erweitern
   Aug. 2007: Erweiter auch bei rel. Pfaden *)
function Erweiter (Pfad,Name,Ext  : string) : string;
begin
  if (length(Ext)>0) and (Ext[1]=Punkt) then delete(Ext,1,1);
  if (pos(Punkt,Name)=0) and (length(Ext)<>0) then Name:=Name+Punkt+ext;
  if (pos (DPunkt,Name)<>0) or (pos(BSlash,Name)=1)
     or (length(Pfad)=0) then Result:=Name
  else if Pfad[length(Pfad)]=BSlash then Result:=Pfad+Name
         else Result:=Pfad+BSlash+Name;
  end;

{ ---------------------------------------------------------------- }
(* Suffix und Erweiterung zu Dateinamen hinzuf�gen *)
function AddNameSuffix (FName,Suffix,Ext : string) : string;
begin
  if (length(Ext)>0) and (Ext[1]<>Punkt) then Ext:=Punkt+Ext;
  Result:=DelExt(FName)+Suffix+Ext;
  end;

{ ---------------------------------------------------------------- }
(* Suffix zu Dateinamen mit Extension hinzuf�gen *)
function InsertNameSuffix (FName,Suffix : string) : string;
begin
  Result:=AddNameSuffix(FName,Suffix,GetExt(FName));
  end;

{ ---------------------------------------------------------------- }
(* Suffix (L�nge: CharCount) von Dateinamen mit Extension entfernen *)
function RemoveNameSuffix (FName : string; CharCount : integer) : string;
var
  s : string;
begin
  s:=DelExt(FName);
  Result:=AnsiLeftStr(s,length(s)-CharCount)+ExtractFileExt(FName);
  end;

{ ---------------------------------------------------------------- }
// FName startet mit Prefixmaske
function HasPrefix(const Pref,FName : string) : boolean;
begin
  Result:=(length(Pref)>0) and MatchesMask(FName,Pref);
  end;

{ ---------------------------------------------------------------- }
(* Pfad aus Dateinamen teilweise entfernen, wenn der Name zu lang wird
  siehe auch MinimizeName in FileCtrl *)
function StripPath (Name : String;
                    Len  : integer) : String;
const
  Punkte = '...';
var
  i,j,nl : integer;
  ok     : boolean;
begin
  nl:=length(Name);
  if nl>=Len then begin
    i:=nl; ok:=true;
    if IsPathDelimiter(Name,i) then dec(i);
    while not IsPathDelimiter(Name,i) and (i>0) do dec(i);
    if i=0 then Name:=Punkte+copy(Name,nl-Len+3,nl)
    else begin
      dec(i); j:=i;
      repeat
        while not IsPathDelimiter(Name,j) and (j>0) do dec(j);
        dec(j);
        if j<0 then Name:=Punkte+copy(Name,nl-Len+3,nl)
        else begin
          ok:=nl-i+j+4<=Len;
          end;
        until ok or (j<0);
      if ok then begin
        inc(j,2);
        delete (Name,j,succ(i-j)); insert (Punkte,Name,j);
        end;
      end;
    end;
  Result:=Name;
  end;

{ ------------------------------------------------------------------- }
(* Extrahiere den �bergeordneten Pfad      *)
function ExtractParentPath (Dir : string) : string;
var
  i : integer;
begin
  Dir:=ExcludeTrailingPathDelimiter(Dir);
  i:=length(Dir);
  while not IsPathDelimiter(Dir,i) and (i>0) do dec(i);
  if i<=1 then Result:=Dir
  else Result:=copy(Dir,1,i-1);
  end;

(* Extrahiere das letzte Unterverzeichnis *)
function ExtractLastDir (const Path : string) : string;
begin
  Result:=ExtractFileName(ExcludeTrailingPathDelimiter(Path));
  end;

{ ------------------------------------------------------------------- }
// "Path2" ist ein untergeordneter Pfad von "Path1"
function IsSubPath (const Path1,Path2 : string) : boolean;
begin
  Result:=AnsiStartsText(SetDirName(Path1),SetDirName(Path2));
  end;

function IsRootPath (const Path : string) : boolean;
begin
  Result:=AnsiSameText(IncludeTrailingPathDelimiter(ExtractFileDrive(Path)),Path);
  end;

(* Name enth�lt vollst�ndigen Pfad *)
function ContainsFullPath (const Name : string) : boolean;
begin
  if length(Name)>0 then Result:=(Name[1]=BSlash) or (pos(DPunkt,Name)>0)
  else Result:=false;
  end;

function MakeRelativePath (const BaseName,DestName: string) : string;
begin
  if AnsiSameText(SetDirName(Basename),SetDirName(DestName)) then Result:=''
  else if IsSubPath(Basename,DestName) then Result:=ExtractRelativePath(SetDirName(BaseName),DestName)
  else Result:=DestName
  end;

function MakeAbsolutePath (const BaseName,DestName: string) : string;
begin
  if ContainsFullPath(DestName) then Result:=DestName
  else Result:=SetDirName(BaseName)+DestName;
  end;

function ExpandPath (const DestName: string) : string;
begin
  if ContainsFullPath(DestName) then Result:=DestName
  else Result:=ExpandFileName(Destname);
  end;

// identischen Teil des Pfads extrahieren
function ExtractSamePath(const Path1,Path2 : string) : string;
var
  i,n1,n2     : integer;
  s1,s2 : string;
  ok    : boolean;
begin
  s1:=AnsiLowerCase(IncludeTrailingPathDelimiter(Path1));
  s2:=AnsiLowerCase(IncludeTrailingPathDelimiter(Path2));
  n2:=0;
  repeat
    n1:=PosEx('\',s1,n2+1);
    ok:=(n1>0) and (n1=PosEx('\',s2,n2+1));
    if ok then for i:=n2+1 to n1-1 do ok:=ok and (s1[i]=s2[i]);
    if ok then n2:=n1;
    until not ok or (n1>length(s1)) or (n1>length(s2));
  if n2>0 then Result:=copy(Path1,1,n2) else Result:='';
  end;

// Laufwerksbuchstaben ersetzen
function ReplaceDrive(const Path,Drive : string) : string;
begin
  Result:=Path;
  if (length(Path)>0) and (length(Drive)>0) then Result[1]:=Drive[1];
  end;

{ ---------------------------------------------------------------- }
(* pr�fe, ob alle Zeichen in APath zul�ssig sind *)
function CheckPathChars (APath : string) : boolean;
var
  s : string;

  function ReadNext (var s   : String) : string;
  var
    i : integer;
  begin
    if length(s)>0 then begin
      i:=pos (BSlash,s);
      if i=0 then i:=succ(length(s));
      Result:=copy(s,1,pred(i));
      delete(s,1,i);
      end
    else Result:='';
    end;

begin
  Result:=true;
  if length(APath)>0 then begin
    if (copy(APath,1,2)='\\') then delete(APath,1,2);
    APath:=ExcludeTrailingPathDelimiter(APath);
    while Result and (length(APath)>0) do begin
      s:=ReadNext(APath);
      if (Length(s)>2) or not MatchesMask(s,'?:') then Result:=CheckFilename(s);
      end;
    end;
  end;

(* pr�fe, ob alle Zeichen in Filename (ohne Pfad) zul�ssig sind *)
function CheckFilename (const Filename : string) : boolean;
var
  i : integer;
begin
  Result:=true; i:=1;
  if length(Filename)>0 then while Result and (i<=length(Filename)) do begin
    Result:=not CharInSet(AnsiChar(Filename[i]),IllegalFilenameChars);
    inc(i);
    end;
  end;

// Ersetze ung�ltige Zeichen in Filename (ohne Pfad) durch AChar
function ReplaceIllegalChars (const Filename : string; AChar : char) : string;
var
  i : integer;
begin
  Result:=Filename;
  for i:=1 to length(Filename) do begin
    if CharInSet(AnsiChar(Result[i]),IllegalFilenameChars) then Result[i]:=AChar;
    end;
  end;

{ ------------------------------------------------------------------- }
// Copy file with timestamp and attributes
// AAttr = -1: copy original attributes
procedure CopyFileTS (const SrcFilename,DestFilename : string;
                      AAttr : integer = -1; BlockSize : integer = defBlockSize);
var
  srcfile, destfile : TFileStream;
  FTime             : TFileTime;
  Buffer            : pointer;
  NRead,NWrite      : Integer;
  Attr              : word;
begin
  if AnsiSameText(srcfilename,destfilename) then Exit;
  if FileExists(srcfilename) and (length(destfilename)>0) then begin
    GetMem(Buffer,BlockSize);
    try
      FTime:=GetFileLastWriteTime(srcfilename);
      if AAttr<0 then Attr:=FileGetAttr(srcfilename) else Attr:=AAttr;
      try
        srcfile:=TFileStream.Create(srcfilename,fmOpenRead+fmShareDenyNone);
      except
        on EFOpenError do
          raise ECopyError.Create (Format(rsErrOpening,[srcfilename]));
        end;
      // Ziel immer �berschreiben
      if FileExists(destfilename) then begin
        if FileSetAttr(destfilename,faArchive)<>0 then begin
          try srcfile.Free; except end;
          raise ECopyError.Create (Format(rsErrCreating,[destfilename]));
          end;
        end;
      try
        destfile:=TFileStream.Create(destfilename,fmCreate);
      except
        on EFCreateError do begin
          try srcfile.Free; except end;
          raise ECopyError.Create (Format(rsErrCreating,[destfilename]));
          end;
        end;
      repeat
        try
          NRead:=srcfile.Read(Buffer^,BlockSize);
        except
          on EReadError do
            raise ECopyError.Create (Format(rsErrReading,[srcfilename]));
          end;
        try
          NWrite:=destfile.Write(Buffer^,NRead);
          if NWrite<NRead then  // Ziel-Medium voll
            raise ECopyError.Create (Format(rsErrWriting,[destfilename]));
        except
          on EWriteError do
            raise ECopyError.Create (Format(rsErrWriting,[destfilename]));
          end;
        until NRead<BlockSize;
      if  destfile.Size<>srcfile.Size then begin
      // z.B. wenn "srcfile" gelockt ist (siehe LockFile)
        srcfile.Free; destfile.Free;
        raise ECopyError.Create (Format(rsErrReading,[srcfilename]));
        end;
      try
        srcfile.Free;
      except
        on EFileStreamError do
          raise ECopyError.Create (Format(rsErrClosing,[srcfilename]));
        end;
      try
        destfile.Free;
      except
        on EFileStreamError do
          raise ECopyError.Create (Format(rsErrClosing,[destfilename]));
        end;
      if SetFileLastWriteTime(destfilename,FTime,true)=0 then begin
        if FileSetAttr(destfilename,Attr)>0 then
          raise ECopyError.Create (Format(rsErrSetAttr,[destfilename]));
        end
      else
        raise ECopyError.Create (Format(rsErrTimeStamp,[destfilename]));
    finally
      FreeMem(Buffer,BlockSize);
      end;
    end
  else raise ECopyError.Create (Format(rsErrNotFound,[srcfilename]));
  end;

{ ---------------------------------------------------------------- }
// Copy files from one directory to another
function CopyFiles (const FromDir,ToDir,AMask : string; OverWrite : boolean) : boolean;
var
  DirInfo    : TSearchRec;
  Findresult : integer;
begin
  Result:=true;
  FindResult:=FindFirst(FromDir+AMask,faAllFiles,DirInfo);
  while (FindResult=0) and Result do with DirInfo do begin
    if OverWrite or not FileExists(ToDir+Name) then begin
      try
        CopyFileTS(FromDir+Name,ToDir+Name);
      except
        Result:=false
        end;
      end;
    FindResult:=FindNext(DirInfo)
    end;
  FindClose(DirInfo);
  end;

{ ---------------------------------------------------------------- }
// Copy file permissions (ACL)
function CopyFileAcl (const srcfilename,destfilename : string) : cardinal;
var
  SidOwner : PSID;
  SidGroup : PSID;
  DAcl     : PACL;
  SAcl     : PACL;
  sd       : PSecurityDescriptor;
  si       : SECURITY_INFORMATION;
begin
  sd:=nil;
  si:=DACL_SECURITY_INFORMATION;
  try
    Result:=GetNamedSecurityInfo (PChar(srcfilename),SE_FILE_OBJECT,si,
                    @SidOwner,@SidGroup,@DAcl,@SAcl,Pointer(sd));
    if Result=NO_ERROR then begin
      Result:=SetNamedSecurityInfo (PChar(destfilename),SE_FILE_OBJECT,si,
                    SidOwner,SidGroup,DAcl,SAcl);
      end;
  finally
    if sd<>nil then LocalFree(cardinal(sd));
    end;
  end;

{ ---------------------------------------------------------------- }
// convert file attribute to string
function FileAttrToString(Attr : word) : string;
var
  s : string;
begin
  s:='';
  if Attr and faReadOnly =0 then s:=s+'-' else s:=s+'r';
  if Attr and faArchive =0 then s:=s+'-' else s:=s+'a';
  if Attr and faHidden =0 then s:=s+'-' else s:=s+'h';
  if Attr and faSysFile =0 then s:=s+'-' else s:=s+'s';
//  if Attr and faSymLink	=0 then s:=s+'-' else s:=s+'L';
  Result:=s;
  end;

{ ---------------------------------------------------------------- }
// Clear = true  : clear given attributes
//       = false : set given attributes
function FileChangeAttr (const FileName: string; Attr: Integer; Clear : boolean;
                         FollowLink: Boolean = True) : Integer;
var
  ao,an : integer;
begin
  Result:=NO_ERROR;
  ao:=FileGetAttr(FileName,FollowLink);
  if ao=INVALID_FILE_ATTRIBUTES then begin
    Result:=ERROR_FILE_NOT_FOUND;
    Exit;   // file does not exist
    end;
  if Clear then an:=ao and not Attr else an:=ao or Attr;
  if ao<>an then Result:=FileSetAttr(FileName,an,FollowLink);
  end;

{ ---------------------------------------------------------------- }
function GetFileSize (const FileData: TWin32FindData) : int64;
begin
  with TInt64(Result) do begin
    Lo:=FileData.nFileSizeLow; Hi:=FileData.nFileSizeHigh;
    end;
  end;

function GetFileSize (const FileData: TWin32FileAttributeData) : int64;
begin
  with TInt64(Result) do begin
    Lo:=FileData.nFileSizeLow; Hi:=FileData.nFileSizeHigh;
    end;
  end;

function LongFileSize (const FileName : string) : int64;
var
  FileData : TWin32FileAttributeData;
begin
  if GetFileAttrData(FileName,FileData) =ERROR_SUCCESS then Result:=GetFileSize(FileData)
  else Result:=0;
  end;

const
  SeekStep = $80000000;

{ ------------------------------------------------------------------- }
// Convert SearchRec to FileInfo
function SearchRecToFileInfo (SearchRec : TSearchRec) : TFileInfo;
begin
  with Result do begin
    Name:=SearchRec.Name;
    Attr:=SearchRec.Attr;
    Size:=GetFileSize(SearchRec.FindData);
    DateTime:=SearchRec.TimeStamp;
    FileTime:=SearchRec.FindData.ftLastWriteTime;
    Res:=SearchRec.FindData.dwReserved0;
    end;
  end;

{ ------------------------------------------------------------------- }
// Delete file even if readonly
function EraseFile(const FileName: string) : Boolean;
begin
  if FileExists(FileName) then begin
    Result:=FileSetAttr(Filename,faArchive,false)=0;
    if Result then Result:=DeleteFile(FileName);
    end
  else Result:=true;
  end;

{ ------------------------------------------------------------------- }
(* kein spezielles Verzeichnis (selbst und eins zur�ck) *)
function NotSpecialDir (const Name : string; StartChars : array of char) : boolean;
var
  i : integer;
begin
  Result:=(Name<>'.') and (Name<>'..');
  if Result and (length(StartChars)>0) then begin
    for i:=Low(StartChars) to High(StartChars) do Result:=Result and (Pos(StartChars[i],Name)<>1);
    end;
  end;

function NotSpecialDir (const Name : string) : boolean;
begin
  Result:=NotSpecialDir(Name,[]);
  end;

{ ------------------------------------------------------------------- }
// get the type of the reparse point (junction or symbolic)
// fr = IO_REPARSE_TAG_MOUNT_POINT    mklink /j ..
//    = IO_REPARSE_TAG_SYMLINK        mklink /d ..
function GetReparsePointType(const FileName : string) : TReparseType;
var
  fs : int64;
  ft : TFileTime;
  fa,fr : cardinal;
begin
  if GetFileInfos(FileName,fs,ft,fa,fr) then begin
    if fr=IO_REPARSE_TAG_MOUNT_POINT then Result:=rtJunction
    else if fr=IO_REPARSE_TAG_SYMLINK then Result:=rtSymbolic
    else Result:=rtNone;
    end
  else Result:=rtNone;
  end;

// Check if reparse point and return linked path if not recursive
function CheckForReparsePoint (const Path : string; Attr : integer;
                               var LinkPath : string; var RpType : TReparseType) : boolean;
begin
  Result:=false; LinkPath:=''; RpType:=rtNone;
  if Attr=-1 then Exit;
  if (Attr and FILE_ATTRIBUTE_REPARSE_POINT <>0) then begin  //directory entry is a reparse point
    Result:=true;
    RpType:=GetReparsePointType(ExcludeTrailingPathDelimiter(Path));
    LinkPath:=GetLinkPath(ExcludeTrailingPathDelimiter(Path));
//    if (length(LinkPath)>0) then begin
//      if AnsiStartsText(LinkPath,Path) then LinkPath:='';  // avoid recursions
//      end;
    end;
  end;

// Check if reparse point is recursive
function IsRecursiveReparsePoint (const Path : string; Attr : integer; AllowJunctions : boolean) : boolean;
var
  s : string;
begin
  Result:=false;
  if Attr=-1 then Exit;
  if (Attr and FILE_ATTRIBUTE_REPARSE_POINT <>0) then begin  //directory entry is a reparse point
    if AllowJunctions then begin  // check if recursive link
      s:=GetLinkPath(Path);
      if (length(s)>0) and AnsiStartsText(s,Path) then Result:=true;  // avoid recursions
      end
    else Result:=true;
    end;
  end;

function IsRecursiveReparsePoint (const Path : string; AllowJunctions : boolean) : boolean;
begin
  Result:=IsRecursiveReparsePoint (Path,FileGetAttr(Path,false),AllowJunctions);
  end;

{ ---------------------------------------------------------------- }
function GetJunction (const Path : string) : string;
var
  hDir   : THandle;
  n      : dword;
  rdb    : TReparseDataBuffer;
begin
  Result:='';
  if ModifyPrivilege(SE_BACKUP_NAME,true) then begin
    hDir:=CreateFile(PChar(Path),GENERIC_READ,0,nil,
                     OPEN_EXISTING,FILE_FLAG_OPEN_REPARSE_POINT or FILE_FLAG_BACKUP_SEMANTICS,0);
    if hDir<>INVALID_HANDLE_VALUE then begin
      if DeviceIoControl(hDir,FSCTL_GET_REPARSE_POINT,nil,0,@rdb,sizeof(rdb),n,nil) then begin
        with rdb do if ReparseTag=IO_REPARSE_TAG_MOUNT_POINT then begin
          Result:=copy(PathBuffer,SubstituteNameOffset,SubstituteNameLength);
          if AnsiStartsText('\??\',Result) then Delete(Result,1,4);
          end;
        end
      else n:=GetLastError;
      CloseHandle(hDir);
      end;
    end;
  end;

function CreateJunction(const Source,Destination : string): integer;
var
  hDir,
  hToken : THandle;
  tkp    : TTokenPrivileges;
  rdb    : TReparseDataBuffer;
  dest   : string;
  nl,n   : dword;
begin
  Result:=NO_ERROR;
  if DirectoryExists(Source) then begin
    if not IsEmptyDir(Source) then Result:=ERROR_DIR_NOT_EMPTY;
    end
  else if not ForceDirectories(Source) then Result:=GetLastError;
  if Result=NO_ERROR then begin
    // For some reason the destination string must be prefixed with \??\ otherwise
    // the IOCTL will fail, ensure it's there.
    if AnsiStartsText('\??\',Destination) then dest:=Destination
    else begin
      // Make sure Destination is a directory or again, the IOCTL will fail.
      Dest:=ExpandFileName(Destination);
      if (length(Dest)=0) or not DirectoryExists(Dest) then Exit;
      Dest:='\??\'+Dest;
      end;
    // Get a token for this process.
    if OpenProcessToken(GetCurrentProcess,TOKEN_ADJUST_PRIVILEGES,hToken) then begin
    // Get the LUID for the backup privilege.
      LookupPrivilegeValue(nil,SE_RESTORE_NAME,tkp.Privileges[0].Luid);
      tkp.PrivilegeCount:=1;  // one privilege to set
      tkp.Privileges[0].Attributes:=SE_PRIVILEGE_ENABLED;
    // Get the backup privilege for this process.
      AdjustTokenPrivileges(hToken,FALSE,tkp,0,nil,n);
      CloseHandle(hToken);
      nl:=length(Dest)*SizeOf(WideChar);
      FillChar(rdb,sizeof(TReparseDataBuffer),0);
      with rdb do begin
        ReparseTag:=IO_REPARSE_TAG_MOUNT_POINT;
        ReparseDataLength:=nl+12;
        SubstituteNameLength:=nl;
        PrintNameOffset:=nl+2;
        Move(Dest[1],PathBuffer,nl);
        end;
      hDir:=CreateFile(PChar(Source),GENERIC_READ or GENERIC_WRITE,0,nil,
                       OPEN_EXISTING,FILE_FLAG_OPEN_REPARSE_POINT or FILE_FLAG_BACKUP_SEMANTICS,0);
      if hDir<>INVALID_HANDLE_VALUE then begin
        try
          n:=0;
          if not DeviceIoControl(hDir,FSCTL_SET_REPARSE_POINT,@rdb,
            rdb.ReparseDataLength+REPARSE_DATA_BUFFER_HEADER_SIZE,nil,0,n,nil) then Result:=GetLastError;
        finally
          CloseHandle(hDir);
          end;
        end
      else Result:=GetLastError;
      end
    else Result:=GetLastError;
    end
  end;

{ ------------------------------------------------------------------- }
// Check if file is read-only
function IsFileReadOnly (const fName : string) : boolean;
var
  n : integer;
begin
  n:=FileGetAttr(fName);
  Result:=(n<>faInvalid) and ((n and faReadOnly)<>0);
  end;

// Check if file is in use
function IsFileInUse (const fName : string) : boolean;
var
  Handle : THandle;
begin
  Handle:=CreateFile(pchar(fName),GENERIC_READ or GENERIC_WRITE,0,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
  Result:=Handle=INVALID_HANDLE_VALUE;
  if not Result then CloseHandle(Handle);
  end;

{ ------------------------------------------------------------------- }
// Check if directory is empty
function IsEmptyDir (const Directory : string) : boolean;
var
  DirInfo    : TSearchRec;
  FindResult,
  n          : integer;
begin
  n:=0;
  FindResult:=FindFirst(SetDirName(Directory)+'*.*',faAnyFile,DirInfo);
  while (n=0) and (FindResult=0) do with DirInfo do begin
    if NotSpecialDir(Name) then inc(n);
    FindResult:=FindNext (DirInfo);
    end;
  FindClose(DirInfo);
  Result:=n=0;
  end;

{ ---------------------------------------------------------------- }
// Delete empty directories
procedure DeleteEmptyDirectories (const Directory : string);
var
  DirInfo    : TSearchRec;
  FindResult : integer;
  s          : string;
begin
  FindResult:=FindFirst(SetDirName(Directory)+'*.*',faDirectory,DirInfo);
  while (FindResult=0) do with DirInfo do begin
    if NotSpecialDir(Name) then begin
      s:=SetDirName(Directory)+Name;
      DeleteEmptyDirectories(s);
      RemoveDir(s);
      end;
    FindResult:=FindNext (DirInfo);
    end;
  FindClose(DirInfo);
  end;

{ ---------------------------------------------------------------- }
// Dateien in einem Verzeichnis z�hlen und Gesamtgr��e bestimmen
procedure CountFiles (const Base,Dir : string; IncludeSubDir : boolean;
                      var FileCount : integer; var FileSize : int64);
var
  DirInfo    : TSearchRec;
  Findresult : integer;
  sd         : string;
begin
  sd:=SetDirName(SetDirName(Base)+Dir);
  FindResult:=FindFirst(sd+'*.*',faAnyFile,DirInfo);
  while FindResult=0 do with DirInfo do begin
    if IncludeSubDir and NotSpecialDir(Name) and ((Attr and faDirectory)<>0) then
      CountFiles (Base,SetDirName(Dir)+DirInfo.Name,IncludeSubDir,FileCount,FileSize);
    FindResult:=FindNext(DirInfo);
    end;
  FindClose(DirInfo);
  FindResult:=FindFirst(sd+'*.*',faAllFiles,DirInfo);
  while FindResult=0 do with DirInfo do begin
    if NotSpecialDir(Name) then begin
      inc(FileCount); inc(FileSize,GetFileSize(DirInfo.FindData));
      end;
    FindResult:=FindNext(DirInfo);
    end;
  FindClose(DirInfo);
  end;

function DirFiles (const Directory : string; IncludeSubDir : boolean) : integer;
var
  n : int64;
begin
  Result:=0; n:=0;
  CountFiles(Directory,'',IncludeSubDir,Result,n);
  end;

function DirSize (const Directory : string; IncludeSubDir : boolean) : int64;
var
  n : integer;
begin
  Result:=0; n:=0;
  CountFiles(Directory,'',IncludeSubDir,n,Result);
  end;

// L�sche ein Verzeichnis einschlie�lich aller Unterverzeichnisse und Dateien
procedure DeleteDirectory (const Base,Dir           : string;
                           DeleteRoot               : boolean;
                           var DCount,FCount,ECount : cardinal);
// DCount: Anzahl gel�schte Verzeichnisses
// FCount: Anzahl gel�schte Dateien
// ECount: Anzahl Fehler
var
  DirInfo    : TSearchRec;
  fc,dc,
  Findresult : integer;
  s,sd       : string;
begin
  if length(Dir)>0 then sd:=SetDirName(Base)+Dir else sd:=Base;
  if DirectoryExists(sd) then begin
    FindResult:=FindFirst(SetDirName(sd)+'*.*',faAnyFile,DirInfo);
    while FindResult=0 do with DirInfo do begin
      if NotSpecialDir(Name) and ((Attr and faDirectory)<>0) then
        DeleteDirectory(Base,SetDirName(Dir)+DirInfo.Name,DeleteRoot,DCount,FCount,ECount);
      FindResult:=FindNext(DirInfo);
      end;
    FindClose(DirInfo);
    fc:=0; dc:=0;
    FindResult:=FindFirst(SetDirName(sd)+'*.*',faAllFiles,DirInfo);
    while FindResult=0 do with DirInfo do begin
      if NotSpecialDir(Name) then begin
        inc(fc);
        (* Dateien l�schen *)
        s:=SetDirName(sd)+Name;
        FileSetAttr(s,faArchive);
        if DeleteFile(s) then begin
          inc(FCount); inc(dc);
          end
        else inc(ECount);  // Fehler
        end;
      FindResult:=FindNext(DirInfo);
      end;
    FindClose(DirInfo);
    if (fc=dc) and (DeleteRoot or (length(Dir)>0)) then begin   // Verzeichnis leer ==> l�schen
      FileSetAttr(sd,0);    // Attribute zum L�schen entfernen
      if RemoveDir(sd) then inc(DCount) else inc(ECount);
      end;
    end;
  end;

function DeleteDirectory (const Directory : string;
                          DeleteRoot      : boolean) : boolean;
var
  fc,dc,ec : cardinal;
begin
  fc:=0; dc:=0; ec:=0;
  DeleteDirectory(Directory,'',DeleteRoot,dc,fc,ec);
  Result:=ec=0;
  end;

{ ---------------------------------------------------------------- }
// pr�fen, ob auf ein Verzeichnis zugegriffen werden  kann
function CanAccess (const Directory : string; var ErrorCode : integer) : boolean;
var
  fd : TSearchRec;
begin
  if not DirectoryExists(Directory) then ErrorCode:=ERROR_PATH_NOT_FOUND
  else ErrorCode:=FindFirst(SetDirName(Directory)+'*.*',faAnyFile,fd);
  Result:=(ErrorCode=0) or (Error=ERROR_NO_MORE_FILES);
  end;

function CanAccess (const Directory : string) : boolean; overload;
var
  ec : integer;
begin
  Result:=CanAccess(Directory,ec);
  end;

{ ------------------------------------------------------------------- }
{$IFDEF Trace}
var
  fDebug : TextFile;
  DebugOn : boolean;

procedure OpenDebugLog (Filename : string);
begin
  Filename:=SetDirName(TempDirectory)+Filename;
  AssignFile(fDebug,Filename);
  if FileExists(Filename) then begin
    {$I-} Append (fDebug) {$I+};
    end
  else {$I-} Rewrite(fDebug) {$I+};
  DebugOn:=IoResult=0;
  if DebugOn then begin
    Writeln(fDebug,'************************************');
    Writeln(fDebug,'Trace started at '+DateTimeToStr(Now));
    Flush(fDebug);
    end;
  end;

procedure CloseDebugLog;
begin
  if DebugOn then begin
    Writeln(fDebug,'Trace ended at '+DateTimeToStr(Now));
    CloseFile(fDebug);
    end;
  end;

procedure WriteDebugLog (const DebugText: string);
begin
  if DebugOn then begin
    Writeln(fDebug,DebugText+' -> '+DateTimeToStr(Now));
    Flush(fDebug);
    end;
  end;
{$ENDIF}

end.
