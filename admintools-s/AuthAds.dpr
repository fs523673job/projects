(* Authenfizieren eines ADS-Benutzers

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU General Public License Version 2 or later (the "GPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1.0 - March 2009
   Vers. 1.1 - May 2012
   Vers. 2.1 - October 2015 - Replacement for ParamStr (see workaround below)
   Vers. 2.5 - January 2016

   Aufruf:
   AuthAds <domain> <username> <password> [<group>] [<coded password>]

   Das Programm prüft, ob in der Domäne <domain> der Benutzer <username> mit
   dem Kennwort <password> authentifiziert werden kann.
   Optional kann die Gruppenzugehörigkeit zu <group> geprüft werden.
   Wenn <password> = base64 ist, wird der Parameter <coded password> benötigt.
   Er stellt das base64-(bzw. mime-)kodierte Passwort dar. In diesem Fall
   muss der Parameter <group> immer angegeben werden. Falls keine Gruppe
   geprüft werden soll, muss hier eine Leerstring ("") stehen.

   Rückgabewert (Exitcode=ErrorLevel):
   0 : OK
   1 : falsche oder fehlende Parameter (z.B. -? für Hilfe)
   2 : Domäne nicht gefunden
   3 : Authentifizierung fehlgeschlagen
   4 : keine Gruppenzugehörigkeit festgestellt

   *)

program AuthAds;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  WinApi.Windows,
  JwaAdsTLB,
  JwaAdsHlp,
  JwaAdsErr,
//  Adsutils in 'Adsutils.pas',
  WinApi.ShellApi,
  WinApi.ActiveX;

const
  PrgName = 'Authenticate ADS user';
  Vers = ' (Vers. 2.5)';
  CopRgt = '© 2016 Dr. J. Rathlev, D-24222 Schwentinental';

//  OuColNames : array[0..OuColCount-1] of WideString =
  OuCols = 'Name,Description,ADsPath';

//  ColCount = 3;
//  ColNames : array[0..ColCount-1] of WideString = ('Name','Description','ADsPath');

type
  TPWideArray = array of PWideChar;

{ ---------------------------------------------------------------- }
var
  Domain,UserName,
  Password,GroupName,
  ap,gp,pw    : string;
  obj         : IAds;
  Group       : IDispatch;
  ok          : boolean;
  CheckResult : integer;
  n,ColCount  : integer;
  ColNames    : TPWideArray;

{ ---------------------------------------------------------------- }
function ReadNxtStr (var s   : String;
                     Del     : char) : string;
var
  i : integer;
begin
  if length(s)>0 then begin
    i:=pos (Del,s);
    if i=0 then i:=succ(length(s));
    ReadNxtStr:=copy(s,1,pred(i));
    delete(s,1,i);
    end
  else ReadNxtStr:='';
  end;

procedure StringToPwArray (SListe : string; var PwArr : TPWideArray; var Count : integer);
var
  s : string;
begin
  Count:=0;
  while length(SListe)>0 do begin
    s:=ReadNxtStr(SListe,',');
    if length(s)>0 then begin
      SetLength(PwArr,Count+1);
      GetMem(PwArr[Count],256);
      StringToWideChar(s,PwArr[Count],256);
      inc(Count)
      end;
    end;
  end;

{ ---------------------------------------------------------------- }
// Workaround to allow \" and "" (empty argument) in command line arguments
// see: http://qc.embarcadero.com/wc/qcmain.aspx?d=43340
type
  PInteger = ^Integer;

function GetNextParam(var CmdLine: PChar; Buffer: PChar; Len: PInteger): Boolean;
var
  InQuotedStr, IsOdd: Boolean;
  NumSlashes, NewLen, I: Integer;
begin
  Result := False;
  if Len <> nil then Len^ := 0;
  if CmdLine = nil then Exit;
  while (CmdLine^ <= ' ') and (CmdLine^ <> #0) do
    CmdLine := CharNext(CmdLine);
  if CmdLine^ = #0 then Exit;
  InQuotedStr := False;
  NewLen := 0;
  repeat
    if CmdLine^ = '\' then
    begin
      NumSlashes := 0;
      repeat
        Inc(NumSlashes);
        CmdLine := CharNext(CmdLine);
      until CmdLine^ <> '\';
      if CmdLine^ = '"' then
      begin
        IsOdd := (NumSlashes mod 2) <> 0;
        NumSlashes := NumSlashes div 2;
        Inc(NewLen, NumSlashes);
        if IsOdd then Inc(NewLen);
        if Buffer <> nil then
        begin
          for I := 0 to NumSlashes-1 do
          begin
            Buffer^ := '\';
            Inc(Buffer);
          end;
          if IsOdd then
          begin
            Buffer^ := '"';
            Inc(Buffer);
          end;
        end;
        if IsOdd then CmdLine := CharNext(CmdLine);
      end else
      begin
        Inc(NewLen, NumSlashes);
        if Buffer <> nil then
        begin
          for I := 0 to NumSlashes-1 do
          begin
            Buffer^ := '\';
            Inc(Buffer);
          end;
        end;
      end;
      Continue;
    end;
    if CmdLine^ <> '"' then
    begin
      if (CmdLine^ <= ' ') and (not InQuotedStr) then Break;
      Inc(NewLen);
      if Buffer <> nil then
      begin
        Buffer^ := CmdLine^;
        Inc(Buffer);
      end;
    end else
      InQuotedStr := not InQuotedStr;
    CmdLine := CharNext(CmdLine);
  until CmdLine^ = #0;
  if Len <> nil then Len^ := NewLen;
  Result := True;
end;

function GetParamStr(Index: Integer): String;
var
  Buffer: array[0..MAX_PATH] of Char;
  CmdLine, P: PChar;
  Len: Integer;
begin
  Result := '';
  if Index <= 0 then
  begin
    Len := GetModuleFileName(0, Buffer, MAX_PATH+1);
    SetString(Result, Buffer, Len);
  end else
  begin
    CmdLine := GetCommandLine;
    GetNextParam(CmdLine, nil, nil);
    repeat
      Dec(Index);
      if Index = 0 then Break;
      if not GetNextParam(CmdLine, nil, nil) then Exit;
    until False;
    P := CmdLine;
    if GetNextParam(P, nil, @Len) then
    begin
      SetLength(Result, Len);
      GetNextParam(CmdLine, PChar(Result), nil);
    end;
  end;
end;

function GetParamCount: Integer;
var
  CmdLine: PChar;
begin
  Result := 0;
  CmdLine := GetCommandLine;
  GetNextParam(CmdLine, nil, nil);
  while GetNextParam(CmdLine, nil, nil) do Inc(Result);
end;

{ ---------------------------------------------------------------- }
// Routines from JclMime
type
  TJclAddr = Cardinal;

  PByte3 = ^TByte3;
  TByte3 = packed record
    B1: Byte;
    B2: Byte;
    B3: Byte;
  end;

const
  MIME_DECODE_TABLE: array [Byte] of Byte = (
    255, 255, 255, 255, 255, 255, 255, 255, //   0 -   7
    255, 255, 255, 255, 255, 255, 255, 255, //   8 -  15
    255, 255, 255, 255, 255, 255, 255, 255, //  16 -  23
    255, 255, 255, 255, 255, 255, 255, 255, //  24 -  31
    255, 255, 255, 255, 255, 255, 255, 255, //  32 -  39
    255, 255, 255, 062, 255, 255, 255, 063, //  40 -  47
    052, 053, 054, 055, 056, 057, 058, 059, //  48 -  55
    060, 061, 255, 255, 255, 255, 255, 255, //  56 -  63
    255, 000, 001, 002, 003, 004, 005, 006, //  64 -  71
    007, 008, 009, 010, 011, 012, 013, 014, //  72 -  79
    015, 016, 017, 018, 019, 020, 021, 022, //  80 -  87
    023, 024, 025, 255, 255, 255, 255, 255, //  88 -  95
    255, 026, 027, 028, 029, 030, 031, 032, //  96 - 103
    033, 034, 035, 036, 037, 038, 039, 040, // 104 - 111
    041, 042, 043, 044, 045, 046, 047, 048, // 112 - 119
    049, 050, 051, 255, 255, 255, 255, 255, // 120 - 127
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255);

function MimeDecodedSize(const InputSize: integer): integer;
begin
  Result := (InputSize + 3) div 4 * 3;
end;

function MimeDecodePartial(const InputBuffer; const InputByteCount: integer; out OutputBuffer;
  var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): integer;
var
  LByteBuffer, LByteBufferSpace, C: Cardinal;
  InPtr, OuterLimit: PByte;
  OutPtr: PByte3;
begin
  if InputByteCount > 0 then
  begin
    InPtr := @InputBuffer;
    OuterLimit := Pointer(TJclAddr(InPtr) + TJclAddr(InputByteCount));
    OutPtr := @OutputBuffer;
    LByteBuffer := ByteBuffer;
    LByteBufferSpace := ByteBufferSpace;
    while InPtr <> OuterLimit do
    begin
      { Read from InputBuffer. }
      C := MIME_DECODE_TABLE[InPtr^];
      Inc(InPtr);
      if C = $FF then
        Continue;
      LByteBuffer := LByteBuffer shl 6;
      LByteBuffer := LByteBuffer or C;
      Dec(LByteBufferSpace);
      { Have we read 4 bytes from InputBuffer? }
      if LByteBufferSpace <> 0 then
        Continue;

      { Write 3 bytes to OutputBuffer (in reverse order). }
      OutPtr^.B3 := Byte(LByteBuffer);
      LByteBuffer := LByteBuffer shr 8;
      OutPtr^.B2 := Byte(LByteBuffer);
      LByteBuffer := LByteBuffer shr 8;
      OutPtr^.B1 := Byte(LByteBuffer);
      LByteBuffer := 0;
      Inc(OutPtr);
      LByteBufferSpace := 4;
    end;
    ByteBuffer := LByteBuffer;
    ByteBufferSpace := LByteBufferSpace;
    Result := integer(TJclAddr(OutPtr) - TJclAddr(@OutputBuffer));
  end
  else
    Result := 0;
end;

function MimeDecodePartialEnd(out OutputBuffer; const ByteBuffer: Cardinal;
  const ByteBufferSpace: Cardinal): integer;
var
  LByteBuffer: Cardinal;
begin
  case ByteBufferSpace of
    1:
      begin
        LByteBuffer := ByteBuffer shr 2;
        PByte3(@OutputBuffer)^.B2 := Byte(LByteBuffer);
        LByteBuffer := LByteBuffer shr 8;
        PByte3(@OutputBuffer)^.B1 := Byte(LByteBuffer);
        Result := 2;
      end;
    2:
      begin
        LByteBuffer := ByteBuffer shr 4;
        PByte3(@OutputBuffer)^.B1 := Byte(LByteBuffer);
        Result := 1;
      end;
  else
    Result := 0;
  end;
end;

function MimeDecodeString(const S: AnsiString): AnsiString;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  L: integer;
  P, R: PAnsiChar;
begin
  if S <> '' then
  begin
    L := Length(S);
    SetLength(Result, MimeDecodedSize(L));
    ByteBuffer := 0;
    ByteBufferSpace := 4;
    P := PAnsiChar(S);
    R := PAnsiChar(Result);
    L := MimeDecodePartial(P^, L, R^, ByteBuffer, ByteBufferSpace);
    Inc(R, L);
    Inc(L, MimeDecodePartialEnd(R^, ByteBuffer, ByteBufferSpace));
    SetLength(Result, L);
  end
  else
    Result := '';
end;

{ ---------------------------------------------------------------- }
procedure ShowHelp;
begin
  writeln (PrgName+Vers);
  writeln (CopRgt);
  writeln;
  writeln ('Calling:');
  writeln ('AuthAds <Domain> <Username> <Password> [<Group>]');
  writeln ('  Domain:   Name of Windows domain where to authenticate');
  writeln ('  Username: User domain account name');
  writeln ('  Password: User password');
  writeln ('  Group:    Check for group membership (optional)');
  writeln;
  CheckResult:=1;
  end;

{ ---------------------------------------------------------------- }
function ConnectToDomain : boolean;
const
  szLen = 256;

var
  bind         : widestring;
  root         : IADs;
begin
  Result:=false;
  bind:='LDAP://'+Domain;
  try
    ADsGetObject(PWideChar(bind),IADs,pointer(root));
    Result:=true;
  except
    end;
  root:=nil;
  end;

{ ---------------------------------------------------------------- }
// Domänen-Gruppe suchen, Rückgabewert: ADSPath
function SearchGroup(CommonName : string) : string;
var
  DSearch      : IDirectorySearch;
  opt          : array[0..0] of ads_searchpref_info; // has to be an array
  ptrResult    : THandle;
  col          : ads_search_column;
begin
  Result:='';
  ADsGetObject(PWideChar('LDAP://'+Domain),IDirectorySearch,pointer(DSearch));
  with opt[0] do begin
    dwSearchPref:=ADS_SEARCHPREF_SEARCH_SCOPE;
    vValue.dwType:=ADSTYPE_INTEGER;
    vValue.Integer:=ADS_SCOPE_SUBTREE;
    end;
  with DSearch do begin
    if Succeeded(SetSearchPreference(@opt[0],1)) then begin
      ExecuteSearch(PWideChar('(&(objectClass=group)(Name='+CommonName+'))'),@ColNames,
                    ColCount,ptrResult);
      if GetNextRow(ptrResult)<>S_ADS_NOMORE_ROWS then begin
        if Succeeded(GetColumn(ptrResult,ColNames[2],col)) then begin
          with col do if pADsValues<>nil then Result:=pAdsvalues^.CaseIgnoreString;
          FreeColumn(@col);
          end;
        end
      end
    end;
  DSearch:=nil;
  end;

{ ---------------------------------------------------------------- }
// Domänen-Benutzer suchen, Rückgabewert: ADSPath
function SearchUser (CommonName : string) : string;
var
  DSearch      : IDirectorySearch;
  opt          : array[0..0] of ads_searchpref_info; // has to be an array
  ptrResult    : THandle;
  col          : ads_search_column;
begin
  Result:='';
  ADsGetObject(PWideChar('LDAP://'+Domain),IDirectorySearch,pointer(DSearch));
  with opt[0] do begin
    dwSearchPref:=ADS_SEARCHPREF_SEARCH_SCOPE;
    vValue.dwType:=ADSTYPE_INTEGER;
    vValue.Integer:=ADS_SCOPE_SUBTREE;
    end;
  with DSearch do begin
    if Succeeded(SetSearchPreference(@opt[0],1)) then begin
      ExecuteSearch(PWideChar('(&(objectClass=user)(sAMAccountName='+CommonName+'))'),
                    @ColNames,ColCount,ptrResult);
      if GetNextRow(ptrResult)<>S_ADS_NOMORE_ROWS then begin
        if Succeeded(GetColumn(ptrResult,ColNames[2],col)) then begin
          with col do if pADsValues<>nil then Result:=pAdsvalues^.CaseIgnoreString;
          FreeColumn(@col);
          end;
        end
      end
    end;
  DSearch:=nil;
  end;

{ ---------------------------------------------------------------- }
procedure ExecuteProgram;
begin
  n:=GetParamCount;
  if failed (CoInitialize(nil)) then CheckResult:=2
  else if n>0 then begin
    Domain:=GetParamStr(1);
    if (Domain='-?') or (Domain='/?') then ShowHelp;
    UserName:=GetParamStr(2);  // ParamStr(2);
    Password:=GetParamStr(3);  // PParamStr(3);
    if AnsiSameText(Password,'base64') then if n>4 then Password:=MimeDecodeString(GetParamStr(5));
    if n>3 then GroupName:=GetParamStr(4) else GroupName:='';
    if (length(UserName)=0) or (length(Password)=0) then CheckResult:=1
    else if not ConnectToDomain then CheckResult:=2
    else begin
      StringToPwArray(OuCols,colnames,ColCount);
      ok:=ADsOpenObject(PWideChar('LDAP://'+Domain),PWideChar(UserName),PWideChar(Password),
                                  ADS_SECURE_AUTHENTICATION,IAds,pointer(obj))=S_OK;
      obj:=nil;
      if ok then begin
        if length(GroupName)>0 then begin
          ap:=SearchUser (UserName);
          gp:=SearchGroup(GroupName);
          if length(gp)>0 then begin
            ADsGetObject(PWideChar(gp),IID_IADsGroup,pointer(Group));
            if (Group as IAdsGroup).IsMember(ap) then CheckResult:=0 else
            CheckResult:=4;
            Group:=nil;
            end
          else CheckResult:=4;
          end
        else CheckResult:=0;
        end
      else CheckResult:=3;
      end;
    end
  else ShowHelp;
  writeln ('Exitcode = ',CheckResult);
//  Readln;
  ExitCode:=CheckResult;
  end;

{ ---------------------------------------------------------------- }
begin
  try
    ExecuteProgram;
  except
    end;
  CoUninitialize;
  end.
