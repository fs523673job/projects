(* Delphi-Unit
   Lesen und Schreiben in Ini-Dateien ohne Objekt (TIniFiles)
   ==========================================================
   Vorteil: Keine Exceptions, Fehler wird als Funktionswert zur�ckgegeben

   � Dr. J. Rathlev, D-24222 Schwentinental (info(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND,either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   letzte �nderung  - Apr. 2013
   *)

unit IniFileUtils;

interface

// Read from ini file
function ReadStringFromIni (IniName,Section,Ident,Default : string) : string;
function ReadIntegerFromIni (IniName,Section,Ident : string; Default : integer) : integer;
function ReadBoolFromIni (IniName,Section,Ident : string; Default : boolean) : boolean;

// Write to ini file
function WriteStringToIniFile(const Filename,Section,Ident,Value: String) : boolean;
function WriteIntegerToIniFile(const Filename,Section,Ident: string; Value: Longint) : boolean;
function WriteBoolToIniFile(const Filename,Section,Ident: string; Value: Boolean) : boolean;
function EraseSectionFromIniFile(const Filename,Section: string) : boolean;
function DeleteKeyFromIniFile(const Filename,Section,Ident: String) : boolean;
function UpdateIniFile(const Filename) : boolean;

{ ---------------------------------------------------------------- }
implementation

uses System.SysUtils, Winapi.Windows, System.IniFiles;

//-----------------------------------------------------------------------------
function ReadStringFromIni (IniName,Section,Ident,Default : string) : string;
begin
  with TIniFile.Create(IniName) do begin
    Result:=ReadString(Section,Ident,Default);
    Free;
    end;
  end;

function ReadIntegerFromIni (IniName,Section,Ident : string; Default : integer) : integer;
begin
  with TIniFile.Create(IniName) do begin
    Result:=ReadInteger(Section,Ident,Default);
    Free;
    end;
  end;

function ReadBoolFromIni (IniName,Section,Ident : string; Default : boolean) : boolean;
begin
  with TIniFile.Create(IniName) do begin
    Result:=ReadBool(Section,Ident,Default);
    Free;
    end;
  end;

//-----------------------------------------------------------------------------
function WriteStringToIniFile(const Filename,Section,Ident,Value: String) : boolean;
begin
  Result:=WritePrivateProfileString(PChar(Section),PChar(Ident),PChar(Value),PChar(FileName));
  end;

function WriteIntegerToIniFile(const Filename,Section,Ident: string; Value: Longint) : boolean;
begin
  Result:= WriteStringToIniFile(Filename,Section,Ident,IntToStr(Value));
  end;

function WriteBoolToIniFile(const Filename,Section,Ident: string; Value: Boolean) : boolean;
const
  Values: array[Boolean] of string = ('0','1');
begin
  Result:=WriteStringToIniFile(Filename,Section,Ident,Values[Value]);
  end;

function EraseSectionFromIniFile(const Filename,Section: string) : boolean;
begin
  Result:=WritePrivateProfileString(PChar(Section),nil,nil,PChar(FileName));
  end;

function DeleteKeyFromIniFile(const Filename,Section,Ident: String) : boolean;
begin
  Result:=WritePrivateProfileString(PChar(Section),PChar(Ident),nil,PChar(FileName));
  end;

function UpdateIniFile(const Filename) : boolean;
begin
  Result:=WritePrivateProfileString(nil,nil,nil,PChar(FileName));
  end;

end.
 
