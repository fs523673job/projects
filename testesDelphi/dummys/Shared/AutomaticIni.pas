unit AutomaticIni;

interface

uses
  Classes, SysUtils, Variants, TypInfo, IniFiles, StdCtrls;

type
  TAutomaticIni = class(TPersistent)
  private
    FPathIni: String;
    FNameIni: String;
    FSection: String;
  public
    procedure Post(const ASection: String = '');
    procedure Load(const ASection: String = '');
    constructor Create;
    function FileIniPath: String;
  strict protected
    property PathIni: String read FPathIni write FPathIni;
    property NameIni: String read FNameIni write FNameIni;
    property Section: String read FSection;
  end;

{$M-}

implementation

{ TAutomaticIni }

constructor TAutomaticIni.Create;
begin
  FPathIni := ExtractFilePath(ParamStr(0));
  FNameIni := 'LdapTest';
end;

function TAutomaticIni.FileIniPath: String;
begin
  Result := Format('%0:s\%1:s.ini', [FPathIni, FNameIni]);
end;

procedure TAutomaticIni.Load(const ASection: String = '');
var
  IniFile: TIniFile;
  Count, Size, c: Integer;
  List: PPropList;
  PropInfo: PPropInfo;
  PropValue: Variant;
begin
  FSection := ASection;
  if (FSection = '') then
    FSection := FNameIni;

  Count := GetPropList(Self.ClassInfo, tkAny, nil);
  Size  := Count * SizeOf(Pointer);
  GetMem(List, Size);
  try
    GetPropList(Self.ClassInfo, tkAny, List);

    IniFile := TIniFile.Create(Format('%0:s\%1:s.ini', [FPathIni, FNameIni]));
    try
      for c := 0 to Count - 1 do
      begin
        PropInfo  := List^[c];
        case PropInfo^.PropType^.Kind of
          tkUString,
          tkString,
          tkLString : PropValue := IniFile.ReadString  (FSection, String(PropInfo^.Name), GetPropValue(Self, String(PropInfo^.Name)));
          tkInteger : PropValue := IniFile.ReadInteger (FSection, String(PropInfo^.Name), GetPropValue(Self, String(PropInfo^.Name)));
          else
            PropValue := IniFile.ReadString  (FSection, String(PropInfo^.Name), GetPropValue(Self, String(PropInfo^.Name)));
        end;

        if (VarToStr(PropValue) <> '') then
        begin
          if (PropInfo^.SetProc <> nil) then
            SetPropValue(Self, String(PropInfo^.Name), PropValue);
        end;
      end;
    finally
      FreeAndNil(IniFile);
    end;
  finally
    FreeMem(List, Size);
  end;
end;

procedure TAutomaticIni.Post;
var
  IniFile: TIniFile;
  Count, Size, c: Integer;
  List: PPropList;
  PropInfo: PPropInfo;
  PropValue: Variant;
begin
  FSection := ASection;
  if (FSection = '') then
    FSection := FNameIni;

  Count := GetPropList(Self.ClassInfo, tkAny, nil);
  Size  := Count * SizeOf(Pointer);
  GetMem(List, Size);
  try
    GetPropList(Self.ClassInfo, tkAny, List);

    IniFile := TIniFile.Create(Format('%0:s\%1:s.ini', [FPathIni, FNameIni]));
    try
      for c := 0 to Count - 1 do
      begin
        PropInfo  := List^[c];
        PropValue := GetPropValue(Self, String(PropInfo^.Name));
        case PropInfo^.PropType^.Kind of
          tkString, tkUString,
          tkLString : IniFile.WriteString  (FSection, String(PropInfo^.Name), VarToStr(PropValue));
          tkInteger : IniFile.WriteInteger (FSection, String(PropInfo^.Name), PropValue);
          else
            IniFile.WriteString  (FSection, String(PropInfo^.Name), VarToStr(PropValue));
        end;
      end;
    finally
      FreeAndNil(IniFile);
    end;
  finally
    FreeMem(List, Size);
  end;
end;

end.
