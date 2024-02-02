program dummyLdap;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Console,
  System.StrUtils,
  System.Generics.Collections,
  System.Win.ComObj,
  System.Variants,
  System.Math,
  System.Diagnostics,
  WinApi.Windows,
  JwaAdsTLB,
  JwaAdsHlp,
  JwaAdsErr,
  WinApi.ShellApi,
  WinApi.ActiveX,
  utils in '..\Shared\utils.pas',
  WinSSPI in '..\Shared\WinSSPI.pas',
  WinSSPI2 in '..\Shared\WinSSPI2.pas',
  AutomaticIni in '..\Shared\AutomaticIni.pas',
  Config in '..\Shared\Config.pas';

type
  TFunctionProc = function(const Params: TArray<String>): Boolean;
  TPasswordMode = (pmLower, pmUpper, pmNumbers, pmExtra);
  TPasswordModes = set of TPasswordMode;

var
  InputArray: TArray<string>;
  FuncMap: TDictionary<String, TFunctionProc>;
  Ldap_User: String;
  Ldap_Pass: String;
  ConfigLdap: TConfigLdap;

function GeneratePassword(ALength: Integer; Mode: TPasswordModes): string;
const
  cLower   = 'abcdefghijklmnopqrstuvwxyz';
  cUpper   = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  cNumbers = '0123456789';
  cExtra   = '_.@#$%';
var
  i : Integer;
  S : string;
  iM: BYTE;
begin
  if Mode = [] then Exit;
  i := 0;
  Randomize;
  while (i < ALength)  do
  begin
    iM := RANDOM(4);
    case iM of
      0: if (pmLower in Mode) then begin
           S := S + cLower[1+RANDOM(Length(cLower))];
           Inc(i);
         end;
      1: if (pmUpper in Mode) then begin
           S := S + cUpper[1+RANDOM(Length(cUpper))];
           Inc(i);
           Mode := Mode - [pmUpper]; // This I added
         end;
      2: if (pmNumbers in Mode) then begin
           S := S + cNumbers[1+RANDOM(Length(cNumbers))];
           Inc(i);
         end;
      3: if (pmExtra in Mode) then begin
           S := S + cExtra[1+RANDOM(Length(cExtra))];
           Inc(i);
           Mode := Mode - [pmExtra];  // This I added
         end;
    end;
  end;
  Result := S;
end;

procedure WriteResult(const AContent: String; const AContentBoolean: Boolean);
begin
  Console.Write(AContent);
  if AContentBoolean then
    Console.WriteColorLine(BoolToStr(AContentBoolean, True), [TConsoleColor.Green])
  else
    Console.WriteColorLine(BoolToStr(AContentBoolean, True), [TConsoleColor.Red]);
end;

function UserExists(const Params: TArray<String>): Boolean;
var
  ads: IADs;
begin
  if Length(Params) < 3 then
    Exit(False);

  Console.WriteLine('Domain: [%s], User: [%s], Password: [%s]', [Params[0], Params[1], Params[2]]);
  WriteResult('Connection with domain: ', (ADsGetObject(PWideChar('LDAP://' + Params[0]), IADs, Pointer(ADs)) = S_OK));
  Exit(ADsOpenObject(PWideChar('LDAP://' + Params[0]), PWideChar(Params[1]), PWideChar(Params[2]), ADS_SECURE_AUTHENTICATION, IAds, Pointer(ads)) = S_OK);
end;

function AuthenticateUser(const Params: TArray<String>): Boolean;
var
  ads: IADs;
begin
  if Length(Params) < 3 then
    Exit(False);

  Console.WriteLine('Domain: [%s], User: [%s], Password: [%s]', [Params[0], Params[1], Params[2]]);
  WriteResult('Connection with domain: ', (ADsGetObject(PWideChar('LDAP://' + Params[0]), IADs, Pointer(ADs)) = S_OK));

  try
    Exit(WinSSPI2.NTLM_CrendentialsCheck(Params[0], Params[1], Params[2]));
  except
    on e: Exception do
    begin
      Result := False;
      Console.WriteLine(e.Message);
    end;
  end;
end;

function SetPassword(const Params: TArray<String>): Boolean;
var
  NameTranslated: String;
  ADs: IADs;
  ADsNameTranslate: IADsNameTranslate;
  ADsUser: IADsUser;
begin
  if Length(Params) < 3 then
    Exit(False);

  try
    ADsNameTranslate := CreateOleObject('NameTranslate') as IADsNameTranslate;
    if not VarIsNull(ADsNameTranslate) then
    begin
      try
        ADsNameTranslate.Init(ADS_NAME_INITTYPE_GC, '');
        ADsNameTranslate.Init(ADS_NAME_INITTYPE_DOMAIN, Params[0]);
        ADsNameTranslate.Set_(ADS_NAME_TYPE_NT4, Format('%s\%s', [Params[0], Params[1]]));
        NameTranslated := ADsNameTranslate.Get(ADS_NAME_TYPE_1779);
      except
        on e : Exception do
        begin
          Console.WriteLine('Error: ' + e.Message);
          Exit(False)
        end;
      end;
    end
    else
      NameTranslated := '';

    Console.WriteLine('Domain: [%s], User: [%s], Password to set: [%s], Name Translated [%s]', [Params[0], Params[1], Params[2], NameTranslated]);
    WriteResult('Connection with domain: ', (ADsGetObject(PWideChar('LDAP://' + Params[0]), IADs, Pointer(ADs)) = S_OK));

    if ADsOpenObject(PChar('LDAP://' + NameTranslated), PChar(Ldap_User), PChar(Ldap_Pass), ADS_SECURE_AUTHENTICATION, IADsUser, Pointer(ADsUser)) = S_OK then
    begin
      ADsUser.SetPassword(Params[2]);
      ADsUser.SetInfo;

      Exit(True);
    end
    else
      Exit(False);
  except
    on e: Exception do
    begin
      Result := False;
      Console.WriteLine(e.Message);
    end;
  end;
end;

function ChangePassword(const Params: TArray<String>): Boolean;
var
  NameTranslated: String;
  ADs: IADs;
  ADsNameTranslate: IADsNameTranslate;
  ADsUser: IADsUser;
begin
  try
    if Length(Params) < 4 then
      Exit(False);

    ADsNameTranslate := CreateOleObject('NameTranslate') as IADsNameTranslate;
    if not VarIsNull(ADsNameTranslate) then
    begin
      try
        ADsNameTranslate.Init(ADS_NAME_INITTYPE_GC, '');
        ADsNameTranslate.Init(ADS_NAME_INITTYPE_DOMAIN, Params[0]);
        ADsNameTranslate.Set_(ADS_NAME_TYPE_NT4, Format('%s\%s', [Params[0], Params[1]]));
        NameTranslated := ADsNameTranslate.Get(ADS_NAME_TYPE_1779);
      except
        on e: Exception do
        begin
          Console.WriteLine(e.Message);
          Exit(False);
        end;
      end;
    end
    else
      NameTranslated := '';

    Console.WriteLine('Domain: [%s], User: [%s], Password Actual: [%s], Password New: [%s], Name Translated [%s]', [Params[0], Params[1], Params[2], Params[3], NameTranslated]);
    WriteResult('Connection with domain: ', (ADsGetObject(PWideChar('LDAP://' + Params[0]), IADs, Pointer(ADs)) = S_OK));
    WriteResult('User exist: : ', ADsOpenObject(PWideChar('LDAP://' + Params[0]), PWideChar(Params[1]), PWideChar(Params[2]), ADS_SECURE_AUTHENTICATION, IAds, Pointer(ADs)) = S_OK);

    if ADsOpenObject(PChar('LDAP://' + NameTranslated), PChar(Ldap_User), PChar(Ldap_Pass), ADS_SECURE_AUTHENTICATION, IADsUser, Pointer(ADsUser)) = S_OK then
    begin
      ADsUser.ChangePassword(Params[2], Params[3]);
      ADsUser.SetInfo;

      Exit(True);
    end
    else
      Exit(False);
  except
    on e: Exception do
    begin
      Result := False;
      Console.WriteLine(e.Message);
    end;
  end;
end;

function SetUserLdap(const Params: TArray<String>): Boolean;
begin
  if Length(Params) < 2 then
    Exit(False);

  Ldap_User := Params[0];
  Ldap_Pass := Params[1];
  Console.WriteLine('LDAP User: [%s], LDAP Password: [%s]', [Params[0], Params[1]]);

  Result := True;
end;

function Clear(const Params: TArray<String>): Boolean;
begin
  Console.Clear;
  Exit(True);
end;

function AutoTest_General: Boolean;
var
  localParams: TArray<String>;
  testResult: Boolean;
begin
  Console.WriteColorLine('Start - Test', [TConsoleColor.Yellow]);
  try
    Console.WriteColorLine('Setting User Ldap', [TConsoleColor.Blue]);
    localParams := TArray<String>.Create(ConfigLdap.LDAPUser, ConfigLdap.LDAPPass);
    testResult := SetUserLdap(localParams);
    WriteResult('Result: ', testResult);

    Console.WriteColorLine('Setting Password', [TConsoleColor.Blue]);
    localParams := TArray<String>.Create(ConfigLdap.TestDomanin, ConfigLdap.TestUser, ConfigLdap.TestPass);
    testResult := SetPassword(localParams);
    WriteResult('Result: ', testResult);

    Console.WriteColorLine('Verify User', [TConsoleColor.Blue]);
    localParams := TArray<String>.Create(ConfigLdap.TestDomanin, ConfigLdap.TestUser, ConfigLdap.TestPass);
    testResult := UserExists(localParams);
    WriteResult('Result: ', testResult);

    Console.WriteColorLine('Setting Password', [TConsoleColor.Blue]);
    localParams := TArray<String>.Create(ConfigLdap.TestDomanin, ConfigLdap.TestUser, 'xghwu$Thyt@#56g3)');
    testResult := SetPassword(localParams);
    WriteResult('Result: ', testResult);

    Console.WriteColorLine('Verify User', [TConsoleColor.Blue]);
    localParams := TArray<String>.Create(ConfigLdap.TestDomanin, ConfigLdap.TestUser, 'xghwu$Thyt@#56g3)');
    testResult := UserExists(localParams);
    WriteResult('Result: ', testResult);

    Console.WriteColorLine('Authenticate User', [TConsoleColor.Blue]);
    localParams := TArray<String>.Create(ConfigLdap.TestDomanin, ConfigLdap.TestUser, 'xghwu$Thyt@#56g3)');
    testResult := AuthenticateUser(localParams);
    WriteResult('Result: ', testResult);

    Console.WriteColorLine('Change Password', [TConsoleColor.Blue]);
    localParams := TArray<String>.Create(ConfigLdap.TestDomanin, ConfigLdap.TestUser, 'xghwu$Thyt@#56g3)', 'odkurP#ikjkdi98@)');
    testResult := ChangePassword(localParams);
    WriteResult('Result: ', testResult);

    Console.WriteColorLine('Verify User', [TConsoleColor.Blue]);
    localParams := TArray<String>.Create(ConfigLdap.TestDomanin, ConfigLdap.TestUser, 'odkurP#ikjkdi98@)');
    testResult := UserExists(localParams);
    WriteResult('Result: ', testResult);

    Console.WriteColorLine('Authenticate User', [TConsoleColor.Blue]);
    localParams := TArray<String>.Create(ConfigLdap.TestDomanin, ConfigLdap.TestUser, 'odkurP#ikjkdi98@)');
    testResult := AuthenticateUser(localParams);
    WriteResult('Result: ', testResult);
  finally
    if Assigned(localParams) then
      Finalize(localParams);

    Console.WriteColorLine('End - Test', [TConsoleColor.Yellow]);
  end;
  Exit(True);
end;

function AutoTest_SetPassword: Boolean;
var
  localParams: TArray<String>;
  testResult: Boolean;
  passTemp: String;
  c: Integer;
begin
  Console.WriteColorLine('Start - Test - SetPassword', [TConsoleColor.Yellow]);
  try
    Console.WriteColorLine('Setting User Ldap', [TConsoleColor.Blue]);
    localParams := TArray<String>.Create(ConfigLdap.LDAPUser, ConfigLdap.LDAPPass);
    testResult := SetUserLdap(localParams);
    WriteResult('Result: ', testResult);

    for c := 1 to 10 do
    begin
      Randomize;
      passTemp := GeneratePassword(RandomRange(10, 15),[pmLower,pmUpper,pmNumbers,pmExtra]);

      Console.WriteColorLine('Setting Password', [TConsoleColor.Blue]);
      localParams := TArray<String>.Create(ConfigLdap.TestDomanin, ConfigLdap.TestUser, passTemp);
      testResult := SetPassword(localParams);
      WriteResult('Result: ', testResult);

      Console.WriteColorLine('Verify User', [TConsoleColor.Blue]);
      localParams := TArray<String>.Create(ConfigLdap.TestDomanin, ConfigLdap.TestUser, passTemp);
      testResult := UserExists(localParams);
      WriteResult('Result: ', testResult);

      Console.WriteColorLine('Authenticate User', [TConsoleColor.Blue]);
      localParams := TArray<String>.Create(ConfigLdap.TestDomanin, ConfigLdap.TestUser, passTemp);
      testResult := AuthenticateUser(localParams);
      WriteResult('Result: ', testResult);
    end;
  finally
    if Assigned(localParams) then
      Finalize(localParams);

    Console.WriteColorLine('End - Test - SetPassword', [TConsoleColor.Yellow]);
  end;
  Exit(True);
end;

function AutoTest_ChangePassword: Boolean;

var
  localParams: TArray<String>;
  testResult: Boolean;
  passOld: String;
  passNew: String;
  c: Integer;

begin
  Console.WriteColorLine('Start - Test - ChangePassword', [TConsoleColor.Yellow]);
  try
    Console.WriteColorLine('Setting User Ldap', [TConsoleColor.Blue]);
    localParams := TArray<String>.Create(ConfigLdap.LDAPUser, ConfigLdap.LDAPPass);
    testResult := SetUserLdap(localParams);
    WriteResult('Result: ', testResult);

    Randomize;
    passOld := GeneratePassword(RandomRange(15, 25),[pmLower,pmUpper,pmNumbers,pmExtra]);

    Console.WriteColorLine('Setting Password', [TConsoleColor.Blue]);
    localParams := TArray<String>.Create(ConfigLdap.TestDomanin, ConfigLdap.TestUser, passOld);
    testResult := SetPassword(localParams);
    WriteResult('Result: ', testResult);

    for c := 1 to 10 do
    begin
      Randomize;
      passNew := GeneratePassword(RandomRange(15, 25),[pmLower,pmUpper,pmNumbers,pmExtra]);

      Console.WriteColorLine('Change Password', [TConsoleColor.Blue]);
      localParams := TArray<String>.Create(ConfigLdap.TestDomanin, ConfigLdap.TestUser, passOld, passNew);
      testResult := ChangePassword(localParams);
      WriteResult('Result: ', testResult);

      Console.WriteColorLine('Verify User', [TConsoleColor.Blue]);
      localParams := TArray<String>.Create(ConfigLdap.TestDomanin, ConfigLdap.TestUser, passNew);
      testResult := UserExists(localParams);
      WriteResult('Result: ', testResult);

      Console.WriteColorLine('Authenticate User', [TConsoleColor.Blue]);
      localParams := TArray<String>.Create(ConfigLdap.TestDomanin, ConfigLdap.TestUser, passNew);
      testResult := AuthenticateUser(localParams);
      WriteResult('Result: ', testResult);

      passOld := passNew;
    end;
  finally
    if Assigned(localParams) then
      Finalize(localParams);

    Console.WriteColorLine('End - Test - SetPassword', [TConsoleColor.Yellow]);
  end;
  Exit(True);
end;

function AutoTest_TestLogin(const ATestPass: String): Boolean;
var
  localParams: TArray<String>;
  testResult: Boolean;
  ldapAutenthicate: Boolean;
  ntlmAuthenthicate: Boolean;
  stopWatch: TStopWatch;
  numLoop: Integer;
begin
  Console.WriteColorLine('Start - Test - TestLogin', [TConsoleColor.Yellow]);
  try
    Console.WriteColorLine('Setting User Ldap', [TConsoleColor.Blue]);
    localParams := TArray<String>.Create(ConfigLdap.LDAPUser, ConfigLdap.LDAPPass);
    testResult := SetUserLdap(localParams);
    WriteResult('Result: ', testResult);

    stopWatch := TStopWatch.StartNew;

    numLoop := 1;
    while True do
    begin
      Console.WriteColorLine(Format('%.2d - Verify User', [numLoop]), [TConsoleColor.Blue]);
      localParams := TArray<String>.Create(ConfigLdap.TestDomanin, ConfigLdap.TestUser, ATestPass);
      testResult := UserExists(localParams);
      WriteResult(Format('%.2d - Result: ', [numLoop]), testResult);

      ldapAutenthicate := testResult;

      Console.WriteColorLine(Format('%.2d - Authenticate User', [numLoop]), [TConsoleColor.Blue]);
      localParams := TArray<String>.Create(ConfigLdap.TestDomanin, ConfigLdap.TestUser, ATestPass);
      testResult := AuthenticateUser(localParams);
      WriteResult(Format('%.2d - Result: ', [numLoop]), testResult);

      ntlmAuthenthicate := testResult;

      WriteResult(Format('%.2d - Authenticate Ldap: ', [numLoop]), ldapAutenthicate);
      WriteResult(Format('%.2d - Authenticate Ntlm: ', [numLoop]), ntlmAuthenthicate);

      if (ldapAutenthicate and ntlmAuthenthicate) then
        Break;

      if (stopWatch.ElapsedMilliseconds >= 1200000) then
        raise Exception.Create('Fail authenticate user');

      Sleep(1000);

      Inc(numLoop);
    end;
  finally
    if Assigned(localParams) then
      Finalize(localParams);

    Console.WriteColorLine('End - Test - TestLogin', [TConsoleColor.Yellow]);
  end;
  Exit(True);
end;


function AutoTest(const Params: TArray<String>): Boolean;
begin
  if (Length(Params) = 1) then
    Exit(AutoTest_General)
  else if AnsiSameText(Params[0], 'setpassword') then
    Exit(AutoTest_SetPassword)
  else if AnsiSameText(Params[0], 'changepassword') then
    Exit(AutoTest_ChangePassword)
  else if AnsiSameText(Params[0], 'testlogin') then
    Exit(AutoTest_TestLogin(Params[1]))
  else
    Exit(False);
end;


procedure CreateFileIni;
begin
  if not Assigned(ConfigLdap) then
    ConfigLdap := TConfigLdap.Create;

  if not FileExists(ConfigLdap.FileIniPath) then
  begin
    ConfigLdap.LDAPUser    := '';
    ConfigLdap.LDAPPass    := '';
    ConfigLdap.TestDomanin := '';
    ConfigLdap.TestUser    := '';
    ConfigLdap.TestPass    := '';

    ConfigLdap.Post('LDAPUSER');
  end
  else
    ConfigLdap.Load('LDAPUSER');
end;


procedure RegisterFunc;
begin
  if not Assigned(FuncMap) then
    FuncMap := TDictionary<String, TFunctionProc>.Create;

  FuncMap.Add('setuserldap', SetUserLdap);
  FuncMap.Add('userexist', UserExists);
  FuncMap.Add('clear', clear);
  FuncMap.Add('changepassword', ChangePassword);
  FuncMap.Add('setpassword', SetPassword);
  FuncMap.Add('autotest', AutoTest);
  FuncMap.Add('authenticateuser', AuthenticateUser);
end;

begin
  CoInitialize(nil);
  try
    CreateFileIni;
    RegisterFunc;
    while True do
    begin
      try
        Console.Write('>');
        InputArray := Console.ReadLine.Split([' ', sLineBreak]);

        if (Length(InputArray) > 0) and (LowerCase(InputArray[0]) = 'exit') then
          Break;

        if Length(InputArray) > 0 then
        begin
          var FuncName := LowerCase(InputArray[0]);
          var Params := Copy(InputArray, 1, Length(InputArray) - 1);

          if FuncMap.ContainsKey(FuncName) then
          begin
            if FuncMap[FuncName](Params) then
              Console.WriteColorLine('Função executada com sucesso', [TConsoleColor.Green])
            else
              Console.WriteColorLine('Função executada com erro', [TConsoleColor.Red])
          end
          else
            Console.WriteColorLine('Função não encontrada', [TConsoleColor.Red]);
        end;
      except
        on E: Exception do
          Writeln(E.ClassName, ': ', E.Message);
      end;
    end;
  finally
    CoUninitialize;
  end;
end.
