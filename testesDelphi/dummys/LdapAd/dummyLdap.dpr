program dummyLdap;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Console,
  System.Generics.Collections,
  System.Win.ComObj,
  System.Variants,
  WinApi.Windows,
  JwaAdsTLB,
  JwaAdsHlp,
  JwaAdsErr,
  WinApi.ShellApi,
  WinApi.ActiveX,
  Data.DB,
  Data.Win.ADODB,
  utils in '..\Shared\utils.pas';

type
  TFunctionProc = function(const Params: TArray<String>): Boolean;

var
  InputArray: TArray<string>;
  FuncMap: TDictionary<String, TFunctionProc>;
  Ldap_User: String;
  Ldap_Pass: String;

function UserExists(const Params: TArray<String>): Boolean;
var
  ads: IADs;
begin
  if Length(Params) < 3 then
    Exit(False);

  Console.WriteLine('Domain: [%s], User: [%s], Password: [%s]', [Params[0], Params[1], Params[2]]);
  Console.WriteLine('Connection with domain: %s', [BoolToStr((ADsGetObject(PWideChar('LDAP://' + Params[0]), IADs, Pointer(ADs)) = S_OK), True)]);
  Exit(ADsOpenObject(PWideChar('LDAP://' + Params[0]), PWideChar(Params[1]), PWideChar(Params[2]), ADS_SECURE_AUTHENTICATION, IAds, Pointer(ads)) = S_OK);
end;

function AuthenticateUser(const Params: TArray<String>): Boolean;
begin
  if Length(Params) < 3 then
    Exit(False);

  Console.WriteLine('Not implemented');
  Exit(True);
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
    Console.WriteLine('Connection with domain: %s', [BoolToStr((ADsGetObject(PWideChar('LDAP://' + Params[0]), IADs, Pointer(ADs)) = S_OK), True)]);

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
    Console.WriteLine('Connection with domain: %s', [BoolToStr((ADsGetObject(PWideChar('LDAP://' + Params[0]), IADs, Pointer(ADs)) = S_OK), True)]);
    Console.WriteLine('User exist: %s', [BoolToStr(ADsOpenObject(PWideChar('LDAP://' + Params[0]), PWideChar(Params[1]), PWideChar(Params[2]), ADS_SECURE_AUTHENTICATION, IAds, Pointer(ADs)) = S_OK, True)]);

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

function AutoTest(const Params: TArray<String>): Boolean;
var
  localParams: TArray<String>;
  testResult: Boolean;
begin
  Console.WriteColorLine('Start - Test', [TConsoleColor.Red]);
  try
    Console.WriteColorLine('Setting User Ldap', [TConsoleColor.Blue]);
    localParams := TArray<String>.Create('Flsantos', 'Fls12345@');
    testResult := SetUserLdap(localParams);
    Console.WriteLine(Format('Result: %s', [BoolToStr(testResult, True)]));

    Console.WriteColorLine('Setting Password', [TConsoleColor.Blue]);
    localParams := TArray<String>.Create('apdatatst', 'caraujo', 'CAgelado!00');
    testResult := SetPassword(localParams);
    Console.WriteLine(Format('Result: %s', [BoolToStr(testResult, True)]));

    Console.WriteColorLine('Verify User', [TConsoleColor.Blue]);
    localParams := TArray<String>.Create('apdatatst', 'caraujo', 'CAgelado!00');
    testResult := UserExists(localParams);
    Console.WriteLine(Format('Result: %s', [BoolToStr(testResult, True)]));

    Console.WriteColorLine('Setting Password', [TConsoleColor.Blue]);
    localParams := TArray<String>.Create('apdatatst', 'caraujo', 'xghwu$Thyt@#56g3)');
    testResult := SetPassword(localParams);
    Console.WriteLine(Format('Result: %s', [BoolToStr(testResult, True)]));

    Console.WriteColorLine('Verify User', [TConsoleColor.Blue]);
    localParams := TArray<String>.Create('apdatatst', 'caraujo', 'xghwu$Thyt@#56g3)');
    testResult := UserExists(localParams);
    Console.WriteLine(Format('Result: %s', [BoolToStr(testResult, True)]));

    Console.WriteColorLine('Authenticate User', [TConsoleColor.Blue]);
    localParams := TArray<String>.Create('apdatatst', 'caraujo', 'xghwu$Thyt@#56g3)');
    testResult := AuthenticateUser(localParams);
    Console.WriteLine(Format('Result: %s', [BoolToStr(testResult, True)]));

    Console.WriteColorLine('Change Password', [TConsoleColor.Blue]);
    localParams := TArray<String>.Create('apdatatst', 'caraujo', 'xghwu$Thyt@#56g3)', 'odkurP#ikjkdi98@)');
    testResult := ChangePassword(localParams);
    Console.WriteLine(Format('Result: %s', [BoolToStr(testResult, True)]));

    Console.WriteColorLine('Verify User', [TConsoleColor.Blue]);
    localParams := TArray<String>.Create('apdatatst', 'caraujo', 'odkurP#ikjkdi98@)');
    testResult := UserExists(localParams);
    Console.WriteLine(Format('Result: %s', [BoolToStr(testResult, True)]));

    Console.WriteColorLine('Authenticate User', [TConsoleColor.Blue]);
    localParams := TArray<String>.Create('apdatatst', 'caraujo', 'odkurP#ikjkdi98@)');
    testResult := AuthenticateUser(localParams);
    Console.WriteLine(Format('Result: %s', [BoolToStr(testResult, True)]));
  finally
    if Assigned(localParams) then
      Finalize(localParams);

    Console.WriteColorLine('End - Test', [TConsoleColor.Red]);
  end;
  Exit(True);
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
