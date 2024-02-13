unit WinSSPI;

interface

uses
  System.SysUtils, JwaSspi, JwaWinError, System.Diagnostics;

function NTLM_CrendentialsCheck(const ADomain, AUsername, APassword: string; TimeoutMilliseconds: Cardinal = 10000): Boolean;
function Kerberos_CrendentialsCheck(const ADomain, AUsername, APassword: string; TimeoutMilliseconds: Cardinal = 10000): Boolean;

implementation

function InitializeAuthIdentity(const ADomain, AUsername, APassword: string): TSecWinNTAuthIdentity;
begin
  if (ADomain = '') or (AUsername = '') or (APassword = '') then
    raise Exception.Create('Domain, Username, and Password must be provided');

  Result.Domain := PChar(ADomain);
  Result.DomainLength := Length(ADomain);
  Result.User := PChar(AUsername);
  Result.UserLength := Length(AUsername);
  Result.Password := PChar(APassword);
  Result.PasswordLength := Length(APassword);
  Result.Flags := SEC_WINNT_AUTH_IDENTITY_UNICODE;
end;

function PerformAuthentication(sft: PSecurityFunctionTable; const AuthIdentity: TSecWinNTAuthIdentity; const SEC_PKG_NAME: string; TimeoutMilliseconds: Cardinal): Boolean;
var
  pkgInfo: PSecPkgInfo;
  cbMaxMessage: Integer;
  ClientDesc, ServerDesc: TSecBufferDesc;
  Client, Server: TSecBuffer;
  pClientBuffer, pServerBuffer: Pointer;
  StatusResult: TSecurityStatus;
  hcredClient, hcredServer: TCredHandle;
  hctxClient, hctxServer: TCtxthandle;
  bClientContinue, bServerContinue: Boolean;
  pClientCtxHandleIn: PCtxtHandle;
  pServerCtxHandleIn: PCtxtHandle;
  pServerCtxHandleOut: PCtxtHandle;
  CtxAttrb: Cardinal;
  expiryClientCtx: TTimeStamp;
  expiryServerCtx: TTimeStamp;
  pClientInput: PSecBufferDesc;
  TargetName: String;
  stopWatch: TStopWatch;
begin
  sft.QuerySecurityPackageInfoW(PSecWChar(PChar(SEC_PKG_NAME)), pkgInfo);
  cbMaxMessage := pkgInfo.cbMaxToken;
  GetMem(pClientBuffer, cbMaxMessage);
  GetMem(pServerBuffer, cbMaxMessage);

  try
    sft.AcquireCredentialsHandleW(nil, PSecWChar(PChar(SEC_PKG_NAME)), SECPKG_CRED_OUTBOUND, nil, @AuthIdentity, nil, nil, @hcredClient, nil); // client
    sft.AcquireCredentialsHandleW(nil, PSecWChar(PChar(SEC_PKG_NAME)), SECPKG_CRED_INBOUND, nil, nil, nil, nil, @hcredServer, nil); // server

    FillChar(Client, SizeOf(Client), 0);
    Client.BufferType := SECBUFFER_TOKEN;
    Client.cbBuffer := cbMaxMessage;
    Client.pvBuffer := pClientBuffer;
    ClientDesc.ulVersion := SECBUFFER_VERSION;
    ClientDesc.cBuffers := 1;
    ClientDesc.pBuffers := @Client;

    FillChar(Server, SizeOf(Server), 0);
    Server.BufferType := SECBUFFER_TOKEN;
    Server.cbBuffer := cbMaxMessage;
    Server.pvBuffer := pServerBuffer;
    ServerDesc.ulVersion := SECBUFFER_VERSION;
    ServerDesc.cBuffers := 1;
    ServerDesc.pBuffers := @Server;

    pClientCtxHandleIn  := nil;
    pServerCtxHandleIn  := nil;
    pServerCtxHandleOut := @hctxServer;
    pClientInput := nil;
    CtxAttrb := 0;

    bClientContinue := True;
    bServerContinue := True;
    stopWatch := TStopWatch.StartNew;

    StatusResult := 0;

    while bClientContinue or bServerContinue do
    begin
      if stopWatch.ElapsedMilliseconds > TimeoutMilliseconds then
        raise Exception.Create('Authentication timeout');

      if bClientContinue then
      begin
        StatusResult := sft.InitializeSecurityContextW(@hcredClient, pClientCtxHandleIn, PSecWChar(TargetName), ISC_REQ_CONNECTION, 0, SECURITY_NATIVE_DREP, pClientInput, 0, @hctxClient, @ClientDesc, CtxAttrb, @expiryClientCtx);
        case StatusResult of
          SEC_E_OK: bClientContinue := False;
          SEC_I_CONTINUE_NEEDED,
          SEC_I_COMPLETE_NEEDED,
          SEC_I_COMPLETE_AND_CONTINUE: begin
            pClientCtxHandleIn := @hctxClient;
            pClientInput := @ServerDesc;

            if (StatusResult = SEC_I_COMPLETE_NEEDED) or (StatusResult = SEC_I_COMPLETE_AND_CONTINUE) then
              sft.CompleteAuthToken(@hctxClient, @ClientDesc);
          end;
        end;
      end;

      if bServerContinue then
      begin
        StatusResult := sft.AcceptSecurityContext(@hcredServer, pServerCtxHandleIn, @ClientDesc, ISC_REQ_CONNECTION, SECURITY_NATIVE_DREP, @hctxServer, @ServerDesc, CtxAttrb, @expiryServerCtx);
        case StatusResult of
          SEC_E_OK: bServerContinue := False;
          SEC_I_CONTINUE_NEEDED,
          SEC_I_COMPLETE_NEEDED,
          SEC_I_COMPLETE_AND_CONTINUE:
          begin
            pServerCtxHandleIn := pServerCtxHandleOut;
            if (StatusResult = SEC_I_COMPLETE_NEEDED) or (StatusResult = SEC_I_COMPLETE_AND_CONTINUE) then
              sft.CompleteAuthToken(@hctxServer, @ServerDesc);
          end;
        end;
      end;
    end;

    Result := StatusResult = SEC_E_OK;
  finally
    FreeMem(pClientBuffer, cbMaxMessage);
    FreeMem(pServerBuffer, cbMaxMessage);
    sft.FreeCredentialsHandle(@hcredClient);
    sft.FreeCredentialsHandle(@hcredServer);
  end;
end;

function NTLM_CrendentialsCheck(const ADomain, AUsername, APassword: string; TimeoutMilliseconds: Cardinal): Boolean;
var
  AuthIdentity: TSecWinNTAuthIdentity;
  sft: PSecurityFunctionTable;
begin
  AuthIdentity := InitializeAuthIdentity(ADomain, AUsername, APassword);
  sft := InitSecurityInterface;
  Result := PerformAuthentication(sft, AuthIdentity, 'NTLM', TimeoutMilliseconds);
end;

function Kerberos_CrendentialsCheck(const ADomain, AUsername, APassword: string; TimeoutMilliseconds: Cardinal): Boolean;
var
  AuthIdentity: TSecWinNTAuthIdentity;
  sft: PSecurityFunctionTable;
begin
  AuthIdentity := InitializeAuthIdentity(ADomain, AUsername, APassword);
  sft := InitSecurityInterface;
  Result := PerformAuthentication(sft, AuthIdentity, 'Kerberos', TimeoutMilliseconds);
end;

end.
