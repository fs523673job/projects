unit WinSSPI2;

interface

uses
  System.SysUtils,
  System.StrUtils,
  JwaSspi,
  JwaWinError,
  System.Diagnostics
  ;

function NTLM_CrendentialsCheck(const ADomain, AUsername, APassword: string): Boolean;
function Kerberos_CrendentialsCheck(const ADomain, AUsername, APassword: string): Boolean;

implementation

function NTLM_CrendentialsCheck(const ADomain, AUsername, APassword: string): Boolean;
var
  pkgInfo: PSecPkgInfo;
  cbMaxMessage: Integer;
  AuthIdentity : TSecWinNTAuthIdentity;
  //
  ClientDesc: TSecBufferDesc;
  ServerDesc: TSecBufferDesc;
  Client: TSecBuffer;
  Server: TSecBuffer;

  pClientInput: PSecBufferDesc;
  //
  pServerBuffer: Pointer;
  pClientBuffer: Pointer;
  StatusResult: TSecurityStatus;
  //
  pClientCtxHandleIn: PCtxtHandle;
  pServerCtxHandleIn: PCtxtHandle;
  pServerCtxHandleOut: PCtxtHandle;
  CtxAttrb: Cardinal;
  expiryClientCtx: TTimeStamp;
  expiryServerCtx: TTimeStamp;
  //
  hcredClient: TCredHandle;
  ExpiryClient: TTimeStamp;
  hcredServer: TCredHandle;
  ExpiryServer: TTimeStamp;
  hctxClient: TCtxthandle;
  hctxServer: TCtxthandle;
  bClientContinue: Boolean;
  bServerContinue: Boolean;
  sft: PSecurityFunctionTable;

  TargetName: String;
  stopWatch: TStopWatch;
const
  SEC_PKG_NAME = 'NTLM';
begin
  try
    sft := InitSecurityInterface;
    sft.QuerySecurityPackageInfoW(PSecWChar(PChar(SEC_PKG_NAME)), pkgInfo);

    cbMaxMessage := pkgInfo.cbMaxToken;
    GetMem(pClientBuffer, cbMaxMessage);
    GetMem(pServerBuffer, cbMaxMessage);
    try
      AuthIdentity.Domain := PChar(ADomain);
      AuthIdentity.DomainLength := ADomain.Length;
      AuthIdentity.User := PChar(AUsername);
      AuthIdentity.UserLength := AUsername.Length;
      AuthIdentity.Password := PChar(APassword);
      AuthIdentity.PasswordLength := APassword.Length;
      AuthIdentity.Flags := SEC_WINNT_AUTH_IDENTITY_UNICODE;

      sft.AcquireCredentialsHandleW(nil, PSecWChar(PChar(SEC_PKG_NAME)), SECPKG_CRED_OUTBOUND, nil, @AuthIdentity, nil, nil, @hcredClient, @ExpiryClient); // client
      sft.AcquireCredentialsHandleW(nil, PSecWChar(PChar(SEC_PKG_NAME)), SECPKG_CRED_INBOUND, nil, nil, nil, nil, @hcredServer, @ExpiryServer); // server
      try
        // buffers client and server
        FillChar(Client, SizeOf(Client), 0);
        Client.BufferType := SECBUFFER_TOKEN;
        Client.cbBuffer := cbMaxMessage;
        Client.pvBuffer := pClientBuffer;
        ClientDesc.ulVersion := SECBUFFER_VERSION;
        ClientDesc.cBuffers := 1;
        ClientDesc.pBuffers := @Client;
        //
        FillChar(Server, SizeOf(Server), 0);
        Server.BufferType := SECBUFFER_TOKEN;
        Server.cbBuffer := cbMaxMessage;
        Server.pvBuffer := pServerBuffer;
        ServerDesc.ulVersion := SECBUFFER_VERSION;
        ServerDesc.cBuffers := 1;
        ServerDesc.pBuffers := @Server;
        // initialize variables
        pClientCtxHandleIn  := nil;
        pServerCtxHandleIn  := nil;
        pServerCtxHandleOut := @hctxServer;
        //
        pClientInput := nil;
        //
        CtxAttrb := 0;
        // loop
        bClientContinue := True;
        bServerContinue := True;

        stopWatch := TStopWatch.StartNew;

        while bClientContinue or bServerContinue do
        begin
          if bClientContinue then
          begin
            Client.cbBuffer := cbMaxMessage;
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
            Server.cbBuffer := cbMaxMessage;
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

          if (stopWatch.ElapsedMilliseconds >= 10000) then
            raise Exception.Create('Fail authenticate user');

          Sleep(1000);
        end;

        // libera recursos...
        if pClientCtxHandleIn <> nil then
          sft.DeleteSecurityContext(pClientCtxHandleIn);

        if pServerCtxHandleIn <> nil then
          sft.DeleteSecurityContext(pServerCtxHandleIn);

        Result := True;
      finally
        sft.FreeCredentialsHandle(@hcredClient);
        sft.FreeCredentialsHandle(@hcredServer);
      end;
    finally
      FreeMem(pServerBuffer, cbMaxMessage);
      FreeMem(pClientBuffer, cbMaxMessage);
    end;
  except
    on e: Exception do
    begin
      Result := False;
      raise;
    end;
  end;
end;

function Kerberos_CrendentialsCheck(const ADomain, AUsername, APassword: string): Boolean;
var
  pkgInfo: PSecPkgInfo;
  cbMaxMessage: Integer;
  AuthIdentity : TSecWinNTAuthIdentity;
  //
  ClientDesc: TSecBufferDesc;
  ServerDesc: TSecBufferDesc;
  Client: TSecBuffer;
  Server: TSecBuffer;

  pClientInput: PSecBufferDesc;
  //
  pServerBuffer: Pointer;
  pClientBuffer: Pointer;
  StatusResult: TSecurityStatus;
  //
  pClientCtxHandleIn: PCtxtHandle;
  pServerCtxHandleIn: PCtxtHandle;
  pServerCtxHandleOut: PCtxtHandle;
  CtxAttrb: Cardinal;
  expiryClientCtx: TTimeStamp;
  expiryServerCtx: TTimeStamp;
  //
  hcredClient: TCredHandle;
  ExpiryClient: TTimeStamp;
  hcredServer: TCredHandle;
  ExpiryServer: TTimeStamp;
  hctxClient: TCtxthandle;
  hctxServer: TCtxthandle;
  bClientContinue: Boolean;
  bServerContinue: Boolean;
  sft: PSecurityFunctionTable;

  TargetName: String;
  stopWatch: TStopWatch;
const
  SEC_PKG_NAME = 'Kerberos';
begin
  try
    sft := InitSecurityInterface;
    sft.QuerySecurityPackageInfoW(PSecWChar(PChar(SEC_PKG_NAME)), pkgInfo);

    cbMaxMessage := pkgInfo.cbMaxToken;
    GetMem(pClientBuffer, cbMaxMessage);
    GetMem(pServerBuffer, cbMaxMessage);
    try
      AuthIdentity.Domain := PChar(ADomain);
      AuthIdentity.DomainLength := ADomain.Length;
      AuthIdentity.User := PChar(AUsername);
      AuthIdentity.UserLength := AUsername.Length;
      AuthIdentity.Password := PChar(APassword);
      AuthIdentity.PasswordLength := APassword.Length;
      AuthIdentity.Flags := SEC_WINNT_AUTH_IDENTITY_UNICODE;

      sft.AcquireCredentialsHandleW(nil, PSecWChar(PChar(SEC_PKG_NAME)), SECPKG_CRED_OUTBOUND, nil, @AuthIdentity, nil, nil, @hcredClient, @ExpiryClient); // client
      sft.AcquireCredentialsHandleW(nil, PSecWChar(PChar(SEC_PKG_NAME)), SECPKG_CRED_INBOUND, nil, nil, nil, nil, @hcredServer, @ExpiryServer); // server
      try
        // buffers client and server
        FillChar(Client, SizeOf(Client), 0);
        Client.BufferType := SECBUFFER_TOKEN;
        Client.cbBuffer := cbMaxMessage;
        Client.pvBuffer := pClientBuffer;
        ClientDesc.ulVersion := SECBUFFER_VERSION;
        ClientDesc.cBuffers := 1;
        ClientDesc.pBuffers := @Client;
        //
        FillChar(Server, SizeOf(Server), 0);
        Server.BufferType := SECBUFFER_TOKEN;
        Server.cbBuffer := cbMaxMessage;
        Server.pvBuffer := pServerBuffer;
        ServerDesc.ulVersion := SECBUFFER_VERSION;
        ServerDesc.cBuffers := 1;
        ServerDesc.pBuffers := @Server;
        // initialize variables
        pClientCtxHandleIn  := nil;
        pServerCtxHandleIn  := nil;
        pServerCtxHandleOut := @hctxServer;
        //
        pClientInput := nil;
        //
        CtxAttrb := 0;
        // loop
        bClientContinue := True;
        bServerContinue := True;

        stopWatch := TStopWatch.StartNew;

        while bClientContinue or bServerContinue do
        begin
          if bClientContinue then
          begin
            Client.cbBuffer := cbMaxMessage;
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
            Server.cbBuffer := cbMaxMessage;
            StatusResult := sft.AcceptSecurityContext(@hcredServer, pServerCtxHandleIn, @ClientDesc, ISC_REQ_CONNECTION, SECURITY_NATIVE_DREP,
                                                      @hctxServer, @ServerDesc, CtxAttrb, @expiryServerCtx);

            case StatusResult of
              SEC_E_OK:
                bServerContinue := False;

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

          if (stopWatch.ElapsedMilliseconds >= 10000) then
            raise Exception.Create('Fail authenticate user');

          Sleep(1000);
        end;

        // libera recursos...
        if pClientCtxHandleIn <> nil then
          sft.DeleteSecurityContext(pClientCtxHandleIn);

        if pServerCtxHandleIn <> nil then
          sft.DeleteSecurityContext(pServerCtxHandleIn);

        Result := True;
      finally
        sft.FreeCredentialsHandle(@hcredClient);
        sft.FreeCredentialsHandle(@hcredServer);
      end;
    finally
      FreeMem(pServerBuffer, cbMaxMessage);
      FreeMem(pClientBuffer, cbMaxMessage);
    end;
  except
    on e: Exception do
    begin
      Result := False;
      raise;
    end;
  end;
end;

end.
