unit OAuthRestHelper;

interface

uses
  System.SysUtils,
  System.Classes,
  System.DateUtils,
  System.SyncObjs,
  System.NetEncoding,
  System.JSON,
  System.Net.HttpClient,
  REST.Types,
  REST.Client;

type
  TOAuthConfig = record
    TokenUrl    : string;
    ClientId    : string;
    ClientSecret: string;
    Scope       : string;
    TimeoutMS   : Integer;
    class function Create(const ATokenUrl, AClientId, AClientSecret: String; const AScope: string = ''; const ATimeoutMS: Integer = 15000): TOAuthConfig; static;
  end;

  TOAuthTokenManager = class
  strict private
    class var FAccessToken  : string;
    class var FExpiresAtUTC : TDateTime;
    class var FLock         : TObject;
    class constructor Create;
    class destructor Destroy;
    class function  IsTokenValid: Boolean; static;
    class function  RequestNewToken(const AConfig: TOAuthConfig): string; static;
    class procedure SetOrReplaceHeader(ARequest: TRESTRequest; const AName, AValue: string); static;
  public
    class function  GetAccessToken(const AConfig: TOAuthConfig): string; static;
    class procedure ForceRefresh; static;
    class procedure AttachBearer(ARequest: TRESTRequest; const AConfig: TOAuthConfig); static;
  end;
  function ExecuteWithOAuthRetry(const AConfig: TOAuthConfig;
                                 const AMethod: TRESTRequestMethod;
                                 const ABaseUrl, AResource: string;
                                 const AQueryOrBodySetup: TProc<TRESTRequest>;
                                 out AResponse: string;
                                 out AStatusCode: Integer): Boolean;
implementation

{ TOAuthConfig }

class function TOAuthConfig.Create;
begin
  Result.TokenUrl     := ATokenUrl;
  Result.ClientId     := AClientId;
  Result.ClientSecret := AClientSecret;
  Result.Scope        := AScope;
  Result.TimeoutMS    := ATimeoutMS;
end;

{ TOAuthTokenManager }

class constructor TOAuthTokenManager.Create;
begin
  FLock := TObject.Create;
  FAccessToken   := '';
  FExpiresAtUTC  := 0;
end;

class destructor TOAuthTokenManager.Destroy;
begin
  FLock.Free;
end;

class function TOAuthTokenManager.IsTokenValid: Boolean;
begin
  Result := (FAccessToken <> '') and (Now < IncSecond(FExpiresAtUTC, -30));
end;

class function TOAuthTokenManager.RequestNewToken(const AConfig: TOAuthConfig): string;
var
  LClient   : TRESTClient;
  LRequest  : TRESTRequest;
  LResponse : TRESTResponse;
  LBasic    : string;
  LJSON     : TJSONObject;
  LVal      : TJSONValue;
  LExpires  : Integer;
begin
  LClient := TRESTClient.Create(AConfig.TokenUrl);
  try
    LClient.Accept          := 'application/json';
    LClient.ContentType     := 'application/x-www-form-urlencoded';
    LClient.SecureProtocols := [THTTPSecureProtocol.TLS12, THTTPSecureProtocol.TLS13];
    if AConfig.TimeoutMS > 0 then
    begin
      LClient.ConnectTimeout := AConfig.TimeoutMS;
      LClient.ReadTimeout    := AConfig.TimeoutMS;
    end;
    LRequest  := TRESTRequest.Create(nil);
    LResponse := TRESTResponse.Create(nil);
    try
      LRequest.Client   := LClient;
      LRequest.Response := LResponse;
      LRequest.Method   := rmPOST;
      // Authorization: Basic base64(client_id:client_secret)
      LBasic := TNetEncoding.Base64.Encode(AConfig.ClientId + ':' + AConfig.ClientSecret);
      LRequest.AddParameter('Authorization', 'Basic ' + LBasic, pkHTTPHEADER, [poDoNotEncode, poTransient]);
      LRequest.AddParameter('Content-Type', 'application/x-www-form-urlencoded', pkHTTPHEADER, [poDoNotEncode, poTransient]);
      LRequest.AddParameter('grant_type', 'client_credentials', pkGETorPOST);
      if AConfig.Scope <> '' then
        LRequest.AddParameter('scope', AConfig.Scope, pkGETorPOST);
      LRequest.Execute;
      if (LResponse.StatusCode < 200) or (LResponse.StatusCode >= 300) then
        raise Exception.CreateFmt('Falha ao obter token. HTTP %d - %s', [LResponse.StatusCode, LResponse.StatusText]);
      LJSON := TJSONObject(TJSONObject.ParseJSONValue(LResponse.Content));
      if not Assigned(LJSON) then
        raise Exception.Create('Resposta de token inválida (JSON nulo).');
      try
        if not LJSON.TryGetValue('access_token', LVal) then
        begin
          if not LJOSN.TryGetValue('token', LVal) then
            raise Exception.Create('Resposta de token inválida: sem access_token');
        end;
        Result := LVal.Value;
        LExpires := 3600;
        if LJSON.TryGetValue('expires_in', LVal) then
          LExpires := StrToIntDef(LVal.Value, 3600);
        TMonitor.Enter(FLock);
        try
          FAccessToken  := Result;
          FExpiresAtUTC := Now + (LExpires / SecsPerDay);
        finally
          TMonitor.Exit(FLock);
        end;
      finally
        LJSON.Free;
      end;
    finally
      LResponse.Free;
      LRequest.Free;
    end;
  finally
    LClient.Free;
  end;
end;

class function TOAuthTokenManager.GetAccessToken(const AConfig: TOAuthConfig): string;
begin
  TMonitor.Enter(FLock);
  try
    if IsTokenValid then
      Exit(FAccessToken);
  finally
    TMonitor.Exit(FLock);
  end;
  Result := RequestNewToken(AConfig);
end;

class procedure TOAuthTokenManager.ForceRefresh;
begin
  TMonitor.Enter(FLock);
  try
    FAccessToken  := '';
    FExpiresAtUTC := 0;
  finally
    TMonitor.Exit(FLock);
  end;
end;

class procedure TOAuthTokenManager.SetOrReplaceHeader(ARequest: TRESTRequest; const AName, AValue: string);
var
  I: Integer;
begin
  for I := ARequest.Params.Count - 1 downto 0 do
    if (ARequest.Params[I].Kind = pkHTTPHEADER) and SameText(ARequest.Params[I].Name, AName) then
      ARequest.Params.Delete(I);
  ARequest.AddParameter(AName, AValue, pkHTTPHEADER, [poDoNotEncode, poTransient]);
end;

class procedure TOAuthTokenManager.AttachBearer(ARequest: TRESTRequest; const AConfig: TOAuthConfig);
var
  LToken: string;
begin
  LToken := GetAccessToken(AConfig);
  SetOrReplaceHeader(ARequest, 'Authorization', 'Bearer ' + LToken);
end;

function ExecuteWithOAuthRetry(const AConfig: TOAuthConfig;
                               const AMethod: TRESTRequestMethod;
                               const ABaseUrl, AResource: string;
                               const AQueryOrBodySetup: TProc<TRESTRequest>;
                               out AResponse: string;
                               out AStatusCode: Integer): Boolean;
var
  LClient   : TRESTClient;
  LRequest  : TRESTRequest;
  LResponse : TRESTResponse;
  LRetried  : Boolean;
begin
  Result := False;
  AResponse := '';
  AStatusCode := 0;
  LClient := TRESTClient.Create(ABaseUrl);
  try
    LClient.Accept          := 'application/json';
    LClient.SecureProtocols := [THTTPSecureProtocol.TLS12, THTTPSecureProtocol.TLS13];
    if AConfig.TimeoutMS > 0 then
    begin
      LClient.ConnectTimeout := AConfig.TimeoutMS;
      LClient.ReadTimeout    := AConfig.TimeoutMS;
    end;
    LRequest  := TRESTRequest.Create(nil);
    LResponse := TRESTResponse.Create(nil);
    try
      LRequest.Client   := LClient;
      LRequest.Response := LResponse;
      LRequest.Method   := AMethod;
      LRequest.Resource := AResource;
      TOAuthTokenManager.AttachBearer(LRequest, AConfig);
      if Assigned(AQueryOrBodySetup) then
        AQueryOrBodySetup(LRequest);
      LRetried := False;
      LRequest.Execute;
      AStatusCode := LResponse.StatusCode;
      AResponse   := LResponse.Content;
      if (AStatusCode = 401) and (not LRetried) then
      begin
        LRetried := True;
        TOAuthTokenManager.ForceRefresh;
        TOAuthTokenManager.AttachBearer(LRequest, AConfig);
        LRequest.Execute;
        AStatusCode := LResponse.StatusCode;
        AResponse   := LResponse.Content;
      end;
      Result := (AStatusCode >= 200) and (AStatusCode < 300);
    finally
      LResponse.Free;
      LRequest.Free;
    end;
  finally
    LClient.Free;
  end;
end;

end.

