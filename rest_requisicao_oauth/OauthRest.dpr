program OauthRest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,

  REST.Json,
  REST.Json.Types,
  REST.Types,
  REST.Authenticator.OAuth,
  REST.Client,

  OAuthRestHelper in 'OAuthRestHelper.pas';

type
  TPayloadTecban = class(TObject)
  public
    matricula         : string;
    dataHoraBatimento : string;
    dataHoraEnvio     : string;
  end;


begin
  try
    var oauthConfig :=
        TOAuthConfig.Create(
                             'https://mob2.tbforte.com.br/api-apdata/auth/login',
                             'APDATA-HML',
                             'bCeNTuufVUpgfK9',
                             ''
                           );

    var Resp: string;
    var StatusCode: integer;
    var execOK := ExecuteWithOAuthRetry(
           oAuthConfig,
           rmPOST,
           'https://mob2.tbforte.com.br/api-apdata/apdata/batimento', // Base URL da API
           '',  // Resource
           procedure(R: TRESTRequest)
           begin
             R.AddParameter('Accept', 'application/json', pkHTTPHEADER, [poDoNotEncode, poTransient]);

             var payload := TPayloadTecban.Create;
             try
               payload.matricula         := '123';
               payload.dataHoraBatimento := FormatDateTime('yyyy-mm-ddThh:nn:ss.zzz', Now);
               payload.dataHoraEnvio     := FormatDateTime('yyyy-mm-ddThh:nn:ss.zzzZ', Now);
               var json := TJson.ObjectToJSonString(payload);
               R.Body.Add(json, ContentTypeFromString('application/json'));
             finally
               payload.free;
             end;
          end,
          Resp,
          StatusCode
        );

    if not execOK then
      WriteLn('Error Send File');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
