unit unImplementacao;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Math,

  SBSymmetricCrypto,
  SBConstants,
  SBTypes
  ;

function EncryptSymetricFile(const AFilePath: String): Boolean;
function DecryptSymetricFile(const AFilePath: String): Boolean;

implementation

function EncryptSymetricFile(const AFilePath: String): Boolean;
var
  factory: TElSymmetricCryptoFactory;
  crypto: TElSymmetricCrypto;
  km: TElSymmetricKeyMaterial;
  keyStr, ivStr: String;
  sbbyteKey: ByteArray;
  sbbyteIV: ByteArray;
  msFileIn, msFileOut: TMemoryStream;
begin
  keyStr := '@@apdatasimfilekey';
  ivStr := '@@apdatasimfileIV';

  SetLength(sbbyteKey, 32);
  SetLength(sbbyteIV, 16);

  Move(keyStr[1], sbbyteKey[0], Min(Length(keyStr), 32));
  Move(ivStr[1], sbbyteIV[0], Min(Length(ivStr), 16));

  Result := True;
  try
    factory := TElSymmetricCryptoFactory.Create;
    try
      crypto := factory.CreateInstance(SB_ALGORITHM_CNT_AES256, cmCBC);
      try
        km := TElSymmetricKeyMaterial.Create;
        try
          km.Key := sbbyteKey;
          km.IV := sbbyteIV;
          crypto.KeyMaterial := km;

          msFileIn := TMemoryStream.Create;
          msFileOut := TMemoryStream.Create;
          try
            msFileIn.LoadFromFile(AFilePath);
            crypto.Encrypt(msFileIn, msFileOut);
            msFileOut.SaveToFile(ChangeFileExt(AFilePath,'.SXT'));
          finally
            msFileIn.Free;
            msFileOut.Free;
          end;
        finally
          km.Free;
        end;
      finally
        crypto.Free;
      end;
    finally
      factory.Free;
    end;
  except
    on E: Exception do
      Result := False;
  end;
end;

function DecryptSymetricFile(const AFilePath: String): Boolean;
begin
end;

end.
