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

function EncryptSymetricFile(var AFilePath: String; const ACreateNewFile: Boolean = False): Boolean;
function DecryptSymetricFile(var AFilePath: String; const ACreateNewFile: Boolean = False): Boolean;

implementation

function EncryptSymetricFile(var AFilePath: String; const ACreateNewFile: Boolean = False): Boolean;
var
  factory: TElSymmetricCryptoFactory;
  crypto: TElSymmetricCrypto;
  km: TElSymmetricKeyMaterial;
  keyStr, ivStr: String;
  sbbyteKey: ByteArray;
  sbbyteIV: ByteArray;
  msFileIn, msFileOut: TMemoryStream;
  newExt: String;
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

            if ACreateNewFile then
            begin
              newExt := ExtractFileExt(AFilePath);
              if not newExt.IsEmpty and (newExt.Length = 4) then
                newExt[2] := 'E';
              AFilePath := ChangeFileExt(AFilePath,newExt);
            end;

            msFileOut.SaveToFile(AFilePath);
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

function DecryptSymetricFile(var AFilePath: String; const ACreateNewFile: Boolean = False): Boolean;
var
  factory: TElSymmetricCryptoFactory;
  crypto: TElSymmetricCrypto;
  km: TElSymmetricKeyMaterial;
  keyStr, ivStr: String;
  sbbyteKey: ByteArray;
  sbbyteIV: ByteArray;
  msFileIn, msFileOut: TMemoryStream;
  newExt: String;
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

            crypto.Decrypt(msFileIn, msFileOut);

            if ACreateNewFile then
            begin
              newExt := ExtractFileExt(AFilePath);
              if not newExt.IsEmpty and (newExt.Length = 4) then
                newExt[2] := 'D';
              AFilePath := ChangeFileExt(AFilePath, newExt);
            end;

            msFileOut.SaveToFile(AFilePath);
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

end.
