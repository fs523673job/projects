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

const
  ENCRYPTED_EXTENSION = '.APC';

function EncryptSymetricFile(var AFilePath: String; const ACreateNewFile: Boolean = False): Boolean;
function DecryptSymetricFile(var AFilePath: String; const ACreateNewFile: Boolean = False): Boolean;

implementation

function EncryptSymetricFile(var AFilePath: String; const ACreateNewFile: Boolean = False): Boolean;
var
  factory: TElSymmetricCryptoFactory;
  crypto: TElSymmetricCrypto;
  km: TElSymmetricKeyMaterial;
  sbbyteKey: ByteArray;
  sbbyteIV: ByteArray;
  msFileIn, msFileOut: TMemoryStream;
  newExt: String;
begin
  sbbyteKey := [64, 64, 97, 112, 100, 97, 116, 97, 115, 105, 109, 102, 105, 108, 101, 107, 101, 121]; // '@@apdatasimfilekey'
  sbbyteIV := [64, 64, 97, 112, 100, 97, 116, 97, 115, 105, 109, 102, 105, 108, 101, 73, 86];         // '@@apdatasimfileIV'

  SetLength(sbbyteKey, 32);
  SetLength(sbbyteIV, 16);

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
              if (DeleteFile(AFilePath)) then
              begin
                newExt := ExtractFileExt(AFilePath) + ENCRYPTED_EXTENSION;
                AFilePath := ChangeFileExt(AFilePath, newExt);
              end;
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
  sbbyteKey: ByteArray;
  sbbyteIV: ByteArray;
  msFileIn, msFileOut: TMemoryStream;
  newExt: String;
begin
  sbbyteKey := [64, 64, 97, 112, 100, 97, 116, 97, 115, 105, 109, 102, 105, 108, 101, 107, 101, 121]; // '@@apdatasimfilekey'
  sbbyteIV := [64, 64, 97, 112, 100, 97, 116, 97, 115, 105, 109, 102, 105, 108, 101, 73, 86];         // '@@apdatasimfileIV'

  SetLength(sbbyteKey, 32);
  SetLength(sbbyteIV, 16);

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
              if (newExt = ENCRYPTED_EXTENSION) then
              begin
                if (DeleteFile(AFilePath)) then
                  newExt := StringReplace(newExt, ENCRYPTED_EXTENSION, '', [rfReplaceAll]);
              end;

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
