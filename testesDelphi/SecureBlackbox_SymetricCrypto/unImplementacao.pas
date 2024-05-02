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

type
  TSecuryHeaderStream = class
  public
    const
      HEADER = 'EncryptedFile';
  public
    class function Add(var AStream: TMemoryStream): Boolean;
    class function Remove(var AStream: TMemoryStream): Boolean;
    class function Check(var AStream: TMemoryStream): Boolean;
  end;

function EncryptSymetricFile(var AFilePath: String; const ACreateNewFile: Boolean = False): Boolean;
function DecryptSymetricFile(var AFilePath: String; const ACreateNewFile: Boolean = False): Boolean;

function EncryptSymetricFile_HeaderSecurity(var AFilePath: String; const ACreateNewFile: Boolean = False): Boolean;
function DecryptSymetricFile_HeaderSecurity(var AFilePath: String; const ACreateNewFile: Boolean = False): Boolean;

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

function EncryptSymetricFile_HeaderSecurity(var AFilePath: String; const ACreateNewFile: Boolean = False): Boolean;
var
  TmpStream: TMemoryStream;
begin
  TmpStream := TMemoryStream.Create;
  try
    TmpStream.LoadFromFile(AFilePath);
    if not (TSecuryHeaderStream.Check(TmpStream)) then
    begin
      TmpStream.Clear;
      if EncryptSymetricFile(AFilePath, ACreateNewFile) then
      begin
        TmpStream.LoadFromFile(AFilePath);
        TSecuryHeaderStream.Add(TmpStream);
        TmpStream.SaveToFile(AFilePath);
        Exit(True);
      end;
    end
    else
      Exit(False);
  finally
    TmpStream.Free;
  end;
end;

function DecryptSymetricFile_HeaderSecurity(var AFilePath: String; const ACreateNewFile: Boolean = False): Boolean;
var
  TmpStream: TMemoryStream;
begin
  TmpStream := TMemoryStream.Create;
  try
    TmpStream.LoadFromFile(AFilePath);
    if TSecuryHeaderStream.Check(TmpStream) then
    begin
      if TSecuryHeaderStream.Remove(TmpStream) then
        TmpStream.SaveToFile(AFilePath);

      Exit(DecryptSymetricFile(AFilePath, ACreateNewFile));
    end
    else
      Exit(False);
  finally
    TmpStream.Free;
  end;
end;

{ TSecuryHeaderStream }

class function TSecuryHeaderStream.Add(var AStream: TMemoryStream): Boolean;
var
  TmpStream: TMemoryStream;
begin
  TmpStream := TMemoryStream.Create;
  try
    try
      TmpStream.WriteBuffer(HEADER[1], Length(HEADER) * SizeOf(Char));
      TmpStream.CopyFrom(AStream, 0);
      AStream.Clear;
      AStream.CopyFrom(TmpStream, 0);
      AStream.Position := 0;
      Exit(True);
    except
      Exit(False);
    end;
  finally
    TmpStream.Free;
  end;
end;

class function TSecuryHeaderStream.Check(var AStream: TMemoryStream): Boolean;
var
  FileHeader: String;
begin
  SetLength(FileHeader, Length(HEADER));
  AStream.Position := 0;
  AStream.ReadBuffer(FileHeader[1], Length(HEADER) * SizeOf(Char));
  AStream.Position := 0;
  Exit(FileHeader = Header);
end;

class function TSecuryHeaderStream.Remove(var AStream: TMemoryStream): Boolean;
var
  TmpStream: TMemoryStream;
begin
  if not Check(AStream) then
    Exit(False);

  TmpStream := TMemoryStream.Create;
  try
    try
      AStream.Position := Length(HEADER) * SizeOf(Char);
      TmpStream.CopyFrom(AStream, AStream.Size - AStream.Position);
      AStream.Clear;
      AStream.CopyFrom(TmpStream, 0);
      Exit(True);
    except
      Exit(False);
    end;
  finally
    TmpStream.Free;
  end;
end;

end.
