unit unSymetricCripto;

interface

uses
  System.Classes,
  System.SysUtils,
  System.IniFiles,
  System.StrUtils,
  System.Generics.Collections,
  System.IOUtils,
  System.Generics.Defaults,
  SBSymmetricCrypto,
  SBConstants,
  SBTypes,
  SBRandom,
  SBHashFunction,
  SBHMAC,
  SBUtils;

type
  TSymetricStatus = (tssEmpty, tssEncrypt, tssDecrypt);

  TSymetricCript = class
  private
    const ENCRYPTED_EXTENSION = '.APC';
    const INI_REPORT_SETTINGS = 'Report Settings';
  private
    ListFiles: TDictionary<String, Boolean>; // Path, (Encrypt/Decrypt = True/False)
    FPassPhrase: String;
    FUseMetaFile: Boolean;

    procedure UpdateFileIni(const AFilePath: String; const AEncrypt: Boolean);
    function GetSymetricStatusIni(const AFilePath: String): TSymetricStatus;
    function IsFileIni(const AFilePath: String): Boolean;
    function IsValidFileForEncryptOrDecrypt(const AFilePath: String; AExtensions: array of String): Boolean;
    procedure EncryptFileMark(const AFilePath: String; const ARenameFile: Boolean);
    procedure DecryptFileMark(const AFilePath: String; const ARenameFile: Boolean);

    // DEK's KEK's
    function GenerateDEK: ByteArray;
    function EncryptDEK(const ADEK: ByteArray; const AKEK: ByteArray; out AEncryptedDEKIV: ByteArray): ByteArray;
    function DecryptDEK(const AEncryptedDEK, AKEK, AEncryptedDEKIV: ByteArray): ByteArray;
    procedure RetrieveEncryptionMetadata(const AFilePath: String; out EncryptedDEK, EncryptedDEKIV, FileIV, Salt: ByteArray);
    procedure StoreEncryptionMetadata(const AFilePath: String; const EncryptedDEK, EncryptedDEKIV, FileIV, Salt: ByteArray);
    procedure GenerateSecureRandom(var Buffer: ByteArray; const Size: Integer);
    function DeriveKEKFromPassword(const APassword: String; const Salt: ByteArray): ByteArray;
    function PBKDF2_HMAC_SHA256(const Password, Salt: ByteArray; Iterations, KeyLength: Integer): ByteArray;
    function XorBytes(const A, B: ByteArray): ByteArray;
    function GetMetadataSizeFromFile(const AFilePath: String): Int64;

    // Info In MetaData or InFile
    procedure RetrieveEncryptionMetadataFromFile(const AFilePath: String; out EncryptedDEK, EncryptedDEKIV, FileIV, Salt: ByteArray);
    procedure RetrieveEncryptionMetadataFromStream(const AFilePath: String; out EncryptedDEK, EncryptedDEKIV, FileIV, Salt: ByteArray);
    procedure StoreEncryptionMetadataInStream(Stream: TMemoryStream; const EncryptedDEK, EncryptedDEKIV, FileIV, Salt: ByteArray);
    procedure StoreEncryptionMetadataToFile(const AFilePath: String; const EncryptedDEK, EncryptedDEKIV, FileIV, Salt: ByteArray);
  public
    constructor Create(const APassPhrase: String; AUseMetaFile: Boolean = True);
    destructor Destroy; override;

    procedure DecryptAndAddList(const AFilePath: String);
    procedure EncryptAndRemoveList(const AFilePath: String);

    procedure EncryptAllListFilesNecessary;

    function EncryptFile(const AFilePath: String): Boolean; overload;
    function DecryptFile(const AFilePath: String): Boolean; overload;
    function EncryptFile(var AFilePath: String; const ARenameFile: Boolean): Boolean; overload;
    function DecryptFile(var AFilePath: String; const ARenameFile: Boolean): Boolean; overload;
  end;

implementation

{ TSymetricCript }

procedure WriteDWordToStream(Stream: TStream; Value: Cardinal);
begin
  Stream.WriteBuffer(Value, SizeOf(Cardinal));
end;

function ReadDWordFromStream(Stream: TStream): Cardinal;
begin
  Stream.ReadBuffer(Result, SizeOf(Cardinal));
end;

procedure WriteInt64ToStream(Stream: TStream; Value: Int64);
begin
  Stream.WriteBuffer(Value, SizeOf(Int64));
end;

function ReadInt64FromStream(Stream: TStream): Int64;
begin
  Stream.ReadBuffer(Result, SizeOf(Int64));
end;

function HMAC_SHA256(const Key, Data: ByteArray): ByteArray;
var
  Hash: TElHashFunction;
  BlockSize: Integer;
  KeyBlock: ByteArray;
  i: Integer;
  OKeyPad, IKeyPad: ByteArray;
  HashInner: ByteArray;
begin
  BlockSize := 64;

  if Length(Key) > BlockSize then
  begin
    Hash := TElHashFunction.Create(SB_ALGORITHM_DGST_SHA256);
    try
      Hash.Update(@Key[0], Length(Key));
      KeyBlock := Hash.Finish;
    finally
      Hash.Free;
    end;
  end
  else
  begin
    KeyBlock := Key;
  end;

  SetLength(KeyBlock, BlockSize);
  SetLength(OKeyPad, BlockSize);
  SetLength(IKeyPad, BlockSize);

  for i := 0 to BlockSize - 1 do
  begin
    OKeyPad[i] := KeyBlock[i] xor $5C;
    IKeyPad[i] := KeyBlock[i] xor $36;
  end;

  Hash := TElHashFunction.Create(SB_ALGORITHM_DGST_SHA256);
  try
    Hash.Update(@IKeyPad[0], BlockSize);
    Hash.Update(@Data[0], Length(Data));
    HashInner := Hash.Finish;
  finally
    Hash.Free;
  end;

  Hash := TElHashFunction.Create(SB_ALGORITHM_DGST_SHA256);
  try
    Hash.Update(@OKeyPad[0], BlockSize);
    Hash.Update(@HashInner[0], Length(HashInner));
    Result := Hash.Finish;
  finally
    Hash.Free;
  end;
end;

constructor TSymetricCript.Create(const APassPhrase: String; AUseMetaFile: Boolean = True);
begin
  ListFiles := TDictionary<String, Boolean>.Create;
  FPassPhrase := APassPhrase;
  FUseMetaFile := AUseMetaFile;
end;

destructor TSymetricCript.Destroy;
begin
  try
    EncryptAllListFilesNecessary;
  except
  end;

  ListFiles.Free;
  inherited;
end;

procedure TSymetricCript.DecryptAndAddList(const AFilePath: String);
var
  NewFilePath: String;
begin
  NewFilePath := AFilePath;

  if not IsValidFileForEncryptOrDecrypt(AFilePath, ['.TMP', '.ZIP']) then
    Exit;

  if (not NewFilePath.IsEmpty) and FileExists(NewFilePath) then
  begin
    if IsFileIni(NewFilePath) then
    begin
      if (GetSymetricStatusIni(NewFilePath) = tssEncrypt) then
      begin
        if DecryptFile(NewFilePath) then
          ListFiles.AddOrSetValue(NewFilePath, False);
      end;
    end
    else
    begin
      if DecryptFile(NewFilePath, False) then
        ListFiles.AddOrSetValue(NewFilePath, False);
    end;
  end;
end;

function TSymetricCript.DecryptDEK(const AEncryptedDEK, AKEK, AEncryptedDEKIV: ByteArray): ByteArray;
var
  factory: TElSymmetricCryptoFactory;
  crypto: TElSymmetricCrypto;
  km: TElSymmetricKeyMaterial;
  msDEKIn, msDEKOut: TMemoryStream;
begin
  factory := TElSymmetricCryptoFactory.Create;
  try
    crypto := factory.CreateInstance(SB_ALGORITHM_CNT_AES256, cmCBC);
    try
      km := TElSymmetricKeyMaterial.Create;
      try
        km.Key := AKEK;
        km.IV := AEncryptedDEKIV;
        crypto.KeyMaterial := km;

        msDEKIn := TMemoryStream.Create;
        msDEKOut := TMemoryStream.Create;
        try
          msDEKIn.WriteBuffer(AEncryptedDEK[0], Length(AEncryptedDEK));
          msDEKIn.Position := 0;
          crypto.Decrypt(msDEKIn, msDEKOut);
          SetLength(Result, msDEKOut.Size);
          msDEKOut.Position := 0;
          msDEKOut.ReadBuffer(Result[0], msDEKOut.Size);
        finally
          msDEKIn.Free;
          msDEKOut.Free;
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
end;

function TSymetricCript.DecryptFile(const AFilePath: String): Boolean;
var
  LocalFilePath: String;
begin
  if (not AFilePath.IsEmpty) and FileExists(AFilePath) then
  begin
    LocalFilePath := AFilePath;
    DecryptFileMark(LocalFilePath, False);
    Result := DecryptFile(LocalFilePath, False);
    if Result then
      UpdateFileIni(AFilePath, False);
  end
  else
    Exit(False);
end;

function TSymetricCript.DecryptFile(var AFilePath: String; const ARenameFile: Boolean): Boolean;
var
  factory: TElSymmetricCryptoFactory;
  crypto: TElSymmetricCrypto;
  km: TElSymmetricKeyMaterial;
  msFileIn, msFileOut: TMemoryStream;
  newExt: String;
  newFilePath: String;
  EncryptedDEK, EncryptedDEKIV, DEK, FileIV, Salt, KEK: ByteArray;
  MetaDataSize: Int64;
begin
  try
    RetrieveEncryptionMetadata(AFilePath, EncryptedDEK, EncryptedDEKIV, FileIV, Salt);

    KEK := DeriveKEKFromPassword(FPassPhrase, Salt);

    DEK := DecryptDEK(EncryptedDEK, KEK, EncryptedDEKIV);

    factory := TElSymmetricCryptoFactory.Create;
    try
      crypto := factory.CreateInstance(SB_ALGORITHM_CNT_AES256, cmCBC);
      try
        km := TElSymmetricKeyMaterial.Create;
        try
          km.Key := DEK;
          km.IV := FileIV;
          crypto.KeyMaterial := km;

          msFileIn := TMemoryStream.Create;
          msFileOut := TMemoryStream.Create;
          try
            msFileIn.LoadFromFile(AFilePath);

            if not FUseMetaFile then
            begin
              MetaDataSize := GetMetadataSizeFromFile(AFilePath);
              msFileIn.Size := msFileIn.Size - MetaDataSize;
            end;

            crypto.Decrypt(msFileIn, msFileOut);
            msFileOut.SaveToFile(AFilePath);
            if ARenameFile then
            begin
              newExt := ExtractFileExt(AFilePath);
              if (newExt = ENCRYPTED_EXTENSION) then
              begin
                newExt := StringReplace(newExt, ENCRYPTED_EXTENSION, '', [rfReplaceAll]);
                newFilePath := ChangeFileExt(AFilePath, newExt);
                if RenameFile(AFilePath, newFilePath) then
                  AFilePath := newFilePath;
              end;
            end;
            Result := True;
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
    Result := False;
  end;
end;

procedure TSymetricCript.DecryptFileMark(const AFilePath: String; const ARenameFile: Boolean);
var
  IniFile: TIniFile;
  IniPath: String;
  FileName: String;
  c: Integer;
begin
  IniPath := ChangeFileExt(AFilePath, '.ini');

  if FileExists(IniPath) then
  begin
    IniFile := TIniFile.Create(IniPath);
    try
      c := 1;

      while IniFile.ValueExists(INI_REPORT_SETTINGS, Format('FileMark_%d', [c])) do
      begin
        FileName := ExtractFilePath(AFilePath) + IniFile.ReadString(INI_REPORT_SETTINGS, Format('FileMark_%d', [c]), '');
        if FileExists(FileName) then
          DecryptFile(FileName, False);
        Inc(c);
      end;
    finally
      IniFile.Free;
    end;
  end;
end;

function TSymetricCript.EncryptFile(var AFilePath: String; const ARenameFile: Boolean): Boolean;
var
  factory: TElSymmetricCryptoFactory;
  crypto: TElSymmetricCrypto;
  km: TElSymmetricKeyMaterial;
  msFileIn, msFileOut: TMemoryStream;
  newExt: String;
  newFilePath: String;
  DEK, EncryptedDEK, EncryptedDEKIV, FileIV, Salt, KEK: ByteArray;
begin
  try
    DEK := GenerateDEK;

    SetLength(Salt, 16);
    GenerateSecureRandom(Salt, Length(Salt));

    KEK := DeriveKEKFromPassword(FPassPhrase, Salt);

    EncryptedDEK := EncryptDEK(DEK, KEK, EncryptedDEKIV);

    SetLength(FileIV, 16);
    GenerateSecureRandom(FileIV, Length(FileIV));

    factory := TElSymmetricCryptoFactory.Create;
    try
      crypto := factory.CreateInstance(SB_ALGORITHM_CNT_AES256, cmCBC);
      try
        km := TElSymmetricKeyMaterial.Create;
        try
          km.Key := DEK;
          km.IV := FileIV;
          crypto.KeyMaterial := km;

          msFileIn := TMemoryStream.Create;
          msFileOut := TMemoryStream.Create;
          try
            msFileIn.LoadFromFile(AFilePath);
            crypto.Encrypt(msFileIn, msFileOut);

            if FUseMetaFile then
            begin
              msFileOut.SaveToFile(AFilePath);
              StoreEncryptionMetadata(AFilePath, EncryptedDEK, EncryptedDEKIV, FileIV, Salt);
            end
            else
            begin
              StoreEncryptionMetadataInStream(msFileOut, EncryptedDEK, EncryptedDEKIV, FileIV, Salt);
              msFileOut.SaveToFile(AFilePath);
            end;

            if ARenameFile then
            begin
              newExt := ExtractFileExt(AFilePath) + ENCRYPTED_EXTENSION;
              newFilePath := ChangeFileExt(AFilePath, newExt);
              if RenameFile(AFilePath, newFilePath) then
                AFilePath := newFilePath;
            end;
            Result := True;
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
    Result := False;
  end;
end;

procedure TSymetricCript.EncryptFileMark(const AFilePath: String; const ARenameFile: Boolean);
var
  IniFile: TIniFile;
  IniPath: String;
  FileName: String;
  c: Integer;
begin
  IniPath := ChangeFileExt(AFilePath, '.ini');

  if FileExists(IniPath) then
  begin
    IniFile := TIniFile.Create(IniPath);
    try
      c := 1;

      while IniFile.ValueExists(INI_REPORT_SETTINGS, Format('FileMark_%d', [c])) do
      begin
        FileName := ExtractFilePath(AFilePath) + IniFile.ReadString(INI_REPORT_SETTINGS, Format('FileMark_%d', [c]), '');
        if FileExists(FileName) then
          EncryptFile(FileName, False);
        Inc(c);
      end;
    finally
      IniFile.Free;
    end;
  end;
end;

function TSymetricCript.EncryptFile(const AFilePath: String): Boolean;
var
  LocalFilePath: String;
begin
  if (not AFilePath.IsEmpty) then
  begin
    LocalFilePath := AFilePath;
    EncryptFileMark(LocalFilePath, False);
    Result := EncryptFile(LocalFilePath, False);
    if Result then
      UpdateFileIni(AFilePath, True);
  end
  else
    Exit(False);
end;

procedure TSymetricCript.EncryptAllListFilesNecessary;
var
  Pair: TPair<String, Boolean>;
  NewPath: String;
begin
  for Pair in ListFiles do
  begin
    if not Pair.Value then
    begin
      if IsFileIni(Pair.Key) then
      begin
        if (GetSymetricStatusIni(Pair.Key) = tssDecrypt) and FileExists(Pair.Key) then
          EncryptFile(Pair.Key);
      end
      else
      begin
        NewPath := Pair.Key;
        if FileExists(NewPath) then
          EncryptFile(NewPath, False);
      end;

      ListFiles[Pair.Key] := True;
    end;
  end;
end;

procedure TSymetricCript.EncryptAndRemoveList(const AFilePath: String);
var
  Key: String;
  Value: Boolean;
begin
  Key := AFilePath;
  if ListFiles.TryGetValue(Key, Value) then
  begin
    if (not Value) then
    begin
      if IsFileIni(Key) then
      begin
        if (GetSymetricStatusIni(Key) = tssDecrypt) then
        begin
          if EncryptFile(Key) then
            ListFiles.Remove(Key);
        end;
      end
      else if EncryptFile(Key, False) then
        ListFiles.Remove(Key);
    end;
  end;
end;

function TSymetricCript.EncryptDEK(const ADEK: ByteArray; const AKEK: ByteArray; out AEncryptedDEKIV: ByteArray): ByteArray;
var
  factory: TElSymmetricCryptoFactory;
  crypto: TElSymmetricCrypto;
  km: TElSymmetricKeyMaterial;
  msDEKIn, msDEKOut: TMemoryStream;
begin
  factory := TElSymmetricCryptoFactory.Create;
  try
    crypto := factory.CreateInstance(SB_ALGORITHM_CNT_AES256, cmCBC);
    try
      km := TElSymmetricKeyMaterial.Create;
      try
        SetLength(AEncryptedDEKIV, 16);
        GenerateSecureRandom(AEncryptedDEKIV, Length(AEncryptedDEKIV));

        km.Key := AKEK;
        km.IV := AEncryptedDEKIV;
        crypto.KeyMaterial := km;

        msDEKIn := TMemoryStream.Create;
        msDEKOut := TMemoryStream.Create;
        try
          msDEKIn.WriteBuffer(ADEK[0], Length(ADEK));
          msDEKIn.Position := 0;
          crypto.Encrypt(msDEKIn, msDEKOut);
          SetLength(Result, msDEKOut.Size);
          msDEKOut.Position := 0;
          msDEKOut.ReadBuffer(Result[0], msDEKOut.Size);
        finally
          msDEKIn.Free;
          msDEKOut.Free;
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
end;

procedure TSymetricCript.UpdateFileIni(const AFilePath: String; const AEncrypt: Boolean);
var
  IniFile: TIniFile;
  IniPath: String;
begin
  IniPath := ChangeFileExt(AFilePath, '.ini');

  if FileExists(IniPath) then
  begin
    IniFile := TIniFile.Create(IniPath);
    try
      IniFile.WriteString(INI_REPORT_SETTINGS, 'SymetricCript', IfThen(AEncrypt, 'E', 'D'));
    finally
      IniFile.Free;
    end;
  end;
end;

function TSymetricCript.GenerateDEK: ByteArray;
begin
  SetLength(Result, 32);
  GenerateSecureRandom(Result, Length(Result));
end;

function TSymetricCript.GetSymetricStatusIni(const AFilePath: String): TSymetricStatus;
var
  IniFile: TIniFile;
  IniPath: String;
  status: String;
begin
  IniPath := ChangeFileExt(AFilePath, '.ini');

  if FileExists(IniPath) then
  begin
    IniFile := TIniFile.Create(IniPath);
    try
      status := IniFile.ReadString(INI_REPORT_SETTINGS, 'SymetricCript', '');

      if status.IsEmpty then
        Exit(tssEmpty);

      case status[1] of
        'E': Result := tssEncrypt;
        'D': Result := tssDecrypt;
      else
        Result := tssEmpty;
      end;
    finally
      IniFile.Free;
    end;
  end
  else
    Exit(tssEmpty);
end;

function TSymetricCript.IsFileIni(const AFilePath: String): Boolean;
begin
  Exit(FileExists(ChangeFileExt(AFilePath, '.ini')));
end;

function TSymetricCript.IsValidFileForEncryptOrDecrypt(const AFilePath: String; AExtensions: array of String): Boolean;
var
  DummyIndex: Integer;
begin
  TArray.Sort<String>(AExtensions, TStringComparer.Ordinal);
  Exit(not (TArray.BinarySearch<String>(AExtensions, TPath.GetExtension(AFilePath).ToUpper, DummyIndex, TStringComparer.Ordinal)));
end;

procedure TSymetricCript.StoreEncryptionMetadata(const AFilePath: String; const EncryptedDEK, EncryptedDEKIV, FileIV, Salt: ByteArray);
begin
  if FUseMetaFile then
    StoreEncryptionMetadataToFile(AFilePath, EncryptedDEK, EncryptedDEKIV, FileIV, Salt);
end;

procedure TSymetricCript.StoreEncryptionMetadataToFile(const AFilePath: String; const EncryptedDEK, EncryptedDEKIV, FileIV, Salt: ByteArray);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFilePath + '.meta', fmCreate);
  try
    WriteDWordToStream(fs, Length(EncryptedDEK));
    fs.WriteBuffer(EncryptedDEK[0], Length(EncryptedDEK));

    WriteDWordToStream(fs, Length(EncryptedDEKIV));
    fs.WriteBuffer(EncryptedDEKIV[0], Length(EncryptedDEKIV));

    WriteDWordToStream(fs, Length(FileIV));
    fs.WriteBuffer(FileIV[0], Length(FileIV));

    WriteDWordToStream(fs, Length(Salt));
    fs.WriteBuffer(Salt[0], Length(Salt));
  finally
    fs.Free;
  end;
end;

procedure TSymetricCript.StoreEncryptionMetadataInStream(Stream: TMemoryStream; const EncryptedDEK, EncryptedDEKIV, FileIV, Salt: ByteArray);
var
  MetaDataStream: TMemoryStream;
  MetaDataSize: Int64;
begin
  MetaDataStream := TMemoryStream.Create;
  try
    WriteDWordToStream(MetaDataStream, Length(EncryptedDEK));
    MetaDataStream.WriteBuffer(EncryptedDEK[0], Length(EncryptedDEK));

    WriteDWordToStream(MetaDataStream, Length(EncryptedDEKIV));
    MetaDataStream.WriteBuffer(EncryptedDEKIV[0], Length(EncryptedDEKIV));

    WriteDWordToStream(MetaDataStream, Length(FileIV));
    MetaDataStream.WriteBuffer(FileIV[0], Length(FileIV));

    WriteDWordToStream(MetaDataStream, Length(Salt));
    MetaDataStream.WriteBuffer(Salt[0], Length(Salt));

    MetaDataSize := MetaDataStream.Size;
    WriteInt64ToStream(MetaDataStream, MetaDataSize);

    MetaDataStream.Position := 0;
    Stream.CopyFrom(MetaDataStream, MetaDataStream.Size);
  finally
    MetaDataStream.Free;
  end;
end;

procedure TSymetricCript.RetrieveEncryptionMetadata(const AFilePath: String; out EncryptedDEK, EncryptedDEKIV, FileIV, Salt: ByteArray);
begin
  if FUseMetaFile then
    RetrieveEncryptionMetadataFromFile(AFilePath, EncryptedDEK, EncryptedDEKIV, FileIV, Salt)
  else
    RetrieveEncryptionMetadataFromStream(AFilePath, EncryptedDEK, EncryptedDEKIV, FileIV, Salt);
end;

procedure TSymetricCript.RetrieveEncryptionMetadataFromFile(const AFilePath: String; out EncryptedDEK, EncryptedDEKIV, FileIV, Salt: ByteArray);
var
  fs: TFileStream;
  EncryptedDEKSize, EncryptedDEKIVSize, FileIVSize, SaltSize: Integer;
begin
  fs := TFileStream.Create(AFilePath + '.meta', fmOpenRead);
  try
    EncryptedDEKSize := ReadDWordFromStream(fs);
    SetLength(EncryptedDEK, EncryptedDEKSize);
    fs.ReadBuffer(EncryptedDEK[0], EncryptedDEKSize);

    EncryptedDEKIVSize := ReadDWordFromStream(fs);
    SetLength(EncryptedDEKIV, EncryptedDEKIVSize);
    fs.ReadBuffer(EncryptedDEKIV[0], EncryptedDEKIVSize);

    FileIVSize := ReadDWordFromStream(fs);
    SetLength(FileIV, FileIVSize);
    fs.ReadBuffer(FileIV[0], FileIVSize);

    SaltSize := ReadDWordFromStream(fs);
    SetLength(Salt, SaltSize);
    fs.ReadBuffer(Salt[0], SaltSize);
  finally
    fs.Free;
  end;
end;

procedure TSymetricCript.RetrieveEncryptionMetadataFromStream(const AFilePath: String; out EncryptedDEK, EncryptedDEKIV, FileIV, Salt: ByteArray);
var
  fs: TFileStream;
  MetaDataSize: Int64;
  EncryptedDEKSize, EncryptedDEKIVSize, FileIVSize, SaltSize: Integer;
begin
  fs := TFileStream.Create(AFilePath, fmOpenRead);
  try
    fs.Position := fs.Size - SizeOf(Int64);
    MetaDataSize := ReadInt64FromStream(fs);

    fs.Position := fs.Size - MetaDataSize - SizeOf(Int64);
    EncryptedDEKSize := ReadDWordFromStream(fs);
    SetLength(EncryptedDEK, EncryptedDEKSize);

    fs.ReadBuffer(EncryptedDEK[0], EncryptedDEKSize);
    EncryptedDEKIVSize := ReadDWordFromStream(fs);
    SetLength(EncryptedDEKIV, EncryptedDEKIVSize);

    fs.ReadBuffer(EncryptedDEKIV[0], EncryptedDEKIVSize);
    FileIVSize := ReadDWordFromStream(fs);
    SetLength(FileIV, FileIVSize);

    fs.ReadBuffer(FileIV[0], FileIVSize);
    SaltSize := ReadDWordFromStream(fs);
    SetLength(Salt, SaltSize);

    fs.ReadBuffer(Salt[0], SaltSize);
  finally
    fs.Free;
  end;
end;

function TSymetricCript.GetMetadataSizeFromFile(const AFilePath: String): Int64;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFilePath, fmOpenRead);
  try
    fs.Position := fs.Size - SizeOf(Int64);
    Result := ReadInt64FromStream(fs) + SizeOf(Int64);
  finally
    fs.Free;
  end;
end;

procedure TSymetricCript.GenerateSecureRandom(var Buffer: ByteArray; const Size: Integer);
var
  RandomGen: TElRandom;
begin
  RandomGen := TElRandom.Create;
  try
    RandomGen.Generate(Buffer, Size);
  finally
    RandomGen.Free;
  end;
end;

function TSymetricCript.DeriveKEKFromPassword(const APassword: String; const Salt: ByteArray): ByteArray;
var
  Iterations: Integer;
  KeyLength: Integer;
  PasswordBytes: ByteArray;
begin
  Iterations := 10000;
  KeyLength := 32;
  PasswordBytes := ByteArray(BytesOf(UTF8String(APassword)));
  Result := PBKDF2_HMAC_SHA256(PasswordBytes, Salt, Iterations, KeyLength);
end;

function TSymetricCript.PBKDF2_HMAC_SHA256(const Password, Salt: ByteArray; Iterations, KeyLength: Integer): ByteArray;
var
  U, F: ByteArray;
  i, j, blocks: Integer;
  Counter: ByteArray;
begin
  blocks := (KeyLength + 31) div 32;
  SetLength(Result, blocks * 32);

  SetLength(Counter, 4);

  for i := 1 to blocks do
  begin
    Counter[0] := Byte((i shr 24) and $FF);
    Counter[1] := Byte((i shr 16) and $FF);
    Counter[2] := Byte((i shr 8) and $FF);
    Counter[3] := Byte(i and $FF);

    U := HMAC_SHA256(Password, Salt + Counter);
    F := U;

    for j := 2 to Iterations do
    begin
      U := HMAC_SHA256(Password, U);
      F := XorBytes(F, U);
    end;

    Move(F[0], Result[(i - 1) * 32], Length(F));
  end;

  SetLength(Result, KeyLength);
end;

function TSymetricCript.XorBytes(const A, B: ByteArray): ByteArray;
var
  i: Integer;
begin
  SetLength(Result, Length(A));
  for i := 0 to Length(A) - 1 do
    Result[i] := A[i] xor B[i];
end;

end.

