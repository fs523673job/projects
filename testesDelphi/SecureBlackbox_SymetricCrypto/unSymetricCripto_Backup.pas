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
  SBPKCS5
  ;

type
  TSymetricStatus = (tssEmpty, tssEncrypt, tssDecrypt);

  TSymetricCript = class
  private
    const ENCRYPTED_EXTENSION = '.APC';
    const INI_REPORT_SETTINGS = 'Report Settings';
    const _SBBYTEkEY : ByteArray = [64, 64, 97, 112, 100, 97, 116, 97, 115, 105, 109, 102, 105, 108, 101, 107, 101, 121];
    const _SBBYTEiv : ByteArray = [64, 64, 97, 112, 100, 97, 116, 97, 115, 105, 109, 102, 105, 108, 101, 73, 86];
  private
    ListFiles: TDictionary<String, Boolean>; //Path, (Encrypt/Decrypt = True/False)
    FKEK: ByteArray;

    procedure UpdateFileIni(const AFilePath: String; const AEncrypt: Boolean);
    function GetSymetricStatusIni(const AFilePath: String): TSymetricStatus;
    function IsFileIni(const AFilePath: String): Boolean;
    function IsValidFileForEncryptOrDecrypt(const AFilePath: String; AExtensions: array of String): Boolean;
    procedure EncryptFileMark(const AFilePath: String; const ARenameFile: Boolean);
    procedure DecryptFileMark(const AFilePath: String; const ARenameFile: Boolean);

    //DEK's KEK's
    function GenerateDEK: ByteArray;
    function EncryptDEK(const ADEK: ByteArray): ByteArray;
    function DecryptDEK(const AEncryptedDEK: ByteArray; AIV: ByteArray): ByteArray;
    procedure RetrieveEncryptionMetadata(const AFilePath: String; out EncryptedDEK, IV: ByteArray);
    procedure StoreEncryptionMetadata(const AFilePath: String; const EncryptedDEK, IV: ByteArray);
    procedure GenerateSecureRandom(var Buffer: ByteArray; const Size: Integer);
    function DeriveKEKFromPassword(const APassword: String): ByteArray;
  public
    constructor Create(const APassPhrase: String);
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

constructor TSymetricCript.Create(const APassPhrase: String);
begin
  ListFiles := TDictionary<String, Boolean>.Create;
  FKEK := DeriveKEKFromPassword(APassPhrase);
  SetLength(FKEK, 32);
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

function TSymetricCript.DecryptDEK(const AEncryptedDEK: ByteArray; AIV: ByteArray): ByteArray;
var
  factory: TElSymmetricCryptoFactory;
  crypto: TElSymmetricCrypto;
  km: TElSymmetricKeyMaterial;
  msDEKIn, msDEKOut: TMemoryStream;
  IV: ByteArray;
  KEKTest: ByteArray;
begin
  factory := TElSymmetricCryptoFactory.Create;
  try
    crypto := factory.CreateInstance(SB_ALGORITHM_CNT_AES256, cmCBC);
    try
      km := TElSymmetricKeyMaterial.Create;
      try
        msDEKIn := TMemoryStream.Create;
        try
          msDEKOut := TMemoryStream.Create;
          try
            SetLength(IV, 16);
            //Move(AEncryptedDEK[0], IV[0], 16);
            SetLength(KEKTest, 32);
            KEKTest := [71, 90, 227, 61, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
            //km.Key := FKEK;
            km.Key := KEKTest;
            km.IV := AIV;
            crypto.KeyMaterial := km;
            msDEKIn.WriteBuffer(AEncryptedDEK[0], Length(AEncryptedDEK));
            msDEKIn.Position := 0;
            crypto.Decrypt(msDEKIn, msDEKOut);
            SetLength(Result, msDEKOut.Size);
            msDEKOut.Position := 0;
            msDEKOut.ReadBuffer(Result[0], msDEKOut.Size);
          finally
            msDEKOut.Free;
          end;
        finally
          msDEKIn.Free;
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
  sbbyteKey: ByteArray;
  sbbyteIV: ByteArray;
  msFileIn, msFileOut: TMemoryStream;
  newExt: String;
  newFilePath: String;
  EncryptedDEK: ByteArray;
  DEK: ByteArray;
  IV: ByteArray;
begin
  try
    RetrieveEncryptionMetadata(AFilePath, EncryptedDEK, IV);
    DEK := DecryptDEK(EncryptedDEK, IV);
    factory := TElSymmetricCryptoFactory.Create;
    try
      crypto := factory.CreateInstance(SB_ALGORITHM_CNT_AES256, cmCBC);
      try
        km := TElSymmetricKeyMaterial.Create;
        try
          km.Key := DEK;
          km.IV := IV;
          crypto.KeyMaterial := km;
          msFileIn := TMemoryStream.Create;
          msFileOut := TMemoryStream.Create;
          try
            msFileIn.LoadFromFile(AFilePath);
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
    on e : Exception do
    begin
      Result := False;
      raise;
    end;
  end;
end;

procedure TSymetricCript.DecryptFileMark(const AFilePath: String; const ARenameFile: Boolean);
var
  IniFile     : TIniFile;
  IniPath     : String;
  FileName    : String;
  c           : Integer;
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
  DEK: ByteArray;
  EncryptedDEK: ByteArray;
  IV: ByteArray;
begin
  try
    DEK := GenerateDEK;
    EncryptedDEK := EncryptDEK(DEK);
    factory := TElSymmetricCryptoFactory.Create;
    try
      crypto := factory.CreateInstance(SB_ALGORITHM_CNT_AES256, cmCBC);
      try
        km := TElSymmetricKeyMaterial.Create;
        try
          SetLength(IV, 16);
          GenerateSecureRandom(IV, 16);
          km.Key := DEK;
          km.IV := IV;
          crypto.KeyMaterial := km;
          msFileIn := TMemoryStream.Create;
          msFileOut := TMemoryStream.Create;
          try
            msFileIn.LoadFromFile(AFilePath);
            crypto.Encrypt(msFileIn, msFileOut);
            msFileOut.SaveToFile(AFilePath);
            StoreEncryptionMetadata(AFilePath, EncryptedDEK, km.IV);
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
  IniFile  : TIniFile;
  IniPath  : String;
  FileName : String;
  c        : Integer;
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

function TSymetricCript.EncryptDEK(const ADEK: ByteArray): ByteArray;
var
  factory: TElSymmetricCryptoFactory;
  crypto: TElSymmetricCrypto;
  km: TElSymmetricKeyMaterial;
  msDEKIn, msDEKOut: TMemoryStream;
  IV: ByteArray;
begin
  factory := TElSymmetricCryptoFactory.Create;
  try
    crypto := factory.CreateInstance(SB_ALGORITHM_CNT_AES256, cmCBC);
    try
      km := TElSymmetricKeyMaterial.Create;
      try
        SetLength(IV, 16);
        GenerateSecureRandom(IV, Length(IV));
        km.Key := FKEK;
        km.IV := IV;
        crypto.KeyMaterial := km;
        msDEKIn := TMemoryStream.Create;
        try
          msDEKOut := TMemoryStream.Create;
          try
            msDEKIn.WriteBuffer(ADEK[0], Length(ADEK));
            msDEKIn.Position := 0;
            crypto.Encrypt(msDEKIn, msDEKOut);
            SetLength(Result, msDEKOut.Size);
            msDEKOut.Position := 0;
            msDEKOut.ReadBuffer(Result[0], msDEKOut.Size);
          finally
            msDEKOut.Free;
          end;
        finally
          msDEKIn.Free;
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
  SetLength(Result, 32); // Para AES-256
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

procedure TSymetricCript.StoreEncryptionMetadata(const AFilePath: String; const EncryptedDEK, IV: ByteArray);
var
  fs: TFileStream;
  MetaSize: Integer;
begin
  fs := TFileStream.Create(AFilePath, fmOpenReadWrite);
  try
    fs.Position := 0;
    MetaSize := Length(EncryptedDEK) + Length(IV) + SizeOf(Integer);
    fs.WriteBuffer(MetaSize, SizeOf(Integer));
    fs.WriteBuffer(EncryptedDEK[0], Length(EncryptedDEK));
    fs.WriteBuffer(IV[0], Length(IV));
  finally
    fs.Free;
  end;
end;

procedure TSymetricCript.RetrieveEncryptionMetadata(const AFilePath: String; out EncryptedDEK, IV: ByteArray);
var
  fs: TFileStream;
  MetaSize: Integer;
  EncryptedDEKSize: Integer;
  IVSize: Integer;
begin
  fs := TFileStream.Create(AFilePath, fmOpenRead);
  try
    fs.Position := 0;
    fs.ReadBuffer(MetaSize, SizeOf(Integer));
    EncryptedDEKSize := 48;
    IVSize := 16;
    SetLength(EncryptedDEK, EncryptedDEKSize);
    SetLength(IV, IVSize);
    fs.ReadBuffer(EncryptedDEK[0], EncryptedDEKSize);
    fs.ReadBuffer(IV[0], IVSize);
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

function TSymetricCript.DeriveKEKFromPassword(const APassword: String): ByteArray;
var
  KDF: TElPKCS5PBE;
  Iterations: Integer;
  KeyLength: Integer;
  Salt: ByteArray;
begin
  // Definir os parâmetros do KDF
  Iterations := 10000; // Ajuste conforme necessário para equilíbrio entre segurança e desempenho
  KeyLength := 32;     // 32 bytes para AES-256
  SetLength(Salt, 16);
  GenerateSecureRandom(Salt, 16);

  // Criar uma instância do KDF
  KDF := TElPKCS5PBE.Create(SB_ALGORITHM_CNT_AES256, SB_ALGORITHM_DGST_SHA256, True);
  try
    KDF.IterationCount := Iterations;
    //KDF.PseudoRandomFunction := SB_ALGORITHM_MAC_HMACSHA256;
    KDF.Salt := Salt;
    Result := KDF.DeriveKey(APassword, KeyLength);
  finally
    KDF.Free;
  end;
end;

end.
