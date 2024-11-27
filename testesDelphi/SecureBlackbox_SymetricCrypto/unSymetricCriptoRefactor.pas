unit unSymetricCriptoRefactor;

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
  SBUtils
  ;

const
  ENCRYPTED_EXTENSION = '.APC';
  INI_REPORT_SETTINGS = 'Report Settings';

type
  TSymetricClass  = (tsclegacy, tscKekDek);
  TSymetricStatus = (tssEmpty, tssEncrypt, tssDecrypt);

  ISymmetricCrypt = interface
    ['{B1F5E0D1-8F74-4F1A-B8D6-FA5D3C5E3F9D}']
    procedure DecryptAndAddList(const AFilePath: String);
    procedure EncryptAndRemoveList(const AFilePath: String);
    procedure EncryptAllListFilesNecessary;

    function EncryptFile(const AFilePath: String): Boolean; overload;
    function DecryptFile(const AFilePath: String): Boolean; overload;
    function EncryptFile(var AFilePath: String; const ARenameFile: Boolean): Boolean; overload;
    function DecryptFile(var AFilePath: String; const ARenameFile: Boolean): Boolean; overload;
  end;

  TLegacySymetricCript = class(TInterfacedObject, ISymmetricCrypt)
  private
    ListFiles: TDictionary<String, Boolean>; //Path, (Encrypt/Decrypt = True/False)

    procedure UpdateFileIni(const AFilePath: String; const AEncrypt: Boolean);
    function GetSymetricStatusIni(const AFilePath: String): TSymetricStatus;

    function IsFileIni(const AFilePath: String): Boolean;
    function IsValidFileForEncryptOrDecrypt(const AFilePath: String; AExtensions: array of String): Boolean;

    procedure EncryptFileMark(const AFilePath: String; const ARenameFile: Boolean);
    procedure DecryptFileMark(const AFilePath: String; const ARenameFile: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure DecryptAndAddList(const AFilePath: String);
    procedure EncryptAndRemoveList(const AFilePath: String);

    procedure EncryptAllListFilesNecessary;

    function EncryptFile(const AFilePath: String): Boolean; overload;
    function DecryptFile(const AFilePath: String): Boolean; overload;
    function EncryptFile(var AFilePath: String; const ARenameFile: Boolean): Boolean; overload;
    function DecryptFile(var AFilePath: String; const ARenameFile: Boolean): Boolean; overload;
  end;

  TKekDekSymetricCript = class(TInterfacedObject, ISymmetricCrypt)
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
    function GetMetadataSizeFromStream(const AStream: TStream): Int64;
    // Info In MetaData or InFile
    procedure RetrieveEncryptionMetadataFromFile(const AFilePath: String; out EncryptedDEK, EncryptedDEKIV, FileIV, Salt: ByteArray);
    procedure RetrieveEncryptionMetadataFromStream(const AFilePath: String; out EncryptedDEK, EncryptedDEKIV, FileIV, Salt: ByteArray);
    procedure StoreEncryptionMetadataInStream(Stream: TStream; const EncryptedDEK, EncryptedDEKIV, FileIV, Salt: ByteArray);
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

  TSymmetricCryptFactory = class(TObject)
    class function NewSymmetricCrypt(const AType: TSymetricClass; const APassPhrase: String = ''; const AUseMetaFile: Boolean = False): ISymmetricCrypt;
  end;


implementation

{ TLegacySymetricCript }

constructor TLegacySymetricCript.Create;
begin
  ListFiles := TDictionary<String, Boolean>.Create;
end;

destructor TLegacySymetricCript.Destroy;
begin
  try
    EncryptAllListFilesNecessary;
  except
  end;

  ListFiles.Free;
  inherited;
end;

procedure TLegacySymetricCript.DecryptAndAddList(const AFilePath: String);
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

function TLegacySymetricCript.DecryptFile(const AFilePath: String): Boolean;
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

function TLegacySymetricCript.DecryptFile(var AFilePath: String; const ARenameFile: Boolean): Boolean;
var
  factory: TElSymmetricCryptoFactory;
  crypto: TElSymmetricCrypto;
  km: TElSymmetricKeyMaterial;
  sbbyteKey: ByteArray;
  sbbyteIV: ByteArray;
  msFileIn: TStreamReader;
  msFileOut: TStreamWriter;
  newExt: String;
  newFilePath: String;
  sFileOut: string;
begin
  try
    sbbyteKey := [64, 64, 97, 112, 100, 97, 116, 97, 115, 105, 109, 102, 105, 108, 101, 107, 101, 121];
    sbbyteIV := [64, 64, 97, 112, 100, 97, 116, 97, 115, 105, 109, 102, 105, 108, 101, 73, 86];
    SetLength(sbbyteKey, 32);
    SetLength(sbbyteIV, 16);
    factory := TElSymmetricCryptoFactory.Create;
    try
      crypto := factory.CreateInstance(SB_ALGORITHM_CNT_AES256, cmCBC);
      try
        km := TElSymmetricKeyMaterial.Create;
        try
          km.Key := sbbyteKey;
          km.IV := sbbyteIV;
          crypto.KeyMaterial := km;
          msFileIn := TStreamReader.Create(AFilePath);
          try
            sFileOut := ChangeFileExt(AFilePath, '.TMP');
            msFileOut := TStreamWriter.Create(sFileOut);
            try
              crypto.Decrypt(msFileIn.BaseStream, msFileOut.BaseStream);
              msFileIn.Close;
              msFileOut.Close;
              TFile.Delete(AFilePath);
              TFile.Move(sFileOut, AFilePath);
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
              msFileOut.Free;
            end;
          finally
            msFileIn.Free;
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
    on E: EElSymmetricCryptoError do
    begin
      TFile.Delete(sFileOut);
      Result := False;
    end;
    on E: Exception do
      raise Exception.CreateFmt('Erro ao descriptografar o relatório "%s": %s', [AFilePath, E.Message]);
  end;
end;

procedure TLegacySymetricCript.DecryptFileMark(const AFilePath: String; const ARenameFile: Boolean);
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

function TLegacySymetricCript.EncryptFile(var AFilePath: String; const ARenameFile: Boolean): Boolean;
var
  factory: TElSymmetricCryptoFactory;
  crypto: TElSymmetricCrypto;
  km: TElSymmetricKeyMaterial;
  sbbyteKey: ByteArray;
  sbbyteIV: ByteArray;
  msFileIn: TStreamReader;
  msFileOut: TStreamWriter;
  sFileOut: string;
  newExt: String;
  newFilePath: String;
begin
  try
    sbbyteKey := [64, 64, 97, 112, 100, 97, 116, 97, 115, 105, 109, 102, 105, 108, 101, 107, 101, 121];
    sbbyteIV := [64, 64, 97, 112, 100, 97, 116, 97, 115, 105, 109, 102, 105, 108, 101, 73, 86];
    SetLength(sbbyteKey, 32);
    SetLength(sbbyteIV, 16);
    factory := TElSymmetricCryptoFactory.Create;
    try
      crypto := factory.CreateInstance(SB_ALGORITHM_CNT_AES256, cmCBC);
      try
        km := TElSymmetricKeyMaterial.Create;
        try
          km.Key := sbbyteKey;
          km.IV := sbbyteIV;
          crypto.KeyMaterial := km;
          msFileIn := TStreamReader.Create(AFilePath);
          try
            sFileOut := ChangeFileExt(AFilePath, '.tmp');
            msFileOut := TStreamWriter.Create(sFileOut);
            try
              crypto.Encrypt(msFileIn.BaseStream, msFileOut.BaseStream);
              msFileIn.Close;
              msFileOut.Close;
              TFile.Delete(AFilePath);
              TFile.Move(sFileOut, AFilePath);
              if ARenameFile then
              begin
                newExt := ExtractFileExt(AFilePath) + ENCRYPTED_EXTENSION;
                newFilePath := ChangeFileExt(AFilePath, newExt);
                if RenameFile(AFilePath, newFilePath) then
                  AFilePath := newFilePath;
              end;
              Result := True;
            finally
              msFileOut.Free;
            end;
          finally
            msFileIn.Free;
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
    on E: EElSymmetricCryptoError do
    begin
      TFile.Delete(sFileOut);
      Result := False;
    end;
    on E: Exception do
      raise Exception.CreateFmt('Erro ao criptografar o relatório "%s": %s', [AFilePath, E.Message]);
  end;
end;

procedure TLegacySymetricCript.EncryptFileMark(const AFilePath: String; const ARenameFile: Boolean);
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

function TLegacySymetricCript.EncryptFile(const AFilePath: String): Boolean;
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

procedure TLegacySymetricCript.EncryptAllListFilesNecessary;
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

procedure TLegacySymetricCript.EncryptAndRemoveList(const AFilePath: String);
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

procedure TLegacySymetricCript.UpdateFileIni(const AFilePath: String; const AEncrypt: Boolean);
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

function TLegacySymetricCript.GetSymetricStatusIni(const AFilePath: String): TSymetricStatus;
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

function TLegacySymetricCript.IsFileIni(const AFilePath: String): Boolean;
begin
  Exit(FileExists(ChangeFileExt(AFilePath, '.ini')));
end;

function TLegacySymetricCript.IsValidFileForEncryptOrDecrypt(const AFilePath: String; AExtensions: array of String): Boolean;
var
  DummyIndex: Integer;
begin
  TArray.Sort<String>(AExtensions, TStringComparer.Ordinal);
  Exit(not (TArray.BinarySearch<String>(AExtensions, TPath.GetExtension(AFilePath).ToUpper, DummyIndex, TStringComparer.Ordinal)));
end;

{ TKekDekSymetricCript  }

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

constructor TKekDekSymetricCript.Create(const APassPhrase: String; AUseMetaFile: Boolean = True);
begin
  ListFiles := TDictionary<String, Boolean>.Create;
  FPassPhrase := APassPhrase;
  FUseMetaFile := AUseMetaFile;
end;

destructor TKekDekSymetricCript.Destroy;
begin
  try
    EncryptAllListFilesNecessary;
  except
  end;
  ListFiles.Free;
  inherited;
end;

procedure TKekDekSymetricCript.DecryptAndAddList(const AFilePath: String);
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

function TKekDekSymetricCript.DecryptDEK(const AEncryptedDEK, AKEK, AEncryptedDEKIV: ByteArray): ByteArray;
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
        try
          msDEKOut := TMemoryStream.Create;
          try
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

function TKekDekSymetricCript.DecryptFile(const AFilePath: String): Boolean;
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

function TKekDekSymetricCript.DecryptFile(var AFilePath: String; const ARenameFile: Boolean): Boolean;
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
          try
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
              msFileOut.Free;
            end;
          finally
            msFileIn.Free;
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

procedure TKekDekSymetricCript.DecryptFileMark(const AFilePath: String; const ARenameFile: Boolean);
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

function TKekDekSymetricCript.EncryptFile(var AFilePath: String; const ARenameFile: Boolean): Boolean;
var
  factory: TElSymmetricCryptoFactory;
  crypto: TElSymmetricCrypto;
  km: TElSymmetricKeyMaterial;
  msFileIn: TStreamReader;
  msFileOut: TStreamWriter;
  newExt: String;
  newFilePath: String;
  DEK, EncryptedDEK, EncryptedDEKIV, FileIV, Salt, KEK: ByteArray;
  sFileOut: string;
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
          msFileIn := TStreamReader.Create(AFilePath);
          try
            sFileOut := ChangeFileExt(AFilePath, '.tmp');
            msFileOut := TStreamWriter.Create(sFileOut);
            try
              crypto.Encrypt(msFileIn.BaseStream, msFileOut.BaseStream);
              if FUseMetaFile then
              begin
                msFileOut.Close;
                StoreEncryptionMetadata(AFilePath, EncryptedDEK, EncryptedDEKIV, FileIV, Salt);
              end
              else
              begin
                StoreEncryptionMetadataInStream(msFileOut.BaseStream, EncryptedDEK, EncryptedDEKIV, FileIV, Salt);
                msFileOut.Close;
              end;
              msFileIn.Close;
              msFileOut.Close;
              TFile.Delete(AFilePath);
              TFile.Move(sFileOut, AFilePath);
              if ARenameFile then
              begin
                newExt := ExtractFileExt(AFilePath) + ENCRYPTED_EXTENSION;
                newFilePath := ChangeFileExt(AFilePath, newExt);
                if RenameFile(AFilePath, newFilePath) then
                  AFilePath := newFilePath;
              end;
              Result := True;
            finally
              msFileOut.Free;
            end;
          finally
            msFileIn.Free;
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
    on E: EElSymmetricCryptoError do
    begin
      TFile.Delete(sFileOut);
      Result := False;
    end;
    on E: Exception do
    begin
      Result := False;
      raise Exception.CreateFmt('Erro ao criptografar o relatório "%s": %s', [AFilePath, E.Message]);
    end;
  end;
end;

procedure TKekDekSymetricCript.EncryptFileMark(const AFilePath: String; const ARenameFile: Boolean);
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

function TKekDekSymetricCript.EncryptFile(const AFilePath: String): Boolean;
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

procedure TKekDekSymetricCript.EncryptAllListFilesNecessary;
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

procedure TKekDekSymetricCript.EncryptAndRemoveList(const AFilePath: String);
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

function TKekDekSymetricCript.EncryptDEK(const ADEK: ByteArray; const AKEK: ByteArray; out AEncryptedDEKIV: ByteArray): ByteArray;
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

procedure TKekDekSymetricCript.UpdateFileIni(const AFilePath: String; const AEncrypt: Boolean);
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

function TKekDekSymetricCript.GenerateDEK: ByteArray;
begin
  SetLength(Result, 32);
  GenerateSecureRandom(Result, Length(Result));
end;

function TKekDekSymetricCript.GetSymetricStatusIni(const AFilePath: String): TSymetricStatus;
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

function TKekDekSymetricCript.IsFileIni(const AFilePath: String): Boolean;
begin
  Exit(FileExists(ChangeFileExt(AFilePath, '.ini')));
end;

function TKekDekSymetricCript.IsValidFileForEncryptOrDecrypt(const AFilePath: String; AExtensions: array of String): Boolean;
var
  DummyIndex: Integer;
begin
  TArray.Sort<String>(AExtensions, TStringComparer.Ordinal);
  Exit(not (TArray.BinarySearch<String>(AExtensions, TPath.GetExtension(AFilePath).ToUpper, DummyIndex, TStringComparer.Ordinal)));
end;

procedure TKekDekSymetricCript.StoreEncryptionMetadata(const AFilePath: String; const EncryptedDEK, EncryptedDEKIV, FileIV, Salt: ByteArray);
begin
  if FUseMetaFile then
    StoreEncryptionMetadataToFile(AFilePath, EncryptedDEK, EncryptedDEKIV, FileIV, Salt);
end;

procedure TKekDekSymetricCript.StoreEncryptionMetadataToFile(const AFilePath: String; const EncryptedDEK, EncryptedDEKIV, FileIV, Salt: ByteArray);
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

(*
procedure TKekDekSymetricCript.StoreEncryptionMetadataInStream(Stream: TStream; const EncryptedDEK, EncryptedDEKIV, FileIV, Salt: ByteArray);
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
*)

procedure TKekDekSymetricCript.StoreEncryptionMetadataInStream(Stream: TStream; const EncryptedDEK, EncryptedDEKIV, FileIV, Salt: ByteArray);
var
  MetaDataSize: Int64;
begin
  MetaDataSize := 0;
  MetaDataSize := MetaDataSize + SizeOf(Cardinal) + Length(EncryptedDEK);
  MetaDataSize := MetaDataSize + SizeOf(Cardinal) + Length(EncryptedDEKIV);
  MetaDataSize := MetaDataSize + SizeOf(Cardinal) + Length(FileIV);
  MetaDataSize := MetaDataSize + SizeOf(Cardinal) + Length(Salt);
  WriteDWordToStream(Stream, Length(EncryptedDEK));
  if Length(EncryptedDEK) > 0 then
    Stream.WriteBuffer(EncryptedDEK[0], Length(EncryptedDEK));
  WriteDWordToStream(Stream, Length(EncryptedDEKIV));
  if Length(EncryptedDEKIV) > 0 then
    Stream.WriteBuffer(EncryptedDEKIV[0], Length(EncryptedDEKIV));
  WriteDWordToStream(Stream, Length(FileIV));
  if Length(FileIV) > 0 then
    Stream.WriteBuffer(FileIV[0], Length(FileIV));
  WriteDWordToStream(Stream, Length(Salt));
  if Length(Salt) > 0 then
    Stream.WriteBuffer(Salt[0], Length(Salt));
  WriteInt64ToStream(Stream, MetaDataSize);
end;

procedure TKekDekSymetricCript.RetrieveEncryptionMetadata(const AFilePath: String; out EncryptedDEK, EncryptedDEKIV, FileIV, Salt: ByteArray);
begin
  if FUseMetaFile then
    RetrieveEncryptionMetadataFromFile(AFilePath, EncryptedDEK, EncryptedDEKIV, FileIV, Salt)
  else
    RetrieveEncryptionMetadataFromStream(AFilePath, EncryptedDEK, EncryptedDEKIV, FileIV, Salt);
end;

procedure TKekDekSymetricCript.RetrieveEncryptionMetadataFromFile(const AFilePath: String; out EncryptedDEK, EncryptedDEKIV, FileIV, Salt: ByteArray);
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

procedure TKekDekSymetricCript.RetrieveEncryptionMetadataFromStream(const AFilePath: String; out EncryptedDEK, EncryptedDEKIV, FileIV, Salt: ByteArray);
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

function TKekDekSymetricCript.GetMetadataSizeFromFile(const AFilePath: String): Int64;
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

function TKekDekSymetricCript.GetMetadataSizeFromStream(const AStream: TStream): Int64;
var
  OriginalPosition: Int64;
begin
  OriginalPosition := AStream.Position;
  try
    AStream.Position := AStream.Size - SizeOf(Int64);
    Result := ReadInt64FromStream(AStream) + SizeOf(Int64);
  finally
    AStream.Position := OriginalPosition;
  end;
end;

procedure TKekDekSymetricCript.GenerateSecureRandom(var Buffer: ByteArray; const Size: Integer);
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

function TKekDekSymetricCript.DeriveKEKFromPassword(const APassword: String; const Salt: ByteArray): ByteArray;
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

function TKekDekSymetricCript.PBKDF2_HMAC_SHA256(const Password, Salt: ByteArray; Iterations, KeyLength: Integer): ByteArray;
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

function TKekDekSymetricCript.XorBytes(const A, B: ByteArray): ByteArray;
var
  i: Integer;
begin
  SetLength(Result, Length(A));
  for i := 0 to Length(A) - 1 do
    Result[i] := A[i] xor B[i];
end;

{ TSymmetricCryptFactory }

class function TSymmetricCryptFactory.NewSymmetricCrypt(const AType: TSymetricClass; const APassPhrase: String = ''; const AUseMetaFile: Boolean = False): ISymmetricCrypt;
begin
  case AType of
    tsclegacy: Result := TLegacySymetricCript.Create;
    tscKekDek: Result := TKekDekSymetricCript.Create(APassPhrase, AUseMetaFile);
    else
      Result := TLegacySymetricCript.Create
  end;
end;

end.
