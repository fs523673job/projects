unit ucapturedoscmd;

interface

function ExecuteConsoleOutput(const ACommand, AParameters, ASystemName: String): Boolean;
function ExecuteConsoleOutputEx(const ACommand, AParameters, ASystemName: String; out AMessages: String): Boolean;

implementation

uses
  Windows,
  System.StrUtils,
  System.SysUtils,
  System.Classes,
  System.Console,
  System.Diagnostics
  ;


function HasDelphiIgnoredHintOrWarning(const AMessage: String): Boolean;
var
  MessageSanitized: String;
begin
  MessageSanitized := AMessage.ToLower;

   if (
       (Pos(('Value assigned to ''qryTmp'' never used').ToLower, MessageSanitized) > 0) or
       (Pos(('Variable ''aFileMajorNumber'' is declared but never used in ''TRPCServer.Start''').ToLower, MessageSanitized) > 0) or
       (Pos(('Variable ''aFileMinorNumber'' is declared but never used in ''TRPCServer.Start''').ToLower, MessageSanitized) > 0) or
       (Pos(('Variable ''AFileRelease'' is declared but never used in ''TRPCServer.Start''').ToLower, MessageSanitized) > 0)  or
       (Pos(('Variable ''aFileMajorNumber'' is declared but never used in').ToLower, MessageSanitized) > 0) or
       (Pos(('Variable ''aFileMinorNumber'' is declared but never used in').ToLower, MessageSanitized) > 0) or
       (Pos(('Variable ''AFileRelease'' is declared but never used in').ToLower, MessageSanitized) > 0) or
       (Pos(('warning W1005: Unit ''RPCServer'' is specific to a platform').ToLower, MessageSanitized) > 0) or
       (Pos(('warning W1002: Symbol ''IncludeTrailingBackslash'' is specific to a platform').ToLower, MessageSanitized) > 0) or
       (Pos(('warning W1002: Symbol ''faArchive'' is specific to a platform').ToLower, MessageSanitized) > 0) or
       (Pos(('warning W1002: Symbol ''Create'' is specific to a platform').ToLower, MessageSanitized) > 0) or
       (Pos(('warning W1029: Duplicate constructor').ToLower, MessageSanitized) > 0) or
       (Pos(('Variable ''FileMajorNumber'' is declared but never used in ''TRPCServer.Start''').ToLower, MessageSanitized) > 0) or
       (Pos(('Variable ''FileMinorNumber'' is declared but never used in ''TRPCServer.Start''').ToLower, MessageSanitized) > 0) or
       (Pos(('Variable ''FileRelease'' is declared but never used in ''TRPCServer.Start''').ToLower, MessageSanitized) > 0) or
       (Pos(('Variable ''FileBuild'' is declared but never used in ''TRPCServer.Start''').ToLower, MessageSanitized) > 0) or
       (Pos(('Variable ''StringPatchSplit'' is declared but never used in ''TRPCServer.Start''').ToLower, MessageSanitized) > 0) or
       (Pos(('Variable ''Conn'' is declared but never used in ''TRPCServer.Start''').ToLower, MessageSanitized) > 0)
    ) then
   begin
     Exit(True);
   end
   else
     Exit(False);
end;

function HasDelphiHintCode(const AMessage: String): Boolean;
const
  HINT_CODES_DELPHI: array[0..18] of string = (
    'H2077', 'H2135', 'H2164', 'H2219', 'H2235', 'H2244',
    'H2365', 'H2368', 'H2369', 'H2384', 'H2440', 'H2443',
    'H2444', 'H2445', 'H2456', 'H2457', 'H2505', 'H2509',
    'H2596'
  );
var
  i: Integer;
begin
  for i := Low(HINT_CODES_DELPHI) to High(HINT_CODES_DELPHI) do
  begin
    if Pos(HINT_CODES_DELPHI[i], AMessage) > 0 then
      Exit(True);
  end;

  Exit(False);
end;

function HasDelphiWarningCode(const AMessage: String): Boolean;
const
  WARNING_CODES_DELPHI: array[0..66] of string = (
    'W1000', 'W1001', 'W1002', 'W1003', 'W1004', 'W1005', 'W1006', 'W1007',
    'W1009', 'W1010', 'W1011', 'W1013', 'W1014', 'W1015', 'W1016', 'W1017',
    'W1018', 'W1021', 'W1022', 'W1023', 'W1024', 'W1029', 'W1031', 'W1032',
    'W1034', 'W1035', 'W1036', 'W1037', 'W1039', 'W1040', 'W1041', 'W1042',
    'W1043', 'W1044', 'W1045', 'W1046', 'W1047', 'W1048', 'W1049', 'W1050',
    'W1051', 'W1055', 'W1057', 'W1058', 'W1059', 'W1060', 'W1061', 'W1062',
    'W1063', 'W1064', 'W1066', 'W1067', 'W1068', 'W1069', 'W1070', 'W1071',
    'W1072', 'W1073', 'W1074', 'W1201', 'W1202', 'W1203', 'W1204', 'W1205',
    'W1206', 'W1207', 'W1208'
  );
var
  i: Integer;
begin
  for i := Low(WARNING_CODES_DELPHI) to High(WARNING_CODES_DELPHI) do
  begin
    if Pos(WARNING_CODES_DELPHI[i], AMessage) > 0 then
      Exit(True);
  end;

  Exit(False);
end;

function ExecuteConsoleOutput(const ACommand, AParameters, ASystemName: String): Boolean;
const
  CReadBuffer = 2400;
var
  saSecurity : TSecurityAttributes;
  hRead      : THandle;
  hWrite     : THandle;
  suiStartup : TStartupInfo;
  piProcess  : TProcessInformation;
  pBuffer    : array[0..CReadBuffer] of AnsiChar;
  dRead      : DWord;
  dRunning   : DWord;
begin
  Result := False;
  saSecurity.nLength              := SizeOf(TSecurityAttributes);
  saSecurity.bInheritHandle       := True;
  saSecurity.lpSecurityDescriptor := nil;
  if CreatePipe(hRead, hWrite, @saSecurity, 0) then
  begin
    try
      FillChar(suiStartup, SizeOf(TStartupInfo), #0);
      suiStartup.cb          := SizeOf(TStartupInfo);
      suiStartup.hStdInput   := hRead;
      suiStartup.hStdOutput  := hWrite;
      suiStartup.hStdError   := hWrite;
      suiStartup.dwFlags     := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      suiStartup.wShowWindow := SW_HIDE;
      if CreateProcess(nil, PChar(ACommand + ' ' + AParameters), @saSecurity, @saSecurity, True, NORMAL_PRIORITY_CLASS, nil, nil, suiStartup, piProcess) then
      begin
        Result := True;
        try
          CloseHandle(hWrite);
          repeat
            dRunning := WaitForSingleObject(piProcess.hProcess, 100);
            CheckSynchronize();
            SleepEx(100, True);
            repeat
              dRead := 0;
              if (windows.ReadFile(hRead, pBuffer[0], CReadBuffer, dRead, nil)) then
              begin
                pBuffer[dRead] := #0;
                OemToAnsi(pBuffer, pBuffer);
                Console.WriteLine(String(pBuffer));
              end;
            until (dRead < CReadBuffer);
          until (dRunning <> WAIT_TIMEOUT);
        finally
          CloseHandle(piProcess.hProcess);
          CloseHandle(piProcess.hThread);
        end;
      end;
    finally
      CloseHandle(hRead);
    end;
  end;
end;

function ExecuteConsoleOutputEx(const ACommand, AParameters, ASystemName: String; out AMessages: String): Boolean;
const
  CReadBuffer = 255;
var
  SecAtrrs        : TSecurityAttributes;
  StartupInfo     : TStartupInfo;
  ProcessInfo     : TProcessInformation;
  StdOutPipeRead  : THandle;
  StdOutPipeWrite : THandle;
  WasOK           : Boolean;
  pCommandLine    : array[0..CReadBuffer] of AnsiChar;
  BytesRead       : Cardinal;
  WorkDir         : string;
  Handle          : Boolean;
  CompError       : Boolean;
  CompHint        : Integer;
  CompWarning     : Integer;
  StopWatch       : TStopWatch;
begin
  Result := False;
  CompError := False;
  CompHint := 0;
  CompWarning := 0;
  AMessages := '';
  Console.WriteLine(StringOfChar('*', 80));
  StopWatch := TStopWatch.StartNew;
  Console.WriteColorLine('Inicializando a compilação do ' + ASystemName, [TConsoleColor.Yellow]);
  Console.WriteLine();
  try
    SecAtrrs.nLength              := SizeOf(SecAtrrs);
    SecAtrrs.bInheritHandle       := True;
    SecAtrrs.lpSecurityDescriptor := nil;
    CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SecAtrrs, 0);
    try
      FillChar(StartupInfo, SizeOf(StartupInfo), 0);
      StartupInfo.cb         := SizeOf(StartupInfo);
      StartupInfo.dwFlags     := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      StartupInfo.wShowWindow := SW_HIDE;
      StartupInfo.hStdInput   := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
      StartupInfo.hStdOutput  := StdOutPipeWrite;
      StartupInfo.hStdError   := StdOutPipeWrite;
      WorkDir := ExtractFileDir(ACommand);
      Handle  := CreateProcess(nil, PChar('cmd.exe /c ' + ACommand + ' ' + AParameters), nil, nil, True, 0, nil, PChar(WorkDir), StartupInfo, ProcessInfo);
      CloseHandle(StdOutPipeWrite);
      if Handle then
        try
          Result := True;
          repeat
            WasOK := Windows.ReadFile(StdOutPipeRead, pCommandLine, CReadBuffer, BytesRead, nil);
            if BytesRead > 0 then
            begin
              pCommandLine[BytesRead] := #0;
              OemToAnsi(pCommandLine, pCommandLine);
              if (Pos('ERROR', UpperCase(String(pCommandLine))) > 0) then
              begin
                Console.WriteColorLine(String(pCommandLine), [TConsoleColor.Red]);
                CompError := True;
              end
              else if (Pos('LINES', UpperCase(String(pCommandLine))) > 0) and (Pos('SECONDS', UpperCase(String(pCommandLine))) > 0) and (Pos('BYTES CODE', UpperCase(String(pCommandLine))) > 0) and (Pos('BYTES DATA', UpperCase(String(pCommandLine))) > 0) then
                Console.WriteColorLine(String('BUILD [OK] -> ' + pCommandLine), [TConsoleColor.Green])
              else if (Pos('Fim do script de compilacao', String(pCommandLine)) > 0) then
              begin
                Console.WriteColorLine(String(pCommandLine), [TConsoleColor.Green]);
                AMessages := Format('%s - Compilado sem erros', [ASystemName]);
              end
              else if (Pos('ApWebDispatcher', String(pCommandLine)) > 0) then
              begin
                Console.WriteColorLine(String(pCommandLine), [TConsoleColor.Green]);
                AMessages := Format('%s - Compilado sem erros', [ASystemName]);
              end
              else if (Pos('ERROS', UpperCase(String(pCommandLine))) > 0) then
              begin
                Console.WriteColorLine(String(pCommandLine), [TConsoleColor.Red]);
                CompError := True;
              end
              else if HasDelphiHintCode(String(pCommandLine)) then
              begin
                if not HasDelphiIgnoredHintOrWarning(String(pCommandLine)) then
                begin
                  Console.WriteColorLine(String(pCommandLine), [TConsoleColor.Yellow]);
                  Inc(CompHint);
                end;
              end
              else if HasDelphiWarningCode(String(pCommandLine)) then
              begin
                if not HasDelphiIgnoredHintOrWarning(String(pCommandLine)) then
                begin
                  Console.WriteColorLine(String(pCommandLine), [TConsoleColor.DarkYellow]);
                  Inc(CompWarning);
                end;
              end
              else
                Console.WriteColorLine(String(pCommandLine), [TConsoleColor.White]);
            end;
          until not WasOK or (BytesRead = 0);
          WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
        finally
          CloseHandle(ProcessInfo.hThread);
          CloseHandle(ProcessInfo.hProcess);
        end;
    finally
      CloseHandle(StdOutPipeRead);
    end;
  finally
    if CompError then
      AMessages := Format('%s - Erros Encontrados Na Compilacao', [ASystemName]);

    if CompHint > 0 then
      AMessages := Format('%s - [Hints Encontrados %d]', [IfThen(AMessages.IsEmpty, ASystemName, AMessages), CompHint]);

    if CompWarning > 0 then
      AMessages := Format('%s - [Warnings Encontrados %d]', [IfThen(AMessages.IsEmpty, ASystemName, AMessages), CompWarning]);

    AMessages := Format('%s - [%d - ms (%s)]', [IfThen(AMessages.IsEmpty, ASystemName, AMessages), StopWatch.ElapsedMilliseconds, StopWatch.Elapsed.ToString]);

    Console.WriteLine();
    Console.WriteColorLine(Format('Finalizando a compilação do %s em %d - ms (%s)', [ASystemName, StopWatch.ElapsedMilliseconds, StopWatch.Elapsed.ToString]), [TConsoleColor.Yellow]);
    Console.WriteLine(StringOfChar('*', 80));
  end;
end;

end.
