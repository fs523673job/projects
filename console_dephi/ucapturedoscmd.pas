unit ucapturedoscmd;

interface

function ExecuteConsoleOutput(const ACommand, AParameters, ASystemName: String): Boolean;
function ExecuteConsoleOutputEx(const ACommand, AParameters, ASystemName: String; out AMessages: String): Boolean;

implementation

uses
  Windows,
  System.SysUtils,
  System.Classes,
  System.Console
  ;

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
begin
  Result := False;
  AMessages := '';
  Console.WriteLine(StringOfChar('*', 80));
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
                AMessages := AMessages + #13#10 + Format('%s - Erro - Verifique novamente', [ASystemName]);
              end
              else if (Pos('LINES', UpperCase(String(pCommandLine))) > 0) and (Pos('SECONDS', UpperCase(String(pCommandLine))) > 0) and (Pos('BYTES CODE', UpperCase(String(pCommandLine))) > 0) and (Pos('BYTES DATA', UpperCase(String(pCommandLine))) > 0) then
                Console.WriteColorLine(String('BUILD [OK] -> ' + pCommandLine), [TConsoleColor.Green])
              else if (Pos('Fim do script de compilacao', String(pCommandLine)) > 0) then
              begin
                Console.WriteColorLine(String(pCommandLine), [TConsoleColor.Green]);
                AMessages := AMessages + #13#10 + Format('%s - Compilado sem erros', [ASystemName]);
              end
              else if (Pos('ERROS', UpperCase(String(pCommandLine))) > 0) then
              begin
                Console.WriteColorLine(String(pCommandLine), [TConsoleColor.DarkYellow]);
                AMessages := AMessages + #13#10 + Format('%s - Compilado com erros', [ASystemName]);
              end
              else if (Pos('WARNING', UpperCase(String(pCommandLine))) > 0) then
                Console.WriteColorLine(String(pCommandLine), [TConsoleColor.Yellow])
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
    Console.WriteLine();
    Console.WriteColorLine('Finalizando a compilação do ' + ASystemName, [TConsoleColor.Yellow]);
    Console.WriteLine(StringOfChar('*', 80));
  end;
end;
end.
