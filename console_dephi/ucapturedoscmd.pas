﻿unit ucapturedoscmd;

interface

function ExecuteConsoleOutput(const ACommand, AParameters: String): Boolean;

implementation

uses
  Windows,
  System.Console
  ;

function ExecuteConsoleOutput(const ACommand, AParameters: String): Boolean;
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
    Console.WriteColor('CreatePipe', [TConsoleColor.Red]);

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

      Console.WriteColor('CreateProcess', [TConsoleColor.Red]);
      repeat
        dRunning  := WaitForSingleObject(piProcess.hProcess, 100);
        SleepEx(10, True);
        repeat
          dRead := 0;
          ReadFile(hRead, pBuffer[0], CReadBuffer, dRead, nil);
          pBuffer[dRead] := #0;

          OemToAnsi(pBuffer, pBuffer);
          Console.WriteLine(String(pBuffer));
        until (dRead < CReadBuffer);
        Console.WriteColor('end repeat 01', [TConsoleColor.Red]);
      until (dRunning <> WAIT_TIMEOUT);
      Console.WriteColor('end repeat 02', [TConsoleColor.Red]);
      CloseHandle(piProcess.hProcess);
      CloseHandle(piProcess.hThread);
    end;
    CloseHandle(hRead);
    CloseHandle(hWrite);
  end;
  Console.WriteColor('close all handle', [TConsoleColor.Red]);
end;


end.
