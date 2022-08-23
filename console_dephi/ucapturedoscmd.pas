﻿unit ucapturedoscmd;

interface

procedure CaptureConsoleOutput(const ACommand, AParameters: String);

implementation

uses
  Windows,
  System.Console
  ;

procedure CaptureConsoleOutput(const ACommand, AParameters: String);
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
  saSecurity.nLength              := SizeOf(TSecurityAttributes);
  saSecurity.bInheritHandle       := True;
  saSecurity.lpSecurityDescriptor := nil;

  if CreatePipe(hRead, hWrite, @saSecurity, 0) then
  begin
    FillChar(suiStartup, SizeOf(TStartupInfo), #0);
    suiStartup.cb          := SizeOf(TStartupInfo);
    suiStartup.hStdInput   := hRead;
    suiStartup.hStdOutput  := hWrite;
    suiStartup.hStdError   := hWrite;
    suiStartup.dwFlags     := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    suiStartup.wShowWindow := SW_HIDE;

    if CreateProcess(nil, PChar(ACommand + ' ' + AParameters), @saSecurity, @saSecurity, True, NORMAL_PRIORITY_CLASS, nil, nil, suiStartup, piProcess) then
    begin
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
      until (dRunning <> WAIT_TIMEOUT);
      CloseHandle(piProcess.hProcess);
      CloseHandle(piProcess.hThread);
    end;
    CloseHandle(hRead);
    CloseHandle(hWrite);
  end;
end;


end.
