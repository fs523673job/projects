﻿program cmdd;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Console,
  System.Classes,
  System.StrUtils,
  System.RTTI,
  ucompile in 'ucompile.pas',
  ucapturedoscmd in 'ucapturedoscmd.pas';

var
  Command           : String;
  CommandParameters : String;
  InputArray        : TArray<string>;

begin
  Console.WriteLine('Test Console - Execute Bat Compilations');
  Console.WriteLine('Digit "exit" or "break" to finalize application');
  try
    while True do
    begin
      Console.Write('>');
      InputArray := Console.ReadLine.Split([' ', sLineBreak]);

      if Length(InputArray) >= 1 then
        Command := InputArray[0].ToUpper
      else
        Command := String.Empty;

      if Length(InputArray) >= 2 then
        CommandParameters := InputArray[1].ToUpper
      else
        CommandParameters := String.Empty;

      case IndexStr(Command, ['EXIT', 'BREAK', 'CLEAR', 'CLS', 'COMPILE']) of
        0    : Exit;
        1    : Break;
        2..3 : Console.Clear;
        4    : MenuCompilex64;
        else
          CaptureConsoleOutput(Command, CommandParameters);
      end;
    end;
  except
    on E: Exception do
      Console.WriteLine(E.ClassName + ': ' + E.Message);
  end;
end.
