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
  ucapturedoscmd in 'ucapturedoscmd.pas',
  unbuildoptions in 'unbuildoptions.pas';

var
  Command             : String;
  CommandParameters   : String;
  InputArray          : TArray<string>;
  DirectoryRepository : String;

  procedure PrintMenuBuild;
  begin
    Console.WriteColorLine('First parameter must be the repository directory. Ex: cmdd.exe c:\apdata_x64', [TConsoleColor.Green]);
    Console.WriteColorLine('Digit "exit" or "break" to finalize application', [TConsoleColor.Green]);
    Console.WriteColorLine('Digit "setdir" to set directory for repository. Ex: setdir c:\apdata_x64', [TConsoleColor.Green]);
    Console.WriteColorLine('Digit "menu" to options', [TConsoleColor.Green]);
  end;

begin
  PrintMenuBuild;

  if (ParamStr(1) <> '') then
    DirectoryRepository := ParamStr(1)
  else
    DirectoryRepository := 'c:\apdata_x64';

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

      case IndexStr(Command, ['EXIT', 'BREAK', 'CLEAR', 'CLS', 'MENU', 'SETDIR']) of
        0    : Exit;
        1    : Break;
        2..3 :
          begin
            Console.Clear;
            PrintMenuBuild;
          end;
        4    :
          begin
            MenuBuildOptions(DirectoryRepository);
            Console.Clear;
            PrintMenuBuild;
          end;
        5    :
          begin
            if (CommandParameters = '') then
              DirectoryRepository := 'c:\apdata_x64'
            else
              DirectoryRepository := CommandParameters;
          end;
        else
        begin
          if not (ExecuteConsoleOutput(Command, CommandParameters)) then
          begin
            Console.WriteColor('Command not executed', [TConsoleColor.Red]);
            Console.WriteLine();
          end;
        end;
      end;
    end;
  except
    on E: Exception do
      Console.WriteLine(E.ClassName + ': ' + E.Message);
  end;
end.
