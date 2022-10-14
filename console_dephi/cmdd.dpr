program cmdd;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Console,
  System.Classes,
  System.StrUtils,
  System.RTTI,
  ucapturedoscmd in 'ucapturedoscmd.pas'
  ;

var
  c                   : Integer;
  Command             : String;
  SubCommand          : String;
  SystemType          : String;
  InputArray          : TArray<string>;
  SystemArray         : TArray<string>;
  DirectoryRepository : String;

  procedure ExecuteInternal(const ACommand: String; const AParameters: String = '');
  begin
    if not ExecuteConsoleOutputEx(ACommand, Aparameters) then
      Console.WriteColorLine('Command not executed', [TConsoleColor.Red])
    else
    begin
      Console.Write('compile>');
      Console.WriteColor('executed!', [TConsoleColor.Green]);
      Console.WriteLine('');
    end;
  end;

  procedure PrintInstructionMenu;
  begin
    Console.WriteColorLine('First parameter must be the repository directory. Ex: cmdd.exe c:\apdata_x64', [TConsoleColor.Green]);
    Console.WriteColorLine('Digit "exit" or "break" to finalize application', [TConsoleColor.Green]);
    Console.WriteColorLine('Digit "setdir" to set directory for repository. Ex: setdir c:\apdata_x64', [TConsoleColor.Green]);
    Console.WriteColorLine('Digit "compile [debug|release|meleak] [system number|system name] to compile system', [TConsoleColor.Green]);
    Console.WriteColorLine('Digit "menu" to options', [TConsoleColor.Green]);
  end;

  procedure PrintOptionsSystemMenu;
  begin
    Console.WriteColorLine('***********************************************************************', [TConsoleColor.Red]);
    Console.WriteColorLine('* Set compiler type                                                   *', [TConsoleColor.Green]);
    Console.WriteColorLine('* DEBUG or RELEASE or MEMLEAK                                         *', [TConsoleColor.Green]);
    Console.WriteColorLine('***********************************************************************', [TConsoleColor.Red]);
    Console.WriteColorLine('* You can type the systems in sequence to compile                     *', [TConsoleColor.Green]);
    Console.WriteColorLine('* Ex: compile debug 01,02,03,10,11 or compile release 01,02,03,04     *', [TConsoleColor.Green]);
    Console.WriteColorLine('*=====================================================================*', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 01 - ApServer [32]                                                  *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 02 - ApServer [64]                                                  *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 03 - ApTools                                                        *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 04 - ApWebDispatcher [Only Copy Jenkins]                            *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 05 - ApLoadBalancer [32]                                            *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 06 - ApLoadBalancer [64]                                            *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 07 - ApESocialMsg                                                   *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 08 - ApScripter [32]                                                *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 09 - ApScripter [64]                                                *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 10 - ApIntegrationServer [32]                                       *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 11 - ApIntegrationServer [64]                                       *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 12 - ApIntegrationInterface [32]                                    *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 13 - ApIntegrationInterface [64]                                    *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 14 - ApManager                                                      *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 15 - ApUsers                                                        *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 16 - Generate Messages                                              *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 17 - Build Sass                                                     *', [TConsoleColor.Blue]);
    Console.WriteColorLine('***********************************************************************', [TConsoleColor.Red]);
  end;

  procedure ExecuteCommand(const ASystemId: Integer; const ASubCommand: String = '');
  begin
    case ASystemId of
      01 : ExecuteInternal(Format('%s\Aplicacoes\ApServer\Source\buildServer.bat', [DirectoryRepository]), Format('%s Win32', [ASubCommand]));
      02 : ExecuteInternal(Format('%s\Aplicacoes\ApServer\Source\buildServer.bat', [DirectoryRepository]), Format('%s Win64', [ASubCommand]));
      03 : ExecuteInternal(Format('%s\Aplicacoes\ApTools\Source\buildTools.bat', [DirectoryRepository]), ASubCommand);
      04 : ExecuteInternal(Format('%s\Aplicacoes\ApWebDispatcher\Source\buildWebDispatcher.bat', [DirectoryRepository]), ASubCommand);
      05 : ExecuteInternal(Format('%s\Aplicacoes\ApLoadBalancer\Source\buildBalancer.bat', [DirectoryRepository]), Format('%s Win32', [ASubCommand]));
      06 : ExecuteInternal(Format('%s\Aplicacoes\ApLoadBalancer\Source\buildBalancer.bat', [DirectoryRepository]), Format('%s Win64', [ASubCommand]));
      07 : ExecuteInternal(Format('%s\Aplicacoes\ApESocialMsg\Source\buildESocialMsg.bat', [DirectoryRepository]), ASubCommand);
      08 : ExecuteInternal(Format('%s\Aplicacoes\ApScripter\Source\buildScripter.bat', [DirectoryRepository]), Format('%s Win32', [ASubCommand]));
      09 : ExecuteInternal(Format('%s\Aplicacoes\ApScripter\Source\buildScripter.bat', [DirectoryRepository]), Format('%s Win64', [ASubCommand]));
      10 : ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationServer\Source\buildIntegrationServer.bat', [DirectoryRepository]), Format('%s Win32', [ASubCommand]));
      11 : ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationServer\Source\buildIntegrationServer.bat', [DirectoryRepository]), Format('%s Win64', [ASubCommand]));
      12 : ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationInterface\Source\buildIntegrationInterface.bat', [DirectoryRepository]), Format('%s Win32', [ASubCommand]));
      13 : ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationInterface\Source\buildIntegrationInterface.bat', [DirectoryRepository]), Format('%s Win64', [ASubCommand]));
      14 : ExecuteInternal(Format('%s\Aplicacoes\ApManager\Source\buildManager.bat', [DirectoryRepository]), ASubCommand);
      15 : ExecuteInternal(Format('%s\Aplicacoes\ApUsers\Source\buildUsers.bat', [DirectoryRepository]), ASubCommand);
      16 : ExecuteInternal(Format('%s\GenerateMessages.bat', [DirectoryRepository]));
      17 : ExecuteInternal(Format('%s\Aplicacoes\ApWebDispatcher\Site\buildSass.bat', [DirectoryRepository]));
      else
        Console.WriteColor('Command not executed', [TConsoleColor.Red]);
    end;
  end;

  function ConvertNameSystem(const ANameSystem: String): Integer;
  begin
    Result := StrToIntDef(ANameSystem, 0);
    if (Result = 0) then
    begin
      if (AnsiSameText(ANameSystem, 'ApServer32') or AnsiSameText(ANameSystem, 'ApServer')) then
        Result := 01
      else if (AnsiSameText(ANameSystem, 'ApServer64')) then
        Result := 02
      else if (AnsiSameText(ANameSystem, 'ApTools')) then
        Result := 03
      else if (AnsiSameText(ANameSystem, 'ApWebDispatcher')) then
        Result := 04
      else if (AnsiSameText(ANameSystem, 'ApLoadBalancer32') or AnsiSameText(ANameSystem, 'ApLoadBalancer')) then
        Result := 05
      else if (AnsiSameText(ANameSystem, 'ApLoadBalancer64')) then
        Result := 06
      else if (AnsiSameText(ANameSystem, 'ApESocialMsg')) then
        Result := 07
      else if (AnsiSameText(ANameSystem, 'ApScripter32') or AnsiSameText(ANameSystem, 'ApScripter')) then
        Result := 08
      else if (AnsiSameText(ANameSystem, 'ApScripter64')) then
        Result := 09
      else if (AnsiSameText(ANameSystem, 'ApIntegrationServer32') or AnsiSameText(ANameSystem, 'ApIntegrationServer')) then
        Result := 10
      else if (AnsiSameText(ANameSystem, 'ApIntegrationServer64')) then
        Result := 11
      else if (AnsiSameText(ANameSystem, 'ApIntegrationInterface32') or AnsiSameText(ANameSystem, 'ApIntegrationInterface')) then
        Result := 12
      else if (AnsiSameText(ANameSystem, 'ApIntegrationInterface64')) then
        Result := 13
      else if (AnsiSameText(ANameSystem, 'ApManager')) then
        Result := 14
      else if (AnsiSameText(ANameSystem, 'ApUsers')) then
        Result := 15
      else if (AnsiSameText(ANameSystem, 'Messages')) then
        Result := 16
      else if (AnsiSameText(ANameSystem, 'Sass')) then
        Result := 17
    end;
  end;

begin
  PrintInstructionMenu;

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
        SubCommand := InputArray[1].ToUpper
      else
        SubCommand := String.Empty;

      if AnsiSameText(Command, 'compile') then
      begin
        if Length(InputArray) >= 3 then
          SystemArray := InputArray[2].Split([',']);
      end;

      case IndexStr(Command, ['EXIT', 'BREAK', 'CLEAR', 'CLS', 'MENU', 'SETDIR', 'COMPILE']) of
        0    : Exit;
        1    : Break;
        2..3 :
          begin
            Console.Clear;
            PrintInstructionMenu;
          end;
        4    :
          begin
            Console.Clear;
            PrintOptionsSystemMenu;
          end;
        5    :
          begin
            if (SubCommand = '') then
              DirectoryRepository := 'c:\apdata_x64'
            else
              DirectoryRepository := SubCommand;
          end;
        6    :
          begin
            if (Length(SystemArray) > 0) then
            begin
              for c := 0 to Length(SystemArray) - 1 do
              begin
                SystemType := SystemArray[c].ToUpper;
                Console.WriteColorLine(Format('compile> compiling %.2d/%.2d', [c + 1, Length(SystemArray)]), [TConsoleColor.DarkGreen]);
                ExecuteCommand(ConvertNameSystem(SystemType), SubCommand);
              end;
            end
            else
              ExecuteCommand(ConvertNameSystem(SubCommand));
          end;
        else
        begin
          if not (ExecuteConsoleOutput(Command, SubCommand)) then
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
