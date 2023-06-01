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
    Console.WriteColorLine('Digit "getdir" to get directory for repository. ' + DirectoryRepository , [TConsoleColor.Green]);
    Console.WriteColorLine('Digit "compile [debug|release|meleak] [system number|system name] to compile system', [TConsoleColor.Green]);
    Console.WriteColorLine('Digit "menu" to options', [TConsoleColor.Green]);
  end;

  procedure PrintOptionsSystemMenu;
  begin
    Console.WriteColorLine('***********************************************************************', [TConsoleColor.Red]);
    Console.WriteColorLine('* Set compiler type                                                   *', [TConsoleColor.Green]);
    Console.WriteColorLine('* DEBUG or RELEASE or MEMLEAK                                         *', [TConsoleColor.Green]);
    Console.WriteColorLine('***********************************************************************', [TConsoleColor.Red]);
    Console.WriteColorLine('* You can to set directory for repository                             *', [TConsoleColor.Green]);
    Console.WriteColorLine('* Ex: setdir c:\apdata_x64                                            *', [TConsoleColor.Green]);
    Console.WriteColorLine('***********************************************************************', [TConsoleColor.Red]);
    Console.WriteColorLine('* You can type the systems in sequence to compile                     *', [TConsoleColor.Green]);
    Console.WriteColorLine('* Ex: compile debug 01, 02, 03, 10, 11 or compile release 01,02,03,04 *', [TConsoleColor.Green]);
    Console.WriteColorLine('* Ex: compile debug ApServer32, ApLoadBalancer32                      *', [TConsoleColor.Green]);
    Console.WriteColorLine('*=====================================================================*', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 01 - ApServer [ApServer32]                                          *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 02 - ApServer [ApServer64]                                          *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 03 - ApTools                                                        *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 04 - ApWebDispatcher [Only Copy Jenkins]                            *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 05 - ApLoadBalancer [ApLoadBalancer32]                              *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 06 - ApLoadBalancer [ApLoadBalancer64]                              *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 07 - ApESocialMsg                                                   *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 08 - ApScripter [ApScripter32]                                      *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 09 - ApScripter [ApScripter64]                                      *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 10 - ApIntegrationServer [ApIntegrationServer32]                    *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 11 - ApIntegrationServer [ApIntegrationServer64]                    *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 12 - ApIntegrationInterface [ApIntegrationInterface32]              *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 13 - ApIntegrationInterface [ApIntegrationInterface64]              *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 14 - ApManager                                                      *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 15 - ApUsers                                                        *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 16 - Generate Messages [compile messages]                           *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 17 - Build Sass [compile sass]                                      *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 18 - Pack Integracao [pintegration32]                               *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 19 - Pack Integracao [pintegration64]                               *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 20 - All                                                            *', [TConsoleColor.Blue]);
    Console.WriteColorLine('***********************************************************************', [TConsoleColor.Red]);
  end;

  procedure ExecuteCommand(const ASystemId: Integer; const ASubCommand: String = '');
  begin
    case ASystemId of
      01 : Console.WriteColorLine('* 01 - Compiling ApServer [ApServer32]                                          *', [TConsoleColor.Yellow]);
      02 : Console.WriteColorLine('* 02 - Compiling ApServer [ApServer64]                                          *', [TConsoleColor.Yellow]);
      03 : Console.WriteColorLine('* 03 - Compiling ApTools                                                        *', [TConsoleColor.Yellow]);
      04 : Console.WriteColorLine('* 04 - Compiling ApWebDispatcher [Only Copy Jenkins]                            *', [TConsoleColor.Yellow]);
      05 : Console.WriteColorLine('* 05 - Compiling ApLoadBalancer [ApLoadBalancer32]                              *', [TConsoleColor.Yellow]);
      06 : Console.WriteColorLine('* 06 - Compiling ApLoadBalancer [ApLoadBalancer64]                              *', [TConsoleColor.Yellow]);
      07 : Console.WriteColorLine('* 07 - Compiling ApESocialMsg                                                   *', [TConsoleColor.Yellow]);
      08 : Console.WriteColorLine('* 08 - Compiling ApScripter [ApScripter32]                                      *', [TConsoleColor.Yellow]);
      09 : Console.WriteColorLine('* 09 - Compiling ApScripter [ApScripter64]                                      *', [TConsoleColor.Yellow]);
      10 : Console.WriteColorLine('* 10 - Compiling ApIntegrationServer [ApIntegrationServer32]                    *', [TConsoleColor.Yellow]);
      11 : Console.WriteColorLine('* 11 - Compiling ApIntegrationServer [ApIntegrationServer64]                    *', [TConsoleColor.Yellow]);
      12 : Console.WriteColorLine('* 12 - Compiling ApIntegrationInterface [ApIntegrationInterface32]              *', [TConsoleColor.Yellow]);
      13 : Console.WriteColorLine('* 13 - Compiling ApIntegrationInterface [ApIntegrationInterface64]              *', [TConsoleColor.Yellow]);
      14 : Console.WriteColorLine('* 14 - Compiling ApManager                                                      *', [TConsoleColor.Yellow]);
      15 : Console.WriteColorLine('* 15 - Compiling ApUsers                                                        *', [TConsoleColor.Yellow]);
      16 : Console.WriteColorLine('* 16 - Compiling Generate Messages [compile messages]                           *', [TConsoleColor.Yellow]);
      17 : Console.WriteColorLine('* 17 - Compiling Build Sass [compile sass]                                      *', [TConsoleColor.Yellow]);
      18 : Console.WriteColorLine('* 18 - Compiling Pack Integracao [pintegration32]                               *', [TConsoleColor.Yellow]);
      19 : Console.WriteColorLine('* 19 - Compiling Pack Integracao [pintegration64]                               *', [TConsoleColor.Yellow]);
      20 : Console.WriteColorLine('* 20 - All                                                                      *', [TConsoleColor.Yellow]);
    end;

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
      18 :
        begin
          ExecuteInternal(Format('%s\Aplicacoes\ApServer\Source\buildServer.bat', [DirectoryRepository]), Format('%s Win32', [ASubCommand]));
          ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationServer\Source\buildIntegrationServer.bat', [DirectoryRepository]), Format('%s Win32', [ASubCommand]));
          ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationInterface\Source\buildIntegrationInterface.bat', [DirectoryRepository]), Format('%s Win32', [ASubCommand]));
        end;
      19 :
        begin
          ExecuteInternal(Format('%s\Aplicacoes\ApServer\Source\buildServer.bat', [DirectoryRepository]), Format('%s Win64', [ASubCommand]));
          ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationServer\Source\buildIntegrationServer.bat', [DirectoryRepository]), Format('%s Win64', [ASubCommand]));
          ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationInterface\Source\buildIntegrationInterface.bat', [DirectoryRepository]), Format('%s Win64', [ASubCommand]));
        end;
      20 :
        begin
          ExecuteInternal(Format('%s\GenerateMessages.bat', [DirectoryRepository]));
          ExecuteInternal(Format('%s\Aplicacoes\ApWebDispatcher\Site\buildSass.bat', [DirectoryRepository]));
          ExecuteInternal(Format('%s\Aplicacoes\ApServer\Source\buildServer.bat', [DirectoryRepository]), Format('%s Win32', [ASubCommand]));
          ExecuteInternal(Format('%s\Aplicacoes\ApServer\Source\buildServer.bat', [DirectoryRepository]), Format('%s Win64', [ASubCommand]));
          ExecuteInternal(Format('%s\Aplicacoes\ApTools\Source\buildTools.bat', [DirectoryRepository]), ASubCommand);
          ExecuteInternal(Format('%s\Aplicacoes\ApWebDispatcher\Source\buildWebDispatcher.bat', [DirectoryRepository]), ASubCommand);
          ExecuteInternal(Format('%s\Aplicacoes\ApLoadBalancer\Source\buildBalancer.bat', [DirectoryRepository]), Format('%s Win32', [ASubCommand]));
          ExecuteInternal(Format('%s\Aplicacoes\ApLoadBalancer\Source\buildBalancer.bat', [DirectoryRepository]), Format('%s Win64', [ASubCommand]));
          ExecuteInternal(Format('%s\Aplicacoes\ApESocialMsg\Source\buildESocialMsg.bat', [DirectoryRepository]), ASubCommand);
          ExecuteInternal(Format('%s\Aplicacoes\ApScripter\Source\buildScripter.bat', [DirectoryRepository]), Format('%s Win32', [ASubCommand]));
          ExecuteInternal(Format('%s\Aplicacoes\ApScripter\Source\buildScripter.bat', [DirectoryRepository]), Format('%s Win64', [ASubCommand]));
          ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationServer\Source\buildIntegrationServer.bat', [DirectoryRepository]), Format('%s Win32', [ASubCommand]));
          ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationServer\Source\buildIntegrationServer.bat', [DirectoryRepository]), Format('%s Win64', [ASubCommand]));
          ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationInterface\Source\buildIntegrationInterface.bat', [DirectoryRepository]), Format('%s Win32', [ASubCommand]));
          ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationInterface\Source\buildIntegrationInterface.bat', [DirectoryRepository]), Format('%s Win64', [ASubCommand]));
          ExecuteInternal(Format('%s\Aplicacoes\ApManager\Source\buildManager.bat', [DirectoryRepository]), ASubCommand);
          ExecuteInternal(Format('%s\Aplicacoes\ApUsers\Source\buildUsers.bat', [DirectoryRepository]), ASubCommand);
        end
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
      else if (AnsiSameText(ANameSystem, 'pintegration32')) then
        Result := 18
      else  if (AnsiSameText(ANameSystem, 'pintegration64')) then
        Result := 19
      else if (AnsiSameText(ANameSystem, 'All')) then
        Result := 20
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
      InputArray := Console.ReadLine.Replace(',', ', ').Split([' ', sLineBreak]);
      Finalize(SystemArray);

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
        if Length(InputArray) = 3 then
          SystemArray := InputArray[2].Trim.Split([','])
        else if Length(InputArray) > 3 then
        begin
          for c := 2 to High(InputArray) do
          begin
            if (InputArray[c] <> '') then
            begin
              SetLength(SystemArray, Length(SystemArray) + 1);
              SystemArray[High(SystemArray)] := InputArray[c].Replace(',','');
            end;
          end;
        end;
      end;

      case IndexStr(Command, ['EXIT', 'BREAK', 'CLEAR', 'CLS', 'MENU', 'SETDIR', 'COMPILE', 'GETDIR']) of
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
        7 :
          begin
            Console.WriteColorLine(Format('> current directory %s', [DirectoryRepository]), [TConsoleColor.DarkGreen]);
          end
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
