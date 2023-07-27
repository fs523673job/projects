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

  procedure ExecuteInternal(const ACommand: String; const AParameters: String; const ASystemName: String);
  begin
    if not ExecuteConsoleOutputEx(ACommand, Aparameters, ASystemName) then
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
      01 : ExecuteInternal(Format('%s\Aplicacoes\ApServer\Source\buildServer.bat', [DirectoryRepository]), Format('%s Win32', [ASubCommand]), 'ApServer');
      02 : ExecuteInternal(Format('%s\Aplicacoes\ApServer\Source\buildServer.bat', [DirectoryRepository]), Format('%s Win64', [ASubCommand]), 'ApServer');
      03 : ExecuteInternal(Format('%s\Aplicacoes\ApTools\Source\buildTools.bat', [DirectoryRepository]), ASubCommand, 'ApTools');
      04 : ExecuteInternal(Format('%s\Aplicacoes\ApWebDispatcher\Source\buildWebDispatcher.bat', [DirectoryRepository]), ASubCommand, 'ApWebDispatcher');
      05 : ExecuteInternal(Format('%s\Aplicacoes\ApLoadBalancer\Source\buildBalancer.bat', [DirectoryRepository]), Format('%s Win32', [ASubCommand]), 'ApLoadBalancer');
      06 : ExecuteInternal(Format('%s\Aplicacoes\ApLoadBalancer\Source\buildBalancer.bat', [DirectoryRepository]), Format('%s Win64', [ASubCommand]), 'ApLoadBalancer');
      07 : ExecuteInternal(Format('%s\Aplicacoes\ApESocialMsg\Source\buildESocialMsg.bat', [DirectoryRepository]), ASubCommand, 'ApESocialMsg');
      08 : ExecuteInternal(Format('%s\Aplicacoes\ApScripter\Source\buildScripter.bat', [DirectoryRepository]), Format('%s Win32', [ASubCommand]), 'ApScripter');
      09 : ExecuteInternal(Format('%s\Aplicacoes\ApScripter\Source\buildScripter.bat', [DirectoryRepository]), Format('%s Win64', [ASubCommand]), 'ApScripter');
      10 : ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationServer\Source\buildIntegrationServer.bat', [DirectoryRepository]), Format('%s Win32', [ASubCommand]), 'ApIntegrationServer');
      11 : ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationServer\Source\buildIntegrationServer.bat', [DirectoryRepository]), Format('%s Win64', [ASubCommand]), 'ApIntegrationServer');
      12 : ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationInterface\Source\buildIntegrationInterface.bat', [DirectoryRepository]), Format('%s Win32', [ASubCommand]), 'ApIntegrationInterface');
      13 : ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationInterface\Source\buildIntegrationInterface.bat', [DirectoryRepository]), Format('%s Win64', [ASubCommand]), 'ApIntegrationInterface');
      14 : ExecuteInternal(Format('%s\Aplicacoes\ApManager\Source\buildManager.bat', [DirectoryRepository]), ASubCommand, 'ApManager');
      15 : ExecuteInternal(Format('%s\Aplicacoes\ApUsers\Source\buildUsers.bat', [DirectoryRepository]), ASubCommand, 'ApUsers');
      16 : ExecuteInternal(Format('%s\GenerateMessages.bat', [DirectoryRepository]), '', 'GenerateMessages');
      17 : ExecuteInternal(Format('%s\Aplicacoes\ApWebDispatcher\Site\buildSass.bat', [DirectoryRepository]), '', 'GenerateSass');
      18 :
        begin
          ExecuteCommand(01, ASubCommand);
          ExecuteCommand(10, ASubCommand);
          ExecuteCommand(12, ASubCommand);
        end;
      19 :
        begin
          ExecuteCommand(02, ASubCommand);
          ExecuteCommand(11, ASubCommand);
          ExecuteCommand(13, ASubCommand);
        end;
      20 :
        begin
          ExecuteCommand(16, ASubCommand);
          ExecuteCommand(17, ASubCommand);

          ExecuteCommand(01, ASubCommand);
          ExecuteCommand(02, ASubCommand);
          ExecuteCommand(03, ASubCommand);
          ExecuteCommand(04, ASubCommand);
          ExecuteCommand(05, ASubCommand);
          ExecuteCommand(06, ASubCommand);
          ExecuteCommand(07, ASubCommand);
          ExecuteCommand(08, ASubCommand);
          ExecuteCommand(09, ASubCommand);
          ExecuteCommand(10, ASubCommand);
          ExecuteCommand(11, ASubCommand);
          ExecuteCommand(12, ASubCommand);
          ExecuteCommand(13, ASubCommand);
          ExecuteCommand(14, ASubCommand);
          ExecuteCommand(15, ASubCommand);
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
          if not (ExecuteConsoleOutput(Command, SubCommand, 'Native Command' )) then
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
