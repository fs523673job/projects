unit ucompile;

interface

procedure MenuCompilex64(const APathDir: String; const ABuildOption: Integer);

implementation

uses
  System.SysUtils,
  System.Console,
  System.Classes,
  System.StrUtils,
  System.RTTI,
  ucapturedoscmd
  ;

procedure PrintMenu;
begin
  Console.WriteColorLine('***********************************************************************', [TConsoleColor.Red]);
  Console.WriteColorLine('* You can type the systems in sequence to compile                     *', [TConsoleColor.Green]);
  Console.WriteColorLine('* Ex: 01,02,03,10,11                                                  *', [TConsoleColor.Green]);
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
  Console.WriteColorLine('* 17 - All                                                            *', [TConsoleColor.Blue]);
  Console.WriteColorLine('* 18 - Back                                                           *', [TConsoleColor.Blue]);
  Console.WriteColorLine('***********************************************************************', [TConsoleColor.Red]);
end;

procedure MenuCompilex64(const APathDir: String; const ABuildOption: Integer);
const
  BACK = 18;
var
  Command    : String;
  InputArray : TArray<String>;
  c          : Integer;
  IsBreak    : Boolean;

  function BuildOptionsToStr(const ABOP: Integer): String;
  begin
    case ABOP of
      1 : Result := 'DEBUG';
      2 : Result := 'RELEASE';
      3 : Result := 'MEMLEAK';
      else
        Result := 'DEBUG'
    end;
  end;

  procedure ExecuteInternal(const ACommand, AParameters: String);
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

  procedure ExecuteCommand(const ACommand: Integer; var AIsBreak: Boolean);
  begin
    case StrToIntDef(Command, 0) of
      01 : ExecuteInternal(Format('%s\Aplicacoes\ApServer\Source\buildServer.bat', [APathDir]), Format('%s Win32', [BuildOptionsToStr(ABuildOption)]));
      02 : ExecuteInternal(Format('%s\Aplicacoes\ApServer\Source\buildServer.bat', [APathDir]), Format('%s Win64', [BuildOptionsToStr(ABuildOption)]));
      03 : ExecuteInternal(Format('%s\Aplicacoes\ApTools\Source\buildTools.bat', [APathDir]), BuildOptionsToStr(ABuildOption));
      04 : ExecuteInternal(Format('%s\Aplicacoes\ApWebDispatcher\Source\buildWebDispatcher.bat', [APathDir]), BuildOptionsToStr(ABuildOption));
      05 : ExecuteInternal(Format('%s\Aplicacoes\ApLoadBalancer\Source\buildBalancer.bat', [APathDir]), Format('%s Win32', [BuildOptionsToStr(ABuildOption)]));
      06 : ExecuteInternal(Format('%s\Aplicacoes\ApLoadBalancer\Source\buildBalancer.bat', [APathDir]), Format('%s Win64', [BuildOptionsToStr(ABuildOption)]));
      07 : ExecuteInternal(Format('%s\Aplicacoes\ApESocialMsg\Source\buildESocialMsg.bat', [APathDir]), BuildOptionsToStr(ABuildOption));
      08 : ExecuteInternal(Format('%s\Aplicacoes\ApScripter\Source\buildScripter.bat', [APathDir]), Format('%s Win32', [BuildOptionsToStr(ABuildOption)]));
      09 : ExecuteInternal(Format('%s\Aplicacoes\ApScripter\Source\buildScripter.bat', [APathDir]), Format('%s Win64', [BuildOptionsToStr(ABuildOption)]));
      10 : ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationServer\Source\buildIntegrationServer.bat', [APathDir]), Format('%s Win32', [BuildOptionsToStr(ABuildOption)]));
      11 : ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationServer\Source\buildIntegrationServer.bat', [APathDir]), Format('%s Win64', [BuildOptionsToStr(ABuildOption)]));
      12 : ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationInterface\Source\buildIntegrationInterface.bat', [APathDir]), Format('%s Win32', [BuildOptionsToStr(ABuildOption)]));
      13 : ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationInterface\Source\buildIntegrationInterface.bat', [APathDir]), Format('%s Win64', [BuildOptionsToStr(ABuildOption)]));
      14 : ExecuteInternal(Format('%s\Aplicacoes\ApManager\Source\buildManager.bat', [APathDir]), BuildOptionsToStr(ABuildOption));
      15 : ExecuteInternal(Format('%s\Aplicacoes\ApUsers\Source\buildUsers.bat', [APathDir]), BuildOptionsToStr(ABuildOption));
      16 : ExecuteInternal(Format('%s\GenerateMessages.bat', [APathDir]), BuildOptionsToStr(ABuildOption));
      17 : Console.WriteColor('not impelmented yet!', [TConsoleColor.Green]);
      18 : AIsBreak := True;
      else
      begin
        Console.Clear;
        PrintMenu;
      end;
    end;
  end;

begin
  Console.Clear;
  PrintMenu;
  try
    IsBreak := False;

    while not (IsBreak) do
    begin
      Console.Write('compile>');
      InputArray := Console.ReadLine.Split([',', sLineBreak]);

      for c := 0 to Length(InputArray) - 1 do
      begin
        Command := InputArray[c].ToUpper;
        Console.WriteColorLine(Format('compile> compiling %.2d/%.2d', [c + 1, Length(InputArray)]), [TConsoleColor.DarkGreen]);
        ExecuteCommand(StrToIntDef(Command, 0), IsBreak);
      end;
    end;
  except
    on E: Exception do
      Console.WriteLine(E.ClassName + ': ' + E.Message);
  end;
end;

end.

