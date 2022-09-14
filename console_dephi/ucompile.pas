﻿unit ucompile;

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
  Console.WriteColorLine('* 16 - All                                                            *', [TConsoleColor.Blue]);
  Console.WriteColorLine('* 17 - Back                                                           *', [TConsoleColor.Blue]);
  Console.WriteColorLine('***********************************************************************', [TConsoleColor.Red]);
end;

procedure MenuCompilex64(const APathDir: String; const ABuildOption: Integer);
var
  Command           : String;
  CommandParameters : String;
  InputArray        : TArray<String>;

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
    Console.Clear;
    if not ExecuteConsoleOutput(ACommand, Aparameters) then
      Console.WriteColorLine('Command not executed', [TConsoleColor.Red])
    else
    begin
      Console.Write('compile>');
      Console.WriteColor('executed!', [TConsoleColor.Green]);
      Console.WriteLine('');
    end;
  end;

begin
  Console.Clear;
  PrintMenu;
  try
    while True do
    begin
      Console.Write('compile>');
      InputArray := Console.ReadLine.Split([' ', sLineBreak]);

      if Length(InputArray) >= 1 then
        Command := InputArray[0].ToUpper
      else
        Command := String.Empty;

      if Length(InputArray) >= 2 then
        CommandParameters := InputArray[1].ToUpper
      else
        CommandParameters := String.Empty;

      case StrToIntDef(Command, 0) of
       01 : ExecuteInternal(Format('%s\Aplicacoes\ApServer\Source\buildServer.bat', [APathDir]), Format('%s Win32', [BuildOptionsToStr(ABuildOption)]));
       02 : ;
       03 : ExecuteInternal(Format('%s\Aplicacoes\ApTools\Source\buildTools.bat', [APathDir]), Format('%s Win32', [BuildOptionsToStr(ABuildOption)]));
       04 : ;
       05 : ;
       06 : ;
       07 : ;
       08 : ;
       09 : ;
       10 : ;
       11 : ;
       12 : ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationInterface\Source\buildIntegrationInterface.bat', [APathDir]), Format('%s Win32', [BuildOptionsToStr(ABuildOption)]));
       13 : ;
       14 : ;
       15 : ;
       16 : ;
       17 : Break;
       else
       begin
         Console.Clear;
         PrintMenu;
       end;
      end;
    end;
  except
    on E: Exception do
      Console.WriteLine(E.ClassName + ': ' + E.Message);
  end;
end;

end.

