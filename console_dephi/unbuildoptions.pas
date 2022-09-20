unit unbuildoptions;

interface

procedure MenuBuildOptions(const APathDir: String);

implementation

uses
  System.SysUtils,
  System.Console,
  System.Classes,
  System.StrUtils,
  System.RTTI,
  ucompile
  ;

procedure PrintMenuBuild;
begin
  Console.WriteColorLine('***********************************************************************', [TConsoleColor.Red]);
  Console.WriteColorLine('* 01 - DEBUG                                                          *', [TConsoleColor.Blue]);
  Console.WriteColorLine('* 02 - RELEASE                                                        *', [TConsoleColor.Blue]);
  Console.WriteColorLine('* 03 - MEMLEAK                                                        *', [TConsoleColor.Blue]);
  Console.WriteColorLine('* 04 - Back                                                           *', [TConsoleColor.Blue]);
  Console.WriteColorLine('***********************************************************************', [TConsoleColor.Red]);
end;

procedure MenuBuildOptions(const APathDir: String);
var
  Command           : String;
  CommandParameters : String;
  InputArray        : TArray<String>;
begin
  Console.Clear;
  PrintMenuBuild;
  try
    while True do
    begin
      Console.Write('buildoptions>');
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
       01..03 : MenuCompilex64(APathDir, StrToIntDef(Command, 0)) ;
       04     : Break;
      end;
      Console.Clear;
      PrintMenuBuild;
    end;
  except
    on E: Exception do
      Console.WriteLine(E.ClassName + ': ' + E.Message);
  end;
end;

end.
