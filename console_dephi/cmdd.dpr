program cmdd;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Console,
  System.Classes,
  System.StrUtils,
  System.RTTI,
  System.IOUtils,
  WinApi.Windows,
  ucapturedoscmd in 'ucapturedoscmd.pas'
  ;

const
  CRLF = #13#10;

var
  Command             : String;
  SubCommand          : String;
  SystemType          : String;
  InputArray          : TArray<string>;
  SystemArray         : TArray<string>;
  DirectoryRepository : String;
  NewBuild            : Boolean;
  StrList             : TStringList;
  versionDelphi       : String = 'alexandria';
  dirDelphi           : String = 'Apdata_X64';

  procedure PrintResumeComp(const AStrListMsg: TStringList);
  const
    COMPSEMERROR = 'Compilado sem erros';
    COMPERROR = 'Compilado com erros';
    ARQQUIVOSBATS : TArray<String> = ['GenerateMessages', 'GenerateSass'];
  var
    c1: Integer;
    values: String;
    findSpecial: Boolean;
  begin
    Console.WriteLine(StringOfChar('*', 80));
    Console.WriteColor('RESUMO DA COMPILACAO', [TConsoleColor.Yellow]);
    Console.WriteColor('', [TConsoleColor.White]);
    Console.WriteLine();

    for c1 := 0 to AStrListMsg.Count - 1 do
    begin
      if (AStrListMsg[c1] <> '') then
      begin
        if (Pos(COMPSEMERROR, AStrListMsg[c1]) > 0)  then
          Console.WriteColorLine(AStrListMsg[c1], [TConsoleColor.Blue])
        else if (Pos(COMPERROR, AStrListMsg[c1]) > 0)  then
          Console.WriteColorLine(AStrListMsg[c1], [TConsoleColor.Red])
        else
        begin
          findSpecial := False;
          for values in ARQQUIVOSBATS do
          begin
            if (Pos(values.ToUpper, AStrListMsg[c1].ToUpper) > 0) then
            begin
              Console.WriteColorLine(AStrListMsg[c1], [TConsoleColor.Blue]);
              findSpecial := True;
            end;
          end;

          if not findSpecial then
            Console.WriteColorLine(AStrListMsg[c1], [TConsoleColor.Yellow]);
        end;
      end
    end;

    Console.WriteLine();
    Console.WriteColor('', [TConsoleColor.White]);
    Console.WriteLine(StringOfChar('*', 80));
  end;

  function ExecuteInternal(const ACommand: String; const AParameters: String; const ASystemName: String): String;
  begin
    //Command debugger
    Console.Write('compile>');
    Console.WriteColorLine(Format('%s %s', [ACommand, AParameters]), [TConsoleColor.Green]);

    if not ExecuteConsoleOutputEx(ACommand, Aparameters, ASystemName, Result) then
    begin
      Console.Write('compile>');
      Console.WriteColorLine('not executed!', [TConsoleColor.Red]);
      Console.WriteLine('');
    end
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
    Console.WriteColorLine('Digit "getnc" to get new compile directive. ' + DirectoryRepository , [TConsoleColor.Green]);
    Console.WriteColorLine('Digit "setnc" to set new compile directive. ' + DirectoryRepository , [TConsoleColor.Green]);
    Console.WriteColorLine('Digit "getvd" to get version delphi. ' + DirectoryRepository , [TConsoleColor.Green]);
    Console.WriteColorLine('Digit "setvd" to set version delphi [Alexandria, Tokyo]. ' + DirectoryRepository , [TConsoleColor.Green]);
    Console.WriteColorLine('Digit "menu" to options', [TConsoleColor.Green]);
  end;

  procedure PrintOptionsSystemMenu;
  begin
    Console.WriteColorLine('***********************************************************************', [TConsoleColor.Red]);
    Console.WriteColorLine('* Set compiler type                                                   *', [TConsoleColor.Green]);
    Console.WriteColorLine('* Debug or Release or Memleak                                         *', [TConsoleColor.Green]);
    Console.WriteColorLine('***********************************************************************', [TConsoleColor.Red]);
    Console.WriteColorLine('* You can to set dir or new build                                     *', [TConsoleColor.Green]);
    Console.WriteColorLine('* Ex: setdir c:\apdata_x64; gerdir; setnc true; getnc                 *', [TConsoleColor.Green]);
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
    Console.WriteColorLine('* 16 - ApADIntegratorWS                                               *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 17 - ApDeveloper                                                    *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 18 - RelogioVirtual                                                 *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 19 - TestParser                                                     *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 20 - Generate Messages [compile messages]                           *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 21 - Build Sass [compile sass]                                      *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 22 - Pack Integracao [pintegration32]                               *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 23 - Pack Integracao [pintegration64]                               *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 24 - Pack Servidores [pservers32]                                   *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 25 - Pack Servidores [pservers64]                                   *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 26 - Pack Dlls [pdlls32]                                            *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 27 - Pack Dlls [pdlls64]                                            *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 28 - Pack Clients [pclients]                                        *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 29 - Pack Tools [ptools]                                            *', [TConsoleColor.Blue]);
    Console.WriteColorLine('* 30 - All [All32, All64]                                             *', [TConsoleColor.Blue]);
    Console.WriteColorLine('***********************************************************************', [TConsoleColor.Red]);
  end;

  procedure ShowHelp;
  begin

  end;

  function ExecuteCommand(const ASystemId: Integer; const ASubCommand: String = ''): String;
  type
    DataBuild = record
      typeBuild   : String;
      dirBase     : String;
      dirApp      : String;
      arquitetura : String;
      appName     : String;
      aplicacoes  : String;
      bin         : String;
      source      : String;
      lib         : String;
      addEureka   : String;
    end;
  const
    CompileBuild: array[0..2] of DataBuild = (
                                                (
                                                  typeBuild   : 'release';
                                                  dirBase     : 'Apdata_X64';
                                                  dirApp      : 'GenVersionRes';
                                                  arquitetura : 'Win32';
                                                  appName     : 'GenVersionRes';
                                                  aplicacoes  : 'Utils\Compilacao';
                                                  bin         : '\';
                                                  source      : '\';
                                                  lib         : 'lib';
                                                  addEureka   : '0'
                                                ),
                                                (
                                                  typeBuild   : 'release';
                                                  dirBase     : 'Apdata_X64';
                                                  dirApp      : 'ApIdControl';
                                                  arquitetura : 'Win32';
                                                  appName     : 'ApIdControl';
                                                  aplicacoes  : 'Utils';
                                                  bin         : 'bin';
                                                  source      : '\';
                                                  lib         : 'lib';
                                                  addEureka   : '0';
                                                ),
                                                (
                                                  typeBuild   : '';
                                                  dirBase     : 'Apdata_X64';
                                                  dirApp      : 'TesteParser';
                                                  arquitetura : 'Win64';
                                                  appName     : 'TesteParser';
                                                  aplicacoes  : '\';
                                                  bin         : '\';
                                                  source      : '\';
                                                  lib         : '\';
                                                  addEureka   : '0'
                                                )
                                             );
  var
    strListMsg: TStringList;
  begin
    case ASystemId of
      01 :
        begin
          if NewBuild then
            Result := ExecuteInternal('C:\github\fs523673job\projects\cmdBAT\build.bat', Format('%s %s %s ApServer Win32 ApServer 0', [versionDelphi, ASubCommand, dirDelphi]) , Format('ApServer 32 - %s', [versionDelphi.ToUpper]))
          else
            Result := ExecuteInternal(Format('%s\Aplicacoes\ApServer\Source\buildServer.bat', [DirectoryRepository]), Format('%s Win32', [ASubCommand]), Format('ApServer 32 - %s', [versionDelphi.ToUpper]));
        end;
      02 :
        begin
          if NewBuild then
            Result := ExecuteInternal('C:\github\fs523673job\projects\cmdBAT\build.bat', Format('%s %s %s ApServer Win64 ApServer 0', [versionDelphi, ASubCommand, dirDelphi]) , Format('ApServer 64 - %s', [versionDelphi.ToUpper]))
          else
            Result := ExecuteInternal(Format('%s\Aplicacoes\ApServer\Source\buildServer.bat', [DirectoryRepository]), Format('%s Win64', [ASubCommand]), Format('ApServer 64 - %s', [versionDelphi.ToUpper]));
        end;
      03 :
        begin
          if NewBuild then
            Result := ExecuteInternal('C:\github\fs523673job\projects\cmdBAT\build.bat', Format('%s %s %s ApTools Win32 ApTools 0', [versionDelphi, ASubCommand, dirDelphi]) , Format('ApTools - %s', [versionDelphi.ToUpper]))
          else
            Result := ExecuteInternal(Format('%s\Aplicacoes\ApTools\Source\buildTools.bat', [DirectoryRepository]), ASubCommand, Format('ApTools - %s', [versionDelphi.ToUpper]));
        end;
      04 :
        begin
          Result := ExecuteInternal(Format('%s\Aplicacoes\ApWebDispatcher\Source\buildWebDispatcher.bat', [DirectoryRepository]), ASubCommand, Format('ApWebDispatcher - %s', [versionDelphi.ToUpper]));
        end;
      05 :
        begin
          if NewBuild then
            Result := ExecuteInternal('C:\github\fs523673job\projects\cmdBAT\build.bat',Format('%s %s %s ApLoadBalancer Win32 ApLoadBalancerServer 1', [versionDelphi, ASubCommand, dirDelphi]) , Format('ApLoadBalancer 32 - %s', [versionDelphi.ToUpper]))
          else
            Result := ExecuteInternal(Format('%s\Aplicacoes\ApLoadBalancer\Source\buildBalancer.bat', [DirectoryRepository]), Format('%s Win32', [ASubCommand]), Format('ApLoadBalancer 32 - %s', [versionDelphi.ToUpper]));
        end;
      06 :
        begin
          if NewBuild then
            Result := ExecuteInternal('C:\github\fs523673job\projects\cmdBAT\build.bat',Format('%s %s %s ApLoadBalancer Win64 ApLoadBalancerServer 1', [versionDelphi, ASubCommand, dirDelphi]) , Format('ApLoadBalancer 64 - %s', [versionDelphi.ToUpper]))
          else
            Result := ExecuteInternal(Format('%s\Aplicacoes\ApLoadBalancer\Source\buildBalancer.bat', [DirectoryRepository]), Format('%s Win64', [ASubCommand]), Format('ApLoadBalancer 64 - %s', [versionDelphi.ToUpper]));
        end;
      07 :
        begin
          if NewBuild then
            Result := ExecuteInternal('C:\github\fs523673job\projects\cmdBAT\build.bat',Format('%s %s %s ApESocialMsg Win32 ApESocialMsg 0', [versionDelphi, ASubCommand, dirDelphi]) , Format('ApESocialMsg 32 - %s', [versionDelphi.ToUpper]))
          else
            Result := ExecuteInternal(Format('%s\Aplicacoes\ApESocialMsg\Source\buildESocialMsg.bat', [DirectoryRepository]), ASubCommand, Format('ApESocial 32 - %s', [versionDelphi.ToUpper]));
        end;
      08 :
        begin
          if NewBuild then
            Result := ExecuteInternal('C:\github\fs523673job\projects\cmdBAT\build.bat',Format('%s %s %s ApScripter Win32 ApScripter 0', [versionDelphi, ASubCommand, dirDelphi]) , Format('ApScripter 32 - %s', [versionDelphi.ToUpper]))
          else
            Result := ExecuteInternal(Format('%s\Aplicacoes\ApScripter\Source\buildScripter.bat', [DirectoryRepository]), Format('%s Win32', [ASubCommand]), Format('ApScripter 32 - %s', [versionDelphi.ToUpper]));
        end;
      09 :
        begin
          if NewBuild then
            Result := ExecuteInternal('C:\github\fs523673job\projects\cmdBAT\build.bat',Format('%s %s %s ApScripter Win64 ApScripter 0', [versionDelphi, ASubCommand, dirDelphi]) , Format('ApScripter 64 - %s', [versionDelphi.ToUpper]))
          else
            Result := ExecuteInternal(Format('%s\Aplicacoes\ApScripter\Source\buildScripter.bat', [DirectoryRepository]), Format('%s Win64', [ASubCommand]), Format('ApScripter 64 - %s', [versionDelphi.ToUpper]));
        end;
      10 :
        begin
          if NewBuild then
            Result := ExecuteInternal('C:\github\fs523673job\projects\cmdBAT\build.bat',Format('%s %s %s ApIntegrationServer Win32 ApIntegrationServer 0', [versionDelphi, ASubCommand, dirDelphi]) , Format('ApIntegrationServer 32 - %s', [versionDelphi.ToUpper]))
          else
            Result := ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationServer\Source\buildIntegrationServer.bat', [DirectoryRepository]), Format('%s Win32', [ASubCommand]), Format('ApIntegrationServer 32 - %s', [versionDelphi.ToUpper]));
        end;
      11 :
        begin
          if NewBuild then
            Result := ExecuteInternal('C:\github\fs523673job\projects\cmdBAT\build.bat',Format('%s %s %s ApIntegrationServer Win64 ApIntegrationServer 0', [versionDelphi, ASubCommand, dirDelphi]) , Format('ApIntegrationServer 64 - %s', [versionDelphi.ToUpper]))
          else
            Result := ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationServer\Source\buildIntegrationServer.bat', [DirectoryRepository]), Format('%s Win64', [ASubCommand]), Format('ApIntegrationServer 64 - %s', [versionDelphi.ToUpper]));
        end;
      12 :
        begin
          if NewBuild then
            Result := ExecuteInternal('C:\github\fs523673job\projects\cmdBAT\build.bat',Format('%s %s %s ApIntegrationInterface Win32 ApIntegrationInterface 0', [versionDelphi, ASubCommand, dirDelphi]) , Format('ApIntegrationInterface 32 - %s', [versionDelphi.ToUpper]))
          else
            Result := ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationInterface\Source\buildIntegrationInterface.bat', [DirectoryRepository]), Format('%s Win32', [ASubCommand]), Format('ApIntegrationInterface 32 - %s', [versionDelphi.ToUpper]));
        end;
      13 :
        begin
          if NewBuild then
            Result := ExecuteInternal('C:\github\fs523673job\projects\cmdBAT\build.bat',Format('%s %s %s ApIntegrationInterface Win64 ApIntegrationInterface 0', [versionDelphi, ASubCommand, dirDelphi]) , Format('ApIntegrationInterface 64 - %s', [versionDelphi.ToUpper]))
          else
            Result := ExecuteInternal(Format('%s\Aplicacoes\ApIntegrationInterface\Source\buildIntegrationInterface.bat', [DirectoryRepository]), Format('%s Win64', [ASubCommand]), Format('ApIntegrationInterface 64 - %s', [versionDelphi.ToUpper]));
        end;
      14 :
        begin
          if NewBuild then
            Result := ExecuteInternal('C:\github\fs523673job\projects\cmdBAT\build.bat',Format('%s %s %s ApManager Win32 ApManager 0', [versionDelphi, ASubCommand, dirDelphi]) , Format('ApManager 32 - %s', [versionDelphi.ToUpper]))
          else
            Result := ExecuteInternal(Format('%s\Aplicacoes\ApManager\Source\buildManager.bat', [DirectoryRepository]), ASubCommand, Format('ApManager 32 - %s', [versionDelphi.ToUpper]));
        end;
      15 :
        begin
          if NewBuild then
            Result := ExecuteInternal('C:\github\fs523673job\projects\cmdBAT\build.bat',Format('%s %s %s ApUsers Win32 ApUsers 0', [versionDelphi, ASubCommand, dirDelphi]) , Format('ApUsers 32 - %s', [versionDelphi.ToUpper]))
          else
            Result := ExecuteInternal(Format('%s\Aplicacoes\ApUsers\Source\buildUsers.bat', [DirectoryRepository]), ASubCommand, Format('ApUsers 32 - %s', [versionDelphi.ToUpper]));
        end;
      16 :
        begin
          Result := ExecuteInternal('C:\github\fs523673job\projects\cmdBAT\build.bat',Format('%s %s %s ApADIntegratorWS Win32 ApADIntegratorWS 0', [versionDelphi, ASubCommand, dirDelphi]) , Format('ApADIntegratorWS ISAPI - %s', [versionDelphi.ToUpper]))
        end;
      17 :
        begin
          Result := ExecuteInternal('C:\github\fs523673job\projects\cmdBAT\build.bat',Format('%s %s %s ApDeveloper Win32 ApDeveloper 0', [versionDelphi, ASubCommand, dirDelphi]) , Format('ApDeveloper 32 - %s', [versionDelphi.ToUpper]))
        end;
      18 :
        begin
          Result := ExecuteInternal('C:\github\fs523673job\projects\cmdBAT\build.bat',Format('%s %s %s RelogioVirtual Win32 RelogioVirtual 0', [versionDelphi, ASubCommand, dirDelphi]) , Format('RelogioVirtual 32 - %s', [versionDelphi.ToUpper]))
        end;
      19 :
        begin
          strListMsg := TStringList.Create;
          try
            for var c := Low(CompileBuild) to High(CompileBuild) do
            begin
              if (CompileBuild[c].dirApp.Equals('TesteParser')) then
              begin
                strListMsg.Add(
                  ExecuteInternal('C:\github\fs523673job\projects\cmdBAT\compile.bat',
                        Format('%s %s %s %s %s %s %s %s %s %s %s',
                                [
                                   versionDelphi,
                                   IfThen(CompileBuild[c].typeBuild.IsEmpty, ASubCommand, CompileBuild[c].typeBuild),
                                   dirDelphi,
                                   CompileBuild[c].dirApp,
                                   CompileBuild[c].arquitetura,
                                   CompileBuild[c].appName,
                                   CompileBuild[c].aplicacoes,
                                   CompileBuild[c].bin,
                                   CompileBuild[c].source,
                                   CompileBuild[c].lib,
                                   CompileBuild[c].addEureka
                                ]
                              ),
                        Format('%s %s - %s', [CompileBuild[c].appName, CompileBuild[c].arquitetura, versionDelphi.ToUpper])
                 ));
              end;
            end;
          finally
            strListMsg.Free;
          end;

          Result := 'compile Tools [TestParser, GenVersionRes, ApIdControl]';
        end;
      20 :
        begin
          var replaceSrvDevel: Boolean := False;
          var OriginalContent := TStringList.Create;
          try
            if not FileExists(Format('%s\ApIdControl.exe',[DirectoryRepository])) then
            begin
              var SourcePath := Format('%s\Utils\ApIdControl\bin\Win32\Release\ApIdControl.exe', [DirectoryRepository]);
              var SourceDest := Format('%s\ApIdControl.exe', [DirectoryRepository]);

              CopyFile(PChar(SourcePath), PChar(SourceDest), False);
            end;

            OriginalContent.LoadFromFile(Format('%s\GenerateMessages.bat', [DirectoryRepository]));

            replaceSrvDevel := Pos('\\srvdevel\dynamicSource', OriginalContent.Text) > 0;

            if (replaceSrvDevel) then
              TFile.WriteAllText(Format('%s\GenerateMessages.bat', [DirectoryRepository]), OriginalContent.Text.Replace('\\srvdevel\', '\\srvdevel1\').TrimRight);

            Result := ExecuteInternal(Format('%s\GenerateMessages.bat', [DirectoryRepository]), '', 'GenerateMessages');
            if TFile.Exists(Format('%\Aplicacoes\ApServer\Source\atualiza_patch.bat', [DirectoryRepository])) then
              Result := Result + #13#10 + 'AtualizaPath ' +  (ExecuteInternal(Format('%\Aplicacoes\ApServer\Source\atualiza_patch.bat', [DirectoryRepository]), '', 'AtualizaPath'));
          finally
            OriginalContent.LoadFromFile(Format('%s\GenerateMessages.bat', [DirectoryRepository]));
            if (replaceSrvDevel) then
              TFile.WriteAllText(Format('%s\GenerateMessages.bat', [DirectoryRepository]), OriginalContent.Text.Replace('\\srvdevel1\', '\\srvdevel\').TrimRight);
            OriginalContent.Free;
          end;
        end;
      21 :
        begin
          Result := ExecuteInternal(Format('%s\Aplicacoes\ApWebDispatcher\Site\buildSass.bat', [DirectoryRepository]), '', 'GenerateSass');
        end;
      22 :
        begin
          strListMsg := TStringList.Create;
          try
            strListMsg.Add(ExecuteCommand(01, ASubCommand));
            strListMsg.Add(ExecuteCommand(10, ASubCommand));
            strListMsg.Add(ExecuteCommand(12, ASubCommand));

            PrintResumeComp(strListMsg);
          finally
            strListMsg.Free;
          end;

          Result := 'compile pack 22';
        end;
      23 :
        begin
          strListMsg := TStringList.Create;
          try
            strListMsg.Add(ExecuteCommand(02, ASubCommand));
            strListMsg.Add(ExecuteCommand(11, ASubCommand));
            strListMsg.Add(ExecuteCommand(13, ASubCommand));

            PrintResumeComp(strListMsg);
          finally
            strListMsg.Free;
          end;

          Result := 'compile pack 23';
        end;
      24 :
        begin
          strListMsg := TStringList.Create;
          try
            strListMsg.Add(ExecuteCommand(01, ASubCommand));
            strListMsg.Add(ExecuteCommand(05, ASubCommand));
            strListMsg.Add(ExecuteCommand(10, ASubCommand));
            strListMsg.Add(ExecuteCommand(16, ASubCommand));

            PrintResumeComp(strListMsg);
          finally
            strListMsg.Free;
          end;

          Result := 'compile pack 24';
        end;
      25 :
         begin
          strListMsg := TStringList.Create;
          try
            strListMsg.Add(ExecuteCommand(02, ASubCommand));
            strListMsg.Add(ExecuteCommand(06, ASubCommand));
            strListMsg.Add(ExecuteCommand(11, ASubCommand));
            strListMsg.Add(ExecuteCommand(16, ASubCommand));

            PrintResumeComp(strListMsg);
          finally
            strListMsg.Free;
          end;

          Result := 'compile pack 25';
        end;
      26 :
        begin
          strListMsg := TStringList.Create;
          try
            strListMsg.Add(ExecuteCommand(12, ASubCommand));
            strListMsg.Add(ExecuteCommand(16, ASubCommand));

            PrintResumeComp(strListMsg);
          finally
            strListMsg.Free;
          end;

          Result := 'compile pack 26';
        end;
      27 :
        begin
          strListMsg := TStringList.Create;
          try
            strListMsg.Add(ExecuteCommand(13, ASubCommand));
            strListMsg.Add(ExecuteCommand(16, ASubCommand));

            PrintResumeComp(strListMsg);
          finally
            strListMsg.Free;
          end;

          Result := 'compile pack 27';
        end;
      28 :
        begin
          strListMsg := TStringList.Create;
          try
            strListMsg.Add(ExecuteCommand(03, ASubCommand));
            strListMsg.Add(ExecuteCommand(09, ASubCommand));
            strListMsg.Add(ExecuteCommand(14, ASubCommand));
            strListMsg.Add(ExecuteCommand(15, ASubCommand));
            strListMsg.Add(ExecuteCommand(18, ASubCommand));

            PrintResumeComp(strListMsg);
          finally
            strListMsg.Free;
          end;

          Result := 'compile pack 28';
        end;
      29 :
        begin
          strListMsg := TStringList.Create;
          try
            for var c := Low(CompileBuild) to High(CompileBuild) do
            begin
              if CompileBuild[c].dirApp.Equals('GenVersionRes') or CompileBuild[c].dirApp.Equals('ApIdControl') then
              begin
                strListMsg.Add(
                  ExecuteInternal('C:\github\fs523673job\projects\cmdBAT\compile.bat',
                        Format('%s %s %s %s %s %s %s %s %s %s %s',
                                [
                                   versionDelphi,
                                   IfThen(CompileBuild[c].typeBuild.IsEmpty, ASubCommand, CompileBuild[c].typeBuild),
                                   dirDelphi,
                                   CompileBuild[c].dirApp,
                                   CompileBuild[c].arquitetura,
                                   CompileBuild[c].appName,
                                   CompileBuild[c].aplicacoes,
                                   CompileBuild[c].bin,
                                   CompileBuild[c].source,
                                   CompileBuild[c].lib,
                                   CompileBuild[c].addEureka
                                ]
                              ),
                        Format('%s %s - %s', [CompileBuild[c].appName, CompileBuild[c].arquitetura, versionDelphi.ToUpper])
                 ));
              end;
            end;
          finally
            strListMsg.Free;
          end;

          Result := 'compile Tools [TestParser, GenVersionRes, ApIdControl]';
        end;
      30 :
        begin
          strListMsg := TStringList.Create;
          try
            strListMsg.Add(ExecuteCommand(19, ASubCommand));
            strListMsg.Add(ExecuteCommand(20, ASubCommand));

            strListMsg.Add(ExecuteCommand(01, ASubCommand));
            strListMsg.Add(ExecuteCommand(02, ASubCommand));
            strListMsg.Add(ExecuteCommand(03, ASubCommand));
            strListMsg.Add(ExecuteCommand(04, ASubCommand));
            strListMsg.Add(ExecuteCommand(05, ASubCommand));
            strListMsg.Add(ExecuteCommand(06, ASubCommand));
            strListMsg.Add(ExecuteCommand(07, ASubCommand));
            strListMsg.Add(ExecuteCommand(08, ASubCommand));
            strListMsg.Add(ExecuteCommand(09, ASubCommand));
            strListMsg.Add(ExecuteCommand(10, ASubCommand));
            strListMsg.Add(ExecuteCommand(11, ASubCommand));
            strListMsg.Add(ExecuteCommand(12, ASubCommand));
            strListMsg.Add(ExecuteCommand(13, ASubCommand));
            strListMsg.Add(ExecuteCommand(14, ASubCommand));
            strListMsg.Add(ExecuteCommand(15, ASubCommand));
            strListMsg.Add(ExecuteCommand(16, ASubCommand));
            strListMsg.Add(ExecuteCommand(17, ASubCommand));
            strListMsg.Add(ExecuteCommand(18, ASubCommand));

            PrintResumeComp(strListMsg);
          finally
            strListMsg.Free;
          end;

          Result := 'compile pack 30';
        end;

      31 :
        begin
          strListMsg := TStringList.Create;
          try
            strListMsg.Add(ExecuteCommand(19, ASubCommand));
            strListMsg.Add(ExecuteCommand(20, ASubCommand));

            strListMsg.Add(ExecuteCommand(01, ASubCommand));
            strListMsg.Add(ExecuteCommand(03, ASubCommand));
            strListMsg.Add(ExecuteCommand(04, ASubCommand));
            strListMsg.Add(ExecuteCommand(05, ASubCommand));
            strListMsg.Add(ExecuteCommand(07, ASubCommand));
            strListMsg.Add(ExecuteCommand(08, ASubCommand));
            strListMsg.Add(ExecuteCommand(10, ASubCommand));
            strListMsg.Add(ExecuteCommand(12, ASubCommand));
            strListMsg.Add(ExecuteCommand(14, ASubCommand));
            strListMsg.Add(ExecuteCommand(15, ASubCommand));
            strListMsg.Add(ExecuteCommand(16, ASubCommand));
            strListMsg.Add(ExecuteCommand(17, ASubCommand));
            strListMsg.Add(ExecuteCommand(18, ASubCommand));

            PrintResumeComp(strListMsg);
          finally
            strListMsg.Free;
          end;

          Result := 'compile pack 31';
        end;

      32 :
        begin
          strListMsg := TStringList.Create;
          try
            strListMsg.Add(ExecuteCommand(19, ASubCommand));
            strListMsg.Add(ExecuteCommand(20, ASubCommand));

            strListMsg.Add(ExecuteCommand(02, ASubCommand));
            strListMsg.Add(ExecuteCommand(03, ASubCommand));
            strListMsg.Add(ExecuteCommand(04, ASubCommand));
            strListMsg.Add(ExecuteCommand(06, ASubCommand));
            strListMsg.Add(ExecuteCommand(07, ASubCommand));
            strListMsg.Add(ExecuteCommand(08, ASubCommand));
            strListMsg.Add(ExecuteCommand(09, ASubCommand));
            strListMsg.Add(ExecuteCommand(11, ASubCommand));
            strListMsg.Add(ExecuteCommand(13, ASubCommand));
            strListMsg.Add(ExecuteCommand(14, ASubCommand));
            strListMsg.Add(ExecuteCommand(15, ASubCommand));
            strListMsg.Add(ExecuteCommand(16, ASubCommand));
            strListMsg.Add(ExecuteCommand(17, ASubCommand));
            strListMsg.Add(ExecuteCommand(18, ASubCommand));

            PrintResumeComp(strListMsg);
          finally
            strListMsg.Free;
          end;

          Result := 'compile pack 32';
        end;
      else
        Console.WriteColor('not executed!', [TConsoleColor.Red]);
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
      else if (AnsiSameText(ANameSystem, 'ApADIntegratorWS')) then
        Result := 16
      else if (AnsiSameText(ANameSystem, 'ApDeveloper')) then
        Result := 17
      else if (AnsiSameText(ANameSystem, 'RelogioVirtual')) then
        Result := 18
      else if (AnsiSameText(ANameSystem, 'TestParser')) then
        Result := 19
      else if (AnsiSameText(ANameSystem, 'Messages')) then
        Result := 20
      else if (AnsiSameText(ANameSystem, 'Sass')) then
        Result := 21
      else if (AnsiSameText(ANameSystem, 'pintegration32')) then
        Result := 22
      else if (AnsiSameText(ANameSystem, 'pintegration64')) then
        Result := 23
      else if (AnsiSameText(ANameSystem, 'pservers32')) then
        Result := 24
      else if (AnsiSameText(ANameSystem, 'pservers64')) then
        Result := 25
      else if (AnsiSameText(ANameSystem, 'pdlls32')) then
        Result := 26
      else if (AnsiSameText(ANameSystem, 'pdlls64')) then
        Result := 27
      else if (AnsiSameText(ANameSystem, 'pclients')) then
        Result := 28
      else if (AnsiSameText(ANameSystem, 'ptools')) then
        Result := 29
      else if (AnsiSameText(ANameSystem, 'All')) then
        Result := 30
      else if (AnsiSameText(ANameSystem, 'All32')) then
        Result := 31
      else if (AnsiSameText(ANameSystem, 'All64')) then
        Result := 32
    end;
  end;

begin
  PrintInstructionMenu;

  if (ParamStr(1) <> '') then
    DirectoryRepository := ParamStr(1)
  else
    DirectoryRepository := 'c:\Apdata_X64';

  dirDelphi := Copy(DirectoryRepository, 4, Length(DirectoryRepository));

  if (ParamStr(2) <> '') then
    NewBuild := ParamStr(2).ToUpper() = 'TRUE'
  else
    NewBuild := True;

  if (not ParamStr(3).IsEmpty) then
  begin
    versionDelphi := ParamStr(3).ToLower;
    if (AnsiIndexStr(versionDelphi, ['alexandria', 'tokyo']) = -1) then
      versionDelphi := 'alexandria';
  end;

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
          for var c := 2 to High(InputArray) do
          begin
            if (InputArray[c] <> '') then
            begin
              SetLength(SystemArray, Length(SystemArray) + 1);
              SystemArray[High(SystemArray)] := InputArray[c].Replace(',','');
            end;
          end;
        end;
      end;

      case IndexStr(Command, ['EXIT', 'BREAK', 'CLEAR', 'CLS', 'MENU', 'SETDIR', 'COMPILE', 'GETDIR', 'SETNC', 'GETNC', 'HELP', 'GETVD', 'SETVD']) of
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
              DirectoryRepository := 'c:\Apdata_X64'
            else
              DirectoryRepository := SubCommand;

            dirDelphi := Copy(DirectoryRepository, 4, Length(DirectoryRepository));
          end;
        6    :
          begin
            StrList := TStringList.Create;
            try
              if (Length(SystemArray) > 0) then
              begin
                for var c := 0 to Length(SystemArray) - 1 do
                begin
                  SystemType := SystemArray[c].ToUpper;
                  Console.WriteColorLine(Format('compile> compiling %.2d/%.2d', [c + 1, Length(SystemArray)]), [TConsoleColor.DarkGreen]);
                  StrList.Add(ExecuteCommand(ConvertNameSystem(SystemType), SubCommand));
                end;
              end
              else
                StrList.Add(ExecuteCommand(ConvertNameSystem(SubCommand)));

              PrintResumeComp(StrList);
            finally
              StrList.Free;
            end;
          end;
        7 :
          begin
            Console.WriteColorLine(Format('> current directory %s', [DirectoryRepository]), [TConsoleColor.DarkGreen]);
          end;
        8 :
          begin
            if (SubCommand = '') then
              NewBuild := False
            else
              NewBuild := SubCommand.ToUpper() = 'TRUE';
          end;
        9 :
          begin
            Console.WriteColorLine(Format('> current set new build %s', [BoolToStr(NewBuild, True)]), [TConsoleColor.DarkGreen]);
          end;
        10 :
          begin
            ShowHelp;
          end;
        11 :
          begin
            Console.WriteColorLine(Format('> current version delphi [alexandria, tokyo]: %s', [versionDelphi]), [TConsoleColor.DarkGreen]);
          end;
        12 :
          begin
            if (SubCommand = '') then
              versionDelphi := 'alexandria'
            else
              versionDelphi := SubCommand.ToLower();
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
