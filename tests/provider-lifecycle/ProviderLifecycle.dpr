program ProviderLifecycle;

{$APPTYPE CONSOLE}

// Small runner for the real Console/VCL providers. Compile with -DHORSE_VCL
// to exercise VCL without running unrelated Console-only fixtures.
uses
  System.SysUtils,
  Horse,
  DUnitX.TestFramework,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  Tests.Horse.Provider.Config in '..\src\tests\Tests.Horse.Provider.Config.pas',
  Tests.Horse.Provider.MaxConnections in '..\src\tests\Tests.Horse.Provider.MaxConnections.pas',
  Tests.CleanupHelper in '..\src\tests\Tests.CleanupHelper.pas';

var
  LRunner: ITestRunner;
  LResults: IRunResults;
begin
  ReportMemoryLeaksOnShutdown := True;
{$IFDEF HORSE_RADIX_ROUTER}
  THorse.UseRadixRouter;
{$ENDIF}
  try
    TDUnitX.CheckCommandLine;
    LRunner := TDUnitX.CreateRunner;
    LRunner.UseRTTI := False;
    LRunner.FailsOnNoAsserts := True;
    LRunner.AddLogger(TDUnitXConsoleLogger.Create(True));
    LRunner.AddLogger(TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile));
    LResults := LRunner.Execute;
    if (LResults.TestCount = 0) or not LResults.AllPassed then
      ExitCode := 1;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
