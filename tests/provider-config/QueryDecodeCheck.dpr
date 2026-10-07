program QueryDecodeCheck;

{$APPTYPE CONSOLE}

// Isolate the real HTTP query/form regression without shared suite build files.
// Compile with HORSE_TEST_ISOLATED_QUERY to use port 19126.
uses
  System.SysUtils,
  Horse,
  DUnitX.TestFramework,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  Tests.Integration.QueryDecode in '..\src\tests\Tests.Integration.QueryDecode.pas',
  Tests.Horse.Provider.RawAdapters in '..\src\tests\Tests.Horse.Provider.RawAdapters.pas',
  Tests.CleanupHelper in '..\src\tests\Tests.CleanupHelper.pas';

var
  Runner: ITestRunner;
  Results: IRunResults;
begin
  ReportMemoryLeaksOnShutdown := True;
{$IFDEF HORSE_RADIX_ROUTER}
  THorse.UseRadixRouter;
{$ENDIF}
  try
    TDUnitX.CheckCommandLine;
    Runner := TDUnitX.CreateRunner;
    Runner.UseRTTI := False;
    Runner.FailsOnNoAsserts := True;
    Runner.AddLogger(TDUnitXConsoleLogger.Create(True));
    Runner.AddLogger(TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile));
    Results := Runner.Execute;
    if (Results.TestCount = 0) or not Results.AllPassed then
      ExitCode := 1;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
