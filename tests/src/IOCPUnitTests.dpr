program IOCPUnitTests;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  DUnitX.Loggers.Console,
  DUnitX.TestFramework,
  Tests.Horse.Provider.IOCP in 'tests\Tests.Horse.Provider.IOCP.pas',
  Tests.Horse.Provider.IOCP.Listen in 'tests\Tests.Horse.Provider.IOCP.Listen.pas';

var
  LRunner: ITestRunner;
  LResults: IRunResults;
begin
  ReportMemoryLeaksOnShutdown := True;
  try
    { --run:<fixture.test,...> runs chosen tests only; the
      listen bench uses it to run a check in a process of its own. }
    TDUnitX.CheckCommandLine;
    LRunner := TDUnitX.CreateRunner;
    LRunner.UseRTTI := False;
    LRunner.FailsOnNoAsserts := True;
    LRunner.AddLogger(TDUnitXConsoleLogger.Create(True));
    LResults := LRunner.Execute;
    { A --run that matches no test runs none and would pass. }
    if (LResults.TestCount = 0) or not LResults.AllPassed then
      ExitCode := 1;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
