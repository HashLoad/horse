program HorseIndyTestServer;

{$APPTYPE CONSOLE}

{
  Horse + Indy Console Provider  —  Integration Test Server
  ==========================================================
  Default provider: no define needed.  Horse.Provider.Console wraps
  TIdHTTPServer via the WebBroker bridge.

  Supports all platforms where Delphi or FPC compile Indy:
    Windows (Win32 / Win64) and Linux (Delphi for Linux / FPC).

  Run this program first, then run HorseIndyTestClient.exe.
  The test client connects to http://127.0.0.1:9010.
}

uses
  System.SysUtils,
  Horse,
  Horse.Indy.TestRoutes;

begin
  try
    RegisterTestRoutes;
    Writeln(Format('[HorseIndyTest] Starting Indy Console server on port %d ...',
      [TEST_PORT]));
    THorse.Listen(TEST_PORT);
    Writeln('[HorseIndyTest] Server stopped.');
  except
    on E: Exception do
    begin
      Writeln('[HorseIndyTest] Fatal: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
