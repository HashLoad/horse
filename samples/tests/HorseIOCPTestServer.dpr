program HorseIOCPTestServer;

{$APPTYPE CONSOLE}
{$DEFINE HORSE_PROVIDER_IOCP}

{
  Horse + IOCP Provider  —  Integration Test Server
  ==================================================
  HORSE_PROVIDER_IOCP selects the custom Windows IOCP HTTP implementation
  (Horse.Provider.IOCP) — a raw socket server with no Indy dependency.

  Windows only.  On other platforms the define is silently ignored and the
  default Console (Indy) provider is used instead.

  Run this program first, then run HorseIndyTestClient.exe.
  The test client connects to http://127.0.0.1:9010.

  Known limitation: multipart/form-data decoding (/upload route) and empty-
  body POST (/methods/post with nil body) depend on the custom parser's
  coverage — the test client accepts 200 or 400 for those routes.
}

uses
  System.SysUtils,
  Horse,
  Horse.Indy.TestRoutes;

begin
  try
    RegisterTestRoutes;
    Writeln(Format('[HorseIOCPTest] Starting IOCP server on port %d ...',
      [TEST_PORT]));
    THorse.Listen(TEST_PORT);
    Writeln('[HorseIOCPTest] Server stopped.');
  except
    on E: Exception do
    begin
      Writeln('[HorseIOCPTest] Fatal: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
