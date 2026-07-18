program HorseHttpSysTestServer;

{$APPTYPE CONSOLE}
{$DEFINE HORSE_PROVIDER_HTTPSYS}

{
  Horse + HTTP.sys Provider  —  Integration Test Server
  ======================================================
  HORSE_PROVIDER_HTTPSYS selects the Windows kernel-mode HTTP.sys driver
  (Horse.Provider.HttpSys) via httpapi.dll — no Indy dependency.

  Windows only.  On other platforms the define is silently ignored and the
  default Console (Indy) provider is used instead.

  URL registration — why the strong wildcard (+) is required:
    HTTP.sys routes requests to applications by matching the request's Host
    header against registered URL prefixes:
      http://localhost:9010/   — explicit host: matches ONLY Host: localhost:9010
      http://127.0.0.1:9010/   — IP-literal:    becomes an "IP-bound weak
                                 wildcard" (shown by netsh as
                                 http://127.0.0.1:9010:127.0.0.1/) and does NOT
                                 match Host: 127.0.0.1:9010 requests
      http://+:9010/           — strong wildcard: matches EVERY Host header
    The test client targets http://127.0.0.1:9010 (Host: 127.0.0.1:9010), so
    only the strong wildcard reliably routes its requests to this server.
    (Verified empirically: with localhost + 127.0.0.1 registered, netsh showed
    "Requests arrived: 0" on the queue while the kernel answered 503.)

    The strong wildcard needs a one-time elevated URL ACL reservation:
      netsh http add urlacl url=http://+:9010/ user=Everyone
    or run this server once from an elevated prompt.

  Run this program first, then run HorseIndyTestClient.exe.

  Known limitation: multipart/form-data decoding (/upload route) and empty-
  body POST depend on the HTTP.sys bridge implementation — the test client
  accepts 200 or 400 for those routes.
}

uses
  System.SysUtils,
  Horse,
  Horse.Indy.TestRoutes;

begin
  try
    RegisterTestRoutes;
    Writeln(Format('[HorseHttpSysTest] Starting HTTP.sys server on port %d ...',
      [TEST_PORT]));
    Writeln('[HorseHttpSysTest] URL prefix: http://+:' + IntToStr(TEST_PORT) +
      '/  (strong wildcard - matches every Host header)');
    THorse.Listen(TEST_PORT, '0.0.0.0',
      procedure
      begin
        Writeln('[HorseHttpSysTest] LISTENING - URL registered and request');
        Writeln('[HorseHttpSysTest] queue receives posted successfully.');
      end);
    Writeln('[HorseHttpSysTest] Server stopped.');
  except
    on E: Exception do
    begin
      Writeln('[HorseHttpSysTest] Fatal: ' + E.ClassName + ': ' + E.Message);
      if Pos('error code: 5', E.Message) > 0 then
      begin
        Writeln('[HorseHttpSysTest] ERROR_ACCESS_DENIED - the strong wildcard');
        Writeln('[HorseHttpSysTest] needs a one-time URL ACL. From an ELEVATED');
        Writeln('[HorseHttpSysTest] prompt run:');
        Writeln('[HorseHttpSysTest]   netsh http add urlacl url=http://+:' +
          IntToStr(TEST_PORT) + '/ user=Everyone');
        Writeln('[HorseHttpSysTest] then start this server again (not elevated).');
      end;
      ExitCode := 1;
    end;
  end;
end.
