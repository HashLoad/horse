program HorseEpollTestServer;

{$APPTYPE CONSOLE}
{$DEFINE HORSE_PROVIDER_EPOLL}

{
  Horse + Epoll Provider  —  Integration Test Server
  ===================================================
  HORSE_PROVIDER_EPOLL selects the Linux epoll HTTP implementation
  (Horse.Provider.Epoll) — a raw socket server using the Linux kernel
  epoll API.  No Indy dependency.

  Linux only (Delphi for Linux or FPC/Linux).
  On Windows the define falls back silently to the Indy Console provider,
  so the server still runs but tests the wrong provider — the startup banner
  will say "Epoll" but the actual backend is Console (Indy).

  Run this program first, then run HorseIndyTestClient.exe on the same
  machine.  The test client connects to http://127.0.0.1:9010.

  Known limitation: multipart/form-data decoding (/upload route) and empty-
  body POST coverage depend on the custom epoll parser — the test client
  accepts 200 or 400 for those routes.
}

uses
  {$IF DEFINED(FPC) AND DEFINED(UNIX)}
  // FPC/Linux: TThread requires the cthreads driver as the FIRST unit of the
  // program — without it the first thread creation dies with RTE 232
  // ("This binary has no thread support compiled in").  Delphi links its
  // threading support implicitly; FPC does not.
  cthreads,
  {$ENDIF}
  SysUtils,
  Horse,
  Horse.Indy.TestRoutes;

begin
  try
    RegisterTestRoutes;
    Writeln(Format('[HorseEpollTest] Starting Epoll server on port %d ...',
      [TEST_PORT]));
    {$IFDEF LINUX}
    Writeln('[HorseEpollTest] Linux epoll backend active.');
    {$ELSE}
    Writeln('[HorseEpollTest] WARNING: not Linux — running Console (Indy) fallback.');
    {$ENDIF}
    THorse.Listen(TEST_PORT);
    Writeln('[HorseEpollTest] Server stopped.');
  except
    on E: Exception do
    begin
      Writeln('[HorseEpollTest] Fatal: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
