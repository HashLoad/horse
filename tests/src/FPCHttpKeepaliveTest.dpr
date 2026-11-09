program FPCHttpKeepaliveTest;

{ Regression test for the FPC keep-alive latency stall (HashLoad/horse#562,
  fixed upstream in 3.3.4 by PR #563).

  ROOT CAUSE — Nagle interacting with the peer's delayed ACK, NOT a poll
  interval. fphttpserver writes each response as two send() calls: the header
  block, then the body. TCP_NODELAY was never set on the accepted socket, so
  Nagle held the small second write until the peer ACKed the first, and the
  peer — having nothing to send — waited out its ~40 ms delayed-ACK timer.

  Established by wire capture: the body segment leaves 11-14 us after the ACK
  arrives, three times, while the ACK delay itself varies 42.5-43.9 ms. The
  body tracks the ACK to microseconds across a varying delay, which only Nagle
  produces. An earlier reading of this file blamed "a fixed ~40 ms select()
  poll interval"; that was wrong, and this header used to state it as fact.
  The server IS idle in select() for ~43 ms, but waiting for the next request,
  which the client cannot send because it is still waiting for a body Nagle is
  holding. Cause and effect inverted.

  THE FIX THIS GUARDS — Horse.Provider.FPC.HTTPApplication sets TCP_NODELAY on
  each accepted socket via THorseNoDelaySocketHandler.Accept, wired through
  OnGetSocketHandler, and enables KeepConnections together with a positive
  KeepConnectionTimeout. Both halves are required: KeepConnections alone
  leaves WaitUntil at 0, so the connection thread serves exactly one request
  and exits.

  WHY KeepConnection := True BELOW IS LOAD-BEARING — the stall only appears on
  a REUSED connection. TFPHTTPClient.KeepConnection defaults to False, so
  without setting it every request gets a fresh socket, and Linux starts every
  socket in quickack mode: the first ACK is immediate, Nagle never engages, and
  this test passes whether or not the fix is present. That is precisely how it
  was written before, and it made the test decorative.

  WHAT THIS CHECKS
    1. REQUEST_COUNT sequential GETs, all on ONE connection, each completing
       within STALL_THRESHOLD_MS. The stall is ~44 ms when present and well
       under 1 ms when fixed, so 35 ms separates the two cleanly while leaving
       headroom over scheduling jitter.
    2. Every response is HTTP 200 with the body 'pong'.
    3. No hang — the process exits rather than blocking.

  Complements tests/fpc_keepalive_regression.py, which upstream added with the
  fix. That one drives FPCHttpKeepAliveServer.dpr from Python; this one is
  self-contained Pascal and runs where grpc_tools/Python are not installed.

  Build (from horse/tests/src/) — TRUNK, not the distro compiler. A bare `fpc`
  finds 3.2.2 on Ubuntu and the version guard below rejects it:

    U=/usr/local/fpc-trunk/lib/fpc/3.3.1/units/x86_64-linux
    mkdir -p /tmp/ka-test
    /usr/local/fpc-trunk/bin/fpc -MDelphi -Sh -B -n \
      -Fu"$U/*" -Fu"$U" -Fu../../src \
      -FU/tmp/ka-test -FE/tmp/ka-test FPCHttpKeepaliveTest.dpr
    /tmp/ka-test/FPCHttpKeepaliveTest

  The -n and explicit -Fu are not optional: without them trunk reads
  /etc/fpc.cfg, finds the distro's 3.2.2 units and dies with
  "PPU Invalid Version 207 expecting 208" — which reads as a missing unit but
  is really a wrong-compiler unit.

  No HORSE_* define is needed: on FPC with no provider define, Horse.pas
  selects Horse.Provider.FPC.HTTPApplication automatically. Indy never enters
  the picture on FPC. Requires FPC >= 3.3.1 — the provider's keep-alive and
  TCP_NODELAY code sits behind an $IF FPC_FULLVERSION >= 30301 guard, so on
  3.2.2 it is not merely untested, it is not compiled.

  (That guard is written without its braces on purpose: a directive's closing
  brace would end this comment early and the prose after it would be compiled.)
}

{$MODE DELPHI}{$H+}

// Refuse to build on a compiler where the code under test does not exist.
// The provider's keep-alive and TCP_NODELAY handling sit behind a
// FPC_FULLVERSION >= 30301 guard, so on 3.2.2 the server never keeps a
// connection open. This test sets KeepConnection := True on the client, so
// the run then dies with "Error reading data from socket" on the third
// request - a socket error that says nothing about the real cause. Failing
// at compile time names it instead.
{$IF FPC_FULLVERSION < 30301}
  {$FATAL This test requires FPC 3.3.1+. On 3.2.2 the provider's keep-alive and TCP_NODELAY code is not compiled at all, so there is nothing here to test.}
{$ENDIF}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, fphttpclient,
  Horse, Horse.Commons;

const
  TEST_PORT         = 9901;
  REQUEST_COUNT     = 30;
  STALL_THRESHOLD_MS = 35;  { any single request above this = stall detected }
  SERVER_STARTUP_MS = 1500; { allow fphttpserver to finish binding }

procedure PingHandler(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send('pong');
end;

type
  TServerThread = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TServerThread.Execute;
begin
  FreeOnTerminate := True;
  try
    THorse.Listen(TEST_PORT, '127.0.0.1');
  except
    on E: Exception do
      Writeln('Server: ', E.Message);
  end;
end;

var
  LClient: TFPHTTPClient;
  LStart: QWord;
  LElapsed, LMax, LTotal: QWord;
  LBody: string;
  LStatusCode: Integer;
  LFailed: Boolean;
  I: Integer;
begin
  Writeln('FPC keep-alive latency regression test (horse#562)');
  Writeln('  requests       : ', REQUEST_COUNT, ' (one reused connection)');
  Writeln('  stall threshold: ', STALL_THRESHOLD_MS, ' ms');

  THorse.Get('/ping', PingHandler);

  TServerThread.Create(False);
  Sleep(SERVER_STARTUP_MS);

  LClient := TFPHTTPClient.Create(nil);
  { Load-bearing, not a tuning knob. Defaults to False, and with a fresh
    connection per request Linux quickack keeps every first ACK immediate, so
    Nagle never engages and this test passes with or without the fix. }
  LClient.KeepConnection := True;
  LFailed := False;
  LMax    := 0;
  LTotal  := 0;
  try
    for I := 1 to REQUEST_COUNT do
    begin
      LStart := GetTickCount64;
      try
        LBody       := LClient.Get(Format('http://127.0.0.1:%d/ping', [TEST_PORT]));
        LStatusCode := LClient.ResponseStatusCode;
      except
        on E: Exception do
        begin
          Writeln('FAIL: request ', I, ' raised ', E.ClassName, ': ', E.Message);
          LFailed := True;
          Continue;
        end;
      end;

      LElapsed := GetTickCount64 - LStart;
      LTotal   := LTotal + LElapsed;
      if LElapsed > LMax then
        LMax := LElapsed;

      if LStatusCode <> 200 then
      begin
        Writeln(Format('FAIL: request %2d  status=%d  body="%s"', [I, LStatusCode, LBody]));
        LFailed := True;
      end
      else if LBody <> 'pong' then
      begin
        Writeln(Format('FAIL: request %2d  body="%s" (expected "pong")', [I, LBody]));
        LFailed := True;
      end
      else if LElapsed > STALL_THRESHOLD_MS then
      begin
        Writeln(Format('FAIL: request %2d  %3d ms > %d ms -- Nagle/delayed-ACK stall detected',
          [I, LElapsed, STALL_THRESHOLD_MS]));
        LFailed := True;
      end;
    end;
  finally
    LClient.Free;
  end;

  Writeln(Format('  max latency    : %d ms', [LMax]));
  Writeln(Format('  avg latency    : %d ms', [LTotal div REQUEST_COUNT]));

  if LFailed then
  begin
    Writeln('RESULT: FAIL');
    ExitCode := 1;
  end
  else
  begin
    Writeln(Format('RESULT: OK  (all %d requests <= %d ms)', [REQUEST_COUNT, LMax]));
    ExitCode := 0;
  end;
end.
