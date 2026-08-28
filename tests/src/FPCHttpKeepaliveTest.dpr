program FPCHttpKeepaliveTest;

{ Regression test for BENCH-FPCHTTP-1.
  Root cause: KeepConnections=True enabled fphttpserver's keepalive loop, which
  polls with a fixed ~40 ms select() interval between requests.  Every response
  cycle waited one full interval even when the next request was already queued.
  Fix (FPC-KEEPALIVE-1 removed from Horse.Provider.FPC.HTTPApplication): revert
  to KeepConnections=False so the server closes the TCP connection after each
  response and exits the per-request thread immediately.

  What this test checks:
    1. All REQUEST_COUNT sequential GET requests complete in <= STALL_THRESHOLD_MS.
       The stall is exactly ~40 ms on every affected kernel, so 35 ms gives clear
       headroom above normal scheduling jitter (<5 ms on a quiet loopback) while
       comfortably below the stall value.
    2. All responses carry HTTP 200 and the body 'pong'.
    3. (Implicit) No deadlock or hang — the test exits within TIMEOUT_S seconds.

  Build (from horse/tests/src/):
    fpc -Mdelphi -Sh \
        -Fu"../../src:modules/horse/src" \
        FPCHttpKeepaliveTest.dpr

  No HORSE_* define is needed: on FPC with no provider define, Horse.pas
  selects Horse.Provider.FPC.HTTPApplication automatically (line 306 / 541
  in the patched Horse.pas).  Indy never enters the picture on FPC.
}

{$MODE DELPHI}{$H+}

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
  Writeln('BENCH-FPCHTTP-1 regression test');
  Writeln('  requests       : ', REQUEST_COUNT);
  Writeln('  stall threshold: ', STALL_THRESHOLD_MS, ' ms');

  THorse.Get('/ping', PingHandler);

  TServerThread.Create(False);
  Sleep(SERVER_STARTUP_MS);

  LClient := TFPHTTPClient.Create(nil);
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
        Writeln(Format('FAIL: request %2d  %3d ms > %d ms -- BENCH-FPCHTTP-1 stall detected',
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
