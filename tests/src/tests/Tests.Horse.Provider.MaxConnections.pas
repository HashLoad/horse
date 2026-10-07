unit Tests.Horse.Provider.MaxConnections;

{ FIX-MAXCONN-RESET-1 regression test.

  The Indy providers (Console, Daemon, VCL) copy THorse.MaxConnections into
  two process-wide limits: WebBroker's WebRequestHandler.MaxConnections and
  the Indy bridge's MaxConnections. Both outlive StopListen. Before the fix,
  Listen only copied the value when it was > 0, so MaxConnections := 0 could
  never lift a limit an earlier Listen had applied.

  That is what made TestStreamingConcurrentStress fail on the Default provider:
  TApiTest sets MaxConnections := 10, Tests.CleanupHelper "resets" it to 0, the
  reset never reached the limits, and the stress test's 15 concurrent requests
  were then refused with "Maximum number of concurrent connections exceeded"
  (WebBroker) or a dropped connection (Indy).

  Only the WebBroker limit can be asserted directly: the Indy bridge is
  private to the provider. Both are restored by the same branch. }

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestHorseProviderMaxConnections = class
  public
    [Test]
    procedure TestZeroLiftsALimitAnEarlierListenApplied;
    [Test]
    procedure TestZeroAllowsConcurrentRequestsAbovePreviousLimit;
  end;

implementation

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.Threading,
  System.Net.HttpClient, System.Net.URLClient,
{$IF NOT DEFINED(HORSE_PROVIDER_HTTPSYS) AND NOT DEFINED(HORSE_PROVIDER_IOCP)}
  Web.WebReq,
{$ENDIF}
  Horse, Tests.CleanupHelper;

const
{$IFDEF HORSE_TEST_ISOLATED_LIFECYCLE}
  TEST_PORT = 19131;
{$ELSE}
  TEST_PORT = 9131;
{$ENDIF}

{$IF NOT DEFINED(HORSE_PROVIDER_HTTPSYS) AND NOT DEFINED(HORSE_PROVIDER_IOCP)}
{ Listen applies MaxConnections before it starts accepting, and on a console
  build Listen blocks, so it runs on its own thread like the other fixtures. }
procedure ListenThenStop(const AWhileListening: TProc = nil);
var
  LThread: TThread;
  LReady: TEvent;
  LError: string;
begin
  LReady := TEvent.Create(nil, True, False, '');
  LError := '';
  LThread := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        THorse.Listen(TEST_PORT, '127.0.0.1',
          procedure
          begin
            LReady.SetEvent;
          end);
      except
        on E: Exception do
        begin
          LError := E.ClassName + ': ' + E.Message;
          LReady.SetEvent;
        end;
      end;
    end);
  LThread.FreeOnTerminate := False;
  try
    LThread.Start;
    Assert.IsTrue(LReady.WaitFor(5000) = wrSignaled, 'Listener startup timed out');
    Assert.AreEqual('', LError, 'Listener startup failed');
    Assert.IsTrue(THorse.IsRunning, 'Listener must be running before stopping');
    if Assigned(AWhileListening) then
      AWhileListening;
  finally
    try
      if THorse.IsRunning then
        THorse.StopListen;
    finally
      // Join before releasing the captured event and strings.
      LThread.WaitFor;
      LThread.Free;
      LReady.Free;
    end;
  end;
end;
{$ENDIF}

procedure TTestHorseProviderMaxConnections.TestZeroAllowsConcurrentRequestsAbovePreviousLimit;
{$IF NOT DEFINED(HORSE_PROVIDER_HTTPSYS) AND NOT DEFINED(HORSE_PROVIDER_IOCP)}
var
  LArrived: Integer;
  LFailures: Integer;
  LRelease: TEvent;
  LTasks: array[0..2] of ITask;
begin
  ClearGlobalState;
  LArrived := 0;
  LFailures := 0;
  LRelease := TEvent.Create(nil, True, False, '');
  try
    THorse.Get('/limit-reset',
      procedure(Req: THorseRequest; Res: THorseResponse)
      begin
        if TInterlocked.Increment(LArrived) = Length(LTasks) then
          LRelease.SetEvent;
        LRelease.WaitFor(5000);
        Res.Send('ok');
      end);
    THorse.MaxConnections := 1;
    ListenThenStop;
    THorse.MaxConnections := 0;
    ListenThenStop(
      procedure
      var
        I: Integer;
      begin
        for I := Low(LTasks) to High(LTasks) do
          LTasks[I] := TTask.Run(
            procedure
            var
              LClient: THTTPClient;
              LResponse: IHTTPResponse;
            begin
              LClient := THTTPClient.Create;
              try
{$IF CompilerVersion >= 31.0}
                LClient.ConnectionTimeout := 3000;
                LClient.ResponseTimeout := 7000;
{$IFEND}
                try
                  LResponse := LClient.Get(Format('http://127.0.0.1:%d/limit-reset', [TEST_PORT]));
                  if (LResponse.StatusCode <> 200) or
                    (LResponse.ContentAsString <> 'ok') then
                    TInterlocked.Increment(LFailures);
                except
                  TInterlocked.Increment(LFailures);
                end;
              finally
                LClient.Free;
              end;
            end);
        try
          Assert.IsTrue(TTask.WaitForAll(LTasks, 15000), 'Concurrent clients timed out');
          Assert.AreEqual(3, LArrived, 'Both WebBroker and Indy must lift the old limit');
          Assert.AreEqual(0, LFailures, 'All concurrent requests must succeed');
        finally
          LRelease.SetEvent;
          TTask.WaitForAll(LTasks);
          // Task closures and the route share this activation record. Release
          // task interfaces explicitly to avoid a reference-counting cycle.
          for I := Low(LTasks) to High(LTasks) do
            LTasks[I] := nil;
        end;
      end);
  finally
    ClearGlobalState;
    LRelease.Free;
  end;
end;
{$ELSE}
begin
  Assert.Pass('Indy-only connection-limit regression');
end;
{$ENDIF}

procedure TTestHorseProviderMaxConnections.TestZeroLiftsALimitAnEarlierListenApplied;
{$IF NOT DEFINED(HORSE_PROVIDER_HTTPSYS) AND NOT DEFINED(HORSE_PROVIDER_IOCP)}
var
  LBaseline: Integer;
  LLimit: Integer;
begin
  ClearGlobalState;
  try
    { 1. Baseline: what a Listen with MaxConnections = 0 leaves in force. With
         the fix this also undoes a limit an EARLIER fixture applied, so the
         baseline does not depend on test order. }
    THorse.MaxConnections := 0;
    ListenThenStop;
    LBaseline := WebRequestHandler.MaxConnections;

    { 2. Control: a positive limit is applied. Without this, step 3 could pass
         against a provider that never applied anything. }
    if LBaseline = 7 then
      LLimit := 9
    else
      LLimit := 7;
    THorse.MaxConnections := LLimit;
    ListenThenStop;
    Assert.AreEqual(LLimit, WebRequestHandler.MaxConnections,
      'control: a positive MaxConnections reaches WebRequestHandler');

    { 3. Back to 0: the limit must be lifted, not left in force. }
    THorse.MaxConnections := 0;
    ListenThenStop;
    Assert.AreEqual(LBaseline, WebRequestHandler.MaxConnections,
      'MaxConnections := 0 must restore the limit in force before it was set');
  finally
    ClearGlobalState;
  end;
end;
{$ELSE}
begin
  Assert.Pass('HttpSys and IOCP do not use WebRequestHandler; Indy providers only');
end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TTestHorseProviderMaxConnections);

end.
