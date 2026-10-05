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
  end;

implementation

uses
  System.SysUtils, System.Classes,
{$IF NOT DEFINED(HORSE_PROVIDER_HTTPSYS) AND NOT DEFINED(HORSE_PROVIDER_IOCP)}
  Web.WebReq,
{$ENDIF}
  Horse, Tests.CleanupHelper;

const
  TEST_PORT = 9131;

{$IF NOT DEFINED(HORSE_PROVIDER_HTTPSYS) AND NOT DEFINED(HORSE_PROVIDER_IOCP)}
{ Listen applies MaxConnections before it starts accepting, and on a console
  build Listen blocks, so it runs on its own thread like the other fixtures. }
procedure ListenThenStop;
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      THorse.Listen(TEST_PORT);
    end).Start;
  Sleep(500);
  THorse.StopListen;
  Sleep(200);
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
