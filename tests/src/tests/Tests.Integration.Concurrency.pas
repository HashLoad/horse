unit Tests.Integration.Concurrency;

interface

uses
  DUnitX.TestFramework, Horse, Horse.Commons, RESTRequest4D, System.SysUtils,
  System.Classes, System.Threading, System.Generics.Collections,
  Tests.CleanupHelper;

type
  [TestFixture]
  TTestIntegrationConcurrency = class
  private
    const TEST_PORT = 9095;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;

    [Test]
    procedure TestConcurrentRequestsUnderStress;
  end;

implementation

{ TTestIntegrationConcurrency }

procedure TTestIntegrationConcurrency.SetupFixture;
begin
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('pong');
    end);

  TThread.CreateAnonymousThread(
    procedure
    begin
      THorse.Listen(TEST_PORT);
    end).Start;

  Sleep(500);
end;

procedure TTestIntegrationConcurrency.TearDownFixture;
begin
  ClearGlobalState;
  Sleep(100);
end;

procedure TTestIntegrationConcurrency.TestConcurrentRequestsUnderStress;
const
  NUM_THREADS = 40;
  REQS_PER_THREAD = 10;
var
  LTasks: array[0..NUM_THREADS - 1] of ITask;
  I: Integer;
begin
  for I := 0 to NUM_THREADS - 1 do
  begin
    LTasks[I] := TTask.Create(
      procedure
      var
        LReq: IRequest;
        LRes: IResponse;
        K: Integer;
      begin
        for K := 1 to REQS_PER_THREAD do
        begin
          LReq := TRequest.New;
          LRes := LReq.BaseURL(Format('http://localhost:%d/ping', [TEST_PORT]))
            .Accept('text/plain')
            .Get;

          Assert.AreEqual(200, LRes.StatusCode, 'HTTP status should be 200 OK');
          Assert.AreEqual('pong', LRes.Content, 'Response content should be "pong"');
        end;
      end);
    LTasks[I].Start;
  end;

  TTask.WaitForAll(LTasks);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationConcurrency);

end.
