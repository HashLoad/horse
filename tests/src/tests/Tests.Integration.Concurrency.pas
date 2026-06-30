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

  THorse.Get('/health',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('OK');
    end);

  THorse.Get('/clientes/:id/detalhes',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('cliente_' + Req.Params['id']);
    end);

  THorse.Post('/dados',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('recebido');
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
  REQS_PER_THREAD = 15;
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
        LUrlIdx: Integer;
        LClientId: string;
      begin
        for K := 1 to REQS_PER_THREAD do
        begin
          LReq := TRequest.New;
          LUrlIdx := (K + TThread.CurrentThread.ThreadID) mod 4;

          case LUrlIdx of
            0:
            begin
              LRes := LReq.BaseURL(Format('http://localhost:%d/ping', [TEST_PORT]))
                .Accept('text/plain')
                .Get;
              Assert.AreEqual(200, LRes.StatusCode, 'Ping status should be 200 OK');
              Assert.AreEqual('pong', LRes.Content, 'Response content should be "pong"');
            end;
            1:
            begin
              LRes := LReq.BaseURL(Format('http://localhost:%d/health', [TEST_PORT]))
                .Accept('text/plain')
                .Get;
              Assert.AreEqual(200, LRes.StatusCode, 'Health status should be 200 OK');
              Assert.AreEqual('OK', LRes.Content, 'Response content should be "OK"');
            end;
            2:
            begin
              LClientId := Format('%d_%d', [TThread.CurrentThread.ThreadID, K]);
              LRes := LReq.BaseURL(Format('http://localhost:%d/clientes/%s/detalhes', [TEST_PORT, LClientId]))
                .Accept('text/plain')
                .Get;
              Assert.AreEqual(200, LRes.StatusCode, 'Clientes status should be 200 OK');
              Assert.AreEqual('cliente_' + LClientId, LRes.Content, 'Client ID parameter should be thread-isolated');
            end;
            3:
            begin
              LRes := LReq.BaseURL(Format('http://localhost:%d/dados', [TEST_PORT]))
                .Accept('text/plain')
                .AddBody('payload_dados')
                .Post;
              Assert.AreEqual(200, LRes.StatusCode, 'Post status should be 200 OK');
              Assert.AreEqual('recebido', LRes.Content, 'Post response should be "recebido"');
            end;
          end;
        end;
      end);
    LTasks[I].Start;
  end;

  TTask.WaitForAll(LTasks);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationConcurrency);

end.
