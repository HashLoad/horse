unit Tests.Integration.Concurrency;

interface

uses
  DUnitX.TestFramework, Horse, Horse.Commons, RESTRequest4D, System.SysUtils,
  System.Classes, System.Threading, System.Generics.Collections,
  System.SyncObjs, Tests.CleanupHelper;

type
  [TestFixture]
  TTestIntegrationConcurrency = class
  private
    const TEST_PORT = 9125;
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

  Sleep(2000);
end;

procedure TTestIntegrationConcurrency.TearDownFixture;
begin
  ClearGlobalState;
  Sleep(500);
end;

procedure TTestIntegrationConcurrency.TestConcurrentRequestsUnderStress;
const
  NUM_THREADS = 3;
  REQS_PER_THREAD = 2;
var
  LTasks: array[0..NUM_THREADS - 1] of ITask;
  I: Integer;
  LFailed: Boolean;
  LFailMessage: string;
  LFailedCS: TCriticalSection;
begin
  LFailed := False;
  LFailMessage := '';
  LFailedCS := TCriticalSection.Create;
  try
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
            try
              LReq := TRequest.New;
              LUrlIdx := (K + TThread.CurrentThread.ThreadID) mod 4;

              case LUrlIdx of
                0:
                begin
                  LRes := LReq.BaseURL(Format('http://localhost:%d/ping', [TEST_PORT]))
                    .Accept('text/plain')
                    .Get;
                  if LRes.StatusCode <> 200 then
                  begin
                    LFailedCS.Enter;
                    LFailed := True;
                    LFailMessage := Format('Ping status expected 200 but got %d', [LRes.StatusCode]);
                    LFailedCS.Leave;
                  end;
                end;
                1:
                begin
                  LRes := LReq.BaseURL(Format('http://localhost:%d/health', [TEST_PORT]))
                    .Accept('text/plain')
                    .Get;
                  if LRes.StatusCode <> 200 then
                  begin
                    LFailedCS.Enter;
                    LFailed := True;
                    LFailMessage := Format('Health status expected 200 but got %d', [LRes.StatusCode]);
                    LFailedCS.Leave;
                  end;
                end;
                2:
                begin
                  LClientId := Format('%d_%d', [TThread.CurrentThread.ThreadID, K]);
                  LRes := LReq.BaseURL(Format('http://localhost:%d/clientes/%s/detalhes', [TEST_PORT, LClientId]))
                    .Accept('text/plain')
                    .Get;
                  if LRes.StatusCode <> 200 then
                  begin
                    LFailedCS.Enter;
                    LFailed := True;
                    LFailMessage := Format('Clientes status expected 200 but got %d', [LRes.StatusCode]);
                    LFailedCS.Leave;
                  end;
                end;
                3:
                begin
                  LRes := LReq.BaseURL(Format('http://localhost:%d/dados', [TEST_PORT]))
                    .Accept('text/plain')
                    .AddBody('payload_dados')
                    .Post;
                  if LRes.StatusCode <> 200 then
                  begin
                    LFailedCS.Enter;
                    LFailed := True;
                    LFailMessage := Format('Post status expected 200 but got %d', [LRes.StatusCode]);
                    LFailedCS.Leave;
                  end;
                end;
              end;
            except
              on E: Exception do
              begin
                LFailedCS.Enter;
                LFailed := True;
                LFailMessage := 'Exception in thread: ' + E.Message;
                LFailedCS.Leave;
              end;
            end;
          end;
        end);
      LTasks[I].Start;
    end;

    TTask.WaitForAll(LTasks);
    Assert.IsFalse(LFailed, 'Thread stress test failed: ' + LFailMessage);
  finally
    LFailedCS.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationConcurrency);

end.
