unit Tests.Integration.Streaming;

interface

uses
  DUnitX.TestFramework, Horse, System.SysUtils, System.Classes,
  System.Threading, System.Net.HttpClient, System.Net.URLClient, Tests.CleanupHelper,
  Horse.Commons, Horse.Response;

type
  [TestFixture]
  TTestIntegrationStreaming = class
  private
    const TEST_PORT = 9099;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;

    [Test]
    procedure TestStreamingNDJSON;
    [Test]
    procedure TestStreamingSSE;
    [Test]
    procedure TestStreamingConcurrentStress;
  end;

implementation

{ TTestIntegrationStreaming }

procedure TTestIntegrationStreaming.SetupFixture;
begin
  // Rota NDJSON (Web Streams)
  THorse.Get('/ndjson',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.ContentType('application/x-ndjson; charset=utf-8');
      Res.SendStream(
        procedure(const AWriter: IHorseStreamWriter)
        var
          I: Integer;
        begin
          for I := 1 to 5 do
          begin
            if not AWriter.IsConnected() then Break;
            AWriter.Write(Format('{"id": %d}'#10, [I]));
          end;
        end);
    end);

  // Rota SSE (Server-Sent Events)
  THorse.Get('/sse',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.ContentType('text/event-stream; charset=utf-8');
      Res.AddHeader('Cache-Control', 'no-cache');
      Res.AddHeader('Connection', 'keep-alive');
      Res.SendStream(
        procedure(const AWriter: IHorseStreamWriter)
        var
          I: Integer;
        begin
          for I := 1 to 5 do
          begin
            if not AWriter.IsConnected() then Break;
            AWriter.Write(Format('event: message'#10'data: {"id": %d}'#10#10, [I]));
          end;
        end);
    end);

  TThread.CreateAnonymousThread(
    procedure
    begin
      THorse.Listen(TEST_PORT);
    end).Start;

  Sleep(1000);
end;

procedure TTestIntegrationStreaming.TearDownFixture;
begin
  ClearGlobalState;
  Sleep(500);
end;

procedure TTestIntegrationStreaming.TestStreamingNDJSON;
var
  LClient: THTTPClient;
  LRes: IHTTPResponse;
  LContent: string;
begin
  LClient := THTTPClient.Create;
  try
    LRes := LClient.Get(Format('http://localhost:%d/ndjson', [TEST_PORT]));
    Assert.AreEqual(200, LRes.StatusCode, 'Erro: ' + LRes.ContentAsString);
    
    // Verifica que o Content-Type foi configurado corretamente
    Assert.AreEqual('application/x-ndjson; charset=utf-8', LRes.MimeType);
    
    // Verifica se os dados chegaram
    LContent := LRes.ContentAsString;
    Assert.Contains(LContent, '{"id": 1}');
    Assert.Contains(LContent, '{"id": 5}');
  finally
    LClient.Free;
  end;
end;

procedure TTestIntegrationStreaming.TestStreamingSSE;
var
  LClient: THTTPClient;
  LRes: IHTTPResponse;
  LContent: string;
begin
  LClient := THTTPClient.Create;
  try
    LRes := LClient.Get(Format('http://localhost:%d/sse', [TEST_PORT]));
    Assert.AreEqual(200, LRes.StatusCode, 'Erro: ' + LRes.ContentAsString);
    
    // Verifica que o Content-Type foi configurado corretamente
    Assert.AreEqual('text/event-stream; charset=utf-8', LRes.MimeType);
    
    // Verifica se os dados chegaram
    LContent := LRes.ContentAsString;
    Assert.Contains(LContent, 'event: message');
    Assert.Contains(LContent, 'data: {"id": 1}');
    Assert.Contains(LContent, 'data: {"id": 5}');
  finally
    LClient.Free;
  end;
end;

procedure TTestIntegrationStreaming.TestStreamingConcurrentStress;
const
  NUM_THREADS = 15;
var
  LTasks: array[0..NUM_THREADS - 1] of ITask;
  LStatusCodes: array[0..NUM_THREADS - 1] of Integer;
  LContents: array[0..NUM_THREADS - 1] of string;
  I: Integer;
  LThreadPool: TThreadPool;

  function BuildTaskProc(const AIndex: Integer): TProc;
  begin
    Result :=
      procedure
      var
        LClient: THTTPClient;
        LRes: IHTTPResponse;
      begin
        LClient := THTTPClient.Create;
        try
          LClient.CustomHeaders['Connection'] := 'close';
          try
            LRes := LClient.Get(Format('http://localhost:%d/ndjson', [TEST_PORT]));
            LStatusCodes[AIndex] := LRes.StatusCode;
            LContents[AIndex] := LRes.ContentAsString;
          except
            on E: Exception do
            begin
              LStatusCodes[AIndex] := 500;
              LContents[AIndex] := E.Message;
            end;
          end;
        finally
          LClient.Free;
        end;
      end;
  end;

begin
  LThreadPool := TThreadPool.Create;
  try
    for I := 0 to NUM_THREADS - 1 do
    begin
      LStatusCodes[I] := 0;
      LContents[I] := '';
    end;

    for I := 0 to NUM_THREADS - 1 do
    begin
      LTasks[I] := TTask.Create(BuildTaskProc(I), LThreadPool);
      LTasks[I].Start;
    end;

    TTask.WaitForAll(LTasks, 15000);
  finally
    LThreadPool.Free;
  end;

  for I := 0 to NUM_THREADS - 1 do
  begin
    Assert.AreEqual(200, LStatusCodes[I], Format('Thread %d retornou erro: %s', [I, LContents[I]]));
    Assert.Contains(LContents[I], '{"id": 1}');
    Assert.Contains(LContents[I], '{"id": 5}');
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationStreaming);

end.
