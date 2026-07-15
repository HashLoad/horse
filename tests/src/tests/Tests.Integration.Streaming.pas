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

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationStreaming);

end.
