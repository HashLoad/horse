unit Tests.Integration.WebSocket;

interface

uses
  DUnitX.TestFramework, Horse, System.SysUtils, System.Classes,
  System.Threading, System.Net.HttpClient, System.Net.URLClient, Tests.CleanupHelper,
  Horse.Commons, Horse.Core.WebSocket, Horse.Exception.Interrupted;

type
  [TestFixture]
  TTestIntegrationWebSocket = class
  private
    const TEST_PORT = 9098;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;

    [Test]
    procedure TestHandshakeCalculation;
    [Test]
    procedure TestWebSocketUpgradeRequestNotWebSocket;
    [Test]
    procedure TestWebSocketUpgradeNotSupported;
  end;

implementation

{ TTestIntegrationWebSocket }

procedure TTestIntegrationWebSocket.SetupFixture;
begin
  // Rota de WebSocket normal (deve falhar se a requisição não for WebSocket)
  THorse.Get('/ws',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.UpgradeToWebSocket(nil);
    end);

  // Rota que simula provedor não suportado (remove o upgrader dos serviços antes do upgrade)
  THorse.Get('/ws-unsupported',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Req.Services.Add(THorseWebSocketUpgrader, nil, False); // Remove/sobreescreve com nil
      Res.UpgradeToWebSocket(nil);
    end);

  TThread.CreateAnonymousThread(
    procedure
    begin
      THorse.Listen(TEST_PORT);
    end).Start;

  Sleep(1000);
end;

procedure TTestIntegrationWebSocket.TearDownFixture;
begin
  ClearGlobalState;
  Sleep(500);
end;

procedure TTestIntegrationWebSocket.TestHandshakeCalculation;
var
  LClientKey: string;
  LAcceptKey: string;
begin
  LClientKey := 'dGhlIHNhbXBsZSBub25jZQ==';
  LAcceptKey := THorseWebSocketHandshake.CalculateAcceptKey(LClientKey);
  // O valor esperado para a chave RFC 6455 acima é 's3pPLMBiTxaQ9kYGzzhZRbK+xOo='
  Assert.AreEqual('s3pPLMBiTxaQ9kYGzzhZRbK+xOo=', LAcceptKey);
end;

procedure TTestIntegrationWebSocket.TestWebSocketUpgradeRequestNotWebSocket;
var
  LClient: THTTPClient;
  LRes: IHTTPResponse;
begin
  LClient := THTTPClient.Create;
  try
    // Requisição HTTP comum (sem os cabeçalhos de upgrade WebSocket)
    LRes := LClient.Get(Format('http://localhost:%d/ws', [TEST_PORT]));
    // Deve retornar 400 Bad Request
    Assert.AreEqual(400, LRes.StatusCode);
    Assert.Contains(LRes.ContentAsString, 'not a WebSocket upgrade request');
  finally
    LClient.Free;
  end;
end;

procedure TTestIntegrationWebSocket.TestWebSocketUpgradeNotSupported;
var
  LClient: THTTPClient;
  LRes: IHTTPResponse;
  LHeaders: TNetHeaders;
begin
  LClient := THTTPClient.Create;
  try
    // Força os headers de upgrade mas na rota '/ws-unsupported' que removeu o upgrader
    SetLength(LHeaders, 2);
    LHeaders[0] := TNetHeader.Create('Upgrade', 'websocket');
    LHeaders[1] := TNetHeader.Create('Sec-WebSocket-Key', 'dGhlIHNhbXBsZSBub25jZQ==');
    
    LRes := LClient.Get(Format('http://localhost:%d/ws-unsupported', [TEST_PORT]), nil, LHeaders);
    // Deve retornar 501 Not Implemented
    Assert.AreEqual(501, LRes.StatusCode);
    Assert.Contains(LRes.ContentAsString, 'not supported by the active provider');
  finally
    LClient.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationWebSocket);

end.
