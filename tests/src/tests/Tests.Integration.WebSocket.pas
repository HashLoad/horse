unit Tests.Integration.WebSocket;

interface

uses
  DUnitX.TestFramework, Horse, System.SysUtils, System.Classes,
  System.Threading, System.Net.HttpClient, System.Net.URLClient, Tests.CleanupHelper,
  Horse.Commons, Horse.Core.WebSocket, Horse.Exception.Interrupted,
  IdTCPClient, IdGlobal;

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
    [Test]
    procedure TestWebSocketDataExchange;
  end;

implementation

{ TTestIntegrationWebSocket }

procedure TTestIntegrationWebSocket.SetupFixture;
begin
  // Rota de WebSocket normal (usada para handshake e troca de dados)
  THorse.Get('/ws',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.UpgradeToWebSocket(
        procedure(const AConn: IHorseWebSocketConnection)
        begin
          AConn.OnMessage :=
            procedure(const AConnection: IHorseWebSocketConnection; const AText: string)
            begin
              AConnection.SendText('echo: ' + AText);
            end;
        end);
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

procedure TTestIntegrationWebSocket.TestWebSocketDataExchange;
var
  LClient: TIdTCPClient;
  LHandshakeReq: string;
  LHandshakeRes: string;
  LHandshakeResLine: string;
  LFrame: TIdBytes;
  LResBytes: TIdBytes;
  LTextRes: string;
begin
  LClient := TIdTCPClient.Create(nil);
  try
    LClient.Host := '127.0.0.1';
    LClient.Port := TEST_PORT;
    LClient.ConnectTimeout := 2000;
    LClient.ReadTimeout := 2000;
    
    LClient.Connect;
    Assert.IsTrue(LClient.Connected, 'Deveria conectar ao servidor WebSocket.');

    // 1. Enviar handshake de upgrade do cliente
    LHandshakeReq :=
      'GET /ws HTTP/1.1' + #13#10 +
      'Host: localhost:' + IntToStr(TEST_PORT) + #13#10 +
      'Upgrade: websocket' + #13#10 +
      'Connection: Upgrade' + #13#10 +
      'Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==' + #13#10 +
      'Sec-WebSocket-Version: 13' + #13#10#13#10;
      
    LClient.IOHandler.Write(LHandshakeReq);

    // 2. Receber o handshake de resposta do servidor
    LHandshakeRes := '';
    repeat
      LHandshakeResLine := LClient.IOHandler.ReadLn;
      LHandshakeRes := LHandshakeRes + LHandshakeResLine + #13#10;
    until LHandshakeResLine = '';
    
    Assert.Contains(LHandshakeRes, '101 Switching Protocols', 'Servidor deveria aceitar o upgrade do protocolo.');

    // 3. Enviar uma mensagem WebSocket curta mascarada (ex: "ola" - 3 bytes)
    // Frame: FIN=1, Opcode=1 (Text), Mask=1, Len=3
    // Chave de máscara: [1, 2, 3, 4]
    // Payload mascarado: "o" (111) xor 1 = 110, "l" (108) xor 2 = 110, "a" (97) xor 3 = 98
    SetLength(LFrame, 9);
    LFrame[0] := $81; // FIN + Text Opcode
    LFrame[1] := $83; // Mask = True + Length 3
    LFrame[2] := 1;   // Mask Key[0]
    LFrame[3] := 2;   // Mask Key[1]
    LFrame[4] := 3;   // Mask Key[2]
    LFrame[5] := 4;   // Mask Key[3]
    LFrame[6] := 110; // "o" xor 1
    LFrame[7] := 110; // "l" xor 2
    LFrame[8] := 98;  // "a" xor 3

    LClient.IOHandler.Write(LFrame);

    // 4. Receber a resposta do servidor ("echo: ola" que tem 9 bytes)
    // Frame servidor: FIN=1, Opcode=1 (Text), Mask=0, Len=9
    // Byte 0: $81 (FIN + Text)
    // Byte 1: $09 (Mask=0 + Len=9)
    // Bytes 2-10: "echo: ola"
    LClient.IOHandler.ReadBytes(LResBytes, 11, False);
    Assert.AreEqual(11, Length(LResBytes), 'Deveria ler 11 bytes de retorno.');
    Assert.AreEqual<Byte>($81, LResBytes[0], 'Deveria ser FIN + Text.');
    Assert.AreEqual<Byte>($09, LResBytes[1], 'Deveria ter tamanho 9 e nao ser mascarado.');
    
    LTextRes := BytesToString(LResBytes, 2, 9, IndyTextEncoding_UTF8);
    Assert.AreEqual('echo: ola', LTextRes, 'A mensagem ecoada deveria ser identica.');
  finally
    LClient.Disconnect;
    LClient.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationWebSocket);

end.
