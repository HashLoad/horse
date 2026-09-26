unit Tests.Integration.GracefulShutdown;

interface

uses
  DUnitX.TestFramework, Horse, Horse.Commons, Horse.Instance,
  System.SysUtils, System.Classes, System.Threading, System.Net.HttpClient,
  System.Net.URLClient, Tests.CleanupHelper;

type
  [TestFixture]
  TTestIntegrationGracefulShutdown = class
  private
    const TEST_PORT = 9098;
    const TEST_PORT_INSTANCE = 9099;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;

    [Test]
    procedure TestGracefulShutdownSuccess;
    [Test]
    procedure TestGracefulShutdownSuccessOnInstance;
  end;

implementation

{ TTestIntegrationGracefulShutdown }

procedure TTestIntegrationGracefulShutdown.SetupFixture;
begin
end;

procedure TTestIntegrationGracefulShutdown.TearDownFixture;
begin
  ClearGlobalState;
end;

procedure TTestIntegrationGracefulShutdown.TestGracefulShutdownSuccess;
var
  LServerThread: TThread;
  LClientThread: TThread;
  LClient: THTTPClient;
  LRes: IHTTPResponse;
  LStart: Int64;
  LDuration: Int64;
  LResponseError: string;
  LWaitStart: Int64;
  LContent: string;
  LStatusCode: Integer;
begin
  ClearGlobalState;

  // Registrar endpoint lento
  THorse.Get('/slow-request',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      TThread.Sleep(1500);
      Res.Send('slow-response-ok');
    end);

  // Iniciar o servidor Horse em uma thread em background
  LServerThread := TThread.CreateAnonymousThread(
    procedure
    begin
      THorse.Listen(TEST_PORT);
    end);
  LServerThread.FreeOnTerminate := False;
  LServerThread.Start;

  // Aguarda o servidor levantar
  TThread.Sleep(500);

  LResponseError := '';
  LContent := '';
  LStatusCode := 0;

  // Dispara a requisição lenta em uma thread de background (assíncrona)
  LStart := TThread.GetTickCount;
  LClientThread := TThread.CreateAnonymousThread(
    procedure
    var
      LHeaders: TNetHeaders;
    begin
      LClient := THTTPClient.Create;
      try
        SetLength(LHeaders, 1);
        LHeaders[0] := TNetHeader.Create('Connection', 'close');
        
        try
          LRes := LClient.Get('http://localhost:' + TEST_PORT.ToString + '/slow-request', TStream(nil), LHeaders);
          LStatusCode := LRes.StatusCode;
          LContent := LRes.ContentAsString;
        except
          on E: Exception do
            LResponseError := E.Message;
        end;
      finally
        LClient.Free;
      end;
    end);
  LClientThread.FreeOnTerminate := False;
  LClientThread.Start;

  try
    // Aguarda deterministicamente a requisição entrar em processamento no servidor
    LWaitStart := TThread.GetTickCount;
    while (THorse.ActiveRequests = 0) and (TThread.GetTickCount - LWaitStart < 3000) do
    begin
      TThread.Sleep(10);
    end;

    // Aciona o desligamento suave na thread principal do teste!
    THorse.StopListenGraceful(4000);

    // Aguarda a thread do cliente terminar (ela deve finalizar porque o request foi escoado)
    LClientThread.WaitFor;

    LDuration := TThread.GetTickCount - LStart;

    // Asserções
    if LResponseError <> '' then
      Assert.Fail('Falha na requisição lenta: ' + LResponseError);

    Assert.AreEqual(200, LStatusCode, 'O código de retorno deve ser 200 (OK)');
    Assert.AreEqual('slow-response-ok', LContent, 'O conteúdo da resposta deve ter sido escoado');
    Assert.IsTrue(LDuration >= 1500, 'A duração total deve refletir o processamento lento');

    // Aguarda um instante para garantir que a thread do servidor terminou
    LServerThread.WaitFor;
    Assert.IsFalse(THorse.IsRunning, 'O servidor deve estar desligado');
  finally
    LClientThread.Free;
    LServerThread.Free;
  end;
end;

// O mesmo cenario do teste acima, porem encerrando por THorseInstance em vez do
// facade estatico THorse. O caminho Multi-Instance tem a sua propria
// implementacao de StopListenGraceful e por isso precisa da sua propria
// cobertura: ate 2026-09 somente o caminho estatico era testado, e nesse periodo
// THorseInstance.StopListenGraceful chamava StopListen (parada imediata) em vez
// de THorseProvider.StopListenGraceful(ATimeoutMS), sem que nada acusasse.
//
// Com o defeito presente a chamada retorna quase instantaneamente e a requisicao
// em andamento e cortada - indistinguivel de um escoamento rapido e bem sucedido
// a menos que se meca a duracao e se verifique a resposta.
procedure TTestIntegrationGracefulShutdown.TestGracefulShutdownSuccessOnInstance;
var
  LInstance: THorseInstance;
  LServerThread: TThread;
  LClientThread: TThread;
  LClient: THTTPClient;
  LRes: IHTTPResponse;
  LStart: Int64;
  LDuration: Int64;
  LResponseError: string;
  LWaitStart: Int64;
  LContent: string;
  LStatusCode: Integer;
begin
  ClearGlobalState;

  LInstance := THorseInstance.Create;

  // Registrar endpoint lento NA INSTANCIA
  LInstance.Get('/slow-request',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      TThread.Sleep(1500);
      Res.Send('slow-response-ok');
    end);

  LServerThread := TThread.CreateAnonymousThread(
    procedure
    begin
      LInstance.Listen(TEST_PORT_INSTANCE);
    end);
  LServerThread.FreeOnTerminate := False;
  LServerThread.Start;

  TThread.Sleep(500);

  LResponseError := '';
  LContent := '';
  LStatusCode := 0;

  LStart := TThread.GetTickCount;
  LClientThread := TThread.CreateAnonymousThread(
    procedure
    var
      LHeaders: TNetHeaders;
    begin
      LClient := THTTPClient.Create;
      try
        SetLength(LHeaders, 1);
        LHeaders[0] := TNetHeader.Create('Connection', 'close');

        try
          LRes := LClient.Get('http://localhost:' + TEST_PORT_INSTANCE.ToString + '/slow-request', TStream(nil), LHeaders);
          LStatusCode := LRes.StatusCode;
          LContent := LRes.ContentAsString;
        except
          on E: Exception do
            LResponseError := E.Message;
        end;
      finally
        LClient.Free;
      end;
    end);
  LClientThread.FreeOnTerminate := False;
  LClientThread.Start;

  try
    // Aguarda deterministicamente a requisicao entrar em processamento.
    // THorseCore expoe a telemetria como propriedade (THorse.ActiveRequests),
    // mas THorseInstance expoe apenas o getter publico GetActiveRequests - dai
    // a diferenca de chamada entre este teste e o anterior.
    LWaitStart := TThread.GetTickCount;
    while (LInstance.GetActiveRequests = 0) and (TThread.GetTickCount - LWaitStart < 3000) do
    begin
      TThread.Sleep(10);
    end;

    // Desligamento suave PELA INSTANCIA
    LInstance.StopListenGraceful(4000);

    LClientThread.WaitFor;

    LDuration := TThread.GetTickCount - LStart;

    if LResponseError <> '' then
      Assert.Fail('Falha na requisicao lenta: ' + LResponseError);

    Assert.AreEqual(200, LStatusCode, 'O codigo de retorno deve ser 200 (OK)');
    Assert.AreEqual('slow-response-ok', LContent, 'O conteudo da resposta deve ter sido escoado');
    // A assercao que detecta o defeito: um StopListen imediato retorna em
    // milissegundos, enquanto o escoamento real aguarda a requisicao terminar.
    Assert.IsTrue(LDuration >= 1500, 'A duracao total deve refletir o escoamento da requisicao em andamento');

    LServerThread.WaitFor;
    Assert.IsFalse(LInstance.Running, 'A instancia deve estar desligada');
  finally
    LClientThread.Free;
    // TThread.Destroy faz Terminate + WaitFor quando FreeOnTerminate e False,
    // entao ao chegar na linha seguinte a thread de escuta ja terminou e a
    // instancia pode ser liberada com seguranca.
    LServerThread.Free;
    // Diferente do teste anterior, que usa o facade global THorse, aqui a
    // instancia e criada pelo teste e portanto pertence a ele: sem este Free o
    // runner acusa "Unexpected Memory Leak" ao encerrar.
    LInstance.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationGracefulShutdown);

end.
