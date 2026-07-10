unit Tests.Integration.GracefulShutdown;

interface

uses
  DUnitX.TestFramework, Horse, Horse.Commons,
  System.SysUtils, System.Classes, System.Threading, System.Net.HttpClient,
  System.Net.URLClient, Tests.CleanupHelper;

type
  [TestFixture]
  TTestIntegrationGracefulShutdown = class
  private
    const TEST_PORT = 9098;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;

    [Test]
    procedure TestGracefulShutdownSuccess;
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

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationGracefulShutdown);

end.
