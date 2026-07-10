unit Tests.Integration.MultiInstance;

interface

uses
  DUnitX.TestFramework, Horse, Horse.Commons, System.SysUtils, System.Classes,
  System.Threading, System.Net.HttpClient, Tests.CleanupHelper;

type
  [TestFixture]
  TTestIntegrationMultiInstance = class
  private
    const PORT_1 = 9081;
    const PORT_2 = 9082;
    var
      FInstance1: THorseInstance;
      FInstance2: THorseInstance;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;

    [Test]
    procedure TestMultiInstanceRoutingIsolation;
  end;

implementation

{ TTestIntegrationMultiInstance }

procedure TTestIntegrationMultiInstance.SetupFixture;
begin
  FInstance1 := THorseInstance.Create;
  FInstance1.Get('/port1',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send('response_from_port_1');
    end);

  FInstance2 := THorseInstance.Create;
  FInstance2.Get('/port2',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send('response_from_port_2');
    end);
end;

procedure TTestIntegrationMultiInstance.TearDownFixture;
begin
  if Assigned(FInstance1) then
  begin
    FInstance1.Free;
    FInstance1 := nil;
  end;
  if Assigned(FInstance2) then
  begin
    FInstance2.Free;
    FInstance2 := nil;
  end;
  ClearGlobalState;
end;

procedure TTestIntegrationMultiInstance.TestMultiInstanceRoutingIsolation;
var
  LClient: THTTPClient;
  LRes: IHTTPResponse;
  LThread: TThread;
begin
  LClient := THTTPClient.Create;
  try
    // --- PARTE 1: Testes na Porta 1 ---
    LThread := TThread.CreateAnonymousThread(
      procedure
      begin
        FInstance1.Listen(PORT_1);
      end);
    LThread.Start;
    Sleep(800); // Aguarda o bind da porta 1

    try
      // Teste 1: Rota Correta na Porta 1
      LRes := LClient.Get(Format('http://localhost:%d/port1', [PORT_1]));
      Assert.AreEqual(200, LRes.StatusCode, 'Port 1 response should be 200 OK');
      Assert.AreEqual('response_from_port_1', LRes.ContentAsString);

      // Teste 2: Rota da Porta 2 chamada na Porta 1 (deve dar 404)
      LRes := LClient.Get(Format('http://localhost:%d/port2', [PORT_1]));
      Assert.AreEqual(404, LRes.StatusCode, 'Accessing port2 route on port1 should return 404 Not Found');
    finally
      FInstance1.StopListen;
      Sleep(500); // Aguarda a liberação do socket pelo Windows
    end;

    // --- PARTE 2: Testes na Porta 2 ---
    LThread := TThread.CreateAnonymousThread(
      procedure
      begin
        FInstance2.Listen(PORT_2);
      end);
    LThread.Start;
    Sleep(800); // Aguarda o bind da porta 2

    try
      // Teste 3: Rota Correta na Porta 2
      LRes := LClient.Get(Format('http://localhost:%d/port2', [PORT_2]));
      Assert.AreEqual(200, LRes.StatusCode, 'Port 2 response should be 200 OK');
      Assert.AreEqual('response_from_port_2', LRes.ContentAsString);

      // Teste 4: Rota da Porta 1 chamada na Porta 2 (deve dar 404)
      LRes := LClient.Get(Format('http://localhost:%d/port1', [PORT_2]));
      Assert.AreEqual(404, LRes.StatusCode, 'Accessing port1 route on port2 should return 404 Not Found');
    finally
      FInstance2.StopListen;
      Sleep(500); // Aguarda a liberação do socket pelo Windows
    end;

  finally
    LClient.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationMultiInstance);

end.
