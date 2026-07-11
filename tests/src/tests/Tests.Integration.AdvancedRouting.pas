unit Tests.Integration.AdvancedRouting;

interface

uses
  DUnitX.TestFramework, Horse, Horse.Commons, System.SysUtils, System.Classes,
  System.Threading, System.Net.HttpClient, Tests.CleanupHelper;

type
  [TestFixture]
  TTestIntegrationAdvancedRouting = class
  private
    FMatchedRoute: string;
    FParamId: string;
    const TEST_PORT = 9095;
    procedure RunRoutingTest(const AUseRadix: Boolean);
  public
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestTreeRouterAdvancedRouting;
    [Test]
    procedure TestRadixRouterAdvancedRouting;
  end;

implementation

uses
  Horse.Core.RouterTree,
  Horse.Core.Router.Radix;

{ TTestIntegrationAdvancedRouting }

procedure TTestIntegrationAdvancedRouting.TearDown;
begin
  THorse.ResetHooks;
  ClearGlobalState;
end;

procedure TTestIntegrationAdvancedRouting.TestTreeRouterAdvancedRouting;
begin
  RunRoutingTest(False);
end;

procedure TTestIntegrationAdvancedRouting.TestRadixRouterAdvancedRouting;
begin
  RunRoutingTest(True);
end;

procedure TTestIntegrationAdvancedRouting.RunRoutingTest(const AUseRadix: Boolean);
var
  LClient: THTTPClient;
  LRes: IHTTPResponse;
  LThread: TThread;
begin
  FMatchedRoute := '';
  FParamId := '';

  // 1. Chaveia o Roteador sob teste
  if AUseRadix then
    THorse.Routes := THorseRadixRouter.Create
  else
    THorse.Routes := THorseRouterTree.Create;

  // 2. Registro das Rotas Avançadas
  
  // Rota Estática de Alta Precedência
  THorse.Get('/users/new',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      FMatchedRoute := 'static-new';
      FParamId := '';
      Res.Send('new-user');
    end);

  // Rota com Restrição de Regex (Apenas números decimais)
  THorse.Get('/users/:id(\d+)',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      FMatchedRoute := 'regex-id';
      FParamId := Req.Params.Items['id'];
      Res.Send('user-numeric');
    end);

  // Rota com Parâmetro Opcional (Aceita texto ou vazio)
  THorse.Get('/users/:id?',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      FMatchedRoute := 'optional-id';
      FParamId := Req.Params.Items['id'];
      Res.Send('user-optional');
    end);

  // Inicia o Servidor em Background
  LThread := TThread.CreateAnonymousThread(
    procedure
    begin
      THorse.Listen(TEST_PORT);
    end);
  LThread.Start;
  Sleep(800); // Aguarda o bind físico da porta

  LClient := THTTPClient.Create;
  try
    try
      // Caso 1: Rota Estática (/users/new)
      LRes := LClient.Get(Format('http://localhost:%d/users/new', [TEST_PORT]));
      Assert.AreEqual(200, LRes.StatusCode);
      Assert.AreEqual('new-user', LRes.ContentAsString);
      Assert.AreEqual('static-new', FMatchedRoute);

      // Caso 2: Rota Paramétrica Numérica com Regex (/users/123)
      LRes := LClient.Get(Format('http://localhost:%d/users/123', [TEST_PORT]));
      Assert.AreEqual(200, LRes.StatusCode);
      Assert.AreEqual('user-numeric', LRes.ContentAsString);
      Assert.AreEqual('regex-id', FMatchedRoute);
      Assert.AreEqual('123', FParamId);

      // Caso 3: Rota com Parâmetro Opcional Texto (/users/abc)
      LRes := LClient.Get(Format('http://localhost:%d/users/abc', [TEST_PORT]));
      Assert.AreEqual(200, LRes.StatusCode);
      Assert.AreEqual('user-optional', LRes.ContentAsString);
      Assert.AreEqual('optional-id', FMatchedRoute);
      Assert.AreEqual('abc', FParamId);

      // Caso 4: Rota com Parâmetro Opcional Vazio (/users)
      LRes := LClient.Get(Format('http://localhost:%d/users', [TEST_PORT]));
      Assert.AreEqual(200, LRes.StatusCode);
      Assert.AreEqual('user-optional', LRes.ContentAsString);
      Assert.AreEqual('optional-id', FMatchedRoute);
      Assert.AreEqual('', FParamId);

      // Caso 5: Roteamento de Regex que não deve coincidir se houver outras restrições
      // Por exemplo, /users/123/edit não deve dar match em nenhuma destas rotas (deve dar 404)
      LRes := LClient.Get(Format('http://localhost:%d/users/123/edit', [TEST_PORT]));
      Assert.AreEqual(404, LRes.StatusCode);

    finally
      THorse.StopListen;
      Sleep(500); // Aguarda liberação física da porta
    end;
  finally
    LClient.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationAdvancedRouting);

end.
