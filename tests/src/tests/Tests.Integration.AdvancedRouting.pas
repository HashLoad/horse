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
  LPreviousCaseSensitive: Boolean;
begin
  FMatchedRoute := '';
  FParamId := '';
  LPreviousCaseSensitive := THorse.CaseSensitive;
  THorse.CaseSensitive := False;

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

  // Registra primeiro a rota parametrizada para provar que a literal UTF-8
  // continua tendo precedência, independentemente da ordem de registro.
  THorse.Get('/ação/:id',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('utf8-param:' + Req.Params.Items['id']);
    end);

  THorse.Get('/ação/fixo',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('utf8-literal');
    end);

  THorse.Use('/área',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.AddHeader('X-UTF8-Middleware', 'matched');
      Next;
    end);

  THorse.Get('/área/recurso',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('utf8-middleware');
    end);

  THorse.Group.Prefix('/catálogo')
    .Get('/produto/:id',
      procedure(Req: THorseRequest; Res: THorseResponse)
      begin
        Res.Send('utf8-group:' + Req.Params.Items['id']);
      end);

  THorse.Get('/ação/CASE',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('ascii-case-fold');
    end);

  THorse.Get('/encoded/:id/tail',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('encoded-param:' + Req.Params.Items['id']);
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
    LClient.CustomHeaders['Connection'] := 'close';
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

      // Caso 6: UTF-8 bruto e precedência da rota literal sobre :id.
      LRes := LClient.Get(Format('http://localhost:%d/ação/fixo', [TEST_PORT]));
      Assert.AreEqual(200, LRes.StatusCode);
      Assert.AreEqual('utf8-literal', LRes.ContentAsString);

      // Caso 7: URI percent-encoded atravessando o provider e parâmetro UTF-8.
      LRes := LClient.Get(Format(
        'http://localhost:%d/a%%C3%%A7%%C3%%A3o/caf%%C3%%A9', [TEST_PORT]));
      Assert.AreEqual(200, LRes.StatusCode);
      Assert.AreEqual('utf8-param:café', LRes.ContentAsString);

      // Caso 8: middleware registrado em path UTF-8.
      LRes := LClient.Get(Format(
        'http://localhost:%d/%%C3%%A1rea/recurso', [TEST_PORT]));
      Assert.AreEqual(200, LRes.StatusCode);
      Assert.AreEqual('utf8-middleware', LRes.ContentAsString);
      Assert.AreEqual('matched', LRes.HeaderValue['X-UTF8-Middleware']);

      // Caso 9: prefixo de grupo UTF-8.
      LRes := LClient.Get(Format(
        'http://localhost:%d/cat%%C3%%A1logo/produto/7', [TEST_PORT]));
      Assert.AreEqual(200, LRes.StatusCode);
      Assert.AreEqual('utf8-group:7', LRes.ContentAsString);

      // Caso 10: CaseSensitive=False continua dobrando ASCII dentro de UTF-8.
      LRes := LClient.Get(Format(
        'http://localhost:%d/a%%C3%%A7%%C3%%A3o/case', [TEST_PORT]));
      Assert.AreEqual(200, LRes.StatusCode);
      Assert.AreEqual('ascii-case-fold', LRes.ContentAsString);

      // Caso 11: uma barra percent-encoded pertence ao parâmetro, não ao path.
      LRes := LClient.Get(Format(
        'http://localhost:%d/encoded/a%%2Fb/tail', [TEST_PORT]));
      Assert.AreEqual(200, LRes.StatusCode);
      Assert.AreEqual('encoded-param:a/b', LRes.ContentAsString);

      // Caso 12: o path e o parâmetro são decodificados exatamente uma vez.
      LRes := LClient.Get(Format(
        'http://localhost:%d/encoded/a%%252Fb/tail', [TEST_PORT]));
      Assert.AreEqual(200, LRes.StatusCode);
      Assert.AreEqual('encoded-param:a%2Fb', LRes.ContentAsString);

    finally
      THorse.StopListen;
      Sleep(500); // Aguarda liberação física da porta
      THorse.CaseSensitive := LPreviousCaseSensitive;
    end;
  finally
    LClient.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationAdvancedRouting);

end.
