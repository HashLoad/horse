unit Tests.Integration.HttpMethods;

interface

uses
  DUnitX.TestFramework, Horse, System.SysUtils, System.Classes,
  System.Threading, System.Net.HttpClient, Tests.CleanupHelper,
  {$IF DEFINED(FPC)} HTTPApp {$ELSE} Web.HTTPApp {$ENDIF}, Horse.Commons;

type
  [TestFixture]
  TTestIntegrationHttpMethods = class
  private
    const TEST_PORT = 9097;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;

    [Test]
    procedure TestOptionsMethodInterceptedByMiddleware;
    [Test]
    procedure TestTraceMethodInterceptedByMiddleware;
    [Test]
    procedure TestMethodNotAllowedAllowHeader;
  end;

implementation

{ TTestIntegrationHttpMethods }

procedure TTestIntegrationHttpMethods.SetupFixture;
begin
  // Middleware global para interceptar verbos customizados na string bruta do Request
  THorse.Use(
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      if Req.MethodType = mtAny then
      begin
        if Req.RawWebRequest.Method = 'OPTIONS' then
        begin
          Res.Send('options-interceptor-ok');
          Exit;
        end;
        if Req.RawWebRequest.Method = 'TRACE' then
        begin
          Res.Send('trace-interceptor-ok');
          Exit;
        end;
      end;
      Next();
    end);

  // Rota GET normal usando TNextProc
  THorse.Get('/resource',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send('get-ok');
    end);

  // Rota POST normal usando TNextProc
  THorse.Post('/resource',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send('post-ok');
    end);

  TThread.CreateAnonymousThread(
    procedure
    begin
      THorse.Listen(TEST_PORT);
    end).Start;

  Sleep(1500);
end;

procedure TTestIntegrationHttpMethods.TearDownFixture;
begin
  ClearGlobalState;
  Sleep(500);
end;

procedure TTestIntegrationHttpMethods.TestOptionsMethodInterceptedByMiddleware;
var
  LClient: THTTPClient;
  LRes: IHTTPResponse;
begin
  LClient := THTTPClient.Create;
  try
    LRes := LClient.Options(Format('http://localhost:%d/resource', [TEST_PORT]));
    Assert.AreEqual(200, LRes.StatusCode);
    Assert.AreEqual('options-interceptor-ok', LRes.ContentAsString);
  finally
    LClient.Free;
  end;
end;

procedure TTestIntegrationHttpMethods.TestTraceMethodInterceptedByMiddleware;
var
  LClient: THTTPClient;
  LRes: IHTTPResponse;
begin
  LClient := THTTPClient.Create;
  try
    LRes := LClient.Execute('TRACE', Format('http://localhost:%d/resource', [TEST_PORT])) as IHTTPResponse;
    Assert.AreEqual(200, LRes.StatusCode);
    Assert.AreEqual('trace-interceptor-ok', LRes.ContentAsString);
  finally
    LClient.Free;
  end;
end;

procedure TTestIntegrationHttpMethods.TestMethodNotAllowedAllowHeader;
var
  LClient: THTTPClient;
  LRes: IHTTPResponse;
  LAllow: string;
begin
  LClient := THTTPClient.Create;
  try
    LRes := LClient.Execute('PUT', Format('http://localhost:%d/resource', [TEST_PORT])) as IHTTPResponse;
    Assert.AreEqual(405, LRes.StatusCode);
    LAllow := LRes.HeaderValue['Allow'];
    Assert.IsTrue(LAllow.Contains('GET'));
    Assert.IsTrue(LAllow.Contains('POST'));
  finally
    LClient.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationHttpMethods);

end.
