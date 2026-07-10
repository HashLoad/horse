unit Tests.Integration.Query;

interface

uses
  DUnitX.TestFramework, Horse, Horse.Commons, System.SysUtils, System.Classes,
  System.Threading, System.Net.HttpClient, System.Net.URLClient, Tests.CleanupHelper;

type
  [TestFixture]
  TTestIntegrationQuery = class
  private
    const TEST_PORT = 9109;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;

    [Test]
    procedure TestQueryMethodWithBody;
  end;

implementation

{ TTestIntegrationQuery }

procedure TTestIntegrationQuery.SetupFixture;
begin
  THorse.Query('/search',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send('QUERY OK: ' + Req.Body);
    end);

  TThread.CreateAnonymousThread(
    procedure
    begin
      THorse.Listen(TEST_PORT);
    end).Start;

  Sleep(1500);
end;

procedure TTestIntegrationQuery.TearDownFixture;
begin
  ClearGlobalState;
  Sleep(500);
end;

procedure TTestIntegrationQuery.TestQueryMethodWithBody;
var
  LClient: THTTPClient;
  LRes: IHTTPResponse;
  LSource: TStringStream;
begin
  LClient := THTTPClient.Create;
  LSource := TStringStream.Create('{"query":"buscar_clientes"}', TEncoding.UTF8);
  try
    LClient.CustomHeaders['Content-Type'] := 'application/json';
    LRes := IHTTPResponse(LClient.Execute('QUERY', Format('http://localhost:%d/search', [TEST_PORT]), LSource));
    
    Assert.AreEqual(200, LRes.StatusCode, 'HTTP status should be 200 OK');
    Assert.AreEqual('QUERY OK: {"query":"buscar_clientes"}', LRes.ContentAsString);
  finally
    LSource.Free;
    LClient.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationQuery);

end.
