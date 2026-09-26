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

    [Test]
    [TestCase('NoCharset', 'application/json')]
    [TestCase('Utf8Charset', 'application/json; charset=utf-8')]
    [TestCase('QuotedUtf8Charset', 'application/json; charset="UTF-8"')]
    [TestCase('Utf8Alias', 'application/json; charset=utf8')]
    [TestCase('StructuredSuffixAndParameters',
      'application/problem+json; profile="error"; charset = "Utf-8"')]
    procedure TestUtf8JsonBody(const AContentType: string);
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

  THorse.Post('/utf8-body',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send(Req.Body);
    end);

  TThread.CreateAnonymousThread(
    procedure
    begin
      THorse.Listen(TEST_PORT);
    end).Start;

  Sleep(1500);
end;

procedure TTestIntegrationQuery.TestUtf8JsonBody(const AContentType: string);
const
  EXPECTED_BODY = '{"message":"ação João"}';
var
  LClient: THTTPClient;
  LRes: IHTTPResponse;
  LSource: TStringStream;
begin
  LClient := THTTPClient.Create;
  LSource := TStringStream.Create(EXPECTED_BODY, TEncoding.UTF8);
  try
    LClient.CustomHeaders['Content-Type'] := AContentType;
    LClient.CustomHeaders['Connection'] := 'close';
    LRes := LClient.Post(Format('http://127.0.0.1:%d/utf8-body', [TEST_PORT]),
      LSource);

    Assert.AreEqual(200, LRes.StatusCode, LRes.ContentAsString);
    Assert.AreEqual(EXPECTED_BODY, LRes.ContentAsString);
  finally
    LSource.Free;
    LClient.Free;
  end;
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
