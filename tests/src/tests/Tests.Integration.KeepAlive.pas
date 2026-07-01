unit Tests.Integration.KeepAlive;

interface

uses
  DUnitX.TestFramework, Horse, Horse.Commons, System.SysUtils, System.Classes,
  System.Threading, System.Net.HttpClient, Tests.CleanupHelper;

type
  [TestFixture]
  TTestIntegrationKeepAlive = class
  private
    const TEST_PORT = 9098;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;

    [Test]
    procedure TestKeepAliveHeadersPreserved;
  end;

implementation

{ TTestIntegrationKeepAlive }

procedure TTestIntegrationKeepAlive.SetupFixture;
begin
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send('pong');
    end);

  TThread.CreateAnonymousThread(
    procedure
    begin
      THorse.Listen(TEST_PORT);
    end).Start;

  Sleep(1500);
end;

procedure TTestIntegrationKeepAlive.TearDownFixture;
begin
  ClearGlobalState;
  Sleep(500);
end;

procedure TTestIntegrationKeepAlive.TestKeepAliveHeadersPreserved;
var
  LClient: THTTPClient;
  LRes: IHTTPResponse;
  I: Integer;
begin
  LClient := THTTPClient.Create;
  try
    for I := 1 to 3 do
    begin
      LRes := LClient.Get(Format('http://localhost:%d/ping', [TEST_PORT]));
      Assert.AreEqual(200, LRes.StatusCode, 'HTTP status should be 200 OK');
      Assert.AreEqual('pong', LRes.ContentAsString);
      
      if LRes.ContainsHeader('Connection') then
      begin
        Assert.IsFalse(SameText(LRes.HeaderValue['Connection'], 'close'),
          'Server should not force connection close under keep-alive');
      end;
    end;
  finally
    LClient.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationKeepAlive);

end.
