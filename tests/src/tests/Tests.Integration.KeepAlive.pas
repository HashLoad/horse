unit Tests.Integration.KeepAlive;

interface

uses
  DUnitX.TestFramework, Horse, Horse.Commons, System.SysUtils, System.Classes,
  System.Threading, IdHTTP, Tests.CleanupHelper;

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
  LClient: TIdHTTP;
  LContent: string;
  LConnectionHeader: string;
  I: Integer;
begin
  LClient := TIdHTTP.Create(nil);
  try
    LClient.Request.Connection := 'keep-alive';
    for I := 1 to 3 do
    begin
      LContent := LClient.Get(Format('http://localhost:%d/ping', [TEST_PORT]));
      Assert.AreEqual(200, LClient.ResponseCode, 'HTTP status should be 200 OK');
      Assert.AreEqual('pong', LContent);

      LConnectionHeader := LClient.Response.RawHeaders.Values['Connection'];
      if LConnectionHeader <> '' then
        Assert.IsFalse(SameText(LConnectionHeader, 'close'),
          'Server should not force connection close under keep-alive');
    end;
  finally
    LClient.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationKeepAlive);

end.
