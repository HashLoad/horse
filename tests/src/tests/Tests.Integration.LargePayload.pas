unit Tests.Integration.LargePayload;

interface

uses
  DUnitX.TestFramework, Horse, Horse.Commons, System.SysUtils, System.Classes,
  System.Threading, System.Net.HttpClient, Tests.CleanupHelper;

type
  [TestFixture]
  TTestIntegrationLargePayload = class
  private
    const TEST_PORT = 9099;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;

    [Test]
    procedure TestLargePayloadHeapRelease;
  end;

implementation

{ TTestIntegrationLargePayload }

procedure TTestIntegrationLargePayload.SetupFixture;
begin
  THorse.Post('/upload-large',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Req.Body;
      Res.Send('received');
    end);

  TThread.CreateAnonymousThread(
    procedure
    begin
      THorse.Listen(TEST_PORT);
    end).Start;

  Sleep(1500);
end;

procedure TTestIntegrationLargePayload.TearDownFixture;
begin
  ClearGlobalState;
  Sleep(500);
end;

procedure TTestIntegrationLargePayload.TestLargePayloadHeapRelease;
var
  LClient: THTTPClient;
  LRes: IHTTPResponse;
  LPayload: string;
  LMemStart, LMemEnd: Int64;
  LStream: TStringStream;
begin
  LMemStart := AllocMemSize;

  LPayload := StringOfChar('A', 10 * 1024 * 1024);
  LStream := TStringStream.Create(LPayload);
  LClient := THTTPClient.Create;
  try
    LRes := LClient.Post(Format('http://localhost:%d/upload-large', [TEST_PORT]), LStream);
    Assert.AreEqual(200, LRes.StatusCode, 'HTTP status should be 200 OK');
    Assert.AreEqual('received', LRes.ContentAsString);
    
    LStream.Free;
    LStream := nil;
    LPayload := '';
    
    Sleep(300);
    
    LMemEnd := AllocMemSize;
    
    // Tolerancia de 1.5MB para flutuacoes normais da RTL e sockets do Indy
    Assert.IsTrue((LMemEnd - LMemStart) < (1536 * 1024), 
      Format('Memory leak detected! Heap grew by %d bytes after large payload.', [LMemEnd - LMemStart]));
  finally
    if Assigned(LStream) then
      LStream.Free;
    LClient.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationLargePayload);

end.
