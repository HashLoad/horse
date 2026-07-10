unit Tests.Integration.DependencyInjection;

interface

uses
  DUnitX.TestFramework, Horse, Horse.Commons, Horse.Core.Context,
  {$IF DEFINED(FPC)}
    SyncObjs,
  {$ELSE}
    System.SyncObjs,
  {$ENDIF}
  System.SysUtils, System.Classes, System.Threading, System.Net.HttpClient, Tests.CleanupHelper;

type
  TTestService = class
  private
    FValue: string;
    class var FActiveCount: Integer;
    class function GetActiveCount: Integer; static;
    class procedure SetActiveCount(const AValue: Integer); static;
  public
    constructor Create;
    destructor Destroy; override;
    property Value: string read FValue write FValue;
    class property ActiveCount: Integer read GetActiveCount write SetActiveCount;
  end;

  [TestFixture]
  TTestIntegrationDependencyInjection = class
  private
    const TEST_PORT = 9099;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;

    [Test]
    procedure TestDirectInstanceInjection;

    [Test]
    procedure TestLazyFactoryInjectionAndCleanup;
  end;

implementation

{ TTestService }

class function TTestService.GetActiveCount: Integer;
begin
  Result := FActiveCount;
end;

class procedure TTestService.SetActiveCount(const AValue: Integer);
begin
  FActiveCount := AValue;
end;

constructor TTestService.Create;
begin
  inherited Create;
  TInterlocked.Increment(FActiveCount);
  FValue := 'default-value';
end;

destructor TTestService.Destroy;
begin
  TInterlocked.Decrement(FActiveCount);
  inherited Destroy;
end;

{ TTestIntegrationDependencyInjection }

procedure TTestIntegrationDependencyInjection.SetupFixture;
begin
end;

procedure TTestIntegrationDependencyInjection.TearDownFixture;
begin
  ClearGlobalState;
end;

procedure TTestIntegrationDependencyInjection.TestDirectInstanceInjection;
var
  LServerThread: TThread;
  LClient: THTTPClient;
  LRes: IHTTPResponse;
begin
  ClearGlobalState;
  TTestService.ActiveCount := 0;

  THorse.Get('/direct',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    var
      LService: TTestService;
      LResolved: TTestService;
    begin
      LService := TTestService.Create;
      LService.Value := 'direct-ok';
      Req.Services.Add(TTestService, LService);
      
      LResolved := TTestService(Req.Services.Resolve(TTestService));
      Res.Send(LResolved.Value);
    end);

  LServerThread := TThread.CreateAnonymousThread(
    procedure
    begin
      THorse.Listen(TEST_PORT);
    end);
  LServerThread.FreeOnTerminate := False;
  LServerThread.Start;

  TThread.Sleep(500);

  LClient := THTTPClient.Create;
  try
    LRes := LClient.Get(Format('http://localhost:%d/direct', [TEST_PORT]));
    Assert.AreEqual(200, LRes.StatusCode);
    Assert.AreEqual('direct-ok', LRes.ContentAsString);
  finally
    LClient.Free;
  end;

  THorse.StopListenGraceful(2000);
  LServerThread.WaitFor;
  LServerThread.Free;

  Assert.AreEqual(0, TTestService.ActiveCount, 'O serviço injetado de forma pertencente deve ter sido destruído');
end;

procedure TTestIntegrationDependencyInjection.TestLazyFactoryInjectionAndCleanup;
var
  LServerThread: TThread;
  LClient: THTTPClient;
  LRes: IHTTPResponse;
  LFactoryCalled: Boolean;
begin
  ClearGlobalState;
  TTestService.ActiveCount := 0;
  LFactoryCalled := False;

  THorse.Get('/lazy',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    var
      LResolved: TTestService;
    begin
      Req.Services.AddFactory(TTestService,
        function: TObject
        begin
          LFactoryCalled := True;
          Result := TTestService.Create;
          TTestService(Result).Value := 'lazy-ok';
        end);
      
      Assert.IsFalse(LFactoryCalled, 'A fábrica não deve ter sido executada antes do Resolve');
      LResolved := TTestService(Req.Services.Resolve(TTestService));
      Assert.IsTrue(LFactoryCalled, 'A fábrica deve ser executada no momento do Resolve');
      Res.Send(LResolved.Value);
    end);

  LServerThread := TThread.CreateAnonymousThread(
    procedure
    begin
      THorse.Listen(TEST_PORT);
    end);
  LServerThread.FreeOnTerminate := False;
  LServerThread.Start;

  TThread.Sleep(500);

  LClient := THTTPClient.Create;
  try
    LRes := LClient.Get(Format('http://localhost:%d/lazy', [TEST_PORT]));
    Assert.AreEqual(200, LRes.StatusCode);
    Assert.AreEqual('lazy-ok', LRes.ContentAsString);
  finally
    LClient.Free;
  end;

  THorse.StopListenGraceful(2000);
  LServerThread.WaitFor;
  LServerThread.Free;

  Assert.AreEqual(0, TTestService.ActiveCount, 'O serviço lazy deve ter sido destruído após o request');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationDependencyInjection);

end.
