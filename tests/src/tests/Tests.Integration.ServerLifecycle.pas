unit Tests.Integration.ServerLifecycle;

interface

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

uses
  DUnitX.TestFramework, Horse, Horse.Commons, System.SysUtils, System.Classes,
  System.Generics.Collections, System.Net.HttpClient, Tests.CleanupHelper;

const
  TEST_PORT = 9085;

type
  [TestFixture]
  TTestIntegrationServerLifecycle = class
  private
    // Contadores de disparo para closures (procs)
    FBeforeListenCount: Integer;
    FAfterListenCount: Integer;
    FBeforeStopCount: Integer;
    FAfterStopCount: Integer;

    // Contadores de disparo para metodos (of object)
    FMethodBeforeListenCount: Integer;
    FMethodAfterListenCount: Integer;
    FMethodBeforeStopCount: Integer;
    FMethodAfterStopCount: Integer;

    FOrderList: TList<string>;
    FThreadFailed: Boolean;

    procedure MethodBeforeListen(const AInstance: THorseInstance);
    procedure MethodAfterListen(const AInstance: THorseInstance);
    procedure MethodBeforeStop(const AInstance: THorseInstance);
    procedure MethodAfterStop(const AInstance: THorseInstance);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestLifecycleEventsExecutionAndOrder;

    [Test]
    procedure TestMulticastEventsRegistration;

    [Test]
    procedure TestBeforeListenExceptionAbortsStartup;

    [Test]
    procedure TestUseStartupConfiguration;
  end;

  // Classe de teste auxiliar que implementa IHorseStartup
  TTestStartup = class(TInterfacedObject, IHorseStartup)
  public
    procedure Configure(const AInstance: THorseInstance);
  end;

implementation

{ TTestStartup }

procedure TAppStartupConfigure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.RawWebResponse.CustomHeaders.Values['X-Startup-Middleware'] := 'active';
  Next();
end;

procedure TTestStartup.Configure(const AInstance: THorseInstance);
begin
  AInstance.Use(TAppStartupConfigure);
  AInstance.Get('/startup-test',
    THorseCallback(
      procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
      begin
        Res.Send('startup_ok');
      end));
end;

{ TTestIntegrationServerLifecycle }

procedure TTestIntegrationServerLifecycle.Setup;
begin
  FBeforeListenCount := 0;
  FAfterListenCount := 0;
  FBeforeStopCount := 0;
  FAfterStopCount := 0;

  FMethodBeforeListenCount := 0;
  FMethodAfterListenCount := 0;
  FMethodBeforeStopCount := 0;
  FMethodAfterStopCount := 0;

  FOrderList := TList<string>.Create;
  FThreadFailed := False;
end;

procedure TTestIntegrationServerLifecycle.TearDown;
begin
  FOrderList.Free;
  ClearGlobalState;
end;

procedure TTestIntegrationServerLifecycle.MethodBeforeListen(const AInstance: THorseInstance);
begin
  Inc(FMethodBeforeListenCount);
  FOrderList.Add('MethodBeforeListen');
end;

procedure TTestIntegrationServerLifecycle.MethodAfterListen(const AInstance: THorseInstance);
begin
  Inc(FMethodAfterListenCount);
  FOrderList.Add('MethodAfterListen');
end;

procedure TTestIntegrationServerLifecycle.MethodBeforeStop(const AInstance: THorseInstance);
begin
  Inc(FMethodBeforeStopCount);
  FOrderList.Add('MethodBeforeStop');
end;

procedure TTestIntegrationServerLifecycle.MethodAfterStop(const AInstance: THorseInstance);
begin
  Inc(FMethodAfterStopCount);
  FOrderList.Add('MethodAfterStop');
end;

procedure TTestIntegrationServerLifecycle.TestLifecycleEventsExecutionAndOrder;
var
  LInstance: THorseInstance;
  LThread: TThread;
begin
  LInstance := THorseInstance.Create;
  LThread := nil;
  try
    // 1. Registra os callbacks via closures
    LInstance
      .AddOnBeforeListen(
        procedure(const AInst: THorseInstance)
        begin
          Inc(FBeforeListenCount);
          FOrderList.Add('BeforeListen');
        end)
      .AddOnAfterListen(
        procedure(const AInst: THorseInstance)
        begin
          Inc(FAfterListenCount);
          FOrderList.Add('AfterListen');
        end)
      .AddOnBeforeStop(
        procedure(const AInst: THorseInstance)
        begin
          Inc(FBeforeStopCount);
          FOrderList.Add('BeforeStop');
        end)
      .AddOnAfterStop(
        procedure(const AInst: THorseInstance)
        begin
          Inc(FAfterStopCount);
          FOrderList.Add('AfterStop');
        end);

    // 2. Registra os callbacks via metodos (of object)
    LInstance
      .AddOnBeforeListen(MethodBeforeListen)
      .AddOnAfterListen(MethodAfterListen)
      .AddOnBeforeStop(MethodBeforeStop)
      .AddOnAfterStop(MethodAfterStop);

    // 3. Inicia e encerra o ciclo de escuta fisica
    LThread := TThread.CreateAnonymousThread(
      procedure
      begin
        LInstance.Listen(TEST_PORT);
      end);
    LThread.FreeOnTerminate := False;
    LThread.Start;
    
    Sleep(800); // Aguarda bind

    Assert.IsTrue(LInstance.Running, 'Server should be running');
    
    LInstance.StopListen;
    
    Sleep(500); // Aguarda libertacao da porta

    Assert.IsFalse(LInstance.Running, 'Server should not be running');

    // 4. Valida contagem dos disparos
    Assert.AreEqual(1, FBeforeListenCount, 'BeforeListen should fire once');
    Assert.AreEqual(1, FAfterListenCount, 'AfterListen should fire once');
    Assert.AreEqual(1, FBeforeStopCount, 'BeforeStop should fire once');
    Assert.AreEqual(1, FAfterStopCount, 'AfterStop should fire once');

    Assert.AreEqual(1, FMethodBeforeListenCount, 'MethodBeforeListen should fire once');
    Assert.AreEqual(1, FMethodAfterListenCount, 'MethodAfterListen should fire once');
    Assert.AreEqual(1, FMethodBeforeStopCount, 'MethodBeforeStop should fire once');
    Assert.AreEqual(1, FMethodAfterStopCount, 'MethodAfterStop should fire once');

    // 5. Valida sequencia de execucao dos eventos
    Assert.IsTrue(FOrderList.Count = 8, 'Should have 8 events logged in total');
    
    // As chamadas multicast executam na ordem de registro: primeiro closure, depois metodo of object
    Assert.AreEqual('BeforeListen', FOrderList[0]);
    Assert.AreEqual('MethodBeforeListen', FOrderList[1]);
    
    Assert.AreEqual('AfterListen', FOrderList[2]);
    Assert.AreEqual('MethodAfterListen', FOrderList[3]);
    
    Assert.AreEqual('BeforeStop', FOrderList[4]);
    Assert.AreEqual('MethodBeforeStop', FOrderList[5]);
    
    Assert.AreEqual('AfterStop', FOrderList[6]);
    Assert.AreEqual('MethodAfterStop', FOrderList[7]);
  finally
    if Assigned(LThread) then
    begin
      LThread.WaitFor;
      LThread.Free;
    end;
    LInstance.Free;
  end;
end;

procedure TTestIntegrationServerLifecycle.TestMulticastEventsRegistration;
var
  LInstance: THorseInstance;
  LThread: TThread;
  LMultiBefore1: Integer;
  LMultiBefore2: Integer;
  LCallback: THorseServerLifecycleProc;
begin
  LInstance := THorseInstance.Create;
  LThread := nil;
  try
    LMultiBefore1 := 0;
    LMultiBefore2 := 0;

    LCallback := procedure(const AInst: THorseInstance)
      begin
        Inc(LMultiBefore1);
      end;
    LInstance.AddOnBeforeListen(LCallback);

    LCallback := procedure(const AInst: THorseInstance)
      begin
        Inc(LMultiBefore2);
      end;
    LInstance.AddOnBeforeListen(LCallback);

    LThread := TThread.CreateAnonymousThread(
      procedure
      begin
        LInstance.Listen(TEST_PORT);
      end);
    LThread.FreeOnTerminate := False;
    LThread.Start;
    
    Sleep(800);

    LInstance.StopListen;
    
    Sleep(500);

    Assert.AreEqual(1, LMultiBefore1, 'First multicast handler should fire once');
    Assert.AreEqual(1, LMultiBefore2, 'Second multicast handler should fire once');
  finally
    if Assigned(LThread) then
    begin
      LThread.WaitFor;
      LThread.Free;
    end;
    LInstance.Free;
  end;
end;

procedure TTestIntegrationServerLifecycle.TestBeforeListenExceptionAbortsStartup;
var
  LInstance: THorseInstance;
  LThread: TThread;
  LCallback: THorseServerLifecycleProc;
begin
  LInstance := THorseInstance.Create;
  LThread := nil;
  try
    LCallback := procedure(const AInst: THorseInstance)
      begin
        raise Exception.Create('Simulated DB connection failure');
      end;
    LInstance.AddOnBeforeListen(LCallback);

    LThread := TThread.CreateAnonymousThread(
      procedure
      begin
        try
          LInstance.Listen(TEST_PORT);
        except
          on E: Exception do
          begin
            FThreadFailed := True;
          end;
        end;
      end);
    LThread.FreeOnTerminate := False;
    LThread.Start;
    
    Sleep(500); // Espera tentar subir

    Assert.IsFalse(LInstance.Running, 'Server must NOT be running after OnBeforeListen failure');
    Assert.AreEqual(0, FAfterListenCount, 'AfterListen must NOT be called on failure');
    Assert.IsTrue(FThreadFailed, 'OnBeforeListen failure exception must propagate and be caught in thread');
  finally
    if Assigned(LThread) then
    begin
      LThread.WaitFor;
      LThread.Free;
    end;
    LInstance.Free;
  end;
end;

procedure TTestIntegrationServerLifecycle.TestUseStartupConfiguration;
var
  LInstance: THorseInstance;
  LThread: TThread;
  LClient: THTTPClient;
  LRes: IHTTPResponse;
  LStartup: IHorseStartup;
begin
  LInstance := THorseInstance.Create;
  LClient := THTTPClient.Create;
  LThread := nil;
  try
    // Configura usando a classe Startup
    LStartup := TTestStartup.Create;
    LInstance.UseStartup(LStartup);

    LThread := TThread.CreateAnonymousThread(
      procedure
      begin
        LInstance.Listen(TEST_PORT);
      end);
    LThread.FreeOnTerminate := False;
    LThread.Start;
    
    Sleep(800);

    // Faz requisicao
    LRes := LClient.Get(Format('http://127.0.0.1:%d/startup-test', [TEST_PORT]));
    
    Assert.AreEqual(200, LRes.StatusCode, 'Should return 200 OK');
    Assert.AreEqual('startup_ok', LRes.ContentAsString, 'Should return startup_ok body');
    Assert.AreEqual('active', LRes.HeaderValue['X-Startup-Middleware'], 'Custom header injected by startup middleware should be active');
    
    LInstance.StopListen;
    
    Sleep(500);
  finally
    LClient.Free;
    if Assigned(LThread) then
    begin
      LThread.WaitFor;
      LThread.Free;
    end;
    LInstance.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationServerLifecycle);

end.
