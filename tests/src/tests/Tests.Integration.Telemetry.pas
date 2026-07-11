unit Tests.Integration.Telemetry;

interface

uses
  DUnitX.TestFramework, Horse, Horse.Commons, System.SysUtils, System.Classes,
  System.Threading, System.Net.HttpClient, Tests.CleanupHelper;

type
  [TestFixture]
  TTestIntegrationTelemetry = class
  private
    const TEST_PORT = 9091;
    const PORT_INSTANCE_1 = 9092;
    const PORT_INSTANCE_2 = 9093;
  public
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestGlobalTelemetryHook;
    [Test]
    procedure TestMultiInstanceTelemetryIsolation;
    [Test]
    procedure TestTelemetryOnRouteError;
  end;

implementation

uses
  System.Diagnostics;

{ TTestIntegrationTelemetry }

procedure TTestIntegrationTelemetry.TearDown;
begin
  THorse.ResetHooks;
  ClearGlobalState;
end;

procedure TTestIntegrationTelemetry.TestGlobalTelemetryHook;
var
  LClient: THTTPClient;
  LRes: IHTTPResponse;
  LThread: TThread;
  LTelemetryCalled: Boolean;
  LTimeElapsed: Double;
  LStatus: Integer;
begin
  LTelemetryCalled := False;
  LTimeElapsed := 0;
  LStatus := 0;

  // Registrar hook de telemetria global
  THorse.AddOnTelemetry(
    procedure(const Req: THorseRequest; const Res: THorseResponse; const ExecutionTimeMS: Double)
    begin
      LTelemetryCalled := True;
      LTimeElapsed := ExecutionTimeMS;
      LStatus := Res.Status;
    end);

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Sleep(100); // Garante que ExecutionTimeMS será > 0
      Res.Send('pong');
    end);

  LThread := TThread.CreateAnonymousThread(
    procedure
    begin
      THorse.Listen(TEST_PORT);
    end);
  LThread.Start;
  Sleep(800); // Aguarda o bind

  LClient := THTTPClient.Create;
  try
    try
      LRes := LClient.Get(Format('http://localhost:%d/ping', [TEST_PORT]));
      Assert.AreEqual(200, LRes.StatusCode);
      Assert.AreEqual('pong', LRes.ContentAsString);
      
      Sleep(200); // Aguarda callback de telemetria finalizar (executa no finally)
      
      Assert.IsTrue(LTelemetryCalled, 'Global telemetry callback should be called.');
      Assert.IsTrue(LTimeElapsed >= 90.0, Format('Elapsed time (%.2f ms) should be at least ~100ms.', [LTimeElapsed]));
      Assert.AreEqual(200, LStatus, 'Status should be 200.');
    finally
      THorse.StopListen;
      Sleep(500); // Aguarda a liberação do socket pelo Windows
    end;
  finally
    LClient.Free;
  end;
end;

procedure TTestIntegrationTelemetry.TestMultiInstanceTelemetryIsolation;
var
  LClient: THTTPClient;
  LRes: IHTTPResponse;
  LThread: TThread;
  LInstance1, LInstance2: THorseInstance;
  LTelemetryCalled1, LTelemetryCalled2: Boolean;
  LPortCalled1, LPortCalled2: Integer;
begin
  LTelemetryCalled1 := False;
  LTelemetryCalled2 := False;
  LPortCalled1 := 0;
  LPortCalled2 := 0;

  LInstance1 := THorseInstance.Create;
  LInstance1.AddOnTelemetry(
    procedure(const Req: THorseRequest; const Res: THorseResponse; const ExecutionTimeMS: Double)
    begin
      LTelemetryCalled1 := True;
      LPortCalled1 := Req.RawWebRequest.ServerPort;
    end);
  LInstance1.Get('/instance1',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('resp1');
    end);

  LInstance2 := THorseInstance.Create;
  LInstance2.AddOnTelemetry(
    procedure(const Req: THorseRequest; const Res: THorseResponse; const ExecutionTimeMS: Double)
    begin
      LTelemetryCalled2 := True;
      LPortCalled2 := Req.RawWebRequest.ServerPort;
    end);
  LInstance2.Get('/instance2',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('resp2');
    end);

  LClient := THTTPClient.Create;
  try
    // Executa Instância 1
    LThread := TThread.CreateAnonymousThread(
      procedure
      begin
        LInstance1.Listen(PORT_INSTANCE_1);
      end);
    LThread.Start;
    Sleep(800);

    try
      LRes := LClient.Get(Format('http://localhost:%d/instance1', [PORT_INSTANCE_1]));
      Assert.AreEqual(200, LRes.StatusCode);
      Assert.AreEqual('resp1', LRes.ContentAsString);
      
      Sleep(200);
      Assert.IsTrue(LTelemetryCalled1, 'Telemetry 1 should be called');
      Assert.IsFalse(LTelemetryCalled2, 'Telemetry 2 should NOT be called yet');
      Assert.AreEqual(PORT_INSTANCE_1, LPortCalled1);
    finally
      LInstance1.StopListen;
      Sleep(500);
    end;

    // Executa Instância 2
    LThread := TThread.CreateAnonymousThread(
      procedure
      begin
        LInstance2.Listen(PORT_INSTANCE_2);
      end);
    LThread.Start;
    Sleep(800);

    try
      LRes := LClient.Get(Format('http://localhost:%d/instance2', [PORT_INSTANCE_2]));
      Assert.AreEqual(200, LRes.StatusCode);
      Assert.AreEqual('resp2', LRes.ContentAsString);
      
      Sleep(200);
      Assert.IsTrue(LTelemetryCalled2, 'Telemetry 2 should be called');
      Assert.AreEqual(PORT_INSTANCE_2, LPortCalled2);
    finally
      LInstance2.StopListen;
      Sleep(500);
    end;

  finally
    LInstance1.Free;
    LInstance2.Free;
    LClient.Free;
  end;
end;

procedure TTestIntegrationTelemetry.TestTelemetryOnRouteError;
var
  LClient: THTTPClient;
  LRes: IHTTPResponse;
  LThread: TThread;
  LTelemetryCalled: Boolean;
  LStatus: Integer;
begin
  LTelemetryCalled := False;
  LStatus := 0;

  THorse.AddOnTelemetry(
    procedure(const Req: THorseRequest; const Res: THorseResponse; const ExecutionTimeMS: Double)
    begin
      LTelemetryCalled := True;
      LStatus := Res.Status;
    end);

  // Endpoint que lança um erro propositalmente
  THorse.Get('/error',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      raise Exception.Create('Test Exception');
    end);

  LThread := TThread.CreateAnonymousThread(
    procedure
    begin
      THorse.Listen(TEST_PORT);
    end);
  LThread.Start;
  Sleep(800); // Aguarda bind

  LClient := THTTPClient.Create;
  try
    try
      LRes := LClient.Get(Format('http://localhost:%d/error', [TEST_PORT]));
      // O Horse retorna 500 por padrão quando há exceção não tratada e nenhum OnError configurado
      Assert.AreEqual(500, LRes.StatusCode);
      
      Sleep(200);
      Assert.IsTrue(LTelemetryCalled, 'Telemetry callback should be called even on exceptions.');
      Assert.AreEqual(500, LStatus, 'Response status in telemetry should be 500.');
    finally
      THorse.StopListen;
      Sleep(500);
    end;
  finally
    LClient.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationTelemetry);

end.
