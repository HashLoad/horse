---
name: horse-telemetry-observability
description: Guidelines for registering, executing, and optimizing native high-precision telemetry hooks (AddOnTelemetry) in the Horse Web Framework.
---

# Horse Telemetry & Observability

## Native Telemetry Hook (AddOnTelemetry)
Horse provides a high-precision, native, and *Zero-Allocation* telemetry hook based on stack-allocated `TStopwatch`. This hook allows monitoring and logging the total processing latency of all requests passing through the server pipeline.

The telemetry callback is defined as follows:
```pascal
THorseOnTelemetry = {$IF DEFINED(FPC)}procedure{$ELSE}reference to procedure{$ENDIF}(const ARequest: THorseRequest; const AResponse: THorseResponse; const AExecutionTimeMS: Double);
```

---

## Registering the Telemetry Hook

### 1. Global Registration
For applications using the standard static `THorse` facade, register the telemetry hook globally during the bootstrap process:

```pascal
uses
  Horse, System.SysUtils;

begin
  THorse.AddOnTelemetry(
    procedure(const Req: THorseRequest; const Res: THorseResponse; const ExecutionTimeMS: Double)
    begin
      Writeln(Format('[Telemetry] %s %s - Status: %d - Latency: %.2f ms', 
        [Req.Method, Req.PathInfo, Res.Status, ExecutionTimeMS]));
    end);

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  THorse.Listen(9000);
end.
```

### 2. Multi-Instance Registration
For applications utilizing `THorseInstance`, register telemetry hooks directly on each instance. Telemetry is fully isolated per port and polymorphically resolved based on the incoming request port:

```pascal
var
  LInstance1, LInstance2: THorseInstance;
begin
  LInstance1 := THorseInstance.Create;
  LInstance1.AddOnTelemetry(
    procedure(const Req: THorseRequest; const Res: THorseResponse; const ExecutionTimeMS: Double)
    begin
      Writeln(Format('[Instance 1 - Port %d] Latency: %.2f ms', [Req.RawWebRequest.ServerPort, ExecutionTimeMS]));
    end);

  LInstance2 := THorseInstance.Create;
  LInstance2.AddOnTelemetry(
    procedure(const Req: THorseRequest; const Res: THorseResponse; const ExecutionTimeMS: Double)
    begin
      Writeln(Format('[Instance 2 - Port %d] Latency: %.2f ms', [Req.RawWebRequest.ServerPort, ExecutionTimeMS]));
    end);
end;
```

---

## Design Safeguards & AI Best Practices

1. **Catch Internal Exceptions**: Always wrap the code inside custom telemetry callbacks with a `try-except` block (or rely on Horse's native try-except boundary) to ensure failures during metrics collecting (like database logging or APM networking errors) never interrupt the HTTP response loop or crash the socket execution thread.
2. **Zero-Allocation Logging**: To maintain Horse's zero-allocation characteristics, avoid dynamic heap allocations (such as concatenating strings or creating new logger objects) inside the telemetry callback. Prefer reusing static buffers or writing to stack-allocated variables.
3. **Multi-Instance Port Resolution**: Always use `Req.RawWebRequest.ServerPort` if you need to determine the active port of the incoming request dynamically inside the telemetry handler.
4. **FPC/Lazarus Compatibility**: In FPC (Lazarus), the callback type is a standard procedural pointer. Do not use inline anonymous methods (`procedure begin end`) when compiling libraries or handlers for Lazarus.
