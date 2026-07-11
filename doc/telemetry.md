# Observability & Telemetry

*Read this in [English](./telemetry.md) or [Português (BR)](./telemetry.pt-BR.md).*

Observability is a critical aspect of running modern APIs in production. Horse supports application monitoring, tracing, and metrics collection through its middleware pipeline. 

By using middleware, you can collect request duration, trace IDs, response codes, and system metrics without cluttering your business logic.

---

## Observability Pillars in Horse

Through third-party integrations, you can set up complete observability for your Delphi and Lazarus applications:

### 1. Distributed Tracing (OpenTelemetry)

Distributed tracing allows you to follow requests as they flow through your architecture. Using the OpenTelemetry standard, you can visualize call graphs, measure database query times, and trace microservice calls.

* **Middleware:** [horse-opentelemetry](https://github.com/regyssilveira/horse-opentelemetry)
* **What it does:** Automatically starts and propagates spans, records requests, injects W3C trace context, and exports trace data to OpenTelemetry collectors (Jaeger, Zipkin, Dynatrace, Datadog, etc.).

### 2. Metrics Collection (Prometheus)

Metrics tell you *how much* and *how fast* your application is running. You can track Request Rates, Error Rates, and Duration percentiles (RED method).

* **Middleware:** [horse-prometheus](https://github.com/regyssilveira/horse-prometheus)
* **What it does:** Tracks metrics internally (counters, histograms, gauges) and exposes an endpoint (typically `/metrics`) in the format expected by the Prometheus scraper.

---

## Quickstart: Setting up Telemetry

To add metrics and tracing to your Horse application, install the packages via Boss:

```sh
boss install regyssilveira/horse-opentelemetry
boss install regyssilveira/horse-prometheus
```

Then, register them in your main file. It is recommended to register observability middlewares early in the middleware pipeline to capture overall response latency and catch any exceptions:

```delphi
uses
  Horse,
  Horse.OpenTelemetry,
  Horse.Prometheus;

begin
  // Register OpenTelemetry first to trace the entire request lifecycle
  THorse.Use(THorseOpenTelemetry.New('my-horse-api'));

  // Register Prometheus to gather request metrics
  THorse.Use(THorsePrometheus.New);

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  THorse.Listen(9000);
end.
```

## Running Admin/Metrics on a Separate Port

In production, it is often a security best practice to expose metrics endpoints (like `/metrics`) only internally. You can run a separate Horse server on a different port solely for administration and metrics collection:

```delphi
uses
  Horse,
  Horse.Prometheus;

begin
  // Main API Server
  THorse.Use(THorsePrometheus.New);
  
  THorse.Get('/api/v1/users', ...);
  // Listen on public port
  THorse.Listen(9000);

  // Metrics scraper endpoint on a private port
  // Note: Running multiple Horse instances is supported.
end;
```

---

## Native Telemetry Hooks

Horse introduces a native telemetry hook of extremely high precision and *Zero-Allocation*, based on `TStopwatch` (stack-allocated).

This feature allows you to monitor and measure the latency of all processed HTTP requests with millisecond precision, enabling easy integration with logging, APM (Application Performance Monitoring) tools, and custom observability collectors.

### Callback Signature

The telemetry callback type is defined as follows:

```delphi
THorseOnTelemetry = {$IF DEFINED(FPC)}procedure{$ELSE}reference to procedure{$ENDIF}(const ARequest: THorseRequest; const AResponse: THorseResponse; const AExecutionTimeMS: Double);
```

### Setting Up the Hook Globally

You can register a global callback that will be triggered at the end of all HTTP requests in the application:

```delphi
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

### Instance Isolation (Multi-Instance)

If your application uses the Multi-Instance architecture (`THorseInstance`), you can register telemetry hooks isolated by port/instance. Horse polymorphically resolves the correct instance associated with the active request:

```delphi
uses
  Horse, System.SysUtils;

var
  LInstance1, LInstance2: THorseInstance;
begin
  LInstance1 := THorseInstance.Create;
  LInstance1.AddOnTelemetry(
    procedure(const Req: THorseRequest; const Res: THorseResponse; const ExecutionTimeMS: Double)
    begin
      Writeln(Format('[Instance 1 - Port %d] Latency: %.2f ms', [Req.RawWebRequest.ServerPort, ExecutionTimeMS]));
    end);
  LInstance1.Get('/service1', ...);

  LInstance2 := THorseInstance.Create;
  LInstance2.AddOnTelemetry(
    procedure(const Req: THorseRequest; const Res: THorseResponse; const ExecutionTimeMS: Double)
    begin
      Writeln(Format('[Instance 2 - Port %d] Latency: %.2f ms', [Req.RawWebRequest.ServerPort, ExecutionTimeMS]));
    end);
  LInstance2.Get('/service2', ...);
end.
```

### Performance Guarantee

* **Zero-Allocation:** Time tracking utilizes `TStopwatch` allocated directly on the thread stack, generating no pressure on the Garbage Collector (FPC/Lazarus) or memory heap allocation stress in Delphi.
* **Security & Isolation:** The telemetry hook is triggered synchronously within the `finally` block of the physical routing, ensuring that the total time captures middlewares, route processing, and any error generated in the pipeline.

---

## See Also
- [Middleware Ecosystem](./middleware-ecosystem.md)
- [Writing a Middleware](./writing-middleware.md)
