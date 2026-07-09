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
end.
```

---

## See Also
- [Middleware Ecosystem](./middleware-ecosystem.md)
- [Writing a Middleware](./writing-middleware.md)
