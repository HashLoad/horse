---
name: horse-multi-instance
description: Guidelines for instantiating and configuring independent Horse server instances (THorseInstance) running concurrently on different ports.
---

# Horse Multi-Instance

## Decoupled Server Contexts
Use `THorseInstance` when your application needs to expose multiple separate HTTP interfaces (e.g., a public REST API on port `8080` and an admin/telemetry dashboard on port `9090`) within the same process.

Each `THorseInstance` allocates its own isolated:
- Routing tables
- Middleware pipelines
- Active requests telemetry counters
- Lifecycle hooks (`onRequest`, `preParsing`, `preValidation`, `onSend`, `onResponse`)

---

## Declaring and Configuring Instances
Always create and configure `THorseInstance` explicitly, registering endpoints and middlewares directly on the instance:

```pascal
var
  FPublicApi: THorseInstance;
  FAdminPanel: THorseInstance;
begin
  // 1. Public API configuration
  FPublicApi := THorseInstance.Create;
  FPublicApi.Use(Jhonson); // Middleware registered only for FPublicApi
  FPublicApi.Get('/api/v1/ping', DoPingHandler);

  // 2. Admin Panel configuration
  FAdminPanel := THorseInstance.Create;
  FAdminPanel.Get('/admin/metrics', DoMetricsHandler);
end;
```

---

## Starting Listeners Concurrently
Since listeners enter blocking execution loops (depending on the provider and application shape), you must start the physical socket listening in separate threads:

```pascal
// Start Instance 1 in a background thread
TThread.CreateAnonymousThread(
  procedure
  begin
    FPublicApi.Listen(9001);
  end).Start;

// Start Instance 2 in a background thread
TThread.CreateAnonymousThread(
  procedure
  begin
    FAdminPanel.Listen(9002);
  end).Start;
```

---

## Stopping Servers and Freeing Resources
When shutting down, stop listening on each instance before freeing the objects:

```pascal
FPublicApi.StopListen;
FAdminPanel.StopListen;

FPublicApi.Free;
FAdminPanel.Free;
```

---

## Design Safeguards & AI Best Practices
1. **Never write static vars inside local middlewares**: Ensure any middleware used inside a `THorseInstance` relies on instance-specific configuration or request-scoped services (`Req.Services`).
2. **Backward Compatibility**: If the project does not require multiple instances, continue using the static `THorse` facade class. It is automatically routed to a default instance under the hood.
3. **FPC/Lazarus Compatibility**: When writing samples or libraries for Lazarus, avoid Delphi anonymous procedures inline (`procedure begin end`) for route handlers. Use standard global procedures instead.
