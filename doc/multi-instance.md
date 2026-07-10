# Multi-Instance

*Read this in [English](./multi-instance.md) or [Português (BR)](./multi-instance.pt-BR.md).*

The **Multi-Instance** architecture in Horse allows developers to instantiate, configure, and execute multiple independent servers concurrently within the same application process.

This feature decouples routing tables, middleware pipelines, and lifecycle hooks from the global class-level state (`class var` singletons) into isolated `THorseInstance` objects.

---

## 🗺️ Visual Architecture

A single process can run multiple HTTP interfaces. Each instance maps to a specific local port and resolves its own route execution path independently:

```mermaid
graph TD
    subgraph Multi-Instance Architecture (Isolated Environments)
        Port9001[Port 9001 - Public API] -->|Resolves to| Instance1[THorseInstance 1]
        Port9002[Port 9002 - Admin & Metrics] -->|Resolves to| Instance2[THorseInstance 2]

        Instance1 -->|Uses| Routes1[Router Tree 1]
        Instance2 -->|Uses| Routes2[Router Tree 2]
    end

    subgraph Legacy Facade (Backward Compatibility)
        THorse[THorse Static Facade] -->|Delegates to| DefaultInstance[Default Instance]
    end
```

---

## 🚀 Basic Usage

To run multiple servers, instantiate `THorseInstance` and call their respective setup methods:

```pascal
program MultiInstanceConsole;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  Horse;

var
  FPublicApi: THorseInstance;
  FAdminPanel: THorseInstance;

begin
  // 1. Configure the first instance (Public API)
  FPublicApi := THorseInstance.Create;
  FPublicApi.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send('Pong from Public API');
    end);

  // 2. Configure the second instance (Admin Panel)
  FAdminPanel := THorseInstance.Create;
  FAdminPanel.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send('Pong from Admin Panel');
    end);

  // 3. Start listening in separate threads
  TThread.CreateAnonymousThread(
    procedure
    begin
      FPublicApi.Listen(9001);
    end).Start;

  TThread.CreateAnonymousThread(
    procedure
    begin
      FAdminPanel.Listen(9002);
    end).Start;

  Writeln('Servers are listening on ports 9001 and 9002.');
  Writeln('Press [ENTER] to exit...');
  Readln;

  // 4. Shutdown and free instances
  FPublicApi.StopListen;
  FAdminPanel.StopListen;

  FPublicApi.Free;
  FAdminPanel.Free;
end.
```

---

## ⚡ How Instance Resolution Works Under the Hood

When an incoming HTTP request is received by the physical server provider (e.g. Indy, IOCP, Http.sys, Epoll), the framework extracts the target destination port from the request headers (`Request.ServerPort`).

The `THorseWebModule` processes the request and resolves the execution context:

1. Calls the global `GetHorseInstanceByPort(LPort)` function to search for an active `THorseInstance` registered on that port.
2. If a matching instance is found, it increments active requests and executes the middleware and routing pipeline registered **specifically** for that instance:
   ```pascal
   LCore := GetHorseInstanceByPort(LPort);
   if LCore <> nil then
     LCore.GetRoutes.Execute(LRequest, LResponse)
   ```
3. If no instance is registered on that port, the execution falls back to the static `THorseCore` global routes.

---

## ⚙️ Backward Compatibility

All existing APIs and legacy code relying on the static facade class `THorse` continue to compile and function without changes. Under the hood, all class-level operations on `THorse` (like `THorse.Get`, `THorse.Use`, `THorse.Listen`) are automatically forwarded to a global, thread-safe default instance of `THorseInstance`.
