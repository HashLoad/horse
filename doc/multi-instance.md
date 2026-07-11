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

To run multiple servers concurrently without manual threads, you can disable the console-blocking loop by setting `IsConsole := False` and managing the shutdown with a `Readln`:

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
  // Disable the console-blocking loop to let Listen() methods return immediately
  IsConsole := False;

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

  // 3. Start listening (both return control immediately)
  FPublicApi.Listen(9001);
  FAdminPanel.Listen(9002);

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

## 🧵 Sockets, Blocking Execution & Thread Management

The Horse physical transport providers (Indy, IOCP, HttpSys, Epoll) are internally multithreaded. When you invoke `Listen()`, the server spawns background thread pools to listen for incoming connections and handle requests asynchronously.

The caller thread (e.g., Main Thread) only blocks inside `Listen()` when `IsConsole := True` is set. This is a didactical safety mechanism designed to prevent console programs from exiting scope and closing immediately.

Depending on your application type, you can control thread execution in three ways:

### A. Non-Console Applications (VCL, LCL, Windows Services, Linux Daemons)
In GUI or background service applications, `IsConsole` is naturally `False`. Thus, calling `Listen()` on multiple instances **never blocks** the UI thread or calling thread. You can safely call `Listen` sequentially on the main thread:
```pascal
FInstance1.Listen(9001); // Returns control immediately
FInstance2.Listen(9002); // Returns control immediately
```

### B. Console Applications (Deactivating Block)
As shown in the basic usage above, setting `IsConsole := False` at the program startup disables the console-blocking loop. Both instances start their socket threads in background and return immediately to the main execution flow, letting you wait for keyboard input using `Readln`.

### C. Hybrid Thread Approach (Keeping Console Block)
If you want to keep `IsConsole := True` and let the console block naturally on the socket execution event, you can spawn background threads for all secondary instances, and run the last instance on the main thread:
```pascal
// Start Instance 1 in a background thread
TThread.CreateAnonymousThread(
  procedure
  begin
    FPublicApi.Listen(9001);
  end).Start;

// Start Instance 2 on the Main Thread (blocks execution and keeps console open)
FAdminPanel.Listen(9002);
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

### 🌐 Provider and Router Compatibility Matrix

The Multi-Instance architecture is fully compatible with all official routers and transport providers in the Horse ecosystem. However, depending on the network transport, physical behavior varies:

#### 1. Routers (Radix Router vs. Classic Router)
**Compatibility: 100% (Agnostic)**
The logical routing pipeline (both the default linear router and the high-performance Radix Tree router — `HORSE_RADIX_ROUTER`) is decoupled from the physical transport layer. Each `THorseInstance` manages its own isolated route tree in memory.

#### 2. Physical Providers (Standalone Sockets vs. Managed Web Servers)
The following matrix highlights the multi-port concurrent listening behavior across transport providers:

| Category | Providers | Listens on Distinct Physical Ports concurrently? | Architectural Behavior |
| :--- | :--- | :---: | :--- |
| **High Performance / Async** | `CrossSocket`, `mORMot2`, `HttpSys` | ✔️ Yes | Each `THorseInstance` spawns and manages its own isolated OS-level socket. |
| **Classic / Monolithic** | `Indy` (Console/VCL/Daemon), `fphttpserver` (LCL/Daemon/HTTPApplication) | ✔️ Yes | A global shared listener manages multiple network bindings transparently for all registered logical instances. |
| **Hosted / Managed** | `IIS` (ISAPI), `Apache` (Module), `CGI` / `FastCGI` | Not applicable | The external host server (IIS/Apache) owns the physical sockets and ports, forwarding incoming requests to `THorseWebModule` with the correct `Request.ServerPort` header. Horse routes them logically to the corresponding instance perfectly. |

---

## ⚙️ Backward Compatibility

All existing APIs and legacy code relying on the static facade class `THorse` continue to compile and function without changes. Under the hood, all class-level operations on `THorse` (like `THorse.Get`, `THorse.Use`, `THorse.Listen`) are automatically forwarded to a global, thread-safe default instance of `THorseInstance`.
