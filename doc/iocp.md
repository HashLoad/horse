# IOCP Transport Provider

*Read this in [English](./iocp.md) or [Português (BR)](./iocp.pt-BR.md).*

The **IOCP** Provider is a Windows-native asynchronous HTTP transport driver built directly into the Horse core. It utilizes Windows' **I/O Completion Ports (IOCP)** and the high-performance **Winsock2** API (`AcceptEx` / `GetAcceptExSockaddrs`) to provide extreme scalability and throughput for Windows self-hosted application types (Console, VCL, or Windows Services/Daemons).

---

## ⚡️ Quickstart

To activate the IOCP transport provider in your application:

1. Add the compiler conditional define `HORSE_PROVIDER_IOCP` to your project configuration.
2. Build the project on Windows (Delphi or Lazarus/FPC).

```delphi
program MyServer;

uses
  Horse;

begin
  // (Optional) Enable the high-performance Radix Router
  THorse.UseRadixRouter;

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  THorse.Listen(9000);
end.
```

---

## ⚙️ Architecture and Concurrency Model

The IOCP provider is designed from the ground up for maximum throughput with minimal resource overhead:

*   **Asynchronous Accepts (`AcceptEx`)**: Unlike the traditional Indy provider which blocks a thread waiting for connections, the IOCP provider schedules multiple concurrent asynchronous accept requests using the Windows kernel extension `AcceptEx`. This allows the server to handle bursts of connection attempts instantly.
*   **Asynchronous Event Loop**: A small pool of IOCP worker threads (`TIocpWorkerThread` — typically scaled to `CPUCount * 2`) monitors the Completion Port via the `GetQueuedCompletionStatus` Windows API. These threads sleep when there is no network traffic and wake up immediately when packets arrive.
*   **Windows Thread Pool Dispatch**: Once a full HTTP request header is received and parsed by the zero-allocation scanner, the worker thread delegates the execution of the Horse middleware chain and route handler to the Windows Thread Pool (`TThread.Queue`). This ensures that user code (which may perform blocking database queries or heavy computations) does not block the core IOCP network loop, maintaining high system responsiveness.
*   **Zero External Dependencies**: The provider binds directly to Windows' kernel APIs and Winsock libraries (`ws2_32.dll`), requiring no DLL distributions or external third-party packages.

---

## 📊 Compatibility and Features

| Feature | Support | Note |
|---|:---:|---|
| **OS Platform** | Windows | Requires Windows Vista / Server 2008 or later. |
| **Compilers** | Delphi & Lazarus | Works seamlessly across both Pascal compilers. |
| **Lifecycle Types** | Console, VCL, Daemon | Fully supports self-hosted lifecycle shapes. |
| **Keep-Alive** | ✔ | Connection reuse is natively managed. |
| **Radix Router** | ✔ | Can be combined with `HORSE_RADIX_ROUTER` for maximum speed. |
| **SSL/TLS** | ✘ | For TLS termination on Windows, we recommend placing a reverse proxy (such as IIS, Nginx, or Caddy) in front of the Horse executable, or using the ICS/CrossSocket providers. |
