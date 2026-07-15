# WebSockets in Horse

Horse provides native, high-performance support for bi-directional WebSocket connections (fully compliant with the RFC 6455 specification). The feature is integrated directly into the framework core, operating without any third-party dependencies, providing a fluid API for connection upgrades, thread-safe session management, and broadcasting.

---

## 🚀 How it Works

Unlike traditional approaches that require separate servers or additional ports, Horse's WebSocket support uses the **Port Sharing** model. The protocol upgrade handshake occurs on the same TCP socket and main HTTP port of the application, ensuring ease of management through firewalls and load balancers.

### Compatible Providers
WebSockets support is available for all physical socket-based providers controlled by the application:
* **Indy** (Console, VCL, Daemon)
* **IOCP** (Windows)
* **Epoll** (Linux)
* **FPC** (`fphttpserver`)

### Constraints and Fail-Fast Behavior
Some providers, due to their architectural design, do **not** support direct WebSockets:
* **HttpSys**: The kernel-mode driver (`http.sys`) on Windows prevents hijacking the raw TCP socket, requiring OS-specific APIs.
* **Apache / ISAPI / CGI / FastCGI**: External web servers manage the lifecycle of connections, not allowing sockets to remain open indefinitely on the Horse execution thread.

For these cases, Horse implements a **Fail-Fast** policy. If a client attempts to perform a WebSocket handshake in these environments, the API detects the absence of the upgrade mechanism in the provider, sets the HTTP status to `501 Not Implemented` or `400 Bad Request` transparently, and immediately halts the execution pipeline using the `EHorseCallbackInterrupted` exception, preventing silent failures or resource leaks.

---

## 🛠️ API and Basic Usage

Activating WebSockets is done by calling the `UpgradeToWebSocket` method on the `THorseResponse` object.

```delphi
uses
  Horse,
  Horse.Core.WebSocket;

begin
  THorse.Get('/ws',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.UpgradeToWebSocket(
        procedure(const AConn: IHorseWebSocketConnection)
        begin
          // Connection Established Event
          Writeln('Connected: ', AConn.ClientIP);

          // Message Received Event
          AConn.OnMessage :=
            procedure(const AConnection: IHorseWebSocketConnection; const AText: string)
            begin
              Writeln('Message: ', AText);
              // Echo message back to the client
              AConnection.SendText('Echo: ' + AText);
            end;

          // Disconnection Event
          AConn.OnDisconnect :=
            procedure(const AConnection: IHorseWebSocketConnection)
            begin
              Writeln('Disconnected: ', AConnection.ClientIP);
            end;

          // Error Event
          AConn.OnError :=
            procedure(const AConnection: IHorseWebSocketConnection; const AException: Exception)
            begin
              Writeln('Error: ', AException.Message);
            end;
        end);
    end);

  THorse.Listen(9000);
end.
```

---

## 📡 Connection Management and Broadcasting

Horse features a global, thread-safe `THorseWebSocketManager` class that automatically manages active connections associated with each route.

### Simple Broadcast
To send a message to all connected clients on a specific route:

```delphi
// Sends to all on the /ws route
THorseWebSocketManager.Broadcast('/ws', 'Hello everyone!');
```

### Broadcast Excluding Sender
It is very common in chat applications not to echo the message back to the client that sent it:

```delphi
AConn.OnMessage :=
  procedure(const AConnection: IHorseWebSocketConnection; const AText: string)
  begin
    // Sends the message to all on the route except the sender (AConnection)
    THorseWebSocketManager.Broadcast('/ws', AText, AConnection);
  end;
```

---

## 💓 Heartbeat and Keep-Alive

To prevent idle connections from being prematurely closed by proxies, firewalls, or load balancers, Horse features a native periodic **Heartbeat** mechanism.

By default, when upgrading the connection, Horse starts a background monitoring thread that sends `Ping` frames (Opcode `$09`) to the client at a set interval (default is 30 seconds). If the client does not respond with a `Pong` frame or if the connection drops, the connection is closed locally, freeing memory safely and immediately.

Developers can customize the Heartbeat interval (in seconds) by passing the corresponding parameter to the Upgrader.

---

## 🔒 Thread-Safety and Best Practices

1. **Mutual Exclusion on Writes**: The `SendText` / `SendBinary` methods are thread-safe. Writing to the physical socket is protected internally by a `TCriticalSection` (`FSyncWrite`), allowing multiple threads to call `Send` concurrently without risk of byte corruption or interleaving.
2. **Request Lifecycle**: Do not attempt to retain `THorseRequest` or `THorseResponse` variables inside WebSocket closures. The HTTP request lifecycle is terminated the moment `UpgradeToWebSocket` performs the connection upgrade. From that point on, only use the `IHorseWebSocketConnection` interface.
3. **Exceptions in Callbacks**: Always handle exceptions inside WebSocket events (`OnMessage`, etc.). Unhandled errors in these callbacks can force an abrupt termination of the corresponding client socket.
