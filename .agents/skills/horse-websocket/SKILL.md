---
name: horse-websocket
description: Guidelines and workflows for developing and maintaining WebSocket endpoints and protocol upgrades within the Horse framework.
---

# Horse WebSocket Coding Skill

This skill provides guidelines and patterns for developers and AI agents implementing, debugging, or maintaining WebSocket endpoints in the Horse framework.

## 🟢 Request Upgrade Pattern

To upgrade an HTTP request to a WebSocket connection, always call `Res.UpgradeToWebSocket(AOnConnectCallback)` inside the route handler.

### Delphi Syntax (Anonymous Procedure / Closure)
```delphi
THorse.Get('/ws',
  procedure(Req: THorseRequest; Res: THorseResponse)
  begin
    Res.UpgradeToWebSocket(
      procedure(const AConn: IHorseWebSocketConnection)
      begin
        AConn.OnMessage := procedure(const AConnection: IHorseWebSocketConnection; const AText: string)
        begin
          AConnection.SendText('Received: ' + AText);
        end;
      end);
  end);
```

### Lazarus/FPC Syntax (Object Methods)
Always use regular object methods or class procedures for callbacks in Lazarus/FPC to ensure compatibility:
```pascal
type
  TWebSocketController = class
  public
    class procedure HandleWS(Req: THorseRequest; Res: THorseResponse);
    class procedure OnConnect(const AConn: IHorseWebSocketConnection);
    class procedure OnMessage(const AConnection: IHorseWebSocketConnection; const AText: string);
  end;

class procedure TWebSocketController.HandleWS(Req: THorseRequest; Res: THorseResponse);
begin
  Res.UpgradeToWebSocket(OnConnect);
end;

class procedure TWebSocketController.OnConnect(const AConn: IHorseWebSocketConnection);
begin
  AConn.OnMessage := OnMessage;
end;

class procedure TWebSocketController.OnMessage(const AConnection: IHorseWebSocketConnection; const AText: string);
begin
  AConnection.SendText('Received: ' + AText);
end;
```

## 🟢 Broadcasts and Session Management

Use `THorseWebSocketManager` for thread-safe operations across multiple connections.

* **Broadcast to all**: `THorseWebSocketManager.Broadcast('/ws', 'Hello all!');`
* **Broadcast excluding sender**: `THorseWebSocketManager.Broadcast('/ws', 'Hello others!', AConnection);`

## 🟢 Error and Exception Handling

* **Silencing normal interruptions**: Normal connection validation errors or manual status sends followed by pipeline interruption should raise `EHorseCallbackInterrupted` (from unit `Horse.Exception.Interrupted`) to gracefully exit the middleware pipeline.
* **Callback Safety**: Always place try-except blocks inside custom event handlers (like `OnMessage`) to prevent crashes inside the worker socket threads.
* **Fail-Fast Detection**: If a client requests a WebSocket upgrade on an unsupported provider (e.g. `HttpSys` or hosted ISAPI/CGI), `Res.UpgradeToWebSocket` will automatically reply with `501 Not Implemented` and raise `EHorseCallbackInterrupted`.

## 🟢 Thread-Safety Rules

1. `IHorseWebSocketConnection.SendText` and `SendBinary` are thread-safe and protected by an internal `TCriticalSection`. Do not add manual synchronization wrappers around them.
2. The HTTP request/response objects (`THorseRequest`, `THorseResponse`) are recycled/destroyed once the upgrade is completed. Do **not** store or reference them inside any of the WebSocket callbacks (`OnMessage`, `OnDisconnect`, etc.). Use the `IHorseWebSocketConnection` context instead.
