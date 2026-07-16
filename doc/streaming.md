# Data Streaming (Web Streams & Server-Sent Events)

Horse has native support for real-time data streaming through integrated hooks in physical transport providers. It allows two main formats of incremental delivery:
1.  **Web Streams (Chunked HTTP/1.1)**: Incremental transmission of data split into chunks, commonly used for AI APIs and NDJSON streams.
2.  **Server-Sent Events (SSE)**: A persistent unidirectional channel kept open for continuous event streams from the server to the client (`text/event-stream`).

---

## 🚀 How to Use on the Server

Streaming is initiated using the `Res.SendStream` method, which accepts a callback receiving an instance of `IHorseStreamWriter`.

### 1. Web Stream Example (NDJSON)
In this example, we incrementally send multiple JSON objects line-by-line (using `\n` as delimiter):

```delphi
uses Horse, Horse.Response, System.SysUtils;

begin
  THorse.Get('/ndjson',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.ContentType('application/x-ndjson; charset=utf-8');
      Res.SendStream(
        procedure(const AWriter: IHorseStreamWriter)
        var
          I: Integer;
          LJSON: string;
        begin
          for I := 1 to 10 do
          begin
            // Check if the client is still connected before writing
            if not AWriter.IsConnected then Break;
            
            LJSON := Format('{"id": %d, "timestamp": "%s"}'#10, [I, FormatDateTime('hh:nn:ss', Now)]);
            AWriter.Write(LJSON);
            Sleep(500); // Simulate asynchronous delay
          end;
        end);
    end);

  THorse.Listen(9000);
end.
```

### 2. Server-Sent Events Example (SSE)
SSE requires specific headers (`text/event-stream`, `Cache-Control: no-cache` and `Connection: keep-alive`) and event formatting `event: <name>\ndata: <content>\n\n`:

```delphi
uses Horse, Horse.Response, System.SysUtils;

begin
  THorse.Get('/sse',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.ContentType('text/event-stream; charset=utf-8');
      Res.AddHeader('Cache-Control', 'no-cache');
      Res.AddHeader('Connection', 'keep-alive');

      Res.SendStream(
        procedure(const AWriter: IHorseStreamWriter)
        var
          I: Integer;
          LPayload: string;
        begin
          for I := 1 to 10 do
          begin
            if not AWriter.IsConnected then Break;
            
            LPayload := Format('event: message'#10'data: {"id": %d, "count": %d}'#10#10, [I, I]);
            AWriter.Write(LPayload);
            Sleep(1000);
          end;
        end);
    end);

  THorse.Listen(9000);
end.
```

---

## 📡 How to Consume the Stream (Native Delphi Client)

You can consume any of these endpoints incrementally using the modern `THTTPClient` class in Delphi by overriding a `TStream` class to intercept chunks as they arrive:

```delphi
uses System.SysUtils, System.Classes, System.Net.HttpClient;

type
  TStreamingReceiveStream = class(TStream)
  private
    FOnChunkReceived: TProc<string>;
  public
    constructor Create(const AOnChunkReceived: TProc<string>);
    function Write(const Buffer; Count: Longint): Longint; override;
    // Read, Seek, and SetSize should be overridden simply returning 0 / empty
  end;

constructor TStreamingReceiveStream.Create(const AOnChunkReceived: TProc<string>);
begin
  inherited Create;
  FOnChunkReceived := AOnChunkReceived;
end;

function TStreamingReceiveStream.Write(const Buffer; Count: Longint): Longint;
var
  LText: string;
  LBytes: TBytes;
begin
  Result := Count;
  if Count <= 0 then Exit;
  SetLength(LBytes, Count);
  Move(Buffer, LBytes[0], Count);
  LText := TEncoding.UTF8.GetString(LBytes);
  if Assigned(FOnChunkReceived) then
    FOnChunkReceived(LText);
end;

// Example usage:
var
  LClient: THTTPClient;
  LStream: TStreamingReceiveStream;
begin
  LClient := THTTPClient.Create;
  LStream := TStreamingReceiveStream.Create(
    procedure(AChunk: string)
    begin
      Write(AChunk); // Print to screen as chunks are received in real-time
    end);
  try
    LClient.Get('http://localhost:9000/ndjson', LStream);
  finally
    LStream.Free;
    LClient.Free;
  end;
end.
```

---

## 🎯 Unicast: Targeted Sending to a Specific Client

Unlike WebSockets (which manage connections asynchronously after the handshake), in Web Streaming and Server-Sent Events (SSE), the HTTP connection remains active **only as long as the callback passed to `Res.SendStream` is executing**. If the callback exits, the stream is closed.

To send events to a specific client from external processes (such as a REST endpoint `/notify` or a background thread), you should map the client ID to a **Thread-Safe Message Queue** (like `TThreadedQueue<string>`). The SSE request thread then waits for new items in the queue inside a loop, keeping the HTTP connection open efficiently without high CPU consumption.

### 1. Creating the SSE Queue Manager (`MySSEManager.pas`)

```delphi
unit MySSEManager;

interface

uses
  System.SysUtils, System.Generics.Collections, System.SyncObjs;

type
  TSSEQueue = TThreadedQueue<string>;

  TMySSEManager = class
  private
    class var FSync: TCriticalSection;
    class var FClients: TDictionary<string, TSSEQueue>;
    class constructor Create;
    class destructor Destroy;
  public
    class procedure RegisterClient(const AClientId: string; const AQueue: TSSEQueue);
    class procedure UnregisterClient(const AClientId: string);
    class function SendToClient(const AClientId: string; const AMessage: string): Boolean;
  end;

implementation

class constructor TMySSEManager.Create;
begin
  FSync := TCriticalSection.Create;
  FClients := TDictionary<string, TSSEQueue>.Create;
end;

class destructor TMySSEManager.Destroy;
begin
  FClients.Free;
  FSync.Free;
end;

class procedure TMySSEManager.RegisterClient(const AClientId: string; const AQueue: TSSEQueue);
begin
  FSync.Enter;
  try
    FClients.AddOrSetValue(AClientId, AQueue);
  finally
    FSync.Leave;
  end;
end;

class procedure TMySSEManager.UnregisterClient(const AClientId: string);
begin
  FSync.Enter;
  try
    FClients.Remove(AClientId);
  finally
    FSync.Leave;
  end;
end;

class function TMySSEManager.SendToClient(const AClientId: string; const AMessage: string): Boolean;
var
  LQueue: TSSEQueue;
begin
  Result := False;
  FSync.Enter;
  try
    if FClients.TryGetValue(AClientId, LQueue) then
    begin
      LQueue.PushItem(AMessage);
      Result := True;
    end;
  finally
    FSync.Leave;
  end;
end;

end.
```

### 2. Binding the Manager to the Horse Route

```delphi
uses Horse, Horse.Response, System.SysUtils, MySSEManager;

begin
  THorse.Get('/sse',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    var
      LClientId: string;
    begin
      LClientId := Req.Query.Items['clientId']; // e.g. /sse?clientId=ClientX

      if LClientId.IsEmpty then
      begin
        Res.Send('clientId is required').Status(400);
        Exit;
      end;

      Res.ContentType('text/event-stream; charset=utf-8');
      Res.AddHeader('Cache-Control', 'no-cache');
      Res.AddHeader('Connection', 'keep-alive');

      Res.SendStream(
        procedure(const AWriter: IHorseStreamWriter)
        var
          LQueue: TSSEQueue;
          LMsg: string;
          LQueueResult: TWaitResult;
        begin
          LQueue := TSSEQueue.Create(100);
          try
            TMySSEManager.RegisterClient(LClientId, LQueue);
            try
              while AWriter.IsConnected do
              begin
                // Wait for items in the queue up to 1000ms to allow checking connectivity periodically
                LQueueResult := LQueue.PopItem(LMsg, 1000);
                
                if LQueueResult = wrSignaled then
                begin
                  if not AWriter.IsConnected then Break;
                  
                  // Write the packet formatted as SSE
                  AWriter.Write('event: message'#10'data: ' + LMsg + #10#10);
                end;
              end;
            finally
              TMySSEManager.UnregisterClient(LClientId);
            end;
          finally
            LQueue.Free;
          end;
        end);
    end);
```

---

## ⚠️ Guidelines and Best Practices

*   **Avoid Inline Anonymous Methods in Lazarus/FPC**: Due to compiler limitations in Free Pascal, prefer declaring regular class methods/procedures instead of inline closures for route and stream callbacks.
*   **Disconnect Detection**: Always check `AWriter.IsConnected` before writing to free resources immediately if the user closes the connection or stops the download.
*   **Middleware Bypass**: Any global middleware (such as gzip/deflate compression or logging) that alters the HTTP response buffer dynamically is automatically bypassed when `Res.IsStreaming` is `True` to prevent chunk buffering and stream corruption.
