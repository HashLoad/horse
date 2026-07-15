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

## ⚠️ Guidelines and Best Practices

*   **Avoid Inline Anonymous Methods in Lazarus/FPC**: Due to compiler limitations in Free Pascal, prefer declaring regular class methods/procedures instead of inline closures for route and stream callbacks.
*   **Disconnect Detection**: Always check `AWriter.IsConnected` before writing to free resources immediately if the user closes the connection or stops the download.
*   **Middleware Bypass**: Any global middleware (such as gzip/deflate compression or logging) that alters the HTTP response buffer dynamically is automatically bypassed when `Res.IsStreaming` is `True` to prevent chunk buffering and stream corruption.
