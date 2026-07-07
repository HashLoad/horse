---
name: horse-performance-tuning
description: Guide for optimizing performance, tuning memory alocations, handling large payloads, and adjusting provider execution pipelines.
---

# Horse Performance Tuning

To exploit the raw speed and low latency of the Horse framework, write handlers that avoid CPU bottlenecks and memory allocation overhead.

---

## 1. Minimizing Heap Allocations
Memory allocations (creating objects, large arrays, or concatenating strings) require thread synchronization locks in the memory manager, which slows down concurrent execution under heavy loads.

*   **Avoid Repeated JSON Parsing**: If you just need to proxy or return static JSON payloads, send them as raw string strings or static stream resources rather than creating and destroying `TJSONObject` instances.
*   **Avoid String Concatenation**: In loops, never concatenate strings using the `+` operator. Use `TStringBuilder` instead to avoid repeatedly reallocating memory on the heap.

```pascal
// Inefficient (creates hundreds of temporary heap strings)
for I := 1 to 1000 do
  LResponseText := LResponseText + LData[I];

// Efficient
LBuilder := TStringBuilder.Create;
try
  for I := 1 to 1000 do
    LBuilder.Append(LData[I]);
  Res.Send(LBuilder.ToString);
finally
  LBuilder.Free;
end;
```

---

## 2. Fast Streaming for Large Payloads
When transferring large JSON strings, files, or reports, do **not** load the entire file contents into a string variable. Stream it directly to the socket chunk-by-chunk using `Res.SendFile` or `Res.Download` to maintain a low RAM profile.

*   **Bad**: Loading a 100MB PDF into a `TStringList` or byte array.
*   **Good**: Passing a `TFileStream` directly to the response (letting Horse stream it efficiently).

```pascal
procedure ServeFileHandler(Req: THorseRequest; Res: THorseResponse);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create('C:\data\largefile.zip', fmOpenRead or fmShareDenyWrite);
  Res.Status(THTTPStatus.OK).SendFile(LStream, 'largefile.zip');
  // Do NOT free LStream. Horse takes ownership of the stream.
end;
```

---

## 3. Selecting and Tuning the Transport Provider
The default Indy provider (`Horse.Provider.Console`) uses a thread-per-connection model. Under massive concurrency (thousands of connections), this model incurs thread-switching overhead.

*   **IOCP / Epoll / KQueue**: Use **`horse-provider-crosssocket`** or **`horse-provider-mormot`** for asynchronous, non-blocking I/O.
*   **HTTP.sys**: Under Windows, utilize `Horse.Provider.HTTPsys`. Since it runs in kernel-mode, it bypasses user-to-kernel context switches, achieving near-native OS speed.

---

## 4. Compiler Optimization Flags
When building Horse applications for production, ensure the compiler optimization is turned on and debugging symbols are disabled (configured in your `.dproj` or `boss.json` script):

*   **Turn off Assertions**: Assertions check code invariants but add CPU overhead in loop-heavy operations. Disable them (`{$ASSERTIONS OFF}` or `{$C-}`).
*   **Enable Optimizations**: Turn on compiler optimizations (`{$OPTIMIZATION ON}` or `{$O+}`).
*   **FastMM4/5**: On older Delphi versions (before 10.4 Sydney), replace the default memory manager with **FastMM4** configured for multithreaded sharing to reduce lock contention.
