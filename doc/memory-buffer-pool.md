# Memory Buffer Pooling

Memory Buffer Pooling in Horse is a high-performance optimization designed to minimize memory allocation overhead (heap churn) and GC/Memory Manager lock contention under heavy concurrent workloads.

## 💡 The Problem

Normally, web frameworks allocate new memory structures for every HTTP request and response payload (using `TMemoryStream` or `TBytesStream`). In highly concurrent Delphi/FPC web applications, continuous allocation and deallocation (`GetMem`/`FreeMem`) can lead to:
1. **Heap Fragmentation:** Splitting physical memory into tiny blocks.
2. **Lock Contention:** Multi-threaded servers locking the memory manager global heap mutex while allocating memory, causing thread stall and reducing CPU efficiency.

## 🚀 The Solution

Horse implements a thread-safe, stack-based buffer pool (`THorseMemoryBufferPool`) and a recyclable stream implementation (`THorsePooledStream` inheriting from `TStream`).

Instead of allocating new memory:
1. **Request Body:** High-performance providers (like HttpSys and Epoll) acquire a pre-allocated stream buffer from the pool to read and process incoming socket data.
2. **Response Body:** Handlers sending bytes or empty response bodies acquire streams from the pool.
3. **Automatic Recycling:** When a handler finishes processing and calls `.Free` on a pooled stream, the underlying `TBytes` buffer is automatically returned to the global pool for the next request.

This results in a **zero-allocation response and request payload mapping** for the vast majority of HTTP operations.

## 🛠️ Transparent Usage

For general framework consumers, this optimization is **100% transparent**. You do not need to rewrite any route or middleware code to benefit from it. Typical code remains exactly the same:

```delphi
THorse.Get('/ping',
  procedure(Req: THorseRequest; Res: THorseResponse)
  begin
    Res.Send('pong'); // Automatically uses the Memory Buffer Pool under the hood
  end);
```

## 🔌 Advanced Usage (for Middleware & Custom I/O)

If you are developing custom middlewares or handlers that require heavy streaming or file manipulation, you can voluntarily leverage the memory buffer pool to prevent heap allocation:

```delphi
uses
  Horse,
  Horse.Core.MemoryBufferPool,
  System.Classes;

begin
  THorse.Get('/report',
    procedure(Req: THorseRequest; Res: THorseResponse)
    var
      LStream: TStream;
    begin
      // Acquire a pre-allocated stream from the global pool (Thread-Safe)
      LStream := THorseMemoryBufferPool.DefaultPool.AcquireStream;
      try
        // Write content into the pooled stream
        LStream.WriteBuffer(PChar('Report Data...')^, 14);
        
        // Return the stream. Horse will transmit the data and automatically 
        // call LStream.Free, returning the buffer to the pool.
        Res.SendFile(LStream, 'report.txt');
      except
        LStream.Free; // Ensure the stream is returned to the pool in case of an exception
        raise;
      end;
    end);

  THorse.Listen(9000);
end.
```

## ⚙️ Pool Internal Settings

By default, the global pool initializes with:
* **Standard Buffer Size:** `65,536 bytes (64 KB)`.
* **Max Pool Stack Size:** `1,024` inactive buffers kept in RAM.
* **Max Recyclable Buffer Size:** `2,097,152 bytes (2 MB)`. 

> [!NOTE]
> If a stream dynamically grows larger than `2 MB` (e.g. handling a very large upload), it is discarded upon destruction and a fresh standard `64 KB` buffer is created and pushed back to the pool to prevent memory leaks and keep RAM consumption stable.
