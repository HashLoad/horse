---
name: horse-zero-allocation
description: Guide for programming with Zero-Allocation principles in Horse, focusing on stack allocations, string slicing, object pooling, and avoiding lock contention.
---

# Horse Zero-Allocation Programming

To write custom routers, high-performance middlewares, or request handlers that scale under extreme concurrency, you must minimize or eliminate memory allocations on the dynamic heap. Heap allocations require global memory manager locks, which cause lock contention and page faults across multi-threaded applications.

---

## 1. Zero-Allocation String Slicing (Avoid String Copying)
Using functions like `Copy(MyString, Start, Length)` or string concatenation (`+`) allocates new string segments on the heap. Under heavy concurrency, this degrades performance.

*   **Rule**: Use index/pointer boundaries (such as `PChar`) or specialized slice types to perform string matches and parsing.
*   **Idiomatic Example**: Create or use a slice structure (like `THorseBufferSlice` in the Horse RouterTree) to match paths without generating new sub-strings.

```pascal
type
  TZeroAllocSlice = record
    Buffer: PChar;
    Start: Integer;
    Length: Integer;
    function Compare(const AString: string): Boolean;
  end;

function TZeroAllocSlice.Compare(const AString: string): Boolean;
var
  I: Integer;
begin
  if Length <> System.Length(AString) then
    Exit(False);
    
  for I := 0 to Length - 1 do
    if (Buffer + Start + I)^ <> AString[I + 1] then
      Exit(False);
      
  Result := True;
end;
```

---

## 2. Using the Stack (Records and Static Arrays)
Avoid creating temporary class instances or dynamic arrays (`array of Byte`) inside loop structures or high-frequency handler routines. These require heap overhead and reference-counting locks.

*   **Stack Allocation (Records)**: Declare local data carriers as `record` rather than `class`. Records are allocated automatically on the fast thread stack and cleaned up instantly upon exit.
*   **Static Arrays**: For local buffers (such as socket buffers or event arrays), declare static arrays with fixed sizes:

```pascal
// Correct (Stack allocation - fast, zero page faults)
procedure ProcessEvents;
var
  LEvents: array[0..255] of epoll_event; // Allocated on the stack
begin
  // Process up to 256 events directly
end;

// Incorrect (Heap allocation - slow thread lock contention)
procedure ProcessEventsBad;
var
  LEvents: array of epoll_event;
begin
  SetLength(LEvents, 256); // Hits the memory manager heap
end;
```

---

## 3. Object and Request Recycling (Pooling)
Instead of instantiating helper objects (like database queries, parser buffers, or connection context blocks) for every HTTP request, utilize a thread-safe Object Pool (e.g. `THorseContextPool`).

*   **Padrão**:
    1.  Acquire a pre-allocated object from the pool at the start of the handler.
    2.  Use the object.
    3.  Reset the object's internal fields (clean/nullify references).
    4.  Return the object to the pool in a `finally` block.

---

## 4. Zero-Copy and Direct Encoding Handling
Avoid redundant string transcoding (e.g. converting `UTF-8` response buffers back and forth to standard `UnicodeString`).

*   **Pass-through Buffer**: Pass raw bytes and stream pointers directly to the socket API (like Windows HTTP.sys kernel API or FPC Epoll socket handles).
*   **Reference Clearing**: Ensure references to pooled objects or structures (such as connection context structures) are explicitly nilled out/cleared immediately after request termination. This prevents the compiler's runtime from executing locking cleanup logic on worker threads.
