# Middleware

*Read this in [English](./middleware.md) or [Português (BR)](./middleware.pt-BR.md).*

Middleware lets you wrap route handlers with cross-cutting logic — parsing bodies, authenticating users, adding CORS headers, logging, error formatting. Horse middleware is shaped like Express middleware: a procedure that receives the request, the response, and a `Next` proc.

For the package catalogue (JSON, JWT, CORS, etc.), see [Middleware Ecosystem](./middleware-ecosystem.md).

---

## The model

A middleware is a procedure with this shape:

```delphi
procedure MyMiddleware(
  Req:  THorseRequest;
  Res:  THorseResponse;
  Next: {$IF DEFINED(FPC)}TNextProc{$ELSE}TProc{$ENDIF});
```

It can:

1. **Inspect or mutate** the request and response.
2. **Call `Next()`** to continue the chain (passes control to the next middleware or, eventually, the route handler).
3. **Skip `Next()`** to short-circuit — useful for auth failures.
4. **Raise an exception** — `EHorseCallbackInterrupted` ends the chain quietly; anything else becomes a `500`.

## A first middleware

```delphi
procedure Logger(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Start: TDateTime;
begin
  Start := Now;
  try
    Next();
  finally
    WriteLn(Format('[%s] %s %s -> %d (%dms)',
      [FormatDateTime('yyyy-mm-dd hh:nn:ss', Start),
       Req.Method, Req.PathInfo, Res.Status,
       MilliSecondsBetween(Now, Start)]));
  end;
end;

// Register at startup, before any routes
THorse.Use(Logger);
```

The `try / finally` pattern is the canonical way to wrap the entire request — the timer fires whether the handler succeeded, failed, or short-circuited.

## Registration

You can register middlewares at different scopes in Horse:

1. **Global**: Registered via `THorse.Use(...)` affecting all routes (or wildcard paths).
2. **Group-level**: Registered via `.Use(...)` inside a route group (`THorse.Group`).
3. **Route-level (Local)**: Passed as an array (`array of THorseCallback`) directly into the HTTP verb of the route.

```delphi
THorse.Use(MyGlobalMiddleware);        // Global

THorse.Group.Prefix('/admin')
  .Use(MyGroupMiddleware)              // Group-level
  .Get('/users', [MyRouteMiddleware], ListUsers); // Route-level (local)
```

**Registration order matters.** Middleware runs in the order it was registered/mapped, in a nested onion model:

```
THorse.Use(A);                         // Global (outermost)
THorse.Group.Prefix('/admin')
  .Use(B)                              // Group-level
  .Get('/x', [C], Handler);            // Route-level (innermost)
```

Request flow:
```
A (Global) → B (Group) → C (Route) → Handler → C → B → A
```

…where the right-hand side of each arrow is the code that runs after `Next()` returns. So `A` runs first and gets the last word; `C` wraps the handler most tightly.

**Practical implication:** register middleware in *outermost-first* order. If logging needs to see the final status code, register `Logger` *first*. If exception handling needs to catch unhandled errors, register `HandleException` *first*. If body parsing is required by everything below, register `Jhonson` early.

A typical startup block:

```delphi
THorse
  .Use(HandleException)    // 1. outermost — turn exceptions into clean responses
  .Use(Logger)             // 2. log every completed request
  .Use(CORS)               // 3. add CORS headers
  .Use(Jhonson)            // 4. parse JSON bodies
  .Use(JWT(SECRET))        // 5. authenticate (closest to handler)

THorse.Get('/users', ListUsers);
```

## Short-circuiting

Skip `Next()` to stop the chain without calling the handler. The most common case is authentication:

```delphi
procedure RequireApiKey(Req: THorseRequest; Res: THorseResponse; Next: TProc);
begin
  if Req.Headers['X-Api-Key'] <> 'expected-key' then
  begin
    Res.Status(THTTPStatus.Unauthorized).Send('Missing or invalid API key');
    Exit;                              // chain stops; handler does not run
  end;
  Next();                              // valid — continue
end;
```

Or raise `EHorseCallbackInterrupted` (defined in `Horse.Exception.Interrupted`) for the same effect — useful when you've already set the response and just want the chain to end cleanly:

```delphi
uses Horse.Exception.Interrupted;

procedure HandlePreflight(Req: THorseRequest; Res: THorseResponse; Next: TProc);
begin
  if Req.Method = 'OPTIONS' then
  begin
    Res.Status(THTTPStatus.NoContent);
    raise EHorseCallbackInterrupted.Create;   // skip remaining chain
  end;
  Next();
end;
```

This is how `Horse.CORS` implements preflight today.

## Writing your own — a complete example

> Looking for the full authoring guide — Provider neutrality, thread safety, packaging for Boss, testing across the Provider matrix? See **[Writing a Middleware](./writing-middleware.md)**. The snippet below is the quick version.

A simple rate limiter, per-IP, per-minute:

```delphi
unit Horse.Middleware.RateLimit;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.SyncObjs, Horse;

type
  THorseRateLimit = class
  private
    class var FBuckets: TDictionary<string, Integer>;
    class var FLock: TCriticalSection;
    class var FLimit: Integer;
    class var FResetEvery: Integer;        // seconds
  public
    class procedure Init(ALimit: Integer = 60; AWindowSeconds: Integer = 60); static;
    class procedure Done; static;
    class function Middleware: THorseCallback; static;
  end;

implementation

class procedure THorseRateLimit.Init(ALimit, AWindowSeconds: Integer);
begin
  FLimit      := ALimit;
  FResetEvery := AWindowSeconds;
  FBuckets    := TDictionary<string, Integer>.Create;
  FLock       := TCriticalSection.Create;
end;

class procedure THorseRateLimit.Done;
begin
  FreeAndNil(FBuckets);
  FreeAndNil(FLock);
end;

class function THorseRateLimit.Middleware: THorseCallback;
begin
  Result :=
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    var
      IP:    string;
      Count: Integer;
    begin
      // Best-effort client IP — proper deployments use X-Forwarded-For
      IP := Req.Headers['X-Real-IP'];
      if IP = '' then IP := Req.RawWebRequest.RemoteAddr;
      if IP = '' then IP := 'unknown';

      FLock.Acquire;
      try
        FBuckets.TryGetValue(IP, Count);
        Inc(Count);
        FBuckets.AddOrSetValue(IP, Count);
      finally
        FLock.Release;
      end;

      if Count > FLimit then
      begin
        Res.AddHeader('X-RateLimit-Remaining', '0');
        Res.Status(THTTPStatus.TooManyRequests).Send('Rate limit exceeded');
        Exit;
      end;

      Res.AddHeader('X-RateLimit-Remaining', IntToStr(FLimit - Count));
      Next();
    end;
end;

end.
```

Usage:

```delphi
uses Horse, Horse.Middleware.RateLimit;

THorseRateLimit.Init(120, 60);   // 120 req/min
THorse.Use(THorseRateLimit.Middleware);
THorse.Listen(9000);
```

(For production use, refresh the buckets every `FResetEvery` seconds; this skeleton omits the timer thread for brevity.)

## Global Error Handler (OnError)

Horse provides a global error handling pipeline to catch all unhandled exceptions occurring during the request lifecycle (such as exceptions thrown in global middlewares, route groups, or final handlers).

To register a global error handler, use the `THorse.OnError` method:

```delphi
procedure MyGlobalErrorHandler(const ARequest: THorseRequest; const AResponse: THorseResponse; const AException: Exception);
begin
  // Log the exception details to a file or external service
  WriteLn('Internal error detected: ' + AException.Message);

  // Return a standardized error response to the client
  AResponse
    .Send('{"error": "' + AException.Message + '"}')
    .Status(THTTPStatus.InternalServerError);
end;

begin
  // Register the callback during framework startup
  THorse.OnError(MyGlobalErrorHandler);

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      raise Exception.Create('Something went wrong unexpectedly!');
    end);

  THorse.Listen(9000);
end.
```

### Characteristics of OnError
- **Backward Compatibility**: The callback signature uses the classic `procedure(...)` pointer type, ensuring complete support for older Delphi versions (XE7+) and Lazarus/FPC.
- **Control Exceptions Handling**: Framework control exceptions such as `EHorseCallbackInterrupted` and `EHorseException` are processed internally by Horse and **do not** trigger the global `OnError` callback.
- **Fail-Safe Mechanism (Safety)**: If your custom `OnError` callback itself throws an exception, Horse intercepts it safely and returns a `500 Internal Server Error` containing the details of the crash, preventing socket leaks or server crashes.
- **Default Behavior (Without Registration)**: If no `OnError` callback is registered, the framework works **exactly as before**: exceptions will continue to propagate up to the HTTP server provider's traditional flow. The only improvement is that the default HTTP 500 body response now details the message of the thrown exception (e.g., `Internal Application Error: Message`), instead of displaying a generic hardcoded string.

## When to write middleware vs put logic in a handler

- **Cross-cutting concern that applies to many routes** → middleware (auth, logging, CORS, body parsing).
- **Logic specific to one resource** → handler or controller method.
- **Logic specific to a few routes within a group** → group-scoped middleware (`THorse.Group.Use(...)`).

If you find yourself repeating the same six lines at the top of every handler, that's a middleware.

## Common pitfalls

| Symptom | Likely cause |
|---|---|
| Handler runs twice | Calling `Next()` twice in the same middleware. |
| Handler never runs | Forgot `Next()`, but also didn't send a response — request hangs until timeout. |
| Headers from middleware don't appear | Middleware called `Next()` *before* setting the header; the response was already flushed by the handler. Set headers before `Next()` or use the `try/finally` wrap. |
| Order doesn't match expectations | Middleware registers late, after some routes. Register all middleware first, then routes. |
| `EHorseCallbackInterrupted` logged as error | A catch-all `on E: Exception` somewhere is grabbing it. Catch `EHorseCallbackInterrupted` *before* the generic handler. |

## See also

- [Middleware Ecosystem](./middleware-ecosystem.md) — `Jhonson`, `CORS`, `JWT`, `compression`, `handle-exception`, and many community packages.
- [Request & Response](./request-response.md) — the API you use inside a middleware body.
- [Routing](./routing.md) — how `THorse.Use` differs from `THorse.Get/Post/...`.
