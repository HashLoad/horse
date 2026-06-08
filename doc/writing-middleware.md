# Writing a Horse Middleware

*Read this in [English](./writing-middleware.md) or [Português (BR)](./writing-middleware.pt-BR.md).*

This guide walks through implementing a production-quality Horse middleware — from the bare procedure skeleton to a published Boss package. For the basics of *using* middleware, see [Middleware](./middleware.md). For why some patterns work across all Providers and others break, see [Providers & Application types](./providers.md).

---

## 1. What "production-quality" means here

Five properties — your middleware should meet all of them if you're publishing it for others:

1. **Provider-neutral** — works on Indy (Delphi default), `fphttpserver` (FPC default), CrossSocket, mORMot2, and any future Provider. Apache / ISAPI / CGI too.
2. **Cross-compiler** — compiles on Delphi 10.4 Sydney or newer **and** FPC 3.2 or newer.
3. **Thread-safe** — Horse handlers run on different threads under different Providers; shared state must be guarded.
4. **Configurable** — accepts options without relying on global variables, where possible.
5. **Boss-installable** — has a `boss.json` so consumers can `boss install` it.

If you only need a one-off middleware for your own app, a top-level `procedure` in your project source is fine — skip ahead to §2 (the skeleton) and §6 (short-circuit), and ignore the rest. The full guide is for the case where you're packaging the middleware for reuse.

---

## 2. The minimum skeleton

```pascal
unit Horse.Middleware.MyThing;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  {$IF DEFINED(FPC)} SysUtils {$ELSE} System.SysUtils {$ENDIF},
  Horse;

procedure MyMiddleware(
  Req: THorseRequest;
  Res: THorseResponse;
  Next: {$IF DEFINED(FPC)}TNextProc{$ELSE}TProc{$ENDIF});

implementation

procedure MyMiddleware(Req: THorseRequest; Res: THorseResponse;
  Next: {$IF DEFINED(FPC)}TNextProc{$ELSE}TProc{$ENDIF});
begin
  // before — inspect / mutate Req, set up state, fail-fast
  try
    Next();
  finally
    // after — inspect / mutate Res, tear down state
  end;
end;

end.
```

The `try / finally` wrap is the canonical shape — it lets your "after" code see the response status whether the handler succeeded, failed, or short-circuited. Add it whenever you need both phases.

`THorseCallback` (the type used by `THorse.Use`) wraps this procedure shape into a `TProc<THorseRequest, THorseResponse, TProc>` on Delphi or a `THorseProc` on FPC.

---

## 3. Adding configuration

Three patterns, in increasing complexity. Pick the lightest one that fits your needs.

### a) No config — bare procedure

For "always-on" middleware that has no knobs (e.g. a stdout logger):

```pascal
THorse.Use(MyMiddleware);   // pass the procedure directly
```

### b) Config record + factory function

For middleware where most users want defaults but a few want overrides:

```pascal
type
  TMyConfig = record
    Threshold: Integer;
    Verbose:   Boolean;
    class function Default: TMyConfig; static;
  end;

function MyMiddleware: THorseCallback; overload;                          // uses defaults
function MyMiddleware(const AConfig: TMyConfig): THorseCallback; overload;
```

Usage:

```pascal
THorse.Use(MyMiddleware);                                     // defaults
THorse.Use(MyMiddleware(TMyConfig.Default.Set(... )));        // custom
```

This is the pattern used by `Horse.Middleware.SecurityHeaders` and `Horse.Middleware.RequestGuard`.

### c) Static class with `.New` factory

For middleware with substantial state (rate-limiter buckets, connection pools):

```pascal
type
  TMyMiddleware = class
  public
    class function New: THorseCallback; overload;
    class function New(const AConfig: TMyConfig): THorseCallback; overload;
  end;

THorse.Use(TMyMiddleware.New);
```

The factory returns a `THorseCallback` (an anonymous procedure / procedure reference) — the caller passes that to `THorse.Use(...)`.

**Avoid module-level global variables for configuration.** Per-process state shared across all consumers of the package is the cause of most third-party middleware bugs (e.g. `horse-cors` historically used this pattern, which is why you can't run two Horse servers with different CORS policies in the same process). Use a record/class that captures the config inside the closure instead.

---

## 4. Per-request vs shared state

| Kind of state | Where it lives | Safe? |
|---|---|---|
| Parsed body object | `Req.Body(AObject)` — Horse owns and frees | Yes |
| Computed value used downstream | A field you add to a record stored in `Req.Body<T>` (or a closure variable that captures the request) | Yes |
| Counter, cache, registry | Class var or unit var | Only if locked |

For passing a value to the next middleware or to the handler, the cleanest pattern is to use `Req.Body<T>` if it makes semantic sense, or to write the value into a response header that downstream middleware reads.

Avoid: stashing per-request state in a global `TThreadList<string,T>` keyed on something derived from the request. It works but is wasteful — the request object's lifetime is already what you want; the thread it runs on doesn't matter.

---

## 5. Thread safety

Horse handlers run on different threads depending on the Provider:

| Provider | Where handlers run | Implication |
|---|---|---|
| Indy (Delphi self-hosted) | One Indy thread per connection | Hundreds of threads under load |
| `fphttpserver` (FPC self-hosted) | One thread per connection | Same as Indy |
| CrossSocket | IO threads (fixed pool, `CPUCount*2+1`) or worker threads (`THorseWorkerPool`, 4–64) | Same handler can run on different threads across requests |
| mORMot2 | Fixed thread pool inside `THttpServer` (default 32, configurable via `THorseMormotConfig.ThreadPool`) | Same handler can run on different threads across requests |
| Apache / ISAPI / CGI | Host's worker thread | Varies by host configuration |

In every Provider, **a single handler invocation runs on a single thread end-to-end**. You don't need locks within one request. But shared state across requests must be protected.

**Rule of thumb**: every `class var` or unit-level `var` your middleware introduces needs explicit guarding (`TCriticalSection`, `TMonitor`, `TThreadList`, etc.). Even `Inc(Counter)` on a shared variable is not atomic on the hot path.

For high-frequency counters specifically, prefer atomic operations:

```pascal
{$IF DEFINED(FPC)}
  Result := InterlockedIncrement(FCounter);
{$ELSE}
  Result := TInterlocked.Increment(FCounter);
{$IFEND}
```

For maps, `System.SyncObjs.TMonitor` on the dictionary instance is the simplest correct pattern:

```pascal
TMonitor.Enter(FBuckets);
try
  if not FBuckets.TryGetValue(IP, Count) then Count := 0;
  Inc(Count);
  FBuckets.AddOrSetValue(IP, Count);
finally
  TMonitor.Exit(FBuckets);
end;
```

---

## 6. Short-circuit cleanly

Two ways to end the chain early.

### a) Set the response and `Exit` without calling `Next()`

Clearest for "auth failed" / "rate limited" cases:

```pascal
if Req.Headers['X-Api-Key'] <> SECRET then
begin
  Res.Status(THTTPStatus.Unauthorized).Send('bad key');
  Exit;     // chain stops here; handler does not run
end;
Next();
```

### b) Raise `EHorseCallbackInterrupted`

Right choice when you're nested inside a `try / except` block in your own code and need to unwind cleanly, or when an existing helper sets the response and you just need to break the chain:

```pascal
uses Horse.Exception.Interrupted;
...
  Res.Status(THTTPStatus.NoContent);
  raise EHorseCallbackInterrupted.Create;
```

**Never catch `EHorseCallbackInterrupted` in a generic `on E: Exception do`** — let it propagate. Horse's pipeline runner catches it explicitly and treats it as a normal end-of-chain signal. A generic catch turns every short-circuit into a logged "error".

---

## 7. Order dependence

If your middleware depends on another having run first, document this loudly at the top of the unit:

```pascal
unit Horse.Middleware.MyValidator;

{
  REQUIRES: Horse.Jhonson MUST be registered BEFORE this middleware,
  because we read Req.Body<TJSONObject> which Jhonson populates.

  RECOMMENDED order:
    THorse.Use(HandleException);    // outermost
    THorse.Use(Logger);
    THorse.Use(Jhonson);
    THorse.Use(MyValidator);        // here
    THorse.Use(JWT(SECRET));
}
```

Common dependency chain across the official middlewares:

```
HandleException → Logger → CORS → Jhonson → JWT → (your middleware) → Routes
```

If you reorder these, weird things happen — e.g. a `Logger` registered *after* the route runs may not see the final status code; `Jhonson` registered *after* JWT means JWT doesn't see a parsed body.

If your middleware has no order dependency, say so. Silence is ambiguous; explicit "order-independent" is reassuring.

---

## 8. Provider compatibility — the big one

This is where most third-party middleware breaks on CrossSocket. **Avoid these anti-patterns:**

### Anti-pattern A: casting `Req.RawWebRequest` to an Indy-specific subclass

```pascal
// DON'T do this — breaks on CrossSocket, FPC, Apache, ISAPI, …
LIndyReq := Req.RawWebRequest as TIdHTTPAppRequest;
LIndyReq.PostStream.Position := 0;
```

`Req.RawWebRequest` is `TWebRequest` (Delphi) or `TRequest` (FPC). The concrete subclass varies by Provider: Indy provides `TIdHTTPAppRequest`; CrossSocket provides `TCrossSocketWebRequest`; Apache provides `TApacheRequest`. Any cast ties your middleware to one Provider.

### Anti-pattern B: assuming a specific underlying library

```pascal
// DON'T — assumes Indy and its specific Body parsing
LField := Req.RawWebRequest.Files.Items[0].FieldName;
```

`Files` exists on Indy's `TIdHTTPAppRequest` but may not be populated correctly on the CrossSocket path. Use `Req.ContentFields` instead — Horse-level, provider-neutral.

### Best practice: stick to the `THorseRequest` / `THorseResponse` API

Provider-neutral by design:

| What you want | Use |
|---|---|
| HTTP method (verb) | `Req.Method` (PATCH-REQ-10) or `Req.MethodType` |
| Path | `Req.PathInfo` |
| Header | `Req.Headers['X-...']` |
| Query string | `Req.Query['name']` |
| Path param | `Req.Params['id']` |
| Cookie | `Req.Cookie['session']` |
| Body as string | `Req.Body` (cached on CrossSocket — PATCH-REQ-9) |
| Body as parsed object | `Req.Body<TJSONObject>` (after Jhonson) |
| Client IP | `Req.RawWebRequest.RemoteAddr` *(no high-level alternative — unavoidable)* |

### When you really must reach the underlying object

Examples: TLS client certificate inspection, raw stream access, `X-Forwarded-For` chain walking. The hybrid-adapter pattern (PATCH-REQ-8) ensures `Req.RawWebRequest.Method` / `.Host` / `.RemoteAddr` / `.GetFieldByName(...)` all work on every Provider. Stay on the abstract `TWebRequest` API:

```pascal
LIP := Req.RawWebRequest.GetFieldByName('X-Forwarded-For');
if LIP = '' then
  LIP := Req.RawWebRequest.RemoteAddr;
```

That snippet works on Indy, `fphttpserver`, CrossSocket, Apache, and ISAPI without changes.

For response-side: `Res.AddHeader` / `Res.RawWebResponse.SetCustomHeader` both work everywhere; `Res.AddHeader` is preferred (it populates Horse's own `FCustomHeaders` which the CrossSocket bridge reads).

---

## 9. Cross-compiler considerations

Three concrete pitfalls.

### a) `Next` type differs

```pascal
procedure MyMiddleware(Req: THorseRequest; Res: THorseResponse;
  Next: {$IF DEFINED(FPC)}TNextProc{$ELSE}TProc{$ENDIF});
```

Always wrap `Next` with this conditional — `TProc` on Delphi, `TNextProc` (a method-of-object alias declared in `Horse.Callback`) on FPC.

### b) Anonymous procedures

Delphi supports anonymous procedures everywhere. FPC supports them since 3.2 in `{$MODE DELPHI}`, but for FPC 3.0 compatibility (still common in older Lazarus installs) prefer top-level procedures:

```pascal
procedure MyMiddleware(...);   // top-level — works on both
begin
  ...
end;

// Delphi:
THorse.Use(MyMiddleware);

// FPC:
THorse.Use(@MyMiddleware);     // the @ is required
```

### c) `string` codepage

`string` is UTF-16 on Delphi, UTF-8 on FPC. For HTTP headers and bodies, Horse normalises to UTF-8 at the request boundary — you generally don't need to think about it inside middleware. **But** if your middleware does byte-level work (computing hashes, signing, byte-counting payloads), `Length(SomeString)` returns code-unit count which differs between platforms. Use `Length(TEncoding.UTF8.GetBytes(SomeString))` when you need the wire-protocol byte count.

---

## 10. Testing your middleware

Three test layers, each catching a different class of bug:

### Unit test — fast, narrow

Call the middleware procedure directly with mocked `THorseRequest` / `THorseResponse`. Fast feedback but doesn't catch integration issues.

### Integration test — slower, broader

Register a test route with your middleware, start a real Horse server on a test port, fire HTTP requests with `TIdHTTP` (Delphi) or `THTTPClient` (Delphi 10.4+) or `TFPHTTPClient` (FPC). Catches order-dependence, threading issues, and Provider-specific bugs.

```pascal
// Test server
THorse.Use(MyMiddleware);
THorse.Get('/test', procedure(Req: THorseRequest; Res: THorseResponse)
                    begin Res.Send('ok'); end);
THorse.Listen(9099);

// Test client
LResp := TIdHTTP.Create.Get('http://127.0.0.1:9099/test');
Assert(LResp = 'ok');
```

### Provider matrix — catches the rare ones

At minimum, run your integration tests under **Indy** (default — no define) and one of the async Providers — **CrossSocket** (`HORSE_PROVIDER_CROSSSOCKET`, or the legacy `HORSE_CROSSSOCKET`) or **mORMot2** (`HORSE_PROVIDER_MORMOT`). The two async Providers share the same hybrid-adapter contract, so middleware that passes one usually passes the other; running both gives the strongest signal. If you claim FPC support, run on FPC too with the `fphttpserver` default. The reference test patterns live in [`horse-provider-crosssocket/samples/tests/`](https://github.com/freitasjca/horse-provider-crosssocket/tree/master/samples/tests) and [`horse-provider-mormot/samples/tests/`](https://github.com/freitasjca/horse-provider-mormot/tree/master/samples/tests) — both register the same 32 black-box HTTP tests covering routing, body handling, concurrent requests, CORS, and shadow-field precedence so a single test client validates both transports.

---

## 11. Packaging for Boss

### Minimum `boss.json`

```json
{
  "name": "horse-middleware-mything",
  "description": "One-line description of what the middleware does",
  "version": "1.0.0",
  "homepage": "https://github.com/your-org/horse-middleware-mything",
  "license": "MIT",
  "mainsrc": "src/",
  "browsingpath": "src/",
  "dependencies": {
    "github.com/HashLoad/horse": ">=3.1.95"
  }
}
```

Bump the `horse` dependency to `>=3.1.96` if your middleware uses PATCH-REQ-9 (cached `Req.Body: string`) or PATCH-REQ-10 (`Req.Method: string`).

### Recommended repo layout

```
horse-middleware-mything/
├── boss.json
├── LICENSE
├── README.md                              ← keep it short; example, install, link to wiki
├── src/
│   └── Horse.Middleware.MyThing.pas
└── samples/
    └── tests/
        ├── HorseMyThingTestServer.dpr     ← starts server, exits via signal
        └── HorseMyThingTestClient.dpr     ← exit code = number of failures
```

Tag a release matching `boss.json.version` (e.g. `v1.0.0`) and Boss can resolve it:

```sh
boss install github.com/your-org/horse-middleware-mything
```

### Version naming

Boss v3.0.12 and later parses **dotted semver only** — `MAJOR.MINOR.PATCH`. **No hyphens, no pre-release suffixes**. So:

- ✅ `1.0.0`, `1.0.1`, `2.3.4`
- ❌ `1.0.0-beta.1`, `1.0.0-rc1`, `2.3.4-alpha`

If you need pre-releases for internal testing, use a separate branch (not a tag).

---

## 12. Publishing

Once the package compiles and the tests pass on the Provider matrix:

1. **Tag the release** on GitHub matching the `boss.json` version.
2. **Create a GitHub Release** with notes — `gh release create v1.0.0 --notes "..."` works.
3. **Open a PR against [`middleware-ecosystem.md`](./middleware-ecosystem.md)** adding a row to the "Third-party middleware" table — **both EN and PT-BR versions** (see [CONTRIBUTING](../CONTRIBUTING.md) for the bilingual rule).
4. **Announce on the Telegram channel** (`@hashload`) so the community sees it.

---

## Reference middlewares

The two middlewares maintained alongside `horse-provider-crosssocket` are the cleanest canonical references for the patterns in this guide:

| Repo | Pattern demonstrated | Particularly useful for |
|---|---|---|
| [`horse-request-guard`](https://github.com/freitasjca/horse-request-guard) | Class with `.New` factory, config record, defence-in-depth validation | Authoring middleware that short-circuits on bad input with specific HTTP status codes (400, 405, 413, 414, 431) |
| [`horse-security-headers`](https://github.com/freitasjca/horse-security-headers) | Response-side header injection, provider-neutral header writes | Authoring middleware that mutates the response after `Next()` returns |

Both ship with integration test suites that exercise the middleware on Indy and CrossSocket — clone, read, copy the test pattern.

---

## See also

- [Middleware](./middleware.md) — *using* middleware (the consumer side).
- [Providers & Application types](./providers.md) — why Provider-neutral matters, and what `Req.RawWebRequest` actually points at on each Provider.
- [Compiler Support](./compiler-support.md) — Delphi / FPC version requirements and the compiler-version guards to use.
- [Middleware Ecosystem](./middleware-ecosystem.md) — published packages you can study for further patterns.
- [`CONTRIBUTING.md`](../CONTRIBUTING.md) — code style, the bilingual rule, the PR process.
