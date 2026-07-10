# Request & Response

*Read this in [English](./request-response.md) or [Português (BR)](./request-response.pt-BR.md).*

Every Horse route callback receives a `THorseRequest` and a `THorseResponse`. This page is the API reference for both.

```delphi
procedure(Req: THorseRequest; Res: THorseResponse)
```

For the route declaration itself, see [Routing](./routing.md). For middleware that wraps these objects, see [Middleware](./middleware.md).

---

## `THorseRequest`

| Accessor | Type | What it returns |
|---|---|---|
| `Body` | `string` | Raw request body decoded as UTF-8. Idempotent — multiple reads return the same cached string. |
| `Body<T>` | generic | Returns `FBody as T` — used when middleware (e.g. `Jhonson`) parses the body into an object. |
| `Body(AObject)` / `Body(AObject, AOwnsBody)` | setter | Used by middleware to attach a parsed body object. With `AOwnsBody = True` (the default / 1-arg form) Horse owns and frees the object, freeing any previous owned value first. Transports whose body is a non-owning reference into a socket buffer (e.g. CrossSocket) pass `AOwnsBody = False` so `Clear` nils the reference without freeing it. |
| `Params` | `THorseCoreParam` | Route path parameters: `Req.Params['id']`. |
| `Query` | `THorseCoreParam` | URL query string: `Req.Query['name']`. |
| `Headers` | `THorseCoreParam` | Request headers: `Req.Headers['Content-Type']`. Case-insensitive lookup. |
| `Cookie` | `THorseCoreParam` | Parsed `Cookie:` header: `Req.Cookie['session']`. Values containing `=` (base64/JWT) are preserved in full (the parser splits on the **first** `=` only). |
| `ContentFields` | `THorseCoreParam` | Parsed `application/x-www-form-urlencoded` body fields. |
| `Sessions` | `THorseSessions` | Server-side session map (one per request). |
| `Method` | `string` | Raw HTTP verb: `'GET'`, `'POST'`, `'OPTIONS'`, etc. |
| `MethodType` | `TMethodType` | Enum form (see [Routing](./routing.md)). |
| `PathInfo` | `string` | Decoded path: `/users/42`. |
| `Host` | `string` | `Host:` header value. |
| `ContentType` | `string` | `Content-Type` request header. |
| `RawWebRequest` | `TWebRequest` / `TRequest` | The underlying provider object — Indy's `TIdHTTPRequestInfo` for the default provider, an adapter for non-Indy providers. |

### Reading a request body

```delphi
THorse.Post('/echo',
  procedure(Req: THorseRequest; Res: THorseResponse)
  begin
    // Raw text body
    Res.Send('You sent: ' + Req.Body);
  end);
```

For JSON, register the `Jhonson` middleware once at startup and then use `Body<TJSONObject>`:

```delphi
uses Horse, Horse.Jhonson, System.JSON;

THorse.Use(Jhonson);

THorse.Post('/items',
  procedure(Req: THorseRequest; Res: THorseResponse)
  var
    Json: TJSONObject;
  begin
    Json := Req.Body<TJSONObject>;
    Res.Send('Got: ' + Json.GetValue('name').Value);
  end);
```

### Reading params, query, headers

All four return `THorseCoreParam`, a dictionary-like accessor:

```delphi
THorse.Get('/search/:type',
  procedure(Req: THorseRequest; Res: THorseResponse)
  var
    Limit: Integer;
  begin
    if not TryStrToInt(Req.Query['limit'], Limit) then
      Limit := 10;

    Res.Send(Format('Searching %s, limit %d, by %s',
      [Req.Params['type'], Limit, Req.Headers['X-User']]));
  end);
```

`THorseCoreParam`:

- `Items[name: string]: string` — default indexer, returns `''` if absent.
- `TryGetValue(name; out value): Boolean` — distinguish absent vs empty.
- `Dictionary: TDictionary<string,string>` (Delphi) / `TStringList` (FPC) — direct collection access if you need to iterate.

### File uploads

`multipart/form-data` requests populate `Req.ContentFields`:

```delphi
THorse.Post('/upload',
  procedure(Req: THorseRequest; Res: THorseResponse)
  var
    Stream: TStream;
  begin
    Stream := Req.ContentFields.Field('file').AsStream;   // file field (text fields: Field('x').AsString)
    try
      Stream.SaveToFile('uploaded.bin');
      Res.Send('Saved ' + IntToStr(Stream.Size) + ' bytes');
    finally
      // CrossSocket path: do NOT free — it's a non-owning ref.
      // Indy path: also do not free; THorseRequest manages it.
    end;
  end);
```

---

## `THorseResponse`

| Method | Returns | Effect |
|---|---|---|
| `Send(AContent: string)` | `THorseResponse` (fluent) | Writes a string body. Default status `200`. |
| `Send<T>(AContent: T)` | `THorseResponse` | Writes an object; the middleware chain typically serialises it (e.g. JSON via `Jhonson`). |
| `Status(AStatus: Integer)` | `THorseResponse` | Sets HTTP status code. Default `200`. |
| `Status(AStatus: THTTPStatus)` | `THorseResponse` | Typed variant — e.g. `THTTPStatus.NotFound`. |
| `Status` (no arg) | `Integer` | Reads the currently-set status. |
| `ContentType(AContentType: string)` | `THorseResponse` | Sets `Content-Type` header. |
| `AddHeader(AName, AValue: string)` | `THorseResponse` | Adds a response header. |
| `RemoveHeader(AName: string)` | `THorseResponse` | Removes a previously-added header. |
| `Cookie(AName, AValue: string)` | `THorseCookie` | Adds a cookie and returns it for fluent attribute setting. See [Cookies](#cookies). |
| `AddCookie(ACookie: THorseCookie)` | `THorseResponse` | Adds a pre-built `THorseCookie` (ownership transferred to the response). |
| `RedirectTo(ALocation: string)` | `THorseResponse` | Sends `302 Found` with `Location:`. |
| `RedirectTo(ALocation, AStatus)` | `THorseResponse` | Lets you choose `301`, `307`, etc. |
| `SendFile(AFileName: string)` | `THorseResponse` | Streams a file as the body; sets `Content-Type` from the extension. |
| `SendFile(AStream, AFileName, AContentType)` | `THorseResponse` | Streams an in-memory stream. |
| `Download(AFileName: string)` | `THorseResponse` | Like `SendFile` but adds `Content-Disposition: attachment`. |
| `Download(AStream, AFileName, AContentType)` | `THorseResponse` | Stream + attachment header. |
| `Render(AFileName: string)` | `THorseResponse` | Streams a file inline (no attachment header). |
| `RawWebResponse` | `TWebResponse` / `TResponse` | Underlying provider response. Used by middleware that needs direct access. |

### Examples

**Plain text:**
```delphi
Res.ContentType('text/plain').Send('hello');
```

**JSON (via Jhonson):**
```delphi
uses System.JSON;

var Json := TJSONObject.Create;
Json.AddPair('ok', TJSONBool.Create(True));
Res.Send<TJSONObject>(Json);   // Jhonson serialises + frees
```

**Status + body for an error:**
```delphi
Res.Status(THTTPStatus.BadRequest)
   .ContentType('application/json')
   .Send('{"error":"missing field"}');
```

**Redirect:**
```delphi
Res.RedirectTo('/login');
```

### Cookies

`Res.Cookie(name, value)` adds an RFC 6265 cookie and returns a `THorseCookie`
(unit `Horse.Core.Cookie`) for fluent attribute setting. Each cookie becomes its
own `Set-Cookie` header — you can set **several** cookies in one response:

```delphi
uses Horse.Core.Cookie;   // for TSameSite (ssStrict / ssLax / ssNone)

Res.Cookie('sid', SessionId)
   .Path('/')
   .HttpOnly(True)
   .Secure(True)
   .SameSite(ssLax)
   .MaxAge(3600);

Res.Cookie('theme', 'dark');   // a second cookie → a second Set-Cookie line
```

| Attribute | Method |
|---|---|
| `Path` / `Domain` | `.Path('/')`, `.Domain('example.com')` |
| `Expires` (UTC) | `.Expires(EncodeDate(2030,1,1))` |
| `Max-Age` (seconds) | `.MaxAge(3600)` |
| `Secure` / `HttpOnly` | `.Secure(True)`, `.HttpOnly(True)` |
| `SameSite` | `.SameSite(ssStrict | ssLax | ssNone)` |

**Validation:** the cookie name must be a token and neither name nor value may
contain control characters, CR/LF or `;` — invalid input raises `EHorseException`.
This validation applies only to the typed API; the legacy
`Res.AddHeader('Set-Cookie', …)` path is unchanged (but holds only **one** cookie,
since it goes through the header map).

**Provider notes:**
- **CrossSocket** and **mORMot** emit every attribute, one `Set-Cookie` line per cookie.
- **Indy (Delphi)** maps onto `TWebResponse.Cookies`: `Max-Age` is not representable
  there (use `.Expires()` on Indy); `HttpOnly` needs Delphi 10.1+, `SameSite` Delphi 10.4+.

**File download:**
```delphi
Res.Download('reports/2026-05.pdf');
// Sends Content-Disposition: attachment; filename="2026-05.pdf"
```

**Stream a generated file:**
```delphi
var Stream := TMemoryStream.Create;
GenerateCSV(Stream);
Stream.Position := 0;
Res.Download(Stream, 'export.csv', 'text/csv');
```

**Adding a custom header:**
```delphi
Res.AddHeader('X-Rate-Limit-Remaining', '47').Send('ok');
```

### Status helpers

`Horse.Commons.THTTPStatus` provides named constants for the common codes — preferred over magic numbers:

```pascal
Res.Status(THTTPStatus.OK);              // 200
Res.Status(THTTPStatus.Created);         // 201
Res.Status(THTTPStatus.NoContent);       // 204
Res.Status(THTTPStatus.BadRequest);      // 400
Res.Status(THTTPStatus.Unauthorized);    // 401
Res.Status(THTTPStatus.NotFound);        // 404
Res.Status(THTTPStatus.InternalServerError); // 500
```

`Horse.Commons.THTTPStatusHelper.ToString` converts the enum back to its standard reason phrase if you ever need to print it.

### Errors and exceptions

Raising `EHorseException` from a callback short-circuits the response:

```delphi
uses Horse.Exception;

THorse.Get('/secret',
  procedure(Req: THorseRequest; Res: THorseResponse)
  begin
    if Req.Headers['X-Auth'] <> 'secret' then
      raise EHorseException.New.Status(THTTPStatus.Unauthorized).Error('Bad token');
    Res.Send('welcome');
  end);
```

The framework converts the exception to a JSON error response. Any other uncaught exception becomes a `500` with a generic body.

To intercept exceptions globally, register the `handle-exception` middleware ([`HashLoad/handle-exception`](https://github.com/HashLoad/handle-exception)) — it formats your errors consistently.

## Chunked Transfer Encoding (Streaming Responses)

When you need to send large payloads (like large report files or live video/audio streams) without loading the entire content into memory at once, use **Chunked Transfer Encoding**:

```delphi
THorse.Get('/data/heavy',
  procedure(Req: THorseRequest; Res: THorseResponse)
  var
    Stream: TStringStream;
  begin
    // Indicated chunked transfer encoding
    Res.AddHeader('Transfer-Encoding', 'chunked');
    Res.ContentType('text/plain');
    
    // Write portions of data
    Stream := TStringStream.Create('First chunk of text...');
    try
      Res.Send(Stream.DataString);
      // Horse handles the underlying socket chunking automatically based on the Provider
    finally
      Stream.Free;
    end;
  end);
```

## Best Practices with Large Payloads (Upload/Download)

*   **Avoid loading entire files into String or MemoryStream**: Always use a `TFileStream` to read from or write to disk directly, passing the stream reference to `Res.SendFile` or `Res.Download`. Horse will stream the file in small chunks (usually 8KB), keeping the server's memory consumption flat.
*   **Request body size limits**: On Indy (default provider), you can configure max content length or timeouts directly in the underlying server instance (`THorse.RawWebserver`).

---

## Provider-specific notes

Most application code never needs to think about the transport. A few exceptions:

- **Body ownership on CrossSocket:** `Req.Body<TStream>` on the CrossSocket provider returns a non-owning reference into the receive buffer. Never `Free` it. If you need the stream after the request returns, copy into a `TMemoryStream` you own. (Doesn't apply to Indy — Indy gives you its own owned stream.)
- **Body ownership on mORMot2:** the mORMot provider does not produce a `TStream` body at all — the request body is buffered as a `RawByteString` (`InContent`) owned by mORMot. `Req.Body: string` is decoded once at request entry and cached (PATCH-REQ-9), so reading it multiple times is O(1). `Req.Body<TStream>` is therefore not the right pattern on this transport; use `Req.Body: string` (text) or `Req.RawWebRequest.Content` (raw bytes) instead.
- **Concurrent handlers:** Indy runs one thread per connection; CrossSocket dispatches to an IO thread pool (and an optional Horse worker pool); mORMot2 dispatches to its own fixed thread pool inside `THttpServer` (default 32, configurable). In every case your handler runs to completion on a single thread, so per-request state is safe. Shared state needs explicit locking (`TCriticalSection`, `TMonitor`).
- **`Req.RawWebRequest` and `Res.RawWebResponse`:** middleware that pokes the underlying objects (e.g. `Horse.CORS` setting `Access-Control-Allow-Origin` directly) keeps working across every Provider — CrossSocket and mORMot2 both return an adapter object backed by the same `IHorseRawRequest` / `IHorseRawResponse` interface that exposes the same surface as the Indy `TIdHTTPAppRequest` / `TIdHTTPAppResponse`.

See [Providers](./providers.md) for the full breakdown.

## See also

- [Routing](./routing.md) — declare the routes that produce these callbacks.
- [Middleware](./middleware.md) — wrap callbacks with cross-cutting logic.
- [Middleware Ecosystem](./middleware-ecosystem.md) — `Jhonson` (JSON), `CORS`, `JWT`, `compression`, and more.
